//! Doc 77c Phase A.4.6 — `NlCharTable` layout-pinned char-table cell.
//!
//! Final A.4.x sub-stage.  Specialized self-managed refcounted box
//! carrying the existing [`CharTableInner`] (= subtype / default_val /
//! entries / parent / extra) in a single allocation.  Replaces the
//! legacy `Sexp::CharTable(Rc<RefCell<CharTableInner>>)' so the
//! boxed-variant ABI is uniform across `Cons' / `Cell' / `MutStr' /
//! `Vector' / `BoolVector' / `Record' / `CharTable' (= every Phase
//! A.4.x box now shares the `value(s) @ 0, refcount @ trailer' shape).
//!
//! Layout (Phase A.4.6 locked):
//!
//! ```text
//! NlCharTable:  +-----+  offset 0                       (sizeof CharTableInner)  inner
//!               +-----+  offset sizeof(CharTableInner)  (8 bytes)                refcount
//!               +-----+
//! ```
//!
//! Self-reference caveat: the `parent' field of [`CharTableInner`] is
//! `Option<NlCharTableRef>' — a self-reference.  Refcount semantics
//! handle the chain naturally: dropping a child decrements the parent
//! refcount, which cascades freeing the parent if it was the last
//! holder.  Cycles cannot form because the only way to install a parent
//! is via `with_inner_mut' on the *child*, and the parent's own parent
//! field is independent (no API for "self-parent").
//!
//! Identity:
//! - `eq': `NlCharTableRef::ptr_eq' (= same allocation).  Strictly
//!   stronger than the legacy `Rc::ptr_eq' on the inner cell, but
//!   observably equivalent — there is exactly one box per char-table.
//!
//! Mutability:
//! - `unsafe with_inner_mut(f)' — closure-style in-place mutation
//!   of the entire [`CharTableInner`].  Mirrors the legacy
//!   `RefCell::borrow_mut()' surface used by `char_table_set_one'
//!   and image-format decode.
//!
//! Out of scope for Phase A.4.6:
//!   - JIT direct emit (Phase A.5).
//!   - elisp self-host primitive access (Phase B).

use crate::eval::sexp::CharTableInner;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned char-table cell.  Heap-allocated, refcounted via an
/// `AtomicUsize` trailer.  Accessed through [`NlCharTableRef`] handles.
#[repr(C)]
pub struct NlCharTable {
    /// The full char-table state.  Offset 0.
    pub inner: CharTableInner,
    /// Strong reference count.  Starts at 1 in [`NlCharTableRef::new`].
    pub refcount: AtomicUsize,
}

/// Refcounted handle to an [`NlCharTable`].  API parity with
/// [`super::nlrecord::NlRecordRef`] / [`super::nlboolvector::NlBoolVectorRef`].
pub struct NlCharTableRef {
    ptr: NonNull<NlCharTable>,
    _marker: PhantomData<NlCharTable>,
}

impl NlCharTableRef {
    /// Allocate a fresh [`NlCharTable`] on the heap with `refcount = 1'.
    pub fn new(inner: CharTableInner) -> NlCharTableRef {
        let layout = Layout::new::<NlCharTable>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlCharTable;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlCharTable' and is
        // exclusively owned here.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).inner), inner);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlCharTableRef {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// In-place mutation closure for the full inner state.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&CharTableInner` borrow into
    /// `self.inner' is live for the duration of the closure.
    /// Reentrant calls are UB.  Phase A.2.1 setcar discipline applies.
    pub unsafe fn with_inner_mut<R>(&self, f: impl FnOnce(&mut CharTableInner) -> R) -> R {
        let inner_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).inner);
        unsafe { f(&mut *inner_ptr) }
    }
}

impl Clone for NlCharTableRef {
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlCharTableRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlCharTableRef {
    fn drop(&mut self) {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        let prev = unsafe {
            (*self.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0 and `NlCharTableRef' is not
        // Send / Sync.  Drop the inner payload (= which transitively
        // drops the parent NlCharTableRef, if any, decrementing the
        // parent chain) then dealloc.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*self.ptr.as_ptr()).inner));
            let layout = Layout::new::<NlCharTable>();
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl Deref for NlCharTableRef {
    type Target = NlCharTable;

    fn deref(&self) -> &NlCharTable {
        // SAFETY: see `NlVectorRef::deref'.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlCharTableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CharTable")
            .field("inner", &self.inner)
            .finish()
    }
}

impl PartialEq for NlCharTableRef {
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.inner == other.inner
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlCharTable, inner) == 0);
    assert!(offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>());
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::sexp::Sexp;

    fn sym(name: &str) -> Sexp {
        Sexp::Symbol(name.to_string())
    }

    fn empty_inner(subtype: Sexp, default_val: Sexp) -> CharTableInner {
        CharTableInner {
            subtype,
            default_val,
            entries: Vec::new(),
            parent: None,
            extra: Vec::new(),
        }
    }

    #[test]
    fn layout_inner_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlCharTable, inner), 0);
    }

    #[test]
    fn layout_refcount_after_inner() {
        use std::mem::{offset_of, size_of};
        assert_eq!(
            offset_of!(NlCharTable, refcount),
            size_of::<CharTableInner>()
        );
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let r = NlCharTableRef::new(empty_inner(sym("syntax"), Sexp::Nil));
        assert_eq!(NlCharTableRef::strong_count(&r), 1);
    }

    #[test]
    fn new_returns_box_holding_inner() {
        let r = NlCharTableRef::new(empty_inner(sym("display"), Sexp::Int(42)));
        assert_eq!(r.inner.subtype, sym("display"));
        assert_eq!(r.inner.default_val, Sexp::Int(42));
        assert!(r.inner.entries.is_empty());
        assert!(r.inner.parent.is_none());
        assert!(r.inner.extra.is_empty());
    }

    #[test]
    fn clone_bumps_refcount_and_shares_inner() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        let b = a.clone();
        assert_eq!(NlCharTableRef::strong_count(&a), 2);
        assert_eq!(b.inner.subtype, sym("x"));
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        {
            let _b = a.clone();
            assert_eq!(NlCharTableRef::strong_count(&a), 2);
        }
        assert_eq!(NlCharTableRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        let b = a.clone();
        assert!(NlCharTableRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_alloc() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        let b = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        assert!(!NlCharTableRef::ptr_eq(&a, &b));
    }

    #[test]
    fn with_inner_mut_pushes_entry() {
        let r = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        // SAFETY: no other borrow live.
        unsafe {
            r.with_inner_mut(|i| {
                i.entries.push((65, Sexp::Int(1)));
            });
        }
        assert_eq!(r.inner.entries, vec![(65, Sexp::Int(1))]);
    }

    #[test]
    fn with_inner_mut_visible_through_clone() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        let b = a.clone();
        unsafe {
            a.with_inner_mut(|i| i.entries.push((97, Sexp::Int(2))));
        }
        assert_eq!(a.inner.entries, vec![(97, Sexp::Int(2))]);
        assert_eq!(b.inner.entries, vec![(97, Sexp::Int(2))]);
    }

    #[test]
    fn with_inner_mut_returns_value_from_closure() {
        let r = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        let count = unsafe { r.with_inner_mut(|i| i.entries.len()) };
        assert_eq!(count, 0);
    }

    #[test]
    fn parent_self_reference_drops_clean() {
        // Child holds parent NlCharTableRef.  Drop both without UB.
        let parent = NlCharTableRef::new(empty_inner(sym("base"), Sexp::Int(7)));
        let child = NlCharTableRef::new(empty_inner(sym("derived"), Sexp::Nil));
        unsafe {
            child.with_inner_mut(|i| i.parent = Some(parent.clone()));
        }
        assert_eq!(NlCharTableRef::strong_count(&parent), 2);
        drop(child);
        assert_eq!(NlCharTableRef::strong_count(&parent), 1);
    }

    #[test]
    fn parent_chain_two_deep() {
        let grandparent = NlCharTableRef::new(empty_inner(sym("g"), Sexp::Int(1)));
        let parent = NlCharTableRef::new(empty_inner(sym("p"), Sexp::Int(2)));
        let child = NlCharTableRef::new(empty_inner(sym("c"), Sexp::Nil));
        unsafe {
            parent.with_inner_mut(|i| i.parent = Some(grandparent.clone()));
            child.with_inner_mut(|i| i.parent = Some(parent.clone()));
        }
        assert_eq!(NlCharTableRef::strong_count(&grandparent), 2);
        assert_eq!(NlCharTableRef::strong_count(&parent), 2);
        drop(parent);
        // child still keeps parent alive
        assert_eq!(NlCharTableRef::strong_count(&grandparent), 2);
        drop(child);
        // chain collapsed
        assert_eq!(NlCharTableRef::strong_count(&grandparent), 1);
    }

    #[test]
    fn debug_format_uses_chartable_struct() {
        let r = NlCharTableRef::new(empty_inner(sym("syntax"), Sexp::Nil));
        let d = format!("{:?}", r);
        assert!(
            d.starts_with("CharTable"),
            "expected `CharTable {{...}}' debug shape, got {:?}",
            d
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Int(1)));
        let b = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Int(1)));
        assert_eq!(a, b);
        let c = NlCharTableRef::new(empty_inner(sym("y"), Sexp::Int(1)));
        assert_ne!(a, c);
        let d = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Int(2)));
        assert_ne!(a, d);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Round-trip clone+drop without panic — ASAN / miri verify
        // no UB on the freed allocation.
        let mut inner = empty_inner(sym("big"), Sexp::Nil);
        for i in 0..100 {
            inner.entries.push((i, Sexp::Int(i)));
        }
        let r = NlCharTableRef::new(inner);
        {
            let _t = r.clone();
        }
        drop(r);
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
        {
            let _b = a.clone();
        }
        let c = a.clone();
        assert!(NlCharTableRef::ptr_eq(&a, &c));
    }

    #[test]
    fn extra_slots_round_trip() {
        let mut inner = empty_inner(sym("case"), Sexp::Nil);
        inner.extra = vec![Sexp::Str("up".into()), Sexp::Str("down".into())];
        let r = NlCharTableRef::new(inner);
        assert_eq!(r.inner.extra.len(), 2);
        assert_eq!(r.inner.extra[0], Sexp::Str("up".into()));
        assert_eq!(r.inner.extra[1], Sexp::Str("down".into()));
    }

    #[test]
    fn empty_chartable_round_trip() {
        let r = NlCharTableRef::new(empty_inner(Sexp::Nil, Sexp::Nil));
        assert!(r.inner.entries.is_empty());
        assert!(r.inner.parent.is_none());
        assert!(r.inner.extra.is_empty());
        assert_eq!(NlCharTableRef::strong_count(&r), 1);
    }

    #[test]
    fn subtype_and_default_preserved_on_clone() {
        let a = NlCharTableRef::new(empty_inner(sym("syntax"), Sexp::Int(99)));
        let b = a.clone();
        assert_eq!(a.inner.subtype, sym("syntax"));
        assert_eq!(a.inner.default_val, Sexp::Int(99));
        assert_eq!(b.inner.subtype, sym("syntax"));
        assert_eq!(b.inner.default_val, Sexp::Int(99));
    }
}
