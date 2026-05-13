//! Doc 77c Phase A.4.4 — `NlBoolVector` layout-pinned mutable bool vector.
//!
//! Specialized self-managed refcounted box carrying one mutable
//! `Vec<bool>` slot.  Replaces the legacy
//! `Sexp::BoolVector(Rc<RefCell<Vec<bool>>>)' with a layout-pinned
//! struct so the boxed-variant ABI is uniform across `Cons' / `Cell' /
//! `MutStr' / `Vector' / `BoolVector' (= NlConsBox / NlCell / NlStr /
//! NlVector / NlBoolVector all share the `value @ 0, refcount @
//! trailer' shape, modulo the cons-specific 2-slot layout).
//!
//! Layout (Phase A.4.4 locked):
//!
//! ```text
//! NlBoolVector:  +-----+  offset 0                       (sizeof Vec<bool>)  value
//!                +-----+  offset sizeof(Vec<bool>)       (8 bytes)           refcount
//!                +-----+
//! ```
//!
//! As with [`super::nlvector::NlVector`], the `Vec<bool>' header (=
//! ptr / len / cap triple) keeps its native Rust layout; `#[repr(C)]'
//! pins the *outer* field order.  Phase B elisp follows the header
//! ptr to walk elements (= 2-load access pattern, same as Rust today).
//!
//! Mutability:
//! - `unsafe set_value(v: Vec<bool>)' — wholesale replace.
//! - `unsafe with_value_mut(f)' — closure-style in-place mutation
//!   (= the `aset' callsite uses this for `vec[idx] = bit' indexing).
//!
//! Out of scope for Phase A.4.4:
//!   - Other variants (Record / CharTable) — A.4.5-A.4.6 follow-up
//!     sub-stages.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned mutable bool vector.  Heap-allocated, refcounted
/// via an `AtomicUsize` trailer.  Accessed through [`NlBoolVectorRef`]
/// handles.
#[repr(C)]
pub struct NlBoolVector {
    /// The mutable bool vector slot.  Offset 0.
    pub value: Vec<bool>,
    /// Strong reference count.  Starts at 1 in [`NlBoolVectorRef::new`].
    pub refcount: AtomicUsize,
}

/// Refcounted handle to an [`NlBoolVector`].  API parity with
/// [`super::nlvector::NlVectorRef`] / [`super::nlstr::NlStrRef`] /
/// [`super::nlcell::NlCellRef`] / [`super::nlconsbox::NlConsBoxRef`].
///
/// Phase A.5.1 (Doc 77c §4.6.1, 2026-05-09): `#[repr(transparent)]' pins
/// the layout to `NonNull<NlBoolVector>' so JIT trampolines + Phase B
/// elisp can read the bool-vector pointer directly off the `Sexp' payload
/// at offset `SEXP_PAYLOAD_OFFSET'.
#[repr(transparent)]
pub struct NlBoolVectorRef {
    ptr: NonNull<NlBoolVector>,
    _marker: PhantomData<NlBoolVector>,
}

impl NlBoolVector { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlBoolVector>; } // Doc 79 v4 C.4-atomic
impl NlBoolVectorRef {
    /// Allocate a fresh [`NlBoolVector`] on the heap with `refcount = 1'.
    pub fn new(value: Vec<bool>) -> NlBoolVectorRef {
        let layout = Layout::new::<NlBoolVector>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlBoolVector;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlBoolVector' and is
        // exclusively owned here.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlBoolVectorRef {
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

    /// Wholesale replace `value`.  Drops the previous Vec, then
    /// writes the new one.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&Vec<bool>` borrow into this
    /// box's `value` is live.  Phase A.2.1 setcar discipline applies.
    pub unsafe fn set_value(&self, val: Vec<bool>) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.
    ///
    /// # Safety
    ///
    /// Same as [`set_value`]: no other `&Vec<bool>` borrow into
    /// `self.value' may be live for the duration of the closure.
    /// Reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut Vec<bool>) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlBoolVectorRef {
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlBoolVectorRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlBoolVectorRef {
    fn drop(&mut self) { unsafe { crate::nlrc_drop_box!(self.ptr.as_ptr(), NlBoolVector, crate::eval::sexp::SEXP_TAG_BOOL_VECTOR); } }
}

impl Deref for NlBoolVectorRef {
    type Target = NlBoolVector;

    fn deref(&self) -> &NlBoolVector {
        // SAFETY: see `NlVectorRef::deref'.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlBoolVectorRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("BoolVector").field(&self.value).finish()
    }
}

impl PartialEq for NlBoolVectorRef {
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.value == other.value
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlBoolVector, value) == 0);
    assert!(offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>());
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_value_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlBoolVector, value), 0);
    }

    #[test]
    fn layout_refcount_after_value() {
        use std::mem::{offset_of, size_of};
        assert_eq!(offset_of!(NlBoolVector, refcount), size_of::<Vec<bool>>());
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let v = NlBoolVectorRef::new(vec![true, false]);
        assert_eq!(NlBoolVectorRef::strong_count(&v), 1);
    }

    #[test]
    fn new_returns_box_holding_value() {
        let v = NlBoolVectorRef::new(vec![true, false, true]);
        assert_eq!(v.value.len(), 3);
        assert_eq!(v.value[0], true);
        assert_eq!(v.value[2], true);
    }

    #[test]
    fn clone_bumps_refcount_and_shares_value() {
        let a = NlBoolVectorRef::new(vec![true]);
        let b = a.clone();
        assert_eq!(NlBoolVectorRef::strong_count(&a), 2);
        assert_eq!(b.value, vec![true]);
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlBoolVectorRef::new(vec![]);
        {
            let _b = a.clone();
            assert_eq!(NlBoolVectorRef::strong_count(&a), 2);
        }
        assert_eq!(NlBoolVectorRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlBoolVectorRef::new(vec![true]);
        let b = a.clone();
        assert!(NlBoolVectorRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_alloc() {
        let a = NlBoolVectorRef::new(vec![true]);
        let b = NlBoolVectorRef::new(vec![true]);
        assert!(!NlBoolVectorRef::ptr_eq(&a, &b));
    }

    #[test]
    fn set_value_replaces_in_place() {
        let v = NlBoolVectorRef::new(vec![true]);
        // SAFETY: no other borrow live.
        unsafe { v.set_value(vec![false, true]) };
        assert_eq!(v.value.len(), 2);
        assert_eq!(v.value[0], false);
    }

    #[test]
    fn set_value_visible_through_clone() {
        let a = NlBoolVectorRef::new(vec![true]);
        let b = a.clone();
        unsafe { a.set_value(vec![false]) };
        assert_eq!(a.value, vec![false]);
        assert_eq!(b.value, vec![false]);
    }

    #[test]
    fn with_value_mut_index_set() {
        let v = NlBoolVectorRef::new(vec![true, true, true]);
        // SAFETY: no other borrow live.
        unsafe {
            v.with_value_mut(|vec| {
                vec[1] = false;
            });
        }
        assert_eq!(v.value[1], false);
    }

    #[test]
    fn with_value_mut_push() {
        let v = NlBoolVectorRef::new(vec![]);
        unsafe {
            v.with_value_mut(|vec| {
                vec.push(true);
                vec.push(false);
            });
        }
        assert_eq!(v.value.len(), 2);
    }

    #[test]
    fn with_value_mut_returns_value_from_closure() {
        let v = NlBoolVectorRef::new(vec![true]);
        let len = unsafe { v.with_value_mut(|vec| vec.len()) };
        assert_eq!(len, 1);
    }

    #[test]
    fn debug_format_uses_boolvector_tuple() {
        let v = NlBoolVectorRef::new(vec![true]);
        let d = format!("{:?}", v);
        assert!(
            d.starts_with("BoolVector("),
            "expected `BoolVector(...)' debug shape, got {:?}",
            d
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlBoolVectorRef::new(vec![true]);
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlBoolVectorRef::new(vec![true, false]);
        let b = NlBoolVectorRef::new(vec![true, false]);
        assert_eq!(a, b);
        let c = NlBoolVectorRef::new(vec![true, true]);
        assert_ne!(a, c);
        let d = NlBoolVectorRef::new(vec![true]);
        assert_ne!(a, d);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Round-trip clone+drop without panic — ASAN / miri verify
        // no UB on the freed allocation.
        let v = NlBoolVectorRef::new(vec![true; 100]);
        {
            let _t = v.clone();
        }
        drop(v);
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlBoolVectorRef::new(vec![true]);
        {
            let _b = a.clone();
        }
        let c = a.clone();
        assert!(NlBoolVectorRef::ptr_eq(&a, &c));
    }

    #[test]
    fn empty_vector_round_trip() {
        let v = NlBoolVectorRef::new(vec![]);
        assert_eq!(v.value.len(), 0);
        assert_eq!(NlBoolVectorRef::strong_count(&v), 1);
    }
}
