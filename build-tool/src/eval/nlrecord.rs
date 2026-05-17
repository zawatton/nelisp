//! Doc 77c Phase A.4.5 — `NlRecord` layout-pinned record cell.
//!
//! Specialized self-managed refcounted box carrying a record's
//! `type_tag: Sexp` and `slots: Vec<Sexp>` in a single allocation.
//! Replaces the legacy
//! `Sexp::Record { type_tag: Box<Sexp>, slots: Rc<RefCell<Vec<Sexp>>> }'
//! struct variant with one boxed cell so the boxed-variant ABI is
//! uniform across `Cons' / `Cell' / `MutStr' / `Vector' / `BoolVector'
//! / `Record' (= NlConsBox / NlCell / NlStr / NlVector / NlBoolVector
//! / NlRecord all share the `value(s) @ 0, refcount @ trailer' shape).
//!
//! Layout (Phase A.4.5 locked):
//!
//! ```text
//! NlRecord:  +-----+  offset 0                       (sizeof Sexp)        type_tag
//!            +-----+  offset sizeof(Sexp)            (sizeof Vec<Sexp>)   slots
//!            +-----+  offset sizeof(Sexp)+sizeof(Vec) (8 bytes)           refcount
//!            +-----+
//! ```
//!
//! Like [`super::nlconsbox::NlConsBox`] this is a 2-payload-field box.
//! Phase B elisp follows the type_tag value at offset 0 (= a single
//! Sexp inline) and the slots vector header at offset
//! `sizeof(Sexp)' for `record-type' / `record-ref' / `record-set'.
//!
//! Identity:
//! - `eq': `NlRecordRef::ptr_eq' (= same allocation).  This is
//!   strictly stronger than the legacy "same slots Rc + value-equal
//!   type_tag" rule but consistent in practice — the ONLY constructor
//!   in `Sexp::record' allocates a fresh box each call, so two records
//!   with shared slots are unreachable from elisp.
//!
//! Mutability:
//! - Slot mutation: `unsafe with_slots_mut(f)' for `record-set'.
//! - The `type_tag' is immutable after construction in elisp surface
//!   (= `cl-defstruct' fixes it once); no setter is exposed.
//!
//! Out of scope for Phase A.4.5:
//!   - `CharTable' (= self-ref parent field) — A.4.6 follow-up.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned record cell.  Heap-allocated, refcounted via an
/// `AtomicUsize` trailer.  Accessed through [`NlRecordRef`] handles.
#[repr(C)]
pub struct NlRecord {
    /// Type tag (= struct type symbol).  Offset 0.
    pub type_tag: Sexp,
    /// User-visible slots vector.  Offset `sizeof(Sexp)'.
    pub slots: Vec<Sexp>,
    /// Strong reference count.  Starts at 1 in [`NlRecordRef::new`].
    pub refcount: AtomicUsize,
}

/// Refcounted handle to an [`NlRecord`].  API parity with
/// [`super::nlvector::NlVectorRef`] / [`super::nlconsbox::NlConsBoxRef`].
///
/// Phase A.5.1 (Doc 77c §4.6.1, 2026-05-09): `#[repr(transparent)]' pins
/// the layout to `NonNull<NlRecord>' so JIT trampolines + Phase B elisp
/// can read the record pointer directly off the `Sexp' payload at offset
/// `SEXP_PAYLOAD_OFFSET'.
#[repr(transparent)]
pub struct NlRecordRef {
    ptr: NonNull<NlRecord>,
    _marker: PhantomData<NlRecord>,
}

impl NlRecord { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlRecord>; } // Doc 79 v4 C.4-atomic
impl NlRecordRef {
    /// Allocate a fresh [`NlRecord`] on the heap with `refcount = 1'.
    pub fn new(type_tag: Sexp, slots: Vec<Sexp>) -> NlRecordRef {
        let layout = Layout::new::<NlRecord>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlRecord;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlRecord' and is
        // exclusively owned here.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).type_tag), type_tag);
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).slots), slots);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlRecordRef {
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

    /// In-place mutation closure for `slots'.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&Vec<Sexp>` borrow into
    /// `self.slots' is live for the duration of the closure.
    /// Reentrant calls are UB.  Phase A.2.1 setcar discipline applies.
    pub unsafe fn with_slots_mut<R>(&self, f: impl FnOnce(&mut Vec<Sexp>) -> R) -> R {
        let slots_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).slots);
        unsafe { f(&mut *slots_ptr) }
    }
}

/// Doc 111 §111.E — C-callable allocator for fresh `NlRecord' boxes
/// pre-filled with `slot_count' `Sexp::Nil' slots + a `Sexp::Nil'
/// type_tag (caller overwrites both before observing).
///
/// Mirrors `nl_alloc_vector' / `nl_alloc_consbox' / `nl_alloc_cell'
/// in shape.  Used by `mirror_install_entry' (= fresh `symbol-entry'
/// record) and `frame_push_rust_direct' (= fresh `nelisp-lexframe'
/// record).  The returned pointer is the only owner; caller wraps in
/// `Sexp::Record(_)' or invokes the standard drop path.
///
/// # Safety
/// Caller must wrap the returned pointer into a `Sexp::Record(_)'
/// whose `NlRecordRef::drop' decrements the refcount (standard
/// ownership transfer).
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_record(slot_count: i64) -> *mut NlRecord {
    let count = if slot_count < 0 { 0 } else { slot_count as usize };
    let layout = Layout::new::<NlRecord>();
    // SAFETY: `Layout::new::<NlRecord>()' is non-zero-sized.
    let raw = unsafe { alloc::alloc(layout) } as *mut NlRecord;
    if raw.is_null() {
        alloc::handle_alloc_error(layout);
    }
    // SAFETY: `raw' was just allocated and is exclusively owned.
    unsafe {
        std::ptr::write(std::ptr::addr_of_mut!((*raw).type_tag), Sexp::Nil);
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).slots),
            vec![Sexp::Nil; count],
        );
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).refcount),
            AtomicUsize::new(1),
        );
    }
    raw
}

/// Doc 111 §111.E — set the `type_tag' field of an `NlRecord' in place,
/// refcount-safely cloning `*val' before write.  Companion to
/// `nl_record_set_slot' (= writes slot N) used by callers that want
/// to install the type-tag symbol after `nl_alloc_record'.
///
/// # Safety
/// - `record' must be non-null and point at an initialized `NlRecord'.
/// - `val' must be non-null and point at an initialized `Sexp'.
#[no_mangle]
pub unsafe extern "C" fn nl_record_set_type_tag(
    record: *mut NlRecord,
    val: *const Sexp,
) {
    let r = unsafe { &mut *record };
    let new_val = unsafe { (*val).clone() };
    r.type_tag = new_val;
}

/// Doc 111 §111.B — set slot N of an NlRecord in place, dropping the
/// old value first (refcount-aware via Sexp's Drop impl).
///
/// # Safety
/// - `record` must be non-null and point at an initialized NlRecord.
/// - `val` must be non-null and point at an initialized Sexp.
/// - `n` must be a valid index into `record.slots`.
#[no_mangle]
pub unsafe extern "C" fn nl_record_set_slot(
    record: *mut NlRecord,
    n: usize,
    val: *const Sexp,
) {
    let r = unsafe { &mut *record };
    let new_val = unsafe { (*val).clone() };
    r.slots[n] = new_val;
}

impl Clone for NlRecordRef {
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlRecordRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlRecordRef {
    fn drop(&mut self) { unsafe { crate::nlrc_drop_box!(self.ptr.as_ptr(), NlRecord, crate::eval::sexp::SEXP_TAG_RECORD); } }
}

impl Deref for NlRecordRef {
    type Target = NlRecord;

    fn deref(&self) -> &NlRecord {
        // SAFETY: see `NlVectorRef::deref'.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlRecordRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Record")
            .field("type_tag", &self.type_tag)
            .field("slots", &self.slots)
            .finish()
    }
}

impl PartialEq for NlRecordRef {
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.type_tag == other.type_tag && self.slots == other.slots
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlRecord, type_tag) == 0);
    assert!(offset_of!(NlRecord, slots) == size_of::<Sexp>());
    assert!(
        offset_of!(NlRecord, refcount) == size_of::<Sexp>() + size_of::<Vec<Sexp>>()
    );
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(name: &str) -> Sexp {
        Sexp::Symbol(name.to_string())
    }

    #[test]
    fn layout_type_tag_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlRecord, type_tag), 0);
    }

    #[test]
    fn layout_slots_after_type_tag() {
        use std::mem::{offset_of, size_of};
        assert_eq!(offset_of!(NlRecord, slots), size_of::<Sexp>());
    }

    #[test]
    fn layout_refcount_after_slots() {
        use std::mem::{offset_of, size_of};
        assert_eq!(
            offset_of!(NlRecord, refcount),
            size_of::<Sexp>() + size_of::<Vec<Sexp>>()
        );
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let r = NlRecordRef::new(sym("foo"), vec![Sexp::Int(1)]);
        assert_eq!(NlRecordRef::strong_count(&r), 1);
    }

    #[test]
    fn new_returns_box_holding_value() {
        let r = NlRecordRef::new(sym("point"), vec![Sexp::Int(3), Sexp::Int(4)]);
        assert_eq!(r.type_tag, sym("point"));
        assert_eq!(r.slots.len(), 2);
        assert_eq!(r.slots[0], Sexp::Int(3));
        assert_eq!(r.slots[1], Sexp::Int(4));
    }

    #[test]
    fn clone_bumps_refcount_and_shares_value() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(7)]);
        let b = a.clone();
        assert_eq!(NlRecordRef::strong_count(&a), 2);
        assert_eq!(b.slots, vec![Sexp::Int(7)]);
        assert_eq!(b.type_tag, sym("x"));
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlRecordRef::new(sym("x"), vec![]);
        {
            let _b = a.clone();
            assert_eq!(NlRecordRef::strong_count(&a), 2);
        }
        assert_eq!(NlRecordRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Nil]);
        let b = a.clone();
        assert!(NlRecordRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_alloc() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
        let b = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
        assert!(!NlRecordRef::ptr_eq(&a, &b));
    }

    #[test]
    fn with_slots_mut_index_set() {
        let r = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(2)]);
        // SAFETY: no other borrow live.
        unsafe {
            r.with_slots_mut(|slots| {
                slots[1] = Sexp::Int(99);
            });
        }
        assert_eq!(r.slots[1], Sexp::Int(99));
    }

    #[test]
    fn with_slots_mut_visible_through_clone() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
        let b = a.clone();
        unsafe {
            a.with_slots_mut(|slots| slots[0] = Sexp::Int(7));
        }
        assert_eq!(a.slots, vec![Sexp::Int(7)]);
        assert_eq!(b.slots, vec![Sexp::Int(7)]);
    }

    #[test]
    fn with_slots_mut_returns_value_from_closure() {
        let r = NlRecordRef::new(sym("x"), vec![Sexp::Int(10)]);
        let len = unsafe { r.with_slots_mut(|s| s.len()) };
        assert_eq!(len, 1);
    }

    #[test]
    fn debug_format_uses_record_struct() {
        let r = NlRecordRef::new(sym("point"), vec![Sexp::Int(1)]);
        let d = format!("{:?}", r);
        assert!(
            d.starts_with("Record"),
            "expected `Record {{...}}' debug shape, got {:?}",
            d
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(2)]);
        let b = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(2)]);
        assert_eq!(a, b);
        let c = NlRecordRef::new(sym("y"), vec![Sexp::Int(1), Sexp::Int(2)]);
        assert_ne!(a, c);
        let d = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(3)]);
        assert_ne!(a, d);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Round-trip clone+drop without panic — ASAN / miri verify
        // no UB on the freed allocation.
        let r = NlRecordRef::new(sym("big"), vec![Sexp::Int(1); 100]);
        {
            let _t = r.clone();
        }
        drop(r);
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlRecordRef::new(sym("x"), vec![Sexp::Nil]);
        {
            let _b = a.clone();
        }
        let c = a.clone();
        assert!(NlRecordRef::ptr_eq(&a, &c));
    }

    #[test]
    fn empty_slots_round_trip() {
        let r = NlRecordRef::new(sym("empty"), vec![]);
        assert_eq!(r.slots.len(), 0);
        assert_eq!(NlRecordRef::strong_count(&r), 1);
    }

    #[test]
    fn type_tag_preserved_on_clone() {
        let a = NlRecordRef::new(sym("frob"), vec![Sexp::Int(1)]);
        let b = a.clone();
        assert_eq!(a.type_tag, sym("frob"));
        assert_eq!(b.type_tag, sym("frob"));
    }
}
