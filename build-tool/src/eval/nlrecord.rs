//! `NlRecord` — layout-pinned record cell.  `#[repr(C)]' with
//! type_tag @ 0, slots @ sizeof(Sexp), refcount @ trailer.  Backs
//! `Sexp::Record'.  `unsafe with_slots_mut(f)' for `record-set';
//! type_tag is immutable after construction.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlRecord {
    pub type_tag: Sexp,
    pub slots: Vec<Sexp>,
    pub refcount: AtomicUsize,
}

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

/// Doc 111 §111.E — allocator helper for Phase 47-compiled
/// `mirror_install_entry' (= helper #12) and friends.  Returns a
/// freshly-allocated [`NlRecord`] with `type_tag = (*type_tag_ptr).clone()',
/// `slot_count' slots all initialised to `Sexp::Nil', and
/// `refcount = 1'.  Caller is responsible for overwriting individual
/// slots via `record-slot-set' / `nl_record_set_slot' before
/// publishing the pointer (= same contract as `nl_alloc_cell' /
/// `nl_alloc_consbox').
///
/// The returned raw `*mut NlRecord' must eventually be wrapped into a
/// `Sexp::Record(NlRecordRef)' (which takes ownership of the strong
/// reference) — letting it leak raw counts as a single permanent
/// refcount.  See Doc 111 §2.4 for the broader allocator audit.
///
/// The two-arg shape (= type_tag passed as `*const Sexp' rather than
/// initialized to `Sexp::Nil' and overwritten by a separate op) was
/// chosen because Phase 47 currently has no grammar form to write the
/// record's type_tag field (= only the user-visible slots are
/// addressable via `record-slot-set').  Adding the tag as an
/// allocator-time parameter keeps the elisp surface minimal.
///
/// # Safety
/// Caller must guarantee:
/// - `type_tag_ptr' is non-null and points at an initialized `Sexp'.
/// - The returned pointer is eventually freed via `NlRecordRef::drop'
///   (= owned by a `Sexp::Record(NlRecordRef)`) or via `Box::from_raw'
///   if it's never wrapped.  Otherwise the allocation leaks.
/// - `slot_count' is not so large that
///   `vec![Sexp::Nil; slot_count]' overflows.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_record(
    type_tag_ptr: *const Sexp,
    slot_count: i64,
) -> *mut NlRecord {
    let n = slot_count as usize;
    let tag = unsafe { (*type_tag_ptr).clone() };
    let boxed = Box::new(NlRecord {
        type_tag: tag,
        slots: vec![Sexp::Nil; n],
        refcount: AtomicUsize::new(1),
    });
    Box::into_raw(boxed)
}

impl Clone for NlRecordRef {
    /// Doc 124 §124.F — refcount +1 dispatched to the Phase 47-compiled
    /// `nelisp_nlrecord_clone' kernel (= §122.E `atomic-fetch-add' delta=+1
    /// at REFCOUNT_OFFSET=24).
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlrecord_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlRecordRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlRecordRef {
    /// Doc 124 §124.L — dispatch through the pure-elisp `nlrecord_drop'
    /// kernel.  Runs `atomic-fetch-add(-1)' then, on pre-sub == 1,
    /// calls `nl_record_drop_inner' (= `drop_in_place::<NlRecord>') +
    /// `dealloc-bytes(64, 8)'.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlrecord_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
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
