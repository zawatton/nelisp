//! `NlRecord` backs `Sexp::Record`: `type_tag` at 0, `slots` next, refcount last.

use crate::eval::sexp::Sexp;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;

#[repr(C)]
pub struct NlRecord {
    pub type_tag: Sexp,
    pub slots: Vec<Sexp>,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(
    NlRecordRef,
    NlRecord,
    drop_fn = crate::elisp_cc_spike::nlrecord_drop
);

impl NlRecord {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlRecord>;

    // Safety: no live borrow into `self.slots` (see nlinner_with_mut! contract).
    crate::nlinner_with_mut!(with_slots_mut, slots, Vec<Sexp>);
}

impl NlRecordRef {
    pub fn new(type_tag: Sexp, slots: Vec<Sexp>) -> NlRecordRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlRecord {
            type_tag,
            slots,
            refcount: AtomicUsize::new(1),
        })));
        NlRecordRef { ptr, _marker: PhantomData }
    }
}

/// # Safety
/// `record` must be live, `val` initialised, and `n` in range.
#[no_mangle]
pub unsafe extern "C" fn nl_record_set_slot(
    record: *mut NlRecord,
    n: usize,
    val: *const Sexp,
) {
    (&mut (*record).slots)[n] = (*val).clone();
}

/// # Safety
/// `type_tag_ptr` must point at an initialised `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_record(
    type_tag_ptr: *const Sexp,
    slot_count: i64,
) -> *mut NlRecord {
    let n = slot_count as usize;
    let tag = unsafe { (*type_tag_ptr).clone() };
    Box::into_raw(Box::new(NlRecord {
        type_tag: tag,
        slots: vec![Sexp::Nil; n],
        refcount: AtomicUsize::new(1),
    }))
}

impl Clone for NlRecordRef {
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlrecord_clone(self.ptr.as_ptr() as *mut i64) };
        NlRecordRef { ptr: self.ptr, _marker: PhantomData }
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlRecord, type_tag) == 0);
    assert!(offset_of!(NlRecord, slots) == size_of::<Sexp>());
    assert!(
        offset_of!(NlRecord, refcount) == size_of::<Sexp>() + size_of::<Vec<Sexp>>()
    );
    assert!(size_of::<AtomicUsize>() == 8);
};
