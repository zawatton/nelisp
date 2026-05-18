//! `NlRecord` backs `Sexp::Record`: `type_tag` at 0, `slots` next, refcount last.

use crate::eval::sexp::Sexp;
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

impl NlRecord {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlRecord>;

    /// # Safety
    /// No other `&Vec<Sexp>` borrow into `self.slots` may be live.
    pub unsafe fn with_slots_mut<R>(&self, f: impl FnOnce(&mut Vec<Sexp>) -> R) -> R {
        let slots_ptr = std::ptr::addr_of!(self.slots) as *mut Vec<Sexp>;
        f(&mut *slots_ptr)
    }
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

    pub fn strong_count(this: &Self) -> usize {
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
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

impl Drop for NlRecordRef {
    fn drop(&mut self) {
        unsafe { crate::elisp_cc_spike::nlrecord_drop(self.ptr.as_ptr() as *mut i64) };
    }
}

impl Deref for NlRecordRef {
    type Target = NlRecord;

    fn deref(&self) -> &NlRecord {
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlRecord, type_tag) == 0);
    assert!(offset_of!(NlRecord, slots) == size_of::<Sexp>());
    assert!(
        offset_of!(NlRecord, refcount) == size_of::<Sexp>() + size_of::<Vec<Sexp>>()
    );
    assert!(size_of::<AtomicUsize>() == 8);
};
