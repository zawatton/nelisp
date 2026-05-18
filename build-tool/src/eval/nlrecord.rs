//! `NlRecord` — layout-pinned record cell.  `#[repr(C)]' with
//! type_tag @ 0, slots @ sizeof(Sexp), refcount @ trailer.  Backs
//! `Sexp::Record'.

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

impl NlRecord {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlRecord>;
}

impl NlRecordRef {
    /// Allocate a fresh [`NlRecord`] on the heap with `refcount = 1'.
    pub fn new(type_tag: Sexp, slots: Vec<Sexp>) -> NlRecordRef {
        let layout = Layout::new::<NlRecord>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlRecord;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' just allocated; exclusively owned.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).type_tag), type_tag);
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).slots), slots);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlRecordRef { ptr, _marker: PhantomData }
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
    /// No other `&Vec<Sexp>' borrow into `self.slots' may be live.  Reentrant
    /// calls are UB.
    pub unsafe fn with_slots_mut<R>(&self, f: impl FnOnce(&mut Vec<Sexp>) -> R) -> R {
        let slots_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).slots);
        unsafe { f(&mut *slots_ptr) }
    }
}

/// Doc 111 §111.E — set the `type_tag' field of an `NlRecord' in place.
///
/// # Safety
/// - `record' must point at an initialized `NlRecord'.
/// - `val' must point at an initialized `Sexp'.
#[no_mangle]
pub unsafe extern "C" fn nl_record_set_type_tag(
    record: *mut NlRecord,
    val: *const Sexp,
) {
    let r = unsafe { &mut *record };
    let new_val = unsafe { (*val).clone() };
    r.type_tag = new_val;
}

/// Doc 111 §111.B — set slot N of an NlRecord in place.
///
/// # Safety
/// - `record` must point at an initialized NlRecord.
/// - `val` must point at an initialized Sexp.
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

/// Doc 111 §111.E — allocator: fresh NlRecord with `type_tag = (*type_tag_ptr).clone()`,
/// `slot_count` slots all `Sexp::Nil`, refcount=1.  Caller owns the strong ref;
/// must wrap into `Sexp::Record(NlRecordRef)`.
///
/// # Safety
/// - `type_tag_ptr' must point at an initialized `Sexp'.
/// - `slot_count' must not be so large that `vec![Nil; slot_count]' overflows.
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
    /// Doc 124 §124.F — refcount +1 via elisp `nelisp_nlrecord_clone'.
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlrecord_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlRecordRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlRecordRef {
    /// Doc 124 §124.L — elisp `nlrecord_drop' kernel.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlrecord_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlRecordRef {
    type Target = NlRecord;

    fn deref(&self) -> &NlRecord {
        // SAFETY: handle keeps the box alive.
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
