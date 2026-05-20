//! `NlBoolVector` backs `Sexp::BoolVector`: `value` at 0, refcount trailer.

use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;

#[repr(C)]
pub struct NlBoolVector {
    pub value: Vec<bool>,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(
    NlBoolVectorRef,
    NlBoolVector,
    drop_fn = crate::elisp_cc_spike::nlboolvector_drop
);

impl NlBoolVector {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlBoolVector>;

    // Safety: no live borrow into `self.value` (see nlinner_set!/nlinner_with_mut! contracts).
    crate::nlinner_set!(set_value, value, Vec<bool>);
    crate::nlinner_with_mut!(with_value_mut, value, Vec<bool>);
}

impl NlBoolVectorRef {
    pub fn new(value: Vec<bool>) -> NlBoolVectorRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlBoolVector {
            value,
            refcount: AtomicUsize::new(1),
        })));
        NlBoolVectorRef {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl Clone for NlBoolVectorRef {
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlboolvector_clone(self.ptr.as_ptr() as *mut i64) };
        NlBoolVectorRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlBoolVector, value) == 0);
    assert!(offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>());
    assert!(size_of::<AtomicUsize>() == 8);
};
