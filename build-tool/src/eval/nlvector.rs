//! `NlVector` backs `Sexp::Vector`: `value` at 0, `refcount` in the trailer.

use crate::eval::sexp::Sexp;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;

#[repr(C)]
pub struct NlVector {
    pub value: Vec<Sexp>,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(
    NlVectorRef,
    NlVector,
    drop_fn = crate::elisp_cc_spike::nlvector_drop
);

impl NlVector {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlVector>;

    // Safety: no live borrow into `self.value` (see nlinner_set!/nlinner_with_mut! contracts).
    crate::nlinner_set!(set_value, value, Vec<Sexp>);
    crate::nlinner_with_mut!(with_value_mut, value, Vec<Sexp>);
}

impl NlVectorRef {
    pub fn new(value: Vec<Sexp>) -> NlVectorRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlVector {
            value,
            refcount: AtomicUsize::new(1),
        })));
        NlVectorRef { ptr, _marker: PhantomData }
    }
}

/// # Safety
/// Caller transfers the returned strong ref into a matching drop path.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_vector(capacity: i64) -> *mut NlVector {
    let cap = if capacity < 0 { 0 } else { capacity as usize };
    Box::into_raw(Box::new(NlVector {
        value: vec![Sexp::Nil; cap],
        refcount: AtomicUsize::new(1),
    }))
}

/// # Safety
/// `vec_ptr` must be live, `val` initialised, and `n` in range.
#[no_mangle]
pub unsafe extern "C" fn nl_vector_set_slot(
    vec_ptr: *mut NlVector,
    n: i64,
    val: *const Sexp,
) {
    (&mut (*vec_ptr).value)[n as usize] = (*val).clone();
}

impl Clone for NlVectorRef {
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlvector_clone(self.ptr.as_ptr() as *mut i64) };
        NlVectorRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl std::fmt::Debug for NlVectorRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Vector").field(&self.value).finish()
    }
}

impl PartialEq for NlVectorRef {
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.value == other.value
    }
}

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlVector, value) == 0);
    assert!(offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>());
    assert!(size_of::<AtomicUsize>() == 8);
};
