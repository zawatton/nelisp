//! `NlBoolVector` backs `Sexp::BoolVector`: `value` at 0, refcount trailer.

use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlBoolVector {
    pub value: Vec<bool>,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlBoolVectorRef {
    ptr: NonNull<NlBoolVector>,
    _marker: PhantomData<NlBoolVector>,
}

impl NlBoolVector {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlBoolVector>;

    /// # Safety
    /// No other `&Vec<bool>` borrow into `self.value` may be live.
    pub unsafe fn set_value(&self, val: Vec<bool>) {
        let value_ptr = std::ptr::addr_of!(self.value) as *mut Vec<bool>;
        std::ptr::drop_in_place(value_ptr);
        std::ptr::write(value_ptr, val);
    }

    /// # Safety
    /// Same as [`NlBoolVector::set_value`]; reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut Vec<bool>) -> R) -> R {
        let value_ptr = std::ptr::addr_of!(self.value) as *mut Vec<bool>;
        f(&mut *value_ptr)
    }
}

impl NlBoolVectorRef {
    pub fn new(value: Vec<bool>) -> NlBoolVectorRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlBoolVector {
            value,
            refcount: AtomicUsize::new(1),
        })));
        NlBoolVectorRef { ptr, _marker: PhantomData }
    }

    pub fn strong_count(this: &Self) -> usize {
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

}

impl Clone for NlBoolVectorRef {
    fn clone(&self) -> Self {
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlBoolVectorRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlBoolVectorRef {
    fn drop(&mut self) {
        unsafe { crate::elisp_cc_spike::nlboolvector_drop(self.ptr.as_ptr() as *mut i64) };
    }
}

impl Deref for NlBoolVectorRef {
    type Target = NlBoolVector;

    fn deref(&self) -> &NlBoolVector {
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlBoolVector, value) == 0);
    assert!(offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>());
    assert!(size_of::<AtomicUsize>() == 8);
};
