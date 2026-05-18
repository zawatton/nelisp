//! `NlCharTable` backs `Sexp::CharTable`: `inner` at 0, refcount trailer.

use crate::eval::sexp::CharTableInner;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlCharTable {
    pub inner: CharTableInner,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlCharTableRef {
    ptr: NonNull<NlCharTable>,
    _marker: PhantomData<NlCharTable>,
}

impl NlCharTable {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlCharTable>;

    /// # Safety
    /// No other `&CharTableInner` borrow into `self.inner` may be live.
    pub unsafe fn with_inner_mut<R>(&self, f: impl FnOnce(&mut CharTableInner) -> R) -> R {
        let inner_ptr = std::ptr::addr_of!(self.inner) as *mut CharTableInner;
        f(&mut *inner_ptr)
    }
}

impl NlCharTableRef {
    pub fn new(inner: CharTableInner) -> NlCharTableRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlCharTable {
            inner,
            refcount: AtomicUsize::new(1),
        })));
        NlCharTableRef { ptr, _marker: PhantomData }
    }

    pub fn strong_count(this: &Self) -> usize {
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

}

impl Clone for NlCharTableRef {
    fn clone(&self) -> Self {
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlCharTableRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlCharTableRef {
    fn drop(&mut self) {
        unsafe { crate::elisp_cc_spike::nlchartable_drop(self.ptr.as_ptr() as *mut i64) };
    }
}

impl Deref for NlCharTableRef {
    type Target = NlCharTable;

    fn deref(&self) -> &NlCharTable {
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlCharTable, inner) == 0);
    assert!(offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>());
    assert!(size_of::<AtomicUsize>() == 8);
};
