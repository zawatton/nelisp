//! `NlStr` — layout-pinned mutable String box.  `#[repr(C)]` with
//! value @ 0, refcount @ size_of::<String>().  Backs `Sexp::MutStr`.
//!
//! Clone/Drop bodies live in `lisp/nelisp-cc-nlstr-{clone,drop}.el` and
//! dispatch through `crate::elisp_cc_spike::nlstr_{clone,drop}`.
//!
//! All `nl_*` `#[no_mangle] extern "C"` ops (allocators, UTF-8 helpers,
//! mut-str builder, float ops) live in the sibling [`super::nlstr_externs`]
//! module; this file is the layout + handle type only.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlStr {
    pub value: String,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlStrRef {
    ptr: NonNull<NlStr>,
    _marker: PhantomData<NlStr>,
}

impl NlStr {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlStr>;
}

impl NlStrRef {
    /// Allocate a fresh [`NlStr`] on the heap with `refcount = 1`.
    pub fn new(value: String) -> NlStrRef {
        let layout = Layout::new::<NlStr>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlStr;
        let ptr = NonNull::new(raw).unwrap_or_else(|| alloc::handle_alloc_error(layout));
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlStrRef { ptr, _marker: PhantomData }
    }

    pub fn strong_count(this: &Self) -> usize {
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.
    ///
    /// # Safety
    /// No other `&String` borrow into `self.value` may be live.
    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.
    ///
    /// # Safety
    /// Same as [`set_value`]; reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlStrRef {
    fn clone(&self) -> Self {
        // Doc 124 §124.F — refcount +1 via elisp `nelisp_nlstr_clone`.
        unsafe { crate::elisp_cc_spike::nlstr_clone(self.ptr.as_ptr() as *mut i64) };
        NlStrRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlStrRef {
    fn drop(&mut self) {
        // Doc 124 §124.L — refcount-- + dealloc via elisp `nlstr_drop`.
        unsafe { crate::elisp_cc_spike::nlstr_drop(self.ptr.as_ptr() as *mut i64) };
    }
}

impl Deref for NlStrRef {
    type Target = NlStr;
    fn deref(&self) -> &NlStr {
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlStrRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("MutStr").field(&self.value).finish()
    }
}

impl PartialEq for NlStrRef {
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || self.value == other.value
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlStr, value) == 0);
    assert!(offset_of!(NlStr, refcount) == size_of::<String>());
    assert!(size_of::<AtomicUsize>() == 8);
};
