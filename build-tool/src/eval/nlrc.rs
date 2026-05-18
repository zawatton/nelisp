//! `NlRc<T>` — self-managed refcounted ptr.  Layout-pinned
//! `#[repr(C)]' inner: refcount @ offset 0 (AtomicUsize), value @ offset 8.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlRcInner<T> {
    pub refcount: AtomicUsize,
    pub value: T,
}

pub struct NlRc<T> {
    ptr: NonNull<NlRcInner<T>>,
    _marker: PhantomData<NlRcInner<T>>,
}

impl<T> NlRc<T> {
    /// Allocate a fresh `NlRcInner<T>` with `refcount = 1`.
    pub fn new(value: T) -> NlRc<T> {
        let layout = Layout::new::<NlRcInner<T>>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlRcInner<T>;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr` just allocated; exclusively owned.
        unsafe {
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
        }
        NlRc { ptr, _marker: PhantomData }
    }

    /// Read the current strong-reference count (Acquire ordering).
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr` is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Internal raw `*const NlRcInner<T>' for future ffi shims.
    #[inline]
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn inner_raw(this: &Self) -> *const NlRcInner<T> {
        this.ptr.as_ptr()
    }
}

impl<T> Clone for NlRc<T> {
    /// Bump the refcount (`Relaxed`, matching `Arc::clone`).
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr` is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlRc { ptr: self.ptr, _marker: PhantomData }
    }
}

impl<T> Drop for NlRc<T> {
    /// Decrement refcount.  At 0, drop the payload + free the alloc.
    /// `Release` decrement + `Acquire` fence — textbook `Arc` teardown.
    fn drop(&mut self) {
        // SAFETY: `self.ptr` is alive because we hold a handle.
        let prev = unsafe {
            (*self.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0; drop payload + free box.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value));
            let layout = Layout::new::<NlRcInner<T>>();
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl<T> Deref for NlRc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        // SAFETY: handle keeps the box alive; pinned offset 8.
        unsafe { &(*self.ptr.as_ptr()).value }
    }
}

// Doc 124 §124.L / §124.L+ — per-type inner-drop ABI externs for the elisp
// Drop kernels.  Each `nl_<type>_drop_inner' wraps `nlrc_payload_drop::<T>'
// (= `drop_in_place::<T>') so the elisp kernel can call it via `extern-call'
// before the matching `dealloc-bytes'.

/// Generic in-place drop helper — runs `T`'s destructor without freeing.
///
/// # Safety
/// `ptr` must point at a fully-initialized `T` whose backing alloc the
/// caller is about to free.
pub unsafe fn nlrc_payload_drop<T>(ptr: *mut std::ffi::c_void) {
    std::ptr::drop_in_place(ptr as *mut T);
}

/// §124.L NlConsBox inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlConsBox'.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlconsbox::NlConsBox::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlVector inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlVector'.
#[no_mangle]
pub unsafe extern "C" fn nl_vector_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlvector::NlVector::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlCell inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlCell'.
#[no_mangle]
pub unsafe extern "C" fn nl_cell_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlcell::NlCell::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlRecord inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlRecord'.
#[no_mangle]
pub unsafe extern "C" fn nl_record_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlrecord::NlRecord::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlStr inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlStr'.
#[no_mangle]
pub unsafe extern "C" fn nl_str_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlstr::NlStr::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L+ NlBoolVector inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlBoolVector'.
#[no_mangle]
pub unsafe extern "C" fn nl_boolvector_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlboolvector::NlBoolVector::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L+ NlCharTable inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlCharTable'.
#[no_mangle]
pub unsafe extern "C" fn nl_chartable_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlchartable::NlCharTable::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

// Compile-time layout assertions — refcount @ 0, value @ 8 for i64 canary.
const _: () = {
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    assert!(std::mem::offset_of!(NlRcInner<i64>, refcount) == 0);
    assert!(std::mem::offset_of!(NlRcInner<i64>, value) == 8);
};
