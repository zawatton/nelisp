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

// Per-type inner-drop ABI externs — each `nl_<type>_drop_inner' wraps
// `drop_in_place::<T>' so elisp Drop kernels can call it via extern-call
// before `dealloc-bytes'.  Safety: `box_ptr' must point at an initialized
// box of the named type.

pub unsafe fn nlrc_payload_drop<T>(ptr: *mut std::ffi::c_void) {
    std::ptr::drop_in_place(ptr as *mut T);
}

macro_rules! drop_inner_extern {
    ($name:ident, $ty:path) => {
        #[no_mangle]
        pub unsafe extern "C" fn $name(box_ptr: *mut i64) -> i64 {
            <$ty>::DROP_FN(box_ptr as *mut std::ffi::c_void);
            1
        }
    };
}

drop_inner_extern!(nl_consbox_drop_inner,    crate::eval::nlconsbox::NlConsBox);
drop_inner_extern!(nl_vector_drop_inner,     crate::eval::nlvector::NlVector);
drop_inner_extern!(nl_cell_drop_inner,       crate::eval::nlcell::NlCell);
drop_inner_extern!(nl_record_drop_inner,     crate::eval::nlrecord::NlRecord);
drop_inner_extern!(nl_str_drop_inner,        crate::eval::nlstr::NlStr);
drop_inner_extern!(nl_boolvector_drop_inner, crate::eval::nlboolvector::NlBoolVector);
drop_inner_extern!(nl_chartable_drop_inner,  crate::eval::nlchartable::NlCharTable);

// Compile-time layout assertions — refcount @ 0, value @ 8 for i64 canary.
const _: () = {
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    assert!(std::mem::offset_of!(NlRcInner<i64>, refcount) == 0);
    assert!(std::mem::offset_of!(NlRcInner<i64>, value) == 8);
};
