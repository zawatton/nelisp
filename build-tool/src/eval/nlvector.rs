//! `NlVector` — layout-pinned mutable Vec<Sexp> box.  `#[repr(C)]' with
//! value @ 0, refcount @ sizeof(Vec<Sexp>).  Backs `Sexp::Vector'.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlVector {
    pub value: Vec<Sexp>,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlVectorRef {
    ptr: NonNull<NlVector>,
    _marker: PhantomData<NlVector>,
}

impl NlVector {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlVector>;
}

impl NlVectorRef {
    /// Allocate a fresh [`NlVector`] on the heap with `refcount = 1'.
    pub fn new(value: Vec<Sexp>) -> NlVectorRef {
        let layout = Layout::new::<NlVector>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlVector;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' just allocated; exclusively owned.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlVectorRef { ptr, _marker: PhantomData }
    }

    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.
    ///
    /// # Safety
    /// No other `&Vec<Sexp>' borrow into `self.value' may be live.
    pub unsafe fn set_value(&self, val: Vec<Sexp>) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.
    ///
    /// # Safety
    /// Same as [`set_value`].  Reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut Vec<Sexp>) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

/// Doc 111 §111.E — C-callable allocator: fresh NlVector pre-filled with
/// `capacity` `Sexp::Nil' elements, refcount=1.
///
/// # Safety
/// Caller must wrap the pointer into a `Sexp::Vector(_)' whose `Drop'
/// decrements the refcount, or call into the standard Rust drop path.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_vector(capacity: i64) -> *mut NlVector {
    let cap = if capacity < 0 { 0 } else { capacity as usize };
    let layout = Layout::new::<NlVector>();
    let raw = unsafe { alloc::alloc(layout) } as *mut NlVector;
    if raw.is_null() {
        alloc::handle_alloc_error(layout);
    }
    // SAFETY: `raw' just allocated; exclusively owned.
    unsafe {
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).value),
            vec![Sexp::Nil; cap],
        );
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).refcount),
            AtomicUsize::new(1),
        );
    }
    raw
}

/// Doc 111 §111.E — set element N of an NlVector in place (refcount-safe).
///
/// # Safety
/// - `vec_ptr' must point at a live `NlVector'.
/// - `val' must point at an initialized `Sexp'.
/// - `n' must be a valid index into `vec.value'.
/// - No other `&Vec<Sexp>' borrow into `vec.value' may be live.
#[no_mangle]
pub unsafe extern "C" fn nl_vector_set_slot(
    vec_ptr: *mut NlVector,
    n: i64,
    val: *const Sexp,
) {
    let v = unsafe { &mut *vec_ptr };
    let new_val = unsafe { (*val).clone() };
    v.value[n as usize] = new_val;
}

impl Clone for NlVectorRef {
    /// Doc 124 §124.F — refcount +1 via elisp `nelisp_nlvector_clone'.
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlvector_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlVectorRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlVectorRef {
    /// Doc 124 §124.L — elisp `nlvector_drop' kernel.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlvector_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlVectorRef {
    type Target = NlVector;

    fn deref(&self) -> &NlVector {
        // SAFETY: handle keeps the box alive.
        unsafe { &*self.ptr.as_ptr() }
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

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlVector, value) == 0);
    assert!(offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>());
    assert!(size_of::<AtomicUsize>() == 8);
};
