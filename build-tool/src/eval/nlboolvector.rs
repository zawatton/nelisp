//! `NlBoolVector` — layout-pinned mutable Vec<bool> box.  `#[repr(C)]'
//! with value @ 0, refcount @ sizeof(Vec<bool>).  Backs `Sexp::BoolVector'.
//! `unsafe set_value' wholesale; `unsafe with_value_mut(f)' for aset.

use std::alloc::{self, Layout};
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

impl NlBoolVector { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlBoolVector>; } // Doc 79 v4 C.4-atomic
impl NlBoolVectorRef {
    /// Allocate a fresh [`NlBoolVector`] on the heap with `refcount = 1'.
    pub fn new(value: Vec<bool>) -> NlBoolVectorRef {
        let layout = Layout::new::<NlBoolVector>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlBoolVector;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlBoolVector' and is
        // exclusively owned here.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlBoolVectorRef {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.  Drops the previous Vec, then
    /// writes the new one.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&Vec<bool>` borrow into this
    /// box's `value` is live.  Phase A.2.1 setcar discipline applies.
    pub unsafe fn set_value(&self, val: Vec<bool>) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.
    ///
    /// # Safety
    ///
    /// Same as [`set_value`]: no other `&Vec<bool>` borrow into
    /// `self.value' may be live for the duration of the closure.
    /// Reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut Vec<bool>) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlBoolVectorRef {
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlBoolVectorRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlBoolVectorRef {
    /// Doc 124 §124.L+ — dispatch through the pure-elisp
    /// `nlboolvector_drop' kernel.  Runs `atomic-fetch-add(-1)' then,
    /// on pre-sub == 1, calls `nl_boolvector_drop_inner' (= `drop_in_place
    /// ::<NlBoolVector>') + `dealloc-bytes(32, 8)'.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlboolvector_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlBoolVectorRef {
    type Target = NlBoolVector;

    fn deref(&self) -> &NlBoolVector {
        // SAFETY: see `NlVectorRef::deref'.
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

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlBoolVector, value) == 0);
    assert!(offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>());
    assert!(size_of::<AtomicUsize>() == 8);
};

