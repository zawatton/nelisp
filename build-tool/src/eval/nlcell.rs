//! `NlCell` — layout-pinned single-slot mutable cell.  `#[repr(C)]'
//! with value at offset 0 and refcount trailer at sizeof(Sexp).  Backs
//! the `Sexp::Cell' variant + `FrameCell' (closure-capture write-through).

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlCell {
    pub value: Sexp,
    pub refcount: AtomicUsize,
}

/// Refcounted handle.  `#[repr(transparent)]' matches `NonNull<NlCell>'.
#[repr(transparent)]
pub struct NlCellRef {
    ptr: NonNull<NlCell>,
    _marker: PhantomData<NlCell>,
}

impl NlCell {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlCell>;
}

/// Doc 111 §111.D — extern allocator returning fresh NlCell with
/// `value = (*initial).clone()` and refcount=1.
///
/// # Safety
/// `initial` must point at an initialized `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_cell(initial: *const Sexp) -> *mut NlCell {
    let value = unsafe { (*initial).clone() };
    let layout = Layout::new::<NlCell>();
    let raw = unsafe { alloc::alloc(layout) } as *mut NlCell;
    if raw.is_null() {
        alloc::handle_alloc_error(layout);
    }
    // SAFETY: `raw' just allocated; exclusively owned.
    unsafe {
        std::ptr::write(std::ptr::addr_of_mut!((*raw).value), value);
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).refcount),
            AtomicUsize::new(1),
        );
    }
    raw
}

/// Doc 111 §111.D — extern wrapper for `(cell-set-value H VAL)'.
///
/// # Safety
/// - `cell' must point at a live `NlCell'.
/// - `val' must point at an initialized `Sexp'.
/// - No other `&Sexp' borrow into `cell.value' may be live.
#[no_mangle]
pub unsafe extern "C" fn nl_cell_set_value(cell: *mut NlCell, val: *const Sexp) {
    let val_owned = unsafe { (*val).clone() };
    let value_ptr = std::ptr::addr_of_mut!((*cell).value);
    unsafe {
        std::ptr::drop_in_place(value_ptr);
        std::ptr::write(value_ptr, val_owned);
    }
}

impl NlCellRef {
    /// Allocate a fresh [`NlCell`] on the heap with `refcount = 1`.
    pub fn new(value: Sexp) -> NlCellRef {
        let layout = Layout::new::<NlCell>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlCell;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated; exclusively owned.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlCellRef { ptr, _marker: PhantomData }
    }

    /// Wrap an externally-allocated `*mut NlCell' into an owning `NlCellRef'.
    /// Used by integration tests around the extern `nl_alloc_cell' allocator.
    ///
    /// # Safety
    /// - `raw' must point at an initialized `NlCell' with `refcount >= 1'.
    /// - One strong reference is transferred into the returned `NlCellRef'.
    #[doc(hidden)]
    pub unsafe fn from_raw_ptr(raw: *mut NlCell) -> NlCellRef {
        NlCellRef {
            ptr: NonNull::new(raw).expect("from_raw_ptr: null pointer"),
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

    /// Mutate `value` in place (drop previous + write new).
    ///
    /// # Safety
    /// Caller must guarantee no other `&Sexp' borrow into `self.value' is live.
    pub unsafe fn set_value(&self, val: Sexp) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        // SAFETY: method-level contract.
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }
}

impl Clone for NlCellRef {
    /// Doc 124 §124.F — refcount +1 via elisp `nelisp_nlcell_clone'.
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlcell_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlCellRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlCellRef {
    /// Doc 124 §124.L — elisp `nlcell_drop' kernel handles refcount-- + dealloc.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlcell_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlCellRef {
    type Target = NlCell;

    fn deref(&self) -> &NlCell {
        // SAFETY: handle keeps the box alive; pinned field offsets.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlCellRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Cell").field(&self.value).finish()
    }
}

impl PartialEq for NlCellRef {
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
    assert!(offset_of!(NlCell, value) == 0);
    assert!(offset_of!(NlCell, refcount) == size_of::<Sexp>());
    assert!(size_of::<AtomicUsize>() == 8);
};
