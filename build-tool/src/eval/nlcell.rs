//! `NlCell` backs `Sexp::Cell`: `value` at 0, refcount in the trailer.

use crate::eval::sexp::Sexp;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;

#[repr(C)]
pub struct NlCell {
    pub value: Sexp,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(
    NlCellRef,
    NlCell,
    drop_fn = crate::elisp_cc_spike::nlcell_drop
);

impl NlCell {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlCell>;

    // Safety: no live `&Sexp` borrow into `self.value` (see nlinner_set! contract).
    crate::nlinner_set!(set_value, value, Sexp);
}

impl NlCellRef {
    pub fn new(value: Sexp) -> NlCellRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlCell {
            value,
            refcount: AtomicUsize::new(1),
        })));
        NlCellRef {
            ptr,
            _marker: PhantomData,
        }
    }

    /// # Safety
    /// `raw` must be non-null, live, and transfer one strong ref.
    #[doc(hidden)]
    pub unsafe fn from_raw_ptr(raw: *mut NlCell) -> NlCellRef {
        NlCellRef {
            ptr: NonNull::new(raw).expect("from_raw_ptr: null pointer"),
            _marker: PhantomData,
        }
    }
}

impl Clone for NlCellRef {
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlcell_clone(self.ptr.as_ptr() as *mut i64) };
        NlCellRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlCell, value) == 0);
    assert!(offset_of!(NlCell, refcount) == size_of::<Sexp>());
    assert!(size_of::<AtomicUsize>() == 8);
};

/// # Safety
/// `initial` must point at an initialised `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_cell(initial: *const Sexp) -> *mut NlCell {
    Box::into_raw(Box::new(NlCell {
        value: (*initial).clone(),
        refcount: AtomicUsize::new(1),
    }))
}

/// # Safety
/// `cell` must be live and `val` initialised.
#[no_mangle]
pub unsafe extern "C" fn nl_cell_set_value(cell: *mut NlCell, val: *const Sexp) {
    (*cell).set_value((*val).clone());
}
