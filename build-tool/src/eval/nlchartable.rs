//! `NlCharTable` backs `Sexp::CharTable`: `inner` at 0, refcount trailer.

use crate::eval::sexp::CharTableInner;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlCharTable {
    pub inner: CharTableInner,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(
    NlCharTableRef,
    NlCharTable,
    drop_fn = crate::elisp_cc_spike::nlchartable_drop
);

impl NlCharTable {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlCharTable>;

    // Safety: no live borrow into `self.inner` (see nlinner_with_mut! contract).
    crate::nlinner_with_mut!(with_inner_mut, inner, CharTableInner);
}

impl NlCharTableRef {
    pub fn new(inner: CharTableInner) -> NlCharTableRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlCharTable {
            inner,
            refcount: AtomicUsize::new(1),
        })));
        NlCharTableRef {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl Clone for NlCharTableRef {
    fn clone(&self) -> Self {
        unsafe {
            (*self.ptr.as_ptr())
                .refcount
                .fetch_add(1, Ordering::Relaxed);
        }
        NlCharTableRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
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
