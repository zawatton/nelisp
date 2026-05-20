//! `NlCharTable` backs `Sexp::CharTable`: `inner` at 0, refcount trailer.

use crate::eval::sexp::CharTableInner;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;

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
        unsafe { crate::elisp_cc_spike::nlchartable_clone(self.ptr.as_ptr() as *mut i64) };
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

use crate::eval::sexp::Sexp;
use crate::jit::{TRAMPOLINE_ERR, TRAMPOLINE_OK};

/// Phase 47 delegate: char_table_get lookup; caller guarantees CharTable tag.
#[no_mangle]
pub unsafe extern "C" fn nl_char_table_get_raw(
    arg: *const Sexp,
    idx: i64,
    out: *mut Sexp,
) -> i64 {
    let r = match &*arg {
        Sexp::CharTable(r) => r,
        _ => return TRAMPOLINE_ERR,
    };
    *out = crate::eval::builtins::char_table_get(r, idx);
    TRAMPOLINE_OK
}

/// Phase 47 delegate: char_table_set_one mutation; caller guarantees CharTable tag.
#[no_mangle]
pub unsafe extern "C" fn nl_char_table_set_raw(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let r = match &*arg {
        Sexp::CharTable(r) => r,
        _ => return TRAMPOLINE_ERR,
    };
    r.with_inner_mut(|i| crate::eval::builtins::char_table_set_one(i, idx, (*val).clone()));
    *out = (*val).clone();
    TRAMPOLINE_OK
}
