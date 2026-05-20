//! `NlCharTable` backs `Sexp::CharTable`: `inner` at 0, refcount trailer.

use crate::eval::sexp::{CharTableInner, Sexp};
use crate::jit::{TRAMPOLINE_ERR, TRAMPOLINE_OK};

crate::define_nlbox!(
    inner          = NlCharTable,
    ref_ty         = NlCharTableRef,
    fields         = { inner: CharTableInner },
    clone_fn       = crate::elisp_cc_spike::nlchartable_clone,
    drop_fn        = crate::elisp_cc_spike::nlchartable_drop,
    layout_asserts = {
        use ::std::mem::{offset_of, size_of};
        assert!(offset_of!(NlCharTable, inner) == 0);
        assert!(offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>());
        assert!(size_of::<AtomicUsize>() == 8);
    }
);

impl NlCharTable {
    // Safety: no live borrow into `self.inner` (see nlinner_with_mut! contract).
    crate::nlinner_with_mut!(with_inner_mut, inner, CharTableInner);
}

impl std::fmt::Debug for NlCharTableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CharTable").field("inner", &self.inner).finish()
    }
}

impl PartialEq for NlCharTableRef {
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || self.inner == other.inner
    }
}

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
