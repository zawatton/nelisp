//! Phase-47-elisp-compiled BoolVector sub-arm helpers (§120.D swap).
//!
//! `nl_jit_access_aref_bool_vector_inner' is provided by the static
//! archive `libnelisp_elisp_spike.a' (= compiled from
//! `lisp/nelisp-cc-jit-access-aref-bool-vector-inner.el' by
//! `scripts/compile-elisp-objects.el').  Rust body deleted.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_ERR: i64 = 1;
const TRAMPOLINE_OK: i64 = 0;

/// BoolVector arm for `nelisp_jit_aset`.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_aset_bool_vector_inner(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let box_ptr = (*arg).bool_vector_box_ptr()
        as *mut crate::eval::nlboolvector::NlBoolVector;
    let len = (&*box_ptr).value.len();
    if (idx as usize) >= len {
        return TRAMPOLINE_ERR;
    }
    let bit = crate::eval::special_forms::is_truthy(&*val);
    let value_ref = &mut (*box_ptr).value;
    value_ref[idx as usize] = bit;
    *out = (*val).clone();
    TRAMPOLINE_OK
}
