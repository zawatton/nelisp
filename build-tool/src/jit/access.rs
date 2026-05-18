//! Narrow BoolVector helpers used by Phase 47 access trampolines.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// BoolVector arm for `nelisp_jit_aref`.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_aref_bool_vector_inner(
    arg: *const Sexp,
    idx: i64,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let box_ref = &*(*arg).bool_vector_box_ptr();
    if let Some(b) = box_ref.value.get(idx as usize) {
        *out = if *b { Sexp::T } else { Sexp::Nil };
        return TRAMPOLINE_OK;
    }
    TRAMPOLINE_ERR
}

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
