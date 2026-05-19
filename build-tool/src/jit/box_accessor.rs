//! Box accessor trampolines reached via `nl-jit-call-out-*' bridge prims.
//! Record-family (type/len/ref/set/alloc) lives in Phase 47 elisp; this
//! file keeps only char-table arms not yet elisp-able.
//! `nl_jit_bool_vector_len', `nl_jit_str_codepoint_at', and
//! `nl_record_type_tag_ptr' live in Phase 47 compiled .o files.
//! `nl_jit_mut_str_set_codepoint' keeps its Rust body (requires
//! NlStrRef::set_value; no Phase 47 grammar op for in-place String replace).

use crate::eval::sexp::Sexp;
const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// In-place MutStr codepoint mutation; writes `*out = CP'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_mut_str_set_codepoint(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    if idx < 0 { return TRAMPOLINE_ERR; }
    let rc = match &*arg { Sexp::MutStr(r) => r, _ => return TRAMPOLINE_ERR };
    let cp = match &*val { Sexp::Int(n) => *n, _ => return TRAMPOLINE_ERR };
    let new_ch = match char::from_u32(cp as u32) { Some(c) => c, None => return TRAMPOLINE_ERR };
    let (i, len) = (idx as usize, rc.value.chars().count());
    if i >= len { return TRAMPOLINE_ERR; }
    let new_str: String = rc.value.chars().enumerate()
        .map(|(j, c)| if j == i { new_ch } else { c }).collect();
    rc.set_value(new_str);
    *out = (*val).clone();
    TRAMPOLINE_OK
}

#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aref(
    arg: *const Sexp, idx: i64, out: *mut Sexp,
) -> i64 {
    let r = match &*arg { Sexp::CharTable(r) => r, _ => return TRAMPOLINE_ERR };
    *out = crate::eval::builtins::char_table_get(r, idx);
    TRAMPOLINE_OK
}

#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aset(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    let r = match &*arg { Sexp::CharTable(r) => r, _ => return TRAMPOLINE_ERR };
    r.with_inner_mut(|i| crate::eval::builtins::char_table_set_one(i, idx, (*val).clone()));
    *out = (*val).clone();
    TRAMPOLINE_OK
}
