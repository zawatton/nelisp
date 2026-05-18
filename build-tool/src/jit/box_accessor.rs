//! Box accessor trampolines reached via `nl-jit-call-out-*' bridge prims.
//! Record-family (type/len/ref/set/alloc) lives in Phase 47 elisp; this
//! file keeps only char-table arms not yet elisp-able.
//! `nl_jit_bool_vector_len' and `nl_jit_str_codepoint_at' live in
//! `lisp/nelisp-cc-jit-bool-vector-len.el' and
//! `lisp/nelisp-cc-jit-str-codepoint-at.el' (Phase 47 compiled .o).

use crate::eval::sexp::Sexp;
const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Returns `*const Sexp' to record's inline `type_tag', or null on non-Record.
/// # Safety: `arg' non-null + initialized Sexp.
#[no_mangle]
pub unsafe extern "C" fn nl_record_type_tag_ptr(arg: *const Sexp) -> *const Sexp {
    match &*arg {
        Sexp::Record(rec) => &rec.type_tag as *const Sexp,
        _ => std::ptr::null(),
    }
}

/// In-place MutStr codepoint mutation; writes `*out = CP'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_mut_str_set_codepoint(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    if idx < 0 { return TRAMPOLINE_ERR; }
    let rc = match &*arg { Sexp::MutStr(r) => r, _ => return TRAMPOLINE_ERR };
    let cp = match &*val { Sexp::Int(n) => *n, _ => return TRAMPOLINE_ERR };
    let new_ch = match char::from_u32(cp as u32) {
        Some(c) => c, None => return TRAMPOLINE_ERR,
    };
    let chars: Vec<char> = rc.value.chars().collect();
    if (idx as usize) >= chars.len() { return TRAMPOLINE_ERR; }
    let new_str: String = chars.iter().enumerate()
        .map(|(i, c)| if i == idx as usize { new_ch } else { *c }).collect();
    rc.set_value(new_str);
    *out = (*val).clone();
    TRAMPOLINE_OK
}

#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aref(
    arg: *const Sexp, idx: i64, out: *mut Sexp,
) -> i64 {
    match &*arg {
        Sexp::CharTable(r) => {
            *out = crate::eval::builtins::char_table_get(r, idx); TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aset(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    match &*arg {
        Sexp::CharTable(r) => {
            r.with_inner_mut(|i|
                crate::eval::builtins::char_table_set_one(i, idx, (*val).clone()));
            *out = (*val).clone(); TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}
