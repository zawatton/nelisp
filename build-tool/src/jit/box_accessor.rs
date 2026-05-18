//! Box accessor trampolines reached via `nl-jit-call-out-{1,1i,2i}'
//! bridge primitives.  5 record-family trampolines (type / len / ref /
//! set / alloc) run in Phase 47-compiled elisp on linux-x86_64.
//!
//! Skipped (grammar gaps):
//!   - str_codepoint_at / mut_str_set_codepoint — need UTF-8 decode +
//!     in-place write ops in the trampoline body.
//!   - bool_vector_len — needs `(bool-vector-len H)' grammar op.
//!   - char_table_aref / _aset — parent-chain walk not yet expressible.
use crate::eval::sexp::Sexp;
const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Helper for the Phase-47-compiled `nl_jit_record_type' swap.
/// Returns `*const Sexp' to the record's inline `type_tag' field, or
/// null when `arg' is not `Sexp::Record'.  Composes with
/// `nl_sexp_clone_into' for the refcount-aware copy into the out-slot,
/// same way `record-slot-ref'
/// composes through `--emit-record-slot-ptr-core' + `nl_sexp_clone_into'.
///
/// # Safety
/// - `arg' must be non-null and point at an initialized `Sexp'.
#[no_mangle]
pub unsafe extern "C" fn nl_record_type_tag_ptr(arg: *const Sexp) -> *const Sexp {
    match &*arg {
        Sexp::Record(rec) => {
            // NlRecord's `type_tag' lives at offset 0 inside the
            // allocation (= `nelisp-nlrecord--offset-type-tag').
            // `NlRecordRef' Derefs to `NlRecord' so `&rec.type_tag'
            // resolves to the inline field address; cast to a raw
            // pointer keeps the lifetime relationship local (= caller
            // is responsible for not outliving `*arg').
            &rec.type_tag as *const Sexp
        }
        _ => std::ptr::null(),
    }
}

/// BoolVector length.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_bool_vector_len(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::BoolVector(v) => { *out = Sexp::Int(v.value.len() as i64); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// Char-indexed codepoint for `Str' / `MutStr'; ERR on OOR or wrong tag.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_str_codepoint_at(
    arg: *const Sexp, idx: i64, out: *mut Sexp,
) -> i64 {
    if idx < 0 { return TRAMPOLINE_ERR; }
    let s: &str = match &*arg {
        Sexp::Str(s) => s.as_str(),
        Sexp::MutStr(rc) => &rc.value,
        _ => return TRAMPOLINE_ERR,
    };
    match s.chars().nth(idx as usize) {
        Some(c) => { *out = Sexp::Int(c as i64); TRAMPOLINE_OK }
        None => TRAMPOLINE_ERR,
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
    // SAFETY: Phase A.4.2 — `new_str' is locally owned; the `chars()'
    // borrow ended at `collect' before `set_value' replaces in-place.
    rc.set_value(new_str);
    *out = (*val).clone();
    TRAMPOLINE_OK
}

/// CharTable read via `char_table_get' (parent-chain walk).
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

/// In-place CharTable set; writes `*out = V'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aset(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    match &*arg {
        Sexp::CharTable(r) => {
            // SAFETY: Phase A.4.6 — `with_inner_mut' closure owns the
            // only live `&mut CharTableInner' borrow; `(*val).clone()'
            // completes before the closure observes any aliasing.
            r.with_inner_mut(|i|
                crate::eval::builtins::char_table_set_one(i, idx, (*val).clone()));
            *out = (*val).clone(); TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

// Doc 86 §86.1.c — `record_type' / `record_len' / `record_ref' /
// `record_set' Rust trampoline bodies deleted (-65 LOC).  Symbols now
// resolve to Phase 47-compiled elisp in `lisp/nelisp-cc-jit-record.el'
// via `bridge::box_accessor_link::nelisp_jit_record_*' on linux-x86_64
// (= the crate's only supported target per `lib.rs:30').

// Doc 120 §120.B — `nl_jit_record_alloc' now resolves to a
// Phase-47-compiled elisp body (`lisp/nelisp-cc-jit-record.el') on
// linux-x86_64 via `bridge.rs` archive anchors.
