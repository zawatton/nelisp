//! Access trampolines.  `length' / `aref' / `aset' / `elt' bodies live
//! in `lisp/nelisp-cc-jit-{length,aref,aset,elt}.el' (Phase 47-compiled
//! `.o' in `libnelisp_elisp_spike.a'); `bridge::access_link' resolves
//! the externs on linux-x86_64 (= the crate's only target per
//! `lib.rs:30').
//!
//! The 3 narrow Rust externs below stay — they shrink the elisp body
//! surface area to the bit decode + tag-byte writes when Phase 47 has
//! no grammar primitive yet (`(str-char-count)' / `bool-vector-*').
//! Reached from the elisp bodies via `(extern-call SYM ARG...)' — same
//! shape `nl_sexp_eq' uses for the §120.A predicate-eq slow path.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// `(length STR)' narrow `Sexp::Str' arm — reached from the
/// Phase 47 `nelisp_jit_length' body's Str tag arm.  Returns
/// codepoint count via `s.chars().count()' — the same Unicode-
/// aware char-walker the pre-§120.D trampoline used, kept in
/// Rust until Phase 47 grows a `(str-char-count H)' grammar op
/// (= Doc 122 §122.A `mut-str-char-count' cluster).
///
/// # Safety
/// - `arg' must point at `Sexp::Str(_)' — elisp tag-checks.
/// - `out' must be non-null + writable for one 32-byte Sexp slot.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_length_str_inner(
    arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if let Sexp::Str(s) = &*arg {
        *out = Sexp::Int(s.chars().count() as i64);
        TRAMPOLINE_OK
    } else {
        TRAMPOLINE_ERR
    }
}

/// `(aref BV INDEX)' narrow BoolVector arm — reached from the
/// Phase 47 `nelisp_jit_aref' body's BoolVector tag arm.
///
/// # Safety
/// - `arg' must point at `Sexp::BoolVector(_)' — the elisp body
///   tag-checks before calling.
/// - `out' must be non-null + writable for one 32-byte Sexp slot.
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

/// `(aset BV INDEX VALUE)' narrow BoolVector arm — reached from the
/// Phase 47 `nelisp_jit_aset' body's BoolVector tag arm.
///
/// # Safety
/// - `arg' must point at `Sexp::BoolVector(_)' — elisp tag-checks.
/// - `val' must point at an initialized `Sexp' for truthiness test.
/// - `out' must be non-null + writable for one 32-byte Sexp slot.
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
    // SAFETY: Phase A.4.4 — same discipline as the pre-§120.D
    // BoolVector arm above.
    let value_ref = &mut (*box_ptr).value;
    value_ref[idx as usize] = bit;
    *out = (*val).clone();
    TRAMPOLINE_OK
}
