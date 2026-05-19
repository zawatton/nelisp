//! String/symbol trampolines reached via `nl-jit-call-out-1' (1-arg)
//! or `nl-jit-call-out-2' (2-arg) from `nelisp-jit-strategy.el'.
//! Surviving Rust bodies: symbol-name.
//! All others migrated to Phase 47 elisp `.o' bodies.
//!
//! Migrated to elisp .o:
//!   `nl_jit_make_symbol'       → `lisp/nelisp-cc-jit-make-symbol.el'
//!   `nl_jit_downcase'          → `lisp/nelisp-cc-jit-downcase.el'
//!   `nl_jit_upcase'            → `lisp/nelisp-cc-jit-upcase.el'
//!   `nl_jit_concat_ints'       → `lisp/nelisp-cc-jit-concat-ints.el'
//!   `nl_jit_split_by_non_alnum' → `lisp/nelisp-cc-jit-split-by-non-alnum.el'
//!   `nl_jit_format_float'       → `lisp/nelisp-cc-jit-format-float.el'

use crate::eval::sexp::Sexp;
use std::sync::atomic::AtomicI64;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Per-process uninterned-symbol counter.  Pointer surfaced to the
/// Phase 47 elisp body via `nl_make_symbol_counter_ptr'.
static MAKE_SYMBOL_COUNTER: AtomicI64 = AtomicI64::new(0);

/// Return `*mut i64' to `MAKE_SYMBOL_COUNTER' for use with the
/// Phase 47 `atomic-fetch-add' grammar op in the elisp body of
/// `nl_jit_make_symbol'.
#[no_mangle]
pub extern "C" fn nl_make_symbol_counter_ptr() -> *mut i64 {
    std::ptr::addr_of!(MAKE_SYMBOL_COUNTER) as *mut i64
}

fn read_text(v: &Sexp) -> Option<String> {
    match v {
        Sexp::Str(s) => Some(s.clone()),
        Sexp::MutStr(rc) => Some(rc.value.clone()),
        Sexp::Symbol(s) => Some(s.clone()),
        Sexp::Nil => Some("nil".into()),
        Sexp::T => Some("t".into()),
        _ => None,
    }
}

/// Symbol(s) → Str(s); Nil → "nil"; T → "t"; else ERR.
/// Called from `nelisp-jit-strategy.el' `symbol-name' wrapper via
/// `(nl-jit-call-out-1 "nelisp_jit_symbol_name" sym)'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_symbol_name(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match read_text(&*arg) {
        Some(s) => { *out = Sexp::Str(s); TRAMPOLINE_OK }
        None => TRAMPOLINE_ERR,
    }
}

// Phase 47 elisp migration: `nl_jit_downcase' + `nl_jit_upcase' Rust bodies
// deleted — replaced by Phase-47-compiled elisp bodies in
// `lisp/nelisp-cc-jit-downcase.el' and `lisp/nelisp-cc-jit-upcase.el'.
// The `#[no_mangle]' symbols are now provided by the `.o' archive linked
// by `build.rs'.

// Phase 47 elisp migration: `nl_jit_split_by_non_alnum' Rust body deleted —
// replaced by Phase-47-compiled elisp body in
// `lisp/nelisp-cc-jit-split-by-non-alnum.el'.  The `#[no_mangle]' symbol
// is now provided by the `.o' archive linked by `build.rs'.
// `_ELISP_ARCHIVE_ANCHOR' count 54→55.

// Doc 86 §86.1.e.2 (2026-05-19): `nl_jit_concat_ints' Rust body
// deleted — replaced by Phase-47-compiled elisp body in
// `lisp/nelisp-cc-jit-concat-ints.el'.  The `#[no_mangle]' symbol is
// now provided by the `.o' archive linked by `build.rs'.

// Phase 47 elisp migration: `nl_jit_format_float' Rust body deleted —
// replaced by Phase-47-compiled elisp body in
// `lisp/nelisp-cc-jit-format-float.el'.  The `#[no_mangle]' symbol is
// now provided by the `.o' archive linked by `build.rs'.
// Bridge change: `bi_nl_jit_call_format_float' now casts to
// `fn(i64, i64, i64, *mut Sexp) -> i64' and passes x as
// `x.to_bits() as i64'; the elisp body uses `(:varargs (:f64 x-bits))'
// (= MOVQ rax → xmm0) to reconstruct the f64 for libc `snprintf'.
// `_ELISP_ARCHIVE_ANCHOR' count 59→60.
