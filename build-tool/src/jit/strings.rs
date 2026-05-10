//! Doc 86 §86.1.d — intern / symbol-name / make-symbol trampolines.
//! `(*const Sexp, *mut Sexp) -> i64', OK = 0 / ERR = 1, reachable via
//! `nl-jit-call-out-1' from `lisp/nelisp-jit-strategy.el'.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Sexp::Str / MutStr → Sexp::Symbol.  Symbol input ERRs (= elisp
/// wrapper handles symbolp passthrough before calling).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_intern(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Str(s) => { *out = Sexp::Symbol(s.clone()); TRAMPOLINE_OK }
        Sexp::MutStr(rc) => { *out = Sexp::Symbol(rc.value.clone()); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// Symbol(s) → Str(s); Nil → "nil"; T → "t"; else ERR.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_symbol_name(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Symbol(s) => { *out = Sexp::Str(s.clone()); TRAMPOLINE_OK }
        Sexp::Nil => { *out = Sexp::Str("nil".into()); TRAMPOLINE_OK }
        Sexp::T => { *out = Sexp::Str("t".into()); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// Fresh uninterned symbol via per-process counter — bit-for-bit
/// identical to pre-§86.1.d `bi_make_symbol' output.  Accepts Str /
/// MutStr / Symbol; ERR otherwise.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_make_symbol(arg: *const Sexp, out: *mut Sexp) -> i64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let name: String = match &*arg {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.value.clone(),
        Sexp::Symbol(s) => s.clone(),
        _ => return TRAMPOLINE_ERR,
    };
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    *out = Sexp::Symbol(format!("{}__nelisp-uninterned-{}", name, n));
    TRAMPOLINE_OK
}
