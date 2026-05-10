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

// Doc 87 §86.1.f (2026-05-10) — string case + tokenize trampolines.
// Replace the deleted `bi_nl_downcase' / `bi_nl_upcase' /
// `bi_nl_split_by_non_alnum' helpers in `eval/builtins.rs'.  Bridged
// via `nl-jit-call-out-1' (1-arg) and `nl-jit-call-out-2' (2-arg, the
// OMIT-EMPTY flag).

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

/// `(nl-downcase STRING)' — UTF-8 lowercase via Rust stdlib.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_downcase(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match read_text(&*arg) {
        Some(s) => {
            *out = Sexp::Str(s.to_lowercase());
            TRAMPOLINE_OK
        }
        None => TRAMPOLINE_ERR,
    }
}

/// `(nl-upcase STRING)' — UTF-8 uppercase via Rust stdlib.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_upcase(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match read_text(&*arg) {
        Some(s) => {
            *out = Sexp::Str(s.to_uppercase());
            TRAMPOLINE_OK
        }
        None => TRAMPOLINE_ERR,
    }
}

/// `(nl-split-by-non-alnum STRING OMIT)' — split on runs of
/// non-alphanumeric chars.  When OMIT is non-Nil, drops empty
/// fragments (= the default Elisp `split-string ... t' behaviour).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_split_by_non_alnum(
    str_arg: *const Sexp,
    omit_arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let s = match read_text(&*str_arg) {
        Some(v) => v,
        None => return TRAMPOLINE_ERR,
    };
    let omit_empty = !matches!(&*omit_arg, Sexp::Nil);
    let parts: Vec<Sexp> = s
        .split(|c: char| !c.is_alphanumeric())
        .filter(|p| if omit_empty { !p.is_empty() } else { true })
        .map(|p| Sexp::Str(p.to_string()))
        .collect();
    *out = Sexp::list_from(&parts);
    TRAMPOLINE_OK
}
