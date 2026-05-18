//! intern / symbol-name / make-symbol + 6 other string/symbol
//! trampolines.  `(*const Sexp, *mut Sexp) -> i64'; OK=0 / ERR=1.
//! Reachable via `nl-jit-call-out-1' from `nelisp-jit-strategy.el'.
//!
//! All 9 stay in Rust pending Phase 47 grammar extensions for
//! `sexp-write-{str,symbol}', mut-str allocator + codepoint push,
//! UTF-8 case-fold + alphanumeric classifier, and f64 → decimal
//! string conversion (= libc snprintf via extern-call-varargs).

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


// Doc 86 §86.1.e (2026-05-10) — Tier 2 simple `bi_*' arms migrated to
// elisp.  The three trampolines below are bit-for-bit ports of the
// deleted `bi_concat_ints' / `bi_make_mut_string' / `bi_format_float_
// body' helpers in `eval/builtins.rs'; type / arity validation that
// the `bi_*' fns did up front now lives in
// `lisp/nelisp-stdlib-format.el' before the bridge call, so the
// trampolines below trust their inputs (or surface ERR for the narrow
// shape mismatches that survive the elisp pre-checks — e.g. dotted-
// tail in a list-of-ints).

/// `nelisp--concat-ints' — flat list of int char codepoints →
/// `Sexp::Str'.  Skips any codepoint that fails `char::from_u32' for
/// the same lossy-but-stable behaviour the deleted `bi_concat_ints'
/// had.  Non-Cons / non-Int Cons-car surfaces as ERR.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_concat_ints(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let mut s = String::new();
    let mut cur = (*arg).clone();
    loop {
        match cur {
            Sexp::Nil => break,
            Sexp::Cons(b) => {
                let v = b.car.clone();
                match &v {
                    Sexp::Int(n) => {
                        if let Some(ch) = char::from_u32(*n as u32) {
                            s.push(ch);
                        }
                    }
                    _ => return TRAMPOLINE_ERR,
                }
                cur = b.cdr.clone();
            }
            _ => return TRAMPOLINE_ERR,
        }
    }
    *out = Sexp::Str(s);
    TRAMPOLINE_OK
}

/// `nelisp--make-mut-string' — `extern "C" fn(*const Sexp{Cons LEN
/// CH}, *mut Sexp) -> i64'.  Builds a fresh `Sexp::MutStr' of LEN
/// copies of CH (= int codepoint).  Validation (= LEN >= 0, CH valid
/// codepoint) lives in the elisp `make-string' wrapper.  The two
/// args are packed into a Cons cell so the `:trampoline-unary' shape
/// suffices; the elisp wrapper builds `(cons LEN CH)' just before
/// calling.  Mismatched shape surfaces as ERR.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_make_mut_str(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let pair = match &*arg {
        Sexp::Cons(b) => b,
        _ => return TRAMPOLINE_ERR,
    };
    let n = match &pair.car {
        Sexp::Int(n) if *n >= 0 => *n as usize,
        _ => return TRAMPOLINE_ERR,
    };
    let c = match &pair.cdr {
        Sexp::Int(c) if (0..=0x10FFFF).contains(c) => {
            char::from_u32(*c as u32).unwrap_or(' ')
        }
        _ => return TRAMPOLINE_ERR,
    };
    *out = Sexp::mut_str(c.to_string().repeat(n));
    TRAMPOLINE_OK
}

/// `nelisp--format-float-body' — IEEE-754 `format' float-conversion
/// body builder.  ABI mode `:trampoline-format-float' (= xmm0 + rsi +
/// rdx + rcx).  CONV is one of ?f / ?F / ?e / ?E / ?g / ?G; PREC is
/// the precision (= caller passes >= 0).  Writes the unsigned,
/// unpadded body string into the out-slot.  Sole surviving sliver of
/// the Rust-min batch 6m `format' migration — the elisp dispatcher in
/// `lisp/nelisp-stdlib-plist-str.el' does spec parsing / sign /
/// padding and only delegates the IEEE-754 round-to-nearest-decimal
/// step (= Rust `{:.*}' / `{:.*e}' / `{:.*E}' format machinery) to
/// this trampoline.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_format_float(
    x: f64,
    conv: u32,
    prec: i64,
    out: *mut Sexp,
) -> i64 {
    let conv_ch = match char::from_u32(conv) {
        Some(c) => c,
        None => return TRAMPOLINE_ERR,
    };
    if prec < 0 {
        return TRAMPOLINE_ERR;
    }
    let p = prec as usize;
    let body = match conv_ch {
        'f' | 'F' => format!("{:.*}", p, x),
        'e' => format!("{:.*e}", p, x),
        'E' => format!("{:.*E}", p, x),
        'g' | 'G' => {
            let f = format!("{:.*}", p, x);
            let e = format!("{:.*e}", p, x);
            if f.len() <= e.len() {
                f
            } else {
                e
            }
        }
        _ => return TRAMPOLINE_ERR,
    };
    *out = Sexp::Str(body);
    TRAMPOLINE_OK
}
