//! String/symbol trampolines reached via `nl-jit-call-out-1' from
//! `nelisp-jit-strategy.el'.  Surviving Rust bodies: Unicode case +
//! tokenize, float-format, concat_ints.  Sig `(*const Sexp,
//! *mut Sexp) -> i64'; OK=0 / ERR=1.
//!
//! `nl_jit_make_symbol' body migrated to
//! `lisp/nelisp-cc-jit-make-symbol.el' (Phase 47 elisp .o).

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

/// Split on non-alphanumeric runs.  OMIT non-Nil → drop empties.
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


// Doc 86 §86.1.e.2 (2026-05-19): `nl_jit_concat_ints' Rust body
// deleted — replaced by Phase-47-compiled elisp body in
// `lisp/nelisp-cc-jit-concat-ints.el'.  The `#[no_mangle]' symbol is
// now provided by the `.o' archive linked by `build.rs'.

/// IEEE-754 float body builder.  CONV ∈ {f/F/e/E/g/G}, PREC ≥ 0.
/// Writes unsigned/unpadded body; elisp does sign + padding.
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
