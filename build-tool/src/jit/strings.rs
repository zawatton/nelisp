//! String/symbol trampolines: symbol-name + float-format Rust bodies.
//! Other entries served by Phase 47 elisp `.o' archive.

use crate::eval::sexp::Sexp;
use std::sync::atomic::AtomicI64;

use super::{read_sexp_str, TRAMPOLINE_ERR, TRAMPOLINE_OK};

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

/// Symbol(s) → Str(s); Nil → "nil"; T → "t"; else ERR.
/// Called from `nelisp-jit-strategy.el' `symbol-name' wrapper via
/// `(nl-jit-call-out-1 "nelisp_jit_symbol_name" sym)'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_symbol_name(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match read_sexp_str(&*arg) {
        Some(s) => { *out = Sexp::Str(s); TRAMPOLINE_OK }
        None => TRAMPOLINE_ERR,
    }
}

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
            let (f, e) = (format!("{:.*}", p, x), format!("{:.*e}", p, x));
            if f.len() <= e.len() { f } else { e }
        }
        _ => return TRAMPOLINE_ERR,
    };
    *out = Sexp::Str(body);
    TRAMPOLINE_OK
}
