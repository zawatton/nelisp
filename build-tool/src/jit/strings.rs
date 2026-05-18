//! String/symbol trampolines reached via `nl-jit-call-out-1' from
//! `nelisp-jit-strategy.el'.  Surviving Rust bodies: Unicode case +
//! tokenize, float-format, concat_ints, make_symbol.  Sig `(*const Sexp,
//! *mut Sexp) -> i64'; OK=0 / ERR=1.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Fresh uninterned symbol via per-process counter.  Accepts Str/MutStr/Symbol.
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

#[no_mangle]
pub unsafe extern "C" fn nl_jit_downcase(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match read_text(&*arg) {
        Some(s) => { *out = Sexp::Str(s.to_lowercase()); TRAMPOLINE_OK }
        None => TRAMPOLINE_ERR,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_jit_upcase(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match read_text(&*arg) {
        Some(s) => { *out = Sexp::Str(s.to_uppercase()); TRAMPOLINE_OK }
        None => TRAMPOLINE_ERR,
    }
}

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


/// List of int codepoints → Sexp::Str.  Lossy on invalid codepoints; ERR on shape.
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
