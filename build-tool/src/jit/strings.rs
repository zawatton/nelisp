//! Doc 86 §86.1.d — intern / symbol-name / make-symbol trampolines.
//! `(*const Sexp, *mut Sexp) -> i64', OK = 0 / ERR = 1, reachable via
//! `nl-jit-call-out-1' from `lisp/nelisp-jit-strategy.el'.
//!
//! # Doc 120 §120.C swap status (2026-05-18)
//!
//! 0 of 9 trampolines moved to Phase-47-compiled elisp.  Every entry
//! in `strings.rs' allocates / writes a `Sexp::Str' / `Sexp::Symbol' /
//! `Sexp::MutStr' value, and Phase 47's current grammar pool exposes
//! ONLY `(sexp-write-nil SLOT)' / `(sexp-write-t SLOT)' for slot-byte
//! writes — there is no `sexp-write-str' / `sexp-write-symbol' /
//! `sexp-write-mut-str' / `mut-str-make' op (= no String-header
//! allocator, no Rust-side `Vec<u8>::with_capacity' analogue).  The
//! Doc 120.A predicate-eq swap relied on `extern-call' to a narrow
//! Rust helper for the slow path (= `nl_sexp_eq'); using `extern-
//! call' as a wholesale body pass-through for the entire trampoline
//! defeats the purpose of the swap.
//!
//! Skipped (= per-entry blockers):
//!
//!   - `nl_jit_intern' — needs `sexp-write-symbol' (= allocate fresh
//!     `Sexp::Symbol' from a `Sexp::Str's String header).  The Rust
//!     impl does `s.clone()' which boils down to `String::clone' (=
//!     fresh `Vec<u8>' allocation + memcpy of the byte range).  Phase
//!     47 has `str-bytes' / `str-len' for reading String headers but
//!     no allocator for emitting a new one.
//!
//!   - `nl_jit_symbol_name' — needs `sexp-write-str' + `sexp-write-
//!     str-literal' (= for the Nil → "nil" / T → "t" specialisation).
//!     The 3-way dispatch (Symbol / Nil / T) is expressible via `if'
//!     + `sexp-tag', but the slot writes themselves require a new
//!     primitive that owns a literal byte-string buffer.
//!
//!   - `nl_jit_make_symbol' — needs `sexp-write-symbol' (= same as
//!     `intern' above) PLUS the per-process `AtomicU64' counter, PLUS
//!     `format!()' string concatenation.  Could partially delegate to
//!     a narrow `extern-call' on a new `nl_make_uninterned_symbol'
//!     helper but the swap then degenerates into a 2-line elisp
//!     dispatch around a single extern call — net zero LOC win.
//!
//!   - `nl_jit_downcase' / `nl_jit_upcase' — needs UTF-8 case-folding
//!     primitive.  The Rust impl calls `String::to_lowercase' /
//!     `to_uppercase' which run the full Unicode SpecialCasing /
//!     UnicodeData tables (multi-codepoint expansions e.g. `'ß' →
//!     "ss"').  ASCII-only fast path is feasible if a new `str-
//!     ascii-fold-case' grammar op lands (= byte-stream walk + per-
//!     byte `if 'A' <= b <= 'Z' then b + 32 else b' branchless add),
//!     but the elisp wrapper would need to fall back to a Rust extern
//!     for non-ASCII anyway.  Out of scope for §120.C.
//!
//!   - `nl_jit_split_by_non_alnum' — needs `char::is_alphanumeric'
//!     (= multi-byte UTF-8 decode + Unicode property lookup) plus a
//!     `cons-make' list builder that closes over a fresh `Sexp::Str'
//!     for each fragment.  The cons-builder shape is fine (`Doc 101
//!     §101.D' shipped `cons-make') but each fragment needs a fresh
//!     `Sexp::Str' allocator (same blocker as `intern'/`symbol_name')
//!     and the alnum classifier needs a UTF-8 decoder (= grammar gap).
//!
//!   - `nl_jit_concat_ints' — needs a mutable byte-buffer (= mut-str-
//!     append-codepoint) to accumulate the result.  Loop over cons
//!     list is expressible via `while' + `cons-cdr', and `cons-car'
//!     + `sexp-int-unwrap' yields the codepoint, but Phase 47 cannot
//!     allocate the accumulator buffer nor encode the codepoint to
//!     UTF-8 bytes.  Needs `mut-str-make' + `mut-str-push-codepoint'
//!     + `mut-str-finalize-to-str' grammar ops.
//!
//!   - `nl_jit_make_mut_str' — needs `mut-str-make-repeat' (= literal
//!     port of `String::repeat' or `Vec::resize_with').  Phase 47 has
//!     no MutStr allocator at all.
//!
//!   - `nl_jit_format_float' — needs IEEE-754 → decimal-string
//!     conversion.  The Rust impl uses `format!("{:.*}")' which is
//!     the dragon4 / Grisu / Ryu pipeline depending on `core::fmt's
//!     selected backend.  Porting any of those algorithms to Phase
//!     47 is a multi-hundred-LOC effort; the realistic path is
//!     `extern-call' to libc `snprintf' once a new `extern-call-
//!     varargs' grammar form (= `... + va_list') exists.  Currently
//!     `extern-call' supports only 6 fixed i64 args (= no f64 / no
//!     varargs).  Out of scope for §120.C.
//!
//! On every target the Rust trampolines below are kept as the
//! resolved fn-ptr in `bridge::unified_fn_ptr' until the grammar
//! gaps above land.  Doc 122 should prioritise:
//!
//!   1. `sexp-write-str' / `sexp-write-symbol' (= unlocks
//!      `intern' + `symbol_name' immediately).
//!   2. `mut-str-make' + `mut-str-push-codepoint' (= unlocks
//!      `make_mut_str' + `concat_ints').
//!   3. `extern-call-f64' / `extern-call-varargs' (= unlocks
//!      `format_float' via libc snprintf bridge).
//!   4. UTF-8 helpers (`str-char-count' / `str-codepoint-at' /
//!      `str-is-alphanumeric-at') (= unlocks `downcase' /
//!      `upcase' / `split_by_non_alnum').

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
