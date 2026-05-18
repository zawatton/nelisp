//! `nl-jit-call-*' bridge primitives.  Elisp `lowered_X' wrappers in
//! `lisp/nelisp-jit-strategy.el' call these to invoke JIT entries by
//! name.  Shapes: `i64-i64' / `ptr-ptr' / `syscall' (i64×7) / float /
//! out-param.  Name → fn-ptr lookup goes through `unified_fn_ptr',
//! a thin `dlsym(RTLD_DEFAULT, name)' wrapper.  Every supported entry
//! name resolves either to a Phase 47-compiled elisp `.o' trampoline
//! in `libnelisp_elisp_spike.a' or to a `#[no_mangle]' Rust trampoline
//! in `jit/{access,box_accessor,cons,hash,predicate,regex,strings,
//! syscall,time}.rs'.  `-rdynamic' (`.cargo/config.toml') exposes
//! both kinds in the dynsym table.  Bridge-only aliases (cons /
//! predicate / nl_jit_* vs nelisp_jit_* name mismatch) live in
//! `alias' below.
//!
//! Supported targets: linux-x86_64 only (= `lib.rs' `compile_error!').

use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;
use std::ffi::CString;

// Phase 47-compiled elisp trampolines live in `libnelisp_elisp_spike.a'.
// `lib.rs::elisp_cc_spike' declares signature-typed externs for arith /
// float / math / predicate / record / access (34 names) but in a
// `cargo test --lib' binary those decls alone don't pull the archive
// members in — no callsite reaches them.  Re-reference all 34 via the
// public `crate::elisp_cc_spike::*' path (= same symbols, signatures
// inherited so no `clashing_extern_declarations' lint), plus 4 locally-
// declared cons names lib.rs doesn't cover.  Stored as `*const u8' so
// the per-arity fn signatures don't need to unify.
#[allow(dead_code, improper_ctypes)]
extern "C" {
    fn nelisp_jit_cons_car(arg: *const Sexp, out: *mut Sexp) -> i64;
    fn nelisp_jit_cons_cdr(arg: *const Sexp, out: *mut Sexp) -> i64;
    fn nelisp_jit_cons_setcar(arg: *const Sexp, val: *const Sexp, out: *mut Sexp) -> i64;
    fn nelisp_jit_cons_setcdr(arg: *const Sexp, val: *const Sexp, out: *mut Sexp) -> i64;
}

/// Force-link anchor: 4 trait-object wrapped arrays grouped by fn
/// signature (= one per shape lib.rs declares).  `#[used]' on each
/// makes LTO retain the symbols; arrays are split by signature to
/// avoid the `*const u8' Sync limitation in const context.
#[used]
static _ANCHOR_I64_I64: [unsafe extern "C" fn(i64, i64) -> i64; 12] = {
    use crate::elisp_cc_spike as e;
    [
        e::nelisp_jit_add2, e::nelisp_jit_sub2, e::nelisp_jit_mul2, e::nelisp_jit_eq2,
        e::nelisp_jit_lt2,  e::nelisp_jit_gt2,  e::nelisp_jit_le2,  e::nelisp_jit_ge2,
        e::nelisp_jit_logior2, e::nelisp_jit_logand2, e::nelisp_jit_logxor2, e::nelisp_jit_ash,
    ]
};

#[used]
static _ANCHOR_F64_F64: [unsafe extern "C" fn(f64, f64) -> f64; 4] = {
    use crate::elisp_cc_spike as e;
    [e::nl_jit_float_add, e::nl_jit_float_sub, e::nl_jit_float_mul, e::nl_jit_float_div]
};

#[used]
static _ANCHOR_F64_CMP: [unsafe extern "C" fn(f64, f64) -> i64; 5] = {
    use crate::elisp_cc_spike as e;
    [e::nl_jit_float_lt, e::nl_jit_float_gt, e::nl_jit_float_le, e::nl_jit_float_ge, e::nl_jit_float_eq_eps]
};

#[used]
static _ANCHOR_F64_UNARY: [unsafe extern "C" fn(f64) -> f64; 3] = {
    use crate::elisp_cc_spike as e;
    [e::nl_jit_float_float, e::nl_jit_float_exp, e::nl_jit_float_log]
};

#[used]
static _ANCHOR_PTR2: [unsafe extern "C" fn(*const Sexp, *const Sexp) -> i64; 1] = {
    [crate::elisp_cc_spike::nelisp_jit_predicate_eq]
};

#[used]
static _ANCHOR_PTR_OUT: [unsafe extern "C" fn(*const Sexp, *mut Sexp) -> i64; 3] = {
    use crate::elisp_cc_spike as e;
    [e::nelisp_jit_record_type, e::nelisp_jit_record_len, e::nelisp_jit_length]
};

#[used]
static _ANCHOR_PTR_I64_OUT: [unsafe extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64; 3] = {
    use crate::elisp_cc_spike as e;
    [e::nelisp_jit_record_ref, e::nelisp_jit_aref, e::nelisp_jit_elt]
};

#[used]
static _ANCHOR_PTR_I64_PTR_OUT: [unsafe extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64; 2] = {
    use crate::elisp_cc_spike as e;
    [e::nelisp_jit_record_set, e::nelisp_jit_aset]
};

#[used]
static _ANCHOR_PTR2_OUT: [unsafe extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64; 3] = {
    [crate::elisp_cc_spike::nelisp_jit_ref_eq, nelisp_jit_cons_setcar, nelisp_jit_cons_setcdr]
};

#[used]
static _ANCHOR_CONS_OUT_1: [unsafe extern "C" fn(*const Sexp, *mut Sexp) -> i64; 2] = {
    [nelisp_jit_cons_car, nelisp_jit_cons_cdr]
};

/// Extract JIT-entry name: accepts `Symbol' or `Str' (2 forms elisp
/// wrappers produce).
fn as_name<'a>(name_arg: &'a str, v: &'a Sexp) -> Result<&'a str, EvalError> {
    match v {
        Sexp::Symbol(s) | Sexp::Str(s) => Ok(s.as_str()),
        other => Err(EvalError::WrongType {
            expected: format!("symbol or string ({} arg 0)", name_arg),
            got: other.clone(),
        }),
    }
}

/// Translate legacy bridge aliases to actual exported symbol names.
/// (1) elisp `.o' archive exports cons-family / predicate as `_cons_*' /
/// `predicate_eq' but elisp wrappers use shorter names; (2) Rust
/// trampolines (`jit/cons.rs::nl_jit_cons_make' etc.) keep `nl_jit_*'
/// names but elisp callers pass `nelisp_jit_*'.
fn alias(name: &str) -> &str {
    match name {
        "nelisp_jit_car"       => "nelisp_jit_cons_car",
        "nelisp_jit_cdr"       => "nelisp_jit_cons_cdr",
        "nelisp_jit_setcar"    => "nelisp_jit_cons_setcar",
        "nelisp_jit_setcdr"    => "nelisp_jit_cons_setcdr",
        "nelisp_jit_eq_inline" => "nelisp_jit_predicate_eq",
        "nelisp_jit_cons"      => "nl_jit_cons_make",
        "nelisp_jit_type_of"   => "nl_jit_type_of",
        "nelisp_jit_sxhash"    => "nl_jit_sxhash",
        "nelisp_jit_intern"        => "nl_jit_intern",
        "nelisp_jit_symbol_name"   => "nl_jit_symbol_name",
        "nelisp_jit_make_symbol"   => "nl_jit_make_symbol",
        "nelisp_jit_syscall"             => "nl_jit_syscall_call",
        "nelisp_jit_syscall_supported_p" => "nl_jit_syscall_supported_p",
        other => other,
    }
}

/// Resolve a `nelisp_jit_*' / `nl_jit_*' name to its trampoline fn-ptr
/// via `dlsym(RTLD_DEFAULT, alias(name))'.  `-rdynamic' exposes every
/// `#[no_mangle]' Rust trampoline AND every Phase 47-compiled elisp
/// `.o' symbol in the dynsym table, so this replaces the pre-Doc-128
/// compile-time `extern "C"' match (~120 LOC + 7 `mod *_link' blocks).
pub(super) fn unified_fn_ptr(name: &str) -> Option<*const u8> {
    let cstr = CString::new(alias(name)).ok()?;
    // SAFETY: dlsym is async-signal-safe and may return NULL; we never
    // deref the returned pointer.  RTLD_DEFAULT covers the nelisp
    // binary + statically linked elisp `.o' archive + libc / libm.
    let addr = unsafe { libc::dlsym(libc::RTLD_DEFAULT, cstr.as_ptr()) };
    if addr.is_null() { None } else { Some(addr as *const u8) }
}

/// Lookup preamble: arity check + name extract + resolve.
fn jit_lookup(prim: &str, args: &[Sexp], arity: usize) -> Result<*const u8, EvalError> {
    require_arity(prim, args, arity, Some(arity))?;
    let name = as_name(prim, &args[0])?;
    unified_fn_ptr(name).ok_or_else(|| {
        EvalError::Internal(format!("{}: unknown JIT entry name `{}'", prim, name))
    })
}

/// `(nl-jit-call-i64-i64 NAME A B) -> Int'.  `extern "C" fn(i64, i64) -> i64'.
pub fn bi_nl_jit_call_i64_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-i64-i64", args, 3)?;
    let a = as_int("nl-jit-call-i64-i64", &args[1])?;
    let b = as_int("nl-jit-call-i64-i64", &args[2])?;
    // SAFETY: NAME resolves to arith slot with `fn(i64, i64) -> i64'.
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(a, b)))
}

/// `(nl-jit-call-ptr-ptr NAME A B) -> Int'.  Raw `*const Sexp' pair.
pub fn bi_nl_jit_call_ptr_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-ptr-ptr", args, 3)?;
    // SAFETY: NAME resolves to `(*const Sexp, *const Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(&args[1] as *const _, &args[2] as *const _)))
}

/// `(nl-jit-call-syscall NAME NR A0 A1 A2 A3 A4 A5) -> Int'.
pub fn bi_nl_jit_call_syscall(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-syscall", args, 8)?;
    let mut ints = [0i64; 7];
    for (i, slot) in ints.iter_mut().enumerate() {
        *slot = as_int("nl-jit-call-syscall", &args[i + 1])?;
    }
    // SAFETY: NAME resolves to `(i64 × 7) -> i64'.
    let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
        unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(ints[0], ints[1], ints[2], ints[3], ints[4], ints[5], ints[6])))
}

// Float-family bridge: arith → Float, cmp → Int (0/1).

fn float_pair(args: &[Sexp], name: &str) -> Result<(*const u8, f64, f64), EvalError> {
    let p = jit_lookup(name, args, 3)?;
    let (a, b, _) = crate::eval::builtins::num_pair(&args[1..], name)?;
    Ok((p, a, b))
}

/// `(nl-jit-call-float-float NAME A B) -> Float'.
pub fn bi_nl_jit_call_float_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-float")?;
    // SAFETY: NAME resolves to `nl_jit_float_{add,sub,mul,div}'.
    let f: extern "C" fn(f64, f64) -> f64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Float(f(a, b)))
}

/// `(nl-jit-call-float-cmp NAME A B) -> Int'.
pub fn bi_nl_jit_call_float_cmp(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-cmp")?;
    // SAFETY: NAME resolves to `nl_jit_float_{eq_eps,lt,gt,le,ge}'.
    let f: extern "C" fn(f64, f64) -> i64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(a, b)))
}

fn to_f64(v: &Sexp, expected: &str) -> Result<f64, EvalError> {
    match v {
        Sexp::Int(i) => Ok(*i as f64),
        Sexp::Float(f) => Ok(*f),
        Sexp::Nil => Ok(0.0),
        other => Err(EvalError::WrongType { expected: expected.into(), got: other.clone() }),
    }
}

/// `(nl-jit-call-float-unary NAME X) -> Float'.  `fn(f64) -> f64'.
pub fn bi_nl_jit_call_float_unary(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-float-unary", args, 2)?;
    let x = to_f64(&args[1], "number")?;
    // SAFETY: NAME resolves to `nl_jit_float_{float,exp,log}'.
    let f: extern "C" fn(f64) -> f64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Float(f(x)))
}

// Out-param trampolines (9 fns: 5 cons + 4 access).  Each takes
// `*const Sexp' inputs, writes via `*mut Sexp', returns `i64'
// (OK=0 / ERR=1).  ERR → `WrongType { expected: "jit-call-out-N" }'.

const TRAMPOLINE_OK: i64 = 0;

fn out_result(rc: i64, out: Sexp, prim: &str, arg: &Sexp) -> Result<Sexp, EvalError> {
    if rc == TRAMPOLINE_OK { Ok(out) }
    else { Err(EvalError::WrongType { expected: prim.into(), got: arg.clone() }) }
}

/// `(nl-jit-call-out-1 NAME ARG) -> Sexp'.
pub fn bi_nl_jit_call_out_1(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-1", args, 2)?;
    // SAFETY: NAME resolves to `(*const Sexp, *mut Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, *mut Sexp) -> i64 = unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-1", &args[1])
}

/// `(nl-jit-call-out-2 NAME ARG1 ARG2) -> Sexp'.
pub fn bi_nl_jit_call_out_2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-2", args, 3)?;
    // SAFETY: NAME resolves to `(*const Sexp, *const Sexp, *mut Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &args[2] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-2", &args[1])
}

/// `(nl-jit-call-out-1i NAME ARG IDX) -> Sexp'.
pub fn bi_nl_jit_call_out_1i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-1i", args, 3)?;
    let idx = as_int("nl-jit-call-out-1i", &args[2])?;
    // SAFETY: NAME resolves to `(*const Sexp, i64, *mut Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &mut out as *mut _);
    out_result(r, out, "jit-call-out-1i", &args[1])
}

/// `(nl-jit-call-out-2i NAME ARG IDX VAL) -> Sexp'.
pub fn bi_nl_jit_call_out_2i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-2i", args, 4)?;
    let idx = as_int("nl-jit-call-out-2i", &args[2])?;
    // SAFETY: NAME resolves to `(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &args[3] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-2i", &args[1])
}

/// `(nl-jit-call-format-float NAME X CONV PREC) -> Sexp'.
pub fn bi_nl_jit_call_format_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-format-float", args, 4)?;
    let x = to_f64(&args[1], "numberp")?;
    let conv = as_int("nl-jit-call-format-float", &args[2])?;
    let prec = as_int("nl-jit-call-format-float", &args[3])?;
    // SAFETY: NAME resolves to `nl_jit_format_float' (xmm0 + rsi + rdx + rcx).
    let f: extern "C" fn(f64, u32, i64, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(x, conv as u32, prec, &mut out as *mut _);
    out_result(r, out, "jit-call-format-float", &args[1])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unified_fn_ptr_resolves_core_entries() {
        // Sample across 4 dispatch families (elisp arith / cons /
        // access / Rust trampolines).  Full per-shape coverage lives
        // in higher-level integration tests.
        for n in [
            "nelisp_jit_add2", "nelisp_jit_eq_inline", "nelisp_jit_car",
            "nelisp_jit_length", "nelisp_jit_aref", "nelisp_jit_intern",
            "nelisp_jit_syscall", "nl_jit_float_add", "nl_jit_float_exp",
        ] {
            let p = unified_fn_ptr(n);
            assert!(p.is_some(), "missing `{}'", n);
            assert!(!p.unwrap().is_null(), "`{}' is null", n);
        }
    }

    #[test]
    fn unified_fn_ptr_unknown_returns_none() {
        assert!(unified_fn_ptr("nelisp_jit_does_not_exist").is_none());
        assert!(unified_fn_ptr("").is_none());
    }

    #[test]
    fn call_i64_i64_add2_smoke() {
        let r = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(7), Sexp::Int(8),
        ]).expect("add2 must succeed");
        assert_eq!(r, Sexp::Int(15));
    }

    #[test]
    fn call_i64_i64_accepts_string_name() {
        let r = bi_nl_jit_call_i64_i64(&[
            Sexp::Str("nelisp_jit_mul2".into()),
            Sexp::Int(6), Sexp::Int(7),
        ]).expect("mul2 (str name) must succeed");
        assert_eq!(r, Sexp::Int(42));
    }

    #[test]
    fn call_i64_i64_errors() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_no_such".into()),
            Sexp::Int(0), Sexp::Int(0),
        ]).expect_err("unknown name must error");
        assert!(matches!(err, EvalError::Internal(_)));
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()), Sexp::Int(1),
        ]).expect_err("arity 2 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Int(0), Sexp::Int(1), Sexp::Int(2),
        ]).expect_err("Int as name must error");
        assert!(matches!(err, EvalError::WrongType { .. }));
    }

    #[test]
    fn call_ptr_ptr_eq_inline() {
        let eq = |a, b| bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()), a, b,
        ]).expect("eq_inline must succeed");
        assert_eq!(eq(Sexp::Int(7), Sexp::Int(7)), Sexp::Int(1));
        assert_eq!(eq(Sexp::Int(7), Sexp::Int(8)), Sexp::Int(0));
    }

    #[test]
    fn call_syscall_errors() {
        let err = bi_nl_jit_call_syscall(&vec![Sexp::Symbol("nelisp_jit_syscall".into()); 7])
            .expect_err("arity 7 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
        let mut args = vec![Sexp::Symbol("nelisp_jit_no_syscall".into())];
        args.extend(std::iter::repeat(Sexp::Int(0)).take(7));
        assert!(matches!(
            bi_nl_jit_call_syscall(&args).expect_err("unknown name must error"),
            EvalError::Internal(_)
        ));
    }

    #[test]
    fn call_out_1_car_cdr_length() {
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let car = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()), lst.clone(),
        ]).expect("car must succeed");
        assert_eq!(car, Sexp::Int(1));
        let cdr = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_cdr".into()), lst,
        ]).expect("cdr must succeed");
        assert_eq!(cdr, Sexp::list_from(&[Sexp::Int(2), Sexp::Int(3)]));
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let len = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_length".into()), v,
        ]).expect("length must succeed");
        assert_eq!(len, Sexp::Int(3));
    }

    #[test]
    fn call_out_1_wrong_type_errors() {
        let err = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()), Sexp::Int(7),
        ]).expect_err("car of int must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-1");
                assert_eq!(got, Sexp::Int(7));
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn call_out_1i_aref() {
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let r = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_aref".into()), v.clone(), Sexp::Int(1),
        ]).expect("aref in-range must succeed");
        assert_eq!(r, Sexp::Int(2));
        let err = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_aref".into()), v.clone(), Sexp::Int(5),
        ]).expect_err("aref out-of-range must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-1i");
                assert_eq!(got, v);
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }
}
