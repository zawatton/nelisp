//! `nl-jit-call-*' bridge — invokes JIT entries by name via
//! `dlsym(RTLD_DEFAULT, alias(name))` (resolves both Phase 47 `.o`
//! trampolines and `#[no_mangle]` Rust trampolines via `-rdynamic`).
//! linux-x86_64 only.

use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;
use std::ffi::CString;

// Force-reference Phase 47 elisp `.o' archive members so the static
// linker pulls them into the binary (otherwise dlsym returns NULL).
// Nullary-typed redecl is intentional — `lib.rs::elisp_cc_spike` has
// the real signatures; here we only need symbol presence.
#[allow(dead_code, clashing_extern_declarations)]
extern "C" {
    fn nelisp_jit_add2(); fn nelisp_jit_sub2(); fn nelisp_jit_mul2();
    fn nelisp_jit_eq2();  fn nelisp_jit_lt2();  fn nelisp_jit_gt2();
    fn nelisp_jit_le2();  fn nelisp_jit_ge2();  fn nelisp_jit_logior2();
    fn nelisp_jit_logand2(); fn nelisp_jit_logxor2(); fn nelisp_jit_ash();
    fn nl_jit_float_add(); fn nl_jit_float_sub(); fn nl_jit_float_mul();
    fn nl_jit_float_div(); fn nl_jit_float_lt();  fn nl_jit_float_gt();
    fn nl_jit_float_le();  fn nl_jit_float_ge();  fn nl_jit_float_eq_eps();
    fn nl_jit_float_float(); fn nl_jit_float_exp(); fn nl_jit_float_log();
    fn nelisp_jit_predicate_eq(); fn nelisp_jit_ref_eq();
    fn nelisp_jit_record_type();  fn nelisp_jit_record_len();
    fn nelisp_jit_record_ref();   fn nelisp_jit_record_set();
    fn nelisp_jit_length(); fn nelisp_jit_aref();
    fn nelisp_jit_aset();   fn nelisp_jit_elt();
    fn nelisp_jit_cons_car();    fn nelisp_jit_cons_cdr();
    fn nelisp_jit_cons_setcar(); fn nelisp_jit_cons_setcdr();
}

/// Force-link anchor: `#[used] static' of fn-ptrs so LTO can't elide
/// the externs above.  Never read; presence keeps the symbols live.
#[used]
static _ELISP_ARCHIVE_ANCHOR: [unsafe extern "C" fn(); 38] = [
    nelisp_jit_add2, nelisp_jit_sub2, nelisp_jit_mul2, nelisp_jit_eq2,
    nelisp_jit_lt2,  nelisp_jit_gt2,  nelisp_jit_le2,  nelisp_jit_ge2,
    nelisp_jit_logior2, nelisp_jit_logand2, nelisp_jit_logxor2, nelisp_jit_ash,
    nl_jit_float_add, nl_jit_float_sub, nl_jit_float_mul, nl_jit_float_div,
    nl_jit_float_lt,  nl_jit_float_gt,  nl_jit_float_le,  nl_jit_float_ge,
    nl_jit_float_eq_eps, nl_jit_float_float, nl_jit_float_exp, nl_jit_float_log,
    nelisp_jit_predicate_eq, nelisp_jit_ref_eq,
    nelisp_jit_record_type, nelisp_jit_record_len,
    nelisp_jit_record_ref,  nelisp_jit_record_set,
    nelisp_jit_length, nelisp_jit_aref, nelisp_jit_aset, nelisp_jit_elt,
    nelisp_jit_cons_car, nelisp_jit_cons_cdr,
    nelisp_jit_cons_setcar, nelisp_jit_cons_setcdr,
];

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

/// Map elisp-wrapper names to exported symbol names (e.g. short-name
/// → `cons_*' / `predicate_*', or `nelisp_jit_*' → `nl_jit_*').
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
        "nelisp_jit_make_symbol"   => "nl_jit_make_symbol",
        "nelisp_jit_syscall"             => "nl_jit_syscall_call",
        "nelisp_jit_syscall_supported_p" => "nl_jit_syscall_supported_p",
        other => other,
    }
}

/// Resolve via `dlsym(RTLD_DEFAULT, alias(name))`.
pub(super) fn unified_fn_ptr(name: &str) -> Option<*const u8> {
    let cstr = CString::new(alias(name)).ok()?;
    // SAFETY: dlsym never deref'd here; NULL → returned as None.
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
