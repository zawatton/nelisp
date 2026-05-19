//! `nl-jit-call-*` bridge via `dlsym(RTLD_DEFAULT, alias(name))`.

use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;
use std::ffi::CString;

// Pull Phase 47 `.o` archive members into the final binary so `dlsym`
// can see them. Nullary redecls are enough here; only symbol presence matters.
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
    fn nl_jit_intern(); fn nl_jit_make_mut_str(); fn nl_jit_mut_str_len();
    fn nl_jit_record_alloc();
    fn nelisp_jit_predicate_eq(); fn nelisp_jit_ref_eq();
    fn nelisp_jit_record_type();  fn nelisp_jit_record_len();
    fn nelisp_jit_record_ref();   fn nelisp_jit_record_set();
    fn nelisp_jit_length(); fn nelisp_jit_aref();
    fn nelisp_jit_aset();   fn nelisp_jit_elt();
    fn nelisp_jit_cons_car();    fn nelisp_jit_cons_cdr();
    fn nelisp_jit_cons_setcar(); fn nelisp_jit_cons_setcdr();
    fn nl_jit_bool_vector_len(); fn nl_jit_str_codepoint_at();
    fn nl_jit_type_of();   fn nl_jit_sxhash();
    fn nl_cons_car_ptr();        fn nl_cons_cdr_ptr();
    fn nl_jit_make_symbol();
    // db33abdd (2026-05-19): `nl_record_type_tag_ptr' — Phase-47 elisp
    // body replaced the Rust body in `jit/box_accessor.rs'; re-anchored
    // here after the revert at bf670ee4 dropped it from the array.
    fn nl_record_type_tag_ptr();
    // Doc 86 §86.1.e.2 (2026-05-19): `nl_jit_concat_ints' — Phase-47
    // elisp body replaces the Rust body in `jit/strings.rs'.
    fn nl_jit_concat_ints();
    // Doc 86 §86.2 (2026-05-19): `nl_sf_quote' — Phase-47 elisp body
    // replaces the Rust body of `sf_quote' in `eval/special_forms.rs'.
    fn nl_sf_quote();
    // Phase 47 elisp migration: `nl_jit_downcase' + `nl_jit_upcase' —
    // ASCII case-fold trampolines; Rust bodies deleted from `jit/strings.rs'.
    fn nl_jit_downcase();
    fn nl_jit_upcase();
    // Phase 47 elisp migration: `nl_jit_split_by_non_alnum' —
    // non-alphanumeric splitter; Rust body deleted from `jit/strings.rs'.
    fn nl_jit_split_by_non_alnum();
}

/// Keep the archive symbols live through LTO.
#[used]
static _ELISP_ARCHIVE_ANCHOR: [unsafe extern "C" fn(); 55] = [
    nelisp_jit_add2, nelisp_jit_sub2, nelisp_jit_mul2, nelisp_jit_eq2,
    nelisp_jit_lt2,  nelisp_jit_gt2,  nelisp_jit_le2,  nelisp_jit_ge2,
    nelisp_jit_logior2, nelisp_jit_logand2, nelisp_jit_logxor2, nelisp_jit_ash,
    nl_jit_float_add, nl_jit_float_sub, nl_jit_float_mul, nl_jit_float_div,
    nl_jit_float_lt,  nl_jit_float_gt,  nl_jit_float_le,  nl_jit_float_ge,
    nl_jit_float_eq_eps, nl_jit_float_float, nl_jit_float_exp, nl_jit_float_log,
    nl_jit_intern, nl_jit_make_mut_str, nl_jit_mut_str_len, nl_jit_record_alloc,
    nelisp_jit_predicate_eq, nelisp_jit_ref_eq,
    nelisp_jit_record_type, nelisp_jit_record_len,
    nelisp_jit_record_ref,  nelisp_jit_record_set,
    nelisp_jit_length, nelisp_jit_aref, nelisp_jit_aset, nelisp_jit_elt,
    nelisp_jit_cons_car, nelisp_jit_cons_cdr,
    nelisp_jit_cons_setcar, nelisp_jit_cons_setcdr,
    nl_jit_bool_vector_len, nl_jit_str_codepoint_at,
    nl_jit_type_of, nl_jit_sxhash,
    nl_cons_car_ptr, nl_cons_cdr_ptr,
    nl_jit_make_symbol,
    nl_record_type_tag_ptr,
    nl_jit_concat_ints,
    nl_sf_quote,
    nl_jit_downcase,
    nl_jit_upcase,
    nl_jit_split_by_non_alnum,
];

fn as_name<'a>(name_arg: &'a str, v: &'a Sexp) -> Result<&'a str, EvalError> {
    match v {
        Sexp::Symbol(s) | Sexp::Str(s) => Ok(s.as_str()),
        other => Err(EvalError::WrongType {
            expected: format!("symbol or string ({} arg 0)", name_arg),
            got: other.clone(),
        }),
    }
}

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

pub(super) fn unified_fn_ptr(name: &str) -> Option<*const u8> {
    let cstr = CString::new(alias(name)).ok()?;
    let addr = unsafe { libc::dlsym(libc::RTLD_DEFAULT, cstr.as_ptr()) };
    if addr.is_null() { None } else { Some(addr as *const u8) }
}

fn jit_lookup(prim: &str, args: &[Sexp], arity: usize) -> Result<*const u8, EvalError> {
    require_arity(prim, args, arity, Some(arity))?;
    let name = as_name(prim, &args[0])?;
    unified_fn_ptr(name).ok_or_else(|| {
        EvalError::Internal(format!("{}: unknown JIT entry name `{}'", prim, name))
    })
}

unsafe fn cast<T: Copy>(p: *const u8) -> T {
    std::mem::transmute_copy(&p)
}

pub fn bi_nl_jit_call_i64_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-i64-i64", args, 3)?;
    let a = as_int("nl-jit-call-i64-i64", &args[1])?;
    let b = as_int("nl-jit-call-i64-i64", &args[2])?;
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { cast(p) };
    Ok(Sexp::Int(f(a, b)))
}

pub fn bi_nl_jit_call_ptr_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-ptr-ptr", args, 3)?;
    let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 = unsafe { cast(p) };
    Ok(Sexp::Int(f(&args[1] as *const _, &args[2] as *const _)))
}

pub fn bi_nl_jit_call_syscall(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-syscall", args, 8)?;
    let mut ints = [0i64; 7];
    for (i, slot) in ints.iter_mut().enumerate() {
        *slot = as_int("nl-jit-call-syscall", &args[i + 1])?;
    }
    let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 = unsafe { cast(p) };
    Ok(Sexp::Int(f(ints[0], ints[1], ints[2], ints[3], ints[4], ints[5], ints[6])))
}

fn float_pair(args: &[Sexp], name: &str) -> Result<(*const u8, f64, f64), EvalError> {
    let p = jit_lookup(name, args, 3)?;
    let (a, b, _) = crate::eval::builtins::num_pair(&args[1..], name)?;
    Ok((p, a, b))
}

pub fn bi_nl_jit_call_float_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-float")?;
    let f: extern "C" fn(f64, f64) -> f64 = unsafe { cast(p) };
    Ok(Sexp::Float(f(a, b)))
}

pub fn bi_nl_jit_call_float_cmp(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-cmp")?;
    let f: extern "C" fn(f64, f64) -> i64 = unsafe { cast(p) };
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

pub fn bi_nl_jit_call_float_unary(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-float-unary", args, 2)?;
    let x = to_f64(&args[1], "number")?;
    let f: extern "C" fn(f64) -> f64 = unsafe { cast(p) };
    Ok(Sexp::Float(f(x)))
}

const TRAMPOLINE_OK: i64 = 0;

fn out_result(rc: i64, out: Sexp, prim: &str, arg: &Sexp) -> Result<Sexp, EvalError> {
    if rc == TRAMPOLINE_OK { Ok(out) }
    else { Err(EvalError::WrongType { expected: prim.into(), got: arg.clone() }) }
}

pub fn bi_nl_jit_call_out_1(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-1", args, 2)?;
    let f: extern "C" fn(*const Sexp, *mut Sexp) -> i64 = unsafe { cast(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-1", &args[1])
}

pub fn bi_nl_jit_call_out_2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-2", args, 3)?;
    let f: extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64 = unsafe { cast(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &args[2] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-2", &args[1])
}

pub fn bi_nl_jit_call_out_1i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-1i", args, 3)?;
    let idx = as_int("nl-jit-call-out-1i", &args[2])?;
    let f: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64 = unsafe { cast(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &mut out as *mut _);
    out_result(r, out, "jit-call-out-1i", &args[1])
}

pub fn bi_nl_jit_call_out_2i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-2i", args, 4)?;
    let idx = as_int("nl-jit-call-out-2i", &args[2])?;
    let f: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64 = unsafe { cast(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &args[3] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-2i", &args[1])
}

pub fn bi_nl_jit_call_format_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-format-float", args, 4)?;
    let x = to_f64(&args[1], "numberp")?;
    let conv = as_int("nl-jit-call-format-float", &args[2])?;
    let prec = as_int("nl-jit-call-format-float", &args[3])?;
    let f: extern "C" fn(f64, u32, i64, *mut Sexp) -> i64 = unsafe { cast(p) };
    let mut out = Sexp::Nil;
    let r = f(x, conv as u32, prec, &mut out as *mut _);
    out_result(r, out, "jit-call-format-float", &args[1])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(name: &str) -> Sexp { Sexp::Symbol(name.into()) }

    #[test]
    fn unified_fn_ptr_resolves_core_entries() {
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
        let r = bi_nl_jit_call_i64_i64(&[sym("nelisp_jit_add2"), Sexp::Int(7), Sexp::Int(8)])
            .expect("add2 must succeed");
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
        let err = bi_nl_jit_call_i64_i64(&[sym("nelisp_jit_no_such"), Sexp::Int(0), Sexp::Int(0)])
            .expect_err("unknown name must error");
        assert!(matches!(err, EvalError::Internal(_)));
        let err = bi_nl_jit_call_i64_i64(&[sym("nelisp_jit_add2"), Sexp::Int(1)])
            .expect_err("arity 2 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
        let err = bi_nl_jit_call_i64_i64(&[Sexp::Int(0), Sexp::Int(1), Sexp::Int(2)])
            .expect_err("Int as name must error");
        assert!(matches!(err, EvalError::WrongType { .. }));
    }

    #[test]
    fn call_ptr_ptr_eq_inline() {
        let eq = |a, b| bi_nl_jit_call_ptr_ptr(&[sym("nelisp_jit_eq_inline"), a, b])
            .expect("eq_inline must succeed");
        assert_eq!(eq(Sexp::Int(7), Sexp::Int(7)), Sexp::Int(1));
        assert_eq!(eq(Sexp::Int(7), Sexp::Int(8)), Sexp::Int(0));
    }

    #[test]
    fn call_syscall_errors() {
        let err = bi_nl_jit_call_syscall(&vec![sym("nelisp_jit_syscall"); 7])
            .expect_err("arity 7 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
        let mut args = vec![sym("nelisp_jit_no_syscall")];
        args.extend(std::iter::repeat(Sexp::Int(0)).take(7));
        assert!(matches!(
            bi_nl_jit_call_syscall(&args).expect_err("unknown name must error"),
            EvalError::Internal(_)
        ));
    }

    #[test]
    fn call_out_1_car_cdr_length() {
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let car = bi_nl_jit_call_out_1(&[sym("nelisp_jit_car"), lst.clone()])
            .expect("car must succeed");
        assert_eq!(car, Sexp::Int(1));
        let cdr = bi_nl_jit_call_out_1(&[sym("nelisp_jit_cdr"), lst]).expect("cdr must succeed");
        assert_eq!(cdr, Sexp::list_from(&[Sexp::Int(2), Sexp::Int(3)]));
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let len = bi_nl_jit_call_out_1(&[sym("nelisp_jit_length"), v]).expect("length must succeed");
        assert_eq!(len, Sexp::Int(3));
    }

    #[test]
    fn call_out_1_wrong_type_errors() {
        let err = bi_nl_jit_call_out_1(&[sym("nelisp_jit_car"), Sexp::Int(7)])
            .expect_err("car of int must error");
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
        let r = bi_nl_jit_call_out_1i(&[sym("nelisp_jit_aref"), v.clone(), Sexp::Int(1)])
            .expect("aref in-range must succeed");
        assert_eq!(r, Sexp::Int(2));
        let err = bi_nl_jit_call_out_1i(&[sym("nelisp_jit_aref"), v.clone(), Sexp::Int(5)])
            .expect_err("aref out-of-range must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-1i");
                assert_eq!(got, v);
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }
}
