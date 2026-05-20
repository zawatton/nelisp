pub(crate) const TRAMPOLINE_OK: i64 = 0;
pub(crate) const TRAMPOLINE_ERR: i64 = 1;
use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;
use std::ffi::CString;
#[allow(dead_code, clashing_extern_declarations)]
extern "C" {
    fn nelisp_jit_add2(); fn nelisp_jit_sub2(); fn nelisp_jit_mul2(); fn nelisp_jit_eq2();
    fn nelisp_jit_lt2(); fn nelisp_jit_gt2(); fn nelisp_jit_le2(); fn nelisp_jit_ge2();
    fn nelisp_jit_logior2(); fn nelisp_jit_logand2(); fn nelisp_jit_logxor2(); fn nelisp_jit_ash();
    fn nl_jit_float_add(); fn nl_jit_float_sub(); fn nl_jit_float_mul(); fn nl_jit_float_div();
    fn nl_jit_float_lt(); fn nl_jit_float_gt(); fn nl_jit_float_le(); fn nl_jit_float_ge();
    fn nl_jit_float_eq_eps(); fn nl_jit_float_float(); fn nl_jit_float_exp(); fn nl_jit_float_log();
    fn nl_jit_intern(); fn nl_jit_make_mut_str(); fn nl_jit_mut_str_len(); fn nl_jit_record_alloc();
    fn nelisp_jit_predicate_eq(); fn nelisp_jit_ref_eq(); fn nelisp_jit_record_type(); fn nelisp_jit_record_len();
    fn nelisp_jit_record_ref(); fn nelisp_jit_record_set(); fn nelisp_jit_length(); fn nelisp_jit_aref();
    fn nelisp_jit_aset(); fn nelisp_jit_elt(); fn nelisp_jit_cons_car(); fn nelisp_jit_cons_cdr();
    fn nelisp_jit_cons_setcar(); fn nelisp_jit_cons_setcdr(); fn nelisp_jit_cons_make();
    fn nl_jit_bool_vector_len(); fn nl_jit_str_codepoint_at(); fn nl_jit_type_of(); fn nl_jit_sxhash();
    fn nl_cons_car_ptr(); fn nl_cons_cdr_ptr(); fn nl_jit_make_symbol(); fn nl_record_type_tag_ptr();
    fn nl_jit_concat_ints(); fn nl_sf_quote(); fn nl_jit_downcase(); fn nl_jit_upcase();
    fn nl_jit_split_by_non_alnum(); fn nl_sf_if(); fn nl_sf_setq(); fn nl_sf_progn(); fn nl_sf_while();
    fn nl_sf_lambda(); fn nl_sf_function(); fn nl_sf_condition_case();
    fn nl_jit_symbol_name(); fn nelisp_bi_nl_fact_i64(); fn nl_cons_prepend_clone();
    fn nl_jit_secure_hash(); fn nl_jit_secure_hash_non_sha1_ext();
    fn nl_jit_string_match_p(); fn nl_eval_inner(); fn nl_jit_alias();
    fn nl_jit_syscall_call(); fn nl_jit_syscall_supported_p();
    fn nl_jit_char_table_aref(); fn nl_jit_char_table_aset(); fn nl_jit_mut_str_set_codepoint();
    #[link_name = "nl_jit_alias"]
    fn nl_jit_alias_call(name: *const Sexp, out: *mut Sexp) -> i64;
}

#[used]
static _ELISP_ARCHIVE_ANCHOR: [unsafe extern "C" fn(); 76] = [
    nelisp_jit_add2, nelisp_jit_sub2, nelisp_jit_mul2, nelisp_jit_eq2,
    nelisp_jit_lt2, nelisp_jit_gt2, nelisp_jit_le2, nelisp_jit_ge2,
    nelisp_jit_logior2, nelisp_jit_logand2, nelisp_jit_logxor2, nelisp_jit_ash,
    nl_jit_float_add, nl_jit_float_sub, nl_jit_float_mul, nl_jit_float_div,
    nl_jit_float_lt, nl_jit_float_gt, nl_jit_float_le, nl_jit_float_ge,
    nl_jit_float_eq_eps, nl_jit_float_float, nl_jit_float_exp, nl_jit_float_log,
    nl_jit_intern, nl_jit_make_mut_str, nl_jit_mut_str_len, nl_jit_record_alloc,
    nelisp_jit_predicate_eq, nelisp_jit_ref_eq, nelisp_jit_record_type, nelisp_jit_record_len,
    nelisp_jit_record_ref, nelisp_jit_record_set, nelisp_jit_length, nelisp_jit_aref,
    nelisp_jit_aset, nelisp_jit_elt, nelisp_jit_cons_car, nelisp_jit_cons_cdr,
    nelisp_jit_cons_setcar, nelisp_jit_cons_setcdr, nelisp_jit_cons_make,
    nl_jit_bool_vector_len, nl_jit_str_codepoint_at, nl_jit_type_of, nl_jit_sxhash,
    nl_cons_car_ptr, nl_cons_cdr_ptr, nl_jit_make_symbol, nl_record_type_tag_ptr,
    nl_jit_concat_ints, nl_sf_quote, nl_jit_downcase, nl_jit_upcase,
    nl_jit_split_by_non_alnum, nl_sf_if, nl_sf_setq, nl_sf_progn, nl_sf_while,
    nl_sf_lambda, nl_sf_function, nl_sf_condition_case, nl_jit_symbol_name,
    nelisp_bi_nl_fact_i64, nl_cons_prepend_clone, nl_jit_secure_hash,
    nl_jit_secure_hash_non_sha1_ext, nl_jit_string_match_p, nl_jit_alias, nl_eval_inner,
    nl_jit_syscall_call, nl_jit_syscall_supported_p,
    nl_jit_char_table_aref, nl_jit_char_table_aset, nl_jit_mut_str_set_codepoint,
];
pub(super) fn unified_fn_ptr(sexp: &Sexp) -> Option<*const u8> {
    let mut resolved = Sexp::Nil;
    let rc = unsafe { nl_jit_alias_call(sexp as *const _, &mut resolved as *mut _) };
    if rc != 0 { return None; }
    let name = match &resolved { Sexp::Str(s) => s.as_str(), _ => return None };
    let cstr = CString::new(name).ok()?;
    let addr = unsafe { libc::dlsym(libc::RTLD_DEFAULT, cstr.as_ptr()) };
    if addr.is_null() { None } else { Some(addr as *const u8) }
}
fn jit_lookup(prim: &str, args: &[Sexp], arity: usize) -> Result<*const u8, EvalError> {
    require_arity(prim, args, arity, Some(arity))?;
    let name_sexp = &args[0];
    let name_str = match name_sexp { Sexp::Symbol(s) | Sexp::Str(s) => s.as_str(),
        other => return Err(EvalError::wrong_type(format!("symbol or string ({} arg 0)", prim), other.clone())) };
    unified_fn_ptr(name_sexp).ok_or_else(|| EvalError::internal(format!("{}: unknown JIT entry name `{}'", prim, name_str)))
}
unsafe fn cast<T: Copy>(p: *const u8) -> T { std::mem::transmute_copy(&p) }
fn num_to_f64(v: &Sexp) -> Result<f64, EvalError> { match v { Sexp::Int(n) => Ok(*n as f64), Sexp::Float(x) => Ok(*x), other => Err(EvalError::wrong_type("numberp", other.clone())) } }
fn to_f64(v: &Sexp, expected: &str) -> Result<f64, EvalError> { match v { Sexp::Int(i) => Ok(*i as f64), Sexp::Float(f) => Ok(*f), Sexp::Nil => Ok(0.0), other => Err(EvalError::wrong_type(expected, other.clone())) } }
fn float_pair(args: &[Sexp], name: &str) -> Result<(*const u8, f64, f64), EvalError> { let p = jit_lookup(name, args, 3)?; Ok((p, num_to_f64(&args[1])?, num_to_f64(&args[2])?)) }
fn out_result(rc: i64, out: Sexp, prim: &str, arg: &Sexp) -> Result<Sexp, EvalError> { if rc == TRAMPOLINE_OK { Ok(out) } else { Err(EvalError::wrong_type(prim, arg.clone())) } }
pub fn bi_nl_jit_call_i64_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-i64-i64", args, 3)?;
    let (a, b) = (as_int("nl-jit-call-i64-i64", &args[1])?, as_int("nl-jit-call-i64-i64", &args[2])?);
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
    for (i, slot) in ints.iter_mut().enumerate() { *slot = as_int("nl-jit-call-syscall", &args[i + 1])?; }
    let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 = unsafe { cast(p) };
    Ok(Sexp::Int(f(ints[0], ints[1], ints[2], ints[3], ints[4], ints[5], ints[6])))
}
pub fn bi_nl_jit_call_float_float(args: &[Sexp]) -> Result<Sexp, EvalError> { let (p,a,b)=float_pair(args,"nl-jit-call-float-float")?; let f:extern "C" fn(f64,f64)->f64=unsafe{cast(p)}; Ok(Sexp::Float(f(a,b))) }
pub fn bi_nl_jit_call_float_cmp(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-cmp")?;
    let f: extern "C" fn(f64, f64) -> i64 = unsafe { cast(p) }; Ok(Sexp::Int(f(a, b)))
}
pub fn bi_nl_jit_call_float_unary(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-float-unary", args, 2)?;
    let f: extern "C" fn(f64) -> f64 = unsafe { cast(p) };
    Ok(Sexp::Float(f(to_f64(&args[1], "number")?)))
}
macro_rules! out_call {
    ($fn:ident, $tag:literal, $n:literal, |$a:ident,$o:ident $(,$i:ident)?|,
     ($($t:ty),+), ($($e:expr),+ $(,)?)) => {
        pub fn $fn($a: &[Sexp]) -> Result<Sexp, EvalError> {
            let p = jit_lookup(concat!("nl-", $tag), $a, $n)?;
            $(let $i = as_int(concat!("nl-", $tag), &$a[2])?;)?
            let f: extern "C" fn($($t),+) -> i64 = unsafe { cast(p) };
            let mut $o = Sexp::Nil;
            out_result(f($($e),+), $o, $tag, &$a[1])
        }
    };
}
out_call!(bi_nl_jit_call_out_1, "jit-call-out-1", 2, |args,out|,
    (*const Sexp, *mut Sexp), (&args[1] as *const _, &mut out as *mut _));
out_call!(bi_nl_jit_call_out_2, "jit-call-out-2", 3, |args,out|,
    (*const Sexp, *const Sexp, *mut Sexp),
    (&args[1] as *const _, &args[2] as *const _, &mut out as *mut _));
out_call!(bi_nl_jit_call_out_1i, "jit-call-out-1i", 3, |args,out,idx|,
    (*const Sexp, i64, *mut Sexp),
    (&args[1] as *const _, idx, &mut out as *mut _));
out_call!(bi_nl_jit_call_out_2i, "jit-call-out-2i", 4, |args,out,idx|,
    (*const Sexp, i64, *const Sexp, *mut Sexp),
    (&args[1] as *const _, idx, &args[3] as *const _, &mut out as *mut _));
pub fn bi_nl_jit_call_format_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-format-float", args, 4)?;
    let x = to_f64(&args[1], "numberp")?;
    let (conv, prec) = (as_int("nl-jit-call-format-float", &args[2])?, as_int("nl-jit-call-format-float", &args[3])?);
    let f: extern "C" fn(f64, u32, i64, *mut Sexp) -> i64 = unsafe { cast(p) };
    let mut out = Sexp::Nil;
    out_result(f(x, conv as u32, prec, &mut out as *mut _), out, "jit-call-format-float", &args[1])
}
#[cfg(test)]
mod tests {
    use super::*;
    fn sym(name: &str) -> Sexp { Sexp::Symbol(name.into()) }
    fn is_err(e: &EvalError, tag: &str) -> bool { matches!(e, EvalError::Generic(t, _) if t == tag) }
    #[test]
    fn unified_fn_ptr_resolves_core_entries() {
        for n in ["nelisp_jit_add2","nelisp_jit_eq_inline","nelisp_jit_car",
                  "nelisp_jit_length","nelisp_jit_aref","nelisp_jit_intern",
                  "nelisp_jit_syscall","nl_jit_float_add","nl_jit_float_exp"] {
            let p = unified_fn_ptr(&sym(n));
            assert!(p.is_some(), "missing `{}'", n);
            assert!(!p.unwrap().is_null(), "`{}' is null", n);
        }
    }
    #[test]
    fn unified_fn_ptr_unknown_returns_none() {
        assert!(unified_fn_ptr(&sym("nelisp_jit_does_not_exist")).is_none());
        assert!(unified_fn_ptr(&sym("")).is_none());
    }
    #[test]
    fn call_i64_i64_smoke() {
        assert_eq!(bi_nl_jit_call_i64_i64(&[sym("nelisp_jit_add2"),Sexp::Int(7),Sexp::Int(8)]).expect("add2"), Sexp::Int(15));
        assert_eq!(bi_nl_jit_call_i64_i64(&[Sexp::Str("nelisp_jit_mul2".into()),Sexp::Int(6),Sexp::Int(7)]).expect("mul2"), Sexp::Int(42));
        assert!(is_err(&bi_nl_jit_call_i64_i64(&[sym("nelisp_jit_no_such"),Sexp::Int(0),Sexp::Int(0)]).unwrap_err(), "error"));
        assert!(is_err(&bi_nl_jit_call_i64_i64(&[sym("nelisp_jit_add2"),Sexp::Int(1)]).unwrap_err(), "wrong-number-of-arguments"));
        assert!(is_err(&bi_nl_jit_call_i64_i64(&[Sexp::Int(0),Sexp::Int(1),Sexp::Int(2)]).unwrap_err(), "wrong-type-argument"));
    }
    #[test]
    fn call_ptr_ptr_eq_inline() {
        let eq = |a,b| bi_nl_jit_call_ptr_ptr(&[sym("nelisp_jit_eq_inline"),a,b]).expect("eq_inline");
        assert_eq!(eq(Sexp::Int(7),Sexp::Int(7)),Sexp::Int(1));
        assert_eq!(eq(Sexp::Int(7),Sexp::Int(8)),Sexp::Int(0));
    }
    #[test]
    fn call_syscall_errors() {
        assert!(is_err(&bi_nl_jit_call_syscall(&vec![sym("nelisp_jit_syscall");7]).unwrap_err(), "wrong-number-of-arguments"));
        let mut args = vec![sym("nelisp_jit_no_syscall")];
        args.extend(std::iter::repeat(Sexp::Int(0)).take(7));
        assert!(is_err(&bi_nl_jit_call_syscall(&args).unwrap_err(), "error"));
    }
    #[test]
    fn call_out_1_ops() {
        let lst = Sexp::list_from(&[Sexp::Int(1),Sexp::Int(2),Sexp::Int(3)]);
        assert_eq!(bi_nl_jit_call_out_1(&[sym("nelisp_jit_car"),lst.clone()]).expect("car"), Sexp::Int(1));
        assert_eq!(bi_nl_jit_call_out_1(&[sym("nelisp_jit_cdr"),lst]).expect("cdr"), Sexp::list_from(&[Sexp::Int(2),Sexp::Int(3)]));
        assert_eq!(bi_nl_jit_call_out_1(&[sym("nelisp_jit_length"),Sexp::vector(vec![Sexp::Int(1),Sexp::Int(2),Sexp::Int(3)])]).expect("len"), Sexp::Int(3));
        let err = bi_nl_jit_call_out_1(&[sym("nelisp_jit_car"),Sexp::Int(7)]).unwrap_err();
        if let EvalError::Generic(ref tag, ref data) = err { if tag == "wrong-type-argument" {
            let elems: Vec<_> = crate::eval::list_elements(data).expect("data is list");
            assert_eq!(elems.get(0), Some(&Sexp::Symbol("jit-call-out-1".into())));
            assert_eq!(elems.get(1), Some(&Sexp::Int(7)));
            return;
        }} panic!("expected wrong-type-argument, got {:?}", err);
    }
    #[test]
    fn call_out_1i_aref() {
        let v = Sexp::vector(vec![Sexp::Int(1),Sexp::Int(2),Sexp::Int(3)]);
        assert_eq!(bi_nl_jit_call_out_1i(&[sym("nelisp_jit_aref"),v.clone(),Sexp::Int(1)]).expect("aref"), Sexp::Int(2));
        let err = bi_nl_jit_call_out_1i(&[sym("nelisp_jit_aref"),v.clone(),Sexp::Int(5)]).unwrap_err();
        if let EvalError::Generic(ref tag, ref data) = err { if tag == "wrong-type-argument" {
            let elems: Vec<_> = crate::eval::list_elements(data).expect("data is list");
            assert_eq!(elems.get(0), Some(&Sexp::Symbol("jit-call-out-1i".into())));
            assert_eq!(elems.get(1), Some(&v));
            return;
        }} panic!("expected wrong-type-argument, got {:?}", err);
    }
}
