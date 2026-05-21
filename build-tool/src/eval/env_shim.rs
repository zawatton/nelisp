use super::error::EvalError;
use super::sexp::Sexp;
use super::Env;
pub(crate) fn bi_globals_op(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let sym = |s: &Sexp| match s { Sexp::Symbol(n) => Ok(n.clone()), o => Err(EvalError::wrong_type("symbolp", o.clone())) };
    let arity = |n: usize| EvalError::wrong_arity("nelisp--env-globals-op", n.to_string(), args.len());
    let op = sym(args.first().ok_or_else(|| arity(1))?)?;
    let (exp, op) = match op.as_str() { "capture-lexical" => (1, op.as_str()), "set-value"|"set-function"|"set-constant" => (3, op.as_str()), _ => (2, op.as_str()) };
    if args.len() != exp { return Err(arity(exp)); }
    if op == "capture-lexical" { return Ok(env.capture_lexical()); }
    let name = sym(&args[1])?;
    let (mut result, mut scratch) = (Sexp::Nil, Sexp::Nil);
    let rc = if op == "set-value" || op == "set-function" || op == "set-constant" {
        let flag = if matches!(args[2], Sexp::Nil) { Sexp::Nil } else { Sexp::T };
        let sc = crate::elisp_cc_spike::build_or_insert_scratch_vec(if op == "set-value" { args[2].clone() } else { env.unbound_marker.clone() }, if op == "set-function" { args[2].clone() } else { env.unbound_marker.clone() }, Sexp::Nil, if op == "set-constant" { flag } else { Sexp::Nil });
        unsafe { crate::elisp_cc_spike::env_shim_set_op(&args[0], &env.globals_record, &args[1], &sc, &mut result, 0) }
    } else { unsafe { crate::elisp_cc_spike::env_shim_op(&args[0], &env.globals_record, &args[1], &env.unbound_marker, &mut result, &mut scratch) } };
    match rc { 1 => Ok(result), 0 => Err(EvalError::unbound_var(name)), -1 => Err(EvalError::unbound_fn(name)), _ => Err(EvalError::internal(format!("nelisp--env-globals-op: unknown OP `{op}'"))) }
}
#[cfg(test)] mod tests { use super::*; use crate::eval::{eval_str, eval_str_all};
    macro_rules! ok { ($e:expr) => { eval_str_all($e).unwrap() }; ($s:literal => $p:pat) => { assert!(matches!(eval_str($s).unwrap(), $p)) }; ($s:literal => all => $p:pat) => { assert!(matches!(eval_str_all($s).unwrap(), $p)) }; }
    #[test] fn set_get_value() { assert!(matches!(ok!("(nelisp--env-globals-set-value 'doc-86-3a-foo 42) (nelisp--env-globals-get-value 'doc-86-3a-foo)"), Sexp::Int(42))); } #[test] fn get_unbound_errors() { assert!(eval_str("(nelisp--env-globals-get-value 'doc-86-3a-undef-symbol)").is_err()); }
    #[test] fn is_bound_reflects_set() { ok!("(nelisp--env-globals-is-bound 'doc-86-3a-bar)" => Sexp::Nil); ok!("(nelisp--env-globals-set-value 'doc-86-3a-bar 7) (nelisp--env-globals-is-bound 'doc-86-3a-bar)" => all => Sexp::T); }
    #[test] fn set_function_round_trip() { ok!("(nelisp--env-globals-set-function 'doc-86-3a-fn (nelisp--env-globals-get-function 'eq)) (nelisp--env-globals-is-fbound 'doc-86-3a-fn)" => all => Sexp::T); ok!("(nelisp--env-globals-set-function 'doc-86-3a-fn2 (nelisp--env-globals-get-function 'eq)) (nelisp--env-globals-clear-function 'doc-86-3a-fn2) (nelisp--env-globals-is-fbound 'doc-86-3a-fn2)" => all => Sexp::Nil); }
    #[test] fn constant_flag_round_trip() { ok!("(nelisp--env-globals-is-constant 'doc-86-3a-cflag)" => Sexp::Nil); ok!("(nelisp--env-globals-set-constant 'doc-86-3a-cflag t) (nelisp--env-globals-is-constant 'doc-86-3a-cflag)" => all => Sexp::T); } #[test] fn capture_and_clear() { ok!("(nelisp--env-globals-op 'capture-lexical)" => Sexp::Nil); ok!("(nelisp--env-globals-set-value 'doc-86-3a-baz 1) (nelisp--env-globals-clear-value 'doc-86-3a-baz) (nelisp--env-globals-is-bound 'doc-86-3a-baz)" => all => Sexp::Nil); } }
