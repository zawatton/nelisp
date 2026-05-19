//! `nelisp--env-globals-op OP NAME &optional ARG' dispatcher (Phase 47).
//! 7 read/clear/pred arms handled by `nelisp_env_shim_op' (.o, Doc 86 §86.4).
//! set-{value,function,constant} and capture-lexical stay in Rust.

use super::error::EvalError;
use super::sexp::Sexp;
use super::Env;

/// Thin arg-check + Phase-47 call + error-mapping shim.
pub(crate) fn bi_globals_op(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let sym = |s: &Sexp| match s { Sexp::Symbol(n) => Ok(n.clone()), o => Err(EvalError::WrongType { expected: "symbolp".into(), got: o.clone() }) };
    let arity = |n: usize| EvalError::WrongNumberOfArguments { function: "nelisp--env-globals-op".to_string(), expected: n.to_string(), got: args.len() };
    let op = sym(args.first().ok_or_else(|| arity(1))?)?;
    let (exp, op) = match op.as_str() { "capture-lexical" => (1, op.as_str()), "set-value"|"set-function"|"set-constant" => (3, op.as_str()), _ => (2, op.as_str()) };
    if args.len() != exp { return Err(arity(exp)); }
    if op == "capture-lexical" { return Ok(env.capture_lexical()); }
    let name = sym(&args[1])?;
    match op {
        "set-value"    => { env.mirror_set_value(&name, args[2].clone());    return Ok(args[2].clone()); }
        "set-function" => { env.mirror_set_function(&name, args[2].clone()); return Ok(args[2].clone()); }
        "set-constant" => { let t = !matches!(args[2], Sexp::Nil); env.mirror_set_constant(&name, t); return Ok(if t { Sexp::T } else { Sexp::Nil }); }
        _ => {}
    }
    let (mut result, mut scratch) = (Sexp::Nil, Sexp::Nil);
    let rc = unsafe { crate::elisp_cc_spike::env_shim_op(&args[0], &env.globals_record, &args[1], &env.unbound_marker, &mut result, &mut scratch) };
    match rc { 1 => Ok(result), 0 => Err(EvalError::UnboundVariable(name)), -1 => Err(EvalError::UnboundFunction(name)), _ => Err(EvalError::Internal(format!("nelisp--env-globals-op: unknown OP `{op}'"))) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{eval_str, eval_str_all};

    fn eval_all_ok(src: &str) -> Sexp {
        eval_str_all(src).unwrap()
    }
    fn assert_t(v: Sexp) {
        assert!(matches!(v, Sexp::T));
    }
    fn assert_nil(v: Sexp) {
        assert!(matches!(v, Sexp::Nil));
    }

    #[test]
    fn primitive_get_set_value_round_trip() {
        // Set then read back in one input so the same env services
        // both forms (= eval_str_all threads state across forms).
        let v = eval_all_ok(
            "(nelisp--env-globals-set-value 'doc-86-3a-foo 42) \
             (nelisp--env-globals-get-value 'doc-86-3a-foo)",
        );
        assert!(matches!(v, Sexp::Int(42)));
    }

    #[test]
    fn primitive_get_value_unbound_signals() {
        let res = eval_str("(nelisp--env-globals-get-value 'doc-86-3a-undef-symbol)");
        assert!(res.is_err(), "expected void-variable on unbound get");
    }

    #[test]
    fn primitive_is_bound_reflects_set_value() {
        assert_nil(eval_str("(nelisp--env-globals-is-bound 'doc-86-3a-bar)").unwrap());
        assert_t(eval_all_ok(
            "(nelisp--env-globals-set-value 'doc-86-3a-bar 7) \
             (nelisp--env-globals-is-bound 'doc-86-3a-bar)",
        ));
    }

    #[test]
    fn primitive_set_function_round_trip() {
        // Use `eq's function cell so we have a valid Sexp to install.
        assert_t(eval_all_ok(
            "(nelisp--env-globals-set-function 'doc-86-3a-fn \
                (nelisp--env-globals-get-function 'eq)) \
             (nelisp--env-globals-is-fbound 'doc-86-3a-fn)",
        ));
        assert_nil(eval_all_ok(
            "(nelisp--env-globals-set-function 'doc-86-3a-fn2 \
                (nelisp--env-globals-get-function 'eq)) \
             (nelisp--env-globals-clear-function 'doc-86-3a-fn2) \
             (nelisp--env-globals-is-fbound 'doc-86-3a-fn2)",
        ));
    }

    #[test]
    fn primitive_constant_flag_round_trip() {
        assert_nil(eval_str("(nelisp--env-globals-is-constant 'doc-86-3a-cflag)").unwrap());
        assert_t(eval_all_ok(
            "(nelisp--env-globals-set-constant 'doc-86-3a-cflag t) \
             (nelisp--env-globals-is-constant 'doc-86-3a-cflag)",
        ));
    }

    #[test]
    fn primitive_capture_lexical_at_top_level_is_nil() {
        // No active frames → capture returns nil (= empty alist).
        // Doc 102 Phase 2.c: call the Rust primitive directly (= the
        // OP-tag form), bypassing the elisp `nelisp--env-globals-
        // capture-lexical' wrapper.  The wrapper IS a `defun' that
        // `push_frame's an empty closure-body frame around the call,
        // which `capture_lexical' would then walk — calling the OP
        // form keeps the test scoped to the Rust dispatcher's
        // zero-frame contract.
        assert_nil(eval_str("(nelisp--env-globals-op 'capture-lexical)").unwrap());
    }

    #[test]
    fn primitive_clear_value_drops_binding() {
        // After clear, is-bound returns nil.
        assert_nil(eval_all_ok(
            "(nelisp--env-globals-set-value 'doc-86-3a-baz 1) \
             (nelisp--env-globals-clear-value 'doc-86-3a-baz) \
             (nelisp--env-globals-is-bound 'doc-86-3a-baz)",
        ));
    }
}
