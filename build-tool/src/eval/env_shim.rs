//! `nelisp--env-globals-op OP NAME &optional ARG' dispatcher.  OP tags:
//! get/set-{value,function}, clear-{value,function}, is-{bound,fbound,constant},
//! set-constant, capture-lexical.  Backs 11 wrappers in
//! `lisp/nelisp-stdlib-env-shim.el'.  set-value bypasses constant-reject
//! (shim enforces); set-function never errors; clear-* no-op on absent.

use super::Env;
use super::error::EvalError;
use super::sexp::Sexp;

fn wrong_args(arity: &str, got: usize) -> EvalError {
    EvalError::WrongNumberOfArguments {
        function: "nelisp--env-globals-op".to_string(),
        expected: arity.to_string(),
        got,
    }
}

fn sym_arg(arg: &Sexp) -> Result<String, EvalError> {
    match arg {
        Sexp::Symbol(s) => Ok(s.clone()),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

fn bool_sexp(b: bool) -> Sexp {
    if b { Sexp::T } else { Sexp::Nil }
}

pub(crate) fn bi_globals_op(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let op = match args.first() {
        Some(Sexp::Symbol(s)) => s.as_str(),
        Some(o) => {
            return Err(EvalError::WrongType {
                expected: "symbolp".into(),
                got: o.clone(),
            });
        }
        None => return Err(wrong_args(">= 1", 0)),
    };
    let expected: usize = match op {
        "capture-lexical" => 1,
        "set-value" | "set-function" | "set-constant" => 3,
        _ => 2,
    };
    if args.len() != expected {
        return Err(wrong_args(&format!("{}", expected), args.len()));
    }
    if op == "capture-lexical" {
        return Ok(env.capture_lexical());
    }
    let name = sym_arg(&args[1])?;
    // `mirror_lookup_*' returns `unbound_marker' for absent; convert to
    // Unbound{Variable,Function} for the public Result surface.
    match op {
        "get-value" => {
            let v = env.mirror_lookup_value(&name);
            if v == env.unbound_marker {
                Err(EvalError::UnboundVariable(name))
            } else {
                Ok(v)
            }
        }
        "set-value" => {
            let v = args[2].clone();
            env.mirror_set_value(&name, v.clone());
            Ok(v)
        }
        "get-function" => {
            let f = env.mirror_lookup_function(&name);
            if f == env.unbound_marker {
                Err(EvalError::UnboundFunction(name))
            } else {
                Ok(f)
            }
        }
        "set-function" => {
            let def = args[2].clone();
            env.mirror_set_function(&name, def.clone());
            Ok(def)
        }
        "clear-value" => {
            env.mirror_clear_value(&name);
            Ok(args[1].clone())
        }
        "clear-function" => {
            env.mirror_clear_function(&name);
            Ok(args[1].clone())
        }
        "is-bound" => Ok(bool_sexp(env.mirror_is_bound(&name))),
        "is-fbound" => Ok(bool_sexp(env.mirror_is_fbound(&name))),
        "is-constant" => Ok(bool_sexp(env.mirror_is_constant(&name))),
        "set-constant" => {
            let truthy = !matches!(args[2], Sexp::Nil);
            env.mirror_set_constant(&name, truthy);
            Ok(bool_sexp(truthy))
        }
        other => Err(EvalError::Internal(format!(
            "nelisp--env-globals-op: unknown OP `{}`",
            other
        ))),
    }
}

/// Install `nelisp--env-globals-op' function-cell sentinel.  Called from
/// `Env::new_global' after `install_builtins' but before STDLIB load so
/// the shim file's 11 elisp wrappers can funcall it.  Idempotent.
pub fn install_env_shim_primitives(env: &mut Env) {
    let sentinel = Sexp::list_from(&[
        Sexp::Symbol("builtin".into()),
        Sexp::Symbol("nelisp--env-globals-op".into()),
    ]);
    env.set_function("nelisp--env-globals-op", sentinel);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{eval_str, eval_str_all};

    #[test]
    fn primitive_get_set_value_round_trip() {
        // Set then read back in one input so the same env services
        // both forms (= eval_str_all threads state across forms).
        let v = eval_str_all(
            "(nelisp--env-globals-set-value 'doc-86-3a-foo 42) \
             (nelisp--env-globals-get-value 'doc-86-3a-foo)",
        )
        .unwrap();
        assert!(matches!(v, Sexp::Int(42)));
    }

    #[test]
    fn primitive_get_value_unbound_signals() {
        let res = eval_str("(nelisp--env-globals-get-value 'doc-86-3a-undef-symbol)");
        assert!(res.is_err(), "expected void-variable on unbound get");
    }

    #[test]
    fn primitive_is_bound_reflects_set_value() {
        let before = eval_str("(nelisp--env-globals-is-bound 'doc-86-3a-bar)").unwrap();
        assert!(matches!(before, Sexp::Nil));
        let after = eval_str_all(
            "(nelisp--env-globals-set-value 'doc-86-3a-bar 7) \
             (nelisp--env-globals-is-bound 'doc-86-3a-bar)",
        )
        .unwrap();
        assert!(matches!(after, Sexp::T));
    }

    #[test]
    fn primitive_set_function_round_trip() {
        // Use `eq's function cell so we have a valid Sexp to install.
        let fb = eval_str_all(
            "(nelisp--env-globals-set-function 'doc-86-3a-fn \
                (nelisp--env-globals-get-function 'eq)) \
             (nelisp--env-globals-is-fbound 'doc-86-3a-fn)",
        )
        .unwrap();
        assert!(matches!(fb, Sexp::T));
        let cleared = eval_str_all(
            "(nelisp--env-globals-set-function 'doc-86-3a-fn2 \
                (nelisp--env-globals-get-function 'eq)) \
             (nelisp--env-globals-clear-function 'doc-86-3a-fn2) \
             (nelisp--env-globals-is-fbound 'doc-86-3a-fn2)",
        )
        .unwrap();
        assert!(matches!(cleared, Sexp::Nil));
    }

    #[test]
    fn primitive_constant_flag_round_trip() {
        let no = eval_str("(nelisp--env-globals-is-constant 'doc-86-3a-cflag)").unwrap();
        assert!(matches!(no, Sexp::Nil));
        let yes = eval_str_all(
            "(nelisp--env-globals-set-constant 'doc-86-3a-cflag t) \
             (nelisp--env-globals-is-constant 'doc-86-3a-cflag)",
        )
        .unwrap();
        assert!(matches!(yes, Sexp::T));
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
        let out = eval_str("(nelisp--env-globals-op 'capture-lexical)").unwrap();
        assert!(matches!(out, Sexp::Nil));
    }

    #[test]
    fn primitive_clear_value_drops_binding() {
        // After clear, is-bound returns nil.
        let v = eval_str_all(
            "(nelisp--env-globals-set-value 'doc-86-3a-baz 1) \
             (nelisp--env-globals-clear-value 'doc-86-3a-baz) \
             (nelisp--env-globals-is-bound 'doc-86-3a-baz)",
        )
        .unwrap();
        assert!(matches!(v, Sexp::Nil));
    }
}
