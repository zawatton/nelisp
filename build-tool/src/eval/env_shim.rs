//! Tier 0 env shim — globals-only access primitive (Doc 86 §86.3 →
//! Doc 102 Phase 2.c, 2026-05-13).
//!
//! Exposes ONE generic Rust primitive — `nelisp--env-globals-op OP
//! NAME &optional ARG' — that wraps the canonical `Env::globals'
//! HashMap so the elisp shim (`lisp/nelisp-stdlib-env-shim.el') can
//! read / write the global environment without bypassing the
//! canonical Rust state.  Doc 102 Phase 2.c consolidated the
//! pre-existing 11 individual `bi_*' primitives into this single
//! dispatcher; the 11 user-visible names (`nelisp--env-globals-
//! get-value' / `set-value' / …) live as elisp `defun's that
//! delegate via the matching OP tag.
//!
//! OP tags + arity (NAME omitted = capture-lexical, otherwise NAME is
//! a symbol; ARG required for the 3 `set-*' ops):
//!   get-value / set-value          — value cell read / write
//!   get-function / set-function    — function cell read / write
//!   clear-value / clear-function   — drop the cell (= makunbound /
//!                                    fmakunbound)
//!   is-bound / is-fbound           — t / nil predicates (globals only)
//!   is-constant / set-constant     — constant flag predicate / setter
//!   capture-lexical                — snapshot frames as alist (0-arg)
//!
//! Set-value bypasses the constant-rejection path (= elisp shim
//! enforces); set-function never errors on existing entries; clear-*
//! is a no-op when the entry is absent.

use super::env::{Env, SymbolEntry};
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

/// Generic `nelisp--env-globals-op OP NAME &optional ARG' dispatcher;
/// see module doc for OP tag semantics.
fn bi_globals_op(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
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
    // Doc 102 Phase 8 sprint Session 5 — reads route through Rust-direct
    // `Env::mirror_lookup_*' (no `apply_function' detour); writes dual-
    // write to the elisp mirror via `env.mirror_set_*' so elisp-driven
    // mutations stay in sync.  Both paths fall back to the Rust HashMap
    // during early bootstrap (= globals_record still Sexp::Nil).
    match op {
        "get-value" => env
            .mirror_lookup_value(&name)
            .or_else(|| env.globals.get(&name).and_then(|e| e.value.clone()))
            .ok_or(EvalError::UnboundVariable(name)),
        "set-value" => {
            let v = args[2].clone();
            env.globals.entry(name.clone()).or_default().value = Some(v.clone());
            env.mirror_set_value(&name, v.clone());
            Ok(v)
        }
        "get-function" => env
            .mirror_lookup_function(&name)
            .or_else(|| env.globals.get(&name).and_then(|e| e.function.clone()))
            .ok_or(EvalError::UnboundFunction(name)),
        "set-function" => {
            let def = args[2].clone();
            env.globals.entry(name.clone()).or_default().function = Some(def.clone());
            env.mirror_set_function(&name, def.clone());
            Ok(def)
        }
        "clear-value" => {
            if let Some(e) = env.globals.get_mut(&name) {
                e.value = None;
            }
            // Doc 102 Phase 8 Sprint Session 6 — sprint-introduced
            // mirror was missing the clear path; `is-bound' below
            // checks mirror first and would otherwise still see the
            // entry.
            env.mirror_clear_value(&name);
            Ok(args[1].clone())
        }
        "clear-function" => {
            if let Some(e) = env.globals.get_mut(&name) {
                e.function = None;
            }
            env.mirror_clear_function(&name);
            Ok(args[1].clone())
        }
        "is-bound" => Ok(bool_sexp(
            env.mirror_is_bound(&name)
                || matches!(env.globals.get(&name), Some(SymbolEntry { value: Some(_), .. })),
        )),
        "is-fbound" => Ok(bool_sexp(
            env.mirror_is_fbound(&name)
                || matches!(env.globals.get(&name), Some(SymbolEntry { function: Some(_), .. })),
        )),
        "is-constant" => Ok(bool_sexp(matches!(
            env.globals.get(&name),
            Some(SymbolEntry { constant: true, .. })
        ))),
        "set-constant" => {
            let truthy = !matches!(args[2], Sexp::Nil);
            env.globals.entry(name).or_default().constant = truthy;
            Ok(bool_sexp(truthy))
        }
        other => Err(EvalError::Internal(format!(
            "nelisp--env-globals-op: unknown OP `{}`",
            other
        ))),
    }
}

/// Install the Doc 102 Phase 2.c generic env-globals dispatcher into
/// `env`.  Idempotent — re-installing overwrites the previous closure
/// (= same contract as `Env::register_extern_builtin`).
///
/// Called from `Env::new_global` after `install_builtins` has run but
/// before `STDLIB_IMAGES` are loaded, so the shim file
/// (`nelisp-stdlib-env-shim.el`) can `funcall` `nelisp--env-globals-op`
/// at load time (= when its 11 elisp wrappers' bodies expand).
pub fn install_env_shim_primitives(env: &mut Env) {
    env.register_extern_builtin("nelisp--env-globals-op", bi_globals_op);
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
