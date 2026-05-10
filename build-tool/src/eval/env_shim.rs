//! Doc 86 §86.3.a / Doc 89 Option C — Tier 0 env shim primitives.
//!
//! This module exposes 11 slim primitives that wrap the canonical Rust
//! `Env::globals` HashMap so the elisp shim
//! (`lisp/nelisp-stdlib-env-shim.el`) can read / write the global
//! environment without bypassing the canonical Rust state.
//!
//! Why a separate module: Doc 86 §86.3 builds toward an elisp-side
//! `nelisp--global-env` mirror that — over §86.3.b shadow path and
//! §86.3.c switch-over — replaces direct `Env::globals` access from
//! `eval/mod.rs`, `eval/special_forms.rs`, and `eval/builtins.rs`
//! Tier 3 dispatch arms.  The precursor stage (= §86.3.a) only adds
//! the slim primitives + an empty elisp shim file; existing callsites
//! remain on the Tier 0 fast path.
//!
//! Why `register_extern_builtin` (= not `install_builtins` + `dispatch`):
//! `eval/builtins.rs` is being concurrently edited by the Doc 86 §86.1.b
//! agent (= `install_builtins` names table + dispatch arm).  Routing
//! these primitives through the extern-builtin registry avoids a
//! merge collision while reusing the same `(builtin NAME)` sentinel
//! plumbing — `Env::register_extern_builtin` automatically writes the
//! sentinel into the function cell, so elisp `(funcall NAME ARG...)`
//! sees them as ordinary builtins.
//!
//! Naming convention: each primitive uses the `nelisp--env-globals-`
//! prefix to make it clear they touch the canonical `Env::globals`
//! HashMap directly (= bypass the lexical frame stack, no `setq`
//! semantics).  The elisp shim's user-visible API (= `boundp` /
//! `fboundp` / `defvar` etc.) is layered on top in §86.3.b.
//!
//! Per Doc 89 §3.2 / §3.3 the 11 primitives are:
//!
//! | primitive                            | role                      |
//! |--------------------------------------+---------------------------|
//! | `nelisp--env-globals-get-value`      | `lookup_value` (globals)  |
//! | `nelisp--env-globals-set-value`      | `set_value` (globals)     |
//! | `nelisp--env-globals-get-function`   | `lookup_function`         |
//! | `nelisp--env-globals-set-function`   | `set_function`            |
//! | `nelisp--env-globals-clear-value`    | `makunbound`              |
//! | `nelisp--env-globals-clear-function` | `fmakunbound`             |
//! | `nelisp--env-globals-is-bound`       | `boundp`                  |
//! | `nelisp--env-globals-is-fbound`      | `fboundp`                 |
//! | `nelisp--env-globals-is-constant`    | constant flag check       |
//! | `nelisp--env-globals-set-constant`   | `defconst` constant flag  |
//! | `nelisp--env-globals-capture-lexical`| capture lexical scope     |
//!
//! NB: `set_value` here is the **globals-only** variant — it never
//! walks lexical frames, never errors on constants in the elisp shim
//! (= constant rejection is the shim's job, not the primitive's), and
//! always writes to the global value cell.  This matches the Doc 89
//! §6 chicken-and-egg gate: the shim itself must be installable
//! before higher-level semantics (`setq` / `defvar`) come online.

use super::env::{Env, SymbolEntry};
use super::error::EvalError;
use super::sexp::Sexp;

/// Argument-arity helper.  Inlined here (= not pulled from
/// `builtins::require_arity`) to avoid a cross-module dependency on
/// the Doc 86 §86.1.b agent's edits.
fn check_arity(name: &str, args: &[Sexp], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongNumberOfArguments {
            function: name.to_string(),
            expected: format!("{}", expected),
            got: args.len(),
        });
    }
    Ok(())
}

/// Extract a symbol name from `arg`, signalling `wrong-type-argument`
/// otherwise.  Used by every primitive below.
fn sym_arg(arg: &Sexp) -> Result<String, EvalError> {
    match arg {
        Sexp::Symbol(s) => Ok(s.clone()),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

/// `(nelisp--env-globals-get-value SYMBOL)` — return the value cell
/// of SYMBOL from the canonical `Env::globals` map.  Signals
/// `void-variable` if the symbol has no value cell.  Does NOT walk
/// lexical frames — the precursor shim is globals-only.
fn bi_get_value(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-get-value", args, 1)?;
    let name = sym_arg(&args[0])?;
    match env.globals.get(&name) {
        Some(SymbolEntry { value: Some(v), .. }) => Ok(v.clone()),
        _ => Err(EvalError::UnboundVariable(name)),
    }
}

/// `(nelisp--env-globals-set-value SYMBOL VALUE)` — overwrite the
/// global value cell of SYMBOL with VALUE.  Returns VALUE.  Bypasses
/// the constant-rejection path (= caller's responsibility).  Creates
/// the entry if missing.
fn bi_set_value(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-set-value", args, 2)?;
    let name = sym_arg(&args[0])?;
    let value = args[1].clone();
    let entry = env
        .globals
        .entry(name)
        .or_insert_with(SymbolEntry::new);
    entry.value = Some(value.clone());
    Ok(value)
}

/// `(nelisp--env-globals-get-function SYMBOL)` — return the function
/// cell of SYMBOL.  Signals `void-function` when missing.
fn bi_get_function(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-get-function", args, 1)?;
    let name = sym_arg(&args[0])?;
    match env.globals.get(&name) {
        Some(SymbolEntry {
            function: Some(f), ..
        }) => Ok(f.clone()),
        _ => Err(EvalError::UnboundFunction(name)),
    }
}

/// `(nelisp--env-globals-set-function SYMBOL DEF)` — overwrite the
/// global function cell.  Returns DEF.  Creates the entry if missing.
fn bi_set_function(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-set-function", args, 2)?;
    let name = sym_arg(&args[0])?;
    let def = args[1].clone();
    let entry = env
        .globals
        .entry(name)
        .or_insert_with(SymbolEntry::new);
    entry.function = Some(def.clone());
    Ok(def)
}

/// `(nelisp--env-globals-clear-value SYMBOL)` — drop the global value
/// cell.  Returns SYMBOL.  No-op when the entry is absent.
fn bi_clear_value(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-clear-value", args, 1)?;
    let name = sym_arg(&args[0])?;
    if let Some(entry) = env.globals.get_mut(&name) {
        entry.value = None;
    }
    Ok(args[0].clone())
}

/// `(nelisp--env-globals-clear-function SYMBOL)` — drop the global
/// function cell.  Returns SYMBOL.
fn bi_clear_function(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-clear-function", args, 1)?;
    let name = sym_arg(&args[0])?;
    if let Some(entry) = env.globals.get_mut(&name) {
        entry.function = None;
    }
    Ok(args[0].clone())
}

/// `(nelisp--env-globals-is-bound SYMBOL)` — t iff the global value
/// cell is set.  Does NOT walk lexical frames (= precursor is
/// globals-only; full `boundp` = §86.3.b).
fn bi_is_bound(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-is-bound", args, 1)?;
    let name = sym_arg(&args[0])?;
    let bound = matches!(
        env.globals.get(&name),
        Some(SymbolEntry { value: Some(_), .. })
    );
    Ok(if bound { Sexp::T } else { Sexp::Nil })
}

/// `(nelisp--env-globals-is-fbound SYMBOL)` — t iff the global
/// function cell is set.
fn bi_is_fbound(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-is-fbound", args, 1)?;
    let name = sym_arg(&args[0])?;
    let bound = matches!(
        env.globals.get(&name),
        Some(SymbolEntry {
            function: Some(_),
            ..
        })
    );
    Ok(if bound { Sexp::T } else { Sexp::Nil })
}

/// `(nelisp--env-globals-is-constant SYMBOL)` — t iff SYMBOL is
/// flagged constant (= `nil` / `t` / keyword / `defconst`'d).
fn bi_is_constant(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-is-constant", args, 1)?;
    let name = sym_arg(&args[0])?;
    let cflag = matches!(
        env.globals.get(&name),
        Some(SymbolEntry { constant: true, .. })
    );
    Ok(if cflag { Sexp::T } else { Sexp::Nil })
}

/// `(nelisp--env-globals-set-constant SYMBOL FLAG)` — set the
/// constant flag.  FLAG is treated as truthy = constant, nil =
/// non-constant.  Returns FLAG (= the truthy / nil value, normalised).
/// Creates the entry if missing.  Used by `defconst' shim.
fn bi_set_constant(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-set-constant", args, 2)?;
    let name = sym_arg(&args[0])?;
    let truthy = !matches!(args[1], Sexp::Nil);
    let entry = env
        .globals
        .entry(name)
        .or_insert_with(SymbolEntry::new);
    entry.constant = truthy;
    Ok(if truthy { Sexp::T } else { Sexp::Nil })
}

/// `(nelisp--env-globals-capture-lexical)` — snapshot the current
/// lexical frame stack as a flat alist `((NAME . CELL) ...)` so a
/// `lambda` body can keep its closure environment as plain Sexp data.
/// Equivalent to `Env::capture_lexical` exposed for the elisp shim.
fn bi_capture_lexical(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    check_arity("nelisp--env-globals-capture-lexical", args, 0)?;
    Ok(env.capture_lexical())
}

/// Install all 11 Tier 0 env shim primitives into `env`.  Idempotent
/// — re-installing overwrites the previous closures (= same contract
/// as `Env::register_extern_builtin`).
///
/// Called from `Env::new_global` after `install_builtins` has run but
/// before `STDLIB_IMAGES` are loaded, so the shim file
/// (`nelisp-stdlib-env-shim.el`) can `funcall` these names at load
/// time.
pub fn install_env_shim_primitives(env: &mut Env) {
    env.register_extern_builtin("nelisp--env-globals-get-value", bi_get_value);
    env.register_extern_builtin("nelisp--env-globals-set-value", bi_set_value);
    env.register_extern_builtin("nelisp--env-globals-get-function", bi_get_function);
    env.register_extern_builtin("nelisp--env-globals-set-function", bi_set_function);
    env.register_extern_builtin("nelisp--env-globals-clear-value", bi_clear_value);
    env.register_extern_builtin("nelisp--env-globals-clear-function", bi_clear_function);
    env.register_extern_builtin("nelisp--env-globals-is-bound", bi_is_bound);
    env.register_extern_builtin("nelisp--env-globals-is-fbound", bi_is_fbound);
    env.register_extern_builtin("nelisp--env-globals-is-constant", bi_is_constant);
    env.register_extern_builtin("nelisp--env-globals-set-constant", bi_set_constant);
    env.register_extern_builtin(
        "nelisp--env-globals-capture-lexical",
        bi_capture_lexical,
    );
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
        let out = eval_str("(nelisp--env-globals-capture-lexical)").unwrap();
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
