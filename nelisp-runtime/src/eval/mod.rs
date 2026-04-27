//! Phase 8.0.2 — Rust-side minimal Elisp evaluator (Doc 44 §3.3 LOCKED).
//!
//! Public surface (per prompt):
//!   - [`eval_str`]      — read one form, evaluate, return the value
//!   - [`eval_str_all`]  — read every form, evaluate sequentially,
//!                         return the *last* value (`progn` semantics)
//!   - [`eval`]          — evaluate a pre-parsed [`Sexp`] in `env`
//!   - [`Env`]           — globals + lexical-frame stack;
//!                         [`Env::new_global`] installs all built-ins
//!   - [`EvalError`]     — closed enum, `condition-case` matches on its
//!                         tag (see `EvalError::error_tag`)
//!
//! Architectural pillars (Doc 44 §3.3):
//!   1. *Heap*: leak-tolerant [`Sexp`] cloning. No GC, no arena. The
//!      arena-drop bridge is Phase 8.0.3's problem.
//!   2. *Symbol table*: `HashMap<String, SymbolEntry { value, function,
//!      plist, constant }>`.  Two-cell value/function model per Elisp.
//!   3. *Environment*: `Vec<HashMap<String, Sexp>>` lexical frame stack.
//!   4. *Evaluator*: recursive `eval(form, env)`, special forms
//!      dispatched by name string match in [`special_forms::apply_special`].
//!   5. *Special forms*: quote, function, if, cond, when, unless, let,
//!      let*, lambda, defun, defmacro, defvar, defconst, setq, set,
//!      while, dolist, dotimes, condition-case, unwind-protect, progn,
//!      prog1, prog2, and, or, not.
//!   6. *Built-ins*: arithmetic, equality, cons/list, higher-order,
//!      predicates, string, symbol/function (~50 entries — see
//!      [`builtins`]).
//!   7. *Errors*: `Result<Sexp, EvalError>` everywhere, never panic on
//!      user input (Doc 44 §4.3).
//!
//! Layer rule: this module owns *only* evaluation.  The reader stays
//! syntactic (Phase 8.0.1) and the MCP server (Phase 8.0.4) is a peer.

pub mod builtins;
pub mod env;
pub mod error;
pub mod sexp;
pub mod special_forms;

pub use env::{Env, Frame, SymbolEntry};
pub use error::{is_error_subtype, EvalError};
pub use sexp::Sexp;

use crate::reader;

/// Read exactly one form from `input` and evaluate it in a fresh
/// global environment.  Trailing tokens after the first form are an
/// error (see [`reader::read_str`]).
pub fn eval_str(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let form = reader::read_str(input)?;
    eval(&form, &mut env)
}

/// Read every top-level form from `input` and evaluate them in
/// sequence in a single fresh global environment, returning the last
/// value (= `progn`).  Empty input returns `nil`.
pub fn eval_str_all(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let forms = reader::read_all(input)?;
    let mut last = Sexp::Nil;
    for f in &forms {
        last = eval(f, &mut env)?;
    }
    Ok(last)
}

/// Evaluate `form` in `env`.  This is the canonical recursive
/// entry point and is what [`eval_str`] / [`eval_str_all`] / built-ins
/// like `funcall` / `apply` / `eval` route through.
///
/// Algorithm:
///   1. self-evaluating atom → return as-is
///   2. symbol → look up in lexical frames then globals
///   3. cons → head decides dispatch:
///        - special form  → [`special_forms::apply_special`]
///        - macro         → expand once, recurse on the expansion
///        - lambda / sym  → evaluate arguments, [`apply_function`]
///   4. anything else → internal error
pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // Recursion guard so a `(defun loop () (loop))` returns an error
    // instead of overflowing the Rust stack.
    if env.current_recursion >= env.max_recursion {
        return Err(EvalError::Internal(format!(
            "max-lisp-eval-depth exceeded ({})",
            env.max_recursion
        )));
    }
    env.current_recursion += 1;
    let result = eval_inner(form, env);
    env.current_recursion -= 1;
    result
}

fn eval_inner(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match form {
        // Self-evaluating atoms (Elisp manual: "Most kinds of objects
        // evaluate to themselves").
        Sexp::Nil | Sexp::T | Sexp::Int(_) | Sexp::Float(_) | Sexp::Str(_) | Sexp::Vector(_) => {
            Ok(form.clone())
        }
        // Symbols.  Per Elisp manual "Constant Variables" §11.2,
        // a symbol whose name begins with `:' is a keyword: it is
        // its own value and cannot be bound.  This rule predates
        // any value-cell lookup.
        Sexp::Symbol(name) if name.starts_with(':') && name.len() > 1 => {
            Ok(form.clone())
        }
        // Plain symbols evaluate via the value cell.
        Sexp::Symbol(name) => env.lookup_value(name),
        // Cons → function application.
        Sexp::Cons(head, tail) => apply_combiner(head, tail, env),
    }
}

/// Apply a combiner head to its argument list (un-evaluated).  The
/// head's runtime kind decides whether arguments get evaluated and
/// how the call is dispatched.
fn apply_combiner(head: &Sexp, tail: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match head {
        // Symbol head — special form? macro? function in fcell?
        Sexp::Symbol(name) => {
            // 1) Special forms — argument evaluation order is form-specific.
            if let Some(result) = special_forms::apply_special(name, tail, env)? {
                return Ok(result);
            }
            // 2) Macro? — expand once, recurse on expansion.
            if let Ok(func) = env.lookup_function(name) {
                if is_macro(&func) {
                    let expansion = expand_macro(&func, tail, env)?;
                    return eval(&expansion, env);
                }
                // 3) Plain function — evaluate args, apply.
                let args = eval_arg_list(tail, env)?;
                return apply_function(&func, &args, env);
            }
            Err(EvalError::UnboundFunction(name.clone()))
        }
        // Lambda head: `((lambda ARGS BODY...) ARG ARG ...)` — evaluate
        // the head form in the current env (it is a literal lambda or
        // closure), then apply.
        Sexp::Cons(_, _) => {
            let func = eval(head, env)?;
            let args = eval_arg_list(tail, env)?;
            apply_function(&func, &args, env)
        }
        other => Err(EvalError::WrongType {
            expected: "function".into(),
            got: other.clone(),
        }),
    }
}

/// Walk a proper list of forms, evaluating each one; return the
/// resulting Vec for argument passing.
pub(crate) fn eval_arg_list(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur = args;
    loop {
        match cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(a, d) => {
                out.push(eval(a, env)?);
                cur = d;
            }
            _ => {
                return Err(EvalError::WrongType {
                    expected: "list".into(),
                    got: cur.clone(),
                })
            }
        }
    }
}

/// Walk a proper list and collect each element without evaluating —
/// used by special forms that take their argument list literally
/// (e.g. `quote`, `lambda` formal-parameter lists, `let` bindings).
pub(crate) fn list_elements(list: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur = list;
    loop {
        match cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(a, d) => {
                out.push((**a).clone());
                cur = d;
            }
            _ => {
                return Err(EvalError::WrongType {
                    expected: "list".into(),
                    got: cur.clone(),
                })
            }
        }
    }
}

/// Apply `func` to `args`.  `func` may be:
///   - a built-in (`(builtin <NAME>)`-shaped sentinel — see
///     [`builtins::install_builtins`])
///   - a closure `(closure ENV ARGS BODY...)` — what `lambda`
///     evaluates to
///   - a raw `(lambda ARGS BODY...)` — what `defun` writes (closures
///     in Emacs sense, but we treat them as dynamically resolved
///     against the call-site env if the leading `closure` marker is
///     missing)
pub fn apply_function(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    match func {
        Sexp::Cons(head, _) => match head.as_ref() {
            Sexp::Symbol(s) if s == "builtin" => apply_builtin(func, args, env),
            Sexp::Symbol(s) if s == "closure" => apply_closure(func, args, env),
            Sexp::Symbol(s) if s == "lambda" => {
                // Bare `(lambda ARGS BODY...)` — apply against an empty
                // captured env.  This is what `defun` produces (the
                // function cell stores the lambda form unmodified).
                apply_lambda(func, &Sexp::Nil, args, env)
            }
            Sexp::Symbol(s) if s == "macro" => Err(EvalError::WrongType {
                expected: "function (not macro)".into(),
                got: func.clone(),
            }),
            _ => Err(EvalError::WrongType {
                expected: "function".into(),
                got: func.clone(),
            }),
        },
        _ => Err(EvalError::WrongType {
            expected: "function".into(),
            got: func.clone(),
        }),
    }
}

fn apply_builtin(
    func: &Sexp,
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // `(builtin <NAME>)` — pull the name out, dispatch via the registry.
    let name = match func {
        Sexp::Cons(_, tail) => match tail.as_ref() {
            Sexp::Cons(name, _) => match name.as_ref() {
                Sexp::Symbol(s) => s.clone(),
                Sexp::Str(s) => s.clone(),
                _ => return Err(EvalError::Internal(
                    "builtin sentinel name not a symbol".into(),
                )),
            },
            _ => return Err(EvalError::Internal(
                "builtin sentinel missing name".into(),
            )),
        },
        _ => return Err(EvalError::Internal("builtin sentinel not a cons".into())),
    };
    builtins::dispatch(&name, args, env)
}

fn apply_closure(closure: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    // Shape: (closure CAPTURED-ENV ARGS BODY...)
    let parts = list_elements(closure)?;
    if parts.len() < 3 {
        return Err(EvalError::Internal(
            "closure missing env / args / body".into(),
        ));
    }
    let captured = &parts[1];
    let formals = &parts[2];
    let body = &parts[3..];
    apply_lambda_inner(captured, formals, body, args, env)
}

fn apply_lambda(
    lambda: &Sexp,
    captured: &Sexp,
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // Shape: (lambda ARGS BODY...)
    let parts = list_elements(lambda)?;
    if parts.len() < 2 {
        return Err(EvalError::Internal("lambda missing args / body".into()));
    }
    let formals = &parts[1];
    let body = &parts[2..];
    apply_lambda_inner(captured, formals, body, args, env)
}

fn apply_lambda_inner(
    captured: &Sexp,
    formals: &Sexp,
    body: &[Sexp],
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // Evaluate body inside captured env + a fresh frame for the
    // formal parameters.  The captured env is pushed first so its
    // bindings act as the enclosing lexical scope.
    env.push_captured(captured)?;
    env.push_frame();
    let bind_result = bind_formals(formals, args, env);
    if let Err(e) = bind_result {
        env.pop_frame();
        env.pop_frame();
        return Err(e);
    }
    let mut last = Sexp::Nil;
    let mut last_err: Option<EvalError> = None;
    for form in body {
        match eval(form, env) {
            Ok(v) => last = v,
            Err(e) => {
                last_err = Some(e);
                break;
            }
        }
    }
    env.pop_frame();
    env.pop_frame();
    if let Some(e) = last_err {
        Err(e)
    } else {
        Ok(last)
    }
}

/// Bind a formal parameter list to actual arguments in the topmost
/// frame.  Supports `&optional` (default-nil) and `&rest` (collect
/// remainder as a list).
pub(crate) fn bind_formals(
    formals: &Sexp,
    args: &[Sexp],
    env: &mut Env,
) -> Result<(), EvalError> {
    let names = list_elements(formals)?;
    enum Mode {
        Required,
        Optional,
        Rest,
    }
    let mut mode = Mode::Required;
    let mut idx = 0usize;
    let mut required_count = 0usize;
    let mut consumed_rest = false;
    let mut saw_rest = false;

    // First pass: count required arity for diagnostics.
    for n in &names {
        if let Sexp::Symbol(s) = n {
            if s == "&optional" || s == "&rest" {
                break;
            }
        }
        required_count += 1;
    }

    for n in names {
        match n {
            Sexp::Symbol(s) if s == "&optional" => {
                if matches!(mode, Mode::Rest) {
                    return Err(EvalError::WrongType {
                        expected: "formal parameter after &rest".into(),
                        got: Sexp::Symbol(s),
                    });
                }
                mode = Mode::Optional;
            }
            Sexp::Symbol(s) if s == "&rest" => {
                if saw_rest {
                    return Err(EvalError::WrongType {
                        expected: "single &rest marker".into(),
                        got: Sexp::Symbol(s),
                    });
                }
                mode = Mode::Rest;
                saw_rest = true;
            }
            Sexp::Symbol(name) => match mode {
                Mode::Required => {
                    if idx >= args.len() {
                        return Err(EvalError::WrongNumberOfArguments {
                            function: "lambda".into(),
                            expected: format!("{}", required_count),
                            got: args.len(),
                        });
                    }
                    env.bind_local(&name, args[idx].clone());
                    idx += 1;
                }
                Mode::Optional => {
                    let v = if idx < args.len() {
                        let value = args[idx].clone();
                        idx += 1;
                        value
                    } else {
                        Sexp::Nil
                    };
                    env.bind_local(&name, v);
                }
                Mode::Rest => {
                    if consumed_rest {
                        return Err(EvalError::WrongType {
                            expected: "single symbol after &rest".into(),
                            got: Sexp::Symbol(name),
                        });
                    }
                    let rest = Sexp::list_from(&args[idx..]);
                    env.bind_local(&name, rest);
                    idx = args.len();
                    consumed_rest = true;
                }
            },
            other => {
                return Err(EvalError::WrongType {
                    expected: "symbol".into(),
                    got: other,
                })
            }
        }
    }
    // Reject excess args only if no &rest consumed them.
    if idx < args.len() && !consumed_rest {
        return Err(EvalError::WrongNumberOfArguments {
            function: "lambda".into(),
            expected: format!("at most {}", idx),
            got: args.len(),
        });
    }
    if saw_rest && !consumed_rest {
        return Err(EvalError::WrongType {
            expected: "symbol after &rest".into(),
            got: Sexp::Symbol("&rest".into()),
        });
    }
    Ok(())
}

/// Macro detector — `(macro lambda ARGS BODY...)` is the canonical
/// shape `defmacro` writes.  GNU Emacs also accepts `(macro . FUN)`
/// where `FUN` is callable; we support both.
fn is_macro(func: &Sexp) -> bool {
    matches!(
        func,
        Sexp::Cons(head, _) if matches!(head.as_ref(), Sexp::Symbol(s) if s == "macro")
    )
}

fn expand_macro(macro_form: &Sexp, args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // `macro_form` is `(macro lambda ARGS BODY...)` — strip the
    // `macro` head, get the underlying lambda, and apply it to the
    // argument forms *un-evaluated* (= macro semantics).
    let parts = list_elements(macro_form)?;
    if parts.len() < 2 {
        return Err(EvalError::Internal("malformed macro".into()));
    }
    let inner = &parts[1];
    let arg_forms = list_elements(args)?;
    apply_function(inner, &arg_forms, env)
}

#[cfg(test)]
mod tests;
