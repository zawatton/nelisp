//! Special form dispatcher — Phase 8.0.2 (Doc 44 §3.3 LOCKED).
//!
//! Special forms differ from ordinary functions in that they decide
//! themselves whether (and when) to evaluate each argument.  The
//! evaluator in `super::eval` calls [`apply_special`] before any
//! generic argument evaluation; if the head symbol matches one of the
//! known special forms, control transfers here.
//!
//! Per-form notes are inline.  All forms return `Ok(Some(value))` on
//! success, `Ok(None)` if the head is not a known special form (so
//! the dispatcher falls through to function/macro lookup), and
//! `Err(EvalError)` on invalid syntax / sub-form failure.

use std::rc::Rc;

use super::env::Env;
use super::error::{is_error_subtype, EvalError};
use super::sexp::Sexp;
use super::{eval, eval_arg_list, list_elements};

/// Dispatch a special form by name.  Returns `Ok(None)` when `name`
/// is not a special form so the evaluator can continue with function
/// lookup; returns `Ok(Some(value))` when the form was handled.
pub fn apply_special(
    name: &str,
    args: &Sexp,
    env: &mut Env,
) -> Result<Option<Sexp>, EvalError> {
    // Phase 7 Stage 7.3.d (Doc 67 §3.4) — only the 13 Tier 1
    // irreducible special forms remain here.  All Tier 2 forms
    // (cond / when / unless / and / or / prog1 / prog2 /
    //  save-excursion / save-restriction / setq-default /
    //  defvar / defvar-local / defconst / defcustom / defgroup /
    //  dolist / dotimes / push / pop / defun / defmacro / cl-defun)
    // have been migrated to elisp macros installed by
    // lisp/nelisp-stdlib-eval-special.el (= STDLIB_SOURCES Layer A
    // 1番目).  When `apply_special' returns Ok(None), the caller
    // falls through to fcell lookup in `apply_combiner', expanding
    // the macro and re-evaluating the expansion.
    let result = match name {
        "quote" => sf_quote(args)?,
        "function" => sf_function(args, env)?,
        "if" => sf_if(args, env)?,
        "let" => sf_let(args, env)?,
        "let*" => sf_let_star(args, env)?,
        "lambda" => sf_lambda(args, env)?,
        "setq" => sf_setq(args, env)?,
        "while" => sf_while(args, env)?,
        "condition-case" => sf_condition_case(args, env)?,
        "unwind-protect" => sf_unwind_protect(args, env)?,
        "progn" => sf_progn(args, env)?,
        "catch" => sf_catch(args, env)?,
        "throw" => sf_throw(args, env)?,
        _ => return Ok(None),
    };
    Ok(Some(result))
}

// ---------- helpers ----------

fn first_arg(args: &Sexp, op: &str) -> Result<Sexp, EvalError> {
    match args {
        Sexp::Cons(a, _) => Ok(a.borrow().clone()),
        _ => Err(EvalError::WrongNumberOfArguments {
            function: op.into(),
            expected: "≥1".into(),
            got: 0,
        }),
    }
}

fn args_vec(args: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    list_elements(args)
}

/// Boolean truthiness per Elisp: only `nil` is false.
pub fn is_truthy(v: &Sexp) -> bool {
    !matches!(v, Sexp::Nil)
}

// ---------- quote / function ----------

fn sf_quote(args: &Sexp) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() != 1 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "quote".into(),
            expected: "1".into(),
            got: parts.len(),
        });
    }
    Ok(parts.into_iter().next().unwrap())
}

fn sf_function(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // `(function FORM)` = `#'FORM`.  For a bare lambda it captures the
    // current lexical environment into a closure; for a symbol it just
    // returns the symbol (the evaluator will fcell-lookup at funcall).
    let form = first_arg(args, "function")?;
    match &form {
        Sexp::Cons(head, _) if matches!(&*head.borrow(), Sexp::Symbol(s) if s == "lambda") => {
            sf_lambda(&Sexp::cons(
                extract_lambda_args(&form)?,
                extract_lambda_body(&form)?,
            ), env)
        }
        _ => Ok(form),
    }
}

fn extract_lambda_args(lam: &Sexp) -> Result<Sexp, EvalError> {
    if let Sexp::Cons(_, rest) = lam {
        if let Sexp::Cons(args, _) = &*rest.borrow() {
            return Ok(args.borrow().clone());
        }
    }
    Err(EvalError::Internal("lambda has no formals".into()))
}

fn extract_lambda_body(lam: &Sexp) -> Result<Sexp, EvalError> {
    if let Sexp::Cons(_, rest) = lam {
        if let Sexp::Cons(_, body) = &*rest.borrow() {
            return Ok(body.borrow().clone());
        }
    }
    Err(EvalError::Internal("lambda has no body".into()))
}

// ---------- if ----------

fn sf_if(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (if COND THEN ELSE...)
    let parts = args_vec(args)?;
    if parts.len() < 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "if".into(),
            expected: "≥2".into(),
            got: parts.len(),
        });
    }
    let cond = eval(&parts[0], env)?;
    if is_truthy(&cond) {
        eval(&parts[1], env)
    } else {
        let mut last = Sexp::Nil;
        for f in parts.iter().skip(2) {
            last = eval(f, env)?;
        }
        Ok(last)
    }
}

// ---------- let / let* ----------

fn sf_let(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (let ((VAR VALUE) ...) BODY...) — value forms eval'd in *outer* env.
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "let".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let bindings = list_elements(&parts[0])?;
    let mut values = Vec::with_capacity(bindings.len());
    for b in &bindings {
        let (name, val) = parse_let_binding(b, env, /* eval_in_outer = */ true)?;
        values.push((name, val));
    }
    env.push_frame();
    for (name, val) in values {
        env.bind_local(&name, val);
    }
    let result = eval_body(&parts[1..], env);
    env.pop_frame();
    result
}

fn sf_let_star(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (let* ((VAR VALUE) ...) BODY...) — sequential bind.
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "let*".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let bindings = list_elements(&parts[0])?;
    env.push_frame();
    for b in &bindings {
        match parse_let_binding(b, env, /* eval_in_outer = */ false) {
            Ok((name, val)) => env.bind_local(&name, val),
            Err(e) => {
                env.pop_frame();
                return Err(e);
            }
        }
    }
    let result = eval_body(&parts[1..], env);
    env.pop_frame();
    result
}

fn parse_let_binding(
    b: &Sexp,
    env: &mut Env,
    _eval_in_outer: bool,
) -> Result<(String, Sexp), EvalError> {
    match b {
        Sexp::Symbol(name) => Ok((name.clone(), Sexp::Nil)),
        Sexp::Cons(_, _) => {
            let parts = list_elements(b)?;
            let name = match &parts[0] {
                Sexp::Symbol(s) => s.clone(),
                other => {
                    return Err(EvalError::WrongType {
                        expected: "symbol".into(),
                        got: other.clone(),
                    })
                }
            };
            let val = if parts.len() >= 2 {
                eval(&parts[1], env)?
            } else {
                Sexp::Nil
            };
            Ok((name, val))
        }
        other => Err(EvalError::WrongType {
            expected: "symbol or (symbol value) pair".into(),
            got: other.clone(),
        }),
    }
}

fn eval_body(body: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let mut last = Sexp::Nil;
    for f in body {
        last = eval(f, env)?;
    }
    Ok(last)
}

// ---------- lambda ----------

fn sf_lambda(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (lambda ARGS BODY...) → (closure CAPTURED-ENV ARGS BODY...)
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "lambda".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let formals = parts[0].clone();
    let body: Vec<Sexp> = parts.iter().skip(1).cloned().collect();
    let captured = env.capture_lexical();
    let mut chain = vec![Sexp::Symbol("closure".into()), captured, formals];
    chain.extend(body);
    Ok(Sexp::list_from(&chain))
}

// ---------- setq ----------

fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (setq SYM VAL SYM VAL ...)
    let parts = args_vec(args)?;
    if parts.len() % 2 != 0 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "setq".into(),
            expected: "even number".into(),
            got: parts.len(),
        });
    }
    let mut last = Sexp::Nil;
    let mut iter = parts.into_iter();
    while let Some(name_form) = iter.next() {
        let value_form = iter.next().unwrap();
        let name = match name_form {
            Sexp::Symbol(s) => s,
            // `nil` and `t` are reader-special: surface them as
            // SettingConstant rather than WrongType so error messages
            // match Elisp's `setting-constant` signal.
            Sexp::Nil => "nil".to_string(),
            Sexp::T => "t".to_string(),
            other => {
                return Err(EvalError::WrongType {
                    expected: "symbol".into(),
                    got: other,
                })
            }
        };
        let val = eval(&value_form, env)?;
        env.set_value(&name, val.clone())?;
        last = val;
    }
    Ok(last)
}

// ---------- while ----------

fn sf_while(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "while".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    loop {
        let cond = eval(&parts[0], env)?;
        if !is_truthy(&cond) {
            return Ok(Sexp::Nil);
        }
        for f in parts.iter().skip(1) {
            eval(f, env)?;
        }
    }
}

// ---------- condition-case / unwind-protect ----------

fn sf_condition_case(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (condition-case VAR PROTECTED-FORM (TAG BODY...) ...)
    let parts = args_vec(args)?;
    if parts.len() < 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "condition-case".into(),
            expected: "≥2".into(),
            got: parts.len(),
        });
    }
    let var = match &parts[0] {
        Sexp::Symbol(s) if s == "nil" => None,
        Sexp::Symbol(s) => Some(s.clone()),
        Sexp::Nil => None,
        other => {
            return Err(EvalError::WrongType {
                expected: "symbol or nil".into(),
                got: other.clone(),
            })
        }
    };
    let protected = &parts[1];
    let handlers: Vec<Sexp> = parts.iter().skip(2).cloned().collect();
    match eval(protected, env) {
        Ok(v) => Ok(v),
        // `throw' is a control-flow primitive, not an error — let it
        // pass through `condition-case' so the matching `catch'
        // upstream can handle it.  Real Emacs has the same rule:
        // `condition-case' only catches conditions in `error-conditions',
        // and `no-catch' (= an unhandled `throw') is NOT a subtype of
        // `error' unless the handler clause explicitly names it.
        Err(EvalError::UncaughtThrow { tag, value }) => {
            // Allow an explicit `(no-catch ...)' clause to catch it,
            // matching Emacs' parity.  Otherwise re-raise.
            for handler in &handlers {
                let h_parts = list_elements(handler)?;
                if h_parts.is_empty() {
                    continue;
                }
                let claims_no_catch = match &h_parts[0] {
                    Sexp::Symbol(s) => s == "no-catch",
                    Sexp::Cons(_, _) => {
                        let tag_list = list_elements(&h_parts[0])?;
                        tag_list.iter().any(|t| {
                            matches!(t, Sexp::Symbol(s) if s == "no-catch")
                        })
                    }
                    _ => false,
                };
                if claims_no_catch {
                    env.push_frame();
                    if let Some(name) = &var {
                        env.bind_local(
                            name,
                            Sexp::cons(
                                Sexp::Symbol("no-catch".into()),
                                Sexp::cons(tag.clone(), Sexp::cons(value.clone(), Sexp::Nil)),
                            ),
                        );
                    }
                    let body: Vec<Sexp> = h_parts.iter().skip(1).cloned().collect();
                    let r = eval_body(&body, env);
                    env.pop_frame();
                    return r;
                }
            }
            Err(EvalError::UncaughtThrow { tag, value })
        }
        Err(e) => {
            let actual_tag = e.error_tag().to_string();
            for handler in &handlers {
                let h_parts = list_elements(handler)?;
                if h_parts.is_empty() {
                    continue;
                }
                let matches = match &h_parts[0] {
                    Sexp::Symbol(s) => is_error_subtype(s, &actual_tag),
                    Sexp::T => true,
                    Sexp::Cons(_, _) => {
                        // Tag list — match if any member matches.
                        let tag_list = list_elements(&h_parts[0])?;
                        tag_list.iter().any(|t| {
                            matches!(t, Sexp::Symbol(s) if is_error_subtype(s, &actual_tag))
                        })
                    }
                    Sexp::Nil => false,
                    _ => false,
                };
                if matches {
                    env.push_frame();
                    if let Some(name) = &var {
                        env.bind_local(name, e.signal_data());
                    }
                    let body: Vec<Sexp> = h_parts.iter().skip(1).cloned().collect();
                    let r = eval_body(&body, env);
                    env.pop_frame();
                    return r;
                }
            }
            Err(e)
        }
    }
}

fn sf_unwind_protect(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (unwind-protect BODY-FORM CLEANUP-FORMS...)
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "unwind-protect".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let body_result = eval(&parts[0], env);
    let mut cleanup_err: Option<EvalError> = None;
    for cleanup in parts.iter().skip(1) {
        if let Err(e) = eval(cleanup, env) {
            // First cleanup error wins; subsequent cleanups still run.
            if cleanup_err.is_none() {
                cleanup_err = Some(e);
            }
        }
    }
    // Real Emacs: a cleanup-form failure (= newer error / throw) takes
    // precedence over the body's failure, because it reflects the most
    // recent control-flow state.  When cleanups all succeed, the body's
    // own outcome (= success or original error) is returned unchanged.
    match cleanup_err {
        Some(ce) => Err(ce),
        None => body_result,
    }
}

// ---------- progn ----------

fn sf_progn(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    let mut last = Sexp::Nil;
    for f in &parts {
        last = eval(f, env)?;
    }
    Ok(last)
}

// ---------- pcase removed: see lisp/nelisp-pcase.el ----------
//
// Rust-min migration 2026-05-06: pcase was historically a special
// form here (`sf_pcase' + `pcase_match_binding') with a restricted
// pattern grammar (literal / quote / cons / keyword).  Moving it to
// elisp unlocks the richer Emacs grammar (or / and / pred / guard /
// backquote / let) without growing the Rust core.  The elisp
// implementation is loaded as part of `Env::new_global'
// `STDLIB_SOURCES' so it's defined before any consumer parses.

// ---------- catch / throw ----------

fn sf_catch(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (catch TAG BODY...) — TAG is evaluated.
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "catch".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let tag = eval(&parts[0], env)?;
    let body: Vec<Sexp> = parts.iter().skip(1).cloned().collect();
    match eval_body(&body, env) {
        Ok(v) => Ok(v),
        Err(EvalError::UncaughtThrow { tag: thrown, value }) => {
            if sexp_eq(&tag, &thrown) {
                Ok(value)
            } else {
                Err(EvalError::UncaughtThrow { tag: thrown, value })
            }
        }
        Err(other) => Err(other),
    }
}

fn sf_throw(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() != 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "throw".into(),
            expected: "2".into(),
            got: parts.len(),
        });
    }
    let tag = eval(&parts[0], env)?;
    let value = eval(&parts[1], env)?;
    Err(EvalError::UncaughtThrow { tag, value })
}
/// `eq` semantics for `Sexp`.  Symbols match by name, integers by
/// value, `nil`/`t` by identity, everything else by structural
/// equality (close enough for the bootstrap; full pointer-eq would
/// need an arena).
pub fn sexp_eq(a: &Sexp, b: &Sexp) -> bool {
    match (a, b) {
        (Sexp::Nil, Sexp::Nil) | (Sexp::T, Sexp::T) => true,
        (Sexp::Int(x), Sexp::Int(y)) => x == y,
        (Sexp::Symbol(x), Sexp::Symbol(y)) => x == y,
        // Heap types: identity = Rc::ptr_eq (= same allocation).  Per
        // the type docs on Sexp, every `Rc<RefCell<...>>'-backed
        // variant has cell identity so `(eq x x)' / `(memq w list)' /
        // `(assq k alist)' / cl-defstruct slot equality work as
        // they do in host Emacs.  Without this, walking a tree of
        // shared cons cells (e.g. window parent/children pointers)
        // would compare structurally — which on a cyclic graph
        // recurses forever.
        (Sexp::Cons(a1, a2), Sexp::Cons(b1, b2)) => {
            Rc::ptr_eq(a1, b1) && Rc::ptr_eq(a2, b2)
        }
        (Sexp::MutStr(a), Sexp::MutStr(b)) => Rc::ptr_eq(a, b),
        (Sexp::Vector(a), Sexp::Vector(b)) => Rc::ptr_eq(a, b),
        (Sexp::CharTable(a), Sexp::CharTable(b)) => Rc::ptr_eq(a, b),
        (Sexp::BoolVector(a), Sexp::BoolVector(b)) => Rc::ptr_eq(a, b),
        // Records (Doc 50 stage 4): identity through the slots Rc.
        // Two records are `eq' iff they share the same slots cell —
        // hash-tables and cl-defstruct values rely on this for
        // mutation-aware sharing.
        (Sexp::Record { slots: a, .. }, Sexp::Record { slots: b, .. }) => {
            Rc::ptr_eq(a, b)
        }
        // Strings + floats: bootstrap subset uses structural eq
        // (close enough — Emacs treats short interned strings + small
        // fixnums similarly; full impl would need an interner).
        (Sexp::Str(x), Sexp::Str(y)) => x == y,
        (Sexp::Float(x), Sexp::Float(y)) => x.to_bits() == y.to_bits(),
        _ => false,
    }
}

/// Re-export `eval_arg_list` for tests if needed.
#[allow(dead_code)]
pub(crate) fn _eval_args(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    eval_arg_list(args, env)
}
