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
    let result = match name {
        "quote" => sf_quote(args)?,
        "function" => sf_function(args, env)?,
        "if" => sf_if(args, env)?,
        "cond" => sf_cond(args, env)?,
        "when" => sf_when(args, env)?,
        "unless" => sf_unless(args, env)?,
        "let" => sf_let(args, env)?,
        "let*" => sf_let_star(args, env)?,
        "lambda" => sf_lambda(args, env)?,
        "defun" => sf_defun(args, env)?,
        "cl-defun" => sf_cl_defun(args, env)?,
        "defmacro" => sf_defmacro(args, env)?,
        "defvar" => sf_defvar(args, env)?,
        "defvar-local" => sf_defvar(args, env)?,
        "defconst" => sf_defconst(args, env)?,
        "defcustom" => sf_defcustom(args, env)?,
        "defgroup" => sf_defgroup(args, env)?,
        "setq" => sf_setq(args, env)?,
        // No buffer-local concept here — the global value IS the
        // default, so setq-default ≡ setq.
        "setq-default" => sf_setq(args, env)?,
        "while" => sf_while(args, env)?,
        "dolist" => sf_dolist(args, env)?,
        "dotimes" => sf_dotimes(args, env)?,
        "condition-case" => sf_condition_case(args, env)?,
        "unwind-protect" => sf_unwind_protect(args, env)?,
        "progn" => sf_progn(args, env)?,
        "prog1" => sf_prog1(args, env)?,
        "prog2" => sf_prog2(args, env)?,
        "and" => sf_and(args, env)?,
        "or" => sf_or(args, env)?,
        "pcase" => sf_pcase(args, env)?,
        "save-excursion" => sf_progn(args, env)?, // no-op stub per Doc 44 §3.3
        "save-restriction" => sf_progn(args, env)?, // no-op stub
        "catch" => sf_catch(args, env)?,
        "throw" => sf_throw(args, env)?,
        "push" => sf_push(args, env)?,
        "pop" => sf_pop(args, env)?,
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

// ---------- if / cond / when / unless ----------

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

fn sf_cond(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (cond (TEST BODY...) (TEST BODY...) ...)
    let clauses = args_vec(args)?;
    for clause in clauses {
        let parts = list_elements(&clause)?;
        if parts.is_empty() {
            return Err(EvalError::WrongType {
                expected: "non-empty cond clause".into(),
                got: clause,
            });
        }
        let test = eval(&parts[0], env)?;
        if is_truthy(&test) {
            if parts.len() == 1 {
                return Ok(test);
            }
            let mut last = Sexp::Nil;
            for body in parts.iter().skip(1) {
                last = eval(body, env)?;
            }
            return Ok(last);
        }
    }
    Ok(Sexp::Nil)
}

fn sf_when(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Ok(Sexp::Nil);
    }
    let cond = eval(&parts[0], env)?;
    if !is_truthy(&cond) {
        return Ok(Sexp::Nil);
    }
    let mut last = Sexp::Nil;
    for b in parts.iter().skip(1) {
        last = eval(b, env)?;
    }
    Ok(last)
}

fn sf_unless(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Ok(Sexp::Nil);
    }
    let cond = eval(&parts[0], env)?;
    if is_truthy(&cond) {
        return Ok(Sexp::Nil);
    }
    let mut last = Sexp::Nil;
    for b in parts.iter().skip(1) {
        last = eval(b, env)?;
    }
    Ok(last)
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

fn expect_symbol(form: &Sexp) -> Result<String, EvalError> {
    match form {
        Sexp::Symbol(s) => Ok(s.clone()),
        other => Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    }
}

fn quote_body(parts: &[Sexp]) -> &[Sexp] {
    match parts.first() {
        Some(Sexp::Str(_)) => &parts[1..],
        _ => parts,
    }
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

// ---------- defun / defmacro / defvar / defconst ----------

fn define_named_lambda(
    function_name: &str,
    parts: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    if parts.len() < 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: function_name.into(),
            expected: "≥2".into(),
            got: parts.len(),
        });
    }
    let name = expect_symbol(&parts[0])?;
    let formals = parts[1].clone();
    let body = quote_body(&parts[2..]);
    let mut chain = vec![Sexp::Symbol("lambda".into()), formals];
    chain.extend(body.iter().cloned());
    let lambda = Sexp::list_from(&chain);
    env.set_function(&name, lambda);
    Ok(Sexp::Symbol(name))
}

fn sf_defun(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (defun NAME ARGS [DOC] BODY...)
    let parts = args_vec(args)?;
    define_named_lambda("defun", &parts, env)
}

fn sf_cl_defun(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    define_named_lambda("cl-defun", &parts, env)
}

fn sf_defmacro(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (defmacro NAME ARGS BODY...)
    let parts = args_vec(args)?;
    if parts.len() < 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "defmacro".into(),
            expected: "≥2".into(),
            got: parts.len(),
        });
    }
    let name = expect_symbol(&parts[0])?;
    let formals = parts[1].clone();
    let body = quote_body(&parts[2..]);
    let mut lambda_chain = vec![Sexp::Symbol("lambda".into()), formals];
    lambda_chain.extend(body.iter().cloned());
    let lambda = Sexp::list_from(&lambda_chain);
    let macro_form = Sexp::list_from(&[Sexp::Symbol("macro".into()), lambda]);
    env.set_function(&name, macro_form);
    Ok(Sexp::Symbol(name))
}

fn sf_defvar(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "defvar".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let name = expect_symbol(&parts[0])?;
    let value = if parts.len() >= 2 {
        eval(&parts[1], env)?
    } else {
        Sexp::Nil
    };
    env.defvar(&name, value, false);
    Ok(Sexp::Symbol(name))
}

fn sf_defconst(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() < 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "defconst".into(),
            expected: "≥2".into(),
            got: parts.len(),
        });
    }
    let name = expect_symbol(&parts[0])?;
    let value = eval(&parts[1], env)?;
    // defconst always overwrites and marks constant.
    let entry = env.globals.entry(name.clone()).or_default();
    entry.value = Some(value);
    entry.constant = true;
    Ok(Sexp::Symbol(name))
}

fn sf_defcustom(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() < 3 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "defcustom".into(),
            expected: "≥3".into(),
            got: parts.len(),
        });
    }
    let name = expect_symbol(&parts[0])?;
    let value = eval(&parts[1], env)?;
    env.defvar(&name, value, false);
    Ok(Sexp::Symbol(name))
}

fn sf_defgroup(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() < 3 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "defgroup".into(),
            expected: "≥3".into(),
            got: parts.len(),
        });
    }
    let name = expect_symbol(&parts[0])?;
    env.globals.entry(name.clone()).or_default();
    Ok(Sexp::Symbol(name))
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

// ---------- while / dolist / dotimes ----------

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

fn sf_dolist(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (dolist (VAR LIST [RESULT]) BODY...)
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "dolist".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let head = list_elements(&parts[0])?;
    if head.len() < 2 {
        return Err(EvalError::WrongType {
            expected: "(VAR LIST [RESULT])".into(),
            got: parts[0].clone(),
        });
    }
    let var_name = match &head[0] {
        Sexp::Symbol(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "symbol".into(),
                got: other.clone(),
            })
        }
    };
    let list_value = eval(&head[1], env)?;
    let result_form = head.get(2).cloned();
    let items = list_elements(&list_value)?;
    env.push_frame();
    for item in items {
        env.bind_local(&var_name, item);
        for f in parts.iter().skip(1) {
            if let Err(e) = eval(f, env) {
                env.pop_frame();
                return Err(e);
            }
        }
    }
    let result = if let Some(rf) = result_form {
        // Bind var to nil before evaluating result form per Elisp.
        env.bind_local(&var_name, Sexp::Nil);
        eval(&rf, env)
    } else {
        Ok(Sexp::Nil)
    };
    env.pop_frame();
    result
}

fn sf_dotimes(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // (dotimes (VAR COUNT [RESULT]) BODY...)
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "dotimes".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let head = list_elements(&parts[0])?;
    if head.len() < 2 {
        return Err(EvalError::WrongType {
            expected: "(VAR COUNT [RESULT])".into(),
            got: parts[0].clone(),
        });
    }
    let var_name = match &head[0] {
        Sexp::Symbol(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "symbol".into(),
                got: other.clone(),
            })
        }
    };
    let count_value = eval(&head[1], env)?;
    let count = match count_value {
        Sexp::Int(n) => n,
        other => {
            return Err(EvalError::WrongType {
                expected: "integerp".into(),
                got: other,
            })
        }
    };
    let result_form = head.get(2).cloned();
    env.push_frame();
    for i in 0..count.max(0) {
        env.bind_local(&var_name, Sexp::Int(i));
        for f in parts.iter().skip(1) {
            if let Err(e) = eval(f, env) {
                env.pop_frame();
                return Err(e);
            }
        }
    }
    let result = if let Some(rf) = result_form {
        env.bind_local(&var_name, Sexp::Int(count.max(0)));
        eval(&rf, env)
    } else {
        Ok(Sexp::Nil)
    };
    env.pop_frame();
    result
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

// ---------- progn / prog1 / prog2 ----------

fn sf_progn(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    let mut last = Sexp::Nil;
    for f in &parts {
        last = eval(f, env)?;
    }
    Ok(last)
}

fn sf_prog1(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "prog1".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let first = eval(&parts[0], env)?;
    for f in parts.iter().skip(1) {
        eval(f, env)?;
    }
    Ok(first)
}

fn sf_prog2(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() < 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "prog2".into(),
            expected: "≥2".into(),
            got: parts.len(),
        });
    }
    eval(&parts[0], env)?;
    let second = eval(&parts[1], env)?;
    for f in parts.iter().skip(2) {
        eval(f, env)?;
    }
    Ok(second)
}

// ---------- and / or ----------

fn sf_and(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Ok(Sexp::T);
    }
    let mut last = Sexp::T;
    for f in &parts {
        last = eval(f, env)?;
        if !is_truthy(&last) {
            return Ok(Sexp::Nil);
        }
    }
    Ok(last)
}

fn sf_or(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    for f in &parts {
        let v = eval(f, env)?;
        if is_truthy(&v) {
            return Ok(v);
        }
    }
    Ok(Sexp::Nil)
}

// ---------- pcase ----------

fn sf_pcase(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "pcase".into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let value = eval(&parts[0], env)?;
    for clause in parts.iter().skip(1) {
        let clause_parts = list_elements(clause)?;
        if clause_parts.len() < 2 {
            return Err(EvalError::WrongType {
                expected: "(PAT BODY...)".into(),
                got: clause.clone(),
            });
        }
        if let Some(binding) = pcase_match_binding(&clause_parts[0], &value)? {
            if let Some(name) = binding {
                env.push_frame();
                env.bind_local(&name, value.clone());
                let result = eval_body(&clause_parts[1..], env);
                env.pop_frame();
                return result;
            }
            return eval_body(&clause_parts[1..], env);
        }
    }
    Ok(Sexp::Nil)
}

fn pcase_match_binding(pattern: &Sexp, value: &Sexp) -> Result<Option<Option<String>>, EvalError> {
    match pattern {
        Sexp::Int(_) | Sexp::Float(_) | Sexp::Str(_) => Ok((pattern == value).then_some(None)),
        Sexp::Nil => Ok(matches!(value, Sexp::Nil).then_some(None)),
        Sexp::T => Ok(matches!(value, Sexp::T).then_some(None)),
        Sexp::Symbol(name) if name == "_" => Ok(Some(None)),
        Sexp::Symbol(name) => Ok(Some(Some(name.clone()))),
        Sexp::Cons(head, _) if matches!(&*head.borrow(), Sexp::Symbol(s) if s == "quote") => {
            let quoted = args_vec(pattern)?;
            if quoted.len() != 2 {
                return Err(EvalError::WrongType {
                    expected: "(quote VALUE)".into(),
                    got: pattern.clone(),
                });
            }
            Ok((quoted[1] == *value).then_some(None))
        }
        other => Err(EvalError::WrongType {
            expected: "supported pcase pattern".into(),
            got: other.clone(),
        }),
    }
}

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

/// `(push X PLACE)` — minimal-form macro: when PLACE is a symbol,
/// expands to `(setq PLACE (cons X PLACE))'.  Generalised places
/// (= `setf'-style accessors) are deferred — Layer 2 callers we have
/// audited only push onto symbol-bound lists.
fn sf_push(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() != 2 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "push".into(),
            expected: "2".into(),
            got: parts.len(),
        });
    }
    let new_head = eval(&parts[0], env)?;
    let place_sym = match &parts[1] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol (generalised places NYI)".into(),
            got: other.clone(),
        }),
    };
    let cur = env.lookup_value(&place_sym).unwrap_or(Sexp::Nil);
    let new_list = Sexp::cons(new_head, cur);
    env.set_value(&place_sym, new_list.clone())?;
    Ok(new_list)
}

/// `(pop PLACE)` — minimal-form macro: when PLACE is a symbol,
/// returns its car and rebinds it to the cdr.
fn sf_pop(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() != 1 {
        return Err(EvalError::WrongNumberOfArguments {
            function: "pop".into(),
            expected: "1".into(),
            got: parts.len(),
        });
    }
    let place_sym = match &parts[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol (generalised places NYI)".into(),
            got: other.clone(),
        }),
    };
    let cur = env.lookup_value(&place_sym).unwrap_or(Sexp::Nil);
    match &cur {
        Sexp::Cons(head, tail) => {
            let head = head.borrow().clone();
            let tail = tail.borrow().clone();
            env.set_value(&place_sym, tail)?;
            Ok(head)
        }
        Sexp::Nil => Ok(Sexp::Nil),
        other => Err(EvalError::WrongType {
            expected: "list".into(),
            got: other.clone(),
        }),
    }
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
        _ => false,
    }
}

/// Re-export `eval_arg_list` for tests if needed.
#[allow(dead_code)]
pub(crate) fn _eval_args(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    eval_arg_list(args, env)
}
