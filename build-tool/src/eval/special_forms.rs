//! Special-form dispatcher — head-controlled argument evaluation.
//! [`apply_special`] returns `Ok(Some(v))' on match, `Ok(None)' to
//! fall through to function/macro lookup, `Err' on syntax error.

use super::env::Env;
use super::error::{is_error_subtype, EvalError};
use super::sexp::Sexp;
use super::{eval, list_elements};

/// Dispatch the 13 Tier 1 irreducible special forms.  Tier 2 forms
/// (`cond' / `when' / `defvar' / `dolist' / etc.) are elisp macros
/// in `lisp/nelisp-stdlib-eval-special.el' — they reach this
/// function as `Ok(None)' and the caller falls through to macro
/// expansion.
pub fn apply_special(
    name: &str,
    args: &Sexp,
    env: &mut Env,
) -> Result<Option<Sexp>, EvalError> {
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
        Sexp::Cons(b) => Ok(b.car.clone()),
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
        Sexp::Cons(b) if matches!(&b.car, Sexp::Symbol(s) if s == "lambda") => {
            sf_lambda(&Sexp::cons(
                extract_lambda_args(&form)?,
                extract_lambda_body(&form)?,
            ), env)
        }
        _ => Ok(form),
    }
}

fn lambda_rest(lam: &Sexp) -> Result<(Sexp, Sexp), EvalError> {
    if let Sexp::Cons(outer) = lam {
        if let Sexp::Cons(rest) = &outer.cdr {
            return Ok((rest.car.clone(), rest.cdr.clone()));
        }
    }
    Err(EvalError::Internal("malformed lambda".into()))
}

fn extract_lambda_args(lam: &Sexp) -> Result<Sexp, EvalError> {
    Ok(lambda_rest(lam)?.0)
}

fn extract_lambda_body(lam: &Sexp) -> Result<Sexp, EvalError> {
    Ok(lambda_rest(lam)?.1)
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
    sf_let_common(args, env, "let", /*sequential=*/ false)
}

fn sf_let_star(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    sf_let_common(args, env, "let*", /*sequential=*/ true)
}

fn sf_let_common(args: &Sexp, env: &mut Env, name: &str, sequential: bool) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: name.into(),
            expected: "≥1".into(),
            got: 0,
        });
    }
    let bindings = list_elements(&parts[0])?;
    // `let' evaluates values in the OUTER env first (= before push);
    // `let*' evaluates inside the growing frame.
    let pre_evaluated = if sequential {
        None
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for b in &bindings {
            values.push(parse_let_binding(b, env)?);
        }
        Some(values)
    };
    env.push_frame();
    let result = (|| -> Result<Sexp, EvalError> {
        if let Some(values) = pre_evaluated {
            for (n, v) in values { env.bind_local(&n, v); }
        } else {
            for b in &bindings {
                let (n, v) = parse_let_binding(b, env)?;
                env.bind_local(&n, v);
            }
        }
        eval_body(&parts[1..], env)
    })();
    env.pop_frame();
    result
}

fn parse_let_binding(b: &Sexp, env: &mut Env) -> Result<(String, Sexp), EvalError> {
    match b {
        Sexp::Symbol(name) => Ok((name.clone(), Sexp::Nil)),
        Sexp::Cons(_) => {
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
        other => return Err(EvalError::WrongType {
            expected: "symbol or nil".into(),
            got: other.clone(),
        }),
    };
    let protected = &parts[1];
    let handlers: Vec<Sexp> = parts.iter().skip(2).cloned().collect();
    match eval(protected, env) {
        Ok(v) => Ok(v),
        // `throw' is control flow, not error — pass through so the
        // upstream `catch' can handle it.  Explicit `no-catch'
        // handler clause catches it here (= Emacs parity).
        Err(EvalError::UncaughtThrow { tag, value }) => {
            for handler in &handlers {
                let h_parts = list_elements(handler)?;
                if h_parts.is_empty() {
                    continue;
                }
                let claims_no_catch = match &h_parts[0] {
                    Sexp::Symbol(s) => s == "no-catch",
                    Sexp::Cons(_) => {
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
                    Sexp::Cons(_) => {
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
            if cleanup_err.is_none() {
                cleanup_err = Some(e);
            }
        }
    }
    // Cleanup error takes precedence over body's outcome (Emacs parity).
    cleanup_err.map_or(body_result, Err)
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
/// `extern "C"` wrapper around [`sexp_eq`] for the Phase-47-compiled
/// JIT eq trampolines (= reached via `(extern-call nl_sexp_eq A B)`).
/// Returns i64 = 1 iff equal else 0.
///
/// # Safety
/// `a` and `b` must be non-null pointers to initialized `Sexp` values.
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_eq(a: *const Sexp, b: *const Sexp) -> i64 {
    if sexp_eq(&*a, &*b) {
        1
    } else {
        0
    }
}

/// `eq' — symbols by name, integers by value, heap types by
/// ptr_eq (= same allocation, so cyclic graphs don't recurse).
pub fn sexp_eq(a: &Sexp, b: &Sexp) -> bool {
    match (a, b) {
        (Sexp::Nil, Sexp::Nil) | (Sexp::T, Sexp::T) => true,
        (Sexp::Int(x), Sexp::Int(y)) => x == y,
        (Sexp::Symbol(x), Sexp::Symbol(y)) => {
            let _ = (x, y);
            let mut slot = Sexp::Nil;
            unsafe {
                crate::elisp_cc_spike::eq_symbol(
                    a as *const Sexp,
                    b as *const Sexp,
                    &mut slot as *mut Sexp,
                );
            }
            matches!(slot, Sexp::T)
        }
        (Sexp::Cons(a), Sexp::Cons(b)) => crate::eval::nlconsbox::NlConsBoxRef::ptr_eq(a, b),
        (Sexp::MutStr(a), Sexp::MutStr(b)) => crate::eval::nlstr::NlStrRef::ptr_eq(a, b),
        (Sexp::Vector(a), Sexp::Vector(b)) => crate::eval::nlvector::NlVectorRef::ptr_eq(a, b),
        (Sexp::CharTable(a), Sexp::CharTable(b)) => crate::eval::nlchartable::NlCharTableRef::ptr_eq(a, b),
        (Sexp::BoolVector(a), Sexp::BoolVector(b)) => crate::eval::nlboolvector::NlBoolVectorRef::ptr_eq(a, b),
        (Sexp::Record(a), Sexp::Record(b)) => crate::eval::nlrecord::NlRecordRef::ptr_eq(a, b),
        // Strings + floats: bootstrap structural eq.
        (Sexp::Str(x), Sexp::Str(y)) => x == y,
        (Sexp::Float(x), Sexp::Float(y)) => x.to_bits() == y.to_bits(),
        _ => false,
    }
}

