use super::Env;
use super::error::{is_error_subtype, EvalError};
use super::sexp::Sexp;
use super::{eval, list_elements};

pub fn apply_special(
    name: &str,
    args: &Sexp,
    env: &mut Env,
) -> Result<Option<Sexp>, EvalError> {
    Ok(Some(match name {
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
    }))
}

fn first_arg(args: &Sexp, op: &str) -> Result<Sexp, EvalError> {
    match args {
        Sexp::Cons(b) => Ok(b.car.clone()),
        _ => Err(wrong_args(op, "≥1", 0)),
    }
}

fn args_vec(args: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    list_elements(args)
}

fn wrong_args(function: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongNumberOfArguments {
        function: function.into(),
        expected: expected.into(),
        got,
    }
}

fn expect_len(parts: &[Sexp], name: &str, expected: usize) -> Result<(), EvalError> {
    if parts.len() == expected {
        Ok(())
    } else {
        Err(wrong_args(name, &expected.to_string(), parts.len()))
    }
}

fn expect_min_len(parts: &[Sexp], name: &str, min: usize) -> Result<(), EvalError> {
    if parts.len() >= min {
        Ok(())
    } else {
        Err(wrong_args(name, &format!("≥{min}"), parts.len()))
    }
}

fn with_frame<T>(env: &mut Env, body: impl FnOnce(&mut Env) -> Result<T, EvalError>) -> Result<T, EvalError> {
    env.push_frame();
    let result = body(env);
    env.pop_frame();
    result
}

pub fn is_truthy(v: &Sexp) -> bool {
    !matches!(v, Sexp::Nil)
}

fn sf_quote(args: &Sexp) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_len(&parts, "quote", 1)?;
    Ok(parts[0].clone())
}

fn sf_function(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let form = first_arg(args, "function")?;
    match &form {
        Sexp::Cons(b) if matches!(&b.car, Sexp::Symbol(s) if s == "lambda") => {
            let (params, body) = lambda_rest(&form)?;
            sf_lambda(&Sexp::cons(params, body), env)
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

fn sf_if(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, "if", 2)?;
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

fn sf_let(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    sf_let_common(args, env, "let", false)
}

fn sf_let_star(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    sf_let_common(args, env, "let*", true)
}

fn sf_let_common(args: &Sexp, env: &mut Env, name: &str, sequential: bool) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, name, 1)?;
    let bindings = list_elements(&parts[0])?;
    let pre_evaluated = if sequential {
        None
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for b in &bindings {
            values.push(parse_let_binding(b, env)?);
        }
        Some(values)
    };
    with_frame(env, |env| {
        if let Some(values) = pre_evaluated {
            for (n, v) in values { env.bind_local(&n, v); }
        } else {
            for b in &bindings {
                let (n, v) = parse_let_binding(b, env)?;
                env.bind_local(&n, v);
            }
        }
        eval_body(&parts[1..], env)
    })
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

fn sf_lambda(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, "lambda", 1)?;
    let formals = parts[0].clone();
    let body: Vec<Sexp> = parts.iter().skip(1).cloned().collect();
    let captured = env.capture_lexical();
    let mut chain = vec![Sexp::Symbol("closure".into()), captured, formals];
    chain.extend(body);
    Ok(Sexp::list_from(&chain))
}

fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    if parts.len() % 2 != 0 {
        return Err(wrong_args("setq", "even number", parts.len()));
    }
    let mut last = Sexp::Nil;
    let mut iter = parts.into_iter();
    while let Some(name_form) = iter.next() {
        let value_form = iter.next().unwrap();
        let name = match name_form {
            Sexp::Symbol(s) => s,
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

fn sf_while(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, "while", 1)?;
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

fn clause_parts(clause: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    list_elements(clause)
}

fn clause_matches(tag_form: &Sexp, tag: &str) -> Result<bool, EvalError> {
    Ok(match tag_form {
        Sexp::Symbol(s) => is_error_subtype(s, tag),
        Sexp::T => true,
        Sexp::Cons(_) => list_elements(tag_form)?.iter().any(|t| {
            matches!(t, Sexp::Symbol(s) if is_error_subtype(s, tag))
        }),
        _ => false,
    })
}

fn eval_handler(
    env: &mut Env,
    var: Option<&str>,
    value: Option<Sexp>,
    handler: &[Sexp],
) -> Result<Sexp, EvalError> {
    with_frame(env, |env| {
        if let (Some(name), Some(value)) = (var, value) {
            env.bind_local(name, value);
        }
        eval_body(&handler[1..], env)
    })
}

fn sf_condition_case(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, "condition-case", 2)?;
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
        Err(EvalError::UncaughtThrow { tag, value }) => {
            for handler in &handlers {
                let parts = clause_parts(handler)?;
                if parts.is_empty() {
                    continue;
                }
                if clause_matches(&parts[0], "no-catch")? {
                    let data = Sexp::cons(
                        Sexp::Symbol("no-catch".into()),
                        Sexp::cons(tag.clone(), Sexp::cons(value.clone(), Sexp::Nil)),
                    );
                    return eval_handler(env, var.as_deref(), Some(data), &parts);
                }
            }
            Err(EvalError::UncaughtThrow { tag, value })
        }
        Err(e) => {
            let actual_tag = e.error_tag().to_string();
            for handler in &handlers {
                let parts = clause_parts(handler)?;
                if parts.is_empty() {
                    continue;
                }
                if clause_matches(&parts[0], &actual_tag)? {
                    return eval_handler(env, var.as_deref(), Some(e.signal_data()), &parts);
                }
            }
            Err(e)
        }
    }
}

fn sf_unwind_protect(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, "unwind-protect", 1)?;
    let body_result = eval(&parts[0], env);
    let mut cleanup_err: Option<EvalError> = None;
    for cleanup in parts.iter().skip(1) {
        if let Err(e) = eval(cleanup, env) {
            if cleanup_err.is_none() {
                cleanup_err = Some(e);
            }
        }
    }
    cleanup_err.map_or(body_result, Err)
}

fn sf_progn(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    eval_body(&args_vec(args)?, env)
}

fn sf_catch(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = args_vec(args)?;
    expect_min_len(&parts, "catch", 1)?;
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
    expect_len(&parts, "throw", 2)?;
    let tag = eval(&parts[0], env)?;
    let value = eval(&parts[1], env)?;
    Err(EvalError::UncaughtThrow { tag, value })
}
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_eq(a: *const Sexp, b: *const Sexp) -> i64 {
    if sexp_eq(&*a, &*b) {
        1
    } else {
        0
    }
}

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
        (Sexp::Str(x), Sexp::Str(y)) => x == y,
        (Sexp::Float(x), Sexp::Float(y)) => x.to_bits() == y.to_bits(),
        _ => false,
    }
}
