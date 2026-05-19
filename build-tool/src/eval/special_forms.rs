use super::Env;
use super::error::{is_error_subtype, EvalError};
use super::sexp::Sexp;
use super::{eval, list_elements};

macro_rules! sf_call_4arg {
    ($name:literal, $ext:ident, $args:expr, $env:expr) => {{
        let mut out = Sexp::Nil;
        let rc = unsafe {
            crate::elisp_cc_spike::$ext(
                $args as *const Sexp,
                $env as *mut Env as *mut std::ffi::c_void,
                &mut out as *mut Sexp,
                0,
            )
        };
        if rc == 0 { Ok::<_, EvalError>(out) } else { Err(EvalError::Internal($name.into())) }
    }};
}

macro_rules! sf_call_with_s1 {
    ($name:literal, $ext:ident, $args:expr, $env:expr) => {{
        let mut out = Sexp::Nil;
        let mut s1 = Sexp::Nil;
        let rc = unsafe {
            crate::elisp_cc_spike::$ext(
                $args as *const Sexp,
                $env as *mut Env as *mut std::ffi::c_void,
                &mut out as *mut Sexp,
                &mut s1 as *mut Sexp,
            )
        };
        if rc == 0 { Ok::<_, EvalError>(out) } else { Err(EvalError::Internal($name.into())) }
    }};
}

pub fn apply_special(
    name: &str,
    args: &Sexp,
    env: &mut Env,
) -> Result<Option<Sexp>, EvalError> {
    Ok(Some(match name {
        "quote" => {
            let mut out = Sexp::Nil;
            let rc = unsafe { crate::elisp_cc_spike::sf_quote_call(args as *const Sexp, &mut out as *mut Sexp) };
            if rc == 0 { out } else { return Err(wrong_args("quote", "1", 0)); }
        }
        "function" => sf_call_with_s1!("sf_function", sf_function_call, args, env)?,
        "if" => sf_call_4arg!("sf_if", sf_if_call, args, env)?,
        "let" => sf_call_4arg!("sf_let", sf_let_call, args, env)?,
        "let*" => sf_call_4arg!("sf_let_star", sf_let_star_call, args, env)?,
        "lambda" => sf_call_with_s1!("sf_lambda", sf_lambda_call, args, env)?,
        "setq" => sf_call_4arg!("sf_setq", sf_setq_call, args, env)?,
        "while" => sf_call_4arg!("sf_while", sf_while_call, args, env)?,
        "condition-case" => sf_condition_case(args, env)?,
        "unwind-protect" => sf_unwind_protect(args, env)?,
        "progn" => sf_call_4arg!("sf_progn", sf_progn_call, args, env)?,
        "catch" => sf_catch(args, env)?,
        "throw" => sf_throw(args, env)?,
        _ => return Ok(None),
    }))
}

fn wrong_args(function: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongNumberOfArguments {
        function: function.into(),
        expected: expected.into(),
        got,
    }
}

fn expect_len(parts: &[Sexp], name: &str, expected: usize) -> Result<(), EvalError> {
    if parts.len() == expected { Ok(()) }
    else { Err(wrong_args(name, &expected.to_string(), parts.len())) }
}

fn expect_min_len(parts: &[Sexp], name: &str, min: usize) -> Result<(), EvalError> {
    if parts.len() >= min { Ok(()) }
    else { Err(wrong_args(name, &format!("≥{min}"), parts.len())) }
}

pub fn is_truthy(v: &Sexp) -> bool {
    !matches!(v, Sexp::Nil)
}

/// `let' / `let*' frame setup: sequential=1 = `let*'.
#[no_mangle]
pub unsafe extern "C" fn nl_let_setup(
    bindings_list: *const Sexp,
    env: *mut std::ffi::c_void,
    sequential: i64,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let bindings = match list_elements(&*bindings_list) {
        Ok(b) => b,
        Err(_) => return 1,
    };
    if sequential != 0 {
        env_ref.push_frame();
        for b in &bindings {
            let (name, val) = match nl_let_parse_binding(b, env_ref) {
                Ok(pair) => pair,
                Err(_) => { env_ref.pop_frame(); return 1; }
            };
            env_ref.bind_local(&name, val);
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for b in &bindings {
            match nl_let_parse_binding(b, env_ref) {
                Ok(pair) => values.push(pair),
                Err(_) => return 1,
            }
        }
        env_ref.push_frame();
        for (name, val) in values {
            env_ref.bind_local(&name, val);
        }
    }
    0
}

fn nl_let_parse_binding(b: &Sexp, env: &mut Env) -> Result<(String, Sexp), EvalError> {
    match b {
        Sexp::Symbol(name) => Ok((name.clone(), Sexp::Nil)),
        Sexp::Cons(_) => {
            let parts = list_elements(b)?;
            let name = match &parts[0] {
                Sexp::Symbol(s) => s.clone(),
                other => return Err(EvalError::WrongType {
                    expected: "symbol".into(),
                    got: other.clone(),
                }),
            };
            let val = if parts.len() >= 2 { eval(&parts[1], env)? } else { Sexp::Nil };
            Ok((name, val))
        }
        other => Err(EvalError::WrongType {
            expected: "symbol or (symbol value) pair".into(),
            got: other.clone(),
        }),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_pop_frame(env: *mut std::ffi::c_void) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    env_ref.pop_frame();
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_push_captured(
    env: *mut std::ffi::c_void,
    alist_ptr: *const Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    match env_ref.push_captured(&*alist_ptr) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

fn bind_formals_impl(formals: &Sexp, args: &[Sexp], env: &mut Env) -> Result<(), EvalError> {
    let names = super::list_elements(formals)?;
    #[derive(Clone, Copy)]
    enum Mode { Required, Optional, Rest }
    let required = names.iter().take_while(|formal| {
        !matches!(formal, Sexp::Symbol(s) if s == "&optional" || s == "&rest")
    }).count();
    let (mut mode, mut idx, mut saw_rest, mut consumed_rest) =
        (Mode::Required, 0usize, false, false);
    for formal in names {
        let Sexp::Symbol(name) = formal else {
            return Err(EvalError::WrongType { expected: "symbol".into(), got: formal });
        };
        match name.as_str() {
            "&optional" => {
                if matches!(mode, Mode::Rest) {
                    return Err(EvalError::WrongType {
                        expected: "formal parameter after &rest".into(),
                        got: Sexp::Symbol(name),
                    });
                }
                mode = Mode::Optional;
            }
            "&rest" => {
                if saw_rest {
                    return Err(EvalError::WrongType {
                        expected: "single &rest marker".into(),
                        got: Sexp::Symbol(name),
                    });
                }
                mode = Mode::Rest;
                saw_rest = true;
            }
            _ if matches!(mode, Mode::Required) => {
                let Some(value) = args.get(idx) else {
                    return Err(EvalError::WrongNumberOfArguments {
                        function: "lambda".into(),
                        expected: required.to_string(),
                        got: args.len(),
                    });
                };
                env.bind_local(&name, value.clone());
                idx += 1;
            }
            _ if matches!(mode, Mode::Optional) => {
                env.bind_local(&name, args.get(idx).cloned().unwrap_or(Sexp::Nil));
                idx += usize::from(idx < args.len());
            }
            _ => {
                if consumed_rest {
                    return Err(EvalError::WrongType {
                        expected: "single symbol after &rest".into(),
                        got: Sexp::Symbol(name),
                    });
                }
                env.bind_local(&name, Sexp::list_from(&args[idx..]));
                idx = args.len();
                consumed_rest = true;
            }
        };
    }
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

#[no_mangle]
pub unsafe extern "C" fn nl_push_and_bind(
    formals_ptr: *const Sexp,
    args_list_ptr: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let formals = &*formals_ptr;
    let args_list = &*args_list_ptr;
    let args = match super::list_elements(args_list) {
        Ok(v) => v,
        Err(_) => return 1,
    };
    env_ref.push_frame();
    match bind_formals_impl(formals, &args, env_ref) {
        Ok(()) => 0,
        Err(_) => {
            env_ref.pop_frame();
            1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_bind_formals(
    formals_ptr: *const Sexp,
    args_list_ptr: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let formals = &*formals_ptr;
    let args_list = &*args_list_ptr;
    let args = match super::list_elements(args_list) {
        Ok(v) => v,
        Err(_) => return 1,
    };
    match bind_formals_impl(formals, &args, env_ref) {
        Ok(()) => 0,
        Err(_) => 1,
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
    let mut out = Sexp::Nil;
    let mut s1 = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_lambda_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            &mut s1 as *mut Sexp,
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_lambda".into())) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_capture_lexical(
    env: *mut std::ffi::c_void,
    out: *mut Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    std::ptr::write(out, env_ref.capture_lexical());
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_symbol_is_lambda(sym: *const Sexp) -> i64 {
    match &*sym {
        Sexp::Symbol(s) if s == "lambda" => 1,
        _ => 0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_cons_prepend_clone(
    car_ptr: *const Sexp,
    cdr_ptr: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let car_owned = (*car_ptr).clone();
    let cdr_owned = (*cdr_ptr).clone();
    *out = Sexp::cons(car_owned, cdr_owned);
    0
}

fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_setq_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0,
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_setq".into())) }
}

fn sf_while(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_while_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0,
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_while".into())) }
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
    env.push_frame();
    if let (Some(name), Some(value)) = (var, value) {
        env.bind_local(name, value);
    }
    let result = eval_body(&handler[1..], env);
    env.pop_frame();
    result
}

fn sf_condition_case(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = list_elements(args)?;
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
                let parts = list_elements(handler)?;
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
                let parts = list_elements(handler)?;
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
    let parts = list_elements(args)?;
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
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_progn_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0,
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_progn".into())) }
}

fn sf_catch(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let parts = list_elements(args)?;
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
    let parts = list_elements(args)?;
    expect_len(&parts, "throw", 2)?;
    let tag = eval(&parts[0], env)?;
    let value = eval(&parts[1], env)?;
    Err(EvalError::UncaughtThrow { tag, value })
}

#[no_mangle]
pub unsafe extern "C" fn nl_eval_is_truthy(
    form: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    match super::eval(&*form, env_ref) {
        Ok(v) => if !matches!(v, Sexp::Nil) { 1 } else { 0 },
        Err(_) => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_set_value(
    env: *mut std::ffi::c_void,
    sym: *const Sexp,
    val: *const Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let name = match &*sym {
        Sexp::Symbol(s) | Sexp::Str(s) => s.as_str(),
        _ => return 1,
    };
    match env_ref.set_value(name, (*val).clone()) {
        Ok(_) => 0,
        Err(_) => 1,
    }
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
