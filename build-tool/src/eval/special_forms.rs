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
    let mut out = Sexp::Nil;
    let rc = unsafe { crate::elisp_cc_spike::sf_quote_call(args as *const Sexp, &mut out as *mut Sexp) };
    if rc == 0 { Ok(out) } else { Err(wrong_args("quote", "1", 0)) }
}

fn sf_function(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let mut out = Sexp::Nil;
    let mut s1 = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_function_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            &mut s1 as *mut Sexp,
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_function".into())) }
}

fn sf_if(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_if_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0, // _pad: alignment pad (nl_sf_if is arity 4/even)
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_if".into())) }
}

fn sf_let(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_let_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0, // _pad: alignment pad (nl_sf_let is arity 4/even)
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_let".into())) }
}

fn sf_let_star(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_let_star_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0, // _pad: alignment pad (nl_sf_let_star is arity 4/even)
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_let_star".into())) }
}

/// Phase 47 elisp primitive — parse + evaluate all bindings in a `let' or
/// `let*' binding list, push a new lexical frame, and bind each variable.
///
/// bindings_list: *const Sexp — the raw bindings list (car of the let form's args).
/// env:           *mut c_void — &mut Env cast.
/// sequential:    i64 — 0 = parallel (let), 1 = sequential (let*).
///
/// For sequential=0: all values are evaluated in the outer frame before any
/// are bound (= standard `let' semantics).
/// For sequential=1: the frame is pushed first, then each binding is evaluated
/// and bound in order (= `let*' semantics: later bindings see earlier ones).
///
/// Returns: 0=Ok (frame IS pushed and all vars are bound),
///          1=Err (frame is NOT pushed; error details via separate channel).
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
        // let*: push frame first, then eval+bind sequentially.
        env_ref.push_frame();
        for b in &bindings {
            let (name, val) = match nl_let_parse_binding(b, env_ref) {
                Ok(pair) => pair,
                Err(_) => { env_ref.pop_frame(); return 1; }
            };
            env_ref.bind_local(&name, val);
        }
    } else {
        // let: pre-eval all bindings in the outer frame.
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

/// Internal helper: parse a single `let' binding form and evaluate its value.
/// Mirrors the deleted `parse_let_binding' private fn.
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

/// Phase 47 elisp primitive — pop the topmost lexical frame from env.
/// env: *mut c_void = &mut Env cast.
/// Returns: 0 always.
#[no_mangle]
pub unsafe extern "C" fn nl_env_pop_frame(env: *mut std::ffi::c_void) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    env_ref.pop_frame();
    0
}

/// Phase 47 ABI — push captured alist as a new lexical frame.
/// env:       *mut c_void  = &mut Env cast.
/// alist_ptr: *const Sexp  = captured alist (Nil = no-op).
/// Returns: 0=Ok, 1=Err.
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

/// Internal helper — bind formal parameter list to argument slice.
/// Contains the actual logic for `bind_formals', factored out so both
/// the Rust thin-shell `bind_formals' in `eval/mod.rs' and the ABI
/// externs `nl_push_and_bind' / `nl_bind_formals' can share it without
/// a circular call chain.
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

/// Phase 47 ABI — push a new (empty) lexical frame, then bind formals to args.
/// If the bind fails, the frame is popped before returning 1.
///
/// formals_ptr:   *const Sexp = cons list of formal parameter symbols.
/// args_list_ptr: *const Sexp = cons list of evaluated argument values.
/// env:           *mut c_void = &mut Env cast.
///
/// Returns: 0=Ok (frame pushed, all formals bound),
///          1=Err (frame popped on failure; no net frame change on error).
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

/// Phase 47 ABI — bind formals to arguments in the CURRENT lexical frame.
/// Does NOT push a frame; caller must have already pushed one.
///
/// formals_ptr:   *const Sexp = cons list of formal parameter symbols.
/// args_list_ptr: *const Sexp = cons list of evaluated argument values.
/// env:           *mut c_void = &mut Env cast.
/// Returns: 0=Ok, 1=Err.
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

/// Phase 47 ABI — capture lexical environment to *out. Returns 0.
#[no_mangle]
pub unsafe extern "C" fn nl_env_capture_lexical(
    env: *mut std::ffi::c_void,
    out: *mut Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    std::ptr::write(out, env_ref.capture_lexical());
    0
}

/// Phase 47 ABI — check if Sexp is Symbol("lambda").
#[no_mangle]
pub unsafe extern "C" fn nl_symbol_is_lambda(sym: *const Sexp) -> i64 {
    match &*sym {
        Sexp::Symbol(s) if s == "lambda" => 1,
        _ => 0,
    }
}

/// Phase 47 ABI — prepend new car to existing list (refcount-safe clone).
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
            0, // _pad: alignment pad (nl_sf_setq is arity 4/even)
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
            0, // _pad: alignment pad (nl_sf_while is arity 4/even)
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_while".into())) }
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
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::sf_progn_call(
            args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0, // _pad: alignment pad (nl_sf_progn is arity 4/even)
        )
    };
    if rc == 0 { Ok(out) } else { Err(EvalError::Internal("sf_progn".into())) }
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

/// Phase 47 elisp primitive — evaluate a form and return its truthiness.
/// Returns: 1=truthy, 0=nil/false, -1=eval error.
/// Called from elisp as `(extern-call nl_eval_is_truthy FORM-PTR ENV)'.
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

/// Phase 47 elisp primitive — set a variable in the environment.
/// env: *mut c_void (= *mut Env).
/// sym: *const Sexp pointing to a Symbol.
/// val: *const Sexp — the already-evaluated value (in the out slot).
/// Returns: 0=Ok, 1=Err.
/// Called from elisp as `(extern-call nl_env_set_value ENV SYM-PTR VAL-PTR)'.
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
