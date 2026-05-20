use super::error::{is_error_subtype, EvalError};
use super::sexp::Sexp;
use super::Env;
use super::{eval, list_elements};

macro_rules! sf_call_4arg {
    ($name:literal, $ext:ident, $args:expr, $env:expr) => {{
        let mut out = Sexp::Nil;
        let rc = unsafe { crate::elisp_cc_spike::$ext($args as *const Sexp, $env as *mut Env as *mut std::ffi::c_void, &mut out as *mut Sexp, 0) };
        if rc == 0 { Ok::<_, EvalError>(out) } else { Err(super::consume_stashed_error($env, $name)) }
    }};
}

macro_rules! sf_call_with_s1 {
    ($name:literal, $ext:ident, $args:expr, $env:expr) => {{
        let mut out = Sexp::Nil; let mut s1 = Sexp::Nil;
        let rc = unsafe { crate::elisp_cc_spike::$ext($args as *const Sexp, $env as *mut Env as *mut std::ffi::c_void, &mut out as *mut Sexp, &mut s1 as *mut Sexp) };
        if rc == 0 { Ok::<_, EvalError>(out) } else { Err(super::consume_stashed_error($env, $name)) }
    }};
}

pub fn apply_special(name: &str, args: &Sexp, env: &mut Env) -> Result<Option<Sexp>, EvalError> {
    Ok(Some(match name {
        "quote" => {
            let mut out = Sexp::Nil;
            let rc = unsafe { crate::elisp_cc_spike::sf_quote_call(args as *const Sexp, &mut out as *mut Sexp) };
            if rc == 0 { out } else { return Err(EvalError::wrong_arity("quote", "1", 0)); }
        }
        "function" => sf_call_with_s1!("sf_function", sf_function_call, args, env)?,
        "if" => sf_call_4arg!("sf_if", sf_if_call, args, env)?,
        "let" => sf_call_4arg!("sf_let", sf_let_call, args, env)?,
        "let*" => sf_call_4arg!("sf_let_star", sf_let_star_call, args, env)?,
        "lambda" => sf_call_with_s1!("sf_lambda", sf_lambda_call, args, env)?,
        "setq" => sf_call_4arg!("sf_setq", sf_setq_call, args, env)?,
        "while" => sf_call_4arg!("sf_while", sf_while_call, args, env)?,
        "condition-case" => sf_call_with_s1!("sf_condition_case", sf_condition_case_call, args, env)?,
        "unwind-protect" => sf_call_4arg!("sf_unwind_protect", sf_unwind_protect_call, args, env)?,
        "progn" => sf_call_4arg!("sf_progn", sf_progn_call, args, env)?,
        _ => return Ok(None),
    }))
}

#[no_mangle]
pub unsafe extern "C" fn nl_let_setup(
    bindings_list: *const Sexp, env: *mut std::ffi::c_void, sequential: i64,
) -> i64 {
    fn parse_binding(b: &Sexp, env: &mut Env) -> Result<(String, Sexp), EvalError> {
        match b {
            Sexp::Symbol(name) => Ok((name.clone(), Sexp::Nil)),
            Sexp::Cons(_) => {
                let parts = list_elements(b)?;
                let name = match &parts[0] {
                    Sexp::Symbol(s) => s.clone(),
                    other => return Err(EvalError::wrong_type("symbol", other.clone())),
                };
                let val = if parts.len() >= 2 { eval(&parts[1], env)? } else { Sexp::Nil };
                Ok((name, val))
            }
            other => Err(EvalError::wrong_type("symbol or (symbol value) pair", other.clone())),
        }
    }
    let env_ref = &mut *(env as *mut Env);
    let bindings = match list_elements(&*bindings_list) { Ok(b) => b, Err(_) => return 1 };
    if sequential != 0 {
        env_ref.frame_push_rust_direct();
        for b in &bindings {
            let (name, val) = match parse_binding(b, env_ref) {
                Ok(pair) => pair,
                Err(_) => { env_ref.frame_pop_rust_direct(); return 1; }
            };
            env_ref.bind_local(&name, val);
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for b in &bindings {
            match parse_binding(b, env_ref) { Ok(pair) => values.push(pair), Err(_) => return 1 }
        }
        env_ref.frame_push_rust_direct();
        for (name, val) in values { env_ref.bind_local(&name, val); }
    }
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_pop_frame(env: *mut std::ffi::c_void) -> i64 {
    (&mut *(env as *mut Env)).frame_pop_rust_direct(); 0
}
#[no_mangle]
pub unsafe extern "C" fn nl_env_push_captured(env: *mut std::ffi::c_void, alist_ptr: *const Sexp) -> i64 {
    match (&mut *(env as *mut Env)).push_captured(&*alist_ptr) { Ok(()) => 0, Err(_) => 1 }
}

#[no_mangle]
pub unsafe extern "C" fn nl_cc_match_and_bind(
    clauses: *const Sexp, err_inout: *mut Sexp, var: *const Sexp, env: *mut std::ffi::c_void,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let err_owned = (*err_inout).clone();
    let actual_tag = match &err_owned {
        Sexp::Cons(b) => match &b.car { Sexp::Symbol(s) => s.clone(), _ => return 1 },
        _ => return 1,
    };
    let mut cur = &*clauses;
    while let Sexp::Cons(cc) = cur {
        if let Sexp::Cons(cb) = &cc.car {
            let m = match &cb.car {
                Sexp::Symbol(s) => is_error_subtype(s, &actual_tag),
                Sexp::T => true,
                Sexp::Cons(_) => list_elements(&cb.car).ok().map_or(false, |elts| {
                    elts.iter().any(|t| matches!(t, Sexp::Symbol(s) if is_error_subtype(s, &actual_tag)))
                }),
                _ => false,
            };
            if m {
                env_ref.frame_push_rust_direct();
                if let Sexp::Symbol(name) = &*var {
                    if name != "nil" { env_ref.bind_local(name, err_owned.clone()); }
                }
                std::ptr::write(err_inout, cb.cdr.clone());
                return 0;
            }
        }
        cur = &cc.cdr;
    }
    let _ = env_ref.set_value("nelisp--last-signal-data", err_owned);
    1
}

// nl_bf_* — bind_formals_impl helpers (nelisp-cc-bind-formals.el).
#[no_mangle]
pub unsafe extern "C" fn nl_bf_bind_sym(
    env: *mut std::ffi::c_void, name_ptr: *const Sexp, val_ptr: *const Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    if let Sexp::Symbol(name) = &*name_ptr { env_ref.bind_local(name, (*val_ptr).clone()); }
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_bf_bind_optional(
    env: *mut std::ffi::c_void, name_ptr: *const Sexp, args_ptr: *const Sexp, idx: i64,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    if let Sexp::Symbol(name) = &*name_ptr {
        let args = match super::list_elements(&*args_ptr) {
            Ok(v) => v,
            Err(_) => { env_ref.bind_local(name, Sexp::Nil); return idx; }
        };
        let idx_usize = idx as usize;
        let val = args.get(idx_usize).cloned().unwrap_or(Sexp::Nil);
        let new_idx = if idx_usize < args.len() { idx + 1 } else { idx };
        env_ref.bind_local(name, val);
        return new_idx;
    }
    idx
}

#[no_mangle]
pub unsafe extern "C" fn nl_bf_err_arity(env: *mut std::ffi::c_void, required: i64, got: i64) -> i64 {
    let _ = (&mut *(env as *mut Env)).set_value("nelisp--last-signal-data", EvalError::wrong_arity("lambda", required.to_string(), got as usize).signal_data());
    1
}

#[no_mangle]
pub unsafe extern "C" fn nl_bf_err_type(env: *mut std::ffi::c_void, name_ptr: *const Sexp) -> i64 {
    let _ = (&mut *(env as *mut Env)).set_value("nelisp--last-signal-data", EvalError::wrong_type("symbol", (*name_ptr).clone()).signal_data());
    1
}

#[no_mangle]
pub unsafe extern "C" fn nl_bf_bind_rest(
    env: *mut std::ffi::c_void, name_ptr: *const Sexp, args_ptr: *const Sexp, idx: i64,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    if let Sexp::Symbol(name) = &*name_ptr {
        let args = match super::list_elements(&*args_ptr) {
            Ok(v) => v,
            Err(_) => { env_ref.bind_local(name, Sexp::Nil); return 0; }
        };
        env_ref.bind_local(name, Sexp::list_from(&args[(idx as usize).min(args.len())..]));
    }
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_bf_err_dangling_rest(env: *mut std::ffi::c_void) -> i64 {
    let _ = (&mut *(env as *mut Env)).set_value("nelisp--last-signal-data", EvalError::wrong_type("symbol after &rest", Sexp::Symbol("&rest".into())).signal_data());
    1
}

#[no_mangle]
pub unsafe extern "C" fn nl_push_and_bind(formals_ptr: *const Sexp, args_list_ptr: *const Sexp, env: *mut std::ffi::c_void) -> i64 {
    let e = &mut *(env as *mut Env);
    e.frame_push_rust_direct();
    let rc = crate::elisp_cc_spike::bind_formals_impl_call(formals_ptr, args_list_ptr, env, 0);
    if rc != 0 { e.frame_pop_rust_direct(); }
    rc
}
#[no_mangle]
pub unsafe extern "C" fn nl_env_capture_lexical(env: *mut std::ffi::c_void, out: *mut Sexp) -> i64 {
    std::ptr::write(out, (&mut *(env as *mut Env)).capture_lexical()); 0
}

#[no_mangle]
pub unsafe extern "C" fn nl_eval_is_truthy(form: *const Sexp, env: *mut std::ffi::c_void) -> i64 {
    match super::eval(&*form, &mut *(env as *mut Env)) {
        Ok(v) => if !matches!(v, Sexp::Nil) { 1 } else { 0 },
        Err(_) => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_set_value(env: *mut std::ffi::c_void, sym: *const Sexp, val: *const Sexp) -> i64 {
    let e = &mut *(env as *mut Env);
    let name = match &*sym { Sexp::Symbol(s) | Sexp::Str(s) => s.as_str(), _ => return 1 };
    match e.set_value(name, (*val).clone()) { Ok(_) => 0, Err(_) => 1 }
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_lookup_val(
    name_ptr: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp,
) -> i64 {
    let e = &mut *(env as *mut Env);
    let Sexp::Symbol(n) = &*name_ptr else { return 1; };
    if n.starts_with(':') && n.len() > 1 { super::sexp::nl_sexp_clone_into(name_ptr, out); return 0; }
    match e.lookup_value(n) {
        Ok(v) => { std::ptr::write(out, v); 0 }
        Err(er) => { let _ = e.set_value("nelisp--last-signal-data", er.signal_data()); 1 }
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_cell_get_value(cell_ptr: *const Sexp, out: *mut Sexp) -> i64 {
    match &*cell_ptr { Sexp::Cell(c) => { std::ptr::write(out, c.value.clone()); 0 } _ => 1 }
}
#[no_mangle]
pub unsafe extern "C" fn nl_eval_inner_cons(
    head_ptr: *const Sexp, tail_ptr: *const Sexp,
    env: *mut std::ffi::c_void, out: *mut Sexp,
) -> i64 {
    let e = &mut *(env as *mut Env);
    macro_rules! tri { ($x:expr) => { match $x { Ok(v)=>v, Err(er)=>{ let _=e.set_value("nelisp--last-signal-data",er.signal_data()); return 1; } } }; }
    macro_rules! put { ($v:expr) => {{ std::ptr::write(out,$v); return 0; }}; }
    macro_rules! stash { ($er:expr) => {{ let _=e.set_value("nelisp--last-signal-data",$er.signal_data()); return 1; }}; }
    let (head, tail) = (&*head_ptr, &*tail_ptr);
    let Sexp::Symbol(name) = head else {
        let func = tri!(super::eval(head, e));
        let args = tri!(super::eval_arg_list(tail, e));
        let is_bi = matches!(&func, Sexp::Cons(c) if matches!(&c.car, Sexp::Symbol(s) if s=="builtin"));
        if e.use_elisp_apply && e.delegation_depth==0 && !is_bi {
            let al = Sexp::list_from(&args);
            let f = Sexp::list_from(&[Sexp::Symbol("nelisp--apply-fn".into()),Sexp::list_from(&[Sexp::Symbol("quote".into()),func.clone()]),Sexp::list_from(&[Sexp::Symbol("quote".into()),al])]);
            e.delegation_depth+=1; let r=super::eval(&f,e); e.delegation_depth-=1;
            return match r { Ok(v)=>{ put!(v) } Err(er)=>{ let _=e.set_value("nelisp--last-signal-data",er.signal_data()); 1 } };
        }
        return match super::apply_function(&func,&args,e) { Ok(v)=>{ put!(v) } Err(er)=>{ stash!(er) } };
    };
    match apply_special(name, tail, e) {
        Ok(Some(v)) => put!(v), Ok(None) => {}, Err(er) => stash!(er),
    }
    let func = tri!(e.lookup_function(name));
    let is_mac = matches!(&func, Sexp::Cons(c) if matches!(&c.car, Sexp::Symbol(s) if s=="macro"));
    let is_hlp = matches!(name.as_str(),"nelisp--apply-fn"|"nelisp--apply-closure"|"nelisp--apply-lambda"|"nelisp--bind-formals--compute"|"nelisp--bind-formals--required-count"|"nelisp--builtinp"|"nelisp--closurep"|"nelisp--lambdap"|"nelisp--macrop"|"nelisp--expand-macro");
    if is_mac {
        if e.delegation_depth==0 && !is_hlp && e.lookup_function("nelisp--expand-macro").is_ok() {
            let f = Sexp::list_from(&[Sexp::Symbol("nelisp--expand-macro".into()),Sexp::list_from(&[Sexp::Symbol("quote".into()),func.clone()]),Sexp::list_from(&[Sexp::Symbol("quote".into()),tail.clone()])]);
            e.delegation_depth+=1;
            let exp = match super::eval(&f,e) { Ok(v)=>{e.delegation_depth-=1;v}, Err(er)=>{e.delegation_depth-=1; stash!(er)} };
            return match super::eval(&exp,e) { Ok(v)=>{ put!(v) } Err(er)=>{ stash!(er) } };
        }
        let parts = tri!(super::list_elements(&func));
        if parts.len()<2 { stash!(super::error::EvalError::internal("malformed macro")) }
        let af = tri!(super::list_elements(tail));
        let exp = tri!(super::apply_function(&parts[1], &af, e));
        return match super::eval(&exp,e) { Ok(v)=>{ put!(v) } Err(er)=>{ stash!(er) } };
    }
    let args = tri!(super::eval_arg_list(tail, e));
    let is_bi = matches!(&func, Sexp::Cons(c) if matches!(&c.car, Sexp::Symbol(s) if s=="builtin"));
    if e.use_elisp_apply && e.delegation_depth==0 && !is_hlp && !is_bi {
        let al = Sexp::list_from(&args);
        let f = Sexp::list_from(&[Sexp::Symbol("nelisp--apply-fn".into()),Sexp::list_from(&[Sexp::Symbol("quote".into()),func.clone()]),Sexp::list_from(&[Sexp::Symbol("quote".into()),al])]);
        e.delegation_depth+=1; let r=super::eval(&f,e); e.delegation_depth-=1;
        return match r { Ok(v)=>{ put!(v) } Err(er)=>{ let _=e.set_value("nelisp--last-signal-data",er.signal_data()); 1 } };
    }
    match super::apply_function(&func,&args,e) { Ok(v)=>{ put!(v) } Err(er)=>{ stash!(er) } }
}

