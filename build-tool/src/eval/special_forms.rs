use super::error::{is_error_subtype, EvalError};
use super::sexp::Sexp;
use super::Env;
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
        if rc == 0 {
            Ok::<_, EvalError>(out)
        } else {
            Err(super::consume_stashed_error($env, $name))
        }
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
        if rc == 0 {
            Ok::<_, EvalError>(out)
        } else {
            Err(super::consume_stashed_error($env, $name))
        }
    }};
}

pub fn apply_special(name: &str, args: &Sexp, env: &mut Env) -> Result<Option<Sexp>, EvalError> {
    Ok(Some(match name {
        "quote" => {
            let mut out = Sexp::Nil;
            let rc = unsafe {
                crate::elisp_cc_spike::sf_quote_call(args as *const Sexp, &mut out as *mut Sexp)
            };
            if rc == 0 {
                out
            } else {
                return Err(wrong_args("quote", "1", 0));
            }
        }
        "function" => sf_call_with_s1!("sf_function", sf_function_call, args, env)?,
        "if" => sf_call_4arg!("sf_if", sf_if_call, args, env)?,
        "let" => sf_call_4arg!("sf_let", sf_let_call, args, env)?,
        "let*" => sf_call_4arg!("sf_let_star", sf_let_star_call, args, env)?,
        "lambda" => sf_call_with_s1!("sf_lambda", sf_lambda_call, args, env)?,
        "setq" => sf_call_4arg!("sf_setq", sf_setq_call, args, env)?,
        "while" => sf_call_4arg!("sf_while", sf_while_call, args, env)?,
        "condition-case" => {
            sf_call_with_s1!("sf_condition_case", sf_condition_case_call, args, env)?
        }
        "unwind-protect" => sf_call_4arg!("sf_unwind_protect", sf_unwind_protect_call, args, env)?,
        "progn" => sf_call_4arg!("sf_progn", sf_progn_call, args, env)?,
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
                Err(_) => {
                    env_ref.pop_frame();
                    return 1;
                }
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

/// Phase 47 `nl_sf_condition_case' clause matcher + frame setup.  On match:
/// push frame, bind `var' to err, write BODY into `*err_inout', return 0.
/// On miss: re-stash err into `nelisp--last-signal-data' for the dispatch
/// macro's `consume_stashed_error', return 1.
#[no_mangle]
pub unsafe extern "C" fn nl_cc_match_and_bind(
    clauses: *const Sexp,
    err_inout: *mut Sexp,
    var: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let err_owned = (*err_inout).clone();
    let actual_tag = match &err_owned {
        Sexp::Cons(b) => match &b.car {
            Sexp::Symbol(s) => s.clone(),
            _ => return 1,
        },
        _ => return 1,
    };
    let mut cur = &*clauses;
    while let Sexp::Cons(cc) = cur {
        if let Sexp::Cons(cb) = &cc.car {
            let m = match &cb.car {
                Sexp::Symbol(s) => is_error_subtype(s, &actual_tag),
                Sexp::T => true,
                Sexp::Cons(_) => list_elements(&cb.car).ok().map_or(false, |elts| {
                    elts.iter()
                        .any(|t| matches!(t, Sexp::Symbol(s) if is_error_subtype(s, &actual_tag)))
                }),
                _ => false,
            };
            if m {
                env_ref.push_frame();
                if let Sexp::Symbol(name) = &*var {
                    if name != "nil" {
                        env_ref.bind_local(name, err_owned.clone());
                    }
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

/// Phase 47 .o bind_formals_impl helpers.
///
/// All functions below are called from `nl_bind_formals_impl' in
/// `lisp/nelisp-cc-bind-formals.el' (Stage 1 parallel implementation).
/// They provide the primitive operations needed by the elisp CPS chain.
///
/// `nl_bf_precompute(formals, args) -> i64`
/// Returns the initial `state' word for the `nl_bind_formals_impl' CPS
/// chain.  State layout (packed i64):
///   bits 0-3:    mode=0, saw-rest=0, consumed-rest=0  (all zero initially)
///   bits 4-19:   idx=0                                (all zero initially)
///   bits 20-35:  args_len (clamped to 16 bits)
///   bits 36+:    required (clamped to 16 bits)
///
/// Both counts are clamped to 0xFFFF; real Lisp functions never approach
/// that limit.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_precompute(formals_ptr: *const Sexp, args_ptr: *const Sexp) -> i64 {
    let names = match super::list_elements(&*formals_ptr) {
        Ok(v) => v,
        Err(_) => return 0,
    };
    let args_len = match super::list_elements(&*args_ptr) {
        Ok(v) => v.len(),
        Err(_) => return 0,
    };
    let required = names
        .iter()
        .take_while(|f| !matches!(f, Sexp::Symbol(s) if s == "&optional" || s == "&rest"))
        .count();
    let required_clamped = required.min(0xFFFF) as i64;
    let args_len_clamped = args_len.min(0xFFFF) as i64;
    // bits 20-35 = args_len, bits 36+ = required, bits 0-19 = 0 (mode/idx/flags)
    (args_len_clamped << 20) | (required_clamped << 36)
}

/// `nl_bf_args_tail(args_ptr, idx, out) -> i64`
/// Builds a new Sexp::Cons list from `args[idx..]' and writes it into
/// `*out'.  Used by the Rest mode branch to collect remaining args.
/// Returns 0 (always succeeds — worst case writes Sexp::Nil when empty).
#[no_mangle]
pub unsafe extern "C" fn nl_bf_args_tail(args_ptr: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    let args = match super::list_elements(&*args_ptr) {
        Ok(v) => v,
        Err(_) => {
            std::ptr::write(out, Sexp::Nil);
            return 0;
        }
    };
    let tail_start = (idx as usize).min(args.len());
    std::ptr::write(out, Sexp::list_from(&args[tail_start..]));
    0
}

/// `nl_bf_bind_sym(env, name_ptr, val_ptr) -> i64`
/// Calls `env.bind_local(name, val.clone())'.  `name_ptr' must point to a
/// `Sexp::Symbol'; if it points to any other variant the bind is skipped
/// and 0 is still returned (caller already validated tag == Symbol).
/// Returns 0 always.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_bind_sym(
    env: *mut std::ffi::c_void,
    name_ptr: *const Sexp,
    val_ptr: *const Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    if let Sexp::Symbol(name) = &*name_ptr {
        env_ref.bind_local(name, (*val_ptr).clone());
    }
    0
}

/// `nl_bf_bind_optional(env, name_ptr, args_ptr, idx) -> i64`
/// Optional-mode bind: binds `args[idx]' if idx < len(args), else Nil.
/// Advances idx by 1 if the arg was consumed.
/// Returns the new idx (= idx+1 if consumed, else idx).
/// Used by the Optional branch in `nl_bind_formals_impl' so the elisp
/// does not need a *const Sexp pointing at Nil.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_bind_optional(
    env: *mut std::ffi::c_void,
    name_ptr: *const Sexp,
    args_ptr: *const Sexp,
    idx: i64,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    if let Sexp::Symbol(name) = &*name_ptr {
        let args = match super::list_elements(&*args_ptr) {
            Ok(v) => v,
            Err(_) => {
                env_ref.bind_local(name, Sexp::Nil);
                return idx;
            }
        };
        let idx_usize = idx as usize;
        let val = args.get(idx_usize).cloned().unwrap_or(Sexp::Nil);
        let new_idx = if idx_usize < args.len() { idx + 1 } else { idx };
        env_ref.bind_local(name, val);
        return new_idx;
    }
    idx
}

/// `nl_bf_err_arity(env, required, got) -> i64`
/// Stashes a `WrongNumberOfArguments' error into
/// `nelisp--last-signal-data' and returns 1.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_err_arity(
    env: *mut std::ffi::c_void,
    required: i64,
    got: i64,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let err = EvalError::WrongNumberOfArguments {
        function: "lambda".into(),
        expected: required.to_string(),
        got: got as usize,
    };
    let _ = env_ref.set_value("nelisp--last-signal-data", err.signal_data());
    1
}

/// `nl_bf_err_type(env, name_ptr) -> i64`
/// Stashes a `WrongType' error into `nelisp--last-signal-data'.
/// Used for: non-Symbol formal, `&optional' after `&rest', double `&rest',
/// and extra symbol after `&rest'.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_err_type(env: *mut std::ffi::c_void, name_ptr: *const Sexp) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let got = (*name_ptr).clone();
    let err = EvalError::WrongType {
        expected: "symbol".into(),
        got,
    };
    let _ = env_ref.set_value("nelisp--last-signal-data", err.signal_data());
    1
}

/// `nl_bf_bind_rest(env, name_ptr, args_ptr, idx) -> i64`
/// Rest-mode bind: builds `list(args[idx..])' and calls
/// `env.bind_local(name, list)'.  Returns 0.
/// Combines nl_bf_args_tail + nl_bf_bind_sym to avoid needing a
/// scratch *mut Sexp slot in the elisp CPS chain.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_bind_rest(
    env: *mut std::ffi::c_void,
    name_ptr: *const Sexp,
    args_ptr: *const Sexp,
    idx: i64,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    if let Sexp::Symbol(name) = &*name_ptr {
        let args = match super::list_elements(&*args_ptr) {
            Ok(v) => v,
            Err(_) => {
                env_ref.bind_local(name, Sexp::Nil);
                return 0;
            }
        };
        let tail_start = (idx as usize).min(args.len());
        env_ref.bind_local(name, Sexp::list_from(&args[tail_start..]));
    }
    0
}

/// `nl_bf_err_dangling_rest(env) -> i64`
/// Stashes `WrongType { expected: "symbol after &rest", got: Symbol("&rest") }`
/// for the case where `&rest' appears in formals but no symbol follows it.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_err_dangling_rest(env: *mut std::ffi::c_void) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let err = EvalError::WrongType {
        expected: "symbol after &rest".into(),
        got: Sexp::Symbol("&rest".into()),
    };
    let _ = env_ref.set_value("nelisp--last-signal-data", err.signal_data());
    1
}

#[no_mangle]
pub unsafe extern "C" fn nl_push_and_bind(
    formals_ptr: *const Sexp,
    args_list_ptr: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    env_ref.push_frame();
    let rc = crate::elisp_cc_spike::bind_formals_impl_call(formals_ptr, args_list_ptr, env, 0);
    if rc != 0 {
        env_ref.pop_frame();
    }
    rc
}

#[no_mangle]
pub unsafe extern "C" fn nl_bind_formals(
    formals_ptr: *const Sexp,
    args_list_ptr: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    crate::elisp_cc_spike::bind_formals_impl_call(formals_ptr, args_list_ptr, env, 0)
}

#[no_mangle]
pub unsafe extern "C" fn nl_env_capture_lexical(env: *mut std::ffi::c_void, out: *mut Sexp) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    std::ptr::write(out, env_ref.capture_lexical());
    0
}

// Body migrated to Phase 47 elisp: lisp/nelisp-cc-cons-prepend-clone.el
// Symbol `nl_cons_prepend_clone' is now provided by the elisp .o archive;
// bridge.rs extern decl + anchor keeps it live for extern-call resolution.
// `nl_symbol_is_lambda' also migrated to lisp/nelisp-cc-symbol-is-lambda.el
// in the prior commit on this branch.

#[no_mangle]
pub unsafe extern "C" fn nl_eval_is_truthy(form: *const Sexp, env: *mut std::ffi::c_void) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    match super::eval(&*form, env_ref) {
        Ok(v) => {
            if !matches!(v, Sexp::Nil) {
                1
            } else {
                0
            }
        }
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

// ─── Phase 47 eval_inner / apply_combiner ABI bridge ─────────────────────────
// Called from `nelisp-cc-eval-inner.o' which exports `nl_eval_inner'.
// The .o handles sexp-tag dispatch; these externs do the heavy lifting.

/// Variable lookup for a Symbol form (keywords self-evaluate).
/// Returns 0=found (value in *out), 1=err (stashed in nelisp--last-signal-data).
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

/// Cell form: extract stored value into *out. Returns 0=ok, 1=not-a-cell.
#[no_mangle]
pub unsafe extern "C" fn nl_cell_get_value(cell_ptr: *const Sexp, out: *mut Sexp) -> i64 {
    match &*cell_ptr { Sexp::Cell(c) => { std::ptr::write(out, c.value.clone()); 0 } _ => 1 }
}

/// apply_combiner for both Symbol-head and Cons/other-head forms.
/// head_ptr: car of the Cons being evaluated (may be Symbol or any Sexp).
/// tail_ptr: cdr of the Cons (unevaluated arg list).
/// Returns 0=ok (result in *out), 1=err (stashed in nelisp--last-signal-data).
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
        if parts.len()<2 { stash!(super::error::EvalError::Internal("malformed macro".into())) }
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
        (Sexp::CharTable(a), Sexp::CharTable(b)) => {
            crate::eval::nlchartable::NlCharTableRef::ptr_eq(a, b)
        }
        (Sexp::BoolVector(a), Sexp::BoolVector(b)) => {
            crate::eval::nlboolvector::NlBoolVectorRef::ptr_eq(a, b)
        }
        (Sexp::Record(a), Sexp::Record(b)) => crate::eval::nlrecord::NlRecordRef::ptr_eq(a, b),
        (Sexp::Str(x), Sexp::Str(y)) => x == y,
        (Sexp::Float(x), Sexp::Float(y)) => x.to_bits() == y.to_bits(),
        _ => false,
    }
}
