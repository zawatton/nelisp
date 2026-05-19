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
        if rc == 0 { Ok::<_, EvalError>(out) } else { Err(super::consume_stashed_error($env, $name)) }
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
        if rc == 0 { Ok::<_, EvalError>(out) } else { Err(super::consume_stashed_error($env, $name)) }
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
        "condition-case" => sf_call_with_s1!("sf_condition_case", sf_condition_case_call, args, env)?,
        "unwind-protect" => sf_unwind_protect(args, env)?,
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

fn expect_min_len(parts: &[Sexp], name: &str, min: usize) -> Result<(), EvalError> {
    if parts.len() >= min { Ok(()) }
    else { Err(wrong_args(name, &format!("≥{min}"), parts.len())) }
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
                    elts.iter().any(|t| matches!(t, Sexp::Symbol(s) if is_error_subtype(s, &actual_tag)))
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

fn bad_sym(expected: &str, name: String) -> EvalError {
    EvalError::WrongType {
        expected: expected.into(),
        got: Sexp::Symbol(name),
    }
}

fn wrong_lambda_arity(expected: String, got: usize) -> EvalError {
    EvalError::WrongNumberOfArguments {
        function: "lambda".into(),
        expected,
        got,
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
                    return Err(bad_sym("formal parameter after &rest", name));
                }
                mode = Mode::Optional;
            }
            "&rest" => {
                if saw_rest {
                    return Err(bad_sym("single &rest marker", name));
                }
                mode = Mode::Rest;
                saw_rest = true;
            }
            _ if matches!(mode, Mode::Required) => {
                let Some(value) = args.get(idx) else {
                    return Err(wrong_lambda_arity(required.to_string(), args.len()));
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
                    return Err(bad_sym("single symbol after &rest", name));
                }
                env.bind_local(&name, Sexp::list_from(&args[idx..]));
                idx = args.len();
                consumed_rest = true;
            }
        };
    }
    if idx < args.len() && !consumed_rest {
        return Err(wrong_lambda_arity(format!("at most {}", idx), args.len()));
    }
    if saw_rest && !consumed_rest {
        return Err(bad_sym("symbol after &rest", "&rest".into()));
    }
    Ok(())
}

unsafe fn bind_formals_common(
    formals_ptr: *const Sexp,
    args_list_ptr: *const Sexp,
    env: *mut std::ffi::c_void,
    push: bool,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let args = match super::list_elements(&*args_list_ptr) {
        Ok(v) => v,
        Err(_) => return 1,
    };
    if push {
        env_ref.push_frame();
    }
    match bind_formals_impl(&*formals_ptr, &args, env_ref) {
        Ok(()) => 0,
        Err(_) => {
            if push {
                env_ref.pop_frame();
            }
            1
        }
    }
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
pub unsafe extern "C" fn nl_bf_precompute(
    formals_ptr: *const Sexp,
    args_ptr: *const Sexp,
) -> i64 {
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

/// `nl_bf_formal_tag(name_ptr) -> i64`
/// Returns the role of the formal symbol pointed to by `name_ptr':
///   0 = regular binding symbol (bind a value)
///   1 = `&optional' marker (switch mode to Optional)
///   2 = `&rest' marker    (switch mode to Rest)
///  -1 = not a Symbol (WrongType — caller should signal error)
#[no_mangle]
pub unsafe extern "C" fn nl_bf_formal_tag(name_ptr: *const Sexp) -> i64 {
    match &*name_ptr {
        Sexp::Symbol(s) => match s.as_str() {
            "&optional" => 1,
            "&rest"     => 2,
            _           => 0,
        },
        _ => -1,
    }
}

/// `nl_bf_args_nth_ptr(args_ptr, idx) -> *const Sexp`
/// Returns a raw pointer (as i64) to the Sexp at position `idx' in the
/// cons list `*args_ptr'.  Returns 0 (null) when `idx >= list length'.
/// Used by the Required and Optional mode branches to fetch the argument
/// value without materialising the full Vec.
#[no_mangle]
pub unsafe extern "C" fn nl_bf_args_nth_ptr(args_ptr: *const Sexp, idx: i64) -> i64 {
    let mut cur = &*args_ptr;
    let mut remaining = idx;
    while let Sexp::Cons(b) = cur {
        if remaining == 0 {
            return &b.car as *const Sexp as i64;
        }
        remaining -= 1;
        cur = &b.cdr;
    }
    0
}

/// `nl_bf_args_tail(args_ptr, idx, out) -> i64`
/// Builds a new Sexp::Cons list from `args[idx..]' and writes it into
/// `*out'.  Used by the Rest mode branch to collect remaining args.
/// Returns 0 (always succeeds — worst case writes Sexp::Nil when empty).
#[no_mangle]
pub unsafe extern "C" fn nl_bf_args_tail(
    args_ptr: *const Sexp,
    idx: i64,
    out: *mut Sexp,
) -> i64 {
    let args = match super::list_elements(&*args_ptr) {
        Ok(v) => v,
        Err(_) => { std::ptr::write(out, Sexp::Nil); return 0; }
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
pub unsafe extern "C" fn nl_bf_err_type(
    env: *mut std::ffi::c_void,
    name_ptr: *const Sexp,
) -> i64 {
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
            Err(_) => { env_ref.bind_local(name, Sexp::Nil); return 0; }
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
    bind_formals_common(formals_ptr, args_list_ptr, env, true)
}

#[no_mangle]
pub unsafe extern "C" fn nl_bind_formals(
    formals_ptr: *const Sexp,
    args_list_ptr: *const Sexp,
    env: *mut std::ffi::c_void,
) -> i64 {
    bind_formals_common(formals_ptr, args_list_ptr, env, false)
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
