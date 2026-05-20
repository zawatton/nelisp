//! Minimal Elisp evaluator.
//! Public surface: [`eval_str`], [`eval_str_all`], [`eval`], [`Env`], [`EvalError`].
//! Special forms dispatch through [`special_forms::apply_special`].
//! User input failures return `Result<Sexp, EvalError>`.

pub mod builtins;
pub mod env_helpers;
#[cfg(unix)]
pub mod tty;
pub mod env_shim;
pub mod error;
pub mod nlboolvector;
pub mod nlcell;
pub mod nlchartable;
pub mod nlconsbox;
pub mod nlrc;
pub mod nlrecord;
pub mod nlstr;
pub mod nlvector;
pub mod alloc_mem;
pub mod quit;
pub mod sexp;
pub mod special_forms;

pub use env_helpers::{Env, ExternBuiltin, FrameCell};
pub use error::{is_error_subtype, EvalError};
pub use sexp::Sexp;

fn expect_single_form(forms: Vec<Sexp>, ctx: &str) -> Result<Sexp, EvalError> {
    match forms.as_slice() {
        [single] => Ok(single.clone()),
        [] => Err(EvalError::internal(format!(
            "{ctx}: empty input - at least one form required"
        ))),
        _ => Err(EvalError::internal(format!(
            "{ctx}: expected exactly one form, got {}",
            forms.len()
        ))),
    }
}

fn eval_forms(forms: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let mut last = Sexp::Nil;
    for form in forms {
        last = eval(form, env)?;
    }
    Ok(last)
}

pub(crate) fn read_all_via_elisp(input: &str, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    let impl_fn = env
        .lookup_function("nelisp--read-all-from-string-impl")
        .map_err(|_| EvalError::internal("nelisp--read-all-from-string-impl not loaded"))?;
    list_elements(&apply_function(&impl_fn, &[Sexp::Str(input.to_string())], env)?)
}

pub fn read_all_with_line_via_elisp(
    input: &str,
    env: &mut Env,
) -> Result<Vec<(u32, Sexp)>, EvalError> {
    let impl_fn = env
        .lookup_function("nelisp--read-all-with-line-from-string-impl")
        .map_err(|_| {
            EvalError::internal("nelisp--read-all-with-line-from-string-impl not loaded")
        })?;
    let pairs = list_elements(&apply_function(&impl_fn, &[Sexp::Str(input.to_string())], env)?)?;
    pairs.into_iter().map(|pair| match pair {
        Sexp::Cons(inner) => match inner.car.clone() {
            Sexp::Int(n) if n >= 0 => Ok((n as u32, inner.cdr.clone())),
            other => Err(EvalError::internal(format!(
                "read_all_with_line_via_elisp: expected (LINE . FORM) with non-negative LINE, got {:?}",
                other
            ))),
        },
        other => Err(EvalError::internal(format!(
            "read_all_with_line_via_elisp: expected (LINE . FORM), got {:?}",
            other
        ))),
    }).collect()
}

pub fn read_one_via_elisp(input: &str, env: &mut Env) -> Result<Sexp, EvalError> {
    expect_single_form(read_all_via_elisp(input, env)?, "read_one_via_elisp")
}

pub fn eval_str(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let form = expect_single_form(read_all_via_elisp(input, &mut env)?, "eval_str")?;
    eval(&form, &mut env)
}

pub fn eval_str_all(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let forms = read_all_via_elisp(input, &mut env)?;
    eval_forms(&forms, &mut env)
}

pub fn eval_str_all_at_path(input: &str, src_path: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let path_buf = std::path::PathBuf::from(src_path);
    let parent_dir = path_buf
        .parent()
        .map(|p| {
            let mut s = p.to_string_lossy().into_owned();
            if s.is_empty() {
                s.push('.');
            }
            if !s.ends_with('/') {
                s.push('/');
            }
            s
        })
        .unwrap_or_else(|| "./".into());
    env.set_value("default-directory", Sexp::Str(parent_dir.clone()))?;
    env.set_value("load-file-name", Sexp::Str(src_path.to_string()))?;
    // Seed `load-path` with the source file's directory.
    env.set_value("load-path", Sexp::cons(Sexp::Str(parent_dir), Sexp::Nil))?;
    let forms = read_all_via_elisp(input, &mut env)?;
    eval_forms(&forms, &mut env)
}

/// Recursive evaluator entry point.
pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // Process-wide quit poll.
    if quit::take_quit_flag() {
        return Err(EvalError::Quit);
    }
    if env.current_recursion >= env.max_recursion {
        return Err(EvalError::internal(format!(
            "max-lisp-eval-depth exceeded ({})",
            env.max_recursion
        )));
    }
    env.current_recursion += 1;
    let mut out = Sexp::Nil;
    let rc = unsafe {
        crate::elisp_cc_spike::eval_inner_call(
            form as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            0,
        )
    };
    env.current_recursion -= 1;
    if rc == 0 { Ok(out) } else { Err(consume_stashed_error(env, "eval_inner")) }
}

fn walk_proper_list(
    head: &Sexp,
    mut yield_elem: impl FnMut(&Sexp) -> Result<Sexp, EvalError>,
) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur: Sexp = head.clone();
    loop {
        let next = match &cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(b) => {
                out.push(yield_elem(&b.car)?);
                b.cdr.clone()
            }
            _ => return Err(EvalError::wrong_type("list", cur.clone())),
        };
        cur = next;
    }
}

/// Evaluate each element of a proper list and collect the results.
pub(crate) fn eval_arg_list(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    walk_proper_list(args, |car| eval(car, env))
}

/// Collect each element of a proper list without evaluating it.
pub(crate) fn list_elements(list: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    walk_proper_list(list, |car| Ok(car.clone()))
}

pub fn apply_function(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let wrong_type = || EvalError::wrong_type("function", func.clone());
    let Sexp::Cons(b) = func else {
        return Err(wrong_type());
    };
    let Sexp::Symbol(head) = &b.car else {
        return Err(wrong_type());
    };
    match head.as_str() {
        "builtin" => {
            // Dispatch builtin directly via name sentinel: (builtin . (NAME . nil)).
            let Sexp::Cons(inner) = &b.cdr else {
                return Err(EvalError::internal("builtin sentinel missing name"));
            };
            let name = match &inner.car {
                Sexp::Symbol(s) | Sexp::Str(s) => s.clone(),
                _ => return Err(EvalError::internal("builtin sentinel name not a symbol")),
            };
            builtins::dispatch(&name, args, env)
        }
        // closure: (closure CAPTURED FORMALS BODY...)
        // lambda:  (lambda FORMALS BODY...)
        // Phase 47: both delegate to nl_apply_lambda_inner elisp .o.
        head @ ("closure" | "lambda") => {
            let parts = list_elements(func)?;
            let (captured, formals_idx, body_start) = if head == "closure" {
                if parts.len() < 3 {
                    return Err(EvalError::internal("closure missing env / args / body"));
                }
                (parts[1].clone(), 2usize, 3usize)
            } else {
                if parts.len() < 2 {
                    return Err(EvalError::internal("lambda missing args / body"));
                }
                (Sexp::Nil, 1, 2)
            };
            let formals   = &parts[formals_idx];
            let body_list = Sexp::list_from(&parts[body_start..]);
            let args_list = Sexp::list_from(args);
            let mut out   = Sexp::Nil;
            let rc = unsafe {
                crate::elisp_cc_spike::apply_lambda_inner_call(
                    &captured as *const Sexp,
                    formals as *const Sexp,
                    &body_list as *const Sexp,
                    &args_list as *const Sexp,
                    env as *mut Env as *mut std::ffi::c_void,
                    &mut out as *mut Sexp,
                )
            };
            if rc == 0 { Ok(out) } else { Err(consume_stashed_error(env, "apply_lambda")) }
        }
        "macro" => Err(EvalError::wrong_type("function (not macro)", func.clone())),
        _ => Err(wrong_type()),
    }
}

// apply_lambda_inner Rust thin-shell deleted — Phase 47 elisp .o
// (nl_apply_lambda_inner in nelisp-cc-apply-lambda-inner.el) is called
// directly via apply_lambda_inner_call at each call site in apply_function.

/// Phase 47 elisp .o から Rust eval() を再帰呼出するための ABI primitive。
/// elisp 側は `(extern-call nelisp_eval_call FORM ENV OUT)` で利用。
/// 戻り値: 0=Ok / 1=Err。エラー詳細は別 channel (後続 wave で実装)。
///
/// # Safety
/// - form: live `*const Sexp`
/// - env: live `&mut Env` を `*mut c_void` に reinterpret したもの
/// - out: 32-byte writable Sexp slot
#[no_mangle]
pub unsafe extern "C" fn nelisp_eval_call(
    form: *const Sexp,
    env: *mut std::ffi::c_void,
    out: *mut Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    match eval(&*form, env_ref) {
        Ok(v) => { std::ptr::write(out, v); 0 }
        Err(e) => {
            // Stash signal data so Phase 47 .o rc=1 callers can recover variant.
            let _ = env_ref.set_value("nelisp--last-signal-data", e.signal_data());
            1
        }
    }
}

/// Sibling of `nelisp_eval_call' that ALSO writes the `signal_data()` sexp
/// directly into `err_out` on rc=1.  Used by Phase 47 elisp .o that need
/// to INTERCEPT (not propagate) errors — e.g., `nl_sf_condition_case_call`.
#[no_mangle]
pub unsafe extern "C" fn nelisp_eval_call_with_err(
    form: *const Sexp,
    env: *mut std::ffi::c_void,
    out: *mut Sexp,
    err_out: *mut Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    match eval(&*form, env_ref) {
        Ok(v) => { std::ptr::write(out, v); 0 }
        Err(e) => { std::ptr::write(err_out, e.signal_data()); 1 }
    }
}

/// Wave g — apply_function as C-callable ABI primitive.
/// Phase 47 elisp .o が関数を apply するための外部エントリ。
/// args_list は cons リスト形式 (= Sexp::list_from で構築)。
/// 戻り値: 0=Ok (*out に書込み済), 1=Err (signal は nelisp--last-signal-data にスタッシュ)。
///
/// # Safety
/// - func: live `*const Sexp` (closure / lambda / builtin sentinel)
/// - args_list: live `*const Sexp` (proper cons list of arguments)
/// - env: live `&mut Env` を `*mut c_void` に reinterpret したもの
/// - out: 32-byte writable Sexp slot
#[no_mangle]
pub unsafe extern "C" fn nelisp_apply_function(
    func: *const Sexp,
    args_list: *const Sexp,
    env: *mut std::ffi::c_void,
    out: *mut Sexp,
) -> i64 {
    let env_ref = &mut *(env as *mut Env);
    let args = list_elements(&*args_list).unwrap_or_default();
    match apply_function(&*func, &args, env_ref) {
        Ok(v) => { std::ptr::write(out, v); 0 }
        Err(e) => { let _ = env_ref.set_value("nelisp--last-signal-data", e.signal_data()); 1 }
    }
}

/// Re-construct EvalError from a `(tag . data)' sexp produced by `signal_data()'.
pub(crate) fn sexp_to_eval_error(sexp: &Sexp, fb: &str) -> EvalError {
    match sexp {
        Sexp::Cons(b) => match &b.car {
            Sexp::Symbol(s) if s == "quit" => EvalError::Quit,
            Sexp::Symbol(tag) => EvalError::Generic(tag.clone(), b.cdr.clone()),
            _ => EvalError::internal(fb),
        },
        _ => EvalError::internal(fb),
    }
}

/// Read stashed signal from `nelisp--last-signal-data' and reconstruct EvalError.
/// Used by Phase 47 special-form dispatch macros on rc=1.
pub(crate) fn consume_stashed_error(env: &mut Env, fallback_name: &str) -> EvalError {
    match env.lookup_value("nelisp--last-signal-data") {
        Ok(sexp) => sexp_to_eval_error(&sexp, fallback_name),
        Err(_) => EvalError::internal(fallback_name),
    }
}
