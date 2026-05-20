pub mod builtins; pub mod env_helpers; #[cfg(unix)] pub mod tty; pub mod env_shim; pub mod error;
pub mod nlboolvector; pub mod nlcell; pub mod nlchartable; pub mod nlconsbox; pub mod nlrc;
pub mod nlrecord; pub mod nlstr; pub mod nlvector; pub mod alloc_mem; pub mod quit; pub mod sexp; pub mod special_forms;
pub use env_helpers::{Env, ExternBuiltin, FrameCell};
pub use error::{is_error_subtype, EvalError};
pub use sexp::Sexp;
fn expect_single_form(forms: Vec<Sexp>, ctx: &str) -> Result<Sexp, EvalError> {
    match forms.as_slice() {
        [s] => Ok(s.clone()),
        [] => Err(EvalError::internal(format!("{ctx}: empty input - at least one form required"))),
        _ => Err(EvalError::internal(format!("{ctx}: expected exactly one form, got {}", forms.len()))),
    }
}
fn eval_forms(forms: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> { forms.iter().try_fold(Sexp::Nil, |_, f| eval(f, env)) }
pub(crate) fn read_all_via_elisp(input: &str, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    let impl_fn = env.lookup_function("nelisp--read-all-from-string-impl")
        .map_err(|_| EvalError::internal("nelisp--read-all-from-string-impl not loaded"))?;
    list_elements(&apply_function(&impl_fn, &[Sexp::Str(input.to_string())], env)?)
}
pub fn read_one_via_elisp(input: &str, env: &mut Env) -> Result<Sexp, EvalError> { expect_single_form(read_all_via_elisp(input, env)?, "read_one_via_elisp") }
pub fn eval_str(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let form = expect_single_form(read_all_via_elisp(input, &mut env)?, "eval_str")?;
    eval(&form, &mut env)
}
pub fn eval_str_all(input: &str) -> Result<Sexp, EvalError> { let mut env = Env::new_global(); eval_forms(&read_all_via_elisp(input, &mut env)?, &mut env) }
pub fn eval_str_all_at_path(input: &str, src_path: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let rd = std::path::PathBuf::from(src_path).parent().map(|p| p.to_string_lossy().into_owned()).unwrap_or_else(|| ".".into());
    let dir = if rd.is_empty() { ".".into() } else if rd.ends_with('/') { rd } else { rd + "/" };
    env.set_value("default-directory", Sexp::Str(dir.clone()))?; env.set_value("load-file-name", Sexp::Str(src_path.to_string()))?; env.set_value("load-path", Sexp::cons(Sexp::Str(dir), Sexp::Nil))?;
    eval_forms(&read_all_via_elisp(input, &mut env)?, &mut env)
}
pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    if quit::take_quit_flag() { return Err(EvalError::Quit); }
    if env.current_recursion >= env.max_recursion { return Err(EvalError::internal(format!("max-lisp-eval-depth exceeded ({})", env.max_recursion))); }
    env.current_recursion += 1;
    let mut out = Sexp::Nil;
    let rc = unsafe { crate::elisp_cc_spike::eval_inner_call(form as *const Sexp, env as *mut Env as *mut std::ffi::c_void, &mut out as *mut Sexp, 0) };
    env.current_recursion -= 1;
    if rc == 0 { Ok(out) } else { Err(consume_stashed_error(env, "eval_inner")) }
}
fn walk_proper_list(head: &Sexp, mut f: impl FnMut(&Sexp) -> Result<Sexp, EvalError>) -> Result<Vec<Sexp>, EvalError> {
    let (mut out, mut cur) = (Vec::new(), head.clone());
    loop { match cur.clone() {
        Sexp::Nil => return Ok(out), Sexp::Cons(b) => { out.push(f(&b.car)?); cur = b.cdr.clone(); }
        o => return Err(EvalError::wrong_type("list", o)),
    }}
}
pub(crate) fn eval_arg_list(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> { walk_proper_list(args, |car| eval(car, env)) }
pub(crate) fn list_elements(list: &Sexp) -> Result<Vec<Sexp>, EvalError> { walk_proper_list(list, |car| Ok(car.clone())) }
pub fn apply_function(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let wt = || EvalError::wrong_type("function", func.clone());
    let Sexp::Cons(b) = func else { return Err(wt()); };
    let Sexp::Symbol(head) = &b.car else { return Err(wt()); };
    match head.as_str() {
        "builtin" => {
            let Sexp::Cons(inner) = &b.cdr else { return Err(EvalError::internal("builtin sentinel missing name")); };
            let name = match &inner.car { Sexp::Symbol(s) | Sexp::Str(s) => s.clone(), _ => return Err(EvalError::internal("builtin sentinel name not a symbol")) };
            builtins::dispatch(&name, args, env)
        }
        head @ ("closure" | "lambda") => {
            let parts = list_elements(func)?;
            let (captured, fi, bs) = if head == "closure" {
                if parts.len() < 3 { return Err(EvalError::internal("closure missing env / args / body")); }
                (parts[1].clone(), 2usize, 3usize)
            } else { if parts.len() < 2 { return Err(EvalError::internal("lambda missing args / body")); } (Sexp::Nil, 1, 2) };
            let mut out = Sexp::Nil;
            let rc = unsafe { crate::elisp_cc_spike::apply_lambda_inner_call(&captured, &parts[fi], &Sexp::list_from(&parts[bs..]), &Sexp::list_from(args), env as *mut Env as *mut std::ffi::c_void, &mut out) };
            if rc == 0 { Ok(out) } else { Err(consume_stashed_error(env, "apply_lambda")) }
        }
        "macro" => Err(EvalError::wrong_type("function (not macro)", func.clone())),
        _ => Err(wt()),
    }
}
#[inline]
unsafe fn eval_stash_err(env: &mut Env, result: Result<Sexp, EvalError>, out: *mut Sexp) -> i64 {
    match result { Ok(v) => { std::ptr::write(out, v); 0 } Err(e) => { let _ = env.set_value("nelisp--last-signal-data", e.signal_data()); 1 } }
}
#[no_mangle] pub unsafe extern "C" fn nelisp_eval_call(form: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp) -> i64 {
    let r = &mut *(env as *mut Env); let res = eval(&*form, r); eval_stash_err(r, res, out)
}
#[no_mangle] pub unsafe extern "C" fn nelisp_eval_call_with_err(form: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, err_out: *mut Sexp) -> i64 {
    let r = &mut *(env as *mut Env); match eval(&*form, r) { Ok(v) => { std::ptr::write(out, v); 0 } Err(e) => { std::ptr::write(err_out, e.signal_data()); 1 } }
}
#[no_mangle] pub unsafe extern "C" fn nelisp_apply_function(func: *const Sexp, args_list: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp) -> i64 {
    let r = &mut *(env as *mut Env); let res = apply_function(&*func, &list_elements(&*args_list).unwrap_or_default(), r); eval_stash_err(r, res, out)
}
pub(crate) fn consume_stashed_error(env: &mut Env, fallback: &str) -> EvalError {
    let Ok(Sexp::Cons(b)) = env.lookup_value("nelisp--last-signal-data") else { return EvalError::internal(fallback); };
    match &b.car { Sexp::Symbol(s) if s == "quit" => EvalError::Quit, Sexp::Symbol(t) => EvalError::Generic(t.clone(), b.cdr.clone()), _ => EvalError::internal(fallback) }
}
