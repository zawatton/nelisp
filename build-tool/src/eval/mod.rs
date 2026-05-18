//! Minimal Elisp evaluator.
//! Public surface: [`eval_str`], [`eval_str_all`], [`eval`], [`Env`], [`EvalError`].
//! Special forms dispatch through [`special_forms::apply_special`].
//! User input failures return `Result<Sexp, EvalError>`.

pub mod builtins;
pub mod cons_primitives;
pub mod env_helpers;
pub mod env_shim;
pub mod error;
pub mod ffi;
pub mod nlboolvector;
pub mod nlcell;
pub mod nlchartable;
pub mod nlconsbox;
pub mod nlrc;
pub mod nlrecord;
pub mod nlstr;
pub mod nlvector;
pub mod quit;
pub mod raw_mem;
pub mod rc_primitives;
pub mod sexp;
pub mod sexp_abi_assert;
pub mod special_forms;

pub use env_helpers::{Env, ExternBuiltin, FrameCell, SymbolEntry};
pub use error::{is_error_subtype, EvalError};
pub use sexp::Sexp;

/// Read `input` via the Elisp reader — entry point for `eval_str` / `eval_str_all`.
pub(crate) fn read_all_via_elisp(input: &str, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    let impl_fn = env
        .lookup_function("nelisp--read-all-from-string-impl")
        .map_err(|_| EvalError::Internal(
            "nelisp--read-all-from-string-impl not loaded".into(),
        ))?;
    let arg = Sexp::Str(input.to_string());
    let mut list = apply_function(&impl_fn, &[arg], env)?;
    let mut out = Vec::new();
    loop {
        match list {
            Sexp::Nil => break,
            Sexp::Cons(b) => {
                out.push(b.car.clone());
                list = b.cdr.clone();
            }
            other => {
                return Err(EvalError::Internal(format!(
                    "eval_str: expected proper list from elisp reader, got {:?}",
                    other
                )));
            }
        }
    }
    Ok(out)
}

/// Read top-level forms returning `(LINE, FORM)` pairs (1-origin lines).
pub fn read_all_with_line_via_elisp(
    input: &str,
    env: &mut Env,
) -> Result<Vec<(u32, Sexp)>, EvalError> {
    let impl_fn = env
        .lookup_function("nelisp--read-all-with-line-from-string-impl")
        .map_err(|_| EvalError::Internal(
            "nelisp--read-all-with-line-from-string-impl not loaded".into(),
        ))?;
    let arg = Sexp::Str(input.to_string());
    let mut list = apply_function(&impl_fn, &[arg], env)?;
    let mut out = Vec::new();
    loop {
        match list {
            Sexp::Nil => break,
            Sexp::Cons(b) => {
                let pair = b.car.clone();
                match pair {
                    Sexp::Cons(inner) => {
                        let line = match inner.car.clone() {
                            Sexp::Int(n) if n >= 0 => n as u32,
                            other => {
                                return Err(EvalError::Internal(format!(
                                    "read_all_with_line_via_elisp: \
                                     expected (LINE . FORM) where LINE is a \
                                     positive integer, got line = {:?}",
                                    other
                                )));
                            }
                        };
                        let form = inner.cdr.clone();
                        out.push((line, form));
                    }
                    other => {
                        return Err(EvalError::Internal(format!(
                            "read_all_with_line_via_elisp: \
                             expected (LINE . FORM) cons element, got {:?}",
                            other
                        )));
                    }
                }
                list = b.cdr.clone();
            }
            other => {
                return Err(EvalError::Internal(format!(
                    "read_all_with_line_via_elisp: \
                     expected proper list from elisp reader, got {:?}",
                    other
                )));
            }
        }
    }
    Ok(out)
}

/// Read exactly one form via the Elisp reader — integration test helper.
pub fn read_one_via_elisp(input: &str, env: &mut Env) -> Result<Sexp, EvalError> {
    let forms = read_all_via_elisp(input, env)?;
    match forms.as_slice() {
        [single] => Ok(single.clone()),
        [] => Err(EvalError::Internal(
            "read_one_via_elisp: empty input — at least one form required".into(),
        )),
        _ => Err(EvalError::Internal(format!(
            "read_one_via_elisp: expected exactly one form, got {}",
            forms.len()
        ))),
    }
}

/// Read exactly one form from `input` and evaluate it in a fresh global env.
pub fn eval_str(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let forms = read_all_via_elisp(input, &mut env)?;
    let form = match forms.as_slice() {
        [single] => single.clone(),
        [] => {
            return Err(EvalError::Internal(
                "eval_str: empty input — at least one form required".into(),
            ));
        }
        _ => {
            return Err(EvalError::Internal(format!(
                "eval_str: expected exactly one form, got {}",
                forms.len()
            )));
        }
    };
    eval(&form, &mut env)
}

/// Read and evaluate all top-level forms from `input` in a fresh global env.
/// Returns the last value; empty input returns `nil`.
pub fn eval_str_all(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let forms = read_all_via_elisp(input, &mut env)?;
    let mut last = Sexp::Nil;
    for f in &forms {
        last = eval(f, &mut env)?;
    }
    Ok(last)
}

/// Like [`eval_str_all`], but seeds path-related globals from `src_path`.
/// Relative `load` and `require` calls resolve against the source file's directory.
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
    let mut last = Sexp::Nil;
    for f in &forms {
        last = eval(f, &mut env)?;
    }
    Ok(last)
}

/// Recursive evaluator entry point.
pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // Process-wide quit poll.
    if quit::take_quit_flag() {
        return Err(EvalError::Quit);
    }
    if env.current_recursion >= env.max_recursion {
        return Err(EvalError::Internal(format!(
            "max-lisp-eval-depth exceeded ({})",
            env.max_recursion
        )));
    }
    env.current_recursion += 1;
    let result = eval_inner(form, env);
    env.current_recursion -= 1;
    result
}

fn eval_inner(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match form {
        Sexp::Nil | Sexp::T | Sexp::Int(_) | Sexp::Float(_) | Sexp::Str(_)
            | Sexp::MutStr(_) | Sexp::Vector(_)
            | Sexp::CharTable(_) | Sexp::BoolVector(_)
            | Sexp::Record(_) => Ok(form.clone()),
        // Cells appear only in captured-env alists; self-evaluate to the stored value.
        Sexp::Cell(c) => Ok(c.value.clone()),
        // Keyword symbols self-evaluate.
        Sexp::Symbol(name) if name.starts_with(':') && name.len() > 1 => {
            Ok(form.clone())
        }
        Sexp::Symbol(name) => env.lookup_value(name),
        Sexp::Cons(b) => apply_combiner(&b.car, &b.cdr, env),
    }
}

/// Apply a combiner head to its unevaluated argument list.
fn apply_combiner(head: &Sexp, tail: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match head {
        // Symbol heads may name a special form, macro, or function.
        Sexp::Symbol(name) => {
            if let Some(result) = special_forms::apply_special(name, tail, env)? {
                return Ok(result);
            }
            if let Ok(func) = env.lookup_function(name) {
                if is_macro(&func) {
                    // Delegate macro expansion unless bootstrap or helper recursion would cycle.
                    if env.delegation_depth == 0
                        && !is_elisp_apply_helper(name)
                        && env.lookup_function("nelisp--expand-macro").is_ok()
                    {
                        return delegate_macro_to_elisp(&func, tail, env);
                    }
                    let expansion = expand_macro(&func, tail, env)?;
                    return eval(&expansion, env);
                }
                let args = eval_arg_list(tail, env)?;
                // Delegate only at the outermost user-level call.
                if env.use_elisp_apply
                    && env.delegation_depth == 0
                    && !is_elisp_apply_helper(name)
                    && !is_builtin_value(&func)
                {
                    return delegate_to_elisp_apply(&func, &args, env);
                }
                return apply_function(&func, &args, env);
            }
            Err(EvalError::UnboundFunction(name.clone()))
        }
        // A lambda head is evaluated first, then applied.
        Sexp::Cons(_) => {
            let func = eval(head, env)?;
            let args = eval_arg_list(tail, env)?;
            if env.use_elisp_apply
                && env.delegation_depth == 0
                && !is_builtin_value(&func)
            {
                return delegate_to_elisp_apply(&func, &args, env);
            }
            apply_function(&func, &args, env)
        }
        other => Err(EvalError::WrongType {
            expected: "function".into(),
            got: other.clone(),
        }),
    }
}

/// Evaluate each element of a proper list and collect the results.
pub(crate) fn eval_arg_list(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur: Sexp = args.clone();
    loop {
        let next = match &cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(b) => {
                out.push(eval(&b.car, env)?);
                b.cdr.clone()
            }
            _ => {
                return Err(EvalError::WrongType {
                    expected: "list".into(),
                    got: cur.clone(),
                })
            }
        };
        cur = next;
    }
}

/// Collect each element of a proper list without evaluating it.
pub(crate) fn list_elements(list: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur: Sexp = list.clone();
    loop {
        let next = match &cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(b) => {
                out.push(b.car.clone());
                b.cdr.clone()
            }
            _ => {
                return Err(EvalError::WrongType {
                    expected: "list".into(),
                    got: cur.clone(),
                })
            }
        };
        cur = next;
    }
}

/// Elisp dispatcher helpers that must not re-enter `delegate_to_elisp_apply`.
fn is_elisp_apply_helper(name: &str) -> bool {
    matches!(
        name,
        "nelisp--apply-fn"
            | "nelisp--apply-closure"
            | "nelisp--apply-lambda"
            | "nelisp--bind-formals--compute"
            | "nelisp--bind-formals--required-count"
            | "nelisp--builtinp"
            | "nelisp--closurep"
            | "nelisp--lambdap"
            | "nelisp--macrop"
            | "nelisp--expand-macro"
    )
}

/// Detect `(builtin NAME)` sentinels to skip Elisp delegation.
fn is_builtin_value(func: &Sexp) -> bool {
    if let Sexp::Cons(b) = func {
        if let Sexp::Symbol(s) = &b.car {
            return s == "builtin";
        }
    }
    false
}

/// Delegate to Elisp `(nelisp--apply-fn FUNC ARGS)`.
fn delegate_to_elisp_apply(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let args_list = Sexp::list_from(args);
    let dispatch_form = Sexp::list_from(&[
        Sexp::Symbol("nelisp--apply-fn".into()),
        Sexp::list_from(&[Sexp::Symbol("quote".into()), func.clone()]),
        Sexp::list_from(&[Sexp::Symbol("quote".into()), args_list]),
    ]);
    env.delegation_depth += 1;
    let result = eval(&dispatch_form, env);
    env.delegation_depth -= 1;
    result
}

/// Delegate macro expansion to Elisp, then evaluate the result in the caller's env.
fn delegate_macro_to_elisp(
    macro_form: &Sexp,
    arg_forms: &Sexp,
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    let dispatch_form = Sexp::list_from(&[
        Sexp::Symbol("nelisp--expand-macro".into()),
        Sexp::list_from(&[Sexp::Symbol("quote".into()), macro_form.clone()]),
        Sexp::list_from(&[Sexp::Symbol("quote".into()), arg_forms.clone()]),
    ]);
    env.delegation_depth += 1;
    let expansion = eval(&dispatch_form, env);
    env.delegation_depth -= 1;
    let expansion = expansion?;
    eval(&expansion, env)
}

/// Apply `func` to `args`.
/// Accepted shapes are `(builtin NAME)`, `(closure ENV ARGS BODY...)`,
/// and `(lambda ARGS BODY...)`.
pub fn apply_function(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    let wrong_type = || EvalError::WrongType {
        expected: "function".into(),
        got: func.clone(),
    };
    let Sexp::Cons(b) = func else { return Err(wrong_type()); };
    let Sexp::Symbol(head) = &b.car else { return Err(wrong_type()); };
    match head.as_str() {
        "builtin" => apply_builtin(func, args, env),
        "closure" => {
            let parts = list_elements(func)?;
            if parts.len() < 3 {
                return Err(EvalError::Internal("closure missing env / args / body".into()));
            }
            apply_lambda_inner(&parts[1], &parts[2], &parts[3..], args, env)
        }
        "lambda" => {
            let parts = list_elements(func)?;
            if parts.len() < 2 {
                return Err(EvalError::Internal("lambda missing args / body".into()));
            }
            apply_lambda_inner(&Sexp::Nil, &parts[1], &parts[2..], args, env)
        }
        "macro" => Err(EvalError::WrongType {
            expected: "function (not macro)".into(),
            got: func.clone(),
        }),
        _ => Err(wrong_type()),
    }
}

fn apply_builtin(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    // `(builtin NAME)` dispatches through the builtin registry.
    let Sexp::Cons(outer) = func else {
        return Err(EvalError::Internal("builtin sentinel not a cons".into()));
    };
    let Sexp::Cons(inner) = &outer.cdr else {
        return Err(EvalError::Internal("builtin sentinel missing name".into()));
    };
    let name = match &inner.car {
        Sexp::Symbol(s) | Sexp::Str(s) => s.clone(),
        _ => return Err(EvalError::Internal("builtin sentinel name not a symbol".into())),
    };
    builtins::dispatch(&name, args, env)
}

/// Shared closure/lambda application path.
pub(crate) fn apply_lambda_inner(
    captured: &Sexp,
    formals: &Sexp,
    body: &[Sexp],
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // Skip empty captured environments to avoid recursive no-op frame pushes.
    let captured_pushed = !matches!(captured, Sexp::Nil);
    if captured_pushed {
        env.push_captured(captured)?;
    }
    env.push_frame();
    let result = (|| {
        bind_formals(formals, args, env)?;
        let mut last = Sexp::Nil;
        for form in body {
            last = eval(form, env)?;
        }
        Ok(last)
    })();
    env.pop_frame();
    if captured_pushed {
        env.pop_frame();
    }
    result
}

/// Bind formal parameters in the topmost frame.
/// Supports `&optional` and `&rest`.
pub(crate) fn bind_formals(
    formals: &Sexp,
    args: &[Sexp],
    env: &mut Env,
) -> Result<(), EvalError> {
    let names = list_elements(formals)?;
    enum Mode {
        Required,
        Optional,
        Rest,
    }
    let mut mode = Mode::Required;
    let mut idx = 0usize;
    let mut required_count = 0usize;
    let mut consumed_rest = false;
    let mut saw_rest = false;

    // First pass: count required arity for diagnostics.
    for n in &names {
        if let Sexp::Symbol(s) = n {
            if s == "&optional" || s == "&rest" {
                break;
            }
        }
        required_count += 1;
    }

    for n in names {
        match n {
            Sexp::Symbol(s) if s == "&optional" => {
                if matches!(mode, Mode::Rest) {
                    return Err(EvalError::WrongType {
                        expected: "formal parameter after &rest".into(),
                        got: Sexp::Symbol(s),
                    });
                }
                mode = Mode::Optional;
            }
            Sexp::Symbol(s) if s == "&rest" => {
                if saw_rest {
                    return Err(EvalError::WrongType {
                        expected: "single &rest marker".into(),
                        got: Sexp::Symbol(s),
                    });
                }
                mode = Mode::Rest;
                saw_rest = true;
            }
            Sexp::Symbol(name) => match mode {
                Mode::Required => {
                    if idx >= args.len() {
                        return Err(EvalError::WrongNumberOfArguments {
                            function: "lambda".into(),
                            expected: format!("{}", required_count),
                            got: args.len(),
                        });
                    }
                    env.bind_local(&name, args[idx].clone());
                    idx += 1;
                }
                Mode::Optional => {
                    let v = if idx < args.len() {
                        let value = args[idx].clone();
                        idx += 1;
                        value
                    } else {
                        Sexp::Nil
                    };
                    env.bind_local(&name, v);
                }
                Mode::Rest => {
                    if consumed_rest {
                        return Err(EvalError::WrongType {
                            expected: "single symbol after &rest".into(),
                            got: Sexp::Symbol(name),
                        });
                    }
                    let rest = Sexp::list_from(&args[idx..]);
                    env.bind_local(&name, rest);
                    idx = args.len();
                    consumed_rest = true;
                }
            },
            other => {
                return Err(EvalError::WrongType {
                    expected: "symbol".into(),
                    got: other,
                })
            }
        }
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

/// Detect `(macro ...)` forms.
fn is_macro(func: &Sexp) -> bool {
    matches!(
        func,
        Sexp::Cons(b) if matches!(&b.car, Sexp::Symbol(s) if s == "macro")
    )
}

fn expand_macro(macro_form: &Sexp, args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // Apply the underlying lambda to the unevaluated argument forms.
    let parts = list_elements(macro_form)?;
    if parts.len() < 2 {
        return Err(EvalError::Internal("malformed macro".into()));
    }
    let inner = &parts[1];
    let arg_forms = list_elements(args)?;
    apply_function(inner, &arg_forms, env)
}

