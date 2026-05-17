//! Rust-side minimal Elisp evaluator.  Public surface: [`eval_str`],
//! [`eval_str_all`], [`eval`], [`Env`], [`EvalError`].  Recursive
//! `eval(form, env)' with special forms dispatched via name match
//! in [`special_forms::apply_special`].  Errors are `Result<Sexp,
//! EvalError>` (never panic on user input).

pub mod builtins;
pub mod cons_primitives;
pub mod dlsym_bridge;
pub mod env;
pub mod env_lexframe;
pub mod env_lexframe_phase47_shims;
pub mod env_mirror;
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
pub mod rc_primitives;
pub mod sexp;
pub mod sexp_abi_assert;
pub mod special_forms;

pub use env::{Env, SymbolEntry};
pub use error::{is_error_subtype, EvalError};
pub use sexp::Sexp;

/// Read `input' via the elisp reader
/// `nelisp--read-all-from-string-impl'.  Production `eval_str' /
/// `eval_str_all' routes through this so the Rust reader is reached
/// only by bridge / baker / cargo-test paths.
pub(crate) fn read_all_via_elisp(input: &str, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
    let impl_fn = env
        .lookup_function("nelisp--read-all-from-string-impl")
        .map_err(|_| {
            EvalError::Internal(
                "eval_str: `nelisp--read-all-from-string-impl' not loaded \
                 — `lisp/nelisp-stdlib-reader.el' missing from STDLIB_IMAGES?"
                    .into(),
            )
        })?;
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

/// Read every top-level form via the elisp reader and return
/// `(LINE, FORM)' tuples (1-origin line numbers).  Used by
/// `bridge::loader::parse_tracked_forms' for source-line-aware
/// `BridgeError::EvalError' messages.
pub fn read_all_with_line_via_elisp(
    input: &str,
    env: &mut Env,
) -> Result<Vec<(u32, Sexp)>, EvalError> {
    let impl_fn = env
        .lookup_function("nelisp--read-all-with-line-from-string-impl")
        .map_err(|_| {
            EvalError::Internal(
                "read_all_with_line_via_elisp: \
                 `nelisp--read-all-with-line-from-string-impl' not loaded \
                 — `lisp/nelisp-stdlib-reader.el' missing from STDLIB_IMAGES?"
                    .into(),
            )
        })?;
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

/// Read exactly one form via the elisp reader; errors on empty or
/// multi-form input.  Test-fixture helper.
#[cfg(test)]
pub(crate) fn read_one_via_elisp(input: &str, env: &mut Env) -> Result<Sexp, EvalError> {
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

/// Read exactly one form from `input' (via elisp reader) and
/// evaluate it in a fresh global env.  Trailing tokens error.
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

/// Read every top-level form from `input' (via elisp reader) and
/// evaluate in sequence in a fresh global env, returning the last
/// value (= `progn').  Empty input returns nil.
pub fn eval_str_all(input: &str) -> Result<Sexp, EvalError> {
    let mut env = Env::new_global();
    let forms = read_all_via_elisp(input, &mut env)?;
    let mut last = Sexp::Nil;
    for f in &forms {
        last = eval(f, &mut env)?;
    }
    Ok(last)
}

/// Doc 47 Stage 8b — like [`eval_str_all`] but seeds the global env
/// with `default-directory' / `load-file-name' / `load-path' derived
/// from `src_path' so the source file can do `(load "sibling.el")` /
/// `(require 'feature)` and have the file I/O builtins resolve paths
/// relative to the file being evaluated.
///
/// `src_path' is taken as-given; callers should pass the same string
/// that was used to read the source so log messages are consistent.
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
    // Single-element `load-path' = the source file's directory.  Users
    // who want extra search roots can `(setq load-path (cons "..."
    // load-path))' inside the source.
    env.set_value("load-path", Sexp::cons(Sexp::Str(parent_dir), Sexp::Nil))?;
    let forms = read_all_via_elisp(input, &mut env)?;
    let mut last = Sexp::Nil;
    for f in &forms {
        last = eval(f, &mut env)?;
    }
    Ok(last)
}

/// Canonical recursive entry — atoms self-eval, symbols look up
/// frames then globals, cons dispatches via head (special form /
/// macro / function).
pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // Process-wide quit poll (= signal handler / C-g flip the flag).
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
        // Cell appears only in captured-env alists; self-eval to value.
        Sexp::Cell(c) => Ok(c.value.clone()),
        // Keyword symbols (`:foo') self-evaluate.
        Sexp::Symbol(name) if name.starts_with(':') && name.len() > 1 => {
            Ok(form.clone())
        }
        Sexp::Symbol(name) => env.lookup_value(name),
        Sexp::Cons(b) => apply_combiner(&b.car, &b.cdr, env),
    }
}

/// Apply a combiner head to its (un-evaluated) argument list.
fn apply_combiner(head: &Sexp, tail: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match head {
        // Symbol head — special form? macro? function in fcell?
        Sexp::Symbol(name) => {
            // 1) Special forms — argument evaluation order is form-specific.
            if let Some(result) = special_forms::apply_special(name, tail, env)? {
                return Ok(result);
            }
            // 2) Macro? — expand once, recurse on expansion.
            if let Ok(func) = env.lookup_function(name) {
                if is_macro(&func) {
                    // Post-bootstrap delegate to elisp
                    // `nelisp--expand-macro'; fall through to Rust
                    // during bootstrap or inside a delegation /
                    // helper call to avoid cycles.
                    if env.delegation_depth == 0
                        && !is_elisp_apply_helper(name)
                        && env.lookup_function("nelisp--expand-macro").is_ok()
                    {
                        return delegate_macro_to_elisp(&func, tail, env);
                    }
                    let expansion = expand_macro(&func, tail, env)?;
                    return eval(&expansion, env);
                }
                // 3) Plain function — evaluate args, apply.
                let args = eval_arg_list(tail, env)?;
                // Delegate to elisp `nelisp--apply-fn' at the
                // outermost user-level call; skip inside delegations
                // / dispatcher helpers / Rust builtins to avoid
                // cycles + no-op round-trips.
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
        // Lambda head: `((lambda ARGS BODY...) ARG ARG ...)` — evaluate
        // the head form in the current env (it is a literal lambda or
        // closure), then apply.
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

/// Walk a proper list of forms, evaluating each one; return the
/// resulting Vec for argument passing.
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

/// Walk a proper list and collect each element without evaluating —
/// used by special forms that take their argument list literally
/// (e.g. `quote`, `lambda` formal-parameter lists, `let` bindings).
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

/// Names of the elisp dispatcher's own support stack, exempted from
/// `delegate_to_elisp_apply' even
/// at the outermost call boundary.  Without this, a *user-level*
/// direct call to e.g. `(nelisp--apply-lambda-inner ...)' would be
/// dispatched through the elisp `nelisp--apply-fn' loop, which
/// recursively re-enters `nelisp--apply-lambda-inner' (= the
/// helper applies *itself* with its OWN formal-list as the user-
/// level formal-list).  The recursive bind-local then writes the
/// helper's `--nl-ali-*' formal NAMES into the topmost user-formals
/// frame, shadowing the helper's own internal state vars, and the
/// state-walk loop reads stale values.  Helper-name skip avoids
/// the entire recursive-dispatch chain; the helper runs straight
/// through Rust `apply_function' / `apply_lambda_inner', binding
/// only its own real call-frame slots.  Stage 7.4.b ERT exercises
/// each helper's behaviour in isolation, and the Stage 7.4.c
/// flag-on tests dispatch *user* defuns whose formal names do not
/// collide with `--nl-ali-*'.
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

/// `(builtin NAME)' shape detector — short-circuits elisp
/// delegation for Rust builtins (no-op round-trip).
fn is_builtin_value(func: &Sexp) -> bool {
    if let Sexp::Cons(b) = func {
        if let Sexp::Symbol(s) = &b.car {
            return s == "builtin";
        }
    }
    false
}

/// Delegate to elisp `(nelisp--apply-fn FUNC ARGS)' (Stage 7.4.c).
/// `delegation_depth' guard prevents infinite recursion when helpers
/// inside the dispatcher (= consp / null / cond) would themselves
/// re-delegate.
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

/// Delegate a macro expansion to elisp `nelisp--expand-macro', then
/// `eval' the result in the caller's env (= matches Rust
/// `expand_macro' lexical-scope semantics).  `delegation_depth'
/// guards helper recursion.
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

/// Apply `func` to `args`.  `func` may be:
///   - a built-in (`(builtin <NAME>)`-shaped sentinel — see
///     [`builtins::install_builtins`])
///   - a closure `(closure ENV ARGS BODY...)` — what `lambda`
///     evaluates to
///   - a raw `(lambda ARGS BODY...)` — what `defun` writes (closures
///     in Emacs sense, but we treat them as dynamically resolved
///     against the call-site env if the leading `closure` marker is
///     missing)
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
            // Shape: (closure CAPTURED-ENV ARGS BODY...)
            let parts = list_elements(func)?;
            if parts.len() < 3 {
                return Err(EvalError::Internal("closure missing env / args / body".into()));
            }
            apply_lambda_inner(&parts[1], &parts[2], &parts[3..], args, env)
        }
        "lambda" => {
            // Bare (lambda ARGS BODY...) — empty captured env.
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
    // (builtin <NAME>) — pull the name out, dispatch via the registry.
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

/// Body of `apply_function''s closure/lambda arms (= also reachable
/// via `bi_apply_lambda_inner').  Evaluates BODY inside the captured
/// env + a fresh frame for FORMALS.
pub(crate) fn apply_lambda_inner(
    captured: &Sexp,
    formals: &Sexp,
    body: &[Sexp],
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // Doc 102 Phase 4 — skip push_captured for `Sexp::Nil' (= a
    // top-level defun closure with no captured state).  The skip
    // breaks the apply_function recursion that Phase 4's elisp
    // dispatch otherwise introduces: push_captured (Rust) calls
    // apply_function(=nelisp-lexframe-stack-push-captured!=), whose
    // own closure has Nil captured env → without this skip, every
    // closure application re-enters push_captured on the empty env,
    // never bottoming out.  Semantically a no-op (= zero entries
    // pushed onto the mirror, same as before).
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

/// Bind a formal parameter list to actual arguments in the topmost
/// frame.  Supports `&optional` (default-nil) and `&rest` (collect
/// remainder as a list).
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
    // Reject excess args only if no &rest consumed them.
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

/// Macro detector — `(macro lambda ARGS BODY...)` is the canonical
/// shape `defmacro` writes.  GNU Emacs also accepts `(macro . FUN)`
/// where `FUN` is callable; we support both.
fn is_macro(func: &Sexp) -> bool {
    matches!(
        func,
        Sexp::Cons(b) if matches!(&b.car, Sexp::Symbol(s) if s == "macro")
    )
}

fn expand_macro(macro_form: &Sexp, args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // `macro_form` is `(macro lambda ARGS BODY...)` — strip the
    // `macro` head, get the underlying lambda, and apply it to the
    // argument forms *un-evaluated* (= macro semantics).
    let parts = list_elements(macro_form)?;
    if parts.len() < 2 {
        return Err(EvalError::Internal("malformed macro".into()));
    }
    let inner = &parts[1];
    let arg_forms = list_elements(args)?;
    apply_function(inner, &arg_forms, env)
}

#[cfg(test)]
mod tests;
