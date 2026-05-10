//! Phase 8.0.2 — Rust-side minimal Elisp evaluator (Doc 44 §3.3 LOCKED).
//!
//! Public surface (per prompt):
//!   - [`eval_str`]      — read one form, evaluate, return the value
//!   - [`eval_str_all`]  — read every form, evaluate sequentially,
//!                         return the *last* value (`progn` semantics)
//!   - [`eval`]          — evaluate a pre-parsed [`Sexp`] in `env`
//!   - [`Env`]           — globals + lexical-frame stack;
//!                         [`Env::new_global`] installs all built-ins
//!   - [`EvalError`]     — closed enum, `condition-case` matches on its
//!                         tag (see `EvalError::error_tag`)
//!
//! Architectural pillars (Doc 44 §3.3):
//!   1. *Heap*: leak-tolerant [`Sexp`] cloning. No GC, no arena. The
//!      arena-drop bridge is Phase 8.0.3's problem.
//!   2. *Symbol table*: `HashMap<String, SymbolEntry { value, function,
//!      plist, constant }>`.  Two-cell value/function model per Elisp.
//!   3. *Environment*: `Vec<HashMap<String, Sexp>>` lexical frame stack.
//!   4. *Evaluator*: recursive `eval(form, env)`, special forms
//!      dispatched by name string match in [`special_forms::apply_special`].
//!   5. *Special forms*: quote, function, if, cond, when, unless, let,
//!      let*, lambda, defun, defmacro, defvar, defconst, setq, set,
//!      while, dolist, dotimes, condition-case, unwind-protect, progn,
//!      prog1, prog2, and, or, not.
//!   6. *Built-ins*: arithmetic, equality, cons/list, higher-order,
//!      predicates, string, symbol/function (~50 entries — see
//!      [`builtins`]).
//!   7. *Errors*: `Result<Sexp, EvalError>` everywhere, never panic on
//!      user input (Doc 44 §4.3).
//!
//! Layer rule: this module owns *only* evaluation.  The reader stays
//! syntactic (Phase 8.0.1) and the MCP server (Phase 8.0.4) is a peer.

pub mod builtins;
pub mod cons_primitives;
pub mod dlsym_bridge;
pub mod env;
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
pub mod special_forms;

pub use env::{Env, Frame, SymbolEntry};
pub use error::{is_error_subtype, EvalError};
pub use sexp::Sexp;

/// Phase 7 Stage 7.7.c.1 (Doc 72) — read `input` via the post-
/// bootstrap elisp reader (`nelisp--read-all-from-string-impl' from
/// `lisp/nelisp-stdlib-reader.el').  The Rust reader is reachable
/// only through the bridge / baker / cargo-test paths; production
/// CLI entry points (`eval_str' / `eval_str_all' / `eval_str_all_
/// at_path') route through this helper so the production `nelisp'
/// binary never touches `reader::read_*' directly.
///
/// Stage 7.7.d (Doc 72): widened to `pub(crate)' so cargo unit-test
/// fixtures in `eval/tests.rs' and `bridge/mod.rs' can construct
/// expected `Sexp' values via the elisp reader instead of calling
/// `crate::reader::read_str' directly.  This brings the cargo test
/// suite to 100% elisp-reader-coverage for fixture parsing — a
/// prerequisite for Phase 8's full Rust reader deletion.
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

/// Stage 8.3.a (Doc 73, Phase 8) — read every top-level form from
/// `input` via the elisp reader and return `(LINE, FORM)` tuples
/// where `LINE` is the 1-origin source line at which each form
/// started.  Used by `bridge::loader::parse_tracked_forms' so the
/// production bridge bootstrap can attach source line numbers to
/// `BridgeError::EvalError' messages without falling back to the
/// Rust reader's `lexer::tokenize' + `parser::parse_one' twin pass.
///
/// Requires `nelisp--read-all-with-line-from-string-impl' from
/// `lisp/nelisp-stdlib-reader.el' to be loaded; that ships in the
/// STDLIB images so by the time `bootstrap_self_host' runs, this
/// helper is callable.
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

/// Stage 7.7.d (Doc 72) — read exactly one form via the elisp reader.
/// Convenience wrapper around `read_all_via_elisp' that errors on
/// empty / multi-form input.  Used by cargo unit tests (= `eval/tests.rs',
/// `bridge/mod.rs') to construct fixture `Sexp' values without calling
/// `crate::reader::read_str' directly.  Phase 8 will delete the Rust
/// reader; this helper is the migration bridge.  Gated with `cfg(test)'
/// because it is purely a test-fixture helper — production callers use
/// `read_all_via_elisp' (or its `eval_str*' wrappers) directly.
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

/// Read exactly one form from `input` and evaluate it in a fresh
/// global environment.  Trailing tokens after the first form are an
/// error.  Stage 7.7.c.1 (Doc 72): reading goes through the elisp
/// reader; the Rust `reader::read_str' is no longer called here.
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

/// Read every top-level form from `input` and evaluate them in
/// sequence in a single fresh global environment, returning the last
/// value (= `progn`).  Empty input returns `nil`.  Stage 7.7.c.1
/// (Doc 72): reading goes through the elisp reader.
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
    let dd_value = Sexp::Str(parent_dir.clone());
    env.set_value("default-directory", dd_value.clone())?;
    // Doc 86 §86.3.b shadow-path verify (cfg-gated, +0 prod LOC).
    #[cfg(feature = "env-shadow-verify")]
    env::verify_elisp_mirror_set_value(&env, "default-directory", &dd_value);
    let lfn_value = Sexp::Str(src_path.to_string());
    env.set_value("load-file-name", lfn_value.clone())?;
    #[cfg(feature = "env-shadow-verify")]
    env::verify_elisp_mirror_set_value(&env, "load-file-name", &lfn_value);
    // Single-element `load-path' = the source file's directory.  Users
    // who want extra search roots can `(setq load-path (cons "..."
    // load-path))' inside the source.
    let lp_value = Sexp::cons(Sexp::Str(parent_dir), Sexp::Nil);
    env.set_value("load-path", lp_value.clone())?;
    #[cfg(feature = "env-shadow-verify")]
    env::verify_elisp_mirror_set_value(&env, "load-path", &lp_value);
    let forms = read_all_via_elisp(input, &mut env)?;
    let mut last = Sexp::Nil;
    for f in &forms {
        last = eval(f, &mut env)?;
    }
    Ok(last)
}

/// Evaluate `form` in `env`.  This is the canonical recursive
/// entry point and is what [`eval_str`] / [`eval_str_all`] / built-ins
/// like `funcall` / `apply` / `eval` route through.
///
/// Algorithm:
///   1. self-evaluating atom → return as-is
///   2. symbol → look up in lexical frames then globals
///   3. cons → head decides dispatch:
///        - special form  → [`special_forms::apply_special`]
///        - macro         → expand once, recurse on the expansion
///        - lambda / sym  → evaluate arguments, [`apply_function`]
///   4. anything else → internal error
pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    // Doc 51 Track M (2026-05-04) — process-wide quit poll.  Any
    // code path that flipped `quit::QUIT_FLAG` (signal handler,
    // C-g key dispatch, sibling thread) gets converted into a
    // proper `EvalError::Quit` here, which then unwinds through
    // `condition-case`'s `quit` clause and `unwind-protect`.
    // `take_quit_flag` is a single atomic swap, so the conversion
    // is exactly-once even under racing setters.
    if quit::take_quit_flag() {
        return Err(EvalError::Quit);
    }
    // Recursion guard so a `(defun loop () (loop))` returns an error
    // instead of overflowing the Rust stack.
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
        // Self-evaluating atoms (Elisp manual: "Most kinds of objects
        // evaluate to themselves").
        Sexp::Nil | Sexp::T | Sexp::Int(_) | Sexp::Float(_) | Sexp::Str(_)
            | Sexp::MutStr(_) | Sexp::Vector(_)
            | Sexp::CharTable(_) | Sexp::BoolVector(_)
            | Sexp::Record(_) => {
            Ok(form.clone())
        }
        // `Sexp::Cell' wraps a let-binding slot for closure write-
        // through; should never be evaluated as a form, but if it
        // is (= someone manually evaluates a captured-env alist
        // entry), treat the inner value as self-evaluating.
        Sexp::Cell(c) => Ok(c.value.clone()),
        // Symbols.  Per Elisp manual "Constant Variables" §11.2,
        // a symbol whose name begins with `:' is a keyword: it is
        // its own value and cannot be bound.  This rule predates
        // any value-cell lookup.
        Sexp::Symbol(name) if name.starts_with(':') && name.len() > 1 => {
            Ok(form.clone())
        }
        // Plain symbols evaluate via the value cell.
        Sexp::Symbol(name) => {
            // Doc 86 §86.3.b shadow-path verify (cfg-gated, +0 prod LOC).
            let rust_result = env.lookup_value(name);
            #[cfg(feature = "env-shadow-verify")]
            env::verify_elisp_mirror_lookup_value(env, name, &rust_result);
            rust_result
        }
        // Cons → function application.
        Sexp::Cons(b) => apply_combiner(&b.car, &b.cdr, env),
    }
}

/// Apply a combiner head to its argument list (un-evaluated).  The
/// head's runtime kind decides whether arguments get evaluated and
/// how the call is dispatched.
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
                    // Phase 7 Stage 7.5.c (Doc 69 §3.1): post-bootstrap →
                    // route through elisp `nelisp--expand-macro'.  The
                    // self-pacing `lookup_function' check on the helper
                    // lets bootstrap fall through to Rust `expand_macro'
                    // (= the elisp helper isn't installed until
                    // nelisp-stdlib-eval-core.el — last STDLIB_SOURCES
                    // entry — completes loading).  Helpers and the
                    // delegation-depth-guarded recursive path also fall
                    // through to avoid cycles: the elisp helper itself
                    // expands macros via its own `cond' chain that
                    // calls back into the dispatcher.
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
                // Phase 7 Stage 7.4.c (Doc 68 §2.7) — delegate to
                // elisp `nelisp--apply-fn' for the *outermost*
                // user-level closure call.  Skip:
                //   (a) when already inside a delegation
                //       (`delegation_depth > 0') — the dispatcher's
                //       own machinery + every defun it internally
                //       invokes (= consp / null / nth / memq / etc.)
                //       must run through Rust apply_function or we
                //       cycle (`consp' delegated → apply-fn body →
                //       builtinp call → consp call → ...)
                //   (b) Rust builtins — round-tripping through the
                //       elisp dispatcher would just call the same
                //       Rust dispatch table indirectly with no
                //       distinct semantics
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

/// Phase 7 Stage 7.4.d (Doc 68) — names of the elisp dispatcher's
/// own support stack, exempted from `delegate_to_elisp_apply' even
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
    // Stage 7.4.e (Doc 70): `nelisp--apply-lambda-inner' was demoted
    // to a Rust builtin to fix the frame-capture leak — it is now
    // short-circuited by `is_builtin_value' before reaching this list,
    // so dropping it here is a cleanup, not a behaviour change.
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

/// Phase 7 Stage 7.4.c (Doc 68 §2.7) — `(builtin NAME)' shape detector.
/// Used by [`apply_combiner`] to short-circuit the elisp delegation
/// for Rust builtins (= round-tripping a builtin through the elisp
/// dispatcher would just call the same Rust dispatch table indirectly
/// with no distinct semantics; Stage 7.4.a ERT already covers the
/// elisp builtin-dispatch branch in isolation).
fn is_builtin_value(func: &Sexp) -> bool {
    if let Sexp::Cons(b) = func {
        if let Sexp::Symbol(s) = &b.car {
            return s == "builtin";
        }
    }
    false
}

/// Phase 7 Stage 7.4.c (Doc 68 §2.7) — delegate to elisp
/// `(nelisp--apply-fn FUNC ARGS)' so the function-application
/// semantics run through the elisp implementation in
/// `lisp/nelisp-stdlib-eval-core.el'.
///
/// Bumps `env.delegation_depth' for the duration of the recursive
/// `eval' so the apply_combiner gate correctly disables further
/// delegation for everything that runs *inside* the dispatcher (=
/// helper defuns + the elisp macros they call internally + their
/// macro-expansion phases).  Without that guard, a single call to
/// `(consp X)' from a predicate body would re-trigger delegation
/// through `nelisp--apply-fn', whose body is itself a `(cond ...)'
/// that calls `consp' again — infinite recursion.
///
/// The macro-expansion path in [`apply_combiner`] step 2 is
/// intentionally NOT routed through this helper: macro expansion is
/// itself a Rust primitive (= [`expand_macro`]) and runs with
/// `delegation_depth' inherited from its caller, so it falls into
/// the no-delegate path automatically.
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

/// Phase 7 Stage 7.5.c (Doc 69 §3.1) — delegate a macro expansion to
/// the elisp `nelisp--expand-macro' helper, then evaluate the
/// resulting form.  `macro_form' is the `(macro . LAMBDA)' shape;
/// `arg_forms' is the *un-evaluated* argument list (= macro semantics).
///
/// Bumps `env.delegation_depth' for the duration of the expansion call
/// so helpers invoked within (= cdr / null / consp / etc.) take the
/// Rust dispatch path and don't recurse back through this delegate.
/// The returned expansion is then `eval'd in the caller's env so that
/// any free variables it references are resolved against the call
/// site's lexical scope, matching Rust `expand_macro` semantics.
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
    // Phase 7 Stage 7.5.d (Doc 69): closure / lambda arms inline the
    // formerly-separate `apply_closure' / `apply_lambda' helpers.
    // Both were thin wrappers that called `list_elements' + dispatched
    // to `apply_lambda_inner' — inlining drops 2 functions (~27 LOC)
    // without changing dispatch semantics.
    match func {
        Sexp::Cons(b) => match &b.car {
            Sexp::Symbol(s) if s == "builtin" => apply_builtin(func, args, env),
            Sexp::Symbol(s) if s == "closure" => {
                // Shape: (closure CAPTURED-ENV ARGS BODY...)
                let parts = list_elements(func)?;
                if parts.len() < 3 {
                    return Err(EvalError::Internal(
                        "closure missing env / args / body".into(),
                    ));
                }
                apply_lambda_inner(&parts[1], &parts[2], &parts[3..], args, env)
            }
            Sexp::Symbol(s) if s == "lambda" => {
                // Bare `(lambda ARGS BODY...)` — apply against an empty
                // captured env.  This is what `defun` produces (the
                // function cell stores the lambda form unmodified).
                let parts = list_elements(func)?;
                if parts.len() < 2 {
                    return Err(EvalError::Internal(
                        "lambda missing args / body".into(),
                    ));
                }
                apply_lambda_inner(&Sexp::Nil, &parts[1], &parts[2..], args, env)
            }
            Sexp::Symbol(s) if s == "macro" => Err(EvalError::WrongType {
                expected: "function (not macro)".into(),
                got: func.clone(),
            }),
            _ => Err(EvalError::WrongType {
                expected: "function".into(),
                got: func.clone(),
            }),
        },
        _ => Err(EvalError::WrongType {
            expected: "function".into(),
            got: func.clone(),
        }),
    }
}

fn apply_builtin(
    func: &Sexp,
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // `(builtin <NAME>)` — pull the name out, dispatch via the registry.
    let name = match func {
        Sexp::Cons(outer) => match &outer.cdr {
            Sexp::Cons(inner) => match &inner.car {
                Sexp::Symbol(s) => s.clone(),
                Sexp::Str(s) => s.clone(),
                _ => return Err(EvalError::Internal(
                    "builtin sentinel name not a symbol".into(),
                )),
            },
            _ => return Err(EvalError::Internal(
                "builtin sentinel missing name".into(),
            )),
        },
        _ => return Err(EvalError::Internal("builtin sentinel not a cons".into())),
    };
    builtins::dispatch(&name, args, env)
}

// Phase 7 Stage 7.5.d (Doc 69) — `fn apply_closure' / `fn apply_lambda'
// were inlined into `apply_function' above (closure / lambda arms).
// Both were thin wrappers; only `apply_lambda_inner' carries the real
// work and remains a separate function (= also called by Stage 7.4.e
// `bi_apply_lambda_inner' builtin).

pub(crate) fn apply_lambda_inner(
    captured: &Sexp,
    formals: &Sexp,
    body: &[Sexp],
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    // Evaluate body inside captured env + a fresh frame for the
    // formal parameters.  The captured env is pushed first so its
    // bindings act as the enclosing lexical scope.
    env.push_captured(captured)?;
    env.push_frame();
    let bind_result = bind_formals(formals, args, env);
    if let Err(e) = bind_result {
        env.pop_frame();
        env.pop_frame();
        return Err(e);
    }
    let mut last = Sexp::Nil;
    let mut last_err: Option<EvalError> = None;
    for form in body {
        match eval(form, env) {
            Ok(v) => last = v,
            Err(e) => {
                last_err = Some(e);
                break;
            }
        }
    }
    env.pop_frame();
    env.pop_frame();
    if let Some(e) = last_err {
        Err(e)
    } else {
        Ok(last)
    }
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
