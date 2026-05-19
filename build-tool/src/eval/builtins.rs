//! Built-in function registry and dispatcher for the Rust eval surface.

use super::Env;
use super::error::EvalError;
use super::quit;
use super::sexp::Sexp;
use super::special_forms::is_truthy;
use std::path::{Path, PathBuf};

macro_rules! builtin_names {
    () => { &[
        "vector", "make-vector", "nelisp--length-cons-cc", "nelisp--recordp-cc", "string-bytes", "nl-jit-call-format-float", "truncate", "nelisp--syscall-canonicalize",
        "nelisp--syscall-stat", "nelisp--syscall-readdir", "nelisp--syscall-read-file", "nelisp--read-all-from-string", "nelisp--syscall", "nelisp--syscall-supported-p",
        "symbol-function", "fset", "nelisp--push-frame", "nelisp--pop-frame", "nelisp--push-captured", "nelisp--bind-local", "nelisp--apply-builtin-dispatch",
        "nelisp--set-use-elisp-apply", "nelisp--apply-lambda-inner", "funcall", "apply", "eval", "signal", "nelisp--write-stdout-bytes", "nelisp--write-stderr-line",
        "read-stdin-bytes", "nelisp--f64-trunc", "nl-write-file", "nl-make-directory", "terminal-raw-mode-enter", "terminal-raw-mode-leave", "read-stdin-byte-available",
        "_termios-saved-p", "_raw-mode-hooks-installed-p", "set-quit-flag", "clear-quit-flag", "quit-flag-pending-p", "install-sigint-handler", "_sigint-handler-installed-p",
        "install-winsize-handler", "_winsize-handler-installed-p", "terminal-take-winsize-changed", "terminal-current-winsize", "install-jobctrl-handlers",
        "_jobctrl-handlers-installed-p", "terminal-take-sigcont", "read", "read-from-string", "require", "nl-jit-call-i64-i64", "nl-jit-call-ptr-ptr",
        "nl-jit-call-syscall", "nl-jit-call-out-1", "nl-jit-call-out-2", "nl-jit-call-out-1i", "nl-jit-call-out-2i", "nl-jit-call-float-float",
        "nl-jit-call-float-cmp", "nl-jit-call-float-unary", "nl-fact-i64",
    ] };
}

macro_rules! builtin_dispatch {
    ($name:ident, $args:ident, $env:ident) => {
        match $name {
            "vector" => Ok(Sexp::vector($args.to_vec())), "make-vector" => bi_make_vector($args),
            "nelisp--length-cons-cc" => {
                require_arity("nelisp--length-cons-cc", $args, 1, Some(1))?;
                match &$args[0] {
                    Sexp::Cons(_) | Sexp::Nil => Ok(cc_slot_1(&$args[0], crate::elisp_cc_spike::length_cons)),
                    other => Err(EvalError::WrongType { expected: "sequencep".into(), got: other.clone() }),
                }
            },
            "nelisp--recordp-cc" => {
                require_arity("nelisp--recordp-cc", $args, 1, Some(1))?;
                Ok(cc_slot_1(&$args[0], crate::elisp_cc_spike::recordp))
            },
            "string-bytes" => {
                require_arity("string-bytes", $args, 1, Some(1))?;
                let str_view: Sexp = match &$args[0] {
                    Sexp::Str(_) => $args[0].clone(),
                    Sexp::MutStr(rc) => Sexp::Str(rc.value.clone()),
                    other => return Err(EvalError::WrongType { expected: "string".into(), got: other.clone() }),
                };
                let mut result_slot: Sexp = Sexp::Nil;
                unsafe { crate::elisp_cc_spike::bi_string_bytes(&str_view as *const Sexp, &mut result_slot as *mut Sexp); }
                Ok(result_slot)
            },
            "nl-jit-call-format-float" => crate::jit::bi_nl_jit_call_format_float($args),
            "truncate" => {
                require_arity("truncate", $args, 1, Some(1))?;
                match &$args[0] {
                    Sexp::Int(_) => Ok(cc_slot_1(&$args[0], crate::elisp_cc_spike::truncate_int)),
                    Sexp::Float(x) => Ok(Sexp::Int(*x as i64)),
                    other => Err(EvalError::WrongType { expected: "numberp".into(), got: other.clone() }),
                }
            },
            "nelisp--syscall-canonicalize" => bi_syscall_canonicalize($args, $env), "nelisp--syscall-stat" => bi_syscall_stat($args, $env), "nelisp--syscall-readdir" => bi_syscall_readdir($args, $env),
            "nelisp--syscall-read-file" => bi_syscall_read_file($args, $env), "nelisp--read-all-from-string" => bi_read_all_from_string($args, $env), "nelisp--syscall" => bi_syscall($args),
            "nelisp--syscall-supported-p" => bi_syscall_supported_p($args),
            "symbol-function" => {
                require_arity("symbol-function", $args, 1, Some(1))?;
                super::env_shim::bi_globals_op(&[Sexp::Symbol("get-function".into()), $args[0].clone()], $env)
            },
            "fset" => {
                require_arity("fset", $args, 2, Some(2))?;
                super::env_shim::bi_globals_op(&[
                    Sexp::Symbol("set-function".into()),
                    $args[0].clone(),
                    match &$args[1] {
                        Sexp::Symbol(s) => $env.lookup_function(s)?,
                        other => other.clone(),
                    },
                ], $env)
            },
            "nelisp--push-frame" => { require_arity("nelisp--push-frame", $args, 0, Some(0))?; $env.push_frame(); Ok(Sexp::T) },
            "nelisp--pop-frame" => { require_arity("nelisp--pop-frame", $args, 0, Some(0))?; $env.pop_frame(); Ok(Sexp::T) },
            "nelisp--push-captured" => { require_arity("nelisp--push-captured", $args, 1, Some(1))?; $env.push_captured(&$args[0])?; Ok(Sexp::T) },
            "nelisp--bind-local" => {
                require_arity("nelisp--bind-local", $args, 2, Some(2))?;
                let name = match &$args[0] {
                    Sexp::Symbol(s) => s.clone(),
                    other => return Err(EvalError::WrongType { expected: "symbol".into(), got: other.clone() }),
                };
                $env.bind_local(&name, $args[1].clone());
                Ok($args[1].clone())
            },
            "nelisp--apply-builtin-dispatch" => {
                require_arity("nelisp--apply-builtin-dispatch", $args, 2, Some(2))?;
                let name = match &$args[0] {
                    Sexp::Symbol(s) | Sexp::Str(s) => s.clone(),
                    other => return Err(EvalError::WrongType { expected: "symbol".into(), got: other.clone() }),
                };
                dispatch(&name, &super::list_elements(&$args[1])?, $env)
            },
            "nelisp--set-use-elisp-apply" => { require_arity("nelisp--set-use-elisp-apply", $args, 1, Some(1))?; $env.use_elisp_apply = !matches!($args[0], Sexp::Nil); Ok(bool_sexp($env.use_elisp_apply)) },
            "nelisp--apply-lambda-inner" => {
                require_arity("nelisp--apply-lambda-inner", $args, 4, Some(4))?;
                super::apply_lambda_inner(&$args[0], &$args[1], &super::list_elements(&$args[2])?, &super::list_elements(&$args[3])?, $env)
            },
            "funcall" => {
                require_arity("funcall", $args, 1, None)?;
                let func = resolve_callable(&$args[0], $env)?;
                super::apply_function(&func, &$args[1..], $env)
            },
            "apply" => {
                require_arity("apply", $args, 2, None)?;
                let func = resolve_callable(&$args[0], $env)?;
                let mut all_args: Vec<Sexp> = $args[1..$args.len() - 1].to_vec();
                all_args.extend(list_to_vec(&$args[$args.len() - 1])?);
                super::apply_function(&func, &all_args, $env)
            },
            "eval" => { require_arity("eval", $args, 1, Some(2))?; super::eval(&$args[0], $env) },
            "signal" => bi_signal($args),
            "nelisp--write-stdout-bytes" => bi_write_stdout_bytes($args), "nelisp--write-stderr-line" => bi_write_stderr_line($args), "read-stdin-bytes" => bi_read_stdin_bytes($args),
            "nelisp--f64-trunc" => bi_f64_trunc($args),
            "nl-write-file" => {
                require_arity("nl-write-file", $args, 2, Some(2))?;
                let path = string_value(&$args[0])?;
                string_value(&$args[1])?;
                kernel_path_ok("nl-write-file", &path, unsafe {
                    crate::elisp_cc_spike::bi_nl_write_file(&$args[0] as *const _, &$args[1] as *const _)
                })
            },
            "nl-make-directory" => {
                require_arity("nl-make-directory", $args, 1, Some(2))?;
                let path = string_value(&$args[0])?;
                kernel_path_ok("nl-make-directory", &path, unsafe {
                    crate::elisp_cc_spike::bi_nl_make_directory(&$args[0] as *const _) as i32 as i64
                })
            },
            "terminal-raw-mode-enter" => bi_terminal_raw_mode_enter($args), "terminal-raw-mode-leave" => bi_terminal_raw_mode_leave($args), "read-stdin-byte-available" => bi_read_stdin_byte_available($args),
            "_termios-saved-p" => {
                require_arity("_termios-saved-p", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(bool_sexp(tty_raw::termios_saved_p())) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "_raw-mode-hooks-installed-p" => {
                require_arity("_raw-mode-hooks-installed-p", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(bool_sexp(tty_raw::hooks_installed_p())) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "set-quit-flag" => {
                require_arity("set-quit-flag", $args, 0, Some(0))?;
                unsafe { crate::elisp_cc_spike::bi_set_quit_flag(quit::nl_quit_flag_ptr()); }
                Ok(Sexp::T)
            },
            "clear-quit-flag" => {
                require_arity("clear-quit-flag", $args, 0, Some(0))?;
                unsafe { crate::elisp_cc_spike::bi_clear_quit_flag(quit::nl_quit_flag_ptr()); }
                Ok(Sexp::T)
            },
            "quit-flag-pending-p" => {
                require_arity("quit-flag-pending-p", $args, 0, Some(0))?;
                Ok(bool_sexp(unsafe { crate::elisp_cc_spike::bi_quit_flag_pending_p(quit::nl_quit_flag_ptr()) } != 0))
            },
            "install-sigint-handler" => {
                require_arity("install-sigint-handler", $args, 0, Some(0))?;
                quit::install_sigint_handler();
                Ok(Sexp::T)
            },
            "_sigint-handler-installed-p" => {
                require_arity("_sigint-handler-installed-p", $args, 0, Some(0))?;
                Ok(bool_sexp(quit::sigint_handler_installed_p()))
            },
            "install-winsize-handler" => {
                require_arity("install-winsize-handler", $args, 0, Some(0))?;
                #[cfg(unix)]    { tty_winsize::install_handler(); Ok(Sexp::T) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "_winsize-handler-installed-p" => {
                require_arity("_winsize-handler-installed-p", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(bool_sexp(tty_winsize::handler_installed_p())) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "terminal-take-winsize-changed" => {
                require_arity("terminal-take-winsize-changed", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(bool_sexp(tty_winsize::take_changed())) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "terminal-current-winsize" => {
                require_arity("terminal-current-winsize", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(match tty_winsize::current_size() {
                    Some((c, r)) => Sexp::cons(Sexp::Int(c as i64), Sexp::Int(r as i64)),
                    None => Sexp::Nil,
                }) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "install-jobctrl-handlers" => {
                require_arity("install-jobctrl-handlers", $args, 0, Some(0))?;
                #[cfg(unix)]    { tty_jobctrl::install_handlers(); Ok(Sexp::T) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "_jobctrl-handlers-installed-p" => {
                require_arity("_jobctrl-handlers-installed-p", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(bool_sexp(tty_jobctrl::handlers_installed_p())) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "terminal-take-sigcont" => {
                require_arity("terminal-take-sigcont", $args, 0, Some(0))?;
                #[cfg(unix)]    { Ok(bool_sexp(tty_jobctrl::take_cont())) }
                #[cfg(not(unix))] { Ok(Sexp::Nil) }
            },
            "read" => bi_read($args, $env), "read-from-string" => bi_read_from_string($args, $env), "require" => bi_require($args, $env),
            "nl-jit-call-i64-i64" => crate::jit::bi_nl_jit_call_i64_i64($args), "nl-jit-call-ptr-ptr" => crate::jit::bi_nl_jit_call_ptr_ptr($args), "nl-jit-call-syscall" => crate::jit::bi_nl_jit_call_syscall($args),
            "nl-jit-call-out-1" => crate::jit::bi_nl_jit_call_out_1($args), "nl-jit-call-out-2" => crate::jit::bi_nl_jit_call_out_2($args), "nl-jit-call-out-1i" => crate::jit::bi_nl_jit_call_out_1i($args),
            "nl-jit-call-out-2i" => crate::jit::bi_nl_jit_call_out_2i($args), "nl-jit-call-float-float" => crate::jit::bi_nl_jit_call_float_float($args),
            "nl-jit-call-float-cmp" => crate::jit::bi_nl_jit_call_float_cmp($args), "nl-jit-call-float-unary" => crate::jit::bi_nl_jit_call_float_unary($args),
            "nl-fact-i64" => {
                require_arity("nl-fact-i64", $args, 1, Some(1))?;
                let Sexp::Int(_) = &$args[0] else {
                    return Err(EvalError::WrongType { expected: "integerp".into(), got: $args[0].clone() });
                };
                let mut out = Sexp::Nil;
                let rc = unsafe { crate::elisp_cc_spike::bi_nl_fact_i64(&$args[0] as *const _, &mut out as *mut _) };
                if rc == 0 { Ok(out) } else { Err(EvalError::Internal("nl-fact-i64: argument out of i64-safe range 0..=20".into())) }
            },
            "nelisp--env-globals-op" => crate::eval::env_shim::bi_globals_op($args, $env),
            _ => match $env.extern_builtins.get($name).cloned() { Some(f) => f($args, $env), None => Err(EvalError::UnboundFunction($name.to_string())) }
        }
    };
}

pub fn install_builtins(env: &mut Env) {
    for n in builtin_names!() {
        let sentinel = Sexp::list_from(&[Sexp::Symbol("builtin".into()), Sexp::Symbol((*n).into())]);
        env.set_function(n, sentinel);
    }
}

pub fn dispatch(name: &str, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    builtin_dispatch!(name, args, env)
}

pub(crate) fn require_arity(name: &str, args: &[Sexp], min: usize, max: Option<usize>) -> Result<(), EvalError> {
    if args.len() < min || max.map_or(false, |m| args.len() > m) {
        let expected = match max {
            Some(m) if m == min => min.to_string(), Some(m) => format!("{}-{}", min, m), None => format!("≥{}", min),
        };
        return Err(EvalError::WrongNumberOfArguments { function: name.into(), expected, got: args.len() });
    }
    Ok(())
}

pub(crate) fn as_int(name: &str, v: &Sexp) -> Result<i64, EvalError> {
    match v {
        Sexp::Int(n) => Ok(*n), Sexp::Float(x) => Ok(*x as i64),
        other => Err(EvalError::WrongType { expected: format!("number ({} arg)", name), got: other.clone() }),
    }
}

pub(crate) fn num_pair(args: &[Sexp], name: &str) -> Result<(f64, f64, bool), EvalError> {
    require_arity(name, args, 2, Some(2))?;
    let af = matches!(args[0], Sexp::Float(_)) || matches!(args[1], Sexp::Float(_));
    let to_f64 = |v: &Sexp| -> Result<f64, EvalError> {
        match v {
            Sexp::Int(n) => Ok(*n as f64), Sexp::Float(x) => Ok(*x),
            other => Err(EvalError::WrongType { expected: "numberp".into(), got: other.clone() }),
        }
    };
    Ok((to_f64(&args[0])?, to_f64(&args[1])?, af))
}

fn list_to_vec(v: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur: Sexp = v.clone();
    loop {
        let next = match &cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(b) => { out.push(b.car.clone()); b.cdr.clone() },
            other => return Err(EvalError::WrongType { expected: "listp".into(), got: other.clone() }),
        };
        cur = next;
    }
}

pub(crate) fn char_table_set_one(inner: &mut crate::eval::sexp::CharTableInner, c: i64, v: Sexp) {
    for entry in inner.entries.iter_mut() {
        if entry.0 == c {
            entry.1 = v;
            return;
        }
    }
    inner.entries.push((c, v));
}

pub(crate) fn char_table_get(rc: &crate::eval::nlchartable::NlCharTableRef, c: i64) -> Sexp {
    let inner = &rc.inner;
    inner
        .entries
        .iter()
        .find(|(k, _)| *k == c)
        .map(|(_, v)| v.clone())
        .or_else(|| inner.parent.as_ref().map(|parent| char_table_get(parent, c)))
        .unwrap_or_else(|| inner.default_val.clone())
}

fn string_value(v: &Sexp) -> Result<String, EvalError> {
    match v {
        Sexp::Str(s) | Sexp::Symbol(s) => Ok(s.clone()), Sexp::MutStr(rc) => Ok(rc.value.clone()),
        Sexp::Nil => Ok("nil".into()), Sexp::T => Ok("t".into()),
        other => Err(EvalError::WrongType { expected: "stringp or symbolp".into(), got: other.clone() }),
    }
}
fn normalize_path(path: &str, base: Option<&str>) -> PathBuf {
    let p = Path::new(path);
    if p.is_absolute() { p.to_path_buf() } else if let Some(base) = base { Path::new(base).join(p) } else if let Ok(cwd) = std::env::current_dir() { cwd.join(p) } else { p.to_path_buf() }
}
fn env_default_directory(env: &Env) -> Option<String> { match env.lookup_value("default-directory") { Ok(Sexp::Str(s)) => Some(s), _ => None } }
fn cc_slot_1(arg: &Sexp, f: unsafe fn(*const Sexp, *mut Sexp) -> *mut Sexp) -> Sexp { let mut slot = Sexp::Nil; unsafe { f(arg as *const _, &mut slot as *mut _) }; slot }
fn bool_sexp(v: bool) -> Sexp { if v { Sexp::T } else { Sexp::Nil } }

fn kernel_path_ok(name: &str, path: &str, rc: i64) -> Result<Sexp, EvalError> {
    if rc < 0 {
        Err(EvalError::Internal(format!("{name}: {path}: kernel returned {rc}")))
    } else {
        Ok(Sexp::T)
    }
}

fn bi_syscall_canonicalize(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-canonicalize", args, 1, Some(1))?;
    let path_sexp = resolve_path_sexp(&args[0], env)?;
    let mut buf = vec![0u8; libc::PATH_MAX as usize];
    if unsafe {
        crate::elisp_cc_spike::bi_syscall_canonicalize(&path_sexp as *const _, buf.as_mut_ptr())
    } == 0
    {
        return Ok(Sexp::Nil);
    }
    let len = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    Ok(Sexp::Str(String::from_utf8_lossy(&buf[..len]).into_owned()))
}

fn bi_syscall_read_file(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-read-file", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    let n_bytes = match std::fs::metadata(&p) {
        Ok(m) if m.is_file() => m.len() as usize,
        _ => return Ok(Sexp::Nil),
    };
    if n_bytes == 0 { return Ok(Sexp::Str(String::new())); }
    let path_sexp = Sexp::Str(p.to_string_lossy().into_owned());
    let mut buf = vec![0u8; n_bytes];
    let rc = unsafe {
        crate::elisp_cc_spike::bi_syscall_read_file(
            &path_sexp as *const _, buf.as_mut_ptr(), n_bytes as i64,
        )
    } as i32 as i64;
    if rc < 0 {
        return Ok(Sexp::Nil);
    }
    buf.truncate((rc as usize).min(n_bytes));
    Ok(Sexp::Str(String::from_utf8_lossy(&buf).into_owned()))
}

fn bi_read_all_from_string(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--read-all-from-string", args, 1, Some(1))?;
    let impl_fn = env.lookup_function("nelisp--read-all-from-string-impl").map_err(|_| {
        EvalError::Internal(
            "nelisp--read-all-from-string: `nelisp--read-all-from-string-impl' not loaded \
             — `lisp/nelisp-stdlib-reader.el' missing from STDLIB_SOURCES?"
                .into(),
        )
    })?;
    super::apply_function(&impl_fn, args, env)
}

fn resolve_existing_path(arg: &Sexp, env: &Env) -> Result<PathBuf, EvalError> {
    let path = string_value(arg)?;
    let base = env_default_directory(env);
    Ok(normalize_path(&path, base.as_deref()))
}

fn resolve_path_sexp(arg: &Sexp, env: &Env) -> Result<Sexp, EvalError> {
    let p = resolve_existing_path(arg, env)?;
    Ok(Sexp::Str(p.to_string_lossy().into_owned()))
}

fn bi_syscall_stat(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-stat", args, 1, Some(1))?;
    let path_sexp = resolve_path_sexp(&args[0], env)?;
    let mut statbuf: libc::stat = unsafe { std::mem::zeroed() };
    let rc = unsafe {
        crate::elisp_cc_spike::bi_syscall_stat(
            &path_sexp as *const _, (&mut statbuf as *mut libc::stat) as *mut u8,
        )
    };
    let tag = if rc < 0 {
        "absent"
    } else {
        match statbuf.st_mode & libc::S_IFMT {
            m if m == libc::S_IFDIR => "directory",
            m if m == libc::S_IFREG => "file",
            _ => "absent",
        }
    };
    Ok(Sexp::Symbol(tag.into()))
}

fn bi_syscall_readdir(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-readdir", args, 1, Some(1))?;
    let dir = resolve_existing_path(&args[0], env)?;
    let dir_str = dir.to_string_lossy().into_owned();
    let entries: Vec<Sexp> = match std::fs::read_dir(&dir) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            .map(|e| Sexp::Str(e.file_name().to_string_lossy().into_owned()))
            .collect(),
        Err(_) => return Ok(Sexp::Nil),
    };
    Ok(Sexp::cons(Sexp::Str(dir_str), Sexp::list_from(&entries)))
}

fn bi_syscall_supported_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-supported-p", args, 0, Some(0))?;
    #[cfg(target_os = "linux")]
    { Ok(Sexp::T) }
    #[cfg(not(target_os = "linux"))]
    { Ok(Sexp::Nil) }
}

#[cfg(target_os = "linux")]
pub(crate) fn syscall_arg_int(name: &str, idx: usize, s: &Sexp) -> Result<i64, EvalError> {
    match s {
        Sexp::Int(n) => Ok(*n),
        Sexp::Nil => Ok(0),
        Sexp::T => Ok(1),
        other => Err(EvalError::WrongType {
            expected: format!("integer (arg {} of {})", idx, name),
            got: other.clone(),
        }),
    }
}

#[cfg(target_os = "linux")]
pub(crate) fn syscall_nr(name_or_nr: &Sexp) -> Result<i64, EvalError> {
    match name_or_nr {
        Sexp::Int(n) => Ok(*n),
        Sexp::Symbol(s) => match s.as_str() {
            "read"       => Ok(libc::SYS_read       as i64),
            "write"      => Ok(libc::SYS_write      as i64),
            "close"      => Ok(libc::SYS_close      as i64),
            "openat"     => Ok(libc::SYS_openat     as i64),
            "exit_group" => Ok(libc::SYS_exit_group as i64),
            "lseek"      => Ok(libc::SYS_lseek      as i64),
            "dup2"       => Ok(libc::SYS_dup2       as i64),
            "getpid"     => Ok(libc::SYS_getpid     as i64),
            "kill"       => Ok(libc::SYS_kill       as i64),
            "mmap"              => Ok(libc::SYS_mmap              as i64),
            "mprotect"          => Ok(libc::SYS_mprotect          as i64),
            "munmap"            => Ok(libc::SYS_munmap            as i64),
            "fcntl"             => Ok(libc::SYS_fcntl             as i64),
            "fork"              => Ok(libc::SYS_fork              as i64),
            "socket"            => Ok(libc::SYS_socket            as i64),
            "listen"            => Ok(libc::SYS_listen            as i64),
            "wait4"             => Ok(libc::SYS_wait4             as i64),
            "getppid"           => Ok(libc::SYS_getppid           as i64),
            "setpgid"           => Ok(libc::SYS_setpgid           as i64),
            "pidfd_open"        => Ok(libc::SYS_pidfd_open        as i64),
            "pidfd_send_signal" => Ok(libc::SYS_pidfd_send_signal as i64),
            "inotify_init1"     => Ok(libc::SYS_inotify_init1     as i64),
            "inotify_rm_watch"  => Ok(libc::SYS_inotify_rm_watch  as i64),
            "eventfd2"          => Ok(libc::SYS_eventfd2          as i64),
            "timerfd_create"    => Ok(libc::SYS_timerfd_create    as i64),
            other => Err(EvalError::Internal(format!(
                "nelisp--syscall: unknown syscall name `{}'", other))),
        },
        other => Err(EvalError::WrongType {
            expected: "syscall name (symbol) or number (integer)".into(),
            got: other.clone(),
        }),
    }
}

#[cfg(target_os = "linux")]
unsafe fn syscall_errno_normalize(r: libc::c_long) -> i64 {
    if r == -1 {
        -(*libc::__errno_location() as i64)
    } else {
        r as i64
    }
}

#[cfg(not(target_os = "linux"))]
macro_rules! syscall_unsupported {
    ($name:ident, $primitive:literal) => {
        fn $name(args: &[Sexp]) -> Result<Sexp, EvalError> {
            let _ = args;
            Err(EvalError::Internal(
                concat!($primitive, ": unsupported platform").into(),
            ))
        }
    };
}

#[cfg(target_os = "linux")]
fn bi_syscall(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Internal(
            "nelisp--syscall: at least one argument (syscall nr / name) required".into()));
    }
    let nr = syscall_nr(&args[0])?;
    let mut a = [0i64; 6];
    for (i, sexp) in args[1..].iter().enumerate().take(6) {
        a[i] = syscall_arg_int("nelisp--syscall", i + 1, sexp)?;
    }
    let r = unsafe { libc::syscall(nr, a[0], a[1], a[2], a[3], a[4], a[5]) };
    Ok(Sexp::Int(unsafe { syscall_errno_normalize(r) }))
}

#[cfg(not(target_os = "linux"))]
syscall_unsupported!(bi_syscall, "nelisp--syscall");

fn resolve_callable(arg: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match arg { Sexp::Symbol(s) => env.lookup_function(s), _ => Ok(arg.clone()) }
}

fn bi_signal(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("signal", args, 2, Some(2))?;
    let Sexp::Symbol(tag) = &args[0] else { return Err(EvalError::WrongType { expected: "symbolp".into(), got: args[0].clone() }); };
    let (q, a, w) = (Sexp::Symbol("quit".into()), Sexp::Symbol("arith-error".into()), Sexp::Symbol("wrong-type-argument".into()));
    fn hd(s: &Sexp) -> Option<&Sexp> { if let Sexp::Cons(b) = s { Some(&b.car) } else { None } }
    match unsafe { crate::elisp_cc_spike::bi_signal_dispatch(&args[0], &q, &a, &w) } {
        0 => Err(EvalError::Quit),
        1 => Err(EvalError::ArithError(match hd(&args[1]).unwrap_or(&args[1]) { Sexp::Str(s) => s.clone(), o => format!("{o:?}") })),
        2 => Err(EvalError::WrongType {
            expected: match hd(&args[1]) { Some(Sexp::Symbol(s) | Sexp::Str(s)) => s.clone(), Some(o) => format!("{o:?}"), None => "argument".into() },
            got: match &args[1] { Sexp::Cons(b) => match &b.cdr { Sexp::Cons(c) => c.car.clone(), o => o.clone() }, o => o.clone() },
        }),
        _ => Err(EvalError::UserError { tag: tag.clone(), data: args[1].clone() }),
    }
}

fn bi_write_stdout_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stdout-bytes", args, 1, Some(1))?;
    let s = args[0].as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: "stringp".into(),
        got: args[0].clone(),
    })?;
    let body_sexp = Sexp::Str(s);
    let rc = unsafe {
        crate::elisp_cc_spike::bi_write_stdout_bytes(
            &body_sexp as *const Sexp,
        )
    };
    if rc < 0 {
        return Err(EvalError::Internal(format!(
            "nelisp--write-stdout-bytes: write returned {}",
            rc,
        )));
    }
    let mut out = std::io::stdout().lock();
    out.flush()
        .map_err(|e| EvalError::Internal(format!("nelisp--write-stdout-bytes: {}", e)))?;
    Ok(args[0].clone())
}

fn bi_write_stderr_line(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stderr-line", args, 1, Some(1))?;
    let s = args[0].as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: "stringp".into(),
        got: args[0].clone(),
    })?;
    let body_sexp = Sexp::Str(s);
    unsafe {
        let _ = crate::elisp_cc_spike::bi_write_stderr_line(
            &body_sexp as *const Sexp,
        );
    }
    let mut err = std::io::stderr().lock();
    let _ = err.write_all(b"\n");
    let _ = err.flush();
    Ok(args[0].clone())
}

fn to_f64(arg: &Sexp) -> Result<f64, EvalError> {
    match arg {
        Sexp::Int(i) => Ok(*i as f64),
        Sexp::Float(f) => Ok(*f),
        Sexp::Nil => Ok(0.0),
        other => Err(EvalError::WrongType {
            expected: "number".into(),
            got: other.clone(),
        }),
    }
}

fn bi_f64_trunc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--f64-trunc", args, 3, Some(3))?;
    let mode = match &args[0] {
        Sexp::Symbol(s) => s.as_str(),
        other => {
            return Err(EvalError::WrongType {
                expected: "symbol".into(),
                got: other.clone(),
            });
        }
    };
    let q = to_f64(&args[1])? / to_f64(&args[2])?;
    Ok(Sexp::Int(match mode {
        "floor" => q.floor() as i64,
        "ceiling" => q.ceil() as i64,
        "round" => q.round() as i64,
        "truncate" => q.trunc() as i64,
        _ => {
            return Err(EvalError::Internal(format!(
                "nelisp--f64-trunc: unknown mode `{mode}`"
            )));
        }
    }))
}

fn bi_read_stdin_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("read-stdin-bytes", args, 1, Some(1))?;
    let limit = match &args[0] {
        Sexp::Int(n) if *n > 0 => *n as usize,
        other => return Err(EvalError::WrongType { expected: "positive integer".into(), got: other.clone() }),
    };
    let mut buf = vec![0u8; limit];
    let rc = unsafe { crate::elisp_cc_spike::bi_read_stdin_bytes(buf.as_mut_ptr(), limit as i64) };
    if rc < 0 {
        return Err(EvalError::Internal(format!("read-stdin-bytes: read returned {}", rc)));
    }
    if rc == 0 { return Ok(Sexp::Nil); }
    buf.truncate((rc as usize).min(buf.len()));
    Ok(Sexp::Str(String::from_utf8_lossy(&buf).into_owned()))
}

// Unix raw-mode and non-blocking stdin helpers.

#[cfg(unix)]
mod tty_raw {
    use super::*;
    use std::mem::MaybeUninit;
    use std::os::unix::io::AsRawFd;
    use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
    use std::sync::Once;

    // ---- Async-signal-safe state ------------------------------------------------
    // The flag tells us whether SAVED_TERMIOS is initialised + the tty is
    // currently in raw mode.  `swap` provides the "claim and clear"
    // primitive both the explicit leave and the signal/atexit hook use.
    static TERMIOS_SAVED: AtomicBool = AtomicBool::new(false);
    static TTY_FD: AtomicI32 = AtomicI32::new(-1);
    // SAFETY contract: only written while TERMIOS_SAVED is false (which
    // implies no signal handler can race on it — handlers early-return on
    // a false flag).  Read only after TERMIOS_SAVED.swap(false) returns
    // true, which transfers ownership to the reader.
    static mut SAVED_TERMIOS: MaybeUninit<libc::termios> = MaybeUninit::uninit();
    static HOOKS_INSTALLED: Once = Once::new();

    // Restore the saved termios.  Called from BOTH the explicit
    // `terminal-raw-mode-leave` path AND the signal/atexit hook, so it
    // must be async-signal-safe — no allocation, no Mutex, no Rust
    // formatting.  `tcsetattr` is async-signal-safe per POSIX.
    fn restore_termios_signal_safe() {
        if TERMIOS_SAVED.swap(false, Ordering::SeqCst) {
            let fd = TTY_FD.load(Ordering::SeqCst);
            if fd >= 0 {
                unsafe {
                    let term_ptr = (*std::ptr::addr_of!(SAVED_TERMIOS)).as_ptr();
                    // Ignore the return code — we are best-effort in signal
                    // context; nothing actionable we can do on failure.
                    libc::tcsetattr(fd, libc::TCSANOW, term_ptr);
                }
            }
        }
    }

    extern "C" fn atexit_hook() {
        restore_termios_signal_safe();
    }

    extern "C" fn sig_handler(signum: libc::c_int) {
        restore_termios_signal_safe();
        // Reset to the system default and re-raise so the canonical
        // disposition (terminate / dump core / etc.) actually runs.
        unsafe {
            libc::signal(signum, libc::SIG_DFL);
            // Unblock the signal in case sigaction's default mask blocked
            // it while we were in this handler.
            let mut mask: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&mut mask);
            libc::sigaddset(&mut mask, signum);
            libc::sigprocmask(libc::SIG_UNBLOCK, &mask, std::ptr::null_mut());
            libc::raise(signum);
        }
    }

    fn install_hooks_once() {
        HOOKS_INSTALLED.call_once(|| unsafe {
            libc::atexit(atexit_hook);

            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = sig_handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            sa.sa_flags = 0;
            for sig in &[libc::SIGTERM, libc::SIGHUP, libc::SIGQUIT] {
                libc::sigaction(*sig, &sa, std::ptr::null_mut());
            }
        });
    }

    pub fn raw_mode_enter() -> Result<(), EvalError> {
        let fd = std::io::stdin().lock().as_raw_fd();
        let mut term: libc::termios = unsafe { std::mem::zeroed() };
        if unsafe { libc::tcgetattr(fd, &mut term) } != 0 {
            return Err(EvalError::Internal(format!(
                "terminal-raw-mode-enter: tcgetattr failed: {}",
                std::io::Error::last_os_error()
            )));
        }

        unsafe {
            std::ptr::write(
                std::ptr::addr_of_mut!(SAVED_TERMIOS) as *mut libc::termios,
                term,
            );
        }
        TTY_FD.store(fd, Ordering::SeqCst);
        TERMIOS_SAVED.store(true, Ordering::SeqCst);

        install_hooks_once();

        unsafe { libc::cfmakeraw(&mut term) };
        term.c_cc[libc::VMIN] = 1;
        term.c_cc[libc::VTIME] = 0;
        if unsafe { libc::tcsetattr(fd, libc::TCSANOW, &term) } != 0 {
            TERMIOS_SAVED.store(false, Ordering::SeqCst);
            return Err(EvalError::Internal(format!(
                "terminal-raw-mode-enter: tcsetattr failed: {}",
                std::io::Error::last_os_error()
            )));
        }
        Ok(())
    }

    pub fn raw_mode_leave() -> Result<(), EvalError> {
        if TERMIOS_SAVED.swap(false, Ordering::SeqCst) {
            let fd = TTY_FD.load(Ordering::SeqCst);
            if fd >= 0 {
                let term =
                    unsafe { (*std::ptr::addr_of!(SAVED_TERMIOS)).assume_init() };
                if unsafe { libc::tcsetattr(fd, libc::TCSANOW, &term) } != 0 {
                    return Err(EvalError::Internal(format!(
                        "terminal-raw-mode-leave: tcsetattr failed: {}",
                        std::io::Error::last_os_error()
                    )));
                }
            }
        }
        Ok(())
    }

    pub fn termios_saved_p() -> bool {
        TERMIOS_SAVED.load(Ordering::SeqCst)
    }

    pub fn hooks_installed_p() -> bool {
        HOOKS_INSTALLED.is_completed()
    }

    pub fn stdin_byte_available(timeout_ms: i32) -> Result<Option<u8>, EvalError> {
        let fd: i32 = 0; // STDIN
        let mut pfd = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
        let r = unsafe { libc::poll(&mut pfd, 1, timeout_ms) };
        if r < 0 {
            return Err(EvalError::Internal(format!(
                "read-stdin-byte-available: poll failed: {}",
                std::io::Error::last_os_error()
            )));
        }
        if r == 0 {
            return Ok(None);
        }
        if pfd.revents & (libc::POLLIN | libc::POLLHUP) == 0 {
            return Ok(None);
        }
        let mut buf = [0u8; 1];
        let n = unsafe {
            libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, 1)
        };
        match n {
            0 => Ok(None), // EOF
            n if n > 0 => Ok(Some(buf[0])),
            _ => {
                let errno = std::io::Error::last_os_error();
                #[allow(unreachable_patterns)]
                if matches!(
                    errno.raw_os_error(),
                    Some(libc::EAGAIN) | Some(libc::EWOULDBLOCK)
                ) {
                    Ok(None)
                } else {
                    Err(EvalError::Internal(format!(
                        "read-stdin-byte-available: read failed: {}",
                        errno
                    )))
                }
            }
        }
    }
}

// SIGWINCH state shared between the handler and the event loop.

#[cfg(unix)]
mod tty_winsize {
    use std::os::unix::io::AsRawFd;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Once;

    static WINSIZE_CHANGED: AtomicBool = AtomicBool::new(true);
    static HANDLER_INSTALLED: Once = Once::new();

    extern "C" fn handler(_signum: libc::c_int) {
        WINSIZE_CHANGED.store(true, Ordering::SeqCst);
    }

    pub fn install_handler() {
        HANDLER_INSTALLED.call_once(|| unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            sa.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGWINCH, &sa, std::ptr::null_mut());
            WINSIZE_CHANGED.store(true, Ordering::SeqCst);
        });
    }

    pub fn handler_installed_p() -> bool {
        HANDLER_INSTALLED.is_completed()
    }

    pub fn take_changed() -> bool {
        WINSIZE_CHANGED.swap(false, Ordering::SeqCst)
    }

    pub fn current_size() -> Option<(u16, u16)> {
        let fd = std::io::stdin().lock().as_raw_fd();
        let mut ws: libc::winsize = unsafe { std::mem::zeroed() };
        if unsafe { libc::ioctl(fd, libc::TIOCGWINSZ, &mut ws) } == 0 {
            Some((ws.ws_col, ws.ws_row))
        } else {
            None
        }
    }
}

// SIGTSTP/SIGCONT state for suspending and resuming raw mode cleanly.

#[cfg(unix)]
mod tty_jobctrl {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Once;

    static SIGCONT_ARRIVED: AtomicBool = AtomicBool::new(false);
    static HANDLER_INSTALLED: Once = Once::new();

    extern "C" fn tstp_handler(signum: libc::c_int) {
        // Restore cooked termios before suspension.
        let _ = super::tty_raw::raw_mode_leave();
        unsafe {
            libc::signal(signum, libc::SIG_DFL);
            let mut mask: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&mut mask);
            libc::sigaddset(&mut mask, signum);
            libc::sigprocmask(libc::SIG_UNBLOCK, &mask, std::ptr::null_mut());
            libc::raise(signum);
            // Re-install in case `signal` reset the disposition.
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = tstp_handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            sa.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGTSTP, &sa, std::ptr::null_mut());
        }
    }

    extern "C" fn cont_handler(_signum: libc::c_int) {
        SIGCONT_ARRIVED.store(true, Ordering::SeqCst);
    }

    pub fn install_handlers() {
        HANDLER_INSTALLED.call_once(|| unsafe {
            let mut sa_tstp: libc::sigaction = std::mem::zeroed();
            sa_tstp.sa_sigaction = tstp_handler as *const () as usize;
            libc::sigemptyset(&mut sa_tstp.sa_mask);
            sa_tstp.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGTSTP, &sa_tstp, std::ptr::null_mut());

            let mut sa_cont: libc::sigaction = std::mem::zeroed();
            sa_cont.sa_sigaction = cont_handler as *const () as usize;
            libc::sigemptyset(&mut sa_cont.sa_mask);
            sa_cont.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGCONT, &sa_cont, std::ptr::null_mut());
        });
    }

    pub fn handlers_installed_p() -> bool {
        HANDLER_INSTALLED.is_completed()
    }

    pub fn take_cont() -> bool {
        SIGCONT_ARRIVED.swap(false, Ordering::SeqCst)
    }
}

fn bi_terminal_raw_mode_enter(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("terminal-raw-mode-enter", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        tty_raw::raw_mode_enter()?;
        return Ok(Sexp::T);
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

fn bi_terminal_raw_mode_leave(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("terminal-raw-mode-leave", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        tty_raw::raw_mode_leave()?;
        return Ok(Sexp::T);
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(read-stdin-byte-available &optional TIMEOUT-MS)' — return one byte or nil.
fn bi_read_stdin_byte_available(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("read-stdin-byte-available", args, 0, Some(1))?;
    let timeout_ms = match args.get(0) {
        None | Some(Sexp::Nil) => 0,
        Some(Sexp::Int(n)) => *n as i32,
        Some(other) => {
            return Err(EvalError::WrongType {
                expected: "integer (timeout-ms)".into(),
                got: other.clone(),
            });
        }
    };
    #[cfg(unix)]
    {
        Ok(tty_raw::stdin_byte_available(timeout_ms)?.map_or(Sexp::Nil, |b| Sexp::Int(b as i64)))
    }
    #[cfg(not(unix))]
    {
        let _ = timeout_ms;
        Ok(Sexp::Nil)
    }
}


fn read_from_string_impl(env: &mut Env, caller: &'static str) -> Result<Sexp, EvalError> {
    env.lookup_function("nelisp--read-from-string-impl").map_err(|_| EvalError::Internal(format!(
        "{caller}: `nelisp--read-from-string-impl' not loaded — `lisp/nelisp-stdlib-reader.el' missing from STDLIB_SOURCES?"
    )))
}

fn bi_read(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("read", args, 0, Some(1))?;
    match args.get(0) {
        Some(Sexp::Str(_)) | Some(Sexp::MutStr(_)) => match super::apply_function(&read_from_string_impl(env, "read")?, &args[0..1], env)? {
            Sexp::Cons(b) => Ok(b.car.clone()),
            other => Err(EvalError::Internal(format!("read: expected `(FORM . CONSUMED-END)' from impl, got {:?}", other))),
        },
        Some(other) => Err(EvalError::NotImplemented(format!("read STREAM type: {:?}", other))),
        None => Err(EvalError::NotImplemented("read from stdin (= no STREAM arg) is deferred".into())),
    }
}

fn bi_read_from_string(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("read-from-string", args, 1, Some(3))?;
    super::apply_function(&read_from_string_impl(env, "read-from-string")?, args, env)
}

fn bi_require(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("require", args, 1, Some(3))?;
    let feature = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType { expected: "symbolp (require feature)".into(), got: other.clone() }),
    };
    if elisp_featurep(env, &feature)? {
        return Ok(Sexp::Symbol(feature));
    }
    let filename = match args.get(1) {
        Some(Sexp::Str(s)) => Some(s.clone()),
        _ => None,
    };
    let noerror = args.get(2).map(is_truthy).unwrap_or(false);
    if env.lookup_value("load-path").is_err() && filename.is_none() {
        elisp_provide(env, &feature)?;
        return Ok(Sexp::Symbol(feature));
    }
    if let Err(e) = super::apply_function(
        &env.lookup_function("load")?,
        &[Sexp::Str(filename.unwrap_or_else(|| feature.clone())), bool_sexp(noerror)],
        env,
    ) {
        if noerror {
            return Ok(Sexp::Nil);
        }
        return Err(e);
    }
    if elisp_featurep(env, &feature)? {
        Ok(Sexp::Symbol(feature))
    } else if noerror {
        Ok(Sexp::Nil)
    } else {
        Err(EvalError::UserError {
            tag: "error".into(),
            data: Sexp::list_from(&[Sexp::Str(format!(
                "Required feature `{}' was not provided",
                feature
            ))]),
        })
    }
}

fn elisp_featurep(env: &mut Env, feature: &str) -> Result<bool, EvalError> {
    let fn_cell = env.lookup_function("featurep")?;
    let result = super::apply_function(&fn_cell, &[Sexp::Symbol(feature.to_string())], env)?;
    Ok(is_truthy(&result))
}

fn elisp_provide(env: &mut Env, feature: &str) -> Result<(), EvalError> {
    let fn_cell = env.lookup_function("provide")?;
    super::apply_function(&fn_cell, &[Sexp::Symbol(feature.to_string())], env)?;
    Ok(())
}

fn bi_make_vector(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("make-vector", args, 2, Some(2))?;
    let len = as_int("make-vector", &args[0])?;
    if len < 0 {
        return Err(EvalError::ArithError(format!("make-vector: negative length {}", len)));
    }
    let mut result_slot: Sexp = Sexp::Nil;
    unsafe { crate::elisp_cc_spike::bi_make_vector(&args[0] as *const Sexp, &args[1] as *const Sexp, &mut result_slot as *mut Sexp) };
    Ok(result_slot)
}
