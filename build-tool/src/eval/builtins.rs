//! Built-in function registry and dispatcher for the Rust eval surface.

use super::Env;
use super::error::EvalError;
use super::quit;
use super::sexp::Sexp;
use super::special_forms::is_truthy;
use std::path::{Path, PathBuf};

/// Install every built-in into the given environment.  Idempotent.
pub fn install_builtins(env: &mut Env) {
    let names: &[&str] = &[
        // JIT-lowered arithmetic primitives.
        "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
        "nelisp--num-lt2", "nelisp--num-gt2",
        "nelisp--num-le2", "nelisp--num-ge2",
        "nelisp--num-eq2",
        // JIT-lowered equality.
        "eq",
        // Cons/list/vector helpers.
        "car", "cdr", "cons", "length", "nelisp--length-cons-cc", "nelisp--recordp-cc", "string-bytes",
        "setcar", "setcdr",
        "aref", "aset", "elt",
        "vector", "make-vector",
        // Bitwise helpers.
        "ash",
        "nelisp--logior2", "nelisp--logand2", "nelisp--logxor2",
        // String/format helpers.
        "truncate",
        "nl-jit-call-format-float",
        "string-match-p",
        // Filesystem helpers.
        "nelisp--syscall-canonicalize",
        "nelisp--syscall-stat",
        "nelisp--syscall-readdir",
        "nelisp--syscall-read-file",
        "nelisp--read-all-from-string",
        // Generic POSIX syscall helpers.
        "nelisp--syscall",
        "nelisp--syscall-supported-p",
        // Socket primitives.
        "nelisp--syscall-socketpair",
        "nelisp--syscall-sendmsg-fds",
        "nelisp--syscall-recvmsg-fds",
        "nelisp--syscall-getsockopt-peercred",
        "nelisp--syscall-bind-inet6-scoped",
        "nelisp--syscall-connect-inet6-scoped",
        "nelisp--syscall-accept-inet6-scoped",
        // Symbol/function cell ops and dispatch core.
        "symbol-function", "funcall", "apply", "eval",
        "fset",
        // Print/error helpers.
        "signal",
        "nelisp--write-stderr-line",
        "nelisp--write-stdout-bytes",
        // Load orchestration.
        "require",
        // Self-process stdio.
        "read-stdin-bytes",
        // Generic FFI primitives.
        "nl-ffi-call",
        "nl-ffi-malloc", "nl-ffi-read-bytes", "nl-ffi-free",
        "nl-ffi-write-bytes", "nl-ffi-errno",
        "nl-ffi-read-i32", "nl-ffi-read-i64",
        "nl-ffi-read-i16", "nl-ffi-read-u16", "nl-ffi-read-u32",
        "nl-ffi-write-i16", "nl-ffi-write-i32",
        "nl-ffi-read-u8", "nl-ffi-write-bytes-at", "nl-ffi-read-bytes-at",
        "nl-ffi-write-i64",
        // Native-only primitives.
        "nl-current-unix-time", "nl-secure-hash", "nl-format-unix-time",
        "nl-downcase", "nl-upcase", "nl-split-by-non-alnum",
        "float", "exp", "log", "nelisp--f64-trunc",
        "nl-write-file", "nl-make-directory",
        // Interactive TTY plumbing; `_`-prefixed names are test helpers.
        "terminal-raw-mode-enter", "terminal-raw-mode-leave",
        "read-stdin-byte-available",
        "_termios-saved-p", "_raw-mode-hooks-installed-p",
        "set-quit-flag", "clear-quit-flag", "quit-flag-pending-p",
        "install-sigint-handler", "_sigint-handler-installed-p",
        "install-winsize-handler", "_winsize-handler-installed-p",
        "terminal-take-winsize-changed", "terminal-current-winsize",
        "install-jobctrl-handlers", "_jobctrl-handlers-installed-p",
        "terminal-take-sigcont",
        // Reader entry points.
        "read", "read-from-string",
        // Apply/call/closure/env primitives.
        "nelisp--push-frame", "nelisp--pop-frame", "nelisp--push-captured",
        "nelisp--bind-local", "nelisp--apply-builtin-dispatch",
        "nelisp--set-use-elisp-apply", "nelisp--get-use-elisp-apply",
        "nelisp--apply-lambda-inner",
        // JIT bridge primitives.
        "nl-jit-call-i64-i64",
        "nl-jit-call-ptr-ptr",
        "nl-jit-call-syscall",
        "nl-jit-call-out-1",
        "nl-jit-call-out-2",
        "nl-jit-call-out-1i",
        "nl-jit-call-out-2i",
        "nl-jit-call-float-float",
        "nl-jit-call-float-cmp",
        "nl-jit-call-float-unary",
        // Layer 2 cons/rc/gc primitives.
        "nl-cons-alloc", "nl-cons-car", "nl-cons-cdr",
        "nl-cons-set-car", "nl-cons-set-cdr",
        "nl-rc-inc", "nl-rc-dec", "nl-rc-count",
        "nl-rc-alloc", "nl-rc-dealloc",
        "nl-rc-inc-strong", "nl-rc-dec-strong", "nl-rc-strong-count",
        "nl-rc-kind", "nl-rc-payload-ptr",
        "nl-gc-walk-children", "nl-gc-buffered-decs", "nl-gc-finalize",
        // dlsym bridge.
        "nelisp-cc--dlsym-resolve",
        "nl-fact-i64",
    ];
    for n in names {
        let sentinel = Sexp::list_from(&[
            Sexp::Symbol("builtin".into()),
            Sexp::Symbol((*n).into()),
        ]);
        env.set_function(n, sentinel);
    }
}

/// Dispatch a built-in by name.  Called from `super::apply_builtin'.
/// See module docstring for the surviving surface categories.
pub fn dispatch(name: &str, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    match name {
        // ---- vector core ----
        "vector" => Ok(Sexp::vector(args.to_vec())),
        "make-vector" => bi_make_vector(args),
        "nelisp--length-cons-cc" => bi_length_cons_cc(args),
        "nelisp--recordp-cc" => bi_recordp_cc(args),
        "string-bytes" => bi_string_bytes(args),
        // ---- string/format helpers ----
        "nl-jit-call-format-float" => crate::jit::bi_nl_jit_call_format_float(args),
        "truncate" => bi_truncate(args),
        // ---- filesystem syscalls ----
        "nelisp--syscall-canonicalize" => bi_syscall_canonicalize(args, env),
        "nelisp--syscall-stat" => bi_syscall_stat(args, env),
        "nelisp--syscall-readdir" => bi_syscall_readdir(args, env),
        "nelisp--syscall-read-file" => bi_syscall_read_file(args, env),
        "nelisp--read-all-from-string" => bi_read_all_from_string(args, env),
        // ---- POSIX syscall ----
        "nelisp--syscall" => bi_syscall(args),
        "nelisp--syscall-supported-p" => bi_syscall_supported_p(args),
        // ---- symbol / function ----
        "symbol-function" => bi_symbol_function(args, env),
        "fset" => bi_fset(args, env),
        // ---- apply/closure/env primitives ----
        "nelisp--push-frame" => bi_frame_op("push-frame", args, env),
        "nelisp--pop-frame" => bi_frame_op("pop-frame", args, env),
        "nelisp--push-captured" => bi_frame_op("push-captured", args, env),
        "nelisp--bind-local" => bi_frame_op("bind-local", args, env),
        "nelisp--apply-builtin-dispatch" => bi_apply_builtin_dispatch(args, env),
        "nelisp--set-use-elisp-apply" => {
            require_arity("nelisp--set-use-elisp-apply", args, 1, Some(1))?;
            let truthy = !matches!(args[0], Sexp::Nil);
            env.use_elisp_apply = truthy;
            Ok(if truthy { Sexp::T } else { Sexp::Nil })
        }
        "nelisp--get-use-elisp-apply" => {
            require_arity("nelisp--get-use-elisp-apply", args, 0, Some(0))?;
            Ok(if env.use_elisp_apply { Sexp::T } else { Sexp::Nil })
        }
        "nelisp--apply-lambda-inner" => bi_apply_lambda_inner(args, env),
        // ---- core dispatch + signal ----
        "funcall" => bi_funcall(args, env),
        "apply" => bi_apply(args, env),
        "eval" => bi_eval(args, env),
        "signal" => bi_signal(args),
        // ---- print/error helpers ----
        "nelisp--write-stdout-bytes" => bi_write_stdout_bytes(args),
        "nelisp--write-stderr-line" => bi_write_stderr_line(args),
        // ---- self-process stdio ----
        "read-stdin-bytes" => bi_read_stdin_bytes(args),
        // ---- dlsym bridge ----
        "nelisp-cc--dlsym-resolve" => super::dlsym_bridge::bi_dlsym_resolve(args),
        // ---- FFI primitives (libffi-backed) ----
        "nl-ffi-call" => super::ffi::nl_ffi_call(args),
        "nl-ffi-malloc" => super::ffi::nl_ffi_malloc(args),
        "nl-ffi-read-bytes" => super::ffi::nl_ffi_read_bytes(args),
        "nl-ffi-free" => super::ffi::nl_ffi_free(args),
        "nl-ffi-write-bytes" => super::ffi::nl_ffi_write_bytes(args),
        "nl-ffi-errno" => super::ffi::nl_ffi_errno(args),
        "nl-ffi-read-i32" => super::ffi::nl_ffi_read_i32(args),
        "nl-ffi-read-i64" => super::ffi::nl_ffi_read_i64(args),
        "nl-ffi-read-i16" => super::ffi::nl_ffi_read_i16(args),
        "nl-ffi-read-u16" => super::ffi::nl_ffi_read_u16(args),
        "nl-ffi-read-u32" => super::ffi::nl_ffi_read_u32(args),
        "nl-ffi-write-i16" => super::ffi::nl_ffi_write_i16(args),
        "nl-ffi-write-i32" => super::ffi::nl_ffi_write_i32(args),
        "nl-ffi-read-u8" => super::ffi::nl_ffi_read_u8(args),
        "nl-ffi-write-bytes-at" => super::ffi::nl_ffi_write_bytes_at(args),
        "nl-ffi-read-bytes-at" => super::ffi::nl_ffi_read_bytes_at(args),
        "nl-ffi-write-i64" => super::ffi::nl_ffi_write_i64(args),
        // ---- math kernel sliver (= shared f64 div + truncate-mode
        // for elisp floor/ceiling/round/truncate) ----
        "nelisp--f64-trunc" => bi_f64_trunc(args),
        // ---- file write + mkdir ----
        "nl-write-file" => bi_nl_write_file(args),
        "nl-make-directory" => bi_nl_make_directory(args),
        // ---- TTY plumbing (Unix only) ----
        "terminal-raw-mode-enter" => bi_terminal_raw_mode_enter(args),
        "terminal-raw-mode-leave" => bi_terminal_raw_mode_leave(args),
        "read-stdin-byte-available" => bi_read_stdin_byte_available(args),
        "_termios-saved-p" => bi_termios_saved_p(args),
        "_raw-mode-hooks-installed-p" => bi_raw_mode_hooks_installed_p(args),
        "set-quit-flag" => bi_set_quit_flag(args),
        "clear-quit-flag" => bi_clear_quit_flag(args),
        "quit-flag-pending-p" => bi_quit_flag_pending_p(args),
        "install-sigint-handler" => bi_install_sigint_handler(args),
        "_sigint-handler-installed-p" => bi_sigint_handler_installed_p(args),
        "install-winsize-handler" => bi_install_winsize_handler(args),
        "_winsize-handler-installed-p" => bi_winsize_handler_installed_p(args),
        "terminal-take-winsize-changed" => bi_terminal_take_winsize_changed(args),
        "terminal-current-winsize" => bi_terminal_current_winsize(args),
        "install-jobctrl-handlers" => bi_install_jobctrl_handlers(args),
        "_jobctrl-handlers-installed-p" => bi_jobctrl_handlers_installed_p(args),
        "terminal-take-sigcont" => bi_terminal_take_sigcont(args),
        // ---- reader ----
        "read" => bi_read(args, env),
        "read-from-string" => bi_read_from_string(args, env),
        // ---- require ----
        "require" => bi_require(args, env),
        // ---- nl-jit-call-* bridge primitives ----
        "nl-jit-call-i64-i64" => crate::jit::bi_nl_jit_call_i64_i64(args),
        "nl-jit-call-ptr-ptr" => crate::jit::bi_nl_jit_call_ptr_ptr(args),
        "nl-jit-call-syscall" => crate::jit::bi_nl_jit_call_syscall(args),
        "nl-jit-call-out-1" => crate::jit::bi_nl_jit_call_out_1(args),
        "nl-jit-call-out-2" => crate::jit::bi_nl_jit_call_out_2(args),
        "nl-jit-call-out-1i" => crate::jit::bi_nl_jit_call_out_1i(args),
        "nl-jit-call-out-2i" => crate::jit::bi_nl_jit_call_out_2i(args),
        "nl-jit-call-float-float" => crate::jit::bi_nl_jit_call_float_float(args),
        "nl-jit-call-float-cmp" => crate::jit::bi_nl_jit_call_float_cmp(args),
        "nl-jit-call-float-unary" => crate::jit::bi_nl_jit_call_float_unary(args),
        // ---- Layer 2 cons / rc / gc primitives ----
        "nl-cons-alloc" => crate::eval::cons_primitives::bi_nl_cons_alloc(args),
        "nl-cons-car" => crate::eval::cons_primitives::bi_nl_cons_car(args),
        "nl-cons-cdr" => crate::eval::cons_primitives::bi_nl_cons_cdr(args),
        "nl-cons-set-car" => crate::eval::cons_primitives::bi_nl_cons_set_car(args),
        "nl-cons-set-cdr" => crate::eval::cons_primitives::bi_nl_cons_set_cdr(args),
        "nl-rc-inc" => crate::eval::cons_primitives::bi_nl_rc_inc(args),
        "nl-rc-dec" => crate::eval::cons_primitives::bi_nl_rc_dec(args),
        "nl-rc-count" => crate::eval::cons_primitives::bi_nl_rc_count(args),
        "nl-rc-alloc" => crate::eval::rc_primitives::bi_nl_rc_alloc(args),
        "nl-rc-dealloc" => crate::eval::rc_primitives::bi_nl_rc_dealloc(args),
        "nl-rc-inc-strong" => crate::eval::rc_primitives::bi_nl_rc_inc_strong(args),
        "nl-rc-dec-strong" => crate::eval::rc_primitives::bi_nl_rc_dec_strong(args),
        "nl-rc-strong-count" => crate::eval::rc_primitives::bi_nl_rc_strong_count(args),
        "nl-rc-kind" => crate::eval::rc_primitives::bi_nl_rc_kind(args),
        "nl-rc-payload-ptr" => crate::eval::rc_primitives::bi_nl_rc_payload_ptr(args),
        "nl-gc-walk-children" => crate::eval::rc_primitives::bi_nl_gc_walk_children(args),
        "nl-gc-buffered-decs" => crate::eval::rc_primitives::bi_nl_gc_buffered_decs(args),
        "nl-gc-finalize" => crate::eval::rc_primitives::bi_nl_gc_finalize(args),
        "nl-fact-i64" => bi_nl_fact_i64(args),
        "nelisp--env-globals-op" => crate::eval::env_shim::bi_globals_op(args, env),
        _ => {
            // Externally-registered builtin (test-only in spirit).  Doc 130
            // (2026-05-18) — ungated for the integration test binary.
            // Production never inserts into `extern_builtins'; the lookup is
            // an empty-HashMap probe on the rare unbound-function path.
            // Clone the Rc first so the `extern_builtins' borrow drops
            // before we re-borrow `env' through the closure.
            if let Some(f) = env.extern_builtins.get(name).cloned() {
                return f(args, env);
            }
            Err(EvalError::UnboundFunction(name.to_string()))
        }
    }
}

// ---------- arity helpers ----------

pub(crate) fn require_arity(
    name: &str,
    args: &[Sexp],
    min: usize,
    max: Option<usize>,
) -> Result<(), EvalError> {
    if args.len() < min || max.map_or(false, |m| args.len() > m) {
        let expected = match max {
            Some(m) if m == min => format!("{}", min),
            Some(m) => format!("{}-{}", min, m),
            None => format!("≥{}", min),
        };
        return Err(EvalError::WrongNumberOfArguments {
            function: name.into(),
            expected,
            got: args.len(),
        });
    }
    Ok(())
}

pub(crate) fn as_int(name: &str, v: &Sexp) -> Result<i64, EvalError> {
    match v {
        Sexp::Int(n) => Ok(*n),
        Sexp::Float(x) => Ok(*x as i64),
        other => Err(EvalError::WrongType {
            expected: format!("number ({} arg)", name),
            got: other.clone(),
        }),
    }
}

// ---------- arithmetic helpers ----------

pub(crate) fn num_pair(args: &[Sexp], name: &str) -> Result<(f64, f64, bool), EvalError> {
    require_arity(name, args, 2, Some(2))?;
    let af = matches!(args[0], Sexp::Float(_)) || matches!(args[1], Sexp::Float(_));
    let to_f64 = |v: &Sexp| -> Result<f64, EvalError> {
        match v {
            Sexp::Int(n) => Ok(*n as f64),
            Sexp::Float(x) => Ok(*x),
            other => Err(EvalError::WrongType {
                expected: "numberp".into(),
                got: other.clone(),
            }),
        }
    };
    Ok((to_f64(&args[0])?, to_f64(&args[1])?, af))
}

/// `(string-bytes STRING)' — UTF-8 byte count.
fn bi_string_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-bytes", args, 1, Some(1))?;
    // Present string variants as a uniform `Sexp::Str`.
    let str_view: Sexp = match &args[0] {
        Sexp::Str(_) => args[0].clone(),
        Sexp::MutStr(rc) => Sexp::Str(rc.value.clone()),
        other => {
            return Err(EvalError::WrongType {
                expected: "string".into(),
                got: other.clone(),
            })
        }
    };
    let mut result_slot: Sexp = Sexp::Nil;
    unsafe {
        crate::elisp_cc_spike::bi_string_bytes(
            &str_view as *const Sexp,
            &mut result_slot as *mut Sexp,
        );
    }
    Ok(result_slot)
}

// ---------- higher-order ----------

fn list_to_vec(v: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur: Sexp = v.clone();
    loop {
        let next = match &cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(b) => {
                out.push(b.car.clone());
                b.cdr.clone()
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "listp".into(),
                    got: other.clone(),
                })
            }
        };
        cur = next;
    }
}

// ---- char-table legacy decode helpers ----
// User-facing builtins retired; helpers stay for image-format decode.

pub(crate) fn char_table_set_one(
    inner: &mut crate::eval::sexp::CharTableInner,
    c: i64,
    v: Sexp,
) {
    for entry in inner.entries.iter_mut() {
        if entry.0 == c {
            entry.1 = v;
            return;
        }
    }
    inner.entries.push((c, v));
}

pub(crate) fn char_table_get(
    rc: &crate::eval::nlchartable::NlCharTableRef,
    c: i64,
) -> Sexp {
    let inner = &rc.inner;
    for (k, v) in inner.entries.iter() {
        if *k == c {
            return v.clone();
        }
    }
    if let Some(parent) = &inner.parent {
        return char_table_get(parent, c);
    }
    inner.default_val.clone()
}

fn string_value(v: &Sexp) -> Result<String, EvalError> {
    match v {
        Sexp::Str(s) => Ok(s.clone()),
        Sexp::MutStr(rc) => Ok(rc.value.clone()),
        Sexp::Symbol(s) => Ok(s.clone()),
        Sexp::Nil => Ok("nil".into()),
        Sexp::T => Ok("t".into()),
        other => Err(EvalError::WrongType {
            expected: "stringp or symbolp".into(),
            got: other.clone(),
        }),
    }
}

fn normalize_path(path: &str, base: Option<&str>) -> PathBuf {
    let p = Path::new(path);
    if p.is_absolute() {
        p.to_path_buf()
    } else if let Some(base) = base {
        Path::new(base).join(p)
    } else if let Ok(cwd) = std::env::current_dir() {
        cwd.join(p)
    } else {
        p.to_path_buf()
    }
}

fn env_default_directory(env: &Env) -> Option<String> {
    match env.lookup_value("default-directory") {
        Ok(Sexp::Str(s)) => Some(s),
        _ => None,
    }
}

/// `(nelisp--syscall-canonicalize PATH)' — libc `realpath' via elisp kernel.
fn bi_syscall_canonicalize(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-canonicalize", args, 1, Some(1))?;
    let path_sexp = resolve_path_sexp(&args[0], env)?;
    let mut buf = vec![0u8; libc::PATH_MAX as usize];
    let rc = unsafe {
        crate::elisp_cc_spike::bi_syscall_canonicalize(&path_sexp as *const Sexp, buf.as_mut_ptr())
    };
    if rc == 0 { return Ok(Sexp::Nil); }
    let len = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    Ok(Sexp::Str(String::from_utf8_lossy(&buf[..len]).into_owned()))
}

/// `(nelisp--syscall-read-file PATH)' — chained open/read/close via elisp kernel.
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
    let rc = (unsafe {
        crate::elisp_cc_spike::bi_syscall_read_file(
            &path_sexp as *const Sexp, buf.as_mut_ptr(), n_bytes as i64,
        )
    }) as i32 as i64;
    if rc < 0 { return Ok(Sexp::Nil); }
    buf.truncate((rc as usize).min(n_bytes));
    Ok(Sexp::Str(String::from_utf8_lossy(&buf).into_owned()))
}

/// `(nelisp--read-all-from-string STR)' — delegate to the elisp reader.
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

// ---------- file I/O helpers ----------

fn resolve_existing_path(arg: &Sexp, env: &Env) -> Result<PathBuf, EvalError> {
    let path = string_value(arg)?;
    let base = env_default_directory(env);
    Ok(normalize_path(&path, base.as_deref()))
}

/// `resolve_existing_path' + `Sexp::Str' wrap for syscall extern-call shims.
fn resolve_path_sexp(arg: &Sexp, env: &Env) -> Result<Sexp, EvalError> {
    let p = resolve_existing_path(arg, env)?;
    Ok(Sexp::Str(p.to_string_lossy().into_owned()))
}

/// `(nelisp--syscall-stat PATH)' — `'absent / 'file / 'directory` via libc stat(2).
fn bi_syscall_stat(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-stat", args, 1, Some(1))?;
    let path_sexp = resolve_path_sexp(&args[0], env)?;
    let mut statbuf: libc::stat = unsafe { std::mem::zeroed() };
    let rc = unsafe {
        crate::elisp_cc_spike::bi_syscall_stat(
            &path_sexp as *const Sexp, (&mut statbuf as *mut libc::stat) as *mut u8,
        )
    };
    let tag = if rc < 0 { "absent" } else {
        match statbuf.st_mode & libc::S_IFMT {
            m if m == libc::S_IFDIR => "directory",
            m if m == libc::S_IFREG => "file",
            _ => "absent",
        }
    };
    Ok(Sexp::Symbol(tag.into()))
}

/// `(nelisp--syscall-readdir DIR)' — return `(ABS-DIR NAME1 NAME2 ...)` or nil.
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

// ---- POSIX syscall surface ----

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
            // Int-only-arg syscalls — buffer-using syscalls live in
            // separate primitives or use nl-ffi.
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

// ---------- symbol / function ----------

fn bi_symbol_function(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    // Keep the builtin name in wrong-arity errors.
    require_arity("symbol-function", args, 1, Some(1))?;
    let op_args = [Sexp::Symbol("get-function".into()), args[0].clone()];
    super::env_shim::bi_globals_op(&op_args, env)
}

fn feature_name_arg(name: &str, arg: &Sexp) -> Result<String, EvalError> {
    match arg {
        Sexp::Symbol(s) => Ok(s.clone()),
        other => Err(EvalError::WrongType {
            expected: format!("symbolp ({} feature)", name),
            got: other.clone(),
        }),
    }
}

/// `(fset SYMBOL DEFINITION)` — install DEFINITION in SYMBOL's function cell.
fn bi_fset(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    // Keep the builtin name in wrong-arity errors.
    require_arity("fset", args, 2, Some(2))?;
    let def = match &args[1] {
        Sexp::Symbol(s) => env.lookup_function(s)?,
        other => other.clone(),
    };
    let op_args = [
        Sexp::Symbol("set-function".into()),
        args[0].clone(),
        def,
    ];
    super::env_shim::bi_globals_op(&op_args, env)
}

fn bi_frame_op(op: &str, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    match op {
        "push-frame" => {
            require_arity("nelisp--push-frame", args, 0, Some(0))?;
            env.push_frame();
            Ok(Sexp::T)
        }
        "pop-frame" => {
            require_arity("nelisp--pop-frame", args, 0, Some(0))?;
            env.pop_frame();
            Ok(Sexp::T)
        }
        "push-captured" => {
            require_arity("nelisp--push-captured", args, 1, Some(1))?;
            env.push_captured(&args[0])?;
            Ok(Sexp::T)
        }
        "bind-local" => {
            require_arity("nelisp--bind-local", args, 2, Some(2))?;
            let name = match &args[0] {
                Sexp::Symbol(s) => s.clone(),
                other => return Err(EvalError::WrongType {
                    expected: "symbol".into(),
                    got: other.clone(),
                }),
            };
            env.bind_local(&name, args[1].clone());
            Ok(args[1].clone())
        }
        _ => unreachable!("op verified at dispatch arm"),
    }
}

/// `(nelisp--apply-builtin-dispatch NAME ARGS)' — direct dispatch by name.
fn bi_apply_builtin_dispatch(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--apply-builtin-dispatch", args, 2, Some(2))?;
    let name = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        Sexp::Str(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    };
    let arg_vec = super::list_elements(&args[1])?;
    dispatch(&name, &arg_vec, env)
}

/// `(nelisp--apply-lambda-inner CAPTURED FORMALS BODY ARGS)' — apply a lambda.
fn bi_apply_lambda_inner(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--apply-lambda-inner", args, 4, Some(4))?;
    let captured = &args[0];
    let formals = &args[1];
    let body_vec = super::list_elements(&args[2])?;
    let args_vec = super::list_elements(&args[3])?;
    super::apply_lambda_inner(captured, formals, &body_vec, &args_vec, env)
}

/// Resolve `arg' to a callable: a symbol points to its function cell,
/// a quoted lambda `(lambda ...)` / `(closure ...)` / `(builtin ...)`
/// is returned as-is.
fn resolve_callable(arg: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match arg {
        Sexp::Symbol(s) => env.lookup_function(s),
        _ => Ok(arg.clone()),
    }
}

fn bi_funcall(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("funcall", args, 1, None)?;
    let func = resolve_callable(&args[0], env)?;
    super::apply_function(&func, &args[1..], env)
}

fn bi_apply(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("apply", args, 2, None)?;
    let func = resolve_callable(&args[0], env)?;
    // The last arg must be a list; preceding args are spread.
    let mut all_args: Vec<Sexp> = args[1..args.len() - 1].to_vec();
    let tail = list_to_vec(&args[args.len() - 1])?;
    all_args.extend(tail);
    super::apply_function(&func, &all_args, env)
}

fn bi_eval(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("eval", args, 1, Some(2))?;
    super::eval(&args[0], env)
}

fn bi_signal(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("signal", args, 2, Some(2))?;
    let tag = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "symbolp".into(),
                got: other.clone(),
            })
        }
    };
    // `quit` must bypass ordinary `error` handlers.
    if tag == "quit" {
        return Err(EvalError::Quit);
    }
    // Canonicalize common tags into structured `EvalError` variants.
    if tag == "arith-error" {
        let msg = match &args[1] {
            Sexp::Cons(b) => match &b.car {
                Sexp::Str(s) => s.clone(),
                other => format!("{:?}", other),
            },
            Sexp::Str(s) => s.clone(),
            _ => "arith-error".to_string(),
        };
        return Err(EvalError::ArithError(msg));
    }
    if tag == "wrong-type-argument" {
        let (expected, got) = match &args[1] {
            Sexp::Cons(b) => {
                let exp = match &b.car {
                    Sexp::Symbol(s) => s.clone(),
                    Sexp::Str(s) => s.clone(),
                    other => format!("{:?}", other),
                };
                let got = match &b.cdr {
                    Sexp::Cons(c) => c.car.clone(),
                    other => other.clone(),
                };
                (exp, got)
            }
            other => ("argument".to_string(), other.clone()),
        };
        return Err(EvalError::WrongType { expected, got });
    }
    // Per Elisp, the second arg is the *data list*.
    Err(EvalError::UserError {
        tag,
        data: args[1].clone(),
    })
}

/// `(nelisp--write-stdout-bytes STR)' — write STR to stdout and flush.
fn bi_write_stdout_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stdout-bytes", args, 1, Some(1))?;
    // Present string variants as a uniform `Sexp::Str`.
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

/// `(nelisp--write-stderr-line STR)' — write STR, newline, and flush.
fn bi_write_stderr_line(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stderr-line", args, 1, Some(1))?;
    // Present string variants as a uniform `Sexp::Str`.
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

/// `(truncate X)' — return X truncated toward zero as an integer.
fn bi_truncate(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("truncate", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(_) => {
            // The callee writes a new `Sexp::Int` into `result_slot`.
            let mut result_slot: Sexp = Sexp::Nil;
            unsafe {
                crate::elisp_cc_spike::truncate_int(
                    &args[0] as *const Sexp,
                    &mut result_slot as *mut Sexp,
                );
            }
            Ok(result_slot)
        }
        Sexp::Float(x) => Ok(Sexp::Int(*x as i64)),
        other => Err(EvalError::WrongType {
            expected: "numberp".into(),
            got: other.clone(),
        }),
    }
}

/// Internal `(length X)' cons/nil bridge.
fn bi_length_cons_cc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--length-cons-cc", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(_) | Sexp::Nil => {
            let mut result_slot: Sexp = Sexp::Nil;
            unsafe {
                crate::elisp_cc_spike::length_cons(
                    &args[0] as *const Sexp,
                    &mut result_slot as *mut Sexp,
                );
            }
            Ok(result_slot)
        }
        other => Err(EvalError::WrongType {
            expected: "sequencep".into(),
            got: other.clone(),
        }),
    }
}

/// Internal `(recordp X)' bridge used by the stdlib `recordp' wrapper.
fn bi_recordp_cc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--recordp-cc", args, 1, Some(1))?;
    let mut result_slot: Sexp = Sexp::Nil;
    unsafe {
        crate::elisp_cc_spike::recordp(
            &args[0] as *const Sexp,
            &mut result_slot as *mut Sexp,
        );
    }
    Ok(result_slot)
}

/// `(nl-fact-i64 N)' — compute `N!` for `0 <= N <= 20`.
fn bi_nl_fact_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-fact-i64", args, 1, Some(1))?;
    let n = as_int("nl-fact-i64", &args[0])?;
    if !(0..=20).contains(&n) {
        return Err(EvalError::Internal(format!(
            "nl-fact-i64: argument {} out of i64-safe range 0..=20",
            n
        )));
    }
    let result = crate::elisp_cc_spike::fact_i64(n);
    Ok(Sexp::Int(result))
}

/// (read-stdin-bytes LIMIT) — block-read up to LIMIT bytes from fd 0.
///
/// Returns:
/// - `Sexp::Str` of 1..=LIMIT bytes when data is available.
/// - `Sexp::Nil` on EOF (peer closed stdin).
///
/// LIMIT must be a positive integer; otherwise signals `wrong-type-argument`.
/// I/O errors propagate as `EvalError::Internal` (= `error' tag at the
/// `condition-case' boundary).
///
/// Bytes are stored as a UTF-8 `String' (the Sexp string variant).  LSP
/// wire bytes are UTF-8 by spec so this is lossless for that consumer.
/// Pathological stdin containing non-UTF-8 bytes passes through
/// `from_utf8_lossy`, substituting U+FFFD; strict binary stdio is left
/// to a later dedicated primitive.
/// Coerce arg to f64 for math ops.  Accepts int / float / nil (= 0.0).
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
        Sexp::Symbol(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "symbol".into(),
                got: other.clone(),
            });
        }
    };
    let x = to_f64(&args[1])?;
    let div = to_f64(&args[2])?;
    let q = x / div;
    let r = match mode.as_str() {
        "floor" => q.floor(),
        "ceiling" => q.ceil(),
        "round" => q.round(),
        "truncate" => q.trunc(),
        _ => {
            return Err(EvalError::Internal(format!(
                "nelisp--f64-trunc: unknown mode `{}'",
                mode
            )));
        }
    };
    Ok(Sexp::Int(r as i64))
}

// `string_value` validates stringp; negative rc becomes `Internal`.
fn bi_nl_write_file(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-write-file", args, 2, Some(2))?;
    let path = string_value(&args[0])?;
    string_value(&args[1])?;
    let rc = unsafe {
        crate::elisp_cc_spike::bi_nl_write_file(&args[0] as *const Sexp, &args[1] as *const Sexp)
    };
    if rc < 0 {
        return Err(EvalError::Internal(format!("nl-write-file: {}: kernel returned {}", path, rc)));
    }
    Ok(Sexp::T)
}

// Sign-extend the 32-bit libc rc so `-1` stays negative.
fn bi_nl_make_directory(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-make-directory", args, 1, Some(2))?;
    let path = string_value(&args[0])?;
    let rc = (unsafe { crate::elisp_cc_spike::bi_nl_make_directory(&args[0] as *const Sexp) })
        as i32 as i64;
    if rc < 0 {
        return Err(EvalError::Internal(format!(
            "nl-make-directory: {}: kernel returned {}", path, rc
        )));
    }
    Ok(Sexp::T)
}

fn bi_read_stdin_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("read-stdin-bytes", args, 1, Some(1))?;
    let limit = match &args[0] {
        Sexp::Int(n) if *n > 0 => *n as usize,
        other => {
            return Err(EvalError::WrongType {
                expected: "positive integer".into(),
                got: other.clone(),
            });
        }
    };
    let mut buf = vec![0u8; limit];
    // Keep the lossy UTF-8 conversion in Rust.
    let rc = unsafe {
        crate::elisp_cc_spike::bi_read_stdin_bytes(
            buf.as_mut_ptr(),
            limit as i64,
        )
    };
    if rc < 0 {
        return Err(EvalError::Internal(format!(
            "read-stdin-bytes: read returned {}",
            rc,
        )));
    }
    if rc == 0 {
        return Ok(Sexp::Nil);
    }
    let n = rc as usize;
    let n = std::cmp::min(n, buf.len());
    buf.truncate(n);
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
            // SIGINT is handled separately through the quit flag.
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

        // Write the saved state before publishing the flag.
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
        // Shared claim path with the signal handler.
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
        // Bypass buffered stdin so `poll` and `read` see the same fd state.
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
                // The constants are equal on Linux but not on all Unix targets.
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
        match tty_raw::stdin_byte_available(timeout_ms)? {
            Some(b) => Ok(Sexp::Int(b as i64)),
            None => Ok(Sexp::Nil),
        }
    }
    #[cfg(not(unix))]
    {
        let _ = timeout_ms;
        Ok(Sexp::Nil)
    }
}

/// `(_termios-saved-p)' — test helper for raw-mode state.
fn bi_termios_saved_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("_termios-saved-p", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(if tty_raw::termios_saved_p() { Sexp::T } else { Sexp::Nil });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(_raw-mode-hooks-installed-p)' — test helper for raw-mode hooks.
fn bi_raw_mode_hooks_installed_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("_raw-mode-hooks-installed-p", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(if tty_raw::hooks_installed_p() { Sexp::T } else { Sexp::Nil });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(set-quit-flag)' — mark the process-wide quit flag.
fn bi_set_quit_flag(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("set-quit-flag", args, 0, Some(0))?;
    unsafe {
        crate::elisp_cc_spike::bi_set_quit_flag(quit::nl_quit_flag_ptr());
    }
    Ok(Sexp::T)
}

/// `(clear-quit-flag)' — clear the process-wide quit flag.
fn bi_clear_quit_flag(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("clear-quit-flag", args, 0, Some(0))?;
    unsafe {
        crate::elisp_cc_spike::bi_clear_quit_flag(quit::nl_quit_flag_ptr());
    }
    Ok(Sexp::T)
}

/// `(quit-flag-pending-p)' — return t if the quit flag is set.
fn bi_quit_flag_pending_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("quit-flag-pending-p", args, 0, Some(0))?;
    let raw = unsafe {
        crate::elisp_cc_spike::bi_quit_flag_pending_p(quit::nl_quit_flag_ptr())
    };
    Ok(if raw != 0 { Sexp::T } else { Sexp::Nil })
}

/// `(install-sigint-handler)' — install the SIGINT quit-flag handler.
fn bi_install_sigint_handler(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("install-sigint-handler", args, 0, Some(0))?;
    quit::install_sigint_handler();
    Ok(Sexp::T)
}

/// `(_sigint-handler-installed-p)' — test helper for SIGINT hooks.
fn bi_sigint_handler_installed_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("_sigint-handler-installed-p", args, 0, Some(0))?;
    Ok(if quit::sigint_handler_installed_p() { Sexp::T } else { Sexp::Nil })
}

/// `(install-winsize-handler)' — install the SIGWINCH handler.
fn bi_install_winsize_handler(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("install-winsize-handler", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        tty_winsize::install_handler();
        return Ok(Sexp::T);
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

fn bi_winsize_handler_installed_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("_winsize-handler-installed-p", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(if tty_winsize::handler_installed_p() { Sexp::T } else { Sexp::Nil });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(terminal-take-winsize-changed)' — claim the resize-pending flag.
fn bi_terminal_take_winsize_changed(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("terminal-take-winsize-changed", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(if tty_winsize::take_changed() { Sexp::T } else { Sexp::Nil });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(terminal-current-winsize)' — return `(COLS . ROWS)` or nil.
fn bi_terminal_current_winsize(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("terminal-current-winsize", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(match tty_winsize::current_size() {
            Some((c, r)) => Sexp::cons(Sexp::Int(c as i64), Sexp::Int(r as i64)),
            None => Sexp::Nil,
        });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(install-jobctrl-handlers)' — install SIGTSTP/SIGCONT handlers.
fn bi_install_jobctrl_handlers(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("install-jobctrl-handlers", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        tty_jobctrl::install_handlers();
        return Ok(Sexp::T);
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

fn bi_jobctrl_handlers_installed_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("_jobctrl-handlers-installed-p", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(if tty_jobctrl::handlers_installed_p() { Sexp::T } else { Sexp::Nil });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(terminal-take-sigcont)' — claim the SIGCONT-arrived flag.
fn bi_terminal_take_sigcont(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("terminal-take-sigcont", args, 0, Some(0))?;
    #[cfg(unix)]
    {
        return Ok(if tty_jobctrl::take_cont() { Sexp::T } else { Sexp::Nil });
    }
    #[cfg(not(unix))]
    {
        Ok(Sexp::Nil)
    }
}

/// `(read &optional STREAM)' — parse one elisp form from a string stream.
fn bi_read(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("read", args, 0, Some(1))?;
    match args.get(0) {
        Some(Sexp::Str(_)) | Some(Sexp::MutStr(_)) => {
            let impl_fn = env.lookup_function("nelisp--read-from-string-impl").map_err(|_| {
                EvalError::Internal(
                    "read: `nelisp--read-from-string-impl' not loaded \
                     — `lisp/nelisp-stdlib-reader.el' missing from STDLIB_SOURCES?"
                        .into(),
                )
            })?;
            let result = super::apply_function(&impl_fn, &args[0..1], env)?;
            // (FORM . CONSUMED-END) → FORM
            match result {
                Sexp::Cons(b) => Ok(b.car.clone()),
                other => Err(EvalError::Internal(format!(
                    "read: expected `(FORM . CONSUMED-END)' from impl, got {:?}",
                    other
                ))),
            }
        }
        Some(other) => Err(EvalError::NotImplemented(format!(
            "read STREAM type: {:?}",
            other
        ))),
        None => Err(EvalError::NotImplemented(
            "read from stdin (= no STREAM arg) is deferred".into(),
        )),
    }
}

/// `(read-from-string STRING &optional START END)' — delegate to the elisp reader.
fn bi_read_from_string(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("read-from-string", args, 1, Some(3))?;
    let impl_fn = env.lookup_function("nelisp--read-from-string-impl").map_err(|_| {
        EvalError::Internal(
            "read-from-string: `nelisp--read-from-string-impl' not loaded \
             — `lisp/nelisp-stdlib-reader.el' missing from STDLIB_SOURCES?"
                .into(),
        )
    })?;
    super::apply_function(&impl_fn, args, env)
}

fn bi_require(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("require", args, 1, Some(3))?;
    let feature = feature_name_arg("require", &args[0])?;
    if elisp_featurep(env, &feature)? {
        return Ok(Sexp::Symbol(feature));
    }
    // Without a load path or explicit filename, keep bootstrap behavior.
    let filename = match args.get(1) {
        Some(Sexp::Str(s)) => Some(s.clone()),
        _ => None,
    };
    let noerror = args.get(2).map(is_truthy).unwrap_or(false);
    let load_path_configured = env.lookup_value("load-path").is_ok();
    if !load_path_configured && filename.is_none() {
        elisp_provide(env, &feature)?;
        return Ok(Sexp::Symbol(feature));
    }
    let target = filename.unwrap_or_else(|| feature.clone());
    let load_args = vec![Sexp::Str(target), if noerror { Sexp::T } else { Sexp::Nil }];
    let load_fn = env.lookup_function("load")?;
    match super::apply_function(&load_fn, &load_args, env) {
        Ok(_) => {}
        Err(_) if noerror => return Ok(Sexp::Nil),
        Err(e) => return Err(e),
    }
    if !elisp_featurep(env, &feature)? {
        if !noerror {
            return Err(EvalError::UserError {
                tag: "error".into(),
                data: Sexp::list_from(&[Sexp::Str(format!(
                    "Required feature `{}' was not provided",
                    feature
                ))]),
            });
        }
        return Ok(Sexp::Nil);
    }
    Ok(Sexp::Symbol(feature))
}

/// Query `featurep` through its function cell.
fn elisp_featurep(env: &mut Env, feature: &str) -> Result<bool, EvalError> {
    let fn_cell = env.lookup_function("featurep")?;
    let result = super::apply_function(
        &fn_cell,
        &[Sexp::Symbol(feature.to_string())],
        env,
    )?;
    Ok(is_truthy(&result))
}

/// Call `provide` through its function cell.
fn elisp_provide(env: &mut Env, feature: &str) -> Result<(), EvalError> {
    let fn_cell = env.lookup_function("provide")?;
    super::apply_function(
        &fn_cell,
        &[Sexp::Symbol(feature.to_string())],
        env,
    )?;
    Ok(())
}

// ===== Vector / generic accessor helpers =====

/// `(make-vector N INIT)' — allocate a vector filled with `INIT`.
fn bi_make_vector(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("make-vector", args, 2, Some(2))?;
    let len = as_int("make-vector", &args[0])?;
    if len < 0 {
        return Err(EvalError::ArithError(format!(
            "make-vector: negative length {}",
            len
        )));
    }
    // The elisp body reads N via `sexp-int-unwrap' on `n_ptr', which
    // requires the slot to be `Sexp::Int'.  `as_int' above guarantees
    // `args[0]' is one, so passing it directly is sound.  `init_ptr'
    // is shape-agnostic (= `vector-slot-set' clones whatever Sexp the
    // pointer references).  `result_slot' must start as `Sexp::Nil'
    // so `vector-make' can write the tag byte + payload pointer
    // without colliding with a live heap-tagged Sexp drop.
    let mut result_slot: Sexp = Sexp::Nil;
    unsafe {
        crate::elisp_cc_spike::bi_make_vector(
            &args[0] as *const Sexp,
            &args[1] as *const Sexp,
            &mut result_slot as *mut Sexp,
        );
    }
    Ok(result_slot)
}
