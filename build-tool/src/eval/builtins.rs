//! Built-in function registry.  [`install_builtins`] writes a
//! `(builtin <NAME>)' sentinel into the function cell of each symbol;
//! the evaluator routes calls to [`dispatch`].
//!
//! Two surface categories survive in Rust:
//!
//! 1. **KEEP arms** — low-level I/O / filesystem / process / signal /
//!    TTY / reader entry points / vector core / load orchestration.
//! 2. **Tier 3 bridge plumbing** — `nl-jit-call-*' / `nl-ffi-*' /
//!    `nl-cons-*' / `nl-rc-*' / `nl-gc-*' / `nelisp--apply-*' /
//!    `nelisp--*-frame' / dlsym-resolve.
//!
//! Tier 1 (predicates / type-of / arithmetic variadics / cXXr / mapcar /
//! assq) is in `lisp/nelisp-stdlib*.el'.  Tier 2 (float / hash / time /
//! regex / strings transforms / sxhash / intern / symbol-name) rides
//! `nl_jit_*' trampolines in `build-tool/src/jit/*.rs'.

use super::env::Env;
use super::error::EvalError;
use super::quit;
use super::sexp::Sexp;
use super::special_forms::is_truthy;
use std::path::{Path, PathBuf};

/// Install every built-in into the given environment.  Idempotent.
pub fn install_builtins(env: &mut Env) {
    let names: &[&str] = &[
        // arithmetic 2-arg primitives (= JIT lowered, see
        // jit/arith.rs::lowered_*).  `/' rides `nl_jit_float_div'.
        "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
        "nelisp--num-lt2", "nelisp--num-gt2",
        "nelisp--num-le2", "nelisp--num-ge2",
        "nelisp--num-eq2",
        // equality (= JIT lowered_eq in jit/predicate.rs)
        "eq",
        // cons / list (= JIT lowered_{car,cdr,cons,length,setcar,setcdr,
        // aref,aset,elt}; `nelisp--length-cons-cc' is the internal
        // bridge to the Doc 101 §101.B elisp-compiled cons walker;
        // `nelisp--recordp-cc' is the Doc 111 §111.B record predicate
        // bridge; `string-bytes' stays plain Rust)
        "car", "cdr", "cons", "length", "nelisp--length-cons-cc", "nelisp--recordp-cc", "string-bytes",
        "setcar", "setcdr",
        "aref", "aset", "elt",
        "vector", "make-vector",
        // bitwise — `ash' stays native for raw shift with clamping;
        // -logior2 / -logand2 / -logxor2 are JIT lowered.
        "ash",
        "nelisp--logior2", "nelisp--logand2", "nelisp--logxor2",
        // string format/build slivers — `truncate' lets elisp `format'
        // coerce float→int; `nl-jit-call-format-float' is the bridge
        // for the IEEE-754 float-body trampoline; `string-match-p'
        // stays as a registered name even though the body lives in
        // jit/regex.rs (= elisp wrapper installs the function-cell
        // override at boot).
        "truncate",
        "nl-jit-call-format-float",
        "string-match-p",
        // file syscall primitives that elisp wrappers ride: canonicalize
        // / stat / readdir / read-file / read-all-from-string back the
        // elisp `file-truename' / `file-exists-p' family /
        // `directory-files' / `load' implementations.
        "nelisp--syscall-canonicalize",
        "nelisp--syscall-stat",
        "nelisp--syscall-readdir",
        "nelisp--syscall-read-file",
        "nelisp--read-all-from-string",
        // generic POSIX syscall + supported-p (= Linux nr-table dispatch).
        "nelisp--syscall",
        "nelisp--syscall-supported-p",
        // Specialised socket primitives — msghdr / cmsg / ucred / scope_id
        // marshaling too involved for nl-ffi in elisp alone.
        "nelisp--syscall-socketpair",
        "nelisp--syscall-sendmsg-fds",
        "nelisp--syscall-recvmsg-fds",
        "nelisp--syscall-getsockopt-peercred",
        "nelisp--syscall-bind-inet6-scoped",
        "nelisp--syscall-connect-inet6-scoped",
        "nelisp--syscall-accept-inet6-scoped",
        // symbol/function cell ops + dispatch core.  `symbol-function' +
        // `fset' stay (env-shim bake reads them before its wrappers load).
        "symbol-function", "funcall", "apply", "eval",
        "fset",
        "macroexpand-1",
        // print/error slivers — `signal' is the unwind primitive.
        // `nelisp--write-stderr-line' / `-write-stdout-bytes' back the
        // elisp `message' / `princ' / `print' / `prin1-to-string'.
        "signal",
        "nelisp--write-stderr-line",
        "nelisp--write-stdout-bytes",
        // `require' orchestrates load + post-load verify, calling back
        // into elisp `load' / `featurep' through their function cells.
        "require",
        // self-process stdio (Phase 9 minimal — needed by stand-alone
        // Lisp servers such as elisp-lsp running on the `nelisp' binary).
        "read-stdin-bytes",
        // Generic FFI primitives (libffi-backed) + buffer + accessor
        // variants for sockaddr_* / pollfd / sigset_t / etc. marshaling.
        "nl-ffi-call",
        "nl-ffi-malloc", "nl-ffi-read-bytes", "nl-ffi-free",
        "nl-ffi-write-bytes", "nl-ffi-errno",
        "nl-ffi-read-i32", "nl-ffi-read-i64",
        "nl-ffi-read-i16", "nl-ffi-read-u16", "nl-ffi-read-u32",
        "nl-ffi-write-i16", "nl-ffi-write-i32",
        "nl-ffi-read-u8", "nl-ffi-write-bytes-at", "nl-ffi-read-bytes-at",
        "nl-ffi-write-i64",
        // Native-only primitives (time / hash / case / math / file I/O).
        "nl-current-unix-time", "nl-secure-hash", "nl-format-unix-time",
        "nl-downcase", "nl-upcase", "nl-split-by-non-alnum",
        "float", "exp", "log", "nelisp--f64-trunc",
        "nl-write-file", "nl-make-directory",
        // Interactive TTY plumbing (Unix only) — raw mode + signal hooks
        // (SIGINT/SIGWINCH/SIGTSTP/SIGCONT) + quit-flag.  `_'-prefixed are test helpers.
        "terminal-raw-mode-enter", "terminal-raw-mode-leave",
        "read-stdin-byte-available",
        "_termios-saved-p", "_raw-mode-hooks-installed-p",
        "set-quit-flag", "clear-quit-flag", "quit-flag-pending-p",
        "install-sigint-handler", "_sigint-handler-installed-p",
        "install-winsize-handler", "_winsize-handler-installed-p",
        "terminal-take-winsize-changed", "terminal-current-winsize",
        "install-jobctrl-handlers", "_jobctrl-handlers-installed-p",
        "terminal-take-sigcont",
        // reader exposed as elisp callable (Track O' Phase 2).
        "read", "read-from-string",
        // apply/call/closure/env primitives + use_elisp_apply toggle +
        // apply-lambda-inner (frame-capture leak fix).
        "nelisp--push-frame", "nelisp--pop-frame", "nelisp--push-captured",
        "nelisp--bind-local", "nelisp--apply-builtin-dispatch",
        "nelisp--set-use-elisp-apply", "nelisp--get-use-elisp-apply",
        "nelisp--apply-lambda-inner",
        // `nl-jit-call-*' bridge primitives — i64-i64 / ptr-ptr / syscall +
        // 4 out-param variants + 3 float bridges.  See `jit/bridge.rs'.
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
        // Layer 2 cons + rc + gc primitives backing nelisp-stdlib-gc.el.
        "nl-cons-alloc", "nl-cons-car", "nl-cons-cdr",
        "nl-cons-set-car", "nl-cons-set-cdr",
        "nl-rc-inc", "nl-rc-dec", "nl-rc-count",
        "nl-rc-alloc", "nl-rc-dealloc",
        "nl-rc-inc-strong", "nl-rc-dec-strong", "nl-rc-strong-count",
        "nl-rc-kind", "nl-rc-payload-ptr",
        "nl-gc-walk-children", "nl-gc-buffered-decs", "nl-gc-finalize",
        // dlsym bridge — standalone NeLisp wires it at startup.
        "nelisp-cc--dlsym-resolve",
        // Phase 47 elisp-only builtin (linux-x86_64 only).
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
        // ---- vector core (= JIT lowered: car/cdr/cons/setcar/setcdr/
        // length/aref/aset/elt; only `vector' / `make-vector' /
        // `string-bytes' stay plain Rust) ----
        "vector" => Ok(Sexp::vector(args.to_vec())),
        "make-vector" => bi_make_vector(args),
        "nelisp--length-cons-cc" => bi_length_cons_cc(args),
        "nelisp--recordp-cc" => bi_recordp_cc(args),
        "string-bytes" => bi_string_bytes(args),
        // ---- string format/build slivers (Doc 86 §86.1.e bridge,
        // `truncate' float→int helper for elisp `format') ----
        "nl-jit-call-format-float" => crate::jit::bi_nl_jit_call_format_float(args),
        "truncate" => bi_truncate(args),
        // ---- filesystem syscalls (= elisp wrappers ride these) ----
        "nelisp--syscall-canonicalize" => bi_syscall_canonicalize(args, env),
        "nelisp--syscall-stat" => bi_syscall_stat(args, env),
        "nelisp--syscall-readdir" => bi_syscall_readdir(args, env),
        "nelisp--syscall-read-file" => bi_syscall_read_file(args, env),
        "nelisp--read-all-from-string" => bi_read_all_from_string(args, env),
        // ---- POSIX syscall (Linux nr-table dispatch) ----
        "nelisp--syscall" => bi_syscall(args),
        "nelisp--syscall-supported-p" => bi_syscall_supported_p(args),
        // Specialised socket primitives (SCM_RIGHTS / SO_PEERCRED / scope_id)
        // dispatch via the externally-registered builtin path below.
        //
        // ---- symbol / function ----
        "symbol-function" => bi_symbol_function(args, env),
        "fset" => bi_fset(args, env),
        "macroexpand-1" => bi_macroexpand_1(args, env),
        // ---- apply/closure/env primitives + use_elisp_apply + apply-lambda-inner ----
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
        // ---- print/error slivers (= byte-write-to-stdout backs elisp
        // `princ' / `print' / `prin1-to-string'; writeln-to-stderr
        // backs elisp `message' / `error') ----
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
        // ---- reader (Track O' Phase 2) ----
        "read" => bi_read(args, env),
        "read-from-string" => bi_read_from_string(args, env),
        // ---- require (orchestrates load + post-load verify; calls
        // back into elisp `load' / `featurep' through function cells) ----
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
        // Phase 47 elisp-only body (`lisp/nelisp-cc-fact-i64.el').
        "nl-fact-i64" => bi_nl_fact_i64(args),
        // Env-shim slim primitive dispatch arm.
        "nelisp--env-globals-op" => crate::eval::env_shim::bi_globals_op(args, env),
        _ => {
            // Externally-registered builtin (test-only).
            #[cfg(test)]
            {
                // Clone the Rc first so the `extern_builtins' borrow drops
                // before we re-borrow `env' through the closure.
                if let Some(f) = env.extern_builtins.get(name).cloned() {
                    return f(args, env);
                }
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

// ---------- arithmetic helpers (= `num_pair' = the lone
//   surviving Float-promote helper for jit/bridge.rs) ----------

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

/// `(string-bytes STRING)' — UTF-8 byte count.  Body dispatches to
/// Phase 47-compiled `nelisp_bi_string_bytes' kernel; Rust keeps arity
/// + tag dispatch + caller-owned out-slot + WrongType error.
fn bi_string_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-bytes", args, 1, Some(1))?;
    // Unify the two string variants behind a `*const Sexp' that the
    // elisp body can `str-len' directly.  `Sexp::Str' is already in
    // the right shape; `Sexp::MutStr' carries the bytes inside an
    // `NlStrMutRef.value: String', so we materialise a transient
    // `Sexp::Str' (= bitwise clone of the same underlying bytes) and
    // hand the elisp body a pointer into that local.
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

/// `(nelisp--syscall-canonicalize PATH)' — wraps `std::fs::canonicalize'.
/// Returns nil on any error so elisp `file-truename' can fall back.
///
/// Doc 117 §117.D.gaps.3 (2026-05-18): the libc syscall body now runs
/// through the Phase 47 elisp object compiled from
/// `lisp/nelisp-cc-bi-syscall-canonicalize.el' (= same shape as the
/// §117.D.3 sibling `bi_syscall_stat' modulo the libc symbol name).
/// The Rust shim keeps arity validation + path normalisation + the
/// PATH_MAX result buffer alloc + the NUL-terminated CStr → Sexp::Str
/// wrap (= `Sexp::Nil' on NULL).  Behaviour parity with
/// `std::fs::canonicalize' via `libc::realpath(3)' which both libstd
/// and this kernel delegate to under the hood.
fn bi_syscall_canonicalize(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-canonicalize", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    let path_sexp = Sexp::Str(p.to_string_lossy().into_owned());
    // PATH_MAX on Linux is 4096; we use that as the result-buffer
    // size.  `libc::realpath' writes a NUL-terminated resolved path
    // here on success.  `MaybeUninit' avoids the zero-init cost for
    // bytes the kernel is about to overwrite.
    let mut result_buf = vec![0u8; libc::PATH_MAX as usize];
    let rc = unsafe {
        crate::elisp_cc_spike::bi_syscall_canonicalize(
            &path_sexp as *const Sexp,
            result_buf.as_mut_ptr(),
        )
    };
    if rc == 0 {
        // libc `realpath' returned NULL — error / not-found / EACCES /
        // ENOTDIR.  Pre-swap `Err(_) => Ok(Sexp::Nil)' parity.
        return Ok(Sexp::Nil);
    }
    // Find the NUL terminator and slice off the resolved path.  The
    // buffer is owned by us — safe to read until the first 0x00.
    let len = result_buf.iter().position(|&b| b == 0).unwrap_or(result_buf.len());
    let resolved = String::from_utf8_lossy(&result_buf[..len]).into_owned();
    Ok(Sexp::Str(resolved))
}

/// `(nelisp--syscall-read-file PATH)' — `std::fs::read_to_string'.
/// Returns nil on I/O error.  Used by elisp `load'.
fn bi_syscall_read_file(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-read-file", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    match std::fs::read_to_string(&p) {
        Ok(s) => Ok(Sexp::Str(s)),
        Err(_) => Ok(Sexp::Nil),
    }
}

/// `(nelisp--read-all-from-string STR)' — mandatory delegation to elisp
/// `nelisp--read-all-from-string-impl' (= `lisp/nelisp-stdlib-reader.el').
/// Hard error if the elisp impl isn't installed.
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

/// `(nelisp--syscall-stat PATH)' — returns `'absent / `'file / `'directory.
/// Other file types + any metadata error → `'absent.  Backs 4 elisp wrappers
/// (file-exists-p / file-readable-p / file-directory-p / file-regular-p).
///
/// Doc 117 §117.D.gaps.3 (2026-05-18): the libc syscall body now runs
/// through the Phase 47 elisp object compiled from
/// `lisp/nelisp-cc-bi-syscall-stat.el' (= `nelisp_cstr_from_sexp' for
/// the path CString + `extern-call stat' for the syscall + `dealloc-
/// bytes' for the CString free, sequenced via a 2-arg `prog2'
/// helper).  The Rust shim keeps arity validation + path
/// normalisation + the `struct stat' buffer alloc + the mode-field
/// inspection + the `'absent / `'file / `'directory' symbol tag
/// dispatch.  Behaviour parity with the pre-swap
/// `std::fs::metadata' shape via `libc::S_IFMT' mask of the
/// populated buffer's `st_mode' field.
fn bi_syscall_stat(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-stat", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    // Build a `Sexp::Str' the elisp kernel can read.  `to_string_lossy'
    // matches the pre-swap path's UTF-8 coercion (= libstd would have
    // done the same on the way through `metadata(&Path)' since
    // `PathBuf' is opaque).
    let path_sexp = Sexp::Str(p.to_string_lossy().into_owned());
    // Rust-owned `struct stat' buffer.  `libc::stat' size depends on
    // glibc / musl — use the libc-crate type directly so the layout
    // matches the kernel's expectation byte-for-byte.
    let mut statbuf: libc::stat = unsafe { std::mem::zeroed() };
    let rc = unsafe {
        crate::elisp_cc_spike::bi_syscall_stat(
            &path_sexp as *const Sexp,
            (&mut statbuf as *mut libc::stat) as *mut u8,
        )
    };
    // Map (rc, mode) → tag symbol.  Negative rc = errno-set error =
    // `'absent (= mirrors the pre-swap `Err(_) => "absent"' arm).
    let tag = if rc < 0 {
        "absent"
    } else {
        // S_IFMT mask + S_IFDIR / S_IFREG compare.  Exotic file types
        // (sockets / fifos / block-dev / char-dev) fall through to
        // `'absent' — same behaviour as the pre-swap `Ok(_) =>
        // "absent"' arm where both `is_file()' and `is_dir()' returned
        // false.
        let mode = statbuf.st_mode & libc::S_IFMT;
        if mode == libc::S_IFDIR {
            "directory"
        } else if mode == libc::S_IFREG {
            "file"
        } else {
            "absent"
        }
    };
    Ok(Sexp::Symbol(tag.into()))
}

/// `(nelisp--syscall-readdir DIR)' — POSIX readdir.  Returns
/// `(ABS-DIR NAME1 NAME2 ...)` (unsorted); nil on error.  Backs elisp
/// `directory-files' which handles sort + regex + FULL + COUNT.
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

// ---- POSIX syscall surface (Linux only via libc::syscall + libc::SYS_*) ----
// Non-Linux: `-supported-p' returns nil for elisp `nl-ffi-call libc' fallback.
// Errors normalised via __errno_location() to (result < 0 = -errno).

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

// Phase 5 Stage 5.8 — non-Linux syscall stub macro (loud-fail catch
// for elisp callers that accidentally bypass the Path B fallback).
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
    require_arity("symbol-function", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => env.lookup_function(s),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
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

/// `(fset SYMBOL DEFINITION)` — install DEFINITION in SYMBOL's function
/// cell.  Last Rust function-cell setter (env-shim bake needs it before
/// its elisp wrappers load).
fn bi_fset(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("fset", args, 2, Some(2))?;
    let name = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    };
    let def = match &args[1] {
        Sexp::Symbol(s) => env.lookup_function(s)?,
        other => other.clone(),
    };
    env.set_function(&name, def.clone());
    Ok(def)
}

/// `(macroexpand-1 FORM &optional ENV)' — expand FORM by ONE level if
/// its head is a macro; otherwise return FORM unchanged.  ENV ignored.
fn bi_macroexpand_1(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("macroexpand-1", args, 1, Some(2))?;
    let form = &args[0];
    // Only `(SYM ARG...)` shape is expandable — atoms / dotted lists /
    // lambda-headed combiners (= `((lambda ...) X)`) self-expand.
    let (head_sym, tail) = match form {
        Sexp::Cons(b) => match &b.car {
            Sexp::Symbol(s) => (s.clone(), b.cdr.clone()),
            _ => return Ok(form.clone()),
        },
        _ => return Ok(form.clone()),
    };
    let func = match env.lookup_function(&head_sym) {
        Ok(f) => f,
        // Unbound symbol → return form unchanged (= Emacs parity for
        // `macroexpand-1' on non-macros).
        Err(_) => return Ok(form.clone()),
    };
    // `(macro . LAMBDA)` shape — expand via the macro's lambda.
    let is_macro = matches!(
        &func,
        Sexp::Cons(b) if matches!(&b.car, Sexp::Symbol(s) if s == "macro")
    );
    if !is_macro {
        return Ok(form.clone());
    }
    // Strip the `macro' tag, get the underlying lambda.
    let parts = super::list_elements(&func)?;
    if parts.len() < 2 {
        return Err(EvalError::Internal("malformed macro".into()));
    }
    let inner = &parts[1];
    let arg_forms = super::list_elements(&tail)?;
    super::apply_function(inner, &arg_forms, env)
}

// Doc 102 Phase 3.a (2026-05-13): 4 frame primitives' bodies merged.
// 4 frame primitives merged; elisp `defun' wrappers would break frame
// semantics (= wrapper pushes a frame, `bind-local' targets the wrapper).
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

/// `(nelisp--apply-builtin-dispatch NAME ARGS)' — direct dispatch by
/// name.  ARGS already evaluated.  Backs elisp `nelisp--apply-fn' for
/// the `(builtin NAME)' sentinel arm.
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

/// `(nelisp--apply-lambda-inner CAPTURED FORMALS BODY ARGS)' —
/// Rust-native lambda apply.  Keeps state on the Rust call stack to
/// avoid closure capture-leak bugs.  BODY + ARGS are proper lists;
/// ARGS already evaluated.
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
    // Doc 51 Track M — `(signal 'quit ...)' must surface as the
    // dedicated `EvalError::Quit` so that `condition-case`'s `error`
    // clause does NOT catch it (per Elisp manual).  The data list
    // is discarded — quit carries no payload in Emacs.
    if tag == "quit" {
        return Err(EvalError::Quit);
    }
    // Doc 80 Stage 80.3 (2026-05-09) — canonicalise `arith-error' /
    // `wrong-type-argument' tags to the structurally-distinct
    // `EvalError' variants.  Without this, the elisp-side fall-
    // through dispatch in `lisp/nelisp-jit-strategy.el' (= `aref' /
    // `aset' / `elt' replacements) would produce `UserError { tag,
    // data }', which `condition-case' catches identically but every
    // existing `matches!(e, EvalError::ArithError(_))' /
    // `EvalError::WrongType { .. })' test would silently break.
    // See `error.rs::error_tag' for the bidirectional `tag ↔ variant'
    // map; this is its inverse for `(signal 'TAG DATA)' callsites.
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

/// `(nelisp--write-stdout-bytes STR)' — write STR's bytes to stdout
/// and flush.  No newline added.  Returns STR unchanged.  Building
/// block for the elisp `princ' (Rust-min 2026-05-06 batch 6i); the
/// previous `bi_princ' was just a stringp/Display dispatch wrapped
/// around this writeln, and moving the dispatch to elisp keeps the
/// I/O sliver as the only Rust-only piece.
fn bi_write_stdout_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stdout-bytes", args, 1, Some(1))?;
    // Validate stringp + materialise the variant-uniform `Sexp::Str'
    // view that the elisp body's `str-bytes-ptr' op can read.  Same
    // dispatch shape as `bi_write_stderr_line' (Doc 117 §117.B /
    // Doc 122 §122.H) — the elisp body handles all string-y variants
    // uniformly via `nl_str_bytes_ptr', but routing through `Sexp::Str'
    // keeps the elisp body free of any variant-specific branching.
    let s = args[0].as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: "stringp".into(),
        got: args[0].clone(),
    })?;
    let body_sexp = Sexp::Str(s);
    // Doc 117 §117.B (cont): the per-payload byte-write
    // `out.write_all(s.as_bytes())' step now runs through the Phase
    // 47 elisp object compiled from
    // `lisp/nelisp-cc-bi-write-stdout-bytes.el' (= a 3-arg
    // `extern-call' to libc `write' using the §122.H `str-bytes-ptr'
    // grammar op + the §101.C `str-len' op).  Return is the libc
    // `write(2)' i64 — `> 0' = bytes written, `0' = success on
    // 0-byte payload, `-1' = error.
    let rc = unsafe {
        crate::elisp_cc_spike::bi_write_stdout_bytes(
            &body_sexp as *const Sexp,
        )
    };
    if rc < 0 {
        // Mirror the pre-swap `map_err' branch: I/O errors propagate
        // through `EvalError::Internal' with the same prefix.  errno
        // is not surfaced today (the libc `write' errno would land
        // here but we don't materialise it across the elisp body);
        // emit a generic message — callers that match on the prefix
        // continue to do so.
        return Err(EvalError::Internal(format!(
            "nelisp--write-stdout-bytes: write returned {}",
            rc,
        )));
    }
    // Flush is kept in the Rust shim — the elisp body is exactly the
    // syscall, no flush opcode in Phase 47's grammar.
    let mut out = std::io::stdout().lock();
    out.flush()
        .map_err(|e| EvalError::Internal(format!("nelisp--write-stdout-bytes: {}", e)))?;
    Ok(args[0].clone())
}

/// `(nelisp--write-stderr-line STR)' — write STR followed by a
/// newline to stderr and flush.  Returns STR unchanged.  Building
/// block for the elisp `message' (Rust-min 2026-05-06 batch 6h);
/// the previous `bi_message' was just `bi_format' + this writeln,
/// and moving the dispatch to elisp means the few-line I/O sliver
/// is the only piece that genuinely needs Rust.
///
/// Doc 117 §117.B / Doc 122 §122.H (2026-05-18): the per-payload
/// byte-write `out.write_all(s.as_bytes())' step now runs through the
/// Phase 47 elisp object compiled from
/// `lisp/nelisp-cc-bi-write-stderr-line.el' (= a 3-arg `extern-call'
/// to libc `write' using the new §122.H `str-bytes-ptr' grammar op
/// + the §101.C `str-len' op).  The Rust shim keeps arity validation
/// + the `WrongType' tag dispatch + the trailing `\n' byte + the
/// final `err.flush()' so user-observable behaviour matches the
/// pre-swap `writeln!' exactly.  First I/O syscall in the Doc 117
/// Tier B sweep — same shape applies to `write-stdout-bytes' /
/// `read-stdin-bytes' / `read-file' / `write-file' next.
fn bi_write_stderr_line(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stderr-line", args, 1, Some(1))?;
    // Validate stringp + materialise the variant-uniform `Sexp::Str'
    // view that the elisp body's `str-bytes-ptr' op can read.
    // `as_string_owned()' returns Some for Str / MutStr (= the same
    // gate the pre-swap body used).  We rebuild a stack-local
    // `Sexp::Str' so the elisp body sees a single layout — the
    // §122.H `nl_str_bytes_ptr' extern handles MutStr natively but
    // routing through `Sexp::Str' keeps the elisp body free of any
    // variant-specific branching.
    let s = args[0].as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: "stringp".into(),
        got: args[0].clone(),
    })?;
    let body_sexp = Sexp::Str(s);
    // Dispatch the body write through the Phase 47 elisp object.
    // The return is the libc `write(2)' i64 (= bytes written or -1);
    // discarded here for parity with the pre-swap `let _ = writeln!()'.
    unsafe {
        let _ = crate::elisp_cc_spike::bi_write_stderr_line(
            &body_sexp as *const Sexp,
        );
    }
    // Trailing newline + flush — kept in the Rust shim so the elisp
    // body is exactly the algorithmic core.  `let _ = ...' preserves
    // the pre-swap error-suppression (= `writeln!' + `flush' both
    // ignored I/O errors).
    let mut err = std::io::stderr().lock();
    let _ = err.write_all(b"\n");
    let _ = err.flush();
    Ok(args[0].clone())
}

/// `(truncate X)' — return X truncated toward zero as an integer.
///
/// For a Float argument we cast via `as i64' (= the same trunc-toward-
/// zero semantics the previous `bi_format' used inline for `%d FLOAT').
/// Added in Rust-min batch 6m so the elisp `format' dispatcher can
/// coerce float→int for `%d/%i/%x/%X/%o/%b' without needing a
/// privileged float-cast.
///
/// Doc 100 v2 §100.C (2026-05-12): the Int arm now routes through the
/// Phase 47-compiled `nelisp_truncate_int' function in
/// `lisp/nelisp-cc-truncate-int.el'.  The Rust side here is a thin
/// dispatch + caller-owned-slot setup; the actual identity body
/// (= unwrap + re-wrap with same payload) lives in elisp.  No Rust
/// algorithmic line survives the swap for the Int variant.
fn bi_truncate(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("truncate", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(_) => {
            // Doc 100 §100.C: route the Int identity through the
            // elisp `.o'.  The slot is a stack-local Sexp::Nil; the
            // elisp body writes tag=SEXP_TAG_INT + i64 payload into
            // bytes [0, 16) of the slot.  Padding bytes [1, 8) +
            // unused tail [16, 32) remain whatever the Nil
            // initialization left them as, which is sound because
            // Sexp::Int reads neither.
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

/// Internal `(length X)' cons/nil arm bridge used by
/// `lisp/nelisp-jit-strategy.el'.  Doc 101 §101.B moved the proper-list
/// walk itself into `lisp/nelisp-cc-length-cons.el`; this Rust body is
/// just arity/type dispatch plus the caller-owned out-slot.
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

/// (nl-fact-i64 N) — Doc 99 §99.C first elisp-only builtin.
///
/// Computes `N!' via the Phase-47-compiled `nelisp_fact_i64' function
/// in `lisp/nelisp-cc-fact-i64.el'.  The Rust dispatch arm is purely
/// a Sexp-unwrap / range-check / Sexp-wrap shim — the algorithmic
/// body lives nowhere except the elisp source.
///
/// Range invariant: 0 ≤ N ≤ 20.  `21!' (= 51090942171709440000) does
/// not fit in i64, so the elisp body's recursion would silently
/// overflow.  Out-of-range inputs signal `arith-error' so the elisp
/// caller can `condition-case' around the failure.
///
/// Doc 114: Phase 47 helpers are x86_64-linux only; the entire crate
/// fails to build on non-x86_64-linux via the top-level guard.
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

fn bi_nl_write_file(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-write-file", args, 2, Some(2))?;
    // Validate stringp for both args + materialise variant-uniform
    // `Sexp::Str' views the elisp kernel can read.  The kernel does
    // not introspect the variant tag — it expects `Sexp::Str' /
    // `Sexp::MutStr' (the §122.I `nelisp_cstr_from_sexp' + §122.H
    // `nl_str_bytes_ptr' externs handle both natively).  Routing
    // through `Sexp::Str' keeps the kernel free of variant-specific
    // branching.
    let path = string_value(&args[0])?;
    let content = string_value(&args[1])?;
    let path_sexp = Sexp::Str(path.clone());
    let content_sexp = Sexp::Str(content);
    // Doc 117 §117.D.gaps.3 (2026-05-18): the chained libc
    // `open(2)' + `write(2)' + `close(2)' steps now run through the
    // Phase 47 elisp object compiled from
    // `lisp/nelisp-cc-bi-nl-write-file.el'.  Rust keeps the arity
    // + `stringp' dispatch + the negative-rc → Internal-err
    // mapping.
    let rc = unsafe {
        crate::elisp_cc_spike::bi_nl_write_file(
            &path_sexp as *const Sexp,
            &content_sexp as *const Sexp,
        )
    };
    if rc < 0 {
        // Mirror the pre-swap `map_err' branch: I/O errors propagate
        // through `EvalError::Internal' with the same `path' prefix.
        // The elisp kernel collapses open / write errors into a
        // single negative-i64 return; the Rust shim cannot distinguish
        // which syscall failed, but neither did the pre-swap
        // `std::fs::write' which only surfaced an opaque
        // `std::io::Error' string.
        return Err(EvalError::Internal(format!(
            "nl-write-file: {}: kernel returned {}",
            path, rc,
        )));
    }
    Ok(Sexp::T)
}

fn bi_nl_make_directory(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-make-directory", args, 1, Some(2))?;
    let path = string_value(&args[0])?;
    std::fs::create_dir_all(&path)
        .map_err(|e| EvalError::Internal(format!("nl-make-directory: {}: {}", path, e)))?;
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
    // Doc 117 §117.B (cont): the `read(0, buf, limit)' libc syscall
    // step now runs through the Phase 47 elisp object compiled from
    // `lisp/nelisp-cc-bi-read-stdin-bytes.el' (= a 3-arg `extern-call'
    // to libc `read').  The Rust shim keeps buffer allocation, EOF /
    // err branching, and `from_utf8_lossy' wrap because Phase 47's
    // §122.A `sexp-write-str' op uses `from_utf8_unchecked' (= would
    // produce UB on non-UTF-8 stdin); a future §122.X
    // `sexp-write-str-lossy' op would let the wrap migrate too.
    let rc = unsafe {
        crate::elisp_cc_spike::bi_read_stdin_bytes(
            buf.as_mut_ptr(),
            limit as i64,
        )
    };
    if rc < 0 {
        // I/O error — match the pre-swap `Err(e)' arm semantically.
        // The libc `read' errno would land here but we don't materialise
        // it across the elisp body; emit a generic message — callers
        // that match on the prefix continue to do so.
        return Err(EvalError::Internal(format!(
            "read-stdin-bytes: read returned {}",
            rc,
        )));
    }
    if rc == 0 {
        // EOF — peer closed stdin.
        return Ok(Sexp::Nil);
    }
    let n = rc as usize;
    // Defensive clamp: libc `read' should never return > limit but
    // guard the truncate in case of an unexpected return value (= the
    // pre-swap body trusted `handle.read' which is bounded by buf.len).
    let n = std::cmp::min(n, buf.len());
    buf.truncate(n);
    Ok(Sexp::Str(String::from_utf8_lossy(&buf).into_owned()))
}

// Doc 51 Track E (2026-05-04) — TTY raw-mode + non-blocking byte reader.
// Doc 51 Track K (2026-05-04) — atexit + SIGINT/SIGTERM/SIGHUP/SIGQUIT
// hook so that `Ctrl+C` / `kill` / panic / unwind never leaves the user's
// terminal in raw mode.  Storage is intentionally async-signal-safe
// (atomic flag + raw static) — `Mutex` is *not* async-signal-safe per
// POSIX.  `tcsetattr` *is* on the POSIX async-signal-safe list, which is
// why this works in a signal handler.
//
// Unix-only.  On non-Unix the corresponding builtins are no-ops returning
// nil so substrate code can run on Windows host without errors.

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
            // atexit cleans up the orderly `exit(N)` / `return from main`
            // path.  Returns 0 on success; we ignore failure (best-effort).
            libc::atexit(atexit_hook);

            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = sig_handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            sa.sa_flags = 0;
            // SIGINT is intentionally NOT in this list — Doc 51
            // Track M owns SIGINT (= sets the quit flag instead of
            // terminating).  These three are the "real shutdown"
            // signals where restoring termios + re-raising the
            // default is the correct response.
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

        // Stash the original termios in the signal-safe slot BEFORE
        // flipping the flag — the handler reads through the flag, so any
        // write that happens-before the flag is visible by the handler.
        unsafe {
            std::ptr::write(
                std::ptr::addr_of_mut!(SAVED_TERMIOS) as *mut libc::termios,
                term,
            );
        }
        TTY_FD.store(fd, Ordering::SeqCst);
        TERMIOS_SAVED.store(true, Ordering::SeqCst);

        install_hooks_once();

        // cfmakeraw is the standard setup: -ICANON, -ECHO, etc.
        unsafe { libc::cfmakeraw(&mut term) };
        // VMIN=1 / VTIME=0: block until at least one byte is available.
        term.c_cc[libc::VMIN] = 1;
        term.c_cc[libc::VTIME] = 0;
        if unsafe { libc::tcsetattr(fd, libc::TCSANOW, &term) } != 0 {
            // Roll back the saved-state flag so a leave at unwind-time
            // does not try to restore a half-applied raw mode.
            TERMIOS_SAVED.store(false, Ordering::SeqCst);
            return Err(EvalError::Internal(format!(
                "terminal-raw-mode-enter: tcsetattr failed: {}",
                std::io::Error::last_os_error()
            )));
        }
        Ok(())
    }

    pub fn raw_mode_leave() -> Result<(), EvalError> {
        // Race-free claim of the saved state — same primitive the signal
        // handler uses, so explicit leave and signal-driven leave cannot
        // both restore.
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
        // Idempotent — calling without a prior enter is a no-op.
        Ok(())
    }

    // Test helper: report whether the raw-mode flag is currently held.
    pub fn termios_saved_p() -> bool {
        TERMIOS_SAVED.load(Ordering::SeqCst)
    }

    // Test helper: report whether atexit/signal hooks have been installed.
    // The hooks are install-once and cannot be uninstalled, so this is
    // monotonic: false until the first enter, true forever after.
    pub fn hooks_installed_p() -> bool {
        HOOKS_INSTALLED.is_completed()
    }

    pub fn stdin_byte_available(timeout_ms: i32) -> Result<Option<u8>, EvalError> {
        // Doc 51 (2026-05-04) — read via libc::read on fd 0 directly,
        // NOT via `std::io::stdin().lock().read()`.  The Rust stdin
        // is internally buffered; reading 1 byte through it pulls
        // many bytes from the kernel into the user-space buffer,
        // and the next `poll()` then sees no data on the fd —
        // POLLHUP without POLLIN — so we incorrectly report EOF
        // while the buffered reader still has the rest queued.
        //
        // Going around the buffered reader keeps poll() and read()
        // looking at the same kernel-side state.
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
        // Some platforms set POLLHUP alongside POLLIN; some only
        // POLLHUP after the writer closed and the kernel buffer
        // drained.  Try to read either way — if no data is left,
        // read() returns 0 (EOF) and we map that to None.
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
                // EAGAIN / EWOULDBLOCK can happen between poll and
                // read on a non-blocking fd; treat as no-byte.  On Linux
                // these constants share a value so the second arm is
                // unreachable — keep both written for portability to
                // BSDs where they differ; allow the lint here.
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

// Doc 51 Track P (2026-05-04) — SIGWINCH (terminal resize) plumbing.
//
// The handler is the simplest possible: flip an `AtomicBool` and
// return.  All real work (= ioctl(TIOCGWINSZ), frame-resize, redisplay
// flush) happens in the event loop on the next iteration.  This is
// the canonical async-signal-safe pattern — same shape as the
// quit-flag in `eval::quit`.
//
// `terminal-current-winsize` is exposed unconditionally (= can be
// queried from non-raw-mode contexts too, e.g. a host-driver
// startup).  The flag query (`terminal-take-winsize-changed`) is
// only meaningful after `install-winsize-handler` runs.

#[cfg(unix)]
mod tty_winsize {
    use std::os::unix::io::AsRawFd;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Once;

    static WINSIZE_CHANGED: AtomicBool = AtomicBool::new(true);
    static HANDLER_INSTALLED: Once = Once::new();

    extern "C" fn handler(_signum: libc::c_int) {
        // AtomicBool::store is async-signal-safe.  No allocation,
        // no Mutex, no formatting — just flip the flag.
        WINSIZE_CHANGED.store(true, Ordering::SeqCst);
    }

    pub fn install_handler() {
        HANDLER_INSTALLED.call_once(|| unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            // SA_RESTART so an in-flight `read`/`poll` on stdin
            // restarts after the handler runs.
            sa.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGWINCH, &sa, std::ptr::null_mut());
            // Pre-seed the flag (already true) — first event-loop
            // iteration picks up the initial size for matrix realise.
            WINSIZE_CHANGED.store(true, Ordering::SeqCst);
        });
    }

    pub fn handler_installed_p() -> bool {
        HANDLER_INSTALLED.is_completed()
    }

    /// Race-free claim: returns whether the flag was set, and resets
    /// it.  The event loop calls this once per iteration and acts
    /// only when the return is true.
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

// Doc 51 Track Q (2026-05-04) — SIGTSTP / SIGCONT (Ctrl+Z / fg).
//
// Pressing Ctrl+Z while the terminal is in raw mode is a multi-step
// dance:
//   1. SIGTSTP is delivered.  The handler must restore termios
//      to "cooked" so the parent shell does not inherit raw mode,
//      then re-raise SIGTSTP with the default disposition (= the
//      kernel actually suspends the process).
//   2. The user types `fg`.  SIGCONT arrives.  The handler must
//      flip a flag so the event loop knows to re-enter raw mode
//      and trigger a full redraw.
//
// This re-uses Track K's `tty_raw` storage for the saved termios
// (= the same termios we'd use for a clean shutdown).  The handler
// pair below extends that contract.

#[cfg(unix)]
mod tty_jobctrl {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Once;

    static SIGCONT_ARRIVED: AtomicBool = AtomicBool::new(false);
    static HANDLER_INSTALLED: Once = Once::new();

    extern "C" fn tstp_handler(signum: libc::c_int) {
        // Restore cooked termios before suspending — the user's
        // shell must not inherit a raw tty.  Track K's
        // `restore_termios_signal_safe` is the right primitive, but
        // it *clears* the saved-state flag.  We need the saved
        // termios to STAY available so SIGCONT can re-apply raw
        // mode.  Trick: read the termios via tcgetattr from the
        // current state (cooked target = whatever we saved before
        // raw-enter, which lives in tty_raw::SAVED_TERMIOS) by
        // calling tty_raw::raw_mode_leave-equivalent inline.
        //
        // For simplicity and because TSTP is rare, we just call
        // tty_raw::raw_mode_leave().  The CONT handler re-enters
        // raw mode through the normal Lisp path, which re-saves
        // the (now cooked) termios.
        let _ = super::tty_raw::raw_mode_leave();
        unsafe {
            // Reset to default + unblock + re-raise so the kernel
            // actually stops us.
            libc::signal(signum, libc::SIG_DFL);
            let mut mask: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&mut mask);
            libc::sigaddset(&mut mask, signum);
            libc::sigprocmask(libc::SIG_UNBLOCK, &mask, std::ptr::null_mut());
            libc::raise(signum);
            // After we resume (= SIGCONT delivered + this handler
            // returns), re-install ourselves (libc::signal reset
            // SIG_DFL in some impls).  Best-effort.
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = tstp_handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            sa.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGTSTP, &sa, std::ptr::null_mut());
        }
    }

    extern "C" fn cont_handler(_signum: libc::c_int) {
        // Just flip the flag; the event loop picks it up and
        // re-enters raw mode on the Lisp side.
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

/// `(read-stdin-byte-available &optional TIMEOUT-MS)' — non-blocking
/// 1-byte read with optional timeout.  Returns:
///   - integer 0..255 when a byte is available
///   - nil when no input arrived within TIMEOUT-MS (default 0)
/// On EOF returns nil (= same as timeout, indistinguishable in MVP).
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

/// Doc 51 Track K — test helper.  Returns t if the raw-mode flag is
/// currently held (= a `terminal-raw-mode-enter` is pending a leave).
/// Always nil on non-Unix.
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

/// Doc 51 Track K — test helper.  Returns t once the atexit + signal
/// hooks have been installed (= after the first `terminal-raw-mode-enter`
/// in this process).  Hooks are install-once per process and cannot be
/// uninstalled, so this is monotonic.
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

/// Doc 51 Track M — `(set-quit-flag)' marks the process-wide quit
/// flag.  The next call to [`crate::eval::eval`] will convert the
/// flag into `EvalError::Quit`.  Used by the C-g key dispatch and
/// any external interrupt source that wants to stop the evaluator
/// at the next safe point instead of immediately.
///
/// Doc 117 §117.B (2026-05-18): the atomic transition itself now
/// runs through the Phase 47 elisp object compiled from
/// `lisp/nelisp-cc-bi-quit-flag.el` (= `atomic-compare-exchange'
/// against the `QUIT_FLAG' static surfaced by `nl_quit_flag_ptr').
/// The Rust side keeps arity validation + the `Sexp::T' return; no
/// algorithmic Rust line survives the swap.
fn bi_set_quit_flag(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("set-quit-flag", args, 0, Some(0))?;
    unsafe {
        crate::elisp_cc_spike::bi_set_quit_flag(quit::nl_quit_flag_ptr());
    }
    Ok(Sexp::T)
}

/// Doc 51 Track M — `(clear-quit-flag)' resets the flag without
/// raising.  Useful in tests and for flushing a stale flag after
/// the user dismisses an unrelated condition.
///
/// Doc 117 §117.B (2026-05-18): elisp-cc swap, see `bi_set_quit_flag'
/// for the dispatch convention.
fn bi_clear_quit_flag(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("clear-quit-flag", args, 0, Some(0))?;
    unsafe {
        crate::elisp_cc_spike::bi_clear_quit_flag(quit::nl_quit_flag_ptr());
    }
    Ok(Sexp::T)
}

/// Doc 51 Track M — `(quit-flag-pending-p)' returns t if the flag
/// is currently set, nil otherwise.  Read-only — does NOT clear.
///
/// Doc 117 §117.B (2026-05-18): elisp-cc swap.  The raw `ptr-read-u64'
/// against the `QUIT_FLAG' slot lives in `lisp/nelisp-cc-bi-quit-flag.el';
/// the Rust side here is the arity check + 0/non-zero → Nil/T mapping.
fn bi_quit_flag_pending_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("quit-flag-pending-p", args, 0, Some(0))?;
    let raw = unsafe {
        crate::elisp_cc_spike::bi_quit_flag_pending_p(quit::nl_quit_flag_ptr())
    };
    Ok(if raw != 0 { Sexp::T } else { Sexp::Nil })
}

/// Doc 51 Track M — install a SIGINT handler that flips the
/// process-wide quit flag instead of terminating.  Idempotent;
/// repeated calls are a no-op.  Returns t.  No-op (returns t) on
/// non-Unix.
fn bi_install_sigint_handler(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("install-sigint-handler", args, 0, Some(0))?;
    quit::install_sigint_handler();
    Ok(Sexp::T)
}

/// Doc 51 Track M — test/diagnostic helper.  Returns t once a
/// SIGINT → quit-flag handler has been installed.  Always nil on
/// non-Unix.
fn bi_sigint_handler_installed_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("_sigint-handler-installed-p", args, 0, Some(0))?;
    Ok(if quit::sigint_handler_installed_p() { Sexp::T } else { Sexp::Nil })
}

/// Doc 51 Track P — install a SIGWINCH handler that flips the
/// resize-pending flag.  Idempotent; pre-seeds the flag so the
/// first event-loop iteration realises the initial geometry.
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

/// Doc 51 Track P — race-free claim of the resize-pending flag.
/// Returns t if a SIGWINCH (or initial-startup seed) is pending,
/// nil otherwise.  Clears the flag in the same step.
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

/// Doc 51 Track P — return the controlling tty's current size as
/// `(COLS . ROWS)' (= integers), or nil if `ioctl(TIOCGWINSZ)`
/// fails (e.g. stdin is not a tty).
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

/// Doc 51 Track Q — install SIGTSTP / SIGCONT handlers so Ctrl+Z
/// suspends cleanly (= termios restored before suspend) and `fg`
/// triggers re-enter of raw mode + redraw.  Idempotent.
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

/// Doc 51 Track Q — race-free claim of the SIGCONT-arrived flag.
/// Returns t if SIGCONT was received since the last claim (= we
/// just resumed from suspension), nil otherwise.
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

/// `(read &optional STREAM)' — parse one elisp form.  STREAM may be
/// a string (= read from it), a marker / buffer / function (= NYI),
/// or absent (= NYI: would read from stdin).
///
/// Phase 7 Stage 7.6.a (Doc 71 §3.1): mandatory delegation to elisp
/// `nelisp--read-from-string-impl' for the string case, matching the
/// Stage 7.2.d retirement of `bi_read_from_string''s Rust fallback.
/// `read-from-string-impl' returns `(FORM . CONSUMED-END)'; `read'
/// returns just FORM, so we take `car' of the result.
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

/// `(read-from-string STRING &optional START END)' — dispatches to
/// the elisp impl `nelisp--read-from-string-impl' bundled in
/// `lisp/nelisp-stdlib-reader.el' (loaded at bootstrap as the last
/// stdlib file).  Stage 7.2.d (Doc 66): the Rust `read_str' fallback
/// previously used here has been retired — elisp impl is mandatory.
/// Bootstrap-time form parsing still uses `reader::read_all'
/// directly (separate path); Stage 7.2.e retires the Rust reader
/// entirely.
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
    // Doc 47 Stage 8b — actually try `load' on the feature name when
    // not yet provided AND a `load-path' is configured.  Without
    // `load-path' set, fall back to the pre-Stage-8b marker behaviour
    // (silently provide the feature) so callers driving the evaluator
    // without file context (= `eval_str_all', most cargo tests) keep
    // working.  The multi-file driver `eval_str_all_at_path' seeds
    // `load-path' so this branch fires there.
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
    // Rust-min batch 7f: dispatch to elisp `load' through the function
    // cell so user-level `(defalias 'load ...)' redefinitions are
    // honoured (the prior direct-call shape couldn't see them).
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

/// Rust-min batch 7i: query the elisp `featurep' through its function
/// cell so any user-level `(defalias 'featurep ...)' is honoured (same
/// rationale as the `bi_require' → elisp `load' dispatch in 7f).
fn elisp_featurep(env: &mut Env, feature: &str) -> Result<bool, EvalError> {
    let fn_cell = env.lookup_function("featurep")?;
    let result = super::apply_function(
        &fn_cell,
        &[Sexp::Symbol(feature.to_string())],
        env,
    )?;
    Ok(is_truthy(&result))
}

/// Rust-min batch 7i: same dispatch shape as `elisp_featurep' for the
/// auto-provide branch in `bi_require' (= "no load-path / no filename"
/// fallback that marks the feature provided without actually loading
/// anything — Phase 8.0.2 no-file-IO bootstrap contract).
fn elisp_provide(env: &mut Env, feature: &str) -> Result<(), EvalError> {
    let fn_cell = env.lookup_function("provide")?;
    super::apply_function(
        &fn_cell,
        &[Sexp::Symbol(feature.to_string())],
        env,
    )?;
    Ok(())
}

// ===== Vector / generic accessor helpers (Phase 8.x core) =====

/// Doc 117 §117.A.1 (2026-05-17): the `(make-vector N INIT)' body
/// now routes through the Phase 47-compiled `nelisp_bi_make_vector'
/// function in `lisp/nelisp-cc-bi-make-vector.el'.  The Rust side
/// here is a thin arity/range-check + caller-owned-slot setup; the
/// allocation (`vector-make' §115.1) and fill loop (`vector-slot-set'
/// §111.E, recursive helper) live only in elisp.  No Rust
/// algorithmic line survives the swap.
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
