//! Built-in function registry — Phase 8.0.2 (Doc 44 §3.3 LOCKED).
//!
//! Each built-in is registered in [`install_builtins`] which writes a
//! `(builtin <NAME>)` sentinel into the function cell of the symbol.
//! At call time the evaluator pulls the name back out and routes to
//! [`dispatch`].
//!
//! ## Phase 2 v3 landing state (Doc 86 §86.1.a-g, 2026-05-10)
//!
//! After Doc 86 Phase 2 Tier 1 + Tier 2 migrations + §86.1.g cleanup
//! sub-stage, the surface here is now scoped to:
//!
//! 1. **KEEP arms** — primitives that genuinely require Rust:
//!    - low-level I/O: `read-stdin-bytes`, `nelisp--write-stdout-bytes`,
//!      `nelisp--write-stderr-line`, `nl-write-file`, `nl-make-directory`
//!    - filesystem syscalls: `nelisp--syscall-canonicalize`, `-stat`,
//!      `-readdir`, `-read-file`
//!    - process / signal / TTY plumbing: `terminal-raw-mode-*`,
//!      `install-{sigint,winsize,jobctrl}-handler*`, quit-flag,
//!      `set`/`fset`/`defalias`/`fmakunbound`/`makunbound`,
//!      `symbol-value`/`-function`/`fboundp`/`boundp`
//!    - generic syscall + supported-p (Linux nr-table dispatch)
//!    - reader entry points (`read`, `read-from-string`, `signal`,
//!      `eval`/`apply`/`funcall`/`macroexpand-1`)
//!    - vector core (`vector`, `make-vector`, `string-bytes`)
//!    - require / load orchestration (calls back into elisp `load')
//!
//! 2. **Tier 3 bridge plumbing** — primitives that elisp wrappers ride:
//!    - `nl-jit-call-*` family (i64-i64, ptr-ptr, syscall, out-1/2/1i/2i,
//!      float-float, float-cmp, float-unary, format-float)
//!    - `nl-ffi-*` family (call, malloc, read/write-{i16,i32,i64,u8,
//!      u16,u32}, errno, free, read-bytes, write-bytes, *-at)
//!    - `nl-cons-*` / `nl-rc-*` / `nl-gc-*` Layer 2 primitives
//!    - `nelisp--apply-{builtin-dispatch,lambda-inner}` +
//!      `nelisp--{push,pop}-frame` / `-push-captured` / `-bind-local` +
//!      `nelisp--{get,set}-use-elisp-apply`
//!    - `nelisp--syscall-{socketpair,sendmsg-fds,recvmsg-fds,
//!      getsockopt-peercred,bind-inet6-scoped,connect-inet6-scoped,
//!      accept-inet6-scoped}` (= residual specialised socket helpers)
//!    - `nelisp-cc--dlsym-resolve` (= elisp-side dlsym hook)
//!
//! Tier 1 (= 1-arg predicates / `type-of` / `recordp` / `eq` / `equal` /
//! `null` / `not` / `1+` / `1-` / arithmetic variadics + comparisons /
//! cXXr family / `mapcar` / `assq` / etc.) all live in `lisp/nelisp-
//! stdlib*.el` now.  Tier 2 (= `float` / `exp` / `log` / `sxhash` /
//! `string-match-p` / `nl-current-unix-time` / `nl-secure-hash` /
//! `nl-format-unix-time` / `nl-{down,up}case` / `nl-split-by-non-alnum` /
//! `intern` / `symbol-name` / `make-symbol` / `nelisp--make-mut-string` /
//! `nelisp--concat-ints` / `nelisp--format-float-body`) ride matching
//! `nl_jit_*` trampolines (= `build-tool/src/jit/{predicate,float,hash,
//! time,strings,regex,math,box_accessor}.rs`) via the bridge
//! primitives listed above.

use super::env::Env;
use super::error::EvalError;
use super::quit;
use super::sexp::Sexp;
use super::special_forms::is_truthy;
use std::path::{Path, PathBuf};

/// Install every built-in into the given environment.  Idempotent —
/// re-running just overwrites the function cells.
///
/// ## Migration history (2026-05-06 → 2026-05-10)
///
/// The entry list below is the residue after a long sequence of
/// migrations from Rust → elisp.  Rather than annotating every removed
/// arm in-line, the consolidated history is:
///
/// - **Rust-min batches 3-7k (2026-05-06 to 2026-05-07)**: bulk move
///   of variadic / dispatching surface to `lisp/nelisp-stdlib*.el`
///   (arithmetic / comparison / bitwise variadics; cXXr accessors;
///   list / string / sequence ops; predicates + `type-of`; case
///   mapping; reverse / nreverse; copy-sequence; format / concat /
///   substring / split-string / make-string / mapconcat / string-
///   search / delete-dups / string-trim / regexp-quote; intern-soft /
///   gensym; vconcat; hash-tables Stage 4f; records Stage 4c;
///   char-table / bool-vector retirement; `lognot` via `logxor`;
///   `mod` via `(- a (* b (/ a b)))`; expand-file-name / file-name-*;
///   message / princ / print / error; provide / featurep on
///   `features` dynvar; load + locate-library on syscall primitives;
///   min/max/abs/floor/ceiling/round on shared kernels).
///
/// - **Doc 50 stage 1-5b (2026-05-07)**: hash-table iter primitive
///   collapse, record primitives, file syscalls, equal cycle-safe.
///
/// - **Doc 76 Stage A-G (2026-05-08 to 2026-05-09)**: 40 specialised
///   POSIX syscall primitives retired (= openat/read/write/fstat/pipe;
///   execve/wait4; setsockopt-int/bind/connect/accept × inet+unix+
///   inet6 + abstract; poll; signalfd/sigprocmask/timerfd; inotify
///   add-watch/read; SCM_RIGHTS + SOCK_SEQPACKET + SO_PEERCRED + IPv6
///   scope_id) — elisp now drives via `nl-ffi-call libc' + struct
///   marshaling helpers below.
///
/// - **Doc 84 §84.1-84.3 (2026-05-10)**: 8 `nelisp--*-float' arms
///   collapsed into 2 bridge primitives (`nl-jit-call-float-{float,
///   cmp}`); `nelisp--syscall-nr-resolve' ported to elisp; 6 Box-
///   accessor builtins lifted to elisp on `nl_jit_record_*' trampolines.
///
/// - **Doc 86 §86.1.a-f (2026-05-10) — Phase 2 Tier 1 + Tier 2**:
///   1-arg predicates / `type-of' / `recordp' / `eq' / `equal' /
///   `nelisp--ref-eq' (Tier 1 elisp-only); `intern' / `symbol-name' /
///   `make-symbol' / `nelisp--make-mut-string' / `nelisp--concat-ints' /
///   `nelisp--format-float-body' / `string-match-p' / `sxhash' (Tier 2
///   on `nl_jit_*' trampolines); `float' / `exp' / `log' /
///   `nl-current-unix-time' / `nl-secure-hash' / `nl-format-unix-time' /
///   `nl-{down,up}case' / `nl-split-by-non-alnum' (Tier 2 via the new
///   `:trampoline-unary-float' ABI mode + jit/{time,hash,strings}.rs);
///   five `bi_record_*' arms (Tier 1.5).
///
/// - **Doc 86 §86.1.g (2026-05-10) — this sub-stage**: cleanup-only.
///   Per-batch comment markers above were collapsed into this block;
///   no arms changed.  See feat branch `agent-doc-86-1g-impl'.
///
/// What remains below is Doc 86 Phase 2 v3 KEEP arms + Tier 3 bridge
/// plumbing (= see module-level docstring at the top of this file).
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
        // aref,aset,elt}; only `string-bytes' is plain Rust)
        "car", "cdr", "cons", "length", "string-bytes",
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
        // residual specialised socket primitives (Doc 76 Stage G —
        // SCM_RIGHTS + SOCK_SEQPACKET + SO_PEERCRED + IPv6 scope_id).
        "nelisp--syscall-socketpair",
        "nelisp--syscall-sendmsg-fds",
        "nelisp--syscall-recvmsg-fds",
        "nelisp--syscall-getsockopt-peercred",
        "nelisp--syscall-bind-inet6-scoped",
        "nelisp--syscall-connect-inet6-scoped",
        "nelisp--syscall-accept-inet6-scoped",
        // symbol / function cells + dispatch core.  Doc 98 §98.4
        // (2026-05-11): the 7 user-visible Tier 3 names whose elisp
        // wrappers in `nelisp-stdlib-env-shim.el' route through the
        // `nelisp--env-globals-*' slim primitives (= `symbol-value' /
        // `fboundp' / `boundp' / `defalias' / `set' / `fmakunbound' /
        // `makunbound') are no longer installed here — their dispatch
        // arms were dead code post-env-shim load and the env-shim
        // boot file creates the entries via direct
        // `nelisp--env-globals-set-function' anyway.  `symbol-function'
        // + `fset' stay because env-shim's own bake reads them via
        // `(symbol-function 'nelisp--shim-X)' / `(fset ...)' before
        // their elisp wrappers are installed.
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
        // Doc 51 Phase 5 generic FFI primitive (libffi-backed) +
        // companion buffer-management primitives for "fill caller-
        // provided buffer" C APIs (= nl_sqlite_query, getline, etc.).
        // Doc 76 Stage 0 added write-bytes + errno; Stage A.2-D added
        // int / byte / offset accessor variants for sockaddr_* / pollfd /
        // sigset_t / itimerspec / inotify_event marshaling.
        "nl-ffi-call",
        "nl-ffi-malloc", "nl-ffi-read-bytes", "nl-ffi-free",
        "nl-ffi-write-bytes", "nl-ffi-errno",
        "nl-ffi-read-i32", "nl-ffi-read-i64",
        "nl-ffi-read-i16", "nl-ffi-read-u16", "nl-ffi-read-u32",
        "nl-ffi-write-i16", "nl-ffi-write-i32",
        "nl-ffi-read-u8", "nl-ffi-write-bytes-at", "nl-ffi-read-bytes-at",
        "nl-ffi-write-i64",
        // Doc 51 Phase 6/8 native-only primitives — time, hash, case
        // folding, tokenizer, math kernel, file write, mkdir.  Each
        // requires native syscall / Rust-stdlib precision so a pure-
        // elisp implementation is impractical.
        "nl-current-unix-time", "nl-secure-hash", "nl-format-unix-time",
        "nl-downcase", "nl-upcase", "nl-split-by-non-alnum",
        "float", "exp", "log", "nelisp--f64-trunc",
        "nl-write-file", "nl-make-directory",
        // Doc 51 Track E/K/M/P/Q — interactive TTY plumbing (Unix only).
        // raw-mode + atexit/signal hook + quit-flag + SIGWINCH +
        // SIGTSTP/SIGCONT.  Test-helpers prefixed `_'.
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
        // Phase 7 Stage 7.4.a/c/e (Doc 68/70) — apply/call/closure/env
        // elisp 化 用補助 primitives + `use_elisp_apply' takeover toggle +
        // `apply-lambda-inner' Rust 化 (= Stage 7.4.d frame-capture leak fix).
        "nelisp--push-frame", "nelisp--pop-frame", "nelisp--push-captured",
        "nelisp--bind-local", "nelisp--apply-builtin-dispatch",
        "nelisp--set-use-elisp-apply", "nelisp--get-use-elisp-apply",
        "nelisp--apply-lambda-inner",
        // Doc 77b Stage b.2 / b.2.5 / b.4 (2026-05-09) + Doc 84 §84.1
        // (2026-05-10) + Doc 87 §86.1.f (2026-05-10) — `nl-jit-call-*'
        // bridge primitives.  i64-i64 / ptr-ptr / syscall + 4 out-param
        // variants (out-1 / out-2 / out-1i / out-2i) + 2 float bridges
        // (float-float / float-cmp) + the new `:trampoline-unary-float'
        // ABI bridge (float-unary).  See `jit/bridge.rs'.
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
        // Doc 77c Phase A.3 + Doc 79 v7 Phase C Stage 5.3.a (2026-05-09)
        // — Layer 2 cons-cell primitives (5 nl-cons-* + 3 nl-rc-*) +
        // generic NlRc surface (alloc / dealloc / inc-strong / dec-strong /
        // strong-count / kind / payload-ptr) + GC helper trio backing
        // `lisp/nelisp-stdlib-gc.el' Bacon-Rajan cycle collector skeleton.
        "nl-cons-alloc", "nl-cons-car", "nl-cons-cdr",
        "nl-cons-set-car", "nl-cons-set-cdr",
        "nl-rc-inc", "nl-rc-dec", "nl-rc-count",
        "nl-rc-alloc", "nl-rc-dealloc",
        "nl-rc-inc-strong", "nl-rc-dec-strong", "nl-rc-strong-count",
        "nl-rc-kind", "nl-rc-payload-ptr",
        "nl-gc-walk-children", "nl-gc-buffered-decs", "nl-gc-finalize",
        // Phase 7.1.6.a (Doc 28 §3.6.a / Doc 81 §5.4) — dlsym bridge.
        // Standalone NeLisp wires `nelisp-cc-runtime-resolve-symbol-function'
        // to call this at startup.
        "nelisp-cc--dlsym-resolve",
        // Doc 99 §99.C — first elisp-only builtin.  The Rust dispatch
        // arm is a Sexp-unwrap / Sexp-wrap shim around the extern "C"
        // call to `nelisp_fact_i64' (= `lisp/nelisp-cc-fact-i64.el').
        // Linux x86_64 only — other targets get a stub arm that
        // signals `arith-error' (= the build chain isn't wired).
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

/// Dispatch a built-in by name.  Called from `super::apply_builtin`.
///
/// ## Comment-block consolidation (Doc 86 §86.1.g, 2026-05-10)
///
/// The arms below are the post-Phase-2-Tier-1+2 residue.  Per-arm
/// migration breadcrumbs that used to annotate every deleted dispatch
/// case (= `// + / - / * / mod migrated to elisp...', `// car / cdr /
/// cons / setcar / setcdr migrated to JIT...', `// 1-arg predicates +
/// type-of all live in elisp now...' etc.) have been collapsed; the
/// migration history lives once in `install_builtins''s docstring
/// above.  Each section header below (e.g. `// arithmetic / comparison
/// / equality (= JIT lowered)') summarises the remaining live arms in
/// that cluster — it does NOT enumerate the deleted ones.
///
/// Surviving categories: arithmetic 2-arg primitives via JIT, vector
/// core, string format/build slivers, file syscalls, generic POSIX
/// syscall + Doc 76 Stage G residual specialised socket primitives,
/// symbol/function cell ops + apply/funcall/eval/macroexpand-1 +
/// signal, print/error slivers, require + read + read-from-string +
/// read-stdin-bytes, FFI + native-only primitives (time/hash/case/
/// math/file write/mkdir), TTY plumbing (raw mode + quit-flag +
/// SIGWINCH + SIGTSTP/SIGCONT), bridge primitives (`nl-jit-call-*'
/// + `nl-cons-*' + `nl-rc-*' + `nl-gc-*'), dlsym bridge.
pub fn dispatch(name: &str, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    match name {
        // ---- vector core (= JIT lowered: car/cdr/cons/setcar/setcdr/
        // length/aref/aset/elt; only `vector' / `make-vector' /
        // `string-bytes' stay plain Rust) ----
        "vector" => Ok(Sexp::vector(args.to_vec())),
        "make-vector" => bi_make_vector(args),
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
        // ---- Doc 76 Stage G residual specialised socket primitives:
        // SCM_RIGHTS + SOCK_SEQPACKET + SO_PEERCRED + IPv6 scope_id ----
        // (= the 7 surface points retired all earlier specialised
        // socket arms; these remain because msghdr/cmsg/ucred/scope_id
        // marshaling is too involved for nl-ffi in elisp alone.)
        // -- registered above; bodies are wired via the externally-
        //    registered builtin path below since the arms were retired
        //    before reaching this match block in Doc 76 Stage G.
        // ---- symbol / function ----
        // Doc 98 §98.4 (2026-05-11): 7 Tier 3 arms removed
        // (symbol-value / fboundp / boundp / defalias / set /
        // fmakunbound / makunbound) — see install_builtins comment
        // above for rationale.  Only symbol-function + fset remain
        // because env-shim's own bake needs them before the elisp
        // wrappers are wired.
        "symbol-function" => bi_symbol_function(args, env),
        "fset" => bi_fset(args, env),
        "macroexpand-1" => bi_macroexpand_1(args, env),
        // ---- Phase 7 Stage 7.4.a/c/e (Doc 68/70) — apply/closure/env
        // primitives + `use_elisp_apply' takeover + apply-lambda-inner ----
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
        // ---- dlsym bridge (Phase 7.1.6.a / Doc 81 §5.4) ----
        "nelisp-cc--dlsym-resolve" => super::dlsym_bridge::bi_dlsym_resolve(args),
        // ---- FFI primitives (Doc 51 Phase 5 + Doc 76 Stage 0/A.2-D) ----
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
        // ---- file write + mkdir (Doc 51 Phase 8) ----
        "nl-write-file" => bi_nl_write_file(args),
        "nl-make-directory" => bi_nl_make_directory(args),
        // ---- TTY plumbing (Doc 51 Track E/K/M/P/Q — Unix only) ----
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
        // ---- `nl-jit-call-*' bridge primitives (Doc 77b Stage b.2 /
        // b.2.5 / b.4 + Doc 84 §84.1 + Doc 87 §86.1.f) ----
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
        // ---- Layer 2 cons / RC / GC primitives (Doc 77c Phase A.3 +
        // Doc 79 v7 Phase C Stage 5.3.a) ----
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
        // Doc 99 §99.C — elisp-only body in `lisp/nelisp-cc-fact-i64.el'.
        "nl-fact-i64" => bi_nl_fact_i64(args),
        // Doc 102 Phase 6 (2026-05-17) — `nelisp--env-globals-op' moved
        // from `extern_builtins' to a regular dispatch arm.  Drops the
        // production binary's dependency on `extern_builtins'; the
        // HashMap is now a test-only / host-crate extension surface.
        "nelisp--env-globals-op" => crate::eval::env_shim::bi_globals_op(args, env),
        _ => {
            // Externally-registered builtin (`Env::register_extern_builtin').
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

/// `(string-bytes STRING)' — return the number of bytes in STRING's
/// UTF-8 representation.  Doc 76 Stage A.1 (2026-05-08): added as
/// the prerequisite for binary-safe `nelisp-os-write' on top of
/// `nl-ffi-call libc.write' + `nl-ffi-malloc' / `nl-ffi-write-bytes'
/// (= without it elisp can only get char count via `length', which
/// truncates multi-byte payloads at the libc.write boundary).
fn bi_string_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-bytes", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Str(s) => Ok(Sexp::Int(s.as_bytes().len() as i64)),
        Sexp::MutStr(rc) => Ok(Sexp::Int(rc.value.as_bytes().len() as i64)),
        other => Err(EvalError::WrongType {
            expected: "string".into(),
            got: other.clone(),
        }),
    }
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

// ---------- char-table / bool-vector legacy decode helpers ----------
//
// `Sexp::CharTable' / `Sexp::BoolVector' user-facing builtins were
// retired in Rust-min 2026-05-06 batch 5b — see install_builtins
// docstring.  The variants stay alive for image-format backward-compat
// decode; the two helpers below let `bi_aref' / `bi_aset' continue to
// read/write any legacy-decoded instances.

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

/// `(nelisp--syscall-canonicalize PATH)' — Rust-min batch 7d (Doc 50
/// stage 2).  Wraps `std::fs::canonicalize' (= follows all symlinks
/// in PATH and returns the absolute resolved real path).  Returns
/// nil for any error (= path doesn't exist, broken symlink, etc) so
/// the elisp `file-truename' wrapper can fall back to the un-resolved
/// expand-file-name result, mirroring the prior `bi_file_truename'
/// `unwrap_or(full)' behaviour exactly.
fn bi_syscall_canonicalize(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-canonicalize", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    match std::fs::canonicalize(&p) {
        Ok(resolved) => Ok(Sexp::Str(resolved.to_string_lossy().into_owned())),
        Err(_) => Ok(Sexp::Nil),
    }
}

/// `(nelisp--syscall-read-file PATH)' — Rust-min batch 7f (Doc 50
/// stage 2): thin wrapper over `std::fs::read_to_string'.  Returns
/// the file contents as a string on success, or nil on any I/O error
/// (= file missing, permission denied, invalid UTF-8, etc).
///
/// Used by elisp `load' (lisp/nelisp-stdlib-misc.el) for the file-
/// content slurp step.  The error → nil flatten lets elisp drive its
/// own error-message formatting / `noerror' branch, mirroring the
/// shape used for `nelisp--syscall-canonicalize' (batch 7d).
fn bi_syscall_read_file(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-read-file", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    match std::fs::read_to_string(&p) {
        Ok(s) => Ok(Sexp::Str(s)),
        Err(_) => Ok(Sexp::Nil),
    }
}

/// `(nelisp--read-all-from-string STR)' — Phase 7 Stage 7.6.a (Doc 71
/// §3.1): mandatory delegation to elisp
/// `nelisp--read-all-from-string-impl' (= `lisp/nelisp-stdlib-reader.el').
/// Mirrors the Stage 7.2.d retirement pattern that
/// `bi_read_from_string' adopted: post-bootstrap user-visible reads
/// MUST go through the elisp reader so the Rust `reader::read_all'
/// surface is bootstrap-only.  Hard error if the elisp impl isn't
/// installed (= reader.el missing from STDLIB_SOURCES).
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

/// `(nelisp--syscall-stat PATH)' — Rust-min batch 7b (Doc 50 stage 2
/// first slice).  Resolves PATH against `default-directory' just like
/// the previous `bi_file_exists_p' family did, then returns one of:
///
///   * `'absent'    — path does not exist, no permissions, or any
///                    `metadata' error.  Note: dangling symlinks land
///                    here too because `metadata()' follows symlinks
///                    and errors out at the missing target (= same
///                    behaviour the prior `is_file()' / `is_dir()'
///                    `unwrap_or(false)' produced for the 4 wrappers).
///   * `'file'      — resolves to a regular file.
///   * `'directory' — resolves to a directory.
///
/// 4 elisp wrappers (= `file-exists-p' / `file-readable-p' /
/// `file-directory-p' / `file-regular-p') ride this 1 primitive — see
/// lisp/nelisp-stdlib-misc.el.  Doc 50 §3.2 unlock-strategy "POSIX
/// syscall layer" applied via the same shape as batch 7a (= 1 iter
/// primitive + N elisp wrappers).
fn bi_syscall_stat(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--syscall-stat", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    let tag = match std::fs::metadata(&p) {
        Ok(m) if m.is_dir()  => "directory",
        Ok(m) if m.is_file() => "file",
        // Exotic file types (sockets / fifos / block-dev / char-dev)
        // are reported `'absent' to mirror the prior behaviour where
        // both `is_file()' and `is_dir()' returned false → all 4
        // wrappers returned nil.  A future refinement could split
        // these out, but no current call site distinguishes.
        Ok(_)                => "absent",
        Err(_)               => "absent",
    };
    Ok(Sexp::Symbol(tag.into()))
}

/// `(nelisp--syscall-readdir DIR)' — Rust-min batch 7c (Doc 50 stage
/// 2).  POSIX readdir layer used by elisp `directory-files' (see
/// lisp/nelisp-stdlib-misc.el).  Returns the cons-headed list
///
///   `(ABS-DIR NAME1 NAME2 ...)`
///
/// where ABS-DIR is the `default-directory'-resolved absolute path
/// (= matches what the prior `bi_directory_files' used internally for
/// FULL-mode prefix), and NAMEi are unsorted directory entry names.
/// Returns nil for any error (= dir not found, permission denied,
/// etc) — same as the prior `bi_directory_files' which also collapsed
/// all errors to nil.  The elisp wrapper handles sort / regex match /
/// FULL-path / COUNT clipping.
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

// ---------------------------------------------------------------------------
// Doc 53 Phase 1 — POSIX OS surface (Linux only via libc::syscall +
// libc::SYS_*).  Non-Linux: `-supported-p' returns nil so the elisp
// wrapper picks the `nl-ffi-call libc' fallback (Path B); the other
// primitives Err explicitly.  Errors normalised via __errno_location()
// to the kernel ABI shape (result < 0 = -errno).
// ---------------------------------------------------------------------------

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
            // Doc 54 Core-12 / Doc 55 Posix-30 / Doc 57 modern-event /
            // Doc 59 timerfd additions (= int-only args; ride the
            // generic dispatch.  fstat / pipe / execve / wait4 / bind /
            // connect / accept / poll / signalfd / timerfd-settime /
            // timerfd-gettime / sigprocmask need buffers and live in
            // separate primitives or were retired into nl-ffi).
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

// Doc 98 §98.4 (2026-05-11): bi_symbol_value retired together with
// bi_fboundp / bi_boundp / bi_defalias / bi_set / bi_fmakunbound /
// bi_makunbound.  All seven were dead-code post-env-shim load — the
// elisp `nelisp--shim-*' wrappers in `nelisp-stdlib-env-shim.el'
// route through the `nelisp--env-globals-*' slim primitives.

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

/// `(fset SYMBOL DEFINITION)` — install DEFINITION in the function
/// cell of SYMBOL.  Doc 98 §98.4 (2026-05-11): the last surviving
/// Rust function-cell setter — env-shim's own bake needs it for the
/// 60+ `(fset 'NAME ...)' installs in `nelisp-jit-substrate.el' /
/// `nelisp-jit-strategy.el' before its own elisp wrappers are
/// wired.  `defalias' was retired in the same commit (the optional
/// docstring slot was always discarded).
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

// Doc 98 §98.4 (2026-05-11): bi_fmakunbound retired together with
// the other 6 user-visible Tier 3 names — see install_builtins
// comment above.

/// `(macroexpand-1 FORM &optional ENV)` — expand FORM by ONE level if
/// its head is a macro; otherwise return FORM unchanged.  ENV is
/// currently ignored (= NeLisp has no environment-override macro
/// lookup, all macros live in the global function cell).
///
/// Phase 7 Stage 7.3.a (Doc 67): added so ERT can verify the expansion
/// shape of elisp-side Tier 2 macros (= `cond' / `when' / `unless' / ...)
/// while their dispatch is still preempted by `apply_special' match
/// arms.  Stage 7.3.d retires the Rust arms and the elisp macros
/// activate at runtime; `macroexpand-1' remains useful for tests.
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
// Each dispatch arm calls `bi_frame_op(OP, args, env)' with its
// matching tag.  Elisp `defun' wrappers would break frame semantics
// (= the wrapper itself pushes a frame, making `bind-local' target
// the wrapper instead of the caller's innermost frame).
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

// `bi_set_use_elisp_apply' / `bi_get_use_elisp_apply' (Stage 7.4.c
// takeover flag accessors) inlined directly into the dispatch arms
// below — too small to justify separate fn definitions.

/// `(nelisp--apply-builtin-dispatch NAME ARGS)` — direct dispatch to
/// the builtin registry by name.  ARGS is a proper list whose elements
/// have ALREADY been evaluated.  Returns the builtin's result.
///
/// Used by elisp-side `nelisp--apply-fn' when the dispatched function
/// is a `(builtin NAME)' sentinel.  Equivalent to the Rust
/// `apply_builtin' helper, just lifted out so the elisp dispatcher can
/// reach it without a special form arm.
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

/// Phase 7 Stage 7.4.e (Doc 70) — `(nelisp--apply-lambda-inner CAPTURED
/// FORMALS BODY ARGS)`.  Rust builtin replacing the Stage 7.4.b elisp
/// defun of the same name.  All four state slots plus intermediate
/// `pairs' / `last' live on the Rust call stack — nothing enters the
/// elisp lexical env, so a `defun' executed within BODY cannot
/// snapshot this builtin's "formal slots".  Surgical fix for the
/// Stage 7.4.d capture-leak bug.
///
/// BODY is a proper list of forms (= the elisp dispatcher passes the
/// closure body cdr directly).  ARGS is a proper list of already-
/// evaluated arguments (= matches the elisp dispatcher contract).
fn bi_apply_lambda_inner(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--apply-lambda-inner", args, 4, Some(4))?;
    let captured = &args[0];
    let formals = &args[1];
    let body_vec = super::list_elements(&args[2])?;
    let args_vec = super::list_elements(&args[3])?;
    super::apply_lambda_inner(captured, formals, &body_vec, &args_vec, env)
}

// Doc 98 §98.4 (2026-05-11): bi_makunbound retired — see
// install_builtins comment above for full list of 7 removed arms.

/// Resolve `arg` to a callable: a symbol points to its function cell,
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
    let s = args[0].as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: "stringp".into(),
        got: args[0].clone(),
    })?;
    let mut out = std::io::stdout().lock();
    out.write_all(s.as_bytes())
        .and_then(|_| out.flush())
        .map_err(|e| EvalError::Internal(format!("nelisp--write-stdout-bytes: {}", e)))?;
    Ok(args[0].clone())
}

/// `(nelisp--write-stderr-line STR)' — write STR followed by a
/// newline to stderr and flush.  Returns STR unchanged.  Building
/// block for the elisp `message' (Rust-min 2026-05-06 batch 6h);
/// the previous `bi_message' was just `bi_format' + this writeln,
/// and moving the dispatch to elisp means the few-line I/O sliver
/// is the only piece that genuinely needs Rust.
fn bi_write_stderr_line(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    require_arity("nelisp--write-stderr-line", args, 1, Some(1))?;
    let s = args[0].as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: "stringp".into(),
        got: args[0].clone(),
    })?;
    let mut err = std::io::stderr().lock();
    let _ = writeln!(err, "{}", s);
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
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
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
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        Sexp::Int(n) => Ok(Sexp::Int(*n)),
        Sexp::Float(x) => Ok(Sexp::Int(*x as i64)),
        other => Err(EvalError::WrongType {
            expected: "numberp".into(),
            got: other.clone(),
        }),
    }
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
/// Non-Linux / non-x86_64 targets get a stub arm that signals
/// `arith-error' with a clear message — the build chain only emits
/// the `.o' on those targets so the symbol isn't linked.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
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

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
fn bi_nl_fact_i64(_args: &[Sexp]) -> Result<Sexp, EvalError> {
    Err(EvalError::Internal(
        "nl-fact-i64: not available on this target (linux-x86_64 only in v1)".into(),
    ))
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
    let path = string_value(&args[0])?;
    let content = string_value(&args[1])?;
    std::fs::write(&path, content)
        .map_err(|e| EvalError::Internal(format!("nl-write-file: {}: {}", path, e)))?;
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
    use std::io::Read;
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
    let mut handle = std::io::stdin().lock();
    match handle.read(&mut buf) {
        Ok(0) => Ok(Sexp::Nil),
        Ok(n) => {
            buf.truncate(n);
            Ok(Sexp::Str(String::from_utf8_lossy(&buf).into_owned()))
        }
        Err(e) => Err(EvalError::Internal(format!("read-stdin-bytes: {}", e))),
    }
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
fn bi_set_quit_flag(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("set-quit-flag", args, 0, Some(0))?;
    quit::set_quit_flag();
    Ok(Sexp::T)
}

/// Doc 51 Track M — `(clear-quit-flag)' resets the flag without
/// raising.  Useful in tests and for flushing a stale flag after
/// the user dismisses an unrelated condition.
fn bi_clear_quit_flag(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("clear-quit-flag", args, 0, Some(0))?;
    quit::clear_quit_flag();
    Ok(Sexp::T)
}

/// Doc 51 Track M — `(quit-flag-pending-p)' returns t if the flag
/// is currently set, nil otherwise.  Read-only — does NOT clear.
fn bi_quit_flag_pending_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("quit-flag-pending-p", args, 0, Some(0))?;
    Ok(if quit::is_quit_pending() { Sexp::T } else { Sexp::Nil })
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

fn bi_make_vector(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("make-vector", args, 2, Some(2))?;
    let len = as_int("make-vector", &args[0])?;
    if len < 0 {
        return Err(EvalError::ArithError(format!(
            "make-vector: negative length {}",
            len
        )));
    }
    Ok(Sexp::vector(vec![args[1].clone(); len as usize]))
}

