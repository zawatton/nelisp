//! Built-in function registry — Phase 8.0.2 (Doc 44 §3.3 LOCKED).
//!
//! Each built-in is registered in [`install_builtins`] which writes a
//! `(builtin <NAME>)` sentinel into the function cell of the symbol.
//! At call time the evaluator pulls the name back out and routes to
//! [`dispatch`].
//!
//! Categories (per prompt §6):
//!   - Arithmetic  : + - * / mod < > <= >= = /=
//!   - Equality    : eq equal
//!   - Cons / list : car cdr cons length append
//!   - Predicates  : consp listp atom symbolp stringp numberp
//!                   integerp floatp
//!   - String      : concat format substring intern symbol-name
//!   - Symbol/func : symbol-value symbol-function fboundp boundp
//!                   funcall apply eval signal error
//!
//! Sweep 9 migrated to Elisp (lisp/nelisp-stdlib*.el):
//!   identity null not 1+ 1- nth nthcdr reverse nreverse
//!   mapcar mapc memq member assq assoc
//!   plist-get plist-put plist-member string-empty-p
//!
//! Sweep 10 migrated to Elisp (lisp/nelisp-stdlib-misc.el):
//!   list alist-get string-prefix-p number-to-string

use super::env::Env;
use super::error::EvalError;
use super::quit;
use super::sexp::Sexp;
use super::special_forms::is_truthy;
use std::path::{Path, PathBuf};

/// Install every built-in into the given environment.  Idempotent —
/// re-running just overwrites the function cells.
pub fn install_builtins(env: &mut Env) {
    let names: &[&str] = &[
        // arithmetic
        // Rust-min (2026-05-06 batch 6l): `mod' migrated to elisp
        // (lisp/nelisp-stdlib.el).
        // Rust-min (2026-05-06 batch 6v): variadic + / - / *
        // migrated to elisp (lisp/nelisp-stdlib.el) on top of new
        // 2-arg primitives.
        // Doc 86 §86.1.b (2026-05-10): `/' migrated to elisp on top
        // of `nl_jit_float_div' (jit/float.rs) — upfront f64 promote
        // + trunc-on-all-int gating live in `lisp/nelisp-stdlib.el'.
        "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
        // Rust-min (2026-05-06 batch 6w): chained-pairwise variadic
        // < / > / <= / >= / = / /= migrated to elisp
        // (lisp/nelisp-stdlib.el).  Float-tolerance epsilon moved
        // into the `nelisp--num-eq2' primitive.
        "nelisp--num-lt2", "nelisp--num-gt2",
        "nelisp--num-le2", "nelisp--num-ge2",
        "nelisp--num-eq2",
        // equality
        // Rust-min (2026-05-06 batch 6e): `eql' / `equal-including-properties'
        // moved to elisp defalias of `equal'.
        // Doc 86 §86.1.b (2026-05-10): `equal' migrated to elisp
        // (already lived in `lisp/nelisp-stdlib-equal.el' since Doc
        // 50 stage 5b — bootstrap shim added in `nelisp-jit-strategy.el'
        // for early load order).  `nelisp--ref-eq' migrated to elisp
        // on top of `nl_jit_ref_eq' trampoline (jit/predicate.rs) via
        // `nl-jit-call-out-2' from `nelisp-jit-strategy.el'.  Both
        // dispatch arms + bi_* helpers + `sexp_equal_safe' deleted.
        "eq",
        // cons / list
        // Rust-min (2026-05-06 batch 6o): `append' migrated to elisp
        // (lisp/nelisp-stdlib-list.el).
        // Rust-min Doc 61 stage 7 (2026-05-07): cXXr accessor family
        // (caar / cadr / cdar / cddr / caaar / caadr / cadar / caddr /
        // cdaar / cdadr / cddar / cdddr / cadddr — 13 names) migrated
        // to elisp (lisp/nelisp-stdlib-list.el) as plain `car'/`cdr'
        // composition.  Only the 2 leaf accessors stay in Rust.
        "car", "cdr", "cons", "length", "string-bytes",
        "setcar", "setcdr",
        // generic sequence / array accessors
        // Rust-min (2026-05-06 batch 6q): `arrayp' / `sequencep'
        // migrated to elisp (lisp/nelisp-stdlib.el).
        "aref", "aset", "elt",
        // Rust-min (2026-05-06 batch 6c): vconcat migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).
        "vector", "make-vector",
        // predicates: all 1-arg predicates + `type-of' migrated to
        // elisp (Rust-min batch 6q/6u/6d/6f + Doc 86 §86.1.a) —
        // `type-of' rides `nl_jit_type_of' (jit/predicate.rs).
        // list ops
        // Rust-min (2026-05-06 batch 6d): `reverse' / `nreverse'
        // shadowed by elisp (lisp/nelisp-stdlib-list.el).  The elisp
        // versions are list-only — vector / string `reverse' was
        // never actually reachable from elisp.
        // Rust-min (2026-05-06 batch 6g): `copy-sequence' partial
        // migration — cons / nil paths in elisp
        // (lisp/nelisp-stdlib-misc.el).
        // Rust-min (2026-05-06 batch 4): sort + copy-tree migrated
        // to elisp (lisp/nelisp-stdlib-plist-str.el).
        // bitwise — required by keymap / event-encoding code
        // Rust-min (2026-05-06 batch 6e): `lsh' moved to elisp
        // defalias of `ash'.
        // Rust-min (2026-05-06 batch 6j): variadic logior / logand /
        // logxor moved to elisp (lisp/nelisp-stdlib.el) folding over
        // the 2-arg primitives `nelisp--logior2' / -logand2 /
        // -logxor2.
        // Rust-min batch 7k (2026-05-07, Doc 65 closing batch):
        // `lognot' migrated to elisp as `(logxor x -1)' on top of
        // -logxor2 (see lisp/nelisp-stdlib.el).  Only `ash' stays
        // native because it needs raw bit-shift with overflow clamping.
        "ash",
        "nelisp--logior2", "nelisp--logand2", "nelisp--logxor2",
        // hashing — used by hash-table key derivation in user code
        // Rust-min (2026-05-06 batch 6e): `sxhash-{equal,eq,eql}'
        // moved to elisp defalias of `sxhash'.
        "sxhash",
        // string
        // Rust-min (2026-05-06 batch 6b): substring migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06 batch 6f): `intern-soft' migrated to
        // elisp (lisp/nelisp-stdlib-misc.el).
        // Rust-min (2026-05-06 batch 6m): `format' migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).  Only the IEEE-754
        // float→string sliver remains as `nelisp--format-float-body';
        // `truncate' is also added so the elisp dispatcher can
        // coerce float→int for `%d/%i' without a privileged cast.
        // Rust-min (2026-05-06 batch 6r): `concat' migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).  Only the
        // "build-Sexp::Str-from-int-list" sliver remains as
        // `nelisp--concat-ints'.
        // Doc 86 §86.1.d (2026-05-10): `intern' / `symbol-name' moved to elisp.
        "nelisp--format-float-body", "truncate",
        "nelisp--concat-ints",
        // Rust-min (2026-05-06 batch 6e): `string=' moved to elisp
        // defalias of `string-equal'.
        // Rust-min (2026-05-06 batch 6n): `string-equal' migrated to
        // elisp (lisp/nelisp-stdlib-plist-str.el).  Dead body removed
        // in batch 6t (resurrected via stash-merge during 6o).
        "string-match-p",
        // Rust-min (2026-05-06): `regexp-quote' migrated to elisp
        // (see lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06 batch 6c): char-to-string / string-to-char
        // / string / unibyte-string migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06 batch 6s): `make-string' migrated to
        // elisp (lisp/nelisp-stdlib-plist-str.el).  Only the
        // build-a-Sexp::MutStr sliver remains as
        // `nelisp--make-mut-string'.
        "nelisp--make-mut-string",
        // Rust-min (2026-05-06 batch 6p): upcase / downcase /
        // capitalize migrated to elisp (ASCII-only case mapping —
        // see lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06 batch 5a): string-to-number migrated
        // to elisp (lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06 batch 6n): split-string migrated to
        // elisp (lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06): string-trim family +
        // string-prefix-p / string-suffix-p migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).
        // Rust-min (2026-05-06 batch 3): mapconcat / string-search /
        // delete-dups migrated to elisp (lisp/nelisp-stdlib-plist-str.el).
        // symbols / sequences
        // gensym / make-symbol migrated to elisp (Rust-min batch 6a + Doc 86 §86.1.d).
        // hash-tables (Track O'')
        // Rust-min (2026-05-06 batch 6k): `hash-table-keys' /
        // `hash-table-values' migrated to elisp via `maphash' fold
        // (lisp/nelisp-stdlib-misc.el).
        // Rust-min (2026-05-07 batch 7a, Doc 50 stage 1): `maphash' +
        // `hash-table-count' migrated to elisp on top of a new low-
        // level iter primitive `nelisp--hash-pairs'.
        // Doc 50 stage 4f (2026-05-07): the remaining 7 hash-table
        // builtins (make-hash-table / hash-table-p / puthash / gethash /
        // remhash / clrhash / nelisp--hash-pairs) migrated to elisp on
        // top of Stage 4c record primitives — `Sexp::HashTable'
        // variant retired alongside.  Hash-table is now `(record
        // 'hash-table TEST ENTRIES)' = first elisp-built data type
        // and proof-of-concept for further container moves.
        // Records (Doc 50 stage 4c, 2026-05-07).  Six low-level
        // primitives that the elisp-side `cl-defstruct' macro
        // (Stage 4e) uses to build constructors / accessors /
        // predicates.  Doc 86 §86.1.a (2026-05-10): `recordp'
        // migrated to elisp — see lisp/nelisp-stdlib.el.
        // Doc 86 §86.1.c (2026-05-10): `nelisp--make-record' /
        // `-record-ref' / `-record-set' / `-record-length' /
        // `-record-type' all migrated to elisp on top of the new
        // `nl_jit_record_*' trampolines in `jit/box_accessor.rs'.
        // See `lisp/nelisp-jit-strategy.el'.
        // Rust-min (2026-05-06 batch 5b): char-table family was
        // unused in NeLisp lisp/ + test/, so the user-facing
        // builtins (make-char-table, char-table-p, char-table-
        // subtype, char-table-parent, set-char-table-parent,
        // set-char-table-range, char-table-extra-slot, set-char-
        // table-extra-slot) were retired wholesale.  bool-vector /
        // bool-vector-p / make-bool-vector migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el) using plain vectors;
        // the `Sexp::BoolVector' variant is kept alive only for
        // legacy image-format decode.
        // file helpers
        // Rust-min batch 7d (2026-05-07, Doc 50 stage 2): expand-
        // file-name migrated to elisp (pure path arithmetic, no new
        // primitive); file-truename migrated to elisp on top of the
        // new `nelisp--syscall-canonicalize' primitive (= POSIX
        // realpath syscall, returns nil on error so the elisp
        // wrapper can fall back to expand-file-name).  See
        // lisp/nelisp-stdlib-misc.el.
        "nelisp--syscall-canonicalize",
        // file I/O (Doc 47 Stage 8b — multi-file load chain)
        // Rust-min batch 7b (2026-05-07, Doc 50 stage 2 first slice):
        // `file-exists-p' / `file-readable-p' / `file-directory-p' /
        // `file-regular-p' migrated to elisp on top of the new
        // `nelisp--syscall-stat' primitive (see
        // lisp/nelisp-stdlib-misc.el).  Same 1+N collapse pattern as
        // batch 7a (hash-table iter).
        "nelisp--syscall-stat",
        // Rust-min batch 7c (2026-05-07, Doc 50 stage 2): pure-string
        // `file-name-extension' migrated to elisp (no new primitive
        // needed); `directory-files' migrated to elisp on top of the
        // new readdir syscall primitive (sort + match-filter + full-
        // path formatting all elisp now).  See
        // lisp/nelisp-stdlib-plist-str.el + lisp/nelisp-stdlib-misc.el.
        "nelisp--syscall-readdir",
        // Rust-min batch 7e (2026-05-07, Doc 50 stage 2): `locate-library'
        // migrated to elisp (load-path walk + suffix probe via
        // `nelisp--syscall-stat').  See lisp/nelisp-stdlib-misc.el.
        // Rust-min batch 7f (2026-05-07, Doc 50 stage 2): `load' itself
        // migrated to elisp on top of two new primitives below
        // (`nelisp--syscall-read-file' = file slurp; `nelisp--read-all-
        // from-string' = reader-loop).  `bi_require' now dispatches to
        // the elisp `load' through the function-cell to honour any
        // user-level redefinition.
        "nelisp--syscall-read-file",
        "nelisp--read-all-from-string",
        // Doc 53 Phase 1 (2026-05-07) — POSIX OS surface generic
        // primitives.  `nelisp--syscall-openat' / `-read' / `-write'
        // were retired in Doc 76 Stage A.1 (2026-05-08); their elisp
        // wrappers now ride `nl-ffi-call libc' directly.
        "nelisp--syscall",
        "nelisp--syscall-supported-p",
        // Doc 54 Phase 3 (2026-05-07) — out-buffer primitives that
        // were retired in Doc 76 Stage A.2/A.3 (2026-05-08).  elisp
        // now decodes struct stat / int[2] from `nl-ffi-malloc' bufs
        // via `nl-ffi-read-i32' / `-i64'.
        // Doc 55 Phase 4 execve / wait4 retired in Doc 76 Stage B
        // (2026-05-08); elisp drives argv/envp char* array + status
        // by-ref via nl-ffi primitives.
        // Doc 55 Phase 4 socket-int (= bind-inet / connect-inet / accept-inet
        // / setsockopt-int / poll) retired in Doc 76 Stage C (2026-05-08);
        // elisp now builds sockaddr_in / pollfd[] via nl-ffi primitives.
        // Doc 56/58 Phase 4.1.x (= AF_UNIX + AF_INET6 sockaddr +
        // abstract namespace + getsockname/peername) retired in Doc 76
        // Stage D (2026-05-08); elisp now drives all 14 wrappers via
        // nl-ffi sockaddr_un / sockaddr_in6 encode/decode helpers.
        // Doc 57 Phase 4.3 (2026-05-07) — modern Linux event surface
        // (inotify path/buffer primitives) retired in Doc 76 Stage E
        // (2026-05-09); elisp now drives inotify_add_watch via
        // nl-ffi-call libc.inotify_add_watch and parses the variable-
        // length inotify_event[] stream via nl-ffi-read-i32/-u32/
        // -bytes-at on a libc.read buffer.  pidfd_* / inotify_init1 /
        // inotify_rm_watch / eventfd2 still ride generic syscall via
        // syscall_nr() symbol map.
        // Doc 59 Phase 4.2 + 4.3.1 signalfd / timerfd / sigprocmask
        // retired in Doc 76 Stage F (2026-05-08); elisp drives sigset_t /
        // itimerspec via libc.sigemptyset / sigaddset / sigismember +
        // nl-ffi-write-i64 + signalfd_siginfo decoder.
        // Doc 60 Phase 4.4 (2026-05-07) — SCM_RIGHTS + SOCK_SEQPACKET +
        // SO_PEERCRED + IPv6 scope_id full surface.
        "nelisp--syscall-socketpair",
        "nelisp--syscall-sendmsg-fds",
        "nelisp--syscall-recvmsg-fds",
        "nelisp--syscall-getsockopt-peercred",
        "nelisp--syscall-bind-inet6-scoped",
        "nelisp--syscall-connect-inet6-scoped",
        "nelisp--syscall-accept-inet6-scoped",
        // Rust-min (2026-05-06): `file-name-directory' /
        // `file-name-nondirectory' / `file-name-as-directory' /
        // `directory-file-name' migrated to elisp (see
        // lisp/nelisp-stdlib-plist-str.el).
        // symbol / function
        "symbol-value", "symbol-function", "fboundp", "boundp", "funcall", "apply", "eval",
        "defalias", "fset", "set", "fmakunbound", "makunbound",
        "macroexpand-1",
        // Rust-min (2026-05-06 batch 6e): `print' moved to elisp
        // defalias of `princ'.
        // Rust-min (2026-05-06 batch 6h): `message' moved to elisp
        // (lisp/nelisp-stdlib-misc.el); only the writeln-to-stderr
        // sliver remains as `nelisp--write-stderr-line'.
        // Rust-min (2026-05-06 batch 6i): `princ' moved to elisp
        // (lisp/nelisp-stdlib-misc.el); only the byte-write-to-stdout
        // sliver remains as `nelisp--write-stdout-bytes'.
        // Rust-min (2026-05-06 batch 6m): `error' moved to elisp
        // (lisp/nelisp-stdlib-misc.el) — uses elisp `format' +
        // `signal' so no Rust wrapper is needed any more.
        // Phase 7 Stage 7.1.4 (2026-05-07, Doc 64): `prin1-to-string'
        // migrated to elisp (lisp/nelisp-stdlib-prn.el).  Rust impl
        // had used `format!("{}", x)' on top of `Sexp' Display; the
        // elisp impl re-implements the dispatch in pure-elisp on top
        // of `number-to-string' / `nelisp--prn-string-escaped' /
        // `nelisp--write-stdout-bytes'.  `princ' / `print' (already
        // elisp via batch 6e/6i) now route through the elisp printer
        // automatically because the `prin1-to-string' function-cell
        // is overridden at stdlib load.  `prin1' and `terpri' are
        // also added in the same batch.
        "signal",
        "nelisp--write-stderr-line",
        "nelisp--write-stdout-bytes",
        // Rust-min batch 7i (2026-05-07, Doc 50 stage 2): `provide' /
        // `featurep' migrated to elisp on top of the `features' dynamic
        // var (the canonical source of provided-feature state — see
        // lisp/nelisp-stdlib-misc.el).  `require' stays Rust because it
        // orchestrates load + post-load verify (calls back through
        // `featurep' via the function cell).
        "require",
        // self-process stdio (Phase 9 minimal — needed by stand-alone Lisp servers
        // such as elisp-lsp running on the `nelisp` binary)
        "read-stdin-bytes",
        // Doc 51 Phase 5: single generic FFI primitive (libffi-backed).
        // Bridges any cdylib (sqlite + syscall + future seed crates) to
        // pure-Elisp wrapper packages without per-function dispatch glue.
        "nl-ffi-call",
        // Companion buffer-management primitives for "fill caller-provided
        // buffer" C APIs (= nl_sqlite_query, getline-style readers, etc.).
        "nl-ffi-malloc", "nl-ffi-read-bytes", "nl-ffi-free",
        // Doc 76 Stage 0 (2026-05-08): write-bytes (= struct field
        // poke for sockaddr_in / pollfd / sigset_t / msghdr+SCM_RIGHTS
        // marshaling) + errno (= libc cross-OS thin shim).
        "nl-ffi-write-bytes", "nl-ffi-errno",
        // Doc 76 Stage A.2/A.3 (2026-05-08): primitive int decoders so
        // pipe / fstat wrappers can read int[2] / struct stat fields
        // out of `nl-ffi-malloc' buffers without UTF-8 munging.
        "nl-ffi-read-i32", "nl-ffi-read-i64",
        // Doc 76 Stage C (2026-05-08): primitive int read/write for
        // sockaddr_in / pollfd field marshaling.
        "nl-ffi-read-i16", "nl-ffi-read-u16", "nl-ffi-read-u32",
        "nl-ffi-write-i16", "nl-ffi-write-i32",
        // Doc 76 Stage D (2026-05-08): byte-level + offset variants for
        // sockaddr_un sun_path (= path / abstract namespace) marshaling.
        "nl-ffi-read-u8", "nl-ffi-write-bytes-at", "nl-ffi-read-bytes-at",
        // Doc 76 Stage F (2026-05-08): 64-bit field write for
        // itimerspec encode (= timerfd_settime).
        "nl-ffi-write-i64",
        // Doc 51 Phase 6 write-path: time + cryptographic hash primitives.
        // Needed by anvil-memory-add etc. (= NOT NULL `created' column +
        // body digest).  Both are inherently OS / native-lib dependent
        // — pure-elisp implementations are impractical (SHA-1 in elisp =
        // ~100 LoC slow; current time has no Lisp-level source).
        "nl-current-unix-time", "nl-secure-hash", "nl-format-unix-time",
        // Doc 51 Phase 8: string + Unicode primitives needed by anvil-memory's
        // tokenizer / save-check / FTS query builder.  Real Emacs implements
        // these in C; Rust stdlib gives us correct UTF-8 case folding for free.
        "nl-downcase", "nl-upcase", "nl-split-by-non-alnum",
        // Doc 51 Phase 8: math primitives needed by anvil-memory's decay
        // formula (exp/log/float coercion + rounding).  Rust-min batch 7g
        // (2026-05-07): `min' / `max' / `abs' migrated to elisp on top of
        // existing chained-pairwise `<' / `>' (= batch 6w 2-arg primitives)
        // — see lisp/nelisp-stdlib.el.  Rust-min batch 7h (2026-05-07):
        // `floor' / `ceiling' / `round' migrated to elisp on top of the
        // unified `nelisp--f64-trunc MODE X DIV' kernel — see
        // lisp/nelisp-stdlib.el.  `float' / `exp' / `log' kept Rust
        // because they require direct f64 ops with no elisp building
        // block of equivalent precision.
        "float", "exp", "log", "nelisp--f64-trunc",
        // Doc 51 Phase 8: file write + mkdir for worklog-export-org write path.
        "nl-write-file", "nl-make-directory",
        // Doc 51 Track E — interactive TTY input (Unix only; no-ops elsewhere)
        "terminal-raw-mode-enter", "terminal-raw-mode-leave",
        "read-stdin-byte-available",
        // Doc 51 Track K — test/debug helpers for the atexit/signal hook
        "_termios-saved-p", "_raw-mode-hooks-installed-p",
        // Doc 51 Track M — process-wide quit-flag plumbing
        "set-quit-flag", "clear-quit-flag", "quit-flag-pending-p",
        "install-sigint-handler", "_sigint-handler-installed-p",
        // Doc 51 Track P — SIGWINCH (terminal resize) plumbing
        "install-winsize-handler", "_winsize-handler-installed-p",
        "terminal-take-winsize-changed", "terminal-current-winsize",
        // Doc 51 Track Q — SIGTSTP / SIGCONT (Ctrl+Z / fg)
        "install-jobctrl-handlers", "_jobctrl-handlers-installed-p",
        "terminal-take-sigcont",
        // reader exposed as elisp callable (Track O' Phase 2)
        "read", "read-from-string",
        // Phase 7 Stage 7.4.a (Doc 68): apply/call/closure/env elisp 化
        // 用補助 primitives.  elisp 側 `nelisp--apply-fn' 等が Rust frame
        // stack を操作するための薄いラッパ.
        "nelisp--push-frame", "nelisp--pop-frame", "nelisp--push-captured",
        "nelisp--bind-local", "nelisp--apply-builtin-dispatch",
        // Phase 7 Stage 7.4.c (Doc 68 §2.7) — `use_elisp_apply' takeover
        // flag の runtime toggle.  ERT 用.
        "nelisp--set-use-elisp-apply", "nelisp--get-use-elisp-apply",
        // Phase 7 Stage 7.4.e (Doc 70): apply-lambda-inner Rust 化.
        // helper の state slot を elisp lexical env に出さないことで
        // Stage 7.4.d で観測した frame-capture leak を解消.
        "nelisp--apply-lambda-inner",
        // Doc 77b Stage b.2 (2026-05-09) — `nl-jit-call-*' bridge
        // primitives.  elisp wrappers shipped in Stage b.4 will replace
        // the `lowered_X' Rust fns by calling these to invoke JIT
        // entries by name (`nelisp_jit_add2', `nelisp_jit_eq_inline',
        // `nelisp_jit_syscall' etc.).  See `jit/bridge.rs'.
        "nl-jit-call-i64-i64",
        "nl-jit-call-ptr-ptr",
        "nl-jit-call-syscall",
        // Doc 77b Stage b.2.5 (2026-05-09) — out-param trampoline
        // primitives covering the 9 lowered_X fns (5 cons + 4 access)
        // whose JIT entries write the result via `*mut Sexp' out-slot
        // and return TRAMPOLINE_OK / _ERR.  See `jit/bridge.rs'.
        "nl-jit-call-out-1",
        "nl-jit-call-out-2",
        "nl-jit-call-out-1i",
        "nl-jit-call-out-2i",
        // Doc 77b Stage b.4 — helper primitives backing the elisp
        // JIT-strategy wrappers (lisp/nelisp-jit-strategy.el).  Phase
        // 7.1.7.a.1 (Doc 28 §3.7.a.1) removed `nelisp--int-eq-zero' +
        // 3 bitwise -impl + ash-impl entries (= moved to elisp).  Doc
        // 84 §84.1 (2026-05-10) replaced the 8 `nelisp--*-float' arms
        // with the 2 bridge primitives below.  Doc 84 §84.2 ported
        // `nelisp--syscall-nr-resolve' to `lisp/nelisp-syscall-table.el'.
        // Doc 84 §84.3 lifted the 6 Box-accessor builtins to elisp.
        "nl-jit-call-float-float",
        "nl-jit-call-float-cmp",
        // Doc 77c Phase A.3 (2026-05-09) — Layer 2 cons-cell
        // primitives.  Not previously registered here because Phase
        // A.3 only exercised them via Rust-side cargo tests; Doc 79
        // Stage 5.3.a wires elisp consumers (= `nelisp-stdlib-gc.el')
        // so the names must resolve.  See `cons_primitives' for
        // bodies.
        "nl-cons-alloc", "nl-cons-car", "nl-cons-cdr",
        "nl-cons-set-car", "nl-cons-set-cdr",
        "nl-rc-inc", "nl-rc-dec", "nl-rc-count",
        // Doc 79 v7 Phase C Stage 5.3.a (2026-05-09) — generic NlRc
        // primitive set + initial GC helper trio backing the
        // Bacon-Rajan cycle collector skeleton.  See
        // `rc_primitives' for bodies and `nelisp-stdlib-gc.el' for
        // the elisp consumer.
        "nl-rc-alloc", "nl-rc-dealloc",
        "nl-rc-inc-strong", "nl-rc-dec-strong", "nl-rc-strong-count",
        "nl-rc-kind", "nl-rc-payload-ptr",
        "nl-gc-walk-children", "nl-gc-buffered-decs", "nl-gc-finalize",
        // Phase 7.1.6.a (Doc 28 §3.6.a / Doc 81 §5.4) — dlsym bridge.
        // The elisp-side `nelisp-cc-runtime-resolve-symbol' contract
        // delegates to this primitive when the override hook is wired
        // (= standalone NeLisp installs the wiring at startup).  See
        // `eval/dlsym_bridge.rs' for the body.
        "nelisp-cc--dlsym-resolve",
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
pub fn dispatch(name: &str, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    match name {
        // ---- arithmetic ----
        // + / - / * / mod migrated to elisp (batch 6v / 6l).
        // / migrated to elisp via `nl_jit_float_div' (Doc 86 §86.1.b).
        // nelisp--add2/sub2/mul2 + nelisp--num-{eq,lt,gt,le,ge}2
        // migrated to JIT-only path (Phase 5 Stage 5.7, Doc 62).
        // See `jit/arith.rs::lowered_*' / `jit/float.rs::nl_jit_float_*'.
        // < / > / <= / >= / = / /= migrated to elisp (Rust-min
        // 2026-05-06 batch 6w, see lisp/nelisp-stdlib.el).
        // ---- equality ----
        // Rust-min (2026-05-06 batch 6e): `equal-including-properties'
        // / `eql' moved to elisp defalias of `equal'.  The MVP impl
        // collapses both into the strict structural-equality path, so
        // the dispatch fanout was a typing burden with no runtime
        // distinction.
        // `eq' migrated to JIT (Phase 5 Stage C-Phase1, Doc 62
        // 2026-05-08) — `lowered_eq' in `jit/predicate.rs' is the
        // single source of truth.
        // `equal' migrated to elisp (Doc 50 stage 5b — see
        // lisp/nelisp-stdlib-equal.el).  Doc 86 §86.1.b (2026-05-10):
        // dead Rust dispatch arm `bi_equal' + `sexp_equal_safe'
        // helper deleted (= the elisp function-cell override
        // displaces the dispatch path before any caller reaches it).
        // `nelisp--ref-eq' migrated to elisp (Doc 86 §86.1.b) on top
        // of the new `nl_jit_ref_eq' trampoline (= jit/predicate.rs);
        // dispatch arm + `bi_ref_eq' helper deleted, retained
        // `sexp_eq' helper because the trampoline body delegates to it.
        // ---- cons / list ----
        // car / cdr / cons / setcar / setcdr migrated to JIT
        // (Phase 5 Stage C-Phase1, Doc 62 2026-05-08).  See
        // `jit/cons.rs::lowered_{car,cdr,cons,setcar,setcdr}'.
        // length / aref / aset / elt migrated to JIT (Phase 5 Stage
        // C-Phase1b, Doc 62 2026-05-08).  See `jit/access.rs::
        // lowered_{length,aref,aset,elt}'.
        // Common compositions (cXXr family) migrated to elisp in
        // Doc 61 stage 7 — see lisp/nelisp-stdlib-list.el.
        // reverse / nreverse migrated to elisp (Rust-min 2026-05-06
        // batch 6d, see lisp/nelisp-stdlib-list.el).
        // copy-sequence migrated to elisp (Rust-min 2026-05-06
        // batch 6g, see lisp/nelisp-stdlib-misc.el).
        // copy-tree / sort migrated to elisp (Rust-min 2026-05-06 batch 4).
        // append migrated to elisp (Rust-min 2026-05-06 batch 6o,
        // see lisp/nelisp-stdlib-list.el).
        // ---- generic accessors ----
        // aref / aset / elt migrated to JIT (Phase 5 Stage C-Phase1b,
        // Doc 62 2026-05-08).  See `jit/access.rs::lowered_*'.
        // arrayp / sequencep migrated to elisp (Rust-min 2026-05-06
        // batch 6q, see lisp/nelisp-stdlib.el).
        "vector" => Ok(Sexp::vector(args.to_vec())),
        "make-vector" => bi_make_vector(args),
        // ---- predicates ----
        // 1-arg predicates + `type-of' all live in elisp now (Rust-min
        // batch 6u + Doc 86 §86.1.a).  `type-of' rides the new
        // `nl_jit_type_of' trampoline (jit/predicate.rs) via
        // `nl-jit-call-out-1' from lisp/nelisp-jit-strategy.el.
        // `null' is the alias for `nil-p' — `(null nil)' = t, anything
        // else = nil.  Distinct from `not' which has identical semantics
        // but is meant to be read as boolean negation in source.
        // null migrated to elisp (Rust-min 2026-05-06 batch 6d, see
        // lisp/nelisp-stdlib.el).
        // booleanp / keywordp migrated to elisp (Rust-min 2026-05-06
        // batch 6f, see lisp/nelisp-stdlib-misc.el).
        // ---- bitwise (essential for keymap / event encoding) ----
        // logior / logand / logxor variadic moved to elisp
        // (Rust-min 2026-05-06 batch 6j, see lisp/nelisp-stdlib.el).
        // 2-arg primitives (nelisp--logior2/logand2/logxor2) + ash
        // migrated to JIT (Phase 5 Stage 5.7, Doc 62 2026-05-08).
        // See `jit/arith.rs::lowered_{logior2,logand2,logxor2,ash}'.
        // Rust-min batch 7k (2026-05-07): `lognot' migrated to elisp
        // as `(logxor x -1)' (see lisp/nelisp-stdlib.el).
        // Rust-min (2026-05-06 batch 6e): `lsh' / `sxhash-{equal,eq,eql}'
        // moved to elisp defalias of `ash' / `sxhash'.
        "sxhash" => bi_sxhash(args),
        // ---- string ----
        // concat migrated to elisp (Rust-min 2026-05-06 batch 6r,
        // see lisp/nelisp-stdlib-plist-str.el).
        "nelisp--concat-ints" => bi_concat_ints(args),
        // format migrated to elisp (Rust-min 2026-05-06 batch 6m,
        // see lisp/nelisp-stdlib-plist-str.el).  Only the IEEE-754
        // body sliver remains as `nelisp--format-float-body'.
        "nelisp--format-float-body" => bi_format_float_body(args),
        "truncate" => bi_truncate(args),
        // Doc 86 §86.1.d: intern / symbol-name / make-symbol → jit/strings.rs.
        // Rust-min (2026-05-06 batch 6e): `string=' moved to elisp
        // defalias of `string-equal'.
        // string-equal migrated to elisp (Rust-min 2026-05-06 batch 6n,
        // see lisp/nelisp-stdlib-plist-str.el; dead body removed in 6t).
        "string-match-p" => bi_string_match_p(args),
        // Doc 76 Stage A.1 (2026-05-08) — UTF-8 byte count, used by
        // `nelisp-os-write' to size `nl-ffi-malloc' / `-write-bytes'
        // for binary-safe libc.write payloads.
        "string-bytes" => bi_string_bytes(args),
        // "regexp-quote" — migrated to elisp (Rust-min 2026-05-06)
        // expand-file-name / file-truename migrated to elisp (Rust-min
        // batch 7d, 2026-05-07; see lisp/nelisp-stdlib-misc.el).
        "nelisp--syscall-canonicalize" => bi_syscall_canonicalize(args, env),
        // Doc 47 Stage 8b — file I/O for multi-file load chains.
        // file-name-* path slicers migrated to elisp (Rust-min 2026-05-06,
        // see lisp/nelisp-stdlib-plist-str.el).
        // Rust-min batch 7b (2026-05-07, Doc 50 stage 2 first slice):
        // `file-exists-p' / `file-readable-p' / `file-directory-p' /
        // `file-regular-p' migrated to elisp on top of 1 syscall
        // primitive — see lisp/nelisp-stdlib-misc.el.
        "nelisp--syscall-stat" => bi_syscall_stat(args, env),
        // file-name-extension migrated to elisp (Rust-min batch 7c,
        // 2026-05-07; pure-string slicer — see
        // lisp/nelisp-stdlib-plist-str.el).
        // Rust-min batch 7c (2026-05-07, Doc 50 stage 2): directory-
        // files migrated to elisp on top of `nelisp--syscall-readdir'
        // (sort + match-filter + full-path formatting move to elisp;
        // only the read_dir syscall stays Rust).
        "nelisp--syscall-readdir" => bi_syscall_readdir(args, env),
        "nelisp--syscall-read-file" => bi_syscall_read_file(args, env),
        "nelisp--read-all-from-string" => bi_read_all_from_string(args, env),
        // Doc 53 Phase 1 — POSIX OS surface dispatch arms.  Doc 76
        // Stage A.1 (2026-05-08) retired the openat / read / write
        // specialized arms; elisp now routes via `nl-ffi-call libc'.
        "nelisp--syscall" => bi_syscall(args),
        "nelisp--syscall-supported-p" => bi_syscall_supported_p(args),
        // Doc 54 Phase 3 dispatch arms — retired in Doc 76 Stage
        // A.2/A.3 (2026-05-08); elisp now routes via nl-ffi-call.
        // Doc 55 Phase 4 (2026-05-07) — Posix-30 specialized primitives.
        // Doc 76 Stage B (2026-05-08) retired execve / wait4 dispatch arms.
        // Doc 76 Stage C (2026-05-08) retired socket-int dispatch arms.
        // Doc 56 Phase 4.1 (2026-05-07) — AF_UNIX + AF_INET6 sockaddr handlers.
        // Doc 76 Stage D (2026-05-08) retired AF_UNIX/INET6 dispatch arms.
        // Doc 58 Phase 4.1.x (2026-05-07) — AF_UNIX abstract + getname pair.
        // Doc 76 Stage D (2026-05-08) retired abstract + getsockname/
        // peername dispatch arms.
        // Doc 57 Phase 4.3 (2026-05-07) — inotify path/buffer primitives.
        // Doc 76 Stage E (2026-05-09) retired inotify-add-watch /
        // inotify-read dispatch arms; elisp drives them via nl-ffi-call.
        // Doc 59 Phase 4.2 + 4.3.1 (2026-05-07) — signal/timer fd surface.
        // Doc 76 Stage F (2026-05-08) retired signal/timer dispatch arms.
        // Doc 60 Phase 4.4 (2026-05-07) — SCM_RIGHTS + SOCK_SEQPACKET +
        // SO_PEERCRED + IPv6 scope_id full surface.
        // Doc 76 Stage G (2026-05-09) retired all 7 specialized arms;
        // elisp drives msghdr / cmsg / ucred / sockaddr_in6+scope_id
        // via nl-ffi-call libc + nl-ffi-malloc / write-i*/read-i* +
        // elisp-side cmsg-len/space helpers (= 64-bit Linux ABI).
        // ---- symbol / function ----
        "symbol-value" => bi_symbol_value(args, env),
        "symbol-function" => bi_symbol_function(args, env),
        "fboundp" => bi_fboundp(args, env),
        "boundp" => bi_boundp(args, env),
        "defalias" => bi_defalias(args, env),
        "fset" => bi_fset(args, env),
        "set" => bi_set(args, env),
        "fmakunbound" => bi_fmakunbound(args, env),
        "makunbound" => bi_makunbound(args, env),
        // Phase 7 Stage 7.3.a (2026-05-07, Doc 67) — macroexpand-1
        // entry for ERT inspection of elisp Tier 2 macros.
        "macroexpand-1" => bi_macroexpand_1(args, env),
        // Phase 7 Stage 7.4.a (Doc 68) — apply/call/closure/env primitives.
        "nelisp--push-frame" => bi_push_frame(args, env),
        "nelisp--pop-frame" => bi_pop_frame(args, env),
        "nelisp--push-captured" => bi_push_captured(args, env),
        "nelisp--bind-local" => bi_bind_local(args, env),
        "nelisp--apply-builtin-dispatch" => bi_apply_builtin_dispatch(args, env),
        // Phase 7 Stage 7.4.c (Doc 68 §2.7) — takeover flag toggle.
        "nelisp--set-use-elisp-apply" => bi_set_use_elisp_apply(args, env),
        "nelisp--get-use-elisp-apply" => bi_get_use_elisp_apply(args, env),
        // Phase 7 Stage 7.4.e (Doc 70) — apply-lambda-inner Rust 化.
        "nelisp--apply-lambda-inner" => bi_apply_lambda_inner(args, env),
        // intern-soft migrated to elisp (Rust-min 2026-05-06 batch 6f,
        // see lisp/nelisp-stdlib-misc.el).
        // Doc 86 §86.1.d: `make-symbol' migrated — see block above.
        // make-string migrated to elisp (Rust-min 2026-05-06 batch 6s,
        // see lisp/nelisp-stdlib-plist-str.el).  Build sliver:
        "nelisp--make-mut-string" => bi_make_mut_string(args),
        // char-to-string / string / unibyte-string / string-to-char
        // migrated to elisp (Rust-min 2026-05-06 batch 6c, see
        // lisp/nelisp-stdlib-plist-str.el).
        // string-to-number migrated to elisp (Rust-min 2026-05-06 batch 5a).
        // upcase / downcase / capitalize migrated to elisp (Rust-min
        // 2026-05-06 batch 6p, see lisp/nelisp-stdlib-plist-str.el).
        // split-string migrated to elisp (Rust-min 2026-05-06 batch 6n,
        // see lisp/nelisp-stdlib-plist-str.el; dead body removed in 6s).
        // string-trim family + string-prefix-p / string-suffix-p
        // migrated to elisp (Rust-min 2026-05-06, see
        // lisp/nelisp-stdlib-plist-str.el).
        // string-search / mapconcat / delete-dups also migrated to
        // elisp (Rust-min 2026-05-06 batch 3).
        // Doc 50 stage 4f (2026-05-07): make-hash-table / hash-table-p /
        // puthash / gethash / remhash / clrhash / nelisp--hash-pairs all
        // migrated to elisp on top of Stage 4c record primitives —
        // see lisp/nelisp-stdlib-hash.el.  `Sexp::HashTable' variant +
        // HashTableInner struct also retired in the same commit.
        // Records (Doc 50 stage 4c, 2026-05-07) — see Stage 4e
        // cl-defstruct macro for the consumer side.  Doc 86 §86.1.c
        // (2026-05-10): five `bi_record_*' dispatch arms migrated to
        // elisp via `nl_jit_record_*' trampolines — see
        // `lisp/nelisp-jit-strategy.el'.
        // char-table / bool-vector dispatch retired (Rust-min
        // 2026-05-06 batch 5b).  See file-top commentary.
        "funcall" => bi_funcall(args, env),
        "apply" => bi_apply(args, env),
        "eval" => bi_eval(args, env),
        "signal" => bi_signal(args),
        // error migrated to elisp (Rust-min 2026-05-06 batch 6m,
        // see lisp/nelisp-stdlib-misc.el).
        // Rust-min (2026-05-06 batch 6e): `print' moved to elisp
        // defalias of `princ'.  In MVP both have the no-quoting
        // behaviour of `princ'; promoting to defalias makes the
        // duplication visible.
        // princ migrated to elisp (Rust-min 2026-05-06 batch 6i,
        // see lisp/nelisp-stdlib-misc.el).  The byte-write-to-stdout
        // primitive is `nelisp--write-stdout-bytes'.
        "nelisp--write-stdout-bytes" => bi_write_stdout_bytes(args),
        // Phase 7 Stage 7.1.4 (2026-05-07, Doc 64): `prin1-to-string'
        // migrated to elisp (lisp/nelisp-stdlib-prn.el).  Internal
        // Rust callers that need a printed Sexp use `format!("{}", x)'
        // directly via the `Sexp' Display impl in `reader::sexp'.
        // message migrated to elisp (Rust-min 2026-05-06 batch 6h,
        // see lisp/nelisp-stdlib-misc.el).  The writeln-to-stderr
        // primitive is `nelisp--write-stderr-line'.
        "nelisp--write-stderr-line" => bi_write_stderr_line(args),
        "read-stdin-bytes" => bi_read_stdin_bytes(args),
        // Phase 7.1.6.a (Doc 28 §3.6.a / Doc 81 §5.4) — dlsym bridge.
        // Standalone NeLisp wires `nelisp-cc-runtime-resolve-symbol-function'
        // to call this at startup; on host Emacs the elisp side keeps
        // the `:host-stub' default unless a user has explicitly opted
        // into routing through this primitive.
        "nelisp-cc--dlsym-resolve" => super::dlsym_bridge::bi_dlsym_resolve(args),
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
        "nl-current-unix-time" => bi_nl_current_unix_time(args),
        "nl-secure-hash" => bi_nl_secure_hash(args),
        "nl-format-unix-time" => bi_nl_format_unix_time(args),
        "nl-downcase" => bi_nl_downcase(args),
        "nl-upcase" => bi_nl_upcase(args),
        "nl-split-by-non-alnum" => bi_nl_split_by_non_alnum(args),
        // min / max / abs migrated to elisp (Rust-min batch 7g, see
        // lisp/nelisp-stdlib.el) — simple folds over `<' / `>'.
        "float" => bi_float(args),
        "exp" => bi_exp(args),
        "log" => bi_log(args),
        // floor / ceiling / round migrated to elisp (Rust-min batch 7h,
        // 2026-05-07; see lisp/nelisp-stdlib.el).  The shared f64 div +
        // truncate-mode kernel stays in Rust as a single primitive.
        "nelisp--f64-trunc" => bi_f64_trunc(args),
        "nl-write-file" => bi_nl_write_file(args),
        "nl-make-directory" => bi_nl_make_directory(args),
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
        "read" => bi_read(args, env),
        "read-from-string" => bi_read_from_string(args, env),
        // provide / featurep migrated to elisp (Rust-min batch 7i,
        // 2026-05-07; see lisp/nelisp-stdlib-misc.el).  Only `require'
        // stays Rust because it orchestrates load + post-load verify.
        "require" => bi_require(args, env),
        // Doc 77b Stage b.2 (2026-05-09) — `nl-jit-call-*' bridge.
        // See `crate::jit::bridge' for the name → fn ptr lookup +
        // arity-checked callers.  Wired here (not via `lower_entries')
        // because these are plain primitives the elisp wrappers call,
        // not lowered hot-path entries themselves.
        "nl-jit-call-i64-i64" => crate::jit::bi_nl_jit_call_i64_i64(args),
        "nl-jit-call-ptr-ptr" => crate::jit::bi_nl_jit_call_ptr_ptr(args),
        "nl-jit-call-syscall" => crate::jit::bi_nl_jit_call_syscall(args),
        // Doc 77b Stage b.2.5 — out-param primitives (cons / access).
        "nl-jit-call-out-1" => crate::jit::bi_nl_jit_call_out_1(args),
        "nl-jit-call-out-2" => crate::jit::bi_nl_jit_call_out_2(args),
        "nl-jit-call-out-1i" => crate::jit::bi_nl_jit_call_out_1i(args),
        "nl-jit-call-out-2i" => crate::jit::bi_nl_jit_call_out_2i(args),
        // Doc 77b Stage b.4 — JIT strategy helpers.  Phase 7.1.7.a.1
        // (Doc 28 §3.7.a.1) deleted 5 arms (int-eq-zero + 3 bitwise +
        // ash-impl) — moved to elisp on top of the bridge.  Doc 84
        // §84.1 (2026-05-10) deleted the 8 `nelisp--*-float' arms —
        // the 2 bridge primitives below replace them.  Doc 84 §84.2
        // ported `nelisp--syscall-nr-resolve' to elisp; §84.3 lifted
        // the 6 Box-accessor arms to elisp on top of
        // `jit/box_accessor.rs' trampolines.
        "nl-jit-call-float-float" => crate::jit::bi_nl_jit_call_float_float(args),
        "nl-jit-call-float-cmp" => crate::jit::bi_nl_jit_call_float_cmp(args),
        // Doc 77c Phase A.3 (2026-05-09) — Layer 2 cons-cell primitives
        // operating directly on `NlConsBox' / `NlConsBoxRef' (Phase A.2).
        // 5 `nl-cons-*' (alloc / car / cdr / set-car / set-cdr) + 3
        // `nl-rc-*' (inc / dec / count) for manual refcount management.
        // See `crate::eval::cons_primitives' for the surface table.
        "nl-cons-alloc" => crate::eval::cons_primitives::bi_nl_cons_alloc(args),
        "nl-cons-car" => crate::eval::cons_primitives::bi_nl_cons_car(args),
        "nl-cons-cdr" => crate::eval::cons_primitives::bi_nl_cons_cdr(args),
        "nl-cons-set-car" => crate::eval::cons_primitives::bi_nl_cons_set_car(args),
        "nl-cons-set-cdr" => crate::eval::cons_primitives::bi_nl_cons_set_cdr(args),
        "nl-rc-inc" => crate::eval::cons_primitives::bi_nl_rc_inc(args),
        "nl-rc-dec" => crate::eval::cons_primitives::bi_nl_rc_dec(args),
        "nl-rc-count" => crate::eval::cons_primitives::bi_nl_rc_count(args),
        // Doc 79 v7 Phase C Stage 5.3.a (2026-05-09) — generic NlRc
        // primitive set + initial GC helper trio.  10 dispatch arms
        // backing `lisp/nelisp-stdlib-gc.el' Bacon-Rajan cycle
        // collector skeleton.  See `crate::eval::rc_primitives' for
        // the surface table + MVP scope (= CONS kind only).
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
        _ => {
            // Externally-registered builtin (= `Env::register_extern_builtin')
            // — host crates like nelisp-emacs-gtk install GTK4 / SDL2 /
            // future backend primitives this way.  Clone the Rc out
            // first so the borrow on `env.extern_builtins' drops before
            // we re-borrow `env' through the closure (= eval re-entry
            // safe).
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

// `as_string' helper removed (Rust-min 2026-05-06 batch 6b): its
// only caller was `bi_substring', which moved to elisp.  Remaining
// builtins that accept string-or-mutstr call `as_string_owned'
// directly on `Sexp' since they already have idiomatic
// `Option<String>' handling.

fn truthy(value: bool) -> Sexp {
    if value { Sexp::T } else { Sexp::Nil }
}

// ---------- arithmetic implementations ----------
//
// bi_add / bi_sub / bi_mul / bi_mod / bi_div all migrated to elisp
// (Rust-min 2026-05-06 batch 6v + 6l + Doc 86 §86.1.b 2026-05-10).
// Variadic + / - / * fold over the 2-arg primitives `nelisp--add2' /
// `nelisp--sub2' / `nelisp--mul2'; `mod' is `(- a (* b (/ a b)))'
// with sign-adjust; `/' rides the new `nl_jit_float_div' trampoline
// (jit/float.rs) for upfront f64 promotion + trunc-on-all-int gating.
// `numeric_promote' / `pack_number' helpers deleted with `bi_div'
// (= had no other callers).

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

// `int_pair_or' removed — its sole callers (`bi_add2' / `bi_sub2' /
// `bi_mul2') were deleted in Phase 5 Stage 5.7.  The JIT side now
// uses `num_pair' directly for the Float promotion path and the
// Int+Int wrapping arithmetic happens via Cranelift `iadd'/`isub'/
// `imul' which is wrapping by IR contract.

// `bi_add2' / `bi_sub2' / `bi_mul2' deleted — Phase 5 Stage 5.7
// (Doc 62, 2026-05-08).  See `jit/arith.rs::lowered_{add2,sub2,mul2}'
// for the JIT-only path (= Int+Int via Cranelift `iadd'/`isub'/`imul',
// Float involvement via inline `num_pair' f64 promotion).

// ---------- bitwise -----------------------------------------------------
//
// Required by keymap / event-encoding code (= `(logior char (lsh 1 26))'
// for the Emacs Ctrl-bit chord encoding) plus general numeric utilities
// on which the substrate's polyfills lean.

// bi_logior / bi_logand / bi_logxor variadic dispatch removed — see
// lisp/nelisp-stdlib.el (Rust-min 2026-05-06 batch 6j).  Replaced
// by 3 thin 2-arg primitives (just below) which the elisp version
// folds over.  Codebase grep for `(log{ior,and,xor} ARG ARG ...)'
// found 54 callers — all exactly 2-arg, so the variadic feature
// was unused in practice and the elisp fold has no real cost.

// `bi_logior2' / `bi_logand2' / `bi_logxor2' / `bi_ash' deleted —
// Phase 5 Stage 5.7 (Doc 62, 2026-05-08).  See
// `jit/arith.rs::lowered_{logior2,logand2,logxor2,ash}'.  ash
// preserves the count-clamping semantics inline; the JIT fast path
// covers count ∈ [-62, +62].

/// `(sxhash OBJECT)' / `sxhash-{equal,eq,eql}' — fold OBJECT into
/// an i64 hash.  All four flavours share the same impl here; that
/// is fine for the substrate's use-cases (= deriving a stable
/// integer key for hashing).  Real Emacs distinguishes the four
/// based on equality predicate — we accept the imprecision since
/// caller code that needs equality-class-stable hashing will go
/// through the hash-table API directly.
fn bi_sxhash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("sxhash", args, 1, Some(1))?;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hasher;
    let mut h = DefaultHasher::new();
    sxhash_into(&args[0], &mut h);
    // Mask to a positive Emacs-fixnum-friendly range.
    let raw = h.finish();
    Ok(Sexp::Int((raw & 0x3FFF_FFFF_FFFF_FFFFu64) as i64))
}

fn sxhash_into<H: std::hash::Hasher>(v: &Sexp, h: &mut H) {
    use std::hash::Hash;
    match v {
        Sexp::Nil => 0u8.hash(h),
        Sexp::T => 1u8.hash(h),
        Sexp::Int(n) => { 2u8.hash(h); n.hash(h); }
        Sexp::Float(x) => { 3u8.hash(h); x.to_bits().hash(h); }
        Sexp::Symbol(s) => { 4u8.hash(h); s.hash(h); }
        Sexp::Str(s) => { 5u8.hash(h); s.hash(h); }
        Sexp::MutStr(rc) => { 5u8.hash(h); rc.value.hash(h); }
        Sexp::Cons(b) => {
            6u8.hash(h);
            sxhash_into(&b.car, h);
            sxhash_into(&b.cdr, h);
        }
        Sexp::Vector(rc) => {
            7u8.hash(h);
            for it in rc.value.iter() { sxhash_into(it, h); }
        }
        Sexp::CharTable(_) => 9u8.hash(h),
        Sexp::BoolVector(rc) => {
            10u8.hash(h);
            for &b in rc.value.iter() { (b as u8).hash(h); }
        }
        // Lexical-binding storage cell — hash through to inner value
        // (= cells should be invisible to user-facing sxhash).
        Sexp::Cell(c) => sxhash_into(&c.value, h),
        Sexp::Record(rec) => {
            11u8.hash(h);
            sxhash_into(&rec.type_tag, h);
            for s in rec.slots.iter() {
                sxhash_into(s, h);
            }
        }
    }
}

// bi_lt / bi_gt / bi_le / bi_ge / bi_eq_num / bi_neq_num removed —
// see lisp/nelisp-stdlib.el (Rust-min 2026-05-06 batch 6w).
// Variadic chained-pairwise comparisons (`(< a b c)' = `(and (< a b)
// (< b c))') collapse to elisp folds over new 2-arg primitives.
// Float-tolerance `=' uses `1e-15' epsilon — moved to the
// `nelisp--num-eq2' primitive.  `/=' is just `(not (= a b))'.

// `bi_num_eq2' / `bi_num_lt2' / `bi_num_gt2' / `bi_num_le2' /
// `bi_num_ge2' deleted — Phase 5 Stage 5.7 (Doc 62, 2026-05-08).
// See `jit/arith.rs::lowered_num_*'.  Int+Int → Cranelift icmp
// (exact); Float involvement → `num_pair' f64 promotion + Rust
// fcmp.  `num-eq2' preserves the 1e-15 epsilon for Float (=
// matches Emacs semantics + the previous `bi_num_eq2').
//
// `cmp2_helper' removed (= no remaining callers).

// ---------- equality ----------
//
// Phase 5 Stage C-Phase1 (Doc 62, 2026-05-08): `bi_eq' deleted.
// The JIT path (`jit/predicate.rs::lowered_eq') is the single source
// of truth; arity / wrong-type errors are emitted from the lowered
// wrapper, no `dispatch'/`bi_eq' fallback.
//
// Doc 86 §86.1.b (2026-05-10): `bi_equal' + `sexp_equal_safe' helper
// deleted (= dead code, displaced by the elisp `equal' defun in
// `lisp/nelisp-stdlib-equal.el' since Doc 50 stage 5b).  `sexp_eq'
// helper stays — still used by `bi_ref_eq' / `nl_jit_pred_eq'.

// `bi_ref_eq' deleted — Doc 86 §86.1.b (2026-05-10).  The body was
// equivalent to `sexp_eq' (= `sexp_eq' already uses `Rc::ptr_eq' for
// every shared-heap variant; the explicit per-variant `match' arms
// in `bi_ref_eq' just duplicated `sexp_eq's coverage and fell through
// to it for the rest).  The new `nl_jit_ref_eq' trampoline in
// `jit/predicate.rs' calls `sexp_eq' once and writes `Sexp::T'/`Nil'
// directly, removing the conversion dance the elisp wrapper would
// otherwise need (= cf. `eq' wrapper's `nelisp--int-eq-zero' guard).

// `sexp_equal_safe' + `SEXP_EQUAL_DEPTH_LIMIT' deleted — Doc 86
// §86.1.b (2026-05-10).  Their sole caller `bi_equal' was deleted in
// the same commit; the elisp `equal' (Doc 50 stage 5b — see
// `lisp/nelisp-stdlib-equal.el') uses a visited hash-table for
// cycle-safety instead of the bounded recursion strategy this Rust
// helper used.

// ---------- cons / list ----------
//
// Phase 5 Stage C-Phase1 (Doc 62, 2026-05-08): `bi_car' / `bi_cdr' /
// `bi_cons' / `bi_setcar' / `bi_setcdr' deleted (5 functions).  The
// JIT path (`jit/cons.rs::lowered_{car,cdr,cons,setcar,setcdr}') is
// the single source of truth; arity / wrong-type errors are emitted
// from each lowered wrapper directly, no `dispatch' fallback.

// `bi_length' deleted — Phase 5 Stage C-Phase1b (Doc 62, 2026-05-08).
// See `jit/access.rs::lowered_length' (= JIT fast path + inline
// MutStr / BoolVector / Cons-walk / WrongType handling).

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

// `vec_to_list' helper removed — its only caller was `bi_reverse'
// (Rust-min 2026-05-06 batch 6d).

// bi_reverse removed — see lisp/nelisp-stdlib-list.el (Rust-min
// 2026-05-06 batch 6d).  The elisp version was already shadowing
// the Rust path since stdlib first loaded; this commit removes the
// orphan code.

// bi_sort migrated to elisp (Rust-min 2026-05-06 batch 4) — see
// lisp/nelisp-stdlib-plist-str.el `sort'.

// bi_append removed — see lisp/nelisp-stdlib-list.el (Rust-min
// 2026-05-06 batch 6o).  The cons-spine walk + vector iter +
// string char-iter + final-arg tail are all expressible via
// `consp' / `vectorp' / `stringp' / `aref' / `length' / `cons'.

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

// ---------- predicates ----------
// (Doc 86 §86.1.a 2026-05-10) — `bi_predicate' / `bi_type_of' /
// `bi_recordp' all migrated to elisp; only the trampoline survives.

// ---------- string ----------

// bi_concat removed — see lisp/nelisp-stdlib-plist-str.el (Rust-min
// 2026-05-06 batch 6r).  The user-facing dispatch (string / nil /
// list-of-ints type-walk + char-codepoint accumulator) is fully
// expressible in elisp; only the irreducible "construct-a-Sexp::Str-
// from-a-flat-int-list" sliver remains here as
// `nelisp--concat-ints' (just below).

/// `(nelisp--concat-ints LIST-OF-INTS)' — return a fresh string
/// whose chars are the codepoints in LIST-OF-INTS.  Nil = empty
/// string.  Each list element must be an integer (= a Unicode
/// codepoint); non-integer signals `wrong-type-argument'.  Improper
/// list signals `listp' wrong-type-argument once the spine exits.
///
/// Sole "build a string" primitive after the batch 6r migration of
/// `concat' to elisp.  The elisp `concat' dispatcher in
/// `lisp/nelisp-stdlib-plist-str.el' walks variadic args (= mixed
/// strings + lists + nil), accumulates a flat int-list, and calls
/// this primitive once at the end.
fn bi_concat_ints(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--concat-ints", args, 1, Some(1))?;
    let mut out = String::new();
    let mut cur = args[0].clone();
    loop {
        match cur {
            Sexp::Nil => break,
            Sexp::Cons(b) => {
                let v = b.car.clone();
                match &v {
                    Sexp::Int(n) => {
                        if let Some(ch) = char::from_u32(*n as u32) {
                            out.push(ch);
                        }
                    }
                    _ => {
                        return Err(EvalError::WrongType {
                            expected: "integerp".into(),
                            got: v,
                        });
                    }
                }
                cur = b.cdr.clone();
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "listp".into(),
                    got: other,
                });
            }
        }
    }
    Ok(Sexp::Str(out))
}

// Rust-min batch 6m (2026-05-06): `format` migrated from Rust to elisp.
// The previous `bi_format` (~200 LOC) + helpers FormatSpec /
// pad_field / fmt_int_with_sign / fmt_float_default lived here;
// see lisp/nelisp-stdlib-plist-str.el for the new pure-elisp
// dispatcher.  Only the IEEE-754 float-body sliver remains as a
// Rust primitive — `bi_format_float_body` (further down in this
// file).

// bi_substring removed — see lisp/nelisp-stdlib-plist-str.el
// (Rust-min 2026-05-06 batch 6b).  Vector substring is not in scope
// because the previous `bi_substring' was string-only as well.
// `normalise_index' was a private helper used only by `bi_substring',
// so dropped along with it.

// bi_intern/_make_symbol/_intern_soft/_gensym all removed (Doc 86
// §86.1.d + Rust-min batch 6f/6a).

// bi_copy_sequence removed — see lisp/nelisp-stdlib-misc.el
// (Rust-min 2026-05-06 batch 6g).  cons / nil paths in elisp; other
// types (str / mutstr / vector / atoms) return identity since the
// previous Rust impl's clone-the-Sexp gave the same observed
// semantics for everything except Str / MutStr — and a codebase
// grep for `(aset (copy-sequence ...))' confirmed no caller relies
// on the fresh-cell behaviour we're dropping for those two.

// Rust-min (2026-05-06 batch 3): `mapconcat' migrated to elisp
// (lisp/nelisp-stdlib-plist-str.el).

// bi_make_string removed — see lisp/nelisp-stdlib-plist-str.el
// (Rust-min 2026-05-06 batch 6s).  The argument-validation +
// arity dispatch is fully expressible in elisp; only the
// "build-a-fresh-Sexp::MutStr" sliver remains here as
// `nelisp--make-mut-string' (just below).  The MutStr return type
// is preserved so that callers (e.g. `emacs-redisplay.el') which
// `aset' into the result keep their mutable-string contract.

/// `(nelisp--make-mut-string LEN CH)' — return a fresh mutable
/// string of LEN copies of CH (= int codepoint).  Validation of
/// LEN being non-negative + CH being a valid codepoint is done by
/// the elisp `make-string' wrapper, so this primitive trusts
/// its inputs.  Sole "build-a-MutStr" sliver after batch 6s.
fn bi_make_mut_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--make-mut-string", args, 2, Some(2))?;
    let n = match &args[0] {
        Sexp::Int(n) if *n >= 0 => *n as usize,
        other => return Err(EvalError::WrongType {
            expected: "non-negative integer".into(),
            got: other.clone(),
        }),
    };
    let c = match &args[1] {
        Sexp::Int(c) if (0..=0x10FFFF).contains(c) => {
            char::from_u32(*c as u32).unwrap_or(' ')
        }
        other => return Err(EvalError::WrongType {
            expected: "character (integer)".into(),
            got: other.clone(),
        }),
    };
    Ok(Sexp::mut_str(c.to_string().repeat(n)))
}

// bi_char_to_string / bi_string_from_chars / bi_string_to_char
// removed — see lisp/nelisp-stdlib-plist-str.el (Rust-min 2026-05-06
// batch 6c).  All three composed trivially over `concat' / `aref',
// no Sexp-internal logic was unique to them.

// bi_string_to_number migrated to elisp (Rust-min 2026-05-06 batch 5a).

// bi_upcase / bi_downcase / bi_capitalize removed — see
// lisp/nelisp-stdlib-plist-str.el (Rust-min 2026-05-06 batch 6p).
// ASCII-only case mapping in elisp; non-ASCII bytes pass through
// unchanged (= functionally equivalent to the previous Rust behaviour
// on NeLisp's byte-as-char string repr, which never delivered
// meaningful Unicode case mapping for multi-byte input anyway).

// bi_split_string removed (Rust-min batch 6n / re-cleanup batch 6s).
// The literal-string split + whitespace fallback lives in elisp at
// lisp/nelisp-stdlib-plist-str.el.  This dead function body
// resurfaced briefly via a stash-merge artefact during batch 6o
// branch surgery and is now wholesale removed.

// bi_string_trim / bi_string_trim_left / bi_string_trim_right /
// bi_string_prefix_p / bi_string_suffix_p removed — see
// lisp/nelisp-stdlib-plist-str.el (Rust-min 2026-05-06).

// bi_string_search removed — see lisp/nelisp-stdlib-plist-str.el
// (Rust-min 2026-05-06 batch 3).

// bi_make_hash_table / bi_hash_table_p / bi_puthash / bi_gethash /
// bi_remhash / bi_clrhash / bi_hash_pairs all retired in Doc 50
// stage 4f (2026-05-07).  Hash-tables are now `(record 'hash-table
// TEST ENTRIES)' — see lisp/nelisp-stdlib-hash.el for the elisp
// implementation built on Stage 4c record primitives.

// --- Doc 50 stage 4c: record primitives ---
//
// Doc 86 §86.1.c (2026-05-10): five `bi_record_*' helpers (= bi_make_
// record / bi_record_ref / bi_record_set / bi_record_length / bi_record_
// type) and `bi_recordp' all migrated to elisp.  The semantic core now
// lives in `lisp/nelisp-jit-strategy.el' on top of five `nl_jit_record_*'
// trampolines in `jit/box_accessor.rs' (Tier 1.5 per Doc 87 §1.2.3 —
// reuses the existing `nl-jit-call-out-{1,1i,2i,2}' bridge primitives,
// no new ABI mode).  See those files for the slot/0-based-index +
// `out-of-range-args' / `wrong-type-argument' contract.

// bi_hash_table_count / bi_maphash / bi_hash_table_keys /
// bi_hash_table_values all retired in Rust-min batch 7a (Doc 50 stage
// 1, 2026-05-07).  All four collapse into elisp folds on top of the
// lone iter primitive `nelisp--hash-pairs' above — see
// lisp/nelisp-stdlib-misc.el.  The previous bi_maphash had to take
// `&mut Env' so it could `apply_function'; the new arrangement keeps
// `apply' / `funcall' as the only Rust-side dispatch routes.
//
// 6k earlier note: -keys / -values used `maphash' fold +
// `cons'+`nreverse' (= O(n)); the rewire to `nelisp--hash-pairs' +
// `mapcar' is the same complexity but skips the closure write-through.

// char-table / bool-vector user-facing builtins retired (Rust-min
// 2026-05-06 batch 5b).  See file-top commentary; surface migrated
// to elisp.  `Sexp::CharTable' / `Sexp::BoolVector' variants kept
// alive for image-format backward-compat decode only — the helpers
// below let `bi_aref' / `bi_aset' continue to read/write any
// legacy-decoded instances even though no new ones are minted.

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

// hash_test_eq helper removed in Doc 50 stage 4f — hash-table key
// comparison now lives in elisp (`nelisp--hash-test-equal' in
// lisp/nelisp-stdlib-hash.el).

// bi_delete_dups removed — see lisp/nelisp-stdlib-plist-str.el
// (Rust-min 2026-05-06 batch 3).
// inner_test_for helper removed (Rust-min cleanup, 2026-05-07): was
// used by bi_delete_dups before its elisp migration; orphan after
// batch 3.

// bi_symbol_name removed — Doc 86 §86.1.d (jit/strings.rs).
// bi_string_eq removed (Rust-min batch 6n / re-cleanup batch 6t).
// Same stash-merge artefact pattern as the dead `bi_split_string'
// scrubbed in batch 6s — the user-facing dispatch was always shadowed
// by the elisp `string-equal' in lisp/nelisp-stdlib-plist-str.el, so
// the dead body was behaviourally invisible.

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

// bi_regexp_quote removed — see lisp/nelisp-stdlib-plist-str.el
// (Rust-min 2026-05-06).

fn bi_string_match_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-match-p", args, 2, Some(2))?;
    let pat = string_value(&args[0])?;
    let text = string_value(&args[1])?;
    let matched = match pat.as_str() {
        "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" => {
            let s = text.as_str();
            let s = s.strip_prefix('-').unwrap_or(s);
            let mut parts = s.split('.');
            let first = parts.next().unwrap_or("");
            let second = parts.next();
            parts.next().is_none()
                && !first.is_empty()
                && first.chars().all(|c| c.is_ascii_digit())
                && second.map_or(true, |tail| !tail.is_empty() && tail.chars().all(|c| c.is_ascii_digit()))
        }
        "\\`{.*}\\'" => text.starts_with('{') && text.ends_with('}'),
        "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" => {
            let parts: Vec<&str> = text.split('.').collect();
            parts.len() == 4 && parts.iter().all(|p| !p.is_empty() && p.chars().all(|c| c.is_ascii_digit()))
        }
        "^[[:space:]]*$" | "\\`[[:space:]]*\\'" => text.chars().all(|c| c.is_whitespace()),
        "^[\u{00A0}]*$" => text.chars().all(|c| c == '\u{00A0}'),
        "[\n\r]" => text.contains('\n') || text.contains('\r'),
        _ => {
            let anchored_start = pat.starts_with("\\`") || pat.starts_with('^');
            let anchored_end = pat.ends_with("\\'") || pat.ends_with('$');
            let literal = pat
                .replace("\\`", "")
                .replace("\\'", "")
                .replace('^', "")
                .replace('$', "")
                .replace("\\.", ".")
                .replace("\\\\", "\\");
            if anchored_start && anchored_end {
                text == literal
            } else if anchored_start {
                text.starts_with(&literal)
            } else if anchored_end {
                text.ends_with(&literal)
            } else {
                text.contains(&literal)
            }
        }
    };
    Ok(truthy(matched))
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

// bi_expand_file_name retired in Rust-min batch 7d (Doc 50 stage 2,
// 2026-05-07).  The function was pure path arithmetic + a
// `default-directory' lookup — both expressible in elisp.  See
// lisp/nelisp-stdlib-misc.el.
//
// bi_file_truename retired in same batch.  The `std::fs::canonicalize'
// syscall sliver is now `nelisp--syscall-canonicalize' below; the
// expand-file-name wrap and fallback-on-error are elisp side.

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

// ---------- Doc 47 Stage 8b — file I/O for multi-file load chains ----------
//
// Pure-string path slicers (`file-name-directory' /
// `file-name-nondirectory' / `file-name-as-directory' /
// `directory-file-name') were migrated to elisp on 2026-05-06 — see
// lisp/nelisp-stdlib-plist-str.el.  The remaining file I/O primitives
// below still need filesystem access and stay Rust-side.

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

// bi_file_name_extension retired in Rust-min batch 7c (Doc 50 stage 2,
// 2026-05-07).  Pure-string slicer, no syscall, so migration cost was
// the lowest of any batch — see lisp/nelisp-stdlib-plist-str.el.

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
// Doc 53 Phase 1 (2026-05-07) — POSIX OS surface (Minimal-5).  Five
// primitives that let elisp build `nelisp-os-open' / `-read' / `-write'
// / `-close' / `-exit' (lisp/nelisp-stdlib-os.el) without further Rust
// glue.  Future syscalls that take only int args (lseek / dup2 / kill /
// getpid etc.) can be invoked directly via `nelisp--syscall NAME-OR-NR
// ...' from elisp without touching Rust again.
//
// Linux-only for Phase 1 (= where libc::syscall + libc::SYS_* are
// stable + portable).  Darwin / Windows reach the same elisp API via
// the Path B fallback (= Doc 51 Phase 5 `nl-ffi-call' libc bindings) —
// see lisp/nelisp-stdlib-os.el.  `nelisp--syscall-supported-p' returns
// nil on those platforms so the elisp wrapper picks the fallback path;
// the four other primitives `Err' explicitly if called.
//
// Error convention: result < 0 = -errno (Linux kernel ABI shape).
// `libc::syscall(3)' translates kernel -errno into (-1, errno) so we
// re-normalize via `__errno_location()' to keep the elisp surface
// uniform.  When native cc lands (Phase 2 / future Doc) the lowering
// can emit raw `syscall' instructions and skip this normalization.
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
            // Doc 54 Phase 3 (2026-05-07) — Core-12 additions.  All
            // take int-only args so they ride the generic dispatch
            // without a specialized primitive.  fstat / pipe need
            // out-buffer handling and have their own primitives.
            "mmap"       => Ok(libc::SYS_mmap       as i64),
            "mprotect"   => Ok(libc::SYS_mprotect   as i64),
            "munmap"     => Ok(libc::SYS_munmap     as i64),
            "fcntl"      => Ok(libc::SYS_fcntl      as i64),
            // Doc 55 Phase 4 (2026-05-07) — Posix-30 additions.  fork
            // / socket / listen / getppid / setpgid take only int
            // arguments and ride the generic dispatch.  execve /
            // wait4 / bind / connect / accept / poll need buffer
            // handling and live in their own primitives below.
            "fork"       => Ok(libc::SYS_fork       as i64),
            "socket"     => Ok(libc::SYS_socket     as i64),
            "listen"     => Ok(libc::SYS_listen     as i64),
            "wait4"      => Ok(libc::SYS_wait4      as i64),
            "getppid"    => Ok(libc::SYS_getppid    as i64),
            "setpgid"    => Ok(libc::SYS_setpgid    as i64),
            // Doc 57 Phase 4.3 (2026-05-07) — modern Linux event
            // surface (pidfd / inotify / eventfd).  inotify_init1 /
            // inotify_rm_watch ride this generic int-only syscall arm;
            // inotify_add_watch / inotify_read (path + variable-length
            // packed buffer) were retired in Doc 76 Stage E (2026-05-09)
            // and now run elisp-side via nl-ffi-call.
            "pidfd_open"        => Ok(libc::SYS_pidfd_open        as i64),
            "pidfd_send_signal" => Ok(libc::SYS_pidfd_send_signal as i64),
            "inotify_init1"     => Ok(libc::SYS_inotify_init1     as i64),
            "inotify_rm_watch"  => Ok(libc::SYS_inotify_rm_watch  as i64),
            "eventfd2"          => Ok(libc::SYS_eventfd2          as i64),
            // Doc 59 Phase 4.2 + 4.3.1 (2026-05-07) — timerfd_create
            // is int-only and rides the generic dispatch.  signalfd /
            // timerfd_settime / timerfd_gettime / sigprocmask have their
            // own primitives below (= sigset_t / itimerspec buffers).
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

// ---------------------------------------------------------------------------
// Phase 5 Stage 5.8 (Doc 62, 2026-05-08) — non-Linux syscall stub macro.
//
// On non-Linux platforms every `nelisp--syscall-*` Rust primitive is a
// dead path: `lisp/nelisp-stdlib-os.el` flips
// `nelisp-os--use-direct-syscall' to nil at load time (via
// `nelisp--syscall-supported-p`) and routes all syscall callers through
// `nl-ffi-call' libc bindings (= Path B) instead.  These stubs only
// surface if elisp accidentally bypasses that routing — they signal a
// canonical Internal error so the bug is loud.
//
// Originally each stub was a 5-line `fn` definition (40+ blocks =
// ~200 LOC).  Stage 5.8 collapses them into 1-line `syscall_unsupported!`
// invocations behind this shared macro.
// ---------------------------------------------------------------------------
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

// Doc 76 Stage A.1 (2026-05-08): `nelisp--syscall-openat' / `-read' /
// `-write' specialized primitives removed.  elisp `nelisp-os-open' /
// `-read' / `-write' now route through `nl-ffi-call libc' unified
// (= single-path, no Path A/B branch).  See lisp/nelisp-stdlib-os.el.

// Doc 76 Stage A.2/A.3 (2026-05-08): `nelisp--syscall-fstat' /
// `nelisp--syscall-pipe' specialized primitives removed.  elisp
// `nelisp-os-fstat' / `nelisp-os-pipe' now route through `nl-ffi-call
// libc' + `nl-ffi-read-i32' / `-i64' for struct decoding.  See
// lisp/nelisp-stdlib-os.el.

// ---------------------------------------------------------------------------
// Doc 55 Phase 4 — Posix-30 specialized primitives (subprocess + network +
// poll).  Each needs buffer / struct handling that the generic
// `nelisp--syscall' int-only dispatch cannot express.  Linux-only via
// `#[cfg(target_os = "linux")]'; non-Linux returns `EvalError::Internal'.
// ---------------------------------------------------------------------------

/// `(nelisp--syscall-execve PATH ARGV ENVP)' — POSIX execve(2).
///
// Doc 76 Stage B (2026-05-08): `nelisp--syscall-execve' / `-wait4'
// specialized primitives removed.  elisp `nelisp-os-execve' /
// `nelisp-os-wait' now drive argv/envp char* array marshaling via
// `nl-ffi-malloc' per-string + `nl-ffi-write-i64' for the pointer
// table, and wait4 status via `nl-ffi-malloc' + `nl-ffi-read-i32'.
// See lisp/nelisp-stdlib-os.el.

// Doc 76 Stage C (2026-05-08): `nelisp--syscall-setsockopt-int' /
// `-bind-inet' / `-connect-inet' / `-accept-inet' specialized
// primitives + `build_sockaddr_in' helper removed.  elisp wrappers
// now build/decode `sockaddr_in' via `nl-ffi-malloc' + `nl-ffi-write-i16/i32'
// + `libc.htons/htonl/ntohs/ntohl'.  See lisp/nelisp-stdlib-os.el.

// ---------------------------------------------------------------------------
// Doc 56 Phase 4.1 — AF_UNIX + AF_INET6 sockaddr handling.
//
// AF_INET (Doc 55) primitives only handle sockaddr_in.  These add the two
// other socket families NeLisp realistically needs: filesystem-path UNIX
// sockets and IPv6 dual-stack networks.  Generic syscalls (socket /
// listen / setsockopt-int / poll) and the fd-flavoured ones (read /
// write / close) are family-independent, so the only Rust additions
// here are bind / connect / accept × {unix, inet6}.
// ---------------------------------------------------------------------------

/// Build a `sockaddr_un' from a filesystem path.  Abstract-namespace
/// sockets (= leading NUL byte) are out of Phase 4.1's scope.  Returns
// Doc 76 Stage D (2026-05-08): bind/connect/accept × {unix, inet6}
// specialized primitives + most sockaddr_un / -_in6 helpers removed.
// elisp wrappers now drive sockaddr_un / sockaddr_in6 encode/decode
// directly via nl-ffi-malloc + nl-ffi-write/read primitives.
//
// Doc 76 Stage G (2026-05-09) retired `decode_in6_groups' /
// `parse_in6_addr_groups' along with the inet6-scoped specialized
// primitives.  IPv6 group encode / decode now lives in
// `lisp/nelisp-stdlib-os.el' as `nelisp-os--encode-sockaddr-in6-scoped'
// / `-decode-sockaddr-in6-scoped' driven by `nl-ffi-write-i16' /
// `nl-ffi-read-u16' + `htons' / `ntohs' libc calls.

// ---------------------------------------------------------------------------
// Doc 58 Phase 4.1.1 + 4.1.2 — AF_UNIX abstract namespace + getsockname /
// getpeername.  Builds on Doc 56's `build_sockaddr_un' / `parse_sockaddr_un_peer'
// and Doc 55's AF_INET sockaddr layout.
// ---------------------------------------------------------------------------

// Doc 76 Stage D (2026-05-08) — abstract namespace + getsockname /
// getpeername helpers + 8 specialized primitives all retired.  elisp
// drives all variants via nl-ffi sockaddr_un / sockaddr_in6 helpers.

// Doc 76 Stage E (2026-05-09): bi_syscall_inotify_add_watch /
// bi_syscall_inotify_read 削除. inotify_add_watch / read は elisp 側で
// nl-ffi-call libc.inotify_add_watch + libc.read + nl-ffi-read-i32/-u32/
// -bytes-at で variable-length inotify_event[] を walk する (= Stage C
// poll() / Stage F signalfd の packed buffer parse pattern を再利用).

// ---------------------------------------------------------------------------
// Doc 59 Phase 4.2 + 4.3.1 — signalfd + timerfd + sigprocmask.
//
// Linux's signalfd lets us receive signals through a regular fd readable
// in `poll(2)', avoiding the async-callback / re-entrancy minefield of
// `sigaction'.  Together with `pthread_sigmask' (= sigprocmask) and
// `timerfd_settime' / -gettime', this rounds out the modern Linux event
// substrate without ever asking the elisp runtime to handle signals
// asynchronously.
// ---------------------------------------------------------------------------

// Doc 76 Stage F (2026-05-08): signalfd / signalfd-read / sigprocmask
// / timerfd-{set,get}time specialized + build_sigset_from_list /
// sigset_to_list helpers all removed.  elisp drives sigset_t via
// libc.sigemptyset / sigaddset / sigismember and itimerspec via
// nl-ffi-write-i64 / read-i64.  signalfd_siginfo (= 128-byte event)
// is decoded directly via read-u32/i32 at known offsets.

// Doc 76 Stage G (2026-05-09): SCM_RIGHTS + SOCK_SEQPACKET +
// SO_PEERCRED + IPv6 scope_id surface fully retired (= last 7
// specialized primitives + `build_sockaddr_in6_with_scope' /
// `decode_in6_groups' / `parse_in6_addr_groups' helpers).  elisp
// drives msghdr / cmsg / ucred / sockaddr_in6+scope_id via
// `nl-ffi-call libc' (sendmsg / recvmsg / getsockopt / bind /
// connect / accept / socketpair / htons / ntohs / htonl / ntohl) +
// `nl-ffi-malloc' / `-write-i*/-read-i*' + elisp-side
// `nelisp-os--cmsg-len' / `-cmsg-space' / `-cmsg-iterate' (=
// 64-bit Linux ABI assumed; see `lisp/nelisp-stdlib-os.el' Stage G
// section for layout constants).  This finishes Doc 76 at 40/40
// specialized primitives migrated.

// ---------------------------------------------------------------------------

// Doc 76 Stage C (2026-05-08): `nelisp--syscall-poll' specialized
// removed.  elisp `nelisp-os-poll' now encodes `pollfd[]' via
// `nl-ffi-malloc' + `nl-ffi-write-i32/i16' and decodes revents via
// `nl-ffi-read-i16'.  See lisp/nelisp-stdlib-os.el.

// `bi_locate_library' removed — Rust-min batch 7e (2026-05-07, Doc 50
// stage 2): migrated to elisp `(defun locate-library ...)' on top of
// existing `expand-file-name' + `nelisp--syscall-stat' primitives.
// See lisp/nelisp-stdlib-misc.el.

// bi_file_name_as_directory / bi_directory_file_name removed — see
// lisp/nelisp-stdlib-plist-str.el (Rust-min 2026-05-06).

// `locate_load_target' / `bi_load' removed — Rust-min batch 7f
// (2026-05-07, Doc 50 stage 2): both migrated to elisp `(defun load
// ...)' on top of new primitives `nelisp--syscall-read-file' and
// `nelisp--read-all-from-string', composed with the elisp
// `locate-library' from batch 7e.  `bi_require' below now dispatches
// to the elisp `load' through the function cell so user-level
// `(defalias 'load ...)' redefinitions are honoured (not possible
// with the prior direct `bi_load(...)' call).  See
// lisp/nelisp-stdlib-misc.el.

// ---------- symbol / function ----------

fn bi_symbol_value(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("symbol-value", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => env.lookup_value(s),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

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

fn bi_fboundp(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("fboundp", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => Ok(if env.is_fbound(s) { Sexp::T } else { Sexp::Nil }),
        _ => Ok(Sexp::Nil),
    }
}

fn bi_boundp(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("boundp", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => Ok(if env.is_bound(s) { Sexp::T } else { Sexp::Nil }),
        _ => Ok(Sexp::Nil),
    }
}

/// `(defalias SYMBOL DEFINITION &optional DOCSTRING)` — set the
/// function cell of SYMBOL to DEFINITION.  Mirrors `fset' in current
/// nelisp; the optional DOCSTRING argument is accepted for API parity
/// with host Emacs but discarded (= we have no doc-cell yet).
fn bi_defalias(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("defalias", args, 2, Some(3))?;
    let name = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    };
    // If DEFINITION is a symbol, follow the function-cell chain so
    // the alias resolves to a callable form.  If it's a lambda /
    // closure / builtin sentinel, store as-is.
    let def = match &args[1] {
        Sexp::Symbol(s) => env.lookup_function(s)?,
        other => other.clone(),
    };
    env.set_function(&name, def);
    Ok(args[0].clone())
}

/// `(set SYMBOL VALUE)` — store VALUE in the value cell of SYMBOL.
/// Required by Stage 7.3.d (Doc 67) elisp `defvar' / `defconst'
/// macros that previously bypassed via Rust `sf_defvar' /
/// `sf_defconst' (which directly called `env.set_value').  Returns
/// VALUE (= host Emacs contract).
fn bi_set(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("set", args, 2, Some(2))?;
    let name = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    };
    env.set_value(&name, args[1].clone())?;
    Ok(args[1].clone())
}

/// `(fset SYMBOL DEFINITION)` — same as `defalias' but without the
/// optional docstring slot.  Returns DEFINITION (= host Emacs
/// contract).
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

/// `(fmakunbound SYMBOL)` — clear the function cell of SYMBOL.
fn bi_fmakunbound(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("fmakunbound", args, 1, Some(1))?;
    let name = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    };
    env.clear_function(&name);
    Ok(args[0].clone())
}

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

// ---- Phase 7 Stage 7.4.a — apply/call/closure/env elisp 化用 補助 builtins ----
//
// Doc 68 §2.4 で定めた 5 件。Stage 7.4.b で install される
// `lisp/nelisp-stdlib-eval-core.el' の elisp 側 `nelisp--apply-fn' /
// `nelisp--apply-closure' / `nelisp--bind-formals' が Rust frame stack
// を操作するための薄いラッパ。Stage 7.4.a の段階では Rust 側 ERT のみ
// が呼ぶ (= dormant、elisp 本体が install される前から primitive 自体は
// 利用可能にしておく)。

/// `(nelisp--push-frame)` — push a fresh empty lexical frame onto the
/// stack.  Returns t.  Pair with `nelisp--pop-frame'.
fn bi_push_frame(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--push-frame", args, 0, Some(0))?;
    env.push_frame();
    Ok(Sexp::T)
}

/// `(nelisp--pop-frame)` — pop the innermost lexical frame.  Returns t.
/// Silently no-ops on under-pop (= matches `Env::pop_frame' contract).
fn bi_pop_frame(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--pop-frame", args, 0, Some(0))?;
    env.pop_frame();
    Ok(Sexp::T)
}

/// `(nelisp--push-captured ALIST)` — push a frame populated from a
/// captured-env alist of `((NAME . CELL) ...)' shape.  Used by
/// `nelisp--apply-closure' to install the closure's captured lexical
/// scope before the formal-param binding frame.  Returns t.
fn bi_push_captured(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--push-captured", args, 1, Some(1))?;
    env.push_captured(&args[0])?;
    Ok(Sexp::T)
}

/// `(nelisp--bind-local NAME VALUE)` — bind NAME to VALUE in the
/// innermost lexical frame.  Returns VALUE.  Mirrors `Env::bind_local'
/// semantics: if no frame exists, the binding falls through to the
/// global value slot (= top-level setq behaviour).
fn bi_bind_local(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
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

/// `(nelisp--set-use-elisp-apply T-OR-NIL)` — flip the Stage 7.4.c
/// takeover flag at runtime.  Returns the new value (= the arg
/// converted to t/nil).  Used by ERT to exercise both Rust dispatch
/// and elisp dispatch within a single subprocess.
fn bi_set_use_elisp_apply(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--set-use-elisp-apply", args, 1, Some(1))?;
    let truthy = !matches!(args[0], Sexp::Nil);
    env.use_elisp_apply = truthy;
    Ok(if truthy { Sexp::T } else { Sexp::Nil })
}

/// `(nelisp--get-use-elisp-apply)` — read the current Stage 7.4.c
/// takeover flag.  Returns t/nil.
fn bi_get_use_elisp_apply(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("nelisp--get-use-elisp-apply", args, 0, Some(0))?;
    Ok(if env.use_elisp_apply { Sexp::T } else { Sexp::Nil })
}

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

/// `(makunbound SYMBOL)` — clear the value cell of SYMBOL.
fn bi_makunbound(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("makunbound", args, 1, Some(1))?;
    let name = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol".into(),
            got: other.clone(),
        }),
    };
    env.clear_value(&name);
    Ok(args[0].clone())
}

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

// bi_error removed — see lisp/nelisp-stdlib-misc.el (Rust-min
// 2026-05-06 batch 6m).  The 3-step (format, build msg, signal)
// pipeline is fully expressible in elisp once `format' is in elisp;
// `signal' (still Rust) does the actual stack-unwind.

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

// bi_princ removed — see lisp/nelisp-stdlib-misc.el (Rust-min
// 2026-05-06 batch 6i).  The stringp/Display dispatch is fully
// expressible in elisp once `nelisp--write-stdout-bytes' exists as
// a primitive (just above) — and `prin1-to-string' produces the
// Display-format output for non-string inputs.

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

// bi_message removed — see lisp/nelisp-stdlib-misc.el (Rust-min
// 2026-05-06 batch 6h).  The nil-guard + format + writeln logic is
// fully expressible in elisp once `nelisp--write-stderr-line'
// exists as a primitive (just above).

/// `(nelisp--format-float-body CONV PREC X)' — return the unsigned,
/// unpadded body string for a `format' float-conversion (CONV one of
/// ?f ?F ?e ?E ?g ?G).  PREC is the precision (>= 0); X is the
/// magnitude (the elisp dispatcher already took the absolute value
/// and will prepend the sign + apply width / padding itself).
///
/// Sole survivor of the Rust-min batch 6m migration of `format' to
/// elisp: the IEEE-754 round-to-nearest-decimal logic is provided by
/// Rust's `{:.*}' / `{:.*e}' / `{:.*E}' format machinery, which is
/// not feasibly re-implementable in pure elisp without ~1000 LOC of
/// Grisu / dragon4.  Everything else (= spec parser, padding, sign,
/// integer→radix-string, %s / %S / %c / %% / %d / %i / %x / %X /
/// %o / %b) lives in lisp/nelisp-stdlib-plist-str.el.
fn bi_format_float_body(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--format-float-body", args, 3, Some(3))?;
    let conv = match &args[0] {
        Sexp::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
            EvalError::Internal(format!("nelisp--format-float-body: bad conv code {}", n))
        })?,
        other => {
            return Err(EvalError::WrongType {
                expected: "integerp".into(),
                got: other.clone(),
            })
        }
    };
    let prec = match &args[1] {
        Sexp::Int(n) if *n >= 0 => *n as usize,
        other => {
            return Err(EvalError::WrongType {
                expected: "non-negative integerp".into(),
                got: other.clone(),
            })
        }
    };
    let x = match &args[2] {
        Sexp::Float(x) => *x,
        Sexp::Int(n) => *n as f64,
        other => {
            return Err(EvalError::WrongType {
                expected: "numberp".into(),
                got: other.clone(),
            })
        }
    };
    let body = match conv {
        'f' | 'F' => format!("{:.*}", prec, x),
        'e' => format!("{:.*e}", prec, x),
        'E' => format!("{:.*E}", prec, x),
        'g' | 'G' => {
            let f = format!("{:.*}", prec, x);
            let e = format!("{:.*e}", prec, x);
            if f.len() <= e.len() {
                f
            } else {
                e
            }
        }
        other => {
            return Err(EvalError::Internal(format!(
                "nelisp--format-float-body: unsupported conv %{}",
                other
            )))
        }
    };
    Ok(Sexp::Str(body))
}

/// `(truncate X)' — return X truncated toward zero as an integer.
/// For a Float argument we cast via `as i64' (= the same trunc-toward-
/// zero semantics the previous `bi_format' used inline for `%d FLOAT').
/// For an Int argument we return it unchanged.  Added in Rust-min
/// batch 6m so the elisp `format' dispatcher can coerce float→int
/// for `%d/%i/%x/%X/%o/%b' without needing a privileged float-cast.
fn bi_truncate(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("truncate", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(n) => Ok(Sexp::Int(*n)),
        Sexp::Float(x) => Ok(Sexp::Int(*x as i64)),
        other => Err(EvalError::WrongType {
            expected: "numberp".into(),
            got: other.clone(),
        }),
    }
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
fn bi_nl_current_unix_time(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-current-unix-time", args, 0, Some(0))?;
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    Ok(Sexp::Int(now))
}

/// `(nl-secure-hash ALGO STRING)` — return the lowercase hex digest of
/// STRING under ALGO (= 'sha1, 'sha256, 'md5, 'sha224, 'sha384,
/// 'sha512).  Mirrors the host Emacs `secure-hash' API surface anvil-
/// memory-add reaches for.  Implemented in pure Rust via the `sha1' /
/// `sha2' / `md5' crates so build-tool stays self-contained.
fn bi_nl_secure_hash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-secure-hash", args, 2, Some(2))?;
    let algo = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        Sexp::Str(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol or string (algo)".into(),
            got: other.clone(),
        }),
    };
    let text = string_value(&args[1])?;
    let bytes = text.as_bytes();
    let hex = match algo.as_str() {
        "sha1" => {
            use sha1::{Digest, Sha1};
            let mut h = Sha1::new();
            h.update(bytes);
            hex_lower(&h.finalize())
        }
        "sha256" => {
            use sha2::{Digest, Sha256};
            let mut h = Sha256::new();
            h.update(bytes);
            hex_lower(&h.finalize())
        }
        "sha224" => {
            use sha2::{Digest, Sha224};
            let mut h = Sha224::new();
            h.update(bytes);
            hex_lower(&h.finalize())
        }
        "sha384" => {
            use sha2::{Digest, Sha384};
            let mut h = Sha384::new();
            h.update(bytes);
            hex_lower(&h.finalize())
        }
        "sha512" => {
            use sha2::{Digest, Sha512};
            let mut h = Sha512::new();
            h.update(bytes);
            hex_lower(&h.finalize())
        }
        "md5" => {
            let digest = md5::compute(bytes);
            hex_lower(&digest.0)
        }
        other => return Err(EvalError::Internal(format!(
            "nl-secure-hash: unsupported algo {:?}", other
        ))),
    };
    Ok(Sexp::Str(hex))
}

/// `(nl-format-unix-time FORMAT EPOCH-INT)` → string formatted via
/// chrono::DateTime<Utc>.format() with strftime-style FORMAT.
/// EPOCH-INT is seconds since the Unix epoch.  Returns Sexp::Str.
fn bi_nl_format_unix_time(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-format-unix-time", args, 2, Some(2))?;
    let fmt = string_value(&args[0])?;
    let epoch = match &args[1] {
        Sexp::Int(i) => *i,
        Sexp::Float(f) => *f as i64,
        other => return Err(EvalError::WrongType {
            expected: "integer (unix epoch)".into(),
            got: other.clone(),
        }),
    };
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_opt(epoch, 0).single().ok_or_else(|| {
        EvalError::Internal(format!("nl-format-unix-time: invalid epoch {}", epoch))
    })?;
    Ok(Sexp::Str(dt.format(&fmt).to_string()))
}

fn bi_nl_downcase(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-downcase", args, 1, Some(1))?;
    let s = string_value(&args[0])?;
    Ok(Sexp::Str(s.to_lowercase()))
}

fn bi_nl_upcase(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-upcase", args, 1, Some(1))?;
    let s = string_value(&args[0])?;
    Ok(Sexp::Str(s.to_uppercase()))
}

/// `(nl-split-by-non-alnum STRING &optional OMIT-EMPTY)` — split STRING
/// on runs of non-alphanumeric characters.  When OMIT-EMPTY is non-nil
/// (default behavior of Elisp `split-string ... t`), drops empty
/// fragments.  This is the common-case shortcut for Elisp's
/// `(split-string s "[^[:alnum:]]+" t)' idiom that anvil-memory's
/// tokenizer reaches for.
fn bi_nl_split_by_non_alnum(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-split-by-non-alnum", args, 1, Some(2))?;
    let s = string_value(&args[0])?;
    let omit_empty = args.get(1).map(|v| !matches!(v, Sexp::Nil)).unwrap_or(true);
    let parts: Vec<Sexp> = s
        .split(|c: char| !c.is_alphanumeric())
        .filter(|p| if omit_empty { !p.is_empty() } else { true })
        .map(|p| Sexp::Str(p.to_string()))
        .collect();
    Ok(Sexp::list_from(&parts))
}

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

// `bi_min' / `bi_max' / `bi_abs' removed — Rust-min batch 7g
// (2026-05-07): all three migrated to elisp folds over the existing
// 2-arg `<' / `>' / `nelisp--sub2' primitives.  See
// lisp/nelisp-stdlib.el.  Result type now matches the winning arg's
// type (= host Emacs contract: `(min 1 2.5)' returns 1, not 1.0)
// where the prior Rust impl always coerced to float when any arg
// was a float; tree-internal callers were all-int so no behavioural
// surprise.

fn bi_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("float", args, 1, Some(1))?;
    Ok(Sexp::Float(to_f64(&args[0])?))
}

fn bi_exp(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("exp", args, 1, Some(1))?;
    Ok(Sexp::Float(to_f64(&args[0])?.exp()))
}

fn bi_log(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("log", args, 1, Some(2))?;
    let x = to_f64(&args[0])?;
    let base = match args.get(1) {
        Some(b) => to_f64(b)?,
        None => std::f64::consts::E,
    };
    Ok(Sexp::Float(x.log(base)))
}

// `bi_floor' / `bi_ceiling' / `bi_round' removed — Rust-min batch 7h
// (2026-05-07): all three migrated to elisp wrappers (see
// lisp/nelisp-stdlib.el).  The float-division kernel stays in Rust as
// the unified `nelisp--f64-trunc' primitive below — symbol-dispatched
// over the four trunc modes that f64 exposes.  Integer 1-arg cases
// short-circuit on the elisp side without entering this primitive.
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

fn hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        out.push_str(&format!("{:02x}", b));
    }
    out
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

// `bi_provide' / `bi_featurep' removed — Rust-min batch 7i (2026-05-07,
// Doc 50 stage 2): both migrated to elisp on top of the `features'
// dynamic var, which is now the single canonical source of provided-
// feature state.  The previous `Env::features' HashSet (which both
// the old Rust `provide' wrote to AND `featurep' read from) duplicated
// the same information and forced `bi_provide' to manually mirror its
// writes onto the elisp-visible `features' var — that mirror logic
// (and the HashSet itself) is gone.  See lisp/nelisp-stdlib-misc.el.

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

// ===== Generic accessors (Phase 8.x core completion) =====================
//
// `aref' / `elt' are language-level operations (Elisp manual §6.6
// "Sequences, Arrays, and Vectors") that anvil-pkg-compat and most
// real-world Elisp packages assume the runtime ships.  Boundary
// policy: language rule -> NeLisp core; Emacs/OS API -> Layer 2.

// `bi_aref' / `bi_elt' deleted — Phase 5 Stage C-Phase1b (Doc 62,
// 2026-05-08).  See `jit/access.rs::lowered_{aref,elt}' (= JIT fast
// path + inline aref_helper for MutStr / CharTable / BoolVector + Cons
// walk for elt).

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

// bi_vconcat removed — see lisp/nelisp-stdlib-plist-str.el (Rust-min
// 2026-05-06 batch 6c).  Reduces to `(apply #'vector (apply #'append
// (append args (list nil))))'; the existing `bi_append' already
// flattens vectors / strings / lists, so there is no Sexp-internal
// logic worth retaining here.

// `bi_setcar' / `bi_setcdr' deleted in Phase 5 Stage C-Phase1
// (Doc 62, 2026-05-08).  See `jit/cons.rs::lowered_{setcar,setcdr}'.

// `bi_aset' deleted — Phase 5 Stage C-Phase1b (Doc 62, 2026-05-08).
// See `jit/access.rs::lowered_aset' (= JIT fast path + inline MutStr
// codepoint mutation / CharTable / BoolVector / immutable-Str
// rejection).
