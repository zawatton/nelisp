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
use crate::reader::sexp::CharTableInner;
use std::cell::RefCell;
use std::rc::Rc;
use super::special_forms::{is_truthy, sexp_eq};
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
        // 2-arg primitives.  / kept in Rust due to upfront-promote
        // semantics (= step-wise fold would lose precision when
        // later args are float, e.g. (/ 10 3 2.0) = 1.666 vs 1.5).
        "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
        "/",
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
        "eq", "equal",
        // cons / list
        // Rust-min (2026-05-06 batch 6o): `append' migrated to elisp
        // (lisp/nelisp-stdlib-list.el).
        "car", "cdr", "cons", "length",
        "caar", "cadr", "cdar", "cddr",
        "caaar", "caadr", "cadar", "caddr",
        "cdaar", "cdadr", "cddar", "cdddr", "cadddr",
        "setcar", "setcdr",
        // generic sequence / array accessors
        // Rust-min (2026-05-06 batch 6q): `arrayp' / `sequencep'
        // migrated to elisp (lisp/nelisp-stdlib.el).
        "aref", "aset", "elt",
        // Rust-min (2026-05-06 batch 6c): vconcat migrated to elisp
        // (lisp/nelisp-stdlib-plist-str.el).
        "vector", "make-vector",
        // predicates
        // Rust-min (2026-05-06 batch 6q): `atom' migrated to elisp
        // (lisp/nelisp-stdlib.el) as `(not (consp x))'.
        // Rust-min (2026-05-06 batch 6u): consp / listp / symbolp /
        // stringp / numberp / integerp / floatp / vectorp migrated to
        // elisp (lisp/nelisp-stdlib.el) as `(eq (type-of x) 'TAG)' on
        // top of the new `type-of' primitive.  `functionp' kept in
        // Rust (HOF dispatch hot path).
        "type-of", "functionp",
        // Rust-min (2026-05-06 batch 6d): `null' shadowed by elisp.
        // Rust-min (2026-05-06 batch 6f): `booleanp' / `keywordp'
        // expressible from `eq' / `symbolp' + `symbol-name' + `aref'
        // — moved to elisp (lisp/nelisp-stdlib-misc.el).
        "vectorp",
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
        // -logxor2.  `lognot' (unary) and `ash' (binary) stay native.
        "lognot", "ash",
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
        "intern", "symbol-name",
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
        // Rust-min (2026-05-06 batch 6a): gensym migrated to elisp
        // (lisp/nelisp-stdlib-misc.el).
        "make-symbol",
        // hash-tables (Track O'')
        // Rust-min (2026-05-06 batch 6k): `hash-table-keys' /
        // `hash-table-values' migrated to elisp via `maphash' fold
        // (lisp/nelisp-stdlib-misc.el).
        "make-hash-table", "hash-table-p", "hash-table-count",
        "puthash", "gethash", "remhash", "clrhash", "maphash",
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
        "expand-file-name", "file-truename",
        // file I/O (Doc 47 Stage 8b — multi-file load chain)
        "file-exists-p", "file-readable-p", "file-directory-p",
        "file-regular-p", "file-name-extension", "directory-files",
        "load", "locate-library",
        // Rust-min (2026-05-06): `file-name-directory' /
        // `file-name-nondirectory' / `file-name-as-directory' /
        // `directory-file-name' migrated to elisp (see
        // lisp/nelisp-stdlib-plist-str.el).
        // symbol / function
        "symbol-value", "symbol-function", "fboundp", "boundp", "funcall", "apply", "eval",
        "defalias", "fset", "fmakunbound", "makunbound",
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
        "signal", "prin1-to-string",
        "nelisp--write-stderr-line",
        "nelisp--write-stdout-bytes",
        "provide", "require", "featurep",
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
        // formula (exp/log/min/max/float coercion).
        "min", "max", "float", "exp", "log", "abs", "floor", "ceiling", "round",
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
        // + / - / * migrated to elisp (Rust-min 2026-05-06 batch 6v,
        // see lisp/nelisp-stdlib.el).  2-arg primitives:
        "nelisp--add2" => bi_add2(args),
        "nelisp--sub2" => bi_sub2(args),
        "nelisp--mul2" => bi_mul2(args),
        "/" => bi_div(args),
        // mod migrated to elisp (Rust-min 2026-05-06 batch 6l, see
        // lisp/nelisp-stdlib.el).
        // < / > / <= / >= / = / /= migrated to elisp (Rust-min
        // 2026-05-06 batch 6w, see lisp/nelisp-stdlib.el).
        "nelisp--num-lt2" => bi_num_lt2(args),
        "nelisp--num-gt2" => bi_num_gt2(args),
        "nelisp--num-le2" => bi_num_le2(args),
        "nelisp--num-ge2" => bi_num_ge2(args),
        "nelisp--num-eq2" => bi_num_eq2(args),
        // ---- equality ----
        // Rust-min (2026-05-06 batch 6e): `equal-including-properties'
        // / `eql' moved to elisp defalias of `equal'.  The MVP impl
        // collapses both into the strict structural-equality path, so
        // the dispatch fanout was a typing burden with no runtime
        // distinction.
        "eq" => bi_eq(args),
        "equal" => bi_equal(args),
        // ---- cons / list ----
        "car" => bi_car(args),
        "cdr" => bi_cdr(args),
        // Common compositions — substrate code uses these everywhere.
        "caar"   => bi_car(&[bi_car(args)?]),
        "cadr"   => bi_car(&[bi_cdr(args)?]),
        "cdar"   => bi_cdr(&[bi_car(args)?]),
        "cddr"   => bi_cdr(&[bi_cdr(args)?]),
        "caaar"  => bi_car(&[bi_car(&[bi_car(args)?])?]),
        "caadr"  => bi_car(&[bi_car(&[bi_cdr(args)?])?]),
        "cadar"  => bi_car(&[bi_cdr(&[bi_car(args)?])?]),
        "caddr"  => bi_car(&[bi_cdr(&[bi_cdr(args)?])?]),
        "cdaar"  => bi_cdr(&[bi_car(&[bi_car(args)?])?]),
        "cdadr"  => bi_cdr(&[bi_car(&[bi_cdr(args)?])?]),
        "cddar"  => bi_cdr(&[bi_cdr(&[bi_car(args)?])?]),
        "cdddr"  => bi_cdr(&[bi_cdr(&[bi_cdr(args)?])?]),
        "cadddr" => bi_car(&[bi_cdr(&[bi_cdr(&[bi_cdr(args)?])?])?]),
        "cons" => bi_cons(args),
        "length" => bi_length(args),
        // reverse / nreverse migrated to elisp (Rust-min 2026-05-06
        // batch 6d, see lisp/nelisp-stdlib-list.el).
        // copy-sequence migrated to elisp (Rust-min 2026-05-06
        // batch 6g, see lisp/nelisp-stdlib-misc.el).
        // copy-tree / sort migrated to elisp (Rust-min 2026-05-06 batch 4).
        // append migrated to elisp (Rust-min 2026-05-06 batch 6o,
        // see lisp/nelisp-stdlib-list.el).
        "setcar" => bi_setcar(args),
        "setcdr" => bi_setcdr(args),
        // ---- generic accessors ----
        "aref" => bi_aref(args),
        "aset" => bi_aset(args),
        "elt" => bi_elt(args),
        // arrayp / sequencep migrated to elisp (Rust-min 2026-05-06
        // batch 6q, see lisp/nelisp-stdlib.el).
        "vector" => Ok(Sexp::vector(args.to_vec())),
        "make-vector" => bi_make_vector(args),
        // ---- predicates ----
        // consp / listp / symbolp / stringp / numberp / integerp /
        // floatp / vectorp migrated to elisp (Rust-min 2026-05-06
        // batch 6u, see lisp/nelisp-stdlib.el) on top of the new
        // `type-of' primitive.  atom migrated separately in batch 6q.
        "type-of" => bi_type_of(args),
        "functionp" => bi_predicate(args, |v| matches!(v,
            Sexp::Cons(h, _) if matches!(&*h.borrow(),
                Sexp::Symbol(s) if s == "lambda" || s == "closure" || s == "builtin"))),
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
        "nelisp--logior2" => bi_logior2(args),
        "nelisp--logand2" => bi_logand2(args),
        "nelisp--logxor2" => bi_logxor2(args),
        "lognot" => bi_lognot(args),
        // Rust-min (2026-05-06 batch 6e): `lsh' / `sxhash-{equal,eq,eql}'
        // moved to elisp defalias of `ash' / `sxhash'.
        "ash" => bi_ash(args),
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
        "intern" => bi_intern(args),
        "symbol-name" => bi_symbol_name(args),
        // Rust-min (2026-05-06 batch 6e): `string=' moved to elisp
        // defalias of `string-equal'.
        // string-equal migrated to elisp (Rust-min 2026-05-06 batch 6n,
        // see lisp/nelisp-stdlib-plist-str.el; dead body removed in 6t).
        "string-match-p" => bi_string_match_p(args),
        // "regexp-quote" — migrated to elisp (Rust-min 2026-05-06)
        "expand-file-name" => bi_expand_file_name(args, env),
        "file-truename" => bi_file_truename(args, env),
        // Doc 47 Stage 8b — file I/O for multi-file load chains.
        // file-name-* path slicers migrated to elisp (Rust-min 2026-05-06,
        // see lisp/nelisp-stdlib-plist-str.el).
        "file-exists-p" => bi_file_exists_p(args, env),
        "file-readable-p" => bi_file_readable_p(args, env),
        "file-directory-p" => bi_file_directory_p(args, env),
        "file-regular-p" => bi_file_regular_p(args, env),
        "file-name-extension" => bi_file_name_extension(args),
        "directory-files" => bi_directory_files(args, env),
        "load" => bi_load(args, env),
        "locate-library" => bi_locate_library(args, env),
        // ---- symbol / function ----
        "symbol-value" => bi_symbol_value(args, env),
        "symbol-function" => bi_symbol_function(args, env),
        "fboundp" => bi_fboundp(args, env),
        "boundp" => bi_boundp(args, env),
        "defalias" => bi_defalias(args, env),
        "fset" => bi_fset(args, env),
        "fmakunbound" => bi_fmakunbound(args, env),
        "makunbound" => bi_makunbound(args, env),
        // intern-soft migrated to elisp (Rust-min 2026-05-06 batch 6f,
        // see lisp/nelisp-stdlib-misc.el).
        "make-symbol" => bi_make_symbol(args),
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
        "make-hash-table" => bi_make_hash_table(args),
        "hash-table-p" => bi_hash_table_p(args),
        "hash-table-count" => bi_hash_table_count(args),
        "puthash" => bi_puthash(args),
        "gethash" => bi_gethash(args),
        "remhash" => bi_remhash(args),
        "clrhash" => bi_clrhash(args),
        "maphash" => bi_maphash(args, env),
        // hash-table-keys / hash-table-values migrated to elisp
        // (Rust-min 2026-05-06 batch 6k, see lisp/nelisp-stdlib-misc.el).
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
        "prin1-to-string" => bi_prin1_to_string(args),
        // message migrated to elisp (Rust-min 2026-05-06 batch 6h,
        // see lisp/nelisp-stdlib-misc.el).  The writeln-to-stderr
        // primitive is `nelisp--write-stderr-line'.
        "nelisp--write-stderr-line" => bi_write_stderr_line(args),
        "read-stdin-bytes" => bi_read_stdin_bytes(args),
        "nl-ffi-call" => super::ffi::nl_ffi_call(args),
        "nl-ffi-malloc" => super::ffi::nl_ffi_malloc(args),
        "nl-ffi-read-bytes" => super::ffi::nl_ffi_read_bytes(args),
        "nl-ffi-free" => super::ffi::nl_ffi_free(args),
        "nl-current-unix-time" => bi_nl_current_unix_time(args),
        "nl-secure-hash" => bi_nl_secure_hash(args),
        "nl-format-unix-time" => bi_nl_format_unix_time(args),
        "nl-downcase" => bi_nl_downcase(args),
        "nl-upcase" => bi_nl_upcase(args),
        "nl-split-by-non-alnum" => bi_nl_split_by_non_alnum(args),
        "min" => bi_min(args),
        "max" => bi_max(args),
        "float" => bi_float(args),
        "exp" => bi_exp(args),
        "log" => bi_log(args),
        "abs" => bi_abs(args),
        "floor" => bi_floor(args),
        "ceiling" => bi_ceiling(args),
        "round" => bi_round(args),
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
        "read" => bi_read(args),
        "read-from-string" => bi_read_from_string(args),
        "locate-library" => bi_locate_library(args, env),
        "provide" => bi_provide(args, env),
        "require" => bi_require(args, env),
        "featurep" => bi_featurep(args, env),
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

fn require_arity(name: &str, args: &[Sexp], min: usize, max: Option<usize>) -> Result<(), EvalError> {
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

fn as_int(name: &str, v: &Sexp) -> Result<i64, EvalError> {
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

/// Numeric promotion: if any input is float, output is float.
fn numeric_promote(args: &[Sexp]) -> Result<(bool, Vec<f64>), EvalError> {
    let mut any_float = false;
    let mut out = Vec::with_capacity(args.len());
    for a in args {
        match a {
            Sexp::Int(n) => out.push(*n as f64),
            Sexp::Float(x) => {
                any_float = true;
                out.push(*x);
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "number".into(),
                    got: other.clone(),
                })
            }
        }
    }
    Ok((any_float, out))
}

fn pack_number(any_float: bool, x: f64) -> Sexp {
    if any_float {
        Sexp::Float(x)
    } else {
        Sexp::Int(x as i64)
    }
}

// ---------- arithmetic implementations ----------
//
// `all_integer' fast-path helper removed in batch 6v — its sole
// remaining callers (bi_add / bi_sub / bi_mul) are gone, and bi_div
// always uses `numeric_promote'.  Doc 51's row-hash precision
// concern (= sxhash-equal collision above 2^53) is handled at the
// `nelisp--add2' / `nelisp--mul2' boundary: int+int stays int with
// wrapping arithmetic, so no precision loss when both args are
// integer (the only path that mattered for that bug).
//
// bi_add / bi_sub / bi_mul removed — see top of this section.

fn bi_div(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("/", args, 1, None)?;
    let (af, vs) = numeric_promote(args)?;
    if vs.len() == 1 {
        if vs[0] == 0.0 {
            return Err(EvalError::ArithError("division by zero".into()));
        }
        return Ok(pack_number(af, 1.0 / vs[0]));
    }
    let mut acc = vs[0];
    for v in vs.iter().skip(1) {
        if *v == 0.0 {
            return Err(EvalError::ArithError("division by zero".into()));
        }
        acc /= v;
    }
    if !af {
        // Integer truncation for all-int inputs.
        Ok(Sexp::Int(acc.trunc() as i64))
    } else {
        Ok(Sexp::Float(acc))
    }
}

// bi_mod removed — see lisp/nelisp-stdlib.el (Rust-min 2026-05-06
// batch 6l).  Built from `/' (int trunc-div) plus a sign-adjust
// step that reproduces the previous `rem_euclid' + sign(b) result
// shape exactly.
//
// bi_add / bi_sub / bi_mul removed — see lisp/nelisp-stdlib.el
// (Rust-min 2026-05-06 batch 6v).  Variadic + / - / * collapse to
// elisp folds over the new 2-arg primitives `nelisp--add2' /
// `nelisp--sub2' / `nelisp--mul2' (just below).  bi_div retained
// because its variadic semantics promote ALL args to f64 upfront
// (= float division throughout, trunc only at end IF originally
// all-int) — a step-wise fold would lose precision when later args
// are float (e.g. `(/ 10 3 2.0)' = 1.666 upfront vs 1.5 step-wise).

fn num_pair(args: &[Sexp], name: &str) -> Result<(f64, f64, bool), EvalError> {
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

fn int_pair_or<F>(args: &[Sexp], name: &str, mixed: F) -> Result<Sexp, EvalError>
where
    F: FnOnce(f64, f64) -> f64,
{
    let (a, b, af) = num_pair(args, name)?;
    if af {
        Ok(Sexp::Float(mixed(a, b)))
    } else {
        // Both args are Int — promote-then-cast loses no precision
        // since neither was Float; use the original i64 path for
        // wrapping arithmetic.  Caller dispatches via the Int branch.
        unreachable!("caller should special-case all-int")
    }
}

/// `(nelisp--add2 A B)' — 2-arg add building block for the elisp
/// `+' fold.  Wrapping semantics for int+int (= matches host emacs
/// integer-overflow behaviour); promote to float when either arg
/// is Float.
fn bi_add2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--add2", args, 2, Some(2))?;
    if let (Sexp::Int(a), Sexp::Int(b)) = (&args[0], &args[1]) {
        return Ok(Sexp::Int(a.wrapping_add(*b)));
    }
    int_pair_or(args, "nelisp--add2", |a, b| a + b)
}

/// `(nelisp--sub2 A B)' — 2-arg subtract building block.  Same
/// promotion rules as `nelisp--add2'; integer wrapping.
fn bi_sub2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--sub2", args, 2, Some(2))?;
    if let (Sexp::Int(a), Sexp::Int(b)) = (&args[0], &args[1]) {
        return Ok(Sexp::Int(a.wrapping_sub(*b)));
    }
    int_pair_or(args, "nelisp--sub2", |a, b| a - b)
}

/// `(nelisp--mul2 A B)' — 2-arg multiply building block.  Same
/// promotion rules as `nelisp--add2'; integer wrapping.
fn bi_mul2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--mul2", args, 2, Some(2))?;
    if let (Sexp::Int(a), Sexp::Int(b)) = (&args[0], &args[1]) {
        return Ok(Sexp::Int(a.wrapping_mul(*b)));
    }
    int_pair_or(args, "nelisp--mul2", |a, b| a * b)
}

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

fn bi_logior2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--logior2", args, 2, Some(2))?;
    Ok(Sexp::Int(as_int("nelisp--logior2", &args[0])?
                 | as_int("nelisp--logior2", &args[1])?))
}

fn bi_logand2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--logand2", args, 2, Some(2))?;
    Ok(Sexp::Int(as_int("nelisp--logand2", &args[0])?
                 & as_int("nelisp--logand2", &args[1])?))
}

fn bi_logxor2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nelisp--logxor2", args, 2, Some(2))?;
    Ok(Sexp::Int(as_int("nelisp--logxor2", &args[0])?
                 ^ as_int("nelisp--logxor2", &args[1])?))
}

fn bi_lognot(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("lognot", args, 1, Some(1))?;
    Ok(Sexp::Int(!as_int("lognot", &args[0])?))
}

/// `(ash N COUNT)' / `(lsh N COUNT)' — arithmetic shift by COUNT bits.
/// Positive COUNT is left shift, negative is right shift.  We treat
/// `lsh` as an alias of `ash` per recent Emacs conventions (= the
/// MVP doesn't distinguish logical vs arithmetic for the use-cases
/// that matter, key-event encoding being the main one).
fn bi_ash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("ash", args, 2, Some(2))?;
    let n = as_int("ash", &args[0])?;
    let count = as_int("ash", &args[1])?;
    let r = if count >= 0 {
        // Left shift; clamp obscene shifts to avoid Rust panic.
        if count >= 63 { 0 } else { n.wrapping_shl(count as u32) }
    } else {
        let abs = (-count) as u32;
        if abs >= 63 { if n < 0 { -1 } else { 0 } } else { n >> abs }
    };
    Ok(Sexp::Int(r))
}

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
    use std::hash::{Hash, Hasher};
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
        Sexp::MutStr(rc) => { 5u8.hash(h); rc.borrow().hash(h); }
        Sexp::Cons(car, cdr) => {
            6u8.hash(h);
            sxhash_into(&car.borrow(), h);
            sxhash_into(&cdr.borrow(), h);
        }
        Sexp::Vector(rc) => {
            7u8.hash(h);
            for it in rc.borrow().iter() { sxhash_into(it, h); }
        }
        Sexp::HashTable(_) => 8u8.hash(h),
        Sexp::CharTable(_) => 9u8.hash(h),
        Sexp::BoolVector(rc) => {
            10u8.hash(h);
            for &b in rc.borrow().iter() { (b as u8).hash(h); }
        }
        // Lexical-binding storage cell — hash through to inner value
        // (= cells should be invisible to user-facing sxhash).
        Sexp::Cell(rc) => sxhash_into(&rc.borrow(), h),
    }
}

// bi_lt / bi_gt / bi_le / bi_ge / bi_eq_num / bi_neq_num removed —
// see lisp/nelisp-stdlib.el (Rust-min 2026-05-06 batch 6w).
// Variadic chained-pairwise comparisons (`(< a b c)' = `(and (< a b)
// (< b c))') collapse to elisp folds over new 2-arg primitives.
// Float-tolerance `=' uses `1e-15' epsilon — moved to the
// `nelisp--num-eq2' primitive.  `/=' is just `(not (= a b))'.

fn cmp2_helper(args: &[Sexp], name: &str, cmp: fn(f64, f64) -> bool) -> Result<Sexp, EvalError> {
    require_arity(name, args, 2, Some(2))?;
    let (_, vs) = numeric_promote(args)?;
    Ok(if cmp(vs[0], vs[1]) { Sexp::T } else { Sexp::Nil })
}

fn bi_num_lt2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp2_helper(args, "nelisp--num-lt2", |a, b| a < b)
}
fn bi_num_gt2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp2_helper(args, "nelisp--num-gt2", |a, b| a > b)
}
fn bi_num_le2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp2_helper(args, "nelisp--num-le2", |a, b| a <= b)
}
fn bi_num_ge2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp2_helper(args, "nelisp--num-ge2", |a, b| a >= b)
}
fn bi_num_eq2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp2_helper(args, "nelisp--num-eq2", |a, b| (a - b).abs() < 1e-15)
}

// ---------- equality ----------

fn bi_eq(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("eq", args, 2, Some(2))?;
    Ok(if sexp_eq(&args[0], &args[1]) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_equal(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("equal", args, 2, Some(2))?;
    Ok(if sexp_equal_safe(&args[0], &args[1], 0) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

/// Cycle-safe structural equality.  For heap-backed variants (Cons,
/// Vector, MutStr, etc.) we first short-circuit via `Rc::ptr_eq': two
/// references to the same allocation are trivially equal, AND this
/// breaks the recursion when a cyclic graph reaches the same node
/// from two paths (e.g. cl-defstruct parent <-> children).  A bounded
/// recursion depth is a backstop against pathological non-shared
/// graphs that we have not encountered in practice.
const SEXP_EQUAL_DEPTH_LIMIT: u32 = 4096;

fn sexp_equal_safe(a: &Sexp, b: &Sexp, depth: u32) -> bool {
    if depth > SEXP_EQUAL_DEPTH_LIMIT {
        return sexp_eq(a, b);
    }
    match (a, b) {
        (Sexp::Cons(a1, a2), Sexp::Cons(b1, b2)) => {
            if std::rc::Rc::ptr_eq(a1, b1) && std::rc::Rc::ptr_eq(a2, b2) {
                return true;
            }
            sexp_equal_safe(&a1.borrow(), &b1.borrow(), depth + 1)
                && sexp_equal_safe(&a2.borrow(), &b2.borrow(), depth + 1)
        }
        (Sexp::Vector(a), Sexp::Vector(b)) => {
            if std::rc::Rc::ptr_eq(a, b) {
                return true;
            }
            let av = a.borrow();
            let bv = b.borrow();
            av.len() == bv.len()
                && av
                    .iter()
                    .zip(bv.iter())
                    .all(|(x, y)| sexp_equal_safe(x, y, depth + 1))
        }
        (Sexp::MutStr(a), Sexp::MutStr(b)) => {
            std::rc::Rc::ptr_eq(a, b) || *a.borrow() == *b.borrow()
        }
        (Sexp::HashTable(a), Sexp::HashTable(b)) => std::rc::Rc::ptr_eq(a, b) || a == b,
        (Sexp::CharTable(a), Sexp::CharTable(b)) => std::rc::Rc::ptr_eq(a, b) || a == b,
        (Sexp::BoolVector(a), Sexp::BoolVector(b)) => std::rc::Rc::ptr_eq(a, b) || a == b,
        // For the trivial leaf variants the derived PartialEq has no
        // cycles to chase; fall through to the existing impl.
        _ => a == b,
    }
}

// ---------- cons / list ----------

fn bi_car(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("car", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Nil),
        Sexp::Cons(a, _) => Ok(a.borrow().clone()),
        other => Err(EvalError::WrongType {
            expected: "listp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_cdr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("cdr", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Nil),
        Sexp::Cons(_, d) => Ok(d.borrow().clone()),
        other => Err(EvalError::WrongType {
            expected: "listp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_cons(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("cons", args, 2, Some(2))?;
    Ok(Sexp::cons(args[0].clone(), args[1].clone()))
}

fn bi_length(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("length", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Int(0)),
        Sexp::Str(s) => Ok(Sexp::Int(s.chars().count() as i64)),
        Sexp::MutStr(rc) => Ok(Sexp::Int(rc.borrow().chars().count() as i64)),
        Sexp::Vector(v) => Ok(Sexp::Int(v.borrow().len() as i64)),
        Sexp::BoolVector(v) => Ok(Sexp::Int(v.borrow().len() as i64)),
        Sexp::Cons(_, _) => {
            let mut n = 0i64;
            let mut cur: Sexp = args[0].clone();
            loop {
                let next = match &cur {
                    Sexp::Nil => return Ok(Sexp::Int(n)),
                    Sexp::Cons(_, d) => {
                        n += 1;
                        d.borrow().clone()
                    }
                    other => {
                        return Err(EvalError::WrongType {
                            expected: "sequence".into(),
                            got: other.clone(),
                        })
                    }
                };
                cur = next;
            }
        }
        other => Err(EvalError::WrongType {
            expected: "sequence".into(),
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
            Sexp::Cons(a, d) => {
                out.push(a.borrow().clone());
                d.borrow().clone()
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

fn bi_predicate(args: &[Sexp], pred: fn(&Sexp) -> bool) -> Result<Sexp, EvalError> {
    require_arity("predicate", args, 1, Some(1))?;
    Ok(if pred(&args[0]) { Sexp::T } else { Sexp::Nil })
}

/// `(type-of OBJECT)' — return a symbol naming the runtime type of
/// OBJECT.  Used by the Rust-min batch 6u predicate elisp ports
/// (consp / listp / symbolp / stringp / numberp / integerp / floatp
/// / vectorp) — each becomes a 1-line `(eq (type-of x) 'TAG)' on
/// top of this primitive.  Tags follow host Emacs conventions where
/// possible: `cons' / `symbol' / `string' / `integer' / `float' /
/// `vector' / `hash-table' / `char-table' / `bool-vector'.  `nil'
/// and `t' both report as `symbol' (= they ARE symbols in Lisp).
fn bi_type_of(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("type-of", args, 1, Some(1))?;
    // Unwrap closure write-through Cells so the user-visible type
    // matches what was captured (= identity of the inner Sexp).
    let mut v: Sexp = args[0].clone();
    while let Sexp::Cell(rc) = v {
        let inner = rc.borrow().clone();
        v = inner;
    }
    let tag = match v {
        Sexp::Cons(_, _) => "cons",
        Sexp::Nil | Sexp::T | Sexp::Symbol(_) => "symbol",
        Sexp::Int(_) => "integer",
        Sexp::Float(_) => "float",
        Sexp::Str(_) | Sexp::MutStr(_) => "string",
        Sexp::Vector(_) => "vector",
        Sexp::HashTable(_) => "hash-table",
        Sexp::CharTable(_) => "char-table",
        Sexp::BoolVector(_) => "bool-vector",
        Sexp::Cell(_) => unreachable!(),
    };
    Ok(Sexp::Symbol(tag.into()))
}

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
            Sexp::Cons(h, t) => {
                let v = h.borrow().clone();
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
                cur = t.borrow().clone();
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

fn bi_intern(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("intern", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Str(s) => Ok(Sexp::Symbol(s.clone())),
        Sexp::MutStr(rc) => Ok(Sexp::Symbol(rc.borrow().clone())),
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

// bi_intern_soft removed — see lisp/nelisp-stdlib-misc.el (Rust-min
// 2026-05-06 batch 6f).  Without an obarray the MVP could not
// implement true soft-fail lookup; the elisp version preserves the
// previous "always returns the symbol" semantics by routing through
// `intern' for stringp input and identity for symbolp input.

/// `(make-symbol NAME)` — return a *fresh* uninterned symbol whose
/// print-name is NAME.  Our Sexp::Symbol is a wrapper around a String,
/// so freshness is achieved by appending a per-process counter to the
/// name (= matching the printable shape of host Emacs's
/// `make-symbol' output for `prin1' purposes; full uninterned-vs-
/// interned distinction is deferred until we have a proper obarray).
fn bi_make_symbol(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    require_arity("make-symbol", args, 1, Some(1))?;
    let name = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(Sexp::Symbol(format!("{}__nelisp-uninterned-{}", name, n)))
}

// bi_gensym removed — see lisp/nelisp-stdlib-misc.el
// (Rust-min 2026-05-06 batch 6a).  `make-symbol' stays here because
// it must construct a fresh `Sexp::Symbol' that bypasses any
// obarray; once that primitive exists, `gensym' is a 4-line elisp
// wrapper.

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

/// `(make-hash-table &rest KEYWORD-ARGS)' — accepts `:test TEST'
/// (default `eql' per host Emacs); other keywords (`:size',
/// `:rehash-size', `:rehash-threshold', `:weakness') are accepted
/// for parity but ignored by the linear-scan storage.
fn bi_make_hash_table(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let mut test = String::from("eql");
    let mut i = 0;
    while i + 1 < args.len() {
        if let Sexp::Symbol(kw) = &args[i] {
            if kw == ":test" {
                test = match &args[i + 1] {
                    Sexp::Symbol(s) => s.clone(),
                    Sexp::Str(s) => s.clone(),
                    Sexp::MutStr(rc) => rc.borrow().clone(),
                    Sexp::Cons(h, _) => match &*h.borrow() {
                        // (quote eq) shape from the reader.
                        Sexp::Symbol(s) if s == "quote" => {
                            // Walk past `quote' to the inner symbol.
                            match &args[i + 1] {
                                Sexp::Cons(_, t) => match &*t.borrow() {
                                    Sexp::Cons(h2, _) => match &*h2.borrow() {
                                        Sexp::Symbol(s) => s.clone(),
                                        _ => "eql".into(),
                                    },
                                    _ => "eql".into(),
                                },
                                _ => "eql".into(),
                            }
                        }
                        _ => "eql".into(),
                    },
                    _ => "eql".into(),
                };
            }
        }
        i += 2;
    }
    Ok(Sexp::hash_table(&test))
}

fn bi_hash_table_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("hash-table-p", args, 1, Some(1))?;
    Ok(if matches!(&args[0], Sexp::HashTable(_)) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_hash_table_count(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("hash-table-count", args, 1, Some(1))?;
    match &args[0] {
        Sexp::HashTable(inner) => {
            Ok(Sexp::Int(inner.borrow().entries.len() as i64))
        }
        other => Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    }
}

/// `(puthash KEY VALUE TABLE)' — set TABLE[KEY] = VALUE.  Returns VALUE.
fn bi_puthash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("puthash", args, 3, Some(3))?;
    let key = args[0].clone();
    let value = args[1].clone();
    let table = match &args[2] {
        Sexp::HashTable(inner) => inner.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    let mut inner = table.borrow_mut();
    let test = inner.test.clone();
    let mut found = false;
    for (k, v) in inner.entries.iter_mut() {
        if hash_test_eq(&test, k, &key) {
            *v = value.clone();
            found = true;
            break;
        }
    }
    if !found {
        inner.entries.push((key, value.clone()));
    }
    Ok(value)
}

/// `(gethash KEY TABLE &optional DEFAULT)' — lookup, returns DEFAULT
/// when missing.
fn bi_gethash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("gethash", args, 2, Some(3))?;
    let key = args[0].clone();
    let default = args.get(2).cloned().unwrap_or(Sexp::Nil);
    let table = match &args[1] {
        Sexp::HashTable(inner) => inner.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    let inner = table.borrow();
    for (k, v) in inner.entries.iter() {
        if hash_test_eq(&inner.test, k, &key) {
            return Ok(v.clone());
        }
    }
    Ok(default)
}

fn bi_remhash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("remhash", args, 2, Some(2))?;
    let key = args[0].clone();
    let table = match &args[1] {
        Sexp::HashTable(inner) => inner.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    let mut inner = table.borrow_mut();
    let test = inner.test.clone();
    let before = inner.entries.len();
    inner.entries.retain(|(k, _)| !hash_test_eq(&test, k, &key));
    Ok(if inner.entries.len() < before { Sexp::T } else { Sexp::Nil })
}

fn bi_clrhash(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("clrhash", args, 1, Some(1))?;
    let table = match &args[0] {
        Sexp::HashTable(inner) => inner.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    table.borrow_mut().entries.clear();
    Ok(args[0].clone())
}

fn bi_maphash(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("maphash", args, 2, Some(2))?;
    let func = resolve_callable(&args[0], env)?;
    let snapshot: Vec<(Sexp, Sexp)> = match &args[1] {
        Sexp::HashTable(inner) => inner.borrow().entries.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    for (k, v) in snapshot {
        super::apply_function(&func, &[k, v], env)?;
    }
    Ok(Sexp::Nil)
}

// bi_hash_table_keys / bi_hash_table_values removed — see
// lisp/nelisp-stdlib-misc.el (Rust-min 2026-05-06 batch 6k).
// Both reduced cleanly to `maphash' folds with `cons'+`nreverse',
// which work because NeLisp's closure-setq write-through (=
// FrameCell pattern from commits eb89f73 / c08d0db / f1fc1f5)
// allows the lambda to mutate the let-bound accumulator.

// char-table / bool-vector user-facing builtins retired (Rust-min
// 2026-05-06 batch 5b).  See file-top commentary; surface migrated
// to elisp.  `Sexp::CharTable' / `Sexp::BoolVector' variants kept
// alive for image-format backward-compat decode only — the helpers
// below let `bi_aref' / `bi_aset' continue to read/write any
// legacy-decoded instances even though no new ones are minted.

fn char_table_set_one(inner: &mut crate::reader::sexp::CharTableInner, c: i64, v: Sexp) {
    for entry in inner.entries.iter_mut() {
        if entry.0 == c {
            entry.1 = v;
            return;
        }
    }
    inner.entries.push((c, v));
}

fn char_table_get(inner: &Rc<RefCell<crate::reader::sexp::CharTableInner>>, c: i64) -> Sexp {
    let borrowed = inner.borrow();
    for (k, v) in borrowed.entries.iter() {
        if *k == c {
            return v.clone();
        }
    }
    if let Some(parent) = &borrowed.parent {
        return char_table_get(parent, c);
    }
    borrowed.default_val.clone()
}

/// Compare two hash-table keys per TEST.  Currently structural for
/// `equal' / `eql'; `eq' falls through to structural for atoms (= host
/// Emacs eq on symbols and small ints is structural anyway), but does
/// not honour pointer identity for cons / vector / hash-table cells
/// (= our Sexp clones share Rc but we have no Rc-aware test here).
fn hash_test_eq(test: &str, a: &Sexp, b: &Sexp) -> bool {
    // For `eq' / `eql' tests we honour cell identity (= Rc::ptr_eq for
    // heap types) via `sexp_eq'.  Structural `==' would recurse on
    // cyclic graphs (e.g. cl-defstruct values with parent <-> children
    // pointers), causing a stack overflow.
    match test {
        "eq" | "eql" => sexp_eq(a, b),
        _ => a == b,
    }
}

/// Pick the test to use when the table-side `test' isn't accessible
/// (= during in-place mutation borrow).  Falls through to structural
/// equality.  Currently a stub kept for symmetry with `hash_test_eq'.
fn inner_test_for(_a: &Sexp, _b: &Sexp) -> String {
    "equal".into()
}

// bi_delete_dups removed — see lisp/nelisp-stdlib-plist-str.el
// (Rust-min 2026-05-06 batch 3).

fn bi_symbol_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("symbol-name", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => Ok(Sexp::Str(s.clone())),
        Sexp::Nil => Ok(Sexp::Str("nil".into())),
        Sexp::T => Ok(Sexp::Str("t".into())),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

// bi_string_eq removed (Rust-min batch 6n / re-cleanup batch 6t).
// Same stash-merge artefact pattern as the dead `bi_split_string'
// scrubbed in batch 6s — the user-facing dispatch was always shadowed
// by the elisp `string-equal' in lisp/nelisp-stdlib-plist-str.el, so
// the dead body was behaviourally invisible.

fn string_value(v: &Sexp) -> Result<String, EvalError> {
    match v {
        Sexp::Str(s) => Ok(s.clone()),
        Sexp::MutStr(rc) => Ok(rc.borrow().clone()),
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

fn bi_expand_file_name(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("expand-file-name", args, 1, Some(2))?;
    let path = string_value(&args[0])?;
    let base = match args.get(1) {
        Some(v) => Some(string_value(v)?),
        None => env_default_directory(env),
    };
    let full = normalize_path(&path, base.as_deref());
    Ok(Sexp::Str(full.to_string_lossy().into_owned()))
}

fn bi_file_truename(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("file-truename", args, 1, Some(1))?;
    let path = string_value(&args[0])?;
    let full = normalize_path(&path, env_default_directory(env).as_deref());
    let resolved = std::fs::canonicalize(&full).unwrap_or(full);
    Ok(Sexp::Str(resolved.to_string_lossy().into_owned()))
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

fn bi_file_exists_p(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("file-exists-p", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    Ok(if p.exists() { Sexp::T } else { Sexp::Nil })
}

fn bi_file_readable_p(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("file-readable-p", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    Ok(if std::fs::metadata(&p).map(|m| m.is_file()).unwrap_or(false) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_file_directory_p(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("file-directory-p", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    Ok(if std::fs::metadata(&p).map(|m| m.is_dir()).unwrap_or(false) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_file_regular_p(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("file-regular-p", args, 1, Some(1))?;
    let p = resolve_existing_path(&args[0], env)?;
    Ok(if std::fs::metadata(&p).map(|m| m.is_file()).unwrap_or(false) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_file_name_extension(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("file-name-extension", args, 1, Some(2))?;
    let s = string_value(&args[0])?;
    // Trim directory prefix.
    let base = s.rsplit('/').next().unwrap_or(&s);
    match base.rsplit_once('.') {
        Some((stem, ext)) if !stem.is_empty() => Ok(Sexp::Str(ext.to_string())),
        _ => Ok(Sexp::Nil),
    }
}

/// `(directory-files DIR &optional FULL MATCH NOSORT COUNT)` — return
/// the list of file names in DIR.  FULL non-nil prepends DIR to each
/// name.  MATCH (regexp) and NOSORT / COUNT are accepted for API
/// compat but only NOSORT is honored (= we always emit sorted unless
/// NOSORT is t; MATCH is simple substring matching for now).
fn bi_directory_files(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("directory-files", args, 1, Some(5))?;
    let dir = resolve_existing_path(&args[0], env)?;
    let full = args.get(1).map(|v| !matches!(v, Sexp::Nil)).unwrap_or(false);
    let match_pat = match args.get(2) {
        Some(Sexp::Str(s)) => Some(s.clone()),
        _ => None,
    };
    let nosort = args.get(3).map(|v| !matches!(v, Sexp::Nil)).unwrap_or(false);
    let mut entries: Vec<String> = match std::fs::read_dir(&dir) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .collect(),
        Err(_) => return Ok(Sexp::Nil),
    };
    if let Some(pat) = match_pat {
        // Crude substring-only match (= sufficient for `\\.el\\'` style
        // suffix tests after we strip backslash escapes).
        let pat = pat.trim_start_matches("\\`").trim_end_matches("\\'").to_string();
        entries.retain(|n| n.contains(&pat));
    }
    if !nosort {
        entries.sort();
    }
    let items: Vec<Sexp> = entries.into_iter().map(|n| {
        if full {
            Sexp::Str(format!("{}/{}", dir.display(), n))
        } else {
            Sexp::Str(n)
        }
    }).collect();
    Ok(Sexp::list_from(&items))
}

/// `(locate-library NAME)' — search `load-path' for a file named NAME
/// (with `.el' / `.elc' suffix).  Returns the absolute path string on
/// hit, or nil.
fn bi_locate_library(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("locate-library", args, 1, Some(4))?;
    let name = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    match locate_load_target(&name, env) {
        Ok(p) => Ok(Sexp::Str(p.to_string_lossy().into_owned())),
        Err(_) => Ok(Sexp::Nil),
    }
}

// bi_file_name_as_directory / bi_directory_file_name removed — see
// lisp/nelisp-stdlib-plist-str.el (Rust-min 2026-05-06).


/// Resolve a path argument against `load-path` if relative, returning
/// the first existing match.  Tries the path as-given, then with `.el`
/// suffix appended (per Elisp `load` SUFFIX search).  Returns the
/// fully expanded path on hit, or `Err` if nothing matched.
fn locate_load_target(name: &str, env: &Env) -> Result<PathBuf, EvalError> {
    let with_suffixes = |base: &Path| -> Option<PathBuf> {
        let p = base.to_path_buf();
        if p.is_file() {
            return Some(p);
        }
        if !name.ends_with(".el") {
            let mut alt = base.to_path_buf();
            alt.set_extension({
                let cur = base.extension().map(|e| e.to_string_lossy().into_owned());
                match cur {
                    Some(ext) if ext.is_empty() => "el".to_string(),
                    Some(_) => format!(
                        "{}.el",
                        base.extension().unwrap().to_string_lossy()
                    ),
                    None => "el".to_string(),
                }
            });
            if alt.is_file() {
                return Some(alt);
            }
            // Append rather than replace extension when name has none.
            let alt2 = base.with_file_name(format!(
                "{}.el",
                base.file_name().map(|f| f.to_string_lossy().into_owned()).unwrap_or_default()
            ));
            if alt2.is_file() {
                return Some(alt2);
            }
        }
        None
    };
    let p = Path::new(name);
    if p.is_absolute() {
        if let Some(hit) = with_suffixes(p) {
            return Ok(hit);
        }
        return Err(EvalError::UserError {
            tag: "file-error".into(),
            data: Sexp::list_from(&[
                Sexp::Str("Cannot open load file".into()),
                Sexp::Str(name.to_string()),
            ]),
        });
    }
    // Relative: try `default-directory` first (mimics Emacs current-buffer behaviour),
    // then walk `load-path`.
    let mut roots: Vec<PathBuf> = Vec::new();
    if let Some(d) = env_default_directory(env) {
        roots.push(PathBuf::from(d));
    }
    if let Ok(load_path) = env.lookup_value("load-path") {
        let mut cur = load_path;
        loop {
            match cur {
                Sexp::Nil => break,
                Sexp::Cons(car, cdr) => {
                    if let Sexp::Str(s) = &*car.borrow() {
                        roots.push(PathBuf::from(s));
                    }
                    cur = cdr.borrow().clone();
                }
                _ => break,
            }
        }
    }
    for root in &roots {
        let candidate = root.join(p);
        if let Some(hit) = with_suffixes(&candidate) {
            return Ok(hit);
        }
    }
    Err(EvalError::UserError {
        tag: "file-error".into(),
        data: Sexp::list_from(&[
            Sexp::Str("Cannot open load file".into()),
            Sexp::Str(name.to_string()),
        ]),
    })
}

fn bi_load(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("load", args, 1, Some(4))?;
    let name = string_value(&args[0])?;
    let noerror = args.get(1).map(is_truthy).unwrap_or(false);
    let resolved = match locate_load_target(&name, env) {
        Ok(p) => p,
        Err(e) => {
            if noerror {
                return Ok(Sexp::Nil);
            }
            return Err(e);
        }
    };
    let source = match std::fs::read_to_string(&resolved) {
        Ok(s) => s,
        Err(io) => {
            if noerror {
                return Ok(Sexp::Nil);
            }
            return Err(EvalError::UserError {
                tag: "file-error".into(),
                data: Sexp::list_from(&[
                    Sexp::Str(format!("read error: {}", io)),
                    Sexp::Str(resolved.to_string_lossy().into_owned()),
                ]),
            });
        }
    };
    let forms = match crate::reader::read_all(&source) {
        Ok(fs) => fs,
        Err(re) => {
            if noerror {
                return Ok(Sexp::Nil);
            }
            return Err(EvalError::Internal(format!(
                "load: read error in {}: {}",
                resolved.display(),
                re
            )));
        }
    };
    // Bind `load-file-name' / `default-directory' for the duration of
    // the load so nested `expand-file-name' / `load' calls resolve
    // siblings of the file currently being loaded.
    let prior_load_file_name = env.lookup_value("load-file-name").ok();
    let prior_default_directory = env.lookup_value("default-directory").ok();
    let parent_dir = resolved
        .parent()
        .map(|p| {
            let mut s = p.to_string_lossy().into_owned();
            if !s.ends_with('/') {
                s.push('/');
            }
            s
        })
        .unwrap_or_else(|| "./".into());
    env.set_value("load-file-name", Sexp::Str(resolved.to_string_lossy().into_owned()))?;
    env.set_value("default-directory", Sexp::Str(parent_dir))?;
    let mut last = Sexp::Nil;
    let mut load_err: Option<EvalError> = None;
    for f in &forms {
        match super::eval(f, env) {
            Ok(v) => last = v,
            Err(e) => {
                load_err = Some(e);
                break;
            }
        }
    }
    // Restore prior bindings whether or not load_err is set.
    match prior_load_file_name {
        Some(v) => {
            env.set_value("load-file-name", v).ok();
        }
        None => {
            env.set_value("load-file-name", Sexp::Nil).ok();
        }
    }
    if let Some(v) = prior_default_directory {
        env.set_value("default-directory", v).ok();
    }
    if let Some(e) = load_err {
        if noerror {
            return Ok(Sexp::Nil);
        }
        return Err(e);
    }
    let _ = last;
    Ok(Sexp::T)
}

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

fn bi_min(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "min".into(), expected: "≥1".into(), got: 0,
        });
    }
    let mut all_int = true;
    for a in args { if matches!(a, Sexp::Float(_)) { all_int = false; } }
    if all_int {
        let vals: Result<Vec<i64>, _> = args.iter().map(|a| match a {
            Sexp::Int(i) => Ok(*i),
            other => Err(EvalError::WrongType { expected: "number".into(), got: other.clone() }),
        }).collect();
        Ok(Sexp::Int(*vals?.iter().min().unwrap()))
    } else {
        let vals: Result<Vec<f64>, _> = args.iter().map(to_f64).collect();
        let v = vals?;
        let m = v.iter().cloned().fold(f64::INFINITY, f64::min);
        Ok(Sexp::Float(m))
    }
}

fn bi_max(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongNumberOfArguments {
            function: "max".into(), expected: "≥1".into(), got: 0,
        });
    }
    let mut all_int = true;
    for a in args { if matches!(a, Sexp::Float(_)) { all_int = false; } }
    if all_int {
        let vals: Result<Vec<i64>, _> = args.iter().map(|a| match a {
            Sexp::Int(i) => Ok(*i),
            other => Err(EvalError::WrongType { expected: "number".into(), got: other.clone() }),
        }).collect();
        Ok(Sexp::Int(*vals?.iter().max().unwrap()))
    } else {
        let vals: Result<Vec<f64>, _> = args.iter().map(to_f64).collect();
        let v = vals?;
        let m = v.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
        Ok(Sexp::Float(m))
    }
}

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

fn bi_abs(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("abs", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(i) => Ok(Sexp::Int(i.abs())),
        Sexp::Float(f) => Ok(Sexp::Float(f.abs())),
        Sexp::Nil => Ok(Sexp::Int(0)),
        other => Err(EvalError::WrongType { expected: "number".into(), got: other.clone() }),
    }
}

fn bi_floor(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("floor", args, 1, Some(2))?;
    let x = to_f64(&args[0])?;
    let div = match args.get(1) { Some(d) => to_f64(d)?, None => 1.0 };
    Ok(Sexp::Int((x / div).floor() as i64))
}

fn bi_ceiling(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("ceiling", args, 1, Some(2))?;
    let x = to_f64(&args[0])?;
    let div = match args.get(1) { Some(d) => to_f64(d)?, None => 1.0 };
    Ok(Sexp::Int((x / div).ceil() as i64))
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

fn bi_round(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("round", args, 1, Some(2))?;
    let x = to_f64(&args[0])?;
    let div = match args.get(1) { Some(d) => to_f64(d)?, None => 1.0 };
    Ok(Sexp::Int((x / div).round() as i64))
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
                // read on a non-blocking fd; treat as no-byte.
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

fn bi_prin1_to_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("prin1-to-string", args, 1, Some(1))?;
    Ok(Sexp::Str(format!("{}", args[0])))
}

/// `(read &optional STREAM)' — parse one elisp form.  STREAM may be
/// a string (= read from it), a marker / buffer / function (= NYI),
/// or absent (= NYI: would read from stdin).  We accept the string
/// case used by callers like `bin/nemacs's `--eval' splice (=
/// `(read "...")').
fn bi_read(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("read", args, 0, Some(1))?;
    match args.get(0) {
        Some(Sexp::Str(s)) => super::super::reader::read_str(s)
            .map_err(|e| EvalError::Internal(format!("read: {}", e))),
        Some(other) => Err(EvalError::NotImplemented(format!(
            "read STREAM type: {:?}",
            other
        ))),
        None => Err(EvalError::NotImplemented(
            "read from stdin (= no STREAM arg) is deferred".into(),
        )),
    }
}

/// `(read-from-string STRING &optional START END)' — return a cons
/// `(FORM . NEW-INDEX)'.  We currently ignore the optional bounds and
/// always return `(FORM . (length STRING))' on success.
fn bi_read_from_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("read-from-string", args, 1, Some(3))?;
    let s = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let form = super::super::reader::read_str(&s)
        .map_err(|e| EvalError::Internal(format!("read-from-string: {}", e)))?;
    Ok(Sexp::cons(form, Sexp::Int(s.len() as i64)))
}

fn bi_provide(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("provide", args, 1, Some(1))?;
    let feature = feature_name_arg("provide", &args[0])?;
    env.provide_feature(&feature);
    // Also mirror the feature symbol onto the `features' dynamic
    // var so introspection from elisp works (= matches host Emacs
    // contract; nemacs-status / package-loaded-p style code reads
    // `features' directly).
    let cur = env.lookup_value("features").unwrap_or(Sexp::Nil);
    let already = {
        let mut found = false;
        let mut node = cur.clone();
        while let Sexp::Cons(h, t) = node {
            let head = h.borrow().clone();
            if let Sexp::Symbol(s) = &head {
                if s == &feature { found = true; break; }
            }
            node = t.borrow().clone();
        }
        found
    };
    if !already {
        let new_features = Sexp::cons(Sexp::Symbol(feature.clone()), cur);
        let _ = env.set_value("features", new_features);
    }
    Ok(Sexp::Symbol(feature))
}

fn bi_require(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("require", args, 1, Some(3))?;
    let feature = feature_name_arg("require", &args[0])?;
    if env.has_feature(&feature) {
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
        env.provide_feature(&feature);
        return Ok(Sexp::Symbol(feature));
    }
    let target = filename.unwrap_or_else(|| feature.clone());
    let load_args = vec![Sexp::Str(target), if noerror { Sexp::T } else { Sexp::Nil }];
    match bi_load(&load_args, env) {
        Ok(_) => {}
        Err(_) if noerror => return Ok(Sexp::Nil),
        Err(e) => return Err(e),
    }
    if !env.has_feature(&feature) {
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

fn bi_featurep(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("featurep", args, 1, Some(1))?;
    let feature = feature_name_arg("featurep", &args[0])?;
    Ok(truthy(env.has_feature(&feature)))
}

#[allow(dead_code)]
fn _unused_truthy(v: &Sexp) -> bool {
    is_truthy(v)
}

// ===== Generic accessors (Phase 8.x core completion) =====================
//
// `aref' / `elt' are language-level operations (Elisp manual §6.6
// "Sequences, Arrays, and Vectors") that anvil-pkg-compat and most
// real-world Elisp packages assume the runtime ships.  Boundary
// policy: language rule -> NeLisp core; Emacs/OS API -> Layer 2.

fn bi_aref(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("aref", args, 2, Some(2))?;
    let index = as_int("aref", &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "aref: negative index {}",
            index
        )));
    }
    match &args[0] {
        Sexp::Str(s) => {
            let chars: Vec<char> = s.chars().collect();
            chars
                .get(index as usize)
                .map(|c| Sexp::Int(*c as i64))
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "aref: index {} out of range for string of length {}",
                        index,
                        chars.len()
                    ))
                })
        }
        Sexp::MutStr(rc) => {
            let s = rc.borrow();
            let chars: Vec<char> = s.chars().collect();
            chars
                .get(index as usize)
                .map(|c| Sexp::Int(*c as i64))
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "aref: index {} out of range for string of length {}",
                        index,
                        chars.len()
                    ))
                })
        }
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            borrowed
                .get(index as usize)
                .cloned()
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "aref: index {} out of range for vector of length {}",
                        index,
                        borrowed.len()
                    ))
                })
        }
        // Char-table indexed by codepoint (= char int).  Out-of-range
        // codepoints return the default rather than erroring (= matches
        // upstream's "lookup never fails" contract).
        Sexp::CharTable(rc) => Ok(char_table_get(rc, index)),
        Sexp::BoolVector(v) => {
            let borrowed = v.borrow();
            borrowed
                .get(index as usize)
                .map(|b| if *b { Sexp::T } else { Sexp::Nil })
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "aref: index {} out of range for bool-vector of length {}",
                        index,
                        borrowed.len()
                    ))
                })
        }
        other => Err(EvalError::WrongType {
            expected: "arrayp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_elt(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("elt", args, 2, Some(2))?;
    let index = as_int("elt", &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "elt: negative index {}",
            index
        )));
    }
    match &args[0] {
        Sexp::Nil => Err(EvalError::ArithError(format!(
            "elt: index {} out of range for empty sequence",
            index
        ))),
        Sexp::Cons(_, _) => {
            let mut cur: Sexp = args[0].clone();
            let mut remaining = index;
            loop {
                let next = match &cur {
                    Sexp::Cons(h, t) => {
                        if remaining == 0 {
                            return Ok(h.borrow().clone());
                        }
                        remaining -= 1;
                        t.borrow().clone()
                    }
                    Sexp::Nil => {
                        return Err(EvalError::ArithError(format!(
                            "elt: index {} out of range for list",
                            index
                        )));
                    }
                    other => {
                        return Err(EvalError::WrongType {
                            expected: "sequencep".into(),
                            got: other.clone(),
                        });
                    }
                };
                cur = next;
            }
        }
        Sexp::Str(_) | Sexp::MutStr(_) | Sexp::Vector(_)
            | Sexp::CharTable(_) | Sexp::BoolVector(_) => bi_aref(args),
        other => Err(EvalError::WrongType {
            expected: "sequencep".into(),
            got: other.clone(),
        }),
    }
}

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

fn bi_setcar(args: &[Sexp]) -> Result<Sexp, EvalError> {
    // (setcar CELL VALUE) — mutate the car of a cons cell in place.
    // Sharing of the cell across bindings is the load-bearing
    // guarantee the Sexp::Cons -> Rc<RefCell<Sexp>> migration buys.
    require_arity("setcar", args, 2, Some(2))?;
    match &args[0] {
        Sexp::Cons(h, _) => {
            *h.borrow_mut() = args[1].clone();
            // Emacs' setcar returns the new value.
            Ok(args[1].clone())
        }
        other => Err(EvalError::WrongType {
            expected: "consp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_setcdr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("setcdr", args, 2, Some(2))?;
    match &args[0] {
        Sexp::Cons(_, t) => {
            *t.borrow_mut() = args[1].clone();
            Ok(args[1].clone())
        }
        other => Err(EvalError::WrongType {
            expected: "consp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_aset(args: &[Sexp]) -> Result<Sexp, EvalError> {
    // (aset ARRAY INDEX VALUE) — mutates ARRAY in place.
    // Phase 8.x supports vectors and `MutStr' (= the make-string
    // result variant).  Plain `Str' literals stay immutable.
    require_arity("aset", args, 3, Some(3))?;
    let index = as_int("aset", &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "aset: negative index {}",
            index
        )));
    }
    match &args[0] {
        Sexp::Vector(v) => {
            let mut borrowed = v.borrow_mut();
            let len = borrowed.len();
            if (index as usize) >= len {
                return Err(EvalError::ArithError(format!(
                    "aset: index {} out of range for vector of length {}",
                    index, len
                )));
            }
            borrowed[index as usize] = args[2].clone();
            // Emacs' `aset' returns the assigned value.
            Ok(args[2].clone())
        }
        Sexp::MutStr(rc) => {
            // Codepoint mutation: replace the char at INDEX with the
            // codepoint VALUE (= integer).  Indexing is by char count
            // (Emacs semantics), not by byte position.  We rebuild the
            // String to keep multi-byte UTF-8 correctness.
            let new_ch = match &args[2] {
                Sexp::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
                    EvalError::WrongType {
                        expected: "valid character codepoint".into(),
                        got: args[2].clone(),
                    }
                })?,
                other => return Err(EvalError::WrongType {
                    expected: "character (integer)".into(),
                    got: other.clone(),
                }),
            };
            let mut s = rc.borrow_mut();
            let chars: Vec<char> = s.chars().collect();
            if (index as usize) >= chars.len() {
                return Err(EvalError::ArithError(format!(
                    "aset: index {} out of range for string of length {}",
                    index,
                    chars.len()
                )));
            }
            let mut new_str = String::with_capacity(s.len());
            for (i, c) in chars.iter().enumerate() {
                if i == index as usize {
                    new_str.push(new_ch);
                } else {
                    new_str.push(*c);
                }
            }
            *s = new_str;
            Ok(args[2].clone())
        }
        Sexp::CharTable(rc) => {
            let mut inner = rc.borrow_mut();
            char_table_set_one(&mut inner, index, args[2].clone());
            Ok(args[2].clone())
        }
        Sexp::BoolVector(rc) => {
            let mut borrowed = rc.borrow_mut();
            let len = borrowed.len();
            if (index as usize) >= len {
                return Err(EvalError::ArithError(format!(
                    "aset: index {} out of range for bool-vector of length {}",
                    index, len
                )));
            }
            borrowed[index as usize] = is_truthy(&args[2]);
            Ok(args[2].clone())
        }
        Sexp::Str(_) => Err(EvalError::WrongType {
            expected: "mutable-array".into(),
            got: args[0].clone(),
        }),
        other => Err(EvalError::WrongType {
            expected: "arrayp".into(),
            got: other.clone(),
        }),
    }
}
