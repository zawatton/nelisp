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
        "+", "-", "*", "/", "mod", "<", ">", "<=", ">=", "=", "/=",
        // equality
        "eq", "equal",
        // cons / list
        "car", "cdr", "cons", "length", "append",
        "setcar", "setcdr",
        // generic sequence / array accessors
        "aref", "aset", "elt", "arrayp", "sequencep",
        "vector", "make-vector",
        // predicates
        "consp", "listp", "atom", "symbolp", "stringp", "numberp", "integerp", "floatp", "functionp",
        // string
        "concat", "format", "substring", "intern", "intern-soft", "symbol-name",
        "string-equal", "string=",
        "string-match-p", "regexp-quote", "mapconcat",
        "make-string", "char-to-string", "string-to-char",
        "string-to-number", "upcase", "downcase", "capitalize",
        "split-string", "string-trim", "string-trim-left", "string-trim-right",
        "string-prefix-p", "string-suffix-p", "string-search",
        // symbols / sequences
        "make-symbol", "gensym", "copy-sequence", "delete-dups",
        // hash-tables (Track O'')
        "make-hash-table", "hash-table-p", "hash-table-count",
        "puthash", "gethash", "remhash", "clrhash", "maphash",
        "hash-table-keys", "hash-table-values",
        // char-table / bool-vector (Track F)
        "make-char-table", "char-table-p", "char-table-subtype",
        "char-table-parent", "set-char-table-parent",
        "set-char-table-range", "char-table-extra-slot",
        "set-char-table-extra-slot",
        "make-bool-vector", "bool-vector-p", "bool-vector",
        // file helpers
        "expand-file-name", "file-truename",
        // file I/O (Doc 47 Stage 8b — multi-file load chain)
        "file-name-directory", "file-name-nondirectory", "file-exists-p",
        "file-readable-p", "load", "locate-library",
        "file-name-as-directory", "directory-file-name", "file-directory-p",
        // symbol / function
        "symbol-value", "symbol-function", "fboundp", "boundp", "funcall", "apply", "eval",
        "defalias", "fset", "fmakunbound", "makunbound",
        "signal", "error", "print", "princ", "prin1-to-string", "message",
        "provide", "require", "featurep",
        // self-process stdio (Phase 9 minimal — needed by stand-alone Lisp servers
        // such as elisp-lsp running on the `nelisp` binary)
        "read-stdin-bytes",
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
        "+" => bi_add(args),
        "-" => bi_sub(args),
        "*" => bi_mul(args),
        "/" => bi_div(args),
        "mod" => bi_mod(args),
        "<" => bi_lt(args),
        ">" => bi_gt(args),
        "<=" => bi_le(args),
        ">=" => bi_ge(args),
        "=" => bi_eq_num(args),
        "/=" => bi_neq_num(args),
        // ---- equality ----
        "eq" => bi_eq(args),
        "equal" => bi_equal(args),
        // ---- cons / list ----
        "car" => bi_car(args),
        "cdr" => bi_cdr(args),
        "cons" => bi_cons(args),
        "length" => bi_length(args),
        "append" => bi_append(args),
        "setcar" => bi_setcar(args),
        "setcdr" => bi_setcdr(args),
        // ---- generic accessors ----
        "aref" => bi_aref(args),
        "aset" => bi_aset(args),
        "elt" => bi_elt(args),
        "arrayp" => bi_predicate(args, |v| matches!(v,
            Sexp::Str(_) | Sexp::MutStr(_) | Sexp::Vector(_)
            | Sexp::CharTable(_) | Sexp::BoolVector(_))),
        "sequencep" => bi_predicate(args, |v| matches!(v,
            Sexp::Nil | Sexp::Cons(_, _) | Sexp::Str(_) | Sexp::MutStr(_)
            | Sexp::Vector(_) | Sexp::CharTable(_) | Sexp::BoolVector(_))),
        "vector" => Ok(Sexp::vector(args.to_vec())),
        "make-vector" => bi_make_vector(args),
        // ---- predicates ----
        "consp" => bi_predicate(args, |v| matches!(v, Sexp::Cons(_, _))),
        "listp" => bi_predicate(args, |v| matches!(v, Sexp::Cons(_, _) | Sexp::Nil)),
        "atom" => bi_predicate(args, |v| !matches!(v, Sexp::Cons(_, _))),
        "symbolp" => bi_predicate(args, |v| matches!(v, Sexp::Symbol(_) | Sexp::Nil | Sexp::T)),
        "stringp" => bi_predicate(args, |v| matches!(v, Sexp::Str(_) | Sexp::MutStr(_))),
        "numberp" => bi_predicate(args, |v| matches!(v, Sexp::Int(_) | Sexp::Float(_))),
        "integerp" => bi_predicate(args, |v| matches!(v, Sexp::Int(_))),
        "floatp" => bi_predicate(args, |v| matches!(v, Sexp::Float(_))),
        "functionp" => bi_predicate(args, |v| matches!(v,
            Sexp::Cons(h, _) if matches!(&*h.borrow(),
                Sexp::Symbol(s) if s == "lambda" || s == "closure" || s == "builtin"))),
        // ---- string ----
        "concat" => bi_concat(args),
        "format" => bi_format(args),
        "substring" => bi_substring(args),
        "intern" => bi_intern(args),
        "symbol-name" => bi_symbol_name(args),
        "string-equal" | "string=" => bi_string_eq(args),
        "string-match-p" => bi_string_match_p(args),
        "regexp-quote" => bi_regexp_quote(args),
        "expand-file-name" => bi_expand_file_name(args, env),
        "file-truename" => bi_file_truename(args, env),
        // Doc 47 Stage 8b — file I/O for multi-file load chains
        "file-name-directory" => bi_file_name_directory(args),
        "file-name-nondirectory" => bi_file_name_nondirectory(args),
        "file-exists-p" => bi_file_exists_p(args, env),
        "file-readable-p" => bi_file_readable_p(args, env),
        "load" => bi_load(args, env),
        "locate-library" => bi_locate_library(args, env),
        "file-name-as-directory" => bi_file_name_as_directory(args),
        "directory-file-name" => bi_directory_file_name(args),
        "file-directory-p" => bi_file_directory_p(args),
        // ---- symbol / function ----
        "symbol-value" => bi_symbol_value(args, env),
        "symbol-function" => bi_symbol_function(args, env),
        "fboundp" => bi_fboundp(args, env),
        "boundp" => bi_boundp(args, env),
        "defalias" => bi_defalias(args, env),
        "fset" => bi_fset(args, env),
        "fmakunbound" => bi_fmakunbound(args, env),
        "makunbound" => bi_makunbound(args, env),
        "intern-soft" => bi_intern_soft(args),
        "make-symbol" => bi_make_symbol(args),
        "gensym" => bi_gensym(args),
        "copy-sequence" => bi_copy_sequence(args),
        "mapconcat" => bi_mapconcat(args, env),
        "delete-dups" => bi_delete_dups(args),
        "make-string" => bi_make_string(args),
        "char-to-string" => bi_char_to_string(args),
        "string-to-char" => bi_string_to_char(args),
        "string-to-number" => bi_string_to_number(args),
        "upcase" => bi_upcase(args),
        "downcase" => bi_downcase(args),
        "capitalize" => bi_capitalize(args),
        "split-string" => bi_split_string(args),
        "string-trim" => bi_string_trim(args),
        "string-trim-left" => bi_string_trim_left(args),
        "string-trim-right" => bi_string_trim_right(args),
        "string-prefix-p" => bi_string_prefix_p(args),
        "string-suffix-p" => bi_string_suffix_p(args),
        "string-search" => bi_string_search(args),
        "make-hash-table" => bi_make_hash_table(args),
        "hash-table-p" => bi_hash_table_p(args),
        "hash-table-count" => bi_hash_table_count(args),
        "puthash" => bi_puthash(args),
        "gethash" => bi_gethash(args),
        "remhash" => bi_remhash(args),
        "clrhash" => bi_clrhash(args),
        "maphash" => bi_maphash(args, env),
        "hash-table-keys" => bi_hash_table_keys(args),
        "hash-table-values" => bi_hash_table_values(args),
        // ---- char-table / bool-vector (Track F) ----
        "make-char-table" => bi_make_char_table(args),
        "char-table-p" => bi_predicate(args, |v| matches!(v, Sexp::CharTable(_))),
        "char-table-subtype" => bi_char_table_subtype(args),
        "char-table-parent" => bi_char_table_parent(args),
        "set-char-table-parent" => bi_set_char_table_parent(args),
        "set-char-table-range" => bi_set_char_table_range(args),
        "char-table-extra-slot" => bi_char_table_extra_slot(args),
        "set-char-table-extra-slot" => bi_set_char_table_extra_slot(args),
        "make-bool-vector" => bi_make_bool_vector(args),
        "bool-vector-p" => bi_predicate(args, |v| matches!(v, Sexp::BoolVector(_))),
        "bool-vector" => bi_bool_vector(args),
        "funcall" => bi_funcall(args, env),
        "apply" => bi_apply(args, env),
        "eval" => bi_eval(args, env),
        "signal" => bi_signal(args),
        "error" => bi_error(args),
        "print" | "princ" => bi_princ(args),
        "prin1-to-string" => bi_prin1_to_string(args),
        "message" => bi_message(args),
        "read-stdin-bytes" => bi_read_stdin_bytes(args),
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
        "provide" => bi_provide(args, env),
        "require" => bi_require(args, env),
        "featurep" => bi_featurep(args, env),
        _ => Err(EvalError::UnboundFunction(name.to_string())),
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

/// Extract the textual content of any string-like Sexp (`Str' or
/// `MutStr').  Used by builtins that accept both immutable string
/// literals and mutable `make-string'-style buffers.  Returns the
/// owned `String' (cheap clone for `Str', borrow+clone for `MutStr').
fn as_string(name: &str, v: &Sexp) -> Result<String, EvalError> {
    v.as_string_owned().ok_or_else(|| EvalError::WrongType {
        expected: format!("stringp ({} arg)", name),
        got: v.clone(),
    })
}

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

fn bi_add(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (af, vs) = numeric_promote(args)?;
    let s = vs.iter().sum();
    Ok(pack_number(af, s))
}

fn bi_sub(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Ok(Sexp::Int(0));
    }
    let (af, vs) = numeric_promote(args)?;
    if vs.len() == 1 {
        return Ok(pack_number(af, -vs[0]));
    }
    let mut acc = vs[0];
    for v in vs.iter().skip(1) {
        acc -= v;
    }
    Ok(pack_number(af, acc))
}

fn bi_mul(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (af, vs) = numeric_promote(args)?;
    let p = vs.iter().product();
    Ok(pack_number(af, p))
}

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

fn bi_mod(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("mod", args, 2, Some(2))?;
    let a = as_int("mod", &args[0])?;
    let b = as_int("mod", &args[1])?;
    if b == 0 {
        return Err(EvalError::ArithError("mod by zero".into()));
    }
    // Elisp `mod` is floor-mod (result has sign of divisor).
    let r = a.rem_euclid(b.abs());
    let signed = if b < 0 { -r } else { r };
    Ok(Sexp::Int(signed))
}

fn cmp_vararg(name: &str, args: &[Sexp], cmp: fn(f64, f64) -> bool) -> Result<Sexp, EvalError> {
    require_arity(name, args, 2, None)?;
    let (_, vs) = numeric_promote(args)?;
    for w in vs.windows(2) {
        if !cmp(w[0], w[1]) {
            return Ok(Sexp::Nil);
        }
    }
    Ok(Sexp::T)
}

fn bi_lt(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg("<", args, |a, b| a < b)
}
fn bi_gt(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg(">", args, |a, b| a > b)
}
fn bi_le(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg("<=", args, |a, b| a <= b)
}
fn bi_ge(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg(">=", args, |a, b| a >= b)
}
fn bi_eq_num(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg("=", args, |a, b| (a - b).abs() < 1e-15)
}
fn bi_neq_num(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("/=", args, 2, Some(2))?;
    let (_, vs) = numeric_promote(args)?;
    Ok(if (vs[0] - vs[1]).abs() < 1e-15 {
        Sexp::Nil
    } else {
        Sexp::T
    })
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
    Ok(if args[0] == args[1] { Sexp::T } else { Sexp::Nil })
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

fn bi_append(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Ok(Sexp::Nil);
    }
    let mut all_but_last: Vec<Sexp> = Vec::new();
    for a in &args[..args.len() - 1] {
        let mut cur: Sexp = a.clone();
        loop {
            let next = match &cur {
                Sexp::Nil => break,
                Sexp::Cons(h, t) => {
                    all_but_last.push(h.borrow().clone());
                    t.borrow().clone()
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
    let mut acc = args.last().unwrap().clone();
    for item in all_but_last.into_iter().rev() {
        acc = Sexp::cons(item, acc);
    }
    Ok(acc)
}

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

// ---------- string ----------

fn bi_concat(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let mut out = String::new();
    for a in args {
        match a {
            Sexp::Str(s) => out.push_str(s),
            Sexp::MutStr(rc) => out.push_str(&rc.borrow()),
            Sexp::Nil => {}
            Sexp::Cons(_, _) => {
                // list of integers (chars) → string
                let chars = list_to_vec(a)?;
                for c in chars {
                    if let Sexp::Int(n) = c {
                        if let Some(ch) = char::from_u32(n as u32) {
                            out.push(ch);
                        }
                    }
                }
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "string or sequence".into(),
                    got: other.clone(),
                })
            }
        }
    }
    Ok(Sexp::Str(out))
}

/// Tiny `format` implementation — enough for the bootstrap (= `%s`,
/// `%d`, `%S`, `%%`).  Doc 44 §3.3 keeps this scope minimal.
#[derive(Default, Debug, Clone)]
struct FormatSpec {
    left_align: bool,
    zero_pad: bool,
    plus: bool,
    space: bool,
    sharp: bool,
    width: Option<usize>,
    precision: Option<usize>,
    conv: char,
}

fn pad_field(body: String, spec: &FormatSpec) -> String {
    let Some(w) = spec.width else { return body };
    let blen = body.chars().count();
    if blen >= w {
        return body;
    }
    let pad_n = w - blen;
    let pad_ch = if spec.zero_pad && !spec.left_align {
        '0'
    } else {
        ' '
    };
    let pad: String = std::iter::repeat(pad_ch).take(pad_n).collect();
    if spec.left_align {
        format!("{}{}", body, pad)
    } else if spec.zero_pad
        && (body.starts_with('-') || body.starts_with('+'))
    {
        // keep sign at the front of the field, pad after it.
        let sign = &body[..1];
        let rest = &body[1..];
        format!("{}{}{}", sign, pad, rest)
    } else {
        format!("{}{}", pad, body)
    }
}

fn fmt_int_with_sign(n: i64, spec: &FormatSpec) -> String {
    if n < 0 {
        format!("-{}", -(n as i128))
    } else if spec.plus {
        format!("+{}", n)
    } else if spec.space {
        format!(" {}", n)
    } else {
        n.to_string()
    }
}

fn fmt_float_default(x: f64, spec: &FormatSpec) -> String {
    let prec = spec.precision.unwrap_or(6);
    let body = match spec.conv {
        'f' | 'F' => format!("{:.*}", prec, x.abs()),
        'e' => format!("{:.*e}", prec, x.abs()),
        'E' => format!("{:.*E}", prec, x.abs()),
        'g' | 'G' => {
            // Use shortest of %e / %f.
            let f = format!("{:.*}", prec, x.abs());
            let e = format!("{:.*e}", prec, x.abs());
            if f.len() <= e.len() {
                f
            } else {
                e
            }
        }
        _ => format!("{}", x.abs()),
    };
    if x.is_sign_negative() {
        format!("-{}", body)
    } else if spec.plus {
        format!("+{}", body)
    } else if spec.space {
        format!(" {}", body)
    } else {
        body
    }
}

fn bi_format(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("format", args, 1, None)?;
    let template = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "stringp".into(),
                got: other.clone(),
            })
        }
    };
    let mut chars = template.chars().peekable();
    let mut out = String::new();
    let mut arg_idx = 1usize;
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        // Parse a format spec: flags, width, .precision, conv
        let mut spec = FormatSpec::default();
        // Flags loop.
        while let Some(&pc) = chars.peek() {
            match pc {
                '-' => spec.left_align = true,
                '0' => spec.zero_pad = true,
                '+' => spec.plus = true,
                ' ' => spec.space = true,
                '#' => spec.sharp = true,
                _ => break,
            }
            chars.next();
        }
        // Width.
        let mut width = String::new();
        while let Some(&pc) = chars.peek() {
            if pc.is_ascii_digit() {
                width.push(pc);
                chars.next();
            } else {
                break;
            }
        }
        if !width.is_empty() {
            spec.width = Some(width.parse().unwrap_or(0));
        }
        // Precision.
        if matches!(chars.peek(), Some(&'.')) {
            chars.next();
            let mut prec = String::new();
            while let Some(&pc) = chars.peek() {
                if pc.is_ascii_digit() {
                    prec.push(pc);
                    chars.next();
                } else {
                    break;
                }
            }
            spec.precision = Some(prec.parse().unwrap_or(0));
        }
        // Conversion char.
        let conv = match chars.next() {
            Some(c) => c,
            None => {
                out.push('%');
                continue;
            }
        };
        spec.conv = conv;
        match conv {
            '%' => out.push('%'),
            's' => {
                let body = match args.get(arg_idx) {
                    Some(Sexp::Str(s)) => s.clone(),
                    Some(other) => format!("{}", other),
                    None => String::new(),
                };
                arg_idx += 1;
                let body = if let Some(p) = spec.precision {
                    body.chars().take(p).collect()
                } else {
                    body
                };
                out.push_str(&pad_field(body, &spec));
            }
            'S' => {
                let body = match args.get(arg_idx) {
                    Some(arg) => format!("{}", arg),
                    None => String::new(),
                };
                arg_idx += 1;
                out.push_str(&pad_field(body, &spec));
            }
            'd' | 'i' => {
                let n = match args.get(arg_idx) {
                    Some(Sexp::Int(n)) => *n,
                    Some(Sexp::Float(x)) => *x as i64,
                    Some(other) => {
                        return Err(EvalError::WrongType {
                            expected: "integerp".into(),
                            got: other.clone(),
                        })
                    }
                    None => 0,
                };
                arg_idx += 1;
                let body = fmt_int_with_sign(n, &spec);
                out.push_str(&pad_field(body, &spec));
            }
            'x' | 'X' | 'o' | 'b' => {
                let n = match args.get(arg_idx) {
                    Some(Sexp::Int(n)) => *n,
                    Some(Sexp::Float(x)) => *x as i64,
                    Some(other) => {
                        return Err(EvalError::WrongType {
                            expected: "integerp".into(),
                            got: other.clone(),
                        })
                    }
                    None => 0,
                };
                arg_idx += 1;
                // Negative numbers in Emacs %x render as a hex of the
                // mathematical value (not two's-complement).  We follow
                // that convention.
                let abs_part = match conv {
                    'x' => format!("{:x}", n.unsigned_abs()),
                    'X' => format!("{:X}", n.unsigned_abs()),
                    'o' => format!("{:o}", n.unsigned_abs()),
                    'b' => format!("{:b}", n.unsigned_abs()),
                    _ => unreachable!(),
                };
                let prefix = if spec.sharp {
                    match conv {
                        'x' => "0x",
                        'X' => "0X",
                        'o' => "0o",
                        'b' => "0b",
                        _ => "",
                    }
                } else {
                    ""
                };
                let body = if n < 0 {
                    format!("-{}{}", prefix, abs_part)
                } else {
                    format!("{}{}", prefix, abs_part)
                };
                out.push_str(&pad_field(body, &spec));
            }
            'c' => {
                let n = match args.get(arg_idx) {
                    Some(Sexp::Int(n)) => *n,
                    Some(other) => {
                        return Err(EvalError::WrongType {
                            expected: "characterp".into(),
                            got: other.clone(),
                        })
                    }
                    None => 0,
                };
                arg_idx += 1;
                let ch = char::from_u32(n as u32).unwrap_or('?');
                out.push_str(&pad_field(ch.to_string(), &spec));
            }
            'f' | 'F' | 'e' | 'E' | 'g' | 'G' => {
                let x = match args.get(arg_idx) {
                    Some(Sexp::Float(x)) => *x,
                    Some(Sexp::Int(n)) => *n as f64,
                    Some(other) => {
                        return Err(EvalError::WrongType {
                            expected: "numberp".into(),
                            got: other.clone(),
                        })
                    }
                    None => 0.0,
                };
                arg_idx += 1;
                let body = fmt_float_default(x, &spec);
                out.push_str(&pad_field(body, &spec));
            }
            other => {
                return Err(EvalError::Internal(format!(
                    "format: unsupported conversion %{}",
                    other
                )))
            }
        }
    }
    Ok(Sexp::Str(out))
}

fn bi_substring(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("substring", args, 2, Some(3))?;
    let s = as_string("substring", &args[0])?;
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len() as i64;
    let from = normalise_index(as_int("substring", &args[1])?, len);
    let to = if args.len() == 3 && !matches!(args[2], Sexp::Nil) {
        normalise_index(as_int("substring", &args[2])?, len)
    } else {
        len
    };
    if from < 0 || to < from || to > len {
        return Err(EvalError::ArithError("substring out of range".into()));
    }
    let slice: String = chars[from as usize..to as usize].iter().collect();
    Ok(Sexp::Str(slice))
}

fn normalise_index(i: i64, len: i64) -> i64 {
    if i < 0 {
        len + i
    } else {
        i
    }
}

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

fn bi_intern_soft(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("intern-soft", args, 1, Some(2))?;
    let name = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp / symbolp".into(),
            got: other.clone(),
        }),
    };
    Ok(Sexp::Symbol(name))
}

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

/// `(gensym &optional PREFIX)` — host Emacs returns a fresh
/// uninterned symbol with a numeric suffix.  We delegate to
/// `make-symbol' with a derived name.
fn bi_gensym(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("gensym", args, 0, Some(1))?;
    let prefix = match args.get(0) {
        Some(Sexp::Str(s)) => s.clone(),
        Some(Sexp::Symbol(s)) => s.clone(),
        _ => "g".to_string(),
    };
    bi_make_symbol(&[Sexp::Str(prefix)])
}

/// `(copy-sequence SEQUENCE)` — return a shallow copy.
fn bi_copy_sequence(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("copy-sequence", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Nil),
        Sexp::Cons(_, _) => {
            let elts = super::list_elements(&args[0])?;
            let mut out = Sexp::Nil;
            for e in elts.into_iter().rev() {
                out = Sexp::cons(e, out);
            }
            Ok(out)
        }
        Sexp::Str(s) => Ok(Sexp::Str(s.clone())),
        Sexp::MutStr(rc) => Ok(Sexp::mut_str(rc.borrow().clone())),
        other => Ok(other.clone()),
    }
}

/// `(mapconcat FUNCTION SEQUENCE &optional SEPARATOR)' — apply
/// FUNCTION to each element of SEQUENCE, concatenate the results
/// joined by SEPARATOR (= empty string default).
fn bi_mapconcat(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("mapconcat", args, 2, Some(3))?;
    let func = resolve_callable(&args[0], env)?;
    let sep: String = match args.get(2) {
        Some(Sexp::Str(s)) => s.clone(),
        Some(Sexp::Nil) | None => String::new(),
        Some(other) => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let elts = super::list_elements(&args[1])?;
    let mut parts: Vec<String> = Vec::new();
    for elt in elts {
        let r = super::apply_function(&func, &[elt], env)?;
        if let Sexp::Str(s) = r {
            parts.push(s);
        } else {
            return Err(EvalError::WrongType {
                expected: "function returning string".into(),
                got: r,
            });
        }
    }
    Ok(Sexp::Str(parts.join(&sep)))
}

/// `(delete-dups LIST)' — return LIST with duplicates removed
/// (`equal' test).  Modifies LIST destructively in host Emacs; here
/// we return a fresh list since our Sexp lacks shared mutability.
fn bi_make_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("make-string", args, 2, Some(3))?;
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
    // make-string is the canonical mutable-string constructor — return
    // MutStr so callers can `aset' into it (= gap-buffer fill, etc.).
    Ok(Sexp::mut_str(c.to_string().repeat(n)))
}

fn bi_char_to_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("char-to-string", args, 1, Some(1))?;
    let c = match &args[0] {
        Sexp::Int(c) => char::from_u32(*c as u32).ok_or_else(|| EvalError::WrongType {
            expected: "valid character codepoint".into(),
            got: args[0].clone(),
        })?,
        other => return Err(EvalError::WrongType {
            expected: "character (integer)".into(),
            got: other.clone(),
        }),
    };
    Ok(Sexp::Str(c.to_string()))
}

fn bi_string_to_char(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-to-char", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Str(s) => match s.chars().next() {
            Some(c) => Ok(Sexp::Int(c as i64)),
            None => Ok(Sexp::Int(0)),
        },
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_string_to_number(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-to-number", args, 1, Some(2))?;
    let s = match &args[0] {
        Sexp::Str(s) => s.trim_start().to_string(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let radix = match args.get(1) {
        Some(Sexp::Int(r)) => *r as u32,
        Some(Sexp::Nil) | None => 10,
        Some(other) => return Err(EvalError::WrongType {
            expected: "integer radix".into(),
            got: other.clone(),
        }),
    };
    if radix == 10 {
        if let Ok(i) = s.parse::<i64>() {
            return Ok(Sexp::Int(i));
        }
        if let Ok(f) = s.parse::<f64>() {
            return Ok(Sexp::Float(f));
        }
        Ok(Sexp::Int(0))
    } else {
        match i64::from_str_radix(&s, radix) {
            Ok(n) => Ok(Sexp::Int(n)),
            Err(_) => Ok(Sexp::Int(0)),
        }
    }
}

fn bi_upcase(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("upcase", args, 1, Some(1))?;
    if let Some(s) = args[0].as_string_owned() {
        return Ok(Sexp::Str(s.to_uppercase()));
    }
    match &args[0] {
        Sexp::Int(c) => Ok(Sexp::Int(
            char::from_u32(*c as u32)
                .map(|ch| ch.to_uppercase().next().unwrap_or(ch))
                .map(|ch| ch as i64)
                .unwrap_or(*c),
        )),
        other => Err(EvalError::WrongType {
            expected: "stringp / characterp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_downcase(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("downcase", args, 1, Some(1))?;
    if let Some(s) = args[0].as_string_owned() {
        return Ok(Sexp::Str(s.to_lowercase()));
    }
    match &args[0] {
        Sexp::Int(c) => Ok(Sexp::Int(
            char::from_u32(*c as u32)
                .map(|ch| ch.to_lowercase().next().unwrap_or(ch))
                .map(|ch| ch as i64)
                .unwrap_or(*c),
        )),
        other => Err(EvalError::WrongType {
            expected: "stringp / characterp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_capitalize(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("capitalize", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Str(s) => {
            let mut out = String::new();
            let mut at_word_start = true;
            for c in s.chars() {
                if c.is_alphabetic() {
                    if at_word_start {
                        out.extend(c.to_uppercase());
                        at_word_start = false;
                    } else {
                        out.extend(c.to_lowercase());
                    }
                } else {
                    out.push(c);
                    at_word_start = true;
                }
            }
            Ok(Sexp::Str(out))
        }
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_split_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("split-string", args, 1, Some(4))?;
    let s = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    // SEPARATORS arg: regexp or default whitespace.  We accept a
    // literal-string regex (no backslash specials).  Falls back to
    // whitespace when nil/missing.
    let parts: Vec<String> = match args.get(1) {
        Some(Sexp::Str(sep)) if !sep.is_empty() => {
            s.split(sep.as_str()).map(|p| p.to_string()).collect()
        }
        _ => s.split_whitespace().map(|p| p.to_string()).collect(),
    };
    let mut out = Sexp::Nil;
    for part in parts.into_iter().rev() {
        out = Sexp::cons(Sexp::Str(part), out);
    }
    Ok(out)
}

fn bi_string_trim(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-trim", args, 1, Some(3))?;
    match &args[0] {
        Sexp::Str(s) => Ok(Sexp::Str(s.trim().to_string())),
        Sexp::MutStr(rc) => Ok(Sexp::Str(rc.borrow().trim().to_string())),
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_string_trim_left(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-trim-left", args, 1, Some(2))?;
    match &args[0] {
        Sexp::Str(s) => Ok(Sexp::Str(s.trim_start().to_string())),
        Sexp::MutStr(rc) => Ok(Sexp::Str(rc.borrow().trim_start().to_string())),
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_string_trim_right(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-trim-right", args, 1, Some(2))?;
    match &args[0] {
        Sexp::Str(s) => Ok(Sexp::Str(s.trim_end().to_string())),
        Sexp::MutStr(rc) => Ok(Sexp::Str(rc.borrow().trim_end().to_string())),
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_string_prefix_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-prefix-p", args, 2, Some(3))?;
    let prefix = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let s = match &args[1] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    Ok(if s.starts_with(&prefix) { Sexp::T } else { Sexp::Nil })
}

fn bi_string_suffix_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-suffix-p", args, 2, Some(3))?;
    let suffix = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let s = match &args[1] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    Ok(if s.ends_with(&suffix) { Sexp::T } else { Sexp::Nil })
}

fn bi_string_search(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-search", args, 2, Some(3))?;
    let needle = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let haystack = match &args[1] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    let from = match args.get(2) {
        Some(Sexp::Int(n)) if *n >= 0 => *n as usize,
        Some(Sexp::Nil) | None => 0,
        Some(other) => return Err(EvalError::WrongType {
            expected: "non-negative integer".into(),
            got: other.clone(),
        }),
    };
    if from > haystack.len() {
        return Ok(Sexp::Nil);
    }
    match haystack[from..].find(&needle) {
        Some(i) => Ok(Sexp::Int((from + i) as i64)),
        None => Ok(Sexp::Nil),
    }
}

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
    let mut found = false;
    for (k, v) in inner.entries.iter_mut() {
        if hash_test_eq(&inner_test_for(k, &key), k, &key) {
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

fn bi_hash_table_keys(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("hash-table-keys", args, 1, Some(1))?;
    let inner = match &args[0] {
        Sexp::HashTable(inner) => inner.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    let mut out = Sexp::Nil;
    for (k, _) in inner.borrow().entries.iter().rev() {
        out = Sexp::cons(k.clone(), out);
    }
    Ok(out)
}

fn bi_hash_table_values(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("hash-table-values", args, 1, Some(1))?;
    let inner = match &args[0] {
        Sexp::HashTable(inner) => inner.clone(),
        other => return Err(EvalError::WrongType {
            expected: "hash-table-p".into(),
            got: other.clone(),
        }),
    };
    let mut out = Sexp::Nil;
    for (_, v) in inner.borrow().entries.iter().rev() {
        out = Sexp::cons(v.clone(), out);
    }
    Ok(out)
}

// ---------- char-table / bool-vector (Track F) -----------------------

fn bi_make_char_table(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("make-char-table", args, 1, Some(2))?;
    let subtype = args[0].clone();
    let init = args.get(1).cloned().unwrap_or(Sexp::Nil);
    Ok(Sexp::char_table(subtype, init))
}

fn bi_char_table_subtype(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("char-table-subtype", args, 1, Some(1))?;
    match &args[0] {
        Sexp::CharTable(rc) => Ok(rc.borrow().subtype.clone()),
        other => Err(EvalError::WrongType {
            expected: "char-table-p".into(),
            got: other.clone(),
        }),
    }
}

fn bi_char_table_parent(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("char-table-parent", args, 1, Some(1))?;
    match &args[0] {
        Sexp::CharTable(rc) => Ok(match &rc.borrow().parent {
            Some(p) => Sexp::CharTable(p.clone()),
            None => Sexp::Nil,
        }),
        other => Err(EvalError::WrongType {
            expected: "char-table-p".into(),
            got: other.clone(),
        }),
    }
}

fn bi_set_char_table_parent(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("set-char-table-parent", args, 2, Some(2))?;
    let parent_rc = match &args[1] {
        Sexp::Nil => None,
        Sexp::CharTable(rc) => Some(rc.clone()),
        other => return Err(EvalError::WrongType {
            expected: "char-table-p / nil".into(),
            got: other.clone(),
        }),
    };
    match &args[0] {
        Sexp::CharTable(rc) => {
            rc.borrow_mut().parent = parent_rc;
            Ok(args[1].clone())
        }
        other => Err(EvalError::WrongType {
            expected: "char-table-p".into(),
            got: other.clone(),
        }),
    }
}

fn bi_set_char_table_range(args: &[Sexp]) -> Result<Sexp, EvalError> {
    // (set-char-table-range TABLE RANGE VALUE)
    //   RANGE: integer (single char), (FROM . TO) cons (inclusive),
    //          or t (apply to default-val).
    require_arity("set-char-table-range", args, 3, Some(3))?;
    let val = args[2].clone();
    let rc = match &args[0] {
        Sexp::CharTable(rc) => rc.clone(),
        other => return Err(EvalError::WrongType {
            expected: "char-table-p".into(),
            got: other.clone(),
        }),
    };
    match &args[1] {
        Sexp::T => {
            rc.borrow_mut().default_val = val;
        }
        Sexp::Int(c) => {
            let mut inner = rc.borrow_mut();
            char_table_set_one(&mut inner, *c, val);
        }
        Sexp::Cons(car_rc, cdr_rc) => {
            let from = match &*car_rc.borrow() {
                Sexp::Int(n) => *n,
                other => return Err(EvalError::WrongType {
                    expected: "integer (range from)".into(),
                    got: other.clone(),
                }),
            };
            let to = match &*cdr_rc.borrow() {
                Sexp::Int(n) => *n,
                other => return Err(EvalError::WrongType {
                    expected: "integer (range to)".into(),
                    got: other.clone(),
                }),
            };
            // Cap range size to avoid runaway allocation; substrate
            // syntax-tables operate on ASCII / latin-1 ranges.
            const MAX_RANGE: i64 = 4096;
            if to - from > MAX_RANGE {
                return Err(EvalError::Internal(format!(
                    "set-char-table-range: range too large ({} chars)",
                    to - from + 1
                )));
            }
            let mut inner = rc.borrow_mut();
            for c in from..=to {
                char_table_set_one(&mut inner, c, val.clone());
            }
        }
        other => return Err(EvalError::WrongType {
            expected: "integer / (FROM . TO) cons / t".into(),
            got: other.clone(),
        }),
    }
    Ok(args[2].clone())
}

fn char_table_set_one(inner: &mut CharTableInner, c: i64, v: Sexp) {
    for entry in inner.entries.iter_mut() {
        if entry.0 == c {
            entry.1 = v;
            return;
        }
    }
    inner.entries.push((c, v));
}

/// Look up CHAR in a char-table, walking the parent chain on miss.
/// Returns the default value (which may itself fall through to the
/// parent's default) if no slot matches.
fn char_table_get(inner: &Rc<RefCell<CharTableInner>>, c: i64) -> Sexp {
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

fn bi_char_table_extra_slot(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("char-table-extra-slot", args, 2, Some(2))?;
    let slot = as_int("char-table-extra-slot", &args[1])? as usize;
    match &args[0] {
        Sexp::CharTable(rc) => {
            let inner = rc.borrow();
            Ok(inner.extra.get(slot).cloned().unwrap_or(Sexp::Nil))
        }
        other => Err(EvalError::WrongType {
            expected: "char-table-p".into(),
            got: other.clone(),
        }),
    }
}

fn bi_set_char_table_extra_slot(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("set-char-table-extra-slot", args, 3, Some(3))?;
    let slot = as_int("set-char-table-extra-slot", &args[1])? as usize;
    match &args[0] {
        Sexp::CharTable(rc) => {
            let mut inner = rc.borrow_mut();
            while inner.extra.len() <= slot {
                inner.extra.push(Sexp::Nil);
            }
            inner.extra[slot] = args[2].clone();
            Ok(args[2].clone())
        }
        other => Err(EvalError::WrongType {
            expected: "char-table-p".into(),
            got: other.clone(),
        }),
    }
}

fn bi_make_bool_vector(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("make-bool-vector", args, 2, Some(2))?;
    let len = as_int("make-bool-vector", &args[0])?;
    if len < 0 {
        return Err(EvalError::ArithError(format!(
            "make-bool-vector: negative length {}",
            len
        )));
    }
    let init = is_truthy(&args[1]);
    Ok(Sexp::bool_vector(len as usize, init))
}

fn bi_bool_vector(args: &[Sexp]) -> Result<Sexp, EvalError> {
    // (bool-vector &rest ARGS) — variadic constructor.
    let bits: Vec<bool> = args.iter().map(is_truthy).collect();
    Ok(Sexp::BoolVector(Rc::new(RefCell::new(bits))))
}

/// Compare two hash-table keys per TEST.  Currently structural for
/// `equal' / `eql'; `eq' falls through to structural for atoms (= host
/// Emacs eq on symbols and small ints is structural anyway), but does
/// not honour pointer identity for cons / vector / hash-table cells
/// (= our Sexp clones share Rc but we have no Rc-aware test here).
fn hash_test_eq(test: &str, a: &Sexp, b: &Sexp) -> bool {
    let _ = test; // accepted for API parity, treated uniformly for now.
    a == b
}

/// Pick the test to use when the table-side `test' isn't accessible
/// (= during in-place mutation borrow).  Falls through to structural
/// equality.  Currently a stub kept for symmetry with `hash_test_eq'.
fn inner_test_for(_a: &Sexp, _b: &Sexp) -> String {
    "equal".into()
}

fn bi_delete_dups(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("delete-dups", args, 1, Some(1))?;
    let elts = super::list_elements(&args[0])?;
    let mut seen: Vec<Sexp> = Vec::new();
    for elt in elts {
        if !seen.iter().any(|s| sexp_eq(s, &elt) || s == &elt) {
            seen.push(elt);
        }
    }
    let mut out = Sexp::Nil;
    for e in seen.into_iter().rev() {
        out = Sexp::cons(e, out);
    }
    Ok(out)
}

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

fn bi_string_eq(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-equal", args, 2, Some(2))?;
    let a = string_value(&args[0])?;
    let b = string_value(&args[1])?;
    Ok(truthy(a == b))
}

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

fn bi_regexp_quote(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("regexp-quote", args, 1, Some(1))?;
    let src = string_value(&args[0])?;
    let mut out = String::with_capacity(src.len());
    for ch in src.chars() {
        if matches!(ch, '.' | '*' | '+' | '?' | '[' | ']' | '^' | '$' | '\\' | '(' | ')' | '{' | '}' | '|') {
            out.push('\\');
        }
        out.push(ch);
    }
    Ok(Sexp::Str(out))
}

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

fn bi_file_name_directory(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("file-name-directory", args, 1, Some(1))?;
    let path = string_value(&args[0])?;
    match path.rfind('/') {
        Some(idx) => Ok(Sexp::Str(path[..=idx].to_string())),
        None => Ok(Sexp::Nil),
    }
}

fn bi_file_name_nondirectory(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("file-name-nondirectory", args, 1, Some(1))?;
    let path = string_value(&args[0])?;
    match path.rfind('/') {
        Some(idx) => Ok(Sexp::Str(path[idx + 1..].to_string())),
        None => Ok(Sexp::Str(path)),
    }
}

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

/// `(file-directory-p PATH)' — t when PATH is an existing directory.
fn bi_file_directory_p(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("file-directory-p", args, 1, Some(1))?;
    let p = match &args[0] {
        Sexp::Str(s) => PathBuf::from(s),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    Ok(if p.is_dir() { Sexp::T } else { Sexp::Nil })
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

/// `(file-name-as-directory PATH)' — append `/' if not already there.
fn bi_file_name_as_directory(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("file-name-as-directory", args, 1, Some(1))?;
    let s = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    Ok(Sexp::Str(if s.ends_with('/') { s } else { format!("{}/", s) }))
}

/// `(directory-file-name PATH)' — strip trailing `/' if any (but
/// preserve the root `/').
fn bi_directory_file_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("directory-file-name", args, 1, Some(1))?;
    let s = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => return Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    };
    if s.len() > 1 && s.ends_with('/') {
        Ok(Sexp::Str(s.trim_end_matches('/').to_string()))
    } else {
        Ok(Sexp::Str(s))
    }
}

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

fn bi_error(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let msg = if args.is_empty() {
        String::new()
    } else if let Sexp::Str(s) = &args[0] {
        // Substitute %s / %d as `format` would.
        let sub_args: Vec<Sexp> = std::iter::once(Sexp::Str(s.clone()))
            .chain(args.iter().skip(1).cloned())
            .collect();
        match bi_format(&sub_args)? {
            Sexp::Str(s) => s,
            _ => s.clone(),
        }
    } else {
        format!("{}", args[0])
    };
    Err(EvalError::UserError {
        tag: "error".into(),
        data: Sexp::list_from(&[Sexp::Str(msg)]),
    })
}

fn bi_princ(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    if args.is_empty() {
        return Ok(Sexp::Nil);
    }
    // Emacs `princ' writes string contents *without* quoting/escaping;
    // non-strings render through their normal Display so `(princ 42)'
    // emits `42' just like `(format "%s" 42)' would.
    let bytes: Vec<u8> = match &args[0] {
        Sexp::Str(s) => s.as_bytes().to_vec(),
        other => format!("{}", other).into_bytes(),
    };
    let mut out = std::io::stdout().lock();
    out.write_all(&bytes)
        .and_then(|_| out.flush())
        .map_err(|e| EvalError::Internal(format!("princ: {}", e)))?;
    Ok(args[0].clone())
}

/// `(message FORMAT-STRING &rest ARGS)' — host Emacs treats the
/// first arg as a format string and substitutes via `format'.  We
/// route through `bi_format' so `%s' / `%d' / `%S' / `%%' all work
/// with the trailing args, then write the result to *stderr* (=
/// host Emacs writes messages to the echo area; in batch mode that
/// surfaces on stderr, which we mirror here so `princ' on stdout
/// stays cleanly user-payload-only).
fn bi_message(args: &[Sexp]) -> Result<Sexp, EvalError> {
    use std::io::Write;
    if args.is_empty() {
        return Ok(Sexp::Nil);
    }
    // `(message nil ...)' clears the echo area in Emacs.  Mirror by
    // returning nil without writing.
    if matches!(&args[0], Sexp::Nil) {
        return Ok(Sexp::Nil);
    }
    let formatted = bi_format(args)?;
    let s = match &formatted {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.borrow().clone(),
        other => format!("{}", other),
    };
    let mut err = std::io::stderr().lock();
    let _ = writeln!(err, "{}", s);
    let _ = err.flush();
    Ok(formatted)
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
        let fd = std::io::stdin().lock().as_raw_fd();
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
        if pfd.revents & libc::POLLIN == 0 {
            return Ok(None);
        }
        // Data is available — read exactly 1 byte.
        use std::io::Read;
        let mut buf = [0u8; 1];
        match std::io::stdin().lock().read(&mut buf) {
            Ok(0) => Ok(None), // EOF
            Ok(_) => Ok(Some(buf[0])),
            Err(e) => Err(EvalError::Internal(format!(
                "read-stdin-byte-available: read failed: {}",
                e
            ))),
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
