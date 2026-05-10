//! Rust helpers for elisp JIT-strategy wrappers in
//! `lisp/nelisp-jit-strategy.el'.  Doc 84 §84.1 (2026-05-10) ported
//! the 8 Float-family `bi_*_float' fns to the new `nl-jit-call-float-
//! {float,cmp}' bridges (see `jit/float.rs').  Doc 84 §84.2 (2026-05-10)
//! ported `bi_syscall_nr_resolve' to the auto-generated
//! `lisp/nelisp-syscall-table.el' (= `build-tool/build.rs' codegen
//! from `libc::SYS_*'); the `syscall_nr' helper in `eval/builtins.rs'
//! is the source-of-truth pair (= still consumed by `bi_syscall').
//! Residual: Doc 80.4 slim length/aref/aset primitives (84.3 will port).

use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// ---------- Doc 80 slim primitives (length / aref / aset fall-through) -------
//
// Doc 80 §7 v2 fold-in (2026-05-09): 6 slim primitives expand through
// the `slim!' macro factoring `require_arity' + arg-0 variant match
// + `WrongType' else-arm.  Indexed variants (= aref / aset / codepoint-at)
// extract `idx = as_int(args[1])' inside the body explicitly.
macro_rules! slim {
    ($args:expr, $name:expr, $n:literal, $exp:literal, $pat:pat => $body:expr) => {{
        crate::eval::builtins::require_arity($name, $args, $n, Some($n))?;
        match &$args[0] { $pat => $body, o => return Err(
            EvalError::WrongType { expected: $exp.into(), got: o.clone() }) }
    }};
}

/// `(nelisp--mut-str-len S)' / `(nelisp--bool-vector-len V)' — single-
/// variant length probes (= `length' fall-through for MutStr / BoolVector).
pub(crate) fn bi_mut_str_len(args: &[Sexp]) -> Result<Sexp, EvalError> {
    slim!(args, "nelisp--mut-str-len", 1, "mut-string",
          Sexp::MutStr(rc) => Ok(Sexp::Int(rc.value.chars().count() as i64)))
}
pub(crate) fn bi_bool_vector_len(args: &[Sexp]) -> Result<Sexp, EvalError> {
    slim!(args, "nelisp--bool-vector-len", 1, "bool-vector",
          Sexp::BoolVector(v) => Ok(Sexp::Int(v.value.len() as i64)))
}

/// `(nelisp--str-codepoint-at S IDX)' — char-indexed codepoint read
/// for Str + MutStr; `arith-error' on out-of-range.
pub(crate) fn bi_str_codepoint_at(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let n = "nelisp--str-codepoint-at";
    crate::eval::builtins::require_arity(n, args, 2, Some(2))?;
    let idx = crate::eval::builtins::as_int(n, &args[1])?;
    let s: &str = match &args[0] {
        Sexp::Str(s) => s, Sexp::MutStr(rc) => &rc.value,
        o => return Err(EvalError::WrongType { expected: "string".into(), got: o.clone() }),
    };
    if idx < 0 { return Err(EvalError::ArithError(format!("negative index {}", idx))); }
    s.chars().nth(idx as usize).map(|c| Sexp::Int(c as i64))
        .ok_or_else(|| EvalError::ArithError(format!("index {} out of range", idx)))
}

/// `(nelisp--mut-str-set-codepoint S IDX CP)' — in-place MutStr codepoint
/// mutation; returns CP per `aset' contract.
pub(crate) fn bi_mut_str_set_codepoint(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let n = "nelisp--mut-str-set-codepoint";
    slim!(args, n, 3, "mut-string", Sexp::MutStr(rc) => {
        let idx = crate::eval::builtins::as_int(n, &args[1])?;
        let cp = crate::eval::builtins::as_int(n, &args[2])?;
        let new_ch = char::from_u32(cp as u32).ok_or_else(|| EvalError::WrongType {
            expected: "valid character codepoint".into(), got: args[2].clone() })?;
        let chars: Vec<char> = rc.value.chars().collect();
        if idx < 0 || (idx as usize) >= chars.len() {
            return Err(EvalError::ArithError(format!("index {} out of range", idx)));
        }
        let new_str: String = chars.iter().enumerate()
            .map(|(i, c)| if i == idx as usize { new_ch } else { *c }).collect();
        // SAFETY: Phase A.4.2 — locals don't alias the box.
        unsafe { rc.set_value(new_str) };
        Ok(args[2].clone())
    })
}

/// `(nelisp--char-table-aref T IDX)' / `(nelisp--char-table-aset T IDX V)'
/// — thin wrappers over `char_table_get' / `char_table_set_one'.
pub(crate) fn bi_char_table_aref(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let n = "nelisp--char-table-aref";
    slim!(args, n, 2, "char-table", Sexp::CharTable(rc) => Ok(
        crate::eval::builtins::char_table_get(rc,
            crate::eval::builtins::as_int(n, &args[1])?)))
}
pub(crate) fn bi_char_table_aset(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let n = "nelisp--char-table-aset";
    slim!(args, n, 3, "char-table", Sexp::CharTable(rc) => {
        let idx = crate::eval::builtins::as_int(n, &args[1])?;
        // SAFETY: Phase A.4.6 — closure does the entire mutation.
        unsafe { rc.with_inner_mut(|i|
            crate::eval::builtins::char_table_set_one(i, idx, args[2].clone())); }
        Ok(args[2].clone())
    })
}


