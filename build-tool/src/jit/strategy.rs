//! Rust helper primitives backing the elisp JIT-strategy wrappers in
//! `lisp/nelisp-jit-strategy.el'.  Post-Phase-7.1.7.a.1 (Doc 28
//! §3.7.a.1, 2026-05-10) the surface is 2 permanent residual
//! categories:
//!
//! 1. *Doc 80 §7 permanent out-of-scope*: `bi_{add,sub,mul}2_float',
//!    `bi_num_{eq,lt,gt,le,ge}2_float', `bi_syscall_nr_resolve' —
//!    Float epsilon arithmetic + host `libc::SYS_*' constants are
//!    not expressible on the pre-stdlib elisp substrate.
//! 2. *Doc 80.4 slim primitives*: `bi_mut_str_len' /
//!    `bi_bool_vector_len' / `bi_str_codepoint_at' /
//!    `bi_mut_str_set_codepoint' / `bi_char_table_{aref,aset}' —
//!    reach into `Sexp' variant boxes the elisp surface cannot
//!    expose.  Used by elisp `length' / `aref' / `aset' / `elt'
//!    fall-through arms.
//!
//! Phase 7.1.7.a.1 deleted `bi_int_eq_zero' + 3 bitwise + `bi_ash_impl'
//! (= 5 fns); their semantics now live in `nelisp-jit-strategy.el' on
//! top of the `nl-jit-call-i64-i64' bridge.

use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// ---------- arith Float fallbacks ----------------------------------

macro_rules! float_arith_helper {
    ($fn_name:ident, $op:tt, $primitive:literal) => {
        pub(crate) fn $fn_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
            let (a, b, _) = crate::eval::builtins::num_pair(args, $primitive)?;
            Ok(Sexp::Float(a $op b))
        }
    };
}

float_arith_helper!(bi_add2_float, +, "nelisp--add2");
float_arith_helper!(bi_sub2_float, -, "nelisp--sub2");
float_arith_helper!(bi_mul2_float, *, "nelisp--mul2");

// ---------- cmp Float fallbacks ------------------------------------

pub(crate) fn bi_num_eq2_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (a, b, _) = crate::eval::builtins::num_pair(args, "nelisp--num-eq2")?;
    Ok(if (a - b).abs() < 1e-15 { Sexp::T } else { Sexp::Nil })
}

macro_rules! float_cmp_helper {
    ($fn_name:ident, $op:tt, $primitive:literal) => {
        pub(crate) fn $fn_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
            let (a, b, _) = crate::eval::builtins::num_pair(args, $primitive)?;
            Ok(if a $op b { Sexp::T } else { Sexp::Nil })
        }
    };
}

float_cmp_helper!(bi_num_lt2_float, <, "nelisp--num-lt2");
float_cmp_helper!(bi_num_gt2_float, >, "nelisp--num-gt2");
float_cmp_helper!(bi_num_le2_float, <=, "nelisp--num-le2");
float_cmp_helper!(bi_num_ge2_float, >=, "nelisp--num-ge2");

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

// ---------- syscall-nr-resolve ------------------------------------

#[cfg(target_os = "linux")]
pub(crate) fn bi_syscall_nr_resolve(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--syscall-nr-resolve", args, 1, Some(1))?;
    let nr = crate::eval::builtins::syscall_nr(&args[0])?;
    Ok(Sexp::Int(nr))
}

#[cfg(not(target_os = "linux"))]
pub(crate) fn bi_syscall_nr_resolve(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let _ = args;
    Err(EvalError::Internal(
        "nelisp--syscall-nr-resolve: unsupported platform".into(),
    ))
}

