//! Rust helper primitives backing the elisp JIT-strategy wrappers in
//! `lisp/nelisp-jit-strategy.el' (= Doc 77b Stage b.4 + Doc 80
//! substrate).  Per-fn docstrings cover the contract; the surface
//! groups into 3 categories per Doc 80 §7 + §3.7.a audit:
//!
//! 1. *Doc 80 §7 permanent out-of-scope* (= cannot be expressed in
//!    pre-stdlib elisp): `bi_int_eq_zero', `bi_{add,sub,mul}2_float',
//!    `bi_num_{eq,lt,gt,le,ge}2_float', `bi_{logior,logand,logxor}2_impl',
//!    `bi_ash_impl', `bi_syscall_nr_resolve'.  Float arith / cmp use
//!    libm via Rust `f64' ops (1e-15 epsilon for `eq2'); bitwise uses
//!    `as_int' cast (Float→Int truncation + WrongType for non-numeric);
//!    `syscall_nr_resolve' wraps the `libc::SYS_*' symbol catalog.
//!    Doc 28 §3.7.a.1 ports `bi_int_eq_zero' / bitwise 3 / `bi_ash_impl'
//!    to elisp on top of nelisp-cc, leaving Float / syscall as
//!    permanent Rust residual.
//!
//! 2. *Doc 80.4 ship slim primitives* (= reach into `Sexp' variant
//!    boxes elisp cannot expose): `bi_mut_str_len',
//!    `bi_bool_vector_len', `bi_str_codepoint_at',
//!    `bi_mut_str_set_codepoint', `bi_char_table_{aref,aset}'.  Used
//!    by elisp `length' / `aref' / `aset' / `elt' fall-through arms
//!    after the JIT trampoline raises ERR.

use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// ---------- nelisp--int-eq-zero ------------------------------------

pub(crate) fn bi_int_eq_zero(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--int-eq-zero", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(0) => Ok(Sexp::T),
        Sexp::Int(_) => Ok(Sexp::Nil),
        other => Err(EvalError::WrongType {
            expected: "integer".into(),
            got: other.clone(),
        }),
    }
}

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

// ---------- bitwise Int-only impls --------------------------------

macro_rules! bitwise_int_helper {
    ($fn_name:ident, $op:tt, $primitive:literal) => {
        pub(crate) fn $fn_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
            crate::eval::builtins::require_arity($primitive, args, 2, Some(2))?;
            let a = crate::eval::builtins::as_int($primitive, &args[0])?;
            let b = crate::eval::builtins::as_int($primitive, &args[1])?;
            Ok(Sexp::Int(a $op b))
        }
    };
}

bitwise_int_helper!(bi_logior2_impl, |, "nelisp--logior2");
bitwise_int_helper!(bi_logand2_impl, &, "nelisp--logand2");
bitwise_int_helper!(bi_logxor2_impl, ^, "nelisp--logxor2");

// ---------- ash impl (verbatim from pre-b.4 lowered_ash) ----------

pub(crate) fn bi_ash_impl(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("ash", args, 2, Some(2))?;
    let n = crate::eval::builtins::as_int("ash", &args[0])?;
    let count = crate::eval::builtins::as_int("ash", &args[1])?;
    if (-62..=62).contains(&count) {
        // Phase 7.1.6.c: call the `nl_jit_arith_ash' trampoline directly
        // (= the deleted Cranelift `JitArith::ash' fn-ptr's replacement).
        // Caller is contractually responsible for the `[-62, +62]'
        // bounds-check, which the conditional above enforces.
        return Ok(Sexp::Int(unsafe {
            super::arith::nl_jit_arith_ash(n, count)
        }));
    }
    let r = if count >= 0 {
        if count >= 63 { 0 } else { n.wrapping_shl(count as u32) }
    } else {
        let abs = (-count) as u32;
        if abs >= 63 { if n < 0 { -1 } else { 0 } } else { n >> abs }
    };
    Ok(Sexp::Int(r))
}

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

