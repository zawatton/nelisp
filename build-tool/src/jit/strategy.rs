//! Doc 77b Stage b.4 — Rust helper primitives for elisp JIT-strategy
//! wrappers (= `lisp/nelisp-jit-strategy.el').
//!
//! Doc 80 Stage 80.3〜80.5 (2026-05-09) — `bi_length_impl' /
//! `aref_helper' / `bi_aref_impl' / `bi_aset_impl' / `bi_elt_impl'
//! deleted (= ~255 LOC).  The multi-variant fall-through dispatch is
//! now expressed in elisp via the Doc 80 substrate (`cond' / `signal'
//! / `nelisp--signal-{wrong-type,arith}'), with narrow per-variant
//! slim primitives in this file (= `bi_mut_str_*' / `bi_bool_vector_*'
//! / `bi_char_table_*') for the Sexp-internal mutations that elisp
//! cannot reach without exposing the variant boxes wholesale.
//!
//! The remaining helpers (`nelisp--int-eq-zero', arith Float, bitwise
//! Int, `ash', `syscall-nr-resolve') stay as before — they are not in
//! Doc 80's scope.
//!
//!   - `nelisp--int-eq-zero N'                — `if'-friendly bool
//!     conversion of bridge `i64' results.  Used by the `eq' wrapper
//!     (= the bridge returns 1/0; elisp wants T/Nil) and the cmp
//!     arith wrappers (= same shape).
//!
//!   - `nelisp--{add2,sub2,mul2}-float A B'    — Float promotion
//!     fallback for `+' / `-' / `*' when at least one arg is Float.
//!     Cannot be expressed in elisp without recursing through the
//!     stdlib `+' / `-' / `*' (which themselves call
//!     `nelisp--{add,sub,mul}2' — the wrapper we are inside).
//!
//!   - `nelisp--num-{eq,lt,gt,le,ge}2-float A B' — Float-cmp fallback
//!     same rationale, and the `eq2' Float case uses 1e-15 epsilon
//!     per the pre-b.4 `lowered_num_eq2' contract.
//!
//!   - `nelisp--{logior2,logand2,logxor2}-impl A B' — Int-only
//!     bitwise (= `as_int' cast handles `Float' implicit truncation
//!     + canonical `WrongType' for non-numeric).  Distinct from the
//!     arith Float fallbacks because elisp bitwise has no Float
//!     output path (= host Emacs contract).
//!
//!   - `nelisp--ash-impl N COUNT'              — `ash' Int-only with
//!     the [-62, +62] JIT bounds + clamping fallback for pathological
//!     counts.  Body lifted verbatim from the pre-b.4 `lowered_ash'.
//!
//!   - `nelisp--mut-str-len S' / `nelisp--bool-vector-len V' — slim
//!     fall-through primitives for `length' (= MutStr UTF-8 char count
//!     / BoolVector bit count).  `Sexp::Str' / `Sexp::Vector' /
//!     `Sexp::Nil' length stay JIT-trampoline-only.
//!
//!   - `nelisp--str-codepoint-at S IDX' — Str + MutStr char-indexed
//!     codepoint read with `arith-error' on out-of-range.  Used by
//!     elisp `aref' / `elt' fall-through for both immutable + mutable
//!     strings.
//!
//!   - `nelisp--mut-str-set-codepoint S IDX CP' — MutStr in-place
//!     codepoint mutation (= rebuilds the backing String).  Returns
//!     CP per Emacs `aset' contract.
//!
//!   - `nelisp--char-table-aref T IDX' / `nelisp--char-table-aset T
//!     IDX V' — CharTable get / set wrappers over `char_table_get' /
//!     `char_table_set_one'.  Kept Rust because `CharTableInner' is
//!     a private box; no general elisp accessor exists.
//!
//!   - `nelisp--syscall-nr-resolve NAME-OR-NR' — wraps
//!     `syscall_nr' (= the symbol → `libc::SYS_*' Int map).  The
//!     elisp `nelisp--syscall' wrapper then forwards the resolved Int
//!     through `nl-jit-call-syscall'.  Keeps the libc symbol catalog
//!     in Rust where the `libc::SYS_*' constants live.

use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// ---------- nelisp--int-eq-zero ------------------------------------

pub fn bi_int_eq_zero(args: &[Sexp]) -> Result<Sexp, EvalError> {
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
        pub fn $fn_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
            let (a, b, _) = crate::eval::builtins::num_pair(args, $primitive)?;
            Ok(Sexp::Float(a $op b))
        }
    };
}

float_arith_helper!(bi_add2_float, +, "nelisp--add2");
float_arith_helper!(bi_sub2_float, -, "nelisp--sub2");
float_arith_helper!(bi_mul2_float, *, "nelisp--mul2");

// ---------- cmp Float fallbacks ------------------------------------

pub fn bi_num_eq2_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (a, b, _) = crate::eval::builtins::num_pair(args, "nelisp--num-eq2")?;
    Ok(if (a - b).abs() < 1e-15 { Sexp::T } else { Sexp::Nil })
}

macro_rules! float_cmp_helper {
    ($fn_name:ident, $op:tt, $primitive:literal) => {
        pub fn $fn_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
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
        pub fn $fn_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
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

pub fn bi_ash_impl(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("ash", args, 2, Some(2))?;
    let n = crate::eval::builtins::as_int("ash", &args[0])?;
    let count = crate::eval::builtins::as_int("ash", &args[1])?;
    if (-62..=62).contains(&count) {
        let arith = &super::unified_jit().arith;
        return Ok(Sexp::Int((arith.ash)(n, count)));
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

/// `(nelisp--mut-str-len S)' — UTF-8 char count for `Sexp::MutStr'.
/// Signals `wrong-type-argument' for any other variant.
pub fn bi_mut_str_len(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--mut-str-len", args, 1, Some(1))?;
    match &args[0] {
        Sexp::MutStr(rc) => Ok(Sexp::Int(rc.value.chars().count() as i64)),
        other => Err(EvalError::WrongType {
            expected: "mut-string".into(),
            got: other.clone(),
        }),
    }
}

/// `(nelisp--bool-vector-len V)' — bit count for `Sexp::BoolVector'.
pub fn bi_bool_vector_len(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--bool-vector-len", args, 1, Some(1))?;
    match &args[0] {
        Sexp::BoolVector(v) => Ok(Sexp::Int(v.value.len() as i64)),
        other => Err(EvalError::WrongType {
            expected: "bool-vector".into(),
            got: other.clone(),
        }),
    }
}

/// `(nelisp--str-codepoint-at S IDX)' — char-indexed codepoint read
/// for `Sexp::Str' / `Sexp::MutStr'.  Signals `arith-error' on
/// out-of-range and `wrong-type-argument' on non-string S.
pub fn bi_str_codepoint_at(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--str-codepoint-at", args, 2, Some(2))?;
    let idx = crate::eval::builtins::as_int("nelisp--str-codepoint-at", &args[1])?;
    let s: &str = match &args[0] {
        Sexp::Str(s) => s.as_str(),
        Sexp::MutStr(rc) => rc.value.as_str(),
        other => return Err(EvalError::WrongType {
            expected: "string".into(),
            got: other.clone(),
        }),
    };
    if idx < 0 {
        return Err(EvalError::ArithError(format!("negative index {}", idx)));
    }
    s.chars().nth(idx as usize)
        .map(|c| Sexp::Int(c as i64))
        .ok_or_else(|| EvalError::ArithError(format!("index {} out of range", idx)))
}

/// `(nelisp--mut-str-set-codepoint S IDX CP)' — in-place codepoint
/// mutation of a `Sexp::MutStr'.  Returns CP per Emacs `aset' contract.
pub fn bi_mut_str_set_codepoint(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--mut-str-set-codepoint", args, 3, Some(3))?;
    let idx = crate::eval::builtins::as_int("nelisp--mut-str-set-codepoint", &args[1])?;
    let cp = crate::eval::builtins::as_int("nelisp--mut-str-set-codepoint", &args[2])?;
    let new_ch = char::from_u32(cp as u32).ok_or_else(|| EvalError::WrongType {
        expected: "valid character codepoint".into(),
        got: args[2].clone(),
    })?;
    let rc = match &args[0] {
        Sexp::MutStr(rc) => rc,
        other => return Err(EvalError::WrongType {
            expected: "mut-string".into(),
            got: other.clone(),
        }),
    };
    let chars: Vec<char> = rc.value.chars().collect();
    if idx < 0 || (idx as usize) >= chars.len() {
        return Err(EvalError::ArithError(format!("index {} out of range", idx)));
    }
    let mut new_str = String::with_capacity(rc.value.len());
    for (i, c) in chars.iter().enumerate() {
        new_str.push(if i == idx as usize { new_ch } else { *c });
    }
    // SAFETY: Phase A.4.2 — same discipline as the pre-Doc-80
    // `bi_aset_impl' MutStr arm; locals don't alias the box.
    unsafe { rc.set_value(new_str) };
    Ok(args[2].clone())
}

/// `(nelisp--char-table-aref T IDX)' — wrapper over `char_table_get'.
pub fn bi_char_table_aref(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--char-table-aref", args, 2, Some(2))?;
    let idx = crate::eval::builtins::as_int("nelisp--char-table-aref", &args[1])?;
    match &args[0] {
        Sexp::CharTable(rc) => Ok(crate::eval::builtins::char_table_get(rc, idx)),
        other => Err(EvalError::WrongType {
            expected: "char-table".into(),
            got: other.clone(),
        }),
    }
}

/// `(nelisp--char-table-aset T IDX V)' — wrapper over
/// `char_table_set_one'.  Returns V per `aset' contract.
pub fn bi_char_table_aset(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--char-table-aset", args, 3, Some(3))?;
    let idx = crate::eval::builtins::as_int("nelisp--char-table-aset", &args[1])?;
    let rc = match &args[0] {
        Sexp::CharTable(rc) => rc,
        other => return Err(EvalError::WrongType {
            expected: "char-table".into(),
            got: other.clone(),
        }),
    };
    // SAFETY: Phase A.4.6 — closure does the entire mutation.
    unsafe {
        rc.with_inner_mut(|inner| {
            crate::eval::builtins::char_table_set_one(inner, idx, args[2].clone());
        });
    }
    Ok(args[2].clone())
}

// ---------- syscall-nr-resolve ------------------------------------

#[cfg(target_os = "linux")]
pub fn bi_syscall_nr_resolve(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("nelisp--syscall-nr-resolve", args, 1, Some(1))?;
    let nr = crate::eval::builtins::syscall_nr(&args[0])?;
    Ok(Sexp::Int(nr))
}

#[cfg(not(target_os = "linux"))]
pub fn bi_syscall_nr_resolve(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let _ = args;
    Err(EvalError::Internal(
        "nelisp--syscall-nr-resolve: unsupported platform".into(),
    ))
}

