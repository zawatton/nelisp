//! Doc 77b Stage b.4 — Rust helper primitives for elisp JIT-strategy
//! wrappers (= `lisp/nelisp-jit-strategy.el').
//!
//! The 24 elisp wrappers shipped in `nelisp-jit-strategy.el' replace
//! the pre-b.4 `lowered_X' Rust strategy fns by routing through the
//! Stage b.2 / b.2.5 `nl-jit-call-*' bridge primitives.  For the
//! Int+Int (or in-range Vector) fast paths the wrappers go straight
//! to the bridge; for fall-through cases they call into one of the
//! helpers below:
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
//!   - `nelisp--{length,aref,aset,elt}-impl ...' — full multi-variant
//!     dispatch for the access primitives (= JIT call covers the
//!     Vector/Str/Nil/Cons-walk fast paths, fall-through handles
//!     MutStr/CharTable/BoolVector with canonical `ArithError' (out-
//!     of-range, negative index) vs `WrongType' (non-arrayp,
//!     immutable Str aset) error shapes).  Body lifted verbatim from
//!     the pre-b.4 `lowered_length' / `aref_helper' / `lowered_aset'
//!     / `lowered_elt'.  Multi-variant dispatch with two distinct
//!     error families is not a clean fit for pre-stdlib elisp (=
//!     `cond' / `signal' aren't installed yet at strategy load time).
//!
//!   - `nelisp--syscall-nr-resolve NAME-OR-NR' — wraps
//!     `syscall_nr' (= the symbol → `libc::SYS_*' Int map).  The
//!     elisp `nelisp--syscall' wrapper then forwards the resolved Int
//!     through `nl-jit-call-syscall'.  Keeps the libc symbol catalog
//!     in Rust where the `libc::SYS_*' constants live.

use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// Match the pre-b.4 trampoline ABI used by `nl_jit_*' fns.
const TRAMPOLINE_OK: i64 = 0;

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

// ---------- length impl (verbatim from pre-b.4 lowered_length) ----

pub fn bi_length_impl(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("length", args, 1, Some(1))?;
    let access = &super::unified_jit().access;
    let mut out = Sexp::Nil;
    let r = (access.length)(&args[0] as *const _, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    match &args[0] {
        Sexp::MutStr(rc) => Ok(Sexp::Int(rc.borrow().chars().count() as i64)),
        Sexp::BoolVector(v) => Ok(Sexp::Int(v.borrow().len() as i64)),
        Sexp::Cons(_) => {
            let mut n = 0i64;
            let mut cur: Sexp = args[0].clone();
            loop {
                let next = match &cur {
                    Sexp::Nil => return Ok(Sexp::Int(n)),
                    Sexp::Cons(b) => {
                        n += 1;
                        b.cdr.clone()
                    }
                    other => {
                        return Err(EvalError::WrongType {
                            expected: "sequence".into(),
                            got: other.clone(),
                        });
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

// ---------- aref / aset / elt impls (verbatim from pre-b.4) -------

fn aref_helper(args: &[Sexp], primitive: &'static str) -> Result<Sexp, EvalError> {
    let index = crate::eval::builtins::as_int(primitive, &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "{}: negative index {}",
            primitive, index
        )));
    }
    let access = &super::unified_jit().access;
    let mut out = Sexp::Nil;
    let r = (access.aref)(&args[0] as *const _, index, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    match &args[0] {
        Sexp::Str(s) => {
            let chars: Vec<char> = s.chars().collect();
            chars
                .get(index as usize)
                .map(|c| Sexp::Int(*c as i64))
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "{}: index {} out of range for string of length {}",
                        primitive,
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
                        "{}: index {} out of range for string of length {}",
                        primitive,
                        index,
                        chars.len()
                    ))
                })
        }
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            Err(EvalError::ArithError(format!(
                "{}: index {} out of range for vector of length {}",
                primitive,
                index,
                borrowed.len()
            )))
        }
        Sexp::CharTable(rc) => Ok(crate::eval::builtins::char_table_get(rc, index)),
        Sexp::BoolVector(v) => {
            let borrowed = v.borrow();
            borrowed
                .get(index as usize)
                .map(|b| if *b { Sexp::T } else { Sexp::Nil })
                .ok_or_else(|| {
                    EvalError::ArithError(format!(
                        "{}: index {} out of range for bool-vector of length {}",
                        primitive,
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

pub fn bi_aref_impl(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("aref", args, 2, Some(2))?;
    aref_helper(args, "aref")
}

pub fn bi_aset_impl(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("aset", args, 3, Some(3))?;
    let index = crate::eval::builtins::as_int("aset", &args[1])?;
    if index < 0 {
        return Err(EvalError::ArithError(format!(
            "aset: negative index {}",
            index
        )));
    }
    let access = &super::unified_jit().access;
    let mut out = Sexp::Nil;
    let r = (access.aset)(
        &args[0] as *const _,
        index,
        &args[2] as *const _,
        &mut out as *mut _,
    );
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    match &args[0] {
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            let len = borrowed.len();
            Err(EvalError::ArithError(format!(
                "aset: index {} out of range for vector of length {}",
                index, len
            )))
        }
        Sexp::MutStr(rc) => {
            let new_ch = match &args[2] {
                Sexp::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
                    EvalError::WrongType {
                        expected: "valid character codepoint".into(),
                        got: args[2].clone(),
                    }
                })?,
                other => {
                    return Err(EvalError::WrongType {
                        expected: "character (integer)".into(),
                        got: other.clone(),
                    });
                }
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
            crate::eval::builtins::char_table_set_one(&mut inner, index, args[2].clone());
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
            borrowed[index as usize] = crate::eval::special_forms::is_truthy(&args[2]);
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

pub fn bi_elt_impl(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity("elt", args, 2, Some(2))?;
    let idx = crate::eval::builtins::as_int("elt", &args[1])?;
    if idx < 0 {
        return Err(EvalError::ArithError(format!(
            "elt: negative index {}",
            idx
        )));
    }
    let access = &super::unified_jit().access;
    let mut out = Sexp::Nil;
    let r = (access.elt)(&args[0] as *const _, idx, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        return Ok(out);
    }
    match &args[0] {
        Sexp::Nil => Err(EvalError::ArithError(format!(
            "elt: index {} out of range for empty sequence",
            idx
        ))),
        Sexp::Cons(_) => Err(EvalError::ArithError(format!(
            "elt: index {} out of range for list",
            idx
        ))),
        Sexp::Str(_) | Sexp::MutStr(_) | Sexp::Vector(_)
        | Sexp::CharTable(_) | Sexp::BoolVector(_) => aref_helper(args, "elt"),
        other => Err(EvalError::WrongType {
            expected: "sequencep".into(),
            got: other.clone(),
        }),
    }
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

