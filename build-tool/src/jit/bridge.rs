//! Doc 77b Stage b.2 — `nl-jit-call-*` bridge primitives.
//!
//! Provides the Rust-side primitives that elisp `lowered_X` wrappers
//! (= the future replacements for `lowered_X` Rust fns shipped in
//! Stage b.4) call to invoke JIT entries by name.  The 3 primitives
//! cover the 3 calling shapes the registered JIT entries need:
//!
//! - `nl-jit-call-i64-i64 NAME A B` — `extern "C" fn(i64, i64) -> i64'.
//!   Used by the 12 arith / cmp / bitwise + `ash' entries (= raw i64
//!   in / i64 out: `nelisp_jit_add2', `nelisp_jit_eq2' etc.).
//! - `nl-jit-call-ptr-ptr NAME A B` — `extern "C" fn(*const Sexp,
//!   *const Sexp) -> i64'.  Used by `nelisp_jit_eq_inline' (= the only
//!   ptr/ptr/i64 entry today).
//! - `nl-jit-call-syscall NAME NR A0 A1 A2 A3 A4 A5` — `extern "C"
//!   fn(i64 × 7) -> i64'.  Used by `nelisp_jit_syscall' /
//!   `nelisp_jit_syscall_supported_p' (= the latter takes 0 i64s but
//!   the trampoline shape forwards 7 padding zeros which Cranelift
//!   ignores when the sig has no params; for supported_p we route
//!   through the same primitive with NR/A0..A5 = 0 for uniformity).
//!
//! The name-keyed lookup is centralized in [`unified_fn_ptr`] which
//! maps every `nelisp_jit_*' symbol to the matching field in
//! [`super::UnifiedJit`].  Returns the raw `*const u8' so the call site
//! casts to the correct shape — the 3 primitives below are the
//! *only* place where that cast happens, keeping `unsafe' centralized.
//!
//! Out of scope for Stage b.2:
//! - elisp wrapper file (= `lisp/nelisp-jit-strategy.el', Stage b.4).
//! - DSL-driven rule registration via `nelisp--jit-bootstrap' (= Stage
//!   b.3 — the registry today is the typed `UnifiedJit' struct, not the
//!   yet-to-be-built name → FuncId alist from elisp).

use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

use super::unified_jit;

/// Extract the JIT-entry name argument: accepts `Symbol' or `Str'
/// (= the 2 forms elisp wrappers will produce — `'nelisp_jit_add2'
/// reads as `Symbol("nelisp_jit_add2")', `"nelisp_jit_add2"' as `Str').
fn as_name<'a>(name_arg: &'a str, v: &'a Sexp) -> Result<&'a str, EvalError> {
    match v {
        Sexp::Symbol(s) | Sexp::Str(s) => Ok(s.as_str()),
        other => Err(EvalError::WrongType {
            expected: format!("symbol or string ({} arg 0)", name_arg),
            got: other.clone(),
        }),
    }
}

/// Resolve a `nelisp_jit_*' name to the matching JIT-compiled fn ptr
/// stored in [`super::UnifiedJit`].  Returned as a raw `*const u8' so
/// each call shape (= i64/i64, ptr/ptr, 7×i64) casts independently.
///
/// Unknown names return `None' (= the bridge primitive raises
/// `EvalError::Internal' wrapping the bad name).
pub(super) fn unified_fn_ptr(name: &str) -> Option<*const u8> {
    let u = unified_jit();
    let p: *const u8 = match name {
        // ---- arith (12) ----
        "nelisp_jit_add2" => u.arith.add as *const u8,
        "nelisp_jit_sub2" => u.arith.sub as *const u8,
        "nelisp_jit_mul2" => u.arith.mul as *const u8,
        "nelisp_jit_eq2" => u.arith.eq as *const u8,
        "nelisp_jit_lt2" => u.arith.lt as *const u8,
        "nelisp_jit_gt2" => u.arith.gt as *const u8,
        "nelisp_jit_le2" => u.arith.le as *const u8,
        "nelisp_jit_ge2" => u.arith.ge as *const u8,
        "nelisp_jit_logior2" => u.arith.logior as *const u8,
        "nelisp_jit_logand2" => u.arith.logand as *const u8,
        "nelisp_jit_logxor2" => u.arith.logxor as *const u8,
        "nelisp_jit_ash" => u.arith.ash as *const u8,
        // ---- cons (5) ----
        "nelisp_jit_car" => u.cons.car as *const u8,
        "nelisp_jit_cdr" => u.cons.cdr as *const u8,
        "nelisp_jit_cons" => u.cons.cons_make as *const u8,
        "nelisp_jit_setcar" => u.cons.setcar as *const u8,
        "nelisp_jit_setcdr" => u.cons.setcdr as *const u8,
        // ---- access (4) ----
        "nelisp_jit_length" => u.access.length as *const u8,
        "nelisp_jit_aref" => u.access.aref as *const u8,
        "nelisp_jit_aset" => u.access.aset as *const u8,
        "nelisp_jit_elt" => u.access.elt as *const u8,
        // ---- predicate (1) ----
        "nelisp_jit_eq_inline" => u.predicate.eq as *const u8,
        // ---- syscall (2) ----
        "nelisp_jit_syscall" => u.syscall.syscall as *const u8,
        "nelisp_jit_syscall_supported_p" => u.syscall.supported_p as *const u8,
        _ => return None,
    };
    Some(p)
}

fn unknown_name_err(prim: &str, name: &str) -> EvalError {
    EvalError::Internal(format!(
        "{}: unknown JIT entry name `{}'", prim, name
    ))
}

/// `(nl-jit-call-i64-i64 NAME A B) -> Int'.  Looks up NAME in the
/// unified registry, casts the resolved fn ptr to
/// `extern "C" fn(i64, i64) -> i64', calls it with `(A, B)', wraps the
/// `i64' result as `Sexp::Int'.
pub fn bi_nl_jit_call_i64_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-i64-i64", args, 3, Some(3))?;
    let name = as_name("nl-jit-call-i64-i64", &args[0])?;
    let a = as_int("nl-jit-call-i64-i64", &args[1])?;
    let b = as_int("nl-jit-call-i64-i64", &args[2])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-i64-i64", name))?;
    // SAFETY: every name resolved by `unified_fn_ptr' to an arith /
    // syscall_supported_p slot has the `extern "C" fn(i64, i64) -> i64'
    // shape (or 0-arg supported_p which we route through
    // call-syscall instead — caller responsibility).
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(p) };
    let v = f(a, b);
    Ok(Sexp::Int(v))
}

/// `(nl-jit-call-ptr-ptr NAME A B) -> Int'.  Same shape as i64-i64 but
/// passes raw `*const Sexp' pointers (= what the cons / access /
/// predicate JIT entries expect).  Wrapping result as `Sexp::Int' so
/// the elisp wrapper can do `(if (= 0 v) nil t)' for predicate-style
/// entries or pass the result through for trampoline-OK / -ERR codes.
pub fn bi_nl_jit_call_ptr_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-ptr-ptr", args, 3, Some(3))?;
    let name = as_name("nl-jit-call-ptr-ptr", &args[0])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-ptr-ptr", name))?;
    // SAFETY: caller is responsible for passing a name whose JIT entry
    // has the `(*const Sexp, *const Sexp) -> i64' shape — currently
    // only `nelisp_jit_eq_inline' qualifies.  Passing a name with a
    // different shape is UB but the surface stays useful for the
    // single-purpose elisp-side `eq' wrapper Stage b.4 will ship.
    let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let v = f(&args[1] as *const _, &args[2] as *const _);
    Ok(Sexp::Int(v))
}

/// `(nl-jit-call-syscall NAME NR A0 A1 A2 A3 A4 A5) -> Int'.  Calls
/// `extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64'.  The
/// shape matches `nelisp_jit_syscall' precisely; for
/// `nelisp_jit_syscall_supported_p' (which has no params) callers
/// should use `nl-jit-call-i64-i64' with dummy a/b (Cranelift does
/// not exercise extra args of a 0-arg fn under the host C ABI but
/// this is technically UB; supported_p is rarely on the hot path).
pub fn bi_nl_jit_call_syscall(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-syscall", args, 8, Some(8))?;
    let name = as_name("nl-jit-call-syscall", &args[0])?;
    let nr = as_int("nl-jit-call-syscall", &args[1])?;
    let a0 = as_int("nl-jit-call-syscall", &args[2])?;
    let a1 = as_int("nl-jit-call-syscall", &args[3])?;
    let a2 = as_int("nl-jit-call-syscall", &args[4])?;
    let a3 = as_int("nl-jit-call-syscall", &args[5])?;
    let a4 = as_int("nl-jit-call-syscall", &args[6])?;
    let a5 = as_int("nl-jit-call-syscall", &args[7])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-syscall", name))?;
    // SAFETY: caller passes a name whose JIT entry has the
    // `(i64 × 7) -> i64' shape — currently `nelisp_jit_syscall'.
    let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
        unsafe { std::mem::transmute(p) };
    let v = f(nr, a0, a1, a2, a3, a4, a5);
    Ok(Sexp::Int(v))
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- unified_fn_ptr coverage ---

    #[test]
    fn unified_fn_ptr_resolves_all_24_entries() {
        // The complete name set the bridge supports (= 12 arith + 5
        // cons + 4 access + 1 predicate + 2 syscall = 24).  Each must
        // resolve to a non-null fn ptr.
        let names = [
            "nelisp_jit_add2",
            "nelisp_jit_sub2",
            "nelisp_jit_mul2",
            "nelisp_jit_eq2",
            "nelisp_jit_lt2",
            "nelisp_jit_gt2",
            "nelisp_jit_le2",
            "nelisp_jit_ge2",
            "nelisp_jit_logior2",
            "nelisp_jit_logand2",
            "nelisp_jit_logxor2",
            "nelisp_jit_ash",
            "nelisp_jit_car",
            "nelisp_jit_cdr",
            "nelisp_jit_cons",
            "nelisp_jit_setcar",
            "nelisp_jit_setcdr",
            "nelisp_jit_length",
            "nelisp_jit_aref",
            "nelisp_jit_aset",
            "nelisp_jit_elt",
            "nelisp_jit_eq_inline",
            "nelisp_jit_syscall",
            "nelisp_jit_syscall_supported_p",
        ];
        for n in names {
            let p = unified_fn_ptr(n);
            assert!(
                p.is_some(),
                "unified_fn_ptr missing entry `{}'", n
            );
            assert!(!p.unwrap().is_null(), "fn ptr for `{}' is null", n);
        }
    }

    #[test]
    fn unified_fn_ptr_unknown_returns_none() {
        assert!(unified_fn_ptr("nelisp_jit_does_not_exist").is_none());
        assert!(unified_fn_ptr("").is_none());
    }

    // --- bi_nl_jit_call_i64_i64 ---

    #[test]
    fn call_i64_i64_add2_smoke() {
        let r = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(7),
            Sexp::Int(8),
        ])
        .expect("add2 must succeed");
        assert_eq!(r, Sexp::Int(15));
    }

    #[test]
    fn call_i64_i64_accepts_string_name() {
        // Symbol or Str both supported (= flexibility for elisp callers
        // that pass `'nelisp_jit_mul2' or `"nelisp_jit_mul2"').
        let r = bi_nl_jit_call_i64_i64(&[
            Sexp::Str("nelisp_jit_mul2".into()),
            Sexp::Int(6),
            Sexp::Int(7),
        ])
        .expect("mul2 (str name) must succeed");
        assert_eq!(r, Sexp::Int(42));
    }

    #[test]
    fn call_i64_i64_unknown_name_errors() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_no_such".into()),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("unknown name must error");
        match err {
            EvalError::Internal(msg) => {
                assert!(msg.contains("nl-jit-call-i64-i64"));
                assert!(msg.contains("nelisp_jit_no_such"));
            }
            other => panic!("expected Internal, got {:?}", other),
        }
    }

    #[test]
    fn call_i64_i64_arity_too_few() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(1),
        ])
        .expect_err("arity 2 must reject");
        match err {
            EvalError::WrongNumberOfArguments { function, .. } => {
                assert_eq!(function, "nl-jit-call-i64-i64");
            }
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn call_i64_i64_arity_too_many() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(1),
            Sexp::Int(2),
            Sexp::Int(3),
        ])
        .expect_err("arity 4 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
    }

    #[test]
    fn call_i64_i64_bad_name_type_errors() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Int(0),
            Sexp::Int(1),
            Sexp::Int(2),
        ])
        .expect_err("Int as name must error");
        assert!(matches!(err, EvalError::WrongType { .. }));
    }

    // --- bi_nl_jit_call_ptr_ptr ---

    #[test]
    fn call_ptr_ptr_eq_inline_match() {
        // Two equal ints → eq returns 1.
        let r = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()),
            Sexp::Int(7),
            Sexp::Int(7),
        ])
        .expect("eq_inline must succeed");
        assert_eq!(r, Sexp::Int(1));
    }

    #[test]
    fn call_ptr_ptr_eq_inline_mismatch() {
        // Two different ints → eq returns 0.
        let r = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()),
            Sexp::Int(7),
            Sexp::Int(8),
        ])
        .expect("eq_inline must succeed");
        assert_eq!(r, Sexp::Int(0));
    }

    #[test]
    fn call_ptr_ptr_unknown_name_errors() {
        let err = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_no_such_ptr".into()),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("unknown name must error");
        assert!(matches!(err, EvalError::Internal(_)));
    }

    #[test]
    fn call_ptr_ptr_arity_too_few() {
        let err = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()),
            Sexp::Int(1),
        ])
        .expect_err("arity 2 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
    }

    // --- bi_nl_jit_call_syscall ---

    #[test]
    fn call_syscall_arity_check() {
        // Wrong arity (= 7 args instead of 8) must reject before
        // touching the syscall fn ptr.
        let err = bi_nl_jit_call_syscall(&[
            Sexp::Symbol("nelisp_jit_syscall".into()),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("arity 7 must reject");
        match err {
            EvalError::WrongNumberOfArguments { function, .. } => {
                assert_eq!(function, "nl-jit-call-syscall");
            }
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn call_syscall_unknown_name_errors() {
        let err = bi_nl_jit_call_syscall(&[
            Sexp::Symbol("nelisp_jit_no_syscall".into()),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("unknown name must error");
        match err {
            EvalError::Internal(msg) => {
                assert!(msg.contains("nl-jit-call-syscall"));
                assert!(msg.contains("nelisp_jit_no_syscall"));
            }
            other => panic!("expected Internal, got {:?}", other),
        }
    }
}
