//! Doc 100 Tier A wear-test #1 — `(nl-int-neg N)' = `(- N)'.
//!
//! Cross-impl equivalence against Rust's native `wrapping_neg` for
//! signed extremes plus a representative spread.  Validates the same
//! `caller-owned-slot' ABI as §100.C, exercising `sexp-int-unwrap'
//! + `-' (arith) + `sexp-int-make' grammar chained into a single
//! elisp expression.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

#[test]
fn int_neg_signed_extremes_match_wrapping_neg() {
    let cases: &[i64] = &[
        i64::MIN, // wraps to itself in two's complement
        i64::MIN + 1,
        -1_000_000_000_000,
        -42,
        -1,
        0,
        1,
        42,
        1_000_000_000_000,
        i64::MAX - 1,
        i64::MAX,
    ];
    for &n in cases {
        let input = Sexp::Int(n);
        let mut slot: Sexp = Sexp::Nil;
        unsafe {
            nelisp_build_tool::elisp_cc_spike::int_neg(
                &input as *const Sexp,
                &mut slot as *mut Sexp,
            );
        }
        let expected = 0i64.wrapping_sub(n);
        match &slot {
            Sexp::Int(got) => assert_eq!(
                *got, expected,
                "int_neg({n}): elisp = {got}, rust ref = {expected}",
            ),
            other => panic!("int_neg({n}) wrote non-Int: {other:?}"),
        }
    }
}

#[test]
fn int_neg_double_negation_is_identity_mod_imin() {
    // (- (- N)) = N except at i64::MIN (= overflows to itself).
    for n in [-9_223_372_036_854_775_807i64, -1, 0, 1, 42, i64::MAX] {
        let input = Sexp::Int(n);
        let mut slot1: Sexp = Sexp::Nil;
        unsafe {
            nelisp_build_tool::elisp_cc_spike::int_neg(
                &input as *const Sexp,
                &mut slot1 as *mut Sexp,
            );
        }
        let mut slot2: Sexp = Sexp::Nil;
        unsafe {
            nelisp_build_tool::elisp_cc_spike::int_neg(
                &slot1 as *const Sexp,
                &mut slot2 as *mut Sexp,
            );
        }
        match &slot2 {
            Sexp::Int(got) => assert_eq!(*got, n, "double-neg({n}) round-trip"),
            other => panic!("double-neg({n}) wrote non-Int: {other:?}"),
        }
    }
}
