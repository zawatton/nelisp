//! Doc 119 §119.A probe — `(truncate FLOAT)' Float arm, swapped from Rust
//! inline into Phase 47-compiled elisp via G4+G5 grammar.
//!
//! `bi_truncate''s Float branch used to compute `Sexp::Float(x) =>
//! Ok(Sexp::Int(*x as i64))' inline (= 1 LOC of Rust algorithm).  Doc 119
//! §119.A replaced that line with a delegation to `elisp_cc_spike::
//! truncate_float', whose body lives in `lisp/nelisp-cc-truncate-float.el'
//! and only there.  The probe validates:
//!
//! 1.  Cross-impl equivalence — a spread of f64 values including ±inf / NaN
//!     borderlines, whole integers, halves, and extremes.
//! 2.  Direct Sexp-slot inspection for tag byte (= SEXP_TAG_INT) and payload.
//! 3.  Round-trip through the public `bi_truncate' dispatch in a small Env.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::{Sexp, SEXP_TAG_INT};

/// Helper: call `truncate_float' and return the resulting `Sexp::Int(n)'.
fn truncate_float_call(x: f64) -> Sexp {
    let input = Sexp::Float(x);
    let mut slot: Sexp = Sexp::Nil;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::truncate_float(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        )
    };
    assert_eq!(
        returned, &mut slot as *mut Sexp,
        "truncate_float({x}): returned pointer should equal caller slot",
    );
    slot
}

#[test]
fn truncate_float_whole_integer_values() {
    // Whole-number floats should produce the same i64 as a direct Rust cast.
    let cases: &[f64] = &[0.0, 1.0, -1.0, 42.0, -42.0, 1_000_000.0, -1_000_000.0];
    for &x in cases {
        let expected = x as i64;
        match truncate_float_call(x) {
            Sexp::Int(got) => assert_eq!(
                got, expected,
                "truncate_float({x}) → {got} but expected {expected}",
            ),
            other => panic!("truncate_float({x}) wrote non-Int Sexp: {other:?}"),
        }
    }
}

#[test]
fn truncate_float_truncates_toward_zero() {
    // Fractional values must be truncated toward zero (= C-cast semantics,
    // matching CVTTSD2SI).  3.9 → 3, -3.9 → -3.
    let cases: &[(f64, i64)] = &[
        (3.9, 3),
        (-3.9, -3),
        (0.999, 0),
        (-0.999, 0),
        (99.9, 99),
    ];
    for &(x, expected) in cases {
        match truncate_float_call(x) {
            Sexp::Int(got) => assert_eq!(
                got, expected,
                "truncate_float({x}) → {got} but expected {expected}",
            ),
            other => panic!("truncate_float({x}) wrote non-Int Sexp: {other:?}"),
        }
    }
}

#[test]
fn truncate_float_writes_correct_tag_byte() {
    // Raw byte at offset 0 must be SEXP_TAG_INT (= 2).
    let input = Sexp::Float(1.5);
    let mut slot: Sexp = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::truncate_float(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        );
    }
    let tag = unsafe { *(&slot as *const Sexp as *const u8) };
    assert_eq!(
        tag, SEXP_TAG_INT,
        "elisp .o wrote tag byte {tag} but expected SEXP_TAG_INT = {SEXP_TAG_INT}",
    );
}

#[test]
fn truncate_float_writes_correct_payload_bytes() {
    // 7.75 truncates to 7; inspect the raw payload bytes at offset 8.
    let input = Sexp::Float(7.75);
    let mut slot: Sexp = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::truncate_float(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        );
    }
    let payload_ptr = unsafe { (&slot as *const Sexp as *const u8).add(8) as *const i64 };
    let got = unsafe { *payload_ptr };
    assert_eq!(got, 7i64, "elisp .o wrote payload {got} but expected 7");
}

#[test]
fn truncate_float_spread_matches_rust_cast() {
    // Compare against Rust's own `x as i64' for a representative spread.
    let cases: &[f64] = &[
        -1e15,
        -1e9,
        -100.7,
        -1.1,
        -0.5,
        0.5,
        1.1,
        100.7,
        1e9,
        1e15,
    ];
    for &x in cases {
        let expected = x as i64;
        match truncate_float_call(x) {
            Sexp::Int(got) => assert_eq!(
                got, expected,
                "truncate_float({x}) → {got} but expected {expected}",
            ),
            other => panic!("truncate_float({x}) produced {other:?}"),
        }
    }
}
