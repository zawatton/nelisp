//! Doc 122 §122.G probes — direct calls into the Rust externs +
//! the Phase 47-compiled `sexp-write-float' grammar op for the
//! Float allocator pair (`nl_sexp_write_float' /
//! `nl_str_to_float').  Verifies end-to-end round-trips:
//!
//!   - `nl_sexp_write_float' writes `Sexp::Float(val)' inline.
//!   - `nl_str_to_float' parses byte ranges into f64 + writes
//!     `Sexp::Float(_)' / `Sexp::Nil' on success / failure.
//!   - The elisp `sexp-write-float' Phase 47 grammar op compiles
//!     and links against `nl_sexp_write_float' (= driven via the
//!     `nelisp_sexp_write_float' probe defun which takes both args
//!     as f64-class params and bit-casts the slot pointer through
//!     xmm0).
//!
//! Substrate role: §122.G is the Reader Float unlock (Doc 116.B+
//! extension).  After this stage, the §116.B elisp parser handles
//! kind 21 Float tokens directly via the `nl_str_to_float' extern
//! — no more Rust fallback for `1.5' / `1e3' inputs.  Future Doc
//! 124.E NlStr Drop + Doc 120.E `format_float' swap also reuse
//! the `nl_sexp_write_float' lowering.
//!
//! Test cases:
//!   1. Write 0.0 — boundary, simplest f64.
//!   2. Write 1.5 — typical Reader input.
//!   3. Write NaN — bit-pattern round-trip check.
//!   4. Write -0.0 — sign-bit preservation.
//!   5. `nl_str_to_float' parse "3.14" success.
//!   6. `nl_str_to_float' parse "not-a-number" failure.
//!   7. Grammar op end-to-end via `nelisp_sexp_write_float'.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::elisp_cc_spike;
use nelisp_build_tool::eval::sexp::Sexp;

// ---- helpers ----

fn write_float_extern(val: f64) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe { elisp_cc_spike::sexp_write_float_extern(slot_ptr, val) };
    assert_eq!(
        returned, slot_ptr,
        "nl_sexp_write_float must return the caller-provided slot pointer"
    );
    slot
}

fn write_float_via_grammar(val: f64) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe { elisp_cc_spike::sexp_write_float_via_grammar(slot_ptr, val) };
    assert_eq!(
        returned, slot_ptr,
        "sexp-write-float grammar op must return the caller-provided slot pointer"
    );
    slot
}

fn parse_float(bytes: &[u8]) -> (i64, Sexp) {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let status =
        unsafe { elisp_cc_spike::str_to_float(bytes.as_ptr(), bytes.len() as i64, slot_ptr) };
    (status, slot)
}

// ---- Case 1: write 0.0 ----

#[test]
fn sexp_write_float_zero_yields_float_variant() {
    let s = write_float_extern(0.0);
    match s {
        Sexp::Float(f) => {
            assert_eq!(f, 0.0);
            assert_eq!(f.to_bits(), 0u64, "0.0 has all-zero bit pattern");
        }
        other => panic!("expected Sexp::Float(0.0), got {:?}", other),
    }
}

// ---- Case 2: write 1.5 ----

#[test]
fn sexp_write_float_one_point_five_round_trips() {
    let s = write_float_extern(1.5);
    match s {
        Sexp::Float(f) => assert_eq!(f, 1.5),
        other => panic!("expected Sexp::Float(1.5), got {:?}", other),
    }
}

// ---- Case 3: write NaN ----

#[test]
fn sexp_write_float_nan_preserves_bit_pattern() {
    // NaN comparison is always false; check the bit pattern.
    let nan = f64::NAN;
    let s = write_float_extern(nan);
    match s {
        Sexp::Float(f) => {
            assert!(f.is_nan(), "NaN round-trip must produce a NaN");
            // The canonical NaN bit pattern survives the std::ptr::write
            // byte copy unchanged.
            assert_eq!(f.to_bits(), nan.to_bits());
        }
        other => panic!("expected Sexp::Float(NaN), got {:?}", other),
    }
}

// ---- Case 4: write -0.0 (sign-bit preservation) ----

#[test]
fn sexp_write_float_negative_zero_keeps_sign_bit() {
    let neg_zero = -0.0_f64;
    let s = write_float_extern(neg_zero);
    match s {
        Sexp::Float(f) => {
            // `f == 0.0' is true for both +0 and -0; check sign bit.
            assert_eq!(f, 0.0);
            assert!(f.is_sign_negative(), "-0.0 must keep its sign bit");
            assert_eq!(f.to_bits(), neg_zero.to_bits());
        }
        other => panic!("expected Sexp::Float(-0.0), got {:?}", other),
    }
}

// ---- Case 5: parse "3.14" → success ----

#[test]
fn nl_str_to_float_parses_decimal() {
    let (status, slot) = parse_float(b"3.14");
    assert_eq!(status, 1, "valid decimal must return success");
    match slot {
        Sexp::Float(f) => assert_eq!(f, 3.14),
        other => panic!("expected Sexp::Float(3.14), got {:?}", other),
    }
}

#[test]
fn nl_str_to_float_parses_exponent() {
    // Confirms the Reader's `1e3' input path round-trips.
    let (status, slot) = parse_float(b"1e3");
    assert_eq!(status, 1);
    match slot {
        Sexp::Float(f) => assert_eq!(f, 1000.0),
        other => panic!("expected Sexp::Float(1000.0), got {:?}", other),
    }
}

#[test]
fn nl_str_to_float_parses_negative_with_exponent() {
    // Combined sign + decimal + signed exponent — exact mirror of
    // the Reader test `-1.5e-2'.
    let (status, slot) = parse_float(b"-1.5e-2");
    assert_eq!(status, 1);
    match slot {
        Sexp::Float(f) => assert_eq!(f, -0.015),
        other => panic!("expected Sexp::Float(-0.015), got {:?}", other),
    }
}

// ---- Case 6: parse "not-a-number" → failure ----

#[test]
fn nl_str_to_float_rejects_garbage() {
    let (status, slot) = parse_float(b"not-a-number");
    assert_eq!(status, 0, "garbage input must return failure");
    assert_eq!(slot, Sexp::Nil, "failure path must leave slot as Sexp::Nil");
}

#[test]
fn nl_str_to_float_rejects_empty() {
    // Empty range is a parse error per `str::parse::<f64>'.
    let (status, slot) = parse_float(b"");
    assert_eq!(status, 0);
    assert_eq!(slot, Sexp::Nil);
}

// ---- Case 7: grammar op end-to-end ----

#[test]
fn sexp_write_float_grammar_op_round_trips() {
    // Drives the Phase 47-compiled `(sexp-write-float SLOT VAL)' op
    // via the `nelisp_sexp_write_float' probe defun.  Slot pointer
    // is bit-cast through xmm0 (= §122.G MVP workaround for the
    // uniform-class defun param restriction); val is the f64 payload.
    let s = write_float_via_grammar(2.5);
    match s {
        Sexp::Float(f) => assert_eq!(f, 2.5),
        other => panic!("grammar op expected Sexp::Float(2.5), got {:?}", other),
    }
}

#[test]
fn sexp_write_float_grammar_op_zero() {
    let s = write_float_via_grammar(0.0);
    match s {
        Sexp::Float(f) => assert_eq!(f, 0.0),
        other => panic!("grammar op expected Sexp::Float(0.0), got {:?}", other),
    }
}

#[test]
fn sexp_write_float_grammar_op_negative() {
    let s = write_float_via_grammar(-3.14);
    match s {
        Sexp::Float(f) => assert_eq!(f, -3.14),
        other => panic!("grammar op expected Sexp::Float(-3.14), got {:?}", other),
    }
}
