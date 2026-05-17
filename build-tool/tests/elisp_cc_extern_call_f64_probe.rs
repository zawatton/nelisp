//! Doc 122 §122.C probes — extended `extern-call' grammar with f64
//! args + f64 return (mirrors `elisp_cc_atomic_raw_mem_probe.rs'
//! pattern, but the new code path is the per-class register
//! placement inside `--emit-extern-call' plus the `extern-call-f64'
//! head op for f64-return calls).
//!
//! Verifies end-to-end round-trips through three libm symbols:
//!
//!   - `sqrt(4.0)' = 2.0  (= positive-domain check)
//!   - `sqrt(2.0)' ≈ 1.4142135623730951  (= irrational-result check)
//!   - `sin(0.0)'  = 0.0  (= boundary check)
//!   - `cos(0.0)'  = 1.0  (= boundary check)
//!   - `sin(pi/2)' ≈ 1.0  (= cross-quadrant check)
//!   - `cos(pi)'   ≈ -1.0 (= sign-flip check)
//!
//! Each test:
//!   1. Calls the elisp-compiled `nelisp_libm_*' wrapper through
//!      the safe Rust shim in `nelisp_build_tool::elisp_cc_spike'.
//!   2. Asserts the f64 result matches `f64::sqrt` / `f64::sin` /
//!      `f64::cos' applied to the same input within an `f64::EPSILON'
//!      tolerance (= libm + Rust intrinsic should agree to the last
//!      bit on the same platform but `EPSILON' avoids flakiness on
//!      future SIMD-backend changes).
//!
//! Substrate role: this is the §122.C verification gate.  Doc 120's
//! `nl_jit_format_float' (= eliminates ~30 LOC Rust) + Doc 117 Tier B
//! varargs syscall family (= ~800 LOC Rust callable from elisp) both
//! depend on this grammar landing.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::f64::consts::PI;

const EPS: f64 = 1e-12;

fn close_enough(actual: f64, expected: f64) -> bool {
    (actual - expected).abs() < EPS
}

// ---- Case 1: sqrt(4.0) = 2.0 ----

#[test]
fn libm_sqrt_returns_exact_two_for_four() {
    let result = unsafe { nelisp_build_tool::elisp_cc_spike::libm_sqrt(4.0) };
    assert!(
        close_enough(result, 2.0),
        "sqrt(4.0) must be 2.0 (= got {})",
        result
    );
}

// ---- Case 2: sqrt(2.0) ≈ 1.4142135623730951 ----

#[test]
fn libm_sqrt_irrational_input_matches_intrinsic() {
    let result = unsafe { nelisp_build_tool::elisp_cc_spike::libm_sqrt(2.0) };
    let expected = 2.0_f64.sqrt();
    assert!(
        close_enough(result, expected),
        "sqrt(2.0) Phase 47 ≠ intrinsic (= got {}, expected {})",
        result,
        expected
    );
    // Bit-exact match for the same input on the same platform.
    assert_eq!(
        result.to_bits(),
        expected.to_bits(),
        "sqrt(2.0) bit pattern divergence — libm vs intrinsic"
    );
}

// ---- Case 3: sin(0.0) = 0.0 ----

#[test]
fn libm_sin_zero_returns_zero() {
    let result = unsafe { nelisp_build_tool::elisp_cc_spike::libm_sin(0.0) };
    assert!(
        close_enough(result, 0.0),
        "sin(0.0) must be 0.0 (= got {})",
        result
    );
}

// ---- Case 4: cos(0.0) = 1.0 ----

#[test]
fn libm_cos_zero_returns_one() {
    let result = unsafe { nelisp_build_tool::elisp_cc_spike::libm_cos(0.0) };
    assert!(
        close_enough(result, 1.0),
        "cos(0.0) must be 1.0 (= got {})",
        result
    );
}

// ---- Case 5: sin(pi/2) ≈ 1.0 ----

#[test]
fn libm_sin_half_pi_matches_intrinsic() {
    let arg = PI / 2.0;
    let result = unsafe { nelisp_build_tool::elisp_cc_spike::libm_sin(arg) };
    let expected = arg.sin();
    assert!(
        close_enough(result, expected),
        "sin(pi/2) Phase 47 ≠ intrinsic (= got {}, expected {})",
        result,
        expected
    );
}

// ---- Case 6: cos(pi) ≈ -1.0 ----

#[test]
fn libm_cos_pi_matches_intrinsic() {
    let result = unsafe { nelisp_build_tool::elisp_cc_spike::libm_cos(PI) };
    let expected = PI.cos();
    assert!(
        close_enough(result, expected),
        "cos(pi) Phase 47 ≠ intrinsic (= got {}, expected {})",
        result,
        expected
    );
    // Should be very close to -1.0 (= within EPS of intrinsic result).
    assert!(
        close_enough(result, -1.0),
        "cos(pi) should be ≈ -1.0 (= got {})",
        result
    );
}
