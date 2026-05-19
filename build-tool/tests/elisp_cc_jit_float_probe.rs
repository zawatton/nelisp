//! Doc 110 §110.E.2.a probe — exercises the 4 Phase-47-compiled
//! elisp `.o' replacements for `build-tool/src/jit/float.rs's
//! arithmetic trampolines (= add / sub / mul / div).
//!
//! When this test passes, every step of the §110.E.2.a pipeline
//! has worked on linux-x86_64:
//!
//!   1. `lisp/nelisp-cc-jit-float.el's 4 `defconst' source forms
//!      survived `nelisp-phase47-compile-to-object' through the
//!      f64 ABI introduced in §110.E.1 (= `(SYM :type f64)' param
//!      annotation + `(f64-add a b)' grammar + xmm0 / xmm1 ABI).
//!   2. `build.rs::link_elisp_cc_spike' bundled the 4 `.o' files
//!      into `libnelisp_elisp_spike.a' alongside the existing
//!      arith + truncate-int + fact-i64 + spike-noop entries.
//!   3. The linker resolved every `nl_jit_float_*' symbol in this
//!      file's `extern "C"' block against the archive's STT_FUNC
//!      entries (= the Rust `nl_jit_float_*' trampolines for add /
//!      sub / mul / div are cfg-gated away on linux-x86_64, so the
//!      elisp `.o' is the sole definer of those symbols).
//!   4. Each function's compiled body computed the same f64 result
//!      the deleted Rust trampoline used to compute via `a + b' /
//!      `a - b' / `a * b' / `a / b' under SSE2 ADDSD / SUBSD /
//!      MULSD / DIVSD.
//!
//! Comparison trampolines (eq_eps / lt / gt / le / ge) still go
//! through `super::float' on every target and are not exercised
//! here — they ship in §110.E.2.b after §110.C compiler
//! integration lands UCOMISD parse + SETcc emit.
//!
//! Non-linux / non-x86_64 builds skip the test because the elisp
//! `.o' files are not emitted (= `compile-elisp-objects.el'
//! `:requires-arch x86_64' gate) and the Rust trampolines are
//! still live on those targets.

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::{
    jit_float_add, jit_float_div, jit_float_eq_eps, jit_float_exp, jit_float_float, jit_float_ge,
    jit_float_gt, jit_float_le, jit_float_log, jit_float_lt, jit_float_mul, jit_float_sub,
};

// ---- ADDSD coverage ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_add_basics() {
    assert_eq!(jit_float_add(1.0, 2.0), 3.0);
    assert_eq!(jit_float_add(0.0, 0.0), 0.0);
    assert_eq!(jit_float_add(-1.5, 1.5), 0.0);
    // Bit-exact: f64(0.1 + 0.2) != 0.3 due to IEEE 754 rounding,
    // so the elisp ADDSD must match Rust's `+' exactly.
    assert_eq!(jit_float_add(0.1, 0.2), 0.1_f64 + 0.2_f64);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_add_extremes() {
    assert_eq!(jit_float_add(f64::INFINITY, 1.0), f64::INFINITY);
    assert_eq!(jit_float_add(f64::NEG_INFINITY, 1.0), f64::NEG_INFINITY);
    // +inf + -inf = NaN (IEEE 754)
    assert!(jit_float_add(f64::INFINITY, f64::NEG_INFINITY).is_nan());
}

// ---- SUBSD coverage ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_sub_basics() {
    assert_eq!(jit_float_sub(5.0, 3.0), 2.0);
    assert_eq!(jit_float_sub(0.0, 0.0), 0.0);
    assert_eq!(jit_float_sub(1.0, 1.0), 0.0);
    assert_eq!(jit_float_sub(-2.5, -1.5), -1.0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_sub_extremes() {
    assert_eq!(jit_float_sub(f64::INFINITY, 1.0), f64::INFINITY);
    assert!(jit_float_sub(f64::INFINITY, f64::INFINITY).is_nan());
}

// ---- MULSD coverage ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_mul_basics() {
    assert_eq!(jit_float_mul(2.0, 3.0), 6.0);
    assert_eq!(jit_float_mul(0.0, 5.0), 0.0);
    assert_eq!(jit_float_mul(1.0, -7.5), -7.5);
    assert_eq!(jit_float_mul(-3.0, -4.0), 12.0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_mul_extremes() {
    assert_eq!(jit_float_mul(f64::INFINITY, 2.0), f64::INFINITY);
    assert_eq!(jit_float_mul(f64::INFINITY, -1.0), f64::NEG_INFINITY);
    assert!(jit_float_mul(f64::INFINITY, 0.0).is_nan());
}

// ---- DIVSD coverage ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_div_basics() {
    assert_eq!(jit_float_div(6.0, 2.0), 3.0);
    assert_eq!(jit_float_div(1.0, 4.0), 0.25);
    assert_eq!(jit_float_div(-9.0, 3.0), -3.0);
    assert_eq!(jit_float_div(0.0, 5.0), 0.0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_div_by_zero() {
    // IEEE 754: x/0 = ±inf when x != 0; 0/0 = NaN.  No trap.
    assert_eq!(jit_float_div(1.0, 0.0), f64::INFINITY);
    assert_eq!(jit_float_div(-1.0, 0.0), f64::NEG_INFINITY);
    assert!(jit_float_div(0.0, 0.0).is_nan());
}

// ---- Cross-op identity: a + b - b == a (within rounding) ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_round_trip_identity() {
    // `(a + b) - b' should equal `a' for small magnitudes (= no
    // cancellation).  Verifies add and sub agree on the same xmm
    // ABI direction.
    let a = 3.14;
    let b = 2.71;
    let sum = jit_float_add(a, b);
    let back = jit_float_sub(sum, b);
    // Allow ULP-level wobble (= same as Rust's a + b - b).
    assert_eq!(back, a + b - b);
}

// ---- NaN propagation ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_nan_propagates() {
    let nan = f64::NAN;
    assert!(jit_float_add(nan, 1.0).is_nan());
    assert!(jit_float_sub(nan, 1.0).is_nan());
    assert!(jit_float_mul(nan, 1.0).is_nan());
    assert!(jit_float_div(nan, 1.0).is_nan());
}

// ---- §110.C.2.a — ordered comparison coverage ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_lt_basics() {
    assert_eq!(jit_float_lt(1.0, 2.0), 1);
    assert_eq!(jit_float_lt(2.0, 1.0), 0);
    assert_eq!(jit_float_lt(1.0, 1.0), 0); // equal → not less
    assert_eq!(jit_float_lt(-1.0, 0.0), 1);
    assert_eq!(jit_float_lt(0.0, -0.0), 0); // ±0 compare equal
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_gt_basics() {
    assert_eq!(jit_float_gt(2.0, 1.0), 1);
    assert_eq!(jit_float_gt(1.0, 2.0), 0);
    assert_eq!(jit_float_gt(1.0, 1.0), 0);
    assert_eq!(jit_float_gt(0.0, -1.0), 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_le_basics() {
    assert_eq!(jit_float_le(1.0, 2.0), 1);
    assert_eq!(jit_float_le(1.0, 1.0), 1); // equal → yes
    assert_eq!(jit_float_le(2.0, 1.0), 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_ge_basics() {
    assert_eq!(jit_float_ge(2.0, 1.0), 1);
    assert_eq!(jit_float_ge(1.0, 1.0), 1);
    assert_eq!(jit_float_ge(1.0, 2.0), 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_cmp_infinities() {
    // +inf comparisons
    assert_eq!(jit_float_lt(1.0, f64::INFINITY), 1);
    assert_eq!(jit_float_gt(f64::INFINITY, 1.0), 1);
    assert_eq!(jit_float_le(f64::INFINITY, f64::INFINITY), 1);
    assert_eq!(jit_float_ge(f64::INFINITY, f64::INFINITY), 1);
    // -inf comparisons
    assert_eq!(jit_float_lt(f64::NEG_INFINITY, 0.0), 1);
    assert_eq!(jit_float_gt(0.0, f64::NEG_INFINITY), 1);
    // mixed inf
    assert_eq!(jit_float_lt(f64::NEG_INFINITY, f64::INFINITY), 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_cmp_nan_is_false() {
    // The headline f64-cmp invariant: every comparison involving
    // NaN returns 0 to match Rust `<' / `>' / `<=' / `>=' which
    // are all defined as false for NaN inputs.  The
    // UCOMISD-operand-swap + SETA/SETAE encoding makes this fall
    // out naturally (= no AND-with-SETNP needed).
    let nan = f64::NAN;
    for v in [1.0_f64, 0.0, -1.0, f64::INFINITY, f64::NEG_INFINITY] {
        assert_eq!(jit_float_lt(nan, v), 0, "lt(NaN, {})", v);
        assert_eq!(jit_float_lt(v, nan), 0, "lt({}, NaN)", v);
        assert_eq!(jit_float_gt(nan, v), 0, "gt(NaN, {})", v);
        assert_eq!(jit_float_gt(v, nan), 0, "gt({}, NaN)", v);
        assert_eq!(jit_float_le(nan, v), 0, "le(NaN, {})", v);
        assert_eq!(jit_float_le(v, nan), 0, "le({}, NaN)", v);
        assert_eq!(jit_float_ge(nan, v), 0, "ge(NaN, {})", v);
        assert_eq!(jit_float_ge(v, nan), 0, "ge({}, NaN)", v);
    }
    // NaN vs NaN — also all false
    assert_eq!(jit_float_lt(nan, nan), 0);
    assert_eq!(jit_float_gt(nan, nan), 0);
    assert_eq!(jit_float_le(nan, nan), 0);
    assert_eq!(jit_float_ge(nan, nan), 0);
}

// ---- §110.C.2.b — EQ-EPS coverage ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_eq_eps_within_tolerance() {
    // Equal values → within 1e-15
    assert_eq!(jit_float_eq_eps(0.0, 0.0), 1);
    assert_eq!(jit_float_eq_eps(1.0, 1.0), 1);
    assert_eq!(jit_float_eq_eps(-3.14, -3.14), 1);
    // Just below tolerance
    assert_eq!(jit_float_eq_eps(1.0, 1.0 + 0.5e-15), 1);
    assert_eq!(jit_float_eq_eps(1.0 + 0.5e-15, 1.0), 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_eq_eps_outside_tolerance() {
    assert_eq!(jit_float_eq_eps(1.0, 2.0), 0);
    assert_eq!(jit_float_eq_eps(1.0, 1.0 + 2e-15), 0);
    assert_eq!(jit_float_eq_eps(1.0 + 2e-15, 1.0), 0);
    assert_eq!(jit_float_eq_eps(1e10, 1e10 + 1.0), 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_eq_eps_nan_is_false() {
    let nan = f64::NAN;
    for v in [1.0_f64, 0.0, -1.0, f64::INFINITY, f64::NEG_INFINITY] {
        assert_eq!(jit_float_eq_eps(nan, v), 0, "eq-eps(NaN, {})", v);
        assert_eq!(jit_float_eq_eps(v, nan), 0, "eq-eps({}, NaN)", v);
    }
    assert_eq!(jit_float_eq_eps(nan, nan), 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_eq_eps_infinity_handling() {
    // +inf vs +inf: a - b = NaN (inf - inf), abs(NaN) = NaN, NaN
    // comparisons unordered → return 0 per Rust `(NaN OP x) =
    // false'.  Matches Rust's `(a - b).abs() < 1e-15' which
    // returns false for inf vs inf (since the abs is NaN).
    assert_eq!(jit_float_eq_eps(f64::INFINITY, f64::INFINITY), 0);
    // +inf vs -inf: a - b = +inf, abs = +inf, +inf < 1e-15 is false
    assert_eq!(jit_float_eq_eps(f64::INFINITY, f64::NEG_INFINITY), 0);
    // +inf vs finite: a - b = +inf, abs = +inf, < 1e-15 false
    assert_eq!(jit_float_eq_eps(f64::INFINITY, 0.0), 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_eq_eps_matches_rust() {
    // Bit-exact agreement with Rust's `(a - b).abs() < 1e-15'.
    // The elisp body hardcodes the f64 bit pattern for 1e-15 (=
    // 0x3CD203AF9EE75616) so any drift between the elisp constant
    // and Rust's compile-time literal surfaces immediately.
    let samples: [(f64, f64); 9] = [
        (0.0, 0.0),
        (-0.0, 0.0),
        (1e-16, 0.0), // below eps → eq
        (1e-14, 0.0), // above eps → ne
        (1.0, 1.0),
        (1.0, 1.0 + 1e-16),
        (1.0, 1.0 + 1e-14),
        (-3.14, -3.14),
        (1e15, 1e15),
    ];
    for (a, b) in samples {
        let want = if (a - b).abs() < 1e-15 { 1 } else { 0 };
        assert_eq!(
            jit_float_eq_eps(a, b),
            want,
            "eq-eps({}, {}) — Rust expected {}",
            a,
            b,
            want
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_cmp_matches_rust() {
    // Crucial bit-exact agreement with Rust's `<' / `>' / `<=' /
    // `>=' across a sample grid.  Confirms the elisp emit doesn't
    // diverge on edge cases like tiny denormals or boundary
    // ordered pairs.
    let samples = [
        (0.0, 0.0),
        (-0.0, 0.0),
        (1e-308, 0.0), // smallest normal vs 0
        (1.0, 1.0 + f64::EPSILON),
        (1e15, 1e15 + 1.0),
        (-1.0, 1.0),
    ];
    for (a, b) in samples {
        assert_eq!(jit_float_lt(a, b), (a < b) as i64, "lt({}, {})", a, b);
        assert_eq!(jit_float_gt(a, b), (a > b) as i64, "gt({}, {})", a, b);
        assert_eq!(jit_float_le(a, b), (a <= b) as i64, "le({}, {})", a, b);
        assert_eq!(jit_float_ge(a, b), (a >= b) as i64, "ge({}, {})", a, b);
    }
}

// ---- §110.F — `jit/math.rs' replacements (identity, exp, log) ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_float_identity() {
    assert_eq!(jit_float_float(0.0), 0.0);
    assert_eq!(jit_float_float(1.5), 1.5);
    assert_eq!(jit_float_float(-3.14), -3.14);
    assert_eq!(jit_float_float(f64::INFINITY), f64::INFINITY);
    assert!(jit_float_float(f64::NAN).is_nan());
    // Bit-exact preservation including denormals + signed zero
    assert_eq!(jit_float_float(1e-308), 1e-308);
    assert_eq!(jit_float_float(0.0_f64).to_bits(), 0_u64);
    assert_eq!(jit_float_float(-0.0_f64).to_bits(), (-0.0_f64).to_bits());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_exp_basics() {
    // exp(0) = 1
    assert_eq!(jit_float_exp(0.0), 1.0);
    // exp(1) ≈ e (= 2.718...)
    assert!((jit_float_exp(1.0) - std::f64::consts::E).abs() < 1e-12);
    // Bit-exact against Rust's own libm-equivalent
    assert_eq!(jit_float_exp(2.0), (2.0_f64).exp());
    assert_eq!(jit_float_exp(-1.0), (-1.0_f64).exp());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_exp_extremes() {
    // exp(+inf) = +inf, exp(-inf) = 0
    assert_eq!(jit_float_exp(f64::INFINITY), f64::INFINITY);
    assert_eq!(jit_float_exp(f64::NEG_INFINITY), 0.0);
    // exp(NaN) = NaN
    assert!(jit_float_exp(f64::NAN).is_nan());
    // exp(very-large) = +inf (= overflow to inf per IEEE 754)
    assert_eq!(jit_float_exp(1000.0), f64::INFINITY);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_log_basics() {
    // log(1) = 0
    assert_eq!(jit_float_log(1.0), 0.0);
    // log(e) = 1
    assert!((jit_float_log(std::f64::consts::E) - 1.0).abs() < 1e-12);
    // Bit-exact agreement
    assert_eq!(jit_float_log(2.0), (2.0_f64).ln());
    assert_eq!(jit_float_log(10.0), (10.0_f64).ln());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_log_extremes() {
    // log(0) = -inf
    assert_eq!(jit_float_log(0.0), f64::NEG_INFINITY);
    // log(negative) = NaN
    assert!(jit_float_log(-1.0).is_nan());
    // log(+inf) = +inf
    assert_eq!(jit_float_log(f64::INFINITY), f64::INFINITY);
    // log(NaN) = NaN
    assert!(jit_float_log(f64::NAN).is_nan());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn float_exp_log_round_trip() {
    // log(exp(x)) ≈ x for ordinary magnitudes
    let samples: [f64; 5] = [0.0, 1.0, -1.0, 3.14, -2.71];
    for x in samples {
        let round_trip = jit_float_log(jit_float_exp(x));
        assert!(
            (round_trip - x).abs() < 1e-10,
            "log(exp({})) = {}",
            x,
            round_trip
        );
    }
}
