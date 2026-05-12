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
    jit_float_add, jit_float_div, jit_float_mul, jit_float_sub,
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
