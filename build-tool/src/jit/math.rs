//! Doc 87 §86.1.f / §5 — Unary float math trampolines.
//! 3 `extern "C" fn(f64) -> f64' trampolines covering the `float' /
//! `exp' / `log' (= 1-arg base defaulted to `e') primitives migrated
//! from `bi_*' helpers in `eval/builtins.rs'.  System V AMD64 / arm64
//! AAPCS pass the f64 arg in xmm0 (d0) and return f64 in xmm0 (d0);
//! resolved by the new `nl-jit-call-float-unary' bridge primitive
//! (see `bridge.rs' for the dispatch fn).
//!
//! The 2-arg form of `log' (= `(log X BASE)') is handled in elisp on
//! top of the 1-arg primitive — `(log X BASE) = (/ (log X) (log BASE))'.
//! Keeping the ABI mode at exactly `fn(f64) -> f64' makes the encoder
//! footprint zero (= int-class register pushes don't disturb xmm0/v0,
//! so the standard prologue / epilogue carries the f64 arg/result
//! through untouched).

#[no_mangle]
pub extern "C" fn nl_jit_float_float(x: f64) -> f64 {
    x
}

#[no_mangle]
pub extern "C" fn nl_jit_float_exp(x: f64) -> f64 {
    x.exp()
}

#[no_mangle]
pub extern "C" fn nl_jit_float_log(x: f64) -> f64 {
    x.ln()
}
