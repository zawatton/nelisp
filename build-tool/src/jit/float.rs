//! Doc 84 §84.1 — Float-family xmm-register trampolines.
//! 8 `extern "C"' fns replacing the deleted `bi_*_float' helpers in
//! `jit/strategy.rs'.  System V AMD64 / arm64 AAPCS pass f64 in
//! xmm0/xmm1 (d0/d1) and return f64 in xmm0 (d0) or i64 in rax (x0).
//! Resolved by `nl-jit-call-float-{float,cmp}' bridges (see `bridge.rs').
//! Epsilon for `=` is hardcoded `1e-15' (matches `bi_num_eq2_float').
#[no_mangle] pub extern "C" fn nl_jit_float_add(a: f64, b: f64) -> f64 { a + b }
#[no_mangle] pub extern "C" fn nl_jit_float_sub(a: f64, b: f64) -> f64 { a - b }
#[no_mangle] pub extern "C" fn nl_jit_float_mul(a: f64, b: f64) -> f64 { a * b }
#[no_mangle] pub extern "C" fn nl_jit_float_eq_eps(a: f64, b: f64) -> i64 {
    if (a - b).abs() < 1e-15 { 1 } else { 0 }
}
#[no_mangle] pub extern "C" fn nl_jit_float_lt(a: f64, b: f64) -> i64 { (a < b) as i64 }
#[no_mangle] pub extern "C" fn nl_jit_float_gt(a: f64, b: f64) -> i64 { (a > b) as i64 }
#[no_mangle] pub extern "C" fn nl_jit_float_le(a: f64, b: f64) -> i64 { (a <= b) as i64 }
#[no_mangle] pub extern "C" fn nl_jit_float_ge(a: f64, b: f64) -> i64 { (a >= b) as i64 }
