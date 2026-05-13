//! Doc 84 §84.1 — Float-family xmm-register trampolines.
//! 8 `extern "C"' fns replacing the deleted `bi_*_float' helpers in
//! `jit/strategy.rs'.  System V AMD64 / arm64 AAPCS pass f64 in
//! xmm0/xmm1 (d0/d1) and return f64 in xmm0 (d0) or i64 in rax (x0).
//! Resolved by `nl-jit-call-float-{float,cmp}' bridges (see `bridge.rs').
//! Epsilon for `=` is hardcoded `1e-15' (matches `bi_num_eq2_float').
//!
//! Doc 110 §110.E.2.a (2026-05-13) — add / sub / mul / div bodies
//! cfg-gated to `not(linux-x86_64)' because the linux-x86_64 build
//! pulls those symbols from Phase-47-compiled elisp `.o' files (=
//! `lisp/nelisp-cc-jit-float.el' + the static archive built by
//! `build.rs::link_elisp_cc_spike').  On every other supported
//! target (= linux-aarch64, macos-aarch64) the Rust trampolines
//! stay live until §110.D ships aarch64 f64 emit.  Defining both
//! would yield a duplicate `nl_jit_float_*' symbol at link time.
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_add(a: f64, b: f64) -> f64 { a + b }
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_sub(a: f64, b: f64) -> f64 { a - b }
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_mul(a: f64, b: f64) -> f64 { a * b }
// Doc 86 §86.1.b (2026-05-10) — `/' migrated to elisp via this binary
// trampoline.  Division-by-zero check stays in elisp (= the bridge's
// `num_pair' Float promotion is what enforces wrong-type already).
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_div(a: f64, b: f64) -> f64 { a / b }
// Doc 110 §110.C.2.b (2026-05-13) — EQ-EPS shipped to elisp on
// linux-x86_64 via the inline-imm64 + MOVQ + ANDPD path (=
// avoids the rodata section growth `.rodata' would require).
// Other supported targets keep the Rust impl until §110.D ships
// aarch64 f64 emit.  Defining both would yield a duplicate
// `nl_jit_float_eq_eps' symbol at link time.
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_eq_eps(a: f64, b: f64) -> i64 {
    if (a - b).abs() < 1e-15 { 1 } else { 0 }
}
// Doc 110 §110.C.2.a (2026-05-13) — 4 ordered compares cfg-gated
// to `not(linux-x86_64)' for the same reason as add / sub / mul /
// div: linux-x86_64 builds resolve these symbols via the elisp
// `.o' files (`lisp/nelisp-cc-jit-float.el's lt / gt / le / ge
// defconsts).  Other supported targets (= linux-aarch64,
// macos-aarch64) keep the Rust impl until §110.D ships aarch64
// f64 emit.
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_lt(a: f64, b: f64) -> i64 { (a < b) as i64 }
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_gt(a: f64, b: f64) -> i64 { (a > b) as i64 }
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_le(a: f64, b: f64) -> i64 { (a <= b) as i64 }
#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle] pub extern "C" fn nl_jit_float_ge(a: f64, b: f64) -> i64 { (a >= b) as i64 }
