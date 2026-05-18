//! Hot-path primitive trampolines for the nelisp-cc / JIT bridge.
//!
//! Lowered primitives are reachable via two paths:
//!   1. `nl-jit-call-*' bridge primitives re-exported from [`bridge`];
//!      elisp wrappers in `lisp/nelisp-jit-strategy.el' invoke by name
//!      and `bridge::unified_fn_ptr' maps `nelisp_jit_*' to the trampoline.
//!   2. dlsym-resolved `nl_jit_*' trampolines that nelisp-cc compiled
//!      code calls directly via `nelisp-cc--dlsym-resolve' (`-rdynamic'
//!      in `.cargo/config.toml' exposes the `#[no_mangle]' symbols).

mod access;
// `jit/arith.rs' deleted — 12 arith trampolines now live as Phase-47-
// compiled elisp `.o' in `libnelisp_elisp_spike.a'.  See
// `bridge.rs::arith_link' for the extern declarations.
mod box_accessor;
// `bridge': `nl-jit-call-*' primitives (3 calling shapes: i64/i64,
// ptr/ptr, 7×i64) — see `bridge.rs' header.
pub(super) mod bridge;
pub(crate) use bridge::{
    bi_nl_jit_call_float_cmp, bi_nl_jit_call_float_float, bi_nl_jit_call_float_unary,
    bi_nl_jit_call_format_float, bi_nl_jit_call_i64_i64, bi_nl_jit_call_out_1, bi_nl_jit_call_out_1i,
    bi_nl_jit_call_out_2, bi_nl_jit_call_out_2i, bi_nl_jit_call_ptr_ptr,
    bi_nl_jit_call_syscall,
};
mod cons;
// `jit/float.rs' (9 cmp/arith trampolines) and `jit/math.rs' (3 unary
// f64) deleted — both now live as Phase-47-compiled elisp `.o' in
// `libnelisp_elisp_spike.a'.  See `bridge.rs::{float,math}_link' for
// the extern declarations.
mod hash;
mod predicate;
mod regex;
mod strings;
mod syscall;
mod time;
