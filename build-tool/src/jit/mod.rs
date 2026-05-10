//! Hot-path primitive trampolines for the nelisp-cc / JIT bridge.
//!
//! Phase 7.1.6 cluster takeover (Doc 28 §3.6 COMPLETE) replaced every
//! Cranelift IR builder with `#[no_mangle] extern "C"' Rust trampolines
//! resolved via the binary's dynamic symbol table.  Phase 7.1.7.b
//! (Doc 28 §3.7.c) then deleted `jit::dsl' and the `cranelift' /
//! `cranelift-jit' / `cranelift-module' Cargo deps.
//!
//! Lowered primitives are reachable via two paths:
//!
//!   1. `nl-jit-call-*' bridge primitives (re-exported from [`bridge`])
//!      that elisp wrappers in `lisp/nelisp-jit-strategy.el' invoke by
//!      name; `bridge::unified_fn_ptr' maps `nelisp_jit_*' names to the
//!      matching trampoline.
//!   2. dlsym-resolved `nl_jit_*' trampolines that nelisp-cc compiled
//!      code calls directly via the `nelisp-cc--dlsym-resolve' link
//!      pass (= the `-rdynamic' link flag in `.cargo/config.toml'
//!      pushes the `#[no_mangle]' symbols into the dynamic symbol
//!      table).

mod access;
mod arith;
mod box_accessor;
// `bridge': `nl-jit-call-*' primitives that elisp wrappers in
// `lisp/nelisp-jit-strategy.el' call to invoke JIT entries by name
// (= the 7 re-exports below, wired into `eval::builtins::dispatch'
// as the corresponding builtins).  See `bridge.rs' header for the
// 3 calling shapes (i64/i64, ptr/ptr, 7×i64).
pub(super) mod bridge;
pub(crate) use bridge::{
    bi_nl_jit_call_float_cmp, bi_nl_jit_call_float_float, bi_nl_jit_call_i64_i64,
    bi_nl_jit_call_out_1, bi_nl_jit_call_out_1i, bi_nl_jit_call_out_2,
    bi_nl_jit_call_out_2i, bi_nl_jit_call_ptr_ptr, bi_nl_jit_call_syscall,
};
mod cons;
mod float;
mod predicate;
mod syscall;
