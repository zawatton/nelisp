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
// Doc 100 §100.D Stage 2/3 — the 12 `nl_jit_arith_*' trampolines have
// been wholly replaced by Phase-47-compiled elisp `.o' files emitted
// by `lisp/nelisp-cc-jit-arith.el' + `lisp/nelisp-phase47-compiler.el's
// x86_64 + aarch64 emit chains + `lisp/nelisp-elf-write.el' (Linux) /
// `lisp/nelisp-mach-o-write.el' (macOS).  The Rust `jit/arith.rs'
// module is therefore deleted — see `bridge.rs::arith_link' for the
// extern declarations the linker now resolves against the elisp `.o'
// static archive (= `libnelisp_elisp_spike.a').
mod box_accessor;
// `bridge': `nl-jit-call-*' primitives that elisp wrappers in
// `lisp/nelisp-jit-strategy.el' call to invoke JIT entries by name
// (= the 7 re-exports below, wired into `eval::builtins::dispatch'
// as the corresponding builtins).  See `bridge.rs' header for the
// 3 calling shapes (i64/i64, ptr/ptr, 7×i64).
pub(super) mod bridge;
pub(crate) use bridge::{
    bi_nl_jit_call_float_cmp, bi_nl_jit_call_float_float, bi_nl_jit_call_float_unary,
    bi_nl_jit_call_format_float, bi_nl_jit_call_i64_i64, bi_nl_jit_call_out_1, bi_nl_jit_call_out_1i,
    bi_nl_jit_call_out_2, bi_nl_jit_call_out_2i, bi_nl_jit_call_ptr_ptr,
    bi_nl_jit_call_syscall,
};
mod cons;
// Doc 110 §110.E.2.b (2026-05-13) — `jit/float.rs's 9 trampolines
// (add / sub / mul / div / eq-eps / lt / gt / le / ge) wholly
// replaced by Phase-47-compiled elisp `.o' files emitted from
// `lisp/nelisp-cc-jit-float.el' on all 3 supported targets
// (linux-x86_64 / linux-aarch64 / macos-aarch64).  `mod float;'
// is therefore deleted — see `bridge.rs::float_link' for the
// extern declarations the linker resolves against the elisp `.o'
// static archive.
// Doc 87 §86.1.f (2026-05-10): hash / math / regex / time trampolines.
// Doc 110 §110.F (2026-05-13) — `jit/math.rs's 3 unary f64
// trampolines (float / exp / log) wholly replaced by Phase-47-
// compiled elisp `.o' files emitted from `lisp/nelisp-cc-jit-
// math.el'.  `mod math;' is therefore deleted — see
// `bridge.rs::math_link' for the extern declarations the linker
// resolves against the elisp `.o' static archive.
mod hash;
mod predicate;
mod regex;
// Doc 86 §86.1.d (2026-05-10): intern / symbol-name / make-symbol.
// Doc 87 §86.1.f (2026-05-10): downcase / upcase / split-by-non-alnum
// trampolines appended.
mod strings;
mod syscall;
mod time;
