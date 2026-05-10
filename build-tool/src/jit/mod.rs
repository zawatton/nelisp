//! Phase 5 — Cranelift JIT lowering for hot-path primitives.
//!
//! Doc 77b Stage b.5 (2026-05-09) — the original Stage 5.0
//! `lower_entries' registry has been removed; the eval loop
//! (`build-tool/src/eval/mod.rs::apply_builtin') now forwards every
//! call directly to `eval::builtins::dispatch'.  Lowered primitives
//! are reachable via two paths:
//!
//!   1. `nl-jit-call-*' bridge primitives (re-exported below from
//!      [`bridge`]) that elisp wrappers in
//!      `lisp/nelisp-jit-strategy.el' invoke by name; `unified_fn_ptr'
//!      maps `nelisp_jit_*' names to the matching `#[no_mangle]
//!      extern "C"' Rust trampoline (post-7.1.6.e: every cluster goes
//!      through this direct mapping).
//!   2. dlsym-resolved `nl_jit_*' trampolines (Phase 7.1.6.a.2+) that
//!      nelisp-cc compiled code calls directly via the
//!      `nelisp-cc--dlsym-resolve' link pass.
//!
//! Phase 7.1.6.a.2 deleted the cons cluster's Cranelift wrapper page
//! entirely; cons trampolines are now reached via path (2) only.
//! Phase 7.1.6.b deleted the access cluster on the same pattern.
//! Phase 7.1.6.c deleted the arith cluster — but unlike cons / access,
//! arith had no pre-existing Rust trampoline body, so the deletion was
//! paired with the introduction of 12 plain Rust `nl_jit_arith_*'
//! trampolines (see `jit::arith') that mirror the deleted Cranelift IR
//! semantics 1-to-1.
//!
//! Phase 7.1.6.d deleted the predicate cluster on the arith pattern —
//! the 7-block IR was consolidated into a single `nl_jit_predicate_eq'
//! trampoline (see `jit::predicate').
//!
//! Phase 7.1.6.e (this commit) deletes the syscall cluster — the FINAL
//! cluster.  syscall's Cranelift IR (= 2-instr `iconst + return' for
//! `supported_p' + single-`call' trampoline forwarding to `libc::
//! syscall') is replaced by two `#[no_mangle]' Rust trampolines
//! (`nl_jit_syscall_call' = the existing libc wrapper now directly
//! dlsym-exported; `nl_jit_syscall_supported_p' = 1-line const fn
//! mirroring the IR).  As a result the [`UnifiedJit`] struct + the
//! `unified_jit()' OnceLock + the 6-step orchestration are deleted
//! entirely — every cluster goes through `bridge::unified_fn_ptr's
//! direct match-arm name table.

// Phase 7.1.6.a.2 / 7.1.6.b / 7.1.6.c.arith / 7.1.6.d / 7.1.6.e
// (this commit) removed every caller of `declare_helper_call' (= the
// shared `(i64 × N) -> i64' helper-call IR builder).  All clusters
// now stand alone as `#[no_mangle] extern "C"' Rust functions
// resolved via dlsym + the `bridge::unified_fn_ptr' name table; no
// Cranelift wrapper page is constructed for any cluster anymore.
// `cranelift_jit' / `cranelift_module' uses in this module are
// therefore gone — the Cargo deps stay until Doc 28 §3.7 (Phase
// 7.1.7) sweeps them.

mod access;
mod arith;
// `bridge': `nl-jit-call-*' primitives that elisp wrappers in
// `lisp/nelisp-jit-strategy.el' call to invoke JIT entries by name
// (= the 7 re-exports below, wired into `eval::builtins::dispatch'
// as the corresponding builtins).  See `bridge.rs' header for the
// 3 calling shapes (i64/i64, ptr/ptr, 7×i64).
pub(super) mod bridge;
pub(crate) use bridge::{
    bi_nl_jit_call_i64_i64, bi_nl_jit_call_out_1, bi_nl_jit_call_out_1i,
    bi_nl_jit_call_out_2, bi_nl_jit_call_out_2i, bi_nl_jit_call_ptr_ptr,
    bi_nl_jit_call_syscall,
};
mod cons;
// `dsl': Stage b.1 JIT IR DSL interpreter (Doc 77b).  No production
// caller yet — Stage b.3 will switch `declare_X_inline' callers to
// use `dsl::build_rule' on elisp-authored Sexp rules.  Test-only
// coverage today, hence `#[allow(dead_code)]'.
#[allow(dead_code)]
mod dsl;
mod predicate;
// `strategy': Rust helper primitives (Stage b.4) backing the elisp
// wrappers in `lisp/nelisp-jit-strategy.el' — arith Float helpers,
// bitwise Int helpers, slim length / aref / aset fall-throughs, and
// the syscall-nr resolver.  See `strategy.rs' header for the surface.
mod strategy;
pub(crate) use strategy::{
    bi_add2_float, bi_ash_impl, bi_bool_vector_len, bi_char_table_aref,
    bi_char_table_aset, bi_int_eq_zero, bi_logand2_impl, bi_logior2_impl,
    bi_logxor2_impl, bi_mul2_float, bi_mut_str_len, bi_mut_str_set_codepoint,
    bi_num_eq2_float, bi_num_ge2_float, bi_num_gt2_float, bi_num_le2_float,
    bi_num_lt2_float, bi_str_codepoint_at, bi_sub2_float, bi_syscall_nr_resolve,
};
mod syscall;

// Phase 7.1.6.e (Doc 28 §3.6.e): `UnifiedJit' struct + `unified_jit()'
// OnceLock + the 6-step orchestration (= shared `JITBuilder' /
// `JITModule' / `register_symbols' / `declare_funcs' /
// `finalize_definitions' / `collect_funcs' / `Box::leak') deleted
// entirely.  After 7.1.6.e no cluster constructs a Cranelift wrapper
// page anymore — every `nelisp_jit_*' name in the bridge resolves
// directly to a `#[no_mangle] extern "C"' Rust trampoline via
// `bridge::unified_fn_ptr's match-arm table.  The dlsym path (used
// by nelisp-cc compiled hot paths) goes through the same trampolines
// from a different entry point (`-rdynamic' + `dlsym(RTLD_DEFAULT,
// ...)' on the binary's dynamic symbol table).
