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
//!      maps `nelisp_jit_*' names to the matching field in
//!      [`UnifiedJit`].
//!   2. dlsym-resolved `nl_jit_*' trampolines (Phase 7.1.6.a.2) that
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
//! Phase 7.1.6.d (this commit) deletes the predicate cluster.
//! Predicate's Cranelift IR (= 7-block same-ref / tag-eq / int-payload
//! fast paths + `sexp_eq' slow path) had only a partial Rust helper
//! (`nl_jit_pred_eq') covering the slow arm, so the deletion is paired
//! with the introduction of a single consolidated `nl_jit_predicate_eq'
//! trampoline (see `jit::predicate') that mirrors all 7 IR blocks
//! 1-to-1 in plain Rust.  As a result [`UnifiedJit`] now holds 1
//! sub-module (syscall) instead of the original 5.

use std::sync::OnceLock;

use cranelift_jit::{JITBuilder, JITModule};

// Phase 7.1.6.a.2 / 7.1.6.b / 7.1.6.c / 7.1.6.d removed all callers of
// `declare_helper_call' (= the shared `(i64 × N) -> i64' helper-call
// IR builder).  Cons / access / arith / predicate trampolines now
// stand alone as `#[no_mangle] extern "C"' Rust functions resolved via
// dlsym + the `bridge::unified_fn_ptr' name table; no Cranelift wrapper
// page is constructed for them anymore.  The remaining `syscall'
// cluster uses a bespoke IR shape that doesn't need this helper either.

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

/// Doc 77 Stage 2-prep (2026-05-09) — unified JITModule.
///
/// Pre-Doc-77 each submodule owned its own `OnceLock<JitX>' which
/// built a separate `JITModule' on first access (= one `mmap' page +
/// independent symbol namespace per submodule, blocking the "JIT fn
/// calls JIT fn" pattern).  Stage 2-prep consolidates the submodules
/// into one shared `JITBuilder' / `JITModule' built at first
/// `unified_jit()' access:
///
/// 1. Each submodule registers its imported helper symbols on a
///    *shared* `JITBuilder' via `register_symbols(&mut JITBuilder)'.
/// 2. `JITModule::new(builder)' once.
/// 3. Each submodule declares + defines its functions on the *shared*
///    JITModule via `declare_funcs(&mut JITModule) -> XxxIds'.
/// 4. `module.finalize_definitions()' once (= one mmap page bring-up).
/// 5. Each submodule fetches its function pointers via
///    `collect_funcs(&JITModule, ids) -> JitX'.
/// 6. `Box::leak(module)' once to keep executable pages alive.
///
/// Phase 7.1.6.a.2 (2026-05-10) reduced the cluster count from 5 to
/// 4 by deleting the cons Cranelift wrapper page; the 5
/// `nl_jit_cons_*' trampolines now live in `jit::cons' as
/// `#[no_mangle] extern "C"' symbols resolved either via the dlsym
/// bridge (nelisp-cc compiled code) or via `bridge::unified_fn_ptr'
/// (substrate.el bootstrap), bypassing `UnifiedJit' entirely.
/// Phase 7.1.6.b / .c / .d each chipped one more cluster (= access /
/// arith / predicate); only `syscall' remains in [`UnifiedJit`].
pub(super) struct UnifiedJit {
    // Phase 7.1.6.a.2 (Doc 28 §3.6.a): cons cluster JIT wrappers
    // deleted (= `JitCons' / `register_symbols' / `declare_funcs' /
    // `collect_funcs' all gone).  The 5 `nl_jit_cons_*' trampolines
    // stay in `jit::cons' as `#[no_mangle] extern "C"' symbols
    // resolved by the dlsym bridge for nelisp-cc compiled hot paths,
    // and by `bridge::unified_fn_ptr' for substrate.el bootstrap
    // paths (= same trampoline body, no Cranelift wrapper).
    //
    // Phase 7.1.6.b (Doc 28 §3.6.b): access cluster JIT wrappers
    // deleted on the same pattern (= `JitAccess' / `register_symbols'
    // / `declare_funcs' / `collect_funcs' / `declare_length_with_
    // inline_nil' all gone).  The 4 `nl_jit_access_*' trampolines
    // stay in `jit::access' as `#[no_mangle] extern "C"' symbols
    // resolved by the dlsym bridge for nelisp-cc compiled hot paths
    // and by `bridge::unified_fn_ptr' for substrate.el bootstrap
    // paths.  The `length' inline-NIL fast path is now handled by
    // the trampoline body's `tag == SEXP_TAG_NIL' arm.
    //
    // Phase 7.1.6.c (Doc 28 §3.6.c): arith cluster JIT wrappers
    // deleted on the same pattern (= `JitArith' / `register_symbols'
    // / `declare_funcs' / `collect_funcs' / `declare_binop' /
    // `declare_ash' all gone).  Unlike cons / access there were no
    // pre-existing Rust trampolines for arith — the Cranelift IR was
    // the implementation — so 12 plain `nl_jit_arith_*' trampolines
    // were introduced in 7.1.6.c (see `jit::arith') that mirror the
    // deleted Cranelift IR semantics 1-to-1.
    //
    // Phase 7.1.6.d (Doc 28 §3.6.d): predicate cluster JIT wrappers
    // deleted on the same pattern (= `JitPredicate' / `PredicateIds'
    // / `register_symbols' / `declare_funcs' / `collect_funcs' /
    // `declare_eq_inline' all gone, plus `nl_jit_pred_eq' helper
    // subsumed).  Predicate's 7-block Cranelift IR (= same-ref /
    // tag-eq / int-payload fast paths + `sexp_eq' slow path) is
    // mirrored 1-to-1 in a single consolidated `nl_jit_predicate_eq'
    // trampoline (see `jit::predicate') as `#[no_mangle] extern "C"'
    // resolved via the same dlsym + bridge dual path.
    pub(in crate::jit) syscall: syscall::JitSyscall,
}

static UNIFIED_JIT: OnceLock<UnifiedJit> = OnceLock::new();

/// Return the shared `UnifiedJit' instance, building it on first
/// access.  See `UnifiedJit' doc for the 6-step orchestration.
pub(super) fn unified_jit() -> &'static UnifiedJit {
    UNIFIED_JIT.get_or_init(|| {
        // Step 1: shared JITBuilder + each submodule registers its
        // imported `nl_jit_*' symbols.
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
            .expect("cranelift_jit: host ISA must resolve");
        // Phase 7.1.6.a.2: cons::register_symbols deleted (= no
        // Cranelift wrapper page for cons; trampolines are reached
        // either via dlsym from nelisp-cc compiled code or via
        // `bridge::unified_fn_ptr' for substrate.el bootstrap).
        // Phase 7.1.6.b: access::register_symbols deleted on the same
        // pattern (= no Cranelift wrapper page for access cluster).
        // Phase 7.1.6.c: arith::register_symbols deleted on the same
        // pattern (= no Cranelift wrapper page for arith cluster;
        // 12 plain `nl_jit_arith_*' trampolines reachable via dlsym
        // / bridge — `arith' module had no imported helpers so this
        // call was a no-op pre-7.1.6.c anyway).
        // Phase 7.1.6.d: predicate::register_symbols deleted (=
        // `nl_jit_pred_eq' helper subsumed into the consolidated
        // `nl_jit_predicate_eq' trampoline body; no imported helper
        // remains for the predicate cluster).
        syscall::register_symbols(&mut builder);

        // Step 2: one JITModule for the remaining submodule.
        let mut module = JITModule::new(builder);

        // Step 3: each submodule declares + defines its JIT entries
        // on the shared module.  FuncIds carry forward to step 5.
        // Phase 7.1.6.a.2: cons::declare_funcs deleted.
        // Phase 7.1.6.b: access::declare_funcs deleted.
        // Phase 7.1.6.c: arith::declare_funcs deleted.
        // Phase 7.1.6.d: predicate::declare_funcs deleted.
        let syscall_ids = syscall::declare_funcs(&mut module);

        // Step 4: single finalize → one mmap of executable pages.
        module
            .finalize_definitions()
            .expect("cranelift: finalize_definitions");

        // Step 5: fetch function pointers per submodule.
        // Phase 7.1.6.a.2: cons::collect_funcs deleted.
        // Phase 7.1.6.b: access::collect_funcs deleted.
        // Phase 7.1.6.c: arith::collect_funcs deleted.
        // Phase 7.1.6.d: predicate::collect_funcs deleted.
        let syscall = syscall::collect_funcs(&module, syscall_ids);

        // Step 6: keep executable pages alive for the process
        // lifetime by leaking the JITModule.
        Box::leak(Box::new(module));

        // Phase 7.1.6.a.2: `cons' field deleted from UnifiedJit.
        // Phase 7.1.6.b: `access' field deleted from UnifiedJit.
        // Phase 7.1.6.c: `arith' field deleted from UnifiedJit.
        // Phase 7.1.6.d: `predicate' field deleted from UnifiedJit.
        UnifiedJit { syscall }
    })
}
