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
//! entirely; cons trampolines are now reached via path (2) only, so
//! [`UnifiedJit`] holds 4 sub-modules (arith / access / predicate /
//! syscall) instead of the original 5.

use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

/// Shared helper used by the access cluster (Phase 7.1.6.a.2 left
/// access as the only consumer; cons / predicate / syscall trampolines
/// have either been deleted or use bespoke IR shapes): declare an
/// `(i64 × N_PARAMS) -> i64' imported helper (= HELPER_NAME, must be
/// `JITBuilder::symbol'-registered before this is called) and a
/// `Linkage::Local' JIT entry (= JIT_NAME) whose body forwards all N
/// i64 args to the helper and returns the helper's i64 result.
///
/// Returns the entry's FuncId so the caller can `get_finalized_function'
/// it after `module.finalize_definitions()'.
pub(super) fn declare_helper_call(
    module: &mut JITModule,
    jit_name: &str,
    helper_name: &str,
    n_params: usize,
) -> FuncId {
    let mut import_sig = module.make_signature();
    for _ in 0..n_params {
        import_sig.params.push(AbiParam::new(types::I64));
    }
    import_sig.returns.push(AbiParam::new(types::I64));
    let helper_id = module
        .declare_function(helper_name, Linkage::Import, &import_sig)
        .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", helper_name, e));

    let mut entry_sig = module.make_signature();
    for _ in 0..n_params {
        entry_sig.params.push(AbiParam::new(types::I64));
    }
    entry_sig.returns.push(AbiParam::new(types::I64));
    let entry_id = module
        .declare_function(jit_name, Linkage::Local, &entry_sig)
        .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", jit_name, e));

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let block = fb.create_block();
        fb.append_block_params_for_function_params(block);
        fb.switch_to_block(block);
        fb.seal_block(block);
        let params: Vec<Value> = fb.block_params(block).to_vec();
        let helper_local = module.declare_func_in_func(helper_id, fb.func);
        let inst = fb.ins().call(helper_local, &params);
        let ret_val = fb.inst_results(inst)[0];
        fb.ins().return_(&[ret_val]);
        fb.finalize();
    }

    module
        .define_function(entry_id, &mut ctx)
        .unwrap_or_else(|e| panic!("cranelift: define_function {}: {}", jit_name, e));
    module.clear_context(&mut ctx);
    entry_id
}

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
pub(super) struct UnifiedJit {
    pub(in crate::jit) arith: arith::JitArith,
    pub(in crate::jit) access: access::JitAccess,
    pub(in crate::jit) predicate: predicate::JitPredicate,
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
        arith::register_symbols(&mut builder);
        access::register_symbols(&mut builder);
        predicate::register_symbols(&mut builder);
        syscall::register_symbols(&mut builder);

        // Step 2: one JITModule for all 4 submodules.
        let mut module = JITModule::new(builder);

        // Step 3: each submodule declares + defines its JIT entries
        // on the shared module.  FuncIds carry forward to step 5.
        let arith_ids = arith::declare_funcs(&mut module);
        let access_ids = access::declare_funcs(&mut module);
        let predicate_ids = predicate::declare_funcs(&mut module);
        let syscall_ids = syscall::declare_funcs(&mut module);

        // Step 4: single finalize → one mmap of executable pages.
        module
            .finalize_definitions()
            .expect("cranelift: finalize_definitions");

        // Step 5: fetch function pointers per submodule.
        let arith = arith::collect_funcs(&module, arith_ids);
        let access = access::collect_funcs(&module, access_ids);
        let predicate = predicate::collect_funcs(&module, predicate_ids);
        let syscall = syscall::collect_funcs(&module, syscall_ids);

        // Step 6: keep executable pages alive for the process
        // lifetime by leaking the JITModule.
        Box::leak(Box::new(module));

        UnifiedJit { arith, access, predicate, syscall }
    })
}
