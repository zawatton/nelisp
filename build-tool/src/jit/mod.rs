//! Phase 5 — Cranelift JIT lowering for hot-path primitives.
//!
//! Stage 5.0 (2026-05-07, Doc 62) — scaffold only.
//!
//! This module hosts the `lower_entries` registry that maps a
//! primitive name (e.g. `nelisp--syscall`, `nelisp--add2`, `car`) to
//! a JIT-compiled lowering of that primitive.  The eval loop
//! (`build-tool/src/eval/mod.rs`) consults `lower_entries` BEFORE the
//! generic `dispatch` for every call site:
//!
//! ```text
//!   call site name → lower_entries.get(name)
//!     ├─ Some(jit_fn) → jit_fn(args, env)        (= JIT path)
//!     └─ None         → dispatch(name, args, env) (= fallback)
//! ```
//!
//! In Stage 5.0 the registry is intentionally **empty**; the eval-loop
//! hook is wired so subsequent stages (5.1〜5.5) can plug their lower
//! entries in without further plumbing changes.  All existing
//! primitives continue to flow through the dispatch fallback, giving
//! us a no-op build with the JIT scaffold present.
//!
//! The 5 lowering submodules each follow the same pattern: a
//! `register(&mut HashMap<...>)` entry-point that the global builder
//! calls once at startup.  A submodule with no lowerings yet (= all
//! 5 today) is a no-op.

use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

/// Shared helper used by Stage 5.1 syscall + 5.3 cons + 5.4 access +
/// 5.5 predicate trampolines: declare an `(i64 × N_PARAMS) -> i64'
/// imported helper (= HELPER_NAME, must be `JITBuilder::symbol'-
/// registered before this is called) and a `Linkage::Local' JIT entry
/// (= JIT_NAME) whose body forwards all N i64 args to the helper and
/// returns the helper's i64 result.
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
// Doc 77b Stage b.5 (2026-05-09): the former `mod bench;' (=
// `build-tool/src/jit/bench.rs', 268 LOC of `#[ignore]' Rust benches
// covering Doc 62 §4.2 dispatcher-bypass speedup measurements) was
// migrated to elisp ERT under `test/nelisp-jit-bench-test.el'.  The
// elisp port talks to the same JIT entries through the Stage b.2
// `nl-jit-call-*' bridge primitives, so the speedup methodology is
// preserved verbatim while the Rust core sheds the dependency on
// `Env' construction inside `#[cfg(test)]'.
//
// Doc 77b Stage b.2 (2026-05-09): `nl-jit-call-*' bridge primitives
// that elisp wrappers (= future replacements for the `lowered_X' Rust
// fns shipped earlier in Doc 62 Stage C-Phase1) call to invoke JIT
// entries by name.  Wired into `eval::builtins::dispatch' as the
// 3 `nl-jit-call-*' built-ins via the re-exports below — keeping
// the module itself `pub(super)' avoids leaking `UnifiedJit' field
// types into the crate-public surface.
pub(super) mod bridge;
pub use bridge::{
    bi_nl_jit_call_i64_i64, bi_nl_jit_call_out_1, bi_nl_jit_call_out_1i,
    bi_nl_jit_call_out_2, bi_nl_jit_call_out_2i, bi_nl_jit_call_ptr_ptr,
    bi_nl_jit_call_syscall,
};
mod cons;
// Doc 77b Stage b.1 (2026-05-09): JIT IR DSL interpreter.  No
// integration with `UnifiedJit` yet — the module is parallel to the
// existing `declare_X_inline` helpers.  Stage b.3 will switch the
// 5 `declare_X_inline` callers to go through `dsl::build_rule' on
// elisp-authored Sexp rules.  Until then the AST + parser + builder
// have no production callers, only `#[cfg(test)]' coverage; allow
// `dead_code` here so the b.1 ship is warning-clean.
#[allow(dead_code)]
mod dsl;
mod predicate;
// Doc 77b Stage b.4 (2026-05-09): Rust helper primitives backing
// the elisp wrappers in `lisp/nelisp-jit-strategy.el'.  Holds the
// multi-variant fall-through bodies (= length / aref / aset / elt)
// + arith Float helpers + bitwise Int helpers + bool conversion +
// syscall-nr resolver.  See `strategy.rs' header for the surface.
mod strategy;
pub use strategy::{
    bi_add2_float, bi_ash_impl, bi_bool_vector_len, bi_char_table_aref,
    bi_char_table_aset, bi_int_eq_zero, bi_logand2_impl, bi_logior2_impl,
    bi_logxor2_impl, bi_mul2_float, bi_mut_str_len, bi_mut_str_set_codepoint,
    bi_num_eq2_float, bi_num_ge2_float, bi_num_gt2_float, bi_num_le2_float,
    bi_num_lt2_float, bi_str_codepoint_at, bi_sub2_float, bi_syscall_nr_resolve,
};
mod syscall;

/// Doc 77 Stage 2-prep (2026-05-09) — unified JITModule.
///
/// Pre-Doc-77 each submodule (arith / cons / access / predicate /
/// syscall) owned its own `OnceLock<JitX>' which built a separate
/// `JITModule' on first access.  That meant 5 `mmap' pages, 5
/// independent symbol namespaces, and Stage 3's "JIT fn calls JIT fn"
/// pattern was structurally infeasible (= each module's `FuncId'
/// only resolves within its own JITModule).
///
/// Stage 2-prep consolidates the 5 modules into one shared
/// `JITBuilder' / `JITModule' built at first `unified_jit()' access:
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
/// Net effect: 5x → 1x mmap, single FuncId namespace.  Submodule
/// `jit() -> &'static JitX' now returns `&unified_jit().x'.
pub(super) struct UnifiedJit {
    pub(super) arith: arith::JitArith,
    pub(super) cons: cons::JitCons,
    pub(super) access: access::JitAccess,
    pub(super) predicate: predicate::JitPredicate,
    pub(super) syscall: syscall::JitSyscall,
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
        cons::register_symbols(&mut builder);
        access::register_symbols(&mut builder);
        predicate::register_symbols(&mut builder);
        syscall::register_symbols(&mut builder);

        // Step 2: one JITModule for all 5 submodules.
        let mut module = JITModule::new(builder);

        // Step 3: each submodule declares + defines its JIT entries
        // on the shared module.  FuncIds carry forward to step 5.
        let arith_ids = arith::declare_funcs(&mut module);
        let cons_ids = cons::declare_funcs(&mut module);
        let access_ids = access::declare_funcs(&mut module);
        let predicate_ids = predicate::declare_funcs(&mut module);
        let syscall_ids = syscall::declare_funcs(&mut module);

        // Step 4: single finalize → one mmap of executable pages.
        module
            .finalize_definitions()
            .expect("cranelift: finalize_definitions");

        // Step 5: fetch function pointers per submodule.
        let arith = arith::collect_funcs(&module, arith_ids);
        let cons = cons::collect_funcs(&module, cons_ids);
        let access = access::collect_funcs(&module, access_ids);
        let predicate = predicate::collect_funcs(&module, predicate_ids);
        let syscall = syscall::collect_funcs(&module, syscall_ids);

        // Step 6: keep executable pages alive for the process
        // lifetime by leaking the JITModule.
        Box::leak(Box::new(module));

        UnifiedJit {
            arith,
            cons,
            access,
            predicate,
            syscall,
        }
    })
}
