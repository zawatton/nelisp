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

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::error::EvalError;
use crate::eval::env::Env;
use crate::eval::sexp::Sexp;

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

/// Lowered primitive function signature.  Identical to
/// `eval::builtins::dispatch` so a JIT lowering and the fallback
/// dispatcher are interchangeable from the eval loop's perspective.
pub type LowerFn = fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>;

mod access;
mod arith;
#[cfg(test)]
mod bench;
mod cons;
mod predicate;
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

/// Doc 77 Stage 1.C (2026-05-09) — env-toggleable JIT entries.
///
/// Names listed here have a working `bi_*' fallback in
/// `eval::builtins::dispatch', so removing them from the JIT registry
/// is safe (= the eval loop's `try_lower` returns `None' and the
/// dispatcher takes over).  Used by the `NELISP_NO_JIT=1' escape hatch
/// for ABI-suspect debugging and side-by-side perf comparison.
///
/// Other JIT entries (= `eq' / `car' / `cdr' / `cons' / `setcar' /
/// `setcdr' / `aref' / `aset' / `length' / `elt' / `nelisp--add2'
/// etc.) are **JIT-only** since their `bi_*' arms were deleted in
/// Doc 62 Stage C-Phase1/1b.  They stay registered unconditionally —
/// without that, `dispatch("car", ...)' would return
/// `UnboundFunction("car")' and elisp would fail to load.
const ENV_TOGGLEABLE_ENTRIES: &[&str] = &[
    "nelisp--syscall",
    "nelisp--syscall-supported-p",
];

/// Return whether the `NELISP_NO_JIT=1' escape hatch is active.
/// Sampled at registry init time (= the first `lower_entries()' call)
/// so the value is sticky for the process lifetime.
fn no_jit_env_set() -> bool {
    std::env::var("NELISP_NO_JIT").as_deref() == Ok("1")
}

/// Apply the `NELISP_NO_JIT=1' filter to a freshly-built JIT registry.
/// Removes only the env-toggleable entries (= those with bi_*
/// fallback); JIT-only entries are kept so dispatch does not fail.
/// Extracted as a free fn so unit tests can exercise the filter logic
/// without touching the global `OnceLock' (= which is initialized once
/// per process and therefore not env-var-controllable from test code).
fn apply_no_jit_filter(map: &mut HashMap<&'static str, LowerFn>, no_jit: bool) {
    if !no_jit {
        return;
    }
    for n in ENV_TOGGLEABLE_ENTRIES {
        map.remove(*n);
    }
}

/// Return the global lowering registry.  Initialized lazily on first
/// access (= the first eval-loop call site that consults it).
///
/// In Stage 5.0 every call to `lower_entries().get(name)` returns
/// `None`, routing the eval loop to the existing dispatcher.  As
/// subsequent stages register entries via the `register(...)`
/// entry-points below, hot-path primitives short-circuit through the
/// JIT path here without any further eval-loop changes.
///
/// Doc 77 Stage 1.C (2026-05-09): the env var `NELISP_NO_JIT=1' is
/// sampled at init time; when set, env-toggleable entries (see
/// `ENV_TOGGLEABLE_ENTRIES') are dropped from the returned map so
/// the dispatcher takes the load.  JIT-only entries stay regardless.
pub fn lower_entries() -> &'static HashMap<&'static str, LowerFn> {
    static ENTRIES: OnceLock<HashMap<&'static str, LowerFn>> = OnceLock::new();
    ENTRIES.get_or_init(|| {
        let mut map: HashMap<&'static str, LowerFn> = HashMap::new();
        // Stage 5.1: SyscallIR — `nelisp--syscall*' family.
        syscall::register(&mut map);
        // Stage 5.2: ArithIR — 2-arg arithmetic / comparison.
        arith::register(&mut map);
        // Stage 5.3: ConsIR — car / cdr / cons / setcar / setcdr.
        cons::register(&mut map);
        // Stage 5.4: AccessIR — aref / aset / length / elt.
        access::register(&mut map);
        // Stage 5.5: PredicateIR — eq / atom / consp / listp / null / *p.
        predicate::register(&mut map);
        // Doc 77 Stage 1.C: NELISP_NO_JIT=1 escape hatch.
        apply_no_jit_filter(&mut map, no_jit_env_set());
        map
    })
}

/// Try to lower a call site through the JIT registry.  Returns
/// `Some(result)` when a lowering is registered for `name`, else
/// `None` so the caller falls back to the generic dispatcher.
pub fn try_lower(
    name: &str,
    args: &[Sexp],
    env: &mut Env,
) -> Option<Result<Sexp, EvalError>> {
    lower_entries().get(name).map(|f| f(args, env))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_covers_stage_5_6_full_set() {
        // Cumulative contract through Stage 5.6 (2026-05-07):
        // - Stage 5.1 SyscallIR: nelisp--syscall, nelisp--syscall-supported-p
        // - Stage 5.2 ArithIR: 11 binary arith/cmp/bitwise + ash
        // - Stage 5.3 ConsIR: car, cdr, cons (+ Stage 5.6 setcar, setcdr)
        // - Stage 5.4 AccessIR: length, aref (+ Stage 5.6 aset, elt)
        // - Stage 5.5 PredicateIR: eq
        let needed = [
            "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
            "nelisp--num-eq2", "nelisp--num-lt2", "nelisp--num-gt2",
            "nelisp--num-le2", "nelisp--num-ge2",
            "nelisp--logior2", "nelisp--logand2", "nelisp--logxor2",
            "ash",
            "nelisp--syscall", "nelisp--syscall-supported-p",
            "car", "cdr", "cons", "setcar", "setcdr",
            "length", "aref", "aset", "elt",
            "eq",
        ];
        for name in needed {
            assert!(
                lower_entries().contains_key(name),
                "lower_entries missing `{}'; found: {:?}",
                name,
                lower_entries().keys().collect::<Vec<_>>(),
            );
        }
    }

    // --- Doc 77 Stage 1.C (2026-05-09) — NELISP_NO_JIT escape hatch ---

    /// Build the same map `lower_entries()' would build, ignoring the
    /// `OnceLock' cache and the live env var.  Used to test the
    /// filter without dirtying global process state.
    fn build_unfiltered_map_for_test() -> HashMap<&'static str, LowerFn> {
        let mut map: HashMap<&'static str, LowerFn> = HashMap::new();
        syscall::register(&mut map);
        arith::register(&mut map);
        cons::register(&mut map);
        access::register(&mut map);
        predicate::register(&mut map);
        map
    }

    #[test]
    fn no_jit_filter_unset_keeps_all_entries() {
        let mut map = build_unfiltered_map_for_test();
        let baseline = map.len();
        apply_no_jit_filter(&mut map, false);
        assert_eq!(map.len(), baseline);
        for n in ENV_TOGGLEABLE_ENTRIES {
            assert!(
                map.contains_key(n),
                "no-op filter must leave env-toggleable `{}' in place",
                n
            );
        }
    }

    #[test]
    fn no_jit_filter_set_removes_only_env_toggleable() {
        let mut map = build_unfiltered_map_for_test();
        let baseline = map.len();
        apply_no_jit_filter(&mut map, true);
        assert_eq!(
            map.len(),
            baseline - ENV_TOGGLEABLE_ENTRIES.len(),
            "filter removed wrong number of entries"
        );
        for n in ENV_TOGGLEABLE_ENTRIES {
            assert!(
                !map.contains_key(n),
                "env-toggleable `{}' must be removed when NELISP_NO_JIT=1",
                n
            );
        }
        // JIT-only entries (sampled): no `bi_*' fallback exists, must
        // stay registered so dispatch does not UnboundFunction-fail.
        for n in [
            "eq", "car", "cdr", "cons", "setcar", "setcdr",
            "aref", "aset", "length", "elt",
            "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
            "ash",
        ] {
            assert!(
                map.contains_key(n),
                "JIT-only `{}' must stay registered even with NELISP_NO_JIT=1",
                n
            );
        }
    }
}
