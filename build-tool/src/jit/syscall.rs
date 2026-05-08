//! Phase 5 Stage 5.1 — SyscallIR lower.
//!
//! Stage 5.1 progress (2026-05-07, Doc 62):
//! - PARTIAL (commit baedc96): `nelisp--syscall-supported-p' (= 0-arg
//!   constant predicate, returns `t' on Linux else `nil') — Cranelift
//!   compiles it to `iconst + return'.
//! - FULL (this commit): `nelisp--syscall NR ARGS...' (= the generic
//!   libc syscall trampoline).  Per Doc 62 §2.2.1 the 25 specialized
//!   primitives stay on the Rust dispatcher because they require
//!   buffer/struct handling (sockaddr / pollfd / signalfd / etc.) that
//!   isn't expressible as int-only SyscallIR.
//!
//! Lowering strategy for the generic syscall (Doc 62 §5 "helper fn 経由
//! の妥協を許容"): Cranelift cannot emit the host `syscall' instruction
//! directly without an inline-asm escape, so we register the non-
//! variadic Rust wrapper `nl_jit_syscall_call' as a JIT-imported symbol
//! and emit a `call' to it.  The wrapper invokes `libc::syscall' with
//! up to 6 integer args and normalizes errno on return (= `-1' →
//! `-errno', else the raw return value).  The dispatcher hop is removed
//! relative to the Rust path:
//!
//!   before: eval → bi_syscall (arity check, syscall_nr resolve, arg
//!           unpack, libc::syscall, normalize, Sexp::Int wrap)
//!   after:  eval → lowered_syscall (syscall_nr, arg unpack, JIT-trampoline
//!           [= libc::syscall + normalize], Sexp::Int wrap)
//!
//! The eval-loop drops the dispatch arm; the trampoline does the
//! libc::syscall + normalize in one hop.  `mov rax,NR` is not literally
//! emitted by Cranelift here (the libc wrapper does that internally),
//! but the Rust → JIT → libc hop is one step shorter than Rust →
//! dispatch → libc and the lower entry is what unblocks the rest of the
//! Stage 5.1 ramp.

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

/// Linux build returns 1, others return 0.  The constant is baked into
/// the JIT-emitted `iconst' so the function is a 2-instruction
/// fragment after register allocation.
#[cfg(target_os = "linux")]
const SUPPORTED_CONST: i64 = 1;
#[cfg(not(target_os = "linux"))]
const SUPPORTED_CONST: i64 = 0;

/// `nl_jit_syscall_call(nr, a0..a5) -> i64' — non-variadic wrapper for
/// `libc::syscall' that JIT code can call through Cranelift's
/// `Linkage::Import' mechanism.  Errno is normalized in-wrapper so the
/// JIT entry returns one i64 (negative on error = `-errno', else the
/// raw return value).
#[cfg(target_os = "linux")]
unsafe extern "C" fn nl_jit_syscall_call(
    nr: i64,
    a0: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
) -> i64 {
    let r = libc::syscall(nr, a0, a1, a2, a3, a4, a5);
    if r == -1 {
        -(*libc::__errno_location() as i64)
    } else {
        r as i64
    }
}

/// Non-Linux build: ENOSYS-equivalent so the JIT entry surface remains
/// identical across platforms even though the OS surface is currently
/// Linux-only (Doc 62 §3 5.6 = future Path B).
#[cfg(not(target_os = "linux"))]
unsafe extern "C" fn nl_jit_syscall_call(
    _nr: i64,
    _a0: i64,
    _a1: i64,
    _a2: i64,
    _a3: i64,
    _a4: i64,
    _a5: i64,
) -> i64 {
    -38 /* ENOSYS */
}

pub(super) struct JitSyscall {
    pub(super) supported_p: extern "C" fn() -> i64,
    /// `(nr, a0..a5) -> i64' (errno already normalized to `-errno' on
    /// error, else raw `libc::syscall' return value).
    pub(super) syscall: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64,
}

pub(super) struct SyscallIds {
    supported_p: FuncId,
    syscall: FuncId,
}

fn declare_const_i64(module: &mut JITModule, name: &str, k: i64) -> FuncId {
    let mut sig = module.make_signature();
    sig.returns.push(AbiParam::new(types::I64));
    let func_id = module
        .declare_function(name, Linkage::Local, &sig)
        .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", name, e));

    let mut ctx = module.make_context();
    ctx.func.signature = sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let block = fb.create_block();
        fb.switch_to_block(block);
        fb.seal_block(block);
        let v = fb.ins().iconst(types::I64, k);
        fb.ins().return_(&[v]);
        fb.finalize();
    }

    module
        .define_function(func_id, &mut ctx)
        .unwrap_or_else(|e| panic!("cranelift: define_function {}: {}", name, e));
    module.clear_context(&mut ctx);
    func_id
}

/// Build the JIT entry that calls `nl_jit_syscall_call'.  The entry has
/// the same `(i64 × 7) -> i64' shape as the helper, with a single
/// `call' instruction body.  Cranelift inlines arg shuffling into
/// register-passing per the host C ABI.
fn declare_syscall_trampoline(module: &mut JITModule) -> FuncId {
    // 1. Imported helper signature.
    let mut import_sig = module.make_signature();
    for _ in 0..7 {
        import_sig.params.push(AbiParam::new(types::I64));
    }
    import_sig.returns.push(AbiParam::new(types::I64));
    let helper_id = module
        .declare_function("nl_jit_syscall_call", Linkage::Import, &import_sig)
        .expect("cranelift: declare_function nl_jit_syscall_call");

    // 2. JIT entry signature (= same as helper).
    let mut entry_sig = module.make_signature();
    for _ in 0..7 {
        entry_sig.params.push(AbiParam::new(types::I64));
    }
    entry_sig.returns.push(AbiParam::new(types::I64));
    let entry_id = module
        .declare_function("nelisp_jit_syscall", Linkage::Local, &entry_sig)
        .expect("cranelift: declare_function nelisp_jit_syscall");

    // 3. Entry body: forward all 7 i64 params to the imported helper
    //    and return its result.
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
        .expect("cranelift: define_function nelisp_jit_syscall");
    module.clear_context(&mut ctx);
    entry_id
}

/// Doc 77 Stage 2-prep (2026-05-09): submodule-level helper that
/// registers all imported syscall trampoline symbols on the shared
/// JITBuilder.
pub(super) fn register_symbols(builder: &mut JITBuilder) {
    builder.symbol("nl_jit_syscall_call", nl_jit_syscall_call as *const u8);
}

/// Doc 77 Stage 2-prep: declare + define every JIT entry this module
/// owns on the *shared* JITModule.
pub(super) fn declare_funcs(module: &mut JITModule) -> SyscallIds {
    let supported_p =
        declare_const_i64(module, "nelisp_jit_syscall_supported_p", SUPPORTED_CONST);
    let syscall = declare_syscall_trampoline(module);
    SyscallIds {
        supported_p,
        syscall,
    }
}

/// Doc 77 Stage 2-prep: fetch finalized function pointers post-
/// `finalize_definitions' and pack them into `JitSyscall'.
pub(super) fn collect_funcs(module: &JITModule, ids: SyscallIds) -> JitSyscall {
    let supported_ptr = module.get_finalized_function(ids.supported_p);
    let syscall_ptr = module.get_finalized_function(ids.syscall);
    // SAFETY: declared signatures match the function-pointer types.
    unsafe {
        JitSyscall {
            supported_p: std::mem::transmute::<_, extern "C" fn() -> i64>(supported_ptr),
            syscall: std::mem::transmute::<
                _,
                extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64,
            >(syscall_ptr),
        }
    }
}

// Doc 77b Stage b.4 (2026-05-09) — `lowered_syscall*' Rust strategy
// fns + `register(map)' deleted.  The `nelisp--syscall' /
// `nelisp--syscall-supported-p' entries are now driven by elisp
// wrappers in `lisp/nelisp-jit-strategy.el' that call the JIT
// trampolines through the Stage b.2 `nl-jit-call-syscall' /
// `nl-jit-call-i64-i64' bridge primitives + the
// `nelisp--syscall-nr-resolve' helper (= the libc symbol catalog
// stays Rust because `libc::SYS_*' constants are Rust-side).
//
// On non-Linux the wrappers still install — the JIT entries return
// a constant -ENOSYS / 0 surface that elisp `nelisp-os-*' callers
// route around via `nelisp--syscall-supported-p' returning nil.
// Original `bi_syscall' / `bi_syscall_supported_p' fallback in
// `eval/builtins.rs::dispatch' is unreachable for these names after
// the elisp wrappers install, but kept for the rare
// `nelisp--apply-builtin-dispatch' direct-route case.

#[cfg(test)]
fn jit() -> &'static JitSyscall {
    &super::unified_jit().syscall
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_syscall_supported_p_returns_const() {
        let j = jit();
        // On Linux must be 1; on other hosts 0.
        assert_eq!((j.supported_p)(), SUPPORTED_CONST);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn jit_syscall_getpid_matches_libc() {
        let j = jit();
        let nr = libc::SYS_getpid as i64;
        let r = (j.syscall)(nr, 0, 0, 0, 0, 0, 0);
        let direct = unsafe { libc::getpid() } as i64;
        assert_eq!(r, direct);
        assert!(r > 0);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn jit_syscall_invalid_fd_returns_neg_ebadf() {
        // SYS_read with fd=999 → -EBADF.
        let j = jit();
        let nr = libc::SYS_read as i64;
        let mut buf = [0u8; 8];
        let r = (j.syscall)(
            nr,
            999,
            buf.as_mut_ptr() as i64,
            buf.len() as i64,
            0,
            0,
            0,
        );
        assert!(r < 0, "expected errno, got {}", r);
        assert_eq!(r, -(libc::EBADF as i64));
    }
}
