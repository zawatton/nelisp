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

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::Sexp;

use super::LowerFn;

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

struct JitSyscall {
    supported_p: extern "C" fn() -> i64,
    /// `(nr, a0..a5) -> i64' (errno already normalized to `-errno' on
    /// error, else raw `libc::syscall' return value).
    syscall: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64,
}

static JIT_SYSCALL: OnceLock<JitSyscall> = OnceLock::new();

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

fn build_jit_syscall() -> JitSyscall {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    // Register the trampoline helper so `Linkage::Import' resolves at
    // finalize time.
    builder.symbol("nl_jit_syscall_call", nl_jit_syscall_call as *const u8);
    let mut module = JITModule::new(builder);

    let supported_id =
        declare_const_i64(&mut module, "nelisp_jit_syscall_supported_p", SUPPORTED_CONST);
    let syscall_id = declare_syscall_trampoline(&mut module);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let supported_ptr = module.get_finalized_function(supported_id);
    let syscall_ptr = module.get_finalized_function(syscall_id);
    // Keep the JITModule alive for the rest of the process lifetime so
    // the executable pages stay mapped.
    Box::leak(Box::new(module));
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

fn jit() -> &'static JitSyscall {
    JIT_SYSCALL.get_or_init(build_jit_syscall)
}

fn lowered_syscall_supported_p(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if !args.is_empty() {
        // Arity error: defer to dispatcher's `require_arity' for the
        // canonical error shape.
        return crate::eval::builtins::dispatch("nelisp--syscall-supported-p", args, env);
    }
    let v = (jit().supported_p)();
    Ok(if v != 0 { Sexp::T } else { Sexp::Nil })
}

#[cfg(target_os = "linux")]
fn lowered_syscall(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        // Arity error: the dispatcher emits the canonical "at least one
        // argument" message.
        return crate::eval::builtins::dispatch("nelisp--syscall", args, env);
    }
    let nr = match crate::eval::builtins::syscall_nr(&args[0]) {
        Ok(n) => n,
        // Unknown syscall name / wrong type: the dispatcher emits the
        // canonical error so behavior is identical to the Rust path.
        Err(_) => return crate::eval::builtins::dispatch("nelisp--syscall", args, env),
    };
    let mut a = [0i64; 6];
    for (i, sexp) in args[1..].iter().enumerate().take(6) {
        match crate::eval::builtins::syscall_arg_int("nelisp--syscall", i + 1, sexp) {
            Ok(v) => a[i] = v,
            Err(_) => return crate::eval::builtins::dispatch("nelisp--syscall", args, env),
        }
    }
    let r = (jit().syscall)(nr, a[0], a[1], a[2], a[3], a[4], a[5]);
    Ok(Sexp::Int(r))
}

#[cfg(not(target_os = "linux"))]
fn lowered_syscall(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    crate::eval::builtins::dispatch("nelisp--syscall", args, env)
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("nelisp--syscall-supported-p", lowered_syscall_supported_p);
    map.insert("nelisp--syscall", lowered_syscall);
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
