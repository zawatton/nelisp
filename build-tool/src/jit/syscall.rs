//! Phase 5 Stage 5.1 — SyscallIR lower (= 26 syscall primitives).
//!
//! Stage 5.1 progress (2026-05-07, Doc 62): start with the trivial
//! `nelisp--syscall-supported-p' (= 0-arg constant predicate, returns
//! `t' on Linux else `nil').  Cranelift compiles it to a `mov rax, K;
//! ret' fragment.  Subsequent commits add the actual `nelisp--syscall
//! NR ARGS...' (= libc syscall instruction emit) and the 25
//! specialized primitives that wrap sockaddr / pollfd / signalfd
//! buffers.
//!
//! The simple supported_p case proves out:
//!   1. Cranelift can compile a 0-arg → i64 function (no params, just
//!      `iconst' + `return').
//!   2. The lower hook can wrap the i64 result as `Sexp::T'/`Sexp::Nil'
//!      via the same `bool_to_sexp' helper we use in arith.rs cmp.

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

static JIT_SYSCALL: OnceLock<extern "C" fn() -> i64> = OnceLock::new();

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

fn build_jit_syscall() -> extern "C" fn() -> i64 {
    let builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    let mut module = JITModule::new(builder);

    let id = declare_const_i64(&mut module, "nelisp_jit_syscall_supported_p", SUPPORTED_CONST);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let ptr = module.get_finalized_function(id);
    Box::leak(Box::new(module));
    // SAFETY: declared signature is `() -> i64`.
    unsafe { std::mem::transmute::<_, extern "C" fn() -> i64>(ptr) }
}

fn jit_supported_p() -> extern "C" fn() -> i64 {
    *JIT_SYSCALL.get_or_init(build_jit_syscall)
}

fn lowered_syscall_supported_p(
    args: &[Sexp],
    env: &mut Env,
) -> Result<Sexp, EvalError> {
    if !args.is_empty() {
        // Arity error: defer to dispatcher's `require_arity' for the
        // canonical error shape.
        return crate::eval::builtins::dispatch(
            "nelisp--syscall-supported-p", args, env);
    }
    let v = jit_supported_p()();
    Ok(if v != 0 { Sexp::T } else { Sexp::Nil })
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("nelisp--syscall-supported-p", lowered_syscall_supported_p);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_syscall_supported_p_returns_const() {
        let f = jit_supported_p();
        // On Linux must be 1; on other hosts 0.
        let expected = SUPPORTED_CONST;
        assert_eq!(f(), expected);
    }
}
