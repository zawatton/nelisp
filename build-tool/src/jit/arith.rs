//! Phase 5 Stage 5.2 — ArithIR lower (= 13 arithmetic primitives).
//!
//! Stage 5.1 POC (2026-05-07, Doc 62): demonstrates the end-to-end
//! Cranelift JIT pipeline by lowering `nelisp--add2`'s fast path
//! (`(Int, Int) → Int`) to a JIT-compiled native function.  The
//! lowered entry intercepts every `(+ A B)` call where both operands
//! are integers; mixed-type / float / arity-error cases fall through
//! to the existing `bi_add2` dispatcher via `dispatch(...)`.
//!
//! This is a single-instruction proof of concept; the full Stage 5.2
//! lower (= 13 arithmetic primitives, all wrapping ops + comparisons)
//! lands in subsequent commits once the JIT pipeline is verified.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::Sexp;

use super::LowerFn;

/// Function pointer to the JIT-compiled `iadd` (wrapping i64 add).
/// Initialized lazily on the first call to `nelisp--add2` whose args
/// hit the integer fast path; subsequent calls reuse the cached
/// pointer.  The `JITModule` that owns the executable memory is
/// `Box::leak`-ed so the function pointer stays valid for the
/// remainder of the process lifetime (= matches Cranelift JIT's
/// "finalize once, run forever" use case).
static JIT_ADD2: OnceLock<extern "C" fn(i64, i64) -> i64> = OnceLock::new();

fn build_jit_add2() -> extern "C" fn(i64, i64) -> i64 {
    // `JITBuilder::new' resolves the host ISA implicitly via
    // `cranelift_native::builder()' (= no direct dependency on
    // `cranelift_native' from this crate).
    let builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    let mut module = JITModule::new(builder);

    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));

    let func_id = module
        .declare_function("nelisp_jit_add2", Linkage::Local, &sig)
        .expect("cranelift: declare_function nelisp_jit_add2");

    let mut ctx = module.make_context();
    ctx.func.signature = sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let block = fb.create_block();
        fb.append_block_params_for_function_params(block);
        fb.switch_to_block(block);
        fb.seal_block(block);
        let a = fb.block_params(block)[0];
        let b = fb.block_params(block)[1];
        let sum = fb.ins().iadd(a, b);
        fb.ins().return_(&[sum]);
        fb.finalize();
    }

    module
        .define_function(func_id, &mut ctx)
        .expect("cranelift: define_function nelisp_jit_add2");
    module.clear_context(&mut ctx);
    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");

    let ptr = module.get_finalized_function(func_id);
    // Leak the module so the executable memory the function pointer
    // references stays valid for the remainder of the process.
    Box::leak(Box::new(module));
    // SAFETY: cranelift-jit guarantees the finalized function pointer
    // matches the declared signature; we declared `(i64, i64) -> i64`.
    unsafe { std::mem::transmute::<_, extern "C" fn(i64, i64) -> i64>(ptr) }
}

fn jit_add2() -> extern "C" fn(i64, i64) -> i64 {
    *JIT_ADD2.get_or_init(build_jit_add2)
}

fn lowered_add2(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    // Fast path: both args are Int → JIT-compiled wrapping add.
    if args.len() == 2 {
        if let (Sexp::Int(a), Sexp::Int(b)) = (&args[0], &args[1]) {
            let sum = jit_add2()(*a, *b);
            return Ok(Sexp::Int(sum));
        }
    }
    // Slow path (= float / arity-error / wrong-type): defer to the
    // existing dispatcher's `bi_add2`.  Calling `dispatch` directly
    // does NOT re-enter the lower hook — the hook only fires from
    // `apply_builtin`.
    crate::eval::builtins::dispatch("nelisp--add2", args, env)
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    // Stage 5.2 POC — only `nelisp--add2` is JIT-lowered today.
    // Stage 5.2 full lands `-sub2` / `-mul2` / `-num-{eq,lt,gt,le,ge}2` /
    // `-logior2` / `-logand2` / `-logxor2` / `ash` in subsequent
    // commits once the POC is bench-verified.
    map.insert("nelisp--add2", lowered_add2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_add2_compiles_and_runs() {
        let f = jit_add2();
        assert_eq!(f(1, 2), 3);
        assert_eq!(f(0, 0), 0);
        assert_eq!(f(-7, 10), 3);
        // Wrapping: i64::MAX + 1 = i64::MIN.
        assert_eq!(f(i64::MAX, 1), i64::MIN);
    }
}
