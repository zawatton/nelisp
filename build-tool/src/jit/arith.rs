//! Phase 5 Stage 5.2 — ArithIR lower (= 11 binary arithmetic /
//! comparison / bitwise primitives).
//!
//! Stage 5.2 (2026-05-07, Doc 62): all 2-arg integer primitives that
//! map cleanly to a single Cranelift binary instruction are JIT-
//! compiled into one shared `JITModule` and registered in
//! `lower_entries`.  Mixed-type / float / arity-error cases fall
//! through to the existing dispatcher via `dispatch(...)`.
//!
//! Lowered set (= every "fast path" `(Int, Int) → Int|bool`):
//!
//! | primitive             | Cranelift IR              | result kind |
//! |-----------------------+---------------------------+-------------|
//! | `nelisp--add2`        | `iadd`                    | Sexp::Int   |
//! | `nelisp--sub2`        | `isub`                    | Sexp::Int   |
//! | `nelisp--mul2`        | `imul`                    | Sexp::Int   |
//! | `nelisp--num-eq2`     | `icmp Equal` + `uextend`  | Sexp::T/Nil |
//! | `nelisp--num-lt2`     | `icmp SignedLessThan`     | Sexp::T/Nil |
//! | `nelisp--num-gt2`     | `icmp SignedGreaterThan`  | Sexp::T/Nil |
//! | `nelisp--num-le2`     | `icmp SignedLessThanOrEqual`        | Sexp::T/Nil |
//! | `nelisp--num-ge2`     | `icmp SignedGreaterThanOrEqual`     | Sexp::T/Nil |
//! | `nelisp--logior2`     | `bor`                     | Sexp::Int   |
//! | `nelisp--logand2`     | `band`                    | Sexp::Int   |
//! | `nelisp--logxor2`     | `bxor`                    | Sexp::Int   |
//!
//! `ash` (= variable shl/sshr) is added in this commit (Stage 5.2
//! follow-up) using Cranelift's `brif' for the count-sign dispatch.
//! All arithmetic op semantics are wrapping by Cranelift contract —
//! matches the existing `wrapping_add` / etc. behaviour.
//!
//! `ash` JIT path covers count ∈ [-62, +62] (= every typical bit-twiddling
//! use); pathological counts fall through to the Rust dispatcher so the
//! 32-bit-truncation / clamping semantics of `bi_ash' are preserved
//! without re-emitting them in IR.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

use super::LowerFn;

/// All 11 JIT-compiled arithmetic / comparison / bitwise primitives.
/// Built once at first access; shared `JITModule` is leaked so each
/// `extern "C" fn` pointer stays valid for the process lifetime.
struct JitArith {
    add: extern "C" fn(i64, i64) -> i64,
    sub: extern "C" fn(i64, i64) -> i64,
    mul: extern "C" fn(i64, i64) -> i64,
    eq: extern "C" fn(i64, i64) -> i64,
    lt: extern "C" fn(i64, i64) -> i64,
    gt: extern "C" fn(i64, i64) -> i64,
    le: extern "C" fn(i64, i64) -> i64,
    ge: extern "C" fn(i64, i64) -> i64,
    logior: extern "C" fn(i64, i64) -> i64,
    logand: extern "C" fn(i64, i64) -> i64,
    logxor: extern "C" fn(i64, i64) -> i64,
    /// `ash(n, count)' for count ∈ [-62, +62]: positive count → ishl,
    /// negative count → sshr by `-count'.  Outside this range the
    /// caller falls through to the Rust dispatcher.
    ash: extern "C" fn(i64, i64) -> i64,
}

static JIT_ARITH: OnceLock<JitArith> = OnceLock::new();

/// Declare + define a 2-arg i64 → i64 function in MODULE; the EMIT
/// closure is invoked with the argument values and must return a
/// Cranelift `Value` of type i64 (= the function's return value).
fn declare_binop<F>(module: &mut JITModule, name: &str, emit: F) -> FuncId
where
    F: FnOnce(&mut FunctionBuilder, Value, Value) -> Value,
{
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
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
        fb.append_block_params_for_function_params(block);
        fb.switch_to_block(block);
        fb.seal_block(block);
        let a = fb.block_params(block)[0];
        let b = fb.block_params(block)[1];
        let r = emit(&mut fb, a, b);
        fb.ins().return_(&[r]);
        fb.finalize();
    }

    module
        .define_function(func_id, &mut ctx)
        .unwrap_or_else(|e| panic!("cranelift: define_function {}: {}", name, e));
    module.clear_context(&mut ctx);
    func_id
}

/// Build the `ash' lowering: `(n, count) -> i64' with two-block
/// control flow (positive count → `ishl', negative → `ineg' + `sshr').
/// Caller is responsible for clamping count ∈ [-62, +62] before
/// invoking — outside that range the JIT path is bypassed.
fn declare_ash(module: &mut JITModule) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    let func_id = module
        .declare_function("nelisp_jit_ash", Linkage::Local, &sig)
        .expect("cranelift: declare_function nelisp_jit_ash");

    let mut ctx = module.make_context();
    ctx.func.signature = sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let entry_b = fb.create_block();
        let pos_b = fb.create_block();
        let neg_b = fb.create_block();
        fb.append_block_params_for_function_params(entry_b);

        // Entry: dispatch on sign of `count'.
        fb.switch_to_block(entry_b);
        let n = fb.block_params(entry_b)[0];
        let count = fb.block_params(entry_b)[1];
        let count_neg = fb.ins().icmp_imm(IntCC::SignedLessThan, count, 0);
        fb.ins().brif(count_neg, neg_b, &[], pos_b, &[]);
        fb.seal_block(entry_b);

        // Positive count: `ishl(n, count)'.
        fb.switch_to_block(pos_b);
        let r_pos = fb.ins().ishl(n, count);
        fb.ins().return_(&[r_pos]);
        fb.seal_block(pos_b);

        // Negative count: `sshr(n, -count)'.
        fb.switch_to_block(neg_b);
        let abs = fb.ins().ineg(count);
        let r_neg = fb.ins().sshr(n, abs);
        fb.ins().return_(&[r_neg]);
        fb.seal_block(neg_b);

        fb.finalize();
    }

    module
        .define_function(func_id, &mut ctx)
        .expect("cranelift: define_function nelisp_jit_ash");
    module.clear_context(&mut ctx);
    func_id
}

fn build_jit_arith() -> JitArith {
    let builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    let mut module = JITModule::new(builder);

    // 3 wrapping arithmetic ops.
    let add_id = declare_binop(&mut module, "nelisp_jit_add2", |fb, a, b| fb.ins().iadd(a, b));
    let sub_id = declare_binop(&mut module, "nelisp_jit_sub2", |fb, a, b| fb.ins().isub(a, b));
    let mul_id = declare_binop(&mut module, "nelisp_jit_mul2", |fb, a, b| fb.ins().imul(a, b));

    // 5 signed integer comparisons → i8 (0/1) → uextend i64.
    fn cmp_to_i64(fb: &mut FunctionBuilder, cc: IntCC, a: Value, b: Value) -> Value {
        let bit = fb.ins().icmp(cc, a, b);
        fb.ins().uextend(types::I64, bit)
    }
    let eq_id = declare_binop(&mut module, "nelisp_jit_eq2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::Equal, a, b)
    });
    let lt_id = declare_binop(&mut module, "nelisp_jit_lt2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedLessThan, a, b)
    });
    let gt_id = declare_binop(&mut module, "nelisp_jit_gt2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedGreaterThan, a, b)
    });
    let le_id = declare_binop(&mut module, "nelisp_jit_le2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedLessThanOrEqual, a, b)
    });
    let ge_id = declare_binop(&mut module, "nelisp_jit_ge2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedGreaterThanOrEqual, a, b)
    });

    // 3 bitwise ops.
    let or_id = declare_binop(&mut module, "nelisp_jit_logior2", |fb, a, b| {
        fb.ins().bor(a, b)
    });
    let and_id = declare_binop(&mut module, "nelisp_jit_logand2", |fb, a, b| {
        fb.ins().band(a, b)
    });
    let xor_id = declare_binop(&mut module, "nelisp_jit_logxor2", |fb, a, b| {
        fb.ins().bxor(a, b)
    });

    // `ash' — signed-count shift with `brif' control flow.  Caller
    // bounds-checks count ∈ [-62, +62] before invoking.
    let ash_id = declare_ash(&mut module);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");

    let get = |id: FuncId| -> extern "C" fn(i64, i64) -> i64 {
        let ptr = module.get_finalized_function(id);
        // SAFETY: declared signature is `(i64, i64) -> i64`.
        unsafe { std::mem::transmute::<_, extern "C" fn(i64, i64) -> i64>(ptr) }
    };

    let arith = JitArith {
        add: get(add_id),
        sub: get(sub_id),
        mul: get(mul_id),
        eq: get(eq_id),
        lt: get(lt_id),
        gt: get(gt_id),
        le: get(le_id),
        ge: get(ge_id),
        logior: get(or_id),
        logand: get(and_id),
        logxor: get(xor_id),
        ash: get(ash_id),
    };

    // Leak the module so the executable memory the function pointers
    // reference stays valid for the remainder of the process.
    Box::leak(Box::new(module));

    arith
}

fn jit() -> &'static JitArith {
    JIT_ARITH.get_or_init(build_jit_arith)
}

/// Try-extract a `(i64, i64)' integer pair from a 2-arg call site.
fn try_int_pair(args: &[Sexp]) -> Option<(i64, i64)> {
    if args.len() != 2 {
        return None;
    }
    if let (Sexp::Int(a), Sexp::Int(b)) = (&args[0], &args[1]) {
        Some((*a, *b))
    } else {
        None
    }
}

/// Wrap a JIT comparison's i64 (1 or 0) result as `Sexp::T' / `Sexp::Nil'.
fn bool_to_sexp(v: i64) -> Sexp {
    if v != 0 { Sexp::T } else { Sexp::Nil }
}

macro_rules! lower_int_binop {
    ($name:ident, $fast:expr, $primitive:literal) => {
        fn $name(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
            if let Some((a, b)) = try_int_pair(args) {
                let r = ($fast)(a, b);
                return Ok(Sexp::Int(r));
            }
            crate::eval::builtins::dispatch($primitive, args, env)
        }
    };
}

macro_rules! lower_int_cmp {
    ($name:ident, $fast:expr, $primitive:literal) => {
        fn $name(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
            if let Some((a, b)) = try_int_pair(args) {
                let r = ($fast)(a, b);
                return Ok(bool_to_sexp(r));
            }
            crate::eval::builtins::dispatch($primitive, args, env)
        }
    };
}

lower_int_binop!(lowered_add2,    |a, b| (jit().add)(a, b),    "nelisp--add2");
lower_int_binop!(lowered_sub2,    |a, b| (jit().sub)(a, b),    "nelisp--sub2");
lower_int_binop!(lowered_mul2,    |a, b| (jit().mul)(a, b),    "nelisp--mul2");
lower_int_binop!(lowered_logior2, |a, b| (jit().logior)(a, b), "nelisp--logior2");
lower_int_binop!(lowered_logand2, |a, b| (jit().logand)(a, b), "nelisp--logand2");
lower_int_binop!(lowered_logxor2, |a, b| (jit().logxor)(a, b), "nelisp--logxor2");

lower_int_cmp!(lowered_num_eq2, |a, b| (jit().eq)(a, b), "nelisp--num-eq2");
lower_int_cmp!(lowered_num_lt2, |a, b| (jit().lt)(a, b), "nelisp--num-lt2");
lower_int_cmp!(lowered_num_gt2, |a, b| (jit().gt)(a, b), "nelisp--num-gt2");
lower_int_cmp!(lowered_num_le2, |a, b| (jit().le)(a, b), "nelisp--num-le2");
lower_int_cmp!(lowered_num_ge2, |a, b| (jit().ge)(a, b), "nelisp--num-ge2");

/// `(ash N COUNT)' — lower for count ∈ [-62, +62] only.  Pathological
/// counts (= ones whose `bi_ash' result depends on the 32-bit
/// truncation of `(-count) as u32') fall through to the dispatcher so
/// the existing semantics remain bit-for-bit identical.
fn lowered_ash(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if let Some((n, count)) = try_int_pair(args) {
        if (-62..=62).contains(&count) {
            let r = (jit().ash)(n, count);
            return Ok(Sexp::Int(r));
        }
    }
    crate::eval::builtins::dispatch("ash", args, env)
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("nelisp--add2", lowered_add2);
    map.insert("nelisp--sub2", lowered_sub2);
    map.insert("nelisp--mul2", lowered_mul2);
    map.insert("nelisp--num-eq2", lowered_num_eq2);
    map.insert("nelisp--num-lt2", lowered_num_lt2);
    map.insert("nelisp--num-gt2", lowered_num_gt2);
    map.insert("nelisp--num-le2", lowered_num_le2);
    map.insert("nelisp--num-ge2", lowered_num_ge2);
    map.insert("nelisp--logior2", lowered_logior2);
    map.insert("nelisp--logand2", lowered_logand2);
    map.insert("nelisp--logxor2", lowered_logxor2);
    map.insert("ash", lowered_ash);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_add2_compiles_and_runs() {
        let f = jit().add;
        assert_eq!(f(1, 2), 3);
        assert_eq!(f(0, 0), 0);
        assert_eq!(f(-7, 10), 3);
        // Wrapping: i64::MAX + 1 = i64::MIN.
        assert_eq!(f(i64::MAX, 1), i64::MIN);
    }

    #[test]
    fn jit_sub_mul() {
        assert_eq!((jit().sub)(10, 3), 7);
        assert_eq!((jit().sub)(0, 1), -1);
        assert_eq!((jit().mul)(6, 7), 42);
        assert_eq!((jit().mul)(-3, 4), -12);
    }

    #[test]
    fn jit_cmp_signed() {
        assert_eq!((jit().eq)(5, 5), 1);
        assert_eq!((jit().eq)(5, 4), 0);
        assert_eq!((jit().lt)(3, 4), 1);
        assert_eq!((jit().lt)(4, 3), 0);
        assert_eq!((jit().lt)(-1, 1), 1);
        assert_eq!((jit().gt)(4, 3), 1);
        assert_eq!((jit().le)(3, 3), 1);
        assert_eq!((jit().le)(4, 3), 0);
        assert_eq!((jit().ge)(3, 3), 1);
        assert_eq!((jit().ge)(2, 3), 0);
    }

    #[test]
    fn jit_bitwise() {
        assert_eq!((jit().logior)(0b1100, 0b0011), 0b1111);
        assert_eq!((jit().logand)(0b1110, 0b0111), 0b0110);
        assert_eq!((jit().logxor)(0b1100, 0b1010), 0b0110);
    }

    #[test]
    fn jit_ash_left_shift() {
        // count > 0 → ishl
        assert_eq!((jit().ash)(1, 3), 8);
        assert_eq!((jit().ash)(0xFF, 4), 0xFF0);
        // count = 0 → identity (ishl by 0)
        assert_eq!((jit().ash)(42, 0), 42);
        assert_eq!((jit().ash)(-42, 0), -42);
        // negatives shift left preserves sign-extension at top bits
        assert_eq!((jit().ash)(-1, 1), -2);
    }

    #[test]
    fn jit_ash_right_shift_signed() {
        // count < 0 → sshr by abs(count); sign bit is replicated.
        assert_eq!((jit().ash)(8, -3), 1);
        assert_eq!((jit().ash)(0xFF0, -4), 0xFF);
        // -8 >> 3 = -1 (all sign bits shifted in)
        assert_eq!((jit().ash)(-8, -3), -1);
        assert_eq!((jit().ash)(-100, -1), -50);
        // -1 >> 1 = -1 (every bit set, sign-extends to all-ones)
        assert_eq!((jit().ash)(-1, -1), -1);
    }
}
