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

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

/// All 11 JIT-compiled arithmetic / comparison / bitwise primitives.
/// Built once at first access via `super::unified_jit'; the shared
/// `JITModule' is leaked so each `extern "C" fn' pointer stays valid
/// for the process lifetime.
pub(super) struct JitArith {
    pub(super) add: extern "C" fn(i64, i64) -> i64,
    pub(super) sub: extern "C" fn(i64, i64) -> i64,
    pub(super) mul: extern "C" fn(i64, i64) -> i64,
    pub(super) eq: extern "C" fn(i64, i64) -> i64,
    pub(super) lt: extern "C" fn(i64, i64) -> i64,
    pub(super) gt: extern "C" fn(i64, i64) -> i64,
    pub(super) le: extern "C" fn(i64, i64) -> i64,
    pub(super) ge: extern "C" fn(i64, i64) -> i64,
    pub(super) logior: extern "C" fn(i64, i64) -> i64,
    pub(super) logand: extern "C" fn(i64, i64) -> i64,
    pub(super) logxor: extern "C" fn(i64, i64) -> i64,
    /// `ash(n, count)' for count ∈ [-62, +62]: positive count → ishl,
    /// negative count → sshr by `-count'.  Outside this range the
    /// caller falls through to the Rust dispatcher.
    pub(super) ash: extern "C" fn(i64, i64) -> i64,
}

pub(super) struct ArithIds {
    add: FuncId,
    sub: FuncId,
    mul: FuncId,
    eq: FuncId,
    lt: FuncId,
    gt: FuncId,
    le: FuncId,
    ge: FuncId,
    logior: FuncId,
    logand: FuncId,
    logxor: FuncId,
    ash: FuncId,
}

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

/// Doc 77 Stage 2-prep (2026-05-09): submodule-level helper that
/// registers imported symbols on the shared JITBuilder.  ArithIR
/// has *no* external helpers (= every binop is pure Cranelift IR),
/// so this is a no-op kept for API symmetry with the other modules.
pub(super) fn register_symbols(_builder: &mut JITBuilder) {
    // Intentionally empty — see doc comment above.
}

/// Doc 77 Stage 2-prep: declare + define every JIT entry this module
/// owns on the *shared* JITModule.
pub(super) fn declare_funcs(module: &mut JITModule) -> ArithIds {
    // 3 wrapping arithmetic ops.
    let add = declare_binop(module, "nelisp_jit_add2", |fb, a, b| fb.ins().iadd(a, b));
    let sub = declare_binop(module, "nelisp_jit_sub2", |fb, a, b| fb.ins().isub(a, b));
    let mul = declare_binop(module, "nelisp_jit_mul2", |fb, a, b| fb.ins().imul(a, b));

    // 5 signed integer comparisons → i8 (0/1) → uextend i64.
    fn cmp_to_i64(fb: &mut FunctionBuilder, cc: IntCC, a: Value, b: Value) -> Value {
        let bit = fb.ins().icmp(cc, a, b);
        fb.ins().uextend(types::I64, bit)
    }
    let eq = declare_binop(module, "nelisp_jit_eq2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::Equal, a, b)
    });
    let lt = declare_binop(module, "nelisp_jit_lt2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedLessThan, a, b)
    });
    let gt = declare_binop(module, "nelisp_jit_gt2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedGreaterThan, a, b)
    });
    let le = declare_binop(module, "nelisp_jit_le2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedLessThanOrEqual, a, b)
    });
    let ge = declare_binop(module, "nelisp_jit_ge2", |fb, a, b| {
        cmp_to_i64(fb, IntCC::SignedGreaterThanOrEqual, a, b)
    });

    // 3 bitwise ops.
    let logior = declare_binop(module, "nelisp_jit_logior2", |fb, a, b| {
        fb.ins().bor(a, b)
    });
    let logand = declare_binop(module, "nelisp_jit_logand2", |fb, a, b| {
        fb.ins().band(a, b)
    });
    let logxor = declare_binop(module, "nelisp_jit_logxor2", |fb, a, b| {
        fb.ins().bxor(a, b)
    });

    // `ash' — signed-count shift with `brif' control flow.  Caller
    // bounds-checks count ∈ [-62, +62] before invoking.
    let ash = declare_ash(module);

    ArithIds {
        add,
        sub,
        mul,
        eq,
        lt,
        gt,
        le,
        ge,
        logior,
        logand,
        logxor,
        ash,
    }
}

/// Doc 77 Stage 2-prep: fetch finalized function pointers post-
/// `finalize_definitions' and pack them into `JitArith'.
pub(super) fn collect_funcs(module: &JITModule, ids: ArithIds) -> JitArith {
    let get = |id: FuncId| -> extern "C" fn(i64, i64) -> i64 {
        let ptr = module.get_finalized_function(id);
        // SAFETY: declared signature is `(i64, i64) -> i64`.
        unsafe { std::mem::transmute::<_, extern "C" fn(i64, i64) -> i64>(ptr) }
    };
    JitArith {
        add: get(ids.add),
        sub: get(ids.sub),
        mul: get(ids.mul),
        eq: get(ids.eq),
        lt: get(ids.lt),
        gt: get(ids.gt),
        le: get(ids.le),
        ge: get(ids.ge),
        logior: get(ids.logior),
        logand: get(ids.logand),
        logxor: get(ids.logxor),
        ash: get(ids.ash),
    }
}

// Doc 77b Stage b.4 (2026-05-09) — `lowered_X' Rust strategy fns
// + `register(map)' deleted.  The 12 entries (= add2/sub2/mul2/
// num-{eq,lt,gt,le,ge}2/logior2/logand2/logxor2/ash) are now
// driven by elisp wrappers in `lisp/nelisp-jit-strategy.el' that
// call the JIT entries through the Stage b.2 `nl-jit-call-i64-i64'
// bridge primitive (Int+Int fast path) + `jit/strategy.rs' helpers
// (Float fallback / bitwise Int / ash bounds).  This module retains
// only the Cranelift IR builders + the `JitArith' fn-ptr struct
// reachable via `super::unified_jit()'.

#[cfg(test)]
fn jit() -> &'static JitArith {
    &super::unified_jit().arith
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
