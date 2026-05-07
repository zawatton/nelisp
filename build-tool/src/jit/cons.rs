//! Phase 5 Stage 5.3 — ConsIR lower with inline NIL fast path.
//!
//! Stage 5.3 (2026-05-07, Doc 62, repr-pin commit 2fb64cd):
//! - Initial scaffold (commit eee1cb9) routed `car' / `cdr' / `cons'
//!   through Rust trampolines (`nl_jit_cons_car/cdr/make') that did
//!   the variant match + clone, with the JIT entry being a thin call
//!   wrapper.  Stage 5.3 v1 = pure trampoline with ~zero perf gain.
//! - This commit adds an inline NIL tag-byte fast path on top of the
//!   trampoline now that `Sexp' has `#[repr(C, u8)]':
//!
//!   ```text
//!   jit_car(arg_ptr, out_ptr):
//!     a_tag = movzx u8 [arg_ptr + 0]
//!     if a_tag == TAG_NIL → return OK  (= caller's out is already Nil)
//!     else                  → call helper(arg_ptr, out_ptr)
//!   ```
//!
//! `(car nil) = nil' / `(cdr nil) = nil' is a hot path in elisp list
//! traversal (= `(while (consp x) ... (setq x (cdr x)))' terminates
//! on Nil); the inline path skips the helper call entirely for that
//! case.  Cons / wrong-type cases still flow through the helper for
//! `Rc<RefCell<...>>' deref + clone + canonical-error fallback.
//!
//! `cons' (= the constructor) has no NIL shortcut, so it keeps the
//! Stage 5.3 v1 pure-trampoline shape (= `declare_helper_call').
//!
//! The trampoline ABI is uniform: each helper takes input Sexp(s) by
//! `*const Sexp' and writes the result to an `*mut Sexp' out-param,
//! returning `TRAMPOLINE_OK = 0' on success or `_ERR = 1' on
//! wrong-type so the caller falls through to the dispatcher.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::{Sexp, SEXP_TAG_NIL};

use super::{declare_helper_call, LowerFn};

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// `(car CELL) -> Sexp' trampoline.  `Nil' is treated as `(car nil)' =
/// `nil' per elisp.  Wrong-type returns `TRAMPOLINE_ERR' so the caller
/// can fall back to the dispatcher's canonical error.
unsafe extern "C" fn nl_jit_cons_car(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Nil => {
            *out = Sexp::Nil;
            TRAMPOLINE_OK
        }
        Sexp::Cons(h, _) => {
            *out = h.borrow().clone();
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

unsafe extern "C" fn nl_jit_cons_cdr(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Nil => {
            *out = Sexp::Nil;
            TRAMPOLINE_OK
        }
        Sexp::Cons(_, d) => {
            *out = d.borrow().clone();
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(cons A B) -> (A . B)' constructor — never wrong-type, always OK.
unsafe extern "C" fn nl_jit_cons_make(
    a: *const Sexp,
    b: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    *out = Sexp::cons((*a).clone(), (*b).clone());
    TRAMPOLINE_OK
}

struct JitCons {
    car: extern "C" fn(*const Sexp, *mut Sexp) -> i64,
    cdr: extern "C" fn(*const Sexp, *mut Sexp) -> i64,
    cons_make: extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64,
}

static JIT_CONS: OnceLock<JitCons> = OnceLock::new();

/// Build a JIT entry that inlines the `(car nil) = nil' / `(cdr nil) =
/// nil' path: load tag at offset 0, branch on `tag == SEXP_TAG_NIL'.
/// Nil → return `TRAMPOLINE_OK' immediately (caller's out is already
/// Nil per `lowered_X' init).  Non-Nil → call HELPER_NAME with the
/// same `(arg_ptr, out_ptr) -> i64' signature.
fn declare_unary_with_nil_inline(
    module: &mut JITModule,
    jit_name: &str,
    helper_name: &str,
) -> FuncId {
    // Imported helper signature.
    let mut helper_sig = module.make_signature();
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.returns.push(AbiParam::new(types::I64));
    let helper_id = module
        .declare_function(helper_name, Linkage::Import, &helper_sig)
        .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", helper_name, e));

    // JIT entry signature.
    let mut entry_sig = module.make_signature();
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.returns.push(AbiParam::new(types::I64));
    let entry_id = module
        .declare_function(jit_name, Linkage::Local, &entry_sig)
        .unwrap_or_else(|e| panic!("cranelift: declare_function {}: {}", jit_name, e));

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let entry_b = fb.create_block();
        let nil_b = fb.create_block();
        let slow_b = fb.create_block();
        fb.append_block_params_for_function_params(entry_b);

        // Entry: load tag byte, dispatch on Nil vs other.
        fb.switch_to_block(entry_b);
        let arg_ptr = fb.block_params(entry_b)[0];
        let out_ptr = fb.block_params(entry_b)[1];
        let flags = MemFlags::trusted();
        let tag_byte = fb.ins().load(types::I8, flags, arg_ptr, 0);
        let tag = fb.ins().uextend(types::I64, tag_byte);
        let is_nil = fb.ins().icmp_imm(IntCC::Equal, tag, SEXP_TAG_NIL as i64);
        fb.ins().brif(is_nil, nil_b, &[], slow_b, &[]);
        fb.seal_block(entry_b);

        // Nil path: write tag byte = SEXP_TAG_NIL (= 0) at offset 0 so
        // callers that pre-populated `out' with a non-Nil placeholder
        // (e.g., unit tests) still see `Sexp::Nil' on return.  The
        // payload bytes are left as-is — `Sexp::Nil' has no payload, so
        // PartialEq's `(Nil, Nil)' arm and Drop's tag-0 dispatch both
        // ignore them.
        fb.switch_to_block(nil_b);
        let nil_tag = fb.ins().iconst(types::I8, SEXP_TAG_NIL as i64);
        fb.ins().store(flags, nil_tag, out_ptr, 0);
        let ok = fb.ins().iconst(types::I64, 0);
        fb.ins().return_(&[ok]);
        fb.seal_block(nil_b);

        // Slow path: forward to the trampoline helper.
        fb.switch_to_block(slow_b);
        let helper_local = module.declare_func_in_func(helper_id, fb.func);
        let inst = fb.ins().call(helper_local, &[arg_ptr, out_ptr]);
        let result = fb.inst_results(inst)[0];
        fb.ins().return_(&[result]);
        fb.seal_block(slow_b);

        fb.finalize();
    }

    module
        .define_function(entry_id, &mut ctx)
        .unwrap_or_else(|e| panic!("cranelift: define_function {}: {}", jit_name, e));
    module.clear_context(&mut ctx);
    entry_id
}

fn build_jit_cons() -> JitCons {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    builder.symbol("nl_jit_cons_car", nl_jit_cons_car as *const u8);
    builder.symbol("nl_jit_cons_cdr", nl_jit_cons_cdr as *const u8);
    builder.symbol("nl_jit_cons_make", nl_jit_cons_make as *const u8);
    let mut module = JITModule::new(builder);

    let car_id = declare_unary_with_nil_inline(
        &mut module, "nelisp_jit_car", "nl_jit_cons_car");
    let cdr_id = declare_unary_with_nil_inline(
        &mut module, "nelisp_jit_cdr", "nl_jit_cons_cdr");
    let make_id =
        declare_helper_call(&mut module, "nelisp_jit_cons", "nl_jit_cons_make", 3);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let car_ptr = module.get_finalized_function(car_id);
    let cdr_ptr = module.get_finalized_function(cdr_id);
    let make_ptr = module.get_finalized_function(make_id);
    Box::leak(Box::new(module));
    // SAFETY: declared signatures match the function-pointer types.
    unsafe {
        JitCons {
            car: std::mem::transmute::<_, extern "C" fn(*const Sexp, *mut Sexp) -> i64>(
                car_ptr,
            ),
            cdr: std::mem::transmute::<_, extern "C" fn(*const Sexp, *mut Sexp) -> i64>(
                cdr_ptr,
            ),
            cons_make: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64,
            >(make_ptr),
        }
    }
}

fn jit() -> &'static JitCons {
    JIT_CONS.get_or_init(build_jit_cons)
}

fn lowered_car(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 1 {
        return crate::eval::builtins::dispatch("car", args, env);
    }
    let mut out = Sexp::Nil;
    let r = (jit().car)(&args[0] as *const _, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("car", args, env)
    }
}

fn lowered_cdr(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 1 {
        return crate::eval::builtins::dispatch("cdr", args, env);
    }
    let mut out = Sexp::Nil;
    let r = (jit().cdr)(&args[0] as *const _, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("cdr", args, env)
    }
}

fn lowered_cons(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return crate::eval::builtins::dispatch("cons", args, env);
    }
    let mut out = Sexp::Nil;
    let r = (jit().cons_make)(
        &args[0] as *const _,
        &args[1] as *const _,
        &mut out as *mut _,
    );
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("cons", args, env)
    }
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("car", lowered_car);
    map.insert("cdr", lowered_cdr);
    map.insert("cons", lowered_cons);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_cons_car_cdr_round_trip() {
        let pair = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let mut out_a = Sexp::Nil;
        let r = (jit().car)(&pair as *const _, &mut out_a as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out_a, Sexp::Int(1));

        let mut out_d = Sexp::Nil;
        let r = (jit().cdr)(&pair as *const _, &mut out_d as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out_d, Sexp::Int(2));
    }

    #[test]
    fn jit_cons_make_constructs_pair() {
        let a = Sexp::Int(7);
        let b = Sexp::Symbol("hello".into());
        let mut out = Sexp::Nil;
        let r = (jit().cons_make)(
            &a as *const _,
            &b as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        // Verify out is a Cons by extracting via cdr trampoline.
        let mut out_d = Sexp::Nil;
        let r = (jit().cdr)(&out as *const _, &mut out_d as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out_d, b);
    }

    #[test]
    fn jit_cons_car_wrong_type_returns_err() {
        let i = Sexp::Int(42);
        let mut out = Sexp::Nil;
        let r = (jit().car)(&i as *const _, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_cons_car_nil_returns_nil() {
        let nil = Sexp::Nil;
        let mut out = Sexp::Int(99);
        let r = (jit().car)(&nil as *const _, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }

    #[test]
    fn jit_cons_cdr_nil_returns_nil() {
        let nil = Sexp::Nil;
        let mut out = Sexp::Int(99);
        let r = (jit().cdr)(&nil as *const _, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }
}
