//! Phase 5 Stage 5.4 — AccessIR lower with inline NIL fast path.
//!
//! Stage 5.4 (2026-05-07, Doc 62, repr-pin commit 2fb64cd):
//! - Initial scaffold (commit 5299e44) registered `length' and `aref'
//!   via pure Rust trampolines that did the variant match + clone.
//! - This commit adds an inline NIL fast path on top of `length' now
//!   that `Sexp' has `#[repr(C, u8)]':
//!
//!   ```text
//!   jit_length(arg_ptr, out_ptr):
//!     a_tag = movzx u8 [arg_ptr + 0]
//!     if a_tag == TAG_NIL:
//!       store i8  SEXP_TAG_INT to [out_ptr + 0]
//!       store i64 0            to [out_ptr + 8]
//!       return OK
//!     else:
//!       call helper(arg_ptr, out_ptr)
//!   ```
//!
//! `(length nil) = 0' fires every time elisp builds an alist scan
//! that ends in Nil, so the inline path is a real hot-path skip.
//! Vector / Str / etc still flow through the helper for `Rc' deref.
//!
//! `aref' has no NIL shortcut (= `(aref nil 0)' is a wrong-type
//! error, the helper's ERR fallback already short-circuits before
//! any heap access), so it keeps the v1 `declare_helper_call' shape.
//!
//! Trampoline coverage:
//! - `length': handles `Sexp::Nil' / `Sexp::Vector' / `Sexp::Str';
//!   `MutStr' / `BoolVector' / `Cons' (spine walk) / others fall
//!   through to `bi_length'.
//! - `aref': handles `Sexp::Vector' only; `Str' / `MutStr' / `CharTable'
//!   / `BoolVector' fall through to `bi_aref'.
//!
//! Stage 5.6 (2026-05-07) — `aset' and `elt' added on the same
//! `declare_helper_call' shape:
//! - `aset': `Vector' fast path only.  `MutStr' (= codepoint mutation
//!   that rebuilds the underlying String) stays in the dispatcher
//!   because the 4-arg helper would not save anything.
//! - `elt': `Vector' (= aref) + `Cons' (= list walk) fast path.  Other
//!   sequence types fall through.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::{Sexp, SEXP_TAG_INT, SEXP_TAG_NIL};

use super::{declare_helper_call, LowerFn};

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// `(length OBJ)' fast path: `Nil' / `Vector' / `Str'.  Other types
/// return `TRAMPOLINE_ERR' so the caller falls through to `bi_length'.
unsafe extern "C" fn nl_jit_access_length(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Nil => {
            *out = Sexp::Int(0);
            TRAMPOLINE_OK
        }
        Sexp::Vector(v) => {
            *out = Sexp::Int(v.borrow().len() as i64);
            TRAMPOLINE_OK
        }
        Sexp::Str(s) => {
            *out = Sexp::Int(s.chars().count() as i64);
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(aref VECTOR INDEX)' fast path: `Sexp::Vector' only with non-
/// negative INDEX in range.  Out-of-range / wrong-type / negative
/// index returns `TRAMPOLINE_ERR' for canonical-error fall-through.
unsafe extern "C" fn nl_jit_access_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    match &*arg {
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            if let Some(elem) = borrowed.get(idx as usize) {
                *out = elem.clone();
                TRAMPOLINE_OK
            } else {
                TRAMPOLINE_ERR
            }
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(aset VECTOR INDEX VALUE)' fast path: `Sexp::Vector' only.
/// Returns VALUE per Emacs' `aset' contract.  `MutStr' aset (=
/// codepoint mutation) is left to the dispatcher because the rebuild-
/// String path is not worth a JIT helper.
unsafe extern "C" fn nl_jit_access_aset(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    match &*arg {
        Sexp::Vector(v) => {
            let mut borrowed = v.borrow_mut();
            if (idx as usize) >= borrowed.len() {
                return TRAMPOLINE_ERR;
            }
            borrowed[idx as usize] = (*val).clone();
            *out = (*val).clone();
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(elt SEQUENCE INDEX)' fast path: `Sexp::Vector' (= aref) /
/// `Sexp::Cons' (= list walk).  `Nil' is treated as out-of-range
/// because every concrete index is "past" an empty sequence.  Other
/// sequence types (`Str' / `MutStr' / `CharTable' / `BoolVector')
/// fall through so the dispatcher's `bi_aref' delegation handles them.
unsafe extern "C" fn nl_jit_access_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    match &*arg {
        Sexp::Vector(v) => {
            let borrowed = v.borrow();
            if let Some(elem) = borrowed.get(idx as usize) {
                *out = elem.clone();
                TRAMPOLINE_OK
            } else {
                TRAMPOLINE_ERR
            }
        }
        Sexp::Cons(_, _) => {
            let mut cur: Sexp = (*arg).clone();
            let mut remaining = idx;
            loop {
                let next = match &cur {
                    Sexp::Cons(h, t) => {
                        if remaining == 0 {
                            *out = h.borrow().clone();
                            return TRAMPOLINE_OK;
                        }
                        remaining -= 1;
                        t.borrow().clone()
                    }
                    Sexp::Nil => return TRAMPOLINE_ERR,
                    _ => return TRAMPOLINE_ERR,
                };
                cur = next;
            }
        }
        _ => TRAMPOLINE_ERR,
    }
}

struct JitAccess {
    length: extern "C" fn(*const Sexp, *mut Sexp) -> i64,
    aref: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
    aset: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64,
    elt: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
}

static JIT_ACCESS: OnceLock<JitAccess> = OnceLock::new();

/// Build the `length' JIT entry with an inline NIL → Int(0) fast path.
/// Cranelift emits an `i8' store + `i64' store for the Nil case,
/// avoiding the helper call entirely.
fn declare_length_with_inline_nil(module: &mut JITModule) -> FuncId {
    let mut helper_sig = module.make_signature();
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.returns.push(AbiParam::new(types::I64));
    let helper_id = module
        .declare_function("nl_jit_access_length", Linkage::Import, &helper_sig)
        .expect("cranelift: declare_function nl_jit_access_length");

    let mut entry_sig = module.make_signature();
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.returns.push(AbiParam::new(types::I64));
    let entry_id = module
        .declare_function("nelisp_jit_length", Linkage::Local, &entry_sig)
        .expect("cranelift: declare_function nelisp_jit_length");

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let entry_b = fb.create_block();
        let nil_b = fb.create_block();
        let slow_b = fb.create_block();
        fb.append_block_params_for_function_params(entry_b);

        // Entry: load tag byte, branch on Nil vs other.
        fb.switch_to_block(entry_b);
        let arg_ptr = fb.block_params(entry_b)[0];
        let out_ptr = fb.block_params(entry_b)[1];
        let flags = MemFlags::trusted();
        let tag_byte = fb.ins().load(types::I8, flags, arg_ptr, 0);
        let tag = fb.ins().uextend(types::I64, tag_byte);
        let is_nil = fb.ins().icmp_imm(IntCC::Equal, tag, SEXP_TAG_NIL as i64);
        fb.ins().brif(is_nil, nil_b, &[], slow_b, &[]);
        fb.seal_block(entry_b);

        // Nil path: write Sexp::Int(0) inline.  Tag byte at offset 0,
        // i64 payload at offset 8 (= `#[repr(C, u8)]' fixed offsets).
        fb.switch_to_block(nil_b);
        let int_tag_i8 = fb.ins().iconst(types::I8, SEXP_TAG_INT as i64);
        let zero_i64 = fb.ins().iconst(types::I64, 0);
        fb.ins().store(flags, int_tag_i8, out_ptr, 0);
        fb.ins().store(flags, zero_i64, out_ptr, 8);
        let ok = fb.ins().iconst(types::I64, 0);
        fb.ins().return_(&[ok]);
        fb.seal_block(nil_b);

        // Slow path: helper handles Vector/Str/other via match arm.
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
        .expect("cranelift: define_function nelisp_jit_length");
    module.clear_context(&mut ctx);
    entry_id
}

fn build_jit_access() -> JitAccess {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    builder.symbol("nl_jit_access_length", nl_jit_access_length as *const u8);
    builder.symbol("nl_jit_access_aref", nl_jit_access_aref as *const u8);
    builder.symbol("nl_jit_access_aset", nl_jit_access_aset as *const u8);
    builder.symbol("nl_jit_access_elt", nl_jit_access_elt as *const u8);
    let mut module = JITModule::new(builder);

    let length_id = declare_length_with_inline_nil(&mut module);
    let aref_id =
        declare_helper_call(&mut module, "nelisp_jit_aref", "nl_jit_access_aref", 3);
    let aset_id =
        declare_helper_call(&mut module, "nelisp_jit_aset", "nl_jit_access_aset", 4);
    let elt_id =
        declare_helper_call(&mut module, "nelisp_jit_elt", "nl_jit_access_elt", 3);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let length_ptr = module.get_finalized_function(length_id);
    let aref_ptr = module.get_finalized_function(aref_id);
    let aset_ptr = module.get_finalized_function(aset_id);
    let elt_ptr = module.get_finalized_function(elt_id);
    Box::leak(Box::new(module));
    // SAFETY: declared signatures match the function-pointer types.
    unsafe {
        JitAccess {
            length: std::mem::transmute::<_, extern "C" fn(*const Sexp, *mut Sexp) -> i64>(
                length_ptr,
            ),
            aref: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
            >(aref_ptr),
            aset: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64,
            >(aset_ptr),
            elt: std::mem::transmute::<
                _,
                extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
            >(elt_ptr),
        }
    }
}

fn jit() -> &'static JitAccess {
    JIT_ACCESS.get_or_init(build_jit_access)
}

fn lowered_length(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 1 {
        return crate::eval::builtins::dispatch("length", args, env);
    }
    let mut out = Sexp::Nil;
    let r = (jit().length)(&args[0] as *const _, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("length", args, env)
    }
}

fn lowered_aref(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return crate::eval::builtins::dispatch("aref", args, env);
    }
    let idx = match &args[1] {
        Sexp::Int(n) => *n,
        _ => return crate::eval::builtins::dispatch("aref", args, env),
    };
    let mut out = Sexp::Nil;
    let r = (jit().aref)(&args[0] as *const _, idx, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("aref", args, env)
    }
}

fn lowered_aset(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 3 {
        return crate::eval::builtins::dispatch("aset", args, env);
    }
    let idx = match &args[1] {
        Sexp::Int(n) => *n,
        _ => return crate::eval::builtins::dispatch("aset", args, env),
    };
    let mut out = Sexp::Nil;
    let r = (jit().aset)(
        &args[0] as *const _,
        idx,
        &args[2] as *const _,
        &mut out as *mut _,
    );
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("aset", args, env)
    }
}

fn lowered_elt(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return crate::eval::builtins::dispatch("elt", args, env);
    }
    let idx = match &args[1] {
        Sexp::Int(n) => *n,
        _ => return crate::eval::builtins::dispatch("elt", args, env),
    };
    let mut out = Sexp::Nil;
    let r = (jit().elt)(&args[0] as *const _, idx, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        crate::eval::builtins::dispatch("elt", args, env)
    }
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("length", lowered_length);
    map.insert("aref", lowered_aref);
    map.insert("aset", lowered_aset);
    map.insert("elt", lowered_elt);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_length_nil_vector_str() {
        let mut out = Sexp::Nil;

        let nil = Sexp::Nil;
        assert_eq!(
            (jit().length)(&nil as *const _, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(0));

        let vec = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        assert_eq!(
            (jit().length)(&vec as *const _, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(3));

        let s = Sexp::Str("hello".into());
        assert_eq!(
            (jit().length)(&s as *const _, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(5));
    }

    #[test]
    fn jit_length_unsupported_returns_err() {
        let mut out = Sexp::Nil;
        let i = Sexp::Int(42);
        assert_eq!(
            (jit().length)(&i as *const _, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_vector_in_range() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![
            Sexp::Symbol("a".into()),
            Sexp::Symbol("b".into()),
            Sexp::Symbol("c".into()),
        ]);
        assert_eq!(
            (jit().aref)(&vec as *const _, 1, &mut out as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Symbol("b".into()));
    }

    #[test]
    fn jit_aref_out_of_range() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![Sexp::Int(7)]);
        assert_eq!(
            (jit().aref)(&vec as *const _, 5, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_negative_index() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![Sexp::Int(7)]);
        assert_eq!(
            (jit().aref)(&vec as *const _, -1, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_non_vector_returns_err() {
        let mut out = Sexp::Nil;
        let s = Sexp::Str("abc".into());
        assert_eq!(
            (jit().aref)(&s as *const _, 0, &mut out as *mut _),
            TRAMPOLINE_ERR
        );
    }

    // --- Stage 5.6 (2026-05-07) — aset / elt trampolines ---

    #[test]
    fn jit_aset_vector_in_range_mutates() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)]);
        let val = Sexp::Symbol("replaced".into());
        let r = (jit().aset)(
            &v as *const _,
            1,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val);
        // Confirm the mutation through aref.
        let mut got = Sexp::Nil;
        let r = (jit().aref)(&v as *const _, 1, &mut got as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(got, val);
    }

    #[test]
    fn jit_aset_out_of_range_returns_err() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(10)]);
        let val = Sexp::Int(99);
        let r = (jit().aset)(
            &v as *const _,
            5,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_aset_non_vector_returns_err() {
        let mut out = Sexp::Nil;
        let s = Sexp::Str("abc".into());
        let val = Sexp::Int(42);
        let r = (jit().aset)(
            &s as *const _,
            0,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_vector_path() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![
            Sexp::Symbol("x".into()),
            Sexp::Symbol("y".into()),
            Sexp::Symbol("z".into()),
        ]);
        let r = (jit().elt)(&v as *const _, 2, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Symbol("z".into()));
    }

    #[test]
    fn jit_elt_list_walks_to_index() {
        let mut out = Sexp::Nil;
        let lst = Sexp::list_from(&[
            Sexp::Int(1),
            Sexp::Int(2),
            Sexp::Int(3),
            Sexp::Int(4),
        ]);
        let r = (jit().elt)(&lst as *const _, 2, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Int(3));
    }

    #[test]
    fn jit_elt_list_overrun_returns_err() {
        let mut out = Sexp::Nil;
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]);
        let r = (jit().elt)(&lst as *const _, 5, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_nil_returns_err() {
        let mut out = Sexp::Nil;
        let nil = Sexp::Nil;
        let r = (jit().elt)(&nil as *const _, 0, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_negative_index_returns_err() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(1)]);
        let r = (jit().elt)(&v as *const _, -1, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }
}
