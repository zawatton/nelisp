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
//! - `aref': handles `Sexp::Vector' + `Sexp::BoolVector' (= Doc 77
//!   Stage 1.B, 2026-05-09); `Str' / `MutStr' / `CharTable' fall
//!   through to `aref_helper'.
//!
//! Stage 5.6 (2026-05-07) — `aset' and `elt' added on the same
//! `declare_helper_call' shape:
//! - `aset': `Vector' + `BoolVector' fast path (= Doc 77 Stage 1.B,
//!   2026-05-09).  `MutStr' (= codepoint mutation that rebuilds the
//!   underlying String) stays in the dispatcher because the 4-arg
//!   helper would not save anything.
//! - `elt': `Vector' (= aref) + `Cons' (= list walk) fast path.  Other
//!   sequence types fall through.

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::sexp::{
    Sexp, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CONS, SEXP_TAG_INT, SEXP_TAG_NIL, SEXP_TAG_STR,
    SEXP_TAG_VECTOR,
};

use super::declare_helper_call;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

// Phase A.5 (Doc 77c §2.1.4): trampolines dispatch on `Sexp::tag()' and
// read box pointers via `Sexp::*_box_ptr()' instead of `match'.  Each
// arm collapses to "tag check → ptr deref → field op", which mirrors
// the cranelift IR shape that the JIT entry will eventually inline.

/// `(length OBJ)' fast path: `Nil' / `Vector' / `Str'.  Other types
/// return `TRAMPOLINE_ERR' so the caller falls through to `bi_length'.
unsafe extern "C" fn nl_jit_access_length(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let tag = (*arg).tag();
    if tag == SEXP_TAG_NIL {
        *out = Sexp::Int(0);
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_VECTOR {
        let box_ref = &*(*arg).vector_box_ptr();
        *out = Sexp::Int(box_ref.value.len() as i64);
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_STR {
        // Str is `String' inline at offset 8; the simplest correct read
        // is through the match arm (= no separate box).
        if let Sexp::Str(s) = &*arg {
            *out = Sexp::Int(s.chars().count() as i64);
            return TRAMPOLINE_OK;
        }
    }
    TRAMPOLINE_ERR
}

/// `(aref VECTOR INDEX)' fast path: `Sexp::Vector' / `Sexp::BoolVector'
/// with non-negative INDEX in range.  Out-of-range / wrong-type /
/// negative index returns `TRAMPOLINE_ERR' for canonical-error fall-
/// through (= the dispatcher's `aref_helper' surfaces the proper
/// out-of-range / wrong-type message).
unsafe extern "C" fn nl_jit_access_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let tag = (*arg).tag();
    if tag == SEXP_TAG_VECTOR {
        let box_ref = &*(*arg).vector_box_ptr();
        if let Some(elem) = box_ref.value.get(idx as usize) {
            *out = elem.clone();
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
    }
    if tag == SEXP_TAG_BOOL_VECTOR {
        let box_ref = &*(*arg).bool_vector_box_ptr();
        if let Some(b) = box_ref.value.get(idx as usize) {
            *out = if *b { Sexp::T } else { Sexp::Nil };
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
    }
    TRAMPOLINE_ERR
}

/// `(aset VECTOR INDEX VALUE)' fast path: `Sexp::Vector' /
/// `Sexp::BoolVector'.  Returns VALUE per Emacs' `aset' contract.
/// `MutStr' aset (= codepoint mutation) is left to the dispatcher
/// because the rebuild-String path is not worth a JIT helper.
unsafe extern "C" fn nl_jit_access_aset(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let tag = (*arg).tag();
    if tag == SEXP_TAG_VECTOR {
        let box_ptr = (*arg).vector_box_ptr() as *mut crate::eval::nlvector::NlVector;
        let len = (&*box_ptr).value.len();
        if (idx as usize) >= len {
            return TRAMPOLINE_ERR;
        }
        // SAFETY: Phase A.4.3 — bounds-checked, no other `&Vec<Sexp>'
        // borrow live.  Phase A.2.1 setcar discipline applies.
        let value_ref = &mut (*box_ptr).value;
        value_ref[idx as usize] = (*val).clone();
        *out = (*val).clone();
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_BOOL_VECTOR {
        let box_ptr = (*arg).bool_vector_box_ptr()
            as *mut crate::eval::nlboolvector::NlBoolVector;
        let len = (&*box_ptr).value.len();
        if (idx as usize) >= len {
            return TRAMPOLINE_ERR;
        }
        let bit = crate::eval::special_forms::is_truthy(&*val);
        // SAFETY: Phase A.4.4 — same discipline as the Vector arm above.
        let value_ref = &mut (*box_ptr).value;
        value_ref[idx as usize] = bit;
        *out = (*val).clone();
        return TRAMPOLINE_OK;
    }
    TRAMPOLINE_ERR
}

/// `(elt SEQUENCE INDEX)' fast path: `Sexp::Vector' (= aref) /
/// `Sexp::Cons' (= list walk).  Other sequence types fall through.
unsafe extern "C" fn nl_jit_access_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let tag = (*arg).tag();
    if tag == SEXP_TAG_VECTOR {
        let box_ref = &*(*arg).vector_box_ptr();
        if let Some(elem) = box_ref.value.get(idx as usize) {
            *out = elem.clone();
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
    }
    if tag == SEXP_TAG_CONS {
        let mut cur_ptr: *const Sexp = arg;
        let mut remaining = idx;
        loop {
            let cur_tag = (*cur_ptr).tag();
            if cur_tag == SEXP_TAG_CONS {
                let box_ref = &*(*cur_ptr).cons_box_ptr();
                if remaining == 0 {
                    *out = box_ref.car.clone();
                    return TRAMPOLINE_OK;
                }
                remaining -= 1;
                cur_ptr = std::ptr::addr_of!(box_ref.cdr);
                continue;
            }
            return TRAMPOLINE_ERR;
        }
    }
    TRAMPOLINE_ERR
}

pub(super) struct JitAccess {
    pub(super) length: extern "C" fn(*const Sexp, *mut Sexp) -> i64,
    pub(super) aref: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
    pub(super) aset: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64,
    pub(super) elt: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
}

pub(super) struct AccessIds {
    length: FuncId,
    aref: FuncId,
    aset: FuncId,
    elt: FuncId,
}

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

/// Doc 77 Stage 2-prep (2026-05-09): submodule-level helper that
/// registers all imported `nl_jit_access_*' symbols on the shared
/// JITBuilder.
pub(super) fn register_symbols(builder: &mut JITBuilder) {
    builder.symbol("nl_jit_access_length", nl_jit_access_length as *const u8);
    builder.symbol("nl_jit_access_aref", nl_jit_access_aref as *const u8);
    builder.symbol("nl_jit_access_aset", nl_jit_access_aset as *const u8);
    builder.symbol("nl_jit_access_elt", nl_jit_access_elt as *const u8);
}

/// Doc 77 Stage 2-prep: declare + define every JIT entry this module
/// owns on the *shared* JITModule.
pub(super) fn declare_funcs(module: &mut JITModule) -> AccessIds {
    let length = declare_length_with_inline_nil(module);
    let aref = declare_helper_call(module, "nelisp_jit_aref", "nl_jit_access_aref", 3);
    let aset = declare_helper_call(module, "nelisp_jit_aset", "nl_jit_access_aset", 4);
    let elt = declare_helper_call(module, "nelisp_jit_elt", "nl_jit_access_elt", 3);
    AccessIds {
        length,
        aref,
        aset,
        elt,
    }
}

/// Doc 77 Stage 2-prep: fetch finalized function pointers post-
/// `finalize_definitions' and pack them into `JitAccess'.
pub(super) fn collect_funcs(module: &JITModule, ids: AccessIds) -> JitAccess {
    let length_ptr = module.get_finalized_function(ids.length);
    let aref_ptr = module.get_finalized_function(ids.aref);
    let aset_ptr = module.get_finalized_function(ids.aset);
    let elt_ptr = module.get_finalized_function(ids.elt);
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

// Doc 77b Stage b.4 (2026-05-09) — `lowered_X' Rust strategy fns
// + `register(map)' + `aref_helper' deleted.  The 4 entries (=
// length/aref/aset/elt) are now driven by elisp wrappers in
// `lisp/nelisp-jit-strategy.el' that call the JIT trampolines
// through the Stage b.2.5 `nl-jit-call-out-{1,1i,2i}' bridge
// primitives + `jit/strategy.rs' multi-variant fall-through helpers
// (`bi_length_impl' / `bi_aref_impl' / `bi_aset_impl' /
// `bi_elt_impl').  The Rust trampolines (`nl_jit_access_*') stay in
// this module, as do the IR builders + the `JitAccess' fn-ptr
// struct reachable via `super::unified_jit()'.

#[cfg(test)]
fn jit() -> &'static JitAccess {
    &super::unified_jit().access
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

    // --- Doc 77 Stage 1.B (2026-05-09) — BoolVector trampoline coverage ---

    #[test]
    fn jit_aref_bool_vector_in_range() {
        // bv = [false, true, false] → aref(bv, 0) = Nil, aref(bv, 1) = T.
        let bv = Sexp::bool_vector(3, false);
        if let Sexp::BoolVector(rc) = &bv {
            // SAFETY: no other borrow live in this test setup.
            unsafe {
                rc.with_value_mut(|v| v[1] = true);
            }
        } else {
            panic!("bool_vector did not produce BoolVector");
        }
        let mut out = Sexp::Nil;
        let r = (jit().aref)(&bv as *const _, 0, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
        let r = (jit().aref)(&bv as *const _, 1, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::T);
        let r = (jit().aref)(&bv as *const _, 2, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }

    #[test]
    fn jit_aref_bool_vector_out_of_range() {
        let bv = Sexp::bool_vector(2, true);
        let mut out = Sexp::Nil;
        let r = (jit().aref)(&bv as *const _, 5, &mut out as *mut _);
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_aset_bool_vector_in_range_mutates() {
        // bv = [true, true, true] → aset(bv, 1, nil) flips slot 1 to false.
        let bv = Sexp::bool_vector(3, true);
        let val_nil = Sexp::Nil;
        let mut out = Sexp::T;
        let r = (jit().aset)(
            &bv as *const _,
            1,
            &val_nil as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
        // Confirm the mutation: aref returns Nil at slot 1, T elsewhere.
        let mut probe = Sexp::Nil;
        assert_eq!(
            (jit().aref)(&bv as *const _, 0, &mut probe as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::T);
        assert_eq!(
            (jit().aref)(&bv as *const _, 1, &mut probe as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::Nil);
        // Truthy non-Nil value sets slot to true.
        let val_int = Sexp::Int(42);
        let r = (jit().aset)(
            &bv as *const _,
            1,
            &val_int as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val_int);
        assert_eq!(
            (jit().aref)(&bv as *const _, 1, &mut probe as *mut _),
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::T);
    }

    #[test]
    fn jit_aset_bool_vector_out_of_range_returns_err() {
        let bv = Sexp::bool_vector(2, false);
        let val = Sexp::T;
        let mut out = Sexp::Nil;
        let r = (jit().aset)(
            &bv as *const _,
            5,
            &val as *const _,
            &mut out as *mut _,
        );
        assert_eq!(r, TRAMPOLINE_ERR);
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
