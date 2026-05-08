//! Phase 5 Stage 5.5 — PredicateIR lower (= inline tag test for `eq').
//!
//! Stage 5.5 (2026-05-07, Doc 62, repr-pin commit 2fb64cd):
//! - Initial scaffold (commit 5299e44) routed `eq' through a Rust
//!   trampoline that wrapped `special_forms::sexp_eq', exercising the
//!   lower hook but adding a JIT call hop with no perf gain.
//! - This commit adds an inline tag-byte fast path on top of the
//!   trampoline now that `Sexp' has `#[repr(C, u8)]' (= discriminant
//!   byte at offset 0, payload at offset 8 with 8-byte alignment):
//!
//!   ```text
//!   jit_eq(a_ptr, b_ptr):
//!     if a_ptr == b_ptr     → return 1       (= same Sexp ref, Doc 77 Stage 1)
//!     a_tag = movzx u8 [a_ptr + 0]
//!     b_tag = movzx u8 [b_ptr + 0]
//!     if a_tag != b_tag    → return 0       (= different variants)
//!     if a_tag == TAG_INT  → return cmp i64 [a_ptr+8] vs [b_ptr+8]
//!     else                  → call helper (= sexp_eq trampoline)
//!   ```
//!
//! Doc 77 Stage 1 (2026-05-09) — eq same-ref short-circuit:
//! prepends a pointer-equality check before the tag-byte load.  When
//! `a_ptr == b_ptr' (= same `Sexp' struct address) the two are
//! definitionally `eq' in every variant, so the JIT can return 1
//! immediately without loading the tag or entering the helper.  Saves
//! 4 host instructions on `(eq x x)' and any case where a Sexp ref is
//! compared to itself (= idiom in pcase / cl-typecase guard chains).
//!
//! Net effect: `(eq INT INT)' is now ~3 host instructions (load, load,
//! cmp + uextend) instead of a full `sexp_eq' match dispatch, and any
//! tag-mismatch comparison short-circuits without entering the helper.
//! Other variant pairings (Nil/T/Symbol/Str/Cons/Vector/etc) still
//! flow through the helper for variant-specific equality semantics.
//!
//! Other predicates listed in Doc 62 §2.2.5 (= `consp', `listp', `null',
//! `integerp', `stringp', `symbolp', `numberp', `floatp', `vectorp',
//! `atom') have already migrated to elisp on top of `nelisp--type-of'
//! (Rust-min batch 6u, 2026-05-06) so they no longer have a Rust
//! dispatcher arm to lower.  Stage 5.5 covers the residual Rust-side
//! predicate, `eq'.

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use crate::eval::sexp::{Sexp, SEXP_TAG_INT};

/// `(eq A B) -> 1 if equal, 0 otherwise' helper trampoline used by
/// the JIT entry's slow path.  Wraps the existing
/// `special_forms::sexp_eq' so behavior is byte-identical to `bi_eq'.
unsafe extern "C" fn nl_jit_pred_eq(a: *const Sexp, b: *const Sexp) -> i64 {
    if crate::eval::special_forms::sexp_eq(&*a, &*b) {
        1
    } else {
        0
    }
}

pub(super) struct JitPredicate {
    pub(super) eq: extern "C" fn(*const Sexp, *const Sexp) -> i64,
}

pub(super) struct PredicateIds {
    eq: FuncId,
}

/// Build the `eq' JIT entry with inline tag-byte fast paths.  The
/// shape is:
///
/// ```text
///   block_entry(a_ptr, b_ptr):
///     same_ref = icmp Equal, a_ptr, b_ptr
///     brif same_ref, block_same, block_load_tags
///   block_same:                    (= Doc 77 Stage 1 short-circuit)
///     return iconst.i64 1
///   block_load_tags:
///     a_tag = uextend.i64 (load.i8 [a_ptr+0])
///     b_tag = uextend.i64 (load.i8 [b_ptr+0])
///     tags_eq = icmp Equal, a_tag, b_tag
///     brif tags_eq, block_match, block_diff
///   block_diff:
///     return iconst.i64 0
///   block_match:
///     is_int = icmp_imm Equal, a_tag, SEXP_TAG_INT
///     brif is_int, block_int_eq, block_slow
///   block_int_eq:
///     a_int = load.i64 [a_ptr+8]
///     b_int = load.i64 [b_ptr+8]
///     eq = icmp Equal, a_int, b_int
///     return uextend.i64 eq
///   block_slow:
///     ret = call helper(a_ptr, b_ptr)
///     return ret
/// ```
fn declare_eq_inline(module: &mut JITModule) -> FuncId {
    // Imported helper = `nl_jit_pred_eq(*const Sexp, *const Sexp) -> i64'.
    let mut helper_sig = module.make_signature();
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.params.push(AbiParam::new(types::I64));
    helper_sig.returns.push(AbiParam::new(types::I64));
    let helper_id = module
        .declare_function("nl_jit_pred_eq", Linkage::Import, &helper_sig)
        .expect("cranelift: declare_function nl_jit_pred_eq");

    // JIT entry = same i64 × 2 → i64 shape.
    let mut entry_sig = module.make_signature();
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.params.push(AbiParam::new(types::I64));
    entry_sig.returns.push(AbiParam::new(types::I64));
    let entry_id = module
        .declare_function("nelisp_jit_eq_inline", Linkage::Local, &entry_sig)
        .expect("cranelift: declare_function nelisp_jit_eq_inline");

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;

    let mut fbcx = FunctionBuilderContext::new();
    {
        let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbcx);
        let entry_b = fb.create_block();
        let same_b = fb.create_block();
        let load_tags_b = fb.create_block();
        let diff_b = fb.create_block();
        let match_b = fb.create_block();
        let int_eq_b = fb.create_block();
        let slow_b = fb.create_block();
        fb.append_block_params_for_function_params(entry_b);

        // Entry: short-circuit if a_ptr == b_ptr (Doc 77 Stage 1).
        fb.switch_to_block(entry_b);
        let a_ptr = fb.block_params(entry_b)[0];
        let b_ptr = fb.block_params(entry_b)[1];
        let same_ref = fb.ins().icmp(IntCC::Equal, a_ptr, b_ptr);
        fb.ins().brif(same_ref, same_b, &[], load_tags_b, &[]);
        fb.seal_block(entry_b);

        // Same Sexp ref → guaranteed eq (every variant).
        fb.switch_to_block(same_b);
        let one = fb.ins().iconst(types::I64, 1);
        fb.ins().return_(&[one]);
        fb.seal_block(same_b);

        // Different refs: load tag bytes from both args + branch.
        fb.switch_to_block(load_tags_b);
        let flags = MemFlags::trusted();
        let a_tag_byte = fb.ins().load(types::I8, flags, a_ptr, 0);
        let b_tag_byte = fb.ins().load(types::I8, flags, b_ptr, 0);
        let a_tag = fb.ins().uextend(types::I64, a_tag_byte);
        let b_tag = fb.ins().uextend(types::I64, b_tag_byte);
        let tags_eq = fb.ins().icmp(IntCC::Equal, a_tag, b_tag);
        fb.ins().brif(tags_eq, match_b, &[], diff_b, &[]);
        fb.seal_block(load_tags_b);

        // Tags differ → return 0 (= guaranteed not eq across variants).
        fb.switch_to_block(diff_b);
        let zero = fb.ins().iconst(types::I64, 0);
        fb.ins().return_(&[zero]);
        fb.seal_block(diff_b);

        // Tags match → check if Int (= cheap fast path) else slow.
        fb.switch_to_block(match_b);
        let is_int = fb
            .ins()
            .icmp_imm(IntCC::Equal, a_tag, SEXP_TAG_INT as i64);
        fb.ins().brif(is_int, int_eq_b, &[], slow_b, &[]);
        fb.seal_block(match_b);

        // Int fast path: load i64 payload at offset 8, cmp.
        fb.switch_to_block(int_eq_b);
        let a_int = fb.ins().load(types::I64, flags, a_ptr, 8);
        let b_int = fb.ins().load(types::I64, flags, b_ptr, 8);
        let int_eq = fb.ins().icmp(IntCC::Equal, a_int, b_int);
        let int_result = fb.ins().uextend(types::I64, int_eq);
        fb.ins().return_(&[int_result]);
        fb.seal_block(int_eq_b);

        // Slow path: call helper for variant-specific equality.
        fb.switch_to_block(slow_b);
        let helper_local = module.declare_func_in_func(helper_id, fb.func);
        let inst = fb.ins().call(helper_local, &[a_ptr, b_ptr]);
        let helper_result = fb.inst_results(inst)[0];
        fb.ins().return_(&[helper_result]);
        fb.seal_block(slow_b);

        fb.finalize();
    }

    module
        .define_function(entry_id, &mut ctx)
        .expect("cranelift: define_function nelisp_jit_eq_inline");
    module.clear_context(&mut ctx);
    entry_id
}

/// Doc 77 Stage 2-prep (2026-05-09): submodule-level helper that
/// registers all imported `nl_jit_pred_*' symbols on the shared
/// JITBuilder.  Called by `super::unified_jit' before the JITModule
/// is constructed so `Linkage::Import' resolves at finalize time.
pub(super) fn register_symbols(builder: &mut JITBuilder) {
    builder.symbol("nl_jit_pred_eq", nl_jit_pred_eq as *const u8);
}

/// Doc 77 Stage 2-prep: declare + define every JIT entry this module
/// owns on the *shared* JITModule, returning their FuncIds for later
/// `collect_funcs' lookup.  Mirrors the body of the old per-module
/// `build_jit_predicate' minus the JITBuilder/JITModule bring-up and
/// the post-finalize symbol fetch.
pub(super) fn declare_funcs(module: &mut JITModule) -> PredicateIds {
    let eq = declare_eq_inline(module);
    PredicateIds { eq }
}

/// Doc 77 Stage 2-prep: after `module.finalize_definitions()`, fetch
/// the executable-page function pointers for every entry declared by
/// `declare_funcs' and pack them into `JitPredicate'.
pub(super) fn collect_funcs(module: &JITModule, ids: PredicateIds) -> JitPredicate {
    let eq_ptr = module.get_finalized_function(ids.eq);
    // SAFETY: declared signature matches the function-pointer type.
    unsafe {
        JitPredicate {
            eq: std::mem::transmute::<_, extern "C" fn(*const Sexp, *const Sexp) -> i64>(
                eq_ptr,
            ),
        }
    }
}

// Doc 77b Stage b.4 (2026-05-09) — `lowered_eq' Rust strategy fn
// + `register(map)' deleted.  The `eq' entry is now driven by an
// elisp wrapper in `lisp/nelisp-jit-strategy.el' that calls the
// `nelisp_jit_eq_inline' JIT entry through the Stage b.2
// `nl-jit-call-ptr-ptr' bridge primitive.  The Rust trampoline
// (`nl_jit_pred_eq') stays in this module, as does the IR builder +
// the `JitPredicate' fn-ptr struct reachable via
// `super::unified_jit()'.

#[cfg(test)]
fn jit() -> &'static JitPredicate {
    &super::unified_jit().predicate
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Inline fast paths ---

    #[test]
    fn jit_eq_int_equal_inline() {
        // Same Int → JIT inline int_eq_b returns 1.
        let a = Sexp::Int(7);
        let b = Sexp::Int(7);
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 1);
    }

    #[test]
    fn jit_eq_int_unequal_inline() {
        // Different Int → JIT inline int_eq_b returns 0.
        let a = Sexp::Int(7);
        let b = Sexp::Int(8);
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 0);
    }

    #[test]
    fn jit_eq_tag_mismatch_short_circuits() {
        // Different tags → JIT inline diff_b returns 0 without entering
        // the helper.  Verifies the Sexp::Int vs Sexp::Float pairing
        // (= same payload size, different tag) routes to the diff_b
        // path, not the int_eq_b fast path.
        let a = Sexp::Int(0);
        let b = Sexp::Float(0.0);
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 0);
    }

    #[test]
    fn jit_eq_nil_t_via_helper() {
        // Nil/T have matching tags but no payload — the helper handles
        // this through `sexp_eq''s `(Nil, Nil) | (T, T) => true' arm.
        let nil = Sexp::Nil;
        let t = Sexp::T;
        assert_eq!((jit().eq)(&nil as *const _, &nil as *const _), 1);
        assert_eq!((jit().eq)(&t as *const _, &t as *const _), 1);
        // Mismatched tags → diff_b inline.
        assert_eq!((jit().eq)(&nil as *const _, &t as *const _), 0);
    }

    // --- Slow paths via helper ---

    #[test]
    fn jit_eq_symbol_by_name_via_helper() {
        // Symbol matches via helper's name-equality arm.
        let a = Sexp::Symbol("foo".into());
        let b = Sexp::Symbol("foo".into());
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 1);
    }

    #[test]
    fn jit_eq_cons_identity_via_helper() {
        // Two separately-constructed cons cells with same value are
        // NOT eq (= identity check via Rc::ptr_eq inside the helper,
        // reached after both same-ref check and tag-equal branches).
        let a = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let b = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 0);
        // The same cell IS eq with itself — Doc 77 Stage 1 short-circuit
        // returns 1 from `same_b' before any helper call.
        assert_eq!((jit().eq)(&a as *const _, &a as *const _), 1);
    }

    // --- Doc 77 Stage 1: eq same-ref short-circuit ---

    #[test]
    fn jit_eq_same_ref_short_circuit() {
        // For every variant, comparing a Sexp ref to itself must
        // return 1 without entering the helper.  Pre-Doc-77 this
        // worked for Int via int_eq_b and for non-Int variants only
        // because the helper happened to handle the same-ref case;
        // now it is handled by the inline `same_b' branch up front.
        let int = Sexp::Int(42);
        let flt = Sexp::Float(3.14);
        let nil = Sexp::Nil;
        let t = Sexp::T;
        let sym = Sexp::Symbol("x".into());
        let s = Sexp::Str("hello".into());
        let cons = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        for r in [&int, &flt, &nil, &t, &sym, &s, &cons] {
            let p = r as *const Sexp;
            assert_eq!((jit().eq)(p, p), 1);
        }
    }
}
