//! Phase 5 Stage 5.5 — PredicateIR lower scaffold via Rust trampoline.
//!
//! Stage 5.5 (2026-05-07, Doc 62): `eq' is registered with the lower
//! hook via the same `JITBuilder::symbol' + `Linkage::Import' pattern
//! Stage 5.3 / 5.4 used.  Per Doc 62 §2.2.5 the inline tag-byte test
//! (= `movzx tag; cmp; sete') depends on `Sexp' having a stable
//! `#[repr]'-pinned discriminant offset, which the substrate doesn't
//! commit to today; the lower entry routes the happy path through a
//! call to a Rust trampoline that wraps `sexp_eq' and returns `i64'
//! (`1' = equal, `0' = not equal).
//!
//! Other predicates listed in §2.2.5 (= `consp', `listp', `null',
//! `integerp', `stringp', `symbolp', `numberp', `floatp', `vectorp',
//! `atom') have already migrated to elisp on top of `nelisp--type-of'
//! (Rust-min batch 6u, 2026-05-06) so they no longer have a Rust
//! dispatcher arm to lower.  The Stage 5.5 scaffold therefore only
//! covers the residual Rust-side predicate, `eq'.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift_jit::{JITBuilder, JITModule};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::Sexp;

use super::{declare_helper_call, LowerFn};

/// `(eq A B) -> 1 if equal, 0 otherwise' trampoline.  Wraps the
/// existing `special_forms::sexp_eq' so behavior is byte-identical to
/// `bi_eq'.
unsafe extern "C" fn nl_jit_pred_eq(a: *const Sexp, b: *const Sexp) -> i64 {
    if crate::eval::special_forms::sexp_eq(&*a, &*b) {
        1
    } else {
        0
    }
}

struct JitPredicate {
    eq: extern "C" fn(*const Sexp, *const Sexp) -> i64,
}

static JIT_PREDICATE: OnceLock<JitPredicate> = OnceLock::new();

fn build_jit_predicate() -> JitPredicate {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    builder.symbol("nl_jit_pred_eq", nl_jit_pred_eq as *const u8);
    let mut module = JITModule::new(builder);

    let eq_id = declare_helper_call(&mut module, "nelisp_jit_eq", "nl_jit_pred_eq", 2);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let eq_ptr = module.get_finalized_function(eq_id);
    Box::leak(Box::new(module));
    // SAFETY: declared signature matches the function-pointer type.
    unsafe {
        JitPredicate {
            eq: std::mem::transmute::<_, extern "C" fn(*const Sexp, *const Sexp) -> i64>(
                eq_ptr,
            ),
        }
    }
}

fn jit() -> &'static JitPredicate {
    JIT_PREDICATE.get_or_init(build_jit_predicate)
}

fn lowered_eq(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return crate::eval::builtins::dispatch("eq", args, env);
    }
    let v = (jit().eq)(&args[0] as *const _, &args[1] as *const _);
    Ok(if v != 0 { Sexp::T } else { Sexp::Nil })
}

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("eq", lowered_eq);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_eq_int_equal() {
        let a = Sexp::Int(7);
        let b = Sexp::Int(7);
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 1);
    }

    #[test]
    fn jit_eq_int_unequal() {
        let a = Sexp::Int(7);
        let b = Sexp::Int(8);
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 0);
    }

    #[test]
    fn jit_eq_symbol_by_name() {
        let a = Sexp::Symbol("foo".into());
        let b = Sexp::Symbol("foo".into());
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 1);
    }

    #[test]
    fn jit_eq_nil_t() {
        let nil = Sexp::Nil;
        let t = Sexp::T;
        assert_eq!((jit().eq)(&nil as *const _, &nil as *const _), 1);
        assert_eq!((jit().eq)(&t as *const _, &t as *const _), 1);
        assert_eq!((jit().eq)(&nil as *const _, &t as *const _), 0);
    }

    #[test]
    fn jit_eq_cons_identity_not_value() {
        // Two separately-constructed cons cells with same value are
        // NOT eq (= identity check via Rc::ptr_eq).
        let a = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let b = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        assert_eq!((jit().eq)(&a as *const _, &b as *const _), 0);
        // But the same cell IS eq with itself.
        assert_eq!((jit().eq)(&a as *const _, &a as *const _), 1);
    }
}
