//! Phase 5 Stage 5.4 — AccessIR lower scaffold via Rust trampoline.
//!
//! Stage 5.4 (2026-05-07, Doc 62): `length' and `aref' are registered
//! with the lower hook via the same `JITBuilder::symbol' +
//! `Linkage::Import' pattern Stage 5.3 (= ConsIR) used.  Per Doc 62
//! §2.2.4 inline emit of `Vec' element access depends on
//! `Rc<RefCell<Vec<Sexp>>>' layout stability — out of scope for this
//! commit — so the trampolines do the variant match + clone in Rust
//! and the JIT path serves only as the dispatch hook.
//!
//! The trampoline coverage is intentionally narrow:
//! - `length': handles `Sexp::Nil' / `Sexp::Vector' / `Sexp::Str';
//!   `MutStr' / `BoolVector' / `Cons' (spine walk) / others fall
//!   through to `bi_length' for canonical errors / heavy work.
//! - `aref': handles `Sexp::Vector' only (= the most common case);
//!   `Str' / `MutStr' / `CharTable' / `BoolVector' fall through.
//!
//! `aset' / `elt' are not yet wired (= 4-arg / list-walk semantics);
//! they continue to flow through `bi_aset' / `bi_elt'.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift_jit::{JITBuilder, JITModule};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::Sexp;

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

struct JitAccess {
    length: extern "C" fn(*const Sexp, *mut Sexp) -> i64,
    aref: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64,
}

static JIT_ACCESS: OnceLock<JitAccess> = OnceLock::new();

fn build_jit_access() -> JitAccess {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    builder.symbol("nl_jit_access_length", nl_jit_access_length as *const u8);
    builder.symbol("nl_jit_access_aref", nl_jit_access_aref as *const u8);
    let mut module = JITModule::new(builder);

    let length_id =
        declare_helper_call(&mut module, "nelisp_jit_length", "nl_jit_access_length", 2);
    let aref_id =
        declare_helper_call(&mut module, "nelisp_jit_aref", "nl_jit_access_aref", 3);

    module
        .finalize_definitions()
        .expect("cranelift: finalize_definitions");
    let length_ptr = module.get_finalized_function(length_id);
    let aref_ptr = module.get_finalized_function(aref_id);
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

pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    map.insert("length", lowered_length);
    map.insert("aref", lowered_aref);
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
}
