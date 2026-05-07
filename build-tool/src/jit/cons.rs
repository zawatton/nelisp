//! Phase 5 Stage 5.3 — ConsIR lower scaffold via Rust trampoline.
//!
//! Stage 5.3 (2026-05-07, Doc 62): `car' / `cdr' / `cons' are
//! registered as JIT-lowered primitives via the same `JITBuilder::
//! symbol' + `Linkage::Import' pattern Stage 5.1 used for the generic
//! syscall.  Per Doc 62 §5 "helper fn 経由の妥協を許容", inline emit
//! of `Rc<RefCell<Sexp>>' field access requires Sexp `#[repr]'
//! stabilization which is out-of-scope for this commit; the Stage 5.3
//! lower entries route the happy-path through a JIT-declared call to
//! a non-variadic Rust trampoline that does the actual variant match
//! + clone, with the dispatcher remaining the canonical-error fallback
//! for arity / wrong-type cases.
//!
//! The trampoline ABI is uniform: each helper takes input Sexp(s) by
//! `*const Sexp' and writes the result to an `*mut Sexp' out-param,
//! returning a status code (`TRAMPOLINE_OK = 0' on success, `_ERR = 1'
//! on wrong-type so the caller falls through to the dispatcher for
//! the canonical `wrong-type-argument' message).  Cranelift sees both
//! pointer and result as `i64', identical to Stage 5.1 syscall layout.
//!
//! Net perf gain over the dispatcher path is intentionally ~zero (the
//! trampoline does the same work `bi_car' did, plus a JIT call hop);
//! this stage closes the IR-family scaffold so Stage 5.0〜5.5 are all
//! exercising the lower hook and ConsIR can be progressively replaced
//! with inline emit once Sexp gets a stable repr.

use std::collections::HashMap;
use std::sync::OnceLock;

use cranelift_jit::{JITBuilder, JITModule};

use crate::eval::env::Env;
use crate::eval::error::EvalError;
use crate::reader::sexp::Sexp;

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

fn build_jit_cons() -> JitCons {
    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
        .expect("cranelift_jit: host ISA must resolve");
    builder.symbol("nl_jit_cons_car", nl_jit_cons_car as *const u8);
    builder.symbol("nl_jit_cons_cdr", nl_jit_cons_cdr as *const u8);
    builder.symbol("nl_jit_cons_make", nl_jit_cons_make as *const u8);
    let mut module = JITModule::new(builder);

    let car_id = declare_helper_call(&mut module, "nelisp_jit_car", "nl_jit_cons_car", 2);
    let cdr_id = declare_helper_call(&mut module, "nelisp_jit_cdr", "nl_jit_cons_cdr", 2);
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
