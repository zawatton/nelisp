//! Phase 7.1.6 cluster takeover (Doc 28 §3.6 COMPLETE) — cons trampolines,
//! dlsym-exported.
//!
//! The 5 `nl_jit_cons_*' trampolines below are `#[no_mangle] pub unsafe
//! extern "C"' so the binary's dynamic symbol table exposes them (via the
//! `-rdynamic' link flag in `.cargo/config.toml').  Two callers reach
//! them at runtime: (1) nelisp-cc compiled hot paths via
//! `:ssa-call-primitive' + `nelisp-cc--dlsym-resolve' direct fixup, and
//! (2) `nelisp-jit-substrate.el' / `-strategy.el' via
//! `bridge::unified_fn_ptr's name → fn-ptr table.
//!
//! Each trampoline body is a "tag check on the `#[repr(C, u8)]' Sexp
//! byte → `cons_box_ptr()' deref → field clone" arm with `OK = 0' /
//! `ERR = 1' status return.  The `TAG_NIL' arm in `car' / `cdr' handles
//! the inline NIL fast path that nelisp-cc-emitted code can also
//! short-circuit before the CALL.

use crate::eval::sexp::{Sexp, SEXP_TAG_CONS, SEXP_TAG_NIL};

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// `(car CELL) -> Sexp' trampoline.  `Nil' is treated as `(car nil)' =
/// `nil' per elisp.  Wrong-type returns `TRAMPOLINE_ERR' so the caller
/// can fall back to the dispatcher's canonical error.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_car(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let tag = (*arg).tag();
    if tag == SEXP_TAG_NIL {
        *out = Sexp::Nil;
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_CONS {
        let box_ptr = (*arg).cons_box_ptr();
        *out = (*box_ptr).car.clone();
        return TRAMPOLINE_OK;
    }
    TRAMPOLINE_ERR
}

/// `(cdr CELL) -> Sexp' trampoline.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_cdr(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let tag = (*arg).tag();
    if tag == SEXP_TAG_NIL {
        *out = Sexp::Nil;
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_CONS {
        let box_ptr = (*arg).cons_box_ptr();
        *out = (*box_ptr).cdr.clone();
        return TRAMPOLINE_OK;
    }
    TRAMPOLINE_ERR
}

/// `(cons A B) -> (A . B)' constructor — never wrong-type, always OK.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_make(
    a: *const Sexp,
    b: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    {
        let car_owned = (*a).clone();
        let cdr_owned = (*b).clone();
        crate::elisp_cc_spike::cons_construct(
            &car_owned as *const Sexp,
            &cdr_owned as *const Sexp,
            out,
        );
        // Transfer ownership of the cloned payloads into the copied
        // bytes now stored in `out`'s `NlConsBox`.
        std::mem::forget(car_owned);
        std::mem::forget(cdr_owned);
    }
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    {
        *out = Sexp::cons((*a).clone(), (*b).clone());
    }
    TRAMPOLINE_OK
}

/// `(setcar CELL VALUE)' trampoline — mutates the car of a Cons in
/// place through the shared [`NlConsBox`] handle.  Returns VALUE per
/// Emacs' `setcar' contract.  Non-Cons → `TRAMPOLINE_ERR' so the
/// dispatcher can surface the canonical wrong-type error.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_setcar(
    arg: *const Sexp,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if (*arg).tag() != SEXP_TAG_CONS {
        return TRAMPOLINE_ERR;
    }
    // SAFETY: tag-checked Cons above; `cons_box_ptr()' is valid for
    // the lifetime of `*arg' and no live `&Sexp' borrow into the box
    // is observable here.  Drop the old car in place then write the
    // new one.
    let box_ptr = (*arg).cons_box_ptr() as *mut crate::eval::nlconsbox::NlConsBox;
    let car_ptr = std::ptr::addr_of_mut!((*box_ptr).car);
    std::ptr::drop_in_place(car_ptr);
    std::ptr::write(car_ptr, (*val).clone());
    *out = (*val).clone();
    TRAMPOLINE_OK
}

/// `(setcdr CELL VALUE)' trampoline.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_setcdr(
    arg: *const Sexp,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if (*arg).tag() != SEXP_TAG_CONS {
        return TRAMPOLINE_ERR;
    }
    // SAFETY: see `nl_jit_cons_setcar'.  cdr lives at
    // `offset_of!(NlConsBox, cdr) == sizeof(Sexp)'.
    let box_ptr = (*arg).cons_box_ptr() as *mut crate::eval::nlconsbox::NlConsBox;
    let cdr_ptr = std::ptr::addr_of_mut!((*box_ptr).cdr);
    std::ptr::drop_in_place(cdr_ptr);
    std::ptr::write(cdr_ptr, (*val).clone());
    *out = (*val).clone();
    TRAMPOLINE_OK
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_cons_car_cdr_round_trip() {
        let pair = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let mut out_a = Sexp::Nil;
        let r = unsafe { nl_jit_cons_car(&pair as *const _, &mut out_a as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out_a, Sexp::Int(1));

        let mut out_d = Sexp::Nil;
        let r = unsafe { nl_jit_cons_cdr(&pair as *const _, &mut out_d as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out_d, Sexp::Int(2));
    }

    #[test]
    fn jit_cons_make_constructs_pair() {
        let a = Sexp::Int(7);
        let b = Sexp::Symbol("hello".into());
        let mut out = Sexp::Nil;
        let r = unsafe {
            nl_jit_cons_make(&a as *const _, &b as *const _, &mut out as *mut _)
        };
        assert_eq!(r, TRAMPOLINE_OK);
        // Verify out is a Cons by extracting via cdr trampoline.
        let mut out_d = Sexp::Nil;
        let r = unsafe { nl_jit_cons_cdr(&out as *const _, &mut out_d as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out_d, b);
    }

    #[test]
    fn jit_cons_car_wrong_type_returns_err() {
        let i = Sexp::Int(42);
        let mut out = Sexp::Nil;
        let r = unsafe { nl_jit_cons_car(&i as *const _, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_cons_car_nil_returns_nil() {
        let nil = Sexp::Nil;
        let mut out = Sexp::Int(99);
        let r = unsafe { nl_jit_cons_car(&nil as *const _, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }

    #[test]
    fn jit_cons_cdr_nil_returns_nil() {
        let nil = Sexp::Nil;
        let mut out = Sexp::Int(99);
        let r = unsafe { nl_jit_cons_cdr(&nil as *const _, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }

    #[test]
    fn jit_setcar_mutates_in_place() {
        let pair = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let val = Sexp::Symbol("new-head".into());
        let mut out = Sexp::Nil;
        let r = unsafe {
            nl_jit_cons_setcar(&pair as *const _, &val as *const _, &mut out as *mut _)
        };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val);
        let mut got_car = Sexp::Nil;
        let r = unsafe { nl_jit_cons_car(&pair as *const _, &mut got_car as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(got_car, val);
    }

    #[test]
    fn jit_setcdr_mutates_in_place() {
        let pair = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let val = Sexp::Str("new-tail".into());
        let mut out = Sexp::Nil;
        let r = unsafe {
            nl_jit_cons_setcdr(&pair as *const _, &val as *const _, &mut out as *mut _)
        };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val);
        let mut got_cdr = Sexp::Nil;
        let r = unsafe { nl_jit_cons_cdr(&pair as *const _, &mut got_cdr as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(got_cdr, val);
    }

    #[test]
    fn jit_setcar_wrong_type_returns_err() {
        let i = Sexp::Int(42);
        let val = Sexp::Nil;
        let mut out = Sexp::Nil;
        let r = unsafe {
            nl_jit_cons_setcar(&i as *const _, &val as *const _, &mut out as *mut _)
        };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_setcdr_wrong_type_returns_err() {
        let i = Sexp::Int(42);
        let val = Sexp::Nil;
        let mut out = Sexp::Nil;
        let r = unsafe {
            nl_jit_cons_setcdr(&i as *const _, &val as *const _, &mut out as *mut _)
        };
        assert_eq!(r, TRAMPOLINE_ERR);
    }
}
