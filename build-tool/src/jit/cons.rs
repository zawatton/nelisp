//! Cons trampolines.  `car' / `cdr' / `setcar' / `setcdr' bodies live
//! in `lisp/nelisp-cc-jit-cons.el' (Phase 47-compiled `.o' in
//! `libnelisp_elisp_spike.a'); `bridge::cons_link'-style resolution
//! routes the externs via `unified_fn_ptr' on linux-x86_64 (= the
//! crate's only supported target per `lib.rs:30').
//!
//! `nl_jit_cons_make' stays Rust pending Phase 47 grammar for inline
//! temporary `Sexp' slots.  `nl_cons_{car,cdr}_ptr' are narrow helpers
//! reached from the elisp bodies via `extern-call' (= `*box_ptr()' field
//! address for `nl_sexp_clone_into' to copy refcount-safely).

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;

/// `(cons A B) -> (A . B)' constructor — never wrong-type, always OK.
/// Stays Rust until Phase 47 gains the grammar to allocate temporary
/// Sexp slots inline.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_make(
    a: *const Sexp,
    b: *const Sexp,
    out: *mut Sexp,
) -> i64 {
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
    TRAMPOLINE_OK
}

// Narrow Rust helpers for the Phase 47 elisp bodies in
// `lisp/nelisp-cc-jit-cons.el'.  Return `*const Sexp' at an inline
// `NlConsBox' field; the elisp body composes `nl_sexp_clone_into' for
// the refcount-safe copy.
//
// # Safety
// `arg' must be non-null and point at an initialized `Sexp'.

#[no_mangle]
pub unsafe extern "C" fn nl_cons_car_ptr(arg: *const Sexp) -> *const Sexp {
    match &*arg {
        Sexp::Cons(_) => (*arg).cons_box_ptr() as *const Sexp,
        _ => std::ptr::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_cons_cdr_ptr(arg: *const Sexp) -> *const Sexp {
    match &*arg {
        Sexp::Cons(_) => &(*(*arg).cons_box_ptr()).cdr as *const Sexp,
        _ => std::ptr::null(),
    }
}
