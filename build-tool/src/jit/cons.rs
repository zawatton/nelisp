//! Remaining Rust cons helpers for the JIT bridge.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;

/// `cons` constructor; still Rust because Phase 47 has no temp `Sexp` slots.
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
    std::mem::forget(car_owned);
    std::mem::forget(cdr_owned);
    TRAMPOLINE_OK
}

// `nl_cons_car_ptr' and `nl_cons_cdr_ptr' moved to Phase 47 elisp:
//   lisp/nelisp-cc-jit-cons-car-ptr.el  (car_ptr = sexp-payload-ptr)
//   lisp/nelisp-cc-jit-cons-cdr-ptr.el  (cdr_ptr = sexp-payload-ptr + 32)
