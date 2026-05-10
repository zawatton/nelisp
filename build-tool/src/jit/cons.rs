//! Phase 7.1.6.a.2 (Doc 28 §3.6.a) — cons trampolines, dlsym-exported.
//!
//! Pre-7.1.6.a.2 this module also hosted Cranelift IR builders that
//! wrapped the trampolines with an inline NIL fast-path (= 8-block
//! `cmp tag, TAG_NIL / je nil_path' shape) plus the `JitCons' fn-ptr
//! struct + the `register_symbols' / `declare_funcs' / `collect_funcs'
//! plumbing that the unified JITModule used to bring those wrappers
//! up at first-access (see commit history).
//!
//! Doc 81 Stage 81.4 + Phase 7.1.6.a.1 dlsym precursor (`6666e61')
//! shipped the elisp-side replacement: `nelisp-cc-pipeline--recognize-
//! primitives' rewrites `:call' sites against the `:fn' meta lookup
//! into `:ssa-call-primitive :symbol nl_jit_cons_*' instructions, and
//! the x86_64 / arm64 backends emit a direct CALL fixup whose target
//! address is resolved via `nelisp-cc--dlsym-resolve' against the
//! binary's dynamic symbol table.  The inline-NIL fast path is now
//! emitted by `nelisp-cc-x86_64.el' / `-arm64.el' as host machine
//! code immediately before the CALL — same shape as the deleted
//! `declare_unary_with_nil_inline' Cranelift IR but produced from
//! elisp via `unibyte-string'.
//!
//! Phase 7.1.6.a.2 (this commit) deletes:
//!
//!   - `JitCons' / `ConsIds' fn-ptr structs.
//!   - `declare_unary_with_nil_inline' Cranelift IR builder.
//!   - `register_symbols' / `declare_funcs' / `collect_funcs' wiring
//!     (= `unified_jit()' no longer constructs a cons cluster JIT
//!     wrapper page).
//!
//! What stays (= the surface this module still owns post-7.1.6.a.2):
//!
//!   - The 5 `nl_jit_cons_*' trampolines themselves, now `#[no_mangle]
//!     pub unsafe extern "C"' so the dlsym bridge resolves them.  Body
//!     unchanged from the pre-7.1.6.a.2 version (= tag check on the
//!     `#[repr(C, u8)]' Sexp byte → `cons_box_ptr()' deref → field
//!     clone, `OK = 0' / `ERR = 1' status return).
//!   - `TRAMPOLINE_OK' / `_ERR' constants (= contract anchor).
//!
//! `nelisp-jit-substrate.el' / `nelisp-jit-strategy.el' still call
//! `(nl-jit-call-out-N "nelisp_jit_*" …)' which goes through
//! `bridge::unified_fn_ptr'.  Post-7.1.6.a.2 those names resolve
//! directly to the `nl_jit_cons_*' trampolines — no Cranelift wrapper
//! in between (= one fewer indirection; the inline NIL fast path is
//! handled by the trampoline body's `tag == SEXP_TAG_NIL' arm, which
//! returns `OK' immediately without further work).
//!
//! The `-rdynamic' link flag in `.cargo/config.toml' pushes the 5
//! `#[no_mangle]' symbols into the binary's dynamic symbol table so
//! `dlsym(RTLD_DEFAULT, ...)' can locate them at runtime.  Without
//! `-rdynamic' the symbols are `T' in `nm' but absent from `nm -D',
//! and the dlsym bridge would return `:not-found' uniformly (which
//! the elisp link pass treats as a deferred fallback per Doc 81 §6.3,
//! not an error — but the recognition pass payoff is zero in that
//! configuration).

use crate::eval::sexp::{Sexp, SEXP_TAG_CONS, SEXP_TAG_NIL};

// Phase 7.1.7.a (2026-05-10): narrowed from `pub(super)' to private —
// these constants are read only inside this module after the cluster
// wrapper deletion (Phase 7.1.6.a.2 removed every cross-module caller).
const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

// Phase A.5 (Doc 77c §2.1.4): trampolines dispatch on the
// `#[repr(C, u8)]' tag byte directly via `Sexp::tag()' and read the
// `NlConsBox' pointer at offset 8 via `Sexp::cons_box_ptr()'.  Each arm
// collapses to "tag check → ptr deref → field clone".

/// `(car CELL) -> Sexp' trampoline.  `Nil' is treated as `(car nil)' =
/// `nil' per elisp.  Wrong-type returns `TRAMPOLINE_ERR' so the caller
/// can fall back to the dispatcher's canonical error.
///
/// Phase 7.1.6.a.2 (Doc 28 §3.6.a / Doc 81 §5.4): `#[no_mangle]' so
/// the dlsym bridge (`bi_dlsym_resolve') can locate this symbol at
/// runtime when the recognition pass emits `:ssa-call-primitive
/// :symbol nl_jit_cons_car'.  The dynamic symbol export requires
/// `-rdynamic' (= `.cargo/config.toml' `[build] rustflags').
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

/// `(cdr CELL) -> Sexp' trampoline (Phase 7.1.6.a.2 dlsym-exported).
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
/// Phase 7.1.6.a.2 dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_make(
    a: *const Sexp,
    b: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    *out = Sexp::cons((*a).clone(), (*b).clone());
    TRAMPOLINE_OK
}

/// `(setcar CELL VALUE)' trampoline — mutates the car of a Cons in
/// place through the shared [`NlConsBox`] handle.  Returns VALUE per
/// Emacs' `setcar' contract.  Non-Cons → `TRAMPOLINE_ERR' so the
/// dispatcher can surface the canonical wrong-type error.
/// Phase 7.1.6.a.2 dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_cons_setcar(
    arg: *const Sexp,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if (*arg).tag() != SEXP_TAG_CONS {
        return TRAMPOLINE_ERR;
    }
    // SAFETY: tag-checked Cons above; `cons_box_ptr()' is valid for the
    // lifetime of `*arg' and no live `&Sexp' borrow into the box is
    // observable here.  Phase A.2.1 setcar discipline applies — drop the
    // old car in place then write the new one.
    let box_ptr = (*arg).cons_box_ptr() as *mut crate::eval::nlconsbox::NlConsBox;
    let car_ptr = std::ptr::addr_of_mut!((*box_ptr).car);
    std::ptr::drop_in_place(car_ptr);
    std::ptr::write(car_ptr, (*val).clone());
    *out = (*val).clone();
    TRAMPOLINE_OK
}

/// `(setcdr CELL VALUE)' trampoline (Phase 7.1.6.a.2 dlsym-exported).
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
