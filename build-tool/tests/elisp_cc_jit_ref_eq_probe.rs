//! Doc 120 §120.A probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/predicate.rs's `nl_jit_ref_eq'
//! trampoline.  ABI: `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
//! writing `Sexp::T' / `Sexp::Nil' into `*out' and returning 0.
//!
//! The elisp body delegates to the `nl_sexp_eq' Rust extern (= thin
//! `#[no_mangle]' wrapper around `eval::special_forms::sexp_eq')
//! through `(extern-call nl_sexp_eq A B)' and then writes the result
//! tag byte into `*out' via `sexp-write-t' / `sexp-write-nil'.
//!
//! Non-linux / non-x86_64 builds skip the test for the same reasons
//! as the predicate_eq probe (= elisp `.o' only emitted on x86_64-
//! linux; Rust trampoline `super::predicate::nl_jit_ref_eq' is
//! still live on other targets through the `predicate_link' stub).

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_ref_eq;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn ref_eq_int_equal_writes_t() {
    let a = Sexp::Int(7);
    let b = Sexp::Int(7);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_ref_eq(&a, &b, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::T));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn ref_eq_int_unequal_writes_nil() {
    let a = Sexp::Int(7);
    let b = Sexp::Int(8);
    let mut out = Sexp::T;
    let rc = unsafe { jit_ref_eq(&a, &b, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn ref_eq_tag_mismatch_writes_nil() {
    // Different tags → sexp_eq returns false → nil.
    let a = Sexp::Int(0);
    let b = Sexp::Float(0.0);
    let mut out = Sexp::T;
    let rc = unsafe { jit_ref_eq(&a, &b, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn ref_eq_nil_t_self() {
    let nil = Sexp::Nil;
    let t = Sexp::T;

    let mut out = Sexp::Nil;
    let rc = unsafe { jit_ref_eq(&nil, &nil, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::T));

    let mut out = Sexp::Nil;
    let rc = unsafe { jit_ref_eq(&t, &t, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::T));

    let mut out = Sexp::T;
    let rc = unsafe { jit_ref_eq(&nil, &t, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn ref_eq_symbol_by_name() {
    let a = Sexp::Symbol("foo".into());
    let b = Sexp::Symbol("foo".into());
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_ref_eq(&a, &b, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::T));

    let c = Sexp::Symbol("bar".into());
    let mut out = Sexp::T;
    let rc = unsafe { jit_ref_eq(&a, &c, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn ref_eq_cons_identity() {
    // Same cons cell → ptr-eq true → T.
    let a = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_ref_eq(&a, &a, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::T));

    // Different cons cells, same content → ptr-eq false → Nil.
    let b = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    let mut out = Sexp::T;
    let rc = unsafe { jit_ref_eq(&a, &b, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_ref_eq_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.A ref-eq probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
