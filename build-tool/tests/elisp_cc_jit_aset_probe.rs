//! Doc 120 §120.D probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/access.rs's `nl_jit_access_
//! aset' trampoline.  ABI: `(*const Sexp, i64, *const Sexp, *mut
//! Sexp) -> i64' returning 0 on OK + mutation applied / `*out =
//! clone(*val)', 1 on ERR (negative idx / OOR / non-array tag).

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::{jit_aref, jit_aset};
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aset_ok_vector_in_range_mutates() {
    let v = Sexp::vector(vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)]);
    let val = Sexp::Symbol("replaced".into());
    let mut out = Sexp::Nil;
    let r = unsafe { jit_aset(&v, 1, &val, &mut out) };
    assert_eq!(r, 0);
    assert_eq!(out, val);
    // Probe via aref to confirm mutation took effect.
    let mut probe = Sexp::Nil;
    assert_eq!(unsafe { jit_aref(&v, 1, &mut probe) }, 0);
    assert_eq!(probe, val);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aset_err_vector_out_of_range() {
    let v = Sexp::vector(vec![Sexp::Int(10)]);
    let val = Sexp::Int(99);
    let mut out = Sexp::Nil;
    let r = unsafe { jit_aset(&v, 5, &val, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aset_err_negative_index() {
    let v = Sexp::vector(vec![Sexp::Int(10)]);
    let val = Sexp::Int(99);
    let mut out = Sexp::Nil;
    let r = unsafe { jit_aset(&v, -1, &val, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aset_err_non_array_returns_err() {
    let s = Sexp::Str("abc".into());
    let val = Sexp::Int(42);
    let mut out = Sexp::Nil;
    let r = unsafe { jit_aset(&s, 0, &val, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aset_ok_bool_vector_truthy_value() {
    // bv = [true, true, true] → aset(bv, 1, nil) flips slot 1 to false.
    let bv = Sexp::bool_vector(3, true);
    let val_nil = Sexp::Nil;
    let mut out = Sexp::T;
    let r = unsafe { jit_aset(&bv, 1, &val_nil, &mut out) };
    assert_eq!(r, 0);
    assert_eq!(out, Sexp::Nil);
    let mut probe = Sexp::Nil;
    assert_eq!(unsafe { jit_aref(&bv, 0, &mut probe) }, 0);
    assert_eq!(probe, Sexp::T);
    assert_eq!(unsafe { jit_aref(&bv, 1, &mut probe) }, 0);
    assert_eq!(probe, Sexp::Nil);
    // Truthy non-Nil value sets slot to true.
    let val_int = Sexp::Int(42);
    let r = unsafe { jit_aset(&bv, 1, &val_int, &mut out) };
    assert_eq!(r, 0);
    assert_eq!(out, val_int);
    assert_eq!(unsafe { jit_aref(&bv, 1, &mut probe) }, 0);
    assert_eq!(probe, Sexp::T);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aset_err_bool_vector_out_of_range() {
    let bv = Sexp::bool_vector(2, false);
    let val = Sexp::T;
    let mut out = Sexp::Nil;
    let r = unsafe { jit_aset(&bv, 5, &val, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_aset_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.D aset probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
