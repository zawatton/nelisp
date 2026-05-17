//! Doc 120 §120.B probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/box_accessor.rs's
//! `nl_jit_record_set' trampoline.  ABI: `(*const Sexp, i64, *const
//! Sexp, *mut Sexp) -> i64' returning 0 on OK (`slots[idx] :=
//! clone(*val)` and `*out = clone(*val)`) / 1 on ERR.

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::{jit_record_ref, jit_record_set};
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_set_ok_writes_slot_and_out() {
    // Overwrite slot 1 with Int(99), verify both record and *out.
    let r = Sexp::record(
        Sexp::Symbol("triple".into()),
        vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)],
    );
    let val = Sexp::Int(99);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_set(&r, 1, &val, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Int(99)));
    // Re-read slot 1 via record-ref to confirm in-place mutation.
    let mut probe = Sexp::Nil;
    let rc2 = unsafe { jit_record_ref(&r, 1, &mut probe) };
    assert_eq!(rc2, 0);
    assert!(matches!(probe, Sexp::Int(99)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_set_ok_first_slot() {
    let r = Sexp::record(Sexp::Symbol("x".into()), vec![Sexp::Int(0), Sexp::Int(1)]);
    let val = Sexp::Symbol("new".into());
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_set(&r, 0, &val, &mut out) };
    assert_eq!(rc, 0);
    match out {
        Sexp::Symbol(ref s) => assert_eq!(s.as_str(), "new"),
        ref other => panic!("expected Symbol, got {:?}", other),
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_set_err_negative_idx() {
    let r = Sexp::record(Sexp::Symbol("x".into()), vec![Sexp::Int(7)]);
    let val = Sexp::Int(99);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_set(&r, -1, &val, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_set_err_oor() {
    let r = Sexp::record(
        Sexp::Symbol("x".into()),
        vec![Sexp::Int(7), Sexp::Int(8)],
    );
    let val = Sexp::Int(99);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_set(&r, 2, &val, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_set_err_on_non_record() {
    let i = Sexp::Int(42);
    let val = Sexp::Int(99);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_set(&i, 0, &val, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_record_set_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.B record-set probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
