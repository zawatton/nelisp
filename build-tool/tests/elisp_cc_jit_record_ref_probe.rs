#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_record_ref;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_ref_ok_first_slot() {
    let r = Sexp::record(
        Sexp::Symbol("triple".into()),
        vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)],
    );
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_ref(&r, 0, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Int(10)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_ref_ok_last_slot() {
    let r = Sexp::record(
        Sexp::Symbol("triple".into()),
        vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)],
    );
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_ref(&r, 2, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Int(30)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_ref_err_negative_idx() {
    let r = Sexp::record(Sexp::Symbol("x".into()), vec![Sexp::Int(7)]);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_ref(&r, -1, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_ref_err_idx_at_len() {
    let r = Sexp::record(Sexp::Symbol("x".into()), vec![Sexp::Int(7), Sexp::Int(8)]);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_ref(&r, 2, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_ref_err_on_non_record() {
    let i = Sexp::Int(42);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_ref(&i, 0, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_record_ref_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.B record-ref probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
