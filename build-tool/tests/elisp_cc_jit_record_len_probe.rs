#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_record_len;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_len_ok_one_slot() {
    // 1-slot record → out = Int(1).
    let r = Sexp::record(Sexp::Symbol("point".into()), vec![Sexp::Int(1)]);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_len(&r, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Int(1)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_len_ok_multi_slot() {
    // 3-slot record → out = Int(3).
    let r = Sexp::record(
        Sexp::Symbol("triple".into()),
        vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)],
    );
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_len(&r, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Int(3)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_len_ok_empty_slots() {
    // 0-slot record (= type-tag-only) → out = Int(0).
    let r = Sexp::record(Sexp::Symbol("singleton".into()), vec![]);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_len(&r, &mut out) };
    assert_eq!(rc, 0);
    assert!(matches!(out, Sexp::Int(0)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_len_err_on_non_record() {
    let i = Sexp::Int(42);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_len(&i, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_record_len_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.B record-len probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
