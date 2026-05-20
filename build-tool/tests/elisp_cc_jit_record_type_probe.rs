#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_record_type;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_type_ok_returns_tag_symbol() {
    // Record with Symbol type-tag → out = Symbol("point"), rc = 0.
    let r = Sexp::record(Sexp::Symbol("point".into()), vec![Sexp::Int(1)]);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_type(&r, &mut out) };
    assert_eq!(rc, 0);
    match out {
        Sexp::Symbol(ref s) => assert_eq!(s.as_str(), "point"),
        ref other => panic!("expected Symbol, got {:?}", other),
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_type_err_on_non_record() {
    // Sexp::Int → ERR=1, out unchanged.
    let i = Sexp::Int(42);
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_type(&i, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn record_type_err_on_cons() {
    // Sexp::Cons → ERR=1 (same code-path as Int: tag != 12).
    let c = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    let mut out = Sexp::Nil;
    let rc = unsafe { jit_record_type(&c, &mut out) };
    assert_eq!(rc, 1);
    assert!(matches!(out, Sexp::Nil));
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_record_type_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.B record-type probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
