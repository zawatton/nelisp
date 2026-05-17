//! Doc 120 §120.B probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/box_accessor.rs's
//! `nl_jit_record_type' trampoline.  ABI: `(*const Sexp, *mut Sexp)
//! -> i64' returning 0 on OK (Record arg, `*out = arg.type_tag') /
//! 1 on ERR (non-Record).
//!
//! When this test passes, every step of the §120.B pipeline has
//! worked on linux-x86_64:
//!
//!   1. `lisp/nelisp-cc-jit-record.el's `nelisp-cc-jit-record-type-
//!      -source' survived `nelisp-phase47-compile-to-object' into an
//!      ET_REL `.o' file.
//!   2. `build.rs::link_elisp_cc_spike' bundled the `.o' into
//!      `libnelisp_elisp_spike.a' via `ar rcs'.
//!   3. The linker resolved the `nelisp_jit_record_type' STT_FUNC
//!      symbol against the archive.
//!   4. The compiled body computes the same result the deleted Rust
//!      trampoline computed: tag-check vs `Sexp::Record' / extern
//!      `record-type-tag' op for the OK arm.
//!
//! Non-linux / non-x86_64 builds skip the test because the elisp
//! `.o' is not emitted (= `compile-elisp-objects.el' `:requires-arch
//! x86_64' gate) and the Rust trampoline `super::box_accessor::
//! nl_jit_record_type' is still live on those targets through the
//! `box_accessor_link' stub.

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
