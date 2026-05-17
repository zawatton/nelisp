//! Doc 120 §120.A probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/predicate.rs's
//! `nl_jit_predicate_eq' trampoline.
//!
//! When this test passes, every step of the §120.A pipeline has
//! worked on linux-x86_64:
//!
//!   1. `lisp/nelisp-cc-jit-predicate-eq.el' survived
//!      `nelisp-phase47-compile-to-object' into an ET_REL `.o' file.
//!   2. `build.rs::link_elisp_cc_spike' bundled the `.o' into
//!      `libnelisp_elisp_spike.a' via `ar rcs'.
//!   3. The linker resolved the `nelisp_jit_predicate_eq' STT_FUNC
//!      symbol against the archive AND the `nl_sexp_eq' SHN_UNDEF
//!      reloc against the Rust `#[no_mangle]' wrapper in
//!      `eval/special_forms.rs'.
//!   4. The compiled body computes the same boolean the deleted Rust
//!      trampoline computed: same-ref short-circuit / tag-byte
//!      mismatch / Int payload compare / slow-path `sexp_eq' for
//!      heap variants (= Symbol-by-name / Cons-by-Rc-ptr-eq).
//!
//! Non-linux / non-x86_64 builds skip the test because the elisp
//! `.o' is not emitted (= `compile-elisp-objects.el' `:requires-arch
//! x86_64' gate) and the Rust trampoline `super::predicate::nl_jit_
//! predicate_eq' is still live on those targets through the
//! `predicate_link' stub.

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_predicate_eq;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

// ---- Inline fast paths ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_int_equal_inline() {
    // Same Int → trampoline int fast path returns 1.
    let a = Sexp::Int(7);
    let b = Sexp::Int(7);
    assert_eq!(unsafe { jit_predicate_eq(&a, &b) }, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_int_unequal_inline() {
    // Different Int → trampoline int fast path returns 0.
    let a = Sexp::Int(7);
    let b = Sexp::Int(8);
    assert_eq!(unsafe { jit_predicate_eq(&a, &b) }, 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_tag_mismatch_short_circuits() {
    // Different tags → tag-byte arm returns 0 without entering the
    // slow path.  Sexp::Int vs Sexp::Float (= same payload size,
    // different tag) is the classic regression probe.
    let a = Sexp::Int(0);
    let b = Sexp::Float(0.0);
    assert_eq!(unsafe { jit_predicate_eq(&a, &b) }, 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_nil_t_via_helper() {
    // Nil/T have matching tags but no payload — the slow path
    // handles this through `nl_sexp_eq's `(Nil, Nil) | (T, T) =>
    // true' arm.
    let nil = Sexp::Nil;
    let t = Sexp::T;
    assert_eq!(unsafe { jit_predicate_eq(&nil, &nil) }, 1);
    assert_eq!(unsafe { jit_predicate_eq(&t, &t) }, 1);
    // Mismatched tags → diff arm inline.
    assert_eq!(unsafe { jit_predicate_eq(&nil, &t) }, 0);
}

// ---- Slow paths via nl_sexp_eq ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_symbol_by_name_via_helper() {
    // Symbol matches via `sexp_eq's name-equality arm.
    let a = Sexp::Symbol("foo".into());
    let b = Sexp::Symbol("foo".into());
    assert_eq!(unsafe { jit_predicate_eq(&a, &b) }, 1);
    let c = Sexp::Symbol("bar".into());
    assert_eq!(unsafe { jit_predicate_eq(&a, &c) }, 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_cons_identity_via_helper() {
    // Two separately-constructed cons cells with same value are
    // NOT eq (= identity check via Rc::ptr_eq inside `sexp_eq',
    // reached after both same-ref check and tag-equal branches).
    let a = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    let b = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    assert_eq!(unsafe { jit_predicate_eq(&a, &b) }, 0);
    // The same cell IS eq with itself — same-ref short-circuit
    // returns 1 from the first arm before any helper call.
    assert_eq!(unsafe { jit_predicate_eq(&a, &a) }, 1);
}

// ---- Same-ref short-circuit ----

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn predicate_eq_same_ref_short_circuit() {
    // For every variant, comparing a Sexp ref to itself must
    // return 1 without entering the slow path.
    let int = Sexp::Int(42);
    let flt = Sexp::Float(3.14);
    let nil = Sexp::Nil;
    let t = Sexp::T;
    let sym = Sexp::Symbol("x".into());
    let s = Sexp::Str("hello".into());
    let cons = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    for r in [&int, &flt, &nil, &t, &sym, &s, &cons] {
        let p = r as *const Sexp;
        assert_eq!(unsafe { jit_predicate_eq(p, p) }, 1);
    }
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_predicate_eq_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.A predicate-eq probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
