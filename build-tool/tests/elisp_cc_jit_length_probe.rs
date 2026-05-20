#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_length;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn length_ok_nil() {
    let mut out = Sexp::Nil;
    let nil = Sexp::Nil;
    let r = unsafe { jit_length(&nil, &mut out) };
    assert_eq!(r, 0);
    assert!(matches!(out, Sexp::Int(0)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn length_ok_vector_three_elems() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
    let r = unsafe { jit_length(&v, &mut out) };
    assert_eq!(r, 0);
    assert!(matches!(out, Sexp::Int(3)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn length_ok_vector_empty() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![]);
    let r = unsafe { jit_length(&v, &mut out) };
    assert_eq!(r, 0);
    assert!(matches!(out, Sexp::Int(0)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn length_ok_str_via_narrow_extern() {
    // Str arm delegates to `nl_jit_access_length_str_inner' (=
    // codepoint count, kept in Rust until Phase 47 grows a
    // `str-char-count' grammar op per Doc 122 §122.A).
    let mut out = Sexp::Nil;
    let s = Sexp::Str("hello".into());
    let r = unsafe { jit_length(&s, &mut out) };
    assert_eq!(r, 0);
    assert!(matches!(out, Sexp::Int(5)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn length_err_int_returns_err() {
    let mut out = Sexp::Nil;
    let i = Sexp::Int(42);
    let r = unsafe { jit_length(&i, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_length_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.D length probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
