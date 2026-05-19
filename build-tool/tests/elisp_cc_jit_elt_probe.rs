//! Doc 120 §120.D probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/access.rs's `nl_jit_access_
//! elt' trampoline.  ABI: `(*const Sexp, i64, *mut Sexp) -> i64'
//! returning 0 on OK (Vector / Cons-list in-range), 1 on ERR.

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_elt;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_ok_vector_path() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![
        Sexp::Symbol("x".into()),
        Sexp::Symbol("y".into()),
        Sexp::Symbol("z".into()),
    ]);
    let r = unsafe { jit_elt(&v, 2, &mut out) };
    assert_eq!(r, 0);
    assert_eq!(out, Sexp::Symbol("z".into()));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_ok_list_walks_to_index() {
    let mut out = Sexp::Nil;
    let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3), Sexp::Int(4)]);
    let r = unsafe { jit_elt(&lst, 2, &mut out) };
    assert_eq!(r, 0);
    assert!(matches!(out, Sexp::Int(3)));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_ok_list_first_element() {
    let mut out = Sexp::Nil;
    let lst = Sexp::list_from(&[
        Sexp::Symbol("head".into()),
        Sexp::Symbol("middle".into()),
        Sexp::Symbol("tail".into()),
    ]);
    let r = unsafe { jit_elt(&lst, 0, &mut out) };
    assert_eq!(r, 0);
    assert_eq!(out, Sexp::Symbol("head".into()));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_err_list_overrun() {
    let mut out = Sexp::Nil;
    let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]);
    let r = unsafe { jit_elt(&lst, 5, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_err_nil_returns_err() {
    let mut out = Sexp::Nil;
    let nil = Sexp::Nil;
    let r = unsafe { jit_elt(&nil, 0, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_err_negative_index() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![Sexp::Int(1)]);
    let r = unsafe { jit_elt(&v, -1, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn elt_err_vector_out_of_range() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![Sexp::Int(1)]);
    let r = unsafe { jit_elt(&v, 5, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_elt_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.D elt probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
