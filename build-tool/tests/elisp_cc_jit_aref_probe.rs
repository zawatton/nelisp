//! Doc 120 §120.D probe — exercises the Phase-47-compiled elisp
//! replacement for `build-tool/src/jit/access.rs's `nl_jit_access_
//! aref' trampoline.  ABI: `(*const Sexp, i64, *mut Sexp) -> i64'
//! returning 0 on OK (Vector / BoolVector in-range), 1 on ERR
//! (negative idx / OOR / non-array tag).

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::jit_aref;
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::eval::sexp::Sexp;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aref_ok_vector_in_range() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![
        Sexp::Symbol("a".into()),
        Sexp::Symbol("b".into()),
        Sexp::Symbol("c".into()),
    ]);
    let r = unsafe { jit_aref(&v, 1, &mut out) };
    assert_eq!(r, 0);
    assert_eq!(out, Sexp::Symbol("b".into()));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aref_err_vector_out_of_range() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![Sexp::Int(7)]);
    let r = unsafe { jit_aref(&v, 5, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aref_err_negative_index() {
    let mut out = Sexp::Nil;
    let v = Sexp::vector(vec![Sexp::Int(7)]);
    let r = unsafe { jit_aref(&v, -1, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aref_err_non_array_returns_err() {
    let mut out = Sexp::Nil;
    let s = Sexp::Str("abc".into());
    let r = unsafe { jit_aref(&s, 0, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aref_ok_bool_vector_in_range() {
    // bv = [false, true, false].
    let bv = Sexp::bool_vector(3, false);
    if let Sexp::BoolVector(rc) = &bv {
        unsafe {
            rc.with_value_mut(|v| v[1] = true);
        }
    } else {
        panic!("bool_vector did not produce BoolVector");
    }
    let mut out = Sexp::Nil;
    assert_eq!(unsafe { jit_aref(&bv, 0, &mut out) }, 0);
    assert_eq!(out, Sexp::Nil);
    assert_eq!(unsafe { jit_aref(&bv, 1, &mut out) }, 0);
    assert_eq!(out, Sexp::T);
    assert_eq!(unsafe { jit_aref(&bv, 2, &mut out) }, 0);
    assert_eq!(out, Sexp::Nil);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn aref_err_bool_vector_out_of_range() {
    let bv = Sexp::bool_vector(2, true);
    let mut out = Sexp::Nil;
    let r = unsafe { jit_aref(&bv, 5, &mut out) };
    assert_eq!(r, 1);
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_aref_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 120 §120.D aref probe skipped: only x86_64-linux \
         emits the elisp `.o' (extern-call ABI ships aarch64 in a follow-up)"
    );
}
