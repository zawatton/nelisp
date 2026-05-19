//! Doc 101 §101.C probe — `(eq SYMBOL SYMBOL)' swapped from the Rust
//! `sexp_eq' symbol arm into a Phase 47-compiled elisp `.o'.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn run_eq_symbol(left: Sexp, right: Sexp) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::eq_symbol(
            &left as *const Sexp,
            &right as *const Sexp,
            slot_ptr,
        )
    };
    assert_eq!(
        returned, slot_ptr,
        "extern must return the caller-provided slot pointer"
    );
    slot
}

#[test]
fn eq_symbol_same_short_returns_t() {
    let slot = run_eq_symbol(Sexp::Symbol("foo".into()), Sexp::Symbol("foo".into()));
    assert_eq!(slot, Sexp::T, "expected `(eq 'foo 'foo)' probe to return t");
}

#[test]
fn eq_symbol_diff_short_returns_nil() {
    let slot = run_eq_symbol(Sexp::Symbol("foo".into()), Sexp::Symbol("bar".into()));
    assert_eq!(
        slot,
        Sexp::Nil,
        "expected `(eq 'foo 'bar)' probe to return nil"
    );
}

#[test]
fn eq_symbol_same_16_byte_name_returns_t() {
    let name = "0123456789abcdef";
    let slot = run_eq_symbol(Sexp::Symbol(name.into()), Sexp::Symbol(name.into()));
    assert_eq!(slot, Sexp::T, "16-byte symbol names must compare equal");
}

#[test]
fn eq_symbol_same_gt_16_byte_name_returns_t() {
    let name = "0123456789abcdefg";
    let slot = run_eq_symbol(Sexp::Symbol(name.into()), Sexp::Symbol(name.into()));
    assert_eq!(slot, Sexp::T, ">16-byte symbol names must compare equal");
}
