//! Doc 111 §111.B probe — direct calls into the
//! Phase 47-compiled `recordp` object.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn run_recordp(input: Sexp) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::recordp(&input as *const Sexp, slot_ptr)
    };
    assert_eq!(returned, slot_ptr, "extern must return the caller-provided slot pointer");
    slot
}

#[test]
fn recordp_record_returns_t() {
    let input = Sexp::record(
        Sexp::Symbol("foo".into()),
        vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)],
    );
    assert_eq!(run_recordp(input), Sexp::T);
}

#[test]
fn recordp_symbol_returns_nil() {
    assert_eq!(run_recordp(Sexp::Symbol("foo".into())), Sexp::Nil);
}

#[test]
fn recordp_list_returns_nil() {
    assert_eq!(
        run_recordp(Sexp::list_from(&[
            Sexp::Symbol("a".into()),
            Sexp::Symbol("b".into()),
            Sexp::Symbol("c".into()),
        ])),
        Sexp::Nil,
    );
}

#[test]
fn recordp_nil_returns_nil() {
    assert_eq!(run_recordp(Sexp::Nil), Sexp::Nil);
}
