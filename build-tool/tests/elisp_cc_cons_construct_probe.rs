//! Doc 101 §101.D probe — `(cons A B)' swapped from the Rust/JIT
//! constructor body into a Phase 47-compiled elisp `.o'.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn run_cons_construct(car: Sexp, cdr: Sexp) -> Sexp {
    let car_owned = car.clone();
    let cdr_owned = cdr.clone();
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::cons_construct(
            &car_owned as *const Sexp,
            &cdr_owned as *const Sexp,
            slot_ptr,
        )
    };
    std::mem::forget(car_owned);
    std::mem::forget(cdr_owned);
    assert_eq!(returned, slot_ptr, "extern must return the caller-provided slot pointer");
    slot
}

#[test]
fn cons_construct_int_dot_int() {
    let got = run_cons_construct(Sexp::Int(1), Sexp::Int(2));
    assert_eq!(got, Sexp::cons(Sexp::Int(1), Sexp::Int(2)));
}

#[test]
fn cons_construct_symbol_dot_symbol() {
    let got = run_cons_construct(Sexp::Symbol("a".into()), Sexp::Symbol("b".into()));
    assert_eq!(got, Sexp::cons(Sexp::Symbol("a".into()), Sexp::Symbol("b".into())));
}

#[test]
fn cons_construct_int_nil_list() {
    let got = run_cons_construct(Sexp::Int(1), Sexp::Nil);
    assert_eq!(got, Sexp::list_from(&[Sexp::Int(1)]));
}

#[test]
fn cons_construct_nested_list() {
    let got = run_cons_construct(
        Sexp::Int(1),
        Sexp::cons(Sexp::Int(2), Sexp::Nil),
    );
    assert_eq!(got, Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]));
}

#[test]
fn cons_construct_string_dot_string() {
    let got = run_cons_construct(Sexp::Str("hello".into()), Sexp::Str("world".into()));
    assert_eq!(got, Sexp::cons(Sexp::Str("hello".into()), Sexp::Str("world".into())));
}
