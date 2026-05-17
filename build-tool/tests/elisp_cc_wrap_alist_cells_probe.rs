//! Doc 111 §111.E #26 probe — `wrap_alist_cells' Phase 47 helper.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::nlcell::NlCellRef;
use nelisp_build_tool::eval::sexp::Sexp;

fn run_wrap(alist: Sexp) -> (i64, Sexp) {
    let mut out = Sexp::Nil;
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::wrap_alist_cells(
            &alist as *const Sexp,
            &mut out as *mut Sexp,
        )
    };
    (rc, out)
}

#[test]
fn wrap_alist_cells_empty_alist_is_nil() {
    let (rc, out) = run_wrap(Sexp::Nil);
    assert_eq!(rc, 1);
    assert_eq!(out, Sexp::Nil);
}

#[test]
fn wrap_alist_cells_wraps_bare_values_in_cells() {
    let alist = Sexp::list_from(&[
        Sexp::cons(Sexp::Symbol("a".into()), Sexp::Int(1)),
        Sexp::cons(Sexp::Symbol("b".into()), Sexp::Int(2)),
    ]);
    let (rc, out) = run_wrap(alist);
    assert_eq!(rc, 1);
    // Walk: ((a . #<cell 1>) (b . #<cell 2>))
    let Sexp::Cons(outer) = &out else {
        panic!("expected outer cons, got {:?}", out);
    };
    let Sexp::Cons(first) = &outer.car else {
        panic!("first inner not cons");
    };
    assert!(matches!(&first.car, Sexp::Symbol(s) if s == "a"));
    assert!(matches!(&first.cdr, Sexp::Cell(_)));
    // Second entry survives in the cdr.
    let Sexp::Cons(rest) = &outer.cdr else {
        panic!("expected second cons, got {:?}", outer.cdr);
    };
    let Sexp::Cons(second) = &rest.car else {
        panic!("second inner not cons");
    };
    assert!(matches!(&second.car, Sexp::Symbol(s) if s == "b"));
    assert!(matches!(&second.cdr, Sexp::Cell(_)));
}

#[test]
fn wrap_alist_cells_preserves_existing_cells() {
    // Cell-typed cdr must not be re-wrapped (= identity check).
    let existing_cell = NlCellRef::new(Sexp::Int(99));
    let initial_refcount = NlCellRef::strong_count(&existing_cell);
    let alist = Sexp::cons(
        Sexp::cons(
            Sexp::Symbol("c".into()),
            Sexp::Cell(existing_cell.clone()),
        ),
        Sexp::Nil,
    );
    let (rc, out) = run_wrap(alist);
    assert_eq!(rc, 1);
    let Sexp::Cons(outer) = &out else { panic!("expected cons") };
    let Sexp::Cons(first) = &outer.car else { panic!("inner not cons") };
    if let Sexp::Cell(handle) = &first.cdr {
        // Same underlying cell — pointer equal to `existing_cell'.
        assert!(NlCellRef::ptr_eq(&existing_cell, handle));
    } else {
        panic!("cdr must remain a Cell");
    }
    // Refcount bumped (= we cloned it through the alist).
    assert!(NlCellRef::strong_count(&existing_cell) > initial_refcount);
}

#[test]
fn wrap_alist_cells_malformed_returns_zero() {
    // Malformed: inner is not a cons.
    let alist = Sexp::cons(Sexp::Int(0), Sexp::Nil);
    let (rc, _out) = run_wrap(alist);
    assert_eq!(rc, 0, "non-cons inner element must produce 0");
}
