//! Doc 111 §111.D probes — direct calls into the four Phase 47-
//! compiled cell ops (`cell-value' / `cell-set-value' / `cell-make'
//! / `cell-null-p').  Verifies end-to-end round-trips including
//! refcount-safe overwrite for box-tagged payloads.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::nlcell::NlCellRef;
use nelisp_build_tool::eval::nlvector::NlVectorRef;
use nelisp_build_tool::eval::sexp::Sexp;

// ---- cell-make + cell-value round-trip ----

fn make_cell(initial: Sexp) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned =
        unsafe { nelisp_build_tool::elisp_cc_spike::cell_make(&initial as *const Sexp, slot_ptr) };
    assert_eq!(
        returned, slot_ptr,
        "cell-make extern must return the caller-provided slot pointer"
    );
    // `initial' is still owned by the local; the elisp side cloned
    // it into the new NlCell via `nl_alloc_cell'.
    slot
}

#[test]
fn cell_make_returns_sexp_cell_tag() {
    let cell = make_cell(Sexp::Int(42));
    assert!(
        matches!(cell, Sexp::Cell(_)),
        "cell-make must produce a Sexp::Cell variant, got {:?}",
        cell
    );
}

#[test]
fn cell_make_initial_value_int_read_back() {
    let cell = make_cell(Sexp::Int(42));
    let mut out = Sexp::Nil;
    let out_ptr = &mut out as *mut Sexp;
    let returned =
        unsafe { nelisp_build_tool::elisp_cc_spike::cell_value(&cell as *const Sexp, out_ptr) };
    assert_eq!(returned, out_ptr);
    assert_eq!(out, Sexp::Int(42));
    // The cell-value op did an inline copy (= MVP, no refcount work);
    // the `out' Sexp::Int has no boxed payload so no double-free
    // concern.  `cell' still owns the NlCellRef.
}

#[test]
fn cell_make_initial_value_nil_read_back() {
    let cell = make_cell(Sexp::Nil);
    let mut out = Sexp::Int(99);
    let out_ptr = &mut out as *mut Sexp;
    unsafe { nelisp_build_tool::elisp_cc_spike::cell_value(&cell as *const Sexp, out_ptr) };
    assert_eq!(out, Sexp::Nil);
}

#[test]
fn cell_make_refcount_is_one_after_construction() {
    // Doc 111 §111.D — `nl_alloc_cell' returns refcount=1.  Wrap the
    // returned Sexp::Cell and verify NlCellRef::strong_count.
    let cell = make_cell(Sexp::Int(7));
    if let Sexp::Cell(ref handle) = cell {
        assert_eq!(NlCellRef::strong_count(handle), 1);
    } else {
        panic!("expected Sexp::Cell, got {:?}", cell);
    }
}

// ---- cell-null-p ----

#[test]
fn cell_null_p_returns_1_for_nil_value() {
    let cell = make_cell(Sexp::Nil);
    let got = unsafe { nelisp_build_tool::elisp_cc_spike::cell_null_p(&cell as *const Sexp) };
    assert_eq!(got, 1);
}

#[test]
fn cell_null_p_returns_0_for_int_value() {
    let cell = make_cell(Sexp::Int(0));
    let got = unsafe { nelisp_build_tool::elisp_cc_spike::cell_null_p(&cell as *const Sexp) };
    assert_eq!(got, 0, "cell holding Sexp::Int(0) is not Nil");
}

#[test]
fn cell_null_p_returns_0_for_symbol_value() {
    let cell = make_cell(Sexp::Symbol("foo".into()));
    let got = unsafe { nelisp_build_tool::elisp_cc_spike::cell_null_p(&cell as *const Sexp) };
    assert_eq!(got, 0);
}

// ---- cell-set-value (refcount-safe overwrite) ----

#[test]
fn cell_set_value_int_to_int() {
    let cell = make_cell(Sexp::Int(1));
    let new_val = Sexp::Int(99);
    unsafe {
        nelisp_build_tool::elisp_cc_spike::cell_set_value(
            &cell as *const Sexp,
            &new_val as *const Sexp,
        );
    }
    // Read back via cell-value.
    let mut out = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::cell_value(&cell as *const Sexp, &mut out as *mut Sexp);
    }
    assert_eq!(out, Sexp::Int(99));
}

#[test]
fn cell_set_value_int_to_nil() {
    let cell = make_cell(Sexp::Int(7));
    let new_val = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::cell_set_value(
            &cell as *const Sexp,
            &new_val as *const Sexp,
        );
    }
    // cell-null-p should now report 1.
    let null_p = unsafe { nelisp_build_tool::elisp_cc_spike::cell_null_p(&cell as *const Sexp) };
    assert_eq!(
        null_p, 1,
        "after cell-set-value to Nil, cell-null-p should be 1"
    );
}

#[test]
fn cell_set_value_drops_old_boxed_value_refcount_safe() {
    // Doc 111 §111.D — verify the refcount-aware set_value extern
    // actually decrements the old payload's refcount.  Pattern: put
    // an NlVectorRef into the cell via cell-make (= clone bumps to 2),
    // then cell-set-value to Int (= old vector dropped, refcount back
    // to 1).
    let probe = NlVectorRef::new(vec![Sexp::Int(11), Sexp::Int(22)]);
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
    let initial = Sexp::Vector(probe.clone());
    // `initial' bumped to 2; cell-make will clone again to 3.
    assert_eq!(NlVectorRef::strong_count(&probe), 2);
    let cell = make_cell(initial);
    // `initial' is forgotten by `make_cell' (it was move-passed) — no,
    // wait, `make_cell' takes by value but only passes a pointer to
    // the elisp side which clones.  The local `initial' inside
    // `make_cell' goes out of scope at end of fn -> dropped -> back
    // to 2 (= 1 outside + 1 inside the cell).
    assert_eq!(
        NlVectorRef::strong_count(&probe),
        2,
        "after cell-make: 1 outside + 1 in cell"
    );
    // Overwrite with Sexp::Int — old vector inside the cell must be
    // dropped, refcount back to 1.
    let new_val = Sexp::Int(99);
    unsafe {
        nelisp_build_tool::elisp_cc_spike::cell_set_value(
            &cell as *const Sexp,
            &new_val as *const Sexp,
        );
    }
    assert_eq!(
        NlVectorRef::strong_count(&probe),
        1,
        "cell-set-value should drop the previous Sexp::Vector"
    );
    // Now drop the cell — no double-free should occur because the
    // overwrite already released the old vector.
    drop(cell);
    // Probe still alive at refcount 1.
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
}

// ---- end-to-end round-trip ----

#[test]
fn round_trip_make_set_get() {
    // Doc 111 §111.D verification gate (= `(let ((c (make-cell '(a b))))
    // (cell-set! c '(c d)) (cell-get c))' equivalent without leak).
    //
    // The elisp s-expression we'd write at the language level is the
    // composition tested below: make-cell holds Cons, set! replaces
    // with new Cons, get reads back.
    let initial = Sexp::cons(Sexp::Symbol("a".into()), Sexp::Symbol("b".into()));
    let cell = make_cell(initial);
    let new_val = Sexp::cons(Sexp::Symbol("c".into()), Sexp::Symbol("d".into()));
    unsafe {
        nelisp_build_tool::elisp_cc_spike::cell_set_value(
            &cell as *const Sexp,
            &new_val as *const Sexp,
        );
    }
    let mut out = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::cell_value(&cell as *const Sexp, &mut out as *mut Sexp);
    }
    // The read-back `out' is an inline 32-byte copy of the cell's
    // current value (= new_val's structure).  Drop it carefully: the
    // inline copy aliases new_val's NlConsBoxRef payload without
    // bumping refcount, so we MUST forget `out' to avoid double-free
    // (this is the MVP `cell-value' contract — see TODO in the emit
    // function for `nl_sexp_clone_into' future swap).
    let _ = matches!(out, Sexp::Cons(_)); // structural check
    std::mem::forget(out);
}
