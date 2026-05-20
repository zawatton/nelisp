#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::nlcell::NlCellRef;
use nelisp_build_tool::eval::nlrecord::NlRecordRef;
use nelisp_build_tool::eval::nlvector::NlVectorRef;
use nelisp_build_tool::eval::sexp::{
    Sexp, SEXP_TAG_CELL, SEXP_TAG_CONS, SEXP_TAG_INT, SEXP_TAG_NIL, SEXP_TAG_RECORD, SEXP_TAG_T,
    SEXP_TAG_VECTOR,
};

fn sexp_as_ptr(s: &Sexp) -> *const u8 {
    s as *const Sexp as *const u8
}

// ---- Case 1: Nil reads back as SEXP_TAG_NIL = 0 ----

#[test]
fn rc_kind_nil_returns_zero() {
    let s = Sexp::Nil;
    let tag = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&s)) };
    assert_eq!(
        tag, SEXP_TAG_NIL as i64,
        "Sexp::Nil tag byte must be SEXP_TAG_NIL = {}",
        SEXP_TAG_NIL
    );
    assert_eq!(tag, 0, "SEXP_TAG_NIL is the zero-valued tag constant");
}

// ---- Case 2: T reads back as SEXP_TAG_T = 1 ----

#[test]
fn rc_kind_t_returns_one() {
    let s = Sexp::T;
    let tag = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&s)) };
    assert_eq!(
        tag, SEXP_TAG_T as i64,
        "Sexp::T tag byte must be SEXP_TAG_T = {}",
        SEXP_TAG_T
    );
}

// ---- Case 3: Int reads back as SEXP_TAG_INT = 2 ----

#[test]
fn rc_kind_int_returns_two() {
    let s = Sexp::Int(42);
    let tag = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&s)) };
    assert_eq!(
        tag, SEXP_TAG_INT as i64,
        "Sexp::Int tag byte must be SEXP_TAG_INT = {}",
        SEXP_TAG_INT
    );
    // Crucially, the i64 payload of the variant must NOT affect the
    // tag read — only the offset-0 discriminant byte is inspected.
    let s_big = Sexp::Int(0x7FFF_FFFF_FFFF_FFFF_i64);
    let tag_big = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&s_big)) };
    assert_eq!(
        tag_big, SEXP_TAG_INT as i64,
        "payload value must not leak into the tag-byte read"
    );
}

// ---- Case 4: Cons reads back as SEXP_TAG_CONS = 7 ----
//
// This is the refcount-bearing variant — the variant §123.A's inc
// kernel targets.  Confirming the tag round-trip closes the loop:
// §123.A bumps the count via the box pointer, §123.C reads the count
// via the same box pointer, §123.C kind-reader reads the tag via the
// outer Sexp pointer.

#[test]
fn rc_kind_cons_returns_seven() {
    // Build a cons via the public NlConsBoxRef constructor (= same
    // path the evaluator uses).
    use nelisp_build_tool::eval::nlconsbox::NlConsBoxRef;
    let cons = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    let s = Sexp::Cons(cons);
    let tag = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&s)) };
    assert_eq!(
        tag, SEXP_TAG_CONS as i64,
        "Sexp::Cons tag byte must be SEXP_TAG_CONS = {} (= the \
         refcount-bearing variant, target of §123.A inc and §123.C \
         read kernels)",
        SEXP_TAG_CONS
    );
}

// ---- Case 5: Vector / Cell / Record — boxed-variant tag coverage ----

#[test]
fn rc_kind_boxed_variants_cover_tag_range() {
    // Vector.
    let v = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2)]);
    let sv = Sexp::Vector(v);
    let tag_v = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&sv)) };
    assert_eq!(
        tag_v, SEXP_TAG_VECTOR as i64,
        "Sexp::Vector tag byte must be SEXP_TAG_VECTOR = {}",
        SEXP_TAG_VECTOR
    );

    // Cell.
    let c = NlCellRef::new(Sexp::Int(99));
    let sc = Sexp::Cell(c);
    let tag_c = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&sc)) };
    assert_eq!(
        tag_c, SEXP_TAG_CELL as i64,
        "Sexp::Cell tag byte must be SEXP_TAG_CELL = {}",
        SEXP_TAG_CELL
    );

    // Record.
    let r = NlRecordRef::new(Sexp::Symbol("test-record".to_string()), vec![Sexp::Int(1)]);
    let sr = Sexp::Record(r);
    let tag_r = unsafe { nelisp_build_tool::elisp_cc_spike::rc_kind(sexp_as_ptr(&sr)) };
    assert_eq!(
        tag_r, SEXP_TAG_RECORD as i64,
        "Sexp::Record tag byte must be SEXP_TAG_RECORD = {}",
        SEXP_TAG_RECORD
    );

    // Confirm tags are mutually distinct (= no offset collision).
    assert_ne!(tag_v, tag_c);
    assert_ne!(tag_c, tag_r);
    assert_ne!(tag_v, tag_r);
}
