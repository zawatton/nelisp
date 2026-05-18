//! Integration tests for `eval::sexp` (= moved out of the source file's
//! `#[cfg(test)] mod tests' for src LOC reduction).  Same coverage,
//! just lives next to the other integration probes.

use nelisp_build_tool::eval::nlboolvector::NlBoolVectorRef;
use nelisp_build_tool::eval::nlcell::NlCellRef;
use nelisp_build_tool::eval::nlrecord::NlRecordRef;
use nelisp_build_tool::eval::sexp::{
    fmt_sexp, variant_tag, Sexp, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CELL,
    SEXP_TAG_CHAR_TABLE, SEXP_TAG_CONS, SEXP_TAG_FLOAT, SEXP_TAG_INT,
    SEXP_TAG_MUT_STR, SEXP_TAG_NIL, SEXP_TAG_RECORD, SEXP_TAG_STR,
    SEXP_TAG_SYMBOL, SEXP_TAG_T, SEXP_TAG_VECTOR,
};
use std::ops::Deref;

#[test]
fn list_from_empty_is_nil() {
    assert_eq!(Sexp::list_from(&[]), Sexp::Nil);
}

#[test]
fn list_from_three_elements_chains_right() {
    let got = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
    let expected = Sexp::cons(
        Sexp::Int(1),
        Sexp::cons(
            Sexp::Int(2),
            Sexp::cons(Sexp::Int(3), Sexp::Nil),
        ),
    );
    assert_eq!(got, expected);
}

#[test]
fn quote_wraps_form() {
    let got = Sexp::quote(Sexp::Symbol("x".into()));
    assert_eq!(fmt_sexp(&got), "'x");
}

#[test]
fn fmt_reader_macros() {
    assert_eq!(
        fmt_sexp(&Sexp::backquote(Sexp::Symbol("x".into()))),
        "`x"
    );
    assert_eq!(fmt_sexp(&Sexp::comma(Sexp::Symbol("x".into()))), ",x");
    assert_eq!(fmt_sexp(&Sexp::comma_at(Sexp::Symbol("x".into()))), ",@x");
    assert_eq!(fmt_sexp(&Sexp::function(Sexp::Symbol("x".into()))), "#'x");
}

#[test]
fn fmt_dotted_pair() {
    let dotted = Sexp::cons(Sexp::Symbol("a".into()), Sexp::Symbol("b".into()));
    assert_eq!(fmt_sexp(&dotted), "(a . b)");
}

#[test]
fn fmt_string_escapes() {
    assert_eq!(
        fmt_sexp(&Sexp::Str("hi\n\"\\".into())),
        "\"hi\\n\\\"\\\\\""
    );
}

#[test]
fn fmt_float_keeps_decimal() {
    assert_eq!(fmt_sexp(&Sexp::Float(1.0)), "1.0");
    assert_eq!(fmt_sexp(&Sexp::Float(3.14)), "3.14");
}

/// Doc 62 Phase 5 — pin every `SEXP_TAG_*' constant to the actual
/// `#[repr(C, u8)]' discriminant byte.  JIT-emitted code reads the
/// tag from offset 0 of every Sexp pointer; if a re-ordering of
/// variants ever changes the discriminant numeric values, this
/// test fails BEFORE the JIT silently mis-classifies cons cells
/// as integers (or worse).
#[test]
fn variant_tags_are_stable() {
    assert_eq!(variant_tag(&Sexp::Nil), SEXP_TAG_NIL);
    assert_eq!(variant_tag(&Sexp::T), SEXP_TAG_T);
    assert_eq!(variant_tag(&Sexp::Int(0)), SEXP_TAG_INT);
    assert_eq!(variant_tag(&Sexp::Float(0.0)), SEXP_TAG_FLOAT);
    assert_eq!(variant_tag(&Sexp::Symbol("x".into())), SEXP_TAG_SYMBOL);
    assert_eq!(variant_tag(&Sexp::Str("x".into())), SEXP_TAG_STR);
    assert_eq!(variant_tag(&Sexp::mut_str("x")), SEXP_TAG_MUT_STR);
    assert_eq!(
        variant_tag(&Sexp::cons(Sexp::Nil, Sexp::Nil)),
        SEXP_TAG_CONS
    );
    assert_eq!(variant_tag(&Sexp::vector(vec![])), SEXP_TAG_VECTOR);
    assert_eq!(
        variant_tag(&Sexp::char_table(Sexp::Nil, Sexp::Nil)),
        SEXP_TAG_CHAR_TABLE
    );
    assert_eq!(
        variant_tag(&Sexp::BoolVector(NlBoolVectorRef::new(vec![]))),
        SEXP_TAG_BOOL_VECTOR
    );
    assert_eq!(
        variant_tag(&Sexp::Cell(NlCellRef::new(Sexp::Nil))),
        SEXP_TAG_CELL
    );
    assert_eq!(
        variant_tag(&Sexp::Record(NlRecordRef::new(
            Sexp::Symbol("foo".into()),
            vec![]
        ))),
        SEXP_TAG_RECORD
    );
}

/// `#[repr(C, u8)]' should keep the Sexp footprint at the same
/// alignment / largest-payload bound it had under default repr.
/// We don't pin the exact byte size (= depends on String/Rc layout
/// details that are stable in practice but not guaranteed by spec)
/// but the alignment is fixed, and the size must accommodate the
/// largest payload.
#[test]
fn sexp_layout_alignment_and_size_sane() {
    assert_eq!(std::mem::align_of::<Sexp>(), 8);
    // String is 24 bytes (3 × usize on 64-bit), payload at offset 8
    // → minimum total 32 bytes.  Allow up to 40 for niche slack.
    let sz = std::mem::size_of::<Sexp>();
    assert!(sz >= 32 && sz <= 48, "Sexp size = {} (expected 32..=48)", sz);
}

// ----------------------------------------------------------------
// Phase A.5 ABI helpers — round-trip read of `*_box_ptr' against
// the existing match-arm path.  If the payload offset (= 8) ever
// shifts under us (= compiler change, repr override), these fail
// BEFORE JIT-emitted IR mis-decodes a Sexp value.
// ----------------------------------------------------------------

#[test]
fn sexp_payload_offset_is_eight() {
    // Manual layout probe: build a Sexp with a known boxed payload
    // and check that the pointer at offset 8 equals the box's ptr.
    let cons = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    let direct = unsafe { cons.cons_box_ptr() } as usize;
    // Read offset 8 manually through the *const Sexp.
    let raw = (&cons as *const Sexp) as *const u8;
    let payload_at_8 = unsafe {
        let p = raw.add(8) as *const std::ptr::NonNull<nelisp_build_tool::eval::nlconsbox::NlConsBox>;
        (*p).as_ptr()
    } as usize;
    assert_eq!(direct, payload_at_8);
}

/// Emit a `*_box_ptr_round_trips_to_match` test for one boxed variant:
/// build a Sexp via `$build`, then assert the `$accessor` raw pointer
/// equals the pointer obtained via `match` + `Deref`.
macro_rules! box_ptr_round_trip_test {
    ($test:ident, $variant:ident, $build:expr, $accessor:ident) => {
        #[test]
        fn $test() {
            let s = $build;
            if let Sexp::$variant(rc) = &s {
                let via_match = rc.deref() as *const _ as usize;
                let via_direct = unsafe { s.$accessor() } as usize;
                assert_eq!(via_match, via_direct);
            } else {
                panic!(concat!("expected ", stringify!($variant)));
            }
        }
    };
}

box_ptr_round_trip_test!(cons_box_ptr_round_trips_to_match, Cons,
    Sexp::cons(Sexp::Int(7), Sexp::Symbol("x".into())), cons_box_ptr);
box_ptr_round_trip_test!(cell_box_ptr_round_trips_to_match, Cell,
    Sexp::Cell(NlCellRef::new(Sexp::Int(99))), cell_box_ptr);
box_ptr_round_trip_test!(mut_str_box_ptr_round_trips_to_match, MutStr,
    Sexp::mut_str("hello"), mut_str_box_ptr);
box_ptr_round_trip_test!(vector_box_ptr_round_trips_to_match, Vector,
    Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2)]), vector_box_ptr);
box_ptr_round_trip_test!(bool_vector_box_ptr_round_trips_to_match, BoolVector,
    Sexp::bool_vector(8, true), bool_vector_box_ptr);
box_ptr_round_trip_test!(record_box_ptr_round_trips_to_match, Record,
    Sexp::record(Sexp::Symbol("point".into()), vec![Sexp::Int(3)]), record_box_ptr);
box_ptr_round_trip_test!(char_table_box_ptr_round_trips_to_match, CharTable,
    Sexp::char_table(Sexp::Symbol("syntax".into()), Sexp::Nil), char_table_box_ptr);

#[test]
fn tag_method_matches_variant_tag_fn() {
    let cases = [
        Sexp::Nil,
        Sexp::T,
        Sexp::Int(0),
        Sexp::Float(0.0),
        Sexp::Symbol("x".into()),
        Sexp::Str("x".into()),
        Sexp::mut_str("x"),
        Sexp::cons(Sexp::Nil, Sexp::Nil),
        Sexp::vector(vec![]),
        Sexp::char_table(Sexp::Nil, Sexp::Nil),
        Sexp::bool_vector(0, false),
        Sexp::Cell(NlCellRef::new(Sexp::Nil)),
        Sexp::record(Sexp::Symbol("k".into()), vec![]),
    ];
    for s in &cases {
        assert_eq!(s.tag(), variant_tag(s));
    }
}
