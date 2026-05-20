use nelisp_build_tool::eval::nlrecord::*;
use nelisp_build_tool::eval::sexp::Sexp;

fn sym(name: &str) -> Sexp {
    Sexp::Symbol(name.to_string())
}

#[test]
fn layout_type_tag_at_offset_0() {
    use std::mem::offset_of;
    assert_eq!(offset_of!(NlRecord, type_tag), 0);
}

#[test]
fn layout_slots_after_type_tag() {
    use std::mem::{offset_of, size_of};
    assert_eq!(offset_of!(NlRecord, slots), size_of::<Sexp>());
}

#[test]
fn layout_refcount_after_slots() {
    use std::mem::{offset_of, size_of};
    assert_eq!(
        offset_of!(NlRecord, refcount),
        size_of::<Sexp>() + size_of::<Vec<Sexp>>()
    );
}

#[test]
fn new_starts_with_refcount_1() {
    let r = NlRecordRef::new(sym("foo"), vec![Sexp::Int(1)]);
    assert_eq!(NlRecordRef::strong_count(&r), 1);
}

#[test]
fn new_returns_box_holding_value() {
    let r = NlRecordRef::new(sym("point"), vec![Sexp::Int(3), Sexp::Int(4)]);
    assert_eq!(r.type_tag, sym("point"));
    assert_eq!(r.slots.len(), 2);
    assert_eq!(r.slots[0], Sexp::Int(3));
    assert_eq!(r.slots[1], Sexp::Int(4));
}

#[test]
fn clone_bumps_refcount_and_shares_value() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(7)]);
    let b = a.clone();
    assert_eq!(NlRecordRef::strong_count(&a), 2);
    assert_eq!(b.slots, vec![Sexp::Int(7)]);
    assert_eq!(b.type_tag, sym("x"));
}

#[test]
fn drop_decrements_refcount() {
    let a = NlRecordRef::new(sym("x"), vec![]);
    {
        let _b = a.clone();
        assert_eq!(NlRecordRef::strong_count(&a), 2);
    }
    assert_eq!(NlRecordRef::strong_count(&a), 1);
}

#[test]
fn ptr_eq_same() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Nil]);
    let b = a.clone();
    assert!(NlRecordRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different_alloc() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
    let b = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
    assert!(!NlRecordRef::ptr_eq(&a, &b));
}

#[test]
fn with_slots_mut_index_set() {
    let r = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(2)]);
    unsafe {
        r.with_slots_mut(|slots| {
            slots[1] = Sexp::Int(99);
        });
    }
    assert_eq!(r.slots[1], Sexp::Int(99));
}

#[test]
fn with_slots_mut_visible_through_clone() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
    let b = a.clone();
    unsafe {
        a.with_slots_mut(|slots| slots[0] = Sexp::Int(7));
    }
    assert_eq!(a.slots, vec![Sexp::Int(7)]);
    assert_eq!(b.slots, vec![Sexp::Int(7)]);
}

#[test]
fn with_slots_mut_returns_value_from_closure() {
    let r = NlRecordRef::new(sym("x"), vec![Sexp::Int(10)]);
    let len = unsafe { r.with_slots_mut(|s| s.len()) };
    assert_eq!(len, 1);
}

#[test]
fn debug_format_uses_record_struct() {
    let r = NlRecordRef::new(sym("point"), vec![Sexp::Int(1)]);
    let d = format!("{:?}", r);
    assert!(
        d.starts_with("Record"),
        "expected `Record {{...}}' debug shape, got {:?}",
        d
    );
}

#[test]
fn partial_eq_same_handle_short_circuits() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1)]);
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn partial_eq_distinct_alloc_compares_value() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(2)]);
    let b = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(2)]);
    assert_eq!(a, b);
    let c = NlRecordRef::new(sym("y"), vec![Sexp::Int(1), Sexp::Int(2)]);
    assert_ne!(a, c);
    let d = NlRecordRef::new(sym("x"), vec![Sexp::Int(1), Sexp::Int(3)]);
    assert_ne!(a, d);
}

#[test]
fn payload_drop_runs_exactly_once() {
    let r = NlRecordRef::new(sym("big"), vec![Sexp::Int(1); 100]);
    {
        let _t = r.clone();
    }
    drop(r);
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlRecordRef::new(sym("x"), vec![Sexp::Nil]);
    {
        let _b = a.clone();
    }
    let c = a.clone();
    assert!(NlRecordRef::ptr_eq(&a, &c));
}

#[test]
fn empty_slots_round_trip() {
    let r = NlRecordRef::new(sym("empty"), vec![]);
    assert_eq!(r.slots.len(), 0);
    assert_eq!(NlRecordRef::strong_count(&r), 1);
}

#[test]
fn type_tag_preserved_on_clone() {
    let a = NlRecordRef::new(sym("frob"), vec![Sexp::Int(1)]);
    let b = a.clone();
    assert_eq!(a.type_tag, sym("frob"));
    assert_eq!(b.type_tag, sym("frob"));
}
