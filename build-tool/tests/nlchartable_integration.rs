use nelisp_build_tool::eval::nlchartable::*;
use nelisp_build_tool::eval::sexp::{CharTableInner, Sexp};

fn sym(name: &str) -> Sexp {
    Sexp::Symbol(name.to_string())
}

fn empty_inner(subtype: Sexp, default_val: Sexp) -> CharTableInner {
    CharTableInner {
        subtype,
        default_val,
        entries: Vec::new(),
        parent: None,
        extra: Vec::new(),
    }
}

#[test]
fn layout_inner_at_offset_0() {
    use std::mem::offset_of;
    assert_eq!(offset_of!(NlCharTable, inner), 0);
}

#[test]
fn layout_refcount_after_inner() {
    use std::mem::{offset_of, size_of};
    assert_eq!(
        offset_of!(NlCharTable, refcount),
        size_of::<CharTableInner>()
    );
}

#[test]
fn new_starts_with_refcount_1() {
    let r = NlCharTableRef::new(empty_inner(sym("syntax"), Sexp::Nil));
    assert_eq!(NlCharTableRef::strong_count(&r), 1);
}

#[test]
fn new_returns_box_holding_inner() {
    let r = NlCharTableRef::new(empty_inner(sym("display"), Sexp::Int(42)));
    assert_eq!(r.inner.subtype, sym("display"));
    assert_eq!(r.inner.default_val, Sexp::Int(42));
    assert!(r.inner.entries.is_empty());
    assert!(r.inner.parent.is_none());
    assert!(r.inner.extra.is_empty());
}

#[test]
fn clone_bumps_refcount_and_shares_inner() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    let b = a.clone();
    assert_eq!(NlCharTableRef::strong_count(&a), 2);
    assert_eq!(b.inner.subtype, sym("x"));
}

#[test]
fn drop_decrements_refcount() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    {
        let _b = a.clone();
        assert_eq!(NlCharTableRef::strong_count(&a), 2);
    }
    assert_eq!(NlCharTableRef::strong_count(&a), 1);
}

#[test]
fn ptr_eq_same() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    let b = a.clone();
    assert!(NlCharTableRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different_alloc() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    let b = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    assert!(!NlCharTableRef::ptr_eq(&a, &b));
}

#[test]
fn with_inner_mut_pushes_entry() {
    let r = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    unsafe {
        r.with_inner_mut(|i| {
            i.entries.push((65, Sexp::Int(1)));
        });
    }
    assert_eq!(r.inner.entries, vec![(65, Sexp::Int(1))]);
}

#[test]
fn with_inner_mut_visible_through_clone() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    let b = a.clone();
    unsafe {
        a.with_inner_mut(|i| i.entries.push((97, Sexp::Int(2))));
    }
    assert_eq!(a.inner.entries, vec![(97, Sexp::Int(2))]);
    assert_eq!(b.inner.entries, vec![(97, Sexp::Int(2))]);
}

#[test]
fn with_inner_mut_returns_value_from_closure() {
    let r = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    let count = unsafe { r.with_inner_mut(|i| i.entries.len()) };
    assert_eq!(count, 0);
}

#[test]
fn parent_self_reference_drops_clean() {
    let parent = NlCharTableRef::new(empty_inner(sym("base"), Sexp::Int(7)));
    let child = NlCharTableRef::new(empty_inner(sym("derived"), Sexp::Nil));
    unsafe {
        child.with_inner_mut(|i| i.parent = Some(parent.clone()));
    }
    assert_eq!(NlCharTableRef::strong_count(&parent), 2);
    drop(child);
    assert_eq!(NlCharTableRef::strong_count(&parent), 1);
}

#[test]
fn parent_chain_two_deep() {
    let grandparent = NlCharTableRef::new(empty_inner(sym("g"), Sexp::Int(1)));
    let parent = NlCharTableRef::new(empty_inner(sym("p"), Sexp::Int(2)));
    let child = NlCharTableRef::new(empty_inner(sym("c"), Sexp::Nil));
    unsafe {
        parent.with_inner_mut(|i| i.parent = Some(grandparent.clone()));
        child.with_inner_mut(|i| i.parent = Some(parent.clone()));
    }
    assert_eq!(NlCharTableRef::strong_count(&grandparent), 2);
    assert_eq!(NlCharTableRef::strong_count(&parent), 2);
    drop(parent);
    assert_eq!(NlCharTableRef::strong_count(&grandparent), 2);
    drop(child);
    assert_eq!(NlCharTableRef::strong_count(&grandparent), 1);
}

#[test]
fn debug_format_uses_chartable_struct() {
    let r = NlCharTableRef::new(empty_inner(sym("syntax"), Sexp::Nil));
    let d = format!("{:?}", r);
    assert!(
        d.starts_with("CharTable"),
        "expected `CharTable {{...}}' debug shape, got {:?}",
        d
    );
}

#[test]
fn partial_eq_same_handle_short_circuits() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn partial_eq_distinct_alloc_compares_value() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Int(1)));
    let b = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Int(1)));
    assert_eq!(a, b);
    let c = NlCharTableRef::new(empty_inner(sym("y"), Sexp::Int(1)));
    assert_ne!(a, c);
    let d = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Int(2)));
    assert_ne!(a, d);
}

#[test]
fn payload_drop_runs_exactly_once() {
    let mut inner = empty_inner(sym("big"), Sexp::Nil);
    for i in 0..100 {
        inner.entries.push((i, Sexp::Int(i)));
    }
    let r = NlCharTableRef::new(inner);
    {
        let _t = r.clone();
    }
    drop(r);
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlCharTableRef::new(empty_inner(sym("x"), Sexp::Nil));
    {
        let _b = a.clone();
    }
    let c = a.clone();
    assert!(NlCharTableRef::ptr_eq(&a, &c));
}

#[test]
fn extra_slots_round_trip() {
    let mut inner = empty_inner(sym("case"), Sexp::Nil);
    inner.extra = vec![Sexp::Str("up".into()), Sexp::Str("down".into())];
    let r = NlCharTableRef::new(inner);
    assert_eq!(r.inner.extra.len(), 2);
    assert_eq!(r.inner.extra[0], Sexp::Str("up".into()));
    assert_eq!(r.inner.extra[1], Sexp::Str("down".into()));
}

#[test]
fn empty_chartable_round_trip() {
    let r = NlCharTableRef::new(empty_inner(Sexp::Nil, Sexp::Nil));
    assert!(r.inner.entries.is_empty());
    assert!(r.inner.parent.is_none());
    assert!(r.inner.extra.is_empty());
    assert_eq!(NlCharTableRef::strong_count(&r), 1);
}

#[test]
fn subtype_and_default_preserved_on_clone() {
    let a = NlCharTableRef::new(empty_inner(sym("syntax"), Sexp::Int(99)));
    let b = a.clone();
    assert_eq!(a.inner.subtype, sym("syntax"));
    assert_eq!(a.inner.default_val, Sexp::Int(99));
    assert_eq!(b.inner.subtype, sym("syntax"));
    assert_eq!(b.inner.default_val, Sexp::Int(99));
}
