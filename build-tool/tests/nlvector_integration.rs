use nelisp_build_tool::eval::nlvector::*;
use nelisp_build_tool::eval::sexp::Sexp;

#[test]
fn layout_value_at_offset_0() {
    use std::mem::offset_of;
    assert_eq!(offset_of!(NlVector, value), 0);
}

#[test]
fn layout_refcount_after_value() {
    use std::mem::{offset_of, size_of};
    assert_eq!(offset_of!(NlVector, refcount), size_of::<Vec<Sexp>>());
}

#[test]
fn new_starts_with_refcount_1() {
    let v = NlVectorRef::new(vec![Sexp::Int(7), Sexp::Int(8)]);
    assert_eq!(NlVectorRef::strong_count(&v), 1);
}

#[test]
fn new_returns_box_holding_value() {
    let v = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
    assert_eq!(v.value.len(), 3);
    assert_eq!(v.value[0], Sexp::Int(1));
    assert_eq!(v.value[2], Sexp::Int(3));
}

#[test]
fn clone_bumps_refcount_and_shares_value() {
    let a = NlVectorRef::new(vec![Sexp::Int(42)]);
    let b = a.clone();
    assert_eq!(NlVectorRef::strong_count(&a), 2);
    assert_eq!(b.value, vec![Sexp::Int(42)]);
}

#[test]
fn drop_decrements_refcount() {
    let a = NlVectorRef::new(vec![]);
    {
        let _b = a.clone();
        assert_eq!(NlVectorRef::strong_count(&a), 2);
    }
    assert_eq!(NlVectorRef::strong_count(&a), 1);
}

#[test]
fn ptr_eq_same() {
    let a = NlVectorRef::new(vec![Sexp::Nil]);
    let b = a.clone();
    assert!(NlVectorRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different_alloc() {
    let a = NlVectorRef::new(vec![Sexp::Int(1)]);
    let b = NlVectorRef::new(vec![Sexp::Int(1)]);
    assert!(!NlVectorRef::ptr_eq(&a, &b));
}

#[test]
fn set_value_replaces_in_place() {
    let v = NlVectorRef::new(vec![Sexp::Int(1)]);
    unsafe { v.set_value(vec![Sexp::Int(99), Sexp::Int(100)]) };
    assert_eq!(v.value.len(), 2);
    assert_eq!(v.value[0], Sexp::Int(99));
}

#[test]
fn set_value_visible_through_clone() {
    let a = NlVectorRef::new(vec![Sexp::Int(1)]);
    let b = a.clone();
    unsafe { a.set_value(vec![Sexp::Int(7)]) };
    assert_eq!(a.value, vec![Sexp::Int(7)]);
    assert_eq!(b.value, vec![Sexp::Int(7)]);
}

#[test]
fn with_value_mut_index_set() {
    let v = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
    unsafe {
        v.with_value_mut(|vec| {
            vec[1] = Sexp::Int(99);
        });
    }
    assert_eq!(v.value[1], Sexp::Int(99));
}

#[test]
fn with_value_mut_push() {
    let v = NlVectorRef::new(vec![]);
    unsafe {
        v.with_value_mut(|vec| {
            vec.push(Sexp::Int(1));
            vec.push(Sexp::Int(2));
        });
    }
    assert_eq!(v.value.len(), 2);
}

#[test]
fn with_value_mut_returns_value_from_closure() {
    let v = NlVectorRef::new(vec![Sexp::Int(10)]);
    let len = unsafe { v.with_value_mut(|vec| vec.len()) };
    assert_eq!(len, 1);
}

#[test]
fn debug_format_uses_vector_tuple() {
    let v = NlVectorRef::new(vec![Sexp::Int(1)]);
    let d = format!("{:?}", v);
    assert!(
        d.starts_with("Vector("),
        "expected `Vector(...)' debug shape, got {:?}",
        d
    );
}

#[test]
fn partial_eq_same_handle_short_circuits() {
    let a = NlVectorRef::new(vec![Sexp::Int(1)]);
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn partial_eq_distinct_alloc_compares_value() {
    let a = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2)]);
    let b = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2)]);
    assert_eq!(a, b);
    let c = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(3)]);
    assert_ne!(a, c);
    let d = NlVectorRef::new(vec![Sexp::Int(1)]);
    assert_ne!(a, d);
}

#[test]
fn payload_drop_runs_exactly_once() {
    let v = NlVectorRef::new(vec![Sexp::Int(1); 100]);
    {
        let _t = v.clone();
    }
    drop(v);
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlVectorRef::new(vec![Sexp::Nil]);
    {
        let _b = a.clone();
    }
    let c = a.clone();
    assert!(NlVectorRef::ptr_eq(&a, &c));
}

#[test]
fn empty_vector_round_trip() {
    let v = NlVectorRef::new(vec![]);
    assert_eq!(v.value.len(), 0);
    assert_eq!(NlVectorRef::strong_count(&v), 1);
}
