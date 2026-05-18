//! Integration tests for `nelisp_build_tool::eval::nlboolvector`.
//! Moved from `src/eval/nlboolvector.rs#[cfg(test)] mod tests`.

use nelisp_build_tool::eval::nlboolvector::*;

#[test]
fn layout_value_at_offset_0() {
    use std::mem::offset_of;
    assert_eq!(offset_of!(NlBoolVector, value), 0);
}

#[test]
fn layout_refcount_after_value() {
    use std::mem::{offset_of, size_of};
    assert_eq!(offset_of!(NlBoolVector, refcount), size_of::<Vec<bool>>());
}

#[test]
fn new_starts_with_refcount_1() {
    let v = NlBoolVectorRef::new(vec![true, false]);
    assert_eq!(NlBoolVectorRef::strong_count(&v), 1);
}

#[test]
fn new_returns_box_holding_value() {
    let v = NlBoolVectorRef::new(vec![true, false, true]);
    assert_eq!(v.value.len(), 3);
    assert_eq!(v.value[0], true);
    assert_eq!(v.value[2], true);
}

#[test]
fn clone_bumps_refcount_and_shares_value() {
    let a = NlBoolVectorRef::new(vec![true]);
    let b = a.clone();
    assert_eq!(NlBoolVectorRef::strong_count(&a), 2);
    assert_eq!(b.value, vec![true]);
}

#[test]
fn drop_decrements_refcount() {
    let a = NlBoolVectorRef::new(vec![]);
    {
        let _b = a.clone();
        assert_eq!(NlBoolVectorRef::strong_count(&a), 2);
    }
    assert_eq!(NlBoolVectorRef::strong_count(&a), 1);
}

#[test]
fn ptr_eq_same() {
    let a = NlBoolVectorRef::new(vec![true]);
    let b = a.clone();
    assert!(NlBoolVectorRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different_alloc() {
    let a = NlBoolVectorRef::new(vec![true]);
    let b = NlBoolVectorRef::new(vec![true]);
    assert!(!NlBoolVectorRef::ptr_eq(&a, &b));
}

#[test]
fn set_value_replaces_in_place() {
    let v = NlBoolVectorRef::new(vec![true]);
    unsafe { v.set_value(vec![false, true]) };
    assert_eq!(v.value.len(), 2);
    assert_eq!(v.value[0], false);
}

#[test]
fn set_value_visible_through_clone() {
    let a = NlBoolVectorRef::new(vec![true]);
    let b = a.clone();
    unsafe { a.set_value(vec![false]) };
    assert_eq!(a.value, vec![false]);
    assert_eq!(b.value, vec![false]);
}

#[test]
fn with_value_mut_index_set() {
    let v = NlBoolVectorRef::new(vec![true, true, true]);
    unsafe {
        v.with_value_mut(|vec| {
            vec[1] = false;
        });
    }
    assert_eq!(v.value[1], false);
}

#[test]
fn with_value_mut_push() {
    let v = NlBoolVectorRef::new(vec![]);
    unsafe {
        v.with_value_mut(|vec| {
            vec.push(true);
            vec.push(false);
        });
    }
    assert_eq!(v.value.len(), 2);
}

#[test]
fn with_value_mut_returns_value_from_closure() {
    let v = NlBoolVectorRef::new(vec![true]);
    let len = unsafe { v.with_value_mut(|vec| vec.len()) };
    assert_eq!(len, 1);
}

#[test]
fn debug_format_uses_boolvector_tuple() {
    let v = NlBoolVectorRef::new(vec![true]);
    let d = format!("{:?}", v);
    assert!(
        d.starts_with("BoolVector("),
        "expected `BoolVector(...)' debug shape, got {:?}",
        d
    );
}

#[test]
fn partial_eq_same_handle_short_circuits() {
    let a = NlBoolVectorRef::new(vec![true]);
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn partial_eq_distinct_alloc_compares_value() {
    let a = NlBoolVectorRef::new(vec![true, false]);
    let b = NlBoolVectorRef::new(vec![true, false]);
    assert_eq!(a, b);
    let c = NlBoolVectorRef::new(vec![true, true]);
    assert_ne!(a, c);
    let d = NlBoolVectorRef::new(vec![true]);
    assert_ne!(a, d);
}

#[test]
fn payload_drop_runs_exactly_once() {
    let v = NlBoolVectorRef::new(vec![true; 100]);
    {
        let _t = v.clone();
    }
    drop(v);
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlBoolVectorRef::new(vec![true]);
    {
        let _b = a.clone();
    }
    let c = a.clone();
    assert!(NlBoolVectorRef::ptr_eq(&a, &c));
}

#[test]
fn empty_vector_round_trip() {
    let v = NlBoolVectorRef::new(vec![]);
    assert_eq!(v.value.len(), 0);
    assert_eq!(NlBoolVectorRef::strong_count(&v), 1);
}
