use nelisp_build_tool::eval::nlstr::*;

#[test]
fn layout_value_at_offset_0() {
    use std::mem::offset_of;
    assert_eq!(offset_of!(NlStr, value), 0);
}

#[test]
fn layout_refcount_after_value() {
    use std::mem::{offset_of, size_of};
    assert_eq!(offset_of!(NlStr, refcount), size_of::<String>());
}

#[test]
fn new_starts_with_refcount_1() {
    let s = NlStrRef::new("hello".to_string());
    assert_eq!(NlStrRef::strong_count(&s), 1);
}

#[test]
fn new_returns_box_holding_value() {
    let s = NlStrRef::new("hello".to_string());
    assert_eq!(s.value, "hello");
}

#[test]
fn clone_bumps_refcount_and_shares_value() {
    let a = NlStrRef::new("shared".to_string());
    let b = a.clone();
    assert_eq!(NlStrRef::strong_count(&a), 2);
    assert_eq!(b.value, "shared");
}

#[test]
fn drop_decrements_refcount() {
    let a = NlStrRef::new("x".to_string());
    {
        let _b = a.clone();
        assert_eq!(NlStrRef::strong_count(&a), 2);
    }
    assert_eq!(NlStrRef::strong_count(&a), 1);
}

#[test]
fn ptr_eq_same() {
    let a = NlStrRef::new("a".to_string());
    let b = a.clone();
    assert!(NlStrRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different_alloc() {
    let a = NlStrRef::new("same".to_string());
    let b = NlStrRef::new("same".to_string());
    assert!(!NlStrRef::ptr_eq(&a, &b));
}

#[test]
fn set_value_replaces_in_place() {
    let s = NlStrRef::new("old".to_string());
    unsafe { s.set_value("new".to_string()) };
    assert_eq!(s.value, "new");
}

#[test]
fn set_value_visible_through_clone() {
    let a = NlStrRef::new("v1".to_string());
    let b = a.clone();
    unsafe { a.set_value("v2".to_string()) };
    assert_eq!(a.value, "v2");
    assert_eq!(b.value, "v2");
}

#[test]
fn with_value_mut_in_place_mutation() {
    let s = NlStrRef::new("hello".to_string());
    unsafe {
        s.with_value_mut(|v| {
            v.push_str(", world");
        });
    }
    assert_eq!(s.value, "hello, world");
}

#[test]
fn with_value_mut_returns_value_from_closure() {
    let s = NlStrRef::new("abc".to_string());
    let len = unsafe { s.with_value_mut(|v| v.len()) };
    assert_eq!(len, 3);
}

#[test]
fn debug_format_uses_mutstr_tuple() {
    let s = NlStrRef::new("x".to_string());
    let d = format!("{:?}", s);
    assert!(
        d.starts_with("MutStr("),
        "expected `MutStr(...)' debug shape, got {:?}",
        d
    );
}

#[test]
fn partial_eq_same_handle_short_circuits() {
    let a = NlStrRef::new("hi".to_string());
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn partial_eq_distinct_alloc_compares_value() {
    let a = NlStrRef::new("hi".to_string());
    let b = NlStrRef::new("hi".to_string());
    assert_eq!(a, b);
    let c = NlStrRef::new("ho".to_string());
    assert_ne!(a, c);
}

#[test]
fn payload_drop_runs_exactly_once() {
    let s = NlStrRef::new("payload".to_string());
    {
        let _t = s.clone();
    }
    drop(s);
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlStrRef::new("a".to_string());
    {
        let _b = a.clone();
    }
    let c = a.clone();
    assert!(NlStrRef::ptr_eq(&a, &c));
}

#[test]
fn set_value_drops_previous_string() {
    let s = NlStrRef::new("0".repeat(1024));
    unsafe { s.set_value("1".to_string()) };
    assert_eq!(s.value, "1");
}

#[test]
fn with_value_mut_truncate() {
    let s = NlStrRef::new("longstring".to_string());
    unsafe {
        s.with_value_mut(|v| v.truncate(4));
    }
    assert_eq!(s.value, "long");
}
