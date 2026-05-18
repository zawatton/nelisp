//! Integration tests for `nelisp_build_tool::eval::nlcell`.
//! Moved from `src/eval/nlcell.rs#[cfg(test)] mod tests`.

use nelisp_build_tool::eval::nlcell::*;
use nelisp_build_tool::eval::sexp::Sexp;
use std::sync::atomic::AtomicUsize;

#[test]
fn layout_value_at_offset_0() {
    use std::mem::offset_of;
    assert_eq!(offset_of!(NlCell, value), 0);
}

#[test]
fn layout_refcount_after_value() {
    use std::mem::{offset_of, size_of};
    assert_eq!(offset_of!(NlCell, refcount), size_of::<Sexp>());
}

#[test]
fn layout_refcount_size_8() {
    use std::mem::size_of;
    assert_eq!(size_of::<AtomicUsize>(), 8);
}

#[test]
fn new_starts_with_refcount_1() {
    let c = NlCellRef::new(Sexp::Int(42));
    assert_eq!(NlCellRef::strong_count(&c), 1);
}

#[test]
fn new_returns_cell_holding_value() {
    let c = NlCellRef::new(Sexp::Int(42));
    assert_eq!(c.value, Sexp::Int(42));
}

#[test]
fn clone_bumps_refcount() {
    let a = NlCellRef::new(Sexp::Nil);
    let b = a.clone();
    assert_eq!(NlCellRef::strong_count(&a), 2);
    assert_eq!(NlCellRef::strong_count(&b), 2);
}

#[test]
fn drop_decrements_refcount() {
    let a = NlCellRef::new(Sexp::Nil);
    {
        let _b = a.clone();
        assert_eq!(NlCellRef::strong_count(&a), 2);
    }
    assert_eq!(NlCellRef::strong_count(&a), 1);
}

#[test]
fn ptr_eq_same() {
    let a = NlCellRef::new(Sexp::Nil);
    let b = a.clone();
    assert!(NlCellRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different() {
    let a = NlCellRef::new(Sexp::Nil);
    let b = NlCellRef::new(Sexp::Nil);
    assert!(!NlCellRef::ptr_eq(&a, &b));
}

#[test]
fn set_value_mutates_in_place() {
    let c = NlCellRef::new(Sexp::Int(1));
    unsafe { c.set_value(Sexp::Int(99)) };
    assert_eq!(c.value, Sexp::Int(99));
}

#[test]
fn set_value_visible_through_clone() {
    let a = NlCellRef::new(Sexp::Int(1));
    let b = a.clone();
    unsafe { a.set_value(Sexp::Int(7)) };
    assert_eq!(a.value, Sexp::Int(7));
    assert_eq!(b.value, Sexp::Int(7));
}

#[test]
fn set_value_drops_previous_payload() {
    let inner = NlCellRef::new(Sexp::Nil);
    let outer = NlCellRef::new(Sexp::Cell(inner.clone()));
    assert_eq!(NlCellRef::strong_count(&inner), 2);
    unsafe { outer.set_value(Sexp::Nil) };
    assert_eq!(NlCellRef::strong_count(&inner), 1);
}

#[test]
fn debug_format_uses_cell_tuple() {
    let c = NlCellRef::new(Sexp::Int(7));
    let s = format!("{:?}", c);
    assert!(
        s.starts_with("Cell("),
        "expected `Cell(...)' debug shape, got {:?}",
        s
    );
}

#[test]
fn partial_eq_same_handle_short_circuits() {
    let a = NlCellRef::new(Sexp::Int(1));
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn partial_eq_distinct_alloc_compares_value() {
    let a = NlCellRef::new(Sexp::Int(42));
    let b = NlCellRef::new(Sexp::Int(42));
    assert_eq!(a, b);
    let c = NlCellRef::new(Sexp::Int(43));
    assert_ne!(a, c);
}

#[test]
fn payload_drop_runs_exactly_once() {
    use nelisp_build_tool::eval::nlvector::NlVectorRef;
    let probe = NlVectorRef::new(vec![Sexp::Int(1)]);
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
    {
        let _c = NlCellRef::new(Sexp::Vector(probe.clone()));
        assert_eq!(NlVectorRef::strong_count(&probe), 2);
    }
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
}

#[test]
fn nl_alloc_cell_returns_initialised_cell_with_refcount_1() {
    let initial = Sexp::Int(42);
    let raw = unsafe { nl_alloc_cell(&initial as *const Sexp) };
    assert!(!raw.is_null());
    let owned = unsafe { NlCellRef::from_raw_ptr(raw) };
    assert_eq!(NlCellRef::strong_count(&owned), 1);
    assert_eq!(owned.value, Sexp::Int(42));
}

#[test]
fn nl_cell_set_value_overwrites_slot_and_drops_old() {
    use nelisp_build_tool::eval::nlvector::NlVectorRef;
    let probe = NlVectorRef::new(vec![Sexp::Int(1)]);
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
    let initial = Sexp::Vector(probe.clone());
    assert_eq!(NlVectorRef::strong_count(&probe), 2);
    let raw = unsafe { nl_alloc_cell(&initial as *const Sexp) };
    assert!(!raw.is_null());
    assert_eq!(NlVectorRef::strong_count(&probe), 3);
    drop(initial);
    assert_eq!(NlVectorRef::strong_count(&probe), 2);
    let new_val = Sexp::Int(7);
    unsafe { nl_cell_set_value(raw, &new_val as *const Sexp) };
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
    let owned = unsafe { NlCellRef::from_raw_ptr(raw) };
    assert_eq!(owned.value, Sexp::Int(7));
    drop(owned);
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlCellRef::new(Sexp::Nil);
    {
        let _b = a.clone();
        let _c = a.clone();
    }
    let d = a.clone();
    assert!(NlCellRef::ptr_eq(&a, &d));
}
