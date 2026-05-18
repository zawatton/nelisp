//! Integration tests for `nelisp_build_tool::eval::nlconsbox`.
//! Moved from `src/eval/nlconsbox.rs#[cfg(test)] mod tests`.

use nelisp_build_tool::eval::nlconsbox::*;
use nelisp_build_tool::eval::sexp::Sexp;
use std::sync::atomic::{AtomicUsize, Ordering};

// ---- Layout invariants (= Doc 77c §2.1.2 contract) ----

#[test]
fn layout_car_at_offset_0() {
    assert_eq!(std::mem::offset_of!(NlConsBox, car), 0);
}

#[test]
fn layout_cdr_after_car() {
    assert_eq!(
        std::mem::offset_of!(NlConsBox, cdr),
        std::mem::size_of::<Sexp>()
    );
}

#[test]
fn layout_refcount_at_trailer() {
    assert_eq!(
        std::mem::offset_of!(NlConsBox, refcount),
        2 * std::mem::size_of::<Sexp>()
    );
}

#[test]
fn layout_atomic_usize_is_8_bytes() {
    assert_eq!(std::mem::size_of::<AtomicUsize>(), 8);
}

// ---- Basic refcount semantics ----

#[test]
fn new_starts_with_refcount_1() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    assert_eq!(NlConsBoxRef::strong_count(&r), 1);
}

#[test]
fn clone_increments_refcount() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
    let _c1 = r.clone();
    let _c2 = r.clone();
    assert_eq!(NlConsBoxRef::strong_count(&r), 3);
}

#[test]
fn drop_decrements_refcount() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
    let c1 = r.clone();
    let c2 = r.clone();
    assert_eq!(NlConsBoxRef::strong_count(&r), 3);
    drop(c1);
    assert_eq!(NlConsBoxRef::strong_count(&r), 2);
    drop(c2);
    assert_eq!(NlConsBoxRef::strong_count(&r), 1);
}

#[test]
fn ptr_eq_clones_share_box() {
    let a = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    let b = a.clone();
    assert!(NlConsBoxRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_distinct_allocations() {
    let a = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    let b = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    assert!(!NlConsBoxRef::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlConsBoxRef::new(Sexp::Int(99), Sexp::Nil);
    let b = a.clone();
    let c = a.clone();
    drop(b);
    assert!(NlConsBoxRef::ptr_eq(&a, &c));
}

// ---- Deref / field access ----

#[test]
fn deref_reads_car_cdr() {
    let r = NlConsBoxRef::new(Sexp::Int(42), Sexp::Symbol("x".into()));
    match (&r.car, &r.cdr) {
        (Sexp::Int(42), Sexp::Symbol(s)) => assert_eq!(s, "x"),
        (c, d) => panic!("unexpected ({:?} . {:?})", c, d),
    }
}

#[test]
fn deref_through_clone_sees_same_payload() {
    let r = NlConsBoxRef::new(Sexp::Int(7), Sexp::Nil);
    let c = r.clone();
    assert!(matches!(&c.car, Sexp::Int(7)));
    assert!(matches!(&c.cdr, Sexp::Nil));
}

// ---- set_car / set_cdr ----

#[test]
fn set_car_overwrites_in_place() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
    unsafe { r.set_car(Sexp::Int(99)) };
    assert!(matches!(&r.car, Sexp::Int(99)));
}

#[test]
fn set_cdr_overwrites_in_place() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
    unsafe { r.set_cdr(Sexp::T) };
    assert!(matches!(&r.cdr, Sexp::T));
}

#[test]
fn set_car_visible_through_clone() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
    let c = r.clone();
    unsafe { r.set_car(Sexp::Int(42)) };
    assert!(matches!(&c.car, Sexp::Int(42)));
}

// ---- Lifecycle: payload destructor exactly-once ----

#[test]
fn payload_drop_runs_exactly_once() {
    use nelisp_build_tool::eval::nlvector::NlVectorRef;
    let probe = NlVectorRef::new(vec![Sexp::Int(1)]);
    let payload = Sexp::Vector(probe.clone());
    let r = NlConsBoxRef::new(payload, Sexp::Nil);
    let c1 = r.clone();
    let c2 = r.clone();
    assert_eq!(NlVectorRef::strong_count(&probe), 2);
    drop(c1);
    drop(c2);
    assert_eq!(NlVectorRef::strong_count(&probe), 2);
    drop(r);
    assert_eq!(NlVectorRef::strong_count(&probe), 1);
}

#[test]
fn multi_clone_drop() {
    let r = NlConsBoxRef::new(Sexp::Int(0xABCD), Sexp::Nil);
    let clones: Vec<NlConsBoxRef> = (0..5).map(|_| r.clone()).collect();
    assert_eq!(NlConsBoxRef::strong_count(&r), 6);
    let mut iter = clones.into_iter();
    for _ in 0..4 {
        drop(iter.next().unwrap());
    }
    assert_eq!(NlConsBoxRef::strong_count(&r), 2);
    drop(iter.next().unwrap());
    assert_eq!(NlConsBoxRef::strong_count(&r), 1);
}

// ---- Niche optimization ----

#[test]
fn niche_optimization_preserved() {
    assert_eq!(
        std::mem::size_of::<Option<NlConsBoxRef>>(),
        std::mem::size_of::<NlConsBoxRef>(),
    );
}

// ---- Raw ptr round-trip (Phase A.3 ffi prep) ----

#[test]
fn as_ptr_round_trips_to_field_access() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    let raw = NlConsBoxRef::as_ptr(&r);
    unsafe {
        assert!(matches!((*raw).car, Sexp::Int(1)));
        assert!(matches!((*raw).cdr, Sexp::Int(2)));
        assert_eq!((*raw).refcount.load(Ordering::Acquire), 1);
    }
}

#[test]
fn refcount_atomic_round_trip() {
    let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    let inner_raw = NlConsBoxRef::inner_raw(&r);
    unsafe {
        for _ in 0..1000 {
            (*inner_raw).refcount.fetch_add(1, Ordering::Relaxed);
        }
        assert_eq!((*inner_raw).refcount.load(Ordering::Acquire), 1001);
        for _ in 0..1000 {
            (*inner_raw).refcount.fetch_sub(1, Ordering::Release);
        }
        assert_eq!((*inner_raw).refcount.load(Ordering::Acquire), 1);
    }
}

// ---- Sexp::Cons compatibility (= Phase A.2.1 prep) ----

#[test]
fn nested_cons_payload_via_sexp_cons() {
    let inner = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
    let r = NlConsBoxRef::new(inner, Sexp::Nil);
    match &r.car {
        Sexp::Cons(b) => {
            assert!(matches!(b.car, Sexp::Int(1)));
            assert!(matches!(b.cdr, Sexp::Int(2)));
        }
        other => panic!("expected nested Sexp::Cons, got {:?}", other),
    }
}
