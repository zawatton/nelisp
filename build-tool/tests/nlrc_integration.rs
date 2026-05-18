//! Integration tests for `nelisp_build_tool::eval::nlrc`.
//! Moved from `src/eval/nlrc.rs#[cfg(test)] mod tests`.

use nelisp_build_tool::eval::nlrc::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

// ---- Layout invariants (= Doc 77c §2.1.1 contract) ----

#[test]
fn layout_refcount_at_offset_0() {
    assert_eq!(std::mem::offset_of!(NlRcInner<i64>, refcount), 0);
}

#[test]
fn layout_value_at_offset_8() {
    assert_eq!(std::mem::offset_of!(NlRcInner<i64>, value), 8);
}

#[test]
fn layout_atomic_usize_is_8_bytes() {
    assert_eq!(std::mem::size_of::<AtomicUsize>(), 8);
}

// ---- Basic refcount semantics ----

#[test]
fn new_then_drop_frees() {
    let rc = NlRc::new(42i64);
    assert_eq!(NlRc::strong_count(&rc), 1);
    drop(rc);
}

#[test]
fn new_starts_with_refcount_1() {
    let rc = NlRc::new("hello".to_string());
    assert_eq!(NlRc::strong_count(&rc), 1);
}

#[test]
fn clone_increments_refcount() {
    let rc = NlRc::new(1u64);
    let _c1 = rc.clone();
    let _c2 = rc.clone();
    assert_eq!(NlRc::strong_count(&rc), 3);
}

#[test]
fn drop_decrements_refcount() {
    let rc = NlRc::new(1u64);
    let c1 = rc.clone();
    let c2 = rc.clone();
    assert_eq!(NlRc::strong_count(&rc), 3);
    drop(c1);
    assert_eq!(NlRc::strong_count(&rc), 2);
    drop(c2);
    assert_eq!(NlRc::strong_count(&rc), 1);
}

// ---- Deref ----

#[test]
fn deref_reads_value_i64() {
    let v = 0xDEAD_BEEF_CAFE_BABEu64 as i64;
    let rc = NlRc::new(v);
    assert_eq!(*rc, v);
}

#[test]
fn deref_reads_value_string() {
    let rc = NlRc::new(String::from("nelisp"));
    assert_eq!(rc.as_str(), "nelisp");
    assert_eq!(rc.len(), 6);
}

#[test]
fn deref_through_clone_sees_same_value() {
    let rc = NlRc::new(vec![1, 2, 3]);
    let c = rc.clone();
    assert_eq!(*rc, *c);
    assert_eq!(rc.len(), 3);
    assert_eq!(c.len(), 3);
}

// ---- Pointer identity ----

#[test]
fn ptr_eq_same_ptr() {
    let a = NlRc::new(7i32);
    let b = a.clone();
    assert!(NlRc::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_different_allocations() {
    let a = NlRc::new(7i32);
    let b = NlRc::new(7i32);
    assert!(!NlRc::ptr_eq(&a, &b));
}

#[test]
fn ptr_eq_after_intermediate_drops() {
    let a = NlRc::new(99i32);
    let b = a.clone();
    let c = a.clone();
    drop(b);
    assert!(NlRc::ptr_eq(&a, &c));
}

// ---- Composite payload ----

#[test]
fn composite_struct() {
    #[derive(Debug, PartialEq, Eq)]
    struct Compound {
        n: i32,
        s: String,
    }
    let rc = NlRc::new(Compound {
        n: 42,
        s: "doc-77c".into(),
    });
    assert_eq!(rc.n, 42);
    assert_eq!(rc.s, "doc-77c");
    let c = rc.clone();
    assert_eq!(c.n, 42);
    assert!(NlRc::ptr_eq(&rc, &c));
}

#[test]
fn composite_tuple() {
    let rc = NlRc::new((1i32, "two".to_string(), 3.0f64));
    assert_eq!(rc.0, 1);
    assert_eq!(rc.1, "two");
    assert_eq!(rc.2, 3.0);
}

// ---- Lifecycle: many clones / many drops ----

#[test]
fn multi_clone_drop() {
    let rc = NlRc::new(0xABCDu32);
    let clones: Vec<NlRc<u32>> = (0..5).map(|_| rc.clone()).collect();
    assert_eq!(NlRc::strong_count(&rc), 6);
    let mut iter = clones.into_iter();
    drop(iter.next().unwrap());
    drop(iter.next().unwrap());
    drop(iter.next().unwrap());
    drop(iter.next().unwrap());
    assert_eq!(NlRc::strong_count(&rc), 2);
    drop(iter.next().unwrap());
    assert_eq!(NlRc::strong_count(&rc), 1);
}

// ---- Payload drop is exactly-once ----

#[test]
fn payload_drop_runs_exactly_once() {
    struct DropProbe(Arc<AtomicUsize>);
    impl Drop for DropProbe {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
    }
    let drops = Arc::new(AtomicUsize::new(0));
    let probe = DropProbe(drops.clone());
    let rc = NlRc::new(probe);
    let c1 = rc.clone();
    let c2 = rc.clone();
    assert_eq!(drops.load(Ordering::SeqCst), 0);
    drop(c1);
    drop(c2);
    assert_eq!(drops.load(Ordering::SeqCst), 0);
    drop(rc);
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn refcount_atomic_round_trip() {
    let rc = NlRc::new(1i64);
    let inner_raw = NlRc::inner_raw(&rc);
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

#[test]
fn niche_optimization_preserved() {
    assert_eq!(
        std::mem::size_of::<Option<NlRc<i64>>>(),
        std::mem::size_of::<NlRc<i64>>(),
    );
}
