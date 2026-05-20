#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;
use std::sync::atomic::{AtomicI64, Ordering};

const SIZE_OF_NLRECORD: i64 = 64;

const ALIGN_OF_NLRECORD: i64 = 8;

const REFCOUNT_OFFSET: i64 = 56;

unsafe fn alloc_probe_box(initial_refcount: i64) -> *mut u8 {
    let ptr = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLRECORD, ALIGN_OF_NLRECORD)
    };
    assert!(
        !ptr.is_null(),
        "alloc-bytes({}, {}) must succeed on a healthy host",
        SIZE_OF_NLRECORD,
        ALIGN_OF_NLRECORD,
    );
    // Doc 124 §124.L: initialize `type_tag: Sexp' to `Sexp::Nil' and
    // `slots: Vec<Sexp>' to a fresh empty `Vec::new()' so the §124.L
    // inner-drop step (= `drop_in_place::<NlRecord>') walks valid
    // values rather than uninitialized bytes (= UB).  Both are trivial
    // drops (Sexp::Nil = no payload, empty Vec = cap=0 skips dealloc).
    unsafe {
        std::ptr::write(ptr as *mut Sexp, Sexp::Nil);
        std::ptr::write((ptr as usize + 32) as *mut Vec<Sexp>, Vec::new());
    }
    // Seed the refcount slot via direct AtomicI64 store — the elisp
    // kernel will read/write it through `nl_atomic_fetch_add', so we
    // need a well-defined initial value at the trailer offset.
    let refcount_slot = (ptr as usize + REFCOUNT_OFFSET as usize) as *mut AtomicI64;
    unsafe {
        std::ptr::write(refcount_slot, AtomicI64::new(initial_refcount));
    }
    ptr
}

unsafe fn read_refcount(ptr: *mut u8) -> i64 {
    let refcount_slot = (ptr as usize + REFCOUNT_OFFSET as usize) as *const AtomicI64;
    unsafe { (*refcount_slot).load(Ordering::SeqCst) }
}

// ---- Case 1: Drop from refcount=2 — no dealloc, slot lands at 1 ----

#[test]
fn nlrecord_drop_with_refcount_2_no_dealloc() {
    // Pre-condition: layout invariant.  REFCOUNT_OFFSET = 56 must
    // match the `nlrecord.rs:251-253' compile-time assert; if either
    // Sexp size (currently 32 via `sexp_abi_assert.rs:45') or Vec
    // header size (locked at 24 = 3 × usize on 64-bit) ever changes,
    // this constant updates in lockstep.
    assert_eq!(REFCOUNT_OFFSET, 56);
    assert_eq!(SIZE_OF_NLRECORD, 64);
    assert_eq!(ALIGN_OF_NLRECORD, 8);

    let ptr = unsafe { alloc_probe_box(2) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 2, "seeded refcount slot must read back as 2");

    let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_drop(ptr as *mut i64) };
    assert_eq!(
        ret, 1,
        "nlrecord_drop must return 1 sentinel on both branches"
    );

    let after = unsafe { read_refcount(ptr) };
    assert_eq!(
        after, 1,
        "post-drop refcount slot must be initial - 1 (= 2 - 1 = 1) \
         when the box is not yet free"
    );

    let cleanup_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(ptr, SIZE_OF_NLRECORD, ALIGN_OF_NLRECORD)
    };
    assert_eq!(cleanup_rc, 1);
}

// ---- Case 2: Drop from refcount=1 — dealloc happens, return = 1 ----

#[test]
fn nlrecord_drop_with_refcount_1_dealloc_happens() {
    let ptr = unsafe { alloc_probe_box(1) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 1, "seeded refcount slot must read back as 1");

    let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_drop(ptr as *mut i64) };
    assert_eq!(
        ret, 1,
        "nlrecord_drop on last-ref must return dealloc-bytes's 1 sentinel"
    );

    let probe = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLRECORD, ALIGN_OF_NLRECORD)
    };
    assert!(
        !probe.is_null(),
        "post-drop allocator must still be functional \
         (= alloc-bytes for a fresh block succeeds)"
    );
    let probe_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(probe, SIZE_OF_NLRECORD, ALIGN_OF_NLRECORD)
    };
    assert_eq!(probe_rc, 1);
}

// ---- Case 3: N consecutive drops — slot walks N → 0 — final dealloc ----

#[test]
fn nlrecord_drop_n_consecutive_reaches_zero_and_deallocs() {
    const N: i64 = 5;

    let ptr = unsafe { alloc_probe_box(N) };
    assert_eq!(unsafe { read_refcount(ptr) }, N);

    for i in 1..N {
        let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_drop(ptr as *mut i64) };
        assert_eq!(ret, 1, "drop {} of {}: must return 1 sentinel", i, N - 1);
        let after = unsafe { read_refcount(ptr) };
        assert_eq!(
            after,
            N - i,
            "after drop {}: slot must be N - i = {} - {} = {}",
            i,
            N,
            i,
            N - i,
        );
    }
    assert_eq!(unsafe { read_refcount(ptr) }, 1);

    let final_ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_drop(ptr as *mut i64) };
    assert_eq!(
        final_ret, 1,
        "final drop on pre-sub=1 must hit dealloc-bytes and return 1"
    );

    let probe = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLRECORD, ALIGN_OF_NLRECORD)
    };
    assert!(!probe.is_null());
    unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
            probe,
            SIZE_OF_NLRECORD,
            ALIGN_OF_NLRECORD,
        );
    }
}
