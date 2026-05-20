#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::sync::atomic::{AtomicI64, Ordering};

const SIZE_OF_NLSTR: i64 = 32;

const ALIGN_OF_NLSTR: i64 = 8;

const REFCOUNT_OFFSET: i64 = 24;

unsafe fn alloc_probe_box(initial_refcount: i64) -> *mut u8 {
    let ptr =
        unsafe { nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLSTR, ALIGN_OF_NLSTR) };
    assert!(
        !ptr.is_null(),
        "alloc-bytes({}, {}) must succeed on a healthy host",
        SIZE_OF_NLSTR,
        ALIGN_OF_NLSTR,
    );
    // Doc 124 §124.L: initialize `value: String' to a fresh empty
    // `String::new()' so the §124.L inner-drop step (= `drop_in_place
    // ::<NlStr>') walks a *valid* String header rather than
    // uninitialized bytes (= UB).  An empty String has cap=0 so its
    // Drop skips the heap-dealloc path, making the inner drop a no-op.
    unsafe {
        std::ptr::write(ptr as *mut String, String::new());
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
fn nlstr_drop_with_refcount_2_no_dealloc() {
    // Pre-condition: layout invariant.  REFCOUNT_OFFSET = 24 must
    // match the `nlstr.rs:783' compile-time assert; if String header
    // size ever changes (very unlikely — pinned by the Rust std
    // String ABI: ptr/len/cap = 3 × usize on 64-bit) this constant
    // updates in lockstep.
    assert_eq!(REFCOUNT_OFFSET, 24);
    assert_eq!(SIZE_OF_NLSTR, 32);
    assert_eq!(ALIGN_OF_NLSTR, 8);

    let ptr = unsafe { alloc_probe_box(2) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 2, "seeded refcount slot must read back as 2");

    let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlstr_drop(ptr as *mut i64) };
    assert_eq!(ret, 1, "nlstr_drop must return 1 sentinel on both branches");

    let after = unsafe { read_refcount(ptr) };
    assert_eq!(
        after, 1,
        "post-drop refcount slot must be initial - 1 (= 2 - 1 = 1) \
         when the box is not yet free"
    );

    let cleanup_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(ptr, SIZE_OF_NLSTR, ALIGN_OF_NLSTR)
    };
    assert_eq!(cleanup_rc, 1);
}

// ---- Case 2: Drop from refcount=1 — dealloc happens, return = 1 ----

#[test]
fn nlstr_drop_with_refcount_1_dealloc_happens() {
    let ptr = unsafe { alloc_probe_box(1) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 1, "seeded refcount slot must read back as 1");

    let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlstr_drop(ptr as *mut i64) };
    assert_eq!(
        ret, 1,
        "nlstr_drop on last-ref must return dealloc-bytes's 1 sentinel"
    );

    let probe =
        unsafe { nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLSTR, ALIGN_OF_NLSTR) };
    assert!(
        !probe.is_null(),
        "post-drop allocator must still be functional \
         (= alloc-bytes for a fresh block succeeds)"
    );
    let probe_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(probe, SIZE_OF_NLSTR, ALIGN_OF_NLSTR)
    };
    assert_eq!(probe_rc, 1);
}

// ---- Case 3: N consecutive drops — slot walks N → 0 — final dealloc ----

#[test]
fn nlstr_drop_n_consecutive_reaches_zero_and_deallocs() {
    const N: i64 = 5;

    let ptr = unsafe { alloc_probe_box(N) };
    assert_eq!(unsafe { read_refcount(ptr) }, N);

    for i in 1..N {
        let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlstr_drop(ptr as *mut i64) };
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

    let final_ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlstr_drop(ptr as *mut i64) };
    assert_eq!(
        final_ret, 1,
        "final drop on pre-sub=1 must hit dealloc-bytes and return 1"
    );

    let probe =
        unsafe { nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLSTR, ALIGN_OF_NLSTR) };
    assert!(!probe.is_null());
    unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(probe, SIZE_OF_NLSTR, ALIGN_OF_NLSTR);
    }
}
