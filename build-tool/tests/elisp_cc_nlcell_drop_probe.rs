#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;
use std::sync::atomic::{AtomicI64, Ordering};

const SIZE_OF_NLCELL: i64 = 40;

const ALIGN_OF_NLCELL: i64 = 8;

const REFCOUNT_OFFSET: i64 = 32;

unsafe fn alloc_probe_box(initial_refcount: i64) -> *mut u8 {
    let ptr =
        unsafe { nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLCELL, ALIGN_OF_NLCELL) };
    assert!(
        !ptr.is_null(),
        "alloc-bytes({}, {}) must succeed on a healthy host",
        SIZE_OF_NLCELL,
        ALIGN_OF_NLCELL,
    );
    // Doc 124 §124.L: initialize `value: Sexp' to `Sexp::Nil' so the
    // §124.L inner-drop step (= `drop_in_place::<NlCell>') walks a
    // valid Sexp rather than uninitialized bytes (= UB).  `Sexp::Nil'
    // is a trivial drop.
    unsafe {
        std::ptr::write(ptr as *mut Sexp, Sexp::Nil);
    }
    // Seed the refcount slot via direct AtomicI64 store — the elisp
    // kernel will read/write it through `nl_atomic_fetch_add', so we
    // need a well-defined initial value at the trailer offset.
    let refcount_slot = (ptr as usize + REFCOUNT_OFFSET as usize) as *mut AtomicI64;
    unsafe {
        // Write via `ptr::write' to construct the AtomicI64 in-place;
        // the alloc-bytes block is fresh uninitialized memory.
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
fn nlcell_drop_with_refcount_2_no_dealloc() {
    // Pre-condition: layout invariant.  REFCOUNT_OFFSET = 32 must
    // match the `nlcell.rs:288' compile-time assert; if Sexp size
    // ever changes (currently locked at 32 via
    // `sexp_abi_assert.rs:45') this constant updates in lockstep.
    assert_eq!(REFCOUNT_OFFSET, 32);
    assert_eq!(SIZE_OF_NLCELL, 40);
    assert_eq!(ALIGN_OF_NLCELL, 8);

    let ptr = unsafe { alloc_probe_box(2) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 2, "seeded refcount slot must read back as 2");

    let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlcell_drop(ptr as *mut i64) };
    // Both branches return 1 sentinel.  The "still alive" branch
    // produces the literal 1; the "dealloc" branch returns
    // `dealloc-bytes`'s 1 sentinel.
    assert_eq!(
        ret, 1,
        "nlcell_drop must return 1 sentinel on both branches"
    );

    // Refcount slot must have advanced from 2 → 1 (= fetch_sub(1)
    // pre-sub was 2 ≠ 1 so the else branch fired, slot is now 1).
    let after = unsafe { read_refcount(ptr) };
    assert_eq!(
        after, 1,
        "post-drop refcount slot must be initial - 1 (= 2 - 1 = 1) \
         when the box is not yet free"
    );

    // The block is still alive — clean up manually via
    // `dealloc-bytes' so we don't leak the test allocation.  This
    // also doubles as a self-consistency check: dealloc-bytes
    // matching the alloc-bytes call must succeed and return 1.
    let cleanup_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(ptr, SIZE_OF_NLCELL, ALIGN_OF_NLCELL)
    };
    assert_eq!(cleanup_rc, 1);
}

// ---- Case 2: Drop from refcount=1 — dealloc happens, return = 1 ----

#[test]
fn nlcell_drop_with_refcount_1_dealloc_happens() {
    let ptr = unsafe { alloc_probe_box(1) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 1, "seeded refcount slot must read back as 1");

    let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlcell_drop(ptr as *mut i64) };
    // Drop with pre-sub == 1 should take the dealloc branch, which
    // calls §125.A `dealloc-bytes' and returns its 1 sentinel.  We
    // cannot read the slot after this — the block has been freed
    // and any subsequent load is UB.
    assert_eq!(
        ret, 1,
        "nlcell_drop on last-ref must return dealloc-bytes's 1 sentinel"
    );

    // Best-effort sanity check: allocate another 40-byte block at
    // align 8.  The bare fact that the second alloc succeeds —
    // combined with the §125.A's `alloc-bytes-then-dealloc' probe
    // already covering the round-trip — is sufficient to call the
    // dealloc effective.
    let probe =
        unsafe { nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLCELL, ALIGN_OF_NLCELL) };
    assert!(
        !probe.is_null(),
        "post-drop allocator must still be functional \
         (= alloc-bytes for a fresh block succeeds)"
    );
    let probe_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(probe, SIZE_OF_NLCELL, ALIGN_OF_NLCELL)
    };
    assert_eq!(probe_rc, 1);
}

// ---- Case 3: N consecutive drops — slot walks N → 0 — final dealloc ----

#[test]
fn nlcell_drop_n_consecutive_reaches_zero_and_deallocs() {
    const N: i64 = 5;

    let ptr = unsafe { alloc_probe_box(N) };
    assert_eq!(unsafe { read_refcount(ptr) }, N);

    // Drops 1..(N-1): each must take the "still alive" branch.  Slot
    // advances from N → N-1 → ... → 2 → 1.  After this loop the
    // slot holds 1 and the box is on the brink of dealloc.
    for i in 1..N {
        let ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlcell_drop(ptr as *mut i64) };
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
    // Now slot == 1.  Confirm.
    assert_eq!(unsafe { read_refcount(ptr) }, 1);

    // The N-th drop is the dealloc-triggering one.  Pre-sub = 1, the
    // `if' branch takes the dealloc-bytes arm.
    let final_ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlcell_drop(ptr as *mut i64) };
    assert_eq!(
        final_ret, 1,
        "final drop on pre-sub=1 must hit dealloc-bytes and return 1"
    );

    // Like Case 2, we don't read the slot after the dealloc — the
    // block has been freed.  Best-effort sanity: a fresh alloc
    // succeeds, demonstrating the allocator is still healthy.
    let probe =
        unsafe { nelisp_build_tool::elisp_cc_spike::alloc_bytes(SIZE_OF_NLCELL, ALIGN_OF_NLCELL) };
    assert!(!probe.is_null());
    unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(probe, SIZE_OF_NLCELL, ALIGN_OF_NLCELL);
    }
}
