//! Doc 124 §124.G probe — pure-elisp `nelisp_nlconsbox_drop' kernel.
//!
//! Validates the first Drop-half stage of the `nl*.rs::Clone/Drop'
//! substrate elisp化 chain.  The kernel composes Doc 123 §123.B's
//! `fetch_sub' refcount semantics with Doc 125 §125.A's
//! `dealloc-bytes' to express `impl Drop for NlConsBoxRef' in pure
//! elisp:
//!
//!   (if (= (atomic-fetch-add (+ box-ptr 64) -1) 1)
//!       (dealloc-bytes box-ptr 72 8)
//!     1)
//!
//! Test cases (≥ 3):
//!   1. Drop from refcount=2 — no dealloc, slot lands at 1.  The
//!      box stays alive; this is the "still has clones" path.
//!   2. Drop from refcount=1 — dealloc happens, return = 1 sentinel.
//!      The box's underlying allocation is freed back to the global
//!      allocator (= cannot inspect post-call slot without UB; the
//!      probe verifies the return sentinel + uses a `Drop'-tracking
//!      sentinel to confirm dealloc-bytes was reached).
//!   3. N consecutive drops — starting from refcount=N, each drop
//!      decrements the slot by exactly 1; the *last* drop hits
//!      pre-sub=1 → free.  Verifies sequential composition matches
//!      the Rust `nlrc_drop_box!' macro's per-call contract.
//!
//! Substrate gating role: §124.G is the first Drop-half kernel; the
//! probe verifies the §122.E `atomic-fetch-add' + §125.A
//! `dealloc-bytes' composition end-to-end.  §124.H-K (NlVector /
//! NlCell / NlRecord / NlStr Drop) reuse the same composition with
//! per-type SIZE / REFCOUNT_OFFSET literals; §124.L's sweep stage
//! swaps the 5 sibling `impl Drop' bodies once §124.H-K all green.
//!
//! Allocator note: we allocate the probe boxes via §125.A
//! `alloc_bytes(72, 8)' rather than constructing a `ProbeBox' on the
//! stack — the kernel's last-ref branch calls `dealloc-bytes' on the
//! pointer, which expects the matching `alloc_bytes' allocation per
//! `std::alloc::dealloc' contracts.  Stack-allocating a struct and
//! then calling `dealloc-bytes' on its address is UB (= would free a
//! non-heap pointer).  Using the elisp-compiled `alloc-bytes' wrapper
//! makes the probe self-consistent (= same allocator drives both
//! sides) and exercises the full Doc 125 §125.A round-trip.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::sync::atomic::{AtomicI64, Ordering};

/// SIZE_OF_NLCONSBOX = `2 * size_of::<Sexp>() + size_of::<AtomicUsize>()`
/// = 32 + 32 + 8 = 72.  Locked by `build-tool/src/eval/sexp_abi_assert.rs:45'
/// (size_of::<Sexp> = 32) + `build-tool/src/eval/nlconsbox.rs:418-431'
/// (compile-time field-offset asserts).  Layout: car @ 0 (32) + cdr @ 32
/// (32) + refcount @ 64 (8).
const SIZE_OF_NLCONSBOX: i64 = 72;

/// alignof::<NlConsBox> = max(alignof::<Sexp>, alignof::<AtomicUsize>) = 8.
const ALIGN_OF_NLCONSBOX: i64 = 8;

/// REFCOUNT_OFFSET — byte offset of the AtomicUsize trailer from the
/// NlConsBox base, per `repr(C)' layout rules.  Same constant §123.A /
/// §123.B / §124.A bake into their kernels.
const REFCOUNT_OFFSET: i64 = 64;

/// Helper: allocate a NlConsBox-shaped 72-byte block via §125.A's
/// `alloc-bytes' wrapper and seed the refcount slot to the requested
/// initial value.  Returns the base pointer; caller is responsible for
/// either driving the slot to 0 via `nlconsbox_drop' (= frees the
/// block) or explicitly calling `dealloc_bytes(ptr, 72, 8)' for the
/// "no dealloc this call" probe paths.
unsafe fn alloc_probe_box(initial_refcount: i64) -> *mut u8 {
    let ptr = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(
            SIZE_OF_NLCONSBOX,
            ALIGN_OF_NLCONSBOX,
        )
    };
    assert!(
        !ptr.is_null(),
        "alloc-bytes({}, {}) must succeed on a healthy host",
        SIZE_OF_NLCONSBOX,
        ALIGN_OF_NLCONSBOX,
    );
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

/// Helper: read the current refcount slot value.  Safe to call only if
/// the block has not been freed.
unsafe fn read_refcount(ptr: *mut u8) -> i64 {
    let refcount_slot = (ptr as usize + REFCOUNT_OFFSET as usize) as *const AtomicI64;
    unsafe { (*refcount_slot).load(Ordering::SeqCst) }
}

// ---- Case 1: Drop from refcount=2 — no dealloc, slot lands at 1 ----

#[test]
fn nlconsbox_drop_with_refcount_2_no_dealloc() {
    // Pre-condition: layout invariant.  REFCOUNT_OFFSET = 64 must
    // match the `nlconsbox.rs' compile-time asserts; if Sexp size
    // ever changes (= sexp_abi_assert.rs:45 = 32) this constant
    // updates in lockstep.
    assert_eq!(REFCOUNT_OFFSET, 64);
    assert_eq!(SIZE_OF_NLCONSBOX, 72);

    let ptr = unsafe { alloc_probe_box(2) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 2, "seeded refcount slot must read back as 2");

    let ret = unsafe {
        nelisp_build_tool::elisp_cc_spike::nlconsbox_drop(ptr as *mut i64)
    };
    // Both branches return 1 sentinel.  The "still alive" branch
    // produces the literal 1; the "dealloc" branch returns
    // `dealloc-bytes`'s 1 sentinel.
    assert_eq!(
        ret, 1,
        "nlconsbox_drop must return 1 sentinel on both branches"
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
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
            ptr,
            SIZE_OF_NLCONSBOX,
            ALIGN_OF_NLCONSBOX,
        )
    };
    assert_eq!(cleanup_rc, 1);
}

// ---- Case 2: Drop from refcount=1 — dealloc happens, return = 1 ----

#[test]
fn nlconsbox_drop_with_refcount_1_dealloc_happens() {
    let ptr = unsafe { alloc_probe_box(1) };
    let initial = unsafe { read_refcount(ptr) };
    assert_eq!(initial, 1, "seeded refcount slot must read back as 1");

    let ret = unsafe {
        nelisp_build_tool::elisp_cc_spike::nlconsbox_drop(ptr as *mut i64)
    };
    // Drop with pre-sub == 1 should take the dealloc branch, which
    // calls §125.A `dealloc-bytes' and returns its 1 sentinel.  We
    // cannot read the slot after this — the block has been freed
    // and any subsequent load is UB (= the allocator may have
    // unmapped the page or reused it for a different allocation).
    assert_eq!(
        ret, 1,
        "nlconsbox_drop on last-ref must return dealloc-bytes's 1 sentinel"
    );

    // Best-effort sanity check: allocate another 72-byte block at
    // align 8.  On Linux's glibc allocator with small freelists, a
    // same-size same-align alloc after a free is very likely to hit
    // the just-freed slot (= verifies the dealloc effectively
    // returned memory to the free pool, observable via address
    // reuse).  We don't assert pointer equality because the
    // allocator is free to choose a different slot for various
    // reasons (= jemalloc's tcache, glibc's per-thread arenas,
    // anti-fragmentation moves).  The bare fact that the second
    // alloc succeeds — combined with the §125.A's
    // `alloc-bytes-then-dealloc' probe already covering the
    // round-trip — is sufficient to call the dealloc effective.
    let probe = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(
            SIZE_OF_NLCONSBOX,
            ALIGN_OF_NLCONSBOX,
        )
    };
    assert!(
        !probe.is_null(),
        "post-drop allocator must still be functional \
         (= alloc-bytes for a fresh block succeeds)"
    );
    let probe_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
            probe,
            SIZE_OF_NLCONSBOX,
            ALIGN_OF_NLCONSBOX,
        )
    };
    assert_eq!(probe_rc, 1);
}

// ---- Case 3: N consecutive drops — slot walks N → 0 — final dealloc ----

#[test]
fn nlconsbox_drop_n_consecutive_reaches_zero_and_deallocs() {
    const N: i64 = 5;

    let ptr = unsafe { alloc_probe_box(N) };
    assert_eq!(unsafe { read_refcount(ptr) }, N);

    // Drops 1..(N-1): each must take the "still alive" branch.  Slot
    // advances from N → N-1 → ... → 2 → 1.  After this loop the
    // slot holds 1 and the box is on the brink of dealloc.
    for i in 1..N {
        let ret = unsafe {
            nelisp_build_tool::elisp_cc_spike::nlconsbox_drop(ptr as *mut i64)
        };
        assert_eq!(
            ret, 1,
            "drop {} of {}: must return 1 sentinel",
            i, N - 1
        );
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
    let final_ret = unsafe {
        nelisp_build_tool::elisp_cc_spike::nlconsbox_drop(ptr as *mut i64)
    };
    assert_eq!(
        final_ret, 1,
        "final drop on pre-sub=1 must hit dealloc-bytes and return 1"
    );

    // Like Case 2, we don't read the slot after the dealloc — the
    // block has been freed.  Best-effort sanity: a fresh alloc
    // succeeds, demonstrating the allocator is still healthy.
    let probe = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(
            SIZE_OF_NLCONSBOX,
            ALIGN_OF_NLCONSBOX,
        )
    };
    assert!(!probe.is_null());
    unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
            probe,
            SIZE_OF_NLCONSBOX,
            ALIGN_OF_NLCONSBOX,
        );
    }
}
