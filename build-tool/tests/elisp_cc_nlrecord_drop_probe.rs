//! Doc 124 §124.J probe — pure-elisp `nelisp_nlrecord_drop' kernel.
//!
//! Mechanical port of §124.G/H's NlConsBox/NlVector Drop probes to
//! NlRecord.  Only differences from §124.H: REFCOUNT_OFFSET = 56
//! (= `size_of::<Sexp>() + size_of::<Vec<Sexp>>()'),
//! SIZE_OF_NLRECORD = 64 (= 56 + 8 AtomicUsize trailer).  Align is
//! unchanged at 8.
//!
//! Kernel body (verbatim from `lisp/nelisp-cc-nlrecord-drop.el'):
//!
//!   (if (= (atomic-fetch-add (+ box-ptr 56) -1) 1)
//!       (dealloc-bytes box-ptr 64 8)
//!     1)
//!
//! Test cases (≥ 3):
//!   1. Drop from refcount=2 — no dealloc, slot lands at 1.  The
//!      box stays alive; this is the "still has clones" path.
//!   2. Drop from refcount=1 — dealloc happens, return = 1 sentinel.
//!      The 64-byte outer allocation is returned to the global
//!      allocator (= cannot inspect post-call slot without UB; the
//!      probe verifies the return sentinel + best-effort allocator-
//!      healthy probe after).
//!   3. N consecutive drops — starting from refcount=N, each drop
//!      decrements the slot by exactly 1; the *last* drop hits
//!      pre-sub=1 → free.  Verifies sequential composition matches
//!      the Rust `nlrc_drop_box!' macro's per-call contract.
//!
//! Substrate gating role: §124.J is the fourth Drop-half kernel
//! (after §124.G NlConsBox + §124.H NlVector + §124.I NlCell).
//! Reuses the same §122.E `atomic-fetch-add' + §125.A `dealloc-bytes'
//! composition with NlRecord-specific layout literals.  §124.L sweep
//! stage will swap the 5 sibling `impl Drop' bodies once §124.H-K all
//! green.
//!
//! Allocator note: as with §124.G/H/I, we allocate the probe boxes
//! via §125.A `alloc_bytes(64, 8)' rather than constructing a
//! `ProbeBox' on the stack — the kernel's last-ref branch calls
//! `dealloc-bytes' on the pointer, which expects the matching
//! `alloc_bytes' allocation per `std::alloc::dealloc' contracts.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;
use std::sync::atomic::{AtomicI64, Ordering};

/// SIZE_OF_NLRECORD = `size_of::<Sexp>() + size_of::<Vec<Sexp>>() +
/// size_of::<AtomicUsize>()` = 32 + 24 + 8 = 64.  Pinned by
/// `build-tool/src/eval/nlrecord.rs:248-254' compile-time asserts:
/// `offset_of!(NlRecord, type_tag) == 0' +
/// `offset_of!(NlRecord, slots) == size_of::<Sexp>() = 32' +
/// `offset_of!(NlRecord, refcount) == size_of::<Sexp>() +
/// size_of::<Vec<Sexp>>() = 56' + `size_of::<AtomicUsize>() == 8'.
/// This matches `Layout::new::<NlRecord>()' used by `NlRecordRef::new'
/// (`nlrecord.rs:77') — the same layout drives the matching
/// `std::alloc::dealloc' call in production Drop.
const SIZE_OF_NLRECORD: i64 = 64;

/// alignof::<NlRecord> = max(alignof::<Sexp>, alignof::<Vec<Sexp>>,
/// alignof::<AtomicUsize>) = 8.
const ALIGN_OF_NLRECORD: i64 = 8;

/// REFCOUNT_OFFSET — byte offset of the AtomicUsize trailer from the
/// NlRecord base, per `repr(C)' layout rules + the compile-time assert
/// at `nlrecord.rs:251-253' (= `size_of::<Sexp>() + size_of::<Vec<Sexp>>()
/// = 32 + 24 = 56').  Same constant §124.D's clone kernel bakes in.
const REFCOUNT_OFFSET: i64 = 56;

/// Helper: allocate a NlRecord-shaped 64-byte block via §125.A's
/// `alloc-bytes' wrapper and seed the refcount slot to the requested
/// initial value.  Returns the base pointer; caller is responsible
/// for either driving the slot to 0 via `nlrecord_drop' (= frees the
/// block) or explicitly calling `dealloc_bytes(ptr, 64, 8)' for the
/// "no dealloc this call" probe paths.
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

/// Helper: read the current refcount slot value.  Safe to call only
/// if the block has not been freed.
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
