//! Doc 122 §122.E probes — direct calls into the six Phase 47-
//! compiled atomic + raw memory grammar ops (= mirrors
//! `elisp_cc_sexp_write_str_probe.rs` (§122.A) and
//! `elisp_cc_mut_str_probe.rs` (§122.B) patterns).
//!
//! Verifies end-to-end round-trips:
//!
//!   - atomic-fetch-add on zero slot → returns 0, slot becomes delta
//!   - atomic-fetch-add on existing slot → returns old, slot becomes old+delta
//!   - atomic-compare-exchange success → returns 1, slot replaced
//!   - atomic-compare-exchange failure → returns 0, slot untouched
//!   - ptr-read-u64 / ptr-write-u64 round-trip with byte offset
//!   - ptr-read-u8 / ptr-write-u8 round-trip + zero-extension verification
//!
//! Each test:
//!   1. Allocates a host-side i64 / u64 / u8 buffer.
//!   2. Calls the elisp-compiled grammar op through the public
//!      wrapper in `nelisp_build_tool::elisp_cc_spike`.
//!   3. Asserts the returned value AND the post-call buffer state
//!      match the expected SeqCst / wrap semantics.
//!
//! Substrate gating role (= Doc 123-128 unblock): every test exercises
//! the exact contract that the refcount + nl*.rs Clone/Drop elisp化
//! patterns need.  Verifying the contract here proves the substrate
//! is ready before Doc 123 lands the first elisp用 refcount macro.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

// ---- Case 1: atomic-fetch-add on a zero slot returns 0 and writes delta ----

#[test]
fn atomic_fetch_add_zero_slot_returns_zero_and_writes_delta() {
    let mut slot: i64 = 0;
    let prev =
        unsafe { nelisp_build_tool::elisp_cc_spike::atomic_fetch_add(&mut slot as *mut i64, 7) };
    assert_eq!(
        prev, 0,
        "atomic-fetch-add must return the pre-add value (= 0 here)"
    );
    assert_eq!(slot, 7, "slot must be old + delta after the op");
}

// ---- Case 2: atomic-fetch-add on an existing slot accumulates ----

#[test]
fn atomic_fetch_add_existing_slot_returns_old_and_accumulates() {
    let mut slot: i64 = 100;
    let prev =
        unsafe { nelisp_build_tool::elisp_cc_spike::atomic_fetch_add(&mut slot as *mut i64, -3) };
    assert_eq!(prev, 100, "fetch-add must return the old value (= 100)");
    assert_eq!(slot, 97, "slot must be 100 + (-3) = 97 after the op");

    // Compose a second op on the same slot — the i64 contract permits
    // chained mutation, this is the exact pattern Doc 123 refcount
    // dec_strong uses (= read previous, branch on `==1' for finalize).
    let prev2 =
        unsafe { nelisp_build_tool::elisp_cc_spike::atomic_fetch_add(&mut slot as *mut i64, 1) };
    assert_eq!(prev2, 97);
    assert_eq!(slot, 98);
}

// ---- Case 3: atomic-compare-exchange success writes new value ----

#[test]
fn atomic_compare_exchange_success_returns_one_and_writes() {
    let mut slot: i64 = 42;
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::atomic_compare_exchange(&mut slot as *mut i64, 42, 999)
    };
    assert_eq!(rc, 1, "CAS must return 1 on success");
    assert_eq!(slot, 999, "slot must be replaced on success");
}

// ---- Case 4: atomic-compare-exchange failure leaves slot alone ----

#[test]
fn atomic_compare_exchange_failure_returns_zero_and_leaves_slot() {
    let mut slot: i64 = 42;
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::atomic_compare_exchange(&mut slot as *mut i64, 7, 999)
    };
    assert_eq!(rc, 0, "CAS must return 0 on expected-mismatch");
    assert_eq!(slot, 42, "slot must be untouched on failure");
}

// ---- Case 5: ptr-read-u64 / ptr-write-u64 round-trip ----

#[test]
fn ptr_read_write_u64_round_trip() {
    // Use an aligned [u64; 4] buffer; access via *mut u8 base + byte
    // offsets to exercise the ptr-arithmetic exactly the way Doc 124
    // nl*.rs header walks will (= base = `Sexp` payload pointer,
    // offset = field offset constant).
    let mut buf: [u64; 4] = [0; 4];
    let base = buf.as_mut_ptr() as *mut u8;

    // Write at byte offset 16 (= slot index 2 in the u64 view).
    let write_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_write_u64(base, 16, 0x0123_4567_89AB_CDEF_i64)
    };
    assert_eq!(write_rc, 1, "ptr-write-u64 must return rax = 1 sentinel");
    assert_eq!(
        buf[2], 0x0123_4567_89AB_CDEF_u64,
        "slot 2 (= byte offset 16) must hold the written value"
    );

    let read = unsafe { nelisp_build_tool::elisp_cc_spike::ptr_read_u64(base as *const u8, 16) };
    assert_eq!(
        read as u64, 0x0123_4567_89AB_CDEF_u64,
        "ptr-read-u64 must return the same bits we just wrote"
    );

    // Verify the other slots are untouched (= no spurious neighboring
    // writes; ptr arithmetic is exact).
    assert_eq!(buf[0], 0);
    assert_eq!(buf[1], 0);
    assert_eq!(buf[3], 0);
}

// ---- Case 6: ptr-read-u8 / ptr-write-u8 round-trip + zero extension ----

#[test]
fn ptr_read_write_u8_round_trip_with_zero_extension() {
    // 8-byte u8 buffer to exercise tag-byte access patterns at
    // various byte offsets (= same shape as `Sexp::tag` reads in
    // Doc 124).
    let mut buf: [u8; 8] = [0; 8];
    let base = buf.as_mut_ptr();

    // Write 0xFF at byte offset 3 — the critical zero-extension test
    // case (= u8 0xFF should read back as i64 255, *not* -1).
    let write_rc = unsafe { nelisp_build_tool::elisp_cc_spike::ptr_write_u8(base, 3, 0xFF) };
    assert_eq!(write_rc, 1, "ptr-write-u8 must return rax = 1 sentinel");
    assert_eq!(buf[3], 0xFF, "byte 3 must hold the written value");

    let read = unsafe { nelisp_build_tool::elisp_cc_spike::ptr_read_u8(base as *const u8, 3) };
    assert_eq!(
        read, 0xFF,
        "ptr-read-u8 must zero-extend (= 255, not -1; the Doc 124 \
         tag-byte walk relies on this for refcount-tag dispatch)"
    );
    assert!(
        read >= 0,
        "ptr-read-u8 must never return a negative i64 \
         (= zero-extension contract)"
    );

    // Verify neighboring bytes are untouched.
    for (i, &b) in buf.iter().enumerate() {
        if i == 3 {
            assert_eq!(b, 0xFF);
        } else {
            assert_eq!(b, 0, "byte {} must remain 0", i);
        }
    }

    // Round-trip a low-value byte at a different offset to verify
    // offset arithmetic doesn't drift.
    unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_write_u8(base, 7, 0x42);
    }
    let r2 = unsafe { nelisp_build_tool::elisp_cc_spike::ptr_read_u8(base as *const u8, 7) };
    assert_eq!(r2, 0x42);
}
