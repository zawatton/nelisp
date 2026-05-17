//! Doc 117 §117.B probe — direct calls into the Phase 47-compiled
//! quit-flag kernels (`set' / `clear' / `pending-p').
//!
//! Verifies the atomic / raw-mem round-trip wired from
//! `lisp/nelisp-cc-bi-quit-flag.el' through the safe wrappers in
//! `nelisp_build_tool::elisp_cc_spike'.  All three kernels operate on
//! the `QUIT_FLAG' static slot in `build-tool/src/eval/quit.rs',
//! reached via the `nl_quit_flag_ptr()` extern.  Each test runs with
//! `--test-threads=1' (per the cargo test convention in this repo
//! when probing static state) so that the global flag transitions
//! are not interleaved across tests.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::quit;

extern "C" {
    fn nl_quit_flag_ptr() -> *mut i64;
}

fn quit_ptr() -> *mut i64 {
    // Round-trip the public extern so the probe also covers the
    // pointer-surfacing path that the Rust shim takes.
    unsafe { nl_quit_flag_ptr() }
}

#[test]
fn set_quit_flag_clear_to_pending_transition() {
    // Start from a known-clear state.
    quit::clear_quit_flag();
    assert!(!quit::is_quit_pending());

    // CAS(0, 1) must succeed and return 1.
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_set_quit_flag(quit_ptr())
    };
    assert_eq!(rc, 1, "first set on clear slot must transition (CAS == 1)");
    assert!(quit::is_quit_pending(),
            "slot must read pending after set");

    // Cleanup so other tests see a clean state.
    quit::clear_quit_flag();
}

#[test]
fn set_quit_flag_already_pending_is_idempotent() {
    // Pre-set via Rust to land at slot == 1.
    quit::set_quit_flag();
    assert!(quit::is_quit_pending());

    // Second set: CAS(0, 1) fails (slot is already 1) → returns 0,
    // but the post-condition (slot == 1) still holds.
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_set_quit_flag(quit_ptr())
    };
    assert_eq!(rc, 0,
               "second set on already-pending slot must be no-op (CAS == 0)");
    assert!(quit::is_quit_pending(),
            "slot must remain pending after idempotent set");

    quit::clear_quit_flag();
}

#[test]
fn clear_quit_flag_pending_to_clear_transition() {
    quit::set_quit_flag();
    assert!(quit::is_quit_pending());

    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_clear_quit_flag(quit_ptr())
    };
    assert_eq!(rc, 1,
               "clear on pending slot must transition (CAS == 1)");
    assert!(!quit::is_quit_pending(),
            "slot must read clear after clear");
}

#[test]
fn clear_quit_flag_already_clear_is_idempotent() {
    quit::clear_quit_flag();
    assert!(!quit::is_quit_pending());

    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_clear_quit_flag(quit_ptr())
    };
    assert_eq!(rc, 0,
               "clear on already-clear slot must be no-op (CAS == 0)");
    assert!(!quit::is_quit_pending(),
            "slot must remain clear after idempotent clear");
}

#[test]
fn quit_flag_pending_p_round_trip() {
    // pending-p reads via `ptr-read-u64'.  Drive both states through
    // the Rust mutators + observe through the elisp reader.
    quit::clear_quit_flag();
    let r0 = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_quit_flag_pending_p(quit_ptr())
    };
    assert_eq!(r0, 0, "pending-p must read 0 when flag is clear");

    quit::set_quit_flag();
    let r1 = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_quit_flag_pending_p(quit_ptr())
    };
    assert_eq!(r1, 1, "pending-p must read 1 when flag is pending");

    quit::clear_quit_flag();
}

#[test]
fn elisp_cc_round_trip_self_consistent() {
    // Exercise the full elisp-cc surface end-to-end without touching
    // the Rust mutator helpers in between — proves the three kernels
    // are self-consistent against the same `QUIT_FLAG' slot.
    quit::clear_quit_flag();
    unsafe {
        // set → pending-p reads 1.
        assert_eq!(
            nelisp_build_tool::elisp_cc_spike::bi_set_quit_flag(quit_ptr()),
            1,
        );
        assert_eq!(
            nelisp_build_tool::elisp_cc_spike::bi_quit_flag_pending_p(quit_ptr()),
            1,
        );
        // clear → pending-p reads 0.
        assert_eq!(
            nelisp_build_tool::elisp_cc_spike::bi_clear_quit_flag(quit_ptr()),
            1,
        );
        assert_eq!(
            nelisp_build_tool::elisp_cc_spike::bi_quit_flag_pending_p(quit_ptr()),
            0,
        );
    }
}
