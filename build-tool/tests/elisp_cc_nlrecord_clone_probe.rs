//! Doc 124 §124.D probe — pure-elisp `nelisp_nlrecord_clone' kernel.
//!
//! Mechanical port of §124.A's NlConsBox Clone probe to NlRecord.
//! Only difference: REFCOUNT_OFFSET = 56 instead of 64.  Note: Doc 124
//! §1's audit table listed 24 (= the Vec header portion only) — the
//! byte-accurate `repr(C)' offset is 56 because `NlRecord' has both a
//! `type_tag: Sexp' (32 bytes) and a `slots: Vec<Sexp>' (24 bytes)
//! ahead of the trailer.  See `nlrecord.rs:248-253' compile-time
//! assert.
//!
//! Test cases (≥ 3):
//!   1. Clone single — refcount @ +56 must advance by 1.
//!   2. Clone N times — slot equals initial + N.
//!   3. Concurrent clone × 2 threads × 10 000 iters — slot equals
//!      20 000.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::sync::atomic::{AtomicI64, Ordering};

/// Layout-pinned struct matching `NlRecord' for the probe:
/// `#[repr(C)]' keeps `refcount' at byte offset 56 (= 32 + 24, per
/// `offset_of!(NlRecord, refcount) == size_of::<Sexp>() +
/// size_of::<Vec<Sexp>>()' at `nlrecord.rs:248-253').  Bytes 0-31
/// stand in for the `type_tag: Sexp' slot; 32-55 stand in for the
/// `slots: Vec<Sexp>' header.  The elisp body never reads them.
#[repr(C)]
struct ProbeBox {
    type_tag: [u8; 32],
    slots_header: [u8; 24],
    refcount: AtomicI64,
}

impl ProbeBox {
    fn new(initial: i64) -> Self {
        ProbeBox {
            type_tag: [0; 32],
            slots_header: [0; 24],
            refcount: AtomicI64::new(initial),
        }
    }

    fn as_box_ptr(&self) -> *mut i64 {
        self as *const ProbeBox as *mut i64
    }
}

// ---- Case 1: single clone ----

#[test]
fn nlrecord_clone_single_increments_and_returns_input_ptr() {
    assert_eq!(
        std::mem::offset_of!(ProbeBox, refcount),
        56,
        "ProbeBox layout must mirror NlRecord (= refcount at +56)"
    );

    let bx = ProbeBox::new(1);
    let input_ptr = bx.as_box_ptr();
    let returned_ptr = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_clone(input_ptr) };
    assert_eq!(
        returned_ptr as usize, input_ptr as usize,
        "nelisp_nlrecord_clone must return the *input* pointer unchanged"
    );
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        2,
        "post-call refcount slot must be old + 1"
    );
}

// ---- Case 2: N clones ----

#[test]
fn nlrecord_clone_n_times_count_equals_n_plus_initial() {
    const INITIAL: i64 = 1;
    const N: usize = 10;

    let bx = ProbeBox::new(INITIAL);
    let input_ptr = bx.as_box_ptr();
    for i in 0..N {
        let returned = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_clone(input_ptr) };
        assert_eq!(
            returned as usize, input_ptr as usize,
            "iteration {i}: returned pointer must equal input (alias, not copy)"
        );
        assert_eq!(
            bx.refcount.load(Ordering::SeqCst),
            INITIAL + (i as i64) + 1,
            "iteration {i}: slot must advance by exactly 1 per call"
        );
    }
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        INITIAL + N as i64,
        "N-call total: slot = INITIAL + N (= 11 here)"
    );
}

// ---- Case 3: concurrent clone ----

#[test]
fn nlrecord_clone_concurrent_no_lost_updates() {
    use std::sync::Arc;
    use std::thread;

    let bx = Arc::new(ProbeBox::new(0));
    const PER_THREAD: i64 = 10_000;

    let base_usize = bx.as_box_ptr() as usize;
    let bx_keep_alive = Arc::clone(&bx);

    let t1 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            let _ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_clone(p) };
        }
    });
    let t2 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            let _ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlrecord_clone(p) };
        }
    });

    t1.join().expect("thread 1 panicked");
    t2.join().expect("thread 2 panicked");

    assert_eq!(
        bx_keep_alive.refcount.load(Ordering::SeqCst),
        2 * PER_THREAD,
        "concurrent clone must converge to N1 + N2 (= no lost updates)"
    );
}
