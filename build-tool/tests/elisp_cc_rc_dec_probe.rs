//! Doc 123 §123.B probe — pure-elisp `nelisp_rc_dec' kernel.
//!
//! Validates the second substrate elisp化 stage: the refcount-dec
//! kernel (= twin of §123.A's `nelisp_rc_inc') pulled out of
//! `build-tool/src/eval/rc_primitives.rs' (specifically the
//! `rc_dec_no_drop' helper + the mutation half of
//! `bi_nl_rc_dec_strong') is functional in elisp via the §122.E
//! `atomic-fetch-add' grammar op with delta = -1.
//!
//! Pattern mirrors `elisp_cc_rc_inc_probe.rs' (= §123.A sibling) —
//! the elisp body computes `box_ptr + 64' internally
//! (= REFCOUNT_OFFSET) before calling `atomic-fetch-add', so the
//! probe just needs to lay out a buffer where the refcount slot
//! lives at byte offset 64 from the supplied `box_ptr'.
//!
//! Test cases (≥ 3):
//!   1. Single dec on a refcount=2 slot returns 2 (pre-sub value)
//!      and writes 1 to the slot.
//!   2. Sequential inc + dec round trip — a sequence of `rc_inc'
//!      followed by `rc_dec' calls leaves the slot at the expected
//!      net delta, and each dec sees the latest count.
//!   3. Atomic ordering check via concurrent dec from 2 host threads
//!      — final slot value equals start minus total iteration count
//!      across both threads (= no lost updates under SeqCst).
//!
//! Substrate gating role: each test case exercises the exact
//! contract that §123.F's dispatch swap (= `rc_dec_no_drop' /
//! `bi_nl_rc_dec_strong' calling `nelisp_rc_dec' instead of the
//! inline `fetch_sub') will rely on.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::sync::atomic::{AtomicI64, Ordering};

/// Layout-pinned struct matching `NlConsBox' for the probe.  Same
/// shape as §123.A's `ProbeBox' — `#[repr(C)]` keeps `refcount' at
/// byte offset 64 (= same offset as the production `NlConsBox' per
/// `nlrc.rs:292' compile-time assert).  `[u8; 32]' stands in for
/// `Sexp' on car/cdr because the elisp body never dereferences
/// those bytes — it only adds 64 to the base pointer and atomic-
/// fetches the i64 slot there.
#[repr(C)]
struct ProbeBox {
    car: [u8; 32],
    cdr: [u8; 32],
    refcount: AtomicI64,
}

impl ProbeBox {
    fn new(initial: i64) -> Self {
        ProbeBox {
            car: [0; 32],
            cdr: [0; 32],
            refcount: AtomicI64::new(initial),
        }
    }

    /// Cast to the elisp body's `*mut i64' arg type.  The elisp body
    /// adds 64 internally; we pass the *base* address.
    fn as_box_ptr(&self) -> *mut i64 {
        self as *const ProbeBox as *mut i64
    }
}

// ---- Case 1: single dec from refcount=2 returns 2, writes 1 ----

#[test]
fn rc_dec_single_returns_two_and_writes_one() {
    // Layout sanity — `refcount' must be at byte offset 64 for the
    // elisp body's `(+ box-ptr 64)' to land on the right slot.
    assert_eq!(
        std::mem::offset_of!(ProbeBox, refcount),
        64,
        "ProbeBox layout must mirror NlConsBox (= refcount at +64)"
    );

    let bx = ProbeBox::new(2);
    let prev = unsafe { nelisp_build_tool::elisp_cc_spike::rc_dec(bx.as_box_ptr()) };
    assert_eq!(
        prev, 2,
        "nelisp_rc_dec must return the *pre-sub* refcount value (= 2 here)"
    );
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        1,
        "post-call refcount slot must be old - 1"
    );
}

// ---- Case 2: sequential inc + dec round trip ----

#[test]
fn rc_dec_round_trip_with_inc() {
    let bx = ProbeBox::new(3);

    // 5 increments take 3 → 8.
    for expected_prev in 3..8 {
        let prev = unsafe { nelisp_build_tool::elisp_cc_spike::rc_inc(bx.as_box_ptr()) };
        assert_eq!(
            prev, expected_prev,
            "inc iteration {} must return the count seen before the +1",
            expected_prev - 3
        );
    }
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        8,
        "after 5 incs from 3, slot must be 8"
    );

    // 5 decrements take 8 → 3 (= net zero).
    for expected_prev in (4..=8).rev() {
        let prev = unsafe { nelisp_build_tool::elisp_cc_spike::rc_dec(bx.as_box_ptr()) };
        assert_eq!(
            prev, expected_prev,
            "dec iteration must return the count seen before the -1"
        );
        assert_eq!(
            bx.refcount.load(Ordering::SeqCst),
            expected_prev - 1,
            "slot must retreat by 1 per dec call"
        );
    }
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        3,
        "after net 5 inc + 5 dec, slot returns to the initial value"
    );
}

// ---- Case 3: concurrent dec from 2 threads — no lost updates ----

#[test]
fn rc_dec_concurrent_no_lost_updates() {
    use std::sync::Arc;
    use std::thread;

    // 10 000 decrements per thread × 2 threads = 20 000 total.  Start
    // refcount high enough that the slot never crosses 0 (avoids any
    // saturating-sub or wrap interpretation in the kernel itself,
    // which returns the raw i64 fetch_add result).
    const PER_THREAD: i64 = 10_000;
    const TOTAL: i64 = 2 * PER_THREAD;
    let bx = Arc::new(ProbeBox::new(TOTAL));

    // SAFETY for the raw pointer cross-thread share: the `AtomicI64'
    // inside `ProbeBox' makes the slot soundly accessible from any
    // thread via `nl_atomic_fetch_add' (= the underlying
    // `AtomicI64::fetch_add' is the same contract `std' exposes).
    // We pass the base pointer as a `usize' to dodge `*mut i64''s
    // !Send bound; each worker casts back to `*mut i64' inside.
    let base_usize = bx.as_box_ptr() as usize;
    let bx_keep_alive = Arc::clone(&bx);

    let t1 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            unsafe { nelisp_build_tool::elisp_cc_spike::rc_dec(p) };
        }
    });
    let t2 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            unsafe { nelisp_build_tool::elisp_cc_spike::rc_dec(p) };
        }
    });

    t1.join().expect("thread 1 panicked");
    t2.join().expect("thread 2 panicked");

    assert_eq!(
        bx_keep_alive.refcount.load(Ordering::SeqCst),
        0,
        "concurrent dec must converge to start - (N1 + N2) (= no lost \
         updates under SeqCst atomic-fetch-add with delta=-1)"
    );
}
