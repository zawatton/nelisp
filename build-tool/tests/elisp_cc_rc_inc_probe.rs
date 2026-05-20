#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::sync::atomic::{AtomicI64, Ordering};

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

    fn as_box_ptr(&self) -> *mut i64 {
        self as *const ProbeBox as *mut i64
    }
}

// ---- Case 1: single inc returns pre-add value, writes old+1 ----

#[test]
fn rc_inc_single_returns_one_and_writes_two() {
    // Layout sanity — `refcount' must be at byte offset 64 for the
    // elisp body's `(+ box-ptr 64)' to land on the right slot.
    assert_eq!(
        std::mem::offset_of!(ProbeBox, refcount),
        64,
        "ProbeBox layout must mirror NlConsBox (= refcount at +64)"
    );

    let bx = ProbeBox::new(1);
    let prev = unsafe { nelisp_build_tool::elisp_cc_spike::rc_inc(bx.as_box_ptr()) };
    assert_eq!(
        prev, 1,
        "nelisp_rc_inc must return the *pre-add* refcount value (= 1 here)"
    );
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        2,
        "post-call refcount slot must be old + 1"
    );
}

// ---- Case 2: multiple inc accumulates monotonically ----

#[test]
fn rc_inc_multiple_calls_accumulate() {
    let bx = ProbeBox::new(5);
    for expected_prev in 5..15 {
        let prev = unsafe { nelisp_build_tool::elisp_cc_spike::rc_inc(bx.as_box_ptr()) };
        assert_eq!(
            prev,
            expected_prev,
            "iteration {} must return the count seen before the +1",
            expected_prev - 5
        );
        assert_eq!(
            bx.refcount.load(Ordering::SeqCst),
            expected_prev + 1,
            "slot must advance by 1 per call"
        );
    }
    // Final state: 5 + 10 increments = 15.
    assert_eq!(bx.refcount.load(Ordering::SeqCst), 15);
}

// ---- Case 3: concurrent inc from 2 threads — no lost updates ----

#[test]
fn rc_inc_concurrent_no_lost_updates() {
    use std::sync::Arc;
    use std::thread;

    // Allocate on the heap so both threads can share the pointer
    // (= `&'static`-equivalent via Arc, dropped at the end of the
    // test).  The `AtomicI64` inside is internally mutable, so the
    // shared `&ProbeBox` is sound.
    let bx = Arc::new(ProbeBox::new(0));

    // 10 000 increments per thread × 2 threads = 20 000 total.  The
    // SeqCst `nl_atomic_fetch_add' contract requires the final slot
    // value equals the total iteration count; a lost update (= non-
    // atomic write race) would fall short.
    const PER_THREAD: i64 = 10_000;

    // SAFETY for the raw pointer cross-thread share: the `AtomicI64`
    // inside `ProbeBox` makes the slot soundly accessible from any
    // thread via `nl_atomic_fetch_add` (= the underlying
    // `AtomicI64::fetch_add` is the same contract `std` exposes).
    // We pass the base pointer as a `usize` to dodge `*mut i64`'s
    // !Send bound; each worker casts back to `*mut i64` inside.
    let base_usize = bx.as_box_ptr() as usize;
    let bx_keep_alive = Arc::clone(&bx);

    let t1 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            unsafe { nelisp_build_tool::elisp_cc_spike::rc_inc(p) };
        }
    });
    let t2 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            unsafe { nelisp_build_tool::elisp_cc_spike::rc_inc(p) };
        }
    });

    t1.join().expect("thread 1 panicked");
    t2.join().expect("thread 2 panicked");

    assert_eq!(
        bx_keep_alive.refcount.load(Ordering::SeqCst),
        2 * PER_THREAD,
        "concurrent inc must converge to N1 + N2 (= no lost updates \
         under SeqCst atomic-fetch-add)"
    );
}
