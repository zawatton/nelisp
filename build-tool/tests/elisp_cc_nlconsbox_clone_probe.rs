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

// ---- Case 1: single clone — rc += 1 and return value == input ----

#[test]
fn nlconsbox_clone_single_increments_and_returns_input_ptr() {
    // Layout sanity — `refcount' must be at byte offset 64 for the
    // elisp body's `(+ box-ptr 64)' to land on the right slot.
    assert_eq!(
        std::mem::offset_of!(ProbeBox, refcount),
        64,
        "ProbeBox layout must mirror NlConsBox (= refcount at +64)"
    );

    let bx = ProbeBox::new(1);
    let input_ptr = bx.as_box_ptr();
    let returned_ptr = unsafe { nelisp_build_tool::elisp_cc_spike::nlconsbox_clone(input_ptr) };
    assert_eq!(
        returned_ptr as usize, input_ptr as usize,
        "nelisp_nlconsbox_clone must return the *input* pointer unchanged \
         (= the cloned handle aliases the same inner box)"
    );
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        2,
        "post-call refcount slot must be old + 1 (= Clone bumps strong count by 1)"
    );
}

// ---- Case 2: N clones — rc advances by N, return ptr stable ----

#[test]
fn nlconsbox_clone_n_times_count_equals_n_plus_initial() {
    const INITIAL: i64 = 1;
    const N: usize = 10;

    let bx = ProbeBox::new(INITIAL);
    let input_ptr = bx.as_box_ptr();
    for i in 0..N {
        let returned = unsafe { nelisp_build_tool::elisp_cc_spike::nlconsbox_clone(input_ptr) };
        assert_eq!(
            returned as usize, input_ptr as usize,
            "iteration {i}: returned pointer must equal input (alias, not copy)"
        );
        // After iteration i (0-indexed), we've done i+1 clones, so
        // the slot should hold INITIAL + (i + 1).
        assert_eq!(
            bx.refcount.load(Ordering::SeqCst),
            INITIAL + (i as i64) + 1,
            "iteration {i}: slot must advance by exactly 1 per call"
        );
    }
    // Final state: INITIAL + N total bumps.
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        INITIAL + N as i64,
        "N-call total: slot = INITIAL + N (= N + 1 = 11 here)"
    );
}

// ---- Case 3: concurrent clone from 2 threads — atomic semantics ----

#[test]
fn nlconsbox_clone_concurrent_no_lost_updates() {
    use std::sync::Arc;
    use std::thread;

    // Allocate on the heap so both threads can share the pointer
    // (= `&'static`-equivalent via Arc, dropped at the end of the
    // test).  The `AtomicI64` inside is internally mutable, so the
    // shared `&ProbeBox` is sound.
    let bx = Arc::new(ProbeBox::new(0));

    // 10 000 clones per thread × 2 threads = 20 000 total.  The
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
            let _ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlconsbox_clone(p) };
            // Returned pointer always equals the input (= alias semantics).
            // We don't assert here per-iteration to keep the loop tight;
            // the slot check after join is the lost-update detector.
        }
    });
    let t2 = thread::spawn(move || {
        let p = base_usize as *mut i64;
        for _ in 0..PER_THREAD {
            let _ret = unsafe { nelisp_build_tool::elisp_cc_spike::nlconsbox_clone(p) };
        }
    });

    t1.join().expect("thread 1 panicked");
    t2.join().expect("thread 2 panicked");

    assert_eq!(
        bx_keep_alive.refcount.load(Ordering::SeqCst),
        2 * PER_THREAD,
        "concurrent clone must converge to N1 + N2 (= no lost updates \
         under SeqCst atomic-fetch-add, mirroring §123.A `nelisp_rc_inc')"
    );
}
