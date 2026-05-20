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

    fn as_box_ptr(&self) -> *const u8 {
        self as *const ProbeBox as *const u8
    }

    fn as_box_ptr_mut(&self) -> *mut i64 {
        self as *const ProbeBox as *mut i64
    }
}

// ---- Case 1: fresh-allocation snapshot reads back the initial value ----

#[test]
fn rc_strong_count_fresh_alloc_reads_one() {
    // Layout sanity — `refcount' must be at byte offset 64 for the
    // elisp body's `(ptr-read-u64 box-ptr 64)' to land on the right slot.
    assert_eq!(
        std::mem::offset_of!(ProbeBox, refcount),
        64,
        "ProbeBox layout must mirror NlConsBox (= refcount at +64)"
    );

    let bx = ProbeBox::new(1);
    let count = unsafe { nelisp_build_tool::elisp_cc_spike::rc_strong_count(bx.as_box_ptr()) };
    assert_eq!(
        count, 1,
        "nelisp_rc_strong_count must return the current refcount value (= 1 here)"
    );
    // The read must not mutate the slot.
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        1,
        "ptr-read-u64 must be a pure load — slot value unchanged"
    );
}

// ---- Case 2: snapshot after one inc reads back 2 ----

#[test]
fn rc_strong_count_after_single_inc_reads_two() {
    let bx = ProbeBox::new(1);
    // Bump via §123.A `nelisp_rc_inc' (= layout-share confirmation; same
    // REFCOUNT_OFFSET = 64 constant compiled into both kernels).
    let prev = unsafe { nelisp_build_tool::elisp_cc_spike::rc_inc(bx.as_box_ptr_mut()) };
    assert_eq!(prev, 1, "rc_inc must return pre-add value");

    let count = unsafe { nelisp_build_tool::elisp_cc_spike::rc_strong_count(bx.as_box_ptr()) };
    assert_eq!(
        count, 2,
        "nelisp_rc_strong_count after one inc must read back as 2 \
         (= layout-share check between §123.A inc and §123.C read)"
    );
}

// ---- Case 3: snapshot after N inc reads back 1 + N ----

#[test]
fn rc_strong_count_after_multi_inc_tracks_total() {
    let bx = ProbeBox::new(1);

    const N: i64 = 7;
    for _ in 0..N {
        unsafe {
            nelisp_build_tool::elisp_cc_spike::rc_inc(bx.as_box_ptr_mut());
        }
    }

    let count = unsafe { nelisp_build_tool::elisp_cc_spike::rc_strong_count(bx.as_box_ptr()) };
    assert_eq!(
        count,
        1 + N,
        "nelisp_rc_strong_count after {} incs must read back as {} \
         (= initial + N)",
        N,
        1 + N
    );
    // SeqCst load from the raw atomic must agree.
    assert_eq!(
        bx.refcount.load(Ordering::SeqCst),
        1 + N,
        "raw atomic load must agree with ptr-read-u64 result"
    );
}

// ---- Case 4: high-value (above 2^31) refcount reads back as u64 ----

#[test]
fn rc_strong_count_high_value_no_sign_extension() {
    // Refcount domain is `usize'; values above 2^31 are valid (on
    // 64-bit hosts).  Verify that `ptr-read-u64' returns the bits
    // verbatim — values up to 2^63-1 read back as positive i64,
    // values 2^63..2^64 wrap to negative.  We test a high but
    // positive value (= 0x0123_4567_89AB_CDEF) to catch any
    // accidental u32 truncation in the lowering.
    let bx = ProbeBox::new(0x0123_4567_89AB_CDEF_i64);

    let count = unsafe { nelisp_build_tool::elisp_cc_spike::rc_strong_count(bx.as_box_ptr()) };
    assert_eq!(
        count, 0x0123_4567_89AB_CDEF_i64,
        "nelisp_rc_strong_count must perform a full 64-bit load \
         (= no u32 truncation in the §122.E `ptr-read-u64' lowering)"
    );
}
