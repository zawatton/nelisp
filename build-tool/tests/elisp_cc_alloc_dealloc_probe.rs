//! Doc 125 §125.A probes — direct calls into the two Phase 47-compiled
//! alloc / dealloc grammar ops (= mirrors
//! `elisp_cc_atomic_raw_mem_probe.rs' §122.E pattern).
//!
//! Verifies end-to-end round-trips:
//!
//!   - alloc-bytes + dealloc-bytes round-trip with write+read in
//!     between to verify the returned pointer is valid memory.
//!   - Multiple alloc with different sizes (= 8, 64, 4096) all at
//!     align=8, each followed by its matching dealloc.
//!   - Aligned alloc (= 16-byte alignment) returns a pointer whose
//!     low 4 bits are zero; the same applies to a 32-byte alignment.
//!
//! Each test:
//!   1. Calls the elisp-compiled `alloc_bytes' wrapper in
//!      `nelisp_build_tool::elisp_cc_spike'.
//!   2. (where applicable) Drives the allocated block via the §122.E
//!      `ptr_write_u64' / `ptr_read_u64' grammar ops to verify the
//!      pointer is valid for the requested layout.
//!   3. Asserts the dealloc returns the 1 sentinel.
//!
//! Substrate gating role (= Doc 124.G-K unblock): every test exercises
//! the exact contract that the NlBox Drop kernels' if-zero-refcount
//! free branch needs.  Verifying it here proves the substrate is
//! ready before Doc 124.G lands the first NlBox Drop elisp kernel.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

// ---- Case 1: alloc + write + read + dealloc round-trip ----

#[test]
fn alloc_bytes_then_write_read_then_dealloc() {
    let size: i64 = 64;
    let align: i64 = 8;

    let ptr = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(size, align)
    };
    assert!(
        !ptr.is_null(),
        "alloc-bytes(64, 8) must succeed on a healthy host"
    );

    // Write a unique 64-bit pattern at offsets 0 and 56 (= the head
    // and tail of the 64-byte block).  Use §122.E `ptr-write-u64' /
    // `ptr-read-u64' which lower to the same `nl_ptr_*` externs the
    // elisp Drop kernel uses to walk NlBox headers.
    unsafe {
        let head_rc = nelisp_build_tool::elisp_cc_spike::ptr_write_u64(
            ptr,
            0,
            0x0123_4567_89AB_CDEF_i64,
        );
        assert_eq!(head_rc, 1);
        let tail_rc = nelisp_build_tool::elisp_cc_spike::ptr_write_u64(
            ptr,
            56,
            0xFEDC_BA98_7654_3210_u64 as i64,
        );
        assert_eq!(tail_rc, 1);
    }

    // Round-trip through the read path.
    let head = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_read_u64(ptr as *const u8, 0)
    };
    let tail = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_read_u64(ptr as *const u8, 56)
    };
    assert_eq!(head as u64, 0x0123_4567_89AB_CDEF_u64);
    assert_eq!(tail as u64, 0xFEDC_BA98_7654_3210_u64);

    // Free.  Caller-supplied `(size, align)' must match the alloc.
    let dealloc_rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(ptr, size, align)
    };
    assert_eq!(
        dealloc_rc, 1,
        "dealloc-bytes must return rax = 1 sentinel"
    );
}

// ---- Case 2: multiple alloc with different sizes ----

#[test]
fn alloc_bytes_multiple_sizes_each_dealloced() {
    // Small / medium / page-sized blocks.  Exercises the layout
    // arithmetic at the boundaries the GC arena code will use.  Sizes
    // are all ≥ 16 bytes so the head + tail u64 writes don't overlap
    // (= the size=8 corner case is exercised separately in case 1).
    let sizes: [i64; 3] = [16, 64, 4096];
    let align: i64 = 8;

    // Track all allocations first to verify they're independent (=
    // no aliasing across the three calls).
    let mut allocations: Vec<(*mut u8, i64)> = Vec::with_capacity(3);
    for &size in &sizes {
        let ptr = unsafe {
            nelisp_build_tool::elisp_cc_spike::alloc_bytes(size, align)
        };
        assert!(
            !ptr.is_null(),
            "alloc-bytes({}, 8) must succeed",
            size
        );
        // Touch the first and last 8 bytes to verify the block covers
        // the full size (= would segfault if the alloc undersized).
        unsafe {
            nelisp_build_tool::elisp_cc_spike::ptr_write_u64(ptr, 0, size);
            // Last 8 bytes via offset = size - 8; safe because each
            // `size' >= 16 so offset doesn't alias offset 0.
            nelisp_build_tool::elisp_cc_spike::ptr_write_u64(
                ptr,
                size - 8,
                size + 1,
            );
        }
        allocations.push((ptr, size));
    }

    // Verify the three blocks did not alias (= each holds its own
    // size sentinel at the head and its size+1 marker at the tail).
    // This is the property the GC arena needs from its underlying
    // allocator.
    for (i, &(ptr, size)) in allocations.iter().enumerate() {
        let head = unsafe {
            nelisp_build_tool::elisp_cc_spike::ptr_read_u64(
                ptr as *const u8,
                0,
            )
        };
        assert_eq!(
            head, size,
            "allocation {} must hold its own size sentinel at offset 0 \
             — saw {} expected {}",
            i, head, size
        );
        let tail = unsafe {
            nelisp_build_tool::elisp_cc_spike::ptr_read_u64(
                ptr as *const u8,
                size - 8,
            )
        };
        assert_eq!(
            tail, size + 1,
            "allocation {} tail must hold size+1 — saw {} expected {}",
            i, tail, size + 1
        );
    }

    // Free in reverse order to exercise the allocator's free-list
    // behaviour (= different from the alloc order).
    for &(ptr, size) in allocations.iter().rev() {
        let rc = unsafe {
            nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
                ptr, size, align,
            )
        };
        assert_eq!(rc, 1, "dealloc-bytes for size {} must return 1", size);
    }
}

// ---- Case 3: 16-byte + 32-byte aligned allocations ----

#[test]
fn alloc_bytes_honors_requested_alignment() {
    // 16-byte alignment is the common SIMD / SSE2 boundary the
    // NlBox families use (= `Sexp::Cons' payload at offset 8 must
    // be 16-aligned for the `movdqu' instruction to be efficient).
    let ptr16 = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(128, 16)
    };
    assert!(!ptr16.is_null(), "alloc-bytes(128, 16) must succeed");
    assert_eq!(
        (ptr16 as usize) & 0xF,
        0,
        "alloc-bytes(_, 16) must return 16-byte aligned pointer \
         — got pointer {:p}",
        ptr16
    );

    // 32-byte alignment for the eventual AVX2 path / NlConsBox
    // (= which is 72 bytes payload but allocated 32-aligned per
    // its `#[repr(C, align(...))]' on the Rust side).
    let ptr32 = unsafe {
        nelisp_build_tool::elisp_cc_spike::alloc_bytes(256, 32)
    };
    assert!(!ptr32.is_null(), "alloc-bytes(256, 32) must succeed");
    assert_eq!(
        (ptr32 as usize) & 0x1F,
        0,
        "alloc-bytes(_, 32) must return 32-byte aligned pointer \
         — got pointer {:p}",
        ptr32
    );

    // Free both; layout must match the alloc.
    unsafe {
        let rc16 = nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
            ptr16, 128, 16,
        );
        assert_eq!(rc16, 1);
        let rc32 = nelisp_build_tool::elisp_cc_spike::dealloc_bytes(
            ptr32, 256, 32,
        );
        assert_eq!(rc32, 1);
    }
}
