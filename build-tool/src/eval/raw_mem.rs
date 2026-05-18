//! Atomic + raw-memory + alloc/dealloc extern "C" primitives.
//!
//! Eight `#[no_mangle]` wrappers backing the Phase 47 grammar ops
//! `atomic-fetch-add` / `atomic-compare-exchange` / `ptr-read-u64` /
//! `ptr-write-u64` / `ptr-read-u8` / `ptr-write-u8` / `alloc-bytes` /
//! `dealloc-bytes`.  All atomics use `Ordering::SeqCst`.  Callers
//! must guarantee alignment (= 8 bytes for u64, 1 for u8) and
//! liveness; offset is in bytes via `wrapping_add` (no bounds check).

use std::alloc::{self, Layout};
use std::sync::atomic::{AtomicI64, Ordering};

/// Atomic fetch-and-add on an `i64` slot, returning the pre-add value.
///
/// # Safety
/// - `ptr` must be non-null, 8-byte aligned, and point at a live
///   `i64` slot.  Concurrent accesses must all go through atomic ops
///   (mixing raw `*p` writes is UB).  Slot must outlive the call.
#[no_mangle]
pub unsafe extern "C" fn nl_atomic_fetch_add(ptr: *mut i64, delta: i64) -> i64 {
    // SAFETY: caller asserts `ptr` is valid + aligned for the lifetime
    // of this call.  `AtomicI64::from_ptr` re-interprets the slot as
    // an atomic; this is sound when all concurrent accesses are
    // atomic.
    let atomic = unsafe { AtomicI64::from_ptr(ptr) };
    atomic.fetch_add(delta, Ordering::SeqCst)
}

/// Atomic compare-and-exchange on an `i64` slot.  Returns `1` on
/// success (= slot equal to `expected`, replaced with `new_val`),
/// `0` on failure (= slot unchanged).
///
/// # Safety
/// Same alignment + lifetime contract as [`nl_atomic_fetch_add`].
#[no_mangle]
pub unsafe extern "C" fn nl_atomic_compare_exchange(
    ptr: *mut i64,
    expected: i64,
    new_val: i64,
) -> i64 {
    // SAFETY: see `nl_atomic_fetch_add`.
    let atomic = unsafe { AtomicI64::from_ptr(ptr) };
    match atomic.compare_exchange(expected, new_val, Ordering::SeqCst, Ordering::SeqCst) {
        Ok(_) => 1,
        Err(_) => 0,
    }
}

/// Raw `u64` read at `ptr + offset` (bytes), returned as `i64`.
///
/// # Safety
/// - `ptr + offset` must be non-null, 8-byte aligned, and point at 8
///   bytes of readable initialized memory.
/// - No concurrent non-atomic writes; mixing with `nl_atomic_*` is UB.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_read_u64(ptr: *const u8, offset: i64) -> i64 {
    let p = (ptr as usize).wrapping_add(offset as usize) as *const u64;
    // SAFETY: caller-asserted alignment + readability.
    let v = unsafe { *p };
    v as i64
}

/// Raw `u64` write at `ptr + offset` (bytes), low 64 bits of `val`.
///
/// # Safety
/// - `ptr + offset` must be non-null, 8-byte aligned, writable.
/// - No concurrent reads/writes (refcount-protected or fresh alloc).
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_write_u64(ptr: *mut u8, offset: i64, val: i64) {
    let p = (ptr as usize).wrapping_add(offset as usize) as *mut u64;
    // SAFETY: caller-asserted alignment + writability.
    unsafe { *p = val as u64 };
}

/// Raw `u8` read at `ptr + offset` (bytes), zero-extended to `i64`.
///
/// # Safety
/// - `ptr + offset` must be a non-null, readable 1-byte slot.
/// - No concurrent non-atomic writes.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_read_u8(ptr: *const u8, offset: i64) -> i64 {
    let p = (ptr as usize).wrapping_add(offset as usize) as *const u8;
    // SAFETY: caller-asserted readability.
    let v = unsafe { *p };
    v as i64
}

/// Raw `u8` write at `ptr + offset` (bytes), low 8 bits of `val`.
///
/// # Safety
/// - `ptr + offset` must be a non-null, writable 1-byte slot.
/// - No concurrent reads/writes.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_write_u8(ptr: *mut u8, offset: i64, val: i64) {
    let p = (ptr as usize).wrapping_add(offset as usize) as *mut u8;
    // SAFETY: caller-asserted writability.
    unsafe { *p = val as u8 };
}

/// Generic byte-level allocator wrapping
/// `std::alloc::alloc(Layout::from_size_align(size, align))`.  Returns
/// null on layout error (bad align, isize overflow), zero/negative
/// args, or OOM.
///
/// # Safety
/// Sound by itself (only touches global allocator); marked `unsafe`
/// for ABI uniformity with the rest of the `nl_*` extern surface.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_bytes(size: i64, align: i64) -> *mut u8 {
    if size <= 0 || align <= 0 {
        return std::ptr::null_mut();
    }
    let Ok(layout) = Layout::from_size_align(size as usize, align as usize) else {
        return std::ptr::null_mut();
    };
    // SAFETY: `Layout::from_size_align` succeeded → layout is valid;
    // size > 0 by the early return above.  The underlying allocator
    // may return null on OOM, which propagates to the caller.
    unsafe { alloc::alloc(layout) }
}

/// Generic byte-level deallocator wrapping `std::alloc::dealloc`.
/// Null pointer / zero-size / invalid layout args are silent no-ops.
///
/// # Safety
/// - `ptr` must be null or returned by `nl_alloc_bytes` with the
///   *same* `(size, align)` arguments — `dealloc` is UB on mismatch.
/// - Slot must not be accessed after this call.
/// - Concurrent free of the same slot is UB.
#[no_mangle]
pub unsafe extern "C" fn nl_dealloc_bytes(ptr: *mut u8, size: i64, align: i64) {
    if ptr.is_null() || size <= 0 || align <= 0 {
        return;
    }
    let Ok(layout) = Layout::from_size_align(size as usize, align as usize) else {
        return;
    };
    // SAFETY: caller's responsibility — see fn-level doc.  Layout
    // matches the matching alloc call by the doc-comment contract.
    unsafe { alloc::dealloc(ptr, layout) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atomic_fetch_add_returns_old_and_updates_slot() {
        let mut slot: i64 = 10;
        let old = unsafe { nl_atomic_fetch_add(&mut slot as *mut i64, 5) };
        assert_eq!(old, 10, "fetch_add must return the pre-add value");
        assert_eq!(slot, 15, "slot must be updated to old + delta");
    }

    #[test]
    fn atomic_fetch_add_handles_zero_baseline() {
        let mut slot: i64 = 0;
        let old = unsafe { nl_atomic_fetch_add(&mut slot as *mut i64, 1) };
        assert_eq!(old, 0);
        assert_eq!(slot, 1);
    }

    #[test]
    fn atomic_compare_exchange_success_writes_new() {
        let mut slot: i64 = 42;
        let rc = unsafe {
            nl_atomic_compare_exchange(&mut slot as *mut i64, 42, 99)
        };
        assert_eq!(rc, 1, "CAS must return 1 on success");
        assert_eq!(slot, 99, "slot must be replaced on success");
    }

    #[test]
    fn atomic_compare_exchange_failure_leaves_slot() {
        let mut slot: i64 = 42;
        let rc = unsafe {
            nl_atomic_compare_exchange(&mut slot as *mut i64, 7, 99)
        };
        assert_eq!(rc, 0, "CAS must return 0 on mismatch");
        assert_eq!(slot, 42, "slot must be untouched on failure");
    }

    #[test]
    fn ptr_read_write_u64_round_trip() {
        let mut buf: [u64; 4] = [0; 4];
        let base = buf.as_mut_ptr() as *mut u8;
        unsafe {
            nl_ptr_write_u64(base, 16, 0xDEAD_BEEF_CAFE_BABE_u64 as i64);
        }
        let v = unsafe { nl_ptr_read_u64(base as *const u8, 16) };
        assert_eq!(v as u64, 0xDEAD_BEEF_CAFE_BABE_u64);
        assert_eq!(buf[2], 0xDEAD_BEEF_CAFE_BABE_u64);
    }

    #[test]
    fn ptr_read_write_u8_round_trip_zero_extends() {
        let mut buf: [u8; 8] = [0; 8];
        let base = buf.as_mut_ptr();
        unsafe {
            nl_ptr_write_u8(base, 3, 0xFF);
        }
        // 0xFF should zero-extend to 0x00000000_000000FF, not sign-extend.
        let v = unsafe { nl_ptr_read_u8(base as *const u8, 3) };
        assert_eq!(v, 0xFF, "u8 read must zero-extend (255, not -1)");
        assert_eq!(buf[3], 0xFF);
    }

    // ---- alloc / dealloc ----

    #[test]
    fn alloc_dealloc_bytes_round_trip_with_writes() {
        let size: i64 = 64;
        let align: i64 = 8;
        let ptr = unsafe { nl_alloc_bytes(size, align) };
        assert!(!ptr.is_null(), "alloc-bytes(64, 8) must succeed");
        // Write + read through the block to verify the returned pointer
        // is genuinely valid for the requested layout.
        unsafe {
            nl_ptr_write_u64(ptr, 0, 0xCAFE_BABE_DEAD_BEEF_u64 as i64);
            nl_ptr_write_u64(ptr, 56, 0x1122_3344_5566_7788_u64 as i64);
        }
        let head = unsafe { nl_ptr_read_u64(ptr as *const u8, 0) };
        let tail = unsafe { nl_ptr_read_u64(ptr as *const u8, 56) };
        assert_eq!(head as u64, 0xCAFE_BABE_DEAD_BEEF_u64);
        assert_eq!(tail as u64, 0x1122_3344_5566_7788_u64);
        unsafe { nl_dealloc_bytes(ptr, size, align) };
    }

    #[test]
    fn alloc_bytes_rejects_bad_alignment() {
        // 3 is not a power of two — `Layout::from_size_align' rejects.
        let ptr = unsafe { nl_alloc_bytes(32, 3) };
        assert!(ptr.is_null(), "alloc-bytes must reject non-pow2 align");
        // Zero size / negative args also return null.
        assert!(unsafe { nl_alloc_bytes(0, 8) }.is_null());
        assert!(unsafe { nl_alloc_bytes(-1, 8) }.is_null());
        assert!(unsafe { nl_alloc_bytes(32, 0) }.is_null());
    }

    #[test]
    fn dealloc_bytes_null_or_invalid_is_no_op() {
        // null pointer must not crash (= matches `free(NULL)' semantics).
        unsafe { nl_dealloc_bytes(std::ptr::null_mut(), 32, 8) };
        // bad align: silent no-op, must not panic.
        let p = unsafe { nl_alloc_bytes(32, 8) };
        assert!(!p.is_null());
        // wrong-but-safe path: bad align triggers early return, leaks
        // the alloc — that's documented; here we just check no panic.
        unsafe { nl_dealloc_bytes(p, 32, 3) };
        unsafe { nl_dealloc_bytes(p, 32, 8) };
    }

    #[test]
    fn alloc_bytes_16_byte_aligned() {
        // SIMD-friendly alignment for the NlBox families.
        let ptr = unsafe { nl_alloc_bytes(128, 16) };
        assert!(!ptr.is_null());
        assert_eq!(
            (ptr as usize) & 0xF,
            0,
            "alloc-bytes(_, 16) must return 16-byte aligned pointer"
        );
        unsafe { nl_dealloc_bytes(ptr, 128, 16) };
    }
}
