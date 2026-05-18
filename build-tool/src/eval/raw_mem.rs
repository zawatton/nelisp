//! Atomic + raw-memory + alloc/dealloc primitives backing Phase 47
//! grammar ops.  All atomics SeqCst.  Callers ensure alignment (8 for
//! u64, 1 for u8) and liveness; offset is byte-wise via `wrapping_add'.

use std::alloc::{self, Layout};
use std::sync::atomic::{AtomicI64, Ordering};

/// # Safety: `ptr` non-null, 8-byte aligned, live; all accesses atomic.
#[no_mangle]
pub unsafe extern "C" fn nl_atomic_fetch_add(ptr: *mut i64, delta: i64) -> i64 {
    let atomic = unsafe { AtomicI64::from_ptr(ptr) };
    atomic.fetch_add(delta, Ordering::SeqCst)
}

/// Returns 1 on success, 0 on failure.  Safety: same as [`nl_atomic_fetch_add`].
#[no_mangle]
pub unsafe extern "C" fn nl_atomic_compare_exchange(
    ptr: *mut i64,
    expected: i64,
    new_val: i64,
) -> i64 {
    let atomic = unsafe { AtomicI64::from_ptr(ptr) };
    match atomic.compare_exchange(expected, new_val, Ordering::SeqCst, Ordering::SeqCst) {
        Ok(_) => 1,
        Err(_) => 0,
    }
}

/// # Safety: `ptr + offset` non-null, 8-byte aligned, readable; no concurrent non-atomic writes.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_read_u64(ptr: *const u8, offset: i64) -> i64 {
    let p = (ptr as usize).wrapping_add(offset as usize) as *const u64;
    unsafe { *p as i64 }
}

/// # Safety: `ptr + offset` non-null, 8-byte aligned, writable, no concurrent r/w.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_write_u64(ptr: *mut u8, offset: i64, val: i64) {
    let p = (ptr as usize).wrapping_add(offset as usize) as *mut u64;
    unsafe { *p = val as u64 };
}

/// # Safety: `ptr + offset` non-null, readable 1-byte slot.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_read_u8(ptr: *const u8, offset: i64) -> i64 {
    let p = (ptr as usize).wrapping_add(offset as usize) as *const u8;
    unsafe { *p as i64 }
}

/// # Safety: `ptr + offset` non-null, writable 1-byte slot, no concurrent r/w.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_write_u8(ptr: *mut u8, offset: i64, val: i64) {
    let p = (ptr as usize).wrapping_add(offset as usize) as *mut u8;
    unsafe { *p = val as u8 };
}

/// Returns null on bad layout / zero/neg args / OOM.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_bytes(size: i64, align: i64) -> *mut u8 {
    if size <= 0 || align <= 0 {
        return std::ptr::null_mut();
    }
    let Ok(layout) = Layout::from_size_align(size as usize, align as usize) else {
        return std::ptr::null_mut();
    };
    unsafe { alloc::alloc(layout) }
}

/// # Safety: ptr must be null or from matching `nl_alloc_bytes(size,align)`.
#[no_mangle]
pub unsafe extern "C" fn nl_dealloc_bytes(ptr: *mut u8, size: i64, align: i64) {
    if ptr.is_null() || size <= 0 || align <= 0 {
        return;
    }
    let Ok(layout) = Layout::from_size_align(size as usize, align as usize) else {
        return;
    };
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
