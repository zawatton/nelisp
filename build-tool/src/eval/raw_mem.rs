//! Doc 122 §122.E + Doc 125 §125.A — atomic + raw memory primitives
//! + alloc / dealloc primitives.
//!
//! These eight `#[no_mangle]` `extern "C"` wrappers provide the
//! substrate gating layer required by Doc 123-128 (= refcount elisp化,
//! `nl*.rs::Clone/Drop` elisp化, alloc/dealloc handlers).  Without
//! `atomic-fetch-add` + `atomic-compare-exchange` + raw `ptr-read-*` /
//! `ptr-write-*` ops, Phase 47 grammar cannot express refcount
//! semantics; without `alloc-bytes` + `dealloc-bytes`, Phase 47
//! grammar cannot express the if-zero-refcount free branch in the
//! Doc 124 NlBox Drop kernels.  Both are blockers for the entire
//! substrate-internalization chain at the elisp compiler boundary.
//!
//! Surface (matching the Phase 47 grammar ops in
//! `lisp/nelisp-phase47-compiler.el`):
//!
//! | extern                                  | grammar op                          |
//! |-----------------------------------------|-------------------------------------|
//! | `nl_atomic_fetch_add(ptr, delta)`       | `(atomic-fetch-add PTR DELTA)`      |
//! | `nl_atomic_compare_exchange(ptr, e, n)` | `(atomic-compare-exchange PTR E N)` |
//! | `nl_ptr_read_u64(ptr, offset)`          | `(ptr-read-u64 PTR OFFSET)`         |
//! | `nl_ptr_write_u64(ptr, offset, val)`    | `(ptr-write-u64 PTR OFFSET VAL)`    |
//! | `nl_ptr_read_u8(ptr, offset)`           | `(ptr-read-u8 PTR OFFSET)`          |
//! | `nl_ptr_write_u8(ptr, offset, val)`     | `(ptr-write-u8 PTR OFFSET VAL)`     |
//! | `nl_alloc_bytes(size, align)`           | `(alloc-bytes SIZE ALIGN)`          |
//! | `nl_dealloc_bytes(ptr, size, align)`    | `(dealloc-bytes PTR SIZE ALIGN)`    |
//!
//! All atomic operations use `Ordering::SeqCst`.  The §122.E spec
//! contemplates per-op memory ordering (= 5-variant integer arg) but
//! the substrate gating use cases (= refcount + nl*.rs lifecycle) only
//! require SeqCst, and the elisp callers compose `atomic-fetch-add`
//! with `cond` branches that already serialize the result inspection.
//! Future Doc 122.E.2 may widen this surface.
//!
//! Memory model contract for callers:
//!
//! - The atomic ops require a well-aligned `*mut i64` (= 8-byte
//!   aligned).  Callers must not pass interior pointers into structs
//!   without confirming alignment.  Phase 47 emit code does no
//!   alignment check; the elisp caller is responsible.
//! - The raw mem ops perform `*p` reads/writes with no ordering
//!   guarantees beyond what the underlying CPU provides for naturally-
//!   aligned loads/stores (= x86_64 native atomicity on aligned u64/u8).
//!   Cross-thread visibility is *not* guaranteed without explicit
//!   atomic ops; use these ops only on caller-pinned (= refcount
//!   protected or fresh-alloc) buffers.
//! - `offset` is in *bytes* and is added as `usize` arithmetic.
//!   Negative offsets are masked via `as usize` wrap (= caller bug
//!   if relied upon).
//!
//! Refcount elisp化 unblock matrix (= Doc 123):
//!
//! - `nl_rc_inc_strong` body shrinks from a `Relaxed::fetch_add(1)`
//!   Rust macro to an elisp expression: `(atomic-fetch-add
//!   (rc-counter-ptr SEXP) 1)`.  The current `rc_primitives.rs`
//!   `NlConsBoxRef::rc_inc_raw` becomes a thin pass-through.
//! - `nl_rc_dec_strong` body becomes: `(atomic-fetch-add
//!   (rc-counter-ptr SEXP) -1)` + `cond` branch on the i64 return.
//! - The Bacon-Rajan cycle collector's CAS-based refcount promotion
//!   uses `(atomic-compare-exchange COUNTER-PTR OLD NEW)` directly.

use std::alloc::{self, Layout};
use std::sync::atomic::{AtomicI64, Ordering};

/// Doc 122 §122.E — atomic fetch-and-add on an `i64` slot with
/// `Ordering::SeqCst`.
///
/// Returns the *previous* (pre-add) value.  Semantically matches
/// `std::sync::atomic::AtomicI64::fetch_add(delta, Ordering::SeqCst)`.
///
/// # Safety
/// - `ptr` must be non-null, 8-byte aligned, and point at a live
///   `i64` slot.  The slot may be shared with other threads provided
///   *all* concurrent accesses go through `nl_atomic_*` or
///   `std::sync::atomic::AtomicI64` — mixing raw `*p` writes with
///   atomic ops is UB.
/// - The slot must outlive the call.  Caller (= elisp Phase 47 emit
///   code) is responsible for keeping the host `Sexp` / `NlConsBox`
///   pinned via refcount or stack ownership.
#[no_mangle]
pub unsafe extern "C" fn nl_atomic_fetch_add(ptr: *mut i64, delta: i64) -> i64 {
    // SAFETY: caller asserts `ptr` is valid + aligned for the lifetime
    // of this call.  `AtomicI64::from_ptr` re-interprets the slot as
    // an atomic; this is sound when all concurrent accesses are
    // atomic.
    let atomic = unsafe { AtomicI64::from_ptr(ptr) };
    atomic.fetch_add(delta, Ordering::SeqCst)
}

/// Doc 122 §122.E — atomic compare-and-exchange on an `i64` slot with
/// `Ordering::SeqCst` for both success and failure.
///
/// Returns `1` on success (= `*ptr` was equal to `expected` and has
/// been replaced with `new_val`) or `0` on failure (= `*ptr` was not
/// equal to `expected`; the slot is unchanged).  The Bacon-Rajan
/// cycle collector's strong-count promotion uses this directly.
///
/// We don't expose the previous value on failure (= caller can `re-
/// load` via `nl_ptr_read_u64` + reinterpret).  Doc 122.E.2 may add
/// `nl_atomic_compare_exchange_weak` / `_full` variants if a future
/// caller needs them.
///
/// # Safety
/// Same alignment + lifetime contract as
/// [`nl_atomic_fetch_add`].
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

/// Doc 122 §122.E — raw `u64` read at `ptr + offset` (= bytes).
///
/// Returns the read value re-cast to `i64`.  No bounds check, no
/// alignment check: caller is responsible.  Used by the Doc 123-128
/// substrate to walk `nl*.rs` headers (= refcount slot, type tag
/// slot, payload pointer) without going through the Rust accessor
/// methods.
///
/// # Safety
/// - `ptr + offset` (= byte arithmetic) must be a non-null, 8-byte
///   aligned address pointing at 8 bytes of readable, initialized
///   memory.
/// - No concurrent writes via non-atomic stores; mixing with
///   `nl_atomic_*` is UB (= same as
///   [`nl_atomic_fetch_add`] contract).
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_read_u64(ptr: *const u8, offset: i64) -> i64 {
    let p = (ptr as usize).wrapping_add(offset as usize) as *const u64;
    // SAFETY: caller-asserted alignment + readability.
    let v = unsafe { *p };
    v as i64
}

/// Doc 122 §122.E — raw `u64` write at `ptr + offset` (= bytes).
///
/// Writes `val as u64` (= low 64 bits of the elisp i64 argument).
/// No bounds / alignment / aliasing check.
///
/// # Safety
/// - `ptr + offset` must be a non-null, 8-byte aligned, writable
///   8-byte slot.
/// - No concurrent reads / writes (= refcount-protected or fresh
///   alloc).
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_write_u64(ptr: *mut u8, offset: i64, val: i64) {
    let p = (ptr as usize).wrapping_add(offset as usize) as *mut u64;
    // SAFETY: caller-asserted alignment + writability.
    unsafe { *p = val as u64 };
}

/// Doc 122 §122.E — raw `u8` read at `ptr + offset` (= bytes).
///
/// Returns the byte zero-extended to `i64` (= no sign extension).
/// Used by the substrate to inspect tag bytes (= `Sexp::tag` at
/// offset 0 of a `Sexp` slot) without going through `Sexp::tag()`.
///
/// # Safety
/// - `ptr + offset` must be a non-null, readable 1-byte slot.
///   Alignment of 1 suffices for a byte access.
/// - No concurrent writes via non-atomic stores.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_read_u8(ptr: *const u8, offset: i64) -> i64 {
    let p = (ptr as usize).wrapping_add(offset as usize) as *const u8;
    // SAFETY: caller-asserted readability.
    let v = unsafe { *p };
    v as i64
}

/// Doc 122 §122.E — raw `u8` write at `ptr + offset` (= bytes).
///
/// Writes the low 8 bits of `val` (= `val as u8`).
///
/// # Safety
/// - `ptr + offset` must be a non-null, writable 1-byte slot.
/// - No concurrent reads / writes.
#[no_mangle]
pub unsafe extern "C" fn nl_ptr_write_u8(ptr: *mut u8, offset: i64, val: i64) {
    let p = (ptr as usize).wrapping_add(offset as usize) as *mut u8;
    // SAFETY: caller-asserted writability.
    unsafe { *p = val as u8 };
}

/// Doc 125 §125.A — generic byte-level allocator wrapping
/// `std::alloc::alloc(Layout::from_size_align(size, align))`.
///
/// Returns the freshly-allocated `*mut u8` on success or
/// `std::ptr::null_mut()` on either layout-arg error (= align not a
/// power of two, or size rounded up to align overflows isize) or
/// underlying OOM.  The elisp grammar op `(alloc-bytes SIZE ALIGN)`
/// re-casts the returned pointer to `i64` and the elisp caller must
/// inspect for 0 before dereferencing.
///
/// The 0-sized allocation is rejected up-front (= return null) to
/// keep the `dealloc-bytes` contract simple — a 0-sized free is a
/// no-op and we don't want callers to distinguish "alloc returned a
/// valid 0-size sentinel pointer" from null.
///
/// # Safety
/// This function itself is sound — it only touches the global
/// allocator.  Marked `unsafe` for ABI uniformity with the rest of
/// the `nl_*` extern surface (= every Phase 47-lowered grammar op
/// is `unsafe extern "C"`, so the elisp wrapper crate doesn't need
/// per-op safety-attr discrimination).
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

/// Doc 125 §125.A — generic byte-level deallocator wrapping
/// `std::alloc::dealloc(ptr, Layout::from_size_align(size, align))`.
///
/// The `(size, align)` pair *must* match the values passed to the
/// matching `nl_alloc_bytes` call — `std::alloc::dealloc` is UB on
/// layout mismatch.  Doc 124 NlBox Drop kernels avoid this risk by
/// going through §125.B-F's per-type externs (= layout pre-baked at
/// Rust compile time via `Layout::new::<NlT>()`); the generic
/// dealloc op is for Doc 126-128's bridge GC arena code which
/// tracks `(size, align)` per allocation in its own metadata.
///
/// Null pointer + zero-size are silent no-ops (= matches `free(NULL)`
/// semantics + avoids the `from_size_align` zero-size rejection).
/// Invalid layout args (= bad align, overflow) are likewise silent
/// no-ops — caller cannot recover meaningfully and the alternative
/// would be a panic across the `extern "C"` boundary.
///
/// # Safety
/// - `ptr` must be either null or a pointer returned by
///   `nl_alloc_bytes` (or compatible `alloc::alloc`) with the *same*
///   `(size, align)` arguments.
/// - The slot must not be accessed (read or write) after this call.
/// - Concurrent free of the same slot is UB; the elisp Drop kernel's
///   `atomic-fetch-add` decrement-to-zero check is the
///   single-thread-wins guard.
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

    // ---- Doc 125 §125.A alloc / dealloc ----

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
