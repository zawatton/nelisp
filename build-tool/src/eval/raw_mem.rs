//! Doc 122 §122.E — atomic + raw memory primitives.
//!
//! These six `#[no_mangle]` `extern "C"` wrappers provide the substrate
//! gating layer required by Doc 123-128 (= refcount elisp化,
//! `nl*.rs::Clone/Drop` elisp化, alloc/dealloc handlers).  Without
//! `atomic-fetch-add` + `atomic-compare-exchange` + raw `ptr-read-*` /
//! `ptr-write-*` ops, Phase 47 grammar cannot express refcount
//! semantics, so the entire substrate-internalization chain is
//! blocked at the elisp compiler boundary.
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
}
