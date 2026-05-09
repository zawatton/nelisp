//! Doc 77c Phase A.1 — `NlRc<T>` self-managed reference-counted ptr.
//!
//! Drop-in replacement for `std::rc::Rc<T>` with a *layout-pinned*
//! inner box so elisp / Cranelift IR can reach the refcount and
//! payload at known byte offsets via `nl-ffi-write-i64' (Phase A.3
//! adds the elisp primitives, Phase A.5 adds the JIT trampolines).
//!
//! Layout (locked by Doc 77c §2.1.1):
//!
//! ```text
//! NlRcInner<T>:  +-------------+  offset 0   (8 bytes)  AtomicUsize refcount
//!                +-------------+  offset 8   (sizeof T) value
//!                +-------------+
//! ```
//!
//! The 8-byte refcount @ offset 0 + value @ offset 8 invariant is
//! enforced by compile-time `const _` assertions at the bottom of
//! this file *and* by runtime layout-tests in `tests' below.  Both
//! gates are critical because elisp will later read/write the
//! refcount via fixed offsets and any drift would silently corrupt
//! GC state.
//!
//! Out of scope for Phase A.1:
//!   - migrating `Sexp` variants to `NlRc` (= Phase A.2)
//!   - `NlConsBox` layout pin                   (= Phase A.2)
//!   - elisp `nl-rc-*` primitives               (= Phase A.3)
//!   - Cranelift trampoline rewrites            (= Phase A.5)
//!
//! Threading: `AtomicUsize` is used for the refcount because the
//! eventual JIT/ffi paths may touch it from any thread that holds a
//! handle.  The `Rc<T>`-equivalent semantics (= "single-threaded
//! ownership") still hold from the elisp side; `AtomicUsize` is
//! defensive plumbing, not a Send/Sync claim.  This struct is
//! intentionally *not* `Send` / `Sync` for now — Phase A.5 will
//! re-evaluate once the JIT trampoline contract is concrete.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Pinned-layout inner box that owns the payload + refcount.  All
/// `NlRc<T>` handles share a single `NlRcInner<T>` allocation.
///
/// `#[repr(C)]` is mandatory: Doc 77c §2.1.1 contracts that
/// `refcount` lives at offset 0 and `value` at offset 8 so elisp
/// `nl-ffi-write-i64` can reach them without going through Rust.
#[repr(C)]
pub struct NlRcInner<T> {
    /// Strong reference count.  Starts at 1 in `NlRc::new`, +1 on
    /// each `Clone`, -1 on each `Drop`.  When it reaches 0 the
    /// dropping handle frees the allocation.
    pub refcount: AtomicUsize,
    /// The payload.  Layout offset is fixed by `#[repr(C)]` + the
    /// 8-byte refcount predecessor (sizeof `AtomicUsize` = 8 on
    /// x86_64 / aarch64; we assert this below).
    pub value: T,
}

/// Self-managed reference-counted smart pointer.  API-equivalent to
/// `std::rc::Rc<T>` for the subset Doc 77c needs (= `new` / `Clone`
/// / `Drop` / `Deref` / `strong_count` / `ptr_eq`).
///
/// The wrapped `NonNull<NlRcInner<T>>` gives us niche optimization
/// (= `Option<NlRc<T>>` is the same size as `NlRc<T>`) and rules out
/// null-ptr UB by construction.
pub struct NlRc<T> {
    ptr: NonNull<NlRcInner<T>>,
    /// Tells the borrow-checker we own a `T` even though the field
    /// is `NonNull<...>` (which is `Copy` and would otherwise let
    /// `T: ?Sized` slip through unsoundly).  Mirrors `std::rc::Rc`.
    _marker: PhantomData<NlRcInner<T>>,
}

impl<T> NlRc<T> {
    /// Allocate a fresh `NlRcInner<T>` on the heap with `refcount = 1`
    /// and return the unique handle.
    ///
    /// Panics on allocation failure (= matches `std::rc::Rc::new`,
    /// which calls `alloc::handle_alloc_error` internally).
    pub fn new(value: T) -> NlRc<T> {
        let layout = Layout::new::<NlRcInner<T>>();
        // SAFETY: `Layout::new::<NlRcInner<T>>()` is non-zero-sized
        // because `NlRcInner` has at least the 8-byte refcount.
        let raw = unsafe { alloc::alloc(layout) } as *mut NlRcInner<T>;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr` was just allocated for `NlRcInner<T>` and is
        // exclusively owned here.  We initialize both fields before
        // anyone else can observe the box.
        unsafe {
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).value),
                value,
            );
        }
        NlRc {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Read the current strong-reference count.  Mirrors
    /// `std::rc::Rc::strong_count`; primarily useful for tests and
    /// the elisp `nl-rc-count` primitive (Phase A.3).
    ///
    /// Uses `Acquire` ordering so a caller observing the returned
    /// value can also rely on having seen all writes published via
    /// the matching `Release` decrement.  This is conservative but
    /// matches the std `Arc` discipline the JIT will eventually
    /// need; the cost on x86_64/aarch64 is a plain load.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr` is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the *underlying box*.  Two clones of the
    /// same `NlRc::new` invocation are pointer-equal; two distinct
    /// `NlRc::new` allocations are not.
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Internal helper — returns a raw `*const NlRcInner<T>` for
    /// callers that need to walk the box (= future ffi shims).
    /// Hidden from the public Phase A.1 surface; expose via a
    /// dedicated module API in Phase A.3.
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn inner_raw(this: &Self) -> *const NlRcInner<T> {
        this.ptr.as_ptr()
    }
}

impl<T> Clone for NlRc<T> {
    /// Bump the refcount and return a new handle that shares the
    /// same inner box.  Uses `Relaxed` because this thread already
    /// holds a handle — no cross-thread synchronization is needed
    /// for the *increment* (= matches `std::sync::Arc::clone`).
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr` is alive because we hold a handle.  The
        // increment cannot wrap because `usize::MAX` handles is
        // physically impossible (= would exhaust 16 EiB of address
        // space at 1 byte per handle); std `Rc` uses the same
        // assumption.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlRc {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Drop for NlRc<T> {
    /// Decrement the refcount.  When it reaches zero, drop the
    /// payload and free the allocation.
    ///
    /// Ordering: `Release` on the decrement so that prior writes by
    /// this handle are visible to whichever thread observes the
    /// final 0; an `Acquire` fence on the freeing path synchronizes
    /// with all earlier `Release` decrements (= textbook `Arc`
    /// teardown pattern).  We must not panic from `Drop`.
    fn drop(&mut self) {
        // SAFETY: `self.ptr` is alive because we hold a handle.
        let prev = unsafe {
            (*self.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        // We were the last handle: synchronize with prior `Release`
        // decrements then drop the payload + free the box.
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0 and we haven't observed any
        // thread re-incrementing through a stale ptr (= `NlRc<T>` is
        // not `Send` / `Sync` for now).  `value` is still valid and
        // aligned; `drop_in_place` runs `T`'s destructor and
        // `dealloc` returns the box's memory.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value));
            let layout = Layout::new::<NlRcInner<T>>();
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl<T> Deref for NlRc<T> {
    type Target = T;

    /// Borrow the payload.  `value` lives at fixed offset 8 inside
    /// `NlRcInner<T>` (asserted at compile time below), so this is a
    /// single load + offset.
    fn deref(&self) -> &T {
        // SAFETY: `self.ptr` is alive because we hold a handle, and
        // `value` was initialized in `NlRc::new` and is only torn
        // down by the last `Drop` (= when no handle exists to call
        // `deref`).  The reference borrows `self`, so it cannot
        // outlive the handle.
        unsafe { &(*self.ptr.as_ptr()).value }
    }
}

// ---- Compile-time layout assertions ----
//
// These guarantee that elisp / Cranelift can reach `refcount` at
// byte offset 0 and `value` at byte offset 8 without consulting
// Rust at runtime.  Phase A.3 (`nl-rc-inc' / `nl-rc-dec' /
// `nl-rc-count' primitives) and Phase A.5 (JIT trampolines) both
// rely on these offsets being constants.
//
// We pick `i64` as the canary `T` because it is the most common
// payload Elisp code stores (= integer cells) and because `i64`
// has the same 8-byte alignment as `AtomicUsize` on x86_64 and
// aarch64, so its offset comes out exactly 8.  Other `T`s with
// stricter alignment (= 16-byte) would push the offset further;
// Phase A.2 will add per-type asserts when `NlConsBox` lands.

const _: () = {
    // `AtomicUsize` is 8 bytes on x86_64 / aarch64; this assert
    // catches any 32-bit target accidentally pulled into the build.
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    // `refcount` is the first field of a `repr(C)` struct, so its
    // offset is always 0; we assert it explicitly anyway so a future
    // refactor that reorders fields fails the build.
    assert!(std::mem::offset_of!(NlRcInner<i64>, refcount) == 0);
    // `value` follows the 8-byte refcount with `i64` alignment = 8.
    assert!(std::mem::offset_of!(NlRcInner<i64>, value) == 8);
};

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;
    use std::sync::Arc;

    // ---- Layout invariants (= Doc 77c §2.1.1 contract) ----

    #[test]
    fn layout_refcount_at_offset_0() {
        assert_eq!(std::mem::offset_of!(NlRcInner<i64>, refcount), 0);
    }

    #[test]
    fn layout_value_at_offset_8() {
        assert_eq!(std::mem::offset_of!(NlRcInner<i64>, value), 8);
    }

    #[test]
    fn layout_atomic_usize_is_8_bytes() {
        // 32-bit targets would break the `value @ offset 8`
        // contract; this test fails loudly on those.
        assert_eq!(std::mem::size_of::<AtomicUsize>(), 8);
    }

    // ---- Basic refcount semantics ----

    #[test]
    fn new_then_drop_frees() {
        // We can't *directly* check that the heap was freed without
        // a custom allocator, but we can confirm the refcount went
        // through the expected lifecycle by sniffing it before drop.
        let rc = NlRc::new(42i64);
        assert_eq!(NlRc::strong_count(&rc), 1);
        drop(rc);
        // No leak detector is wired here; miri (= cargo +nightly
        // miri test) would catch a leak.  The presence of the
        // `dealloc` call in `Drop` + the `multi_clone_drop' test
        // below cover the all-handles-dropped path explicitly.
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let rc = NlRc::new("hello".to_string());
        assert_eq!(NlRc::strong_count(&rc), 1);
    }

    #[test]
    fn clone_increments_refcount() {
        let rc = NlRc::new(1u64);
        let _c1 = rc.clone();
        let _c2 = rc.clone();
        assert_eq!(NlRc::strong_count(&rc), 3);
    }

    #[test]
    fn drop_decrements_refcount() {
        let rc = NlRc::new(1u64);
        let c1 = rc.clone();
        let c2 = rc.clone();
        assert_eq!(NlRc::strong_count(&rc), 3);
        drop(c1);
        assert_eq!(NlRc::strong_count(&rc), 2);
        drop(c2);
        assert_eq!(NlRc::strong_count(&rc), 1);
    }

    // ---- Deref ----

    #[test]
    fn deref_reads_value_i64() {
        // 0xDEAD_BEEF_CAFE_BABE overflows `i64`; cast through `u64`
        // so the bit pattern survives intact (this also matches how
        // elisp `nl-ffi-write-i64` will eventually round-trip the
        // payload through the FFI boundary).
        let v = 0xDEAD_BEEF_CAFE_BABEu64 as i64;
        let rc = NlRc::new(v);
        assert_eq!(*rc, v);
    }

    #[test]
    fn deref_reads_value_string() {
        let rc = NlRc::new(String::from("nelisp"));
        assert_eq!(rc.as_str(), "nelisp");
        // Deref-coercion lets us call `&str` methods directly.
        assert_eq!(rc.len(), 6);
    }

    #[test]
    fn deref_through_clone_sees_same_value() {
        let rc = NlRc::new(vec![1, 2, 3]);
        let c = rc.clone();
        assert_eq!(*rc, *c);
        assert_eq!(rc.len(), 3);
        assert_eq!(c.len(), 3);
    }

    // ---- Pointer identity ----

    #[test]
    fn ptr_eq_same_ptr() {
        let a = NlRc::new(7i32);
        let b = a.clone();
        assert!(NlRc::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_allocations() {
        let a = NlRc::new(7i32);
        let b = NlRc::new(7i32); // same value, different alloc
        assert!(!NlRc::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlRc::new(99i32);
        let b = a.clone();
        let c = a.clone();
        drop(b);
        assert!(NlRc::ptr_eq(&a, &c));
    }

    // ---- Composite payload ----

    #[test]
    fn composite_struct() {
        #[derive(Debug, PartialEq, Eq)]
        struct Compound {
            n: i32,
            s: String,
        }
        let rc = NlRc::new(Compound {
            n: 42,
            s: "doc-77c".into(),
        });
        assert_eq!(rc.n, 42);
        assert_eq!(rc.s, "doc-77c");
        let c = rc.clone();
        assert_eq!(c.n, 42);
        assert!(NlRc::ptr_eq(&rc, &c));
    }

    #[test]
    fn composite_tuple() {
        let rc = NlRc::new((1i32, "two".to_string(), 3.0f64));
        assert_eq!(rc.0, 1);
        assert_eq!(rc.1, "two");
        assert_eq!(rc.2, 3.0);
    }

    // ---- Lifecycle: many clones / many drops ----

    #[test]
    fn multi_clone_drop() {
        let rc = NlRc::new(0xABCDu32);
        let clones: Vec<NlRc<u32>> = (0..5).map(|_| rc.clone()).collect();
        // 1 original + 5 clones = 6 handles.
        assert_eq!(NlRc::strong_count(&rc), 6);
        // Drop 4 clones individually.
        let mut iter = clones.into_iter();
        drop(iter.next().unwrap());
        drop(iter.next().unwrap());
        drop(iter.next().unwrap());
        drop(iter.next().unwrap());
        // 1 original + 1 remaining clone = 2 handles.
        assert_eq!(NlRc::strong_count(&rc), 2);
        drop(iter.next().unwrap());
        assert_eq!(NlRc::strong_count(&rc), 1);
        // `rc` itself drops at end-of-scope; payload destructor runs
        // exactly once (= `u32` is trivial; verified more stringently
        // by `payload_drop_runs_exactly_once' below).
    }

    // ---- Payload drop is exactly-once ----

    #[test]
    fn payload_drop_runs_exactly_once() {
        struct DropProbe(Arc<AtomicUsize>);
        impl Drop for DropProbe {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }
        let drops = Arc::new(AtomicUsize::new(0));
        let probe = DropProbe(drops.clone());
        let rc = NlRc::new(probe);
        let c1 = rc.clone();
        let c2 = rc.clone();
        // 3 handles all alive — payload not dropped yet.
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(c1);
        drop(c2);
        // 1 handle left — payload still alive.
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(rc);
        // Last handle gone — payload destructor must have run once.
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    // ---- Concurrent drop safety ----
    //
    // `NlRc<T>` is NOT `Send` or `Sync` in Phase A.1 (= matches the
    // single-threaded elisp ownership model).  We can't move handles
    // across threads without unsafe, so a true cross-thread test
    // would lie about the public contract.  Instead we exercise the
    // `AtomicUsize` itself: the prompt notes this test is "optional
    // if complicated", so we keep it minimal — confirm the
    // `fetch_sub` + `fetch_add` on the inner refcount field round-
    // trip correctly under repeated single-threaded mutation, which
    // is what the JIT trampoline in Phase A.5 will exercise.

    #[test]
    fn refcount_atomic_round_trip() {
        let rc = NlRc::new(1i64);
        let inner_raw = NlRc::inner_raw(&rc);
        // Hammer the AtomicUsize field directly through the inner
        // ptr (= what elisp `nl-rc-inc' / `nl-rc-dec' will do).  We
        // inc 1000 times then dec 1000 times and confirm we land
        // back at 1.  This validates that the AtomicUsize lives at
        // offset 0 and survives raw-ptr access.
        unsafe {
            for _ in 0..1000 {
                (*inner_raw).refcount.fetch_add(1, Ordering::Relaxed);
            }
            assert_eq!((*inner_raw).refcount.load(Ordering::Acquire), 1001);
            for _ in 0..1000 {
                (*inner_raw).refcount.fetch_sub(1, Ordering::Release);
            }
            assert_eq!((*inner_raw).refcount.load(Ordering::Acquire), 1);
        }
        // `rc` drops normally at end-of-scope; refcount = 1 → 0,
        // payload `i64` is trivial, alloc freed.
    }

    // ---- Send/Sync gating (compile-time assertion) ----
    //
    // We document the *current* Phase A.1 contract: `NlRc<T>` must
    // NOT auto-implement `Send` or `Sync` even when `T: Send + Sync`,
    // because the `*mut`-equivalent inside `NonNull` is not auto-
    // `Send`/`Sync` and `PhantomData<NlRcInner<T>>` would inherit
    // those bounds otherwise.  Phase A.5 (JIT trampolines) will
    // re-evaluate.
    //
    // If the build accidentally makes `NlRc<i64>` `Send`, the
    // following lines fail to compile, surfacing the leak loudly:
    //   fn assert_not_send<T: Send>() {}
    //   assert_not_send::<NlRc<i64>>(); // <- intentional compile error
    //
    // We don't ship that as a runtime test (= it would always fail
    // to compile if uncommented); the doc comment is the contract.

    #[test]
    fn niche_optimization_preserved() {
        // `Option<NlRc<T>>` should be the same size as `NlRc<T>`
        // because `NonNull` has a niche.  This is observable
        // behaviour and a regression here would mean the inner
        // field accidentally went to a nullable type.
        assert_eq!(
            std::mem::size_of::<Option<NlRc<i64>>>(),
            std::mem::size_of::<NlRc<i64>>(),
        );
    }
}
