//! `NlRc<T>` — self-managed refcounted ptr.  `Rc<T>'-equivalent API
//! with a layout-pinned `#[repr(C)]' inner: refcount @ offset 0
//! (8 bytes AtomicUsize), value @ offset 8.  Offsets are asserted
//! at compile-time + runtime so elisp + Phase 47 helpers can reach
//! the refcount via fixed-offset arithmetic.  Not `Send` / `Sync'.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlRcInner<T> {
    pub refcount: AtomicUsize,
    pub value: T,
}

pub struct NlRc<T> {
    ptr: NonNull<NlRcInner<T>>,
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

// NLRC_DROP_TABLE + nlrc_drop_box! — per-tag drop dispatch.  SEXP_TAG_*
// slots 0..=5 (unboxed) panic; slots 6..=12 forward to each box's DROP_FN.

/// Generic in-place drop helper — runs `T`'s destructor without freeing
/// the heap slot.  The `nlrc_drop_box!` macro pairs each invocation
/// with exactly one `dealloc`.
///
/// # Safety
/// `ptr` must point at a fully-initialized `T` whose backing alloc the
/// caller is about to free.
pub unsafe fn nlrc_payload_drop<T>(ptr: *mut std::ffi::c_void) {
    std::ptr::drop_in_place(ptr as *mut T);
}

/// Panic stub for unboxed `SEXP_TAG_*` slots (NIL/T/INT/FLOAT/SYMBOL/STR).
unsafe fn nl_rc_unboxed_drop_panic(_ptr: *mut std::ffi::c_void) {
    panic!("NLRC_DROP_TABLE: unboxed Sexp tag dispatched (= tag corruption?)");
}

// Doc 124 §124.L — per-type inner-drop ABI externs for the elisp Drop
// kernels.  Each `nl_<type>_drop_inner' wraps the per-type
// `nlrc_payload_drop::<NlT>' (= `std::ptr::drop_in_place::<NlT>') so
// the elisp `nl<type>-drop' kernels can call it via `extern-call'
// before the matching `dealloc-bytes' op.  Mirrors the recursive
// payload-drop step of `nlrc_drop_box!' (= the
// `NLRC_DROP_TABLE[tag](raw)' line) but exposed as a name-stable
// `extern "C"' symbol so the elisp emitter can emit a PLT call.
//
// # Safety
//
// `box_ptr' must point at a fully-initialized `NlT' whose backing
// allocation the caller is about to free.  Each function takes
// `*mut i64' to match the elisp kernel's `box-ptr' arg type and
// reinterprets through the typed `nlrc_payload_drop::<T>'.

/// §124.L NlConsBox inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlConsBox' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlconsbox::NlConsBox::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlVector inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlVector' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_vector_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlvector::NlVector::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlCell inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlCell' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_cell_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlcell::NlCell::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlRecord inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlRecord' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_record_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlrecord::NlRecord::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L NlStr inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlStr' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_str_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlstr::NlStr::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L+ NlBoolVector inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlBoolVector' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_boolvector_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlboolvector::NlBoolVector::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// §124.L+ NlCharTable inner-drop ABI extern.
///
/// # Safety
/// `box_ptr' must point at a fully-initialized `NlCharTable' whose
/// backing alloc the caller is about to free.
#[no_mangle]
pub unsafe extern "C" fn nl_chartable_drop_inner(box_ptr: *mut i64) -> i64 {
    crate::eval::nlchartable::NlCharTable::DROP_FN(box_ptr as *mut std::ffi::c_void);
    1
}

/// Per-tag drop dispatch.  Indexed by `SEXP_TAG_*`; slots 0..=5 panic,
/// slots 6..=12 forward to each box's `DROP_FN`.
pub const NLRC_DROP_TABLE: [unsafe fn(*mut std::ffi::c_void); 13] = [
    nl_rc_unboxed_drop_panic,                          // 0 NIL
    nl_rc_unboxed_drop_panic,                          // 1 T
    nl_rc_unboxed_drop_panic,                          // 2 INT
    nl_rc_unboxed_drop_panic,                          // 3 FLOAT
    nl_rc_unboxed_drop_panic,                          // 4 SYMBOL
    nl_rc_unboxed_drop_panic,                          // 5 STR (immutable)
    crate::eval::nlstr::NlStr::DROP_FN,                // 6 MUT_STR
    crate::eval::nlconsbox::NlConsBox::DROP_FN,        // 7 CONS
    crate::eval::nlvector::NlVector::DROP_FN,          // 8 VECTOR
    crate::eval::nlchartable::NlCharTable::DROP_FN,    // 9 CHAR_TABLE
    crate::eval::nlboolvector::NlBoolVector::DROP_FN,  // 10 BOOL_VECTOR
    crate::eval::nlcell::NlCell::DROP_FN,              // 11 CELL
    crate::eval::nlrecord::NlRecord::DROP_FN,          // 12 RECORD
];

/// Slim Drop dispatch.  Expansion: `fetch_sub(Release)` → if was 1,
/// `fence(Acquire)` → `NLRC_DROP_TABLE[tag](ptr)` → `dealloc(layout)`.
/// Refcount access via `(*typed_ptr).refcount` so each box's trailer
/// offset resolves correctly per `repr(C)`.
///
/// # Safety
/// Caller must pass a typed `*mut $T` reachable from a live handle.
#[macro_export]
macro_rules! nlrc_drop_box {
    ($ptr:expr, $T:ty, $tag:expr) => {{
        let raw: *mut $T = $ptr;
        let prev = (*raw).refcount.fetch_sub(1, ::std::sync::atomic::Ordering::Release);
        if prev == 1 {
            ::std::sync::atomic::fence(::std::sync::atomic::Ordering::Acquire);
            $crate::eval::nlrc::NLRC_DROP_TABLE[$tag as usize](raw as *mut ::std::ffi::c_void);
            ::std::alloc::dealloc(raw as *mut u8, ::std::alloc::Layout::new::<$T>());
        }
    }};
}

// Compile-time layout assertions — refcount @ 0, value @ 8 for i64 canary.
const _: () = {
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    assert!(std::mem::offset_of!(NlRcInner<i64>, refcount) == 0);
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
