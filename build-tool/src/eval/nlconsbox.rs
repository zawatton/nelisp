//! Doc 77c Phase A.2.0 — `NlConsBox` layout-pinned cons cell.
//!
//! Specialized self-managed refcounted cons cell with `car` / `cdr`
//! pinned at known byte offsets so the JIT (Phase A.5) and the elisp
//! `nl-cons-*' / `nl-rc-*' primitives (Phase A.3) can reach them
//! without consulting Rust at runtime.
//!
//! Layout (locked by Doc 77c §2.1.2):
//!
//! ```text
//! NlConsBox:  +----------+  offset 0                     (sizeof Sexp)  car
//!             +----------+  offset sizeof(Sexp)          (sizeof Sexp)  cdr
//!             +----------+  offset 2 * sizeof(Sexp)      (8 bytes)      refcount
//!             +----------+
//! ```
//!
//! Why a specialized box (vs `NlRc<(Sexp, Sexp)>`)?
//! - JIT IR can emit `mov rax, qword [rdi + 0]' for `car' and
//!   `mov rax, qword [rdi + sizeof(Sexp)]' for `cdr' without paying
//!   an 8-byte refcount predecessor on every list walk.
//! - The refcount-as-trailer keeps the hottest path (= read-only
//!   `(car ...)' / `(cdr ...)' in list eval) cache-tight.
//! - Phase A.5 trampoline `nl_jit_cons_car' becomes a direct field
//!   load through `addr_of!((*box).car)'.
//!
//! Phase A.1's [`NlRc<T>`](super::nlrc::NlRc) is a *generic* refcounted
//! pointer (refcount @ 0, payload @ 8).  This file introduces the
//! *specialized* counterpart for `Sexp::Cons'.  Both coexist; Phase
//! A.2.1 will migrate `Sexp::Cons(Rc<...>, Rc<...>)' to
//! `Sexp::Cons(NlConsBoxRef)'.
//!
//! Out of scope for Phase A.2.0:
//!   - Migrating `Sexp::Cons' callsites                  (= Phase A.2.1)
//!   - `nl-cons-*' / `nl-rc-*' elisp primitives          (= Phase A.3)
//!   - Cranelift trampoline rewrites                     (= Phase A.5)
//!
//! Threading: `AtomicUsize` mirrors the `NlRc<T>' rationale — the
//! eventual JIT/ffi paths may touch the refcount from any thread that
//! holds a handle.  `NlConsBoxRef' is intentionally NOT `Send` /
//! `Sync` for now; Phase A.5 will re-evaluate.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned cons cell.  Heap-allocated, refcounted via an
/// `AtomicUsize` trailer.  Accessed through [`NlConsBoxRef`] handles.
///
/// `#[repr(C)]` is mandatory: Phase A.5 JIT will read `car` / `cdr`
/// at known byte offsets, and Phase A.3 elisp `nl-rc-inc' /
/// `nl-rc-dec' / `nl-rc-count' primitives will touch the refcount
/// trailer through the same offset contract.
#[repr(C)]
pub struct NlConsBox {
    /// First half of the cons pair.  Offset 0 is the JIT contract.
    pub car: Sexp,
    /// Second half of the cons pair.  Offset = `size_of::<Sexp>()`
    /// by `repr(C)' layout rules.
    pub cdr: Sexp,
    /// Strong reference count.  Starts at 1 in [`NlConsBoxRef::new`],
    /// +1 on each `Clone`, -1 on each `Drop`.  When it reaches 0 the
    /// last handle drops the payload and frees the allocation.
    pub refcount: AtomicUsize,
}

impl NlConsBox {
    /// Doc 79 v4 Stage C.4-atomic in-place destructor (= `NLRC_DROP_TABLE`).
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlConsBox>;

    /// Mutate `car' in place through a raw `&NlConsBox' borrow.  Used by
    /// the Phase A.5.2 cons trampolines after they resolve `Sexp::Cons'
    /// to `*const NlConsBox' via [`Sexp::cons_box_ptr`].  See
    /// [`NlConsBoxRef::set_car`] for the higher-level API + safety
    /// contract — they share the same drop-then-write semantics.
    ///
    /// # Safety
    ///
    /// Same contract as [`NlConsBoxRef::set_car`]: caller must guarantee
    /// no other `&Sexp' borrow into `self.car' is live for the duration
    /// of the call.
    #[inline]
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of!(self.car) as *mut Sexp;
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// Mutate `cdr' in place.  See [`NlConsBox::set_car`].
    ///
    /// # Safety
    ///
    /// Same contract as [`NlConsBox::set_car`].
    #[inline]
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of!(self.cdr) as *mut Sexp;
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }
}

/// Refcounted handle to an [`NlConsBox`].  API parity with
/// [`NlRc<T>`](super::nlrc::NlRc): `new` / `Clone` / `Drop` / `Deref`
/// (returns `&NlConsBox`).
///
/// The `NonNull<NlConsBox>' inner gives niche optimization
/// (= `Option<NlConsBoxRef>' is the same size as `NlConsBoxRef') and
/// rules out null-ptr UB by construction.
///
/// Phase A.5.1 (Doc 77c §4.6.1, 2026-05-09): pinned to
/// `#[repr(transparent)]' so the on-disk layout matches `NonNull<NlConsBox>'
/// exactly.  This is the load-bearing invariant for `Sexp::cons_box_ptr',
/// which reads the box pointer at offset `SEXP_PAYLOAD_OFFSET' of the
/// outer `Sexp' enum.
#[repr(transparent)]
pub struct NlConsBoxRef {
    ptr: NonNull<NlConsBox>,
    /// Tells the borrow-checker we own an `NlConsBox` even though the
    /// field is `NonNull<...>'.  Mirrors `std::rc::Rc' /
    /// `super::nlrc::NlRc'.
    _marker: PhantomData<NlConsBox>,
}

impl NlConsBoxRef {
    /// Allocate a fresh [`NlConsBox`] on the heap with `refcount = 1`
    /// and return the unique handle.  The supplied `car` / `cdr` are
    /// moved into the box.
    ///
    /// Panics on allocation failure (= matches `NlRc::new`, which
    /// calls `alloc::handle_alloc_error' internally).
    pub fn new(car: Sexp, cdr: Sexp) -> NlConsBoxRef {
        let layout = Layout::new::<NlConsBox>();
        // SAFETY: `Layout::new::<NlConsBox>()' is non-zero-sized
        // because the struct contains at least the 8-byte refcount
        // (and two `Sexp' fields besides).
        let raw = unsafe { alloc::alloc(layout) } as *mut NlConsBox;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlConsBox' and is
        // exclusively owned here.  We initialize all three fields
        // before anyone else can observe the box.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).car), car);
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).cdr), cdr);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlConsBoxRef {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Read the current strong-reference count.  Mirrors
    /// `NlRc::strong_count'; primarily useful for tests and the elisp
    /// `nl-rc-count' primitive (Phase A.3).
    ///
    /// Uses `Acquire` ordering so a caller observing the returned
    /// value can also rely on having seen all writes published via
    /// the matching `Release` decrement.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the *underlying box*.  Two clones of the
    /// same [`NlConsBoxRef::new`] invocation are pointer-equal; two
    /// distinct allocations are not.
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Raw pointer to the box, for ffi shims.  Phase A.3 elisp
    /// `nl-cons-car' / `nl-rc-inc' will use this through
    /// `nl-ffi-write-i64' / `nl-ffi-read-i64'.
    #[inline]
    pub fn as_ptr(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }

    /// Phase A.3: bump the refcount **without acquiring a Rust handle**.
    /// Backs the elisp `nl-rc-inc' primitive.  Layer 2 elisp takes
    /// responsibility for matching every `nl-rc-inc' with exactly one
    /// `nl-rc-dec' — typical use is keeping the box alive while a raw
    /// pointer / opaque handle is stashed in an elisp data structure
    /// outside of Rust ownership tracking.
    ///
    /// `Relaxed` is the same ordering [`NlConsBoxRef::clone`] uses for
    /// the corresponding +1; the synchronization story matches.
    ///
    /// # Safety
    ///
    /// The caller must guarantee a matching [`Self::rc_dec_raw`]
    /// follows.  Unbalanced calls leak (extra inc) or use-after-free
    /// (extra dec).
    pub unsafe fn rc_inc_raw(this: &Self) {
        // SAFETY: `this.ptr' is alive because the caller holds the
        // handle backing `this'.  fetch_add cannot wrap (= would need
        // 2^64 handles, physically impossible).
        unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Phase A.3: decrement the refcount, freeing the box when it
    /// reaches zero.  Backs the elisp `nl-rc-dec' primitive.  Mirrors
    /// the body of [`NlConsBoxRef::drop`] but takes `&Self` instead of
    /// `&mut Self` so it can be invoked from a primitive that received
    /// the cons through `args: &[Sexp]'.
    ///
    /// # Safety
    ///
    /// (a) Must be paired with a prior [`Self::rc_inc_raw`] (or be the
    ///     final balancing decrement of a normal handle's lifecycle).
    /// (b) If the resulting refcount is 0 the box's `car' / `cdr' are
    ///     dropped and the allocation is freed; any other handle
    ///     pointing at this box is then dangling.  Layer 2 elisp must
    ///     guarantee no such handle survives.
    pub unsafe fn rc_dec_raw(this: &Self) {
        // SAFETY: `this.ptr' is alive on entry.  The Release/Acquire
        // pattern matches `Drop' below — we cannot reuse `Drop' here
        // because we have `&Self', not `&mut Self', and re-creating a
        // temporary `NlConsBoxRef' for the sole purpose of dropping
        // would re-enter `Clone' on the way in (= +1) and Drop on the
        // way out (= -1), leaving the count unchanged.
        let prev = unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0.  See `Drop' for the same
        // invariants.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!(
                (*this.ptr.as_ptr()).car
            ));
            std::ptr::drop_in_place(std::ptr::addr_of_mut!(
                (*this.ptr.as_ptr()).cdr
            ));
            let layout = Layout::new::<NlConsBox>();
            alloc::dealloc(this.ptr.as_ptr() as *mut u8, layout);
        }
    }

    /// Mutate `car` in place.  Drops the previous `car' value, then
    /// writes the new one.  Phase A.2.0 deliberately drops the
    /// `RefCell<>' indirection that the legacy
    /// `Sexp::Cons(Rc<RefCell<Sexp>>, ..)' used for `setcar' / `setcdr'
    /// aliasing — Phase A.2.1 callsites observe mutations through any
    /// clone of the same `NlConsBoxRef' (= shared box semantics).
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&Sexp` borrow into this box's
    /// `car` field is live at the time of the write.  In Phase A.2.1
    /// the `setcar' bridge will document the surrounding invariants;
    /// in Phase A.2.0 the only callers are this module's tests.
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).car);
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// Mutate `cdr` in place.  See [`NlConsBoxRef::set_car`] for the
    /// safety contract.
    ///
    /// # Safety
    ///
    /// Same as [`NlConsBoxRef::set_car`].
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).cdr);
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }

    /// Internal helper — returns a raw `*const NlConsBox' for callers
    /// that need to walk the box (= future ffi shims).  Hidden from
    /// the public Phase A.2.0 surface; expose via a dedicated module
    /// API in Phase A.3.
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn inner_raw(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }
}

impl Clone for NlConsBoxRef {
    /// Bump the refcount and return a new handle that shares the
    /// same inner box.  Uses `Relaxed` because this thread already
    /// holds a handle — no cross-thread synchronization is needed
    /// for the *increment* (= matches `NlRc::clone' /
    /// `std::sync::Arc::clone').
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.  The
        // increment cannot wrap because `usize::MAX' handles is
        // physically impossible (= would exhaust 16 EiB of address
        // space at 1 byte per handle).
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlConsBoxRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlConsBoxRef {
    fn drop(&mut self) { unsafe { crate::nlrc_drop_box!(self.ptr.as_ptr(), NlConsBox, crate::eval::sexp::SEXP_TAG_CONS); } }
}

impl Deref for NlConsBoxRef {
    type Target = NlConsBox;

    /// Borrow the box.  `car' / `cdr' / `refcount' live at fixed
    /// offsets inside `NlConsBox' (asserted at compile time below),
    /// so this is a single load + offset.
    fn deref(&self) -> &NlConsBox {
        // SAFETY: `self.ptr' is alive because we hold a handle, and
        // the box's fields were initialized in `NlConsBoxRef::new' and
        // are only torn down by the last `Drop' (= when no handle
        // exists to call `deref').  The reference borrows `self', so
        // it cannot outlive the handle.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlConsBoxRef {
    /// Forward to the inner box so `Sexp::Cons' debug output keeps
    /// the legacy `Cons(<car>, <cdr>)' shape downstream consumers
    /// (= ERT panics, log lines) already expect.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Cons").field(&self.car).field(&self.cdr).finish()
    }
}

impl PartialEq for NlConsBoxRef {
    /// Structural equality — matches the legacy
    /// `Rc<RefCell<Sexp>> == Rc<RefCell<Sexp>>' derive that compared
    /// inner Sexps recursively.  We add a `ptr_eq' fast path *first*
    /// so a Sexp value compared to itself short-circuits without
    /// cycling — the legacy derived `Cons' arm could hang on
    /// `(let ((x (cons 1 nil))) (setcdr x x) (equal x x))' style
    /// inputs because `Rc<RefCell<>>' derive recursed unconditionally
    /// (only `sexp_equal_safe' had the bounded recursion guard).
    /// `ptr_eq' converts that pathology into a constant-time return
    /// for the self-compare case while still recursing for distinct
    /// allocations with the same logical shape.
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.car == other.car && self.cdr == other.cdr
    }
}

// ---- Compile-time layout assertions ----
//
// These guarantee that the JIT (Phase A.5) and the elisp `nl-cons-*' /
// `nl-rc-*' primitives (Phase A.3) can reach `car' / `cdr' / `refcount'
// at known byte offsets without consulting Rust at runtime.
//
// `car @ 0' is locked by `repr(C)' (= first field).
// `cdr @ size_of::<Sexp>()' is locked by `repr(C)' field ordering.
// `refcount @ 2 * size_of::<Sexp>()' is locked by the same rule.
// Any future struct reorder fails the build here.

const _: () = {
    // `AtomicUsize` is 8 bytes on x86_64 / aarch64; this assert
    // catches any 32-bit target accidentally pulled into the build.
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    // `car` is the first field of a `repr(C)' struct.
    assert!(std::mem::offset_of!(NlConsBox, car) == 0);
    // `cdr` follows `car' with `Sexp' alignment (= 8 on x86_64 /
    // aarch64).  `size_of::<Sexp>()' is the canonical offset.
    assert!(std::mem::offset_of!(NlConsBox, cdr) == std::mem::size_of::<Sexp>());
    // `refcount' follows `cdr' at the trailer position.
    assert!(
        std::mem::offset_of!(NlConsBox, refcount) == 2 * std::mem::size_of::<Sexp>()
    );
};

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    // ---- Layout invariants (= Doc 77c §2.1.2 contract) ----

    #[test]
    fn layout_car_at_offset_0() {
        assert_eq!(std::mem::offset_of!(NlConsBox, car), 0);
    }

    #[test]
    fn layout_cdr_after_car() {
        assert_eq!(
            std::mem::offset_of!(NlConsBox, cdr),
            std::mem::size_of::<Sexp>()
        );
    }

    #[test]
    fn layout_refcount_at_trailer() {
        assert_eq!(
            std::mem::offset_of!(NlConsBox, refcount),
            2 * std::mem::size_of::<Sexp>()
        );
    }

    #[test]
    fn layout_atomic_usize_is_8_bytes() {
        // 32-bit targets would break the JIT contract; this test
        // fails loudly on those.
        assert_eq!(std::mem::size_of::<AtomicUsize>(), 8);
    }

    // ---- Basic refcount semantics ----

    #[test]
    fn new_starts_with_refcount_1() {
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
        assert_eq!(NlConsBoxRef::strong_count(&r), 1);
    }

    #[test]
    fn clone_increments_refcount() {
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
        let _c1 = r.clone();
        let _c2 = r.clone();
        assert_eq!(NlConsBoxRef::strong_count(&r), 3);
    }

    #[test]
    fn drop_decrements_refcount() {
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
        let c1 = r.clone();
        let c2 = r.clone();
        assert_eq!(NlConsBoxRef::strong_count(&r), 3);
        drop(c1);
        assert_eq!(NlConsBoxRef::strong_count(&r), 2);
        drop(c2);
        assert_eq!(NlConsBoxRef::strong_count(&r), 1);
    }

    #[test]
    fn ptr_eq_clones_share_box() {
        let a = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
        let b = a.clone();
        assert!(NlConsBoxRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_distinct_allocations() {
        let a = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
        // Same logical value, different alloc — not pointer-equal.
        let b = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
        assert!(!NlConsBoxRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlConsBoxRef::new(Sexp::Int(99), Sexp::Nil);
        let b = a.clone();
        let c = a.clone();
        drop(b);
        assert!(NlConsBoxRef::ptr_eq(&a, &c));
    }

    // ---- Deref / field access ----

    #[test]
    fn deref_reads_car_cdr() {
        let r = NlConsBoxRef::new(Sexp::Int(42), Sexp::Symbol("x".into()));
        match (&r.car, &r.cdr) {
            (Sexp::Int(42), Sexp::Symbol(s)) => assert_eq!(s, "x"),
            (c, d) => panic!("unexpected ({:?} . {:?})", c, d),
        }
    }

    #[test]
    fn deref_through_clone_sees_same_payload() {
        let r = NlConsBoxRef::new(Sexp::Int(7), Sexp::Nil);
        let c = r.clone();
        assert!(matches!(&c.car, Sexp::Int(7)));
        assert!(matches!(&c.cdr, Sexp::Nil));
    }

    // ---- set_car / set_cdr ----

    #[test]
    fn set_car_overwrites_in_place() {
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
        unsafe { r.set_car(Sexp::Int(99)) };
        assert!(matches!(&r.car, Sexp::Int(99)));
    }

    #[test]
    fn set_cdr_overwrites_in_place() {
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
        unsafe { r.set_cdr(Sexp::T) };
        assert!(matches!(&r.cdr, Sexp::T));
    }

    #[test]
    fn set_car_visible_through_clone() {
        // Phase A.2.1's `setcar' contract requires that mutation
        // through any handle is observed by every clone — this is
        // the cons-cell-identity guarantee that the legacy
        // `Rc<RefCell<Sexp>>' provided.
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Nil);
        let c = r.clone();
        unsafe { r.set_car(Sexp::Int(42)) };
        assert!(matches!(&c.car, Sexp::Int(42)));
    }

    // ---- Lifecycle: payload destructor exactly-once ----

    #[test]
    fn payload_drop_runs_exactly_once() {
        // We probe via `Sexp::Vector(NlVectorRef)' — the inner
        // strong_count lets us observe the round-trip:
        //   1 (probe) + 1 (moved into box) = 2
        //   after drop(box) → 1
        // This catches both 'destructor never ran' (= leak) and
        // 'destructor ran twice' (= UB; would underflow refcount to
        // panic on second drop).  Phase A.4.3 (2026-05-09): switched
        // from legacy `Rc<RefCell<Vec<Sexp>>>' probe to NlVectorRef
        // since the Rc shape no longer exists for `Sexp::Vector'.
        use crate::eval::nlvector::NlVectorRef;
        let probe = NlVectorRef::new(vec![Sexp::Int(1)]);
        let payload = Sexp::Vector(probe.clone());
        let r = NlConsBoxRef::new(payload, Sexp::Nil);
        let c1 = r.clone();
        let c2 = r.clone();
        // 3 handles all alive — payload NlVectorRef still bumped by 1
        // (the moved-in `Sexp::Vector' holds it; clones share the box).
        assert_eq!(NlVectorRef::strong_count(&probe), 2);
        drop(c1);
        drop(c2);
        // 1 handle left — payload NlVectorRef still bumped.
        assert_eq!(NlVectorRef::strong_count(&probe), 2);
        drop(r);
        // Last handle gone — payload destructor must have run once,
        // dropping the Sexp::Vector and decrementing the inner refcount.
        assert_eq!(NlVectorRef::strong_count(&probe), 1);
    }

    #[test]
    fn multi_clone_drop() {
        let r = NlConsBoxRef::new(Sexp::Int(0xABCD), Sexp::Nil);
        let clones: Vec<NlConsBoxRef> = (0..5).map(|_| r.clone()).collect();
        // 1 original + 5 clones = 6 handles.
        assert_eq!(NlConsBoxRef::strong_count(&r), 6);
        let mut iter = clones.into_iter();
        for _ in 0..4 {
            drop(iter.next().unwrap());
        }
        // 1 original + 1 remaining clone = 2 handles.
        assert_eq!(NlConsBoxRef::strong_count(&r), 2);
        drop(iter.next().unwrap());
        assert_eq!(NlConsBoxRef::strong_count(&r), 1);
        // `r' itself drops at end-of-scope; payload destructor runs
        // exactly once (`payload_drop_runs_exactly_once' covers the
        // observable side of that path explicitly).
    }

    // ---- Niche optimization ----

    #[test]
    fn niche_optimization_preserved() {
        // `Option<NlConsBoxRef>' should be the same size as
        // `NlConsBoxRef' because `NonNull' has a niche.  A regression
        // here would mean the inner field accidentally went to a
        // nullable type.
        assert_eq!(
            std::mem::size_of::<Option<NlConsBoxRef>>(),
            std::mem::size_of::<NlConsBoxRef>(),
        );
    }

    // ---- Raw ptr round-trip (Phase A.3 ffi prep) ----

    #[test]
    fn as_ptr_round_trips_to_field_access() {
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
        let raw = NlConsBoxRef::as_ptr(&r);
        // SAFETY: `r' is alive for the entire test; `raw' is a
        // borrowed pointer.
        unsafe {
            assert!(matches!((*raw).car, Sexp::Int(1)));
            assert!(matches!((*raw).cdr, Sexp::Int(2)));
            assert_eq!((*raw).refcount.load(Ordering::Acquire), 1);
        }
    }

    #[test]
    fn refcount_atomic_round_trip() {
        // Hammer the AtomicUsize trailer directly through the inner
        // ptr (= what elisp `nl-rc-inc' / `nl-rc-dec' will do via
        // `nl-ffi-write-i64' on the `refcount' offset).  Inc 1000
        // times then dec 1000 times and confirm we land back at 1.
        // This validates that the AtomicUsize lives at the trailer
        // offset and survives raw-ptr access.
        let r = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
        let inner_raw = NlConsBoxRef::inner_raw(&r);
        // SAFETY: `r' is alive for the entire test.
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
        // `r' drops normally; refcount = 1 → 0, payload Sexp::Int
        // values are POD, alloc freed.
    }

    // ---- Sexp::Cons compatibility (= Phase A.2.1 prep) ----

    #[test]
    fn nested_cons_payload_via_sexp_cons() {
        // Nested cons: car of the outer `NlConsBoxRef' is a Sexp
        // whose `Sexp::Cons' variant wraps a *different*
        // `NlConsBoxRef'.  Phase A.2.1 unified the cons variant on
        // `NlConsBoxRef', so the inner box is the same self-managed
        // type as the outer.
        let inner = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let r = NlConsBoxRef::new(inner, Sexp::Nil);
        match &r.car {
            Sexp::Cons(b) => {
                assert!(matches!(b.car, Sexp::Int(1)));
                assert!(matches!(b.cdr, Sexp::Int(2)));
            }
            other => panic!("expected nested Sexp::Cons, got {:?}", other),
        }
    }
}
