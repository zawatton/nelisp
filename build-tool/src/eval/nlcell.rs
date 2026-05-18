//! Doc 77c Phase A.4 — `NlCell` layout-pinned single-slot mutable cell.
//!
//! Specialized self-managed refcounted cell carrying one mutable
//! `Sexp` slot.  Replaces the legacy `Sexp::Cell(Rc<RefCell<Sexp>>)' /
//! `FrameCell = Rc<RefCell<Sexp>>' pair with a layout-pinned struct so:
//!
//! 1. The JIT can later (Phase A.5 follow-up) emit `mov rax, qword
//!    [rdi + 0]' to read the cell's value through `addr_of!`.
//! 2. Phase B elisp self-host can reach the slot at known offsets
//!    via the same mechanism `nl-cons-*' uses for cons cells.
//! 3. The `RefCell' borrow-flag overhead drops out — eval-time access
//!    is a single load instead of `borrow().clone()'.
//!
//! Layout (Phase A.4 locked):
//!
//! ```text
//! NlCell:  +----------+  offset 0                  (sizeof Sexp)  value
//!          +----------+  offset sizeof(Sexp)       (8 bytes)      refcount
//!          +----------+
//! ```
//!
//! The single-`Sexp' shape mirrors [`super::nlconsbox::NlConsBox`]'s
//! `(car, cdr, refcount)' triple but with one slot instead of two.
//! [`NlRc<T>`](super::nlrc::NlRc) is the generic counterpart with
//! `(refcount, value)' ordering — `NlCell' deliberately puts the
//! value first so eval / closure access doesn't pay the 8-byte
//! refcount predecessor.
//!
//! Mutability: like `NlConsBox::set_car' / `set_cdr', mutation is
//! through `unsafe fn set_value'.  Callers (= eval frame setq path,
//! closure capture) take responsibility for not holding `&Sexp'
//! borrows into the slot at the time of the write — the same
//! discipline `setcar' relies on (Phase A.2.1).
//!
//! Out of scope for Phase A.4 Cell pilot:
//!   - Other variants (= MutStr / Vector / CharTable / BoolVector /
//!     Record) — separate sub-stages
//!   - Cranelift JIT trampoline using `addr_of!' — Phase A.5
//!
//! Threading: `AtomicUsize` mirrors `NlRc' / `NlConsBox' rationale.
//! Not `Send` / `Sync`.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned single-slot cell.  Heap-allocated, refcounted via an
/// `AtomicUsize` trailer.  Accessed through [`NlCellRef`] handles.
///
/// `#[repr(C)]` is mandatory: Phase A.5 / Phase B elisp will read
/// `value` at offset 0 and (eventually) the refcount trailer at the
/// next 8-byte aligned offset without consulting Rust.
#[repr(C)]
pub struct NlCell {
    /// The mutable slot.  Offset 0 is the JIT contract.
    pub value: Sexp,
    /// Strong reference count.  Starts at 1 in [`NlCellRef::new`],
    /// +1 on each `Clone`, -1 on each `Drop`.  When it reaches 0 the
    /// last handle drops `value' and frees the allocation.
    pub refcount: AtomicUsize,
}

/// Refcounted handle to an [`NlCell`].  API parity with
/// [`super::nlconsbox::NlConsBoxRef`]: `new` / `Clone` / `Drop` /
/// `Deref` (returns `&NlCell`).
///
/// The `NonNull<NlCell>' inner gives niche optimization
/// (= `Option<NlCellRef>' is the same size as `NlCellRef') and
/// rules out null-ptr UB by construction.
///
/// Phase A.5.1 (Doc 77c §4.6.1, 2026-05-09): `#[repr(transparent)]'
/// pins the layout to `NonNull<NlCell>' so JIT trampolines + Phase B
/// elisp can read the cell pointer directly off the `Sexp' payload at
/// offset `SEXP_PAYLOAD_OFFSET'.
#[repr(transparent)]
pub struct NlCellRef {
    ptr: NonNull<NlCell>,
    /// Tells the borrow-checker we own an `NlCell` even though the
    /// field is `NonNull<...>'.  Mirrors `std::rc::Rc' /
    /// `super::nlrc::NlRc'.
    _marker: PhantomData<NlCell>,
}

impl NlCell { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlCell>; } // Doc 79 v4 C.4-atomic

/// Doc 111 §111.D — allocator helper for Phase 47-compiled elisp.
/// Returns a freshly-allocated `NlCell` initialized with `value =
/// (*initial).clone()` (refcount-aware) and `refcount = 1`.  Caller
/// is responsible for wrapping the returned pointer into a
/// `Sexp::Cell(_)` whose `NlCellRef::drop` decrements the refcount
/// (standard ownership transfer).
///
/// Mirrors `nl_alloc_consbox` (Doc 101 §101.D) in shape and
/// `NlCellRef::new` in semantics — except `new` *moves* its `Sexp`
/// argument in by value, which doesn't survive the
/// `extern "C"` boundary because `Sexp` is not `#[repr(C)]`-friendly
/// to pass by value.  So we take a pointer and `clone()` it
/// internally; this also matches `nl_consbox_set_car`'s
/// `(*val).clone()` convention.
///
/// # Safety
/// - `initial` must be non-null and point at an initialized `Sexp`.
/// - No other `&Sexp` borrow into `*initial` may be mutated for the
///   duration of the call.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_cell(initial: *const Sexp) -> *mut NlCell {
    let value = unsafe { (*initial).clone() };
    let layout = Layout::new::<NlCell>();
    // SAFETY: `Layout::new::<NlCell>()' is non-zero-sized.
    let raw = unsafe { alloc::alloc(layout) } as *mut NlCell;
    if raw.is_null() {
        alloc::handle_alloc_error(layout);
    }
    // SAFETY: `raw' was just allocated for `NlCell' and is exclusively
    // owned here.  Initialize both fields before anyone observes the
    // cell.
    unsafe {
        std::ptr::write(std::ptr::addr_of_mut!((*raw).value), value);
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).refcount),
            AtomicUsize::new(1),
        );
    }
    raw
}

/// Doc 111 §111.D — extern wrapper for Phase 47 `(cell-set-value H VAL)'.
/// Wraps the existing `NlCellRef::set_value' unsafe method which does
/// `std::ptr::drop_in_place' + `std::ptr::write' on the inner NlCell's
/// `value' field.  Refcount maintenance is handled automatically by
/// `Sexp::Drop' (= old value's drop) and `Sexp::Clone' (= refcount-
/// aware clone of `*val' before the write).
///
/// # Safety
/// - `cell' must be non-null and point at an initialized `NlCell'.
/// - `val' must be non-null and point at an initialized `Sexp'.
/// - No other `&Sexp' borrow into `cell.value' may be live.
#[no_mangle]
pub unsafe extern "C" fn nl_cell_set_value(cell: *mut NlCell, val: *const Sexp) {
    let val_owned = unsafe { (*val).clone() };
    let value_ptr = std::ptr::addr_of_mut!((*cell).value);
    unsafe {
        std::ptr::drop_in_place(value_ptr);
        std::ptr::write(value_ptr, val_owned);
    }
}
impl NlCellRef {
    /// Allocate a fresh [`NlCell`] on the heap with `refcount = 1`
    /// and return the unique handle.  The supplied `value` is moved
    /// into the cell.
    ///
    /// Panics on allocation failure (= matches `NlConsBoxRef::new`,
    /// which calls `alloc::handle_alloc_error' internally).
    pub fn new(value: Sexp) -> NlCellRef {
        let layout = Layout::new::<NlCell>();
        // SAFETY: `Layout::new::<NlCell>()' is non-zero-sized — at
        // minimum the `Sexp' value + 8-byte refcount.
        let raw = unsafe { alloc::alloc(layout) } as *mut NlCell;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlCell' and is
        // exclusively owned here.  We initialize both fields before
        // anyone else can observe the cell.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlCellRef {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Read the current strong-reference count.  Mirrors
    /// `NlConsBoxRef::strong_count' / `NlRc::strong_count'.
    ///
    /// Uses `Acquire` ordering so a caller observing the returned
    /// value can also rely on having seen all writes published via
    /// the matching `Release` decrement.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the *underlying cell*.  Two clones of the
    /// same [`NlCellRef::new`] invocation are pointer-equal; two
    /// distinct allocations are not.
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Mutate `value` in place.  Drops the previous `value`, then
    /// writes the new one.  Replaces the legacy
    /// `*FrameCell.borrow_mut() = new' pattern; behaves identically
    /// from the callers' perspective (= setq-write-through that any
    /// clone of this handle observes).
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&Sexp` borrow into this
    /// cell's `value` field is live at the time of the write.  In
    /// the env-frame setq path the `args' slice has already been
    /// evaluated to owned `Sexp's by the time we reach `set_value';
    /// no aliasing borrows into the frame slot exist.
    pub unsafe fn set_value(&self, val: Sexp) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        // SAFETY: see method-level contract.
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }
}

impl Clone for NlCellRef {
    /// Doc 124 §124.F — refcount +1 dispatched to the Phase 47-compiled
    /// `nelisp_nlcell_clone' kernel (= §122.E `atomic-fetch-add' delta=+1
    /// at REFCOUNT_OFFSET=32).
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlcell_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlCellRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlCellRef {
    fn drop(&mut self) { unsafe { crate::nlrc_drop_box!(self.ptr.as_ptr(), NlCell, crate::eval::sexp::SEXP_TAG_CELL); } }
}

impl Deref for NlCellRef {
    type Target = NlCell;

    /// Borrow the cell.  `value' / `refcount' live at fixed offsets
    /// (asserted at compile time below), so this is a single load +
    /// offset.
    fn deref(&self) -> &NlCell {
        // SAFETY: see `NlConsBoxRef::deref' for the same invariant.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlCellRef {
    /// Forward to the inner cell so `Sexp::Cell' debug output keeps
    /// the legacy `Cell(<value>)' shape.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Cell").field(&self.value).finish()
    }
}

impl PartialEq for NlCellRef {
    /// Structural equality — matches the legacy
    /// `Rc<RefCell<Sexp>> == Rc<RefCell<Sexp>>' derive that
    /// compared inner Sexps recursively.  Adds a `ptr_eq' fast path
    /// first so a self-compare short-circuits without recursing
    /// (matches `NlConsBoxRef::eq' rationale).
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.value == other.value
    }
}

// ---- Compile-time layout assertions ----
//
// These guarantee Phase A.5 JIT and Phase B elisp self-host can reach
// `value' / `refcount' at known byte offsets without consulting Rust
// at runtime.

const _: () = {
    use std::mem::{offset_of, size_of};
    // value @ offset 0 — same JIT contract as `NlConsBox.car'.
    assert!(offset_of!(NlCell, value) == 0);
    // refcount @ offset sizeof(Sexp) — `repr(C)' linear layout.
    assert!(offset_of!(NlCell, refcount) == size_of::<Sexp>());
    // refcount is 8 bytes on every supported arch (= matches
    // AtomicUsize on x86_64 + aarch64).
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_value_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlCell, value), 0);
    }

    #[test]
    fn layout_refcount_after_value() {
        use std::mem::{offset_of, size_of};
        assert_eq!(offset_of!(NlCell, refcount), size_of::<Sexp>());
    }

    #[test]
    fn layout_refcount_size_8() {
        use std::mem::size_of;
        assert_eq!(size_of::<AtomicUsize>(), 8);
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let c = NlCellRef::new(Sexp::Int(42));
        assert_eq!(NlCellRef::strong_count(&c), 1);
    }

    #[test]
    fn new_returns_cell_holding_value() {
        let c = NlCellRef::new(Sexp::Int(42));
        assert_eq!(c.value, Sexp::Int(42));
    }

    #[test]
    fn clone_bumps_refcount() {
        let a = NlCellRef::new(Sexp::Nil);
        let b = a.clone();
        assert_eq!(NlCellRef::strong_count(&a), 2);
        assert_eq!(NlCellRef::strong_count(&b), 2);
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlCellRef::new(Sexp::Nil);
        {
            let _b = a.clone();
            assert_eq!(NlCellRef::strong_count(&a), 2);
        }
        assert_eq!(NlCellRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlCellRef::new(Sexp::Nil);
        let b = a.clone();
        assert!(NlCellRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different() {
        let a = NlCellRef::new(Sexp::Nil);
        let b = NlCellRef::new(Sexp::Nil);
        assert!(!NlCellRef::ptr_eq(&a, &b));
    }

    #[test]
    fn set_value_mutates_in_place() {
        let c = NlCellRef::new(Sexp::Int(1));
        // SAFETY: no other borrow into `c.value' is live; this is a
        // local single-threaded test.
        unsafe { c.set_value(Sexp::Int(99)) };
        assert_eq!(c.value, Sexp::Int(99));
    }

    #[test]
    fn set_value_visible_through_clone() {
        let a = NlCellRef::new(Sexp::Int(1));
        let b = a.clone();
        // SAFETY: no live borrow into `a.value'.
        unsafe { a.set_value(Sexp::Int(7)) };
        assert_eq!(a.value, Sexp::Int(7));
        assert_eq!(b.value, Sexp::Int(7));
    }

    #[test]
    fn set_value_drops_previous_payload() {
        // Use a nested NlCellRef as the payload so we can observe its
        // refcount drop after `set_value' replaces it.
        let inner = NlCellRef::new(Sexp::Nil);
        let outer = NlCellRef::new(Sexp::Cell(inner.clone()));
        assert_eq!(NlCellRef::strong_count(&inner), 2);
        // SAFETY: outer is local, no borrow into outer.value.
        unsafe { outer.set_value(Sexp::Nil) };
        assert_eq!(NlCellRef::strong_count(&inner), 1);
    }

    #[test]
    fn debug_format_uses_cell_tuple() {
        let c = NlCellRef::new(Sexp::Int(7));
        let s = format!("{:?}", c);
        assert!(
            s.starts_with("Cell("),
            "expected `Cell(...)' debug shape, got {:?}",
            s
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlCellRef::new(Sexp::Int(1));
        let b = a.clone();
        // Same underlying cell — PartialEq returns true via ptr_eq
        // fast path before unwrapping `value'.
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlCellRef::new(Sexp::Int(42));
        let b = NlCellRef::new(Sexp::Int(42));
        assert_eq!(a, b);
        let c = NlCellRef::new(Sexp::Int(43));
        assert_ne!(a, c);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Probe via `Sexp::Vector(NlVectorRef)' — the inner
        // NlVectorRef strong_count lets us observe the drop round-trip
        // when the NlCell drops the value.  Same approach as
        // nlconsbox tests.  Phase A.4.3 (2026-05-09): switched from
        // `Rc<RefCell<Vec<Sexp>>>' probe to `NlVectorRef' since the
        // legacy Rc shape no longer exists for `Sexp::Vector'.
        use crate::eval::nlvector::NlVectorRef;
        let probe = NlVectorRef::new(vec![Sexp::Int(1)]);
        assert_eq!(NlVectorRef::strong_count(&probe), 1);
        {
            let _c = NlCellRef::new(Sexp::Vector(probe.clone()));
            assert_eq!(NlVectorRef::strong_count(&probe), 2);
        }
        assert_eq!(NlVectorRef::strong_count(&probe), 1);
    }

    #[test]
    fn nl_alloc_cell_returns_initialised_cell_with_refcount_1() {
        // Doc 111 §111.D — `nl_alloc_cell' extern wrapper round-trip.
        let initial = Sexp::Int(42);
        let raw = unsafe { nl_alloc_cell(&initial as *const Sexp) };
        assert!(!raw.is_null());
        // Wrap back into the safe handle so `Drop' runs at scope-exit
        // and the allocation is reclaimed.
        let owned = NlCellRef {
            ptr: NonNull::new(raw).unwrap(),
            _marker: PhantomData,
        };
        assert_eq!(NlCellRef::strong_count(&owned), 1);
        assert_eq!(owned.value, Sexp::Int(42));
    }

    #[test]
    fn nl_cell_set_value_overwrites_slot_and_drops_old() {
        // Doc 111 §111.D — verify the extern set_value path runs
        // drop-in-place on the previous value (refcount of an inner
        // boxed payload returns to 1 after the overwrite).
        use crate::eval::nlvector::NlVectorRef;
        let probe = NlVectorRef::new(vec![Sexp::Int(1)]);
        assert_eq!(NlVectorRef::strong_count(&probe), 1);
        let initial = Sexp::Vector(probe.clone());
        assert_eq!(NlVectorRef::strong_count(&probe), 2);
        let raw = unsafe { nl_alloc_cell(&initial as *const Sexp) };
        assert!(!raw.is_null());
        // `nl_alloc_cell' clones `initial' so the probe refcount is now 3.
        assert_eq!(NlVectorRef::strong_count(&probe), 3);
        // Drop the local `initial' to bring the probe refcount back to
        // 2 (= one inside the cell, one outside).
        drop(initial);
        assert_eq!(NlVectorRef::strong_count(&probe), 2);
        let new_val = Sexp::Int(7);
        unsafe { nl_cell_set_value(raw, &new_val as *const Sexp) };
        // After set_value the previous Sexp::Vector is dropped, and
        // only the local `probe' handle remains -> refcount 1.
        assert_eq!(NlVectorRef::strong_count(&probe), 1);
        // Reclaim by wrapping into an NlCellRef and letting it drop.
        let owned = NlCellRef {
            ptr: NonNull::new(raw).unwrap(),
            _marker: PhantomData,
        };
        assert_eq!(owned.value, Sexp::Int(7));
        drop(owned);
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlCellRef::new(Sexp::Nil);
        {
            let _b = a.clone();
            let _c = a.clone();
        }
        let d = a.clone();
        // Even after the intermediate clones drop, `a' and `d'
        // still share the same underlying cell.
        assert!(NlCellRef::ptr_eq(&a, &d));
    }
}
