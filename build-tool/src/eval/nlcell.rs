//! `NlCell` — layout-pinned single-slot mutable cell.  `#[repr(C)]'
//! with value at offset 0 and refcount trailer at sizeof(Sexp).  Backs
//! the `Sexp::Cell' variant + `FrameCell' (closure-capture write-through).
//! Not `Send` / `Sync`.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlCell {
    pub value: Sexp,
    pub refcount: AtomicUsize,
}

/// Refcounted handle.  `#[repr(transparent)]' so the on-disk layout
/// matches `NonNull<NlCell>' — read from `Sexp::Cell' payload offset.
#[repr(transparent)]
pub struct NlCellRef {
    ptr: NonNull<NlCell>,
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

    /// Wrap an externally-allocated `*mut NlCell' into an owning
    /// `NlCellRef'.  Used by integration tests that exercise the
    /// extern `nl_alloc_cell' allocator and need to hand ownership
    /// back to safe Rust (so `Drop' runs).
    ///
    /// # Safety
    /// - `raw' must point at an initialized `NlCell' with `refcount >= 1'.
    /// - Ownership of one strong reference is transferred into the
    ///   returned `NlCellRef'; the caller must not also drop it.
    #[doc(hidden)]
    pub unsafe fn from_raw_ptr(raw: *mut NlCell) -> NlCellRef {
        NlCellRef {
            ptr: NonNull::new(raw).expect("from_raw_ptr: null pointer"),
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
    /// Doc 124 §124.L — dispatch through the pure-elisp `nlcell_drop'
    /// kernel.  Runs `atomic-fetch-add(-1)' then, on pre-sub == 1,
    /// calls `nl_cell_drop_inner' (= `drop_in_place::<NlCell>') +
    /// `dealloc-bytes(40, 8)'.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlcell_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
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

