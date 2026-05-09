//! Doc 77c Phase A.4.2 — `NlStr` layout-pinned single-slot mutable
//! string buffer.
//!
//! Specialized self-managed refcounted cell carrying one mutable
//! `String` slot.  Replaces the legacy `Sexp::MutStr(Rc<RefCell<String>>)'
//! with a layout-pinned struct so:
//!
//! 1. The refcount lives at a known offset relative to the `value`
//!    field (= same architectural shape as [`super::nlcell::NlCell`]
//!    and [`super::nlconsbox::NlConsBox`]), unifying the elisp /
//!    Phase B self-host ABI across `Sexp' boxed variants.
//! 2. The `RefCell' borrow-flag overhead drops out — eval-time read
//!    is a single load of the `String' header instead of `borrow()'.
//! 3. `make-string' / `aset' on a mutable string get the same write
//!    contract as `setcar' / `setcdr' (= `unsafe set_value' /
//!    `with_value_mut').
//!
//! Layout (Phase A.4.2 locked):
//!
//! ```text
//! NlStr:  +----------+  offset 0                    (sizeof String)  value
//!         +----------+  offset sizeof(String)       (8 bytes)        refcount
//!         +----------+
//! ```
//!
//! Note: `String' itself is `#[repr(Rust)]'.  The `#[repr(C)]' on the
//! outer `NlStr' fixes field *ordering* + offsets, but the `String'
//! header (= `(ptr, len, cap)' triple) keeps its native Rust layout.
//! Phase B elisp will reach the chars via the String's `ptr' field
//! (= 2-load) — same access pattern Rust uses today.
//!
//! Mutability:
//! - `unsafe set_value(s: String)' — wholesale replace (= the legacy
//!   `*borrow_mut() = new_str' pattern used by `aset' on MutStr).
//! - `unsafe with_value_mut(f: FnOnce(&mut String) -> R) -> R' — in-
//!   place mutation closure (= the `let mut s = borrow_mut(); s.push_str' /
//!   `s.replace_range' style; not currently used by the migrated call
//!   sites but exposed for future Phase B / `nelisp-text-buffer' code).
//!
//! Out of scope for Phase A.4.2 MutStr migrate:
//!   - Other variants (Vector / BoolVector / Record / CharTable) —
//!     A.4.3-A.4.6 follow-up sub-stages.
//!   - elisp `nl-str-*' primitives — Phase B (`nelisp-text-buffer'
//!     uses host MutStr today).
//!
//! Threading: `AtomicUsize` mirrors `NlCell' / `NlConsBox' rationale.
//! Not `Send` / `Sync`.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned mutable string buffer.  Heap-allocated, refcounted
/// via an `AtomicUsize` trailer.  Accessed through [`NlStrRef`]
/// handles.
#[repr(C)]
pub struct NlStr {
    /// The mutable string slot.  Offset 0 — same JIT contract as
    /// `NlCell.value' / `NlConsBox.car'.
    pub value: String,
    /// Strong reference count.  Starts at 1 in [`NlStrRef::new`],
    /// +1 on each `Clone`, -1 on each `Drop`.  When it reaches 0 the
    /// last handle drops `value' (= frees the heap chars buffer) and
    /// frees the [`NlStr`] allocation itself.
    pub refcount: AtomicUsize,
}

/// Refcounted handle to an [`NlStr`].  API parity with
/// [`super::nlcell::NlCellRef`] + [`super::nlconsbox::NlConsBoxRef`]:
/// `new` / `Clone` / `Drop` / `Deref` (returns `&NlStr`).
pub struct NlStrRef {
    ptr: NonNull<NlStr>,
    /// Tells the borrow-checker we own an `NlStr` even though the
    /// field is `NonNull<...>'.  Mirrors `std::rc::Rc' / `NlCellRef' /
    /// `NlConsBoxRef'.
    _marker: PhantomData<NlStr>,
}

impl NlStrRef {
    /// Allocate a fresh [`NlStr`] on the heap with `refcount = 1`
    /// and return the unique handle.  The supplied `value` is moved
    /// into the box.
    ///
    /// Panics on allocation failure.
    pub fn new(value: String) -> NlStrRef {
        let layout = Layout::new::<NlStr>();
        // SAFETY: `Layout::new::<NlStr>()' is non-zero-sized — at
        // minimum the `String' header (= 24 bytes on 64-bit) + 8-byte
        // refcount.
        let raw = unsafe { alloc::alloc(layout) } as *mut NlStr;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlStr' and is
        // exclusively owned here.  We initialize both fields before
        // anyone else can observe the box.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlStrRef {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Read the current strong-reference count.  Mirrors
    /// `NlCellRef::strong_count' / `NlConsBoxRef::strong_count'.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the *underlying box*.  Two clones of the
    /// same [`NlStrRef::new`] invocation are pointer-equal; two
    /// distinct allocations are not.  Used by `eq' / `eql' on
    /// `Sexp::MutStr' (= identity comparison).
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.  Drops the previous String, then
    /// writes the new one.  Equivalent to the legacy
    /// `*borrow_mut() = new_str' pattern (= `aset' on MutStr).
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&String` borrow into this
    /// box's `value` field is live at the time of the write.  In
    /// Phase A.4.2 the migrated `aset' callsite owns `new_str'
    /// locally and holds no outstanding borrow into `self.value';
    /// the same Phase A.2.1 setcar discipline applies.
    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        // SAFETY: see method-level contract.
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.  Hands the caller a `&mut String'
    /// so they can use any `String' API (`push_str' / `replace_range'
    /// / `truncate' / etc.) without going through `set_value'.
    /// Returns whatever the closure returns.
    ///
    /// # Safety
    ///
    /// Same as [`set_value`]: no other `&String` borrow into
    /// `self.value' may be live for the duration of the closure.
    /// Reentrant calls (= the closure recursing into another
    /// `with_value_mut' on the same handle) are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        // SAFETY: see method-level contract.  Caller-provided closure
        // observes a `&mut String' that aliases nothing else.
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlStrRef {
    /// Bump the refcount and return a new handle that shares the
    /// same inner box.  `Relaxed' for the +1.
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlStrRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlStrRef {
    /// Decrement the refcount.  When it reaches zero, drop the
    /// `value' String (= frees its heap chars buffer) and free the
    /// allocation.
    fn drop(&mut self) {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        let prev = unsafe {
            (*self.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0 and `NlStrRef' is not Send /
        // Sync — no concurrent re-increment can race the free.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value));
            let layout = Layout::new::<NlStr>();
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl Deref for NlStrRef {
    type Target = NlStr;

    fn deref(&self) -> &NlStr {
        // SAFETY: see `NlCellRef::deref' for the same invariant.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlStrRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("MutStr").field(&self.value).finish()
    }
}

impl PartialEq for NlStrRef {
    /// Structural equality + ptr_eq fast path.  Mirrors
    /// `NlCellRef::eq' / `NlConsBoxRef::eq' rationale.
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.value == other.value
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    // value @ offset 0 — same JIT contract as `NlCell.value'.
    assert!(offset_of!(NlStr, value) == 0);
    // refcount @ offset sizeof(String) — `repr(C)' linear layout.
    assert!(offset_of!(NlStr, refcount) == size_of::<String>());
    // refcount is 8 bytes on every supported arch.
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_value_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlStr, value), 0);
    }

    #[test]
    fn layout_refcount_after_value() {
        use std::mem::{offset_of, size_of};
        assert_eq!(offset_of!(NlStr, refcount), size_of::<String>());
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let s = NlStrRef::new("hello".to_string());
        assert_eq!(NlStrRef::strong_count(&s), 1);
    }

    #[test]
    fn new_returns_box_holding_value() {
        let s = NlStrRef::new("hello".to_string());
        assert_eq!(s.value, "hello");
    }

    #[test]
    fn clone_bumps_refcount_and_shares_value() {
        let a = NlStrRef::new("shared".to_string());
        let b = a.clone();
        assert_eq!(NlStrRef::strong_count(&a), 2);
        assert_eq!(b.value, "shared");
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlStrRef::new("x".to_string());
        {
            let _b = a.clone();
            assert_eq!(NlStrRef::strong_count(&a), 2);
        }
        assert_eq!(NlStrRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlStrRef::new("a".to_string());
        let b = a.clone();
        assert!(NlStrRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_alloc() {
        let a = NlStrRef::new("same".to_string());
        let b = NlStrRef::new("same".to_string());
        assert!(!NlStrRef::ptr_eq(&a, &b));
    }

    #[test]
    fn set_value_replaces_in_place() {
        let s = NlStrRef::new("old".to_string());
        // SAFETY: no other borrow into s.value is live.
        unsafe { s.set_value("new".to_string()) };
        assert_eq!(s.value, "new");
    }

    #[test]
    fn set_value_visible_through_clone() {
        let a = NlStrRef::new("v1".to_string());
        let b = a.clone();
        // SAFETY: no live borrow.
        unsafe { a.set_value("v2".to_string()) };
        assert_eq!(a.value, "v2");
        assert_eq!(b.value, "v2");
    }

    #[test]
    fn with_value_mut_in_place_mutation() {
        let s = NlStrRef::new("hello".to_string());
        // SAFETY: no other borrow live.
        unsafe {
            s.with_value_mut(|v| {
                v.push_str(", world");
            });
        }
        assert_eq!(s.value, "hello, world");
    }

    #[test]
    fn with_value_mut_returns_value_from_closure() {
        let s = NlStrRef::new("abc".to_string());
        let len = unsafe { s.with_value_mut(|v| v.len()) };
        assert_eq!(len, 3);
    }

    #[test]
    fn debug_format_uses_mutstr_tuple() {
        let s = NlStrRef::new("x".to_string());
        let d = format!("{:?}", s);
        assert!(
            d.starts_with("MutStr("),
            "expected `MutStr(...)' debug shape, got {:?}",
            d
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlStrRef::new("hi".to_string());
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlStrRef::new("hi".to_string());
        let b = NlStrRef::new("hi".to_string());
        assert_eq!(a, b);
        let c = NlStrRef::new("ho".to_string());
        assert_ne!(a, c);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Probe via a fresh String allocation — observe that `drop'
        // returns the chars buffer to the allocator.  We can't
        // trivially detect this from inside the test, but the
        // round-trip check via large allocation + memory inspection
        // is impractical.  Use a clone-bump round-trip instead:
        // create + clone + drop the clone + drop the original; if
        // the payload were leaked the second drop would not free
        // (= no panic, no observable effect from inside test, but
        // miri / ASAN would catch).  We rely on miri / ASAN runs in
        // CI; the unit test asserts no UB-flag panics.
        let s = NlStrRef::new("payload".to_string());
        {
            let _t = s.clone();
        }
        drop(s);
        // If we reach here without panic / UB, the drop sequence
        // works.  The companion miri / ASAN run is what gives this
        // test real teeth.
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlStrRef::new("a".to_string());
        {
            let _b = a.clone();
        }
        let c = a.clone();
        assert!(NlStrRef::ptr_eq(&a, &c));
    }

    #[test]
    fn set_value_drops_previous_string() {
        // Build a String large enough that we'd notice if it leaked.
        let s = NlStrRef::new("0".repeat(1024));
        // SAFETY: no other borrow.
        unsafe { s.set_value("1".to_string()) };
        assert_eq!(s.value, "1");
        // ASAN / miri verify the 1024-byte buffer was returned to
        // the allocator on `drop_in_place'.
    }

    #[test]
    fn with_value_mut_truncate() {
        let s = NlStrRef::new("longstring".to_string());
        unsafe {
            s.with_value_mut(|v| v.truncate(4));
        }
        assert_eq!(s.value, "long");
    }
}
