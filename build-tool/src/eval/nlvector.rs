//! `NlVector` — layout-pinned mutable Vec<Sexp> box.  `#[repr(C)]' with
//! value @ 0, refcount @ sizeof(Vec<Sexp>).  Backs `Sexp::Vector'.
//! `unsafe set_value' wholesale replaces; `unsafe with_value_mut(f)' for
//! in-place (aset).

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlVector {
    pub value: Vec<Sexp>,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlVectorRef {
    ptr: NonNull<NlVector>,
    _marker: PhantomData<NlVector>,
}

impl NlVector { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlVector>; } // Doc 79 v4 C.4-atomic
impl NlVectorRef {
    /// Allocate a fresh [`NlVector`] on the heap with `refcount = 1'.
    pub fn new(value: Vec<Sexp>) -> NlVectorRef {
        let layout = Layout::new::<NlVector>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlVector;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlVector' and is
        // exclusively owned here.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlVectorRef {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.  Drops the previous Vec, then
    /// writes the new one.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&Vec<Sexp>` borrow into this
    /// box's `value` is live.  Phase A.2.1 setcar discipline applies.
    pub unsafe fn set_value(&self, val: Vec<Sexp>) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.
    ///
    /// # Safety
    ///
    /// Same as [`set_value`]: no other `&Vec<Sexp>` borrow into
    /// `self.value' may be live for the duration of the closure.
    /// Reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut Vec<Sexp>) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

/// Doc 111 §111.E — C-callable allocator for fresh `NlVector` boxes
/// pre-filled with `capacity' `Sexp::Nil' elements.
///
/// Mirrors `nl_alloc_consbox` / `nl_alloc_cell' in shape and return-
/// raw-pointer convention.  Used by `frame_stack_ensure_capacity'
/// (= grow lexframe-stack BACKING when full) and may also be used by
/// `install_empty_frames_record_rust_direct' (= 8-slot initial vector)
/// once Rust callers swap to it.
///
/// # Safety
/// Caller must wrap the returned pointer into a `Sexp::Vector(_)' whose
/// `NlVectorRef::drop' decrements the refcount (standard ownership
/// transfer), or call into the standard Rust drop path.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_vector(capacity: i64) -> *mut NlVector {
    let cap = if capacity < 0 { 0 } else { capacity as usize };
    let layout = Layout::new::<NlVector>();
    // SAFETY: `Layout::new::<NlVector>()' is non-zero-sized.
    let raw = unsafe { alloc::alloc(layout) } as *mut NlVector;
    if raw.is_null() {
        alloc::handle_alloc_error(layout);
    }
    // SAFETY: `raw' was just allocated and is exclusively owned.
    unsafe {
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).value),
            vec![Sexp::Nil; cap],
        );
        std::ptr::write(
            std::ptr::addr_of_mut!((*raw).refcount),
            AtomicUsize::new(1),
        );
    }
    raw
}

/// Doc 111 §111.E — set element N of an NlVector in place, refcount-
/// safely cloning `*val' before write.  Mirrors `nl_record_set_slot'
/// for record-slot writes.
///
/// Used by `frame_stack_ensure_capacity' (Group E) and by
/// `mirror_install_entry' (= Phase 47 helper #12, Group B) to overwrite
/// a bucket-vector slot when prepending a fresh `symbol-entry'.
///
/// # Safety
/// - `vec_ptr' must be non-null and point at a live `NlVector'.
/// - `val' must be non-null and point at an initialised `Sexp'.
/// - `n' must be a valid index into `vec.value'.
/// - No other `&Vec<Sexp>' borrow into `vec.value' may be live.
#[no_mangle]
pub unsafe extern "C" fn nl_vector_set_slot(
    vec_ptr: *mut NlVector,
    n: i64,
    val: *const Sexp,
) {
    let v = unsafe { &mut *vec_ptr };
    let new_val = unsafe { (*val).clone() };
    v.value[n as usize] = new_val;
}

impl Clone for NlVectorRef {
    /// Doc 124 §124.F — refcount +1 dispatched to the Phase 47-compiled
    /// `nelisp_nlvector_clone' kernel.  SIGSEGV pre-investigation blocker
    /// turned out to be Doc 115 §115.3 odd-arity GP defun alignment bug;
    /// per-defun fix shipped in commit 6eb73197 unblocked this sweep.
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlvector_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlVectorRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlVectorRef {
    fn drop(&mut self) { unsafe { crate::nlrc_drop_box!(self.ptr.as_ptr(), NlVector, crate::eval::sexp::SEXP_TAG_VECTOR); } }
}

impl Deref for NlVectorRef {
    type Target = NlVector;

    fn deref(&self) -> &NlVector {
        // SAFETY: see `NlStrRef::deref'.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlVectorRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Vector").field(&self.value).finish()
    }
}

impl PartialEq for NlVectorRef {
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
    assert!(offset_of!(NlVector, value) == 0);
    assert!(offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>());
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_value_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlVector, value), 0);
    }

    #[test]
    fn layout_refcount_after_value() {
        use std::mem::{offset_of, size_of};
        assert_eq!(offset_of!(NlVector, refcount), size_of::<Vec<Sexp>>());
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let v = NlVectorRef::new(vec![Sexp::Int(7), Sexp::Int(8)]);
        assert_eq!(NlVectorRef::strong_count(&v), 1);
    }

    #[test]
    fn new_returns_box_holding_value() {
        let v = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        assert_eq!(v.value.len(), 3);
        assert_eq!(v.value[0], Sexp::Int(1));
        assert_eq!(v.value[2], Sexp::Int(3));
    }

    #[test]
    fn clone_bumps_refcount_and_shares_value() {
        let a = NlVectorRef::new(vec![Sexp::Int(42)]);
        let b = a.clone();
        assert_eq!(NlVectorRef::strong_count(&a), 2);
        assert_eq!(b.value, vec![Sexp::Int(42)]);
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlVectorRef::new(vec![]);
        {
            let _b = a.clone();
            assert_eq!(NlVectorRef::strong_count(&a), 2);
        }
        assert_eq!(NlVectorRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlVectorRef::new(vec![Sexp::Nil]);
        let b = a.clone();
        assert!(NlVectorRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_alloc() {
        let a = NlVectorRef::new(vec![Sexp::Int(1)]);
        let b = NlVectorRef::new(vec![Sexp::Int(1)]);
        assert!(!NlVectorRef::ptr_eq(&a, &b));
    }

    #[test]
    fn set_value_replaces_in_place() {
        let v = NlVectorRef::new(vec![Sexp::Int(1)]);
        // SAFETY: no other borrow live.
        unsafe { v.set_value(vec![Sexp::Int(99), Sexp::Int(100)]) };
        assert_eq!(v.value.len(), 2);
        assert_eq!(v.value[0], Sexp::Int(99));
    }

    #[test]
    fn set_value_visible_through_clone() {
        let a = NlVectorRef::new(vec![Sexp::Int(1)]);
        let b = a.clone();
        unsafe { a.set_value(vec![Sexp::Int(7)]) };
        assert_eq!(a.value, vec![Sexp::Int(7)]);
        assert_eq!(b.value, vec![Sexp::Int(7)]);
    }

    #[test]
    fn with_value_mut_index_set() {
        let v = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        // SAFETY: no other borrow live.
        unsafe {
            v.with_value_mut(|vec| {
                vec[1] = Sexp::Int(99);
            });
        }
        assert_eq!(v.value[1], Sexp::Int(99));
    }

    #[test]
    fn with_value_mut_push() {
        let v = NlVectorRef::new(vec![]);
        unsafe {
            v.with_value_mut(|vec| {
                vec.push(Sexp::Int(1));
                vec.push(Sexp::Int(2));
            });
        }
        assert_eq!(v.value.len(), 2);
    }

    #[test]
    fn with_value_mut_returns_value_from_closure() {
        let v = NlVectorRef::new(vec![Sexp::Int(10)]);
        let len = unsafe { v.with_value_mut(|vec| vec.len()) };
        assert_eq!(len, 1);
    }

    #[test]
    fn debug_format_uses_vector_tuple() {
        let v = NlVectorRef::new(vec![Sexp::Int(1)]);
        let d = format!("{:?}", v);
        assert!(
            d.starts_with("Vector("),
            "expected `Vector(...)' debug shape, got {:?}",
            d
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlVectorRef::new(vec![Sexp::Int(1)]);
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2)]);
        let b = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(2)]);
        assert_eq!(a, b);
        let c = NlVectorRef::new(vec![Sexp::Int(1), Sexp::Int(3)]);
        assert_ne!(a, c);
        let d = NlVectorRef::new(vec![Sexp::Int(1)]);
        assert_ne!(a, d);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Round-trip clone+drop without panic — ASAN / miri verify
        // no UB on the freed allocation.
        let v = NlVectorRef::new(vec![Sexp::Int(1); 100]);
        {
            let _t = v.clone();
        }
        drop(v);
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlVectorRef::new(vec![Sexp::Nil]);
        {
            let _b = a.clone();
        }
        let c = a.clone();
        assert!(NlVectorRef::ptr_eq(&a, &c));
    }

    #[test]
    fn empty_vector_round_trip() {
        let v = NlVectorRef::new(vec![]);
        assert_eq!(v.value.len(), 0);
        assert_eq!(NlVectorRef::strong_count(&v), 1);
    }
}
