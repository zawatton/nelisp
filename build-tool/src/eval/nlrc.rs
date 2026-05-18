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
    #[doc(hidden)]
    pub fn inner_raw(this: &Self) -> *const NlRcInner<T> {
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

// Doc 124 §124.L / §124.L+ — per-type inner-drop ABI externs for the
// elisp Drop kernels (now the sole production Drop path for all 7
// NlBox types).  Each `nl_<type>_drop_inner' wraps the per-type
// `nlrc_payload_drop::<NlT>' (= `std::ptr::drop_in_place::<NlT>') so
// the elisp `nl<type>-drop' kernels can call it via `extern-call'
// before the matching `dealloc-bytes' op.  This is the recursive
// payload-drop step that the legacy `nlrc_drop_box!' macro used to
// dispatch through `NLRC_DROP_TABLE[tag](raw)' before §124.L+
// migrated all 7 `impl Drop' bodies to elisp.
//
// # Safety
//
// `box_ptr' must point at a fully-initialized `NlT' whose backing
// allocation the caller is about to free.  Each function takes
// `*mut i64' to match the elisp kernel's `box-ptr' arg type and
// reinterprets through the typed `nlrc_payload_drop::<T>'.

/// Generic in-place drop helper — runs `T`'s destructor without freeing
/// the heap slot.  Each `nl_<type>_drop_inner' wrapper below
/// monomorphises this through the corresponding `NlT::DROP_FN'
/// constant.
///
/// # Safety
/// `ptr` must point at a fully-initialized `T` whose backing alloc the
/// caller is about to free.
pub unsafe fn nlrc_payload_drop<T>(ptr: *mut std::ffi::c_void) {
    std::ptr::drop_in_place(ptr as *mut T);
}

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

// Doc 124 §124.L+ — `NLRC_DROP_TABLE' + `nlrc_drop_box!' macro +
// `nl_rc_unboxed_drop_panic' stub deleted (= zero callers after the
// 7-of-7 NlBox `impl Drop' bodies all dispatch through the per-type
// elisp `nelisp_nl<type>_drop' kernels).  The legacy tag-dispatch
// table is no longer needed because each `impl Drop' now knows its
// concrete type at the Rust call site and the elisp kernel encodes
// the per-type layout literals (SIZE / REFCOUNT_OFFSET / ALIGN).

// Compile-time layout assertions — refcount @ 0, value @ 8 for i64 canary.
const _: () = {
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    assert!(std::mem::offset_of!(NlRcInner<i64>, refcount) == 0);
    assert!(std::mem::offset_of!(NlRcInner<i64>, value) == 8);
};

