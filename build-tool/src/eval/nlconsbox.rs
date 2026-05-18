//! `NlConsBox` — layout-pinned cons cell.  `#[repr(C)]' with car / cdr
//! at byte offsets 0 / sizeof(Sexp) and refcount trailer at 2*sizeof(Sexp).
//! Phase 47 + elisp `nl-cons-*' / `nl-rc-*' primitives reach the fields
//! by direct offset arithmetic.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlConsBox {
    pub car: Sexp,
    pub cdr: Sexp,
    pub refcount: AtomicUsize,
}

impl NlConsBox {
    /// Doc 79 v4 Stage C.4-atomic in-place destructor.
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlConsBox>;

    /// Mutate `car' in place — drop previous + write new.
    ///
    /// # Safety
    /// Caller must guarantee no other `&Sexp' borrow into `self.car' is live.
    #[inline]
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of!(self.car) as *mut Sexp;
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// Mutate `cdr' in place.  See [`NlConsBox::set_car`].
    ///
    /// # Safety
    /// Same contract as [`NlConsBox::set_car`].
    #[inline]
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of!(self.cdr) as *mut Sexp;
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }
}

/// Doc 101 §101.D — allocator helper for Phase 47-compiled elisp.
/// Returns a freshly-allocated NlConsBox with car=Nil, cdr=Nil, refcount=1.
///
/// # Safety
/// Caller must wrap the pointer into a `Sexp::Cons(_)' whose `Drop' decrements
/// the refcount, or call into the standard Rust drop path.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_consbox() -> *mut NlConsBox {
    let boxed = Box::new(NlConsBox {
        car: Sexp::Nil,
        cdr: Sexp::Nil,
        refcount: AtomicUsize::new(1),
    });
    Box::into_raw(boxed)
}

/// Doc 101 §101.D — C-callable wrapper around [`NlConsBox::set_car`].
///
/// # Safety
/// `box_ptr` must point at a live `NlConsBox`; `val` must point at an
/// initialized `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_car(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_car((*val).clone());
}

/// Doc 101 §101.D — C-callable wrapper around [`NlConsBox::set_cdr`].
///
/// # Safety
/// `box_ptr` must point at a live `NlConsBox`; `val` must point at an
/// initialized `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_cdr(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_cdr((*val).clone());
}

/// Refcounted handle to an [`NlConsBox`].  `#[repr(transparent)]' so the
/// on-disk layout matches `NonNull<NlConsBox>' — load-bearing for
/// `Sexp::cons_box_ptr'.
#[repr(transparent)]
pub struct NlConsBoxRef {
    ptr: NonNull<NlConsBox>,
    _marker: PhantomData<NlConsBox>,
}

impl NlConsBoxRef {
    /// Allocate a fresh [`NlConsBox`] on the heap with `refcount = 1`.
    pub fn new(car: Sexp, cdr: Sexp) -> NlConsBoxRef {
        let layout = Layout::new::<NlConsBox>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlConsBox;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlConsBox' and is exclusively owned.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).car), car);
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).cdr), cdr);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlConsBoxRef { ptr, _marker: PhantomData }
    }

    /// Read the current strong-reference count.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the underlying box.
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Raw pointer to the box, for ffi shims.
    #[inline]
    pub fn as_ptr(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }

    /// Bump the refcount without acquiring a Rust handle.  Backs elisp `nl-rc-inc`.
    ///
    /// # Safety
    /// Caller must guarantee a matching [`Self::rc_dec_raw`] follows.
    pub unsafe fn rc_inc_raw(this: &Self) {
        // SAFETY: `this.ptr' is alive (caller holds the backing handle).
        unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Decrement the refcount, freeing the box at 0.  Backs elisp `nl-rc-dec`.
    ///
    /// # Safety
    /// Must pair with a prior [`Self::rc_inc_raw`] or be the final balancing
    /// decrement.  On refcount=0 car/cdr are dropped and the alloc freed.
    pub unsafe fn rc_dec_raw(this: &Self) {
        // SAFETY: `this.ptr' is alive on entry.  Release/Acquire pattern
        // matches `Drop' below; cannot reuse `Drop' because we have `&Self'.
        let prev = unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*this.ptr.as_ptr()).car));
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*this.ptr.as_ptr()).cdr));
            let layout = Layout::new::<NlConsBox>();
            alloc::dealloc(this.ptr.as_ptr() as *mut u8, layout);
        }
    }

    /// Mutate `car' in place.
    ///
    /// # Safety
    /// No other `&Sexp' borrow into `self.car' may be live.
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).car);
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// Mutate `cdr` in place.
    ///
    /// # Safety
    /// Same as [`NlConsBoxRef::set_car`].
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).cdr);
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }

    /// Internal raw `*const NlConsBox' for box-walking shims.
    #[doc(hidden)]
    #[inline]
    #[allow(dead_code)]
    pub fn inner_raw(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }
}

impl Clone for NlConsBoxRef {
    /// Doc 124 §124.F — refcount +1 via elisp `nelisp_nlconsbox_clone'.
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive (we hold a handle).
        unsafe {
            crate::elisp_cc_spike::nlconsbox_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlConsBoxRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlConsBoxRef {
    /// Doc 124 §124.L — elisp `nlconsbox_drop' kernel does refcount-- + dealloc.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlconsbox_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlConsBoxRef {
    type Target = NlConsBox;

    fn deref(&self) -> &NlConsBox {
        // SAFETY: handle keeps the box alive; field offsets are pinned.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlConsBoxRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Cons").field(&self.car).field(&self.cdr).finish()
    }
}

impl PartialEq for NlConsBoxRef {
    /// Structural eq with `ptr_eq' fast path (handles self-referential cycles).
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.car == other.car && self.cdr == other.cdr
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    assert!(std::mem::offset_of!(NlConsBox, car) == 0);
    assert!(std::mem::offset_of!(NlConsBox, cdr) == std::mem::size_of::<Sexp>());
    assert!(
        std::mem::offset_of!(NlConsBox, refcount) == 2 * std::mem::size_of::<Sexp>()
    );
};
