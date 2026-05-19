//! `NlConsBox` — layout-pinned cons cell.  `#[repr(C)]' with car / cdr
//! at byte offsets 0 / sizeof(Sexp) and refcount trailer at 2*sizeof(Sexp).
//! Phase 47 + elisp `nl-cons-*' / `nl-rc-*' primitives reach the fields
//! by direct offset arithmetic.

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlConsBox {
    pub car: Sexp,
    pub cdr: Sexp,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(
    NlConsBoxRef,
    NlConsBox,
    drop_fn = crate::elisp_cc_spike::nlconsbox_drop
);

impl NlConsBox {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlConsBox>;

    // Safety: no live `&Sexp` borrow into the field (see nlinner_set! contract).
    crate::nlinner_set!(set_car, car, Sexp);
    crate::nlinner_set!(set_cdr, cdr, Sexp);
}

/// Fresh NlConsBox(Nil, Nil, refcount=1).  Safety: caller wraps in `Sexp::Cons(_)'.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_consbox() -> *mut NlConsBox {
    let boxed = Box::new(NlConsBox {
        car: Sexp::Nil,
        cdr: Sexp::Nil,
        refcount: AtomicUsize::new(1),
    });
    Box::into_raw(boxed)
}

/// # Safety: live box + initialized Sexp.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_car(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_car((*val).clone());
}

/// # Safety: live box + initialized Sexp.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_cdr(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_cdr((*val).clone());
}

impl NlConsBoxRef {
    pub fn new(car: Sexp, cdr: Sexp) -> NlConsBoxRef {
        let layout = Layout::new::<NlConsBox>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlConsBox;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
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

    #[inline]
    pub fn as_ptr(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }

    /// # Safety: caller pairs with matching `rc_dec_raw'.
    pub unsafe fn rc_inc_raw(this: &Self) {
        unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_add(1, Ordering::Relaxed);
        }
    }

    /// # Safety: pair with prior `rc_inc_raw' or be final decrement.
    pub unsafe fn rc_dec_raw(this: &Self) {
        let prev = unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*this.ptr.as_ptr()).car));
            std::ptr::drop_in_place(std::ptr::addr_of_mut!((*this.ptr.as_ptr()).cdr));
            let layout = Layout::new::<NlConsBox>();
            alloc::dealloc(this.ptr.as_ptr() as *mut u8, layout);
        }
    }

    /// # Safety: no other `&Sexp' borrow into `self.car' may be live.
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).car);
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// # Safety: same as `set_car'.
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).cdr);
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }

    #[doc(hidden)]
    #[inline]
    #[allow(dead_code)]
    pub fn inner_raw(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }
}

impl Clone for NlConsBoxRef {
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlconsbox_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlConsBoxRef { ptr: self.ptr, _marker: PhantomData }
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

const _: () = {
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    assert!(std::mem::offset_of!(NlConsBox, car) == 0);
    assert!(std::mem::offset_of!(NlConsBox, cdr) == std::mem::size_of::<Sexp>());
    assert!(
        std::mem::offset_of!(NlConsBox, refcount) == 2 * std::mem::size_of::<Sexp>()
    );
};
