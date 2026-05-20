//! `NlConsBox` — layout-pinned cons cell.  `#[repr(C)]' with car / cdr
//! at byte offsets 0 / sizeof(Sexp) and refcount trailer at 2*sizeof(Sexp).
//! Phase 47 + elisp `nl-cons-*' / `nl-rc-*' primitives reach the fields
//! by direct offset arithmetic.

use crate::eval::sexp::Sexp;

crate::define_nlbox!(
    inner          = NlConsBox,
    ref_ty         = NlConsBoxRef,
    fields         = { car: Sexp, cdr: Sexp },
    clone_fn       = crate::elisp_cc_spike::nlconsbox_clone,
    drop_fn        = crate::elisp_cc_spike::nlconsbox_drop,
    layout_asserts = {
        use ::std::mem::{offset_of, size_of};
        assert!(size_of::<AtomicUsize>() == 8);
        assert!(offset_of!(NlConsBox, car) == 0);
        assert!(offset_of!(NlConsBox, cdr) == size_of::<Sexp>());
        assert!(offset_of!(NlConsBox, refcount) == 2 * size_of::<Sexp>());
    }
);

impl NlConsBox {
    // Safety: no live `&Sexp` borrow into the field (see nlinner_set! contract).
    crate::nlinner_set!(set_car, car, Sexp);
    crate::nlinner_set!(set_cdr, cdr, Sexp);
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
    #[inline]
    pub fn as_ptr(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
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
    pub fn inner_raw(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }
}

impl std::fmt::Debug for NlConsBoxRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Cons")
            .field(&self.car)
            .field(&self.cdr)
            .finish()
    }
}

impl PartialEq for NlConsBoxRef {
    /// Structural eq with `ptr_eq' fast path (handles self-referential cycles).
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || (self.car == other.car && self.cdr == other.cdr)
    }
}
