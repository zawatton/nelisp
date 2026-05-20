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

#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_car(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_car((*val).clone());
}

#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_cdr(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_cdr((*val).clone());
}

impl NlConsBoxRef {
    #[inline]
    pub fn as_ptr(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }

    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).car);
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

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
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || (self.car == other.car && self.cdr == other.cdr)
    }
}
