use crate::eval::sexp::Sexp;
crate::define_nlbox!(
    inner=NlConsBox, ref_ty=NlConsBoxRef, fields={car: Sexp, cdr: Sexp},
    clone_fn=crate::elisp_cc_spike::nlconsbox_clone, drop_fn=crate::elisp_cc_spike::nlconsbox_drop,
    layout_asserts={
        use ::std::mem::{offset_of, size_of};
        assert!(size_of::<AtomicUsize>() == 8);
        assert!(offset_of!(NlConsBox, car) == 0);
        assert!(offset_of!(NlConsBox, cdr) == size_of::<Sexp>());
        assert!(offset_of!(NlConsBox, refcount) == 2 * size_of::<Sexp>());
    }
);
macro_rules! consbox_set_field {
    ($box_ptr:expr, $field:ident, $val:expr) => {{
        let p = ::std::ptr::addr_of_mut!((*$box_ptr).$field);
        ::std::ptr::drop_in_place(p); ::std::ptr::write(p, $val);
    }};
}
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_car(box_ptr: *mut NlConsBox, val: *const Sexp) { consbox_set_field!(box_ptr, car, (*val).clone()); }
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_cdr(box_ptr: *mut NlConsBox, val: *const Sexp) { consbox_set_field!(box_ptr, cdr, (*val).clone()); }
impl std::fmt::Debug for NlConsBoxRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_tuple("Cons").field(&self.car).field(&self.cdr).finish() }
}
impl PartialEq for NlConsBoxRef {
    fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || (self.car == other.car && self.cdr == other.cdr) }
}
