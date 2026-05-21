use crate::eval::sexp::Sexp;
crate::define_nlbox!(
    inner=NlConsBox, ref_ty=NlConsBoxRef, fields={car: Sexp, cdr: Sexp},
    clone_fn=crate::elisp_cc_spike::nlconsbox_clone, drop_fn=crate::elisp_cc_spike::nlconsbox_drop,
    layout_asserts={ assert!(offset_of!(NlConsBox, car) == 0); assert!(offset_of!(NlConsBox, cdr) == size_of::<Sexp>()); assert!(offset_of!(NlConsBox, refcount) == 2 * size_of::<Sexp>()); }
);
#[no_mangle] pub unsafe extern "C" fn nl_consbox_set_car(box_ptr: *mut NlConsBox, val: *const Sexp) { let p = ::std::ptr::addr_of_mut!((*box_ptr).car); ::std::ptr::drop_in_place(p); ::std::ptr::write(p, (*val).clone()); }
#[no_mangle] pub unsafe extern "C" fn nl_consbox_set_cdr(box_ptr: *mut NlConsBox, val: *const Sexp) { let p = ::std::ptr::addr_of_mut!((*box_ptr).cdr); ::std::ptr::drop_in_place(p); ::std::ptr::write(p, (*val).clone()); }
impl std::fmt::Debug for NlConsBoxRef { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_tuple("Cons").field(&self.car).field(&self.cdr).finish() } }
impl PartialEq for NlConsBoxRef { fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || (self.car == other.car && self.cdr == other.cdr) } }
