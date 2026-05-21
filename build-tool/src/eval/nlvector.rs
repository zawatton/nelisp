use crate::eval::sexp::Sexp;
crate::define_nlbox!(
    inner=NlVector, ref_ty=NlVectorRef, fields={value: Vec<Sexp>},
    clone_fn=crate::elisp_cc_spike::nlvector_clone, drop_fn=crate::elisp_cc_spike::nlvector_drop,
    layout_asserts={ assert!(offset_of!(NlVector, value) == 0); assert!(offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>()); }
);
impl NlVector {
    crate::nlinner_set!(set_value, value, Vec<Sexp>);
    crate::nlinner_with_mut!(with_value_mut, value, Vec<Sexp>);
}
impl std::fmt::Debug for NlVectorRef { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_tuple("Vector").field(&self.value).finish() } }
impl PartialEq for NlVectorRef { fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || self.value == other.value } }
#[no_mangle] pub unsafe extern "C" fn nl_vector_set_slot(vec_ptr: *mut NlVector, n: i64, val: *const Sexp) { (&mut (*vec_ptr).value)[n as usize] = (*val).clone(); }
