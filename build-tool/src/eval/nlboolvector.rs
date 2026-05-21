crate::define_nlbox!(
    inner=NlBoolVector, ref_ty=NlBoolVectorRef, fields={value: Vec<bool>},
    clone_fn=crate::elisp_cc_spike::nlboolvector_clone, drop_fn=crate::elisp_cc_spike::nlboolvector_drop,
    layout_asserts={ assert!(offset_of!(NlBoolVector, value) == 0); assert!(offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>()); }
);
impl NlBoolVector { crate::nlinner_set!(set_value, value, Vec<bool>); crate::nlinner_with_mut!(with_value_mut, value, Vec<bool>); }
impl std::fmt::Debug for NlBoolVectorRef { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_tuple("BoolVector").field(&self.value).finish() } }
impl PartialEq for NlBoolVectorRef { fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || self.value == other.value } }
