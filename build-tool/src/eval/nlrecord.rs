use crate::eval::sexp::Sexp;
crate::define_nlbox!(
    inner=NlRecord, ref_ty=NlRecordRef, fields={type_tag: Sexp, slots: Vec<Sexp>},
    clone_fn=crate::elisp_cc_spike::nlrecord_clone, drop_fn=crate::elisp_cc_spike::nlrecord_drop,
    layout_asserts={ assert!(offset_of!(NlRecord, type_tag) == 0); assert!(offset_of!(NlRecord, slots) == size_of::<Sexp>()); assert!(offset_of!(NlRecord, refcount) == size_of::<Sexp>() + size_of::<Vec<Sexp>>()); }
);
impl NlRecord { crate::nlinner_with_mut!(with_slots_mut, slots, Vec<Sexp>); }
impl std::fmt::Debug for NlRecordRef { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_struct("Record").field("type_tag", &self.type_tag).field("slots", &self.slots).finish() } }
impl PartialEq for NlRecordRef { fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || (self.type_tag == other.type_tag && self.slots == other.slots) } }
#[no_mangle] pub unsafe extern "C" fn nl_record_set_slot(record: *mut NlRecord, n: usize, val: *const Sexp) { (&mut (*record).slots)[n] = (*val).clone(); }
