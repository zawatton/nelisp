use crate::eval::sexp::Sexp;
crate::define_nlbox!(
    inner=NlCell, ref_ty=NlCellRef, fields={value: Sexp},
    clone_fn=crate::elisp_cc_spike::nlcell_clone, drop_fn=crate::elisp_cc_spike::nlcell_drop,
    layout_asserts={
        use ::std::mem::{offset_of, size_of};
        assert!(offset_of!(NlCell, value) == 0);
        assert!(offset_of!(NlCell, refcount) == size_of::<Sexp>());
        assert!(size_of::<AtomicUsize>() == 8);
    }
);
impl NlCell { crate::nlinner_set!(set_value, value, Sexp); }
impl NlCellRef {
    pub unsafe fn from_raw_ptr(raw: *mut NlCell) -> NlCellRef { NlCellRef { ptr: ::std::ptr::NonNull::new(raw).expect("null"), _marker: ::std::marker::PhantomData } }
}
impl std::fmt::Debug for NlCellRef { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_tuple("Cell").field(&self.value).finish() } }
impl PartialEq for NlCellRef { fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || self.value == other.value } }
#[allow(improper_ctypes)]
extern "C" { pub fn nl_alloc_cell(initial: *const Sexp) -> *mut NlCell; }
#[no_mangle] pub unsafe extern "C" fn nl_cell_set_value(cell: *mut NlCell, val: *const Sexp) { (*cell).set_value((*val).clone()); }
