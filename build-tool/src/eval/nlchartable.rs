use crate::eval::sexp::{CharTableInner, Sexp}; use crate::jit::{TRAMPOLINE_ERR, TRAMPOLINE_OK};
crate::define_nlbox!(inner=NlCharTable, ref_ty=NlCharTableRef, fields={inner: CharTableInner},
    clone_fn=crate::elisp_cc_spike::nlchartable_clone, drop_fn=crate::elisp_cc_spike::nlchartable_drop,
    layout_asserts={ assert!(offset_of!(NlCharTable, inner) == 0); assert!(offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>()); });
impl NlCharTable { crate::nlinner_with_mut!(with_inner_mut, inner, CharTableInner); } impl std::fmt::Debug for NlCharTableRef { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_struct("CharTable").field("inner", &self.inner).finish() } }
impl PartialEq for NlCharTableRef { fn eq(&self, other: &Self) -> bool { Self::ptr_eq(self, other) || self.inner == other.inner } }
fn ct_set(i: &mut CharTableInner, c: i64, v: Sexp) { match i.entries.iter_mut().find(|(k,_)| *k==c) { Some(e) => e.1=v, None => i.entries.push((c,v)) } } fn ct_get(rc: &NlCharTableRef, c: i64) -> Sexp { rc.inner.entries.iter().find(|(k,_)| *k==c).map(|(_,v)| v.clone()).or_else(|| rc.inner.parent.as_ref().map(|p| ct_get(p,c))).unwrap_or_else(|| rc.inner.default_val.clone()) }
#[no_mangle] pub unsafe extern "C" fn nl_char_table_get_raw(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 { let r = match &*arg { Sexp::CharTable(r) => r, _ => return TRAMPOLINE_ERR }; *out = ct_get(r, idx); TRAMPOLINE_OK }
#[no_mangle] pub unsafe extern "C" fn nl_char_table_set_raw(arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp) -> i64 { let r = match &*arg { Sexp::CharTable(r) => r, _ => return TRAMPOLINE_ERR }; r.with_inner_mut(|i| ct_set(i, idx, (*val).clone())); *out = (*val).clone(); TRAMPOLINE_OK }
