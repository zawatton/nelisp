use crate::eval::nlboolvector::NlBoolVectorRef;
use crate::eval::nlcell::{NlCell, NlCellRef};
use crate::eval::nlchartable::NlCharTableRef;
use crate::eval::nlconsbox::{NlConsBox, NlConsBoxRef};
use crate::eval::nlrecord::{NlRecord, NlRecordRef};
use crate::eval::nlstr::NlStrRef;
use crate::eval::nlvector::{NlVector, NlVectorRef};
use std::fmt;
#[derive(Debug, Clone, PartialEq)]
#[repr(C, u8)]
pub enum Sexp {
    Nil,
    T,
    Int(i64),
    Float(f64),
    Symbol(String),
    Str(String),
    MutStr(NlStrRef),
    Cons(NlConsBoxRef),
    Vector(NlVectorRef),
    CharTable(NlCharTableRef),
    BoolVector(NlBoolVectorRef),
    Cell(NlCellRef),
    Record(NlRecordRef),
}
macro_rules! sexp_tags {
    ($($name:ident = $val:expr;)*) => { $(pub const $name: u8 = $val;)* }
}
sexp_tags! {
    SEXP_TAG_NIL=0; SEXP_TAG_T=1; SEXP_TAG_INT=2; SEXP_TAG_FLOAT=3;
    SEXP_TAG_SYMBOL=4; SEXP_TAG_STR=5; SEXP_TAG_MUT_STR=6; SEXP_TAG_CONS=7;
    SEXP_TAG_VECTOR=8; SEXP_TAG_CHAR_TABLE=9; SEXP_TAG_BOOL_VECTOR=10;
    SEXP_TAG_CELL=11; SEXP_TAG_RECORD=12;
}
pub const SEXP_PAYLOAD_OFFSET: usize = 8;
macro_rules! sexp_box_ptr_accessor {
    ($name:ident, $ty:ty) => {
        #[inline] pub unsafe fn $name(&self) -> *const $ty {
            let payload = (self as *const Sexp as *const u8).add(SEXP_PAYLOAD_OFFSET) as *const std::ptr::NonNull<$ty>;
            unsafe { (*payload).as_ptr() } } };
}
impl Sexp {
    #[inline] pub fn tag(&self) -> u8 { unsafe { *(self as *const Sexp as *const u8) } }
    sexp_box_ptr_accessor!(cons_box_ptr, crate::eval::nlconsbox::NlConsBox); sexp_box_ptr_accessor!(cell_box_ptr, crate::eval::nlcell::NlCell);
    sexp_box_ptr_accessor!(mut_str_box_ptr, crate::eval::nlstr::NlStr); sexp_box_ptr_accessor!(vector_box_ptr, crate::eval::nlvector::NlVector);
    sexp_box_ptr_accessor!(bool_vector_box_ptr, crate::eval::nlboolvector::NlBoolVector); sexp_box_ptr_accessor!(record_box_ptr, crate::eval::nlrecord::NlRecord);
    sexp_box_ptr_accessor!(char_table_box_ptr, crate::eval::nlchartable::NlCharTable);
}
const _: () = { use std::mem::size_of; use crate::eval::*;
    assert!(size_of::<nlconsbox::NlConsBoxRef>()==8); assert!(size_of::<nlcell::NlCellRef>()==8);
    assert!(size_of::<nlstr::NlStrRef>()==8); assert!(size_of::<nlvector::NlVectorRef>()==8);
    assert!(size_of::<nlboolvector::NlBoolVectorRef>()==8); assert!(size_of::<nlrecord::NlRecordRef>()==8);
    assert!(size_of::<nlchartable::NlCharTableRef>()==8); };
#[no_mangle] pub unsafe extern "C" fn nl_sexp_clone_into(src: *const Sexp, dst: *mut Sexp) { core::ptr::write(dst, (*src).clone()); }
#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct CharTableInner {
    pub subtype: Sexp, pub default_val: Sexp,
    pub entries: Vec<(i64, Sexp)>, pub parent: Option<NlCharTableRef>, pub extra: Vec<Sexp>,
}
impl Sexp {
    pub fn list_from(items: &[Sexp]) -> Sexp {
        let mut acc = Sexp::Nil; for item in items.iter().rev() { acc = Sexp::cons(item.clone(), acc); } acc }
    pub fn cons(car: Sexp, cdr: Sexp) -> Sexp { Sexp::Cons(NlConsBoxRef::new(car, cdr)) }
    pub fn vector(items: Vec<Sexp>) -> Sexp { Sexp::Vector(NlVectorRef::new(items)) }
    pub fn mut_str(s: impl Into<String>) -> Sexp { Sexp::MutStr(NlStrRef::new(s.into())) }
    pub fn char_table(subtype: Sexp, init: Sexp) -> Sexp { Sexp::CharTable(NlCharTableRef::new(CharTableInner { subtype, default_val: init, entries: Vec::new(), parent: None, extra: Vec::new() })) }
    pub fn bool_vector(len: usize, init: bool) -> Sexp { Sexp::BoolVector(NlBoolVectorRef::new(vec![init; len])) }
    pub fn as_string_owned(&self) -> Option<String> { match self { Sexp::Str(s) => Some(s.clone()), Sexp::MutStr(s) => Some(s.value.clone()), _ => None } }
    pub fn record(type_tag: Sexp, init: Vec<Sexp>) -> Sexp { Sexp::Record(NlRecordRef::new(type_tag, init)) }
}
const NLREC_SLOTS: usize = std::mem::offset_of!(NlRecord, slots);
const NLVEC_VALUE: usize = std::mem::offset_of!(NlVector, value);
pub const ABI_EXPORT: &[(&str, i64)] = &[
    ("tag-nil",SEXP_TAG_NIL as i64), ("tag-t",SEXP_TAG_T as i64), ("tag-int",SEXP_TAG_INT as i64), ("tag-float",SEXP_TAG_FLOAT as i64), ("tag-symbol",SEXP_TAG_SYMBOL as i64), ("tag-str",SEXP_TAG_STR as i64),
    ("tag-mut-str",SEXP_TAG_MUT_STR as i64), ("tag-cons",SEXP_TAG_CONS as i64), ("tag-vector",SEXP_TAG_VECTOR as i64), ("tag-char-table",SEXP_TAG_CHAR_TABLE as i64), ("tag-bool-vector",SEXP_TAG_BOOL_VECTOR as i64), ("tag-cell",SEXP_TAG_CELL as i64), ("tag-record",SEXP_TAG_RECORD as i64),
    ("offset-tag",0), ("offset-payload",SEXP_PAYLOAD_OFFSET as i64), ("slot-size",std::mem::size_of::<Sexp>() as i64),
    ("nlconsbox-offset-car",std::mem::offset_of!(NlConsBox,car) as i64), ("nlconsbox-offset-cdr",std::mem::offset_of!(NlConsBox,cdr) as i64), ("nlconsbox-offset-refcount",std::mem::offset_of!(NlConsBox,refcount) as i64), ("nlconsbox-size",std::mem::size_of::<NlConsBox>() as i64),
    ("string-offset-capacity",SEXP_PAYLOAD_OFFSET as i64), ("string-offset-ptr",(SEXP_PAYLOAD_OFFSET+8) as i64), ("string-offset-length",(SEXP_PAYLOAD_OFFSET+16) as i64), ("string-header-size",std::mem::size_of::<String>() as i64),
    ("nlrecord-offset-type-tag",std::mem::offset_of!(NlRecord,type_tag) as i64), ("nlrecord-offset-slots-vec",NLREC_SLOTS as i64), ("nlrecord-offset-slots-ptr",NLREC_SLOTS as i64), ("nlrecord-offset-slots-capacity",(NLREC_SLOTS+8) as i64), ("nlrecord-offset-slots-length",(NLREC_SLOTS+16) as i64), ("nlrecord-offset-refcount",std::mem::offset_of!(NlRecord,refcount) as i64), ("nlrecord-size",std::mem::size_of::<NlRecord>() as i64),
    ("nlvector-offset-value-vec",NLVEC_VALUE as i64), ("nlvector-offset-value-ptr",NLVEC_VALUE as i64), ("nlvector-offset-value-capacity",(NLVEC_VALUE+8) as i64), ("nlvector-offset-value-length",(NLVEC_VALUE+16) as i64), ("nlvector-offset-refcount",std::mem::offset_of!(NlVector,refcount) as i64), ("nlvector-size",std::mem::size_of::<NlVector>() as i64),
    ("nlcell-offset-value",std::mem::offset_of!(NlCell,value) as i64), ("nlcell-offset-refcount",std::mem::offset_of!(NlCell,refcount) as i64), ("nlcell-size",std::mem::size_of::<NlCell>() as i64),
];
pub fn fmt_sexp(s: &Sexp) -> String {
    let mut slot = Sexp::Nil; unsafe { crate::elisp_cc_spike::fmt_sexp_call(s as *const Sexp, &mut slot) };
    match slot { Sexp::Str(text) => text, _ => "<fmt_sexp:error>".into() } }
impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.write_str(&fmt_sexp(self)) }
}
