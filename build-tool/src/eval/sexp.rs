//! Eval-owned s-expression value type.

use crate::eval::nlboolvector::NlBoolVectorRef;
use crate::eval::nlcell::NlCellRef;
use crate::eval::nlchartable::NlCharTableRef;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::nlrecord::NlRecordRef;
use crate::eval::nlstr::NlStrRef;
use crate::eval::nlvector::NlVectorRef;
use std::fmt;

/// Parsed s-expression. `#[repr(C, u8)]` keeps the tag at offset 0; `SEXP_TAG_*`
/// are ABI and variants must only be appended.
#[derive(Debug, Clone, PartialEq)]
#[repr(C, u8)]
pub enum Sexp {
    Nil,
    T,
    Int(i64),
    Float(f64),
    Symbol(String),
    Str(String),
    /// Mutable string buffer.
    MutStr(NlStrRef),
    Cons(NlConsBoxRef),
    Vector(NlVectorRef),
    /// Integer codepoint map.
    CharTable(NlCharTableRef),
    BoolVector(NlBoolVectorRef),
    /// Write-through lexical cell.
    Cell(NlCellRef),
    /// Host `record' / `cl-defstruct`.
    Record(NlRecordRef),
}

// Tag constants mirror enum declaration order.

pub const SEXP_TAG_NIL: u8 = 0;
pub const SEXP_TAG_T: u8 = 1;
pub const SEXP_TAG_INT: u8 = 2;
pub const SEXP_TAG_FLOAT: u8 = 3;
pub const SEXP_TAG_SYMBOL: u8 = 4;
pub const SEXP_TAG_STR: u8 = 5;
pub const SEXP_TAG_MUT_STR: u8 = 6;
pub const SEXP_TAG_CONS: u8 = 7;
pub const SEXP_TAG_VECTOR: u8 = 8;
pub const SEXP_TAG_CHAR_TABLE: u8 = 9;
pub const SEXP_TAG_BOOL_VECTOR: u8 = 10;
pub const SEXP_TAG_CELL: u8 = 11;
pub const SEXP_TAG_RECORD: u8 = 12;

/// Read the discriminant byte.
#[inline]
pub fn variant_tag(s: &Sexp) -> u8 {
    // SAFETY: `#[repr(C, u8)]` stores the discriminant as a `u8` at offset 0.
    unsafe { *(s as *const Sexp as *const u8) }
}

/// Byte offset of the boxed handle within a `Sexp`.
pub const SEXP_PAYLOAD_OFFSET: usize = 8;

/// Emit `unsafe fn $name(&self) -> *const $ty` loading the box ptr at offset 8.
macro_rules! sexp_box_ptr_accessor {
    ($name:ident, $ty:ty) => {
        #[inline]
        pub unsafe fn $name(&self) -> *const $ty {
            let payload = (self as *const Sexp as *const u8).add(SEXP_PAYLOAD_OFFSET)
                as *const std::ptr::NonNull<$ty>;
            unsafe { (*payload).as_ptr() }
        }
    };
}

impl Sexp {
    /// Equivalent to [`variant_tag`].
    #[inline]
    pub fn tag(&self) -> u8 {
        variant_tag(self)
    }

    sexp_box_ptr_accessor!(cons_box_ptr, crate::eval::nlconsbox::NlConsBox);
    sexp_box_ptr_accessor!(cell_box_ptr, crate::eval::nlcell::NlCell);
    sexp_box_ptr_accessor!(mut_str_box_ptr, crate::eval::nlstr::NlStr);
    sexp_box_ptr_accessor!(vector_box_ptr, crate::eval::nlvector::NlVector);
    sexp_box_ptr_accessor!(bool_vector_box_ptr, crate::eval::nlboolvector::NlBoolVector);
    sexp_box_ptr_accessor!(record_box_ptr, crate::eval::nlrecord::NlRecord);
    sexp_box_ptr_accessor!(char_table_box_ptr, crate::eval::nlchartable::NlCharTable);
}

// Every NlXxxRef must stay pointer-sized so payload remains at offset 8.
const _: () = {
    use std::mem::size_of;
    assert!(size_of::<crate::eval::nlconsbox::NlConsBoxRef>() == 8);
    assert!(size_of::<crate::eval::nlcell::NlCellRef>() == 8);
    assert!(size_of::<crate::eval::nlstr::NlStrRef>() == 8);
    assert!(size_of::<crate::eval::nlvector::NlVectorRef>() == 8);
    assert!(size_of::<crate::eval::nlboolvector::NlBoolVectorRef>() == 8);
    assert!(size_of::<crate::eval::nlrecord::NlRecordRef>() == 8);
    assert!(size_of::<crate::eval::nlchartable::NlCharTableRef>() == 8);
};

/// Clone `*src` into `*dst` without dropping prior contents.
///
/// # Safety
/// - `src` must point at an initialized `Sexp` valid for the call.
/// - `dst` must be a writable 32-byte `Sexp` slot treated as *uninit*
///   (caller must drop prior contents first, or `*dst` was `Nil`).
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_clone_into(src: *const Sexp, dst: *mut Sexp) {
    let cloned: Sexp = unsafe { (*src).clone() };
    unsafe {
        core::ptr::write(dst, cloned);
    }
}

/// Inner storage for [`Sexp::CharTable`].
#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct CharTableInner {
    /// Subtype symbol.
    pub subtype: Sexp,
    /// Default value.
    pub default_val: Sexp,
    /// Sparse char -> value map.
    pub entries: Vec<(i64, Sexp)>,
    /// Optional parent.
    pub parent: Option<NlCharTableRef>,
    /// Extra slots.
    pub extra: Vec<Sexp>,
}

impl Sexp {
    pub fn list_from(items: &[Sexp]) -> Sexp {
        let mut acc = Sexp::Nil;
        for item in items.iter().rev() {
            acc = Sexp::cons(item.clone(), acc);
        }
        acc
    }

    pub fn cons(car: Sexp, cdr: Sexp) -> Sexp {
        Sexp::Cons(NlConsBoxRef::new(car, cdr))
    }

    pub fn vector(items: Vec<Sexp>) -> Sexp {
        Sexp::Vector(NlVectorRef::new(items))
    }

    pub fn mut_str(s: impl Into<String>) -> Sexp {
        Sexp::MutStr(NlStrRef::new(s.into()))
    }

    pub fn char_table(subtype: Sexp, init: Sexp) -> Sexp {
        Sexp::CharTable(NlCharTableRef::new(CharTableInner {
            subtype,
            default_val: init,
            entries: Vec::new(),
            parent: None,
            extra: Vec::new(),
        }))
    }

    pub fn bool_vector(len: usize, init: bool) -> Sexp {
        Sexp::BoolVector(NlBoolVectorRef::new(vec![init; len]))
    }

    pub fn as_string_owned(&self) -> Option<String> {
        match self {
            Sexp::Str(s) => Some(s.clone()),
            Sexp::MutStr(s) => Some(s.value.clone()),
            _ => None,
        }
    }

    pub fn record(type_tag: Sexp, init: Vec<Sexp>) -> Sexp {
        Sexp::Record(NlRecordRef::new(type_tag, init))
    }
}

/// Pretty-printer used by debug and tests.
/// Body replaced by Phase 47 elisp kernel `nelisp_fmt_sexp' (Doc 122 §122.J).
pub fn fmt_sexp(s: &Sexp) -> String {
    let mut slot = Sexp::Nil;
    unsafe { crate::elisp_cc_spike::fmt_sexp_call(s as *const Sexp, &mut slot) };
    match slot {
        Sexp::Str(text) => text,
        _ => String::from("<fmt_sexp:error>"),
    }
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&fmt_sexp(self))
    }
}
