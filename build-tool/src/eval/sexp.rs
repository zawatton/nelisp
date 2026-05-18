//! S-expression value type — eval-owned since Phase 8 Stage 8.1 (Doc 73).
//! See `reader/mod.rs' for the parser and `eval/mod.rs' for the evaluator.

use crate::eval::nlboolvector::NlBoolVectorRef;
use crate::eval::nlcell::NlCellRef;
use crate::eval::nlchartable::NlCharTableRef;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::nlrecord::NlRecordRef;
use crate::eval::nlstr::NlStrRef;
use crate::eval::nlvector::NlVectorRef;
use std::fmt;

/// A parsed s-expression.  `#[repr(C, u8)]` puts the tag byte at offset 0
/// + 8-byte-aligned payload (JIT reads the tag inline; `SEXP_TAG_*` are
/// stable — variants must only be appended).
#[derive(Debug, Clone, PartialEq)]
#[repr(C, u8)]
pub enum Sexp {
    Nil,
    T,
    Int(i64),
    Float(f64),
    Symbol(String),
    Str(String),
    /// Mutable string buffer (= `make-string' / `aset'-able), refcount-shared.
    MutStr(NlStrRef),
    Cons(NlConsBoxRef),
    Vector(NlVectorRef),
    /// Maps integer codepoints to Sexp (syntax/category/case/display tables).
    CharTable(NlCharTableRef),
    BoolVector(NlBoolVectorRef),
    /// Write-through cell for let-binding closure `setq' (Env::capture_lexical).
    Cell(NlCellRef),
    /// Host emacs `record' / `cl-defstruct': slot 0 = type_tag, rest = user slots.
    Record(NlRecordRef),
}

// Tag constants — mirror enum declaration order; JIT depends on stability.

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

/// Read the discriminant byte (offset 0).  Matches one of `SEXP_TAG_*'.
#[inline]
pub fn variant_tag(s: &Sexp) -> u8 {
    // SAFETY: `#[repr(C, u8)]' puts the discriminant as a `u8' at
    // offset 0; reading through `*const u8' is sound.
    unsafe { *(s as *const Sexp as *const u8) }
}

/// Byte offset of the boxed `NonNull<NlXxx>` handle within a Sexp.
pub const SEXP_PAYLOAD_OFFSET: usize = 8;

/// Emit `unsafe fn $name(&self) -> *const $ty` loading the box ptr at
/// offset 8.  Caller must match tag; wrong-variant access is UB.
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
    /// Read the discriminant byte (offset 0); spelled as a method for
    /// trampoline ergonomics.  Equivalent to [`variant_tag`].
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

// Compile-time check: every NlXxxRef must be pointer-sized (8 bytes
// on 64-bit) so the payload offset stays at 8.
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

/// Doc 111 §111.C — refcount-aware Sexp slot copy.  Clones `*src` and
/// writes the result to `*dst` without dropping prior contents.
///
/// # Safety
/// - `src` must point at an initialized `Sexp` valid for the call.
/// - `dst` must be a writable 32-byte `Sexp` slot treated as *uninit*
///   (caller must drop prior contents first, or `*dst` was `Nil`).
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_clone_into(src: *const Sexp, dst: *mut Sexp) {
    let cloned: Sexp = unsafe { (*src).clone() };
    unsafe { core::ptr::write(dst, cloned); }
}

/// Inner storage for [`Sexp::CharTable`].  Sparse linear-scan; typical
/// entry count < 256 (ASCII range).  Parent chain holds
/// [`NlCharTableRef`] handles for refcount-tracked self-reference.
#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct CharTableInner {
    /// Subtype symbol (e.g., `syntax-table', `display-table').
    pub subtype: Sexp,
    /// Default value returned for chars not in `entries`.
    pub default_val: Sexp,
    /// Sparse char → value map.  Linear scan on lookup.
    pub entries: Vec<(i64, Sexp)>,
    /// Optional parent — lookups falling through `entries' consult it.
    pub parent: Option<NlCharTableRef>,
    /// Per-table extra slots (= `char-table-extra-slot').
    pub extra: Vec<Sexp>,
}

impl Sexp {
    /// Build a proper list from a slice of values (empty → `Nil`).
    pub fn list_from(items: &[Sexp]) -> Sexp {
        let mut acc = Sexp::Nil;
        for item in items.iter().rev() {
            acc = Sexp::cons(item.clone(), acc);
        }
        acc
    }

    /// Build a cons cell — single layout-pinned [`NlConsBoxRef`].
    pub fn cons(car: Sexp, cdr: Sexp) -> Sexp {
        Sexp::Cons(NlConsBoxRef::new(car, cdr))
    }

    /// Build a vector Sexp from an owned `Vec<Sexp>`.
    pub fn vector(items: Vec<Sexp>) -> Sexp {
        Sexp::Vector(NlVectorRef::new(items))
    }

    /// Build a mutable string Sexp (= `make-string' aset-able backing).
    pub fn mut_str(s: impl Into<String>) -> Sexp {
        Sexp::MutStr(NlStrRef::new(s.into()))
    }

    /// Build an empty char-table with SUBTYPE / INIT default.
    pub fn char_table(subtype: Sexp, init: Sexp) -> Sexp {
        Sexp::CharTable(NlCharTableRef::new(CharTableInner {
            subtype,
            default_val: init,
            entries: Vec::new(),
            parent: None,
            extra: Vec::new(),
        }))
    }

    /// Build a bool-vector of LEN bits all initialised to INIT.
    pub fn bool_vector(len: usize, init: bool) -> Sexp {
        Sexp::BoolVector(NlBoolVectorRef::new(vec![init; len]))
    }

    /// Return Str/MutStr content as owned String, else None.
    pub fn as_string_owned(&self) -> Option<String> {
        match self {
            Sexp::Str(s) => Some(s.clone()),
            Sexp::MutStr(s) => Some(s.value.clone()),
            _ => None,
        }
    }

    /// Build a record with TYPE_TAG and INIT slot vector.
    pub fn record(type_tag: Sexp, init: Vec<Sexp>) -> Sexp {
        Sexp::Record(NlRecordRef::new(type_tag, init))
    }

    /// Wrap a form in `(quote <form>)` (= desugaring of `'x`).
    pub fn quote(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("quote".to_string()), inner])
    }

    /// Wrap a form in `(backquote <form>)`.
    pub fn backquote(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("backquote".to_string()), inner])
    }

    /// Wrap a form in `(comma <form>)`.
    pub fn comma(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("comma".to_string()), inner])
    }

    /// Wrap a form in `(comma-at <form>)`.
    pub fn comma_at(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("comma-at".to_string()), inner])
    }

    /// Wrap a form in `(function <form>)` (= desugaring of `#'x`).
    pub fn function(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("function".to_string()), inner])
    }
}

/// Pretty-printer used by debug + tests.  Not the evaluator's `prin1`;
/// intentionally lossy where Elisp would diverge (e.g., no round-trip
/// guarantee for floats with no fractional part).
pub fn fmt_sexp(s: &Sexp) -> String {
    let mut out = String::new();
    write_sexp(&mut out, s);
    out
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&fmt_sexp(self))
    }
}

/// Push `"text"` with backslash escapes for `"`/`\`/`\n`/`\t`/`\r`.
fn write_quoted_string(out: &mut String, text: &str) {
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c => out.push(c),
        }
    }
    out.push('"');
}

fn write_sexp(out: &mut String, s: &Sexp) {
    if write_reader_macro(out, s) {
        return;
    }
    match s {
        Sexp::Nil => out.push_str("nil"),
        Sexp::T => out.push('t'),
        Sexp::Int(n) => out.push_str(&n.to_string()),
        Sexp::Float(x) => {
            // Keep a decimal point so round-trip does not coerce to Int.
            let s = format!("{}", x);
            if s.contains('.') || s.contains('e') || s.contains('E') || s == "inf" || s == "-inf"
                || s == "NaN"
            {
                out.push_str(&s);
            } else {
                out.push_str(&s);
                out.push_str(".0");
            }
        }
        Sexp::Symbol(name) => out.push_str(name),
        Sexp::Str(text) => write_quoted_string(out, text),
        Sexp::MutStr(rc) => write_quoted_string(out, &rc.value),
        Sexp::Cons(_) => {
            out.push('(');
            write_list_body(out, s);
            out.push(')');
        }
        Sexp::Vector(items) => {
            out.push('[');
            for (i, item) in items.value.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                write_sexp(out, item);
            }
            out.push(']');
        }
        Sexp::CharTable(rc) => {
            // Compact printer — emit subtype, default, populated count.
            let inner = &rc.inner;
            out.push_str("#<char-table");
            if !matches!(inner.subtype, Sexp::Nil) {
                out.push(' ');
                write_sexp(out, &inner.subtype);
            }
            out.push_str(" default=");
            write_sexp(out, &inner.default_val);
            out.push_str(" entries=");
            out.push_str(&inner.entries.len().to_string());
            out.push('>');
        }
        Sexp::BoolVector(rc) => {
            let v = &rc.value;
            out.push_str("#&");
            out.push_str(&v.len().to_string());
            out.push('"');
            // Pack 8 bits per char (= upstream bool-vector printer shape,
            // human inspection only — no round-trip guarantee).
            for chunk in v.chunks(8) {
                let mut byte = 0u8;
                for (i, &b) in chunk.iter().enumerate() {
                    if b {
                        byte |= 1 << i;
                    }
                }
                if byte == b'"' || byte == b'\\' || byte < 0x20 || byte >= 0x7F {
                    out.push_str(&format!("\\{:03o}", byte));
                } else {
                    out.push(byte as char);
                }
            }
            out.push('"');
        }
        // Cell — print inner value transparently.
        Sexp::Cell(c) => write_sexp(out, &c.value),
        Sexp::Record(rec) => {
            // Round-trippable positional shape `#s(TYPE V0 V1 ...)'.
            out.push_str("#s(");
            write_sexp(out, &rec.type_tag);
            for v in rec.slots.iter() {
                out.push(' ');
                write_sexp(out, v);
            }
            out.push(')');
        }
    }
}

fn write_reader_macro(out: &mut String, s: &Sexp) -> bool {
    let Some((head, arg)) = list_tag_and_arg(s) else {
        return false;
    };
    let prefix = match head.as_str() {
        "quote" => "'",
        "backquote" => "`",
        "comma" => ",",
        "comma-at" => ",@",
        "function" => "#'",
        _ => return false,
    };
    out.push_str(prefix);
    write_sexp(out, &arg);
    true
}

/// Recognise `(SYMBOL ARG)' 2-element list; return cloned (TAG, ARG).
fn list_tag_and_arg(s: &Sexp) -> Option<(String, Sexp)> {
    match s {
        Sexp::Cons(b) => match (&b.car, &b.cdr) {
            (Sexp::Symbol(tag), Sexp::Cons(rest)) if matches!(&rest.cdr, Sexp::Nil) => {
                Some((tag.clone(), rest.car.clone()))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Walk a (possibly improper) list, printing body without enclosing
/// parens.  Non-`Nil` final cdr renders as ` . tail'.
fn write_list_body(out: &mut String, s: &Sexp) {
    let mut cur: Sexp = s.clone();
    let mut first = true;
    loop {
        let next = match &cur {
            Sexp::Cons(b) => {
                if !first {
                    out.push(' ');
                }
                first = false;
                write_sexp(out, &b.car);
                b.cdr.clone()
            }
            Sexp::Nil => return,
            other => {
                out.push_str(" . ");
                write_sexp(out, other);
                return;
            }
        };
        cur = next;
    }
}
