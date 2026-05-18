//! S-expression value type — *eval-owned* since Phase 8 Stage 8.1
//! (Doc 73, 2026-05-08).  Originally lived in `reader/sexp.rs' but
//! the reader is now parse-only and eval owns the value type that
//! flows through the entire interpreter (Doc 73 §2.1).  History
//! preserved via `git mv'.
//!
//! Doc 44 §3.2 LOCKED scope subset:
//!   - integer (signed/unsigned, decimal/hex/octal)
//!   - float (with exponent)
//!   - string (= "..." with backslash escapes \n \t \\ \")
//!   - symbol (= alphanumeric + - _ . :)
//!   - cons cell (= (a . b))
//!   - list (= (a b c) → (a . (b . (c . nil))))
//!   - vector (= [a b c])
//!   - nil + t literals
//!   - quote-family prefixes (`'x`, `` `x ``, `,x`, `,@x`, `#'x`)
//!
//! Deferred:
//!   - meta char literals (?\\M-a, multi-modifier combinations)
//!   - sharpsign read forms (#[...] byte-code, #s structure)
//!   - multibyte / non-ASCII string literal handling
//!
//! The enum is intentionally NOT a Lisp_Object yet — that is the
//! evaluator's concern (Phase 7.5.4.2 = next sub-phase).  The reader
//! must NOT depend on the evaluator (= layer separation, see prompt
//! constraints).

use crate::eval::nlboolvector::NlBoolVectorRef;
use crate::eval::nlcell::NlCellRef;
use crate::eval::nlchartable::NlCharTableRef;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::nlrecord::NlRecordRef;
use crate::eval::nlstr::NlStrRef;
use crate::eval::nlvector::NlVectorRef;
use std::fmt;

/// A parsed s-expression.  The variants form the minimal value
/// universe that the Phase 7.5.4.1 reader can produce; the evaluator
/// (Phase 7.5.4.2) will widen this with `Lambda`, `Macro`, `Function`
/// etc.
///
/// `Cons` is intentionally `Box`ed so the enum stays a fixed size and
/// list traversal is O(n) without a recursion-blow risk on the *enum*
/// itself.  Deeply nested forms are still bounded by parser recursion
/// depth which is checked separately.
///
/// **Layout** (Doc 62 Phase 5 inline-emit unlock, 2026-05-07): `#[repr(C, u8)]`
/// pins the discriminant as a single `u8` at offset 0, followed by the
/// variant payload at the next 8-byte aligned offset.  This lets the
/// JIT (build-tool/src/jit/) read the variant tag inline (`movzx tag,
/// byte ptr [rdi]`) without going through a `match` helper.  The tag
/// values are kept stable via the `SEXP_TAG_*` constants below;
/// re-ordering variants would invalidate compiled JIT code, so any
/// future variant must be appended at the end (or guarded by a JIT
/// recompile).
#[derive(Debug, Clone, PartialEq)]
#[repr(C, u8)]
pub enum Sexp {
    /// `nil` literal — also the empty list `()`.
    Nil,
    /// `t` literal.
    T,
    /// 64-bit signed integer.  Per Doc 44 §3.2 the bootstrap subset
    /// only needs fixnum-width integers; bignum promotion is deferred
    /// to Phase 7.5.4.2 with the rest of the evaluator's numeric
    /// tower.
    Int(i64),
    /// IEEE-754 double.
    Float(f64),
    /// Interned-style symbol string.  The reader does NOT intern; the
    /// evaluator will own the obarray (Doc 44 §4.2 case 1 = inject at
    /// takeover).
    Symbol(String),
    /// String literal.  Stored as a Rust `String` for now; multibyte
    /// handling is deferred (Phase 7.5.4.2 + Phase 7.4 NeLisp coding).
    ///
    /// Immutable in the sense that `aset' refuses to mutate it; the
    /// reader produces this variant for `"text"' literals.  Functions
    /// that need to return mutable strings (e.g. `make-string',
    /// `copy-sequence' on a mutable string) yield [`Sexp::MutStr`]
    /// instead.
    Str(String),
    /// Mutable string buffer.  Returned by `make-string' so substrate
    /// code (= `nelisp-text-buffer''s gap-buffer mgmt) can do
    /// `(aset BYTES I N)' to fill in raw bytes.  Clone is a cheap
    /// refcount bump on the underlying [`NlStrRef`]; mutation through
    /// any alias is shared.  Identity comparison goes through
    /// `NlStrRef::ptr_eq'; structural equality (the derived
    /// `PartialEq') unwraps the inner `String'.
    ///
    /// Predicates such as `stringp' / `arrayp' treat `MutStr' the
    /// same as `Str'; printers, equality, and format conversions all
    /// share the helper [`Sexp::as_string_owned`].
    ///
    MutStr(NlStrRef),
    /// Cons cell.  Lists are right-leaning `Cons' chains terminated by
    /// `Nil'; dotted pairs `(a . b)' leave cdr as any non-Nil value.
    /// Backed by [`NlConsBoxRef`] (layout-pinned, refcount-shared);
    /// `setcar' / `setcdr' mutate through all aliases.
    Cons(NlConsBoxRef),
    /// `[a b c]' vector literal.  Backed by [`NlVectorRef`].
    Vector(NlVectorRef),
    // Sexp::HashTable variant retired in Doc 50 stage 4f (2026-05-07).
    // Hash-tables are now `(record 'hash-table TEST ENTRIES)' built
    // on top of the Stage 4c record primitives — see
    // lisp/nelisp-stdlib-hash.el for the elisp implementation.
    /// Char-table — maps integer codepoints to Sexp values.  Used by
    /// syntax-table / category-table / case-table / display-table.
    /// Sparse linear-scan; `parent' is `Option<NlCharTableRef>' (no
    /// cycle API — only the child installs).  Backed by [`NlCharTableRef`].
    CharTable(NlCharTableRef),
    /// Bool-vector — packed boolean array.  `aref' returns t/nil;
    /// `aset' stores truthy/falsy; `length' returns the bit count.
    BoolVector(NlBoolVectorRef),
    /// Mutable write-through cell.  Backs let-binding storage so
    /// closure `setq' mutates the captured slot.  Reader doesn't
    /// produce this; only `Env::capture_lexical' does.
    Cell(NlCellRef),
    /// Record (host emacs `record' / pvec subtype).  Underlies
    /// `cl-defstruct'; slot 0 is `type_tag', remaining are user slots.
    /// Printer round-trips as `#s(TYPE V0 V1 ...)' (positional).
    Record(NlRecordRef),
}

// ---------------------------------------------------------------------------
// Sexp variant tag constants (Doc 62 Phase 5 inline-emit unlock).
//
// These mirror the declaration order of the `Sexp' enum above and are
// the values stored in the `#[repr(C, u8)]' discriminant byte at offset
// 0 of every Sexp value.  The `variant_tags_are_stable' unit test in
// the `mod tests' block below asserts each constant matches what
// `variant_tag(&Sexp::FOO)' actually returns.
//
// JIT code (build-tool/src/jit/) is allowed to depend on these values
// being stable — re-ordering variants WILL invalidate compiled JIT
// modules.  When adding a new variant, append it at the end of the
// enum and the constant list, and add a corresponding assertion in
// the unit test.
// ---------------------------------------------------------------------------

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

/// Read the discriminant byte (offset 0) of a Sexp value.  Stable per
/// `#[repr(C, u8)]'; matches one of the `SEXP_TAG_*' constants above.
#[inline]
pub fn variant_tag(s: &Sexp) -> u8 {
    // SAFETY: `#[repr(C, u8)]' guarantees the discriminant is a `u8'
    // at offset 0 of the enum value.  We read it through a `*const u8'
    // cast which is well-defined as long as the cast does not extend
    // past the enum's first byte.
    unsafe { *(s as *const Sexp as *const u8) }
}

// ---------------------------------------------------------------------------
// Sexp ABI direct-access helpers (Doc 77c Phase A.5).
//
// `#[repr(C, u8)]` lays out a Sexp as { tag: u8, _pad: [u8; 7], payload: T }
// where `T` is the variant payload, aligned to the largest variant's
// alignment requirement.  All boxed variants (Cons / Cell / MutStr /
// Vector / BoolVector / Record / CharTable) carry an `NlXxxRef` handle
// that internally contains a single `NonNull<NlXxx>` pointer (= 8 bytes,
// pointer-aligned).  Therefore the box pointer is always at byte offset
// `SEXP_PAYLOAD_OFFSET = 8` of every boxed Sexp.
//
// The helpers below let JIT trampolines and Phase B elisp wrappers read
// the box pointer directly without a `match` on the enum, which collapses
// each trampoline arm from "match → borrow → clone" to "if tag == X →
// load *const NlXxx + clone".  Phase A.4.x layout-pinned every box's
// `value(s) @ offset 0, refcount @ trailer' so the loaded pointer is
// stable across compiler versions.
//
// SAFETY contract: every `*_box_ptr` accessor is an `unsafe fn` that
// caller must guard by a tag check.  Reading the payload bytes for the
// wrong variant is UB (= e.g. reading a Vector's `Vec<Sexp>` header as
// an `NlConsBox*` would dereference the Vec ptr-len-cap as a struct).
// ---------------------------------------------------------------------------

/// Byte offset of the variant payload within a `Sexp` value.  Pinned by
/// `#[repr(C, u8)]` + 8-byte payload alignment (= max alignment of any
/// payload = pointer / `f64` / `String` ptr / NonNull ptr = 8).  Phase
/// A.5 JIT IR emits direct loads at this offset.
pub const SEXP_PAYLOAD_OFFSET: usize = 8;

/// Emit a `pub unsafe fn $name(&self) -> *const $ty` that reads the
/// boxed `NonNull<$ty>` from `Sexp` payload at offset 8.  Layout:
/// `{ tag: u8 @ 0, _pad: [u8; 7], handle: NonNull<$ty> @ 8 }`.
///
/// # Safety (every emitted fn)
///
/// Caller must guarantee `self.tag() == SEXP_TAG_*` matching `$ty`.
/// Reading the wrong variant's payload is UB.  The returned pointer is
/// borrowed for the lifetime of `self`; cloning the pointed-to handle
/// requires a separate refcount bump (= go through the `Sexp::Variant(rc)`
/// clone if you need an owned reference).
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
    /// Read the discriminant byte (offset 0).  Equivalent to
    /// [`variant_tag`] but spelled as a method for trampoline ergonomics.
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

// Compile-time check: every NlXxxRef handle must be exactly pointer-
// sized (= 8 bytes on 64-bit) so the payload offset stays at 8.
const _: () = {
    use std::mem::size_of;
    assert!(size_of::<std::ptr::NonNull<crate::eval::nlconsbox::NlConsBox>>() == 8);
    assert!(size_of::<crate::eval::nlconsbox::NlConsBoxRef>() == 8);
    assert!(size_of::<crate::eval::nlcell::NlCellRef>() == 8);
    assert!(size_of::<crate::eval::nlstr::NlStrRef>() == 8);
    assert!(size_of::<crate::eval::nlvector::NlVectorRef>() == 8);
    assert!(size_of::<crate::eval::nlboolvector::NlBoolVectorRef>() == 8);
    assert!(size_of::<crate::eval::nlrecord::NlRecordRef>() == 8);
    assert!(size_of::<crate::eval::nlchartable::NlCharTableRef>() == 8);
};

/// Doc 111 §111.C — refcount-aware Sexp slot copy.
///
/// Reads the Sexp at `*src` by reference, produces a fresh `Sexp::clone`
/// (= refcount bump for box-tagged variants), and writes the result
/// into `*dst` without running Drop on the (uninitialized) destination.
///
/// This is the ABI-stable replacement for the inline SIMD 32-byte slot
/// copy that Phase 47 used in §111.B (`record-slot-ref`) and the
/// initial §111.C (`vector-ref`) emit.  The SIMD copy is NOT refcount-
/// aware: copying a box-tagged Sexp via raw bytes leaves both the
/// source and destination pointing at the same `NlXxx` heap object,
/// and two `Drop`s on that handle cause a double-free.
///
/// # Safety
/// - `src` must be non-null, properly aligned, and point at an
///   initialized `Sexp` value that remains valid for the duration of
///   this call.
/// - `dst` must be non-null, properly aligned to a 32-byte `Sexp`
///   slot, and writable.  The bytes at `*dst` are treated as
///   *uninitialized*: this helper does NOT drop the prior contents.
///   Callers that need to overwrite a live `Sexp` slot must drop the
///   prior value first (the JIT does this implicitly because every
///   result slot is initialized to `Sexp::Nil`, which is `Copy`-shape
///   so dropping the byte pattern is harmless).
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_clone_into(src: *const Sexp, dst: *mut Sexp) {
    // Read `*src' as a borrow (= do NOT move out of `*src') so the
    // source slot retains ownership of its boxed payload, then take
    // a fresh `Sexp::clone' which performs the variant-specific
    // refcount bump (one fetch_add per boxed variant; String/Symbol
    // do a heap allocation; Nil/T/Int/Float are bit-copies).
    let cloned: Sexp = unsafe { (*src).clone() };
    // Write the cloned value into `*dst' WITHOUT running Drop on the
    // prior contents — `core::ptr::write' treats the destination as
    // uninitialized.  The caller has guaranteed `*dst' is either
    // `Sexp::Nil' (Copy-shape) or otherwise dead bytes.
    unsafe { core::ptr::write(dst, cloned); }
}

// HashTableInner struct retired in Doc 50 stage 4f (2026-05-07);
// see lisp/nelisp-stdlib-hash.el for the elisp implementation that
// stores equivalent state inside a Sexp::Record.

/// Inner storage for [`Sexp::CharTable`].  Sparse linear-scan; for
/// substrate use cases (= syntax-table, category-table, case-table)
/// the typical entry count is < 256 (ASCII range).  Future scaling
/// to full Unicode would replace this with a paged table.
///
/// Phase A.4.6: parent chain now holds [`NlCharTableRef`] handles
/// (= refcount-tracked self-reference) instead of the legacy
/// `Rc<RefCell<CharTableInner>>'.
#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct CharTableInner {
    /// Subtype symbol (e.g., `syntax-table', `display-table',
    /// `category-table').  Stored verbatim; we do not interpret it.
    pub subtype: Sexp,
    /// Default value returned for chars not in `entries`.
    pub default_val: Sexp,
    /// Sparse char → value map.  Linear scan on lookup.
    pub entries: Vec<(i64, Sexp)>,
    /// Optional parent char-table.  When set, lookups that miss the
    /// local `entries' fall through to the parent.  Used by syntax
    /// tables that derive from a base.
    pub parent: Option<NlCharTableRef>,
    /// Per-table extra slots (= upstream `char-table-extra-slot').
    /// Allocated lazily by `set-char-table-extra-slot'.  We keep this
    /// minimal — most substrate consumers only touch slots 0-3.
    pub extra: Vec<Sexp>,
}

impl Sexp {
    /// Build a proper list from a slice of values.  The empty slice
    /// returns `Nil`.
    pub fn list_from(items: &[Sexp]) -> Sexp {
        let mut acc = Sexp::Nil;
        for item in items.iter().rev() {
            acc = Sexp::cons(item.clone(), acc);
        }
        acc
    }

    /// Build a cons cell.  Doc 77c Phase A.2.1: allocates a single
    /// layout-pinned [`NlConsBox`](crate::eval::nlconsbox::NlConsBox)
    /// (= `car @ 0' / `cdr @ sizeof(Sexp)' / `refcount' trailer) and
    /// returns a [`NlConsBoxRef`] handle wrapped in `Sexp::Cons'.
    /// The cell has identity (= same box ptr through any clone) and
    /// supports `setcar' / `setcdr' via in-place mutation through
    /// the shared box.
    pub fn cons(car: Sexp, cdr: Sexp) -> Sexp {
        Sexp::Cons(NlConsBoxRef::new(car, cdr))
    }

    /// Build a vector Sexp from an owned `Vec<Sexp>` without forcing
    /// every call site to spell out `Rc::new(RefCell::new(...))'.
    pub fn vector(items: Vec<Sexp>) -> Sexp {
        Sexp::Vector(NlVectorRef::new(items))
    }

    /// Build a mutable string Sexp from a `String` (or `&str`).  Used
    /// by `make-string' / similar constructors that need `aset'-able
    /// content.
    pub fn mut_str(s: impl Into<String>) -> Sexp {
        Sexp::MutStr(NlStrRef::new(s.into()))
    }

    /// Build an empty char-table with the given SUBTYPE and INIT
    /// (= default value for unset chars).  Used by `make-char-table'.
    pub fn char_table(subtype: Sexp, init: Sexp) -> Sexp {
        Sexp::CharTable(NlCharTableRef::new(CharTableInner {
            subtype,
            default_val: init,
            entries: Vec::new(),
            parent: None,
            extra: Vec::new(),
        }))
    }

    /// Build a bool-vector of LEN bits all initialised to INIT.  Used
    /// by `make-bool-vector'.
    pub fn bool_vector(len: usize, init: bool) -> Sexp {
        Sexp::BoolVector(NlBoolVectorRef::new(vec![init; len]))
    }

    /// Return the string content of any string-like variant as an
    /// owned `String', or `None' for non-string values.  Cheap-clones
    /// for `Str(String)`; for `MutStr` it borrows + clones the inner
    /// `String'.
    pub fn as_string_owned(&self) -> Option<String> {
        match self {
            Sexp::Str(s) => Some(s.clone()),
            Sexp::MutStr(s) => Some(s.value.clone()),
            _ => None,
        }
    }

    /// Return `true' for both `Str' and `MutStr'.  Use in patterns
    /// where you only care that a value is *some* string.
    pub fn is_string(&self) -> bool {
        matches!(self, Sexp::Str(_) | Sexp::MutStr(_))
    }

    // Sexp::hash_table() helper retired in Doc 50 stage 4f
    // (2026-05-07) along with the HashTable variant — hash-tables are
    // now constructed via the elisp `make-hash-table' which builds a
    // Sexp::Record on top of Stage 4c primitives.

    /// Build a record with TYPE_TAG (= type symbol) and INIT slot
    /// vector.  Doc 52 §2.1 — used by `cl-defstruct' constructor
    /// macros after the user-side keyword args are shuffled into
    /// positional order.
    pub fn record(type_tag: Sexp, init: Vec<Sexp>) -> Sexp {
        Sexp::Record(NlRecordRef::new(type_tag, init))
    }

    /// Read the car of a cons cell as a fresh `Sexp` clone.  Returns
    /// `Nil' for non-cons input — same shape as Emacs' `car'.
    pub fn cons_car(&self) -> Sexp {
        match self {
            Sexp::Cons(b) => b.car.clone(),
            _ => Sexp::Nil,
        }
    }

    /// Read the cdr of a cons cell as a fresh `Sexp` clone.
    pub fn cons_cdr(&self) -> Sexp {
        match self {
            Sexp::Cons(b) => b.cdr.clone(),
            _ => Sexp::Nil,
        }
    }

    /// Wrap a form in `(quote <form>)` (the desugaring of `'x`).
    pub fn quote(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("quote".to_string()), inner])
    }

    pub fn backquote(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("backquote".to_string()), inner])
    }

    pub fn comma(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("comma".to_string()), inner])
    }

    pub fn comma_at(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("comma-at".to_string()), inner])
    }

    pub fn function(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("function".to_string()), inner])
    }

    /// Convenience accessor: is this the `nil` literal (or the empty
    /// list, which is the same thing in Elisp)?
    pub fn is_nil(&self) -> bool {
        matches!(self, Sexp::Nil)
    }
}

/// Pretty-printer used by Phase 8.0.1 debug + tests.  This is *not*
/// the evaluator's `prin1`; it is intentionally lossy where Elisp's
/// printer would diverge (e.g., we do not promise round-trip identity
/// on floats with no fractional part).  Phase 7.5.4.2 will own a
/// faithful `prin1-to-string` once the evaluator owns the value
/// representation.
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

/// Push `"text"` with the standard backslash escapes for `"`/`\`/`\n`/`\t`/`\r`.
/// Shared between [`Sexp::Str`] and [`Sexp::MutStr`] arms of [`write_sexp`].
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
            // Match Elisp printer behaviour for the common case: keep
            // a decimal point so round-tripping back through the reader
            // does not coerce to Int.
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
        // Sexp::HashTable arm retired in Doc 50 stage 4f.  The
        // hash-table printer (= `#s(hash-table test TEST data (K1
        // V1 ...))') is now implicit via `Sexp::Record' below: the
        // record's printer emits `#s(hash-table TEST ENTRIES)' which
        // is round-trip readable by `parse_record'.
        Sexp::CharTable(rc) => {
            // Compact printer — substrate use cases never need the
            // upstream `#^[...]' faithful shape.  We dump the populated
            // entries and the default in a self-describing form.
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
            // Pack 8 bits per char (= upstream's bool-vector printer
            // shape).  We approximate the per-char encoding without
            // matching the exact bit-order; substrate use cases do
            // not round-trip-print bool-vectors, so this is for
            // human inspection only.
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
        // Lexical-binding cell — print the inner value transparently
        // so user-facing prints never reveal the storage wrapper.
        Sexp::Cell(c) => write_sexp(out, &c.value),
        Sexp::Record(rec) => {
            // Round-trippable positional shape: `#s(TYPE V0 V1 ...)'.
            // The reader (lexer.rs) accepts the same form; the
            // `cl-defstruct' macro handles keyword desugaring before
            // values reach here.
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

/// Recognise a 2-element list `(TAG ARG)' whose head is a symbol and
/// return `(TAG, ARG)' as cloned values.  Used by the printer to
/// detect quote-family forms.
fn list_tag_and_arg(s: &Sexp) -> Option<(String, Sexp)> {
    match s {
        Sexp::Cons(b) => match (&b.car, &b.cdr) {
            (Sexp::Symbol(tag), Sexp::Cons(rest))
                if matches!(&rest.cdr, Sexp::Nil) =>
            {
                Some((tag.clone(), rest.car.clone()))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Walk a (possibly improper) list, printing the body without the
/// enclosing parens.  A non-`Nil` final cdr is rendered with the
/// classic ` . tail` notation per Elisp printer.
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

