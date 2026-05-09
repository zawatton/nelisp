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
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::nlstr::NlStrRef;
use crate::eval::nlvector::NlVectorRef;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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
    /// Phase A.4.2 (Doc 77c §4.5.2, 2026-05-09): migrated from
    /// `Rc<RefCell<String>>' to layout-pinned [`NlStrRef`] for the
    /// same reason `Cell' moved in A.4.1 — unifies the boxed-variant
    /// ABI for Phase A.5 JIT direct emit and Phase B elisp self-host.
    MutStr(NlStrRef),
    /// Cons cell.  Lists are encoded as right-leaning `Cons` chains
    /// terminated by `Nil`; dotted pairs (`(a . b)`) leave the cdr as
    /// any non-`Nil` value.
    ///
    /// Doc 77c Phase A.2.1 (2026-05-09): the legacy
    /// `(Rc<RefCell<Sexp>>, Rc<RefCell<Sexp>>)' tuple was replaced
    /// with a single `NlConsBoxRef' handle backed by the layout-
    /// pinned [`NlConsBox`](crate::eval::nlconsbox::NlConsBox).  The
    /// box embeds `car' / `cdr' / `refcount' at fixed byte offsets
    /// so the JIT (Phase A.5) and elisp `nl-cons-*' / `nl-rc-*'
    /// primitives (Phase A.3) can reach them without consulting
    /// Rust at runtime.  Clone is one refcount bump (= cheaper than
    /// the old two `Rc::clone'); equality remains structural with a
    /// `ptr_eq' fast path; `setcar' / `setcdr' mutate the shared box
    /// in place via [`NlConsBoxRef::set_car`] /
    /// [`NlConsBoxRef::set_cdr`] so the change is still visible
    /// through every aliased handle.
    Cons(NlConsBoxRef),
    /// `[a b c]` vector literal.
    ///
    /// Backed by [`NlVectorRef`] so `aset' / in-place mutation work
    /// while keeping `Sexp: Clone` cheap (refcount bump only).
    /// Identity comparison goes through `NlVectorRef::ptr_eq';
    /// structural equality (the derived `PartialEq') unwraps the
    /// inner `Vec'.
    ///
    /// Phase A.4.3 (Doc 77c §4.5.3, 2026-05-09): migrated from
    /// `Rc<RefCell<Vec<Sexp>>>' to layout-pinned [`NlVectorRef`].
    Vector(NlVectorRef),
    // Sexp::HashTable variant retired in Doc 50 stage 4f (2026-05-07).
    // Hash-tables are now `(record 'hash-table TEST ENTRIES)' built
    // on top of the Stage 4c record primitives — see
    // lisp/nelisp-stdlib-hash.el for the elisp implementation.
    /// Char-table (Track F minimum impl): maps integer codepoints
    /// to arbitrary Sexp values.  Used by Emacs syntax-table /
    /// category-table / case-table / display-table substrates.
    /// Sparse linear-scan storage; substrate use cases hold tens
    /// of entries (= ASCII coverage), occasional whole-range fills
    /// via `set-char-table-range'.
    CharTable(Rc<RefCell<CharTableInner>>),
    /// Bool-vector (Track F minimum impl): packed boolean array.
    /// Used by Emacs syntax classes / region-mark bookkeeping.
    /// `aref' returns t / nil; `aset' takes any Sexp and stores
    /// truthy/falsy.  `length' returns the bit count.
    ///
    /// Phase A.4.4 (Doc 77c §4.5, 2026-05-09): migrated from
    /// `Rc<RefCell<Vec<bool>>>' to a layout-pinned [`NlBoolVectorRef`]
    /// for the same reason `Vector' moved to `NlVectorRef' in Phase
    /// A.4.3 — fixed offset of `value' enables Phase A.5 JIT direct
    /// emit and Phase B elisp self-host primitive access.
    BoolVector(NlBoolVectorRef),
    /// Mutable cell (= write-through reference) used to back let-
    /// binding storage so `setq' inside a closure mutates the
    /// captured slot, not a copy.  The reader does NOT produce this
    /// variant — it appears only inside captured-environment alists
    /// emitted by `Env::capture_lexical' (build-tool/src/eval/env.rs).
    /// Identity goes through `NlCellRef::ptr_eq'; structural equality
    /// unwraps the inner Sexp.
    ///
    /// Phase A.4 (Doc 77c §4.5, 2026-05-09): migrated from
    /// `Rc<RefCell<Sexp>>' to a layout-pinned [`NlCellRef`] for the
    /// same reason `Cons' moved to `NlConsBoxRef' in Phase A.2.1 —
    /// fixed offset of `value' enables Phase A.5 JIT direct emit and
    /// Phase B elisp self-host primitive access.
    Cell(NlCellRef),
    /// Record (= host emacs `record' / pvec subtype).  Underlies
    /// `cl-defstruct' user types — the first slot (`type_tag') names
    /// the struct type so `type-of' can return that symbol verbatim
    /// instead of `record'.  Remaining slots are user-visible and
    /// `aset'-able.  Doc 52 §2.1 (Doc 50 Stage 4).
    ///
    /// Identity goes through `Rc::ptr_eq' on `slots'; structural
    /// equality (the derived `PartialEq') compares both `type_tag'
    /// and inner slot vector.  Printer round-trips as
    /// `#s(TYPE V0 V1 ...)' (positional shape — keyword forms are
    /// desugared by the `cl-defstruct' macro before reaching the
    /// reader).
    Record {
        type_tag: Box<Sexp>,
        slots: Rc<RefCell<Vec<Sexp>>>,
    },
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

// HashTableInner struct retired in Doc 50 stage 4f (2026-05-07);
// see lisp/nelisp-stdlib-hash.el for the elisp implementation that
// stores equivalent state inside a Sexp::Record.

/// Inner storage for [`Sexp::CharTable`].  Sparse linear-scan; for
/// substrate use cases (= syntax-table, category-table, case-table)
/// the typical entry count is < 256 (ASCII range).  Future scaling
/// to full Unicode would replace this with a paged table.
#[derive(Debug, Clone, PartialEq)]
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
    pub parent: Option<Rc<RefCell<CharTableInner>>>,
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
        Sexp::CharTable(Rc::new(RefCell::new(CharTableInner {
            subtype,
            default_val: init,
            entries: Vec::new(),
            parent: None,
            extra: Vec::new(),
        })))
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
        Sexp::Record {
            type_tag: Box::new(type_tag),
            slots: Rc::new(RefCell::new(init)),
        }
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
        Sexp::Str(text) => {
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
        Sexp::MutStr(rc) => {
            let text = &rc.value;
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
        Sexp::CharTable(inner) => {
            // Compact printer — substrate use cases never need the
            // upstream `#^[...]' faithful shape.  We dump the populated
            // entries and the default in a self-describing form.
            let inner = inner.borrow();
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
        Sexp::Record { type_tag, slots } => {
            // Round-trippable positional shape: `#s(TYPE V0 V1 ...)'.
            // The reader (lexer.rs) accepts the same form; the
            // `cl-defstruct' macro handles keyword desugaring before
            // values reach here.
            out.push_str("#s(");
            write_sexp(out, type_tag);
            for v in slots.borrow().iter() {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list_from_empty_is_nil() {
        assert_eq!(Sexp::list_from(&[]), Sexp::Nil);
    }

    #[test]
    fn list_from_three_elements_chains_right() {
        let got = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let expected = Sexp::cons(
            Sexp::Int(1),
            Sexp::cons(
                Sexp::Int(2),
                Sexp::cons(Sexp::Int(3), Sexp::Nil),
            ),
        );
        assert_eq!(got, expected);
    }

    #[test]
    fn quote_wraps_form() {
        let got = Sexp::quote(Sexp::Symbol("x".into()));
        assert_eq!(fmt_sexp(&got), "'x");
    }

    #[test]
    fn fmt_reader_macros() {
        assert_eq!(
            fmt_sexp(&Sexp::backquote(Sexp::Symbol("x".into()))),
            "`x"
        );
        assert_eq!(fmt_sexp(&Sexp::comma(Sexp::Symbol("x".into()))), ",x");
        assert_eq!(fmt_sexp(&Sexp::comma_at(Sexp::Symbol("x".into()))), ",@x");
        assert_eq!(fmt_sexp(&Sexp::function(Sexp::Symbol("x".into()))), "#'x");
    }

    #[test]
    fn fmt_dotted_pair() {
        let dotted = Sexp::cons(Sexp::Symbol("a".into()), Sexp::Symbol("b".into()));
        assert_eq!(fmt_sexp(&dotted), "(a . b)");
    }

    #[test]
    fn fmt_string_escapes() {
        assert_eq!(
            fmt_sexp(&Sexp::Str("hi\n\"\\".into())),
            "\"hi\\n\\\"\\\\\""
        );
    }

    #[test]
    fn fmt_float_keeps_decimal() {
        assert_eq!(fmt_sexp(&Sexp::Float(1.0)), "1.0");
        assert_eq!(fmt_sexp(&Sexp::Float(3.14)), "3.14");
    }

    /// Doc 62 Phase 5 — pin every `SEXP_TAG_*' constant to the actual
    /// `#[repr(C, u8)]' discriminant byte.  JIT-emitted code reads the
    /// tag from offset 0 of every Sexp pointer; if a re-ordering of
    /// variants ever changes the discriminant numeric values, this
    /// test fails BEFORE the JIT silently mis-classifies cons cells
    /// as integers (or worse).
    #[test]
    fn variant_tags_are_stable() {
        assert_eq!(variant_tag(&Sexp::Nil), SEXP_TAG_NIL);
        assert_eq!(variant_tag(&Sexp::T), SEXP_TAG_T);
        assert_eq!(variant_tag(&Sexp::Int(0)), SEXP_TAG_INT);
        assert_eq!(variant_tag(&Sexp::Float(0.0)), SEXP_TAG_FLOAT);
        assert_eq!(variant_tag(&Sexp::Symbol("x".into())), SEXP_TAG_SYMBOL);
        assert_eq!(variant_tag(&Sexp::Str("x".into())), SEXP_TAG_STR);
        assert_eq!(variant_tag(&Sexp::mut_str("x")), SEXP_TAG_MUT_STR);
        assert_eq!(
            variant_tag(&Sexp::cons(Sexp::Nil, Sexp::Nil)),
            SEXP_TAG_CONS
        );
        assert_eq!(variant_tag(&Sexp::vector(vec![])), SEXP_TAG_VECTOR);
        assert_eq!(
            variant_tag(&Sexp::char_table(Sexp::Nil, Sexp::Nil)),
            SEXP_TAG_CHAR_TABLE
        );
        assert_eq!(
            variant_tag(&Sexp::BoolVector(NlBoolVectorRef::new(vec![]))),
            SEXP_TAG_BOOL_VECTOR
        );
        assert_eq!(
            variant_tag(&Sexp::Cell(NlCellRef::new(Sexp::Nil))),
            SEXP_TAG_CELL
        );
        assert_eq!(
            variant_tag(&Sexp::Record {
                type_tag: Box::new(Sexp::Symbol("foo".into())),
                slots: Rc::new(RefCell::new(vec![]))
            }),
            SEXP_TAG_RECORD
        );
    }

    /// `#[repr(C, u8)]' should keep the Sexp footprint at the same
    /// alignment / largest-payload bound it had under default repr.
    /// We don't pin the exact byte size (= depends on String/Rc layout
    /// details that are stable in practice but not guaranteed by spec)
    /// but the alignment is fixed, and the size must accommodate the
    /// largest payload.
    #[test]
    fn sexp_layout_alignment_and_size_sane() {
        assert_eq!(std::mem::align_of::<Sexp>(), 8);
        // String is 24 bytes (3 × usize on 64-bit), payload at offset 8
        // → minimum total 32 bytes.  Allow up to 40 for niche slack.
        let sz = std::mem::size_of::<Sexp>();
        assert!(sz >= 32 && sz <= 48, "Sexp size = {} (expected 32..=48)", sz);
    }
}
