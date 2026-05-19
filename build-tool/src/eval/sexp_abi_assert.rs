// Compile-time Sexp ABI assertions. Elisp constants in
// `lisp/nelisp-sexp-layout.el` must match these values.

use crate::eval::nlcell::NlCell;
use crate::eval::nlconsbox::NlConsBox;
use crate::eval::nlrecord::NlRecord;
use crate::eval::nlvector::NlVector;
use crate::eval::sexp::{
    Sexp, SEXP_PAYLOAD_OFFSET, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CELL,
    SEXP_TAG_CHAR_TABLE, SEXP_TAG_CONS, SEXP_TAG_FLOAT, SEXP_TAG_INT, SEXP_TAG_MUT_STR,
    SEXP_TAG_NIL, SEXP_TAG_RECORD, SEXP_TAG_STR, SEXP_TAG_SYMBOL, SEXP_TAG_T,
    SEXP_TAG_VECTOR,
};

// Tag bytes.
const _: () = assert!(SEXP_TAG_NIL == 0);
const _: () = assert!(SEXP_TAG_T == 1);
const _: () = assert!(SEXP_TAG_INT == 2);
const _: () = assert!(SEXP_TAG_FLOAT == 3);
const _: () = assert!(SEXP_TAG_SYMBOL == 4);
const _: () = assert!(SEXP_TAG_STR == 5);
const _: () = assert!(SEXP_TAG_MUT_STR == 6);
const _: () = assert!(SEXP_TAG_CONS == 7);
const _: () = assert!(SEXP_TAG_VECTOR == 8);
const _: () = assert!(SEXP_TAG_CHAR_TABLE == 9);
const _: () = assert!(SEXP_TAG_BOOL_VECTOR == 10);
const _: () = assert!(SEXP_TAG_CELL == 11);
const _: () = assert!(SEXP_TAG_RECORD == 12);

// Layout offsets.
const _: () = assert!(SEXP_PAYLOAD_OFFSET == 8);
const _: () = assert!(std::mem::size_of::<Sexp>() == 32);
const _: () = assert!(std::mem::align_of::<Sexp>() == 8);

// `NlConsBox` field offsets.
const _: () = assert!(std::mem::offset_of!(NlConsBox, car) == 0);
const _: () = assert!(std::mem::offset_of!(NlConsBox, cdr) == 32);
const _: () = assert!(std::mem::offset_of!(NlConsBox, refcount) == 64);
const _: () = assert!(std::mem::size_of::<NlConsBox>() == 72);

// Rust `String` header within `Symbol` / `Str` payloads.
const _: () = assert!(std::mem::size_of::<String>() == 24);
const _: () = assert!(std::mem::align_of::<String>() == 8);

// `NlRecord` / `NlVector` / `NlCell` field offsets.
const _: () = assert!(std::mem::offset_of!(NlRecord, type_tag) == 0);
const _: () = assert!(std::mem::offset_of!(NlRecord, slots) == 32);
const _: () = assert!(std::mem::offset_of!(NlRecord, refcount) == 56);
const _: () = assert!(std::mem::size_of::<NlRecord>() == 64);

const _: () = assert!(std::mem::offset_of!(NlVector, value) == 0);
const _: () = assert!(std::mem::offset_of!(NlVector, refcount) == 24);
const _: () = assert!(std::mem::size_of::<NlVector>() == 32);

const _: () = assert!(std::mem::offset_of!(NlCell, value) == 0);
const _: () = assert!(std::mem::offset_of!(NlCell, refcount) == 32);
const _: () = assert!(std::mem::size_of::<NlCell>() == 40);

// Public exports for `sexp-abi-emit`.
const NLREC_SLOTS: usize = std::mem::offset_of!(NlRecord, slots);
const NLVEC_VALUE: usize = std::mem::offset_of!(NlVector, value);

/// Layout entries for `sexp-abi-emit`; order matches `nelisp-sexp--abi-export`.
pub const ABI_EXPORT: &[(&str, i64)] = &[
    ("tag-nil", SEXP_TAG_NIL as i64),
    ("tag-t", SEXP_TAG_T as i64),
    ("tag-int", SEXP_TAG_INT as i64),
    ("tag-float", SEXP_TAG_FLOAT as i64),
    ("tag-symbol", SEXP_TAG_SYMBOL as i64),
    ("tag-str", SEXP_TAG_STR as i64),
    ("tag-mut-str", SEXP_TAG_MUT_STR as i64),
    ("tag-cons", SEXP_TAG_CONS as i64),
    ("tag-vector", SEXP_TAG_VECTOR as i64),
    ("tag-char-table", SEXP_TAG_CHAR_TABLE as i64),
    ("tag-bool-vector", SEXP_TAG_BOOL_VECTOR as i64),
    ("tag-cell", SEXP_TAG_CELL as i64),
    ("tag-record", SEXP_TAG_RECORD as i64),
    ("offset-tag", 0),
    ("offset-payload", SEXP_PAYLOAD_OFFSET as i64),
    ("slot-size", std::mem::size_of::<Sexp>() as i64),
    ("nlconsbox-offset-car", std::mem::offset_of!(NlConsBox, car) as i64),
    ("nlconsbox-offset-cdr", std::mem::offset_of!(NlConsBox, cdr) as i64),
    ("nlconsbox-offset-refcount", std::mem::offset_of!(NlConsBox, refcount) as i64),
    ("nlconsbox-size", std::mem::size_of::<NlConsBox>() as i64),
    ("string-offset-capacity", SEXP_PAYLOAD_OFFSET as i64),
    ("string-offset-ptr", (SEXP_PAYLOAD_OFFSET + 8) as i64),
    ("string-offset-length", (SEXP_PAYLOAD_OFFSET + 16) as i64),
    ("string-header-size", std::mem::size_of::<String>() as i64),
    ("nlrecord-offset-type-tag", std::mem::offset_of!(NlRecord, type_tag) as i64),
    ("nlrecord-offset-slots-vec", NLREC_SLOTS as i64),
    ("nlrecord-offset-slots-ptr", NLREC_SLOTS as i64),
    ("nlrecord-offset-slots-capacity", (NLREC_SLOTS + 8) as i64),
    ("nlrecord-offset-slots-length", (NLREC_SLOTS + 16) as i64),
    ("nlrecord-offset-refcount", std::mem::offset_of!(NlRecord, refcount) as i64),
    ("nlrecord-size", std::mem::size_of::<NlRecord>() as i64),
    ("nlvector-offset-value-vec", NLVEC_VALUE as i64),
    ("nlvector-offset-value-ptr", NLVEC_VALUE as i64),
    ("nlvector-offset-value-capacity", (NLVEC_VALUE + 8) as i64),
    ("nlvector-offset-value-length", (NLVEC_VALUE + 16) as i64),
    ("nlvector-offset-refcount", std::mem::offset_of!(NlVector, refcount) as i64),
    ("nlvector-size", std::mem::size_of::<NlVector>() as i64),
    ("nlcell-offset-value", std::mem::offset_of!(NlCell, value) as i64),
    ("nlcell-offset-refcount", std::mem::offset_of!(NlCell, refcount) as i64),
    ("nlcell-size", std::mem::size_of::<NlCell>() as i64),
];
