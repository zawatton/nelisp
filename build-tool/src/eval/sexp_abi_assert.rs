// Doc 100 v2 §100.B — frozen Sexp ABI assertions.
//
// CI gate that fails compilation if the `Sexp' layout drifts from
// the contract that Phase 47-compiled elisp `.o' objects assume.
// The single source of truth is `docs/arch/sexp-abi.md'; the elisp
// constants in `lisp/nelisp-sexp-layout.el' must match the values
// below.
//
// This module is `pub' only so a stand-alone `cargo run' driver
// (`bin/sexp-abi-emit', invoked by `make sexp-abi-check') can iterate
// the same set the elisp side iterates and emit a diff-friendly dump.
// The assertions themselves run at compile time — adding `pub` does
// not relax them.

use crate::eval::sexp::{
    Sexp, SEXP_PAYLOAD_OFFSET, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CELL,
    SEXP_TAG_CHAR_TABLE, SEXP_TAG_CONS, SEXP_TAG_FLOAT, SEXP_TAG_INT, SEXP_TAG_MUT_STR,
    SEXP_TAG_NIL, SEXP_TAG_RECORD, SEXP_TAG_STR, SEXP_TAG_SYMBOL, SEXP_TAG_T,
    SEXP_TAG_VECTOR,
};

// ---------------------------------------------------------------------------
// Tag byte values — match `lisp/nelisp-sexp-layout.el' (`nelisp-sexp--tag-*').
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Layout offsets — match `nelisp-sexp--offset-*' and `nelisp-sexp--slot-size'.
// ---------------------------------------------------------------------------

const _: () = assert!(SEXP_PAYLOAD_OFFSET == 8);
const _: () = assert!(std::mem::size_of::<Sexp>() == 32);
const _: () = assert!(std::mem::align_of::<Sexp>() == 8);

// ---------------------------------------------------------------------------
// Doc 101 §101.A — NlConsBox struct field offsets.  Match
// `nelisp-nlconsbox--offset-*' in `lisp/nelisp-sexp-layout.el'.
// Layout pinned by `#[repr(C)]` in `build-tool/src/eval/nlconsbox.rs:56-67'.
// ---------------------------------------------------------------------------

const _: () = assert!(std::mem::offset_of!(crate::eval::nlconsbox::NlConsBox, car) == 0);
const _: () = assert!(std::mem::offset_of!(crate::eval::nlconsbox::NlConsBox, cdr) == 32);
const _: () = assert!(std::mem::offset_of!(crate::eval::nlconsbox::NlConsBox, refcount) == 64);
const _: () = assert!(std::mem::size_of::<crate::eval::nlconsbox::NlConsBox>() == 72);

// ---------------------------------------------------------------------------
// Doc 101 §101.A — Rust String header field offsets within a
// Sexp::Symbol / Sexp::Str slot.  The String header is laid out
// (capacity, ptr, length) starting at SEXP_PAYLOAD_OFFSET (= 8) within
// the Sexp slot.  Per std::mem::offset_of on String fields (which are
// stdlib-internal but stable in practice since Rust 1.0):
//
//   String slot offset 0:  Vec<u8> header
//                          - capacity at offset 0 of header => Sexp slot 8
//                          - ptr      at offset 8 of header => Sexp slot 16
//                          - length   at offset 16 of header => Sexp slot 24
//
// We cannot directly assert these via `offset_of!(String, ptr)' because
// String's internal fields are not public; instead we assert the total
// size matches the 24-byte expectation and let the runtime driver
// (`sexp-abi-emit') verify the layout via a probe-built String value.
// ---------------------------------------------------------------------------

const _: () = assert!(std::mem::size_of::<String>() == 24);
const _: () = assert!(std::mem::align_of::<String>() == 8);

// ---------------------------------------------------------------------------
// Public exports for the `sexp-abi-emit' driver.
// ---------------------------------------------------------------------------

/// Layout entries exposed to the `make sexp-abi-check' diff driver.
/// Order matches `nelisp-sexp--abi-export' in
/// `lisp/nelisp-sexp-layout.el' — keep them in lockstep.
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
    // Doc 101 §101.A additions
    (
        "nlconsbox-offset-car",
        std::mem::offset_of!(crate::eval::nlconsbox::NlConsBox, car) as i64,
    ),
    (
        "nlconsbox-offset-cdr",
        std::mem::offset_of!(crate::eval::nlconsbox::NlConsBox, cdr) as i64,
    ),
    (
        "nlconsbox-offset-refcount",
        std::mem::offset_of!(crate::eval::nlconsbox::NlConsBox, refcount) as i64,
    ),
    (
        "nlconsbox-size",
        std::mem::size_of::<crate::eval::nlconsbox::NlConsBox>() as i64,
    ),
    ("string-offset-capacity", SEXP_PAYLOAD_OFFSET as i64),
    ("string-offset-ptr", (SEXP_PAYLOAD_OFFSET + 8) as i64),
    ("string-offset-length", (SEXP_PAYLOAD_OFFSET + 16) as i64),
    ("string-header-size", std::mem::size_of::<String>() as i64),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abi_export_is_non_empty() {
        assert!(!ABI_EXPORT.is_empty());
    }

    #[test]
    fn abi_export_keys_are_unique() {
        let mut seen = std::collections::HashSet::new();
        for (k, _) in ABI_EXPORT {
            assert!(seen.insert(*k), "duplicate key {k} in ABI_EXPORT");
        }
    }

    #[test]
    fn abi_export_matches_constants() {
        // Spot-checks — the real assertions are the `const _: ()' blocks
        // above, but a runtime test makes the failure mode crystal clear
        // if someone bypasses the compile-time assertions by feature-
        // gating them.
        let map: std::collections::HashMap<_, _> =
            ABI_EXPORT.iter().copied().collect();
        assert_eq!(map["tag-int"], SEXP_TAG_INT as i64);
        assert_eq!(map["offset-payload"], SEXP_PAYLOAD_OFFSET as i64);
        assert_eq!(map["slot-size"], std::mem::size_of::<Sexp>() as i64);
    }

    #[test]
    fn string_header_runtime_probe_matches_exported_offsets() {
        let mut s = String::with_capacity(32);
        s.push_str("foo");
        let words = &s as *const String as *const usize;
        let map: std::collections::HashMap<_, _> =
            ABI_EXPORT.iter().copied().collect();
        let cap_word = unsafe { *words.add(0) } as i64;
        let ptr_word = unsafe { *words.add(1) } as i64;
        let len_word = unsafe { *words.add(2) } as i64;
        assert_eq!(cap_word, 32);
        assert_ne!(ptr_word, 0);
        assert_eq!(len_word, 3);
        assert_eq!(map["string-offset-capacity"], SEXP_PAYLOAD_OFFSET as i64);
        assert_eq!(map["string-offset-ptr"], (SEXP_PAYLOAD_OFFSET + 8) as i64);
        assert_eq!(map["string-offset-length"], (SEXP_PAYLOAD_OFFSET + 16) as i64);
    }
}
