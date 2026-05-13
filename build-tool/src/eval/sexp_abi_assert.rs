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
}
