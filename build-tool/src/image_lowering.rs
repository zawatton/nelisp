//! Doc 47 Stage 6d — lower a reader-output `Sexp` to a NlImage v1
//! heap-bytes + reloc-records pair.
//!
//! This is the first build-tool surface that *produces* image data
//! end-to-end.  Stages 6a / 6b / 6c hand-coded heap bytes inside
//! `nelisp-runtime/src/main.rs`; Stage 6d replaces those hand-codings
//! with a generic `Sexp -> (heap, relocs)' lowering keyed off the
//! same `image::value' tag scheme + `image::reloc' record format.
//!
//! Output convention:
//!   - `heap[0..8]' is the head value: a 64-bit little-endian word,
//!     either an immediate (NIL / tagged int) or a reloc placeholder
//!     for a tagged cons pointer to a cell elsewhere in the heap.
//!   - Cons cells live at later 16-byte aligned offsets; each cell
//!     is laid out as `(car, cdr)' = 8 + 8 bytes, both filled by
//!     the same recursive lowering rule.
//!   - Pointer slots are zero-filled at mint time; the reloc table
//!     drives the actual values at boot (`heap_base + addend' with
//!     `addend = cell_offset | TAG_CONS' so the OR-tag rides along
//!     in the addend's low 3 bits — see Stage 6b for the alignment
//!     identity that justifies it).
//!
//! Stage 6d only supports `Nil`, `Int`, and `Cons`.  Any other Sexp
//! variant (Symbol / Str / Float / T / Vector) returns
//! `LowerError::Unsupported' so callers can blame the right node.
//! Future Stage 6e+ will widen the surface as the corresponding
//! seed-side native assets land.

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use nelisp_runtime::image::{
    tag_int, ImageReloc, NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET, NL_VALUE_TAG_CONS,
    NL_VALUE_TAG_NIL,
};

use crate::reader::Sexp;

/// Bytes per pointer / immediate slot (matches `NL_VALUE_TAG_*' word
/// width; both immediates and tagged pointers fit in 8 bytes).
const SLOT_SIZE: usize = 8;
/// Bytes per cons cell (car + cdr).
const CELL_SIZE: usize = 16;

#[derive(Debug)]
pub enum LowerError {
    /// Sexp variant Stage 6d does not yet handle.  Carries the
    /// human-readable variant name so the caller can produce a
    /// useful error to the user.
    Unsupported { variant: &'static str },
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LowerError::Unsupported { variant } => write!(
                f,
                "image lowering: Sexp variant `{}' is not supported in Stage 6d \
                 (only Nil, Int, Cons are wired); add a native asset + tag \
                 before lowering this kind of value",
                variant
            ),
        }
    }
}

impl std::error::Error for LowerError {}

/// Lower a Sexp to (heap_bytes, relocs).  The heap always begins
/// with a single 8-byte slot holding the head value (immediate or
/// reloc'd tagged ptr); this is the slot the seed reads at
/// `*argv[0]'.
///
/// The returned heap is byte-deterministic for a given Sexp tree
/// — tests verify this by minting Stage 6c's `(1 2 3)` and
/// comparing against the hand-coded version.
pub fn lower_to_heap(sexp: &Sexp) -> Result<(Vec<u8>, Vec<ImageReloc>), LowerError> {
    let mut lowerer = Lowerer::default();
    let head_slot = lowerer.alloc_slot();
    debug_assert_eq!(head_slot, 0, "head slot must land at heap[0..8]");
    lowerer.write_value_at(head_slot, sexp)?;
    Ok((lowerer.heap, lowerer.relocs))
}

#[derive(Default)]
struct Lowerer {
    heap: Vec<u8>,
    relocs: Vec<ImageReloc>,
}

impl Lowerer {
    /// Reserve an 8-byte slot at the current end of the heap and
    /// return its offset.  Newly-allocated slots are zero-filled.
    fn alloc_slot(&mut self) -> u64 {
        let off = self.heap.len();
        self.heap.resize(off + SLOT_SIZE, 0);
        off as u64
    }

    /// Reserve a 16-byte cons cell (`(car, cdr)`) and return the
    /// car's offset (= cell base).  The cell is zero-filled so
    /// recursive lowering can overwrite each slot in turn.
    fn alloc_cell(&mut self) -> u64 {
        let off = self.heap.len();
        self.heap.resize(off + CELL_SIZE, 0);
        off as u64
    }

    /// Place an 8-byte little-endian word at `slot_offset`.
    fn write_word(&mut self, slot_offset: u64, word: u64) {
        let s = slot_offset as usize;
        self.heap[s..s + SLOT_SIZE].copy_from_slice(&word.to_le_bytes());
    }

    /// Lower `value' into the 8-byte slot at `slot_offset' (which the
    /// caller has already reserved).  May allocate further heap
    /// space + emit relocs to encode cons-pointer slots.
    fn write_value_at(&mut self, slot_offset: u64, value: &Sexp) -> Result<(), LowerError> {
        match value {
            Sexp::Nil => {
                self.write_word(slot_offset, NL_VALUE_TAG_NIL);
            }
            Sexp::Int(n) => {
                self.write_word(slot_offset, tag_int(*n));
            }
            Sexp::Cons(car_rc, cdr_rc) => {
                let cell_offset = self.alloc_cell();
                self.relocs.push(ImageReloc {
                    kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                    _pad: 0,
                    write_at: slot_offset,
                    addend: cell_offset | NL_VALUE_TAG_CONS,
                });
                // The slot itself stays zero — the reloc overwrites
                // it at boot.  Now recurse into the cell's car/cdr
                // halves.
                self.write_value_at(cell_offset, &borrow_clone(car_rc))?;
                self.write_value_at(cell_offset + 8, &borrow_clone(cdr_rc))?;
            }
            Sexp::T => return Err(unsupported("T")),
            Sexp::Float(_) => return Err(unsupported("Float")),
            Sexp::Symbol(_) => return Err(unsupported("Symbol")),
            Sexp::Str(_) => return Err(unsupported("Str")),
            Sexp::Vector(_) => return Err(unsupported("Vector")),
        }
        Ok(())
    }
}

fn borrow_clone(slot: &Rc<RefCell<Sexp>>) -> Sexp {
    slot.borrow().clone()
}

fn unsupported(variant: &'static str) -> LowerError {
    LowerError::Unsupported { variant }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::read_str;
    use nelisp_runtime::image::{NL_VALUE_TAG_INT, NL_VALUE_TAG_MASK};

    fn read_word(heap: &[u8], offset: usize) -> u64 {
        u64::from_le_bytes(heap[offset..offset + 8].try_into().unwrap())
    }

    #[test]
    fn nil_lowers_to_immediate() {
        let (heap, relocs) = lower_to_heap(&Sexp::Nil).unwrap();
        assert_eq!(heap.len(), 8);
        assert_eq!(read_word(&heap, 0), NL_VALUE_TAG_NIL);
        assert!(relocs.is_empty());
    }

    #[test]
    fn int_lowers_to_immediate() {
        for n in [0i64, 7, 42, -1] {
            let (heap, relocs) = lower_to_heap(&Sexp::Int(n)).unwrap();
            assert_eq!(heap.len(), 8);
            assert_eq!(read_word(&heap, 0) & NL_VALUE_TAG_MASK, NL_VALUE_TAG_INT);
            assert!(relocs.is_empty());
        }
    }

    #[test]
    fn list_one_two_three_matches_stage_6c_layout() {
        // Same input the hand-coded `mint_list_skeleton_image' bakes
        // in `nelisp-runtime/src/main.rs'.  Lowering must produce the
        // same heap size, the same reloc count, and the same per-slot
        // tagged-int payloads.
        let sexp = read_str("(1 2 3)").expect("reader rejected (1 2 3)");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();

        assert_eq!(heap.len(), 56, "heap size must match Stage 6c hand-coding");
        assert_eq!(relocs.len(), 3, "three cdr links + head ptr → 3 relocs");

        // Cars at offsets 8 / 24 / 40 must be tag_int(1..3).
        assert_eq!(read_word(&heap, 8), tag_int(1));
        assert_eq!(read_word(&heap, 24), tag_int(2));
        assert_eq!(read_word(&heap, 40), tag_int(3));
        // Tail cdr at offset 48 is NIL immediate.
        assert_eq!(read_word(&heap, 48), NL_VALUE_TAG_NIL);

        // Head + middle + last cdr slots are reloc placeholders (zero
        // before reloc apply — the seed overwrites at boot).
        assert_eq!(read_word(&heap, 0), 0);
        assert_eq!(read_word(&heap, 16), 0);
        assert_eq!(read_word(&heap, 32), 0);

        // Reloc layout matches the hand-coded version 1:1.
        let expected = [
            (0u64, 8u64 | NL_VALUE_TAG_CONS),
            (16, 24 | NL_VALUE_TAG_CONS),
            (32, 40 | NL_VALUE_TAG_CONS),
        ];
        for ((write_at, addend), reloc) in expected.iter().zip(relocs.iter()) {
            assert_eq!(reloc.kind, NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET);
            assert_eq!(reloc.write_at, *write_at);
            assert_eq!(reloc.addend, *addend);
        }
    }

    #[test]
    fn unsupported_variant_blames_correctly() {
        // Reader produces a Symbol for `foo'; lowering must reject
        // it with a name the user can act on.
        let sexp = read_str("foo").expect("reader rejected `foo'");
        let err = lower_to_heap(&sexp).unwrap_err();
        match err {
            LowerError::Unsupported { variant } => assert_eq!(variant, "Symbol"),
        }
    }

    #[test]
    fn dotted_pair_lowers_as_two_slot_cell() {
        // (1 . 2) — cdr is non-nil, non-cons.
        let sexp = read_str("(1 . 2)").expect("reader rejected dotted pair");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 24);
        assert_eq!(relocs.len(), 1);
        assert_eq!(read_word(&heap, 8), tag_int(1));
        assert_eq!(read_word(&heap, 16), tag_int(2));
    }
}
