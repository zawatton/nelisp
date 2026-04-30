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
    tag_int, ImageReloc, NL_IMMEDIATE_T, NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
    NL_VALUE_TAG_CONS, NL_VALUE_TAG_FLOAT, NL_VALUE_TAG_NIL, NL_VALUE_TAG_STRING,
    NL_VALUE_TAG_SYMBOL, NL_VALUE_TAG_VECTOR,
};

use crate::reader::Sexp;

/// Bytes per pointer / immediate slot (matches `NL_VALUE_TAG_*' word
/// width; both immediates and tagged pointers fit in 8 bytes).
const SLOT_SIZE: usize = 8;
/// Bytes per cons cell (car + cdr).
const CELL_SIZE: usize = 16;
/// Bytes per symbol struct (`[name-ptr][value]`).  Name is a tagged
/// string pointer; value is currently NIL but reserves the slot for
/// Stage 6f when symbol bindings get a real value.
const SYMBOL_SIZE: usize = 16;

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

    /// Reserve a 16-byte symbol struct (`[name-ptr][value]`).
    fn alloc_symbol(&mut self) -> u64 {
        let off = self.heap.len();
        self.heap.resize(off + SYMBOL_SIZE, 0);
        off as u64
    }

    /// Reserve and write an 8-byte aligned f64 struct.  No length
    /// prefix or type header — the heap pointer that points at this
    /// struct carries `NL_VALUE_TAG_FLOAT' which pins the type.
    fn alloc_float(&mut self, f: f64) -> u64 {
        let off = self.heap.len() as u64;
        self.heap.resize(self.heap.len() + SLOT_SIZE, 0);
        self.heap[off as usize..(off as usize + SLOT_SIZE)]
            .copy_from_slice(&f.to_bits().to_le_bytes());
        off
    }

    /// Reserve a vector struct (`[u64 length][N tagged element
    /// slots]`).  Element slots are zero-filled — caller fills them
    /// in via `write_value_at'.  Returns the struct base offset.
    fn alloc_vector(&mut self, len: u64) -> u64 {
        let off = self.heap.len() as u64;
        let total = SLOT_SIZE + (len as usize) * SLOT_SIZE;
        self.heap.resize(self.heap.len() + total, 0);
        self.heap[off as usize..(off as usize + SLOT_SIZE)]
            .copy_from_slice(&len.to_le_bytes());
        off
    }

    /// Lay out a length-prefixed string struct in the heap and return
    /// its base offset.  Layout (8-byte aligned):
    ///   `[ 0..  8]`  u64 byte length
    ///   `[ 8..  8+n]` n bytes of content
    ///   `[8+n..  8+pad(n,8)]`  zero pad
    fn alloc_string(&mut self, bytes: &[u8]) -> u64 {
        let off = self.heap.len() as u64;
        let n = bytes.len();
        let padded = n.next_multiple_of(SLOT_SIZE);
        let total = SLOT_SIZE + padded;
        self.heap.resize(self.heap.len() + total, 0);
        // Length field at offset 0 of the struct.
        self.heap[off as usize..(off as usize + SLOT_SIZE)]
            .copy_from_slice(&(n as u64).to_le_bytes());
        // Content bytes immediately after, padding stays zero from
        // the resize above.
        let content_start = off as usize + SLOT_SIZE;
        self.heap[content_start..content_start + n].copy_from_slice(bytes);
        off
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
            Sexp::Str(s) => {
                let str_offset = self.alloc_string(s.as_bytes());
                self.relocs.push(ImageReloc {
                    kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                    _pad: 0,
                    write_at: slot_offset,
                    addend: str_offset | NL_VALUE_TAG_STRING,
                });
            }
            Sexp::Symbol(name) => {
                // Order matters: emit the name string first so the
                // symbol struct can carry a tagged pointer at its
                // offset 0.  Name padding keeps the symbol struct
                // 8-byte aligned automatically.
                let str_offset = self.alloc_string(name.as_bytes());
                let sym_offset = self.alloc_symbol();
                // Symbol's name slot → tagged string ptr.
                self.relocs.push(ImageReloc {
                    kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                    _pad: 0,
                    write_at: sym_offset,
                    addend: str_offset | NL_VALUE_TAG_STRING,
                });
                // Symbol's value slot — Stage 6e leaves it NIL.  The
                // 8-byte slot at sym_offset+8 already starts zero
                // from `alloc_symbol`'s resize, so write the NIL
                // immediate explicitly.
                self.write_word(sym_offset + 8, NL_VALUE_TAG_NIL);
                // Caller's slot → tagged symbol ptr.
                self.relocs.push(ImageReloc {
                    kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                    _pad: 0,
                    write_at: slot_offset,
                    addend: sym_offset | NL_VALUE_TAG_SYMBOL,
                });
            }
            Sexp::T => {
                self.write_word(slot_offset, NL_IMMEDIATE_T);
            }
            Sexp::Float(f) => {
                let f64_offset = self.alloc_float(*f);
                self.relocs.push(ImageReloc {
                    kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                    _pad: 0,
                    write_at: slot_offset,
                    addend: f64_offset | NL_VALUE_TAG_FLOAT,
                });
            }
            Sexp::Vector(items) => {
                let elements: Vec<Sexp> = items.borrow().iter().cloned().collect();
                let n = elements.len() as u64;
                let vec_offset = self.alloc_vector(n);
                self.relocs.push(ImageReloc {
                    kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                    _pad: 0,
                    write_at: slot_offset,
                    addend: vec_offset | NL_VALUE_TAG_VECTOR,
                });
                // Recursively fill each element slot.  Slots start
                // at `vec_offset + SLOT_SIZE' (after the length
                // header) and run for N consecutive 8-byte words.
                for (i, elt) in elements.iter().enumerate() {
                    let elt_slot = vec_offset + SLOT_SIZE as u64 + (i as u64) * SLOT_SIZE as u64;
                    self.write_value_at(elt_slot, elt)?;
                }
            }
        }
        Ok(())
    }
}

fn borrow_clone(slot: &Rc<RefCell<Sexp>>) -> Sexp {
    slot.borrow().clone()
}

// Stage 7b finished wiring every reader-output `Sexp' variant; a
// future reader extension that introduces a new variant should
// rewire `write_value_at' (preferred) or fall through to a
// `LowerError::Unsupported { variant: "..." }' return.  The helper
// to construct that variant lives below — kept dead-code'd until
// the next variant lands so the compiler tells us the moment we
// regress.
#[allow(dead_code)]
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
    fn t_lowers_to_immediate() {
        // Stage 7b-1 — `t' is an immediate at heap[0..8] = 11.
        let sexp = read_str("t").expect("reader rejected `t'");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 8);
        assert_eq!(read_word(&heap, 0), NL_IMMEDIATE_T);
        assert!(relocs.is_empty());
    }

    #[test]
    fn float_lowers_to_struct_plus_reloc() {
        // Stage 7b-2 — heap[0..8] reloc placeholder, heap[8..16] f64.
        let sexp = Sexp::Float(3.14);
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 16);
        assert_eq!(relocs.len(), 1);
        // f64 at offset 8.
        let bits = u64::from_le_bytes(heap[8..16].try_into().unwrap());
        assert_eq!(f64::from_bits(bits), 3.14);
        // Head reloc.
        let r = &relocs[0];
        assert_eq!(r.write_at, 0);
        assert_eq!(r.addend, 8 | NL_VALUE_TAG_FLOAT);
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

    #[test]
    fn string_lowers_to_struct_plus_reloc() {
        // "hello" → heap[0..8] (head reloc) + heap[8..16] (length=5)
        // + heap[16..24] ("hello\0\0\0").
        let sexp = read_str("\"hello\"").expect("reader rejected \"hello\"");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 24, "8 head + 8 len + 8 (5 bytes + 3 pad)");
        assert_eq!(relocs.len(), 1);
        assert_eq!(read_word(&heap, 0), 0, "head slot is reloc placeholder");
        assert_eq!(read_word(&heap, 8), 5, "length field = byte count");
        assert_eq!(&heap[16..21], b"hello");
        assert_eq!(&heap[21..24], &[0u8; 3], "remaining bytes are zero pad");

        let r = &relocs[0];
        assert_eq!(r.kind, NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET);
        assert_eq!(r.write_at, 0);
        assert_eq!(r.addend, 8 | NL_VALUE_TAG_STRING);
    }

    #[test]
    fn empty_string_uses_only_length_word() {
        let sexp = read_str("\"\"").expect("reader rejected \"\"");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 16, "8 head + 8 length-only struct");
        assert_eq!(read_word(&heap, 8), 0, "empty string length = 0");
        assert_eq!(relocs.len(), 1);
    }

    #[test]
    fn string_byte_length_uses_str_len_directly() {
        // Construct the Sexp directly to keep the test about lowering
        // (rather than the reader's byte-stream → Latin-1 → UTF-8
        // re-encoding behavior, which is a separate concern).  The
        // length we write into the image is `str::len()` — i.e., the
        // UTF-8 byte count of the input string.
        for content in ["", "a", "hello", "abcdefghi", "\u{3042}"] {
            let sexp = Sexp::Str(content.to_string());
            let (heap, _) = lower_to_heap(&sexp).unwrap();
            assert_eq!(
                read_word(&heap, 8),
                content.len() as u64,
                "length field for {:?}",
                content
            );
            // Content bytes follow at offset 16, padded to 8.
            let n = content.len();
            assert_eq!(&heap[16..16 + n], content.as_bytes());
        }
    }

    #[test]
    fn symbol_lowers_to_name_struct_plus_symbol_struct() {
        // `foo' → heap layout:
        //   [0..8]   head reloc placeholder → tagged symbol ptr
        //   [8..16]  name string length = 3
        //   [16..24] "foo\0\0\0\0\0"
        //   [24..32] symbol.name reloc placeholder → tagged string ptr
        //   [32..40] symbol.value = NIL
        let sexp = read_str("foo").expect("reader rejected `foo'");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 40);
        assert_eq!(relocs.len(), 2);

        // Name string struct.
        assert_eq!(read_word(&heap, 8), 3);
        assert_eq!(&heap[16..19], b"foo");
        // Symbol value slot is NIL.
        assert_eq!(read_word(&heap, 32), NL_VALUE_TAG_NIL);

        // Reloc 1: symbol's name slot at offset 24 → tagged string at offset 8.
        let r1 = &relocs[0];
        assert_eq!(r1.write_at, 24);
        assert_eq!(r1.addend, 8 | NL_VALUE_TAG_STRING);
        // Reloc 2: head slot at offset 0 → tagged symbol at offset 24.
        let r2 = &relocs[1];
        assert_eq!(r2.write_at, 0);
        assert_eq!(r2.addend, 24 | NL_VALUE_TAG_SYMBOL);
    }

    #[test]
    fn empty_vector_lowers_to_struct_with_zero_length() {
        let sexp = read_str("[]").expect("reader rejected []");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(heap.len(), 16); // 8 head + 8 length-only struct
        assert_eq!(read_word(&heap, 8), 0);
        assert_eq!(relocs.len(), 1);
        assert_eq!(relocs[0].addend, 8 | NL_VALUE_TAG_VECTOR);
    }

    #[test]
    fn vector_with_int_elements_lowers_inline() {
        let sexp = read_str("[1 2 3]").expect("reader rejected [1 2 3]");
        let (heap, relocs) = lower_to_heap(&sexp).unwrap();
        // 8 head + (8 length + 3*8 elements) = 8 + 32 = 40
        assert_eq!(heap.len(), 40);
        assert_eq!(read_word(&heap, 8), 3); // length at struct offset 0
        assert_eq!(read_word(&heap, 16), tag_int(1)); // element 0
        assert_eq!(read_word(&heap, 24), tag_int(2)); // element 1
        assert_eq!(read_word(&heap, 32), tag_int(3)); // element 2
        // Just one reloc: the head ptr.  Element ints are immediates.
        assert_eq!(relocs.len(), 1);
        assert_eq!(relocs[0].addend, 8 | NL_VALUE_TAG_VECTOR);
    }

    #[test]
    fn vector_with_string_elements_emits_extra_relocs() {
        // Vec[ "a", "bc" ] = 1 head reloc + 2 element relocs (each
        // string slot points at its own length-prefixed struct).
        let sexp = Sexp::vector(vec![Sexp::Str("a".into()), Sexp::Str("bc".into())]);
        let (_, relocs) = lower_to_heap(&sexp).unwrap();
        assert_eq!(relocs.len(), 3);
    }

    // Stage 7c — closure / defun support.  Lowering a `(lambda ...)'
    // or `(defun ...)' value needs *no* new image-format machinery:
    // the build-tool evaluator returns lambdas as `(closure nil
    // (params) body)` cons lists, defuns return symbols, and a
    // fully-applied `(progn (defun ...) (call ...))' returns a
    // primitive value.  All three already round-trip via Stages
    // 6 + 7b.  These tests lock the contract as regression cases.

    #[test]
    fn lambda_lowers_via_cons_list_path() {
        let result = crate::eval::eval_str("(lambda (x) (* x 2))").unwrap();
        // The evaluator returns the closure as a cons-list literal
        // `(closure nil (x) (* x 2))' — same shape Stage 6c handles.
        match &result {
            Sexp::Cons(_, _) => {}
            other => panic!("expected closure as cons, got {:?}", other),
        }
        let (heap, relocs) = lower_to_heap(&result).unwrap();
        // We don't assert exact byte counts here — the closure has
        // nested cons / symbol / etc., so the heap shape is
        // implementation-defined.  Lowering must merely succeed and
        // produce *some* relocs (the head ptr at minimum).
        assert!(!heap.is_empty());
        assert!(!relocs.is_empty());
    }

    #[test]
    fn defun_returns_symbol_lowers_cleanly() {
        // `(defun NAME ARGS BODY)' returns the function name as a
        // symbol — Stage 6e's symbol path handles it directly.
        let result = crate::eval::eval_str("(defun greet () (concat \"hi\"))").unwrap();
        match &result {
            Sexp::Symbol(name) => assert_eq!(name, "greet"),
            other => panic!("expected defun → symbol, got {:?}", other),
        }
        let (_, relocs) = lower_to_heap(&result).unwrap();
        // Symbol lowering = head ptr reloc + name reloc = 2 relocs.
        assert_eq!(relocs.len(), 2);
    }

    #[test]
    fn defun_call_evaluates_at_build_time() {
        // The flagship Stage 7 promise: a recursive defun + call is
        // evaluated entirely by the build-tool, the image carries
        // only the primitive result.
        let result = crate::eval::eval_str(
            "(progn (defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10))",
        )
        .unwrap();
        assert_eq!(result, Sexp::Int(55));
        let (heap, relocs) = lower_to_heap(&result).unwrap();
        // Pure Int → 8-byte single-slot heap, no relocs.
        assert_eq!(heap.len(), 8);
        assert!(relocs.is_empty());
    }

    #[test]
    fn lambda_funcall_evaluates_at_build_time() {
        let result = crate::eval::eval_str("(funcall (lambda (x y) (* x y)) 6 7)").unwrap();
        assert_eq!(result, Sexp::Int(42));
    }
}
