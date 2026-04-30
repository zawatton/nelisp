//! Doc 47 Stage 6a — NlValue tag scheme v1 (immediates only).
//!
//! Walking-skeleton scope: enough tagging machinery to carry a
//! single tagged Elisp integer through a NlImage v1 heap and let
//! the seed untag it on boot.  Future stages add cons / symbol /
//! vector tags + heap-pointer tags.
//!
//! Layout: 64-bit word, low 3 bits = tag, top 61 bits = payload.
//! Negative ints rely on arithmetic right-shift to preserve sign,
//! mirroring the native asset's `sar` / `asr` instruction choice.
//!
//!   ```text
//!   bit  63 .................. 3 2 1 0
//!        |  payload (61 bits)    | tag |
//!   ```
//!
//! Tags reserved for Stage 6a (Stage 6b/6c will fill the rest):
//!   `001` = INT       (= NL_VALUE_TAG_INT)

/// Number of low-order tag bits.  The image format spec freezes this
/// at 3 — once Stage 6a ships, changing it would invalidate every
/// previously-minted image.
pub const NL_VALUE_TAG_BITS: u32 = 3;

/// Mask to extract the tag field from a 64-bit word.
pub const NL_VALUE_TAG_MASK: u64 = (1u64 << NL_VALUE_TAG_BITS) - 1;

/// Immediate integer tag (Stage 6a).
pub const NL_VALUE_TAG_INT: u64 = 0b001;

/// Heap-pointer tag for cons cells (Stage 6b).
///
/// Cons cells live as 16-byte (car, cdr) pairs in the image heap.
/// The tagged pointer is `(cell_addr | NL_VALUE_TAG_CONS)` — the
/// cell address is 8-byte aligned, so the low 3 bits are guaranteed
/// zero before tagging.  Untagging is a single `AND ~7`.
pub const NL_VALUE_TAG_CONS: u64 = 0b010;

/// Immediate `nil' sentinel (Stage 6b).  Distinct from any tagged
/// pointer (low 3 bits = 0b011) and from any tagged int (low 3
/// bits = 0b001).  Cdr-of-last-cell uses this to terminate lists.
pub const NL_VALUE_TAG_NIL: u64 = 0b011;

/// Build the on-image word for the tagged-int representing `n`.
///
/// The asm side expects the same arithmetic — see
/// `NATIVE_LOAD_HEAP_INT_UNTAG` for the matching `sar` / `asr` 3.
pub fn tag_int(n: i64) -> u64 {
    ((n as u64) << NL_VALUE_TAG_BITS) | NL_VALUE_TAG_INT
}

/// Read back the i64 payload from an int-tagged word.  Sign-extends
/// via arithmetic right-shift so negatives round-trip cleanly.
pub fn untag_int(v: u64) -> i64 {
    (v as i64) >> NL_VALUE_TAG_BITS
}

/// Whether the low-order tag bits identify an int.  Used by future
/// type-dispatching code; Stage 6a only ships ints so the predicate
/// always holds for legitimately-minted images.
pub fn is_int(v: u64) -> bool {
    (v & NL_VALUE_TAG_MASK) == NL_VALUE_TAG_INT
}

/// Stuff the cons tag into a heap-aligned pointer value.  The caller
/// is responsible for guaranteeing 8-byte alignment of `cell_addr`.
pub fn tag_cons(cell_addr: u64) -> u64 {
    debug_assert_eq!(
        cell_addr & NL_VALUE_TAG_MASK,
        0,
        "cons cell address must be 8-byte aligned before tagging"
    );
    cell_addr | NL_VALUE_TAG_CONS
}

/// Recover the underlying cell address from a cons-tagged pointer.
pub fn untag_cons(v: u64) -> u64 {
    v & !NL_VALUE_TAG_MASK
}

/// Whether the low-order tag bits identify a cons-cell pointer.
pub fn is_cons(v: u64) -> bool {
    (v & NL_VALUE_TAG_MASK) == NL_VALUE_TAG_CONS
}

/// Whether the value is the immediate `nil' sentinel.  Distinct
/// from "the tag is NIL" because all 64 bits must equal
/// `NL_VALUE_TAG_NIL` — there is only one nil value, no payload.
pub fn is_nil(v: u64) -> bool {
    v == NL_VALUE_TAG_NIL
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_positive() {
        for n in [0i64, 1, 7, 42, 12345, i32::MAX as i64] {
            let tagged = tag_int(n);
            assert!(is_int(tagged), "{} should round-trip is_int", n);
            assert_eq!(untag_int(tagged), n, "round-trip for {}", n);
        }
    }

    #[test]
    fn round_trip_negative() {
        for n in [-1i64, -7, -42, i32::MIN as i64] {
            let tagged = tag_int(n);
            assert!(is_int(tagged), "{} should round-trip is_int", n);
            assert_eq!(untag_int(tagged), n, "round-trip for {}", n);
        }
    }

    #[test]
    fn tag_bits_present() {
        for n in [0i64, 1, -1, 42, -42] {
            assert_eq!(tag_int(n) & NL_VALUE_TAG_MASK, NL_VALUE_TAG_INT);
        }
    }

    #[test]
    fn payload_loses_no_low_bits_when_in_range() {
        // The reserved tag bits mean the largest representable int is
        // i64 >> 3 — i.e. tag_int(i64::MAX >> 3) is the boundary.
        let max = i64::MAX >> NL_VALUE_TAG_BITS;
        assert_eq!(untag_int(tag_int(max)), max);
        let min = i64::MIN >> NL_VALUE_TAG_BITS;
        assert_eq!(untag_int(tag_int(min)), min);
    }

    #[test]
    fn tag_int_constant_is_nonzero() {
        // Catches the common bootstrapping bug of accidentally
        // reusing tag 0 for ints (which is reserved for cons-cell
        // pointers in most Lisp tag schemes — Doc 47 follows suit).
        assert_ne!(NL_VALUE_TAG_INT, 0);
    }

    #[test]
    fn cons_round_trip() {
        for addr in [0x0u64, 0x8, 0x1000, 0x7f_0000_0000u64, u64::MAX & !NL_VALUE_TAG_MASK] {
            let tagged = tag_cons(addr);
            assert!(is_cons(tagged));
            assert!(!is_int(tagged));
            assert!(!is_nil(tagged));
            assert_eq!(untag_cons(tagged), addr);
        }
    }

    #[test]
    fn cons_tag_distinct_from_int_tag() {
        assert_ne!(NL_VALUE_TAG_CONS, NL_VALUE_TAG_INT);
        assert_ne!(NL_VALUE_TAG_CONS, NL_VALUE_TAG_NIL);
    }

    #[test]
    fn nil_is_only_nil() {
        assert!(is_nil(NL_VALUE_TAG_NIL));
        assert!(!is_nil(0));
        assert!(!is_nil(NL_VALUE_TAG_INT));
        assert!(!is_nil(tag_int(0))); // tag_int(0) = 0b001, not 0b011
        assert!(!is_nil(tag_cons(0))); // tag_cons(0) = 0b010
    }

    #[test]
    fn reloc_addend_trick_round_trips() {
        // Doc 47 Stage 6b: the heap dumper avoids needing a new
        // reloc kind by stuffing the cons tag into the addend's
        // low bits.  When the loader writes `heap_base + addend'
        // and `heap_base` is page-aligned (≥ 8-byte aligned), the
        // OR semantics fall out for free.  This test certifies
        // the arithmetic identity.
        let cell_offset = 8u64;
        let addend = cell_offset | NL_VALUE_TAG_CONS; // = 10
        for heap_base in [0x1000u64, 0x7f_aabb_c000u64] {
            assert_eq!(heap_base & NL_VALUE_TAG_MASK, 0,
                       "test precondition: heap_base must be 8-byte aligned");
            let written = heap_base.wrapping_add(addend);
            let expected = tag_cons(heap_base + cell_offset);
            assert_eq!(written, expected,
                       "+= addend must equal | TAG_CONS for aligned heap_base");
        }
    }
}
