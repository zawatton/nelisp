//! Doc 47 Stage 4b — relocation table record format and heap patching.

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ImageReloc {
    pub kind: u32,
    pub _pad: u32,
    pub write_at: u64,
    pub addend: u64,
}

pub const NL_RELOC_RECORD_SIZE: usize = 24;
const _: [(); NL_RELOC_RECORD_SIZE] = [(); core::mem::size_of::<ImageReloc>()];

pub const NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET: u32 = 1;

#[derive(Debug, PartialEq, Eq)]
pub enum RelocError {
    OutOfBounds { write_at: u64, heap_size: u64 },
    UnknownKind { kind: u32 },
}

/// Apply `relocs` to the heap mapping starting at `heap_base`.
/// Each HEAP_BASE_PLUS_OFFSET entry writes a little-endian u64
/// = (heap_base as u64).wrapping_add(addend) at heap_base.add(write_at).
/// SAFETY: caller guarantees `heap_base..heap_base+heap_size` is
/// a valid writable mapping.
pub unsafe fn apply_relocations(
    heap_base: *mut u8,
    heap_size: usize,
    relocs: &[ImageReloc],
) -> Result<(), RelocError> {
    for reloc in relocs {
        if reloc.kind != NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET {
            return Err(RelocError::UnknownKind { kind: reloc.kind });
        }
        if reloc.write_at > (heap_size as u64).saturating_sub(8) {
            return Err(RelocError::OutOfBounds {
                write_at: reloc.write_at,
                heap_size: heap_size as u64,
            });
        }

        let value = (heap_base as u64).wrapping_add(reloc.addend).to_le_bytes();
        // SAFETY: bounds check above guarantees the 8-byte write stays
        // within the caller-provided writable heap mapping.
        unsafe {
            core::ptr::copy_nonoverlapping(
                value.as_ptr(),
                heap_base.add(reloc.write_at as usize),
                value.len(),
            );
        }
    }
    Ok(())
}

/// Serialise a slice of relocs to the on-disk byte representation.
/// Emits exactly relocs.len() * NL_RELOC_RECORD_SIZE bytes.
pub fn relocs_to_bytes(relocs: &[ImageReloc]) -> Vec<u8> {
    let mut out = Vec::with_capacity(relocs.len() * NL_RELOC_RECORD_SIZE);
    for reloc in relocs {
        out.extend_from_slice(&reloc.kind.to_le_bytes());
        out.extend_from_slice(&reloc._pad.to_le_bytes());
        out.extend_from_slice(&reloc.write_at.to_le_bytes());
        out.extend_from_slice(&reloc.addend.to_le_bytes());
    }
    out
}

/// Parse `bytes` as a packed array of `count` ImageReloc records.
/// Returns the parsed Vec; bytes.len() must be exactly count * 24.
pub fn relocs_from_bytes(bytes: &[u8], count: usize) -> Option<Vec<ImageReloc>> {
    if bytes.len() != count.checked_mul(NL_RELOC_RECORD_SIZE)? {
        return None;
    }

    let mut out = Vec::with_capacity(count);
    for chunk in bytes.chunks_exact(NL_RELOC_RECORD_SIZE) {
        let kind = u32::from_le_bytes(chunk[0..4].try_into().ok()?);
        let pad = u32::from_le_bytes(chunk[4..8].try_into().ok()?);
        let write_at = u64::from_le_bytes(chunk[8..16].try_into().ok()?);
        let addend = u64::from_le_bytes(chunk[16..24].try_into().ok()?);
        out.push(ImageReloc {
            kind,
            _pad: pad,
            write_at,
            addend,
        });
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reloc_record_size_is_locked() {
        assert_eq!(core::mem::size_of::<ImageReloc>(), NL_RELOC_RECORD_SIZE);
    }

    #[test]
    fn single_reloc_writes_little_endian_pointer() {
        let mut heap = [0u8; 16];
        let heap_base = heap.as_mut_ptr();
        let relocs = [ImageReloc {
            kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
            _pad: 0,
            write_at: 0,
            addend: 8,
        }];

        unsafe { apply_relocations(heap_base, heap.len(), &relocs) }.unwrap();

        let expected = ((heap_base as u64).wrapping_add(8)).to_le_bytes();
        assert_eq!(&heap[..8], &expected);
        assert_eq!(&heap[8..], &[0; 8]);
    }

    #[test]
    fn out_of_bounds_rejected() {
        let mut heap = [0u8; 8];
        let relocs = [ImageReloc {
            kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
            _pad: 0,
            write_at: 1,
            addend: 0,
        }];

        let err = unsafe { apply_relocations(heap.as_mut_ptr(), heap.len(), &relocs) }.unwrap_err();
        assert_eq!(
            err,
            RelocError::OutOfBounds {
                write_at: 1,
                heap_size: 8
            }
        );
    }

    #[test]
    fn unknown_kind_rejected() {
        let mut heap = [0u8; 16];
        let relocs = [ImageReloc {
            kind: 99,
            _pad: 0,
            write_at: 0,
            addend: 0,
        }];

        let err = unsafe { apply_relocations(heap.as_mut_ptr(), heap.len(), &relocs) }.unwrap_err();
        assert_eq!(err, RelocError::UnknownKind { kind: 99 });
    }

    #[test]
    fn empty_slice_is_no_op() {
        let mut heap = [0x55u8; 16];
        let before = heap;

        unsafe { apply_relocations(heap.as_mut_ptr(), heap.len(), &[]) }.unwrap();

        assert_eq!(heap, before);
    }

    #[test]
    fn multiple_relocs_apply_in_slice_order() {
        let mut heap = [0u8; 24];
        let heap_base = heap.as_mut_ptr();
        let relocs = [
            ImageReloc {
                kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                _pad: 0,
                write_at: 8,
                addend: 16,
            },
            ImageReloc {
                kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                _pad: 0,
                write_at: 0,
                addend: 8,
            },
        ];

        unsafe { apply_relocations(heap_base, heap.len(), &relocs) }.unwrap();

        assert_eq!(&heap[0..8], &((heap_base as u64).wrapping_add(8)).to_le_bytes());
        assert_eq!(&heap[8..16], &((heap_base as u64).wrapping_add(16)).to_le_bytes());
    }

    #[test]
    fn reloc_bytes_round_trip() {
        let relocs = vec![
            ImageReloc {
                kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                _pad: 0,
                write_at: 0,
                addend: 8,
            },
            ImageReloc {
                kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
                _pad: 7,
                write_at: 24,
                addend: 40,
            },
        ];

        let bytes = relocs_to_bytes(&relocs);
        assert_eq!(bytes.len(), relocs.len() * NL_RELOC_RECORD_SIZE);
        assert_eq!(relocs_from_bytes(&bytes, relocs.len()), Some(relocs));
        assert_eq!(relocs_from_bytes(&bytes[..bytes.len() - 1], 2), None);
    }
}
