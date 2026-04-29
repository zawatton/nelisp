//! Mint a fresh NlImage v1 file on disk.
//!
//! Stage 2 only writes the 104-byte header — every payload offset is
//! 0, the entry point is unset, and there is no native code arena.
//! That is enough to lock the wire format end-to-end against the
//! Stage 1 struct: dumper writes, loader reads, round-trip test
//! asserts byte-equality + verifier acceptance.
//!
//! Stage 3 will extend this to actually serialise heap snapshots and
//! native code arenas; the function names are picked so callers can
//! keep using `write_empty_image` for smoke tests once the real
//! `write_image` lands beside it.
//!
//! Like `loader.rs`, this uses `std::fs` for now and will move to the
//! `nelisp_syscall_*` ABI in Stage 3 (TODO marker preserved).

use std::fs;
use std::path::Path;

use super::error::ImageError;
use super::format::{NlImageHeader, NL_IMAGE_HEADER_SIZE, NL_IMAGE_PAGE_SIZE};

/// Write a Stage 2 "empty" image: a freshly initialised `NlImageHeader`
/// (magic = `NLIMAGE\0`, abi = 1, all sizes / offsets = 0) and nothing
/// else.  The file is exactly `NL_IMAGE_HEADER_SIZE` bytes long.
///
/// The resulting file passes `verify_magic_and_version` but cannot be
/// booted — Stage 3 will add `write_image` that actually attaches
/// heap and code segments.
///
/// TODO(stage-3-seed-syscall): swap to `nelisp_syscall_open` /
/// `nelisp_syscall_write` once the seed crate is extracted.
pub fn write_empty_image<P: AsRef<Path>>(path: P) -> Result<(), ImageError> {
    let path = path.as_ref();
    let header = NlImageHeader::new_v1();
    let bytes = header.to_bytes();
    fs::write(path, bytes).map_err(|source| ImageError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    Ok(())
}

/// Stage 3 walking-skeleton dumper: write a NlImage v1 file whose only
/// payload is a native code arena containing `native_bytes`.  The
/// entry point is set to offset 0 of the code segment.
///
/// On-disk layout produced:
///   bytes [0, 104)              header
///   bytes [104, NL_IMAGE_PAGE_SIZE)
///                               zero padding to page boundary
///   bytes [NL_IMAGE_PAGE_SIZE,
///          NL_IMAGE_PAGE_SIZE + native_bytes.len())
///                               native code segment
///
/// Header fields set:
///   payload_len   = (NL_IMAGE_PAGE_SIZE - HEADER_SIZE) + native_bytes.len()
///   code_offset   = NL_IMAGE_PAGE_SIZE
///   code_size     = native_bytes.len()
///   entry_offset  = 0  (= start of code segment)
///   heap_*, reloc_*, signal_vector_*  = 0  (no heap segment in Stage 3)
///
/// Stage 4 will replace this with `write_image' that also serialises a
/// heap snapshot + relocation table so booted code can call into NeLisp.
pub fn write_image_with_native_entry<P: AsRef<Path>>(
    path: P,
    native_bytes: &[u8],
) -> Result<(), ImageError> {
    if native_bytes.is_empty() {
        return Err(ImageError::UnsupportedTarget {
            reason: "native_bytes empty (no asset for this target)",
        });
    }

    let path = path.as_ref();
    let code_offset = NL_IMAGE_PAGE_SIZE;
    let code_size = native_bytes.len() as u64;
    let payload_len = (code_offset - NL_IMAGE_HEADER_SIZE as u64) + code_size;

    let mut header = NlImageHeader::new_v1();
    header.payload_len = payload_len;
    header.code_offset = code_offset;
    header.code_size = code_size;
    header.entry_offset = 0; // start of code segment

    // Build the full file in memory, then write atomically.  Total
    // size is small (~4 KiB + a few bytes) so a single Vec is cheap.
    let mut buf = Vec::with_capacity((code_offset as usize) + native_bytes.len());
    buf.extend_from_slice(&header.to_bytes());
    buf.resize(code_offset as usize, 0); // zero-pad to page boundary
    buf.extend_from_slice(native_bytes);

    fs::write(path, &buf).map_err(|source| ImageError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    Ok(())
}

/// Stage 4a walking-skeleton dumper: write a NlImage v1 file with both
/// a heap segment AND a native code arena.  The heap is page-aligned
/// (rounded up to `NL_IMAGE_PAGE_SIZE`) so the code arena always starts
/// on its own page.
///
/// On-disk layout produced:
///   bytes [0, 104)                              header
///   bytes [104, NL_IMAGE_PAGE_SIZE)             zero pad to page
///   bytes [NL_IMAGE_PAGE_SIZE,
///          NL_IMAGE_PAGE_SIZE + heap_size)      heap segment
///   bytes [NL_IMAGE_PAGE_SIZE + heap_size,
///          code_offset)                         zero pad to next page
///   bytes [code_offset,
///          code_offset + native_bytes.len())    native code segment
///
/// Header fields set:
///   payload_len   = (code_offset - HEADER_SIZE) + native_bytes.len()
///   heap_offset   = NL_IMAGE_PAGE_SIZE
///   heap_size     = heap_bytes.len()
///   code_offset   = NL_IMAGE_PAGE_SIZE + ceil(heap_size / PAGE) * PAGE
///   code_size     = native_bytes.len()
///   entry_offset  = 0  (= start of code segment)
///   reloc_*, signal_vector_*  = 0  (Stage 4b/4c)
///
/// `heap_bytes` may be empty — in that case the on-disk layout matches
/// `write_image_with_native_entry` exactly (heap_size = 0,
/// code_offset = PAGE_SIZE).  The seed boot path uses
/// `header.heap_size > 0` to decide whether to mmap a heap segment, so
/// callers can mint heap-less images through this entry point too.
pub fn write_image_with_heap_and_native_entry<P: AsRef<Path>>(
    path: P,
    native_bytes: &[u8],
    heap_bytes: &[u8],
) -> Result<(), ImageError> {
    if native_bytes.is_empty() {
        return Err(ImageError::UnsupportedTarget {
            reason: "native_bytes empty (no asset for this target)",
        });
    }

    let path = path.as_ref();
    let page = NL_IMAGE_PAGE_SIZE;
    let heap_offset = page;
    let heap_size = heap_bytes.len() as u64;
    // Round heap up to a page boundary so the code arena always sits
    // on its own page (matches the Stage 3 layout where code starts at
    // an exact multiple of PAGE).  If `heap_size == 0`, `heap_padded`
    // is also 0 and `code_offset == PAGE`.
    let heap_padded = ((heap_size + page - 1) / page) * page;
    let code_offset = heap_offset + heap_padded;
    let code_size = native_bytes.len() as u64;
    let payload_len = (code_offset - NL_IMAGE_HEADER_SIZE as u64) + code_size;

    let mut header = NlImageHeader::new_v1();
    header.payload_len = payload_len;
    header.heap_offset = heap_offset;
    header.heap_size = heap_size;
    header.code_offset = code_offset;
    header.code_size = code_size;
    header.entry_offset = 0; // start of code segment

    let mut buf = Vec::with_capacity((code_offset as usize) + native_bytes.len());
    buf.extend_from_slice(&header.to_bytes());
    buf.resize(heap_offset as usize, 0); // pad to heap page
    buf.extend_from_slice(heap_bytes);
    buf.resize(code_offset as usize, 0); // pad to code page
    buf.extend_from_slice(native_bytes);

    fs::write(path, &buf).map_err(|source| ImageError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::image::loader::read_header;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    static COUNTER: AtomicU64 = AtomicU64::new(0);

    fn temp_path(label: &str) -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let ctr = COUNTER.fetch_add(1, Ordering::SeqCst);
        let mut p = std::env::temp_dir();
        p.push(format!(
            "nelisp-image-stage3-dumper-{}-{}-{}-{}.bin",
            label,
            std::process::id(),
            nanos,
            ctr
        ));
        p
    }

    #[test]
    fn skeleton_image_layout_is_correct() {
        // 16-byte fake "native" payload — bytes meaningless on this path
        // because dumper / loader don't execute, only verify offsets.
        let fake = (0u8..16u8).collect::<Vec<_>>();
        let path = temp_path("layout");
        write_image_with_native_entry(&path, &fake).expect("dumper failed");

        let bytes = std::fs::read(&path).unwrap();
        // Header + page padding + payload.
        assert_eq!(bytes.len(), NL_IMAGE_PAGE_SIZE as usize + fake.len());
        // Page-padding region is all zero.
        assert!(bytes[NL_IMAGE_HEADER_SIZE..NL_IMAGE_PAGE_SIZE as usize]
            .iter()
            .all(|&b| b == 0));
        // Payload bytes match.
        assert_eq!(&bytes[NL_IMAGE_PAGE_SIZE as usize..], fake.as_slice());

        let hdr = read_header(&path).expect("loader rejected dumper output");
        assert_eq!(hdr.code_offset, NL_IMAGE_PAGE_SIZE);
        assert_eq!(hdr.code_size, fake.len() as u64);
        assert_eq!(hdr.entry_offset, 0);
        assert_eq!(hdr.heap_size, 0);
        assert_eq!(hdr.reloc_count, 0);

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn empty_native_bytes_rejected() {
        let path = temp_path("empty");
        match write_image_with_native_entry(&path, &[]) {
            Err(ImageError::UnsupportedTarget { .. }) => {}
            other => panic!("expected UnsupportedTarget, got {:?}", other),
        }
        // File should not have been created.
        assert!(!path.exists());
    }

    #[test]
    fn heap_skeleton_image_layout_is_correct() {
        // 8-byte fake "native" payload + 3-byte heap.
        let native = vec![0x90u8; 8]; // x86 nop * 8 — meaningless on this path, dumper doesn't execute
        let heap = vec![0x37u8, 0xab, 0xcd];
        let path = temp_path("heap_layout");
        write_image_with_heap_and_native_entry(&path, &native, &heap)
            .expect("dumper failed");

        let bytes = std::fs::read(&path).unwrap();
        let page = NL_IMAGE_PAGE_SIZE as usize;

        // Total file = 2 pages (header+heap, then code).
        assert_eq!(bytes.len(), 2 * page + native.len());
        // Header padding region zeroed.
        assert!(bytes[NL_IMAGE_HEADER_SIZE..page].iter().all(|&b| b == 0));
        // Heap segment matches.
        assert_eq!(&bytes[page..page + heap.len()], heap.as_slice());
        // Heap padding region zeroed.
        assert!(bytes[page + heap.len()..2 * page].iter().all(|&b| b == 0));
        // Native code segment matches.
        assert_eq!(&bytes[2 * page..], native.as_slice());

        let hdr = read_header(&path).expect("loader rejected dumper output");
        assert_eq!(hdr.heap_offset, page as u64);
        assert_eq!(hdr.heap_size, heap.len() as u64);
        assert_eq!(hdr.code_offset, 2 * page as u64);
        assert_eq!(hdr.code_size, native.len() as u64);
        assert_eq!(hdr.entry_offset, 0);
        assert_eq!(hdr.reloc_count, 0);

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn heap_skeleton_with_empty_heap_matches_stage3_layout() {
        // heap_bytes empty must produce the same layout as
        // write_image_with_native_entry: heap_size=0, code_offset=PAGE.
        let native = vec![0xc3u8]; // ret
        let path = temp_path("heap_empty");
        write_image_with_heap_and_native_entry(&path, &native, &[])
            .expect("dumper failed");

        let hdr = read_header(&path).expect("loader rejected dumper output");
        assert_eq!(hdr.heap_offset, NL_IMAGE_PAGE_SIZE);
        assert_eq!(hdr.heap_size, 0);
        assert_eq!(hdr.code_offset, NL_IMAGE_PAGE_SIZE);
        assert_eq!(hdr.code_size, native.len() as u64);

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn heap_skeleton_rejects_empty_native() {
        let path = temp_path("heap_no_native");
        match write_image_with_heap_and_native_entry(&path, &[], b"data") {
            Err(ImageError::UnsupportedTarget { .. }) => {}
            other => panic!("expected UnsupportedTarget, got {:?}", other),
        }
        assert!(!path.exists());
    }
}
