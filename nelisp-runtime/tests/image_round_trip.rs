//! Doc 47 Stage 2 — integration test that the dumper / loader pair
//! agree on the v1 wire format end-to-end through the kernel.
//!
//! Lives in `tests/` (not `src/`) because we want the same code path
//! the CLI hits — `std::fs::write` → real fd → `std::fs::read_exact`
//! — rather than the in-memory `from_bytes` round-trip already
//! covered by the `image::format::tests` unit suite.

use std::fs;
use std::path::PathBuf;
use std::process;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use nelisp_runtime::image::{
    read_header, relocs_from_bytes, write_empty_image, write_image_with_heap_and_native_entry,
    write_image_with_heap_code_and_relocs, ImageError, ImageReloc, NlImageHeader,
    NL_IMAGE_ABI_VERSION, NL_IMAGE_COMPRESSION_NONE, NL_IMAGE_HEADER_SIZE, NL_IMAGE_MAGIC,
    NL_IMAGE_PAGE_SIZE, NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET, NL_RELOC_RECORD_SIZE,
};

static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Build a per-test temp path under the OS temp dir.  No external
/// crate dependency — pid + monotonic counter + unix nanos is enough
/// to keep parallel `cargo test` runs from clobbering each other.
fn fresh_temp_path(label: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let counter = UNIQUE_COUNTER.fetch_add(1, Ordering::SeqCst);
    let mut p = std::env::temp_dir();
    p.push(format!(
        "nelisp-image-stage2-{}-{}-{}-{}.bin",
        label,
        process::id(),
        nanos,
        counter
    ));
    p
}

#[test]
fn empty_image_round_trips_through_disk() {
    let path = fresh_temp_path("rt");
    write_empty_image(&path).expect("dumper failed");

    // File is exactly the header size.
    let bytes = fs::read(&path).expect("re-read failed");
    assert_eq!(
        bytes.len(),
        NL_IMAGE_HEADER_SIZE,
        "empty image must be exactly the header size"
    );
    assert_eq!(
        &bytes[..NL_IMAGE_MAGIC.len()],
        &NL_IMAGE_MAGIC[..],
        "magic prefix must survive disk round trip"
    );

    // Loader accepts what the dumper produced.
    let hdr = read_header(&path).expect("loader rejected freshly-minted image");
    assert_eq!(hdr.magic, NL_IMAGE_MAGIC);
    assert_eq!(hdr.abi_version, NL_IMAGE_ABI_VERSION);
    assert_eq!(hdr.compression, NL_IMAGE_COMPRESSION_NONE);
    assert_eq!(hdr.payload_len, 0);

    let _ = fs::remove_file(&path);
}

#[test]
fn loader_rejects_truncated_file() {
    let path = fresh_temp_path("trunc");
    // Write 50 bytes — well short of the 104-byte header.
    fs::write(&path, vec![0u8; 50]).expect("write truncated file failed");

    match read_header(&path) {
        Err(ImageError::Truncated { expected, .. }) => {
            assert_eq!(expected, NL_IMAGE_HEADER_SIZE);
        }
        Err(other) => panic!("expected Truncated, got {:?}", other),
        Ok(_) => panic!("expected loader to reject a 50-byte file"),
    }

    let _ = fs::remove_file(&path);
}

#[test]
fn loader_rejects_bad_magic() {
    let path = fresh_temp_path("badmagic");

    // Synthesize a header-shaped file whose magic is wrong but whose
    // size matches.  Easiest path: build a real header, flip the
    // magic, write it.
    let mut hdr = NlImageHeader::new_v1();
    hdr.magic = *b"NOPENOPE";
    fs::write(&path, hdr.to_bytes()).expect("write bad-magic file failed");

    match read_header(&path) {
        Err(ImageError::Header { .. }) => {} // expected — HeaderError::BadMagic upstream
        other => panic!("expected Header(BadMagic), got {:?}", other),
    }

    let _ = fs::remove_file(&path);
}

#[test]
fn loader_rejects_missing_file() {
    let path = fresh_temp_path("missing");
    // Do NOT create the file.
    match read_header(&path) {
        Err(ImageError::Io { .. }) => {} // expected — ENOENT
        other => panic!("expected Io(ENOENT), got {:?}", other),
    }
}

#[test]
fn heap_image_round_trips_through_disk() {
    // Stage 4a: dumper writes a 2-page (header+heap, code) file, the
    // loader is happy with the heap_offset / heap_size / code_offset
    // it reads back from disk, and the actual heap byte the dumper
    // wrote is at exactly `heap_offset` on the file.
    let path = fresh_temp_path("heap-rt");
    let native = vec![0xc3u8]; // ret — meaningless on this path, dumper doesn't execute
    let heap = vec![0x55u8, 0x66, 0x77];

    write_image_with_heap_and_native_entry(&path, &native, &heap)
        .expect("dumper failed");

    let hdr = read_header(&path).expect("loader rejected heap-skeleton image");
    assert_eq!(hdr.heap_offset, NL_IMAGE_PAGE_SIZE);
    assert_eq!(hdr.heap_size, heap.len() as u64);
    assert_eq!(hdr.code_offset, 2 * NL_IMAGE_PAGE_SIZE);
    assert_eq!(hdr.code_size, native.len() as u64);
    assert_eq!(hdr.entry_offset, 0);
    assert_eq!(hdr.reloc_count, 0);

    // Re-read the file and verify the heap bytes landed at the right
    // disk offset — guards against an off-by-one / wrong-page bug
    // sneaking into the layout.
    let bytes = fs::read(&path).expect("re-read failed");
    let p = NL_IMAGE_PAGE_SIZE as usize;
    assert_eq!(&bytes[p..p + heap.len()], heap.as_slice());

    let _ = fs::remove_file(&path);
}

#[test]
fn reloc_image_round_trips_through_disk() {
    let path = fresh_temp_path("reloc-rt");
    let native = vec![0xc3u8];
    let heap = {
        let mut heap = vec![0u8; 16];
        heap[8] = 0x42;
        heap
    };
    let relocs = [ImageReloc {
        kind: NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET,
        _pad: 0,
        write_at: 0,
        addend: 8,
    }];

    write_image_with_heap_code_and_relocs(&path, &native, &heap, &relocs)
        .expect("dumper failed");

    let hdr = read_header(&path).expect("loader rejected reloc image");
    assert_eq!(hdr.heap_offset, NL_IMAGE_PAGE_SIZE);
    assert_eq!(hdr.heap_size, heap.len() as u64);
    assert_eq!(hdr.code_offset, 2 * NL_IMAGE_PAGE_SIZE);
    assert_eq!(hdr.code_size, native.len() as u64);
    assert_eq!(hdr.reloc_offset, 3 * NL_IMAGE_PAGE_SIZE);
    assert_eq!(hdr.reloc_count, 1);

    let bytes = fs::read(&path).expect("re-read failed");
    let reloc_start = hdr.reloc_offset as usize;
    let reloc_end = reloc_start + NL_RELOC_RECORD_SIZE;
    assert_eq!(
        relocs_from_bytes(&bytes[reloc_start..reloc_end], relocs.len()).unwrap(),
        relocs
    );

    let _ = fs::remove_file(&path);
}
