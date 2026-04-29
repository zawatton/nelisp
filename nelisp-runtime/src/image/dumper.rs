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
use super::format::NlImageHeader;

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
