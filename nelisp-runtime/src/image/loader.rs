//! Read an `NlImageHeader` from a file on disk.
//!
//! Stage 2 deliberately uses `std::fs::File` rather than the
//! `nelisp_syscall_*` ABI: for the dumper / verifier CLI the host
//! kernel's stdio is fine, and the loader will move to the syscall
//! ABI in Stage 3 once the seed crate is being carved out.  Comment
//! marker `TODO(stage-3-seed-syscall)` flags the upgrade point.
//!
//! See `dumper.rs` for the matching write path.  The two files
//! together prove the v1 wire format round-trips byte-for-byte
//! through the kernel.

use std::fs::File;
use std::io::Read;
use std::path::Path;

use super::error::ImageError;
use super::format::{NlImageHeader, NL_IMAGE_HEADER_SIZE};

/// Read the first `NL_IMAGE_HEADER_SIZE` bytes from `path`, parse
/// them into an `NlImageHeader`, and run `verify_magic_and_version`.
///
/// Returns the validated header on success; on failure the caller
/// receives an `ImageError` describing whether the file was missing,
/// truncated, or malformed.  This is the single entry point Stage 3
/// `boot_from_image` will compose with `load_segment` and
/// `apply_relocations`.
///
/// TODO(stage-3-seed-syscall): swap `File::open` / `read_exact` for
/// `nelisp_syscall_open` / `nelisp_syscall_read` once we extract the
/// seed-only crate, so the runtime keeps its libc surface minimal.
pub fn read_header<P: AsRef<Path>>(path: P) -> Result<NlImageHeader, ImageError> {
    let path = path.as_ref();
    let mut file = File::open(path).map_err(|source| ImageError::Io {
        path: path.to_path_buf(),
        source,
    })?;

    let mut buf = [0u8; NL_IMAGE_HEADER_SIZE];
    if let Err(source) = file.read_exact(&mut buf) {
        if matches!(source.kind(), std::io::ErrorKind::UnexpectedEof) {
            // Distinguish "file shorter than header" from generic I/O
            // so verification messages stay readable.
            return Err(ImageError::Truncated {
                path: path.to_path_buf(),
                expected: NL_IMAGE_HEADER_SIZE,
                got: 0, // refined below if a partial read becomes possible
            });
        }
        return Err(ImageError::Io {
            path: path.to_path_buf(),
            source,
        });
    }

    let hdr = NlImageHeader::from_bytes(&buf).ok_or(ImageError::Truncated {
        path: path.to_path_buf(),
        expected: NL_IMAGE_HEADER_SIZE,
        got: NL_IMAGE_HEADER_SIZE,
    })?;

    hdr.verify_magic_and_version()
        .map_err(|source| ImageError::Header {
            path: path.to_path_buf(),
            source,
        })?;

    Ok(hdr)
}
