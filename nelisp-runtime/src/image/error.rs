//! Unified error type for the `image` module.
//!
//! `format::HeaderError` covers static structural defects (bad magic,
//! future ABI, unsupported compression).  `ImageError` adds I/O and
//! truncation failures so the loader / dumper / CLI can surface a
//! single error vocabulary to callers without leaking `std::io::Error`
//! at every layer.
//!
//! Stage 2 keeps this small.  Stage 3 will extend with relocation /
//! segment-bounds variants once `apply_relocations` lands.

use core::fmt;
use std::io;
use std::path::PathBuf;

use super::format::HeaderError;

/// Top-level error returned by `read_header` / `write_*_image` and
/// the `--mint-empty-image` / `--verify-image` CLI dispatch.
#[derive(Debug)]
pub enum ImageError {
    /// Anything coming back from `std::io` while reading or writing
    /// the image file.  `path` is the file the seed / dumper tried
    /// to touch — useful in CLI error messages.
    Io { path: PathBuf, source: io::Error },

    /// File is shorter than `NL_IMAGE_HEADER_SIZE` bytes.  Distinct
    /// from `Io` so callers can `match` on it without classifying
    /// platform-dependent `io::ErrorKind` variants.
    Truncated {
        path: PathBuf,
        expected: usize,
        got: usize,
    },

    /// Header parsed but failed semantic validation.  Wraps
    /// `format::HeaderError` so the loader can return a single error
    /// type while preserving the original cause.
    Header { path: PathBuf, source: HeaderError },

    /// Image's `code_size` is zero.  Stage 3 boot path requires a
    /// non-empty native code arena — header-only images mint cleanly
    /// (Stage 2 path) but cannot be booted.
    NoCodeSegment { path: PathBuf },

    /// Build target lacks a hand-written native asset.  Surfaced by
    /// `mint-skeleton-image` on architectures that are neither
    /// x86_64 nor aarch64.
    UnsupportedTarget { reason: &'static str },
}

impl ImageError {
    /// Path the failure refers to — every variant carries one.  Lets
    /// CLI surface print `nelisp-runtime: <op> <path>: <reason>`
    /// without a separate match.
    pub fn path(&self) -> Option<&PathBuf> {
        match self {
            ImageError::Io { path, .. } => Some(path),
            ImageError::Truncated { path, .. } => Some(path),
            ImageError::Header { path, .. } => Some(path),
            ImageError::NoCodeSegment { path } => Some(path),
            ImageError::UnsupportedTarget { .. } => None,
        }
    }
}

impl fmt::Display for ImageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImageError::Io { path, source } => {
                write!(f, "i/o error on {}: {}", path.display(), source)
            }
            ImageError::Truncated {
                path,
                expected,
                got,
            } => write!(
                f,
                "image truncated at {}: expected {} bytes of header, got {}",
                path.display(),
                expected,
                got
            ),
            ImageError::Header { path, source } => {
                write!(f, "header invalid in {}: {:?}", path.display(), source)
            }
            ImageError::NoCodeSegment { path } => write!(
                f,
                "image at {} has no code segment (Stage 3 boot requires a non-empty native arena)",
                path.display()
            ),
            ImageError::UnsupportedTarget { reason } => {
                write!(f, "unsupported target: {}", reason)
            }
        }
    }
}

impl std::error::Error for ImageError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ImageError::Io { source, .. } => Some(source),
            ImageError::Truncated { .. } => None,
            ImageError::Header { .. } => None,
            ImageError::NoCodeSegment { .. } => None,
            ImageError::UnsupportedTarget { .. } => None,
        }
    }
}
