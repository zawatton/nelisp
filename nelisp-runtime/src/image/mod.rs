//! NlImage v1 — Rust seed image loader (Doc 47 Stage 1 stub).
//!
//! Stage 1 ships only the on-disk header format (`format.rs`).  The
//! seed boot path that consumes it lives in Stage 2:
//!
//! ```text
//! Stage 1 (this commit)        — header struct + magic + version + verify
//! Stage 2 (next)               — `read_header`, `load_segment`, `apply_relocations`
//! Stage 3 (= ABI walking-skel) — `mint_image` (build-tool) + `boot_from_image` (seed)
//! ```
//!
//! See `docs/design/47-rust-seed-image-abi.org` for the full design.
//! Re-exports below are the public surface that Stage 2 will extend
//! without breaking the Stage 1 import paths.

pub mod dumper;
pub mod error;
pub mod format;
pub mod loader;

pub use error::ImageError;
pub use format::{
    HeaderError, NlImageHeader, NL_IMAGE_ABI_VERSION, NL_IMAGE_ABI_VERSION_MAX,
    NL_IMAGE_COMPRESSION_LZ4, NL_IMAGE_COMPRESSION_NONE, NL_IMAGE_COMPRESSION_ZSTD,
    NL_IMAGE_HEADER_SIZE, NL_IMAGE_MAGIC, NL_IMAGE_PAGE_SIZE,
};
pub use loader::read_header;
pub use dumper::write_empty_image;
