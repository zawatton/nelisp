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

pub mod boot;
pub mod dumper;
pub mod error;
pub mod format;
pub mod loader;
pub mod native_assets;
pub mod reloc;
pub mod signal;
pub mod value;

pub use boot::boot_from_image;
pub use dumper::{
    write_empty_image, write_image_with_heap_and_native_entry,
    write_image_with_heap_code_and_relocs, write_image_with_native_entry,
};
pub use error::ImageError;
pub use format::{
    HeaderError, NlImageHeader, NL_IMAGE_ABI_VERSION, NL_IMAGE_ABI_VERSION_MAX,
    NL_IMAGE_COMPRESSION_LZ4, NL_IMAGE_COMPRESSION_NONE, NL_IMAGE_COMPRESSION_ZSTD,
    NL_IMAGE_HEADER_SIZE, NL_IMAGE_MAGIC, NL_IMAGE_PAGE_SIZE,
};
pub use loader::read_header;
pub use native_assets::{
    NlImageEntry, HAS_NATIVE_DELIBERATE_NULL_DEREF, HAS_NATIVE_LIST_LENGTH,
    HAS_NATIVE_LOAD_CAR_INT_UNTAG, HAS_NATIVE_LOAD_HEAP_BYTE0, HAS_NATIVE_LOAD_HEAP_INT_UNTAG,
    HAS_NATIVE_LOAD_HEAP_STRING_LEN, HAS_NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
    HAS_NATIVE_LOAD_HEAP_THROUGH_PTR, HAS_NATIVE_RETURN_42, NATIVE_DELIBERATE_NULL_DEREF,
    NATIVE_LIST_LENGTH, NATIVE_LOAD_CAR_INT_UNTAG, NATIVE_LOAD_HEAP_BYTE0,
    NATIVE_LOAD_HEAP_INT_UNTAG, NATIVE_LOAD_HEAP_STRING_LEN, NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
    NATIVE_LOAD_HEAP_THROUGH_PTR, NATIVE_RETURN_42,
};
pub use reloc::{
    apply_relocations, relocs_from_bytes, relocs_to_bytes, ImageReloc, RelocError,
    NL_RELOC_KIND_HEAP_BASE_PLUS_OFFSET, NL_RELOC_RECORD_SIZE,
};
pub use signal::{install_signal_handlers, NL_IMAGE_FAULT_EXIT_CODE};
pub use value::{
    is_cons, is_int, is_nil, is_string, is_symbol, tag_cons, tag_int, tag_string, tag_symbol,
    untag_cons, untag_int, untag_string, untag_symbol, NL_VALUE_TAG_BITS, NL_VALUE_TAG_CONS,
    NL_VALUE_TAG_INT, NL_VALUE_TAG_MASK, NL_VALUE_TAG_NIL, NL_VALUE_TAG_STRING,
    NL_VALUE_TAG_SYMBOL,
};
