//! Doc 47 Stage 3 — hand-written native code assets for the
//! walking-skeleton boot path.
//!
//! Goal: lock the loader / dumper / mmap / clear-icache / transmute
//! chain end-to-end against real CPU execution, *without* pulling in
//! the Rust evaluator (which Doc 47 demotes to a build-time tool).
//! The image entry point is a tiny `extern "C" fn(argc, argv) -> i32`
//! that ignores its arguments and returns 42 — small enough to read
//! the bytes by eye and grep for in `objdump -d`.
//!
//! Per-architecture encodings (verified against System V AMD64 / AAPCS64):
//!   x86_64:
//!     b8 2a 00 00 00      mov eax, 42
//!     c3                  ret
//!   aarch64:
//!     40 05 80 52         mov w0, #42         (MOV (immediate, wide), sf=0, hw=0, imm16=42)
//!     c0 03 5f d6         ret                 (RET x30)
//!
//! On unsupported targets the const is empty and the
//! `mint-skeleton-image' CLI surface refuses to mint with
//! `ImageError::UnsupportedTarget`.

#[cfg(target_arch = "x86_64")]
pub const NATIVE_RETURN_42: &[u8] = &[
    0xb8, 0x2a, 0x00, 0x00, 0x00, // mov eax, 42
    0xc3,                         // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_RETURN_42: &[u8] = &[
    0x40, 0x05, 0x80, 0x52, // mov w0, #42
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_RETURN_42: &[u8] = &[];

/// Walking-skeleton entry-point ABI — mirrors Doc 47 §2.2 exactly so
/// the same signature is reused when Stage 4 promotes from
/// `native_assets` to real Phase 7 native arenas.
pub type NlImageEntry = unsafe extern "C" fn(argc: i32, argv: *const *const u8) -> i32;

/// Whether the current build targets an architecture for which
/// `NATIVE_RETURN_42' contains real machine code.  The CLI uses this
/// to fail fast with a friendly message on unsupported hosts instead
/// of writing an empty code segment that boot-from-image would refuse.
pub const HAS_NATIVE_RETURN_42: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));
