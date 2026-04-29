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

/// Doc 47 Stage 4a — load byte 0 of `argv[0]` and return it as `i32`.
/// Used to prove the heap mmap + argv plumbing end-to-end: the seed
/// mmaps the heap segment RW, builds a one-element argv whose only
/// element is the heap base pointer, and the canned code reads the
/// first byte from that pointer.  If the heap is filled with byte
/// `0x37`, the process exits with code `55`.
///
/// Per-architecture encodings (verified against System V AMD64 / AAPCS64):
///   x86_64:
///     48 8b 06            mov rax, [rsi]      (rsi = argv, [rsi] = argv[0] = heap_ptr)
///     0f b6 00            movzx eax, byte [rax]
///     c3                  ret
///   aarch64:
///     20 00 40 f9         ldr x0, [x1]        (x1 = argv, [x1] = argv[0] = heap_ptr)
///     00 00 40 39         ldrb w0, [x0]
///     c0 03 5f d6         ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_HEAP_BYTE0: &[u8] = &[
    0x48, 0x8b, 0x06, // mov rax, [rsi]
    0x0f, 0xb6, 0x00, // movzx eax, byte [rax]
    0xc3,             // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_HEAP_BYTE0: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr x0, [x1]
    0x00, 0x00, 0x40, 0x39, // ldrb w0, [x0]
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_HEAP_BYTE0: &[u8] = &[];

/// Whether the current build targets an architecture for which
/// `NATIVE_LOAD_HEAP_BYTE0' contains real machine code.  Mirrors the
/// `HAS_NATIVE_RETURN_42' flag for Stage 3 so the CLI can refuse to
/// mint on unsupported hosts before writing the file.
pub const HAS_NATIVE_LOAD_HEAP_BYTE0: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 4c — deliberately dereference null to trigger SIGSEGV.
/// Used by `mint-fault-skeleton-image` to smoke-test the
/// signal-handler skeleton: with handlers installed the seed
/// exits cleanly with `NL_IMAGE_FAULT_EXIT_CODE` (= 130).
#[cfg(target_arch = "x86_64")]
pub const NATIVE_DELIBERATE_NULL_DEREF: &[u8] = &[
    0x48, 0x31, 0xc0, // xor rax, rax
    0x0f, 0xb6, 0x00, // movzx eax, byte [rax]
    0xc3,             // ret (unreachable)
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_DELIBERATE_NULL_DEREF: &[u8] = &[
    0x00, 0x00, 0x80, 0xd2, // movz x0, #0
    0x00, 0x00, 0x40, 0x39, // ldrb w0, [x0]
    0xc0, 0x03, 0x5f, 0xd6, // ret (unreachable)
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_DELIBERATE_NULL_DEREF: &[u8] = &[];

pub const HAS_NATIVE_DELIBERATE_NULL_DEREF: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));
