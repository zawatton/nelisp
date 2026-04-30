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

#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_HEAP_THROUGH_PTR: &[u8] = &[
    0x48, 0x8b, 0x06, // mov rax, [rsi]
    0x48, 0x8b, 0x00, // mov rax, [rax]
    0x0f, 0xb6, 0x00, // movzx eax, byte [rax]
    0xc3, // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_HEAP_THROUGH_PTR: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0x00, 0x00, 0x40, 0x39, // ldrb w0, [x0]
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_HEAP_THROUGH_PTR: &[u8] = &[];

pub const HAS_NATIVE_LOAD_HEAP_THROUGH_PTR: bool =
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

/// Stage 6a — load a tagged Elisp integer from `*argv[0]` and return
/// the untagged i64 (truncated to i32 by the entry signature).  The
/// SAR / ASR by 3 matches `image::value::NL_VALUE_TAG_BITS`.
///
/// Per-architecture encodings:
///   x86_64 (11 bytes):
///     48 8b 06            mov rax, [rsi]      ; rsi = argv
///     48 8b 00            mov rax, [rax]      ; *argv[0] = tagged int
///     48 c1 f8 03         sar rax, 3          ; arithmetic shift right
///     c3                  ret
///   aarch64 (16 bytes):
///     20 00 40 f9         ldr  x0, [x1]       ; x1 = argv
///     00 00 40 f9         ldr  x0, [x0]       ; *argv[0]
///     00 fc 43 93         asr  x0, x0, #3     ; SBFM xd, xn, #3, #63
///     c0 03 5f d6         ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_HEAP_INT_UNTAG: &[u8] = &[
    0x48, 0x8b, 0x06,             // mov rax, [rsi]
    0x48, 0x8b, 0x00,             // mov rax, [rax]
    0x48, 0xc1, 0xf8, 0x03,       // sar rax, 3
    0xc3,                         // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_HEAP_INT_UNTAG: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0x00, 0xfc, 0x43, 0x93, // asr  x0, x0, #3
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_HEAP_INT_UNTAG: &[u8] = &[];

pub const HAS_NATIVE_LOAD_HEAP_INT_UNTAG: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 6b — dereference a cons-cell pointer and return the car's
/// untagged i64.  Heap layout: `*argv[0]' is a tagged cons pointer
/// (low 3 bits = NL_VALUE_TAG_CONS = 0b010), the cell at the cleared
/// address holds (car, cdr) as 16 consecutive bytes, the car is a
/// tagged int.  Loader applies a reloc with addend = `8 | TAG_CONS`
/// so the OR-tag falls out of `heap_base + addend` arithmetic
/// (see `image::value' tests for the alignment-based identity).
///
/// Per-architecture encodings:
///   x86_64 (18 bytes):
///     48 8b 06            mov rax, [rsi]      ; rsi = argv
///     48 8b 08            mov rcx, [rax]      ; rcx = tagged cons ptr
///     48 83 e1 f8         and rcx, ~7         ; clear low-3-bit tag
///     48 8b 01            mov rax, [rcx]      ; rax = car (tagged int)
///     48 c1 f8 03         sar rax, 3          ; untag int
///     c3                  ret
///   aarch64 (28 bytes):
///     20 00 40 f9         ldr  x0, [x1]       ; x1 = argv
///     01 00 40 f9         ldr  x1, [x0]       ; x1 = tagged cons ptr
///     21 fc 43 d3         lsr  x1, x1, #3     ; shift out tag bits
///     21 f0 7d d3         lsl  x1, x1, #3     ; restore alignment
///     20 00 40 f9         ldr  x0, [x1]       ; x0 = car
///     00 fc 43 93         asr  x0, x0, #3     ; untag int
///     c0 03 5f d6         ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_CAR_INT_UNTAG: &[u8] = &[
    0x48, 0x8b, 0x06,                   // mov rax, [rsi]
    0x48, 0x8b, 0x08,                   // mov rcx, [rax]
    0x48, 0x83, 0xe1, 0xf8,             // and rcx, ~7
    0x48, 0x8b, 0x01,                   // mov rax, [rcx]
    0x48, 0xc1, 0xf8, 0x03,             // sar rax, 3
    0xc3,                               // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_CAR_INT_UNTAG: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x01, 0x00, 0x40, 0xf9, // ldr  x1, [x0]
    0x21, 0xfc, 0x43, 0xd3, // lsr  x1, x1, #3
    0x21, 0xf0, 0x7d, 0xd3, // lsl  x1, x1, #3
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x00, 0xfc, 0x43, 0x93, // asr  x0, x0, #3
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_CAR_INT_UNTAG: &[u8] = &[];

pub const HAS_NATIVE_LOAD_CAR_INT_UNTAG: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 6c — walk a NIL-terminated tagged-cons list and return its
/// length.  `argv[0]` is the heap_base; `*argv[0]` (= heap[0..8]) is
/// the head: either a cons pointer (low 3 bits = NL_VALUE_TAG_CONS)
/// or the all-bits `NL_VALUE_TAG_NIL` immediate.  At each cell the
/// asm advances to the cdr (offset +8 from the untagged cell pointer)
/// and bumps the count register; on NIL the loop exits and returns
/// the count.
///
/// Note the *two* heap dereferences before the loop.  Stage 4a / 6a
/// also load `*argv[0]` once to read the leaf value; Stage 6b loads
/// twice (heap_base → tagged head → cell content).  This routine
/// matches the Stage 6b dereference depth to land on the head ptr.
///
/// Per-architecture encodings:
///   x86_64 (27 bytes):
///     31 c0                xor eax, eax            ; count = 0
///     48 8b 0e             mov rcx, [rsi]          ; rcx = heap_base
///     48 8b 09             mov rcx, [rcx]          ; rcx = *heap_base = head
///     ; .loop:
///     48 83 f9 03          cmp rcx, 3              ; NL_VALUE_TAG_NIL
///     74 0c                je  .end (+12)
///     ff c0                inc eax                  ; count++
///     48 83 e1 f8          and rcx, ~7             ; cell ptr
///     48 8b 49 08          mov rcx, [rcx + 8]      ; cdr
///     eb ee                jmp .loop (-18)
///     ; .end:
///     c3                   ret
///   aarch64 (44 bytes):
///     00 00 80 52          mov  w0, #0
///     21 00 40 f9          ldr  x1, [x1]           ; x1 = heap_base
///     21 00 40 f9          ldr  x1, [x1]           ; x1 = *heap_base = head
///     ; .loop:
///     3f 0c 00 f1          cmp  x1, #3             ; NL_VALUE_TAG_NIL
///     c0 00 00 54          b.eq .end (+24)
///     00 04 00 11          add  w0, w0, #1          ; count++
///     21 fc 43 d3          lsr  x1, x1, #3
///     21 f0 7d d3          lsl  x1, x1, #3          ; clear tag bits
///     21 04 40 f9          ldr  x1, [x1, #8]        ; cdr
///     fa ff ff 17          b    .loop (-24)
///     ; .end:
///     c0 03 5f d6          ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LIST_LENGTH: &[u8] = &[
    0x31, 0xc0,                   // xor eax, eax
    0x48, 0x8b, 0x0e,             // mov rcx, [rsi]
    0x48, 0x8b, 0x09,             // mov rcx, [rcx]
    // .loop:
    0x48, 0x83, 0xf9, 0x03,       // cmp rcx, 3
    0x74, 0x0c,                   // je .end
    0xff, 0xc0,                   // inc eax
    0x48, 0x83, 0xe1, 0xf8,       // and rcx, ~7
    0x48, 0x8b, 0x49, 0x08,       // mov rcx, [rcx+8]
    0xeb, 0xee,                   // jmp .loop
    // .end:
    0xc3,                         // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LIST_LENGTH: &[u8] = &[
    0x00, 0x00, 0x80, 0x52, // mov  w0, #0
    0x21, 0x00, 0x40, 0xf9, // ldr  x1, [x1]
    0x21, 0x00, 0x40, 0xf9, // ldr  x1, [x1]
    // .loop:
    0x3f, 0x0c, 0x00, 0xf1, // cmp  x1, #3
    0xc0, 0x00, 0x00, 0x54, // b.eq .end
    0x00, 0x04, 0x00, 0x11, // add  w0, w0, #1
    0x21, 0xfc, 0x43, 0xd3, // lsr  x1, x1, #3
    0x21, 0xf0, 0x7d, 0xd3, // lsl  x1, x1, #3
    0x21, 0x04, 0x40, 0xf9, // ldr  x1, [x1, #8]
    0xfa, 0xff, 0xff, 0x17, // b    .loop
    // .end:
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LIST_LENGTH: &[u8] = &[];

pub const HAS_NATIVE_LIST_LENGTH: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 6e — load the byte length of a tagged string at heap[0..8].
/// String layout: `[u64 len][bytes..][pad]'.  Asm chases:
///   *argv[0] → tagged string ptr → AND ~7 → struct addr →
///   *(struct + 0) → u64 len → ret.
///
///   x86_64 (14 bytes):
///     48 8b 06            mov rax, [rsi]      ; rsi = argv
///     48 8b 00            mov rax, [rax]      ; tagged string ptr
///     48 83 e0 f8         and rax, ~7         ; struct addr
///     48 8b 00            mov rax, [rax]      ; u64 length
///     c3                  ret
///   aarch64 (24 bytes):
///     20 00 40 f9         ldr  x0, [x1]
///     00 00 40 f9         ldr  x0, [x0]
///     00 fc 43 d3         lsr  x0, x0, #3
///     00 f0 7d d3         lsl  x0, x0, #3
///     00 00 40 f9         ldr  x0, [x0]
///     c0 03 5f d6         ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_HEAP_STRING_LEN: &[u8] = &[
    0x48, 0x8b, 0x06,                   // mov rax, [rsi]
    0x48, 0x8b, 0x00,                   // mov rax, [rax]
    0x48, 0x83, 0xe0, 0xf8,             // and rax, ~7
    0x48, 0x8b, 0x00,                   // mov rax, [rax]
    0xc3,                               // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_HEAP_STRING_LEN: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0x00, 0xfc, 0x43, 0xd3, // lsr  x0, x0, #3
    0x00, 0xf0, 0x7d, 0xd3, // lsl  x0, x0, #3
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_HEAP_STRING_LEN: &[u8] = &[];

pub const HAS_NATIVE_LOAD_HEAP_STRING_LEN: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 6e — chase a tagged symbol → its name string struct → name's
/// byte length.  Symbol layout: `[ptr-to-name][value-slot]'.  Two
/// untag-and-deref steps before the final length load:
///   *argv[0] → tagged symbol → AND ~7 → symbol struct →
///   *(struct + 0) → tagged string ptr → AND ~7 → string struct →
///   *(struct + 0) → u64 length → ret.
///
///   x86_64 (21 bytes):
///     48 8b 06            mov rax, [rsi]
///     48 8b 00            mov rax, [rax]      ; tagged sym ptr
///     48 83 e0 f8         and rax, ~7         ; sym struct
///     48 8b 00            mov rax, [rax]      ; tagged name ptr
///     48 83 e0 f8         and rax, ~7         ; name string struct
///     48 8b 00            mov rax, [rax]      ; u64 length
///     c3                  ret
///   aarch64 (36 bytes):
///     20 00 40 f9         ldr  x0, [x1]
///     00 00 40 f9         ldr  x0, [x0]       ; tagged sym ptr
///     00 fc 43 d3         lsr  x0, x0, #3
///     00 f0 7d d3         lsl  x0, x0, #3     ; sym struct
///     00 00 40 f9         ldr  x0, [x0]       ; tagged name ptr
///     00 fc 43 d3         lsr  x0, x0, #3
///     00 f0 7d d3         lsl  x0, x0, #3     ; name struct
///     00 00 40 f9         ldr  x0, [x0]       ; u64 length
///     c0 03 5f d6         ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN: &[u8] = &[
    0x48, 0x8b, 0x06,                   // mov rax, [rsi]
    0x48, 0x8b, 0x00,                   // mov rax, [rax]
    0x48, 0x83, 0xe0, 0xf8,             // and rax, ~7
    0x48, 0x8b, 0x00,                   // mov rax, [rax]
    0x48, 0x83, 0xe0, 0xf8,             // and rax, ~7
    0x48, 0x8b, 0x00,                   // mov rax, [rax]
    0xc3,                               // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0x00, 0xfc, 0x43, 0xd3, // lsr  x0, x0, #3
    0x00, 0xf0, 0x7d, 0xd3, // lsl  x0, x0, #3
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0x00, 0xfc, 0x43, 0xd3, // lsr  x0, x0, #3
    0x00, 0xf0, 0x7d, 0xd3, // lsl  x0, x0, #3
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN: &[u8] = &[];

pub const HAS_NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 7b-2 — chase a tagged f64 pointer, load the unboxed double,
/// truncate to i64 and return.  Heap layout: `[f64-bits]' (single
/// 8-byte word).  Asm:
///   *argv[0] -> tagged float ptr -> AND ~7 -> struct addr ->
///   movsd xmm0, [addr] -> cvttsd2si rax, xmm0 -> ret
///
///   x86_64 (20 bytes):
///     48 8b 06            mov rax, [rsi]
///     48 8b 00            mov rax, [rax]      ; tagged float ptr
///     48 83 e0 f8         and rax, ~7         ; struct addr
///     f2 0f 10 00         movsd xmm0, [rax]
///     f2 48 0f 2c c0      cvttsd2si rax, xmm0
///     c3                  ret
///   aarch64 (28 bytes):
///     20 00 40 f9         ldr  x0, [x1]
///     00 00 40 f9         ldr  x0, [x0]
///     00 fc 43 d3         lsr  x0, x0, #3
///     00 f0 7d d3         lsl  x0, x0, #3
///     00 00 40 fd         ldr  d0, [x0]
///     00 00 78 9e         fcvtzs x0, d0
///     c0 03 5f d6         ret
#[cfg(target_arch = "x86_64")]
pub const NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC: &[u8] = &[
    0x48, 0x8b, 0x06,                   // mov rax, [rsi]
    0x48, 0x8b, 0x00,                   // mov rax, [rax]
    0x48, 0x83, 0xe0, 0xf8,             // and rax, ~7
    0xf2, 0x0f, 0x10, 0x00,             // movsd xmm0, [rax]
    0xf2, 0x48, 0x0f, 0x2c, 0xc0,       // cvttsd2si rax, xmm0
    0xc3,                               // ret
];

#[cfg(target_arch = "aarch64")]
pub const NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC: &[u8] = &[
    0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
    0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
    0x00, 0xfc, 0x43, 0xd3, // lsr  x0, x0, #3
    0x00, 0xf0, 0x7d, 0xd3, // lsl  x0, x0, #3
    0x00, 0x00, 0x40, 0xfd, // ldr  d0, [x0]
    0x00, 0x00, 0x78, 0x9e, // fcvtzs x0, d0
    0xc0, 0x03, 0x5f, 0xd6, // ret
];

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC: &[u8] = &[];

pub const HAS_NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));
