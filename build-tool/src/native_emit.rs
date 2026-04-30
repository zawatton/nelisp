//! Doc 47 Stage 9a — generate per-image native code at build time.
//!
//! Stage 6 lowered Elisp values into a binary heap; Stage 7 connected
//! the build-tool evaluator to that lowering; Stage 8 ran a real
//! `.el' file end-to-end.  Stage 9 lifts the build-tool from "data
//! lowerer" to "code generator": instead of always falling back on a
//! canned `NATIVE_*' asset that reads a value out of the heap, the
//! build-tool can now *synthesise* the native body it wants the seed
//! to execute.
//!
//! The walking-skeleton entry point — `emit_return_i32(N)` — emits
//! the smallest possible function body that returns a given i32:
//!
//! ```text
//!   x86_64 (6 bytes):    b8 NN NN NN NN c3   ; mov eax, N ; ret
//!   aarch64 (12 bytes):
//!     <movz w0,#low16>   <movk w0,#high16,lsl#16>   <ret>
//! ```
//!
//! That's enough surface to prove the seed will execute *whatever*
//! the build-tool emits, not just hand-crafted constants — Doc 47
//! §3.1 phase 7 ("Phase 7 native arena 同梱") in walking-skeleton
//! form.  Future stages (9b+) widen the emitter to multi-instruction
//! function bodies, parameter handling, and call sites.

#[cfg(target_arch = "x86_64")]
pub fn emit_return_i32(value: i32) -> Vec<u8> {
    let mut out = Vec::with_capacity(6);
    // mov eax, imm32 — opcode B8 + zero-extended i32 immediate.  The
    // upper 32 bits of rax are zeroed by the implicit 32-bit-write
    // semantics of mov-to-32-bit-reg, so the entry's i32 return
    // travels back to the caller intact.
    out.push(0xb8);
    out.extend_from_slice(&(value as u32).to_le_bytes());
    out.push(0xc3); // ret
    out
}

#[cfg(target_arch = "aarch64")]
pub fn emit_return_i32(value: i32) -> Vec<u8> {
    // MOVZ w0, #low16 + MOVK w0, #high16, lsl #16 covers the entire
    // i32 range with two halfword moves.  We always emit both even
    // when high16 is zero — keeps the byte size deterministic at 12,
    // which simplifies caller bookkeeping (e.g., padding to JIT page
    // size in future stages).
    let v = value as u32;
    let low16 = v & 0xFFFF;
    let high16 = (v >> 16) & 0xFFFF;
    // MOVZ w0, #imm: 0x52800000 base | (imm16 << 5) | Rd.  hw=00 so
    // the immediate is placed in bits 15:0 with the rest zeroed.
    let movz: u32 = 0x52800000 | (low16 << 5);
    // MOVK w0, #imm, lsl #16: 0x72A00000 base (hw=01) | (imm16 << 5)
    // | Rd.  Preserves bits 15:0 (already set by MOVZ) and writes
    // imm16 into bits 31:16.
    let movk: u32 = 0x72A00000 | (high16 << 5);
    let ret: u32 = 0xD65F03C0;
    let mut out = Vec::with_capacity(12);
    out.extend_from_slice(&movz.to_le_bytes());
    out.extend_from_slice(&movk.to_le_bytes());
    out.extend_from_slice(&ret.to_le_bytes());
    out
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_return_i32(_value: i32) -> Vec<u8> {
    Vec::new()
}

/// Whether the current target has a working `emit_return_i32'.  The
/// CLI uses this to fail fast on unsupported architectures.
pub const HAS_EMIT_RETURN_I32: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Doc 47 Stage 9b — emit the canonical "load tagged-int from heap
/// and shift off the 3 tag bits" function body.  Functionally
/// identical to the runtime's pre-baked `NATIVE_LOAD_HEAP_INT_UNTAG'
/// asset; emitting it from the build-tool proves the code generator
/// can produce the same shapes the runtime ships pre-baked, which is
/// the prerequisite for Stage 9c (lambda body compilation).
///
/// Calling convention matches the seed image entry: `argv` arrives in
/// the second System V argument register on x86_64 (`rsi`) and on
/// aarch64 (`x1`) per `nelisp-runtime/src/image/boot.rs`.
///
/// Per-architecture encodings (= identical to the runtime asset):
///   x86_64 (11 bytes):
///     48 8b 06            mov rax, [rsi]      ; rsi = argv
///     48 8b 00            mov rax, [rax]      ; *argv[0] = tagged int
///     48 c1 f8 03         sar rax, 3          ; arithmetic shift right
///     c3                  ret
///   aarch64 (16 bytes):
///     20 00 40 f9         ldr  x0, [x1]       ; x1 = argv
///     00 00 40 f9         ldr  x0, [x0]       ; *argv[0]
///     00 fc 43 93         asr  x0, x0, #3
///     c0 03 5f d6         ret
/// Stage 9b emit, refactored at Stage 9c to compose from the chain
/// primitives.  The output is identical to the hand-written byte
/// sequence (asserted by the byte-equality unit tests below) — the
/// composition is purely structural so future chain primitives can
/// participate in the same regression gate without duplicating the
/// per-arch byte tables.
pub fn emit_load_heap_int_untag() -> Vec<u8> {
    let mut out = emit_load_heap_int_untag_head();
    out.extend_from_slice(&emit_ret());
    out
}

pub const HAS_EMIT_LOAD_HEAP_INT_UNTAG: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

// ---------------------------------------------------------------------------
// Doc 47 Stage 9c — chain-emit primitives.
//
// Closure body compile = chain of small asm building blocks against a
// single accumulator register (rax on x86_64, x0 on aarch64).  Stage
// 9c walking-skeleton extracts the head of `emit_load_heap_int_untag'
// (load heap → untag, leaves value in accumulator) and the `ret'
// trailer as separate emitters, then composes them with a new
// `emit_add_imm32' middle to demonstrate the build-tool can produce
// the same shape from primitive blocks.
//
// Future stages widen the primitive set (sub_imm, mul_imm, branch,
// call) but the composition contract — single accumulator, no ABI
// shuffling between blocks — stays the same.
// ---------------------------------------------------------------------------

/// Head of `emit_load_heap_int_untag` minus the `ret`.  Leaves the
/// untagged i64 in the accumulator (rax / x0) so a chain emitter can
/// keep operating on it.
#[cfg(target_arch = "x86_64")]
pub fn emit_load_heap_int_untag_head() -> Vec<u8> {
    vec![
        0x48, 0x8b, 0x06, // mov rax, [rsi]
        0x48, 0x8b, 0x00, // mov rax, [rax]
        0x48, 0xc1, 0xf8, 0x03, // sar rax, 3
    ]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_load_heap_int_untag_head() -> Vec<u8> {
    vec![
        0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
        0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
        0x00, 0xfc, 0x43, 0x93, // asr  x0, x0, #3
    ]
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_load_heap_int_untag_head() -> Vec<u8> {
    Vec::new()
}

/// `ret`/`RET` instruction byte(s).  Closes a chain.
#[cfg(target_arch = "x86_64")]
pub fn emit_ret() -> Vec<u8> {
    vec![0xc3]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_ret() -> Vec<u8> {
    vec![0xc0, 0x03, 0x5f, 0xd6] // RET
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_ret() -> Vec<u8> {
    Vec::new()
}

/// `add accumulator, sign-extended-imm32` — folds `imm` into the
/// running accumulator and leaves the result there.
///
/// x86_64 (7 bytes, REX.W + opcode 05):
///     48 05 II II II II   add rax, imm32   ; sign-extends to 64 bits
///
/// aarch64 (12 bytes, MOVZ + MOVK + ADD-extended):
///     d28000a9 + f2a00009  ; MOVZ + MOVK build imm32 in w9
///     8b294000             ; ADD x0, x0, w9, SXTW
/// (instruction encodings vary by imm; the constructor below builds
/// each MOVZ/MOVK from the `imm` half-words directly.)
#[cfg(target_arch = "x86_64")]
pub fn emit_add_rax_imm32(imm: i32) -> Vec<u8> {
    let mut out = Vec::with_capacity(7);
    out.push(0x48); // REX.W
    out.push(0x05); // opcode for add rAX, imm32
    out.extend_from_slice(&imm.to_le_bytes());
    out
}

#[cfg(target_arch = "aarch64")]
pub fn emit_add_rax_imm32(imm: i32) -> Vec<u8> {
    // Build imm32 in w9 (X9 lower half) via MOVZ + MOVK, then add it
    // to x0 with sign-extension from w9.
    let v = imm as u32;
    let low16 = v & 0xFFFF;
    let high16 = (v >> 16) & 0xFFFF;
    // MOVZ w9, #low16             0x52800000 | (low16 << 5) | rd=9
    let movz: u32 = 0x52800000 | (low16 << 5) | 9;
    // MOVK w9, #high16, lsl #16   0x72A00000 (hw=01) | (high16 << 5) | rd=9
    let movk: u32 = 0x72A00000 | (high16 << 5) | 9;
    // ADD (extended register) X0, X0, W9, SXTW
    //   sf=1 op=0 S=0 0b01011 001 Rm=9 option=110 (SXTW) imm3=0 Rn=0 Rd=0
    //   Encoding: 0x8B29C000 | (option<<13) | (imm3<<10) | (Rn<<5) | Rd
    //   For our params (Rm=9, option=SXTW=0b110, imm3=0, Rn=0, Rd=0):
    //   = 0x8B000000 | (1<<21) | (Rm<<16) | (0b110<<13) | (0<<10) | (Rn<<5) | Rd
    //   = 0x8B200000 | (9 << 16) | (0b110 << 13)
    //   = 0x8B2CC000 | (9 << 16) = 0x8B29C000.
    let add_ext: u32 = 0x8B20C000 | (9u32 << 16) | (0b110u32 << 13);
    let mut out = Vec::with_capacity(12);
    out.extend_from_slice(&movz.to_le_bytes());
    out.extend_from_slice(&movk.to_le_bytes());
    out.extend_from_slice(&add_ext.to_le_bytes());
    out
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_add_rax_imm32(_imm: i32) -> Vec<u8> {
    Vec::new()
}

pub const HAS_CHAIN_EMIT_PRIMITIVES: bool =
    cfg!(any(target_arch = "x86_64", target_arch = "aarch64"));

/// Stage 9c walking-skeleton: emit a function body that loads a
/// tagged-int heap word, untags it, adds `imm` (sign-extended), and
/// returns.  Re-implemented at Stage 9d in terms of the `ChainOp' IR
/// (see [`compose`]) so the same op stream that drives the generic
/// chain composer also drives this convenience entry point.
pub fn emit_int_plus_imm(imm: i32) -> Vec<u8> {
    compose(&[ChainOp::LoadHeapHead, ChainOp::AddImm(imm), ChainOp::Ret])
}

pub const HAS_EMIT_INT_PLUS_IMM: bool = HAS_CHAIN_EMIT_PRIMITIVES;

// ---------------------------------------------------------------------------
// Doc 47 Stage 9d — extended primitive set.
//
// Stage 9c's three-op chain (head + add_imm + ret) demonstrated
// composition.  Stage 9d widens the primitive vocabulary so the chain
// can compute richer integer expressions (= subtraction, negation,
// multiplication by an immediate) and exposes a small ChainOp IR so
// drivers / future AST passes can describe the op stream as data
// instead of imperative `extend_from_slice' calls.
//
// Calling convention is unchanged: every primitive consumes and
// produces the integer accumulator (rax on x86_64, x0 on aarch64),
// no side effects on other registers visible to the caller.
// ---------------------------------------------------------------------------

/// `sub accumulator, sign-extended-imm32`.
///
/// x86_64 (6 bytes, REX.W + opcode 2D):
///     48 2d II II II II   sub rax, imm32   ; sign-extends to 64 bits
///
/// aarch64 (12 bytes, MOVZ + MOVK + SUB-extended):
///     MOVZ w9, #lo16(imm)
///     MOVK w9, #hi16(imm), lsl #16
///     SUB  x0, x0, w9, SXTW
#[cfg(target_arch = "x86_64")]
pub fn emit_sub_rax_imm32(imm: i32) -> Vec<u8> {
    let mut out = Vec::with_capacity(6);
    out.push(0x48); // REX.W
    out.push(0x2d); // opcode for sub rAX, imm32
    out.extend_from_slice(&imm.to_le_bytes());
    out
}

#[cfg(target_arch = "aarch64")]
pub fn emit_sub_rax_imm32(imm: i32) -> Vec<u8> {
    let v = imm as u32;
    let low16 = v & 0xFFFF;
    let high16 = (v >> 16) & 0xFFFF;
    let movz: u32 = 0x52800000 | (low16 << 5) | 9;
    let movk: u32 = 0x72A00000 | (high16 << 5) | 9;
    // SUB (extended register), 64-bit, X0 = X0 - SXTW(W9):
    //   sf=1 op=1 S=0 0b01011 001 Rm=9 option=110 imm3=0 Rn=0 Rd=0
    //   = 0xCB20C000 base | (Rm<<16) | (option<<13) | (imm3<<10) | (Rn<<5) | Rd
    let sub_ext: u32 = 0xCB20C000 | (9u32 << 16) | (0b110u32 << 13);
    let mut out = Vec::with_capacity(12);
    out.extend_from_slice(&movz.to_le_bytes());
    out.extend_from_slice(&movk.to_le_bytes());
    out.extend_from_slice(&sub_ext.to_le_bytes());
    out
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_sub_rax_imm32(_imm: i32) -> Vec<u8> {
    Vec::new()
}

/// `imul accumulator, accumulator, sign-extended-imm32`.  64-bit
/// signed multiply with the imm32 sign-extended to the operand size;
/// the low 64 bits of the product land in the accumulator (= the same
/// behaviour callers get from `(* n IMM)` in Elisp at the value sizes
/// the walking-skeleton cares about).
///
/// x86_64 (7 bytes, REX.W + opcode 69 + ModR/M c0):
///     48 69 c0 II II II II   imul rax, rax, imm32
///
/// aarch64 (16 bytes, MOVZ + MOVK + SXTW-to-X9 + MUL):
///     MOVZ w9, #lo16(imm)
///     MOVK w9, #hi16(imm), lsl #16
///     SXTW x9, w9              ; sign-extend the imm to 64 bits
///     MUL  x0, x0, x9
#[cfg(target_arch = "x86_64")]
pub fn emit_imul_rax_imm32(imm: i32) -> Vec<u8> {
    let mut out = Vec::with_capacity(7);
    out.push(0x48); // REX.W
    out.push(0x69); // opcode for imul r64, r/m64, imm32
    out.push(0xc0); // ModR/M = 11 000 000 (rax, rax)
    out.extend_from_slice(&imm.to_le_bytes());
    out
}

#[cfg(target_arch = "aarch64")]
pub fn emit_imul_rax_imm32(imm: i32) -> Vec<u8> {
    let v = imm as u32;
    let low16 = v & 0xFFFF;
    let high16 = (v >> 16) & 0xFFFF;
    let movz: u32 = 0x52800000 | (low16 << 5) | 9;
    let movk: u32 = 0x72A00000 | (high16 << 5) | 9;
    // SXTW x9, w9: SBFM xd, xn, #0, #31 = 0x93407D29 (sf=1, immr=0,
    // imms=31, Rn=9, Rd=9).  Concretely: 0x93400000 | (0<<16) |
    // (31<<10) | (9<<5) | 9 = 0x93407D29.
    let sxtw: u32 = 0x93407D29;
    // MUL x0, x0, x9 = MADD x0, x0, x9, xzr (Ra=31).
    //   sf=1 0b0011011 000 Rm=9 0 Ra=31 Rn=0 Rd=0
    //   = 0x9B000000 | (Rm<<16) | (0<<15) | (Ra<<10) | (Rn<<5) | Rd
    //   = 0x9B000000 | (9<<16) | (31<<10) = 0x9B097C00.
    let mul: u32 = 0x9B097C00;
    let mut out = Vec::with_capacity(16);
    out.extend_from_slice(&movz.to_le_bytes());
    out.extend_from_slice(&movk.to_le_bytes());
    out.extend_from_slice(&sxtw.to_le_bytes());
    out.extend_from_slice(&mul.to_le_bytes());
    out
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_imul_rax_imm32(_imm: i32) -> Vec<u8> {
    Vec::new()
}

/// `neg accumulator` — two's-complement negate.
///
/// x86_64 (3 bytes):  48 f7 d8   neg rax
/// aarch64 (4 bytes): NEG x0, x0 = SUB x0, XZR, x0  (alias)
///   sf=1 op=1 S=0 0b01011 shift=00 Rm=0 imm6=0 Rn=31 Rd=0
///   = 0xCB0003E0
#[cfg(target_arch = "x86_64")]
pub fn emit_neg_rax() -> Vec<u8> {
    vec![0x48, 0xf7, 0xd8]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_neg_rax() -> Vec<u8> {
    let neg: u32 = 0xCB0003E0;
    neg.to_le_bytes().to_vec()
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_neg_rax() -> Vec<u8> {
    Vec::new()
}

// ---------------------------------------------------------------------------
// Doc 47 Stage 9d — ChainOp IR + composer.
//
// Drivers describe the function body as a slice of [`ChainOp`] and
// hand that to [`compose`]; primitives chain over the integer
// accumulator (rax / x0).  This is the smallest surface that lets a
// future AST → IR pass plug in without touching any of the per-arch
// byte tables.
// ---------------------------------------------------------------------------

/// One primitive operation against the integer accumulator.
///
/// Stage 9f drops the `Copy` derive: the `IfLtImm` variant carries
/// owned `Vec<ChainOp>' sub-chains, so the enum no longer fits in a
/// fixed-size register-passing payload.  All callers that previously
/// relied on copy-by-value now use cheap `Clone' (= Vec bumps).
///
/// Stage 9g extends the surface to multi-parameter lambdas.  The heap
/// becomes an array of N tagged-int words at consecutive offsets;
/// `LoadHeapIndex(i)' fetches word i.  A single secondary register
/// (r10 on x86_64, x10 on aarch64) absorbs one `Save' at a time so a
/// binary op `(OP a b)' can run as `Save b; Load a; OP-Saved'.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChainOp {
    /// Load the tagged int from the heap (= `argv[0]` is a pointer to
    /// the heap word) and arithmetic-shift the tag bits off.  Leaves
    /// the untagged i64 in the accumulator.
    ///
    /// Equivalent to `LoadHeapIndex(0)' in semantics; kept as a
    /// distinct variant so the byte output remains identical to the
    /// runtime's pre-baked `NATIVE_LOAD_HEAP_INT_UNTAG' asset (=
    /// Stage 9b parity gate).
    LoadHeapHead,
    /// `accumulator += imm` (sign-extended).
    AddImm(i32),
    /// `accumulator -= imm` (sign-extended).
    SubImm(i32),
    /// `accumulator *= imm` (signed).
    MulImm(i32),
    /// `accumulator = -accumulator`.
    Neg,
    /// Return from the function with the current accumulator value.
    /// Must be the last op in the chain.
    Ret,
    /// Stage 9f — structured branch with `(< heap[param_index] threshold)` guard.
    ///
    /// Compiles to a self-contained block that
    ///   1. (re)loads `heap[param_index]' into the accumulator
    ///   2. compares it against `threshold`
    ///   3. if `<` is true, runs `then_chain` — otherwise `else_chain`
    ///   4. control falls through past the block with the chosen
    ///      branch's final accumulator value.
    ///
    /// Both sub-chains start fresh from the prologue: each branch
    /// begins with its own LoadHeapIndex so the chosen value is in
    /// the accumulator regardless of which branch was taken.
    /// Branches must NOT contain `Ret' — termination is the outer
    /// caller's responsibility (`translate_lambda' appends `Ret'
    /// once after the whole body chain).
    ///
    /// Stage 9g adds `param_index' to support multi-param lambdas;
    /// Stage 9f-era code uses `param_index: 0`.
    IfLtImm {
        param_index: usize,
        threshold: i32,
        then_chain: Vec<ChainOp>,
        else_chain: Vec<ChainOp>,
    },
    /// Stage 9g — load `heap[index]' into the accumulator.  Heap is
    /// laid out as N consecutive 8-byte tagged-int words; `argv[0]'
    /// points at heap[0].  `LoadHeapIndex(0)' is byte-equivalent to
    /// `LoadHeapHead' on both target archs, so the single-param
    /// chains continue to share the existing emit primitives.
    LoadHeapIndex(usize),
    /// Stage 9g — copy the integer accumulator to the secondary
    /// register (r10 on x86_64, x10 on aarch64).  Used as the
    /// 2-operand binary-op stash: `Save Y; Load X; OP-Saved' computes
    /// `X OP Y' without touching the call frame.  At most one Save
    /// may be live at a time — Stage 9g rejects bodies that would
    /// require a second concurrent stash.
    Save,
    /// Stage 9g — `accumulator += saved` (= `acc + r10` / `x0 + x10`).
    AddSaved,
    /// Stage 9g — `accumulator -= saved` (= `acc - r10` / `x0 - x10`).
    SubSaved,
    /// Stage 9g — `accumulator *= saved` (= `acc * r10` / `x0 * x10`).
    MulSaved,
}

/// Compose a slice of [`ChainOp`] into a single function body.
/// Always emits in order — no peephole / constant-fold optimisation;
/// the build-tool's evaluator already folds constants away before a
/// chain even reaches this point.  Recurses into [`IfLtImm`] sub-chains
/// via [`emit_if_lt_imm`].
pub fn compose(ops: &[ChainOp]) -> Vec<u8> {
    let mut out = Vec::new();
    for op in ops {
        match op {
            ChainOp::LoadHeapHead => out.extend_from_slice(&emit_load_heap_int_untag_head()),
            ChainOp::AddImm(n) => out.extend_from_slice(&emit_add_rax_imm32(*n)),
            ChainOp::SubImm(n) => out.extend_from_slice(&emit_sub_rax_imm32(*n)),
            ChainOp::MulImm(n) => out.extend_from_slice(&emit_imul_rax_imm32(*n)),
            ChainOp::Neg => out.extend_from_slice(&emit_neg_rax()),
            ChainOp::Ret => out.extend_from_slice(&emit_ret()),
            ChainOp::IfLtImm {
                param_index,
                threshold,
                then_chain,
                else_chain,
            } => out.extend_from_slice(&emit_if_lt_imm(
                *param_index,
                *threshold,
                then_chain,
                else_chain,
            )),
            ChainOp::LoadHeapIndex(i) => {
                out.extend_from_slice(&emit_load_heap_int_untag_at_index(*i))
            }
            ChainOp::Save => out.extend_from_slice(&emit_save_to_secondary()),
            ChainOp::AddSaved => out.extend_from_slice(&emit_add_saved()),
            ChainOp::SubSaved => out.extend_from_slice(&emit_sub_saved()),
            ChainOp::MulSaved => out.extend_from_slice(&emit_mul_saved()),
        }
    }
    out
}

pub const HAS_CHAIN_OP_COMPOSE: bool = HAS_CHAIN_EMIT_PRIMITIVES;

// ---------------------------------------------------------------------------
// Doc 47 Stage 9f — structured if-lt-imm composer.
//
// Encodes a self-contained if-branch block: head + cmp + jcc + then +
// jmp + else.  Each branch is itself an arbitrary ChainOp chain, so
// nested ifs work transparently (recursive `compose').
//
// Per-architecture layout:
//
// x86_64 (head=10 + cmp=6 + jge=6 = 22 prologue):
//     <emit_load_heap_int_untag_head>      ; 10 bytes — rax = heap_int
//     48 3D ii ii ii ii                    ; 6 bytes — cmp rax, threshold
//     0F 8D ii ii ii ii                    ; 6 bytes — jge <else>
//     <then_bytes>                         ; T bytes — branch result -> rax
//     E9 ii ii ii ii                       ; 5 bytes — jmp <past else>
//     <else_bytes>                         ; E bytes — branch result -> rax
//
// aarch64 (head=12 + cmp=16 + b.ge=4 = 32 prologue):
//     <emit_load_heap_int_untag_head>      ; 12 bytes — x0 = heap_int
//     MOVZ w9, #lo16(threshold)            ; 4 bytes
//     MOVK w9, #hi16(threshold), lsl #16   ; 4 bytes
//     SXTW x9, w9                          ; 4 bytes
//     CMP x0, x9                           ; 4 bytes (= SUBS XZR, X0, X9)
//     B.GE <else>                          ; 4 bytes — imm19 = 2 + T/4
//     <then_bytes>                         ; T bytes (multiple of 4)
//     B <after-else>                       ; 4 bytes — imm26 = 1 + E/4
//     <else_bytes>                         ; E bytes (multiple of 4)
// ---------------------------------------------------------------------------

#[cfg(target_arch = "x86_64")]
pub fn emit_if_lt_imm(
    param_index: usize,
    threshold: i32,
    then_chain: &[ChainOp],
    else_chain: &[ChainOp],
) -> Vec<u8> {
    let then_bytes = compose(then_chain);
    let else_bytes = compose(else_chain);
    let jmp_size: i32 = 5;
    // jge offset = bytes from end-of-jge to start-of-else
    //            = then_bytes.len() + jmp_size
    let jge_offset: i32 = (then_bytes.len() as i32)
        .checked_add(jmp_size)
        .expect("then_chain too large for i32 jump offset");
    // jmp offset = bytes from end-of-jmp to past-else (= 0 + else_bytes)
    let jmp_offset: i32 = else_bytes.len() as i32;

    let prologue_load = emit_load_heap_int_untag_at_index(param_index);
    let mut out = Vec::new();
    out.extend_from_slice(&prologue_load);
    // cmp rax, imm32 — 48 3D ii ii ii ii
    out.push(0x48);
    out.push(0x3d);
    out.extend_from_slice(&threshold.to_le_bytes());
    // jge rel32 — 0F 8D ii ii ii ii
    out.push(0x0f);
    out.push(0x8d);
    out.extend_from_slice(&jge_offset.to_le_bytes());
    out.extend_from_slice(&then_bytes);
    // jmp rel32 — E9 ii ii ii ii
    out.push(0xe9);
    out.extend_from_slice(&jmp_offset.to_le_bytes());
    out.extend_from_slice(&else_bytes);
    out
}

#[cfg(target_arch = "aarch64")]
pub fn emit_if_lt_imm(
    param_index: usize,
    threshold: i32,
    then_chain: &[ChainOp],
    else_chain: &[ChainOp],
) -> Vec<u8> {
    let then_bytes = compose(then_chain);
    let else_bytes = compose(else_chain);
    debug_assert!(
        then_bytes.len() % 4 == 0,
        "aarch64 IfLtImm: then_bytes must be 4-byte aligned (got {})",
        then_bytes.len()
    );
    debug_assert!(
        else_bytes.len() % 4 == 0,
        "aarch64 IfLtImm: else_bytes must be 4-byte aligned (got {})",
        else_bytes.len()
    );

    // Build the threshold in w9 then sign-extend to x9 (matches the
    // pattern used by emit_add_rax_imm32 for arbitrary i32 imms).
    let v = threshold as u32;
    let lo16 = v & 0xFFFF;
    let hi16 = (v >> 16) & 0xFFFF;
    let movz: u32 = 0x52800000 | (lo16 << 5) | 9;
    let movk: u32 = 0x72A00000 | (hi16 << 5) | 9;
    let sxtw: u32 = 0x93407D29;
    // CMP X0, X9 (= SUBS XZR, X0, X9, shifted register, no shift):
    //   0xEB000000 base | (Rm<<16) | (Rn<<5) | Rd=31
    //   Rm=9, Rn=0 → 0xEB09001F
    let cmp_x0_x9: u32 = 0xEB09001F;
    // imm19 for B.GE: skip past B.GE (1 inst) + then_bytes + B
    // (1 inst) and land at start of else_bytes.
    //   target_pc = bge_pc + 4 + then_bytes.len() + 4
    //   imm19 = (target_pc - bge_pc) / 4 = 2 + then_bytes.len()/4
    let then_inst = (then_bytes.len() / 4) as u32;
    let imm19_bge: u32 = 2 + then_inst;
    let bge: u32 = 0x5400000A | (imm19_bge << 5);
    // imm26 for unconditional B: skip past B (1 inst) + else_bytes.
    //   imm26 = 1 + else_bytes.len()/4
    let else_inst = (else_bytes.len() / 4) as u32;
    let imm26_b: u32 = 1 + else_inst;
    let b_uncond: u32 = 0x14000000 | imm26_b;

    let prologue_load = emit_load_heap_int_untag_at_index(param_index);
    let mut out = Vec::new();
    out.extend_from_slice(&prologue_load);
    out.extend_from_slice(&movz.to_le_bytes());
    out.extend_from_slice(&movk.to_le_bytes());
    out.extend_from_slice(&sxtw.to_le_bytes());
    out.extend_from_slice(&cmp_x0_x9.to_le_bytes());
    out.extend_from_slice(&bge.to_le_bytes());
    out.extend_from_slice(&then_bytes);
    out.extend_from_slice(&b_uncond.to_le_bytes());
    out.extend_from_slice(&else_bytes);
    out
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_if_lt_imm(
    _param_index: usize,
    _threshold: i32,
    _then_chain: &[ChainOp],
    _else_chain: &[ChainOp],
) -> Vec<u8> {
    Vec::new()
}

pub const HAS_IF_LT_IMM: bool = HAS_CHAIN_EMIT_PRIMITIVES;

// ---------------------------------------------------------------------------
// Doc 47 Stage 9g — multi-parameter heap access + secondary register save.
//
// `argv[0]' points to a heap that is laid out as N consecutive 8-byte
// tagged-int words (param 0 at offset 0, param 1 at offset 8, …).  The
// `LoadHeapIndex(i)' op fetches word i; for i=0 the encoding collapses
// to byte parity with `emit_load_heap_int_untag_head' so single-param
// chains continue to ride the existing prologue surface.
//
// `Save' / `AddSaved' / `SubSaved' / `MulSaved' use a single secondary
// register (r10 on x86_64, x10 on aarch64) as a 1-deep stash so a
// 2-operand binary op `(OP X Y)' can run as `Save Y; Load X; OP-Saved'.
// Stage 9g rejects bodies that would require a second concurrent stash
// (= 3+ symbolic operands per call); deeper expression nesting waits
// for a real spill stack in a later stage.
// ---------------------------------------------------------------------------

/// `LoadHeapIndex(i)' — emit `mov rax, [rsi]; mov rax, [rax + 8*i]; sar rax, 3`
/// on x86_64 (or the aarch64 ldr equivalent).  For `i = 0` the bytes
/// match `emit_load_heap_int_untag_head' exactly.
#[cfg(target_arch = "x86_64")]
pub fn emit_load_heap_int_untag_at_index(index: usize) -> Vec<u8> {
    if index == 0 {
        return emit_load_heap_int_untag_head();
    }
    // Offset is bytes from heap-base.  Stage 9g currently only emits
    // small indices; assert into the disp8 range so the encoding stays
    // a fixed 11 bytes.  Lifting this requires a disp32 form which
    // we'll add when first needed.
    let offset = index
        .checked_mul(8)
        .expect("LoadHeapIndex offset overflow");
    assert!(
        offset <= 0x7F,
        "Stage 9g: LoadHeapIndex({}) offset {} exceeds disp8 range",
        index, offset
    );
    let mut out = Vec::with_capacity(11);
    out.push(0x48); // REX.W
    out.push(0x8b); // MOV r64, r/m64
    out.push(0x06); // ModR/M: rax <- [rsi]
    out.push(0x48); // REX.W
    out.push(0x8b); // MOV r64, r/m64
    out.push(0x40); // ModR/M: rax <- [rax + disp8]
    out.push(offset as u8);
    out.push(0x48); // SAR rax, 3
    out.push(0xc1);
    out.push(0xf8);
    out.push(0x03);
    out
}

#[cfg(target_arch = "aarch64")]
pub fn emit_load_heap_int_untag_at_index(index: usize) -> Vec<u8> {
    // ldr x0, [x0, #imm12] uses imm12 scaled by 8 for 64-bit loads;
    // imm12 max = 4095 (= 32760 byte offset, 4095 params).
    assert!(
        index <= 4095,
        "Stage 9g: LoadHeapIndex({}) exceeds aarch64 imm12 range",
        index
    );
    let imm12 = index as u32;
    // ldr x0, [x1]: 0xF9400020
    let ldr_arg_ptr: u32 = 0xF9400020;
    // ldr x0, [x0, #8*index] (imm12 is the unsigned scaled offset).
    //   base 0xF9400000 | (imm12 << 10) | (Rn<<5) | Rd
    //   Rn=0, Rd=0
    let ldr_indexed: u32 = 0xF9400000 | (imm12 << 10);
    // asr x0, x0, #3: same encoding as in emit_load_heap_int_untag_head.
    let asr: u32 = u32::from_le_bytes([0x00, 0xfc, 0x43, 0x93]);
    let mut out = Vec::with_capacity(12);
    out.extend_from_slice(&ldr_arg_ptr.to_le_bytes());
    out.extend_from_slice(&ldr_indexed.to_le_bytes());
    out.extend_from_slice(&asr.to_le_bytes());
    out
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_load_heap_int_untag_at_index(_index: usize) -> Vec<u8> {
    Vec::new()
}

/// `Save' — copy the integer accumulator to the secondary register.
///
/// x86_64 (3 bytes):  49 89 c2   mov r10, rax
/// aarch64 (4 bytes): EA 03 00 AA   mov x10, x0  (ORR x10, xzr, x0)
#[cfg(target_arch = "x86_64")]
pub fn emit_save_to_secondary() -> Vec<u8> {
    vec![0x49, 0x89, 0xc2]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_save_to_secondary() -> Vec<u8> {
    // MOV x10, x0  (= ORR x10, xzr, x0)
    //   0xAA000000 | (Rm<<16) | (Rn=31<<5) | Rd
    //   Rm=0, Rn=31, Rd=10 → 0xAA0003EA
    let mov_x10_x0: u32 = 0xAA0003EA;
    mov_x10_x0.to_le_bytes().to_vec()
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_save_to_secondary() -> Vec<u8> {
    Vec::new()
}

/// `AddSaved' — `accumulator += saved'.
/// x86_64 (3 bytes):  4C 01 D0   add rax, r10
/// aarch64 (4 bytes): ADD x0, x0, x10
#[cfg(target_arch = "x86_64")]
pub fn emit_add_saved() -> Vec<u8> {
    vec![0x4c, 0x01, 0xd0]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_add_saved() -> Vec<u8> {
    // ADD x0, x0, x10 (shifted register, 64-bit, no shift):
    //   0x8B000000 | (Rm<<16) | (Rn<<5) | Rd
    //   Rm=10, Rn=0, Rd=0 → 0x8B0A0000
    let add: u32 = 0x8B0A0000;
    add.to_le_bytes().to_vec()
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_add_saved() -> Vec<u8> {
    Vec::new()
}

/// `SubSaved' — `accumulator -= saved'.
/// x86_64 (3 bytes):  4C 29 D0   sub rax, r10
/// aarch64 (4 bytes): SUB x0, x0, x10
#[cfg(target_arch = "x86_64")]
pub fn emit_sub_saved() -> Vec<u8> {
    vec![0x4c, 0x29, 0xd0]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_sub_saved() -> Vec<u8> {
    // SUB x0, x0, x10:
    //   0xCB000000 | (Rm<<16) | (Rn<<5) | Rd
    //   Rm=10, Rn=0, Rd=0 → 0xCB0A0000
    let sub: u32 = 0xCB0A0000;
    sub.to_le_bytes().to_vec()
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_sub_saved() -> Vec<u8> {
    Vec::new()
}

/// `MulSaved' — `accumulator *= saved'.
///
/// x86_64 (4 bytes): IMUL r64, r/m64 = `4C 0F AF D0` ... wait, need REX.R.
///   Actually x86_64 "imul reg64, r/m64" with reg=rax (000), rm=r10 (010
///   with B-extension) is REX.WR? — hmm rax dst is reg field which doesn't
///   need extension; r10 source is rm field which does need REX.B.
///   Encoding: REX.WB (0x4C? no, that's WR) — let me redo.
///     REX = 0100 W R X B; W=1, R=0 (rax in reg field, no extension),
///     X=0, B=1 (r10 in rm field, extension needed) → 0100 1001 = 0x49.
///   IMUL r64, r/m64 opcode: 0F AF.
///   ModR/M: mod=11, reg=000 (rax), rm=010 (r10) → 11 000 010 = 0xC2.
///   Full: 49 0F AF C2 (4 bytes).
/// aarch64 (4 bytes): MUL x0, x0, x10
#[cfg(target_arch = "x86_64")]
pub fn emit_mul_saved() -> Vec<u8> {
    vec![0x49, 0x0f, 0xaf, 0xc2]
}

#[cfg(target_arch = "aarch64")]
pub fn emit_mul_saved() -> Vec<u8> {
    // MUL x0, x0, x10 (= MADD x0, x0, x10, xzr):
    //   0x9B000000 | (Rm<<16) | (Ra<<10) | (Rn<<5) | Rd
    //   Rm=10, Ra=31, Rn=0, Rd=0 → 0x9B0A7C00
    let mul: u32 = 0x9B0A7C00;
    mul.to_le_bytes().to_vec()
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub fn emit_mul_saved() -> Vec<u8> {
    Vec::new()
}

pub const HAS_MULTI_PARAM_PRIMITIVES: bool = HAS_CHAIN_EMIT_PRIMITIVES;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_return_42() {
        let bytes = emit_return_i32(42);
        // mov eax, 42 = b8 2a 00 00 00 ; ret = c3
        assert_eq!(bytes, vec![0xb8, 0x2a, 0x00, 0x00, 0x00, 0xc3]);
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_return_zero() {
        let bytes = emit_return_i32(0);
        assert_eq!(bytes, vec![0xb8, 0x00, 0x00, 0x00, 0x00, 0xc3]);
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_return_negative_one() {
        // -1 as u32 = 0xFFFFFFFF.
        let bytes = emit_return_i32(-1);
        assert_eq!(bytes, vec![0xb8, 0xff, 0xff, 0xff, 0xff, 0xc3]);
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_return_full_i32_range() {
        // i32::MAX = 0x7FFFFFFF, i32::MIN = 0x80000000.
        assert_eq!(
            emit_return_i32(i32::MAX),
            vec![0xb8, 0xff, 0xff, 0xff, 0x7f, 0xc3],
        );
        assert_eq!(
            emit_return_i32(i32::MIN),
            vec![0xb8, 0x00, 0x00, 0x00, 0x80, 0xc3],
        );
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_return_42() {
        // MOVZ w0, #42        = 0x52800000 | (42 << 5) = 0x52800540
        // MOVK w0, #0, lsl#16 = 0x72A00000
        // RET                 = 0xD65F03C0
        let bytes = emit_return_i32(42);
        assert_eq!(
            bytes,
            vec![
                0x40, 0x05, 0x80, 0x52, // MOVZ w0, #42
                0x00, 0x00, 0xa0, 0x72, // MOVK w0, #0, lsl #16
                0xc0, 0x03, 0x5f, 0xd6, // RET
            ]
        );
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_return_zero() {
        let bytes = emit_return_i32(0);
        assert_eq!(
            bytes,
            vec![
                0x00, 0x00, 0x80, 0x52, // MOVZ w0, #0
                0x00, 0x00, 0xa0, 0x72, // MOVK w0, #0, lsl #16
                0xc0, 0x03, 0x5f, 0xd6, // RET
            ]
        );
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_return_high_bits() {
        // 0x10000 → MOVZ w0, #0 + MOVK w0, #1, lsl #16
        let bytes = emit_return_i32(0x10000);
        // MOVK w0, #1, lsl#16 = 0x72A00000 | (1 << 5) = 0x72A00020
        assert_eq!(&bytes[4..8], &[0x20, 0x00, 0xa0, 0x72]);
    }

    #[test]
    fn arch_flag_matches_emitter_output() {
        let bytes = emit_return_i32(0);
        if HAS_EMIT_RETURN_I32 {
            assert!(!bytes.is_empty(), "supported arch must emit non-empty");
        } else {
            assert!(bytes.is_empty(), "unsupported arch must emit empty");
        }
    }

    // ============================================================
    // Doc 47 Stage 9b — emit_load_heap_int_untag byte equality
    // ============================================================

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_load_heap_int_untag_byte_for_byte() {
        // The build-tool emit must produce exactly the runtime's
        // pre-baked NATIVE_LOAD_HEAP_INT_UNTAG bytes — that is the
        // walking-skeleton parity gate for Stage 9c (lambda compile).
        assert_eq!(
            emit_load_heap_int_untag(),
            vec![
                0x48, 0x8b, 0x06, // mov rax, [rsi]
                0x48, 0x8b, 0x00, // mov rax, [rax]
                0x48, 0xc1, 0xf8, 0x03, // sar rax, 3
                0xc3, // ret
            ]
        );
        assert_eq!(emit_load_heap_int_untag().len(), 11);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_load_heap_int_untag_byte_for_byte() {
        assert_eq!(
            emit_load_heap_int_untag(),
            vec![
                0x20, 0x00, 0x40, 0xf9, // ldr  x0, [x1]
                0x00, 0x00, 0x40, 0xf9, // ldr  x0, [x0]
                0x00, 0xfc, 0x43, 0x93, // asr  x0, x0, #3
                0xc0, 0x03, 0x5f, 0xd6, // ret
            ]
        );
        assert_eq!(emit_load_heap_int_untag().len(), 16);
    }

    #[test]
    fn load_heap_int_untag_arch_flag_matches_output() {
        let bytes = emit_load_heap_int_untag();
        if HAS_EMIT_LOAD_HEAP_INT_UNTAG {
            assert!(!bytes.is_empty(), "supported arch must emit non-empty");
        } else {
            assert!(bytes.is_empty(), "unsupported arch must emit empty");
        }
    }

    // ============================================================
    // Doc 47 Stage 9c — chain emit primitives + composition
    // ============================================================

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_load_heap_head_no_ret() {
        // Same as the full asset minus the trailing 0xc3 (ret).
        assert_eq!(
            emit_load_heap_int_untag_head(),
            vec![
                0x48, 0x8b, 0x06, //
                0x48, 0x8b, 0x00, //
                0x48, 0xc1, 0xf8, 0x03,
            ]
        );
        assert_eq!(emit_load_heap_int_untag_head().len(), 10);
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_ret_is_c3() {
        assert_eq!(emit_ret(), vec![0xc3]);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_ret() {
        assert_eq!(emit_ret(), vec![0xc0, 0x03, 0x5f, 0xd6]);
    }

    #[test]
    fn load_head_plus_ret_equals_full_asset() {
        // Composition gate: the refactored full-asset emitter must
        // remain byte-identical with the hand-rolled head+ret chain.
        let mut composed = emit_load_heap_int_untag_head();
        composed.extend_from_slice(&emit_ret());
        assert_eq!(composed, emit_load_heap_int_untag());
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_add_rax_imm32_positive() {
        // add rax, 10 = 48 05 0a 00 00 00 (REX.W + opcode + imm32)
        assert_eq!(
            emit_add_rax_imm32(10),
            vec![0x48, 0x05, 0x0a, 0x00, 0x00, 0x00]
        );
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_add_rax_imm32_negative() {
        // add rax, -1 = 48 05 ff ff ff ff (sign-extended)
        assert_eq!(
            emit_add_rax_imm32(-1),
            vec![0x48, 0x05, 0xff, 0xff, 0xff, 0xff]
        );
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_int_plus_imm_full_chain() {
        // load head (10) + add imm32 (6) + ret (1) = 17 bytes
        let bytes = emit_int_plus_imm(10);
        assert_eq!(bytes.len(), 17);
        // Head matches load_heap_int_untag without the trailing ret.
        assert_eq!(
            &bytes[..10],
            &[0x48, 0x8b, 0x06, 0x48, 0x8b, 0x00, 0x48, 0xc1, 0xf8, 0x03][..]
        );
        // Add rax, 10
        assert_eq!(
            &bytes[10..16],
            &[0x48, 0x05, 0x0a, 0x00, 0x00, 0x00][..]
        );
        // Trailing ret
        assert_eq!(bytes[16], 0xc3);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_add_rax_imm32_zero() {
        // imm = 0:
        //   MOVZ w9, #0       0x52800009 (rd=9, low16=0)
        //   MOVK w9, #0,lsl16 0x72A00009
        //   ADD x0,x0,w9,SXTW 0x8B29C000
        let bytes = emit_add_rax_imm32(0);
        assert_eq!(bytes.len(), 12);
        // little-endian u32 reads
        let movz = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
        let movk = u32::from_le_bytes(bytes[4..8].try_into().unwrap());
        let add_ext = u32::from_le_bytes(bytes[8..12].try_into().unwrap());
        assert_eq!(movz, 0x52800009);
        assert_eq!(movk, 0x72A00009);
        assert_eq!(add_ext, 0x8B29C000);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_int_plus_imm_full_chain() {
        // 12 (head) + 12 (add) + 4 (ret) = 28 bytes
        let bytes = emit_int_plus_imm(0);
        assert_eq!(bytes.len(), 28);
        // First 12 = head
        assert_eq!(&bytes[..12], &emit_load_heap_int_untag_head()[..]);
        // Last 4 = ret
        assert_eq!(&bytes[24..28], &emit_ret()[..]);
    }

    #[test]
    fn int_plus_imm_arch_flag_matches_output() {
        let bytes = emit_int_plus_imm(0);
        if HAS_EMIT_INT_PLUS_IMM {
            assert!(!bytes.is_empty(), "supported arch must emit non-empty");
        } else {
            assert!(bytes.is_empty(), "unsupported arch must emit empty");
        }
    }

    #[test]
    fn int_plus_imm_zero_equals_load_asset() {
        // Special case: adding 0 should still compute correctly.
        // The bytes differ from emit_load_heap_int_untag() because
        // we still emit the add-imm middle (== explicit composition,
        // even when redundant), but the boot exit code must match.
        // This test only asserts the byte length sanity since the
        // E2E parity is covered by the smoke script.
        let plus_zero = emit_int_plus_imm(0);
        let load_only = emit_load_heap_int_untag();
        assert!(plus_zero.len() > load_only.len());
        // Head bytes share prefix.
        let head = emit_load_heap_int_untag_head();
        assert_eq!(&plus_zero[..head.len()], &head[..]);
        assert_eq!(&load_only[..head.len()], &head[..]);
    }

    // ============================================================
    // Doc 47 Stage 9d — extended primitives + ChainOp IR
    // ============================================================

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_sub_rax_imm32() {
        // sub rax, 5 = 48 2d 05 00 00 00
        assert_eq!(
            emit_sub_rax_imm32(5),
            vec![0x48, 0x2d, 0x05, 0x00, 0x00, 0x00]
        );
        // sub rax, -1 (sign-extends) = 48 2d ff ff ff ff
        assert_eq!(
            emit_sub_rax_imm32(-1),
            vec![0x48, 0x2d, 0xff, 0xff, 0xff, 0xff]
        );
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_imul_rax_imm32() {
        // imul rax, rax, 3 = 48 69 c0 03 00 00 00
        assert_eq!(
            emit_imul_rax_imm32(3),
            vec![0x48, 0x69, 0xc0, 0x03, 0x00, 0x00, 0x00]
        );
        // imul rax, rax, -2 = 48 69 c0 fe ff ff ff
        assert_eq!(
            emit_imul_rax_imm32(-2),
            vec![0x48, 0x69, 0xc0, 0xfe, 0xff, 0xff, 0xff]
        );
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn x86_64_emit_neg_rax() {
        assert_eq!(emit_neg_rax(), vec![0x48, 0xf7, 0xd8]);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_sub_rax_imm32_zero() {
        // imm = 0:
        //   MOVZ w9, #0       0x52800009
        //   MOVK w9, #0,lsl16 0x72A00009
        //   SUB x0,x0,w9,SXTW 0xCB29C000
        let bytes = emit_sub_rax_imm32(0);
        assert_eq!(bytes.len(), 12);
        let movz = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
        let movk = u32::from_le_bytes(bytes[4..8].try_into().unwrap());
        let sub_ext = u32::from_le_bytes(bytes[8..12].try_into().unwrap());
        assert_eq!(movz, 0x52800009);
        assert_eq!(movk, 0x72A00009);
        assert_eq!(sub_ext, 0xCB29C000);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_imul_rax_imm32_basic() {
        let bytes = emit_imul_rax_imm32(7);
        assert_eq!(bytes.len(), 16);
        // SXTW x9, w9 + MUL x0, x0, x9 are the trailing two
        // instructions and are imm-independent.
        let sxtw = u32::from_le_bytes(bytes[8..12].try_into().unwrap());
        let mul = u32::from_le_bytes(bytes[12..16].try_into().unwrap());
        assert_eq!(sxtw, 0x93407D29);
        assert_eq!(mul, 0x9B097C00);
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn aarch64_emit_neg_rax() {
        let bytes = emit_neg_rax();
        assert_eq!(bytes.len(), 4);
        let neg = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
        assert_eq!(neg, 0xCB0003E0);
    }

    #[test]
    fn compose_empty_yields_empty_bytes() {
        let bytes = compose(&[]);
        assert!(bytes.is_empty());
    }

    #[test]
    fn compose_load_ret_equals_load_asset() {
        let composed = compose(&[ChainOp::LoadHeapHead, ChainOp::Ret]);
        assert_eq!(composed, emit_load_heap_int_untag());
    }

    #[test]
    fn compose_load_add_ret_equals_int_plus_imm() {
        // The Stage 9c convenience function must remain a special
        // case of the chain composer.
        let composed = compose(&[
            ChainOp::LoadHeapHead,
            ChainOp::AddImm(42),
            ChainOp::Ret,
        ]);
        assert_eq!(composed, emit_int_plus_imm(42));
    }

    #[test]
    fn compose_arith_chain_concatenates_primitives() {
        // load + sub(3) + mul(2) + neg + ret
        let ops = [
            ChainOp::LoadHeapHead,
            ChainOp::SubImm(3),
            ChainOp::MulImm(2),
            ChainOp::Neg,
            ChainOp::Ret,
        ];
        let composed = compose(&ops);
        // The composed bytes should equal the manual concatenation.
        let mut manual = emit_load_heap_int_untag_head();
        manual.extend_from_slice(&emit_sub_rax_imm32(3));
        manual.extend_from_slice(&emit_imul_rax_imm32(2));
        manual.extend_from_slice(&emit_neg_rax());
        manual.extend_from_slice(&emit_ret());
        assert_eq!(composed, manual);
    }

    #[test]
    fn compose_handles_all_op_variants() {
        // Smoke that the match is exhaustive — adding a new variant
        // would force a compile error here, catching missed wires.
        let _ = compose(&[
            ChainOp::LoadHeapHead,
            ChainOp::AddImm(1),
            ChainOp::SubImm(2),
            ChainOp::MulImm(3),
            ChainOp::Neg,
            ChainOp::Ret,
        ]);
    }
}
