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
}
