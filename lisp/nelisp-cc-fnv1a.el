;;; nelisp-cc-fnv1a.el --- Doc 115 §115.7 pure-elisp FNV-1a hash  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash, replacing the Rust
;; `mirror_fnv1a' free fn + `nl_mirror_fnv1a_sexp' extern wrapper
;; in `build-tool/src/eval/env_helpers.rs'.  This is the last
;; Rust intermediary in the env_mirror / env_lexframe Phase 47
;; dispatch chain; after §115.7 ships, every hash-related call
;; site (= the `mirror_lookup_entry' / `frame_bind' / `frame_stack_
;; find' helpers' inner `(logand H (- COUNT 1))' index mask) lands
;; directly in this elisp `.o' through `extern-call'.
;;
;; Algorithm (= literal transcription of the Rust body in
;; `env_helpers.rs::mirror_fnv1a'):
;;
;;   fn mirror_fnv1a(s: &str) -> u32 {
;;       let mut h: u32 = 0x811C9DC5;       // OFFSET_BASIS
;;       for b in s.bytes() {
;;           h ^= b as u32;
;;           h = h.wrapping_mul(0x01000193); // FNV_PRIME
;;       }
;;       h
;;   }
;;
;; The Rust impl iterates `s.chars()' (= Unicode codepoints).  This
;; elisp transcription iterates *bytes* via `str-byte-at' (= the only
;; primitive the Phase 47 grammar exposes for indexed string access).
;; For the nelisp symbol table the distinction is invisible because
;; every elisp identifier is ASCII (= a Rust `char' and a UTF-8 byte
;; coincide for codepoints below 0x80) and the env mirror only stores
;; ASCII symbol names.  The probe test asserts bit-equality with the
;; Rust loop on ASCII inputs; documented limit on non-ASCII inputs
;; (= codepoint-vs-byte divergence) is benign because the env mirror
;; never receives non-ASCII keys.
;;
;; ABI deps satisfied:
;;   §101.C  `str-len' + `str-byte-at' — byte iteration over the
;;                                        Sexp::Str / Sexp::Symbol
;;                                        payload (= raw UTF-8 bytes
;;                                        at offset 24-32 of the
;;                                        24-byte `String' header).
;;   §115.0  `logxor' / `logand'        — the 32-bit truncation +
;;                                        XOR step.
;;   §100.D  `*' (arith-mul)            — the wrapping multiply by
;;                                        FNV_PRIME (= 0x01000193).
;;
;; The recursive helper `nelisp_fnv1a_step' is tail-recursive; Phase
;; 47's emit treats `defun' bodies as ordinary recursion with the
;; argument-pinned hash state threading through the call chain in
;; rdi-shape registers.  Termination is `(< i n)' on the byte index.
;;
;; Signature (= matches the `nl_mirror_fnv1a_sexp' extern that the
;; existing `mirror_lookup_entry' / `frame_bind' / `frame_stack_find'
;; helpers call via `extern-call'):
;;
;;   nelisp_fnv1a(str_ptr: *const Sexp) -> i64
;;
;; Returns: i64 — the u32 hash zero-extended into the low 32 bits.
;; The high 32 bits are always 0 because the `(logand h
;; #xFFFFFFFF)' truncation runs after every multiply.

;;; Code:

(defconst nelisp-cc-fnv1a--source
  '(seq
    (defun nelisp_fnv1a_step (h str-ptr i n)
      ;; Tail-recursive inner loop.
      ;;
      ;;   h:       current 32-bit hash state (zero-extended to i64).
      ;;   str-ptr: *const Sexp pointing at Sexp::Str / Sexp::Symbol.
      ;;   i:       current byte index into the string payload.
      ;;   n:       byte count of the string (= `str-len' on str-ptr).
      ;;
      ;; Each step XORs the byte at position `i', multiplies by the
      ;; FNV_PRIME (0x01000193), masks back to 32 bits, then recurses
      ;; on (i+1).  Base case: `(< i n)' false → return the final hash.
      ;;
      ;; 32-bit truncation note: the obvious encoding
      ;; `(logand X 4294967295)' breaks because Phase 47's `MOV r/m64,
      ;; imm32' opcode sign-extends, turning `0xFFFFFFFF' into the
      ;; all-ones 64-bit `0xFFFFFFFF_FFFFFFFF' mask (= identity).
      ;; Instead we compose the mask register-only via two shifts plus
      ;; a subtract: `(- (shl 1 32) 1) = 0x00000000_FFFFFFFF', where
      ;; the `shl' uses the variable-shift path (rcx) so no sign
      ;; extension creeps in.  The intermediate `(shl 1 32)' value
      ;; `0x1_00000000' is the smallest 64-bit literal larger than
      ;; 32 bits; subtracting 1 yields exactly the unsigned 32-bit
      ;; mask we need.
      (if (< i n)
          (nelisp_fnv1a_step
           (logand (* (logxor h (str-byte-at str-ptr i)) 16777619)
                   (- (shl 1 32) 1))
           str-ptr (+ i 1) n)
        h))
    (defun nelisp_fnv1a (str-ptr)
      ;; str-ptr: *const Sexp pointing at Sexp::Str / Sexp::Symbol.
      ;; Returns: i64 — the 32-bit FNV-1a hash of the string's bytes,
      ;; matching the Rust `mirror_fnv1a' for ASCII inputs bit-for-bit.
      ;;
      ;; Empty string returns the FNV_OFFSET_BASIS (= 0x811C9DC5 =
      ;; 2166136261) per the algorithm's seed value.
      ;;
      ;; Seed materialisation: the literal 2166136261 (= 0x811C9DC5)
      ;; sits between 2^31 and 2^32, so the Phase 47 `MOV r/m64,
      ;; imm32' opcode would sign-extend its bit-31 to all high bits
      ;; (= `0xFFFFFFFF_811C9DC5'), polluting the output on the empty-
      ;; string path that never reaches the per-step `logand' mask.
      ;; We materialise the seed register-only via `(+ (shl 1 31)
      ;; 18652613)' — `(shl 1 31)' produces the clean 64-bit value
      ;; `0x80000000' (bit 31 set, all high bits 0) and adding the
      ;; positive low-31-bits remainder yields exactly 0x811C9DC5
      ;; in the low 32 bits, zeros in the high 32 bits.
      (nelisp_fnv1a_step (+ (shl 1 31) 18652613)
                         str-ptr 0 (str-len str-ptr))))
  "Phase 47 source for Doc 115 §115.7 `mirror_fnv1a' pure-elisp
replacement.

Implements the 32-bit FNV-1a hash via tail-recursive byte iteration
over a Sexp::Str / Sexp::Symbol payload.  Composes `str-len' /
`str-byte-at' (§101.C) + `logxor' / `logand' (§115.0) + `*' (§100.D)
through plain Phase 47 grammar — no `extern-call'.

Replaces the ~25 LOC Rust `mirror_fnv1a' free fn + ~8 LOC
`nl_mirror_fnv1a_sexp' extern wrapper in `env_helpers.rs'.
After §115.7 ships, the Rust hash impl is deleted outright and
every elisp-side `extern-call nl_mirror_fnv1a_sexp' is rewired
to `extern-call nelisp_fnv1a'.")

(provide 'nelisp-cc-fnv1a)

;;; nelisp-cc-fnv1a.el ends here
