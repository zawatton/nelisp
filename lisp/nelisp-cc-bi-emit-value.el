;;; nelisp-cc-bi-emit-value.el --- Wave A33.N emit-value leaf-arm Phase 47 kernels  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A33.N (Phase 47 self-application — emit-value hot-arm native,
;; 回避策 A / "emit-only").  Phase 47-compiles the two *leaf* hot arms
;; of `nelisp-phase47-compiler--emit-value' (= the integer-tag dispatch
;; introduced in A33.3 over the A33.4 flat key-value IR vectors) so the
;; standalone NeLisp self-host can emit the `imm' and `ref' (GP class)
;; machine-code arms without re-entering the elisp interpreter for the
;; per-node byte computation.
;;
;; Why only `imm' + `ref' (GP).  Among the imm/ref/arith/call hot arms,
;; only `imm' and the GP-class `ref' are *leaf* emitters — they read a
;; single integer field out of the IR node and emit a fixed-layout byte
;; sequence with no recursion into `--emit-value' and no Rust extern
;; helper:
;;
;;   imm  (tag 30): `mov rax, imm32'        = 0x48 0xC7 0xC0 + imm32-LE
;;                  (7 bytes; reads `:value' at the A33.4 fixed offset 2).
;;   ref  (tag 53, GP class): `mov rax, [rbp - 8*(slot+1)]'
;;                  = 0x48 0x8B 0x45 + disp8  (4 bytes; reads `:slot'
;;                  at the A33.4 fixed offset 6).
;;
;; `arith' and `call' both *recurse* into `--emit-value' on their
;; sub-nodes (`:a' / `:b' / args) and interleave push/pop + call
;; sequences, so a standalone kernel would have to re-implement the
;; entire recursive emitter (= out of scope for 回避策 A; they stay on
;; the elisp path).  The f64-class `ref' arm reads via MOVSD into xmm0
;; and is likewise left on the elisp path.
;;
;; A33.4 fixed offsets.  After A33.4, an IR node is the flat key-value
;; vector `[KIND K1 V1 K2 V2 ...]' with a per-kind-constant layout:
;;
;;   imm  = `[imm :value V]'                         → V at slot 2.
;;   ref  = `[ref :var SYM :reg R :slot S :class C]' → S at slot 6.
;;
;; The native kernels therefore read the integer payload at the fixed
;; vector offset (= no `:kind'-symbol scan, no `--ir-get' key match),
;; exactly the read shape A33.4's docstrings promised the A33.N path
;; would use (`vector-ref-ptr' + `sexp-int-unwrap').
;;
;; Bridge contract.  Both kernels follow the established
;; `nl-jit-call-out-2' shape (see jit.rs / the A26 / A30 parents):
;;
;;   fn nelisp_emit_value_imm(
;;       node:        *const Sexp,  // Sexp::Vec — the `imm' IR node
;;       out_vec:     *const Sexp,  // Sexp::Vec — preallocated ≥7 slots
;;       result:      *mut Sexp,    // Sexp::Int(byte-count) writeback
;;   ) -> i64                        // TRAMPOLINE_OK (= 0)
;;
;;   fn nelisp_emit_value_ref_gp(
;;       node:        *const Sexp,  // Sexp::Vec — the `ref' (GP) IR node
;;       out_vec:     *const Sexp,  // Sexp::Vec — preallocated ≥4 slots
;;       result:      *mut Sexp,    // Sexp::Int(byte-count) writeback
;;   ) -> i64                        // TRAMPOLINE_OK (= 0)
;;
;; OUT-VEC is preallocated by the elisp caller to ≥ the emit length and
;; each emitted byte is written as `Sexp::Int(byte)' into out_vec[i] via
;; `vector-slot-set'.  RESULT-SLOT receives `Sexp::Int(byte-count)' (7
;; for `imm', 4 for `ref'); the elisp caller reads it to know how many
;; out_vec slots to splice into the asm buffer, then appends those bytes
;; with the same `nelisp-asm-x86_64-emit-bytes' path the legacy arm uses
;; — so the *buffer* mutation (= the heap-allocating chunk-list append)
;; stays in elisp and the byte *computation* moves into the .o.  Net
;; result is byte-identical to the legacy arm by construction.
;;
;; Disp8 computation (`ref').  The legacy `--emit-ref-load' computes
;; `disp = (- (* 8 (1+ slot)))' then `disp8 = (logand disp #xFF)'.  The
;; kernel mirrors this exactly: `(logand (- 0 (* 8 (+ slot 1))) #xFF)'.
;; slot is in 0..13 (= Doc 97 arity + let-rt cap), so disp is in
;; [-112, -8] and the masked byte is the two's-complement disp8 the
;; legacy path emits.  The kernel does *not* range-check slot (= the
;; elisp caller already guards 0..13 before dispatching native; an
;; out-of-range slot never reaches the kernel).
;;
;; Imm32 LE bytes (`imm').  The legacy `nelisp-asm-x86_64--imm32-bytes'
;; emits `[v&0xFF, (v>>8)&0xFF, (v>>16)&0xFF, (v>>24)&0xFF]' (low byte
;; first) after masking `v` to 32 bits.  The kernel mirrors this with
;; `sar' (arithmetic right shift — sign bits land above bit 31 and are
;; masked off by the final `logand #xFF', matching the legacy `logand
;; u #xFF' on the pre-masked `u = v & 0xFFFFFFFF').  Range-checking
;; (`-2^31 ≤ v < 2^32') stays on the elisp caller side (= the legacy
;; arm's `mov-imm32' would signal there too); the kernel assumes a
;; pre-validated 32-bit-fitting value.
;;
;; Substrate composition (= existing Phase 47 grammar only, no new
;; opcode, no Rust extern added — net Rust LOC delta = 0):
;;
;;   §111.C  `vector-ref-ptr' / `vector-slot-set' — borrow the IR node's
;;            integer payload slot + refcount-safe write into out_vec.
;;   §100    `sexp-int-unwrap' / `sexp-int-make' — read the i64 field +
;;            materialise each `Sexp::Int(byte)' before vector-slot-set.
;;   §100.D  `logand' / `sar' / `+' / `-' / `*' — byte extraction
;;            (`logand #xFF'), imm32 LE shifts (`sar'), disp8 arithmetic.
;;   §125.A  `alloc-bytes' / `dealloc-bytes' — 32-byte scratch slot for
;;            `sexp-int-make' before each `vector-slot-set' (freed via
;;            the seq3 sequencer, same pattern as the A30 dispatch loop).
;;
;; Linux-x86_64 only — same arch gate as the A26 / A30 parents (= the
;; emitted byte sequences are x86_64-specific; the aarch64 `--emit-value'
;; path is a different instruction encoding and stays on the elisp arm).

;;; Code:

(defconst nelisp-cc-bi-emit-value--source
  '(seq
    ;; 3-arg side-effect sequencer — `(val _e1 _e2) -> val'.  Threads
    ;; the `dealloc-bytes' scratch cleanup behind the `vector-slot-set'
    ;; write so the byte-write helper returns the running offset while
    ;; the scratch slot has already been freed.  Odd arity (3) — Phase
    ;; 47 emits the rsp alignment pad.
    (defun nelisp_emit_value_seq3 (val _e1 _e2) val)

    ;; 4-arg single-byte writer — materialise `Sexp::Int(byte)' into a
    ;; fresh 32-byte scratch slot, `vector-slot-set' it into out[idx],
    ;; free the scratch, and return `idx + 1' (= the next write
    ;; position) so the imm/ref bodies thread the offset linearly.
    ;; Even arity (4) — register-only under SysV AMD64.
    (defun nelisp_emit_value_put_byte (out-vec idx byte next)
      (nelisp_emit_value_seq3
       (vector-slot-set out-vec idx (sexp-int-make (alloc-bytes 32 8) byte))
       0
       next))

    ;; 3-arg `imm' arm — emit `mov rax, imm32' (= 0x48 0xC7 0xC0 +
    ;; imm32-LE, 7 bytes).  Reads `:value' at the A33.4 fixed slot 2,
    ;; writes the 3 opcode bytes + 4 little-endian imm bytes into
    ;; out-vec, and writes `Sexp::Int(7)' into result-slot.  The
    ;; `put-byte' calls are nested so the byte writes happen left to
    ;; right (= ascending out-vec slots) before the `sexp-int-make'
    ;; finalises the count.  Odd arity (3) — rsp pad emitted.
    (defun nelisp_emit_value_imm (node out-vec result-slot)
      (nelisp_emit_value_seq3
       0
       (nelisp_emit_value_put_byte
        out-vec 6
        (logand (sar (sexp-int-unwrap (vector-ref-ptr node 2)) 24) 255)
        (nelisp_emit_value_put_byte
         out-vec 5
         (logand (sar (sexp-int-unwrap (vector-ref-ptr node 2)) 16) 255)
         (nelisp_emit_value_put_byte
          out-vec 4
          (logand (sar (sexp-int-unwrap (vector-ref-ptr node 2)) 8) 255)
          (nelisp_emit_value_put_byte
           out-vec 3
           (logand (sexp-int-unwrap (vector-ref-ptr node 2)) 255)
           (nelisp_emit_value_put_byte
            out-vec 2 192
            (nelisp_emit_value_put_byte
             out-vec 1 199
             (nelisp_emit_value_put_byte out-vec 0 72 0)))))))
       (sexp-int-make result-slot 7)))

    ;; 3-arg `ref' (GP class) arm — emit `mov rax, [rbp - 8*(slot+1)]'
    ;; (= 0x48 0x8B 0x45 + disp8, 4 bytes).  Reads `:slot' at the A33.4
    ;; fixed slot 6, computes disp8 = `(- 0 (* 8 (+ slot 1))) & 0xFF'
    ;; (= two's-complement signed byte the legacy `--emit-ref-load'
    ;; emits), writes the 3 opcode bytes + disp8 into out-vec, and
    ;; writes `Sexp::Int(4)' into result-slot.  Odd arity (3) — rsp pad.
    (defun nelisp_emit_value_ref_gp (node out-vec result-slot)
      (nelisp_emit_value_seq3
       0
       (nelisp_emit_value_put_byte
        out-vec 3
        (logand
         (- 0 (* 8 (+ (sexp-int-unwrap (vector-ref-ptr node 6)) 1)))
         255)
        (nelisp_emit_value_put_byte
         out-vec 2 69
         (nelisp_emit_value_put_byte
          out-vec 1 139
          (nelisp_emit_value_put_byte out-vec 0 72 0))))
       (sexp-int-make result-slot 4))))
  "Phase 47 source for the Wave A33.N emit-value leaf-arm kernels.

Four-entry `(seq DEFUN ...)' manifest:

- `nelisp_emit_value_seq3 (val _e1 _e2) -> val' — 3-arg side-effect
  sequencer; threads scratch `dealloc-bytes' cleanup behind the
  `vector-slot-set' write, and (at the public entries) returns
  literal TRAMPOLINE_OK (= 0) as VAL while evaluating the byte-write
  chain and the `sexp-int-make' count writeback as discarded side
  effects E1/E2 — so the `nl-jit-call-out-2' bridge surfaces the byte
  count via the caller-owned RESULT-SLOT.

- `nelisp_emit_value_put_byte (out-vec idx byte next) -> next' — 4-arg
  single-byte writer; materialises `Sexp::Int(byte)' into a scratch
  slot, `vector-slot-set's into out[idx], frees the scratch, returns
  NEXT (= the threaded running offset).

- `nelisp_emit_value_imm (node out-vec result-slot) -> 0' — 3-arg
  `imm' arm; reads `:value' at the A33.4 fixed offset 2, emits the
  7-byte `mov rax, imm32' (0x48 0xC7 0xC0 + imm32-LE) into out-vec,
  writes `Sexp::Int(7)' into result-slot.

- `nelisp_emit_value_ref_gp (node out-vec result-slot) -> 0' — 3-arg
  `ref' (GP class) arm; reads `:slot' at the A33.4 fixed offset 6,
  emits the 4-byte `mov rax, [rbp - 8*(slot+1)]' (0x48 0x8B 0x45 +
  disp8) into out-vec, writes `Sexp::Int(4)' into result-slot.

Bridge contract (= `nl-jit-call-out-2' shape, see jit.rs):

  fn nelisp_emit_value_imm(
      node:    *const Sexp,  // Sexp::Vec — the `imm' IR node
      out_vec: *const Sexp,  // Sexp::Vec — preallocated ≥7 slots
      result:  *mut Sexp,    // Sexp::Int(7) writeback
  ) -> i64                    // TRAMPOLINE_OK (= 0)

  fn nelisp_emit_value_ref_gp(
      node:    *const Sexp,  // Sexp::Vec — the `ref' (GP) IR node
      out_vec: *const Sexp,  // Sexp::Vec — preallocated ≥4 slots
      result:  *mut Sexp,    // Sexp::Int(4) writeback
  ) -> i64                    // TRAMPOLINE_OK (= 0)

Caller-side preparation (elisp wrapper in `--emit-value'):

  1. Build `out-vec = (make-vector 7 (sexp::int 0))' (≥ emit len).
  2. result = (sexp::nil) placeholder.
  3. (nl-jit-call-out-2 \"nelisp_emit_value_imm\" node out-vec)  (or
     \"nelisp_emit_value_ref_gp\").
  4. Read RESULT-SLOT = byte-count (7 or 4).
  5. Read out-vec[0..count) Sexp::Int payloads → unibyte-string →
     `nelisp-asm-x86_64-emit-bytes buf'.

  The buffer chunk-list append stays in elisp; only the per-node byte
  computation crosses the bridge.  Output bytes equal the legacy arm's
  bytes, so the produced `.o' objects are byte-identical.

Composes only existing Phase 47 grammar — no new opcode, no new Rust
extern:

- §111.C `vector-ref-ptr' / `vector-slot-set' — IR payload borrow +
  refcount-safe out-vec write.
- §100 `sexp-int-unwrap' / `sexp-int-make' — read i64 field + write
  each `Sexp::Int(byte)' and the final byte count.
- §100.D `logand' / `sar' / `+' / `-' / `*' — byte extraction, imm32
  LE shifts, disp8 arithmetic.
- §125.A `alloc-bytes' / `dealloc-bytes' — 32-byte scratch for
  `sexp-int-make' before each `vector-slot-set'.

Linux-x86_64 only — same arch gate as the A26 / A30 parents.")

(provide 'nelisp-cc-bi-emit-value)

;;; nelisp-cc-bi-emit-value.el ends here
