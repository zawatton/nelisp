;;; nelisp-asm-arm64.el --- AArch64 macro assembler (Doc 92 §92.b)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 92 §92.b — freestanding pure-elisp AArch64 (arm64) macro
;; assembler for the Phase 47 AOT compile chain, sibling of §92.a's
;; `lisp/nelisp-asm-x86_64.el'.
;;
;; This is intentionally separate from `src/nelisp-cc-arm64.el' (=
;; the Phase 7.x JIT backend with its SSA register allocator hooks +
;; cl-defstruct buffer).  The JIT side patches a single uint32 per
;; instruction into a vector held inside its codegen struct; the AOT
;; side builds a flat byte buffer that is later handed to Doc 91 ELF
;; writer / Doc 93 static linker.  The two use cases share no live
;; state — keeping them physically split avoids namespace pollution
;; and makes the §92.b spike self-contained, mirroring §92.a.
;;
;; AArch64 instruction encoding is fixed-width 4 bytes, little-endian
;; on Linux + macOS (= big-endian arm64 is OOS per Doc 92 §3.1).  The
;; word is computed by ORing a constant base with bit-field operand
;; shifts, then emitted as 4 LE bytes.
;;
;; Buffer abstraction (= mirrors §92.a):
;;
;;   (nelisp-asm-arm64-make-buffer)          ; -> opaque state
;;   (nelisp-asm-arm64-buffer-bytes BUF)     ; -> unibyte-string
;;   (nelisp-asm-arm64-buffer-pos   BUF)     ; -> integer
;;   (nelisp-asm-arm64-define-label BUF NM)
;;   (nelisp-asm-arm64-emit-fixup   BUF SLOT-OFFSET LABEL TYPE)
;;     ; TYPE = 'b26 (= B  imm26) / 'bl26 (= BL imm26)
;;   (nelisp-asm-arm64-resolve-fixups BUF)   ; -> patched unibyte-string
;;   (nelisp-asm-arm64-emit-reloc   BUF TYPE SYM &optional ADDEND)
;;     ; TYPE = 'b26-pc (= R_AARCH64_CALL26) / 'abs64 (= R_AARCH64_ABS64)
;;
;; State shape (§92.d-arm64 chunk-build, mirroring §92.d x86_64):
;;   `(:chunks (CHUNK_N ... CHUNK_2 CHUNK_1) :length N
;;     :bytes "" :pos 0
;;     :labels ((NAME . POS) ...)
;;     :fixups ((SLOT LABEL TYPE) ...) :relocs (RELOC ...))'
;; — a plist held in a single-element vector.  Each emitter
;; conses a fresh unibyte-string onto =:chunks= (= reverse order,
;; O(1) per write) and bumps =:length= (= O(1) read).  Finalize
;; via =(apply #'concat (nreverse :chunks))= once at
;; =buffer-bytes= call time — O(total-bytes) total.  Replaces
;; §92.b's =(concat old new)= accumulator that was O(N²) for
;; long buffers.  The =:bytes= / =:pos= keys are retained as
;; vestigial init values for API compat; only =:chunks= /
;; =:length= are touched on the hot path.  Public emitter
;; signatures are unchanged.
;;
;; Instruction emitters (= §92.b scope):
;;
;;   mov-imm-z (= MOVZ #imm16, LSL #0)
;;   mov-imm-k (= MOVK #imm16, LSL #(0|16|32|48))
;;   mov-imm64 (meta: chain of MOVZ + up to 3 MOVK for full 64-bit imm)
;;   mov-reg-reg (= ORR Xd, XZR, Xm)
;;   add-imm     sub-imm     cmp-imm     (12-bit unsigned)
;;   svc         brk         ret         nop
;;   bl          b           (with imm26 fixup against a label)
;;
;; Relocation marker API records `:b26-pc' / `:abs64' entries for
;; Doc 93 linker handoff — placeholder bytes (4 zero for instr,
;; 8 zero for abs64) are emitted at the recorded offset by the caller.
;;
;; Not wired into baker — freestanding spike per Doc 92 §0.2 + §8.1.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-asm-arm64-error
  "nelisp-asm-arm64 invariant violated")

;; ---- register table (= §92.b (2)) ----

(defconst nelisp-asm-arm64--reg
  '((x0 . 0)  (x1 . 1)  (x2 . 2)  (x3 . 3)
    (x4 . 4)  (x5 . 5)  (x6 . 6)  (x7 . 7)
    (x8 . 8)  (x9 . 9)  (x10 . 10) (x11 . 11)
    (x12 . 12) (x13 . 13) (x14 . 14) (x15 . 15)
    (x16 . 16) (x17 . 17) (x18 . 18) (x19 . 19)
    (x20 . 20) (x21 . 21) (x22 . 22) (x23 . 23)
    (x24 . 24) (x25 . 25) (x26 . 26) (x27 . 27)
    (x28 . 28) (x29 . 29) (x30 . 30)
    ;; Encoding 31 is shared by SP and the zero register XZR/WZR; the
    ;; instruction encoder discriminates by opcode (= MOVZ uses XZR
    ;; semantics, ADD/SUB to/from `sp' uses SP).  AArch64 keeps them
    ;; distinct at the symbolic level but identical at bit 4:0 = 11111.
    (sp . 31) (xzr . 31) (wzr . 31))
  "AArch64 64-bit GPR encoding map.
Each cell is `(NAME . N)' where N is the 5-bit register number.
Covers X0-X30 + SP / XZR / WZR aliases (= encoding 31).  SIMD vN /
half / single / double precision regs are OOS per Doc 92.")

(defun nelisp-asm-arm64--reg-num (reg)
  "Return the 5-bit register number for REG.
Signals `nelisp-asm-arm64-error' if REG is unknown."
  (let ((cell (assq reg nelisp-asm-arm64--reg)))
    (unless cell
      (signal 'nelisp-asm-arm64-error
              (list :unknown-register reg)))
    (cdr cell)))

;; ---- 32-bit word -> 4-byte LE encoder (= §3.1) ----

(defun nelisp-asm-arm64--word-bytes (word)
  "Encode WORD (= 32-bit unsigned) as 4 little-endian bytes (LSB first).
Returns a unibyte-string of length 4."
  (unless (integerp word)
    (signal 'nelisp-asm-arm64-error (list :word-not-integer word)))
  (let ((u (logand word #xFFFFFFFF)))
    (unibyte-string (logand u #xFF)
                    (logand (ash u  -8) #xFF)
                    (logand (ash u -16) #xFF)
                    (logand (ash u -24) #xFF))))

;; ---- buffer abstraction (= mirror §92.a) ----
;;
;; Same single-cell-vector-wrapping-plist shape as §92.a so that
;; downstream Doc 93 linker code can treat the two backends
;; identically.  The fixup record gains a TYPE field (= 'b26 / 'bl26)
;; because AArch64 fixups patch a 26-bit imm26 field, not a 32-bit
;; rel32 slot.

(defsubst nelisp-asm-arm64--unwrap (buf)
  "Return the plist held inside BUF (= buffer state vector)."
  (aref buf 0))

(defsubst nelisp-asm-arm64--rewrap (buf plist)
  "Replace BUF's backing plist with PLIST.  Mutates BUF in place."
  (aset buf 0 plist)
  buf)

(defun nelisp-asm-arm64-make-buffer ()
  "Return a fresh empty arm64 assembler buffer.
The buffer is opaque; use the accessors below to inspect or
extend it.  §92.d-arm64 chunk-build: =:chunks= holds the
reverse-order list of unibyte-string chunks pushed by per-
instruction emitters, =:length= tracks the running cumulative
byte count (= O(1) read).  The legacy =:bytes= / =:pos= keys
are kept as vestigial init values for API compat; they are not
read on the hot path."
  (vector (list :chunks nil :length 0
                :bytes "" :pos 0
                :labels nil :fixups nil :relocs nil)))

(defun nelisp-asm-arm64-buffer-bytes (buf)
  "Return BUF's accumulated bytes as a unibyte-string.
Finalizes the §92.d-arm64 chunk-build accumulator via one
`(apply #\\='concat (nreverse :chunks))' call (= O(total-bytes)
not O(N²)).  Not patched — call `nelisp-asm-arm64-resolve-
fixups' first if any `emit-fixup' entries are pending.
Idempotent: uses `copy-sequence' on the chunk spine before
`nreverse' so repeated reads stay safe."
  (let ((plist (nelisp-asm-arm64--unwrap buf)))
    (apply #'concat
           (nreverse (copy-sequence (plist-get plist :chunks))))))

(defun nelisp-asm-arm64-buffer-pos (buf)
  "Return BUF's current byte offset (= number of bytes written).
§92.d-arm64: read from the cached `:length' field (= O(1))."
  (plist-get (nelisp-asm-arm64--unwrap buf) :length))

(defun nelisp-asm-arm64-buffer-labels (buf)
  "Return BUF's labels alist `((NAME . POS) ...)' (reverse-defn order)."
  (plist-get (nelisp-asm-arm64--unwrap buf) :labels))

(defun nelisp-asm-arm64-buffer-fixups (buf)
  "Return BUF's pending fixups list of `(SLOT . LABEL . TYPE)' triples.
Each entry is `(SLOT LABEL TYPE)'."
  (plist-get (nelisp-asm-arm64--unwrap buf) :fixups))

(defun nelisp-asm-arm64-buffer-relocs (buf)
  "Return BUF's pending relocations as a list of plists.
Each entry is `(:type TYPE :sym SYM :offset OFFSET :addend N)' —
order matches emit order, suitable for Doc 93 linker handoff."
  (plist-get (nelisp-asm-arm64--unwrap buf) :relocs))

(defun nelisp-asm-arm64--append-bytes (buf bs)
  "Append unibyte-string BS to BUF's byte stream and advance pos.
Internal mutator — call sites are the per-instruction emitters.
§92.d-arm64 chunk-build: cons BS onto =:chunks= (= O(1) push)
and bump =:length=, instead of =(concat old bs)= which was
O(N²) for long buffers."
  (let* ((plist (nelisp-asm-arm64--unwrap buf))
         (chunks (plist-get plist :chunks))
         (len (plist-get plist :length)))
    (setq plist (plist-put plist :chunks (cons bs chunks)))
    (setq plist (plist-put plist :length (+ len (length bs))))
    (nelisp-asm-arm64--rewrap buf plist)))

(defun nelisp-asm-arm64--emit-word (buf word)
  "Emit WORD (= 32-bit instruction) into BUF as 4 LE bytes."
  (nelisp-asm-arm64--append-bytes
   buf (nelisp-asm-arm64--word-bytes word)))

(defun nelisp-asm-arm64-define-label (buf name)
  "Mark NAME as resolved at BUF's current byte position.
Signals `nelisp-asm-arm64-error' on duplicate label — silent
shadow would mask codegen bugs."
  (let* ((plist (nelisp-asm-arm64--unwrap buf))
         (labels (plist-get plist :labels)))
    (when (assq name labels)
      (signal 'nelisp-asm-arm64-error
              (list :duplicate-label name)))
    (setq plist (plist-put plist :labels
                           (cons (cons name (plist-get plist :length))
                                 labels)))
    (nelisp-asm-arm64--rewrap buf plist)))

(defun nelisp-asm-arm64-emit-fixup (buf slot-offset label type)
  "Record a 4-byte arm64 branch fixup at SLOT-OFFSET against LABEL.
TYPE is one of `b26' / `bl26' / `b19'.  The first two share a
26-bit immediate field at the low end of the instruction word
(= base 0x14000000 for B, 0x94000000 for BL).  `b19' is for
`B.cond' instructions (= base 0x54000000) where the 19-bit signed
byte-offset / 4 lives in bits [23:5] of the word.  Resolution
computes the appropriate imm field at finalize time and ORs it
into the existing constant base already written; the caller is
responsible for emitting the 4-byte placeholder (= base only,
imm field = 0) before recording the fixup."
  (unless (memq type '(b26 bl26 b19))
    (signal 'nelisp-asm-arm64-error
            (list :unknown-fixup-type type)))
  (let* ((plist (nelisp-asm-arm64--unwrap buf))
         (fixups (plist-get plist :fixups)))
    (setq plist (plist-put plist :fixups
                           (cons (list slot-offset label type)
                                 fixups)))
    (nelisp-asm-arm64--rewrap buf plist)))

(defun nelisp-asm-arm64-emit-reloc (buf type sym &optional addend)
  "Record a pending relocation entry against external symbol SYM.
TYPE is one of `b26-pc' (= R_AARCH64_CALL26, PC-relative 26-bit
BL imm) / `abs64' (= R_AARCH64_ABS64, 64-bit absolute).  This
helper records only; the caller is responsible for emitting the
placeholder bytes (4 zeros for b26-pc, 8 zeros for abs64) at the
recorded offset.  ADDEND defaults to 0 (= matches ELF64
r_addend)."
  (unless (memq type '(b26-pc abs64))
    (signal 'nelisp-asm-arm64-error
            (list :unknown-reloc-type type)))
  (let* ((plist (nelisp-asm-arm64--unwrap buf))
         (relocs (plist-get plist :relocs))
         (entry (list :type type :sym sym
                      :offset (plist-get plist :length)
                      :addend (or addend 0))))
    (setq plist (plist-put plist :relocs (append relocs (list entry))))
    (nelisp-asm-arm64--rewrap buf plist)))

(defun nelisp-asm-arm64--read-word-le (vec slot)
  "Read 4 LE bytes at SLOT from byte vector VEC, return 32-bit word."
  (logior (aref vec    slot)
          (ash (aref vec (+ slot 1))  8)
          (ash (aref vec (+ slot 2)) 16)
          (ash (aref vec (+ slot 3)) 24)))

(defun nelisp-asm-arm64--write-word-le (vec slot word)
  "Write 32-bit WORD at SLOT in byte vector VEC as 4 LE bytes."
  (let ((u (logand word #xFFFFFFFF)))
    (aset vec    slot      (logand u #xFF))
    (aset vec (+ slot 1)   (logand (ash u  -8) #xFF))
    (aset vec (+ slot 2)   (logand (ash u -16) #xFF))
    (aset vec (+ slot 3)   (logand (ash u -24) #xFF))))

(defun nelisp-asm-arm64-resolve-fixups (buf)
  "Apply every pending fixup in BUF, returning the patched bytes.
Each fixup `(SLOT LABEL TYPE)' is resolved to imm26 = `(label-pos
- slot) >> 2' and ORed into the placeholder word's low 26 bits.
TYPE is informational here (= both \\='b26 and \\='bl26 share the
same 26-bit field shape; the base opcode bits already in the slot
distinguish B from BL).  Signals `nelisp-asm-arm64-error' on a
fixup whose LABEL was never defined or whose displacement is not
4-byte aligned or out of ±128 MiB range.  Returns the patched
unibyte-string; BUF is mutated in place.

§92.d-arm64 chunk-build: finalize chunks once into a single
materialized unibyte-string, patch via a mutable vector, then
store back as a single chunk (= the cached chunk list collapses
to length 1 so subsequent `buffer-bytes' calls remain
O(total-bytes))."
  (let* ((plist  (nelisp-asm-arm64--unwrap buf))
         (chunks (plist-get plist :chunks))
         (bytes  (apply #'concat (nreverse (copy-sequence chunks))))
         (labels (plist-get plist :labels))
         (fixups (plist-get plist :fixups))
         (n (length bytes))
         (vec (make-vector n 0))
         (i 0))
    (while (< i n)
      (aset vec i (aref bytes i))
      (setq i (1+ i)))
    (dolist (fix fixups)
      (let* ((slot  (nth 0 fix))
             (label (nth 1 fix))
             (type  (or (nth 2 fix) 'b26))
             (cell  (assq label labels)))
        (unless cell
          (signal 'nelisp-asm-arm64-error
                  (list :unresolved-label label :at-slot slot)))
        (let ((disp (- (cdr cell) slot)))
          (unless (zerop (logand disp #x3))
            (signal 'nelisp-asm-arm64-error
                    (list :branch-misaligned disp :at-slot slot)))
          (pcase type
            ((or 'b26 'bl26)
             (let ((imm26 (ash disp -2)))
               (unless (and (>= imm26 (- (ash 1 25)))
                            (<  imm26 (ash 1 25)))
                 (signal 'nelisp-asm-arm64-error
                         (list :branch-out-of-range disp :at-slot slot)))
               (let* ((cur  (nelisp-asm-arm64--read-word-le vec slot))
                      (new  (logior cur
                                    (logand imm26 #x3FFFFFF))))
                 (nelisp-asm-arm64--write-word-le vec slot new))))
            ('b19
             ;; Doc 100 §100.D Stage 2: B.cond imm19 patches bits
             ;; [23:5] of the instruction word.  imm19 is a signed
             ;; 19-bit byte-offset / 4 (= ±1 MiB / 4-aligned).
             (let ((imm19 (ash disp -2)))
               (unless (and (>= imm19 (- (ash 1 18)))
                            (<  imm19 (ash 1 18)))
                 (signal 'nelisp-asm-arm64-error
                         (list :bcond-out-of-range disp :at-slot slot)))
               (let* ((cur  (nelisp-asm-arm64--read-word-le vec slot))
                      (field (ash (logand imm19 #x7FFFF) 5))
                      (new  (logior cur field)))
                 (nelisp-asm-arm64--write-word-le vec slot new))))
            (other
             (signal 'nelisp-asm-arm64-error
                     (list :unknown-fixup-type other :at-slot slot)))))))
    (let ((patched (apply #'unibyte-string (append vec nil))))
      ;; Collapse chunk list to a single materialized chunk so
      ;; subsequent `buffer-bytes' calls return the patched form.
      (setq plist (plist-put plist :chunks (list patched)))
      (nelisp-asm-arm64--rewrap buf plist)
      patched)))

;; ---- instruction emitters (= §92.b (3)) ----
;;
;; All emit-* helpers MUTATE BUF and return BUF (= chainable).

(defun nelisp-asm-arm64-mov-imm-z (buf reg imm16)
  "Emit `MOVZ Xd, #IMM16, LSL #0' (= clears upper 48 bits).
Base 0xD2800000 | (imm16 << 5) | Rd.  IMM16 must fit in 0..#xFFFF."
  (unless (and (integerp imm16) (>= imm16 0) (<= imm16 #xFFFF))
    (signal 'nelisp-asm-arm64-error
            (list :movz-imm-out-of-range imm16)))
  (let* ((d (logand (nelisp-asm-arm64--reg-num reg) #x1F))
         (word (logior #xD2800000
                       (ash (logand imm16 #xFFFF) 5)
                       d)))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-mov-imm-k (buf reg imm16 lsl)
  "Emit `MOVK Xd, #IMM16, LSL #LSL' (= patches one 16-bit slice).
LSL is one of 0 / 16 / 32 / 48.  Base 0xF2800000 | (hw << 21) |
(imm16 << 5) | Rd, where hw = LSL/16."
  (unless (memq lsl '(0 16 32 48))
    (signal 'nelisp-asm-arm64-error
            (list :movk-bad-lsl lsl)))
  (unless (and (integerp imm16) (>= imm16 0) (<= imm16 #xFFFF))
    (signal 'nelisp-asm-arm64-error
            (list :movk-imm-out-of-range imm16)))
  (let* ((d (logand (nelisp-asm-arm64--reg-num reg) #x1F))
         (hw (/ lsl 16))
         (word (logior #xF2800000
                       (ash hw 21)
                       (ash (logand imm16 #xFFFF) 5)
                       d)))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64--emit-movz-shifted (buf reg imm16 lsl)
  "Internal: emit a `MOVZ Xd, #IMM16, LSL #LSL'.
Same as `mov-imm-z' but with an explicit LSL (= 0/16/32/48) hw
field.  Used by `mov-imm64' for the first non-zero slice when it
is not the lowest 16 bits."
  (unless (memq lsl '(0 16 32 48))
    (signal 'nelisp-asm-arm64-error (list :movz-bad-lsl lsl)))
  (let* ((d (logand (nelisp-asm-arm64--reg-num reg) #x1F))
         (hw (/ lsl 16))
         (word (logior #xD2800000
                       (ash hw 21)
                       (ash (logand imm16 #xFFFF) 5)
                       d)))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-mov-imm64 (buf reg imm64)
  "Emit a MOVZ + MOVK chain that loads the full 64-bit IMM64 into REG.
Emits one MOVZ for the lowest non-zero 16-bit slice (or MOVZ #0
if the value is zero), then a MOVK for each remaining non-zero
slice.  Total = 1..4 instructions = 4..16 bytes.  IMM64 is masked
to 64 bits — negative values land on their two's-complement bit
pattern."
  (unless (integerp imm64)
    (signal 'nelisp-asm-arm64-error (list :imm-not-integer imm64)))
  (let* ((u (logand imm64 #xFFFFFFFFFFFFFFFF))
         (slices (list (cons (logand u #xFFFF)               0)
                       (cons (logand (ash u -16) #xFFFF)    16)
                       (cons (logand (ash u -32) #xFFFF)    32)
                       (cons (logand (ash u -48) #xFFFF)    48)))
         (emitted nil))
    (dolist (cell slices)
      (let ((slice (car cell))
            (shift (cdr cell)))
        (unless (zerop slice)
          (if emitted
              (nelisp-asm-arm64-mov-imm-k buf reg slice shift)
            (nelisp-asm-arm64--emit-movz-shifted buf reg slice shift)
            (setq emitted t)))))
    (unless emitted
      ;; All-zero immediate -> a single MOVZ #0.
      (nelisp-asm-arm64-mov-imm-z buf reg 0))
    buf))

(defun nelisp-asm-arm64-mov-reg-reg (buf dst src)
  "Emit `MOV Xd, Xm' (= alias for ORR Xd, XZR, Xm).
Base 0xAA0003E0 | (Xm << 16) | Xd."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num src) #x1F))
         (word (logior #xAA0003E0
                       (ash m 16)
                       d)))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64--imm12-check (imm)
  "Signal if IMM is out of the unsigned 12-bit range 0..#xFFF."
  (unless (and (integerp imm) (>= imm 0) (< imm #x1000))
    (signal 'nelisp-asm-arm64-error
            (list :imm12-out-of-range imm))))

(defun nelisp-asm-arm64-add-imm (buf dst src imm12)
  "Emit `ADD Xd, Xn, #IMM12' (12-bit unsigned, no shift).
Base 0x91000000 | (imm12 << 10) | (Rn << 5) | Rd."
  (nelisp-asm-arm64--imm12-check imm12)
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num src) #x1F))
         (word (logior #x91000000
                       (ash (logand imm12 #xFFF) 10)
                       (ash n 5)
                       d)))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-sub-imm (buf dst src imm12)
  "Emit `SUB Xd, Xn, #IMM12' (12-bit unsigned, no shift).
Base 0xD1000000 | (imm12 << 10) | (Rn << 5) | Rd."
  (nelisp-asm-arm64--imm12-check imm12)
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num src) #x1F))
         (word (logior #xD1000000
                       (ash (logand imm12 #xFFF) 10)
                       (ash n 5)
                       d)))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-cmp-imm (buf reg imm12)
  "Emit `CMP Xn, #IMM12' (= alias for SUBS XZR, Xn, #IMM12).
Base 0xF100001F | (imm12 << 10) | (Rn << 5).  Updates NZCV, result
discarded into XZR."
  (nelisp-asm-arm64--imm12-check imm12)
  (let* ((n (logand (nelisp-asm-arm64--reg-num reg) #x1F))
         (word (logior #xF100001F
                       (ash (logand imm12 #xFFF) 10)
                       (ash n 5))))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-svc (buf imm16)
  "Emit `SVC #IMM16' (= supervisor call, Linux syscall on imm = 0).
Base 0xD4000001 | (imm16 << 5).  IMM16 must fit in 0..#xFFFF."
  (unless (and (integerp imm16) (>= imm16 0) (<= imm16 #xFFFF))
    (signal 'nelisp-asm-arm64-error
            (list :svc-imm-out-of-range imm16)))
  (nelisp-asm-arm64--emit-word
   buf (logior #xD4000001 (ash (logand imm16 #xFFFF) 5))))

(defun nelisp-asm-arm64-brk (buf imm16)
  "Emit `BRK #IMM16' (= breakpoint trap).
Base 0xD4200000 | (imm16 << 5)."
  (unless (and (integerp imm16) (>= imm16 0) (<= imm16 #xFFFF))
    (signal 'nelisp-asm-arm64-error
            (list :brk-imm-out-of-range imm16)))
  (nelisp-asm-arm64--emit-word
   buf (logior #xD4200000 (ash (logand imm16 #xFFFF) 5))))

(defun nelisp-asm-arm64-ret (buf &optional reg)
  "Emit `RET [Xn]' (default Xn = X30, the link register).
Base 0xD65F0000 | (Xn << 5).  With Xn=30 collapses to 0xD65F03C0."
  (let* ((n (logand (nelisp-asm-arm64--reg-num (or reg 'x30)) #x1F))
         (word (logior #xD65F0000 (ash n 5))))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-nop (buf)
  "Emit `NOP' (= constant 0xD503201F, 4 bytes)."
  (nelisp-asm-arm64--emit-word buf #xD503201F))

(defun nelisp-asm-arm64-bl (buf label)
  "Emit `BL imm26' (= branch with link) with a fixup against LABEL.
Writes a 4-byte placeholder = base 0x94000000 (imm26 field = 0),
then records a `bl26' fixup at the placeholder offset.
`resolve-fixups' ORs imm26 = `(label-pos - slot) >> 2' into the
low 26 bits."
  (let ((slot (nelisp-asm-arm64-buffer-pos buf)))
    (nelisp-asm-arm64--emit-word buf #x94000000)
    (nelisp-asm-arm64-emit-fixup buf slot label 'bl26)))

(defun nelisp-asm-arm64-b (buf label)
  "Emit `B imm26' (= unconditional branch) with a fixup against LABEL.
Writes a 4-byte placeholder = base 0x14000000 (imm26 field = 0),
then records a `b26' fixup at the placeholder offset."
  (let ((slot (nelisp-asm-arm64-buffer-pos buf)))
    (nelisp-asm-arm64--emit-word buf #x14000000)
    (nelisp-asm-arm64-emit-fixup buf slot label 'b26)))

;; ---- §92.d-arm64 benchmark helper (= chunk-build perf gate) ----

(defun nelisp-asm-arm64-benchmark-emit (buf nbytes)
  "Emit ~NBYTES of synthetic NOP instructions into BUF.
Used by §92.d-arm64 perf gate (= 1 MB synthetic emit must
finish in < 5 sec on commodity hardware).  Each iteration calls
`nelisp-asm-arm64-nop' which pushes a 4-byte chunk onto
=:chunks=; the loop runs ceil(NBYTES/4) times so the total
emitted byte count is the smallest multiple of 4 >= NBYTES
(= AArch64 instructions are fixed-width 4 bytes).  Returns BUF
(= chainable).  Mirrors §92.d's x86_64 benchmark pattern."
  (let* ((words (/ (+ nbytes 3) 4))
         (i 0))
    (while (< i words)
      (nelisp-asm-arm64-nop buf)
      (setq i (1+ i))))
  buf)

(provide 'nelisp-asm-arm64)

;;; nelisp-asm-arm64.el ends here

;; ---- Doc 100 §100.D Stage 2 reg-reg / cmp / cset / bitwise / shift ----
;;
;; The 10 helpers below cover the AArch64 instructions Phase 47 needs
;; to emit a defun whose body is one of the 12 elisp `nelisp_jit_*'
;; trampoline sources (= add2 / sub2 / mul2 / 5 signed comparisons /
;; 3 bitwise / `ash').  Encoding strategy mirrors the existing
;; mov-imm / add-imm helpers — each instruction word is built by
;; OR-ing fixed opcode bits with the variable register / immediate
;; fields, then handed to `--emit-word'.

(defun nelisp-asm-arm64-add-reg-reg (buf dst lhs rhs)
  "Emit `ADD Xd, Xn, Xm' (shifted-register form, LSL #0).
Base 0x8B000000 | (Xm << 16) | (Xn << 5) | Xd."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x8B000000 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-sub-reg-reg (buf dst lhs rhs)
  "Emit `SUB Xd, Xn, Xm' (shifted-register form, LSL #0).
Base 0xCB000000 | (Xm << 16) | (Xn << 5) | Xd."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xCB000000 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-mul-reg-reg (buf dst lhs rhs)
  "Emit `MUL Xd, Xn, Xm' (= alias for MADD Xd, Xn, Xm, XZR).
Base 0x9B007C00 | (Xm << 16) | (Xn << 5) | Xd (Ra field fixed = 31)."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x9B007C00 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-and-reg-reg (buf dst lhs rhs)
  "Emit `AND Xd, Xn, Xm' (shifted-register).
Base 0x8A000000 | (Xm << 16) | (Xn << 5) | Xd."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x8A000000 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-orr-reg-reg (buf dst lhs rhs)
  "Emit `ORR Xd, Xn, Xm' (shifted-register).
Base 0xAA000000 | (Xm << 16) | (Xn << 5) | Xd."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xAA000000 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-eor-reg-reg (buf dst lhs rhs)
  "Emit `EOR Xd, Xn, Xm' (shifted-register, = bitwise XOR).
Base 0xCA000000 | (Xm << 16) | (Xn << 5) | Xd."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xCA000000 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-cmp-reg-reg (buf lhs rhs)
  "Emit `CMP Xn, Xm' (= SUBS XZR, Xn, Xm).
Updates NZCV flags; result discarded into XZR.
Base 0xEB00001F | (Xm << 16) | (Xn << 5)."
  (let* ((n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xEB00001F (ash m 16) (ash n 5)))))

(defconst nelisp-asm-arm64--cond-codes
  '((eq . #x0) (ne . #x1) (hs . #x2) (cs . #x2) (lo . #x3) (cc . #x3)
    (mi . #x4) (pl . #x5) (vs . #x6) (vc . #x7) (hi . #x8) (ls . #x9)
    (ge . #xA) (lt . #xB) (gt . #xC) (le . #xD) (al . #xE))
  "AArch64 4-bit condition code map.  Used by `cset' (= CSINC with
inverted cond) and `b.cond'.")

(defun nelisp-asm-arm64--cond-num (cond-sym)
  "Return the 4-bit cond code for COND-SYM, or signal on unknown."
  (let ((cell (assq cond-sym nelisp-asm-arm64--cond-codes)))
    (unless cell
      (signal 'nelisp-asm-arm64-error
              (list :unknown-cond-code cond-sym)))
    (cdr cell)))

(defun nelisp-asm-arm64-cset (buf dst cond-sym)
  "Emit `CSET Xd, COND' (= CSINC Xd, XZR, XZR, INVERT(COND)).
Materialises the COND flag as 1 (true) or 0 (false) in Xd.
Encoding: 0x9A9F07E0 | (invert_cond << 12) | Xd.
`invert_cond' = cond XOR 1 — toggles the low bit so that the
CSINC's `else' arm fires when COND is true."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (cond-bits (nelisp-asm-arm64--cond-num cond-sym))
         (inv (logxor cond-bits 1)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x9A9F07E0 (ash (logand inv #xF) 12) d))))

(defun nelisp-asm-arm64-lslv (buf dst lhs rhs)
  "Emit `LSLV Xd, Xn, Xm' (= logical-left shift by register).
Base 0x9AC02000 | (Xm << 16) | (Xn << 5) | Xd.  Only the low 6
bits of Xm are honoured by hardware (= shift count mod 64)."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x9AC02000 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-asrv (buf dst lhs rhs)
  "Emit `ASRV Xd, Xn, Xm' (= arithmetic-right shift by register).
Base 0x9AC02800 | (Xm << 16) | (Xn << 5) | Xd.  Sign bit replicates
into the high bits; shift count uses Xm[5:0] (= mod 64)."
  (let* ((d (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n (logand (nelisp-asm-arm64--reg-num lhs) #x1F))
         (m (logand (nelisp-asm-arm64--reg-num rhs) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x9AC02800 (ash m 16) (ash n 5) d))))

(defun nelisp-asm-arm64-b-cond (buf cond-sym label)
  "Emit `B.cond LABEL' with a fixup against LABEL.
Writes a 4-byte placeholder = base 0x54000000 | cond (imm19 field
= 0), then records a `b19' fixup at the placeholder offset.
`resolve-fixups' patches imm19 = `(label-pos - slot) >> 2' into
bits [23:5] of the placeholder word.  The branch displacement
must fit in a signed 19-bit byte offset (= ±1 MiB) and be 4-byte
aligned."
  (let* ((cond-bits (nelisp-asm-arm64--cond-num cond-sym))
         (slot (nelisp-asm-arm64-buffer-pos buf)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x54000000 (logand cond-bits #xF)))
    (nelisp-asm-arm64-emit-fixup buf slot label 'b19)))

(defun nelisp-asm-arm64-str-pre-sp-16 (buf src)
  "Emit `STR Xn, [SP, #-16]!' (= push Xn, pre-index SP -= 16).
Base 0xF81F0FE0 | Xt.  Stack stays 16-byte aligned per AAPCS."
  (let* ((t-reg (logand (nelisp-asm-arm64--reg-num src) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xF81F0FE0 t-reg))))

(defun nelisp-asm-arm64-ldr-post-sp-16 (buf dst)
  "Emit `LDR Xn, [SP], #16' (= pop Xn, post-index SP += 16).
Base 0xF84107E0 | Xt.  Stack stays 16-byte aligned per AAPCS."
  (let* ((t-reg (logand (nelisp-asm-arm64--reg-num dst) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xF84107E0 t-reg))))

;; ---- Doc 110 §110.D AArch64 SIMD/FP scalar-double helpers ----
;;
;; D-register encoding mirrors the X register table: low 5 bits land
;; in the Rn / Rm / Rd fields.  No REX-equivalent — AArch64 always
;; uses 5-bit register fields regardless of operand class.  Kept as
;; a separate table from `--reg' so passing a GP symbol to a D-only
;; helper signals immediately rather than emitting wrong encoding.

(defconst nelisp-asm-arm64--fp-reg
  '((d0 . 0)   (d1 . 1)   (d2 . 2)   (d3 . 3)
    (d4 . 4)   (d5 . 5)   (d6 . 6)   (d7 . 7)
    (d8 . 8)   (d9 . 9)   (d10 . 10) (d11 . 11)
    (d12 . 12) (d13 . 13) (d14 . 14) (d15 . 15)
    (d16 . 16) (d17 . 17) (d18 . 18) (d19 . 19)
    (d20 . 20) (d21 . 21) (d22 . 22) (d23 . 23)
    (d24 . 24) (d25 . 25) (d26 . 26) (d27 . 27)
    (d28 . 28) (d29 . 29) (d30 . 30) (d31 . 31))
  "AArch64 scalar f64 (= D-register) encoding map.
Doc 110 §110.D f64 ABI groundwork.  Each cell is `(NAME . N)'
where N is the 5-bit register number.  D-registers alias the
lower 64 bits of the V SIMD vector registers; scalar f64
encoding uses the D mnemonic.")

(defun nelisp-asm-arm64--fp-reg-num (reg)
  "Return the 5-bit f64 register number for REG.
Signals `nelisp-asm-arm64-error' with `:unknown-fp-register' when
REG is not in `nelisp-asm-arm64--fp-reg'."
  (let ((cell (assq reg nelisp-asm-arm64--fp-reg)))
    (unless cell
      (signal 'nelisp-asm-arm64-error
              (list :unknown-fp-register reg)))
    (cdr cell)))

;; Helper for f64 binop emit — bits 31..21 are constant for FADD /
;; FSUB / FMUL / FDIV (scalar double), only the 4-bit opcode field
;; at bits 15..12 differs.  Base shape:
;;   0001 1110 011 mmmmm OPCODE 10 nnnnn ddddd
;;   = 0x1E60_0800 | (OPCODE << 12) | (m << 16) | (n << 5) | d
;; Opcode field: 0000 FMUL / 0001 FDIV / 0010 FADD / 0011 FSUB.

(defun nelisp-asm-arm64--emit-fp-binop (buf opcode4 dst lhs rhs)
  "Internal: emit a `FADD / FSUB / FMUL / FDIV Dd, Dn, Dm' word.
OPCODE4 is the 4-bit op selector at bits 15..12.  Bits 11..10 are
fixed `10' for these data-processing 2-source FP instructions."
  (let* ((d (nelisp-asm-arm64--fp-reg-num dst))
         (n (nelisp-asm-arm64--fp-reg-num lhs))
         (m (nelisp-asm-arm64--fp-reg-num rhs))
         (word (logior #x1E600800
                       (ash (logand opcode4 #xF) 12)
                       (ash (logand m #x1F) 16)
                       (ash (logand n #x1F) 5)
                       (logand d #x1F))))
    (nelisp-asm-arm64--emit-word buf word)))

(defun nelisp-asm-arm64-fmul-reg-reg (buf dst lhs rhs)
  "Emit `FMUL Dd, Dn, Dm' (scalar double-precision multiply).
Base 0x1E600800 | (m<<16) | (n<<5) | d."
  (nelisp-asm-arm64--emit-fp-binop buf #x0 dst lhs rhs))

(defun nelisp-asm-arm64-fdiv-reg-reg (buf dst lhs rhs)
  "Emit `FDIV Dd, Dn, Dm' (scalar double-precision divide).
Base 0x1E601800.  Division-by-zero produces ±inf / NaN per IEEE 754."
  (nelisp-asm-arm64--emit-fp-binop buf #x1 dst lhs rhs))

(defun nelisp-asm-arm64-fadd-reg-reg (buf dst lhs rhs)
  "Emit `FADD Dd, Dn, Dm' (scalar double-precision add).
Base 0x1E602800."
  (nelisp-asm-arm64--emit-fp-binop buf #x2 dst lhs rhs))

(defun nelisp-asm-arm64-fsub-reg-reg (buf dst lhs rhs)
  "Emit `FSUB Dd, Dn, Dm' (scalar double-precision subtract).
Base 0x1E603800."
  (nelisp-asm-arm64--emit-fp-binop buf #x3 dst lhs rhs))

(defun nelisp-asm-arm64-fabs-reg-reg (buf dst src)
  "Emit `FABS Dd, Dn' (scalar double-precision absolute value).
Encoding:
  0001 1110 011 00000 1 10000 nnnnn ddddd
  = 0x1E60C000 | (n << 5) | d
Used by Doc 110 §110.D EQ-EPS to clear the sign bit of `(a - b)'
without going through bitmask materialisation (= AArch64 advantage
over x86_64's ANDPD path)."
  (let* ((d (nelisp-asm-arm64--fp-reg-num dst))
         (n (nelisp-asm-arm64--fp-reg-num src)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x1E60C000 (ash (logand n #x1F) 5) (logand d #x1F)))))

(defun nelisp-asm-arm64-fcmp-reg-reg (buf lhs rhs)
  "Emit `FCMP Dn, Dm' (scalar double-precision compare).
Sets NZCV flags; xmm registers unchanged.  Encoding:
  0001 1110 011 mmmmm 001000 nnnnn 00000
  = 0x1E602000 | (m << 16) | (n << 5)
For NaN inputs FCMP sets `V=1' (= unordered) so the standard
ordered conds (`gt' / `ge' / `mi' / `lt') yield 0 — caller picks
the right cond when materialising via `cset'."
  (let* ((n (nelisp-asm-arm64--fp-reg-num lhs))
         (m (nelisp-asm-arm64--fp-reg-num rhs)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x1E602000
                 (ash (logand m #x1F) 16)
                 (ash (logand n #x1F) 5)))))

(defun nelisp-asm-arm64-fmov-d-from-x (buf dst src)
  "Emit `FMOV Dd, Xn' (= GP→FP 64-bit transfer).
Encoding (= Conversion between floating-point and integer):
  1001 1110 011 00111 000000 nnnnn ddddd
  = 0x9E670000 | (n << 5) | d
Used by Doc 110 §110.D EQ-EPS to materialise the 1e-15 bit
pattern (= constructed in Xn via MOV/MOVK) into Dn for FCMP."
  (let* ((d (nelisp-asm-arm64--fp-reg-num dst))
         (n (logand (nelisp-asm-arm64--reg-num src) #x1F)))
    (nelisp-asm-arm64--emit-word
     buf (logior #x9E670000 (ash n 5) (logand d #x1F)))))

(defun nelisp-asm-arm64-stur-d-base-disp (buf src base imm9)
  "Emit `STUR Dt, [Xn, #IMM9]' (= unscaled store of low 8 bytes).
SRC is the Dt source, BASE is the Xn base register, IMM9 is the
signed 9-bit unscaled byte offset (= [-256, 255]).  Encoding:
  1111 1100 000 imm9 00 nnnnn ttttt
  = 0xFC000000 | ((imm9 & 0x1FF) << 12) | (n << 5) | t
Used by Doc 110 §110.D defun prologue to spill f64 params to the
local frame.  BASE may be SP or any X-register; alignment is the
caller's responsibility."
  (unless (and (integerp imm9) (<= -256 imm9 255))
    (signal 'nelisp-asm-arm64-error
            (list :stur-d-imm9-out-of-range imm9)))
  (let* ((t-reg (nelisp-asm-arm64--fp-reg-num src))
         (n-reg (logand (nelisp-asm-arm64--reg-num base) #x1F))
         (imm9-u (logand imm9 #x1FF)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xFC000000 (ash imm9-u 12) (ash n-reg 5) t-reg))))

(defun nelisp-asm-arm64-ldur-d-base-disp (buf dst base imm9)
  "Emit `LDUR Dt, [Xn, #IMM9]' (= unscaled load of low 8 bytes).
DST is the Dt destination.  IMM9 same constraints as STUR above.
Encoding:
  1111 1100 010 imm9 00 nnnnn ttttt
  = 0xFC400000 | ((imm9 & 0x1FF) << 12) | (n << 5) | t
Used by Doc 110 §110.D `--emit-f64-ref-load' to read a spilled
f64 param from `[x29 - 8*(slot+1)]'."
  (unless (and (integerp imm9) (<= -256 imm9 255))
    (signal 'nelisp-asm-arm64-error
            (list :ldur-d-imm9-out-of-range imm9)))
  (let* ((t-reg (nelisp-asm-arm64--fp-reg-num dst))
         (n-reg (logand (nelisp-asm-arm64--reg-num base) #x1F))
         (imm9-u (logand imm9 #x1FF)))
    (nelisp-asm-arm64--emit-word
     buf (logior #xFC400000 (ash imm9-u 12) (ash n-reg 5) t-reg))))

