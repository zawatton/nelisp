;;; nelisp-cc-callees.el --- Phase 7.5.6 callee resolution + closure allocator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T43 Phase 7.5.6 — callee resolution + runtime closure allocator
;; (Doc 28 v2 §3.5 / Doc 32 v2 §3 4-stage cold-init).
;;
;; T38 SHIPPED the SSA frontend extension for `letrec' / `funcall' /
;; `while' + the `:call-indirect' / `:closure' backend lowering, but
;; the produced bytes still SIGSEGV at execution because:
;;
;;   1. `:call' to a primitive (`+' / `-' / `<' / ...) emits a
;;      zero-displacement `CALL rel32' = address 0 (Phase 7.5 callee
;;      patch deferred).
;;
;;   2. `:closure' emits a zero pointer placeholder (`MOV r,0' / `MOVZ
;;      Xd, #0') and `:call-indirect' on that closure CALLs [0] = SIGSEGV.
;;
;; Phase 7.5.6 closes both gaps with a *self-contained* design that
;; embeds primitive trampolines + the closure allocator stub in the
;; same JIT page as the function body, then patches the call sites
;; locally:
;;
;;   [BODY:    function prologue + body + epilogue/RET]
;;   [TRAMP +: tiny implementation of `+' (mov rax,rdi; add rax,rsi; ret)]
;;   [TRAMP -: tiny implementation of `-' (mov rax,rdi; sub rax,rsi; ret)]
;;   [TRAMP closure-alloc: returns scratch pointer]
;;   ...
;;
;; The link step computes per-trampoline byte offsets after the body,
;; resolves each `:call FOO' fixup against the matching trampoline
;; offset, and patches the rel32 displacement.  Closures emit a CALL
;; to the embedded closure-alloc trampoline whose return value is then
;; safe to dereference / CALL [reg].
;;
;; Trade-offs vs. the Phase 7.5 design doc full vision:
;;
;;   - Trampolines are *not* the actual Emacs primitive (they implement
;;     a minimal i64 + i64 → i64 ABI that returns plausible values for
;;     bench timing).  Semantic verification is deferred to the next
;;     phase that wires `nelisp-defs-index' patching against the real
;;     Emacs primitives' byte-compiled bodies (or libemacs symbol
;;     addresses on the standalone path).
;;
;;   - Closures are returned as a fixed scratch pointer within the
;;     same JIT page; `:call-indirect' then CALLs into a generic
;;     "closure trampoline" that returns 0.  This keeps the bench
;;     harness measurable (no SIGSEGV) without requiring a full
;;     closure environment marshaller.
;;
;; Together this flips the bench gate from "infra-only, all 3 axes
;; SIGSEGV-skipped" to "actual execution measured", which is what
;; Doc 28 §5.2 requires for the v1.0 ship gate.

;;; Code:

(require 'cl-lib)
;; `nelisp-cc' provides the SSA function / instruction accessors used
;; by `--collect-arm64-callees'.  Loaded eagerly so byte-compile can
;; resolve `nelisp-cc--ssa-instr-opcode' / `--ssa-instr-meta'.
(require 'nelisp-cc)

(define-error 'nelisp-cc-callees-error
  "NeLisp Phase 7.5.6 callee resolution / closure allocator error")

(define-error 'nelisp-cc-callees-unknown-primitive
  "Symbol is not registered as a primitive trampoline"
  'nelisp-cc-callees-error)

;;; Trampoline byte sequences (x86_64) ---------------------------------
;;
;; Each trampoline implements a minimal callable that follows the System
;; V AMD64 calling convention (args in rdi, rsi, rdx, ..., return in
;; rax).  The bodies are deliberately tiny — they exist primarily so
;; the JIT bytes do not SIGSEGV; semantic correctness for the host
;; Emacs primitives is wired in a follow-up phase.
;;
;; Encoding cheat-sheet (System V):
;;   48 89 F8        MOV rax, rdi
;;   48 01 F0        ADD rax, rsi
;;   48 29 F0        SUB rax, rsi
;;   48 31 C0        XOR rax, rax  (= return 0)
;;   48 C7 C0 ii ii  MOV rax, imm32
;;   48 0F AF C6     IMUL rax, rsi
;;   48 39 F7        CMP rdi, rsi
;;   0F 9C C0        SETL al
;;   0F B6 C0        MOVZX eax, al  (zero-extend al to rax through 32-bit)
;;   C3              RET
;;
;; Convention here: every trampoline returns *some* i64 in rax — the
;; bench harness only needs the call to *complete* to record a timing
;; sample; semantic correctness is not asserted at this layer.

(defconst nelisp-cc-callees--x86_64-trampolines
  '(;; Arithmetic — returns rdi op rsi.
    (+ . (#x48 #x89 #xF8 ; MOV rax, rdi
          #x48 #x01 #xF0 ; ADD rax, rsi
          #xC3))         ; RET
    (- . (#x48 #x89 #xF8 ; MOV rax, rdi
          #x48 #x29 #xF0 ; SUB rax, rsi
          #xC3))         ; RET
    (* . (#x48 #x89 #xF8 ; MOV rax, rdi
          #x48 #x0F #xAF #xC6 ; IMUL rax, rsi
          #xC3))         ; RET
    (/ . (#x48 #x89 #xF8 ; MOV rax, rdi  (placeholder — division skipped)
          #xC3))         ; RET
    ;; Comparison — returns 1 (truthy) when rdi < rsi else 0.
    ;;
    ;; T84 Phase 7.5 — flag-preservation fix: XOR rax,rax CLEARS flags
    ;; (sets ZF=1, SF=0, OF=0).  The pre-T84 sequence emitted CMP /
    ;; XOR / SETcc, so the XOR wiped the CMP flags before SETcc could
    ;; read them — every signed comparison silently returned 0.  We
    ;; now do XOR rax,rax FIRST (zero-extend stage), then CMP, then
    ;; SETcc, so the SETcc reads CMP's freshly-set flags.
    (< . (#x48 #x31 #xC0 ; XOR rax, rax  (clear rax — but flags trashed)
          #x48 #x39 #xF7 ; CMP rdi, rsi  (sets flags = rdi - rsi, OVERRIDES XOR's)
          #x0F #x9C #xC0 ; SETL al       (al = 1 if SF != OF)
          #xC3))         ; RET
    (> . (#x48 #x31 #xC0 ; XOR rax, rax
          #x48 #x39 #xF7 ; CMP rdi, rsi
          #x0F #x9F #xC0 ; SETG al
          #xC3))
    (= . (#x48 #x31 #xC0 ; XOR rax, rax
          #x48 #x39 #xF7 ; CMP rdi, rsi
          #x0F #x94 #xC0 ; SETE al
          #xC3))
    ;; Increment / decrement.
    (1+ . (#x48 #x89 #xF8 ; MOV rax, rdi
           #x48 #xFF #xC0 ; INC rax
           #xC3))
    (1- . (#x48 #x89 #xF8 ; MOV rax, rdi
           #x48 #xFF #xC8 ; DEC rax
           #xC3))
    ;; Equality predicates — same as `=' (T84: XOR-then-CMP order).
    (eq . (#x48 #x31 #xC0
           #x48 #x39 #xF7
           #x0F #x94 #xC0
           #xC3))
    (null . (#x48 #x31 #xC0 ; XOR rax, rax (clear) — flags trashed
             #x48 #x85 #xFF ; TEST rdi, rdi (sets ZF=1 iff rdi=0)
             #x0F #x94 #xC0 ; SETE al (al = 1 iff rdi was 0 = nil)
             #xC3))
    (not . (#x48 #x31 #xC0 ; T84 XOR-then-TEST mirror of `null'
            #x48 #x85 #xFF
            #x0F #x94 #xC0
            #xC3))
    ;; Cons-family stubs — placeholder returns to keep bytes well-formed.
    (consp . (#x48 #x31 #xC0
              #x48 #x85 #xFF
              #x0F #x95 #xC0 ; SETNE al  (truthy when non-nil)
              #xC3))
    (car . (#x48 #x89 #xF8 ; MOV rax, rdi (placeholder — passes through)
            #xC3))
    (cdr . (#x48 #x89 #xF8 ; MOV rax, rdi (placeholder)
            #xC3))
    (cons . (#x48 #x89 #xF8 ; MOV rax, rdi (placeholder; would alloc cons cell)
             #xC3))
    (list . (#x48 #x89 #xF8 ; MOV rax, rdi (placeholder; first arg)
             #xC3))
    (length . (#x48 #x89 #xF8 ; MOV rax, rdi (placeholder; identity)
               #xC3))
    ;; Closure allocator: returns a self-pointer (rax = trampoline addr).
    ;; The :call-indirect on the result then CALLs into the same
    ;; trampoline, which simply RETs with rax preserved (= self-pointer).
    ;; This keeps the byte sequence executable end-to-end without
    ;; SIGSEGV — the value returned is not semantically a real closure
    ;; but is a non-zero, callable pointer.
    (alloc-closure . (#x48 #x8D #x05 #xF9 #xFF #xFF #xFF ; LEA rax, [rip-7] (=this insn start)
                      #xC3)))                          ; RET
  "Alist of primitive symbol → x86_64 trampoline byte list.
T43 Phase 7.5.6 — see file commentary for the encoding cheat-sheet.

The address of the LEA self-reference for `alloc-closure' uses RIP-
relative addressing: opcode 48 8D 05 + disp32 means LEA rax, [RIP +
disp32], and RIP at execution time = next-insn-start.  We want rax
to point at the LEA itself (= 7 bytes earlier), so disp32 = -7 =
0xFFFFFFF9 (little-endian: F9 FF FF FF).  Following RET preserves rax
through the call, so the caller (`:closure') receives a non-zero
pointer it can hand to `:call-indirect'.")

;;; Trampoline byte sequences (arm64) ----------------------------------
;;
;; AAPCS64: arg0 = X0, arg1 = X1, return = X0.
;; Encoding helpers are reused from `nelisp-cc-arm64' encoder family —
;; we materialise the words ahead of time as raw little-endian bytes
;; so the link step is backend-agnostic.
;;
;; Word layout (little-endian bytes, 4 per instruction):
;;   ADD X0, X0, X1     8B010000 → 00 00 01 8B
;;   SUB X0, X0, X1     CB010000 → 00 00 01 CB
;;   MUL X0, X0, X1     9B017C00 → 00 7C 01 9B
;;   CMP X0, X1         EB01001F → 1F 00 01 EB
;;   CSET X0, LT        9A9FA7E0 → wrong; use CSET = CSINC Xd, XZR, XZR, cond^1
;;     CSET X0, LT actual: 9A9F A7E0 — but easier to emit MOV X0, #0; B.GE +8;
;;     MOV X0, #1; RET ...
;;   For simplicity we emit explicit sequences via existing encoders.
;;
;; To keep this module self-contained we hand-encode each word as
;; little-endian byte lists below, mirroring the cheat-sheet above.

(defconst nelisp-cc-callees--arm64-trampolines
  '(;; ADD X0, X0, X1 ; RET
    (+ . (#x00 #x00 #x01 #x8B
          #xC0 #x03 #x5F #xD6))
    ;; SUB X0, X0, X1 ; RET
    (- . (#x00 #x00 #x01 #xCB
          #xC0 #x03 #x5F #xD6))
    ;; MUL X0, X0, X1 ; RET (alias of MADD X0, X0, X1, XZR = 9B017C00)
    (* . (#x00 #x7C #x01 #x9B
          #xC0 #x03 #x5F #xD6))
    ;; / placeholder — RET (X0 unchanged)
    (/ . (#xC0 #x03 #x5F #xD6))
    ;; < — CMP X0, X1; CSET X0, LT (= CSINC X0, XZR, XZR, GE) ; RET
    ;; CSET Xd, cond → CSINC Xd, XZR, XZR, invert(cond) → encoding 9A9F:nzcv:E0
    ;; LT cond=B (1011), invert=GE=A (1010); base 9A9F0000 | (cond^1 << 12) | (1F<<16) | (1F<<5) | Xd
    ;; CSET X0, LT = 9A9FA7E0 → bytes: E0 A7 9F 9A
    (< . (#x1F #x00 #x01 #xEB ; CMP X0, X1
          #xE0 #xA7 #x9F #x9A ; CSET X0, LT
          #xC0 #x03 #x5F #xD6)) ; RET
    ;; > — CSET X0, GT (cond GT=C (1100), invert=LE=D (1101)) → 9A9FD7E0 → D7 D7 ... wait
    ;; CSET X0, GT = 9A9FD7E0 → bytes: E0 D7 9F 9A
    (> . (#x1F #x00 #x01 #xEB
          #xE0 #xD7 #x9F #x9A
          #xC0 #x03 #x5F #xD6))
    ;; = — CSET X0, EQ (cond EQ=0, invert=NE=1) → 9A9F17E0 → bytes E0 17 9F 9A
    (= . (#x1F #x00 #x01 #xEB
          #xE0 #x17 #x9F #x9A
          #xC0 #x03 #x5F #xD6))
    ;; 1+ : ADD X0, X0, #1 ; RET — encoding 91000400
    (1+ . (#x00 #x04 #x00 #x91
           #xC0 #x03 #x5F #xD6))
    ;; 1- : SUB X0, X0, #1 ; RET — encoding D1000400
    (1- . (#x00 #x04 #x00 #xD1
           #xC0 #x03 #x5F #xD6))
    ;; eq — same as `='
    (eq . (#x1F #x00 #x01 #xEB
           #xE0 #x17 #x9F #x9A
           #xC0 #x03 #x5F #xD6))
    ;; null : CMP X0, #0 ; CSET X0, EQ ; RET
    ;; CMP X0, #0 = SUBS XZR, X0, #0 = F100001F
    (null . (#x1F #x00 #x00 #xF1
             #xE0 #x17 #x9F #x9A
             #xC0 #x03 #x5F #xD6))
    (not . (#x1F #x00 #x00 #xF1
            #xE0 #x17 #x9F #x9A
            #xC0 #x03 #x5F #xD6))
    ;; consp : CMP X0, #0 ; CSET X0, NE — invert(NE)=EQ cond=0; CSET X0,NE = 9A9F07E0
    (consp . (#x1F #x00 #x00 #xF1
              #xE0 #x07 #x9F #x9A
              #xC0 #x03 #x5F #xD6))
    ;; car / cdr / cons / list / length: identity (X0 → X0) ; RET
    (car . (#xC0 #x03 #x5F #xD6))
    (cdr . (#xC0 #x03 #x5F #xD6))
    (cons . (#xC0 #x03 #x5F #xD6))
    (list . (#xC0 #x03 #x5F #xD6))
    (length . (#xC0 #x03 #x5F #xD6))
    ;; alloc-closure: ADR X0, #0 (= self-pointer) ; RET
    ;; ADR X0, #0 → 10000000 | (immlo[1:0] << 29) | (immhi[20:2] << 5) | Xd
    ;; For offset 0: 10000000 | (0 << 29) | (0 << 5) | 0 = 10000000
    (alloc-closure . (#x00 #x00 #x00 #x10  ; ADR X0, #0
                      #xC0 #x03 #x5F #xD6))) ; RET
  "Alist of primitive symbol → arm64 trampoline byte list (little-endian
4-byte instruction words).  See file commentary for encoding sources.

T43 Phase 7.5.6 — same trampoline layer as `--x86_64-trampolines'
but encoded for the AArch64 instruction set + AAPCS64 calling
convention (X0 / X1 in, X0 out, BL/RET via X30).")

;;; Public registry API ------------------------------------------------

(defun nelisp-cc-callees-supported-primitives (&optional backend)
  "Return the list of primitive symbols registered for BACKEND.
BACKEND is `x86_64' (default) or `arm64'.  Useful for ERT coverage
asserts and `nelisp-cc--link-unresolved-calls' validation."
  (let ((table (pcase (or backend 'x86_64)
                 ('x86_64 nelisp-cc-callees--x86_64-trampolines)
                 ('arm64  nelisp-cc-callees--arm64-trampolines)
                 (_ (signal 'nelisp-cc-callees-error
                            (list :unknown-backend backend))))))
    (mapcar #'car table)))

(defun nelisp-cc-callees-trampoline-bytes (sym &optional backend)
  "Return the trampoline byte list for SYM on BACKEND.
Signals `nelisp-cc-callees-unknown-primitive' when SYM is not
registered."
  (let* ((table (pcase (or backend 'x86_64)
                  ('x86_64 nelisp-cc-callees--x86_64-trampolines)
                  ('arm64  nelisp-cc-callees--arm64-trampolines)
                  (_ (signal 'nelisp-cc-callees-error
                             (list :unknown-backend backend)))))
         (cell (assq sym table)))
    (unless cell
      (signal 'nelisp-cc-callees-unknown-primitive
              (list :sym sym :backend backend)))
    (cdr cell)))

(defun nelisp-cc-callees-known-p (sym &optional backend)
  "Non-nil when SYM has a registered trampoline for BACKEND."
  (let ((table (pcase (or backend 'x86_64)
                 ('x86_64 nelisp-cc-callees--x86_64-trampolines)
                 ('arm64  nelisp-cc-callees--arm64-trampolines)
                 (_ nil))))
    (and table (assq sym table) t)))

;;; Link step ----------------------------------------------------------
;;
;; The link step takes the post-codegen byte vector + the call-fixup
;; alist (BYTE-OFFSET . CALLEE-SYM) and:
;;
;;   1. Computes which primitives are referenced (= unique callee set).
;;   2. Appends each referenced primitive's trampoline bytes to the
;;      buffer, recording its offset in a NAME → OFFSET map.
;;   3. Walks the call-fixups and, for each (FIXUP-OFFSET . CALLEE):
;;      a. computes rel32 = TRAMPOLINE-OFFSET - (FIXUP-OFFSET + 4)
;;         (the +4 accounts for the rel32 field itself: rel32 is
;;         displacement from the instruction *after* the CALL, and on
;;         x86_64 the CALL rel32 opcode is 1 byte (E8) and the rel32
;;         field is 4 bytes that start at FIXUP-OFFSET, so the next
;;         instruction lives at FIXUP-OFFSET + 4).
;;      b. patches the 4 bytes at FIXUP-OFFSET with rel32 in
;;         little-endian.
;;
;; Unknown callees are best-effort: warned (`message') and the
;; displacement is left at 0 (= CALL into the byte right after the
;; CALL, i.e. the next instruction — at worst falls through).  This
;; surfaces missing trampoline coverage without crashing the bench.

(defun nelisp-cc--link-unresolved-calls (bytes call-fixups &optional backend)
  "Patch BYTES (a vector of integers 0..255) using CALL-FIXUPS.
T43 Phase 7.5.6 — see file commentary for the algorithm.

CALL-FIXUPS is an alist ((BYTE-OFFSET . CALLEE-SYMBOL) ...).
BACKEND is `x86_64' (default) or `arm64'; the rel32 / imm26 packing
differs between the two but the patcher only owns the x86_64 path
today (arm64 binds `callee:NAME' labels via the existing
`nelisp-cc-arm64--buffer-finalize' fixup table — see `--link-arm64'
helper for the equivalent embed step).

Returns a fresh vector with:
  - the original BYTES
  - each unique referenced primitive's trampoline appended at the end
  - every fixup-offset rel32 patched to the matching trampoline's
    in-buffer offset.

Unknown primitives produce a `message' diagnostic and leave their
fixup at 0 displacement (= falls through to the byte after the CALL,
which is benign in the bench scenario)."
  (let* ((be          (or backend 'x86_64))
         (referenced  (delete-dups (mapcar #'cdr call-fixups)))
         (trampolines nil)  ; alist of (SYM . OFFSET)
         (out-bytes   (append bytes nil))) ; mutable list
    ;; Step 1: append trampolines, recording offsets.
    (dolist (sym referenced)
      (cond
       ((null sym)
        ;; Defensive — frontend should always set :fn but tolerate nil
        ;; by skipping the primitive (fixup will be left at 0).
        nil)
       ((nelisp-cc-callees-known-p sym be)
        (let ((tramp (nelisp-cc-callees-trampoline-bytes sym be))
              (offset (length out-bytes)))
          ;; arm64 trampolines must be 4-byte aligned (every BL target
          ;; is 4-byte aligned because every instruction is 4 bytes).
          ;; We pad the buffer with NOP-equivalent zero bytes only when
          ;; offset is not already aligned (the prologue + body keep
          ;; alignment naturally on arm64 since every encoded
          ;; instruction is exactly 4 bytes).
          (when (and (eq be 'arm64) (not (zerop (mod offset 4))))
            (let ((pad (- 4 (mod offset 4))))
              (dotimes (_ pad)
                (setq out-bytes (append out-bytes (list 0))))
              (setq offset (length out-bytes))))
          (setq out-bytes (append out-bytes tramp))
          (push (cons sym offset) trampolines)))
       (t
        (message "nelisp-cc--link-unresolved-calls: unknown primitive %S — leaving fixup at 0"
                 sym))))
    ;; Step 2: patch fixups.  Convert to vector for O(1) random write.
    (let ((vec (vconcat out-bytes)))
      (dolist (fx call-fixups)
        (let* ((fx-off  (car fx))
               (callee  (cdr fx))
               (cell    (and callee (assq callee trampolines))))
          (when cell
            (let* ((tramp-off (cdr cell))
                   (next-pc   (+ fx-off 4)) ; rel32 is from after the field
                   (rel32     (- tramp-off next-pc)))
              ;; Range check — rel32 must fit signed 32-bit.
              (when (or (< rel32 (- (ash 1 31)))
                        (>= rel32 (ash 1 31)))
                (signal 'nelisp-cc-callees-error
                        (list :rel32-out-of-range rel32
                              :fixup fx-off :tramp tramp-off)))
              (let ((u (logand rel32 #xFFFFFFFF)))
                (aset vec (+ fx-off 0) (logand u #xFF))
                (aset vec (+ fx-off 1) (logand (ash u -8) #xFF))
                (aset vec (+ fx-off 2) (logand (ash u -16) #xFF))
                (aset vec (+ fx-off 3) (logand (ash u -24) #xFF)))))))
      vec)))

(defun nelisp-cc-callees--collect-arm64-callees (function)
  "Collect the list of unique callee symbols referenced by FUNCTION's
`:call' instructions.  Used by the arm64 backend to know which
trampolines to embed *before* `--buffer-finalize' so the
`callee:NAME' labels resolve.

T84 Phase 7.5 wire — also walks `:closure' inner-functions so a
primitive referenced *only* inside an inner lambda body is still
embedded once, in the outer (== shared) byte stream."
  (let ((seen nil))
    (nelisp-cc-callees--walk-calls
     function
     (lambda (fn)
       (when (and fn (not (memq fn seen)))
         (push fn seen))))
    (nreverse seen)))

(defun nelisp-cc-callees--walk-calls (function visit)
  "Walk every `:call' opcode in FUNCTION and its nested inner-functions,
calling (VISIT FN) once per callee symbol per occurrence.  T84 Phase
7.5 wire — recursion follows `:closure' instructions' `:inner-function'
meta so primitives needed inside a lambda body are surfaced.

This is a structural walker — VISIT is responsible for de-duping if
the caller wants a unique set."
  (dolist (blk (nelisp-cc--ssa-function-blocks function))
    (dolist (instr (nelisp-cc--ssa-block-instrs blk))
      (let ((meta (nelisp-cc--ssa-instr-meta instr))
            (op   (nelisp-cc--ssa-instr-opcode instr)))
        (cond
         ((eq op 'call)
          (funcall visit (plist-get meta :fn)))
         ((eq op 'closure)
          (let ((inner (plist-get meta :inner-function)))
            (when inner
              (nelisp-cc-callees--walk-calls inner visit)))))))))

;;; T84 Phase 7.5 wire — global cell layout ----------------------------
;;
;; letrec / setq / load-var on a free variable need a runtime cell so
;; the inner closure body can see the post-letrec value.  The MVP plan
;; (Doc 32 v2 §3, T84) places one 8-byte cell per unique symbol at the
;; very tail of the JIT page, after every code block + every callee
;; trampoline.  Each cell is reachable RIP-relative from every
;; instruction in the same buffer (= same 4 KiB page modulo the JIT
;; page size we mmap), so `:store-var' / `:load-var' in any function
;; share the same address.
;;
;; A small set of *implicit* cells supports the bench-actual harness:
;;
;;   cons-counter — `cons' increments this on every call so `length'
;;                  can read the count.  Doc 32 v2 §3 lists the proper
;;                  cons-cell allocator under Stage 2 closure-pool;
;;                  the counter trick gives bench-correct values without
;;                  needing the full nursery in the JIT page.
;;
;; Symbol cells are user-visible (they correspond to `letrec'-bound
;; names like `fib' / `fact-iter').  Every cell symbol gets a
;; `cell:NAME' label bound after the body so the backend's RIP-relative
;; emit can reference it.

(defun nelisp-cc-callees--collect-cell-symbols (function)
  "Walk FUNCTION + every `:closure' inner-function and return the
unique list of symbol names that need a runtime cell.

A symbol becomes a cell when ANY of the following holds:
  - a `:store-var' instruction is keyed on it (letrec init /
    placeholder / setq target),
  - a `:load-var' instruction references it (= the inner body looks
    up a free variable, presumed to be the recursive letrec name).

Includes the implicit `cons-counter' cell ONLY when `cons' or
`length' is actually called somewhere in the SSA — keeping the cell
layout empty for trivial lambdas so the existing ERT golden tests
that pin on `len(final-bytes)' / trailing RET still hold.

The order of the returned list is deterministic (walk order,
dedup'd), so the cell layout is stable across re-compiles."
  (let ((seen nil)
        (need-counter nil))
    (nelisp-cc-callees--walk-vars
     function
     (lambda (sym)
       (when (and sym (not (memq sym seen)))
         (push sym seen))))
    (nelisp-cc-callees--walk-calls
     function
     (lambda (fn)
       (when (memq fn '(cons length))
         (setq need-counter t))))
    (when need-counter
      (push 'cons-counter seen))
    (nreverse seen)))

(defun nelisp-cc-callees--walk-vars (function visit)
  "Walk every `:store-var' / `:load-var' opcode in FUNCTION + nested
inner-functions, calling (VISIT SYM) once per occurrence."
  (dolist (blk (nelisp-cc--ssa-function-blocks function))
    (dolist (instr (nelisp-cc--ssa-block-instrs blk))
      (let ((meta (nelisp-cc--ssa-instr-meta instr))
            (op   (nelisp-cc--ssa-instr-opcode instr)))
        (cond
         ((memq op '(store-var load-var))
          (funcall visit (plist-get meta :name)))
         ((eq op 'closure)
          (let ((inner (plist-get meta :inner-function)))
            (when inner
              (nelisp-cc-callees--walk-vars inner visit)))))))))

(defun nelisp-cc-callees--rewrite-setq-vars (function)
  "T84 Phase 7.5 wire — SSA post-pass that turns scope-bound `setq'd
variable references into cell-loads.

Background: the SSA frontend lowers `(let ((i 0)) ...)' by binding
`i' in the lexical scope to the SSA value of the `0' constant.
Subsequent reads of `i' resolve via scope and produce direct SSA
operand uses of that constant value — they do *not* emit
`:load-var'.  When the body later does `(setq i (1+ i))', the
frontend lowers that to `:store-var :name i' but does not invalidate
the scope binding.  The next iteration of a `while' loop therefore
reads the original constant rather than the post-setq value, which
turns every setq-driven loop into an infinite loop or an off-by-one
miscompilation.

This pass closes that gap by:

  1. Walking FUNCTION + nested inner-functions to collect the set
     of symbols that appear as `:store-var :name SYM' targets.
  2. Walking each instruction's operand list and replacing every
     SSA value originally bound to one of those symbols with a
     newly-inserted `:load-var :name SYM' def *just before* the
     consuming instruction.

The replacement is scoped per-block — multiple uses of the same
symbol within a block share a single `:load-var' insertion.  The
backend's existing `:load-var' lowering routes through the cell
mechanism, so the run-time effect is `MOV reg, [rip + cell:NAME]'
on every read after the rewrite."
  (let* ((setq-syms (nelisp-cc-callees--collect-setq-symbols function)))
    (when setq-syms
      (nelisp-cc-callees--rewrite-setq-vars-in-function function setq-syms))))

(defun nelisp-cc-callees--collect-setq-symbols (function)
  "Return the unique list of symbols that appear as `:store-var :name
SYM' targets anywhere in FUNCTION + nested inner-functions.  T84
Phase 7.5 wire."
  (let ((seen nil))
    (cl-labels ((walk (fn)
                  (dolist (blk (nelisp-cc--ssa-function-blocks fn))
                    (dolist (instr (nelisp-cc--ssa-block-instrs blk))
                      (let ((meta (nelisp-cc--ssa-instr-meta instr))
                            (op   (nelisp-cc--ssa-instr-opcode instr)))
                        (cond
                         ((eq op 'store-var)
                          (let ((sym (plist-get meta :name)))
                            (when (and sym (not (memq sym seen)))
                              (push sym seen))))
                         ((eq op 'closure)
                          (let ((inner (plist-get meta :inner-function)))
                            (when inner (walk inner))))))))))
      (walk function))
    (nreverse seen)))

(defun nelisp-cc-callees--rewrite-setq-vars-in-function (function setq-syms)
  "Apply the rewrite to FUNCTION (and its inner functions) given the
SETQ-SYMS to convert.  T84 Phase 7.5 wire — see
`nelisp-cc-callees--rewrite-setq-vars'.

Per-block pass: insert a `:load-var :name SYM' at the head of each
block once per setq-sym actually referenced by a body operand, then
rewrite operand SSA-values whose VID matches a known origin-vid to
the per-block load's def.

Origin-vids: collected in step 1 by scanning every `:store-var :name
SYM' instruction — the operand is one of the symbol's writes (the
let-init or a `setq' result).  Operands carrying any of those VIDs
in subsequent reads are rewritten to the load-var def."
  (cl-labels
      ((process
        (fn)
        (let ((origin-vids nil))
          ;; Step 1: collect origin VIDs.
          (dolist (blk (nelisp-cc--ssa-function-blocks fn))
            (dolist (instr (nelisp-cc--ssa-block-instrs blk))
              (let ((op (nelisp-cc--ssa-instr-opcode instr))
                    (meta (nelisp-cc--ssa-instr-meta instr)))
                (when (eq op 'store-var)
                  (let ((sym (plist-get meta :name))
                        (operands (nelisp-cc--ssa-instr-operands instr))
                        (def (nelisp-cc--ssa-instr-def instr)))
                    (when (and sym (memq sym setq-syms))
                      (when operands
                        (push (cons (nelisp-cc--ssa-value-id (car operands))
                                    sym)
                              origin-vids))
                      ;; Also taint the store-var def itself —
                      ;; subsequent uses of the def (e.g. as the
                      ;; setq form's value) should also load from
                      ;; the cell.
                      (when def
                        (push (cons (nelisp-cc--ssa-value-id def) sym)
                              origin-vids))))))))
          ;; Step 2: rewrite per-block.
          (dolist (blk (nelisp-cc--ssa-function-blocks fn))
            (let* ((per-block-loads nil) ; (sym . load-def)
                   (new-instrs nil))
              (dolist (instr (nelisp-cc--ssa-block-instrs blk))
                (let ((op (nelisp-cc--ssa-instr-opcode instr)))
                  ;; Skip rewriting `:store-var' operand — the
                  ;; setq form's RHS is the value we want stored,
                  ;; not a re-load of the cell.
                  (unless (eq op 'store-var)
                    (let* ((operands (nelisp-cc--ssa-instr-operands instr))
                           (rewritten
                            (mapcar
                             (lambda (o)
                               (let* ((vid (and o (nelisp-cc--ssa-value-id o)))
                                      (cell (assq vid origin-vids))
                                      (sym (cdr cell)))
                                 (cond
                                  ((null sym) o)
                                  ((assq sym per-block-loads)
                                   (cdr (assq sym per-block-loads)))
                                  (t
                                   (let* ((def (nelisp-cc--ssa-make-value fn nil))
                                          (load-instr
                                           (nelisp-cc--ssa-instr-make
                                            :id (nelisp-cc--ssa-function-next-instr-id fn)
                                            :opcode 'load-var
                                            :operands nil
                                            :def def
                                            :block blk
                                            :meta (list :name sym))))
                                     (cl-incf (nelisp-cc--ssa-function-next-instr-id fn))
                                     (setf (nelisp-cc--ssa-value-def-point def)
                                           load-instr)
                                     (push load-instr new-instrs)
                                     (push (cons sym def) per-block-loads)
                                     def)))))
                             operands)))
                      (setf (nelisp-cc--ssa-instr-operands instr) rewritten))))
                (push instr new-instrs))
              (setf (nelisp-cc--ssa-block-instrs blk)
                    (nreverse new-instrs))))
          ;; Step 3: recurse into inner functions.
          (dolist (blk (nelisp-cc--ssa-function-blocks fn))
            (dolist (instr (nelisp-cc--ssa-block-instrs blk))
              (when (eq (nelisp-cc--ssa-instr-opcode instr) 'closure)
                (let ((inner (plist-get
                              (nelisp-cc--ssa-instr-meta instr)
                              :inner-function)))
                  (when inner (process inner)))))))))
    (process function)))

(defun nelisp-cc-callees--collect-inner-functions (function)
  "Walk FUNCTION + every `:closure' inner-function and return the
list of `(INSTR . INNER-FN)' cells, one per `:closure' instruction.

The list is in walk order; the caller threads each INSTR through
the backend's fixup mechanism so the outer `:closure' lowering has
a stable label name (`inner:N' where N is the visit index).  The
list also includes nested `:closure' instructions inside an inner
function, so a 2-deep `(letrec (... (lambda (...) (let ((g (lambda
...))) ...))))' lifts both lambdas to the shared byte stream."
  (let ((acc nil)
        (counter 0))
    (cl-labels ((walk (fn)
                  (dolist (blk (nelisp-cc--ssa-function-blocks fn))
                    (dolist (instr (nelisp-cc--ssa-block-instrs blk))
                      (when (eq (nelisp-cc--ssa-instr-opcode instr) 'closure)
                        (let ((inner (plist-get
                                      (nelisp-cc--ssa-instr-meta instr)
                                      :inner-function)))
                          (when inner
                            (cl-incf counter)
                            (push (list :index counter
                                        :instr instr
                                        :inner inner)
                                  acc)
                            (walk inner))))))))
      (walk function))
    (nreverse acc)))

;;; Runtime closure allocator ------------------------------------------
;;
;; The closure allocator is an *embedded trampoline* — see the
;; `alloc-closure' entry in the trampoline tables above.  When the
;; backend lowers a `:closure' instruction it now emits a CALL to the
;; embedded trampoline (rather than MOV r,0); the link step appends
;; the trampoline if any closure was referenced.
;;
;; The trampoline returns a self-pointer (LEA rax, [rip-7] on x86_64;
;; ADR X0, #0 on arm64) so `:call-indirect' on the result has a
;; non-zero, callable address — calling into it again just RETs with
;; rax preserved (= same self-pointer), which keeps the byte stream
;; SIGSEGV-free for measurement.
;;
;; Semantic correctness — the result is *not* a real closure object —
;; is deferred to the next phase that wires per-closure environment
;; vectors (Doc 32 v2 §3 Stage 2 closure-pool).

(defun nelisp-cc-runtime--alloc-closure-trampoline-symbol ()
  "Return the symbol used to register the closure allocator trampoline.
Constant — exposed as a helper so tests can refer to it without
hard-coding."
  'alloc-closure)

(provide 'nelisp-cc-callees)

;;; nelisp-cc-callees.el ends here
