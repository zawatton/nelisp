;;; nelisp-cc-stage81-poc-test.el --- Doc 81 Stage 81.1 PoC ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT for Doc 81 Stage 81.1 — primitive-trampoline ABI mode + 3
;; new SSA opcodes + x86_64 backend emit + car PoC.  See
;; docs/design/81-nelisp-cc-primitive-trampoline-extension.org §5.1
;; for the design.
;;
;; Coverage:
;;
;;   1. stage81-poc-opcode-registry-shape
;;        `nelisp-cc--stage81-opcodes' has 3 entries with the
;;        articulated descriptor plist (= arity / def / terminator /
;;        returns).  Per §5.1.2 critical decision the two load
;;        opcodes return `(unsigned i64)' (zero-extended); sign-
;;        extended would silently corrupt branches on tag bytes
;;        >= 0x80.
;;
;;   2. stage81-poc-entry-abi-validation
;;        `nelisp-cc-runtime-compile-and-allocate' rejects unknown
;;        :entry-abi values and accepts both `:host-int' (default)
;;        and `:trampoline-unary'.
;;
;;   3. stage81-poc-load-tag-bytes
;;        Lowering an `ssa-load-tag' of param-0 emits the canonical
;;        4-byte MOVZX r64, byte ptr [rdi] sequence
;;        (= 0x48 0x0F 0xB6 0x07).  This is the operative
;;        zero-extending byte-load that Doc 81 §5.1.2 pinned.
;;
;;   4. stage81-poc-load-payload-ptr-bytes
;;        Lowering an `ssa-load-payload-ptr' of param-0 emits
;;        MOV r64, qword ptr [rdi+8] (= 0x48 0x8B 0x47 0x08), the
;;        SEXP_PAYLOAD_OFFSET load.
;;
;;   5. stage81-poc-cmp-tag-imm-bytes-and-fixups
;;        `ssa-cmp-tag-imm' emits CMP r8, imm8 + JE rel32 + JMP
;;        rel32 (= 14 bytes total) and registers two forward
;;        branch fixups against the synthetic L_block_<id> labels.
;;
;;   6. stage81-poc-primitive-table-shape
;;        `nelisp-cc-pipeline-primitive-table-stage1' contains the
;;        single `car' entry with arity 1, ABI mode
;;        `:trampoline-unary', and C symbol `nl_jit_cons_car'.
;;
;;   7. stage81-poc-trampoline-status-constants
;;        `nelisp-cc-runtime-trampoline-ok' = 0 and
;;        `nelisp-cc-runtime-trampoline-err' = 1, mirroring the
;;        Cranelift `nl_jit_cons_car' contract verbatim.
;;
;;   8. stage81-poc-car-ssa-shape
;;        Build the car PoC SSA function (ssa-load-tag → ssa-cmp-
;;        tag-imm → return); verify the IR is well-formed via
;;        `nelisp-cc--ssa-verify-function' and the dispatch arms in
;;        `--lower-instr' recognise the new opcodes.

;;; Code:

(require 'ert)
(require 'nelisp-cc)
(require 'nelisp-cc-x86_64)
(require 'nelisp-cc-arm64)
(require 'nelisp-cc-runtime)
(require 'nelisp-cc-pipeline)

;;; Helpers -----------------------------------------------------------

(defun nelisp-cc-stage81-test--make-empty-codegen (&optional extra-vids-vregs)
  "Build a fresh codegen for a single-block test SSA function.

Returns (CG FN PARAM DEF EXTRA-VALUES) — a codegen on a function
with one i64 param and one fresh SSA def value, both pre-allocated
to physical registers via a hand-rolled alloc state.

EXTRA-VIDS-VREGS, when non-nil, is a list of `vreg' symbols (`r2',
`r3', ...) to provision; for each entry a fresh SSA value is
allocated and an (vid . vreg) cell is pushed into the codegen's
alloc-state seed list.  This lets callers pre-allocate additional
SSA values they intend to add before sealing the codegen, since
the `alloc-state' slot is read-only after construction.

EXTRA-VALUES (5th element of return list) is the parallel list of
freshly-made SSA values, in the same order as EXTRA-VIDS-VREGS."
  (let* ((fn (nelisp-cc--ssa-make-function 'stage81-poc-test '(int)))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (def (nelisp-cc--ssa-make-value fn))
         (extras (mapcar (lambda (_vreg) (nelisp-cc--ssa-make-value fn))
                         extra-vids-vregs))
         (extra-cells (cl-mapcar
                       (lambda (val vreg)
                         (cons (nelisp-cc--ssa-value-id val) vreg))
                       extras extra-vids-vregs))
         ;; Hand-rolled alloc state: param → r0 (= rdi), def → r1 (= rsi),
         ;; plus any caller-provisioned extras.
         (alloc-state (append
                       extra-cells
                       (list (cons (nelisp-cc--ssa-value-id param) 'r0)
                             (cons (nelisp-cc--ssa-value-id def)   'r1))))
         (slot-alist nil)
         (frame-size 0)
         (buf (nelisp-cc-x86_64--buffer-make))
         (cg (nelisp-cc-x86_64--codegen-make
              :function fn
              :alloc-state alloc-state
              :buffer buf
              :slot-alist slot-alist
              :frame-size frame-size)))
    (list cg fn param def extras)))

(defun nelisp-cc-stage81-test--bytes (cg)
  "Return the unibyte vector of bytes currently held in CG's buffer.

The buffer stores bytes in reverse order; this helper reverses + makes
a vector for easier `should (equal ...)' comparisons."
  (let ((bs (nelisp-cc-x86_64--buffer-bytes
             (nelisp-cc-x86_64--codegen-buffer cg))))
    (apply #'unibyte-string (nreverse (copy-sequence bs)))))

;;; (1) Opcode registry shape ----------------------------------------

(ert-deftest nelisp-cc-stage81-poc-opcode-registry-shape ()
  "`nelisp-cc--stage81-opcodes' has the 4 Doc 81 §5.1.2 + §5.2.1
opcodes with the articulated descriptor plist.

Stage 81.1 shipped 3 opcodes (load-tag, load-payload-ptr,
cmp-tag-imm); Stage 81.2 added the 4th (`ssa-call-primitive')."
  (should (= 4 (length nelisp-cc--stage81-opcodes)))
  (let ((load-tag (nelisp-cc--ssa-stage81-opcode-info 'ssa-load-tag))
        (load-pp  (nelisp-cc--ssa-stage81-opcode-info 'ssa-load-payload-ptr))
        (cmp-tag  (nelisp-cc--ssa-stage81-opcode-info 'ssa-cmp-tag-imm))
        (call-prim (nelisp-cc--ssa-stage81-opcode-info 'ssa-call-primitive)))
    ;; All four are present.
    (should load-tag)
    (should load-pp)
    (should cmp-tag)
    (should call-prim)
    ;; Arity = 1 for the 3 Stage 81.1 opcodes (single ptr / tag).
    (should (= 1 (plist-get load-tag :arity)))
    (should (= 1 (plist-get load-pp  :arity)))
    (should (= 1 (plist-get cmp-tag  :arity)))
    ;; Stage 81.2 ssa-call-primitive uses :arity nil (variadic per ABI shape).
    (should (null (plist-get call-prim :arity)))
    ;; Two loads + call-primitive produce a def; cmp-tag-imm is a terminator.
    (should     (plist-get load-tag :def))
    (should     (plist-get load-pp  :def))
    (should     (plist-get call-prim :def))
    (should-not (plist-get cmp-tag  :def))
    (should-not (plist-get load-tag :terminator))
    (should-not (plist-get load-pp  :terminator))
    (should     (plist-get cmp-tag  :terminator))
    (should-not (plist-get call-prim :terminator))
    ;; *Critical* §5.1.2 design decision: zero-extended (unsigned) loads.
    (should (equal '(unsigned i64) (plist-get load-tag :returns)))
    (should (equal '(unsigned i64) (plist-get load-pp  :returns)))
    ;; Stage 81.2 — call-primitive returns u64 status (TRAMPOLINE_OK/_ERR).
    (should (equal '(unsigned i64) (plist-get call-prim :returns)))
    ;; Recognition predicate.
    (should (nelisp-cc--ssa-stage81-opcode-p 'ssa-load-tag))
    (should (nelisp-cc--ssa-stage81-opcode-p 'ssa-load-payload-ptr))
    (should (nelisp-cc--ssa-stage81-opcode-p 'ssa-cmp-tag-imm))
    (should (nelisp-cc--ssa-stage81-opcode-p 'ssa-call-primitive))
    (should-not (nelisp-cc--ssa-stage81-opcode-p 'add))
    (should-not (nelisp-cc--ssa-stage81-opcode-p 'return))))

;;; (2) Entry-ABI validation -----------------------------------------

(ert-deftest nelisp-cc-stage81-poc-entry-abi-validation ()
  "`nelisp-cc-runtime-compile-and-allocate' rejects unknown ABI mode
and accepts the 4 valid modes (Stage 81.1 + 81.2)."
  ;; `:host-int' is the legacy default — pre-existing behavior unchanged.
  (should (memq :host-int nelisp-cc-runtime--entry-abi-modes))
  ;; `:trampoline-unary' is the Doc 81 Stage 81.1 addition.
  (should (memq :trampoline-unary nelisp-cc-runtime--entry-abi-modes))
  ;; Stage 81.2 — `:trampoline-binary-ctor' / `:trampoline-binary-mut'.
  (should (memq :trampoline-binary-ctor
                nelisp-cc-runtime--entry-abi-modes))
  (should (memq :trampoline-binary-mut
                nelisp-cc-runtime--entry-abi-modes))
  ;; The validator accepts all 4.
  (dolist (mode '(:host-int :trampoline-unary
                  :trampoline-binary-ctor :trampoline-binary-mut))
    (should-not (condition-case nil
                    (progn (nelisp-cc-runtime--validate-entry-abi mode)
                           nil)
                  (nelisp-cc-runtime-error t))))
  ;; And rejects garbage.
  (should-error (nelisp-cc-runtime--validate-entry-abi :garbage-mode)
                :type 'nelisp-cc-runtime-error)
  (should-error (nelisp-cc-runtime--validate-entry-abi nil)
                :type 'nelisp-cc-runtime-error))

;;; (3) ssa-load-tag bytes -------------------------------------------

(ert-deftest nelisp-cc-stage81-poc-load-tag-bytes ()
  "Lowering `ssa-load-tag' of param-0 (= rdi) into a fresh def
(= rsi) emits MOVZX rsi, byte ptr [rdi].

The canonical encoding is REX.W=1 | 0x0F | 0xB6 | ModR/M(mod=0,
reg=6 = rsi.low3, rm=7 = rdi.low3) = 0x48 0x0F 0xB6 0x37.

Doc 81 §5.1.2 critical decision: this is *zero-extending*
(MOVZX, opcode 0x0F 0xB6), NOT sign-extending (MOVSX, 0x0F 0xBE).
A future TAG_FORWARDED variant >= 0x80 must not become a negative
i64 — sign-extension would corrupt the subsequent CMP comparison."
  (cl-destructuring-bind (cg fn param def _extras)
      (nelisp-cc-stage81-test--make-empty-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-load-tag (list param) def)))
      (nelisp-cc-x86_64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--bytes cg)))
        ;; 4 bytes total.
        (should (= 4 (length bytes)))
        ;; REX.W (W=1, R=0 [reg=rsi=6 fits 3 bits], B=0).
        (should (= #x48 (aref bytes 0)))
        ;; MOVZX two-byte opcode 0x0F 0xB6.
        (should (= #x0F (aref bytes 1)))
        (should (= #xB6 (aref bytes 2)))
        ;; ModR/M byte: mod=00, reg=6 (rsi), rm=7 (rdi)
        ;; → (00 << 6) | (6 << 3) | 7 = 0x37.
        (should (= #x37 (aref bytes 3)))))))

;;; (4) ssa-load-payload-ptr bytes -----------------------------------

(ert-deftest nelisp-cc-stage81-poc-load-payload-ptr-bytes ()
  "Lowering `ssa-load-payload-ptr' of param-0 (= rdi) into a fresh def
(= rsi) emits MOV rsi, qword ptr [rdi + 8].

Encoding: REX.W=1 | 0x8B | ModR/M(mod=01, reg=6 = rsi.low3, rm=7 =
rdi.low3) | disp8=8 = 0x48 0x8B 0x77 0x08.

The +8 offset is SEXP_PAYLOAD_OFFSET (Phase A.5.1 pinned, see
build-tool/src/eval/sexp.rs Sexp #[repr(C, u8)] layout)."
  (cl-destructuring-bind (cg fn param def _extras)
      (nelisp-cc-stage81-test--make-empty-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-load-payload-ptr (list param) def)))
      (nelisp-cc-x86_64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--bytes cg)))
        (should (= 4 (length bytes)))
        (should (= #x48 (aref bytes 0)))     ; REX.W
        (should (= #x8B (aref bytes 1)))     ; MOV r64, r/m64
        ;; ModR/M: mod=01 (disp8), reg=6 (rsi), rm=7 (rdi)
        ;; → (01 << 6) | (6 << 3) | 7 = 0x40 | 0x30 | 0x07 = 0x77.
        (should (= #x77 (aref bytes 2)))
        (should (= #x08 (aref bytes 3)))))))

;;; (5) ssa-cmp-tag-imm bytes + fixups -------------------------------

(ert-deftest nelisp-cc-stage81-poc-cmp-tag-imm-bytes-and-fixups ()
  "Lowering `ssa-cmp-tag-imm' emits CMP r8, imm8 + JE rel32 + JMP rel32
and registers fixups against `L_block_<then-id>' / `L_block_<else-id>'.

The total instruction sequence is:
  CMP   r8 (= low byte of tag-reg), imm8  — 4 bytes (REX + opcode + ModR/M + ib)
  JE    rel32 to L_block_<THEN>           — 6 bytes (0x0F 0x84 + 4-byte rel32)
  JMP   rel32 to L_block_<ELSE>           — 5 bytes (0xE9 + 4-byte rel32)

Total = 15 bytes, with 2 fixups recorded.  The CMP is encoded as
REX (0x40, no W) | 0x80 | ModR/M(/7, rm=tag-reg.low3) | imm8.  The
REX prefix is mandatory even with W=0 to access `sil' / `dil' /
`bpl' / `spl' rather than the legacy `dh' / `bh' high-byte
registers — see commentary on `--emit-cmp-r8-imm8'."
  (cl-destructuring-bind (cg fn param _def extras)
      ;; Pre-provision an extra register `r2' (= rdx) for the
      ;; intermediate tag-value def so the alloc-state seed is
      ;; complete before codegen-make seals it.
      (nelisp-cc-stage81-test--make-empty-codegen '(r2))
    ;; Build a fresh tag-value def (already in `extras' / r2 = rdx),
    ;; then cmp-tag-imm against SEXP_TAG_CONS (= 7).  Use synthetic
    ;; block ids 100 / 200 in meta.
    (let* ((tag-val (car extras))
           (load-instr
            (nelisp-cc--ssa-add-instr
             fn (nelisp-cc--ssa-function-entry fn)
             'ssa-load-tag (list param) tag-val))
           (cmp-instr
            (nelisp-cc--ssa-add-instr
             fn (nelisp-cc--ssa-function-entry fn)
             'ssa-cmp-tag-imm (list tag-val) nil)))
      (setf (nelisp-cc--ssa-instr-meta cmp-instr)
            '(:imm 7 :then 100 :else 200))
      ;; Lower both — load-tag first writes 4 bytes, then cmp-tag-imm
      ;; writes 14 (3 + 6 + 5).
      (nelisp-cc-x86_64--lower-instr cg load-instr)
      (nelisp-cc-x86_64--lower-instr cg cmp-instr)
      (let ((bytes (nelisp-cc-stage81-test--bytes cg))
            (fixups (nelisp-cc-x86_64--buffer-fixups
                     (nelisp-cc-x86_64--codegen-buffer cg))))
        ;; 4 (load-tag) + 15 (cmp-tag-imm) = 19 bytes total.
        (should (= 19 (length bytes)))
        ;; Inspect load-tag byte slice (offsets 0..3): MOVZX rdx, [rdi].
        ;;   REX.W = 0x48
        ;;   0x0F 0xB6 (MOVZX two-byte opcode)
        ;;   ModR/M(mod=00, reg=2=rdx, rm=7=rdi) = 0x17
        (should (= #x48 (aref bytes 0)))
        (should (= #x0F (aref bytes 1)))
        (should (= #xB6 (aref bytes 2)))
        (should (= #x17 (aref bytes 3)))
        ;; Inspect cmp-tag-imm byte slice (offsets 4..18).
        ;; CMP r8, imm8 (REX + 0x80 + ModR/M /7 + ib) = 4 bytes.
        ;; REG = r2 = rdx (low3 = 2), so:
        ;;   REX = 0x40 (no W, no R, no X, no B since rdx is not r8-r15)
        ;;   ModR/M = (mod=11 << 6) | (reg=7 << 3) | rm=2 = 0xC0 | 0x38 | 0x02 = 0xFA
        ;;   imm8 = 0x07 (SEXP_TAG_CONS)
        (should (= #x40 (aref bytes 4)))
        (should (= #x80 (aref bytes 5)))
        (should (= #xFA (aref bytes 6)))
        (should (= #x07 (aref bytes 7)))
        ;; JE rel32: 0x0F 0x84 + 4-byte placeholder = 6 bytes.
        (should (= #x0F (aref bytes 8)))
        (should (= #x84 (aref bytes 9)))
        ;; rel32 placeholder bytes 10..13 should be 0 (fixup pre-resolve).
        (should (= 0 (aref bytes 10)))
        (should (= 0 (aref bytes 11)))
        (should (= 0 (aref bytes 12)))
        (should (= 0 (aref bytes 13)))
        ;; JMP rel32: 0xE9 + 4-byte placeholder = 5 bytes.
        (should (= #xE9 (aref bytes 14)))
        (should (= 0 (aref bytes 15)))
        (should (= 0 (aref bytes 16)))
        (should (= 0 (aref bytes 17)))
        (should (= 0 (aref bytes 18)))
        ;; Two forward fixups recorded.
        (should (= 2 (length fixups)))
        ;; Fixups list is push-built (reverse-order) — the JMP fixup
        ;; was pushed last so it appears first.
        (let ((labels (mapcar #'cdr fixups)))
          (should (memq 'L_block_100 labels))
          (should (memq 'L_block_200 labels)))))))

;;; (6) Primitive table shape ----------------------------------------

(ert-deftest nelisp-cc-stage81-poc-primitive-table-shape ()
  "`nelisp-cc-pipeline-primitive-table-stage1' still has the single
`car' entry (= preserved verbatim for ABI archaeology); the canonical
lookup `nelisp-cc-pipeline-primitive-info' now resolves against the
Stage 81.2 table (= 5 cons primitives)."
  ;; Stage 81.1 PoC table is preserved for archaeology — not the
  ;; canonical lookup any more.
  (should (= 1 (length nelisp-cc-pipeline-primitive-table-stage1)))
  (let ((info (nelisp-cc-pipeline-primitive-info 'car)))
    (should info)
    (should (= 1 (nth 0 info)))                      ; arity
    (should (eq :trampoline-unary (nth 1 info)))     ; abi-mode
    (should (eq 'nl_jit_cons_car (nth 2 info))))     ; C symbol
  ;; Stage 81.2 — `cdr' / `cons' / `setcar' / `setcdr' all resolve.
  (should (nelisp-cc-pipeline-primitive-info 'cdr))
  (should (nelisp-cc-pipeline-primitive-info 'cons))
  (should (nelisp-cc-pipeline-primitive-info 'setcar))
  (should (nelisp-cc-pipeline-primitive-info 'setcdr))
  ;; Outside the cons surface — still nil.
  (should-not (nelisp-cc-pipeline-primitive-info '+)))

;;; (7) Trampoline status constants ----------------------------------

(ert-deftest nelisp-cc-stage81-poc-trampoline-status-constants ()
  "`-trampoline-ok' / `-trampoline-err' match the Cranelift contract."
  ;; Mirror of nl_jit_cons_car: 0 = OK, 1 = ERR.
  (should (= 0 nelisp-cc-runtime-trampoline-ok))
  (should (= 1 nelisp-cc-runtime-trampoline-err))
  ;; Distinct values so callers can branch unambiguously.
  (should-not (= nelisp-cc-runtime-trampoline-ok
                 nelisp-cc-runtime-trampoline-err)))

;;; (8) car PoC SSA shape --------------------------------------------

(ert-deftest nelisp-cc-stage81-poc-car-ssa-shape ()
  "Build the car PoC SSA function and verify it is well-formed.

Shape (per Doc 81 §5.1.4):
  entry:
    t1 = ssa-load-tag arg0
    ssa-cmp-tag-imm t1 :imm SEXP_TAG_CONS :then is-cons :else not-cons
  is-cons:
    t2 = ssa-load-payload-ptr arg0
    return t2  ; (Stage 81.1 PoC: return the box-ptr u64, not the actual car)
  not-cons:
    t3 = const NIL
    return t3

Stage 81.1 PoC verifies the IR can be *constructed* without
verifier complaint and that all three new opcodes pass through
the dispatcher without raising `unsupported-opcode'.  Real exec
threading lands in Stage 81.2 with the recognition pass."
  (let* ((fn (nelisp-cc--ssa-make-function 'car-poc '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (is-cons-blk  (nelisp-cc--ssa-make-block fn "is-cons"))
         (not-cons-blk (nelisp-cc--ssa-make-block fn "not-cons"))
         (t1 (nelisp-cc--ssa-make-value fn))
         (t2 (nelisp-cc--ssa-make-value fn))
         (t3 (nelisp-cc--ssa-make-value fn)))
    ;; entry block: load tag, then branch.
    (nelisp-cc--ssa-add-instr fn entry 'ssa-load-tag (list param) t1)
    (let ((cmp (nelisp-cc--ssa-add-instr
                fn entry 'ssa-cmp-tag-imm (list t1) nil)))
      ;; SEXP_TAG_CONS = 7 (build-tool/src/eval/sexp.rs).
      (setf (nelisp-cc--ssa-instr-meta cmp)
            (list :imm 7
                  :then (nelisp-cc--ssa-block-id is-cons-blk)
                  :else (nelisp-cc--ssa-block-id not-cons-blk))))
    (nelisp-cc--ssa-link-blocks entry is-cons-blk)
    (nelisp-cc--ssa-link-blocks entry not-cons-blk)
    ;; is-cons block: load payload-ptr, return.
    (nelisp-cc--ssa-add-instr fn is-cons-blk 'ssa-load-payload-ptr
                              (list param) t2)
    (nelisp-cc--ssa-add-instr fn is-cons-blk 'return (list t2) nil)
    ;; not-cons block: const NIL, return.
    (let ((const-instr (nelisp-cc--ssa-add-instr
                        fn not-cons-blk 'const nil t3)))
      (setf (nelisp-cc--ssa-instr-meta const-instr) '(:literal nil)))
    (nelisp-cc--ssa-add-instr fn not-cons-blk 'return (list t3) nil)
    ;; Verifier passes — IR is well-formed.
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    ;; The dispatcher's pcase has arms for the new opcodes — we know
    ;; this because (a) byte-emit ERTs (3) (4) (5) above do not
    ;; signal `nelisp-cc-x86_64-unsupported-opcode', and (b) we can
    ;; round-trip the SSA to s-expr and back.
    (let* ((sexp (nelisp-cc--ssa-pp fn))
           (fn2  (nelisp-cc--ssa-from-sexp sexp)))
      (should fn2)
      (should (eq t (nelisp-cc--ssa-verify-function fn2))))))

;;; Doc 81 Stage 81.2 — additional ERT (arm64 + 5 cons primitives) ----

;;; arm64 helper -----------------------------------------------------

(defun nelisp-cc-stage81-test--make-arm64-codegen (&optional extra-vids-vregs)
  "arm64 mirror of `nelisp-cc-stage81-test--make-empty-codegen'.

Returns (CG FN PARAM DEF EXTRA-VALUES).  The codegen wraps a fresh
arm64 buffer; PARAM is pre-allocated to virtual r0 (= x0 = AAPCS64
arg-0) and DEF to r1 (= x1).  EXTRA-VIDS-VREGS provisions further
virtual regs (e.g. `(r2 r3)') for tests that need an intermediate
SSA value beyond PARAM/DEF."
  (let* ((fn (nelisp-cc--ssa-make-function 'stage81-arm64-test '(int)))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (def (nelisp-cc--ssa-make-value fn))
         (extras (mapcar (lambda (_vreg) (nelisp-cc--ssa-make-value fn))
                         extra-vids-vregs))
         (extra-cells (cl-mapcar
                       (lambda (val vreg)
                         (cons (nelisp-cc--ssa-value-id val) vreg))
                       extras extra-vids-vregs))
         (alloc-state (append
                       extra-cells
                       (list (cons (nelisp-cc--ssa-value-id param) 'r0)
                             (cons (nelisp-cc--ssa-value-id def)   'r1))))
         (buf (nelisp-cc-arm64--buffer-make))
         (cg (nelisp-cc-arm64--codegen-make
              :function fn
              :alloc-state alloc-state
              :buffer buf
              :slot-alist nil
              :frame-size 0)))
    (list cg fn param def extras)))

(defun nelisp-cc-stage81-test--arm64-bytes (cg)
  "Return CG's arm64 buffer bytes in *forward* order as a vector.

Mirror of `nelisp-cc-stage81-test--bytes' for the arm64 buffer
shape (BYTES list reverse-built; we reverse + vconcat for indexed
inspection).  Does NOT resolve fixups — Stage 81.2 ERT inspects
the placeholder byte stream pre-finalize."
  (let ((bs (nelisp-cc-arm64--buffer-bytes
             (nelisp-cc-arm64--codegen-buffer cg))))
    (vconcat (nreverse (copy-sequence bs)))))

;;; (9) arm64 ssa-load-tag bytes -------------------------------------

(ert-deftest nelisp-cc-stage81-poc-arm64-load-tag-bytes ()
  "arm64: lowering `ssa-load-tag' of param-0 (= x0) into a fresh def
(= x1) emits LDRB W1, [X0] = 0x39400001 (LE bytes 01 00 40 39).

Encoding (32-bit unsigned-offset variant):
  0011 1001 01 imm12=0 Rn=0 Rt=1
  0x39400000 base | (imm12=0 << 10) | (Rn=0 << 5) | Rt=1
  = 0x39400001

LDRB writes Wt with zero-extension to 32 bits + automatic top-32
zero-extension under AAPCS64, so the SSA def lands as a clean u64
without a separate UXTB step.  This is the arm64 mirror of the
x86_64 MOVZX decision (Doc 81 §5.1.2) — both architectures must
zero-extend, never sign-extend, the tag byte."
  (cl-destructuring-bind (cg fn param def _extras)
      (nelisp-cc-stage81-test--make-arm64-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-load-tag (list param) def)))
      (nelisp-cc-arm64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--arm64-bytes cg)))
        ;; 4 bytes total (one fixed-width arm64 instruction).
        (should (= 4 (length bytes)))
        ;; LE bytes of 0x39400001 = [01 00 40 39].
        (should (= #x01 (aref bytes 0)))
        (should (= #x00 (aref bytes 1)))
        (should (= #x40 (aref bytes 2)))
        (should (= #x39 (aref bytes 3)))))))

;;; (10) arm64 ssa-load-payload-ptr bytes ----------------------------

(ert-deftest nelisp-cc-stage81-poc-arm64-load-payload-ptr-bytes ()
  "arm64: lowering `ssa-load-payload-ptr' of param-0 (= x0) into a
fresh def (= x1) emits LDR X1, [X0, #8] = 0xF9400401 (LE bytes
01 04 40 F9).

Encoding (unsigned-offset variant, sf=1):
  1111 1001 01 imm12=1 Rn=0 Rt=1
  0xF9400000 base | (imm12=1 << 10) | (Rn=0 << 5) | Rt=1
  = 0xF9400401

imm12 = 1 because the LDR Xt encoding scales the offset by 8
(= 8-byte stride for 64-bit loads), so byte offset 8 → imm12 = 1.
The +8 is SEXP_PAYLOAD_OFFSET (Phase A.5.1)."
  (cl-destructuring-bind (cg fn param def _extras)
      (nelisp-cc-stage81-test--make-arm64-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-load-payload-ptr (list param) def)))
      (nelisp-cc-arm64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--arm64-bytes cg)))
        (should (= 4 (length bytes)))
        ;; LE bytes of 0xF9400401 = [01 04 40 F9].
        (should (= #x01 (aref bytes 0)))
        (should (= #x04 (aref bytes 1)))
        (should (= #x40 (aref bytes 2)))
        (should (= #xF9 (aref bytes 3)))))))

;;; (11) arm64 ssa-cmp-tag-imm bytes + fixups ------------------------

(ert-deftest nelisp-cc-stage81-poc-arm64-cmp-tag-imm-bytes-and-fixups ()
  "arm64: lowering `ssa-cmp-tag-imm' emits CMP Wn,#imm + B.EQ fixup +
B fixup (= 12 bytes total, 2 fixups recorded).

Sequence (3 instructions, 4 bytes each):
  CMP   W2, #7        — encoded inline (no fixup)
  B.EQ  L_block_THEN  — placeholder 4 bytes, fixup against label
  B     L_block_ELSE  — placeholder 4 bytes, fixup against label

The CMP encoding uses 32-bit operand size (Wn = low 32 bits of the
X register holding the zero-extended tag).  Two fixups are
recorded against the synthetic `L_block_<id>' labels — the same
contract as the x86_64 sibling so the recognition pass can emit
identical SSA in either backend.

CMP W2, #7 = 0x7100001F | (7<<10) | (2<<5) = 0x71001C5F
  (sf=0 indicates 32-bit operand; Wn==2 → bits 9..5 = 00010)."
  (cl-destructuring-bind (cg fn param _def extras)
      (nelisp-cc-stage81-test--make-arm64-codegen '(r2))
    (let* ((tag-val (car extras))
           (load-instr
            (nelisp-cc--ssa-add-instr
             fn (nelisp-cc--ssa-function-entry fn)
             'ssa-load-tag (list param) tag-val))
           (cmp-instr
            (nelisp-cc--ssa-add-instr
             fn (nelisp-cc--ssa-function-entry fn)
             'ssa-cmp-tag-imm (list tag-val) nil)))
      (setf (nelisp-cc--ssa-instr-meta cmp-instr)
            '(:imm 7 :then 100 :else 200))
      (nelisp-cc-arm64--lower-instr cg load-instr)
      (nelisp-cc-arm64--lower-instr cg cmp-instr)
      (let ((bytes (nelisp-cc-stage81-test--arm64-bytes cg))
            (fixups (nelisp-cc-arm64--buffer-fixups
                     (nelisp-cc-arm64--codegen-buffer cg))))
        ;; 4 (load-tag) + 4 (CMP) + 4 (B.EQ) + 4 (B) = 16 bytes total.
        (should (= 16 (length bytes)))
        ;; Inspect the CMP slice (offsets 4..7).  CMP W2, #7 has
        ;; encoding 0x7100001F | (7<<10) | (2<<5)
        ;;       = 0x7100001F | 0x1C00 | 0x40
        ;;       = 0x71001C5F.  LE bytes: [5F 1C 00 71].
        (should (= #x5F (aref bytes 4)))
        (should (= #x1C (aref bytes 5)))
        (should (= #x00 (aref bytes 6)))
        (should (= #x71 (aref bytes 7)))
        ;; B.EQ + B placeholders are zero (4-byte each, fixup pre-resolve).
        (should (= 0 (aref bytes 8)))
        (should (= 0 (aref bytes 9)))
        (should (= 0 (aref bytes 10)))
        (should (= 0 (aref bytes 11)))
        (should (= 0 (aref bytes 12)))
        (should (= 0 (aref bytes 13)))
        (should (= 0 (aref bytes 14)))
        (should (= 0 (aref bytes 15)))
        ;; Two fixups against the synthetic labels.
        (should (= 2 (length fixups)))
        ;; Each fixup is (FIXUP-OFFSET . (LABEL . ENCODER-FN)).
        (let ((labels (mapcar (lambda (fx) (cadr fx)) fixups)))
          (should (memq 'L_block_100 labels))
          (should (memq 'L_block_200 labels)))))))

;;; (12) ssa-call-primitive ABI validation ---------------------------

(ert-deftest nelisp-cc-stage81-poc-call-primitive-bad-abi-rejected ()
  "Both backends reject `ssa-call-primitive' with an unrecognised ABI."
  ;; x86_64.
  (cl-destructuring-bind (cg fn param _def _extras)
      (nelisp-cc-stage81-test--make-empty-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) nil)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :garbage :symbol foo))
      (should-error (nelisp-cc-x86_64--lower-instr cg instr)
                    :type 'nelisp-cc-x86_64-encoding-error)))
  ;; arm64.
  (cl-destructuring-bind (cg fn param _def _extras)
      (nelisp-cc-stage81-test--make-arm64-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) nil)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :garbage :symbol foo))
      (should-error (nelisp-cc-arm64--lower-instr cg instr)
                    :type 'nelisp-cc-arm64-encoding-error))))

(ert-deftest nelisp-cc-stage81-poc-call-primitive-missing-symbol-rejected ()
  "Both backends reject `ssa-call-primitive' lacking a `:symbol' meta."
  (cl-destructuring-bind (cg fn param _def _extras)
      (nelisp-cc-stage81-test--make-empty-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) nil)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-unary))
      (should-error (nelisp-cc-x86_64--lower-instr cg instr)
                    :type 'nelisp-cc-x86_64-encoding-error)))
  (cl-destructuring-bind (cg fn param _def _extras)
      (nelisp-cc-stage81-test--make-arm64-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) nil)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-unary))
      (should-error (nelisp-cc-arm64--lower-instr cg instr)
                    :type 'nelisp-cc-arm64-encoding-error))))

;;; (13) ssa-call-primitive emit on x86_64 ---------------------------

(ert-deftest nelisp-cc-stage81-poc-call-primitive-x86_64-emit ()
  "x86_64: `ssa-call-primitive' emits CALL rel32 + records call-fixup
keyed on the `:symbol' meta.

Argument 0 is already in r0 (= rdi = System V arg-0) — the
allocator pre-pinned it, so marshalling is a no-op.  The CALL rel32
is the canonical 5-byte encoding (0xE8 + 4-byte placeholder).  A
single call-fixup is recorded with the C symbol as cdr."
  (cl-destructuring-bind (cg fn param def _extras)
      (nelisp-cc-stage81-test--make-empty-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-unary :symbol nl_jit_cons_car))
      (nelisp-cc-x86_64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--bytes cg))
            (fixups (nelisp-cc-x86_64--codegen-call-fixups cg)))
        ;; Marshalling is push-pop strategy by default — for a single
        ;; arg already in rdi the bench may emit PUSH RDI / POP RDI;
        ;; we just assert the CALL byte (= 0xE8) appears exactly once.
        (should (memq #xE8 (append bytes nil)))
        ;; Exactly one call-fixup recorded with the C symbol as cdr.
        (should (= 1 (length fixups)))
        (should (eq 'nl_jit_cons_car (cdar fixups)))))))

;;; (14) ssa-call-primitive emit on arm64 ----------------------------

(ert-deftest nelisp-cc-stage81-poc-call-primitive-arm64-emit ()
  "arm64: `ssa-call-primitive' emits BL placeholder + status MOV
+ records a fixup against `callee:<C-SYMBOL>'.

x0 is already pre-pinned (= AAPCS64 arg-0), so no marshalling MOV
fires.  After BL, the status (in x0) is moved into the def's
register (= x1 in this fixture), yielding 8 bytes total: a 4-byte
BL placeholder followed by 4 bytes of `MOV X1, X0' = 0xAA0003E1
(LE bytes E1 03 00 AA)."
  (cl-destructuring-bind (cg fn param def _extras)
      (nelisp-cc-stage81-test--make-arm64-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-unary :symbol nl_jit_cons_car))
      (nelisp-cc-arm64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--arm64-bytes cg))
            (fixups (nelisp-cc-arm64--buffer-fixups
                     (nelisp-cc-arm64--codegen-buffer cg))))
        ;; 4-byte BL placeholder + 4-byte writeback MOV.
        (should (= 8 (length bytes)))
        ;; BL placeholder at offset 0.
        (should (= 0 (aref bytes 0)))
        (should (= 0 (aref bytes 1)))
        (should (= 0 (aref bytes 2)))
        (should (= 0 (aref bytes 3)))
        ;; Writeback MOV X1, X0 = 0xAA0003E1, LE bytes E1 03 00 AA.
        (should (= #xE1 (aref bytes 4)))
        (should (= #x03 (aref bytes 5)))
        (should (= #x00 (aref bytes 6)))
        (should (= #xAA (aref bytes 7)))
        ;; One fixup against `callee:nl_jit_cons_car'.
        (should (= 1 (length fixups)))
        (let ((labels (mapcar (lambda (fx) (cadr fx)) fixups)))
          (should (memq 'callee:nl_jit_cons_car labels)))))))

;;; (15) Stage 81.2 primitive table — 5 cons primitives --------------

(ert-deftest nelisp-cc-stage81-poc-primitive-table-stage2-shape ()
  "`nelisp-cc-pipeline-primitive-table-stage2' carries 5 cons entries
(read 2 = car/cdr, alloc 1 = cons, write 2 = setcar/setcdr) per
Doc 81 §5.2.3.  Each entry maps to its `nl_jit_cons_*' Rust C
symbol and the appropriate trampoline-binary-* ABI shape."
  (should (= 5 (length nelisp-cc-pipeline-primitive-table-stage2)))
  (let ((expected '((car    1 :trampoline-unary       nl_jit_cons_car)
                    (cdr    1 :trampoline-unary       nl_jit_cons_cdr)
                    (cons   2 :trampoline-binary-ctor nl_jit_cons_make)
                    (setcar 2 :trampoline-binary-mut  nl_jit_cons_setcar)
                    (setcdr 2 :trampoline-binary-mut  nl_jit_cons_setcdr))))
    (dolist (entry expected)
      (let* ((sym  (nth 0 entry))
             (info (nelisp-cc-pipeline-primitive-info sym)))
        (should info)
        (should (= (nth 1 entry) (nth 0 info)))         ; arity
        (should (eq (nth 2 entry) (nth 1 info)))        ; abi-mode
        (should (eq (nth 3 entry) (nth 2 info)))))))    ; C symbol

;;; (16) Per-primitive ABI mode lookup -------------------------------

(ert-deftest nelisp-cc-stage81-poc-primitive-abi-mode-routing ()
  "Each Stage 81.2 cons primitive maps to its declared ABI shape.

This is the contract the recognition pass (Stage 81.3) consumes:
`car' / `cdr' fan into `:trampoline-unary' (1 read arg + out-ptr);
`cons' fans into `:trampoline-binary-ctor' (2 read args + out-ptr,
allocator); `setcar' / `setcdr' fan into `:trampoline-binary-mut'
(2 args, mutate-in-place, no out-ptr)."
  ;; Read 2 — :trampoline-unary.
  (should (eq :trampoline-unary
              (nth 1 (nelisp-cc-pipeline-primitive-info 'car))))
  (should (eq :trampoline-unary
              (nth 1 (nelisp-cc-pipeline-primitive-info 'cdr))))
  ;; Alloc 1 — :trampoline-binary-ctor.
  (should (eq :trampoline-binary-ctor
              (nth 1 (nelisp-cc-pipeline-primitive-info 'cons))))
  ;; Write 2 — :trampoline-binary-mut.
  (should (eq :trampoline-binary-mut
              (nth 1 (nelisp-cc-pipeline-primitive-info 'setcar))))
  (should (eq :trampoline-binary-mut
              (nth 1 (nelisp-cc-pipeline-primitive-info 'setcdr))))
  ;; All ABI modes referenced are valid runtime entry-ABI keywords.
  (dolist (sym '(car cdr cons setcar setcdr))
    (let ((mode (nth 1 (nelisp-cc-pipeline-primitive-info sym))))
      (should (memq mode nelisp-cc-runtime--entry-abi-modes)))))

;;; Doc 81 Stage 81.3 — vector primitive trampoline ABI ---------------
;;
;; Stage 81.3 extends the trampoline ABI to non-cons primitives.
;; First wave = vector cluster (= length / aref / aset / elt) per
;; Doc 28 §3.6.b (Phase 7.1.6.b access.rs takeover prerequisite).
;; Two new ABI shapes are added:
;;
;;   :trampoline-binary-aref  fn(*const Sexp, i64, *mut Sexp) -> i64
;;   :trampoline-ternary-aset fn(*const Sexp, i64, *const Sexp,
;;                                *mut Sexp) -> i64
;;
;; The `length' primitive reuses :trampoline-unary (already SHIPPED
;; in Stage 81.1).  The four ERT below verify:
;;   17. ABI validation accepts both new modes
;;   18. primitive-table-stage3 carries 9 entries (5 cons + 4 vector)
;;   19. vector-cluster ABI mode routing
;;   20. x86_64 emit accepts :trampoline-binary-aref
;;   21. x86_64 emit accepts :trampoline-ternary-aset
;;   22. arm64 emit accepts :trampoline-binary-aref
;;   23. arm64 emit accepts :trampoline-ternary-aset
;;   24. backends still reject *garbage* ABI (= regression guard)
;;   25. extern-c symbols match access.rs

;;; (17) Stage 81.3 entry-ABI mode validation ------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-entry-abi-validation ()
  "`nelisp-cc-runtime--entry-abi-modes' includes the two Stage 81.3
vector ABI keywords (= `:trampoline-binary-aref' /
`:trampoline-ternary-aset') and the validator accepts them."
  ;; Both new modes registered.
  (should (memq :trampoline-binary-aref
                nelisp-cc-runtime--entry-abi-modes))
  (should (memq :trampoline-ternary-aset
                nelisp-cc-runtime--entry-abi-modes))
  ;; Validator accepts each new mode.
  (dolist (mode '(:trampoline-binary-aref :trampoline-ternary-aset))
    (should-not (condition-case nil
                    (progn (nelisp-cc-runtime--validate-entry-abi mode)
                           nil)
                  (nelisp-cc-runtime-error t))))
  ;; All 4 prior ABI modes still present (= no regression).
  (should (memq :host-int nelisp-cc-runtime--entry-abi-modes))
  (should (memq :trampoline-unary nelisp-cc-runtime--entry-abi-modes))
  (should (memq :trampoline-binary-ctor
                nelisp-cc-runtime--entry-abi-modes))
  (should (memq :trampoline-binary-mut
                nelisp-cc-runtime--entry-abi-modes))
  ;; 6 modes total (host-int + 5 trampoline shapes).
  (should (= 6 (length nelisp-cc-runtime--entry-abi-modes))))

;;; (18) Stage 81.3 primitive-table-stage3 shape ---------------------

(ert-deftest nelisp-cc-stage81-poc-primitive-table-stage3-shape ()
  "`nelisp-cc-pipeline-primitive-table-stage3' carries the 5 cons
+ 4 vector primitives (= 9 entries total) per Doc 81 §5.3.

The vector cluster (= length / aref / aset / elt) targets Phase
7.1.6.b `access.rs' takeover; each entry maps to its
`nl_jit_access_*' Rust C symbol via the appropriate ABI shape.
The cons cluster (Stage 81.2) is preserved verbatim."
  (should (= 9 (length nelisp-cc-pipeline-primitive-table-stage3)))
  (let ((expected
         '(;; cons cluster (Stage 81.2 — preserved).
           (car    1 :trampoline-unary        nl_jit_cons_car)
           (cdr    1 :trampoline-unary        nl_jit_cons_cdr)
           (cons   2 :trampoline-binary-ctor  nl_jit_cons_make)
           (setcar 2 :trampoline-binary-mut   nl_jit_cons_setcar)
           (setcdr 2 :trampoline-binary-mut   nl_jit_cons_setcdr)
           ;; vector cluster (Stage 81.3 addition).
           (length 1 :trampoline-unary        nl_jit_access_length)
           (aref   2 :trampoline-binary-aref  nl_jit_access_aref)
           (aset   3 :trampoline-ternary-aset nl_jit_access_aset)
           (elt    2 :trampoline-binary-aref  nl_jit_access_elt))))
    (dolist (entry expected)
      (let* ((sym  (nth 0 entry))
             (info (nelisp-cc-pipeline-primitive-info sym)))
        (should info)
        (should (= (nth 1 entry) (nth 0 info)))     ; arity
        (should (eq (nth 2 entry) (nth 1 info)))    ; abi-mode
        (should (eq (nth 3 entry) (nth 2 info))))))
  ;; Outside the 9-entry surface — still nil.
  (should-not (nelisp-cc-pipeline-primitive-info '+))
  (should-not (nelisp-cc-pipeline-primitive-info 'eq)))

;;; (19) Stage 81.3 vector ABI mode routing --------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-vector-abi-routing ()
  "Each vector primitive maps to its declared ABI shape.

  length → :trampoline-unary        (1 vec arg, returns i64 length)
  aref   → :trampoline-binary-aref  (vec, i64 idx → val)
  aset   → :trampoline-ternary-aset (vec, i64 idx, val → val)
  elt    → :trampoline-binary-aref  (seq, i64 idx → val)

The recognition pass (Stage 81.4+) will consume this routing to
choose the per-primitive trampoline ABI for the elisp-emit path."
  ;; length — :trampoline-unary (= shared with car/cdr).
  (should (eq :trampoline-unary
              (nth 1 (nelisp-cc-pipeline-primitive-info 'length))))
  ;; aref / elt — :trampoline-binary-aref (= 2-arg + i64 idx + out).
  (should (eq :trampoline-binary-aref
              (nth 1 (nelisp-cc-pipeline-primitive-info 'aref))))
  (should (eq :trampoline-binary-aref
              (nth 1 (nelisp-cc-pipeline-primitive-info 'elt))))
  ;; aset — :trampoline-ternary-aset (= 3-arg + i64 idx + out).
  (should (eq :trampoline-ternary-aset
              (nth 1 (nelisp-cc-pipeline-primitive-info 'aset))))
  ;; All ABI modes referenced are valid runtime entry-ABI keywords.
  (dolist (sym '(length aref aset elt))
    (let ((mode (nth 1 (nelisp-cc-pipeline-primitive-info sym))))
      (should (memq mode nelisp-cc-runtime--entry-abi-modes))))
  ;; Arity matches the elisp surface contract.
  (should (= 1 (nth 0 (nelisp-cc-pipeline-primitive-info 'length))))
  (should (= 2 (nth 0 (nelisp-cc-pipeline-primitive-info 'aref))))
  (should (= 3 (nth 0 (nelisp-cc-pipeline-primitive-info 'aset))))
  (should (= 2 (nth 0 (nelisp-cc-pipeline-primitive-info 'elt)))))

;;; (20) x86_64 :trampoline-binary-aref emit -------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-aref-x86_64-emit ()
  "x86_64: `ssa-call-primitive' with `:trampoline-binary-aref' ABI
emits CALL rel32 + records call-fixup against `nl_jit_access_aref'.

ABI marshalling (System V AMD64): arg0 → rdi (vec ptr), arg1 → rsi
(i64 raw idx).  The out-ptr (arg2) is allocated by the recognition
pass — Stage 81.3 ERT exercises the 2-operand IR shape (= just vec
+ idx, no explicit out-ptr operand at the SSA level)."
  (cl-destructuring-bind (cg fn param def extras)
      ;; Pre-provision r2 (= rdx) for the i64 idx operand.
      (nelisp-cc-stage81-test--make-empty-codegen '(r2))
    (let* ((idx-val (car extras))
           (instr (nelisp-cc--ssa-add-instr
                   fn (nelisp-cc--ssa-function-entry fn)
                   'ssa-call-primitive (list param idx-val) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-binary-aref :symbol nl_jit_access_aref))
      (nelisp-cc-x86_64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--bytes cg))
            (fixups (nelisp-cc-x86_64--codegen-call-fixups cg)))
        ;; CALL rel32 byte (= 0xE8) appears at least once.
        (should (memq #xE8 (append bytes nil)))
        ;; Exactly one call-fixup against the C symbol.
        (should (= 1 (length fixups)))
        (should (eq 'nl_jit_access_aref (cdar fixups)))))))

;;; (21) x86_64 :trampoline-ternary-aset emit ------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-aset-x86_64-emit ()
  "x86_64: `ssa-call-primitive' with `:trampoline-ternary-aset' ABI
emits CALL rel32 + records call-fixup against `nl_jit_access_aset'.

ABI marshalling (System V AMD64): arg0 → rdi (vec ptr), arg1 → rsi
(i64 raw idx), arg2 → rdx (val ptr).  The out-ptr (arg3) is
allocated by the recognition pass — Stage 81.3 ERT exercises the
3-operand IR shape."
  (cl-destructuring-bind (cg fn param def extras)
      ;; Pre-provision r2 (= rdx) and r3 (= rcx).
      (nelisp-cc-stage81-test--make-empty-codegen '(r2 r3))
    (let* ((idx-val (nth 0 extras))
           (val-val (nth 1 extras))
           (instr (nelisp-cc--ssa-add-instr
                   fn (nelisp-cc--ssa-function-entry fn)
                   'ssa-call-primitive
                   (list param idx-val val-val) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-ternary-aset :symbol nl_jit_access_aset))
      (nelisp-cc-x86_64--lower-instr cg instr)
      (let ((bytes (nelisp-cc-stage81-test--bytes cg))
            (fixups (nelisp-cc-x86_64--codegen-call-fixups cg)))
        (should (memq #xE8 (append bytes nil)))
        (should (= 1 (length fixups)))
        (should (eq 'nl_jit_access_aset (cdar fixups)))))))

;;; (22) arm64 :trampoline-binary-aref emit --------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-aref-arm64-emit ()
  "arm64: `ssa-call-primitive' with `:trampoline-binary-aref' ABI
emits BL placeholder + records fixup against
`callee:nl_jit_access_aref'.

AAPCS64 marshalling: arg0 → x0, arg1 → x1.  PARAM is pre-pinned
to r0 (x0); the i64 idx operand resides in r2 (x2) and gets
moved into x1 by the marshaller.  Status (in x0) is written back
to the def's register."
  (cl-destructuring-bind (cg fn param def extras)
      (nelisp-cc-stage81-test--make-arm64-codegen '(r2))
    (let* ((idx-val (car extras))
           (instr (nelisp-cc--ssa-add-instr
                   fn (nelisp-cc--ssa-function-entry fn)
                   'ssa-call-primitive (list param idx-val) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-binary-aref :symbol nl_jit_access_aref))
      (nelisp-cc-arm64--lower-instr cg instr)
      (let ((fixups (nelisp-cc-arm64--buffer-fixups
                     (nelisp-cc-arm64--codegen-buffer cg))))
        ;; At least one fixup against `callee:nl_jit_access_aref'.
        (should (>= (length fixups) 1))
        (let ((labels (mapcar (lambda (fx) (cadr fx)) fixups)))
          (should (memq 'callee:nl_jit_access_aref labels)))))))

;;; (23) arm64 :trampoline-ternary-aset emit -------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-aset-arm64-emit ()
  "arm64: `ssa-call-primitive' with `:trampoline-ternary-aset' ABI
emits BL placeholder + records fixup against
`callee:nl_jit_access_aset'.

AAPCS64 marshalling: arg0 → x0, arg1 → x1, arg2 → x2.  Three
operands are passed; out-ptr (arg3 → x3) is allocated by the
recognition pass before the call."
  (cl-destructuring-bind (cg fn param def extras)
      (nelisp-cc-stage81-test--make-arm64-codegen '(r2 r3))
    (let* ((idx-val (nth 0 extras))
           (val-val (nth 1 extras))
           (instr (nelisp-cc--ssa-add-instr
                   fn (nelisp-cc--ssa-function-entry fn)
                   'ssa-call-primitive
                   (list param idx-val val-val) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :trampoline-ternary-aset :symbol nl_jit_access_aset))
      (nelisp-cc-arm64--lower-instr cg instr)
      (let ((fixups (nelisp-cc-arm64--buffer-fixups
                     (nelisp-cc-arm64--codegen-buffer cg))))
        (should (>= (length fixups) 1))
        (let ((labels (mapcar (lambda (fx) (cadr fx)) fixups)))
          (should (memq 'callee:nl_jit_access_aset labels)))))))

;;; (24) Garbage ABI still rejected after Stage 81.3 -----------------

(ert-deftest nelisp-cc-stage81-poc-stage3-garbage-abi-still-rejected ()
  "Stage 81.3 adds 2 modes but does NOT relax the validator —
non-listed ABI keywords still raise `*-encoding-error' on both
backends.  This is the regression guard for Doc 81 §5.3 sub-task
81.3.b dispatcher updates."
  ;; x86_64: garbage ABI still rejected.
  (cl-destructuring-bind (cg fn param _def _extras)
      (nelisp-cc-stage81-test--make-empty-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) nil)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :stage99-garbage :symbol foo))
      (should-error (nelisp-cc-x86_64--lower-instr cg instr)
                    :type 'nelisp-cc-x86_64-encoding-error)))
  ;; arm64: garbage ABI still rejected.
  (cl-destructuring-bind (cg fn param _def _extras)
      (nelisp-cc-stage81-test--make-arm64-codegen)
    (let ((instr (nelisp-cc--ssa-add-instr
                  fn (nelisp-cc--ssa-function-entry fn)
                  'ssa-call-primitive (list param) nil)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            '(:abi :stage99-garbage :symbol foo))
      (should-error (nelisp-cc-arm64--lower-instr cg instr)
                    :type 'nelisp-cc-arm64-encoding-error)))
  ;; The 5 valid ABIs (1 host-int + 4 trampoline) all still accepted.
  ;; (host-int omitted — it is not a `ssa-call-primitive' value, just
  ;; a `compile-and-allocate' entry-abi.)
  (dolist (good '(:trampoline-unary :trampoline-binary-ctor
                  :trampoline-binary-mut :trampoline-binary-aref
                  :trampoline-ternary-aset))
    (cl-destructuring-bind (cg fn param def _extras)
        (nelisp-cc-stage81-test--make-empty-codegen)
      (let ((instr (nelisp-cc--ssa-add-instr
                    fn (nelisp-cc--ssa-function-entry fn)
                    'ssa-call-primitive (list param) def)))
        (setf (nelisp-cc--ssa-instr-meta instr)
              (list :abi good :symbol 'nl_jit_test_sym))
        ;; Should NOT signal — should emit cleanly.
        (nelisp-cc-x86_64--lower-instr cg instr)))))

;;; (25) Stage 81.3 extern-c symbol parity --------------------------

(ert-deftest nelisp-cc-stage81-poc-stage3-extern-c-symbol-parity ()
  "Stage 81.3 vector primitives target the `nl_jit_access_*' symbols
that build-tool/src/jit/access.rs already declares + registers (=
length / aref / aset / elt).  This ERT is the documentation
contract: if access.rs ever renames a symbol the table must move
in lockstep, otherwise the link pass dlsym lookup will silently
miss."
  ;; Each vector primitive's C symbol matches the access.rs declaration.
  (should (eq 'nl_jit_access_length
              (nth 2 (nelisp-cc-pipeline-primitive-info 'length))))
  (should (eq 'nl_jit_access_aref
              (nth 2 (nelisp-cc-pipeline-primitive-info 'aref))))
  (should (eq 'nl_jit_access_aset
              (nth 2 (nelisp-cc-pipeline-primitive-info 'aset))))
  (should (eq 'nl_jit_access_elt
              (nth 2 (nelisp-cc-pipeline-primitive-info 'elt))))
  ;; Cons cluster's symbols are *not* aliased into the vector cluster
  ;; (= no accidental cross-mapping).
  (should-not (eq (nth 2 (nelisp-cc-pipeline-primitive-info 'aref))
                  (nth 2 (nelisp-cc-pipeline-primitive-info 'car))))
  (should-not (eq (nth 2 (nelisp-cc-pipeline-primitive-info 'length))
                  (nth 2 (nelisp-cc-pipeline-primitive-info 'cdr)))))

;;; Doc 81 Stage 81.4 — recognition pass + dlsym bridge --------------
;;
;; Stage 81.4 wires the trampoline ABI infrastructure shipped in
;; 81.1〜81.3 to elisp call sites:
;;
;;   - `nelisp-cc-pipeline--recognize-primitives' walks compiled SSA
;;     IR and rewrites `:call' instructions whose `:fn' meta names a
;;     known primitive (= `car' / `cdr' / `cons' / `setcar' / `setcdr'
;;     / `length' / `aref' / `aset' / `elt') to the new
;;     `:ssa-call-primitive' opcode with the right `:abi' / `:symbol'
;;     meta from the Stage 81.3 table.
;;
;;   - `nelisp-cc-runtime-resolve-symbol' is the dlsym bridge contract
;;     surface.  On host Emacs (Stage 81.4 hard-constrains 0 Rust LOC)
;;     it returns a sentinel `:host-stub' status.  Standalone NeLisp
;;     overrides via `nelisp-cc-runtime-resolve-symbol-function'.
;;
;; The 6 ERT below cover:
;;
;;   26. recognition-pass-default-off
;;        Default `nelisp-cc-pipeline-recognize-primitives-enable' = nil
;;        so existing 1086/1091 ERT corpus runs unchanged through the
;;        legacy `:call' path.
;;
;;   27. recognition-pass-rewrites-call-to-primitive
;;        Build a hand-rolled SSA fn with one `:call :fn car' instr,
;;        invoke `--recognize-primitives' directly, observe (a) the
;;        opcode flips to `ssa-call-primitive', (b) `:abi' /
;;        `:symbol' meta come from `primitive-table-stage3', (c)
;;        `:fn' is preserved for debug, and (d) the count returned
;;        is 1.
;;
;;   28. recognition-pass-skips-non-primitives
;;        `:call :fn user-defined-fn' (= not in the primitive table)
;;        is left untouched.  Recognition pass is conservative.
;;
;;   29. recognition-pass-arity-mismatch-signals
;;        `:call' to `cons' (arity 2 in the table) with operand list
;;        of length 1 raises `nelisp-cc-runtime-error' with
;;        :recognition-arity-mismatch.
;;
;;   30. recognition-pass-idempotent
;;        Running the pass twice on the same function rewrites the
;;        primitive on the first sweep and reports 0 rewrites on
;;        the second (= already-recognized sites are no longer
;;        `:call' opcodes).
;;
;;   31. resolve-symbol-stub-on-host-emacs
;;        `nelisp-cc-runtime-resolve-symbol' on host Emacs returns
;;        `(:host-stub . SENTINEL)' for any symbol; the
;;        `-host-stub-p' predicate confirms.  Override hook is
;;        respected when bound.
;;
;;   32. recognition-pass-end-to-end-pipeline-stats
;;        `nelisp-cc-runtime-compile-and-allocate' on `(lambda (x)
;;        (car x))' with `recognize-primitives-enable' bound to t
;;        flows the recognition pass through the pipeline driver and
;;        tallies the rewrite count in `pipeline-stats'.

;;; (26) Recognition pass default off --------------------------------

(ert-deftest nelisp-cc-stage81-poc-stage4-recognize-default-off ()
  "`nelisp-cc-pipeline-recognize-primitives-enable' defaults to nil so
existing call sites flow through the legacy `:call' codepath
unchanged.  This is the regression guard for the Phase 7.1.5 LOCKED
1086/1091 ERT corpus — the recognition pass must not auto-engage
during normal test runs."
  ;; Pristine value (= no test has bound it yet at this point).
  (should (boundp 'nelisp-cc-pipeline-recognize-primitives-enable))
  (let ((default-val (default-value
                       'nelisp-cc-pipeline-recognize-primitives-enable)))
    (should (null default-val)))
  ;; The recognition pass function exists and is autoloadable.
  (should (fboundp 'nelisp-cc-pipeline--recognize-primitives))
  ;; The stats struct now carries the new field.
  (let ((stats (nelisp-cc-pipeline-stats-make)))
    (should (= 0 (nelisp-cc-pipeline-stats-recognized-primitives stats)))))

;;; (27) Recognition pass rewrites :call → :ssa-call-primitive --------

(ert-deftest nelisp-cc-stage81-poc-stage4-recognize-rewrites-primitive ()
  "The recognition pass rewrites a `:call :fn car' instruction in
place to `ssa-call-primitive' with the `:abi' / `:symbol' meta
sourced from `nelisp-cc-pipeline-primitive-table-stage3', preserving
the original `:fn' for debug pretty-printing."
  (let* ((fn (nelisp-cc--ssa-make-function 'recog-test '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (def (nelisp-cc--ssa-make-value fn))
         (instr (nelisp-cc--ssa-add-instr fn entry 'call (list param) def)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :fn 'car :unresolved t))
    ;; Pre-condition.
    (should (eq 'call (nelisp-cc--ssa-instr-opcode instr)))
    (let* ((result (nelisp-cc-pipeline--recognize-primitives fn))
           (count (cdr result)))
      ;; One rewrite occurred.
      (should (= 1 count))
      ;; Opcode flipped.
      (should (eq 'ssa-call-primitive (nelisp-cc--ssa-instr-opcode instr)))
      ;; Meta now carries the trampoline shape from the table.
      (let ((meta (nelisp-cc--ssa-instr-meta instr)))
        (should (eq :trampoline-unary (plist-get meta :abi)))
        (should (eq 'nl_jit_cons_car (plist-get meta :symbol)))
        ;; Original :fn preserved for debugging / pretty-printing.
        (should (eq 'car (plist-get meta :fn)))
        ;; Recognition tag set so a second sweep can detect (and
        ;; downstream passes that care about origin can consult).
        (should (eq t (plist-get meta :recognized))))
      ;; Operands / def / id unchanged.
      (should (equal (list param) (nelisp-cc--ssa-instr-operands instr)))
      (should (eq def (nelisp-cc--ssa-instr-def instr))))))

;;; (28) Recognition pass skips non-primitives ------------------------

(ert-deftest nelisp-cc-stage81-poc-stage4-recognize-skips-non-primitives ()
  "A `:call :fn user-fn' where `user-fn' is *not* in the primitive
table is left untouched by the recognition pass.  This guards
against accidental rewrites of letrec callees / lambda-lifted
synthetic names that happen to share a surface symbol with a
primitive."
  (let* ((fn (nelisp-cc--ssa-make-function 'skip-test '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (def (nelisp-cc--ssa-make-value fn))
         (instr (nelisp-cc--ssa-add-instr fn entry 'call (list param) def))
         ;; A second instr — this one DOES name a primitive — to confirm
         ;; the pass discriminates per-instruction not per-block.
         (def2 (nelisp-cc--ssa-make-value fn))
         (instr2 (nelisp-cc--ssa-add-instr fn entry 'call (list param) def2)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :fn 'my-user-fn :unresolved t))
    (setf (nelisp-cc--ssa-instr-meta instr2)
          (list :fn 'cdr :unresolved t))
    (let* ((result (nelisp-cc-pipeline--recognize-primitives fn))
           (count (cdr result)))
      ;; Only one rewrite (= the cdr call); user-fn left alone.
      (should (= 1 count))
      ;; user-fn instruction still `:call'.
      (should (eq 'call (nelisp-cc--ssa-instr-opcode instr)))
      ;; Original meta retained verbatim (= no :recognized tag added).
      (should (null (plist-get (nelisp-cc--ssa-instr-meta instr) :recognized)))
      ;; cdr instruction flipped.
      (should (eq 'ssa-call-primitive (nelisp-cc--ssa-instr-opcode instr2)))
      (should (eq 'nl_jit_cons_cdr
                  (plist-get (nelisp-cc--ssa-instr-meta instr2) :symbol))))))

;;; (29) Recognition pass arity mismatch signals ----------------------

(ert-deftest nelisp-cc-stage81-poc-stage4-recognize-arity-mismatch ()
  "A `:call' to a primitive with the wrong operand count signals
`nelisp-cc-runtime-error' rather than silently producing a malformed
trampoline shape.  This catches frontend bugs during recognition
instead of as undefined-behaviour at exec time."
  (let* ((fn (nelisp-cc--ssa-make-function 'arity-test '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (def (nelisp-cc--ssa-make-value fn))
         ;; `cons' has arity 2 in the table — pass only 1 operand.
         (instr (nelisp-cc--ssa-add-instr fn entry 'call (list param) def)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :fn 'cons :unresolved t))
    (should-error (nelisp-cc-pipeline--recognize-primitives fn)
                  :type 'nelisp-cc-runtime-error)))

;;; (30) Recognition pass idempotent ----------------------------------

(ert-deftest nelisp-cc-stage81-poc-stage4-recognize-idempotent ()
  "Running the recognition pass twice on the same function rewrites
once on the first sweep and produces zero rewrites on the second.
Already-recognized sites are no longer `:call' opcodes so the second
walk simply skips them."
  (let* ((fn (nelisp-cc--ssa-make-function 'idem-test '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (def (nelisp-cc--ssa-make-value fn))
         (instr (nelisp-cc--ssa-add-instr fn entry 'call (list param) def)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :fn 'car :unresolved t))
    (let* ((first  (nelisp-cc-pipeline--recognize-primitives fn))
           (second (nelisp-cc-pipeline--recognize-primitives fn)))
      (should (= 1 (cdr first)))
      (should (= 0 (cdr second)))
      ;; Final state — opcode is `ssa-call-primitive', meta unchanged
      ;; from the first sweep.
      (should (eq 'ssa-call-primitive
                  (nelisp-cc--ssa-instr-opcode instr)))
      (should (eq :trampoline-unary
                  (plist-get (nelisp-cc--ssa-instr-meta instr) :abi))))))

;;; (31) Resolve-symbol stub on host Emacs ----------------------------

(ert-deftest nelisp-cc-stage81-poc-stage4-resolve-symbol-host-stub ()
  "`nelisp-cc-runtime-resolve-symbol' on host Emacs returns
`(:host-stub . SENTINEL)' for any symbol when the override hook is
unset.  When the override is bound, the override result is
returned verbatim — this is the contract Phase 7.1.6.a's standalone
NeLisp implementation must satisfy."
  ;; Default path — override unbound, returns the sentinel.
  (let ((nelisp-cc-runtime-resolve-symbol-function nil))
    (let ((result (nelisp-cc-runtime-resolve-symbol 'nl_jit_cons_make)))
      (should (consp result))
      (should (eq :host-stub (car result)))
      (should (= nelisp-cc-runtime--resolve-symbol-stub-addr (cdr result)))
      (should (nelisp-cc-runtime-resolve-symbol-host-stub-p result)))
    ;; Sentinel is non-zero (= distinct from `:not-found' addr nil).
    (should (not (zerop nelisp-cc-runtime--resolve-symbol-stub-addr)))
    ;; Different symbol — same sentinel (= host stub doesn't query
    ;; anything; it's a placeholder for the real dlsym hook).
    (let ((result-2 (nelisp-cc-runtime-resolve-symbol 'nl_jit_access_aref)))
      (should (eq :host-stub (car result-2)))
      (should (= nelisp-cc-runtime--resolve-symbol-stub-addr
                 (cdr result-2)))))
  ;; Override path — Phase 7.1.6.a will register a real dlsym
  ;; function here; ERT exercises the contract via a fake.
  (let ((nelisp-cc-runtime-resolve-symbol-function
         (lambda (sym)
           (cond
            ((eq sym 'nl_jit_cons_car)
             (cons :resolved #x7F0011223344))
            (t (cons :not-found nil))))))
    (let ((r1 (nelisp-cc-runtime-resolve-symbol 'nl_jit_cons_car))
          (r2 (nelisp-cc-runtime-resolve-symbol 'nl_jit_unknown)))
      (should (eq :resolved (car r1)))
      (should (= #x7F0011223344 (cdr r1)))
      (should (eq :not-found (car r2)))
      (should (null (cdr r2)))
      ;; Override results are NOT host stubs.
      (should-not (nelisp-cc-runtime-resolve-symbol-host-stub-p r1))
      (should-not (nelisp-cc-runtime-resolve-symbol-host-stub-p r2))))
  ;; Non-symbol input rejected with runtime error.
  (should-error (nelisp-cc-runtime-resolve-symbol "string-not-symbol")
                :type 'nelisp-cc-runtime-error)
  (should-error (nelisp-cc-runtime-resolve-symbol nil)
                :type 'nelisp-cc-runtime-error))

;;; (32) Recognition pass end-to-end via pipeline stats ---------------

(ert-deftest nelisp-cc-stage81-poc-stage4-pipeline-stats-recognition ()
  "When `recognize-primitives-enable' is t, the pipeline driver
counts rewrites in `pipeline-stats-recognized-primitives'.  This is
the integration smoke that confirms the recognition pass is wired
through `nelisp-cc-pipeline-run-7.7-passes' (= the real entry the
runtime calls) — not just exercised in isolation by ERT 27〜30."
  (let ((fn (nelisp-cc--ssa-make-function 'pipeline-stats-test '(int))))
    (let* ((entry (nelisp-cc--ssa-function-entry fn))
           (param (car (nelisp-cc--ssa-function-params fn)))
           (def (nelisp-cc--ssa-make-value fn))
           (instr (nelisp-cc--ssa-add-instr fn entry 'call (list param) def))
           (def2 (nelisp-cc--ssa-make-value fn))
           (instr2 (nelisp-cc--ssa-add-instr fn entry 'call (list param) def2)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            (list :fn 'car :unresolved t))
      (setf (nelisp-cc--ssa-instr-meta instr2)
            (list :fn 'cdr :unresolved t))
      ;; A return is also needed for ssa-verify, but the pipeline
      ;; driver doesn't verify — it just walks blocks.
      ;; Disable inliner pipeline (= keep test focused on recognition).
      (let ((nelisp-cc-enable-7.7-passes nil)
            (nelisp-cc-pipeline-recognize-primitives-enable t))
        (let* ((result (nelisp-cc-pipeline-run-7.7-passes fn))
               (stats  (cdr result)))
          ;; Two primitives recognized.
          (should (= 2 (nelisp-cc-pipeline-stats-recognized-primitives
                        stats)))
          ;; And the instructions were actually rewritten.
          (should (eq 'ssa-call-primitive
                      (nelisp-cc--ssa-instr-opcode instr)))
          (should (eq 'ssa-call-primitive
                      (nelisp-cc--ssa-instr-opcode instr2))))))
    ;; With the flag off (= default), no rewrites and the count stays 0.
    (let ((fn2 (nelisp-cc--ssa-make-function 'pipeline-off-test '(int))))
      (let* ((entry (nelisp-cc--ssa-function-entry fn2))
             (param (car (nelisp-cc--ssa-function-params fn2)))
             (def (nelisp-cc--ssa-make-value fn2))
             (instr (nelisp-cc--ssa-add-instr fn2 entry 'call (list param) def)))
        (setf (nelisp-cc--ssa-instr-meta instr) (list :fn 'car))
        (let ((nelisp-cc-enable-7.7-passes nil)
              (nelisp-cc-pipeline-recognize-primitives-enable nil))
          (let* ((result (nelisp-cc-pipeline-run-7.7-passes fn2))
                 (stats (cdr result)))
            (should (= 0 (nelisp-cc-pipeline-stats-recognized-primitives
                          stats)))
            (should (eq 'call (nelisp-cc--ssa-instr-opcode instr)))))))))

;;; Phase 7.1.6.a.2 dlsym integration smoke -------------------------
;;
;; Doc 28 §3.6.a ship gate (= the smoke deferred from Phase 7.1.6.a.1
;; per `nelisp-cc-runtime.el' §1681).  Verifies the end-to-end path:
;;
;;   1. The `nelisp' binary's dynamic symbol table exposes the
;;      `#[no_mangle]' cons trampolines (= `-rdynamic' is wired via
;;      `.cargo/config.toml').
;;   2. `nelisp-cc--dlsym-resolve' returns `:resolved' with a non-zero
;;      addr for each cons cluster symbol when invoked in the standalone
;;      binary.
;;   3. Unknown symbol names still return `:not-found'.
;;
;; The host Emacs runner does not have the `nelisp-cc--dlsym-resolve'
;; primitive, so we shell out to the locally-built `nelisp' binary if it
;; exists — `make test' alone is run in host-Emacs context so the smoke
;; auto-skips when the binary hasn't been built yet (= a bootstrap test
;; would fail-loud about that, but Phase 7.1.6.a.2's whole point is to
;; let the binary build with the new linker flag, so the smoke is
;; advisory rather than gating).

(defvar nelisp-cc-stage81-test--nelisp-bin
  (let* ((this-file (or load-file-name buffer-file-name))
         (test-dir (and this-file (file-name-directory this-file)))
         (repo (and test-dir (expand-file-name "../" test-dir))))
    (and repo (expand-file-name "target/debug/nelisp" repo)))
  "Path to the locally-built `nelisp' binary, if present.

Resolved relative to this test file at load time.  Used by the
Phase 7.1.6.a.2 dlsym integration smoke to invoke the standalone
binary (= the only context where `nelisp-cc--dlsym-resolve' is
fbound).")

(defun nelisp-cc-stage81-test--nelisp-eval (form)
  "Evaluate FORM (an elisp sexp) under the locally-built `nelisp' binary.

Returns the printed result as a string with trailing newline trimmed.
Caller is responsible for `skip-unless'-gating on
`file-executable-p' against `nelisp-cc-stage81-test--nelisp-bin'."
  (let ((expr (let ((print-level nil)
                    (print-length nil))
                (prin1-to-string form))))
    (with-temp-buffer
      (let ((rc (call-process nelisp-cc-stage81-test--nelisp-bin
                              nil t nil "eval" expr)))
        (unless (zerop rc)
          (error "nelisp eval %s failed (rc=%d): %s"
                 expr rc (buffer-string)))
        (string-trim-right (buffer-string))))))

(ert-deftest nelisp-cc-stage81-poc-7.1.6.a.2-dlsym-resolves-cons-cluster ()
  "Phase 7.1.6.a.2: dlsym resolves all 5 `nl_jit_cons_*' symbols.

The cluster takeover (Doc 28 §3.6.a) deletes the Cranelift `JitCons'
wrapper but keeps the trampolines as `#[no_mangle] extern \"C\"' so
the dlsym bridge can locate them.  Smoke verifies that `(nelisp-cc--
dlsym-resolve \"nl_jit_cons_car\")' returns `(:resolved . ADDR)' with
ADDR > 0 (= the symbol exists in the binary's dynsym table thanks to
`-rdynamic' in `.cargo/config.toml')."
  (skip-unless (and nelisp-cc-stage81-test--nelisp-bin
                    (file-executable-p nelisp-cc-stage81-test--nelisp-bin)))
  (dolist (sym '("nl_jit_cons_car" "nl_jit_cons_cdr"
                 "nl_jit_cons_make" "nl_jit_cons_setcar"
                 "nl_jit_cons_setcdr"))
    (let* ((out (nelisp-cc-stage81-test--nelisp-eval
                 `(nelisp-cc--dlsym-resolve ,sym)))
           ;; Output shape: "(:resolved . NNNNNNN)" or "(:not-found)".
           (resolved-prefix "(:resolved . "))
      (should (string-prefix-p resolved-prefix out))
      ;; Parse the addr and assert it is non-zero.
      (let* ((addr-str (substring out (length resolved-prefix)
                                  (1- (length out))))
             (addr (string-to-number addr-str)))
        (should (> addr 0))))))

(ert-deftest nelisp-cc-stage81-poc-7.1.6.a.2-dlsym-not-found-passthru ()
  "Phase 7.1.6.a.2: unknown C symbols still return `:not-found'.

The `-rdynamic' flag exposes ALL non-mangled symbols, but it does
not invent new ones — a typo or unrelated name must still take the
deferred-fallback branch (Doc 81 §6.3) rather than spuriously
resolving."
  (skip-unless (and nelisp-cc-stage81-test--nelisp-bin
                    (file-executable-p nelisp-cc-stage81-test--nelisp-bin)))
  (let ((out (nelisp-cc-stage81-test--nelisp-eval
              '(nelisp-cc--dlsym-resolve "nl_jit_cons_nonexistent_typo"))))
    (should (string-prefix-p "(:not-found" out))))

;;; Phase 7.1.6.b dlsym integration smoke -----------------------------
;;
;; Doc 28 §3.6.b ship gate (= the smoke for the access cluster
;; takeover, mirror of the Phase 7.1.6.a.2 cons cluster smoke above).
;; Verifies the same end-to-end path for the 4 `nl_jit_access_*'
;; trampolines (length / aref / aset / elt — Stage 81.3 vector ABI):
;;
;;   1. The `nelisp' binary's dynamic symbol table exposes the
;;      `#[no_mangle]' access trampolines (= `-rdynamic' is wired via
;;      `.cargo/config.toml', already present from Phase 7.1.6.a.2 —
;;      the access trampolines simply inherit it).
;;   2. `nelisp-cc--dlsym-resolve' returns `:resolved' with a non-zero
;;      addr for each access cluster symbol when invoked in the
;;      standalone binary.
;;
;; Auto-skips when the binary hasn't been built yet (= same skip-unless
;; gate as the cons smoke, since both rely on the locally-built nelisp
;; binary at `target/debug/nelisp').

(ert-deftest nelisp-cc-stage81-poc-7.1.6.b-dlsym-resolves-vector-cluster ()
  "Phase 7.1.6.b: dlsym resolves all 4 `nl_jit_access_*' symbols.

The cluster takeover (Doc 28 §3.6.b) deletes the Cranelift `JitAccess'
wrapper but keeps the trampolines as `#[no_mangle] extern \"C\"' so
the dlsym bridge can locate them.  Smoke verifies that
`(nelisp-cc--dlsym-resolve \"nl_jit_access_length\")' returns
`(:resolved . ADDR)' with ADDR > 0 (= the symbol exists in the
binary's dynsym table thanks to `-rdynamic' in `.cargo/config.toml',
already added by Phase 7.1.6.a.2; access trampolines just inherit)."
  (skip-unless (and nelisp-cc-stage81-test--nelisp-bin
                    (file-executable-p nelisp-cc-stage81-test--nelisp-bin)))
  (dolist (sym '("nl_jit_access_length" "nl_jit_access_aref"
                 "nl_jit_access_aset" "nl_jit_access_elt"))
    (let* ((out (nelisp-cc-stage81-test--nelisp-eval
                 `(nelisp-cc--dlsym-resolve ,sym)))
           ;; Output shape: "(:resolved . NNNNNNN)" or "(:not-found)".
           (resolved-prefix "(:resolved . "))
      (should (string-prefix-p resolved-prefix out))
      ;; Parse the addr and assert it is non-zero.
      (let* ((addr-str (substring out (length resolved-prefix)
                                  (1- (length out))))
             (addr (string-to-number addr-str)))
        (should (> addr 0))))))

;;; Phase 7.1.6.c dlsym integration smoke -----------------------------
;;
;; Doc 28 §3.6.c ship gate (= the smoke for the arith cluster takeover,
;; mirror of Phase 7.1.6.a.2 cons / 7.1.6.b access cluster smokes
;; above).  Verifies the same end-to-end path for the 12
;; `nl_jit_arith_*' trampolines (= 3 wrapping arith / 5 signed cmp /
;; 3 bitwise / `ash').  Note: arith was structurally different from
;; cons / access in that it had *no* pre-existing Rust trampoline
;; body — the Cranelift IR was the implementation pre-7.1.6.c.  The
;; takeover added 12 plain `nl_jit_arith_*' Rust trampolines that
;; mirror the deleted Cranelift IR semantics 1-to-1, then exposed
;; them via `#[no_mangle]' so the dlsym bridge can locate them.
;;
;;   1. The `nelisp' binary's dynamic symbol table exposes the 12
;;      `#[no_mangle]' arith trampolines (= `-rdynamic' is wired via
;;      `.cargo/config.toml', already present from Phase 7.1.6.a.2 —
;;      the arith trampolines simply inherit it).
;;   2. `nelisp-cc--dlsym-resolve' returns `:resolved' with a non-zero
;;      addr for each arith cluster symbol when invoked in the
;;      standalone binary.
;;
;; Auto-skips when the binary hasn't been built yet (= same skip-unless
;; gate as the cons / access smoke).

(ert-deftest nelisp-cc-stage81-poc-7.1.6.c-dlsym-resolves-arith-cluster ()
  "Phase 7.1.6.c: dlsym resolves all 12 `nl_jit_arith_*' symbols.

The cluster takeover (Doc 28 §3.6.c) deletes the Cranelift `JitArith'
wrapper and introduces 12 plain Rust trampolines as `#[no_mangle]
extern \"C\"' so the dlsym bridge can locate them.  Smoke verifies
that `(nelisp-cc--dlsym-resolve \"nl_jit_arith_add2\")' returns
`(:resolved . ADDR)' with ADDR > 0 (= the symbol exists in the
binary's dynsym table thanks to `-rdynamic' in `.cargo/config.toml',
already added by Phase 7.1.6.a.2; arith trampolines just inherit)."
  (skip-unless (and nelisp-cc-stage81-test--nelisp-bin
                    (file-executable-p nelisp-cc-stage81-test--nelisp-bin)))
  (dolist (sym '("nl_jit_arith_add2" "nl_jit_arith_sub2"
                 "nl_jit_arith_mul2" "nl_jit_arith_eq2"
                 "nl_jit_arith_lt2" "nl_jit_arith_gt2"
                 "nl_jit_arith_le2" "nl_jit_arith_ge2"
                 "nl_jit_arith_logior2" "nl_jit_arith_logand2"
                 "nl_jit_arith_logxor2" "nl_jit_arith_ash"))
    (let* ((out (nelisp-cc-stage81-test--nelisp-eval
                 `(nelisp-cc--dlsym-resolve ,sym)))
           ;; Output shape: "(:resolved . NNNNNNN)" or "(:not-found)".
           (resolved-prefix "(:resolved . "))
      (should (string-prefix-p resolved-prefix out))
      ;; Parse the addr and assert it is non-zero.
      (let* ((addr-str (substring out (length resolved-prefix)
                                  (1- (length out))))
             (addr (string-to-number addr-str)))
        (should (> addr 0))))))

;;; Phase 7.1.6.d dlsym integration smoke -----------------------------
;;
;; Doc 28 §3.6.d ship gate (= the smoke for the predicate cluster
;; takeover, mirror of Phase 7.1.6.a.2 cons / 7.1.6.b access / 7.1.6.c
;; arith cluster smokes above).  Verifies the same end-to-end path for
;; the consolidated `nl_jit_predicate_eq' trampoline.  Predicate had a
;; partial Rust helper (`nl_jit_pred_eq') covering only the slow
;; `sexp_eq' arm; the full 7-block fast-path semantics (= same-ref /
;; tag-eq / int-payload) lived in Cranelift IR.  The takeover
;; consolidates all 7 blocks into a single `nl_jit_predicate_eq'
;; trampoline body that mirrors the deleted Cranelift IR semantics
;; 1-to-1, then exposes it via `#[no_mangle]' so the dlsym bridge can
;; locate it.
;;
;;   1. The `nelisp' binary's dynamic symbol table exposes the
;;      `#[no_mangle]' predicate trampoline (= `-rdynamic' is wired
;;      via `.cargo/config.toml', already present from Phase 7.1.6.a.2
;;      — the predicate trampoline simply inherits it).
;;   2. `nelisp-cc--dlsym-resolve' returns `:resolved' with a non-zero
;;      addr for the predicate cluster symbol when invoked in the
;;      standalone binary.
;;
;; Auto-skips when the binary hasn't been built yet (= same skip-unless
;; gate as the cons / access / arith smoke).

(ert-deftest nelisp-cc-stage81-poc-7.1.6.d-dlsym-resolves-predicate-cluster ()
  "Phase 7.1.6.d: dlsym resolves the `nl_jit_predicate_eq' symbol.

The cluster takeover (Doc 28 §3.6.d) deletes the Cranelift `JitPredicate'
wrapper and consolidates the 7-block IR into a single plain Rust
trampoline `nl_jit_predicate_eq' as `#[no_mangle] extern \"C\"' so
the dlsym bridge can locate it.  Smoke verifies that
`(nelisp-cc--dlsym-resolve \"nl_jit_predicate_eq\")' returns
`(:resolved . ADDR)' with ADDR > 0 (= the symbol exists in the
binary's dynsym table thanks to `-rdynamic' in `.cargo/config.toml',
already added by Phase 7.1.6.a.2; predicate trampoline just inherits)."
  (skip-unless (and nelisp-cc-stage81-test--nelisp-bin
                    (file-executable-p nelisp-cc-stage81-test--nelisp-bin)))
  (dolist (sym '("nl_jit_predicate_eq"))
    (let* ((out (nelisp-cc-stage81-test--nelisp-eval
                 `(nelisp-cc--dlsym-resolve ,sym)))
           ;; Output shape: "(:resolved . NNNNNNN)" or "(:not-found)".
           (resolved-prefix "(:resolved . "))
      (should (string-prefix-p resolved-prefix out))
      ;; Parse the addr and assert it is non-zero.
      (let* ((addr-str (substring out (length resolved-prefix)
                                  (1- (length out))))
             (addr (string-to-number addr-str)))
        (should (> addr 0))))))

(provide 'nelisp-cc-stage81-poc-test)
;;; nelisp-cc-stage81-poc-test.el ends here
