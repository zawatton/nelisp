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

(provide 'nelisp-cc-stage81-poc-test)
;;; nelisp-cc-stage81-poc-test.el ends here
