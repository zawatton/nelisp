;;; nelisp-cc.el --- NeLisp native compiler scaffold (Phase 7.1.1 SSA IR)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 7.1.1 *scaffold subset* — see docs/design/28-phase7.1-native-compiler.org
;; §3.1.  Doc 28 LOCKED-2026-04-25-v2 commits Phase 7.1.1 to ~1500-3000 LOC
;; (full).  This scaffold lands the *data-structure half only* (~500 LOC)
;; so subsequent agents can layer AST→SSA conversion, optimisation passes,
;; and the linear-scan register allocator on a frozen substrate.
;;
;; In scope (this file):
;;   - cl-defstruct quartet for SSA IR
;;       * `nelisp-cc--ssa-value'    — produced exactly once (instr or :param)
;;       * `nelisp-cc--ssa-instr'    — opcode + operand SSA values + def value
;;       * `nelisp-cc--ssa-block'    — basic block (id / instrs / pred / succ)
;;       * `nelisp-cc--ssa-function' — name / blocks / entry / params
;;   - basic builder helpers (make-function, make-block, add-instr,
;;     link-blocks, add-use)
;;   - s-expression round-trip stub (`nelisp-cc--ssa-pp' /
;;     `nelisp-cc--ssa-from-sexp') — syntactic only, no semantic passes
;;   - `nelisp-cc--ssa-verify-function' invariant checker
;;       1. entry block exists and is in the block table
;;       2. predecessor / successor edges are bidirectional
;;       3. every instruction's `def' value is unique within the function
;;          (SSA single-assignment invariant)
;;       4. every block in the table is reachable from entry (no orphans)
;;       5. every successor referenced by some block is itself in the table
;;
;; Phase 7.5.5 (T38) extension — three deferred forms now lowered:
;;
;;   - `letrec' — mutually recursive bindings via the
;;     allocate-then-fill pattern (each NAME placeholder-stored to nil
;;     before any INIT is evaluated, so each INIT sees every NAME).
;;   - `funcall' — first-class function call.  `(funcall FN-EXPR ARG...)'
;;     lowers FN-EXPR to an SSA value and emits `:call-indirect' with
;;     the callee value as operand[0].
;;   - `while' — loop with explicit back-edge.  Three blocks
;;     (loop-header / loop-body / loop-exit) are wired with one
;;     forward branch (header → body / exit) and one back-edge
;;     (body → header).  The form's value is `nil' (per Emacs Lisp
;;     spec), materialised as a `:const' on the exit block.
;;
;; Out of scope (deferred to subsequent Phase 7.1.x):
;;   - AST → SSA conversion (Phase 7.1.1 full)
;;   - Linear-scan register allocator (Poletto-Sarkar 1999, Phase 7.1.1 full)
;;   - SSA optimisation passes — constant folding / dead-code elimination
;;     (Phase 7.1.1 full)
;;   - Inline-primitive uptake from `nelisp-jit--inline-primitives' (30 prims)
;;   - x86_64 / arm64 backend codegen (Phase 7.1.2 / 7.1.3)
;;   - mmap PROT_EXEC integration, tail-call, safe-point (Phase 7.1.4)
;;   - GC root bitmap on `nelisp-cc--ssa-instr' — Doc 28 §2.9 commits the
;;     metadata contract to Phase 7.1 in-scope, but the *bitmap field*
;;     attaches at Phase 7.1.4 (safe-point insertion).  Scaffold leaves an
;;     extension hook: `nelisp-cc--ssa-instr-meta' is a free-form alist for
;;     later phases to staple per-instruction metadata onto without
;;     widening the struct.  Doc 28 §3.1 risk mitigation freezes the
;;     other fields in 7.1.1.
;;
;; Module convention (matches existing nelisp-bytecode / nelisp-jit):
;;   - `nelisp-cc-' = public API
;;   - `nelisp-cc--' = private helper
;;   - errors derive from a single `nelisp-cc-error' parent

;;; Code:

(require 'cl-lib)
(require 'pcase)

;;; Errors -----------------------------------------------------------

(define-error 'nelisp-cc-error "NeLisp native compiler error")
(define-error 'nelisp-cc-verify-error
  "NeLisp SSA IR well-formedness violation" 'nelisp-cc-error)

;;; SSA value --------------------------------------------------------
;;
;; An SSA value is the result of exactly one definition site (an
;; instruction whose `def' field points at this value, or a function
;; parameter whose `def-point' is the keyword :param).  It is consumed
;; by zero or more instructions, tracked in `use-list' for verifier
;; queries and later optimisation passes.

(cl-defstruct (nelisp-cc--ssa-value
               (:constructor nelisp-cc--ssa-value-make)
               (:copier nil))
  "An SSA value — produced once, used many times.

ID is a function-scoped integer (unique within the enclosing
`nelisp-cc--ssa-function').  TYPE is a symbol tag for later type
inference passes; the scaffold treats nil as `unknown' and never
inspects it.  DEF-POINT is either an `nelisp-cc--ssa-instr' (for
instruction results) or the keyword `:param' (for function arguments).
USE-LIST is an unordered list of `nelisp-cc--ssa-instr' that consume
this value via their OPERANDS field; updated by
`nelisp-cc--ssa-add-use'."
  (id 0)
  (type nil)
  (def-point nil)
  (use-list nil))

;;; SSA instruction --------------------------------------------------
;;
;; An instruction holds an opcode symbol, its operand SSA values, and
;; (for ops that produce a value) a single `def' SSA value.  The DEF
;; field is canonical — the value's `def-point' must point back at this
;; instruction (verifier checks both directions).

(cl-defstruct (nelisp-cc--ssa-instr
               (:constructor nelisp-cc--ssa-instr-make)
               (:copier nil))
  "A single SSA instruction.

ID is unique within the enclosing function.  OPCODE is a symbol
(`add', `sub', `call', `phi', `branch', `return', ...), opaque to the
scaffold and interpreted by future passes.  OPERANDS is a list of
`nelisp-cc--ssa-value' that this instruction reads.  DEF is the
`nelisp-cc--ssa-value' it produces, or nil for void instructions
(branches, stores, returns).  BLOCK is a back-pointer to the
containing `nelisp-cc--ssa-block', set by `nelisp-cc--ssa-add-instr'.
META is a free-form alist reserved for later phases to attach
metadata (live-root bitmap, source-position info, debug name) without
amending this struct — Doc 28 §3.1 freezes the listed fields."
  (id 0)
  (opcode nil)
  (operands nil)
  (def nil)
  (block nil)
  (meta nil))

;;; SSA block --------------------------------------------------------

(cl-defstruct (nelisp-cc--ssa-block
               (:constructor nelisp-cc--ssa-block-make)
               (:copier nil))
  "A basic block in the SSA control-flow graph.

ID is unique within the enclosing function.  LABEL is an optional
human-readable string for printing (set by `nelisp-cc--ssa-make-block'
when callers pass one).  INSTRS is an ordered list (head = first
instruction); the *terminator* lives at the end and is conventionally a
branch / return — the scaffold does not enforce this, leaving it to
later optimiser passes.  PREDECESSORS / SUCCESSORS are lists of
`nelisp-cc--ssa-block' maintained pairwise by
`nelisp-cc--ssa-link-blocks'."
  (id 0)
  (label nil)
  (instrs nil)
  (predecessors nil)
  (successors nil))

;;; SSA function -----------------------------------------------------

(cl-defstruct (nelisp-cc--ssa-function
               (:constructor nelisp-cc--ssa-function-make)
               (:copier nil))
  "A compilation unit in SSA form.

NAME is a symbol (nil for anonymous lambdas).  PARAMS is a list of
`nelisp-cc--ssa-value' whose DEF-POINT is `:param', in positional
argument order.  ENTRY is the `nelisp-cc--ssa-block' that execution
begins in; it lives in BLOCKS like every other block.  BLOCKS is the
full block table (order is creation order, not topological).
NEXT-VALUE-ID / NEXT-INSTR-ID / NEXT-BLOCK-ID are monotonic counters
the builders advance — do not mutate from outside the builders."
  (name nil)
  (params nil)
  (entry nil)
  (blocks nil)
  (next-value-id 0)
  (next-instr-id 0)
  (next-block-id 0))

;;; Builder helpers --------------------------------------------------

(defun nelisp-cc--ssa-make-function (name param-types)
  "Construct a function with NAME and PARAM-TYPES.

PARAM-TYPES is a list of type symbols, one per positional argument
(nil = unknown).  Returns a `nelisp-cc--ssa-function' with one empty
ENTRY block already linked, parameter values whose DEF-POINT is
`:param', and counters seeded past the IDs handed out for params and
the entry block."
  (let* ((fn (nelisp-cc--ssa-function-make :name name))
         (params
          (let ((acc nil))
            (dolist (ty param-types)
              (let ((v (nelisp-cc--ssa-value-make
                        :id (nelisp-cc--ssa-function-next-value-id fn)
                        :type ty
                        :def-point :param)))
                (cl-incf (nelisp-cc--ssa-function-next-value-id fn))
                (push v acc)))
            (nreverse acc)))
         (entry (nelisp-cc--ssa-block-make
                 :id (nelisp-cc--ssa-function-next-block-id fn)
                 :label "entry")))
    (cl-incf (nelisp-cc--ssa-function-next-block-id fn))
    (setf (nelisp-cc--ssa-function-params fn) params
          (nelisp-cc--ssa-function-entry fn) entry
          (nelisp-cc--ssa-function-blocks fn) (list entry))
    fn))

(defun nelisp-cc--ssa-make-block (fn &optional label)
  "Allocate a fresh basic block in FN with optional LABEL string.
The block is appended to FN's BLOCKS list and returned.  The block
starts disconnected from any other; callers wire edges via
`nelisp-cc--ssa-link-blocks'."
  (let ((blk (nelisp-cc--ssa-block-make
              :id (nelisp-cc--ssa-function-next-block-id fn)
              :label label)))
    (cl-incf (nelisp-cc--ssa-function-next-block-id fn))
    (setf (nelisp-cc--ssa-function-blocks fn)
          (append (nelisp-cc--ssa-function-blocks fn) (list blk)))
    blk))

(defun nelisp-cc--ssa-make-value (fn &optional type)
  "Allocate a fresh SSA value in FN with optional TYPE tag.
DEF-POINT is left nil — `nelisp-cc--ssa-add-instr' patches it when the
defining instruction is appended."
  (let ((v (nelisp-cc--ssa-value-make
            :id (nelisp-cc--ssa-function-next-value-id fn)
            :type type)))
    (cl-incf (nelisp-cc--ssa-function-next-value-id fn))
    v))

(defun nelisp-cc--ssa-add-instr (fn block opcode operands &optional def)
  "Append a new instruction to BLOCK in FN.

OPCODE is a symbol.  OPERANDS is a list of `nelisp-cc--ssa-value' (or
nil for ops that take none).  DEF, when non-nil, must be a
`nelisp-cc--ssa-value' that has not yet been defined elsewhere; the
helper sets its DEF-POINT to the new instruction.  Updates each
operand's USE-LIST and the block's INSTRS list (preserving order),
then returns the new instruction."
  (let ((instr (nelisp-cc--ssa-instr-make
                :id (nelisp-cc--ssa-function-next-instr-id fn)
                :opcode opcode
                :operands operands
                :def def
                :block block)))
    (cl-incf (nelisp-cc--ssa-function-next-instr-id fn))
    (when def
      (when (nelisp-cc--ssa-value-def-point def)
        (signal 'nelisp-cc-verify-error
                (list "value already has a def-point"
                      (nelisp-cc--ssa-value-id def))))
      (setf (nelisp-cc--ssa-value-def-point def) instr))
    (dolist (op operands)
      (nelisp-cc--ssa-add-use op instr))
    (setf (nelisp-cc--ssa-block-instrs block)
          (append (nelisp-cc--ssa-block-instrs block) (list instr)))
    instr))

(defun nelisp-cc--ssa-add-use (value instr)
  "Record that INSTR consumes VALUE.
The check is `memq'-based — re-adding an already-recorded use is a
no-op, which keeps the operation idempotent for verifier rebuilds."
  (unless (memq instr (nelisp-cc--ssa-value-use-list value))
    (push instr (nelisp-cc--ssa-value-use-list value))))

(defun nelisp-cc--ssa-link-blocks (from to)
  "Add a control-flow edge FROM → TO, both directions.
Idempotent — duplicate calls do not double-add."
  (unless (memq to (nelisp-cc--ssa-block-successors from))
    (setf (nelisp-cc--ssa-block-successors from)
          (append (nelisp-cc--ssa-block-successors from) (list to))))
  (unless (memq from (nelisp-cc--ssa-block-predecessors to))
    (setf (nelisp-cc--ssa-block-predecessors to)
          (append (nelisp-cc--ssa-block-predecessors to) (list from)))))

;;; Pretty printer / reader (round-trip stub) -----------------------
;;
;; The s-expression form is the *transport format* between scaffold and
;; future passes — it is intentionally lossy w.r.t. counter state and
;; use-list back-pointers (those are recomputed on read).  What the
;; round-trip preserves: the function's name + param types, every
;; block's id + label + ordered instructions, every instruction's
;; opcode + operand value-ids + def value-id, and the predecessor-set
;; of each block (successors are derived from the predecessor map for
;; symmetry, see `nelisp-cc--ssa-from-sexp').

(defun nelisp-cc--ssa-pp (fn)
  "Pretty-print FN (a `nelisp-cc--ssa-function') as an s-expression.

Shape:
  (:function NAME
   :params ((VID . TYPE) ...)
   :blocks ((BID :label LABEL
                 :preds (BID ...)
                 :instrs ((IID :op OP :operands (VID ...)
                               [:def VID :type TYPE])
                          ...))
            ...)
   :entry BID
   :next-value-id N :next-instr-id N :next-block-id N)"
  (let ((params
         (mapcar (lambda (v)
                   (cons (nelisp-cc--ssa-value-id v)
                         (nelisp-cc--ssa-value-type v)))
                 (nelisp-cc--ssa-function-params fn)))
        (blocks
         (mapcar
          (lambda (blk)
            (let ((bid (nelisp-cc--ssa-block-id blk))
                  (label (nelisp-cc--ssa-block-label blk))
                  (preds (mapcar #'nelisp-cc--ssa-block-id
                                 (nelisp-cc--ssa-block-predecessors blk)))
                  (instrs
                   (mapcar
                    (lambda (instr)
                      (let* ((iid (nelisp-cc--ssa-instr-id instr))
                             (op  (nelisp-cc--ssa-instr-opcode instr))
                             (ops (mapcar #'nelisp-cc--ssa-value-id
                                          (nelisp-cc--ssa-instr-operands instr)))
                             (def (nelisp-cc--ssa-instr-def instr))
                             (base (list iid :op op :operands ops)))
                        (if def
                            (append base
                                    (list :def (nelisp-cc--ssa-value-id def)
                                          :type (nelisp-cc--ssa-value-type def)))
                          base)))
                    (nelisp-cc--ssa-block-instrs blk))))
              (list bid :label label :preds preds :instrs instrs)))
          (nelisp-cc--ssa-function-blocks fn))))
    (list :function (nelisp-cc--ssa-function-name fn)
          :params params
          :blocks blocks
          :entry (nelisp-cc--ssa-block-id (nelisp-cc--ssa-function-entry fn))
          :next-value-id (nelisp-cc--ssa-function-next-value-id fn)
          :next-instr-id (nelisp-cc--ssa-function-next-instr-id fn)
          :next-block-id (nelisp-cc--ssa-function-next-block-id fn))))

(defun nelisp-cc--ssa--plist-get (sexp key)
  "Fetch KEY from SEXP, signalling if absent (round-trip is strict)."
  (let ((cell (memq key sexp)))
    (unless cell
      (signal 'nelisp-cc-verify-error
              (list "missing key in pp form" key)))
    (cadr cell)))

(defun nelisp-cc--ssa-from-sexp (sexp)
  "Reconstruct a `nelisp-cc--ssa-function' from SEXP (output of `-pp').

Counters are restored from the printed values so subsequent builder
calls do not collide with already-issued IDs.  Use-list back-pointers
are rebuilt from operand walks; predecessor / successor edges are
re-linked symmetrically using the printed predecessor map."
  (unless (and (consp sexp) (eq (car sexp) :function))
    (signal 'nelisp-cc-verify-error
            (list "not a printed SSA function" sexp)))
  (let* ((name           (cadr sexp))
         (rest           (cddr sexp))
         (params-spec    (nelisp-cc--ssa--plist-get rest :params))
         (blocks-spec    (nelisp-cc--ssa--plist-get rest :blocks))
         (entry-id       (nelisp-cc--ssa--plist-get rest :entry))
         (next-value-id  (nelisp-cc--ssa--plist-get rest :next-value-id))
         (next-instr-id  (nelisp-cc--ssa--plist-get rest :next-instr-id))
         (next-block-id  (nelisp-cc--ssa--plist-get rest :next-block-id))
         (fn (nelisp-cc--ssa-function-make :name name))
         (value-table (make-hash-table :test 'eql))
         (block-table (make-hash-table :test 'eql)))
    ;; Pass 1: param values
    (let ((params nil))
      (dolist (p params-spec)
        (let ((v (nelisp-cc--ssa-value-make
                  :id (car p) :type (cdr p) :def-point :param)))
          (puthash (car p) v value-table)
          (push v params)))
      (setf (nelisp-cc--ssa-function-params fn) (nreverse params)))
    ;; Pass 2: blocks (without instrs)
    (dolist (bspec blocks-spec)
      (let* ((bid   (car bspec))
             (label (nelisp-cc--ssa--plist-get (cdr bspec) :label))
             (blk   (nelisp-cc--ssa-block-make :id bid :label label)))
        (puthash bid blk block-table)))
    ;; Pass 3: instructions + def values (now that all blocks exist)
    (dolist (bspec blocks-spec)
      (let* ((bid    (car bspec))
             (blk    (gethash bid block-table))
             (instrs (nelisp-cc--ssa--plist-get (cdr bspec) :instrs))
             (acc    nil))
        (dolist (ispec instrs)
          (let* ((iid    (car ispec))
                 (rest   (cdr ispec))
                 (op     (nelisp-cc--ssa--plist-get rest :op))
                 (op-ids (nelisp-cc--ssa--plist-get rest :operands))
                 (def-id (let ((c (memq :def rest))) (and c (cadr c))))
                 (def-ty (let ((c (memq :type rest))) (and c (cadr c))))
                 (operands
                  (mapcar
                   (lambda (vid)
                     (or (gethash vid value-table)
                         (signal 'nelisp-cc-verify-error
                                 (list "unknown operand value id" vid))))
                   op-ids))
                 (def (and def-id
                           (let ((v (nelisp-cc--ssa-value-make
                                     :id def-id :type def-ty)))
                             (puthash def-id v value-table)
                             v)))
                 (instr (nelisp-cc--ssa-instr-make
                         :id iid :opcode op :operands operands
                         :def def :block blk)))
            (when def
              (setf (nelisp-cc--ssa-value-def-point def) instr))
            (dolist (operand operands)
              (nelisp-cc--ssa-add-use operand instr))
            (push instr acc)))
        (setf (nelisp-cc--ssa-block-instrs blk) (nreverse acc))))
    ;; Pass 4: re-link predecessor / successor edges (bidirectional)
    (dolist (bspec blocks-spec)
      (let* ((bid   (car bspec))
             (preds (nelisp-cc--ssa--plist-get (cdr bspec) :preds))
             (blk   (gethash bid block-table)))
        (dolist (pid preds)
          (let ((pred (gethash pid block-table)))
            (unless pred
              (signal 'nelisp-cc-verify-error
                      (list "unknown predecessor block id" pid)))
            (nelisp-cc--ssa-link-blocks pred blk)))))
    ;; Finalise function fields
    (setf (nelisp-cc--ssa-function-blocks fn)
          (mapcar (lambda (bspec) (gethash (car bspec) block-table))
                  blocks-spec)
          (nelisp-cc--ssa-function-entry fn)
          (or (gethash entry-id block-table)
              (signal 'nelisp-cc-verify-error
                      (list "unknown entry block id" entry-id)))
          (nelisp-cc--ssa-function-next-value-id fn) next-value-id
          (nelisp-cc--ssa-function-next-instr-id fn) next-instr-id
          (nelisp-cc--ssa-function-next-block-id fn) next-block-id)
    fn))

;;; Verifier ---------------------------------------------------------

(defun nelisp-cc--ssa--reachable-block-ids (fn)
  "Return a hash-table of block IDs reachable from FN's entry."
  (let ((seen (make-hash-table :test 'eql))
        (stack (list (nelisp-cc--ssa-function-entry fn))))
    (while stack
      (let ((b (pop stack)))
        (unless (gethash (nelisp-cc--ssa-block-id b) seen)
          (puthash (nelisp-cc--ssa-block-id b) t seen)
          (dolist (s (nelisp-cc--ssa-block-successors b))
            (push s stack)))))
    seen))

(defun nelisp-cc--ssa-verify-function (fn)
  "Check FN's SSA invariants.  Return t on success.

Signals `nelisp-cc-verify-error' with a (REASON . DETAILS) tail on the
first violation found.  Invariants enforced (see commentary):

  1. ENTRY is in BLOCKS.
  2. Every successor pointer of every block resolves to a block in
     BLOCKS, and the reverse predecessor edge exists (and vice versa).
  3. Each instruction's DEF (if non-nil) has a unique value ID across
     the whole function (single-assignment).
  4. No orphan blocks — every block in BLOCKS is reachable from ENTRY.
  5. Every operand value's DEF-POINT is either `:param' or an
     instruction that sits in some block of FN."
  (let ((blocks (nelisp-cc--ssa-function-blocks fn))
        (entry  (nelisp-cc--ssa-function-entry fn)))
    ;; (1) entry membership
    (unless (memq entry blocks)
      (signal 'nelisp-cc-verify-error
              (list :entry-not-in-blocks
                    (and entry (nelisp-cc--ssa-block-id entry)))))
    ;; (2) edge bidirectionality
    (dolist (b blocks)
      (dolist (s (nelisp-cc--ssa-block-successors b))
        (unless (memq s blocks)
          (signal 'nelisp-cc-verify-error
                  (list :successor-not-in-blocks
                        (nelisp-cc--ssa-block-id b)
                        (nelisp-cc--ssa-block-id s))))
        (unless (memq b (nelisp-cc--ssa-block-predecessors s))
          (signal 'nelisp-cc-verify-error
                  (list :missing-predecessor-back-edge
                        :from (nelisp-cc--ssa-block-id b)
                        :to   (nelisp-cc--ssa-block-id s)))))
      (dolist (p (nelisp-cc--ssa-block-predecessors b))
        (unless (memq p blocks)
          (signal 'nelisp-cc-verify-error
                  (list :predecessor-not-in-blocks
                        (nelisp-cc--ssa-block-id b)
                        (nelisp-cc--ssa-block-id p))))
        (unless (memq b (nelisp-cc--ssa-block-successors p))
          (signal 'nelisp-cc-verify-error
                  (list :missing-successor-back-edge
                        :from (nelisp-cc--ssa-block-id p)
                        :to   (nelisp-cc--ssa-block-id b))))))
    ;; (3) SSA single-assignment
    (let ((seen (make-hash-table :test 'eql)))
      (dolist (b blocks)
        (dolist (instr (nelisp-cc--ssa-block-instrs b))
          (let ((def (nelisp-cc--ssa-instr-def instr)))
            (when def
              (let ((vid (nelisp-cc--ssa-value-id def)))
                (when (gethash vid seen)
                  (signal 'nelisp-cc-verify-error
                          (list :duplicate-def vid)))
                (puthash vid t seen)
                (unless (eq (nelisp-cc--ssa-value-def-point def) instr)
                  (signal 'nelisp-cc-verify-error
                          (list :def-point-mismatch vid)))))))))
    ;; (4) reachability — every block must be reachable from entry
    (let ((reach (nelisp-cc--ssa--reachable-block-ids fn)))
      (dolist (b blocks)
        (unless (gethash (nelisp-cc--ssa-block-id b) reach)
          (signal 'nelisp-cc-verify-error
                  (list :orphan-block (nelisp-cc--ssa-block-id b))))))
    ;; (5) operand def-point integrity
    (let ((all-instrs (make-hash-table :test 'eq)))
      (dolist (b blocks)
        (dolist (instr (nelisp-cc--ssa-block-instrs b))
          (puthash instr t all-instrs)))
      (dolist (b blocks)
        (dolist (instr (nelisp-cc--ssa-block-instrs b))
          (dolist (op (nelisp-cc--ssa-instr-operands instr))
            (let ((dp (nelisp-cc--ssa-value-def-point op)))
              (cond
               ((eq dp :param) t)
               ((and dp (gethash dp all-instrs)) t)
               (t (signal 'nelisp-cc-verify-error
                          (list :operand-without-def
                                :value (nelisp-cc--ssa-value-id op)
                                :user  (nelisp-cc--ssa-instr-id instr))))))))))
    t))

;;; Linear-scan register allocation (Poletto-Sarkar 1999) ------------
;;
;; This section layers a *linear-scan register allocator* on top of
;; the SSA scaffold above.  The algorithm is the classical Poletto-
;; Sarkar 1999 design (ACM TOPLAS 21(5)), see Doc 28 §2.5 candidate A
;; and §3.1 Phase 7.1.1 scope.  Two stages:
;;
;;   1. `nelisp-cc--compute-intervals' — walks the function in a
;;      linearised order (reverse postorder of the forward DFS from
;;      ENTRY) and assigns each instruction a *linear position*.  For
;;      every SSA value (including parameters) it then records a
;;      `nelisp-cc--ssa-interval' whose START is the def-point
;;      position and whose END is the last-use position.  Parameters
;;      conventionally start at position 0 (before the first
;;      instruction); a value with no uses keeps END = START so it
;;      still occupies a valid 1-position interval.
;;
;;   2. `nelisp-cc--linear-scan' — sorts the intervals by START and
;;      sweeps once.  At each step it
;;        (a) expires intervals in ACTIVE whose END < current START
;;            (their register returns to FREE),
;;        (b) if FREE has a slot, hands the current interval the slot
;;            and inserts the interval into ACTIVE keyed by END,
;;        (c) else *spills*: picks the longest-end candidate from
;;            ACTIVE ∪ {current}, marks it `:spill', and assigns the
;;            freed register (if any) to the survivor.
;;
;; *Scaffold caveats* (Doc 28 §3.1, deferred to Phase 7.1.5+):
;;   - loop-induced live-interval extension is *not* performed; values
;;     defined before a loop and used inside one will see their END
;;     truncated to the in-linear-order last use.  A real allocator
;;     would extend each interval to cover every loop that contains a
;;     use.  Phase 7.1.5 graph-coloring will revisit this.
;;   - the spill marker is the keyword `:spill'.  Stack-slot assignment
;;     and reload code emission live in Phase 7.1.4 (mmap exec page
;;     stage); this scaffold only marks who needs to spill.
;;   - phi-merge handling, register coalescing and constraint-based
;;     pre-coloring are out of scope (Phase 7.1.5 graph-coloring).

(cl-defstruct (nelisp-cc--ssa-interval
               (:constructor nelisp-cc--ssa-interval-make)
               (:copier nil))
  "Linear live interval for a single SSA value.
VALUE is the `nelisp-cc--ssa-value' this interval describes.  START
is the linear position of its def-point (0 for parameters).  END is
the linear position of its last use, or START if the value has no
uses (a defined-but-dead value still occupies its def-point slot).
USES is the ordered list of linear positions at which the value is
read (may be empty).  END is *not* :read-only — Phase 7.1.5 may
extend it to cover loops; the scaffold leaves the field mutable so
later passes do not have to allocate a fresh struct.

CROSSES-CALL is t when the interval is *live* across at least one
`:call' or `:call-indirect' instruction (i.e. some call-position P
satisfies START < P <= END *and* P is also < END or there is a
later use).  T63 Phase 7.5.7 — the linear-scan allocator forces such
intervals to a stack slot rather than a caller-saved register, so
the value survives the System V / AAPCS64 caller-saved clobber that
every CALL / BL inflicts.  The default register pool maps r0..r7 to
caller-saved (rdi..r11 / x0..x7), so without this flag a value live
across a call site would silently corrupt — this was the root cause
of the T49 audit critical #3 / T50 letrec self-recursion SIGSEGV."
  (value nil :read-only t)
  (start 0 :read-only t)
  (end 0)
  (uses nil)
  (crosses-call nil))

(cl-defstruct (nelisp-cc--alloc-state
               (:constructor nelisp-cc--alloc-state-make)
               (:copier nil))
  "Linear-scan allocator state, threaded through the sweep loop.
FREE is the list of register names not currently held by any active
interval.  ACTIVE is the list of intervals that have been assigned a
register and whose END has not yet passed; it is kept sorted by END
ascending so `expire-old' can stop at the first non-expiring entry.
ASSIGNMENTS is the alist (VALUE-ID . REGISTER-OR-:spill) accumulating
allocator decisions; the final result is the public output of
`nelisp-cc--linear-scan'."
  (free nil)
  (active nil)
  (assignments nil))

(defconst nelisp-cc--default-int-registers
  '(r0 r1 r2 r3 r4 r5 r6 r7)
  "Default integer-register pool for the linear-scan allocator.
Phase 7.1.2 (x86_64 System V) maps these to physical registers, and
Phase 7.1.3 (arm64 AAPCS64) provides a separate mapping; the
allocator itself stays ABI-agnostic and operates on these symbolic
names.  Callers wanting a different pool (e.g. an 8-register
floating-point bank, or a 4-register pressure stress test) pass an
explicit REGISTERS list to `nelisp-cc--linear-scan'.")

;;; Linearisation ----------------------------------------------------

(defun nelisp-cc--ssa--reverse-postorder (fn)
  "Return FN's blocks in reverse postorder of a forward DFS from ENTRY.

Reverse postorder is the standard linearisation for forward dataflow
problems: every block precedes its successors except across back
edges (loops).  The scaffold ignores back edges entirely — see the
file header for the loop-extension caveat.

Implementation: iterative DFS with an explicit stack.  Each block is
finished (postorder) when all its successors have been visited.
Pushing each finished block onto the head of POST yields the reverse
postorder directly — no extra `nreverse' needed at the end."
  (let ((entry (nelisp-cc--ssa-function-entry fn))
        (visited (make-hash-table :test 'eq))
        (post nil)
        (stack nil))
    (when entry
      (push (cons entry (nelisp-cc--ssa-block-successors entry)) stack)
      (puthash entry t visited)
      (while stack
        (let* ((top (car stack))
               (blk (car top))
               (succs (cdr top)))
          (cond
           ((null succs)
            ;; All successors visited — finish this block.
            (push blk post)
            (pop stack))
           (t
            (let ((next (car succs)))
              (setcdr top (cdr succs))
              (unless (gethash next visited)
                (puthash next t visited)
                (push (cons next (nelisp-cc--ssa-block-successors next))
                      stack))))))))
    post))

(defun nelisp-cc--compute-intervals (fn)
  "Compute live intervals for every SSA value of FN.

Returns a list of `nelisp-cc--ssa-interval', sorted by START
ascending (and by VALUE-ID as a stable tie-breaker, so two values
defined at the same position keep a deterministic order).

Linearisation rule:
  - Position 0 is reserved for function parameters (their START).
  - Instructions are numbered sequentially starting at 1, in the
    order produced by `nelisp-cc--ssa--reverse-postorder' for blocks
    and the natural INSTRS list order within each block.
  - Each operand's USE position is the position of the consuming
    instruction.
  - END is the maximum of (START, max use-position).

Caveat: loop-induced extension is deferred to Phase 7.1.5.  See
section commentary above."
  (let ((rpo (nelisp-cc--ssa--reverse-postorder fn))
        (pos 1)
        (instr-pos (make-hash-table :test 'eq))
        (intervals (make-hash-table :test 'eql)) ; vid -> interval
        (call-positions nil))                    ; T63: list of call instr positions
    ;; Seed parameter intervals at position 0.
    (dolist (p (nelisp-cc--ssa-function-params fn))
      (puthash (nelisp-cc--ssa-value-id p)
               (nelisp-cc--ssa-interval-make :value p :start 0 :end 0)
               intervals))
    ;; First pass: assign linear positions to every instruction in the
    ;; reverse-postorder block stream, and seed an interval for every
    ;; def value at its def position.  T63 Phase 7.5.7 also collects
    ;; the linear position of each `:call' / `:call-indirect' so a
    ;; second pass can flag intervals that span any of them.
    (dolist (blk rpo)
      (dolist (instr (nelisp-cc--ssa-block-instrs blk))
        (puthash instr pos instr-pos)
        (when (memq (nelisp-cc--ssa-instr-opcode instr)
                    '(call call-indirect))
          (push pos call-positions))
        (let ((def (nelisp-cc--ssa-instr-def instr)))
          (when def
            (puthash (nelisp-cc--ssa-value-id def)
                     (nelisp-cc--ssa-interval-make
                      :value def :start pos :end pos)
                     intervals)))
        (cl-incf pos)))
    ;; Second pass: walk operand uses, extend each interval's END to
    ;; cover the use position, and record the use position in USES.
    (dolist (blk rpo)
      (dolist (instr (nelisp-cc--ssa-block-instrs blk))
        (let ((upos (gethash instr instr-pos)))
          (dolist (op (nelisp-cc--ssa-instr-operands instr))
            (let ((iv (gethash (nelisp-cc--ssa-value-id op) intervals)))
              (when iv
                (when (> upos (nelisp-cc--ssa-interval-end iv))
                  (setf (nelisp-cc--ssa-interval-end iv) upos))
                (setf (nelisp-cc--ssa-interval-uses iv)
                      (append (nelisp-cc--ssa-interval-uses iv)
                              (list upos)))))))))
    ;; T63 Phase 7.5.7 — third pass: mark every interval whose span
    ;; spans a call position as call-crossing.  An interval is "live
    ;; across" a call if some call-pos P satisfies START < P AND P <
    ;; END (i.e. the value is defined before the call and read after
    ;; — the call's caller-saved clobber would otherwise destroy it).
    ;; Note: `START < P' (strict) — a value *defined by* a call (its
    ;; own return-value harvest at P=START) does not cross itself.
    ;; `P < END' (strict) — a value last *used* at the call site (e.g.
    ;; an operand) does not need to survive past the call.
    (when call-positions
      (maphash
       (lambda (_vid iv)
         (let ((start (nelisp-cc--ssa-interval-start iv))
               (end   (nelisp-cc--ssa-interval-end iv)))
           (when (cl-some (lambda (p) (and (< start p) (< p end)))
                          call-positions)
             (setf (nelisp-cc--ssa-interval-crosses-call iv) t))))
       intervals))
    ;; Collect, sort by (start, value-id) for determinism.
    (let ((all nil))
      (maphash (lambda (_vid iv) (push iv all)) intervals)
      (sort all
            (lambda (a b)
              (let ((sa (nelisp-cc--ssa-interval-start a))
                    (sb (nelisp-cc--ssa-interval-start b)))
                (cond
                 ((< sa sb) t)
                 ((> sa sb) nil)
                 (t (< (nelisp-cc--ssa-value-id
                        (nelisp-cc--ssa-interval-value a))
                       (nelisp-cc--ssa-value-id
                        (nelisp-cc--ssa-interval-value b)))))))))))

;;; Linear-scan sweep -----------------------------------------------

(defun nelisp-cc--alloc--insert-active (interval active)
  "Return ACTIVE with INTERVAL inserted, keeping the list sorted by END."
  (let ((end (nelisp-cc--ssa-interval-end interval))
        (head nil)
        (tail active))
    (while (and tail
                (<= (nelisp-cc--ssa-interval-end (car tail)) end))
      (push (car tail) head)
      (setq tail (cdr tail)))
    (append (nreverse head) (cons interval tail))))

(defun nelisp-cc--alloc--expire-old (state start)
  "Move intervals whose END < START out of ACTIVE, returning their registers.
Mutates STATE's ACTIVE and FREE.  ACTIVE is sorted by END so we stop at
the first non-expiring entry.  This is exactly the EXPIREOLDINTERVALS
routine of Poletto-Sarkar."
  (let ((active (nelisp-cc--alloc-state-active state))
        (free (nelisp-cc--alloc-state-free state))
        (assignments (nelisp-cc--alloc-state-assignments state)))
    (while (and active
                (< (nelisp-cc--ssa-interval-end (car active)) start))
      (let* ((iv (car active))
             (vid (nelisp-cc--ssa-value-id
                   (nelisp-cc--ssa-interval-value iv)))
             (cell (assq vid assignments))
             (reg (and cell (cdr cell))))
        ;; Only return the register if the interval was actually
        ;; assigned a physical reg — spilled intervals never held one.
        (when (and reg (not (eq reg :spill)))
          (setq free (append free (list reg))))
        (setq active (cdr active))))
    (setf (nelisp-cc--alloc-state-active state) active
          (nelisp-cc--alloc-state-free state) free)
    state))

(defun nelisp-cc--alloc--spill-at (state interval)
  "Apply the SPILLATINTERVAL action: pick the longest-end victim.

Either the new INTERVAL itself is spilled (if every active interval
ends earlier or equal), or the existing active interval with the
greatest END is evicted, its register handed to INTERVAL, and the
victim recorded as `:spill'."
  (let* ((active (nelisp-cc--alloc-state-active state))
         (assignments (nelisp-cc--alloc-state-assignments state))
         ;; Active is sorted by END ascending — last cell is the
         ;; longest-end candidate.
         (victim (car (last active))))
    (cond
     ((or (null victim)
          (<= (nelisp-cc--ssa-interval-end victim)
              (nelisp-cc--ssa-interval-end interval)))
      ;; Spill the new interval; active is untouched.
      (push (cons (nelisp-cc--ssa-value-id
                   (nelisp-cc--ssa-interval-value interval))
                  :spill)
            assignments)
      (setf (nelisp-cc--alloc-state-assignments state) assignments))
     (t
      ;; Evict victim, hand its register to interval, record victim
      ;; as :spill, insert interval into active sorted by end.
      (let* ((vvid (nelisp-cc--ssa-value-id
                    (nelisp-cc--ssa-interval-value victim)))
             (vcell (assq vvid assignments))
             (reg (and vcell (cdr vcell))))
        ;; Drop victim from active.
        (setf (nelisp-cc--alloc-state-active state)
              (cl-remove victim active :test #'eq))
        ;; Re-mark victim as spilled.
        (when vcell
          (setcdr vcell :spill))
        ;; Assign reg to the new interval, insert into active.
        (push (cons (nelisp-cc--ssa-value-id
                     (nelisp-cc--ssa-interval-value interval))
                    reg)
              (nelisp-cc--alloc-state-assignments state))
        (setf (nelisp-cc--alloc-state-active state)
              (nelisp-cc--alloc--insert-active
               interval
               (nelisp-cc--alloc-state-active state))))))
    state))

(defun nelisp-cc--linear-scan (fn &optional registers)
  "Linear-scan register allocation on FN.

REGISTERS is the available register pool (default
`nelisp-cc--default-int-registers').  Returns the allocator's
ASSIGNMENTS alist: each entry is (VALUE-ID . REGISTER-NAME) for an
SSA value that received a register, or (VALUE-ID . :spill) for a
value the allocator could not fit into the pool.

Algorithm (Poletto-Sarkar 1999, fig.\\ 1):

  for each interval i, in increasing START order:
    EXPIREOLDINTERVALS(i)
    if i CROSSES-CALL:                  ; T63 Phase 7.5.7
       assignments[i.vid] := :spill     ; survive caller-saved clobber
       continue
    if length(ACTIVE) == length(REGISTERS):
       SPILLATINTERVAL(i)
    else:
       register := free.pop()
       assignments[i.vid] := register
       ACTIVE.insert(i, sorted by END)

T63 Phase 7.5.7 call-aware spill — the default pool maps r0..r7 to
caller-saved physical registers (rdi..r11 / x0..x7), so any value
live across a `:call' / `:call-indirect' would be silently clobbered
by the callee's prologue.  We force such intervals to a stack slot
where the bits survive the call.  This was the direct root cause of
the T49 audit critical #3 + T50 letrec self-recursion SIGSEGV — the
bench-actual fib(30) recursion holds the partial sum and the loop
counter live across the recursive `funcall fib` step, both of which
landed in caller-saved regs and were destroyed when fib re-entered.
A future Phase 7.1.5 graph-colouring pass may re-promote some of
these to callee-saved slots once it pushes/pops the matching
prologue/epilogue; until then, spill is correct.

The scaffold extends nothing beyond the paper otherwise; loop-induced
live ranges are *not* widened.  Phase 7.1.5 graph-coloring will revisit."
  (let* ((regs (or registers nelisp-cc--default-int-registers))
         (intervals (nelisp-cc--compute-intervals fn))
         (state (nelisp-cc--alloc-state-make
                 :free (copy-sequence regs)
                 :active nil
                 :assignments nil)))
    (dolist (iv intervals)
      (nelisp-cc--alloc--expire-old
       state (nelisp-cc--ssa-interval-start iv))
      (cond
       ;; T63 Phase 7.5.7 — call-crossing intervals always spill.
       ;; Their bits must survive the callee's caller-saved register
       ;; clobber, and the default register pool only contains
       ;; caller-saved physical registers.  See docstring above for
       ;; the SIGSEGV root cause this branch closes.
       ((nelisp-cc--ssa-interval-crosses-call iv)
        (push (cons (nelisp-cc--ssa-value-id
                     (nelisp-cc--ssa-interval-value iv))
                    :spill)
              (nelisp-cc--alloc-state-assignments state)))
       ((null (nelisp-cc--alloc-state-free state))
        (nelisp-cc--alloc--spill-at state iv))
       (t
        (let* ((free (nelisp-cc--alloc-state-free state))
               (reg (car free)))
          (setf (nelisp-cc--alloc-state-free state) (cdr free))
          (push (cons (nelisp-cc--ssa-value-id
                       (nelisp-cc--ssa-interval-value iv))
                      reg)
                (nelisp-cc--alloc-state-assignments state))
          (setf (nelisp-cc--alloc-state-active state)
                (nelisp-cc--alloc--insert-active
                 iv (nelisp-cc--alloc-state-active state)))))))
    ;; Reverse so the alist is in interval start order, which is
    ;; friendlier for human reading and downstream printing.
    (nreverse (nelisp-cc--alloc-state-assignments state))))

;;; Public allocator helpers ----------------------------------------

(defun nelisp-cc--alloc-spilled-values (assignments)
  "Return the list of value IDs that ASSIGNMENTS marked `:spill'.
The result is in the same order ASSIGNMENTS uses."
  (let ((acc nil))
    (dolist (cell assignments)
      (when (eq (cdr cell) :spill)
        (push (car cell) acc)))
    (nreverse acc)))

(defun nelisp-cc--alloc-register-of (assignments value-id)
  "Look up the register assigned to VALUE-ID in ASSIGNMENTS.
Returns the register symbol, the keyword `:spill' if the value was
spilled, or nil if VALUE-ID is absent (e.g. a dead value the
interval pass omitted, or an out-of-function id)."
  (cdr (assq value-id assignments)))

(defun nelisp-cc--alloc-pp (assignments)
  "Pretty-print ASSIGNMENTS as an s-expression for debugging.
Shape: (:assignments ((VID . REG) ...) :spilled (VID ...)).
This is a *debug* helper — the round-trippable transport form is the
raw alist returned by `nelisp-cc--linear-scan'."
  (list :assignments (copy-sequence assignments)
        :spilled (nelisp-cc--alloc-spilled-values assignments)))

;;; Phase 7.1 T15 — stack-slot allocation for spilled values ---------
;;
;; The linear-scan allocator (T4) marks values it cannot fit in the
;; register pool with the `:spill' keyword.  T15 lays them out into
;; an 8-byte-per-slot stack frame, one slot per spilled value, and
;; returns an alist mapping value-ids to *byte offsets* (positive,
;; measured from the frame base — backend translates to negative
;; rbp-relative or positive sp-relative as ABI dictates).
;;
;; Slot ordering: ascending value-id.  This is deterministic across
;; runs and keeps unit tests reproducible without dragging the
;; allocator's internal interval ordering into the slot map.
;;
;; Frame size: total bytes occupied by spill slots, rounded up to a
;; 16-byte multiple (System V AMD64 §3.2.2 / AAPCS64 §6.4 stack
;; alignment requirement).  Backends prepend `[saved-fp + saved-lr]'
;; or `[saved-rbp]' as the ABI dictates *outside* this number.
;;
;; Return shape:
;;   (SLOT-ALIST . FRAME-SIZE)
;;     SLOT-ALIST is ((VALUE-ID . BYTE-OFFSET) ...) in ascending
;;     VALUE-ID order, where BYTE-OFFSET starts at 8 for the first
;;     slot, 16 for the second, etc. (positive offsets from frame
;;     base, suitable for both x86_64 `[rbp - off]' and arm64
;;     `[sp + off]' addressing — backends apply the sign).
;;     FRAME-SIZE is the total bytes (always a multiple of 16,
;;     minimum 0 when no spill).

(defun nelisp-cc--allocate-stack-slots (assignments)
  "Assign 8-byte stack slots to every `:spill' value in ASSIGNMENTS.
ASSIGNMENTS is the linear-scan output (alist (VID . REG-OR-:spill)).
Returns a cons (SLOT-ALIST . FRAME-SIZE).

SLOT-ALIST is ((VID . BYTE-OFFSET) ...) sorted by VID; BYTE-OFFSET
counts from 1 (= 8) for the first slot.  FRAME-SIZE is the total
bytes occupied (multiple of 16, possibly 0).

Backends interpret BYTE-OFFSET as the *positive distance from the
frame base*: x86_64 emits MOV [rbp - OFF], REG and MOV REG,
[rbp - OFF]; arm64 emits STR/LDR [SP + OFF].  Either way the offset
is the same magnitude — ABI sign convention is the backend's job."
  (let* ((spilled-vids (nelisp-cc--alloc-spilled-values assignments))
         (sorted (sort (copy-sequence spilled-vids) #'<))
         (offset 8)
         (slots nil))
    (dolist (vid sorted)
      (push (cons vid offset) slots)
      (cl-incf offset 8))
    (let* ((raw-size (* 8 (length sorted)))
           (frame-size
            ;; Round up to a multiple of 16 (ABI alignment).
            (* 16 (/ (+ raw-size 15) 16))))
      (cons (nreverse slots) frame-size))))

(defun nelisp-cc--stack-slot-of (slot-alist value-id)
  "Return the byte offset assigned to VALUE-ID in SLOT-ALIST, or nil.
SLOT-ALIST is the car of `nelisp-cc--allocate-stack-slots'."
  (cdr (assq value-id slot-alist)))

;;; Phase 7.1 T15 — phi resolution -----------------------------------
;;
;; The AST→SSA frontend (T6) emits `:phi' instructions in merge
;; blocks.  Backends do not lower `:phi' directly — instead T15
;; lowers them out of the SSA *before* codegen by inserting a `:copy'
;; instruction at the *end* of every predecessor block (just before
;; its terminator) that copies the incoming arm value to the phi's
;; destination value.  After resolution every `:phi' is replaced by
;; a no-op marker, which the backend can detect and skip.
;;
;; The pass intentionally does NOT re-verify the resulting function:
;; `:copy' multi-defines the phi's destination value, which violates
;; SSA single-assignment.  Post-allocation we no longer need that
;; invariant — the values are now register slots, and multiple
;; assignments-to-the-same-register are the *whole point* of phi
;; resolution.
;;
;; Parallel-copy hazard (the classical phi swap problem) is *out of
;; scope* for the MVP — we trust the linear-scan allocator to issue
;; non-conflicting allocations across phi arms.  Phase 7.1.5 graph
;; coloring will revisit and add a swap-aware ordering pass.

(defun nelisp-cc--resolve-phis (function)
  "Lower `:phi' instructions out of FUNCTION in place.

For each `:phi' instruction in block B with arms
`((PRED-BID . VID) ...)':
  1. For each (PRED-BID, VID) pair, append a `:copy' instruction at
     the end of block PRED-BID (just *before* its terminator if any
     — terminators are :branch / :jump / :return) whose operand is
     the incoming SSA value VID and whose def is the phi's def.
  2. Remove the `:phi' instruction itself from B.

After this pass FUNCTION is no longer in strict SSA form (the
phi-def value has multiple defs — one per predecessor edge).  The
function is *not* re-verified — backends that consume the post-pass
form must accept the relaxed invariant.

Return value: FUNCTION (mutated in place)."
  (let ((blocks (nelisp-cc--ssa-function-blocks function))
        ;; Look up blocks by ID for fast pred resolution.
        (block-by-id (make-hash-table :test 'eql)))
    (dolist (b blocks)
      (puthash (nelisp-cc--ssa-block-id b) b block-by-id))
    (dolist (b blocks)
      (let ((phis nil)
            (rest nil))
        ;; Partition block instructions: all :phi vs all non-:phi.
        (dolist (instr (nelisp-cc--ssa-block-instrs b))
          (if (eq (nelisp-cc--ssa-instr-opcode instr) 'phi)
              (push instr phis)
            (push instr rest)))
        ;; Drop the phi nodes; preserve original instruction order.
        (setf (nelisp-cc--ssa-block-instrs b) (nreverse rest))
        ;; For each phi, append :copy in each predecessor.
        (dolist (phi (nreverse phis))
          (let* ((meta (nelisp-cc--ssa-instr-meta phi))
                 (arms (plist-get meta :phi-arms))
                 (def (nelisp-cc--ssa-instr-def phi)))
            (dolist (arm arms)
              (let* ((pred-bid (car arm))
                     (src-vid  (cdr arm))
                     (pred-blk (gethash pred-bid block-by-id))
                     ;; Locate the source SSA value via the phi's
                     ;; operands list (operands are positional with
                     ;; :phi-arms — both share the predecessor order).
                     (operands (nelisp-cc--ssa-instr-operands phi))
                     (src-val
                      (cl-find-if
                       (lambda (v)
                         (= (nelisp-cc--ssa-value-id v) src-vid))
                       operands)))
                (unless pred-blk
                  (signal 'nelisp-cc-verify-error
                          (list :phi-pred-not-found pred-bid)))
                (unless src-val
                  (signal 'nelisp-cc-verify-error
                          (list :phi-arm-value-not-in-operands src-vid)))
                ;; Build a `:copy' instruction.  We do NOT call the
                ;; standard add-instr because that would mutate def's
                ;; def-point and add extra uses; we want a transparent
                ;; "register-level" mov that the backend lowers to a
                ;; physical MOV without touching the value-table.
                (let ((copy
                       (nelisp-cc--ssa-instr-make
                        :id (nelisp-cc--ssa-function-next-instr-id function)
                        :opcode 'copy
                        :operands (list src-val)
                        :def def
                        :block pred-blk
                        :meta (list :phi-resolution t))))
                  (cl-incf (nelisp-cc--ssa-function-next-instr-id function))
                  ;; Insert before the terminator (last :branch / :jump
                  ;; / :return).  When the block has no terminator
                  ;; instr we just append.
                  (let* ((instrs (nelisp-cc--ssa-block-instrs pred-blk))
                         (last (car (last instrs)))
                         (term-p
                          (and last
                               (memq (nelisp-cc--ssa-instr-opcode last)
                                     '(branch jump return)))))
                    (cond
                     (term-p
                      (let ((head (butlast instrs)))
                        (setf (nelisp-cc--ssa-block-instrs pred-blk)
                              (append head (list copy last)))))
                     (t
                      (setf (nelisp-cc--ssa-block-instrs pred-blk)
                            (append instrs (list copy)))))))))))))
    function))

;;; AST → SSA conversion (frontend) ---------------------------------
;;
;; This section layers a *NeLisp lambda → SSA function* lowering pass
;; on top of the data-structure substrate above.  It is the third and
;; final stage of Phase 7.1.1: scaffold (T2) → linear-scan (T4) →
;; AST→SSA conversion (T6).  After this stage the front-half of the
;; compiler pipeline is closed; `nelisp-cc-build-ssa-from-ast' takes
;; raw NeLisp source `(lambda (PARAMS...) BODY...)' and produces a
;; verified `nelisp-cc--ssa-function' that the linear-scan allocator
;; can drive without further preprocessing.
;;
;; *Supported forms* (MVP, fail-fast on anything else):
;;   - literal:    number / string / vector / nil / t / keyword
;;   - (quote X):                                    -> :const
;;   - variable reference (bare symbol):             -> :load-var
;;   - (setq SYM EXPR):                              -> :store-var
;;   - (if COND THEN ELSE):                          -> :branch + :phi
;;   - (let / let* ((VAR INIT) ...) BODY ...):       -> sequential
;;   - (lambda (PARAMS) BODY...):                    -> :closure
;;   - (function FN-EXPR):                           -> wraps lambda
;;   - (progn BODY...):                              -> sequential
;;   - (FN ARG ...):                                 -> :call
;;
;; Anything else (catch / condition-case / unwind-protect / while /
;; explicit defun outside top-level / unquote tokens / dot-pair calls)
;; raises `nelisp-cc-unsupported-form' immediately — silent miscompile
;; is a much worse failure mode than fail-fast.
;;
;; Macro expansion: when `nelisp-macroexpand-all' is loaded (the
;; production path) we run it once over the whole body so user-defined
;; macros and core macros (`when' / `unless' / `dolist' / `dotimes' /
;; `pcase' / `cl-block' ...) get rewritten to the small kernel above.
;; When `nelisp-macro' has not been loaded — typical in standalone
;; ERT runs that exercise only `nelisp-cc' — a built-in scaffold
;; expander handles the *fixed* set of derived macros (`and' / `or' /
;; `cond' / `when' / `unless') by lowering them to the supported
;; kernel.  This keeps the unit tests free of a hard dependency on
;; the larger NeLisp runtime.
;;
;; Function-call resolution is *not* performed here.  `:call'
;; instructions carry the callee symbol in their META plist
;; (`:fn SYM :args (...)`) and Phase 7.1.2 backend pulls
;; `nelisp-defs-index' (Phase 6.5) to resolve definitions.  The frontend
;; only commits a stable surface for the backend to consume.

(define-error 'nelisp-cc-unsupported-form
  "NeLisp source form not supported by the SSA frontend"
  'nelisp-cc-error)

;;; Macro expansion shim --------------------------------------------

(defun nelisp-cc--frontend-self-expand (form)
  "Recursively expand FORM using the built-in scaffold expander.
Handles `and' / `or' / `cond' / `when' / `unless' by rewriting them
into the supported kernel (`if' / `progn').  Everything else is
recurred-into structurally (preserving `quote' opacity and `lambda'
parameter lists).  This expander is intentionally minimal — the
production path layers `nelisp-macroexpand-all' on top of this."
  (cond
   ;; Atoms pass through.
   ((not (consp form)) form)
   ;; (quote DATUM) is opaque.
   ((eq (car form) 'quote) form)
   ;; (function ARG): if ARG is a lambda, recur into its body only.
   ((eq (car form) 'function)
    (let ((arg (cadr form)))
      (if (and (consp arg) (eq (car arg) 'lambda))
          (list 'function
                (cons 'lambda
                      (cons (cadr arg)
                            (mapcar #'nelisp-cc--frontend-self-expand
                                    (cddr arg)))))
        form)))
   ;; (lambda (PARAMS) BODY...): keep params literal, recur body.
   ((eq (car form) 'lambda)
    (cons 'lambda
          (cons (cadr form)
                (mapcar #'nelisp-cc--frontend-self-expand (cddr form)))))
   ;; (let / let* ((VAR INIT) ...) BODY...): recur init forms + body.
   ((memq (car form) '(let let*))
    (let* ((head (car form))
           (bindings (cadr form))
           (body (cddr form))
           (new-bindings
            (mapcar (lambda (b)
                      (cond
                       ((symbolp b) b)
                       ((and (consp b) (consp (cdr b)))
                        (list (car b)
                              (nelisp-cc--frontend-self-expand (cadr b))))
                       (t b)))
                    bindings)))
      (cons head (cons new-bindings
                       (mapcar #'nelisp-cc--frontend-self-expand body)))))
   ;; (and BODY...) → nested `if': empty → t, single → expr,
   ;; (and a b c) → (if a (if b c nil) nil).
   ((eq (car form) 'and)
    (let ((args (cdr form)))
      (cond
       ((null args) t)
       ((null (cdr args)) (nelisp-cc--frontend-self-expand (car args)))
       (t (list 'if
                (nelisp-cc--frontend-self-expand (car args))
                (nelisp-cc--frontend-self-expand (cons 'and (cdr args)))
                nil)))))
   ;; (or BODY...) → nested `if': empty → nil, single → expr,
   ;; (or a b ...) → (let ((g a)) (if g g (or ...))).  We use a
   ;; gensym to avoid double-evaluating the test.
   ((eq (car form) 'or)
    (let ((args (cdr form)))
      (cond
       ((null args) nil)
       ((null (cdr args)) (nelisp-cc--frontend-self-expand (car args)))
       (t (let ((g (make-symbol "or-tmp")))
            (list 'let (list (list g (nelisp-cc--frontend-self-expand
                                      (car args))))
                  (list 'if g g
                        (nelisp-cc--frontend-self-expand
                         (cons 'or (cdr args))))))))))
   ;; (when COND BODY...) → (if COND (progn BODY...) nil)
   ((eq (car form) 'when)
    (let ((c (cadr form))
          (body (cddr form)))
      (list 'if
            (nelisp-cc--frontend-self-expand c)
            (cons 'progn (mapcar #'nelisp-cc--frontend-self-expand body))
            nil)))
   ;; (unless COND BODY...) → (if COND nil (progn BODY...))
   ((eq (car form) 'unless)
    (let ((c (cadr form))
          (body (cddr form)))
      (list 'if
            (nelisp-cc--frontend-self-expand c)
            nil
            (cons 'progn (mapcar #'nelisp-cc--frontend-self-expand body)))))
   ;; (cond CLAUSE...) — empty → nil.  Each clause (TEST BODY...).
   ;; Special case: (TEST) with no body becomes (or TEST <rest>).
   ((eq (car form) 'cond)
    (nelisp-cc--frontend-cond-expand (cdr form)))
   ;; (setq SYM EXPR ...): every odd cdr position is a value form.
   ((eq (car form) 'setq)
    (let ((rest (cdr form))
          (out nil))
      (while rest
        (push (car rest) out)
        (setq rest (cdr rest))
        (when rest
          (push (nelisp-cc--frontend-self-expand (car rest)) out)
          (setq rest (cdr rest))))
      (cons 'setq (nreverse out))))
   ;; Default: (HEAD ARG ...) — recur into every arg position.
   (t (cons (car form)
            (mapcar #'nelisp-cc--frontend-self-expand (cdr form))))))

(defun nelisp-cc--frontend-cond-expand (clauses)
  "Helper: expand a list of `cond' CLAUSES into nested `if' forms."
  (cond
   ((null clauses) nil)
   (t
    (let* ((c (car clauses))
           (rest (cdr clauses)))
      (cond
       ((not (consp c))
        (signal 'nelisp-cc-unsupported-form
                (list :bad-cond-clause c)))
       ((null (cdr c))
        ;; (TEST) with empty body — value of TEST when truthy, else
        ;; recurse.  We model this with `(let ((g TEST)) (if g g
        ;; <rest>))' to match Elisp semantics (don't evaluate TEST
        ;; twice).
        (let ((g (make-symbol "cond-tmp")))
          (list 'let
                (list (list g (nelisp-cc--frontend-self-expand (car c))))
                (list 'if g g
                      (nelisp-cc--frontend-cond-expand rest)))))
       (t
        (list 'if
              (nelisp-cc--frontend-self-expand (car c))
              (cons 'progn
                    (mapcar #'nelisp-cc--frontend-self-expand (cdr c)))
              (nelisp-cc--frontend-cond-expand rest))))))))

(defun nelisp-cc--frontend-expand (form)
  "Run macro expansion over FORM before SSA lowering.
When `nelisp-macroexpand-all' is loaded, defer to it (covers user-
defined macros).  Otherwise fall back to the scaffold expander.  The
result is always re-walked through the scaffold so derived kernel
forms (`when' / `unless' / `cond' / etc.) are guaranteed to be
collapsed into the small set the lowering pass natively supports."
  (let ((expanded
         (if (fboundp 'nelisp-macroexpand-all)
             (funcall (symbol-function 'nelisp-macroexpand-all) form)
           form)))
    (nelisp-cc--frontend-self-expand expanded)))

;;; Scope helpers ---------------------------------------------------

(defun nelisp-cc--scope-extend (scope bindings)
  "Return SCOPE prepended with BINDINGS (an alist of (SYM . SSA-VALUE)).
The original SCOPE is not mutated.  Lookups via `assq' will find the
freshest binding first because new entries are pushed onto the head."
  (append bindings scope))

(defun nelisp-cc--scope-lookup (scope sym)
  "Return the SSA value bound to SYM in SCOPE, or nil if free.
Free variables (no scope entry) are emitted as `:load-var' by the
caller — a free variable is not an error, it is a deferred resolution
the backend completes against `nelisp-defs-index'."
  (cdr (assq sym scope)))

;;; Builder API ------------------------------------------------------

(defun nelisp-cc--fresh-block-id (function)
  "Return the next block ID to be issued by FUNCTION.
This is a peek — the counter is not advanced.  Real allocation goes
through `nelisp-cc--ssa-make-block', which advances the counter."
  (nelisp-cc--ssa-function-next-block-id function))

(defun nelisp-cc--emit-phi (fn block predecessors values &optional type)
  "Append a `phi' instruction to BLOCK in FN with parallel PREDECESSORS / VALUES.
PREDECESSORS is a list of `nelisp-cc--ssa-block', and VALUES is the
matching list of `nelisp-cc--ssa-value' (one per predecessor).  The
instruction's META plist records `:phi-arms' as the alist
`((PRED-BLOCK-ID . VALUE-ID) ...)' so backend lowering knows which
incoming edge feeds each operand without rebuilding the mapping."
  (unless (= (length predecessors) (length values))
    (signal 'nelisp-cc-verify-error
            (list :phi-mismatch (length predecessors) (length values))))
  (let* ((def (nelisp-cc--ssa-make-value fn type))
         (instr (nelisp-cc--ssa-add-instr fn block 'phi values def))
         (arms (cl-mapcar (lambda (p v)
                            (cons (nelisp-cc--ssa-block-id p)
                                  (nelisp-cc--ssa-value-id v)))
                          predecessors values)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :phi-arms arms))
    def))

;;; Lowering ---------------------------------------------------------

;; The lowering helpers thread three pieces of mutable state:
;;   - FN     : the `nelisp-cc--ssa-function' under construction
;;   - BLOCK  : the *current* block we are appending into.  Control-
;;              flow forms (if / let in some shapes) replace this with
;;              a fresh merge block.  The lowering helpers therefore
;;              return *both* the result SSA value *and* the block we
;;              ended up in — callers thread that block forward.
;;   - SCOPE  : the lexical environment alist (see scope-extend).
;;
;; Every helper has the contract:  (SSA-VALUE BLOCK)
;; — i.e. a 2-element list where the first item is the SSA value
;; carrying the result of the form (for void forms, the literal
;; `nil-value' constant) and the second is the block control flow
;; reached after evaluation.

(defun nelisp-cc--literal-p (form)
  "Return non-nil when FORM is a self-evaluating literal."
  (or (numberp form)
      (stringp form)
      (vectorp form)
      (null form)
      (eq form t)
      (keywordp form)))

(defun nelisp-cc--lower-const (fn block literal &optional type)
  "Emit `:const' for LITERAL in BLOCK and return (VALUE . BLOCK)."
  (let* ((v (nelisp-cc--ssa-make-value fn type))
         (instr (nelisp-cc--ssa-add-instr fn block 'const nil v)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :literal literal))
    (list v block)))

(defun nelisp-cc--lower-load-var (fn block sym)
  "Emit `:load-var' for free variable SYM in BLOCK."
  (let* ((v (nelisp-cc--ssa-make-value fn nil))
         (instr (nelisp-cc--ssa-add-instr fn block 'load-var nil v)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :name sym))
    (list v block)))

(defun nelisp-cc--lower-store-var (fn block scope sym expr)
  "Lower (setq SYM EXPR) in BLOCK with SCOPE."
  (cl-destructuring-bind (val block2)
      (nelisp-cc--lower-expr fn block scope expr)
    (let* ((def (nelisp-cc--ssa-make-value fn nil))
           (instr (nelisp-cc--ssa-add-instr fn block2 'store-var
                                            (list val) def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            (list :name sym))
      (list def block2))))

(defun nelisp-cc--lower-progn (fn block scope body)
  "Lower a sequence BODY in BLOCK; return last value + final block."
  (cond
   ((null body)
    ;; Empty progn evaluates to nil.
    (nelisp-cc--lower-const fn block nil))
   (t
    (let ((cur-block block)
          (result nil)
          (forms body))
      (while forms
        (cl-destructuring-bind (val nb)
            (nelisp-cc--lower-expr fn cur-block scope (car forms))
          (setq result val
                cur-block nb
                forms (cdr forms))))
      (list result cur-block)))))

(defun nelisp-cc--lower-if (fn block scope cond-form then-form else-form)
  "Lower (if COND-FORM THEN-FORM ELSE-FORM) in BLOCK with SCOPE.
Allocates a then-block, an else-block, and a merge-block; emits a
`:branch' terminator on the entry block, recurses into THEN/ELSE,
and merges via `:phi' on the merge-block.  Returns (PHI-VALUE
MERGE-BLOCK)."
  (cl-destructuring-bind (cval cblock)
      (nelisp-cc--lower-expr fn block scope cond-form)
    (let* ((then-blk (nelisp-cc--ssa-make-block fn "then"))
           (else-blk (nelisp-cc--ssa-make-block fn "else"))
           (merge-blk (nelisp-cc--ssa-make-block fn "merge")))
      ;; Branch terminator on the predecessor block.
      (let ((br (nelisp-cc--ssa-add-instr fn cblock 'branch (list cval) nil)))
        (setf (nelisp-cc--ssa-instr-meta br)
              (list :then (nelisp-cc--ssa-block-id then-blk)
                    :else (nelisp-cc--ssa-block-id else-blk))))
      (nelisp-cc--ssa-link-blocks cblock then-blk)
      (nelisp-cc--ssa-link-blocks cblock else-blk)
      ;; Lower then-arm.
      (cl-destructuring-bind (tval tblock)
          (nelisp-cc--lower-expr fn then-blk scope then-form)
        (nelisp-cc--ssa-add-instr fn tblock 'jump nil nil)
        (nelisp-cc--ssa-link-blocks tblock merge-blk)
        ;; Lower else-arm.
        (cl-destructuring-bind (eval-val eblock)
            (nelisp-cc--lower-expr fn else-blk scope else-form)
          (nelisp-cc--ssa-add-instr fn eblock 'jump nil nil)
          (nelisp-cc--ssa-link-blocks eblock merge-blk)
          ;; phi on merge-blk.
          (let ((phi (nelisp-cc--emit-phi
                      fn merge-blk
                      (list tblock eblock)
                      (list tval eval-val))))
            (list phi merge-blk)))))))

(defun nelisp-cc--lower-let (fn block scope bindings body)
  "Lower `(let ((VAR INIT) ...) BODY...)' in BLOCK with SCOPE.
INIT forms are evaluated in the *outer* SCOPE (Elisp `let' semantics);
BODY is evaluated in SCOPE extended with all (VAR . SSA-VALUE) pairs.

T84 Phase 7.5 wire — when VAR appears as a `setq' target anywhere in
BODY (the AST-level walker reuses
`nelisp-cc--source-symbol-setq-p'), additionally emit a `:store-var
:name VAR :let-init t' instruction so the post-codegen rewriter
sees the binding's origin VID.  Pre-T84 ERT pinned the let-binding
SSA to `(const call return)' (no store-var) — keeping the conditional
emit preserves those golden tests when the body never `setq's VAR."
  (let ((cur-block block)
        (new-bindings nil))
    (dolist (b bindings)
      (let ((var nil) (init nil))
        (cond
         ((symbolp b) (setq var b init nil))
         ((and (consp b) (symbolp (car b)) (consp (cdr b)))
          (setq var (car b) init (cadr b)))
         (t (signal 'nelisp-cc-unsupported-form
                    (list :bad-let-binding b))))
        (cl-destructuring-bind (val nb)
            (nelisp-cc--lower-expr fn cur-block scope init)
          (setq cur-block nb)
          ;; T84 — only emit let-init store-var when the body does
          ;; `(setq VAR ...)'.  This keeps the const-fold ERT golden
          ;; tests stable while still surfacing the origin VID for
          ;; loops that mutate the binding.
          (when (nelisp-cc--source-symbol-setq-p var body)
            (let* ((store-def (nelisp-cc--ssa-make-value fn nil))
                   (store-instr (nelisp-cc--ssa-add-instr
                                 fn cur-block 'store-var
                                 (list val) store-def)))
              (setf (nelisp-cc--ssa-instr-meta store-instr)
                    (list :name var :let-init t))))
          (push (cons var val) new-bindings))))
    (nelisp-cc--lower-progn
     fn cur-block
     (nelisp-cc--scope-extend scope (nreverse new-bindings))
     body)))

(defun nelisp-cc--source-symbol-setq-p (sym forms)
  "Return non-nil when any FORM in FORMS contains `(setq SYM ...)' or
`(setq ... SYM ...)' as a syntactic subform.  T84 Phase 7.5 wire —
used by `--lower-let' to decide whether to emit a let-init
store-var marker.

The walk is a structural recursion that does *not* expand macros —
the frontend pipeline expands first via `nelisp-cc--frontend-expand'
so any setq introduced by macroexpansion has already surfaced into
the lowered form."
  (catch 'found
    (nelisp-cc--symbol-setq-walk sym forms)
    nil))

(defun nelisp-cc--symbol-setq-walk (sym tree)
  "Helper for `--source-symbol-setq-p' — throw `'found' when SYM is
the target of a `setq' anywhere within TREE."
  (cond
   ((null tree) nil)
   ((not (consp tree)) nil)
   ((eq (car tree) 'setq)
    (let ((rest (cdr tree)))
      (while (and rest (cdr rest))
        (when (eq (car rest) sym)
          (throw 'found t))
        (nelisp-cc--symbol-setq-walk sym (cadr rest))
        (setq rest (cddr rest)))))
   (t
    (dolist (sub tree)
      (nelisp-cc--symbol-setq-walk sym sub)))))

(defun nelisp-cc--lower-let* (fn block scope bindings body)
  "Lower `(let* ((VAR INIT) ...) BODY...)' — sequential (each INIT sees prior).
T84 Phase 7.5 wire — same conditional `:let-init' store-var as
`--lower-let'."
  (let ((cur-block block)
        (cur-scope scope))
    (dolist (b bindings)
      (let ((var nil) (init nil))
        (cond
         ((symbolp b) (setq var b init nil))
         ((and (consp b) (symbolp (car b)) (consp (cdr b)))
          (setq var (car b) init (cadr b)))
         (t (signal 'nelisp-cc-unsupported-form
                    (list :bad-let*-binding b))))
        (cl-destructuring-bind (val nb)
            (nelisp-cc--lower-expr fn cur-block cur-scope init)
          (when (nelisp-cc--source-symbol-setq-p var body)
            (let* ((store-def (nelisp-cc--ssa-make-value fn nil))
                   (store-instr (nelisp-cc--ssa-add-instr
                                 fn nb 'store-var
                                 (list val) store-def)))
              (setf (nelisp-cc--ssa-instr-meta store-instr)
                    (list :name var :let-init t))))
          (setq cur-block nb
                cur-scope (nelisp-cc--scope-extend
                           cur-scope (list (cons var val)))))))
    (nelisp-cc--lower-progn fn cur-block cur-scope body)))

(defun nelisp-cc--lower-letrec (fn block scope bindings body)
  "Lower `(letrec ((NAME INIT) ...) BODY...)' in BLOCK with SCOPE.
T38 Phase 7.5.5 — mutually recursive bindings.

Strategy (allocate-then-fill, matching Emacs Lisp `letrec' semantics):
  1. For each (NAME INIT) emit a `:const' nil placeholder, capture its
     SSA value as the initial binding for NAME, and emit a `:store-var'
     so the slot exists in the symbol-value cell when an INIT closure
     references NAME at runtime.
  2. Extend SCOPE so every NAME is visible to every INIT (fixes the
     classic mutual-recursion case `(letrec ((f (lambda () (g)))
     (g (lambda () (f)))) ...)').
  3. Evaluate each INIT in the extended scope and emit `:store-var' for
     NAME to update the cell to the final value; the SSA scope binding
     is replaced with the new value so subsequent BODY references see
     the resolved binding (rather than the placeholder nil).
  4. Lower BODY as an implicit progn with the extended scope.

Bindings without an INIT are accepted (`(letrec (x) BODY)') and bind
NAME to nil — same shape as `let'."
  (let ((cur-block block)
        (placeholders nil))
    ;; Pass 1: allocate placeholder nil + initial store-var per NAME.
    ;; Each NAME starts bound to its placeholder SSA value so that
    ;; pass-2 evaluation of its INIT can see itself + every sibling.
    (dolist (b bindings)
      (let ((var nil))
        (cond
         ((symbolp b) (setq var b))
         ((and (consp b) (symbolp (car b)) (consp (cdr b)))
          (setq var (car b)))
         (t (signal 'nelisp-cc-unsupported-form
                    (list :bad-letrec-binding b))))
        (cl-destructuring-bind (placeholder nb)
            (nelisp-cc--lower-const fn cur-block nil)
          (setq cur-block nb)
          ;; Emit a store-var so the symbol-value cell holds nil for
          ;; the placeholder window — keeps backend semantics simple
          ;; (free-variable references to NAME during INIT walk read
          ;; nil rather than an undefined slot).
          (let* ((store-def (nelisp-cc--ssa-make-value fn nil))
                 (store-instr (nelisp-cc--ssa-add-instr
                               fn cur-block 'store-var
                               (list placeholder) store-def)))
            (setf (nelisp-cc--ssa-instr-meta store-instr)
                  (list :name var :letrec-placeholder t))
            (push (cons var store-def) placeholders)))))
    (let ((scope-with-placeholders
           (nelisp-cc--scope-extend scope (nreverse placeholders)))
          (final-bindings nil))
      ;; Pass 2: evaluate each INIT in the extended scope, store-var
      ;; the result, replace the binding with the final SSA value.
      (dolist (b bindings)
        (let ((var nil) (init nil))
          (cond
           ((symbolp b) (setq var b init nil))
           ((and (consp b) (symbolp (car b)) (consp (cdr b)))
            (setq var (car b) init (cadr b))))
          (cl-destructuring-bind (val nb)
              (nelisp-cc--lower-expr fn cur-block
                                     scope-with-placeholders init)
            (setq cur-block nb)
            (let* ((store-def (nelisp-cc--ssa-make-value fn nil))
                   (store-instr (nelisp-cc--ssa-add-instr
                                 fn cur-block 'store-var
                                 (list val) store-def)))
              (setf (nelisp-cc--ssa-instr-meta store-instr)
                    (list :name var :letrec-init t))
              (push (cons var store-def) final-bindings)))))
      ;; Pass 3: lower BODY with NAMES bound to their *final* SSA
      ;; values.  We rebuild the scope rather than mutating the
      ;; placeholder alist so two distinct lookups (during the
      ;; placeholder window vs. after) return different values, which
      ;; preserves SSA single-assignment semantics.
      (let ((final-scope
             (nelisp-cc--scope-extend scope (nreverse final-bindings))))
        (nelisp-cc--lower-progn fn cur-block final-scope body)))))

(defun nelisp-cc--lower-funcall (fn block scope fn-form args)
  "Lower `(funcall FN-FORM ARGS...)' as `:call-indirect'.
T38 Phase 7.5.5 — first-class function call.

FN-FORM is lowered first (its result is the callee function pointer),
then ARGS are lowered left-to-right and threaded as the remaining
operands.  The emitted instruction's META plist carries `:indirect t'
so the backend can branch on indirect vs. direct call.

This shares the `:call-indirect' opcode with the existing `((lambda
...) ARG)' indirect-call path in `--lower-expr', so backend lowering
only has to handle one indirect-call shape."
  (cl-destructuring-bind (fn-val fb)
      (nelisp-cc--lower-expr fn block scope fn-form)
    (let ((cur-block fb)
          (operand-vals nil))
      (dolist (a args)
        (cl-destructuring-bind (v nb)
            (nelisp-cc--lower-expr fn cur-block scope a)
          (setq cur-block nb)
          (push v operand-vals)))
      (let* ((operands (cons fn-val (nreverse operand-vals)))
             (def (nelisp-cc--ssa-make-value fn nil))
             (instr (nelisp-cc--ssa-add-instr
                     fn cur-block 'call-indirect operands def)))
        (setf (nelisp-cc--ssa-instr-meta instr)
              (list :indirect t :funcall t))
        (list def cur-block)))))

(defun nelisp-cc--lower-while (fn block scope cond-form body)
  "Lower `(while COND-FORM BODY...)' in BLOCK with SCOPE.
T38 Phase 7.5.5 — loop with explicit back-edge.

Three basic blocks are allocated:

  loop-header   COND-FORM evaluated → :branch (then=loop-body,
                                              else=loop-exit)
  loop-body     BODY... evaluated, last value discarded → :jump back
                to loop-header (the back-edge)
  loop-exit     :const nil — the form's value (Emacs Lisp `while'
                always returns nil)

The current BLOCK is wired with an unconditional `:jump' to
loop-header so straight-line dataflow reaches the loop entry — this
keeps the verifier happy (loop-header has at least one predecessor)
and gives the linear-scan allocator a deterministic linearisation
point.

Returns `(NIL-VALUE LOOP-EXIT-BLOCK)' as the standard 2-element
contract.  Caveat: the linearisation pass
`nelisp-cc--ssa--reverse-postorder' deliberately ignores the back-edge
(the visited bitmap prevents a re-visit of loop-header), which is the
Phase 7.1.5-deferred loop-extension behaviour Doc 28 §3.1 calls out.
The interval allocator therefore truncates each loop-carried value's
END to its in-linear-order last use; live values that cross the
back-edge spill at the cost of correctness preservation only when T15
spill-aware reload kicks in.  The fib / fact-iter / alloc-heavy bench
exercises this on real silicon."
  (let ((header (nelisp-cc--ssa-make-block fn "while-header"))
        (body-blk (nelisp-cc--ssa-make-block fn "while-body"))
        (exit-blk (nelisp-cc--ssa-make-block fn "while-exit")))
    ;; Wire BLOCK → header with an unconditional jump.
    (nelisp-cc--ssa-add-instr fn block 'jump nil nil)
    (nelisp-cc--ssa-link-blocks block header)
    ;; loop-header: lower COND-FORM, emit branch.
    (cl-destructuring-bind (cval cblock)
        (nelisp-cc--lower-expr fn header scope cond-form)
      (let ((br (nelisp-cc--ssa-add-instr
                 fn cblock 'branch (list cval) nil)))
        (setf (nelisp-cc--ssa-instr-meta br)
              (list :then (nelisp-cc--ssa-block-id body-blk)
                    :else (nelisp-cc--ssa-block-id exit-blk)
                    :loop-header t))
        (nelisp-cc--ssa-link-blocks cblock body-blk)
        (nelisp-cc--ssa-link-blocks cblock exit-blk)))
    ;; loop-body: lower BODY (discard result), then jump back to
    ;; header — this is the back-edge.
    (let* ((body-result (nelisp-cc--lower-progn fn body-blk scope body))
           (bblock (cadr body-result)))
      ;; Body's last value is discarded by `while' semantics.
      (let ((j (nelisp-cc--ssa-add-instr fn bblock 'jump nil nil)))
        (setf (nelisp-cc--ssa-instr-meta j)
              (list :loop-back-edge t)))
      (nelisp-cc--ssa-link-blocks bblock header))
    ;; loop-exit: materialise nil as the form's value.
    (nelisp-cc--lower-const fn exit-blk nil)))

(defun nelisp-cc--lower-call (fn block scope head args)
  "Lower a function call (HEAD ARGS...) in BLOCK with SCOPE."
  (let ((cur-block block)
        (operand-vals nil))
    (dolist (a args)
      (cl-destructuring-bind (v nb)
          (nelisp-cc--lower-expr fn cur-block scope a)
        (setq cur-block nb)
        (push v operand-vals)))
    (let* ((operands (nreverse operand-vals))
           (def (nelisp-cc--ssa-make-value fn nil))
           (instr (nelisp-cc--ssa-add-instr fn cur-block 'call operands def)))
      (setf (nelisp-cc--ssa-instr-meta instr)
            (list :fn head :unresolved t))
      (list def cur-block))))

(defun nelisp-cc--lower-lambda (fn block lambda-form)
  "Lower a nested `(lambda (PARAMS) BODY...)' — recurses into a fresh SSA fn."
  (let* ((inner (nelisp-cc-build-ssa-from-ast lambda-form))
         (def (nelisp-cc--ssa-make-value fn nil))
         (instr (nelisp-cc--ssa-add-instr fn block 'closure nil def)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :inner-function inner))
    (list def block)))

(defun nelisp-cc--lower-expr (fn block scope expr)
  "Lower EXPR into FN's BLOCK with SCOPE; return (SSA-VALUE END-BLOCK).
This is the master dispatch.  Unknown / unsupported forms raise
`nelisp-cc-unsupported-form' immediately."
  (cond
   ;; Self-evaluating literals.
   ((nelisp-cc--literal-p expr)
    (nelisp-cc--lower-const fn block expr))
   ;; Bare symbol — variable reference.
   ((symbolp expr)
    (let ((bound (nelisp-cc--scope-lookup scope expr)))
      (if bound
          (list bound block)
        (nelisp-cc--lower-load-var fn block expr))))
   ;; Compound forms — dispatch on car.
   ((consp expr)
    (let ((head (car expr))
          (rest (cdr expr)))
      (cond
       ((eq head 'quote)
        (nelisp-cc--lower-const fn block (car rest)))
       ((eq head 'function)
        (let ((arg (car rest)))
          (cond
           ((and (consp arg) (eq (car arg) 'lambda))
            (nelisp-cc--lower-lambda fn block arg))
           ((symbolp arg)
            ;; Treat (function SYM) as a const symbol reference for
            ;; backend resolution.
            (nelisp-cc--lower-const fn block arg))
           (t (signal 'nelisp-cc-unsupported-form
                      (list :bad-function arg))))))
       ((eq head 'lambda)
        (nelisp-cc--lower-lambda fn block expr))
       ((eq head 'progn)
        (nelisp-cc--lower-progn fn block scope rest))
       ((eq head 'if)
        (let ((c (nth 0 rest))
              (th (nth 1 rest))
              (el (nth 2 rest)))
          (nelisp-cc--lower-if fn block scope c th el)))
       ((eq head 'let)
        (nelisp-cc--lower-let fn block scope (car rest) (cdr rest)))
       ((eq head 'let*)
        (nelisp-cc--lower-let* fn block scope (car rest) (cdr rest)))
       ((eq head 'letrec)
        ;; T38 Phase 7.5.5: mutually recursive bindings.
        (nelisp-cc--lower-letrec fn block scope (car rest) (cdr rest)))
       ((eq head 'while)
        ;; T38 Phase 7.5.5: loop with explicit back-edge.
        (let ((cond-form (car rest))
              (body (cdr rest)))
          (nelisp-cc--lower-while fn block scope cond-form body)))
       ((eq head 'funcall)
        ;; T38 Phase 7.5.5: first-class function call.  `(funcall FN
        ;; ARG...)' lowers FN to an SSA value and emits
        ;; `:call-indirect' with the function pointer as operand[0].
        ;; Empty `(funcall)' is malformed (Emacs Lisp signals
        ;; `wrong-number-of-arguments' at run time); we treat it as a
        ;; frontend error to avoid emitting a degenerate instruction.
        (unless rest
          (signal 'nelisp-cc-unsupported-form
                  (list :empty-funcall expr)))
        (nelisp-cc--lower-funcall fn block scope (car rest) (cdr rest)))
       ((eq head 'setq)
        (nelisp-cc--lower-setq-chain fn block scope rest))
       ((memq head '(catch condition-case unwind-protect
                           save-excursion save-restriction
                           save-current-buffer
                           defun defmacro defvar defconst
                           lambda*))
        (signal 'nelisp-cc-unsupported-form
                (list :head head :form expr)))
       ((symbolp head)
        (nelisp-cc--lower-call fn block scope head rest))
       (t
        ;; (FN-EXPR ARG...) where FN-EXPR is itself a non-symbol form
        ;; (e.g. ((lambda ...) ARG)) — supported by lowering FN-EXPR
        ;; first and emitting an indirect call.
        (cl-destructuring-bind (fn-val fb)
            (nelisp-cc--lower-expr fn block scope head)
          (let ((cur-block fb)
                (operand-vals nil))
            (dolist (a rest)
              (cl-destructuring-bind (v nb)
                  (nelisp-cc--lower-expr fn cur-block scope a)
                (setq cur-block nb)
                (push v operand-vals)))
            (let* ((operands (cons fn-val (nreverse operand-vals)))
                   (def (nelisp-cc--ssa-make-value fn nil))
                   (instr (nelisp-cc--ssa-add-instr
                           fn cur-block 'call-indirect operands def)))
              (setf (nelisp-cc--ssa-instr-meta instr)
                    (list :indirect t))
              (list def cur-block))))))))
   (t
    (signal 'nelisp-cc-unsupported-form
            (list :unknown-form expr)))))

(defun nelisp-cc--lower-setq-chain (fn block scope pairs)
  "Lower `(setq SYM1 V1 SYM2 V2 ...)' as a sequence of stores.
The result of the form is the value of the *last* assignment, matching
Elisp `setq' semantics.  An empty `(setq)' is `nil'."
  (cond
   ((null pairs)
    (nelisp-cc--lower-const fn block nil))
   (t
    (let ((cur-block block)
          (result nil)
          (rest pairs))
      (while rest
        (let ((sym (car rest))
              (val (cadr rest)))
          (unless (and (symbolp sym) (consp (cdr rest)))
            (signal 'nelisp-cc-unsupported-form
                    (list :bad-setq-chain rest)))
          (cl-destructuring-bind (v nb)
              (nelisp-cc--lower-store-var fn cur-block scope sym val)
            (setq result v
                  cur-block nb
                  rest (cddr rest)))))
      (list result cur-block)))))

;;; Public entry ----------------------------------------------------

(defun nelisp-cc--lambda-extract-params (param-list)
  "Return (CLEAN-PARAMS . PARAM-TYPES) from PARAM-LIST.
For now &optional / &rest are *passed through* verbatim — they remain
in CLEAN-PARAMS so backend ABI lowering can see them, and PARAM-TYPES
is a list of nils of equal length so every positional slot gets an SSA
value placeholder.  The marker symbols `&optional' / `&rest' are
themselves represented as scope entries pointing to a `:const' nil
value — the lowering pass never *reads* them, only the param list
position matters for ABI lowering."
  (let ((clean nil)
        (types nil))
    (dolist (p param-list)
      (cond
       ((memq p '(&optional &rest))
        ;; Skip ABI markers — they consume no argument slot.
        nil)
       ((symbolp p)
        (push p clean)
        (push nil types))
       (t (signal 'nelisp-cc-unsupported-form
                  (list :bad-param p)))))
    (cons (nreverse clean) (nreverse types))))

(defun nelisp-cc-build-ssa-from-ast (lambda-form)
  "Convert NeLisp LAMBDA-FORM into a verified `nelisp-cc--ssa-function'.

LAMBDA-FORM is `(lambda (PARAMS...) BODY...)' (the `function'
wrapper, if present, is unwrapped automatically).  The result is a
fully built SSA function that
  - has a single entry block plus enough subordinate blocks to encode
    every `if' branch / merge,
  - terminates in a `:return' instruction whose operand carries the
    last expression's value,
  - passes `nelisp-cc--ssa-verify-function' (the caller may rely on
    this).

Macro expansion is performed via `nelisp-macroexpand-all' (when
loaded) plus a built-in scaffold expander (see
`nelisp-cc--frontend-self-expand').  Unsupported forms raise
`nelisp-cc-unsupported-form' rather than miscompiling silently."
  ;; Tolerate (function (lambda ...)).
  (when (and (consp lambda-form)
             (eq (car lambda-form) 'function)
             (consp (cdr lambda-form))
             (consp (cadr lambda-form))
             (eq (car (cadr lambda-form)) 'lambda))
    (setq lambda-form (cadr lambda-form)))
  (unless (and (consp lambda-form)
               (eq (car lambda-form) 'lambda)
               (consp (cdr lambda-form)))
    (signal 'nelisp-cc-unsupported-form
            (list :not-a-lambda lambda-form)))
  (let* ((param-list (cadr lambda-form))
         (body (cddr lambda-form))
         (parsed (nelisp-cc--lambda-extract-params param-list))
         (clean-params (car parsed))
         (param-types (cdr parsed))
         (fn (nelisp-cc--ssa-make-function nil param-types))
         (entry (nelisp-cc--ssa-function-entry fn))
         (initial-scope
          (cl-mapcar #'cons clean-params
                     (nelisp-cc--ssa-function-params fn)))
         (expanded-body
          (mapcar #'nelisp-cc--frontend-expand body)))
    ;; Lower body as an implicit progn; if empty, the lambda evaluates
    ;; to nil.
    (cl-destructuring-bind (rval rblock)
        (if expanded-body
            (nelisp-cc--lower-progn fn entry initial-scope expanded-body)
          (nelisp-cc--lower-const fn entry nil))
      (nelisp-cc--ssa-add-instr fn rblock 'return (list rval) nil))
    ;; Final invariant check — a fail here is a frontend bug.
    (nelisp-cc--ssa-verify-function fn)
    fn))

(provide 'nelisp-cc)
;;; nelisp-cc.el ends here
