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
later passes do not have to allocate a fresh struct."
  (value nil :read-only t)
  (start 0 :read-only t)
  (end 0)
  (uses nil))

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
        (intervals (make-hash-table :test 'eql))) ; vid -> interval
    ;; Seed parameter intervals at position 0.
    (dolist (p (nelisp-cc--ssa-function-params fn))
      (puthash (nelisp-cc--ssa-value-id p)
               (nelisp-cc--ssa-interval-make :value p :start 0 :end 0)
               intervals))
    ;; First pass: assign linear positions to every instruction in the
    ;; reverse-postorder block stream, and seed an interval for every
    ;; def value at its def position.
    (dolist (blk rpo)
      (dolist (instr (nelisp-cc--ssa-block-instrs blk))
        (puthash instr pos instr-pos)
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
    if length(ACTIVE) == length(REGISTERS):
       SPILLATINTERVAL(i)
    else:
       register := free.pop()
       assignments[i.vid] := register
       ACTIVE.insert(i, sorted by END)

The scaffold extends nothing beyond the paper; loop-induced live
ranges are *not* widened.  Phase 7.1.5 graph-coloring will revisit."
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

(provide 'nelisp-cc)
;;; nelisp-cc.el ends here
