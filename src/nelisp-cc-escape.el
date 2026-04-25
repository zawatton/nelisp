;;; nelisp-cc-escape.el --- Escape analysis (Doc 42 §3.1, Phase 7.7.1) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.7.1 prototype — *escape analysis* for the SSA IR produced
;; by `src/nelisp-cc.el' (Phase 7.1 SSA scaffold).  Doc 42 LOCKED v2
;; §3.1 specifies this pass as the prerequisite for Phase 7.7.2-7.7.4
;; (simple inline / recursive inline / lambda lift): without an escape
;; verdict per SSA value the inliner cannot know whether a captured
;; binding is safe to scalarise or whether a closure environment is
;; safe to drop.
;;
;; This is a *standalone analysis pass* — no codegen change.  The
;; deliverable is a hashmap VALUE-ID → escape-kind that downstream
;; passes (or `nelisp-cc-escape-info-can-stack-allocate-p' /
;; `nelisp-cc-escape-info-can-inline-p' helpers) consult.
;;
;; Escape kinds:
;;
;;   :stack-only      — value never reaches an instruction that exposes
;;                      it to other frames.  Safe for stack allocation
;;                      and for SROA inside the inliner.
;;
;;   :heap-required   — value flows into one of the *escape sources*
;;                      below and therefore must remain heap-allocated
;;                      (or, equivalently, the inliner must leave its
;;                      defining instruction in place).
;;
;;   :unknown         — analysis ran into a construct whose escape
;;                      semantics it cannot prove statically (default
;;                      conservative class C of Doc 42 §2.4).  Treated
;;                      identically to :heap-required by the helpers
;;                      when `nelisp-cc-escape-conservative' is t.
;;
;; Escape *sources* — instructions whose operands escape:
;;
;;   `return'                — operand flows out of the function frame.
;;   `closure'               — captured into a closure object whose
;;                             lifetime exceeds the current frame.
;;   `store-var'             — stored into a mutable variable cell
;;                             (semantically heap-resident).
;;   `call' / `call-indirect'— passed to an opaque function the analysis
;;                             cannot inspect.  Unless overridden by a
;;                             leaf-side annotation, treated as escape.
;;
;; Construct flagged :unknown:
;;
;;   - operand whose def-point is not :param and not in any block of
;;     the function (= dangling reference, normally a verifier bug).
;;
;; The pass is a single fix-point walk over def-use chains:
;;   1. seed every value's class as :stack-only
;;   2. walk every instruction; for each escape source, mark each
;;      operand value as :heap-required and propagate to its
;;      transitive operand sources (= phi/copy/identity producers)
;;   3. walk again until no class changes (small SSA, terminates fast)
;;
;; The propagation step matters for `phi' and `copy' — a value that is
;; itself never the operand of an escape instruction can still escape
;; transitively when one of its sources does.  Doc 42 §2.4 calls this
;; "lattice join" (`nelisp-cc--escape-class-merge').

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)

(define-error 'nelisp-cc-escape-error
  "NeLisp escape analysis error" 'nelisp-cc-error)

(defcustom nelisp-cc-escape-conservative t
  "If non-nil, treat :unknown values as if they escaped.

Doc 42 §2.4 class C — when the analysis cannot prove a value's class
is :stack-only it falls back to :unknown.  With this flag set
`nelisp-cc-escape-info-can-stack-allocate-p' and
`nelisp-cc-escape-info-can-inline-p' both refuse the value, matching
the Doc 42 conservative default.  Set to nil only inside dedicated
opt-in benches that want to measure the analysis precision in
isolation."
  :type 'boolean
  :group 'nelisp-cc)

;;; Escape source opcode set ----------------------------------------
;;
;; Doc 42 §3.1 lists four escape sources (return / closure / store /
;; opaque call).  `call' and `call-indirect' both count as opaque
;; calls because the prototype has no callee-side annotation yet —
;; later phases (7.7.2 simple-inline) will mark known-pure leaves and
;; we will then narrow this set.

(defconst nelisp-cc--escape-source-opcodes
  '(return closure store-var call call-indirect)
  "Opcodes whose operands force their operand SSA values to escape.

The set is intentionally conservative — the analysis treats *every*
operand of these instructions as escaping.  When Phase 7.7.2 lands a
leaf-side `:pure' annotation we will subtract the matching `call'
forms here without reshaping the lattice.")

;;; escape-info container -------------------------------------------

(cl-defstruct (nelisp-cc--escape-info
               (:constructor nelisp-cc--escape-info-make)
               (:copier nil))
  "Per-function escape verdict produced by `nelisp-cc-escape-analyze'.

FUNCTION is the `nelisp-cc--ssa-function' that was analysed.  CLASSES
is a hash table keyed by SSA value ID (integer) whose value is one of
:stack-only / :heap-required / :unknown.  REASONS, when non-nil, is a
parallel hash table keyed by value ID whose value is a short symbol
explaining why the verdict is non-:stack-only — :returned, :captured,
:stored, :passed, or :unknown-source.  Reasons are cleared when the
class is downgraded back to :stack-only (which never happens in the
current pass, but the field is reserved for the iterative refinement
hook Phase 7.7 second-iteration may add)."
  (function nil)
  (classes nil)
  (reasons nil))

;;; class lattice ----------------------------------------------------
;;
;; The lattice is a 3-element total order:
;;
;;     :stack-only  <  :unknown  <  :heap-required
;;
;; Join (= "the strictest of the two") is implemented in
;; `nelisp-cc--escape-class-join'.  The analysis only ever raises a
;; class — never lowers — so monotonicity guarantees the worklist
;; terminates.

(defconst nelisp-cc--escape-class-rank
  '((:stack-only    . 0)
    (:unknown       . 1)
    (:heap-required . 2)))

(defun nelisp-cc--escape-class-rank (class)
  "Return the integer rank of CLASS, or signal if unknown."
  (or (cdr (assq class nelisp-cc--escape-class-rank))
      (signal 'nelisp-cc-escape-error
              (list :unknown-class class))))

(defun nelisp-cc--escape-class-join (a b)
  "Return the lattice join of class A and class B."
  (if (>= (nelisp-cc--escape-class-rank a)
          (nelisp-cc--escape-class-rank b))
      a
    b))

;;; helpers over the escape-info hashes -----------------------------

(defun nelisp-cc--escape-class-of (info value)
  "Return the recorded class for VALUE in INFO.

Falls back to :stack-only when VALUE has no entry — that matches the
seed step of the worklist and keeps callers simple."
  (or (gethash (nelisp-cc--ssa-value-id value)
               (nelisp-cc--escape-info-classes info))
      :stack-only))

(defun nelisp-cc--escape-set-class (info value class &optional reason)
  "Raise the recorded class for VALUE in INFO to CLASS, return t if changed.

Joins with the existing entry so concurrent escape sources never
overwrite a stricter verdict.  REASON, when non-nil, is recorded the
first time the class is raised above :stack-only — later raises
preserve the original reason because they don't add information."
  (let* ((vid (nelisp-cc--ssa-value-id value))
         (cur (gethash vid (nelisp-cc--escape-info-classes info) :stack-only))
         (new (nelisp-cc--escape-class-join cur class)))
    (cond
     ((eq new cur) nil)
     (t
      (puthash vid new (nelisp-cc--escape-info-classes info))
      (when (and reason
                 (not (gethash vid
                               (nelisp-cc--escape-info-reasons info))))
        (puthash vid reason
                 (nelisp-cc--escape-info-reasons info)))
      t))))

;;; transitive propagation ------------------------------------------
;;
;; When a value V is forced to escape we must also force every value
;; that V is *defined from* — phi/copy/identity producers.  The
;; current frontend opcode set only really needs phi/copy, but we
;; widen the predicate to "any def-point that is an instruction" so
;; future opcodes (e.g. select / move / cast) inherit the correct
;; behaviour for free.
;;
;; The propagation walks operands of the def-point recursively until
;; it hits a :param value or a value already in the worklist.

(defun nelisp-cc--escape-propagate (info value reason work)
  "Mark VALUE as escaping with REASON, push transitive sources onto WORK.

WORK is a mutable list — caller drains it.  Returns the (possibly
extended) WORK list.  Idempotent — values whose class is already
:heap-required are no-ops."
  (when (nelisp-cc--escape-set-class info value :heap-required reason)
    ;; Value just got raised → its operand sources may need raising
    ;; too.  Push the def-point's operands onto the worklist so the
    ;; main loop revisits them.
    (let ((dp (nelisp-cc--ssa-value-def-point value)))
      (when (and dp (not (eq dp :param)))
        (dolist (op (nelisp-cc--ssa-instr-operands dp))
          (push (cons op reason) work)))))
  work)

;;; analysis core ---------------------------------------------------

(defun nelisp-cc--escape-seed (info)
  "Initialise INFO's hashes with every value seeded to :stack-only.

Walks function params + every def in every block.  We seed the
classes hash explicitly so `nelisp-cc--escape-class-of' can rely on
the value being present (the verifier-side iteration over the hash
is more useful than a sparse table)."
  (let ((fn (nelisp-cc--escape-info-function info)))
    (dolist (p (nelisp-cc--ssa-function-params fn))
      (puthash (nelisp-cc--ssa-value-id p)
               :stack-only
               (nelisp-cc--escape-info-classes info)))
    (dolist (b (nelisp-cc--ssa-function-blocks fn))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (let ((def (nelisp-cc--ssa-instr-def instr)))
          (when def
            (puthash (nelisp-cc--ssa-value-id def)
                     :stack-only
                     (nelisp-cc--escape-info-classes info))))))))

(defun nelisp-cc--escape-reason-for-opcode (opcode)
  "Return the symbolic escape reason associated with OPCODE."
  (pcase opcode
    ('return        :returned)
    ('closure       :captured)
    ('store-var     :stored)
    ('call          :passed)
    ('call-indirect :passed)
    (_              :passed)))

(defun nelisp-cc--escape-walk-uses (info)
  "Walk every escape source in INFO's function and propagate verdicts.

Implements the fix-point worklist of Doc 42 §3.1.  Returns INFO."
  (let ((fn (nelisp-cc--escape-info-function info))
        (work nil))
    ;; Pass 1 — direct escape sources.
    (dolist (b (nelisp-cc--ssa-function-blocks fn))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (let ((op (nelisp-cc--ssa-instr-opcode instr)))
          (when (memq op nelisp-cc--escape-source-opcodes)
            (let ((reason (nelisp-cc--escape-reason-for-opcode op)))
              (dolist (v (nelisp-cc--ssa-instr-operands instr))
                (setq work
                      (nelisp-cc--escape-propagate info v reason work))))))))
    ;; Pass 2 — drain transitive worklist.  The propagator pushes
    ;; (VALUE . REASON) pairs; we re-call it for each so phi / copy
    ;; chains are flattened in O(n_operands) per worklist visit.  The
    ;; class lattice is monotone so termination is guaranteed.
    (while work
      (let* ((entry (pop work))
             (v (car entry))
             (reason (cdr entry)))
        (setq work (nelisp-cc--escape-propagate info v reason work))))
    info))

(defun nelisp-cc--escape-mark-unknown-operands (info)
  "Mark any operand whose def-point is foreign-to-FN as :unknown.

A well-formed function never has such operands (the SSA verifier
rejects them); this exists so a partial / mid-construction SSA can
still be analysed, and so future opcodes that legitimately reference
external state (e.g. a future global-load instruction) get a
predictable verdict."
  (let* ((fn (nelisp-cc--escape-info-function info))
         (in-fn (make-hash-table :test 'eq)))
    (dolist (b (nelisp-cc--ssa-function-blocks fn))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (puthash instr t in-fn)))
    (dolist (b (nelisp-cc--ssa-function-blocks fn))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (dolist (op (nelisp-cc--ssa-instr-operands instr))
          (let ((dp (nelisp-cc--ssa-value-def-point op)))
            (unless (or (eq dp :param)
                        (and dp (gethash dp in-fn)))
              (let ((vid (nelisp-cc--ssa-value-id op)))
                (puthash vid :unknown
                         (nelisp-cc--escape-info-classes info))
                (unless (gethash vid
                                 (nelisp-cc--escape-info-reasons info))
                  (puthash vid :unknown-source
                           (nelisp-cc--escape-info-reasons info)))))))))))

;;; Public API ------------------------------------------------------

;;;###autoload
(defun nelisp-cc-escape-analyze (fn)
  "Run escape analysis on FN, return the populated `nelisp-cc--escape-info'.

FN must be a `nelisp-cc--ssa-function' produced by the Phase 7.1
scaffold.  The function is *not* mutated.  The returned info object
exposes per-value verdicts via
`nelisp-cc-escape-info-can-stack-allocate-p' and
`nelisp-cc-escape-info-can-inline-p'."
  (unless (nelisp-cc--ssa-function-p fn)
    (signal 'nelisp-cc-escape-error
            (list :not-a-function fn)))
  (let ((info (nelisp-cc--escape-info-make
               :function fn
               :classes (make-hash-table :test 'eql)
               :reasons (make-hash-table :test 'eql))))
    (nelisp-cc--escape-seed info)
    (nelisp-cc--escape-mark-unknown-operands info)
    (nelisp-cc--escape-walk-uses info)
    info))

;;;###autoload
(defun nelisp-cc-escape-info-can-stack-allocate-p (var info)
  "Return non-nil when VAR can be stack-allocated according to INFO.

VAR is an `nelisp-cc--ssa-value' that was a member of the function
analysed by `nelisp-cc-escape-analyze'.  Treats :unknown as unsafe
when `nelisp-cc-escape-conservative' is non-nil (Doc 42 §2.4 default
class C handling); set the defcustom to nil to allow :unknown values
through for measurement-only experiments."
  (let ((cls (nelisp-cc--escape-class-of info var)))
    (cond
     ((eq cls :stack-only)    t)
     ((eq cls :heap-required) nil)
     ((eq cls :unknown)       (not nelisp-cc-escape-conservative))
     (t (signal 'nelisp-cc-escape-error
                (list :unknown-class cls))))))

;;;###autoload
(defun nelisp-cc-escape-info-can-inline-p (var info)
  "Return non-nil when VAR's defining site is safe to inline per INFO.

The current prototype answers the same predicate as
`nelisp-cc-escape-info-can-stack-allocate-p' — both gates need the
value to *not* escape.  They are kept distinct because Phase 7.7.2
will narrow `can-inline-p' (cost model + leaf check intersect with
the escape verdict) while `can-stack-allocate-p' will track only the
SROA precondition."
  (nelisp-cc-escape-info-can-stack-allocate-p var info))

;;;###autoload
(defun nelisp-cc-escape-info-class-of (var info)
  "Return the raw escape class symbol for VAR in INFO.

This is the un-conservatised view used by debug tooling and tests —
prefer the `-can-*' helpers in production callers."
  (nelisp-cc--escape-class-of info var))

;;;###autoload
(defun nelisp-cc-escape-info-reason-of (var info)
  "Return the symbolic reason VAR is escaping per INFO, or nil if not."
  (gethash (nelisp-cc--ssa-value-id var)
           (nelisp-cc--escape-info-reasons info)))

;;;###autoload
(defun nelisp-cc-escape-info-stats (info)
  "Return an alist summarising INFO's class distribution.

The shape is `((:stack-only . N) (:unknown . N) (:heap-required . N))'.
Useful for the `nelisp-cc-escape-stats' surface anticipated in Doc 42
§4.4 — the full instrumentation lands with Phase 7.7.2."
  (let ((counts (list (cons :stack-only    0)
                      (cons :unknown       0)
                      (cons :heap-required 0))))
    (maphash (lambda (_id cls)
               (cl-incf (cdr (assq cls counts))))
             (nelisp-cc--escape-info-classes info))
    counts))

(provide 'nelisp-cc-escape)
;;; nelisp-cc-escape.el ends here
