;;; nelisp-cc-inline.el --- Simple inline pass (Doc 42 §3.2, Phase 7.7.2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.7.2 prototype — *simple inline pass* for the SSA IR produced
;; by `src/nelisp-cc.el' (Phase 7.1 SSA scaffold).  Doc 42 LOCKED v2
;; §3.2 specifies a focused first-iteration inliner that takes only the
;; safest `call' sites:
;;
;;   1. *leaf callee*       — callee SSA contains no further `call' /
;;                            `call-indirect' instructions
;;   2. *small body*        — callee SSA insn count ≤
;;                            `nelisp-cc-inline-size-threshold' (default
;;                            20, per §3.2)
;;   3. *non-recursive*     — callee name ≠ caller name (the recursive
;;                            unroller is Phase 7.7.3, not this one)
;;   4. *escape-clean args* — every `call' operand is :stack-only per
;;                            T139 escape analysis output (no escaped
;;                            value can be SROA-substituted into an
;;                            inlined leaf body without losing the heap
;;                            requirement marker)
;;
;; The pass *consumes* the escape-info produced by Doc 42 §3.1 (T139
;; `nelisp-cc-escape-analyze') — it does *not* reanalyse.  Consumers
;; that want the full sequence call escape-analyze first, then this
;; pass.
;;
;; *Single-block-callee restriction* — the prototype only inlines
;; callees whose SSA has exactly one block (= the entry block) and at
;; most one `return' instruction.  This covers all leaf arithmetic
;; helpers (the §3.2 fact-iter target) without needing the multi-block
;; CFG-splice machinery that Phase 7.7.3 will require for recursive
;; inlining.  Multi-block callees are returned as :reason
;; :multi-block-callee in the abort log so the dashboard surfaces them
;; as a Phase 7.7.3 follow-up candidate.
;;
;; The pass mutates the caller in place — it adds new value/instr IDs
;; via the existing builders, drops the original `call' instruction
;; from its block's INSTRS list, and splices the renumbered callee body
;; in its place.  USE-LIST back-pointers are rebuilt for the inserted
;; instructions; the verifier (`nelisp-cc--ssa-verify-function') is
;; expected to pass on the post-pass function.
;;
;; Public API (Doc 42 §4.1 `nelisp-cc-inline-simple' / §4.2 helpers):
;;
;;   `nelisp-cc-inline-pass'      — top-level entry, runs on a caller
;;                                  + escape-info + callee-registry,
;;                                  returns (CALLER . INLINED-COUNT).
;;   `nelisp-cc-inline-can-inline-p'
;;                                — predicate over a single `call' instr
;;                                  + escape-info + callee-registry,
;;                                  returns t when the four conditions
;;                                  above hold.
;;   `nelisp-cc-inline-cost'      — per-callee size cost (= insn count),
;;                                  used by the predicate.
;;
;; Private helpers follow the `nelisp-cc--inline-' namespace.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)

(define-error 'nelisp-cc-inline-error
  "NeLisp inline pass error" 'nelisp-cc-error)

(defcustom nelisp-cc-inline-size-threshold 20
  "Maximum callee SSA instruction count eligible for `nelisp-cc-inline-pass'.

Doc 42 §3.2 — leaf-bias heuristic threshold.  Callees with strictly
more than this many instructions are rejected with reason
`:size-cap-exceeded'.  Tunable at runtime so opt-in benches can sweep
the curve without recompiling."
  :type 'integer
  :group 'nelisp-cc)

(defcustom nelisp-cc-inline-simple-enable nil
  "If non-nil, callers that wrap `nelisp-cc-inline-pass' may run it.

The flag is *advisory* — `nelisp-cc-inline-pass' itself runs when
called regardless of this value, so tests do not need to flip the
defcustom.  Production drivers (= the SSA pipeline registrar in
`src/nelisp-cc.el', when wired) are expected to gate on this flag so
the opt-in semantics of Doc 42 §3.2 are preserved end-to-end."
  :type 'boolean
  :group 'nelisp-cc)

;;; cost model -------------------------------------------------------

;;;###autoload
(defun nelisp-cc-inline-cost (fn-def)
  "Return the inline cost of FN-DEF (an `nelisp-cc--ssa-function').

Cost = total instruction count across every block.  This is the size
metric the Doc 42 §2.2 `text size cap' approximates at the SSA
level — backend lowering blows it up by a roughly constant factor so
the SSA count is a fine first-order proxy."
  (unless (nelisp-cc--ssa-function-p fn-def)
    (signal 'nelisp-cc-inline-error
            (list :not-a-function fn-def)))
  (let ((n 0))
    (dolist (b (nelisp-cc--ssa-function-blocks fn-def))
      (cl-incf n (length (nelisp-cc--ssa-block-instrs b))))
    n))

;;; leaf / recursion / single-block predicates ----------------------

(defun nelisp-cc--inline-leaf-p (fn-def)
  "Return t when FN-DEF makes no further `call' / `call-indirect'."
  (catch 'not-leaf
    (dolist (b (nelisp-cc--ssa-function-blocks fn-def))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (memq (nelisp-cc--ssa-instr-opcode instr)
                    '(call call-indirect))
          (throw 'not-leaf nil))))
    t))

(defun nelisp-cc--inline-single-block-p (fn-def)
  "Return t when FN-DEF has exactly one block."
  (= 1 (length (nelisp-cc--ssa-function-blocks fn-def))))

(defun nelisp-cc--inline-recursive-p (fn-def callee-name)
  "Return t when FN-DEF self-references CALLEE-NAME via a `call'."
  (catch 'rec
    (dolist (b (nelisp-cc--ssa-function-blocks fn-def))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
          (let ((meta (nelisp-cc--ssa-instr-meta instr)))
            (when (eq (plist-get meta :fn) callee-name)
              (throw 'rec t))))))
    nil))

;;; call-site introspection ----------------------------------------

(defun nelisp-cc--inline-call-callee-name (instr)
  "Return the symbolic callee name carried in INSTR's meta, or nil."
  (and (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
       (plist-get (nelisp-cc--ssa-instr-meta instr) :fn)))

(defun nelisp-cc--inline-list-call-sites (caller)
  "Return every `call' instruction in CALLER, in block-then-instr order."
  (let (acc)
    (dolist (b (nelisp-cc--ssa-function-blocks caller))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
          (push instr acc))))
    (nreverse acc)))

(defun nelisp-cc--inline-count-callsites-of (caller callee-name)
  "Return how many `call' instructions in CALLER target CALLEE-NAME."
  (let ((n 0))
    (dolist (instr (nelisp-cc--inline-list-call-sites caller))
      (when (eq (nelisp-cc--inline-call-callee-name instr) callee-name)
        (cl-incf n)))
    n))

;;; can-inline-p ----------------------------------------------------

;;;###autoload
(defun nelisp-cc-inline-can-inline-p (call-instr escape-info &optional callee-registry caller)
  "Return t when CALL-INSTR is safe and cheap to inline.

CALL-INSTR must be an `nelisp-cc--ssa-instr' with opcode `call' (the
caller-side site).  ESCAPE-INFO is the verdict produced by
`nelisp-cc-escape-analyze' on the *caller* function — its operand-side
escape classes gate point #4.  CALLEE-REGISTRY is an alist
`((NAME . SSA-FUNCTION) ...)' the caller maintains to map call-site
metadata to callee bodies.  CALLER, when non-nil, scopes the recursive
self-call check (Doc 42 §3.2 condition #3); when nil the predicate
skips that check (callers that already filtered by site cardinality
can pass nil).

Returns nil for any unmet condition; the symbolic abort reason is
recorded in the returned `nelisp-cc--inline-status' container — see
`nelisp-cc--inline-decide' for the full list."
  (let ((decision (nelisp-cc--inline-decide
                   call-instr escape-info callee-registry caller)))
    (eq :go (nelisp-cc--inline-status-verdict decision))))

;;; status container -----------------------------------------------

(cl-defstruct (nelisp-cc--inline-status
               (:constructor nelisp-cc--inline-status-make)
               (:copier nil))
  "Per-callsite inline decision, populated by `nelisp-cc--inline-decide'.

VERDICT is `:go' (inline allowed) or `:abort' (inline rejected).
REASON, when VERDICT is :abort, is one of:
  :unresolved-callee, :no-callee-name, :recursive,
  :multi-block-callee, :not-leaf, :size-cap-exceeded,
  :escaped-arg, :no-return.
CALLEE is the resolved `nelisp-cc--ssa-function' or nil."
  (verdict :abort)
  (reason nil)
  (callee nil))

(defun nelisp-cc--inline-decide (call-instr escape-info callee-registry caller)
  "Compute the inline decision for CALL-INSTR; return an inline-status struct.

See `nelisp-cc-inline-can-inline-p' for the condition list and the
exact set of REASONs.  The function is pure (read-only over its
inputs) — it can be invoked from dashboards / test harnesses with no
side effects."
  (let* ((callee-name (nelisp-cc--inline-call-callee-name call-instr))
         (callee (and callee-name
                      (cdr (assq callee-name callee-registry)))))
    (cond
     ((null callee-name)
      (nelisp-cc--inline-status-make :verdict :abort :reason :no-callee-name))
     ((null callee)
      (nelisp-cc--inline-status-make :verdict :abort :reason :unresolved-callee))
     ((and caller
           (eq callee-name (nelisp-cc--ssa-function-name caller)))
      (nelisp-cc--inline-status-make :verdict :abort :reason :recursive
                                     :callee callee))
     ((nelisp-cc--inline-recursive-p callee callee-name)
      ;; Self-recursive callee — leaf check would also reject it (it
      ;; contains a `call') but we surface a more specific reason.
      (nelisp-cc--inline-status-make :verdict :abort :reason :recursive
                                     :callee callee))
     ((not (nelisp-cc--inline-single-block-p callee))
      (nelisp-cc--inline-status-make :verdict :abort
                                     :reason :multi-block-callee
                                     :callee callee))
     ((not (nelisp-cc--inline-leaf-p callee))
      (nelisp-cc--inline-status-make :verdict :abort :reason :not-leaf
                                     :callee callee))
     ((> (nelisp-cc-inline-cost callee) nelisp-cc-inline-size-threshold)
      (nelisp-cc--inline-status-make :verdict :abort
                                     :reason :size-cap-exceeded
                                     :callee callee))
     ((nelisp-cc--inline-any-arg-escaped-p call-instr escape-info)
      (nelisp-cc--inline-status-make :verdict :abort :reason :escaped-arg
                                     :callee callee))
     ((null (nelisp-cc--inline-callee-return-instr callee))
      ;; Leaf without an explicit return — we have nothing to bind to
      ;; the caller's def site.  Skip rather than guess.
      (nelisp-cc--inline-status-make :verdict :abort :reason :no-return
                                     :callee callee))
     (t
      (nelisp-cc--inline-status-make :verdict :go :callee callee)))))

(defun nelisp-cc--inline-any-arg-escaped-p (call-instr escape-info)
  "Return t when any operand of CALL-INSTR escapes via a non-call sink.

Doc 42 §3.2 condition #4 + §3.1 narrowing note — escape analysis
treats every `call' / `call-indirect' as an opaque sink because the
prototype has no callee-side `:pure' annotation yet.  The simple
inliner must *subtract* the call it is about to eliminate from that
sink set, otherwise no call operand could ever satisfy the gate
(every call operand would be :passed by definition).

The predicate therefore:
  1. ignores values whose only escape reason is `:passed' caused by
     CALL-INSTR's own use-list entry, and
  2. still rejects values that escape via `return' / `closure' /
     `store-var' / `call-indirect' (those reasons survive the inline).

`:unknown' is let through — callers that want the conservative gate
can wrap this with their own `(when nelisp-cc-escape-conservative
...)' check."
  (catch 'esc
    (dolist (op (nelisp-cc--ssa-instr-operands call-instr))
      (when (nelisp-cc--inline-value-escapes-besides-call-p
             op call-instr escape-info)
        (throw 'esc t)))
    nil))

(defconst nelisp-cc--inline-non-call-escape-opcodes
  '(return closure store-var call-indirect)
  "Escape-source opcodes that survive the inliner's call subtraction.

Doc 42 §3.1 lists `call' / `call-indirect' alongside `return' /
`closure' / `store-var' as escape sources.  The simple inliner's job
is to *eliminate* `call' instructions, so for the escape gate it
discounts the entire `call' opcode — every other call site of the
value is also a candidate for the same inliner pass, and even the
ones that stay (because their callee was rejected) carry the same
operand reference they had before so the post-inline IR is no worse
than the input.  `call-indirect' stays in the gate because Phase 7.7.2
never inlines it (no callee name) — its operand is genuinely opaque.")

(defun nelisp-cc--inline-value-escapes-besides-call-p (val _call-instr _escape-info)
  "Return t when VAL escapes via a sink other than `call'.

The cached escape verdict folds every `call' site's `:passed' source
into VAL's class — without subtraction every call operand would be
permanently :heap-required, blocking the inliner end-to-end.  We
therefore re-walk VAL's USE-LIST directly with a narrower escape-source
predicate (`nelisp-cc--inline-non-call-escape-opcodes') that drops
`call' and keeps `return' / `closure' / `store-var' / `call-indirect'.

Transitive forward propagation (through `copy' / `phi' results that
themselves escape) is not chased here — those producers' own escape
verdicts would surface independently when the splice rewrites their
operands during the Phase 7.7.3 fixpoint pass.  For the simple
inliner's narrow scope (leaf arithmetic) the direct use-list check
matches the §3.2 acceptance condition exactly."
  (catch 'esc
    (dolist (user (nelisp-cc--ssa-value-use-list val))
      (when (memq (nelisp-cc--ssa-instr-opcode user)
                  nelisp-cc--inline-non-call-escape-opcodes)
        (throw 'esc t)))
    nil))

(defun nelisp-cc--inline-callee-return-instr (callee)
  "Return the `return' instruction in CALLEE's only block, or nil."
  (let ((blk (car (nelisp-cc--ssa-function-blocks callee))))
    (cl-find-if (lambda (instr)
                  (eq 'return (nelisp-cc--ssa-instr-opcode instr)))
                (nelisp-cc--ssa-block-instrs blk))))

;;; substitution helpers --------------------------------------------

(defun nelisp-cc--inline-clone-value (caller src-value)
  "Allocate a fresh SSA value in CALLER cloned from SRC-VALUE.

The clone preserves SRC-VALUE's TYPE; DEF-POINT is left nil (the
splice loop sets it when the cloned instruction is appended).  Returns
the new value."
  (nelisp-cc--ssa-make-value caller (nelisp-cc--ssa-value-type src-value)))

(defun nelisp-cc--inline-build-value-map (caller callee call-instr)
  "Build a hash-table mapping CALLEE values → caller-side substitutes.

The map is keyed by the *value object* (`eq', not by id).  Param
values map to the corresponding CALL-INSTR operand; each callee def
that is *not* the return instruction's def gets a fresh caller-side
clone via `nelisp-cc--inline-clone-value'.  The return instruction's
def (when present) is also cloned — the splice loop then has a
canonical caller-side value to bind the original call's def to.

Returns the populated hash-table."
  (let ((map (make-hash-table :test 'eq))
        (params (nelisp-cc--ssa-function-params callee))
        (args (nelisp-cc--ssa-instr-operands call-instr)))
    (unless (= (length params) (length args))
      (signal 'nelisp-cc-inline-error
              (list :arity-mismatch
                    :callee (nelisp-cc--ssa-function-name callee)
                    :params (length params)
                    :args   (length args))))
    (cl-mapc (lambda (p a) (puthash p a map)) params args)
    (let ((blk (car (nelisp-cc--ssa-function-blocks callee))))
      (dolist (instr (nelisp-cc--ssa-block-instrs blk))
        (let ((def (nelisp-cc--ssa-instr-def instr)))
          (when def
            (puthash def (nelisp-cc--inline-clone-value caller def) map)))))
    map))

(defun nelisp-cc--inline-translate-operands (operands map)
  "Translate every operand in OPERANDS through MAP, returning the new list.

Operands that are not in MAP fall through unchanged (= caller-side
values that came in via params)."
  (mapcar (lambda (v) (or (gethash v map) v)) operands))

;;; splice ----------------------------------------------------------

(defun nelisp-cc--inline-splice-into (caller block call-instr callee)
  "Splice CALLEE's body into CALLER's BLOCK at the position of CALL-INSTR.

Removes CALL-INSTR from BLOCK's INSTRS list, allocates fresh
caller-side values for every callee def, copies non-return callee
instructions in order, and replaces the original `call' def (if any)
with a `copy' instruction whose operand is the translated `return'
operand of the callee.  When the original `call' had no def (the
caller threw the value away) we skip the copy.

The function is destructive on CALLER; returns CALLER for chaining."
  (let* ((map (nelisp-cc--inline-build-value-map caller callee call-instr))
         (callee-blk (car (nelisp-cc--ssa-function-blocks callee)))
         (return-instr (nelisp-cc--inline-callee-return-instr callee))
         (return-operand (and return-instr
                              (car (nelisp-cc--ssa-instr-operands return-instr))))
         (call-def (nelisp-cc--ssa-instr-def call-instr))
         ;; Snapshot the existing block's instr list so we can rebuild
         ;; in-place without disturbing other passes' iterators.
         (orig-instrs (nelisp-cc--ssa-block-instrs block))
         (new-instrs nil))
    (dolist (instr orig-instrs)
      (cond
       ((eq instr call-instr)
        ;; Splice point — drop the call and insert callee body.
        (dolist (cinstr (nelisp-cc--ssa-block-instrs callee-blk))
          (unless (eq cinstr return-instr)
            (let* ((new-def (and (nelisp-cc--ssa-instr-def cinstr)
                                 (gethash (nelisp-cc--ssa-instr-def cinstr) map)))
                   (new-ops (nelisp-cc--inline-translate-operands
                             (nelisp-cc--ssa-instr-operands cinstr) map))
                   (cloned (nelisp-cc--inline-make-instr
                            caller block
                            (nelisp-cc--ssa-instr-opcode cinstr)
                            new-ops new-def
                            (nelisp-cc--ssa-instr-meta cinstr))))
              (push cloned new-instrs))))
        ;; If the caller cared about the return value, materialise a
        ;; `copy' instruction — its def is the original call's def
        ;; (preserving downstream uses that already point at it).
        (when (and call-def return-operand)
          (let* ((translated (or (gethash return-operand map) return-operand))
                 (copy-instr (nelisp-cc--inline-make-instr
                              caller block 'copy (list translated)
                              call-def nil)))
            (push copy-instr new-instrs)))
        ;; Remove the call's outgoing use back-pointers (it is gone
        ;; from the IR; old operand use-lists must drop it).
        (nelisp-cc--inline-drop-uses-of call-instr))
       (t
        (push instr new-instrs))))
    (setf (nelisp-cc--ssa-block-instrs block) (nreverse new-instrs))
    caller))

(defun nelisp-cc--inline-make-instr (fn block opcode operands def meta)
  "Allocate an SSA instruction for FN/BLOCK without appending it.

Mirrors `nelisp-cc--ssa-add-instr' but lets the caller order the
inserts manually — `nelisp-cc--inline-splice-into' rebuilds the block
INSTRS list as a single batch, so the per-call append in the standard
builder would O(n²) the splice.  Updates use-lists and DEF-POINT just
like the standard builder.  Block backref is set."
  (let ((instr (nelisp-cc--ssa-instr-make
                :id (nelisp-cc--ssa-function-next-instr-id fn)
                :opcode opcode
                :operands operands
                :def def
                :block block
                :meta meta)))
    (cl-incf (nelisp-cc--ssa-function-next-instr-id fn))
    (when def
      (when (and (nelisp-cc--ssa-value-def-point def)
                 (not (eq (nelisp-cc--ssa-value-def-point def) instr)))
        ;; Only legitimate when re-binding the original call's def to
        ;; the inserted `copy' — overwrite is fine in that case.
        (setf (nelisp-cc--ssa-value-def-point def) instr))
      (unless (nelisp-cc--ssa-value-def-point def)
        (setf (nelisp-cc--ssa-value-def-point def) instr)))
    (dolist (op operands)
      (nelisp-cc--ssa-add-use op instr))
    instr))

(defun nelisp-cc--inline-drop-uses-of (instr)
  "Remove INSTR from the USE-LIST of every operand it consumed."
  (dolist (op (nelisp-cc--ssa-instr-operands instr))
    (setf (nelisp-cc--ssa-value-use-list op)
          (delq instr (nelisp-cc--ssa-value-use-list op)))))

;;; top-level pass --------------------------------------------------

;;;###autoload
(defun nelisp-cc-inline-pass (caller escape-info &optional callee-registry)
  "Run the simple-inline pass on CALLER, return (CALLER . INLINED-COUNT).

CALLER is mutated in place — every `call' instruction whose decision
is :go (per `nelisp-cc--inline-decide') is replaced by the inlined
callee body.  ESCAPE-INFO is the verdict produced by
`nelisp-cc-escape-analyze' on CALLER.  CALLEE-REGISTRY is an alist
mapping callee names to their `nelisp-cc--ssa-function' bodies; any
callsite whose name is missing from the registry is left untouched
(reason :unresolved-callee).

Inlining is *single-pass* — newly inserted instructions are not
re-scanned for further inline opportunities.  The recursive unroller
(Phase 7.7.3) will iterate to a fixpoint; the simple inliner ships as
a one-shot pass to make its complexity bound trivially analysable."
  (unless (nelisp-cc--ssa-function-p caller)
    (signal 'nelisp-cc-inline-error
            (list :not-a-function caller)))
  (let* ((sites (nelisp-cc--inline-list-call-sites caller))
         (count 0))
    (dolist (site sites)
      (let ((decision (nelisp-cc--inline-decide
                       site escape-info callee-registry caller)))
        (when (eq :go (nelisp-cc--inline-status-verdict decision))
          (let ((blk (nelisp-cc--ssa-instr-block site))
                (callee (nelisp-cc--inline-status-callee decision)))
            (nelisp-cc--inline-splice-into caller blk site callee)
            (cl-incf count)))))
    (cons caller count)))

;;;###autoload
(defun nelisp-cc-inline-stats (caller escape-info &optional callee-registry)
  "Return an alist tallying decision verdicts at every call site of CALLER.

Useful for the Doc 42 §4.4 dashboard surface.  Shape:
  ((:go . N)
   (:abort
    ((:no-callee-name . N) (:unresolved-callee . N) (:recursive . N)
     (:multi-block-callee . N) (:not-leaf . N) (:size-cap-exceeded . N)
     (:escaped-arg . N) (:no-return . N))))"
  (let ((go 0)
        (reasons (list (cons :no-callee-name 0)
                       (cons :unresolved-callee 0)
                       (cons :recursive 0)
                       (cons :multi-block-callee 0)
                       (cons :not-leaf 0)
                       (cons :size-cap-exceeded 0)
                       (cons :escaped-arg 0)
                       (cons :no-return 0))))
    (dolist (site (nelisp-cc--inline-list-call-sites caller))
      (let ((decision (nelisp-cc--inline-decide
                       site escape-info callee-registry caller)))
        (cond
         ((eq :go (nelisp-cc--inline-status-verdict decision))
          (cl-incf go))
         (t
          (let* ((reason (nelisp-cc--inline-status-reason decision))
                 (cell (assq reason reasons)))
            (when cell (cl-incf (cdr cell))))))))
    (list (cons :go go)
          (cons :abort reasons))))

(provide 'nelisp-cc-inline)
;;; nelisp-cc-inline.el ends here
