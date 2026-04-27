;;; nelisp-cc-recursive-inline.el --- Recursive inline pass (Doc 42 §3.3, Phase 7.7.3) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.7.3 prototype — *recursive inline pass* for the SSA IR built
;; by `src/nelisp-cc.el'.  Doc 42 LOCKED v2 §3.3 specifies the pass as
;; the *主軸* of the Phase 7.7 inliner suite — a depth-bounded
;; *unroller* that takes self-recursive `call' sites and inlines the
;; callee body up to `nelisp-cc-rec-inline-depth-limit' (= 4 by
;; default) levels deep.  At every depth level recursive call sites
;; that would exceed the budget are *kept* (= partial inline), so the
;; resulting function still terminates — the unroller never blows the
;; depth limit.
;;
;; What this pass does (Doc 42 §3.3 deliverables):
;;
;;   1. *bounded depth unroll* — when CALLER itself is the callee
;;      (= self-recursive), the pass clones CALLER's body up to
;;      DEPTH-LIMIT times, splicing each clone in place of a self-call
;;      site.  The deepest clone keeps its self-calls (= recursive
;;      *base case fallback*), so semantics are preserved.
;;   2. *cost model gate* — every prospective splice is checked
;;      against a code-size budget (default = 5x the original cost).
;;      Once the cumulative SSA insn count would exceed the cap, the
;;      pass aborts further unrolling at that frontier.
;;   3. *base-case fold* — when the callee body has the canonical
;;      `(if BASE-COND BASE-VALUE RECURSIVE-EXPR)' shape and a
;;      constant-folded BASE-COND can be decided at unroll time, the
;;      pass collapses the branch (drops the recursive arm or the
;;      base arm depending on the constant).  The first-iteration
;;      implementation is conservative: it folds only when the
;;      condition operand is itself a `const' instruction whose
;;      literal is decidable (eq nil / non-nil).
;;   4. *multi-block CFG splice* — unlike the simple inliner
;;      (Phase 7.7.2) this pass clones every block of the callee,
;;      rewrites jump / branch terminator metadata to the cloned IDs,
;;      remaps phi arms, and stitches the cloned entry block into the
;;      caller via a `jump' from the splice site's predecessor and an
;;      injected merge block at the splice site's successor that picks
;;      up the cloned `return' values via a phi (when the original
;;      call had a def).
;;   5. *escape-info consume + propagate* — the T139 escape verdict
;;      gates each prospective splice (inherited from the simple
;;      inliner predicate `nelisp-cc-inline-can-inline-p'; non-self
;;      callees that satisfy it are ignored here — they belong to the
;;      Phase 7.7.2 pass).  Escape verdicts on cloned values are
;;      *re-propagated* downstream by re-running
;;      `nelisp-cc-escape-analyze' once per pass invocation; this
;;      keeps the per-pass complexity bounded by the splice count, not
;;      by per-clone re-analysis.
;;
;; Public API (Doc 42 §4.1 — `nelisp-cc-inline-recursive' family):
;;
;;   `nelisp-cc-rec-inline-pass'
;;       (CALLER ESCAPE-INFO &optional REGISTRY DEPTH-LIMIT)
;;       → (TRANSFORMED-SSA . INLINED-COUNT).  Mutates CALLER in place
;;       and returns it together with the count of *call sites
;;       eliminated* (= depth × per-iteration callsite count).  Pure
;;       observers can ignore INLINED-COUNT.
;;
;;   `nelisp-cc-rec-inline-can-inline-p'
;;       (CALL-INSTR DEPTH ESCAPE-INFO REGISTRY)
;;       → bool — predicate over a single self-recursive call site
;;       gated by the supplied DEPTH (= unroll iteration index, 0 =
;;       outermost).  Returns nil when the cost budget is exhausted,
;;       the call is non-self-recursive (this pass only handles
;;       self-recursion in the MVP), the escape gate fails, or the
;;       depth-limit was reached.
;;
;;   `nelisp-cc-rec-inline-cost-estimate'
;;       (FN-DEF DEPTH-LIMIT)
;;       → integer — *projected* SSA insn count after a full
;;       depth-limit unroll, computed as
;;          base + base × (DEPTH-LIMIT − 1)
;;       where `base' = `nelisp-cc-inline-cost' on FN-DEF.  This is a
;;       coarse upper bound (ignores base-case folds that would
;;       shrink it) used by the cost gate.
;;
;;   `nelisp-cc-rec-inline-stats'
;;       (CALLER ESCAPE-INFO &optional REGISTRY DEPTH-LIMIT)
;;       → alist `((:inlined . N) (:abort ((REASON . N) ...)))'.
;;
;; Private helpers follow the `nelisp-cc--rec-inline-' namespace.
;;
;; *Non-goals (= deferred to later sub-phases)*:
;;   - lambda lift (= Phase 7.7.4)
;;   - mutual recursion (= caller and callee differ but form a cycle).
;;     The MVP rejects with `:non-self-recursive' so the dashboard
;;     surfaces them as future-phase candidates.
;;   - inter-procedural specialization (= cross-function constant
;;     propagation).  Out of Phase 7.7 scope entirely.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)
(require 'nelisp-cc-inline)

(define-error 'nelisp-cc-rec-inline-error
  "NeLisp recursive inline pass error" 'nelisp-cc-error)

;;; defcustom -------------------------------------------------------

(defcustom nelisp-cc-rec-inline-depth-limit 4
  "Default depth budget for the recursive inliner.

Doc 42 §3.3 — bounded unroll depth.  Each prospective self-call site
is allowed to splice up to this many copies of the callee body
before the unroller leaves the remaining recursion intact.  Tunable
at runtime so opt-in benches can sweep the curve without
recompiling.  See §6.4 for the rationale behind the default = 4."
  :type 'integer
  :group 'nelisp-cc)

(defcustom nelisp-cc-rec-inline-size-multiplier 5
  "Code-size budget multiplier for the recursive inliner.

Doc 42 §2.2 — projected unroll cost (per `nelisp-cc-rec-inline-cost-estimate')
must be ≤ MULTIPLIER × original SSA insn count.  Default of 5
mirrors the §2.2 .text-size hard cap divided by the typical leaf
function footprint.  Larger values trade I-cache pressure for more
inline opportunities; smaller values starve the unroller."
  :type 'integer
  :group 'nelisp-cc)

(defcustom nelisp-cc-rec-inline-recursive-enable nil
  "If non-nil, callers that wrap `nelisp-cc-rec-inline-pass' may run it.

Advisory — `nelisp-cc-rec-inline-pass' itself runs unconditionally
when invoked, so tests do not need to flip the defcustom.  Doc 42
§4.3 — Phase 7.7.3 is opt-in until the bench gate (§5.3) measures
its own outcome."
  :type 'boolean
  :group 'nelisp-cc)

;;; internal stats container ---------------------------------------

(cl-defstruct (nelisp-cc--rec-inline-status
               (:constructor nelisp-cc--rec-inline-status-make)
               (:copier nil))
  "Per-callsite recursive-inline decision returned by
`nelisp-cc--rec-inline-decide'.

VERDICT is `:go' or `:abort'.  REASON, when VERDICT is :abort, is
one of:
  :no-callee-name, :unresolved-callee, :non-self-recursive,
  :depth-limit-reached, :cost-budget-exceeded, :escaped-arg.
CALLEE is the resolved `nelisp-cc--ssa-function' (= CALLER itself
for self-recursion) or nil."
  (verdict :abort)
  (reason nil)
  (callee nil))

;;; cost estimation -------------------------------------------------

;;;###autoload
(defun nelisp-cc-rec-inline-cost-estimate (fn-def depth-limit)
  "Return the projected post-unroll SSA insn count for FN-DEF at DEPTH-LIMIT.

Coarse upper bound — assumes every depth level reproduces the full
body cost.  Doc 42 §2.2 cost gate uses this against the
multiplier-times-baseline cap.

Returns 0 when DEPTH-LIMIT is non-positive; signals
`nelisp-cc-rec-inline-error' when FN-DEF is not an SSA function."
  (unless (nelisp-cc--ssa-function-p fn-def)
    (signal 'nelisp-cc-rec-inline-error
            (list :not-a-function fn-def)))
  (cond
   ((<= depth-limit 0) 0)
   (t
    (let ((base (nelisp-cc-inline-cost fn-def)))
      (* base depth-limit)))))

;;; predicate ------------------------------------------------------

(defun nelisp-cc--rec-inline-call-callee-name (instr)
  "Return the symbolic callee name carried in INSTR's meta, or nil."
  (and (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
       (plist-get (nelisp-cc--ssa-instr-meta instr) :fn)))

(defun nelisp-cc--rec-inline-list-self-call-sites (caller)
  "Return every self-recursive `call' instruction in CALLER, in order."
  (let ((self-name (nelisp-cc--ssa-function-name caller))
        acc)
    (dolist (b (nelisp-cc--ssa-function-blocks caller))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (and (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
                   (eq (nelisp-cc--rec-inline-call-callee-name instr)
                       self-name))
          (push instr acc))))
    (nreverse acc)))

(defun nelisp-cc--rec-inline-any-arg-escaped-p (call-instr escape-info)
  "Return t when any operand of CALL-INSTR escapes via a non-call sink.

Doc 42 §3.3 escape gate — the recursive splice replaces a `call'
with a clone of the callee body whose operand uses are *exactly*
the same shape as the original.  We must therefore subtract every
`call' / `call-indirect' use (every callee-side `call' that
references the operand) from the escape consideration, because
those will eventually be inlined too.  Reuses the simple
inliner's narrow predicate
(`nelisp-cc--inline-value-escapes-besides-call-p'), which checks
the use-list for `return' / `closure' / `store-var' /
`call-indirect' sinks only."
  (catch 'esc
    (dolist (op (nelisp-cc--ssa-instr-operands call-instr))
      (when (nelisp-cc--inline-value-escapes-besides-call-p
             op call-instr escape-info)
        (throw 'esc t)))
    nil))

(defun nelisp-cc--rec-inline-decide (call-instr depth escape-info caller registry)
  "Compute the recursive-inline decision for CALL-INSTR at unroll DEPTH.

CALL-INSTR must be a self-recursive `call' against CALLER.
ESCAPE-INFO is the verdict produced by `nelisp-cc-escape-analyze'
on CALLER.  REGISTRY is the standard callee alist (`((NAME . SSA)
...)') — when the callee name is missing the function returns
:unresolved-callee so the dashboard can surface it.  Returns an
`nelisp-cc--rec-inline-status' instance."
  (let* ((callee-name (nelisp-cc--rec-inline-call-callee-name call-instr))
         (self-name (nelisp-cc--ssa-function-name caller))
         (callee (or (and callee-name
                          (cdr (assq callee-name registry)))
                     ;; fall back to caller for self-recursion
                     ;; (registry might not list the caller itself).
                     (and callee-name (eq callee-name self-name)
                          caller))))
    (cond
     ((null callee-name)
      (nelisp-cc--rec-inline-status-make
       :verdict :abort :reason :no-callee-name))
     ((null callee)
      (nelisp-cc--rec-inline-status-make
       :verdict :abort :reason :unresolved-callee))
     ((not (eq callee-name self-name))
      (nelisp-cc--rec-inline-status-make
       :verdict :abort :reason :non-self-recursive
       :callee callee))
     ((>= depth nelisp-cc-rec-inline-depth-limit)
      (nelisp-cc--rec-inline-status-make
       :verdict :abort :reason :depth-limit-reached
       :callee callee))
     ((> (nelisp-cc-rec-inline-cost-estimate
          callee (- nelisp-cc-rec-inline-depth-limit depth))
         (* nelisp-cc-rec-inline-size-multiplier
            (nelisp-cc-inline-cost callee)))
      (nelisp-cc--rec-inline-status-make
       :verdict :abort :reason :cost-budget-exceeded
       :callee callee))
     ((nelisp-cc--rec-inline-any-arg-escaped-p call-instr escape-info)
      (nelisp-cc--rec-inline-status-make
       :verdict :abort :reason :escaped-arg
       :callee callee))
     (t
      (nelisp-cc--rec-inline-status-make
       :verdict :go :callee callee)))))

;;;###autoload
(defun nelisp-cc-rec-inline-can-inline-p (call-instr depth escape-info registry)
  "Return t when CALL-INSTR is safe and within budget at unroll DEPTH.

CALL-INSTR must be a `call' instruction whose containing block
exists.  DEPTH is the prospective unroll iteration (0 = outermost).
ESCAPE-INFO is the verdict from `nelisp-cc-escape-analyze' on the
function that owns CALL-INSTR.  REGISTRY is the standard callee
alist used by the simple inliner.

The predicate derives the caller from CALL-INSTR's block backref;
when the block has no function backref (test-only stub) the
predicate falls back to looking the caller up via REGISTRY using
the callee name as the key."
  (let* ((blk (nelisp-cc--ssa-instr-block call-instr))
         (caller (or (and blk
                          ;; Block does not store function backref —
                          ;; we cheat by finding the registry entry
                          ;; whose blocks include this one.
                          (cl-some (lambda (cell)
                                     (let ((fn (cdr cell)))
                                       (and (memq blk
                                                  (nelisp-cc--ssa-function-blocks fn))
                                            fn)))
                                   registry))
                     ;; final fallback — assume the caller name is
                     ;; the same as the callee meta (= self-call).
                     (let ((callee-name
                            (nelisp-cc--rec-inline-call-callee-name call-instr)))
                       (and callee-name
                            (cdr (assq callee-name registry)))))))
    (unless caller
      (signal 'nelisp-cc-rec-inline-error
              (list :unresolved-caller call-instr)))
    (let ((decision (nelisp-cc--rec-inline-decide
                     call-instr depth escape-info caller registry)))
      (eq :go (nelisp-cc--rec-inline-status-verdict decision)))))

;;; cloning helpers ------------------------------------------------

(defun nelisp-cc--rec-inline-clone-value (caller src-value value-map)
  "Allocate a fresh SSA value in CALLER cloned from SRC-VALUE.

VALUE-MAP is the active hash-table — when SRC-VALUE already maps
to a clone (= same value used twice in the source), the previous
clone is returned.  Param values *do not* clone here; they are
substituted via the param-arg map populated by the caller."
  (or (gethash src-value value-map)
      (let ((new (nelisp-cc--ssa-make-value
                  caller (nelisp-cc--ssa-value-type src-value))))
        (puthash src-value new value-map)
        new)))

(defun nelisp-cc--rec-inline-translate-operands (operands value-map)
  "Translate every operand in OPERANDS through VALUE-MAP."
  (mapcar (lambda (v) (or (gethash v value-map) v)) operands))

(defun nelisp-cc--rec-inline-clone-blocks
    (caller callee value-map block-map &optional original-snapshot original-state)
  "Allocate fresh blocks in CALLER cloned from CALLEE's blocks.

VALUE-MAP is populated with mappings for every callee def value as
the matching cloned instructions are issued; entries already
present (= the param-arg map) are preserved.  BLOCK-MAP is
populated with mappings from the original callee block to its new
caller-side clone.  The function does not link successor /
predecessor edges — the caller does that pass after clones are in
place so the link order is independent of CALLEE's internal block
ordering.

When CALLEE is CALLER (= self-recursion unroll), the source block
list is *snapshotted* up front — calls to `nelisp-cc--ssa-make-block'
append to CALLER's BLOCKS in place, and re-walking the live list
would cause the cloner to clone the clones (= unbounded recursion).

ORIGINAL-SNAPSHOT, when non-nil (T162 fix per Doc 42 §3.3 Phase 7.7.5+
post-mortem), overrides reading `(blocks callee)' at clone time.
Driver must pass the pristine pre-pass blocks list captured before any
splice — otherwise the second+ depth's `nelisp-cc-rec-inline-pass'
iteration sees the *grown* caller (= original blocks + previously-cloned
blocks), and the clone walker re-clones the previous clones →
exponential growth (block count: 2 → 22 → 766 → ∞ at depths 1/2/3 in the
original report).  When ORIGINAL-SNAPSHOT is nil, falls back to the live
blocks list (= original behaviour, OK when CALLEE != CALLER).

ORIGINAL-STATE (Phase 7.1 rec-inline CFG fix), when non-nil, is a
hash-table mapping each pre-pass source block to a cons
`(INSTRS-SNAPSHOT . SUCCS-SNAPSHOT)' captured by the driver before
any splice.  This is required at depth>=1 for the self-recursion
path: prior splices mutate the original blocks' INSTRS list (e.g.
the `else' block of a fib-like callee gets its self-call sites
removed and its terminator replaced with `jump' to a previously-
cloned entry).  Without this snapshot the cloner reads the *post-
mutation* instrs and the link pass reads the *post-mutation*
successors, leaving cloned blocks with `jump'/`branch' terminators
whose successor edges point to blocks that aren't in BLOCK-MAP →
silently dropped → backend `:jump-with-no-successor' encoding
error."
  (let ((src-blocks
         (or (and original-snapshot (copy-sequence original-snapshot))
             (copy-sequence (nelisp-cc--ssa-function-blocks callee))))
        (src-instr-snapshots (make-hash-table :test 'eq)))
    ;; Pass 0: snapshot every source block's instr list because the
    ;; clone might mutate CALLER's blocks (callee = caller path).
    ;; Prefer the driver-supplied ORIGINAL-STATE snapshot (pristine
    ;; pre-pass instrs) over the live block instrs, because by the
    ;; time depth>=1 splices run the live instrs may already have
    ;; been mutated by prior splices in this pass.
    (dolist (b src-blocks)
      (let ((snap-cell (and original-state (gethash b original-state))))
        (puthash b
                 (copy-sequence
                  (if snap-cell
                      (car snap-cell)
                    (nelisp-cc--ssa-block-instrs b)))
                 src-instr-snapshots)))
    (dolist (b src-blocks)
      (let ((label (nelisp-cc--ssa-block-label b)))
        (puthash b
                 (nelisp-cc--ssa-make-block
                  caller
                  (and label (concat label "/inl")))
                 block-map)))
    ;; Pass 2: clone instructions, populate value-map for defs.
    (dolist (b src-blocks)
      (let ((new-blk (gethash b block-map)))
        (dolist (instr (gethash b src-instr-snapshots))
          (let* ((src-def (nelisp-cc--ssa-instr-def instr))
                 (new-def (and src-def
                               (nelisp-cc--rec-inline-clone-value
                                caller src-def value-map))))
            ;; Translate operands now (by-value lookup in value-map);
            ;; values not yet mapped fall through unchanged so we
            ;; resolve them after the second pass below — but for
            ;; the common case (def precedes use within block order),
            ;; this is sufficient.  We re-walk + retranslate at the
            ;; end to catch inter-block forward references.
            (let* ((new-ops
                    (nelisp-cc--rec-inline-translate-operands
                     (nelisp-cc--ssa-instr-operands instr) value-map))
                   (new-meta
                    (nelisp-cc--rec-inline-rewrite-meta
                     (nelisp-cc--ssa-instr-meta instr) block-map value-map))
                   (cloned (nelisp-cc--ssa-instr-make
                            :id (nelisp-cc--ssa-function-next-instr-id caller)
                            :opcode (nelisp-cc--ssa-instr-opcode instr)
                            :operands new-ops
                            :def new-def
                            :block new-blk
                            :meta new-meta)))
              (cl-incf (nelisp-cc--ssa-function-next-instr-id caller))
              (when new-def
                (setf (nelisp-cc--ssa-value-def-point new-def) cloned))
              (dolist (op new-ops)
                (nelisp-cc--ssa-add-use op cloned))
              (setf (nelisp-cc--ssa-block-instrs new-blk)
                    (append (nelisp-cc--ssa-block-instrs new-blk)
                            (list cloned))))))))
    ;; Pass 3: re-translate operands now that every def is mapped
    ;; (catches phi arms that reference defs from later-cloned blocks).
    (dolist (b src-blocks)
      (let ((new-blk (gethash b block-map)))
        (dolist (instr (nelisp-cc--ssa-block-instrs new-blk))
          (let* ((old-ops (nelisp-cc--ssa-instr-operands instr))
                 (new-ops
                  (nelisp-cc--rec-inline-translate-operands old-ops value-map)))
            (unless (equal old-ops new-ops)
              ;; Operand list changed — patch use lists.
              (dolist (op old-ops)
                (setf (nelisp-cc--ssa-value-use-list op)
                      (delq instr (nelisp-cc--ssa-value-use-list op))))
              (setf (nelisp-cc--ssa-instr-operands instr) new-ops)
              (dolist (op new-ops)
                (nelisp-cc--ssa-add-use op instr))))
          ;; Re-translate meta (phi arms reference value-ids that
          ;; may still be the original callee's; rewrite-meta is
          ;; idempotent w.r.t. already-translated arms because the
          ;; lookup walks by ID).
          (let ((new-meta (nelisp-cc--rec-inline-rewrite-meta
                           (nelisp-cc--ssa-instr-meta instr)
                           block-map value-map)))
            (setf (nelisp-cc--ssa-instr-meta instr) new-meta)))))))

(defun nelisp-cc--rec-inline-rewrite-meta (meta block-map value-map)
  "Rewrite META plist — translate :then/:else block-ids and :phi-arms.

BLOCK-MAP maps original blocks to cloned blocks.  VALUE-MAP maps
original values to cloned values.  Returns a fresh plist (never
mutates META in place); plist keys not in the rewrite set fall
through unchanged."
  (when meta
    (let ((kv (copy-sequence meta))
          (out nil))
      (while kv
        (let ((k (pop kv))
              (v (pop kv)))
          (push k out)
          (push
           (cond
            ((memq k '(:then :else))
             ;; Block ID — locate the cloned block by walking the map.
             (or (catch 'found
                   (maphash (lambda (orig new)
                              (when (= v (nelisp-cc--ssa-block-id orig))
                                (throw 'found
                                       (nelisp-cc--ssa-block-id new))))
                            block-map)
                   v)
                 v))
            ((eq k :phi-arms)
             ;; alist of (PRED-BID . VAL-ID).  Rewrite both.
             (mapcar
              (lambda (cell)
                (let* ((pred-bid (car cell))
                       (val-id (cdr cell))
                       (new-pred-bid
                        (or (catch 'p
                              (maphash
                               (lambda (orig new)
                                 (when (= pred-bid
                                          (nelisp-cc--ssa-block-id orig))
                                   (throw 'p (nelisp-cc--ssa-block-id new))))
                               block-map)
                              pred-bid)
                            pred-bid))
                       (new-val-id
                        (or (catch 'v
                              (maphash
                               (lambda (orig new)
                                 (when (= val-id
                                          (nelisp-cc--ssa-value-id orig))
                                   (throw 'v (nelisp-cc--ssa-value-id new))))
                               value-map)
                              val-id)
                            val-id)))
                  (cons new-pred-bid new-val-id)))
              v))
            (t v))
           out)))
      (nreverse out))))

(defun nelisp-cc--rec-inline-link-cloned-edges
    (_callee block-map &optional original-state)
  "After clone, restore predecessor / successor edges between cloned blocks.

Walks BLOCK-MAP entries (= the snapshot of source blocks captured by
`nelisp-cc--rec-inline-clone-blocks') so the link pass is robust
against the self-recursion case where CALLEE = CALLER and the
caller's BLOCKS list now also contains the clones themselves.

ORIGINAL-STATE (Phase 7.1 rec-inline CFG fix), when non-nil, is the
same `(INSTRS . SUCCS)' snapshot hash-table consumed by
`nelisp-cc--rec-inline-clone-blocks'.  At depth>=1 each prior splice
mutates the original block's `successors' list in place (= sets it
to the just-spliced cloned-entry), so reading the live successor
list here would link the clones to the wrong (or missing) targets.
With the snapshot the cloner sees the pristine pre-pass CFG and the
clone-of-X gets edges to clone-of-Y for every original X→Y
edge (= preserves the callee body's CFG verbatim)."
  (let (src-blocks)
    (maphash (lambda (orig _new) (push orig src-blocks)) block-map)
    (dolist (b src-blocks)
      (let* ((new-from (gethash b block-map))
             (snap-cell (and original-state (gethash b original-state)))
             (succs (if snap-cell
                        (cdr snap-cell)
                      (nelisp-cc--ssa-block-successors b))))
        (dolist (s succs)
          (let ((new-to (gethash s block-map)))
            (when (and new-from new-to)
              (nelisp-cc--ssa-link-blocks new-from new-to))))))))

;;; base-case fold -------------------------------------------------

(defun nelisp-cc--rec-inline-const-literal (instr)
  "When INSTR is a `const' return its literal; nil otherwise.

This is the constant detector used by the base-case fold step."
  (and instr
       (eq (nelisp-cc--ssa-instr-opcode instr) 'const)
       (let ((meta (nelisp-cc--ssa-instr-meta instr)))
         (plist-member meta :literal))))

(defun nelisp-cc--rec-inline-resolve-const (val)
  "Return (LITERAL . t) when VAL is the def of a `const' instruction.
Returns nil otherwise."
  (let ((dp (and val (nelisp-cc--ssa-value-def-point val))))
    (when (and dp (not (eq dp :param))
               (eq (nelisp-cc--ssa-instr-opcode dp) 'const))
      (let ((meta (nelisp-cc--ssa-instr-meta dp)))
        (when (plist-member meta :literal)
          (cons (plist-get meta :literal) t))))))

(defun nelisp-cc--rec-inline-fold-branch (caller block)
  "When BLOCK ends in a `branch' on a constant operand, drop the dead arm.

Doc 42 §3.3 base-case fold — when the unroller has substituted a
literal into the branch condition the dead arm becomes
unreachable.  We rewrite the branch into a `jump' to the live arm
and *prune* the dead successor edge.  The dead block itself is
left in BLOCKS for now — Phase 7.7.5 dead-block elimination is
out of scope.  Returns t when a fold happened."
  (let ((terminator (car (last (nelisp-cc--ssa-block-instrs block)))))
    (when (and terminator
               (eq (nelisp-cc--ssa-instr-opcode terminator) 'branch))
      (let* ((cond-val (car (nelisp-cc--ssa-instr-operands terminator)))
             (lit (nelisp-cc--rec-inline-resolve-const cond-val)))
        (when lit
          (let* ((meta (nelisp-cc--ssa-instr-meta terminator))
                 (then-bid (plist-get meta :then))
                 (else-bid (plist-get meta :else))
                 (live-bid (if (car lit) then-bid else-bid))
                 (dead-bid (if (car lit) else-bid then-bid))
                 (live-blk
                  (cl-find-if (lambda (b)
                                (= live-bid (nelisp-cc--ssa-block-id b)))
                              (nelisp-cc--ssa-function-blocks caller)))
                 (dead-blk
                  (cl-find-if (lambda (b)
                                (= dead-bid (nelisp-cc--ssa-block-id b)))
                              (nelisp-cc--ssa-function-blocks caller))))
            (when (and live-blk dead-blk)
              ;; Drop the dead edge from CFG.
              (setf (nelisp-cc--ssa-block-successors block)
                    (delq dead-blk
                          (nelisp-cc--ssa-block-successors block)))
              (setf (nelisp-cc--ssa-block-predecessors dead-blk)
                    (delq block
                          (nelisp-cc--ssa-block-predecessors dead-blk)))
              ;; Rewrite the terminator to a void `jump' (no operand).
              (dolist (op (nelisp-cc--ssa-instr-operands terminator))
                (setf (nelisp-cc--ssa-value-use-list op)
                      (delq terminator
                            (nelisp-cc--ssa-value-use-list op))))
              (setf (nelisp-cc--ssa-instr-opcode terminator) 'jump
                    (nelisp-cc--ssa-instr-operands terminator) nil
                    (nelisp-cc--ssa-instr-meta terminator)
                    (list :folded t :live live-bid))
              t)))))))

;;; splice machinery -----------------------------------------------

(defun nelisp-cc--rec-inline-find-return (_callee block-map)
  "Find the cloned `return' instruction in CALLEE's image, or nil.

Returns a list (BLOCK INSTR OPERAND) when a unique return survives;
nil when multiple returns exist (splice falls back to leaving the
returns in place and using a phi).  For the MVP we only handle
single-return CFGs — multi-return needs the merge-block phi
construction which is Doc 42 §3.3 follow-up."
  (let (hits)
    (maphash
     (lambda (_orig new-blk)
       (dolist (instr (nelisp-cc--ssa-block-instrs new-blk))
         (when (eq 'return (nelisp-cc--ssa-instr-opcode instr))
           (push (list new-blk instr
                       (car (nelisp-cc--ssa-instr-operands instr)))
                 hits))))
     block-map)
    (and (= 1 (length hits)) (car hits))))

(defun nelisp-cc--rec-inline-splice-self
    (caller block call-instr callee value-map
            &optional original-snapshot original-state)
  "Splice CALLEE's body into CALLER replacing CALL-INSTR.

CALLER is the caller (= self-recursion target).  BLOCK is the
block containing CALL-INSTR.  CALL-INSTR is the self-recursive
`call' that is being unrolled this iteration.  CALLEE is CALLER
itself — the same SSA function — but the *clone source*; we
clone its blocks/instructions into CALLER's IR, substituting
parameter values with the call's argument values.

VALUE-MAP is a freshly-allocated hash-table to seed with the
param→arg mappings (the cloner reads it for operand
translation).

ORIGINAL-SNAPSHOT (T162 fix) is the pristine pre-pass blocks list
captured by the driver before any splice; passed through to
`nelisp-cc--rec-inline-clone-blocks' to avoid re-cloning previously
cloned blocks (Doc 42 §3.3 Phase 7.7.5+ post-mortem).

Multi-block CFG strategy:
  1. Split BLOCK at the call site into PRE (everything before the
     call) and POST (everything after the call, including the
     original block terminator).  PRE keeps the original block
     handle; POST is materialised as a *new* block.
  2. PRE's outgoing terminator becomes a `jump' to the cloned
     callee's entry block.
  3. The cloned callee's `return' instruction(s) are rewritten to
     `jump' to POST (single-return MVP).
  4. The original `call' instruction's def value (if any) is
     rebound via a `copy' instruction at POST's head whose operand
     is the cloned return's operand.

Returns BLOCK (the rewritten PRE block).  Mutates CALLER in place."
  (let* ((params (nelisp-cc--ssa-function-params callee))
         (args (nelisp-cc--ssa-instr-operands call-instr)))
    (unless (= (length params) (length args))
      (signal 'nelisp-cc-rec-inline-error
              (list :arity-mismatch
                    :callee (nelisp-cc--ssa-function-name callee)
                    :params (length params)
                    :args (length args))))
    (cl-mapc (lambda (p a) (puthash p a value-map)) params args)
    (let* ((block-map (make-hash-table :test 'eq))
           ;; Snapshot original instr list so the split is
           ;; deterministic.
           (orig-instrs (nelisp-cc--ssa-block-instrs block))
           (call-pos (cl-position call-instr orig-instrs :test #'eq))
           (pre-instrs (cl-subseq orig-instrs 0 call-pos))
           (post-instrs (cl-subseq orig-instrs (1+ call-pos)))
           (post-blk (nelisp-cc--ssa-make-block caller "rec-inline/post"))
           (call-def (nelisp-cc--ssa-instr-def call-instr)))
      ;; (1) clone the callee body into CALLER.
      (nelisp-cc--rec-inline-clone-blocks
       caller callee value-map block-map original-snapshot original-state)
      (nelisp-cc--rec-inline-link-cloned-edges callee block-map original-state)
      ;; (2) split BLOCK: keep PRE here, move POST to post-blk.
      (let ((cloned-entry
             (gethash (nelisp-cc--ssa-function-entry callee) block-map)))
        (setf (nelisp-cc--ssa-block-instrs block)
              (append pre-instrs
                      (list (nelisp-cc--rec-inline-build-jump
                             caller block cloned-entry))))
        ;; PRE → cloned entry edge.  Drop original successors of
        ;; BLOCK; they will be reattached to post-blk in step (3).
        (let ((orig-succs (nelisp-cc--ssa-block-successors block)))
          (dolist (s orig-succs)
            (setf (nelisp-cc--ssa-block-predecessors s)
                  (delq block (nelisp-cc--ssa-block-predecessors s)))
            (nelisp-cc--ssa-link-blocks post-blk s))
          (setf (nelisp-cc--ssa-block-successors block) nil))
        (nelisp-cc--ssa-link-blocks block cloned-entry)
        ;; Move POST instrs to post-blk (and patch their block
        ;; backref).
        (setf (nelisp-cc--ssa-block-instrs post-blk) post-instrs)
        (dolist (instr post-instrs)
          (setf (nelisp-cc--ssa-instr-block instr) post-blk))
        ;; (3) rewrite cloned `return' → `jump' to post-blk; bind
        ;; the original call's def via a `copy' at post-blk's head.
        (let ((return-info
               (nelisp-cc--rec-inline-find-return callee block-map)))
          (when return-info
            (cl-destructuring-bind (ret-blk ret-instr ret-operand)
                return-info
              ;; Patch call-def → translated return operand via copy.
              (when call-def
                (let* ((translated
                        (or (gethash ret-operand value-map) ret-operand))
                       (copy-instr
                        (nelisp-cc--rec-inline-make-instr
                         caller post-blk 'copy (list translated)
                         call-def nil)))
                  ;; Insert at post-blk's HEAD (before former post-instrs).
                  (setf (nelisp-cc--ssa-block-instrs post-blk)
                        (cons copy-instr
                              (nelisp-cc--ssa-block-instrs post-blk)))))
              ;; Replace return → jump.
              (dolist (op (nelisp-cc--ssa-instr-operands ret-instr))
                (setf (nelisp-cc--ssa-value-use-list op)
                      (delq ret-instr
                            (nelisp-cc--ssa-value-use-list op))))
              (setf (nelisp-cc--ssa-instr-opcode ret-instr) 'jump
                    (nelisp-cc--ssa-instr-operands ret-instr) nil
                    (nelisp-cc--ssa-instr-meta ret-instr)
                    (list :inlined-return t))
              (nelisp-cc--ssa-link-blocks ret-blk post-blk))))
        ;; (4) drop the eliminated `call' instr's outgoing use
        ;; back-pointers.
        (dolist (op (nelisp-cc--ssa-instr-operands call-instr))
          (setf (nelisp-cc--ssa-value-use-list op)
                (delq call-instr (nelisp-cc--ssa-value-use-list op))))
        block))))

(defun nelisp-cc--rec-inline-build-jump (caller block target-block)
  "Construct a `jump' instr at BLOCK's tail targeting TARGET-BLOCK.

The jump is *not* yet appended — the caller threads it into the
PRE instr list along with the surviving PRE instructions.  The
TARGET-BLOCK ID is recorded as `:target' meta for backend
inspection."
  (let ((instr (nelisp-cc--ssa-instr-make
                :id (nelisp-cc--ssa-function-next-instr-id caller)
                :opcode 'jump
                :operands nil
                :def nil
                :block block
                :meta (list :target
                            (nelisp-cc--ssa-block-id target-block)))))
    (cl-incf (nelisp-cc--ssa-function-next-instr-id caller))
    instr))

(defun nelisp-cc--rec-inline-make-instr (fn block opcode operands def meta)
  "Allocate an SSA instruction for FN/BLOCK without appending.

Mirror of `nelisp-cc--inline-make-instr' — exposes a builder that
the splice loop calls with manual ordering control."
  (let ((instr (nelisp-cc--ssa-instr-make
                :id (nelisp-cc--ssa-function-next-instr-id fn)
                :opcode opcode
                :operands operands
                :def def
                :block block
                :meta meta)))
    (cl-incf (nelisp-cc--ssa-function-next-instr-id fn))
    (when def
      (setf (nelisp-cc--ssa-value-def-point def) instr))
    (dolist (op operands)
      (nelisp-cc--ssa-add-use op instr))
    instr))

;;; top-level pass --------------------------------------------------

;;;###autoload
(defun nelisp-cc-rec-inline-pass (caller escape-info &optional registry depth-limit)
  "Run the recursive-inline pass on CALLER.

CALLER is mutated in place; every self-recursive `call' site whose
unroll decision is :go (per `nelisp-cc--rec-inline-decide') gets
the callee body spliced in.  ESCAPE-INFO is the verdict from
`nelisp-cc-escape-analyze' on CALLER.  REGISTRY is the standard
callee alist; the caller itself does not need to be in REGISTRY
(self-recursion lookup falls back to CALLER directly).

DEPTH-LIMIT, when non-nil, overrides
`nelisp-cc-rec-inline-depth-limit' for this call only — useful in
tests that want to drive the cost gate without flipping the
defcustom.  Negative values are clamped to 0.

Returns (CALLER . INLINED-COUNT) where INLINED-COUNT is the total
number of callsites eliminated across all unroll iterations.
The pass *does not* recurse into newly-inlined cloned bodies past
the depth limit — the deepest clone keeps its self-calls verbatim.

After splicing, the pass invokes
`nelisp-cc--rec-inline-fold-branch' on every cloned block whose
terminator branches on a constant operand, collapsing dead arms
when the unroller has substituted a literal into the condition
(Doc 42 §3.3 base-case fold)."
  (unless (nelisp-cc--ssa-function-p caller)
    (signal 'nelisp-cc-rec-inline-error
            (list :not-a-function caller)))
  (let* ((nelisp-cc-rec-inline-depth-limit
          (max 0 (or depth-limit nelisp-cc-rec-inline-depth-limit)))
         (count 0)
         ;; T162 fix: snapshot caller's blocks BEFORE any splice.
         ;; Each block's instr list is also snapshotted (= clone-blocks
         ;; will further snapshot per block, but we cache the outer
         ;; list here so subsequent depth iterations don't pick up the
         ;; cloned bodies that splice-self appends to caller's blocks).
         ;; This converts the previous exponential growth (2 → 22 → 766
         ;; at depths 1/2/3) into linear depth-bounded growth.
         (original-snapshot
          (copy-sequence (nelisp-cc--ssa-function-blocks caller)))
         ;; Phase 7.1 rec-inline CFG fix: also snapshot per-block
         ;; INSTRS and SUCCESSORS lists so that depth>=1 iterations
         ;; clone the *pristine* pre-pass callee body, not the post-
         ;; splice mutated form (where the original `else'-block has
         ;; lost its self-call instrs and its successor has been
         ;; redirected to a previously-cloned entry block).  Without
         ;; this snapshot the cloned blocks pick up `jump' opcodes
         ;; whose successor edges drop in `link-cloned-edges' (the
         ;; mutated successor isn't in the new block-map), surfacing
         ;; later as `:jump-with-no-successor' encoding error in the
         ;; backend.
         (original-state (make-hash-table :test 'eq)))
    (dolist (b original-snapshot)
      (puthash b
               (cons (copy-sequence (nelisp-cc--ssa-block-instrs b))
                     (copy-sequence (nelisp-cc--ssa-block-successors b)))
               original-state))
    (let ((sites (nelisp-cc--rec-inline-list-self-call-sites caller))
          (depth 0))
      (while (and sites
                  (< depth nelisp-cc-rec-inline-depth-limit))
        (let ((iter-count 0)
              (next-sites nil))
          (dolist (site sites)
            (let ((decision (nelisp-cc--rec-inline-decide
                             site depth escape-info caller registry)))
              (cond
               ((eq :go (nelisp-cc--rec-inline-status-verdict decision))
                (let ((blk (nelisp-cc--ssa-instr-block site))
                      (callee (nelisp-cc--rec-inline-status-callee decision))
                      (vmap (make-hash-table :test 'eq)))
                  (nelisp-cc--rec-inline-splice-self
                   caller blk site callee vmap
                   original-snapshot original-state)
                  (cl-incf count)
                  (cl-incf iter-count)))
               (t nil))))
          ;; After iterating once at this depth, harvest the new
          ;; self-call sites that the cloned bodies introduced for
          ;; the next iteration.
          (when (> iter-count 0)
            ;; Constant-fold any branch whose condition was
            ;; substituted with a literal during the splice (base-
            ;; case fold).
            (dolist (b (nelisp-cc--ssa-function-blocks caller))
              (nelisp-cc--rec-inline-fold-branch caller b))
            (setq next-sites
                  (nelisp-cc--rec-inline-list-self-call-sites caller)))
          (setq sites next-sites
                depth (1+ depth))
          ;; Stop if no new sites emerged (= unroll converged).
          (unless next-sites
            (setq sites nil)))))
    (cons caller count)))

;;;###autoload
(defun nelisp-cc-rec-inline-stats (caller escape-info &optional registry depth-limit)
  "Return an alist tallying decision verdicts for CALLER's self-call sites.

Shape:
  ((:inlined . N)
   (:abort
    ((:no-callee-name . N) (:unresolved-callee . N)
     (:non-self-recursive . N) (:depth-limit-reached . N)
     (:cost-budget-exceeded . N) (:escaped-arg . N))))

The function does not mutate CALLER — it samples the
decisions by calling `nelisp-cc--rec-inline-decide' against the
*current* IR shape at depth 0 and depth (DEPTH-LIMIT - 1).  Useful
for the Doc 42 §4.4 dashboard surface."
  (let* ((nelisp-cc-rec-inline-depth-limit
          (max 0 (or depth-limit nelisp-cc-rec-inline-depth-limit)))
         (inlined 0)
         (reasons (list (cons :no-callee-name 0)
                        (cons :unresolved-callee 0)
                        (cons :non-self-recursive 0)
                        (cons :depth-limit-reached 0)
                        (cons :cost-budget-exceeded 0)
                        (cons :escaped-arg 0))))
    (dolist (site (nelisp-cc--rec-inline-list-self-call-sites caller))
      (let ((decision (nelisp-cc--rec-inline-decide
                       site 0 escape-info caller registry)))
        (cond
         ((eq :go (nelisp-cc--rec-inline-status-verdict decision))
          (cl-incf inlined))
         (t
          (let* ((reason (nelisp-cc--rec-inline-status-reason decision))
                 (cell (assq reason reasons)))
            (when cell (cl-incf (cdr cell))))))))
    (list (cons :inlined inlined)
          (cons :abort reasons))))

(provide 'nelisp-cc-recursive-inline)
;;; nelisp-cc-recursive-inline.el ends here
