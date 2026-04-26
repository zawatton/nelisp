;;; nelisp-cc-lambda-lift.el --- Lambda lift pass (Doc 42 §3.4, Phase 7.7.4) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.7.4 prototype — *lambda lift pass* for the SSA IR built by
;; `src/nelisp-cc.el'.  Doc 42 LOCKED v2 §3.4 specifies the pass as the
;; *closing piece* of the Phase 7.7 inliner suite: where Phase 7.7.2
;; (simple inline) and Phase 7.7.3 (recursive inline) eliminate `call'
;; instructions to known top-level callees, the lambda lift pass
;; eliminates `closure' instructions whose def values are *locally
;; consumed* — the closure is materialised, immediately invoked via a
;; `call-indirect', and never escapes.  The pass rewrites the
;; (`closure' + `call-indirect') idiom into a single direct `call' to a
;; *lifted* top-level function, recovering the per-call constant
;; overhead that the indirect dispatch incurred.
;;
;; What this pass does (Doc 42 §3.4 deliverables):
;;
;;   1. *escape gate* — only closures whose def value is :stack-only
;;      per the T139 escape verdict are considered (= the closure does
;;      not flow into `return' / outer `closure' / `store-var' /
;;      foreign call).  Doc 42 §6.7 risk mitigation: lifting an
;;      escaping closure would observe caller-scope mutation through
;;      the materialised closure object, breaking semantics.  The
;;      :stack-only restriction sidesteps this entirely.
;;
;;   2. *single-call-site preference* — for the MVP a closure is
;;      lifted only when every use of its def value is a
;;      `call-indirect' with the closure value as operand[0] (= the
;;      function position).  Closures used as data (passed as a
;;      regular argument to another call) are :non-callee-use and
;;      stay untouched — they would need a different IR rewrite.
;;
;;   3. *captured-vars promotion (currently a no-op)* — the Phase 7.1
;;      frontend (`nelisp-cc--lower-lambda') does not yet thread
;;      captured/free variables through the `closure' instruction's
;;      operand list; the inner SSA function is built fresh and any
;;      free-var reference inside it lowers as a global `load-var'.
;;      The lambda lift transform that *would* add captured vars as
;;      explicit params is therefore exercised in this MVP only via
;;      its zero-capture branch — every operand of the matched
;;      `closure' is propagated verbatim into the lifted function's
;;      parameter signature.  The signature reshape machinery is
;;      written so the future frontend extension (Phase 7.7.5+) can
;;      add operands to `closure' without rewriting the lift pass.
;;
;;   4. *lifted-function registry* — the inner SSA function carried in
;;      the `closure' instr's META is registered under a freshly
;;      synthesised top-level name `<CALLER>$lift$N' (where N is a
;;      monotonically increasing counter inside this pass).  The
;;      registry is returned to callers as part of the pass's stats
;;      object so a downstream pipeline registrar can hand the lifted
;;      functions to the simple/recursive inliner for further
;;      flattening.
;;
;;   5. *call rewrite* — the matched `call-indirect' is mutated in
;;      place: opcode becomes `call', the closure operand is dropped
;;      from the operand list (the remaining operands are the actual
;;      arguments), and the META plist is rewritten to carry the
;;      lifted callee name (`:fn LIFTED-NAME :unresolved t :lifted t').
;;      The original `closure' instruction is deleted from its block —
;;      its def value's USE-LIST is then empty (every consumer was a
;;      `call-indirect' we just rewrote, and the rewrite already
;;      dropped the closure operand from each).
;;
;; Public API (Doc 42 §3.4 — `nelisp-cc-lift-' family):
;;
;;   `nelisp-cc-lift-pass'
;;       (CALLER ESCAPE-INFO &optional REGISTRY)
;;       → (TRANSFORMED-SSA . LIFTED-COUNT).  Mutates CALLER in place
;;       and returns it together with the count of *closures lifted*.
;;       REGISTRY, when supplied, is *augmented* with each newly lifted
;;       (NAME . SSA) cell so callers that reuse the alist across a
;;       multi-function pipeline accumulate every lifted function.
;;
;;   `nelisp-cc-lift-can-lift-p'
;;       (CLOSURE-INSTR ESCAPE-INFO)
;;       → bool — predicate over a single `closure' instruction; t
;;       iff the closure's def is :stack-only and every use is a
;;       `call-indirect' with the closure at operand[0].
;;
;;   `nelisp-cc-lift-stats'
;;       (CALLER ESCAPE-INFO &optional REGISTRY)
;;       → alist `((:lifted . N) (:abort ((REASON . N) ...))
;;                 (:registry . ((NAME . SSA) ...)))'.
;;       Sampling form — does not mutate CALLER.  The registry slot is
;;       a *copy* of REGISTRY (or nil) so the stats sample is read-only.
;;
;; Private helpers follow the `nelisp-cc--lift-' namespace.
;;
;; *Non-goals (= deferred to later sub-phases)*:
;;   - flat dispatch table emission (= backend-side optimisation,
;;     Phase 7.7.5+ — this pass only rewrites the IR, the backend
;;     still emits a normal direct call for the synthesised name).
;;   - captured-var operand threading (= Phase 7.1 frontend
;;     extension; once `closure' carries captured-var operands the
;;     `nelisp-cc--lift-extend-params' helper here will activate).
;;   - mutation-aware lifting (= post-Phase 7.7 follow-up; current
;;     conservative gate is :stack-only escape class only).

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)

(define-error 'nelisp-cc-lift-error
  "NeLisp lambda lift pass error" 'nelisp-cc-error)

;;; defcustom -------------------------------------------------------

(defcustom nelisp-cc-lambda-lift-enable nil
  "If non-nil, callers that wrap `nelisp-cc-lift-pass' may run it.

Advisory — `nelisp-cc-lift-pass' itself runs unconditionally when
invoked, so tests do not need to flip the defcustom.  Doc 42 §4.3 —
Phase 7.7.4 is opt-in until the closure-heavy bench (§5.4) measures
its own outcome."
  :type 'boolean
  :group 'nelisp-cc)

(defcustom nelisp-cc-lift-name-template "%s$lift$%d"
  "Format string used to synthesise lifted-function names.

Two arguments: the caller's symbol name (string) and the per-pass
monotonic counter (integer, 0-based).  The default produces names
shaped like `caller$lift$0', `caller$lift$1', etc., matching the
sigil convention used elsewhere in the Phase 7+ codebase."
  :type 'string
  :group 'nelisp-cc)

;;; internal stats container ---------------------------------------

(cl-defstruct (nelisp-cc--lift-status
               (:constructor nelisp-cc--lift-status-make)
               (:copier nil))
  "Per-closure-site lambda-lift decision returned by
`nelisp-cc--lift-decide'.

VERDICT is `:go' or `:abort'.  REASON, when VERDICT is :abort, is
one of:
  :not-a-closure, :no-inner-function, :no-def-value,
  :not-stack-only, :no-uses, :non-callee-use,
  :wrong-operand-position.
INNER, when the closure is well-formed, is the inner
`nelisp-cc--ssa-function' (= the body of the lambda being lifted)
or nil."
  (verdict :abort)
  (reason nil)
  (inner nil))

;;; predicates -----------------------------------------------------

(defun nelisp-cc--lift-closure-inner-fn (closure-instr)
  "Return the inner SSA function carried in CLOSURE-INSTR's meta, or nil.

The Phase 7.1 frontend stores the freshly built lambda body under
`:inner-function' (see `nelisp-cc--lower-lambda')."
  (and closure-instr
       (eq (nelisp-cc--ssa-instr-opcode closure-instr) 'closure)
       (plist-get (nelisp-cc--ssa-instr-meta closure-instr)
                  :inner-function)))

(defun nelisp-cc--lift-list-closure-sites (caller)
  "Return every `closure' instruction in CALLER, in block / instr order."
  (let (acc)
    (dolist (b (nelisp-cc--ssa-function-blocks caller))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) 'closure)
          (push instr acc))))
    (nreverse acc)))

(defun nelisp-cc--lift-uses-only-as-callee-p (closure-def)
  "Return t when every CLOSURE-DEF use is callee-position or letrec-init store.

CLOSURE-DEF is the SSA value produced by a `closure' instruction.
The lambda lift MVP rewrites uses where the closure flows into
the function position of an indirect call.  T161 additionally
tolerates `:store-var :letrec-init' uses — once the T161 pre-pass
rewrote every call-indirect referencing the slot to a direct
`:call', the letrec slot is write-only and the store survives only
as dead-code-pending IR.  Anywhere else (return / data argument /
non-letrec store-var / opaque call) the rewrite would change
semantics or require a different transform.

Returns nil when the use-list is empty (= :no-uses; surfaced as a
distinct abort reason by the decision helper)."
  (let ((uses (nelisp-cc--ssa-value-use-list closure-def)))
    (and uses
         (cl-every
          (lambda (user)
            (let ((opcode (nelisp-cc--ssa-instr-opcode user))
                  (ops (nelisp-cc--ssa-instr-operands user))
                  (meta (nelisp-cc--ssa-instr-meta user)))
              (cond
               ;; Allowed: callee-position of call-indirect.
               ((and (eq opcode 'call-indirect)
                     ops
                     (eq (car ops) closure-def))
                t)
               ;; Allowed (T161): letrec-init store-var.
               ((and (eq opcode 'store-var)
                     (plist-get meta :letrec-init))
                t)
               (t nil))))
          uses))))

(defun nelisp-cc--lift-def-escapes-besides-callee-p (closure-def escape-info)
  "Return t when CLOSURE-DEF escapes via a sink other than callee-position.

The cached escape verdict (T139) marks every operand of
`call-indirect' as :heap-required because the analysis treats the
opcode as an opaque escape source — but the lambda-lift rewrite
*proves* the closure does not actually escape: every callee-position
use is converted into a direct `call', so the closure object is
never observable outside the current frame.

This helper re-walks the def's USE-LIST and returns t iff there is
at least one user whose use is *not* the callee-position operand of
a `call-indirect' (= a sink that the lift transform would not
neutralise).  Mirrors the narrow re-walk that
`nelisp-cc--inline-value-escapes-besides-call-p' does for the
simple inliner.

ESCAPE-INFO is currently unused; the helper takes it for symmetry
with the other lift-side predicates and so the future captured-var
extension can consult per-operand verdicts."
  (ignore escape-info)
  (catch 'esc
    (dolist (user (nelisp-cc--ssa-value-use-list closure-def))
      (let ((opcode (nelisp-cc--ssa-instr-opcode user))
            (ops (nelisp-cc--ssa-instr-operands user))
            (meta (nelisp-cc--ssa-instr-meta user)))
        (cond
         ;; Allowed sink: callee position of `call-indirect'.
         ((and (eq opcode 'call-indirect)
               ops
               (eq (car ops) closure-def)
               ;; Closure must NOT also appear in the data-arg
               ;; positions — that would still be an escape.
               (not (memq closure-def (cdr ops))))
          nil)
         ;; T161 — Allowed sink: `:store-var :letrec-init' write
         ;; (= the outer letrec slot fill).  Once every
         ;; `:call-indirect' use of the closure is rewritten to a
         ;; direct `:call' (T161 pre-pass) the letrec slot becomes
         ;; *write-only* — no remaining instruction in the function
         ;; reads back from the slot.  The store survives in the IR
         ;; (a future DCE pass would remove it) but does not
         ;; constitute a real escape — the closure object is never
         ;; observed outside the current frame.
         ((and (eq opcode 'store-var)
               (plist-get meta :letrec-init))
          nil)
         (t (throw 'esc t)))))
    nil))

(defun nelisp-cc--lift-decide (closure-instr escape-info)
  "Compute the lambda-lift decision for CLOSURE-INSTR.

ESCAPE-INFO is the verdict produced by `nelisp-cc-escape-analyze'
on the function that owns CLOSURE-INSTR.  Returns an
`nelisp-cc--lift-status'.

Order of checks (= ordered abort reasons for stable dashboards):
  1. :not-a-closure          — opcode is not `closure'
  2. :no-inner-function      — META is missing `:inner-function'
  3. :no-def-value           — instruction has no def (defensive)
  4. :no-uses                — def is unused (no callsite to lift)
  5. :non-callee-use         — at least one use is not `call-indirect'
                                or the closure is at the wrong operand slot
  6. :not-stack-only         — escape gate (narrow re-walk subtracting
                                callee-position uses, since the rewrite
                                eliminates them) detects another sink

The :not-stack-only check uses
`nelisp-cc--lift-def-escapes-besides-callee-p' instead of the cached
T139 verdict because the cached verdict over-approximates: every
`call-indirect' operand is :heap-required by default, but the lift
rewrite removes those uses, so for our purposes the only escapes
that matter are the *other* sinks (`return' / outer `closure' /
`store-var' / opaque `call' as a data argument)."
  (cond
   ((not (eq (nelisp-cc--ssa-instr-opcode closure-instr) 'closure))
    (nelisp-cc--lift-status-make
     :verdict :abort :reason :not-a-closure))
   ((null (nelisp-cc--lift-closure-inner-fn closure-instr))
    (nelisp-cc--lift-status-make
     :verdict :abort :reason :no-inner-function))
   ((null (nelisp-cc--ssa-instr-def closure-instr))
    (nelisp-cc--lift-status-make
     :verdict :abort :reason :no-def-value
     :inner (nelisp-cc--lift-closure-inner-fn closure-instr)))
   (t
    (let* ((def (nelisp-cc--ssa-instr-def closure-instr))
           (inner (nelisp-cc--lift-closure-inner-fn closure-instr))
           (uses (nelisp-cc--ssa-value-use-list def)))
      (cond
       ((null uses)
        (nelisp-cc--lift-status-make
         :verdict :abort :reason :no-uses :inner inner))
       ((not (nelisp-cc--lift-uses-only-as-callee-p def))
        ;; Differentiate the two failure modes: if every use *is* a
        ;; `call-indirect' but at least one places the closure at a
        ;; non-zero operand index, surface :wrong-operand-position;
        ;; otherwise surface :non-callee-use.  Both are dashboard-
        ;; visible Phase 7.7.5+ candidates.
        (nelisp-cc--lift-status-make
         :verdict :abort
         :reason
         (if (cl-every
              (lambda (u)
                (eq (nelisp-cc--ssa-instr-opcode u) 'call-indirect))
              uses)
             :wrong-operand-position
           :non-callee-use)
         :inner inner))
       ((nelisp-cc--lift-def-escapes-besides-callee-p def escape-info)
        (nelisp-cc--lift-status-make
         :verdict :abort :reason :not-stack-only :inner inner))
       (t
        (nelisp-cc--lift-status-make
         :verdict :go :inner inner)))))))

;;;###autoload
(defun nelisp-cc-lift-can-lift-p (closure-instr escape-info)
  "Return t when CLOSURE-INSTR is a lift candidate per ESCAPE-INFO.

CLOSURE-INSTR must be a `closure' instruction.  ESCAPE-INFO is the
verdict from `nelisp-cc-escape-analyze' on the function that owns
the instruction.

Convenience wrapper around `nelisp-cc--lift-decide' returning
just the boolean verdict.  Useful for ad-hoc inspection /
dashboard surfaces; the full decision struct is available via
the private decision helper for callers that need the abort
reason."
  (eq :go
      (nelisp-cc--lift-status-verdict
       (nelisp-cc--lift-decide closure-instr escape-info))))

;;; name synthesis -------------------------------------------------

(defun nelisp-cc--lift-make-name (caller counter)
  "Synthesise a fresh top-level symbol for the COUNTER-th lift in CALLER.

Uses `nelisp-cc-lift-name-template' to format the name.  The result
is interned so callers can use `eq' / `assq' on the registry."
  (let* ((caller-name (or (nelisp-cc--ssa-function-name caller)
                          'anon))
         (name-str (format nelisp-cc-lift-name-template
                           (symbol-name caller-name)
                           counter)))
    (intern name-str)))

;;; rewrite primitives ---------------------------------------------

(defun nelisp-cc--lift-rewrite-call-indirect (call-instr lifted-name)
  "Mutate CALL-INSTR in place from `call-indirect' into a direct `call'.

The closure operand (at position 0) is dropped from the operand
list — its USE-LIST entry for CALL-INSTR is removed so the
post-rewrite IR remains consistent.  The META plist is rebuilt as
`(:fn LIFTED-NAME :unresolved t :lifted t)' so the backend's
direct-call lowering can route to the lifted function and the
dashboard can flag synthesised callees.

Returns CALL-INSTR (mutated)."
  (unless (eq (nelisp-cc--ssa-instr-opcode call-instr) 'call-indirect)
    (signal 'nelisp-cc-lift-error
            (list :not-a-call-indirect call-instr)))
  (let* ((operands (nelisp-cc--ssa-instr-operands call-instr))
         (closure-val (car operands))
         (rest-args (cdr operands)))
    (unless closure-val
      (signal 'nelisp-cc-lift-error
              (list :empty-call-indirect call-instr)))
    ;; Drop CALL-INSTR from closure-val's USE-LIST.
    (setf (nelisp-cc--ssa-value-use-list closure-val)
          (delq call-instr
                (nelisp-cc--ssa-value-use-list closure-val)))
    (setf (nelisp-cc--ssa-instr-opcode call-instr) 'call
          (nelisp-cc--ssa-instr-operands call-instr) rest-args
          (nelisp-cc--ssa-instr-meta call-instr)
          (list :fn lifted-name :unresolved t :lifted t))
    call-instr))

(defun nelisp-cc--lift-drop-closure-instr (_caller closure-instr)
  "Remove CLOSURE-INSTR from its block and clear its operands' USE-LIST.

CALLER is unused today (the block back-pointer carries enough
context) but is reserved as a positional argument so the future
captured-var extension can re-thread caller-side bookkeeping
without changing the call site.

The Phase 7.1 frontend currently emits `closure' with no operands
(captured vars are not yet threaded), so the operand walk is a
defensive no-op — but we keep it so the future captured-var
extension does not silently leak USE-LIST back-pointers."
  (let ((blk (nelisp-cc--ssa-instr-block closure-instr)))
    (unless blk
      (signal 'nelisp-cc-lift-error
              (list :orphan-closure-instr closure-instr)))
    ;; Defensive: drop USE-LIST entries for any operand of the
    ;; closure (none today, future-proof against captured-var
    ;; extension).
    (dolist (op (nelisp-cc--ssa-instr-operands closure-instr))
      (setf (nelisp-cc--ssa-value-use-list op)
            (delq closure-instr
                  (nelisp-cc--ssa-value-use-list op))))
    ;; Detach from block INSTRS.
    (setf (nelisp-cc--ssa-block-instrs blk)
          (delq closure-instr
                (nelisp-cc--ssa-block-instrs blk)))
    ;; Clear backref so a stale reference cannot reach a live block.
    (setf (nelisp-cc--ssa-instr-block closure-instr) nil)
    nil))

(defun nelisp-cc--lift-extend-params (inner _closure-instr)
  "Future hook — extend INNER's params with captured-var operands.

Today the Phase 7.1 frontend never threads captures through
`closure', so the call collapses to returning INNER unchanged.  The
helper exists so the Phase 7.7.5+ extension that materialises
captured vars at the SSA level can plug in without reshaping the
top-level pass.

Always returns INNER."
  inner)

;;; lift driver ----------------------------------------------------

(defun nelisp-cc--lift-name-and-record (caller inner counter registry-cell)
  "Allocate a fresh lifted-name for INNER inside CALLER.

REGISTRY-CELL is a *cons* whose `car' is the live registry alist
(`((NAME . SSA) ...)').  When a name is synthesised the helper
re-binds the cell's car to the *augmented* alist so callers reading
`(car registry-cell)' after the lift see the new entry.  Mutating
through the cell rather than returning a fresh alist preserves
caller-side identity for the multi-function pipeline use case
(passing the same alist into successive `nelisp-cc-lift-pass' calls
across a registry of functions).

Sets INNER's NAME to the synthesised symbol so the lifted function
can be pretty-printed / inspected without ambiguity.

Returns the synthesised name."
  (let ((name (nelisp-cc--lift-make-name caller counter)))
    (setf (nelisp-cc--ssa-function-name inner) name)
    (setcar registry-cell
            (cons (cons name inner) (car registry-cell)))
    name))

(defun nelisp-cc--lift-drop-letrec-init-store (store-instr closure-def)
  "Drop a `:store-var :letrec-init t' use whose operand is CLOSURE-DEF.

The store survives in the IR after T161 rewrote every `:call-indirect'
slot reader to a direct `:call' (no remaining instruction reads from
the slot).  Lambda lift now eliminates the closure that fed the
store; the store itself becomes dead — we remove it from its block
and drop CLOSURE-DEF from its USE-LIST entry.

Defensive: also clears the store's def-value's USE-LIST in case any
spurious downstream reference exists (none expected by construction
since slot reads have all been rewritten)."
  (let ((blk (nelisp-cc--ssa-instr-block store-instr)))
    (when blk
      (setf (nelisp-cc--ssa-block-instrs blk)
            (delq store-instr (nelisp-cc--ssa-block-instrs blk))))
    (setf (nelisp-cc--ssa-value-use-list closure-def)
          (delq store-instr (nelisp-cc--ssa-value-use-list closure-def)))
    (setf (nelisp-cc--ssa-instr-block store-instr) nil)
    nil))

(defun nelisp-cc--lift-apply-one (caller closure-instr inner counter registry-cell)
  "Apply the lambda lift transform to one CLOSURE-INSTR in CALLER.

Threads through `nelisp-cc--lift-name-and-record' to mint a name +
register the lifted callee, rewrites every use-site
`call-indirect' to a direct `call', drops any `:store-var
:letrec-init t' uses (T161 — write-only after slot reads are
rewritten), then drops the `closure' instruction.  Returns the
synthesised lifted name."
  (let* ((extended-inner (nelisp-cc--lift-extend-params inner closure-instr))
         (name (nelisp-cc--lift-name-and-record
                caller extended-inner counter registry-cell))
         (def (nelisp-cc--ssa-instr-def closure-instr))
         ;; Snapshot the use-list — the rewrite mutates it.
         (uses (copy-sequence (nelisp-cc--ssa-value-use-list def))))
    (dolist (use uses)
      (let ((opcode (nelisp-cc--ssa-instr-opcode use))
            (meta (nelisp-cc--ssa-instr-meta use)))
        (cond
         ((eq opcode 'call-indirect)
          (nelisp-cc--lift-rewrite-call-indirect use name))
         ((and (eq opcode 'store-var)
               (plist-get meta :letrec-init))
          (nelisp-cc--lift-drop-letrec-init-store use def))
         (t
          (signal 'nelisp-cc-lift-error
                  (list :unexpected-use opcode use))))))
    (nelisp-cc--lift-drop-closure-instr caller closure-instr)
    name))

;;; top-level pass --------------------------------------------------

;;;###autoload
(defun nelisp-cc-lift-pass (caller escape-info &optional registry)
  "Run the lambda-lift pass on CALLER.

CALLER is mutated in place; every `closure' instruction whose
decision is :go (per `nelisp-cc--lift-decide') is replaced by a
direct `call' to a freshly synthesised top-level name.  ESCAPE-INFO
is the verdict from `nelisp-cc-escape-analyze' on CALLER (the
:stack-only gate consumes it directly).

REGISTRY, when supplied, is treated as a *seed* alist for the
lifted-function registry — every newly minted (NAME . SSA) cell is
*prepended* to it (so the order matches lift order in reverse), and
the augmented registry is reachable via `nelisp-cc-lift-stats' (or
by the caller persisting the same alist across multiple pass
invocations through a wrapping cons).

Returns (CALLER . LIFTED-COUNT) — the count of `closure'
instructions eliminated.  When no closure passes the gate the IR is
unchanged and the count is 0."
  (unless (nelisp-cc--ssa-function-p caller)
    (signal 'nelisp-cc-lift-error
            (list :not-a-function caller)))
  (let* ((sites (nelisp-cc--lift-list-closure-sites caller))
         (registry-cell (cons (or registry nil) nil))
         (counter 0)
         (count 0))
    (dolist (site sites)
      (let ((decision (nelisp-cc--lift-decide site escape-info)))
        (when (eq :go (nelisp-cc--lift-status-verdict decision))
          (let ((inner (nelisp-cc--lift-status-inner decision)))
            (nelisp-cc--lift-apply-one
             caller site inner counter registry-cell)
            (cl-incf counter)
            (cl-incf count)))))
    (cons caller count)))

;;;###autoload
(defun nelisp-cc-lift-stats (caller escape-info &optional registry)
  "Return an alist tallying decision verdicts at every `closure' site.

Shape:
  ((:lifted . N)
   (:abort
    ((:not-a-closure . N) (:no-inner-function . N) (:no-def-value . N)
     (:no-uses . N) (:non-callee-use . N) (:wrong-operand-position . N)
     (:not-stack-only . N)))
   (:registry . ((NAME . SSA) ...)))

The function does *not* mutate CALLER — every site's decision is
sampled by calling `nelisp-cc--lift-decide' against the *current* IR
shape.  REGISTRY, when supplied, is included verbatim in the
:registry slot (a copy is made so caller-side mutations to the
returned alist do not propagate).

Useful for the Doc 42 §4.4 dashboard surface (mirrors
`nelisp-cc-rec-inline-stats')."
  (unless (nelisp-cc--ssa-function-p caller)
    (signal 'nelisp-cc-lift-error
            (list :not-a-function caller)))
  (let ((lifted 0)
        (aborts (list (cons :not-a-closure 0)
                      (cons :no-inner-function 0)
                      (cons :no-def-value 0)
                      (cons :no-uses 0)
                      (cons :non-callee-use 0)
                      (cons :wrong-operand-position 0)
                      (cons :not-stack-only 0))))
    (dolist (site (nelisp-cc--lift-list-closure-sites caller))
      (let ((decision (nelisp-cc--lift-decide site escape-info)))
        (cond
         ((eq :go (nelisp-cc--lift-status-verdict decision))
          (cl-incf lifted))
         (t
          (let* ((reason (nelisp-cc--lift-status-reason decision))
                 (cell (assq reason aborts)))
            (when cell
              (cl-incf (cdr cell))))))))
    (list (cons :lifted lifted)
          (cons :abort aborts)
          (cons :registry (copy-sequence (or registry nil))))))

(provide 'nelisp-cc-lambda-lift)
;;; nelisp-cc-lambda-lift.el ends here
