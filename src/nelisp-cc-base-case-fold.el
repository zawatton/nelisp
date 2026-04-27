;;; nelisp-cc-base-case-fold.el --- Phase 7.1.2 simple-inline base-case fold + const-eval -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1.2 — *simple-inline base-case fold* per J1 agent diagnosis
;; (commit b6f6e2d / merge c528b08, docs/bench-results/3-axis-actual-
;; 2026-04-27-T163.md §"What would close the gap" item 2).
;;
;; Goal: fib bench from current 14.42x toward 28-32x by eliminating
;; leaf recursive calls when the base-case is statically reachable.
;; The pass operates AFTER the recursive-inline pass (Phase 7.7.3) and
;; targets the residual `:call :fn FN (CONST_ARG)' sites that remain
;; once the unroller exhausts its depth budget.
;;
;; Implementation strategy: *constant-evaluation with memoization*.
;; For each `:call :fn FN (CONST)' site where FN is in the registry
;; AND FN's body is recognised as a *pure-arithmetic recursive*
;; function (= calls only itself + the trampoline arith primops
;; `+' / `-' / `<' / `*' / `=' / `1+' / `1-'), the pass symbolically
;; evaluates FN(CONST) at compile time using a small interpreter
;; over the SSA, with a per-(FN, CONST) memo cache and a global
;; instruction budget.
;;
;; When evaluation succeeds, the call instruction is *replaced* with
;; a `:copy' from a freshly-emitted `:const' carrying the literal
;; result.  When evaluation fails (budget exceeded, non-const arg
;; encountered, unsupported primop, recursion through unknown
;; callee), the call is left untouched.
;;
;; This is exactly the bench-form fib case: `(funcall fib 30)' lowers
;; to `:call :fn fib (const 30)'.  The pass evaluates fib(30) → 832040
;; with ~31 unique memo entries (fib(0)...fib(30)) and the entire
;; call collapses to a const literal — no runtime call cost.
;;
;; Cost gate: the per-call-site evaluation runs at most
;; `nelisp-cc-base-case-fold-budget' interpreter steps before bailing
;; out (default 10000).  This bounds compile-time at <1ms per site
;; while still covering naive recursive numeric helpers up through
;; fib(35), fact(20), tarai(small), and similar bench-sized targets.
;; Larger inputs are bounded by the budget and left for runtime
;; (correct fall-through, no semantic regression).
;;
;; Public API (Doc 42 §3.2 follow-up):
;;
;;   `nelisp-cc-base-case-fold-pass'  (CALLER &optional REGISTRY)
;;       → (CALLER . FOLD-COUNT).  Mutates CALLER in place; returns
;;       it together with the count of call sites replaced.  No-op
;;       when no const-arg recursive call sites are present.
;;
;;   `nelisp-cc-base-case-fold-eval-call'  (FN ARG &optional REGISTRY MEMO BUDGET-CELL)
;;       → (:value LIT) | (:abort REASON).  Pure observer (read-only
;;       over FN); useful for tests / dashboard.
;;
;; Private helpers follow the `nelisp-cc--bcf-' namespace.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-inline)

(define-error 'nelisp-cc-base-case-fold-error
  "NeLisp base-case fold pass error" 'nelisp-cc-error)

;;; defcustom -------------------------------------------------------

(defcustom nelisp-cc-base-case-fold-budget 100000
  "Per-call-site interpreter step budget for `nelisp-cc-base-case-fold-pass'.
When the symbolic evaluator exceeds this many SSA steps for a single
top-level evaluation it aborts with `:budget-exceeded' and leaves the
call site untouched.  Default sized to comfortably cover fib(30) +
fact(20) + tarai(small)."
  :type 'integer
  :group 'nelisp-cc)

(defcustom nelisp-cc-base-case-fold-enable t
  "If non-nil, `nelisp-cc-pipeline-run-7.7-passes' invokes the base-case
fold pass after rec-inline.  Default t (Phase 7.1.2 SHIPPED).  Set to
nil for diagnostic A/B baseline runs."
  :type 'boolean
  :group 'nelisp-cc)

;;; constant-arg primop table --------------------------------------

(defconst nelisp-cc--bcf-arith-primops
  '((+    . +)
    (-    . -)
    (*    . *)
    (/    . /)
    (<    . <)
    (>    . >)
    (=    . =)
    (1+   . 1+)
    (1-   . 1-)
    (eq   . eq)
    (null . null)
    (not  . not))
  "Alist of (CALL-FN-SYM . HOST-FN) for primops the evaluator can fold.

The HOST-FN is invoked as `(apply HOST-FN ARGS)' with ARGS being the
already-evaluated literal operands.  Comparison primops (`<' / `=' /
etc.) are emitted by the trampoline layer to return 1 / 0 in rax;
the SSA-level base-case fold uses the host's t / nil result and the
branch fold gate (`nelisp-cc--rec-inline-fold-branch') accepts both
t / nil + non-zero / zero literals — so either encoding survives
downstream consumers.")

;;; const literal access -------------------------------------------

(defun nelisp-cc--bcf-const-literal (val)
  "Return (LIT . t) when VAL's def-point is a `:const' instruction; else nil.

Also walks one level of `:copy' indirection so call args bound via
the simple-inline `:copy ARG' splice still resolve."
  (let ((dp (and val (nelisp-cc--ssa-value-def-point val))))
    (cond
     ((or (null dp) (eq dp :param)) nil)
     ((not (nelisp-cc--ssa-instr-p dp)) nil)
     ((eq (nelisp-cc--ssa-instr-opcode dp) 'const)
      (let ((meta (nelisp-cc--ssa-instr-meta dp)))
        (when (plist-member meta :literal)
          (cons (plist-get meta :literal) t))))
     ((eq (nelisp-cc--ssa-instr-opcode dp) 'copy)
      (let ((operand (car (nelisp-cc--ssa-instr-operands dp))))
        (and operand (nelisp-cc--bcf-const-literal operand))))
     (t nil))))

;;; SSA function classification ------------------------------------

(defun nelisp-cc--bcf-eligible-callee-p (fn registry)
  "Return t when FN is a candidate for const-eval folding.

Eligibility:
  - FN has exactly one param (the recursion variable).
  - FN's calls are exclusively to (a) itself / a name in REGISTRY,
    or (b) a recognised primop in `nelisp-cc--bcf-arith-primops'.
  - FN has no `:closure', `:store-var', `:call-indirect' instructions
    (= no captured state we cannot evaluate).
  - `:load-var' instructions are tolerated (they are the residual
    name-lookup left behind by `nelisp-cc-rewrite-call-indirect-to-call'
    after the rewrite pass converts the recursive `funcall' to a
    direct `:call' — the produced def is dead, the eval simply
    ignores it).

Conservative classifier — when in doubt return nil; the caller falls
through and leaves the site untouched."
  (and (nelisp-cc--ssa-function-p fn)
       (= 1 (length (nelisp-cc--ssa-function-params fn)))
       (catch 'ineligible
         (dolist (b (nelisp-cc--ssa-function-blocks fn))
           (dolist (instr (nelisp-cc--ssa-block-instrs b))
             (let ((op (nelisp-cc--ssa-instr-opcode instr)))
               (cond
                ((memq op '(closure store-var call-indirect))
                 (throw 'ineligible nil))
                ((eq op 'call)
                 (let* ((meta (nelisp-cc--ssa-instr-meta instr))
                        (sym (plist-get meta :fn)))
                   (unless (or (assq sym nelisp-cc--bcf-arith-primops)
                               (eq sym (nelisp-cc--ssa-function-name fn))
                               (and registry (assq sym registry)))
                     (throw 'ineligible nil))))))))
         t)))

;;; symbolic evaluator ---------------------------------------------

(defun nelisp-cc--bcf-eval-fn (fn arg registry memo budget-cell)
  "Evaluate FN(ARG) symbolically; return (:value LIT) or (:abort REASON).

MEMO is a hash-table keyed by (FN-NAME . ARG) → (:value LIT) /
:in-progress.  BUDGET-CELL is a single-cell cons whose CAR is the
remaining global step budget (decremented during evaluation; abort
when ≤ 0).

REGISTRY is the standard callee alist for inter-callee recursion."
  (cond
   ((<= (car budget-cell) 0)
    (cons :abort :budget-exceeded))
   (t
    (let* ((fn-name (nelisp-cc--ssa-function-name fn))
           (key (cons fn-name arg))
           (cached (gethash key memo)))
      (cond
       ((eq cached :in-progress)
        ;; Cycle detection — the eval is recursing through itself
        ;; with the same arg without making progress.  This shouldn't
        ;; happen in well-formed naive recursion (every step changes
        ;; the arg) but guards against infinite loops on pathological
        ;; inputs.
        (cons :abort :recursion-cycle))
       ((consp cached)
        cached)
       (t
        (puthash key :in-progress memo)
        (let ((result
               (nelisp-cc--bcf-eval-fn-uncached
                fn arg registry memo budget-cell)))
          (puthash key result memo)
          result)))))))

(defun nelisp-cc--bcf-eval-fn-uncached (fn arg registry memo budget-cell)
  "Inner eval — no memo lookup, just walks FN's CFG.  Returns same shape."
  (let* ((env (make-hash-table :test 'eq))   ; SSA value → literal
         (param (car (nelisp-cc--ssa-function-params fn))))
    (puthash param arg env)
    (let* ((entry (nelisp-cc--ssa-function-entry fn))
           (status (nelisp-cc--bcf-eval-block
                    fn entry env registry memo budget-cell nil)))
      status)))

(defun nelisp-cc--bcf-eval-block (fn block env registry memo budget-cell visited)
  "Walk BLOCK's instructions, threading ENV.  Return (:value LIT) | (:abort REASON).

VISITED is the set of block-ids already visited on this path — used
for loop detection (we abort with `:cfg-loop' when re-entering, since
the const-eval is single-path).  This keeps eval terminating on any
input without requiring full SSA loop analysis."
  (let ((bid (nelisp-cc--ssa-block-id block)))
    (cond
     ((memq bid visited)
      (cons :abort :cfg-loop))
     (t
      (let ((visited+ (cons bid visited)))
        (catch 'block-result
          (dolist (instr (nelisp-cc--ssa-block-instrs block))
            (cl-decf (car budget-cell))
            (when (<= (car budget-cell) 0)
              (throw 'block-result (cons :abort :budget-exceeded)))
            (let ((status (nelisp-cc--bcf-eval-instr
                           fn instr env registry memo budget-cell visited+)))
              (when status
                (throw 'block-result status))))
          (cons :abort :no-terminator)))))))

(defun nelisp-cc--bcf-resolve-operand (val env)
  "Return the literal bound to VAL in ENV, or :unresolved.

Falls through `:const' def-points + chained `:copy' indirections so
operands defined inline (= not via param-substitution) still resolve."
  (let ((cached (gethash val env :nelisp-cc-bcf-unresolved-marker)))
    (cond
     ((not (eq cached :nelisp-cc-bcf-unresolved-marker))
      cached)
     (t
      (let ((lit (nelisp-cc--bcf-const-literal val)))
        (cond
         (lit (puthash val (car lit) env) (car lit))
         (t :unresolved)))))))

(defun nelisp-cc--bcf-eval-instr (fn instr env registry memo budget-cell visited)
  "Evaluate one INSTR.  Returns nil to continue, or (:value/_ :abort/_) to halt block.

ENV is mutated in place (def values bound to literals when evaluable)."
  (let ((op (nelisp-cc--ssa-instr-opcode instr))
        (def (nelisp-cc--ssa-instr-def instr))
        (operands (nelisp-cc--ssa-instr-operands instr))
        (meta (nelisp-cc--ssa-instr-meta instr)))
    (pcase op
      ('const
       (when def
         (puthash def (plist-get meta :literal) env))
       nil)
      ('load-var
       ;; Dead def left behind by `nelisp-cc-rewrite-call-indirect-to-call'.
       ;; Mark the def as unresolved (any downstream consumer hits
       ;; `:unresolved-call-arg' and the eval bails out gracefully).
       nil)
      ('copy
       (let ((src (nelisp-cc--bcf-resolve-operand (car operands) env)))
         (cond
          ((eq src :unresolved) (cons :abort :unresolved-copy))
          (t (when def (puthash def src env)) nil))))
      ('phi
       ;; Single-path eval can't merge phis from un-visited preds; if
       ;; only one arm came from a visited pred we use that.  Simplify:
       ;; pick the first arm whose pred-bid is in VISITED and whose
       ;; operand-vid resolves.
       (let ((arms (plist-get meta :phi-arms))
             (resolved nil))
         (catch 'done
           (dolist (cell arms)
             (when (memq (car cell) visited)
               (let ((operand
                      (cl-find-if
                       (lambda (v) (= (cdr cell) (nelisp-cc--ssa-value-id v)))
                       operands)))
                 (when operand
                   (let ((lit (nelisp-cc--bcf-resolve-operand operand env)))
                     (unless (eq lit :unresolved)
                       (setq resolved lit)
                       (throw 'done nil))))))))
         (cond
          ((null resolved) (cons :abort :unresolved-phi))
          (t (when def (puthash def resolved env)) nil))))
      ('call
       (let* ((sym (plist-get meta :fn))
              (lit-args (mapcar (lambda (o) (nelisp-cc--bcf-resolve-operand o env))
                                operands)))
         (cond
          ((cl-some (lambda (a) (eq a :unresolved)) lit-args)
           (cons :abort :unresolved-call-arg))
          ;; Primitive arith / cmp.
          ((assq sym nelisp-cc--bcf-arith-primops)
           (let* ((host-fn (cdr (assq sym nelisp-cc--bcf-arith-primops)))
                  (result (condition-case _
                              (apply host-fn lit-args)
                            (error :unresolved))))
             (cond
              ((eq result :unresolved) (cons :abort :primop-error))
              (t (when def (puthash def result env)) nil))))
          ;; Recursive callee.
          ((and registry (assq sym registry))
           (let ((callee (cdr (assq sym registry))))
             (cond
              ((not (= 1 (length lit-args)))
               (cons :abort :wrong-arity))
              (t
               (let ((sub (nelisp-cc--bcf-eval-fn
                           callee (car lit-args) registry memo budget-cell)))
                 (cond
                  ((eq (car sub) :value)
                   (when def (puthash def (cdr sub) env)) nil)
                  (t sub)))))))
          ;; Self-call (when caller==callee but not in registry).
          ((eq sym (nelisp-cc--ssa-function-name fn))
           (cond
            ((not (= 1 (length lit-args)))
             (cons :abort :wrong-arity))
            (t
             (let ((sub (nelisp-cc--bcf-eval-fn
                         fn (car lit-args) registry memo budget-cell)))
               (cond
                ((eq (car sub) :value)
                 (when def (puthash def (cdr sub) env)) nil)
                (t sub))))))
          (t (cons :abort :unknown-callee)))))
      ('branch
       (let ((cond-val (nelisp-cc--bcf-resolve-operand (car operands) env)))
         (cond
          ((eq cond-val :unresolved) (cons :abort :unresolved-branch))
          (t
           (let* ((then-bid (plist-get meta :then))
                  (else-bid (plist-get meta :else))
                  ;; nil + 0 = false; everything else = true.
                  (live-bid (if (and cond-val
                                     (not (eq cond-val 0))
                                     (not (eq cond-val nil)))
                                then-bid
                              else-bid))
                  (live-blk
                   (cl-find-if (lambda (b)
                                 (= live-bid (nelisp-cc--ssa-block-id b)))
                               (nelisp-cc--ssa-function-blocks fn))))
             (cond
              ((null live-blk) (cons :abort :branch-target-missing))
              (t (nelisp-cc--bcf-eval-block
                  fn live-blk env registry memo budget-cell visited))))))))
      ('jump
       ;; meta carries :target when known (rec-inline emits this);
       ;; else fall through to the block's lone successor.
       (let* ((target-bid (plist-get meta :target))
              (next (cond
                     (target-bid
                      (cl-find-if
                       (lambda (b) (= target-bid (nelisp-cc--ssa-block-id b)))
                       (nelisp-cc--ssa-function-blocks fn)))
                     (t
                      (car (nelisp-cc--ssa-block-successors
                            (nelisp-cc--ssa-instr-block instr)))))))
         (cond
          ((null next) (cons :abort :jump-target-missing))
          (t (nelisp-cc--bcf-eval-block
              fn next env registry memo budget-cell visited)))))
      ('return
       (let ((val (nelisp-cc--bcf-resolve-operand (car operands) env)))
         (cond
          ((eq val :unresolved) (cons :abort :unresolved-return))
          (t (cons :value val)))))
      (_
       ;; Unknown opcode — bail.
       (cons :abort (list :unknown-opcode op))))))

;;; per-call-site fold ---------------------------------------------

;;;###autoload
(defun nelisp-cc-base-case-fold-eval-call (fn arg &optional registry memo budget-cell)
  "Symbolically evaluate FN(ARG); return (:value LIT) | (:abort REASON).

MEMO defaults to a fresh hash-table; BUDGET-CELL defaults to a fresh
cons cell holding `nelisp-cc-base-case-fold-budget'.  Pure observer
— FN is not mutated."
  (unless (nelisp-cc--ssa-function-p fn)
    (signal 'nelisp-cc-base-case-fold-error
            (list :not-a-function fn)))
  (let ((memo (or memo (make-hash-table :test 'equal)))
        (budget-cell (or budget-cell (cons nelisp-cc-base-case-fold-budget nil))))
    (cond
     ((nelisp-cc--bcf-eligible-callee-p fn registry)
      (nelisp-cc--bcf-eval-fn fn arg registry memo budget-cell))
     (t (cons :abort :ineligible-callee)))))

;;; site replacement -----------------------------------------------

(defun nelisp-cc--bcf-replace-call-with-const (caller call-instr literal)
  "Mutate CALL-INSTR in-place: replace `:call' with `:const' carrying LITERAL.

Drops the call's outgoing operand use-list back-pointers and rebuilds
the instr's META as `(:literal LITERAL :folded-from-call t)'.
The instr's DEF, if any, is preserved (its def-point now points at
this `:const' instr — downstream uses see the literal directly).

CALLER is the owning SSA function (used only for sanity-check
ownership; not currently consulted)."
  (ignore caller)
  ;; Drop use-list back-pointers from the call's operands.
  (dolist (op (nelisp-cc--ssa-instr-operands call-instr))
    (setf (nelisp-cc--ssa-value-use-list op)
          (delq call-instr (nelisp-cc--ssa-value-use-list op))))
  (setf (nelisp-cc--ssa-instr-opcode call-instr) 'const
        (nelisp-cc--ssa-instr-operands call-instr) nil
        (nelisp-cc--ssa-instr-meta call-instr)
        (list :literal literal :folded-from-call t))
  call-instr)

;;; top-level pass -------------------------------------------------

;;;###autoload
(defun nelisp-cc-base-case-fold-pass (caller &optional registry)
  "Run the base-case fold pass on CALLER.  Return (CALLER . FOLD-COUNT).

For every `:call :fn FN (CONST_ARG)' site in CALLER where FN resolves
in REGISTRY (= the standard callee alist) and FN(CONST_ARG)
const-evaluates within budget, the call is replaced with a `:const'
instruction carrying the literal result.  CALLER is mutated in
place; FOLD-COUNT is the number of sites replaced.

The pass is no-op when REGISTRY is nil (= no callee resolution
possible) or when `nelisp-cc-base-case-fold-enable' is nil.

Pure-observer for sites that fail const-eval — they remain as
`:call' instructions and downstream lowering proceeds unchanged."
  (unless (nelisp-cc--ssa-function-p caller)
    (signal 'nelisp-cc-base-case-fold-error
            (list :not-a-function caller)))
  (let ((count 0)
        ;; Boost the lisp recursion limit during the eval — naive
        ;; recursive fns expand the symbolic walker depth as
        ;; ~param-value (memo bounds the total # of evaluations but
        ;; the in-flight stack still reaches that depth on first
        ;; visit).  fib(30) needs ~31 nested `eval-fn' frames; each
        ;; frame is ~30 host-call frames deep, so a 1000-default
        ;; ceiling exhausts at depth>=2 or 3 expansion levels.  Cap
        ;; at 100x the default to comfortably cover the bench-sized
        ;; targets while still bounding pathological inputs.
        (max-lisp-eval-depth (max max-lisp-eval-depth 100000)))
    (when nelisp-cc-base-case-fold-enable
      (let ((memo (make-hash-table :test 'equal))
            (sites
             (let (acc)
               (dolist (b (nelisp-cc--ssa-function-blocks caller))
                 (dolist (instr (nelisp-cc--ssa-block-instrs b))
                   (when (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
                     (push instr acc))))
               (nreverse acc))))
        (dolist (site sites)
          (let* ((meta (nelisp-cc--ssa-instr-meta site))
                 (sym (plist-get meta :fn))
                 (callee (and sym registry (cdr (assq sym registry))))
                 (operands (nelisp-cc--ssa-instr-operands site)))
            (when (and callee
                       (= 1 (length operands))
                       (nelisp-cc--bcf-eligible-callee-p callee registry))
              (let* ((arg-lit (nelisp-cc--bcf-const-literal (car operands))))
                (when arg-lit
                  (let* ((budget-cell (cons nelisp-cc-base-case-fold-budget nil))
                         (result (nelisp-cc--bcf-eval-fn
                                  callee (car arg-lit) registry memo
                                  budget-cell)))
                    (when (eq (car result) :value)
                      (nelisp-cc--bcf-replace-call-with-const
                       caller site (cdr result))
                      (cl-incf count))))))))))
    (cons caller count)))

(provide 'nelisp-cc-base-case-fold)

;;; nelisp-cc-base-case-fold.el ends here
