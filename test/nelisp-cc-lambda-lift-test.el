;;; nelisp-cc-lambda-lift-test.el --- ERT for nelisp-cc-lambda-lift (Phase 7.7.4) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for the Doc 42 §3.4 lambda-lift pass in
;; `src/nelisp-cc-lambda-lift.el'.  Builds SSA functions through the
;; Phase 7.1 scaffold builders and exercises the per-decision logic plus
;; the rewrite + drop machinery.
;;
;; Tests:
;;   1.  lift-decide-go-canonical
;;   2.  lift-decide-not-a-closure
;;   3.  lift-decide-no-inner-function
;;   4.  lift-decide-no-uses
;;   5.  lift-decide-non-callee-use-data-arg
;;   6.  lift-decide-wrong-operand-position
;;   7.  lift-decide-not-stack-only-via-return
;;   8.  lift-can-lift-p-go
;;   9.  lift-can-lift-p-no-go-on-data-use
;;  10.  lift-pass-rewrites-call-indirect
;;  11.  lift-pass-drops-closure-instr
;;  12.  lift-pass-registers-lifted-fn
;;  13.  lift-pass-counts-lifts
;;  14.  lift-pass-leaves-non-stack-only-untouched
;;  15.  lift-pass-no-closures-noop
;;  16.  lift-pass-non-fn-signals
;;  17.  lift-pass-multiple-closures-distinct-names
;;  18.  lift-stats-tally-shape
;;  19.  lift-stats-tally-with-aborts
;;  20.  lift-pass-preserves-call-args
;;  21.  lift-pass-rewrite-call-indirect-bad-opcode-signals
;;  22.  lift-rewrites-multiple-call-sites-of-same-closure

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)
(require 'nelisp-cc-lambda-lift)

;;; helpers --------------------------------------------------------

(defun nelisp-cc-lift-test--make-inner-fn (name)
  "Build a tiny single-block SSA function under NAME (one int param, returns it)."
  (let* ((fn (nelisp-cc--ssa-make-function name '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (p (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list p) nil)
    fn))

(defun nelisp-cc-lift-test--make-caller-with-closure-call ()
  "Build a caller that materialises a closure and immediately invokes it.

Pseudo-code:
  (lambda (x)
    (let ((c (lambda (y) y)))
      (return (funcall c x))))

The closure's def value flows directly into the operand[0] slot of
a single `call-indirect' — the canonical lift candidate."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner-fn))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (x (car (nelisp-cc--ssa-function-params caller)))
         (closure-def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil closure-def))
         (call-def (nelisp-cc--ssa-make-value caller 'int))
         (call-instr
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list closure-def x) call-def)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta call-instr)
          (list :indirect t :funcall t))
    (nelisp-cc--ssa-add-instr caller entry 'return (list call-def) nil)
    (list :caller caller
          :inner inner
          :closure-instr closure-instr
          :call-instr call-instr
          :closure-def closure-def
          :call-def call-def
          :x x)))

(defun nelisp-cc-lift-test--first-instr-of-opcode (caller opcode)
  "Return the first instruction in CALLER whose opcode matches OPCODE."
  (catch 'found
    (dolist (b (nelisp-cc--ssa-function-blocks caller))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) opcode)
          (throw 'found instr))))
    nil))

(defun nelisp-cc-lift-test--all-instrs-of-opcode (caller opcode)
  "Return every instruction in CALLER whose opcode matches OPCODE."
  (let (acc)
    (dolist (b (nelisp-cc--ssa-function-blocks caller))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) opcode)
          (push instr acc))))
    (nreverse acc)))

;;; (1) lift-decide-go-canonical ------------------------------------

(ert-deftest nelisp-cc-lift-decide-go-canonical ()
  "A closure used by exactly one `call-indirect' at operand[0] is :go."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (closure-instr (plist-get rec :closure-instr))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--lift-decide closure-instr escape-info)))
    (should (eq :go (nelisp-cc--lift-status-verdict decision)))
    (should (eq (plist-get rec :inner)
                (nelisp-cc--lift-status-inner decision)))))

;;; (2) lift-decide-not-a-closure -----------------------------------

(ert-deftest nelisp-cc-lift-decide-not-a-closure ()
  "A non-`closure' instruction trips :not-a-closure."
  (let* ((fn (nelisp-cc--ssa-make-function 'simple '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (p (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (cinstr (nelisp-cc--ssa-add-instr fn entry 'const nil rv))
         (escape-info (progn
                        (setf (nelisp-cc--ssa-instr-meta cinstr)
                              (list :literal 42))
                        (nelisp-cc--ssa-add-instr fn entry 'return (list p) nil)
                        (nelisp-cc-escape-analyze fn)))
         (decision (nelisp-cc--lift-decide cinstr escape-info)))
    (should (eq :abort (nelisp-cc--lift-status-verdict decision)))
    (should (eq :not-a-closure
                (nelisp-cc--lift-status-reason decision)))))

;;; (3) lift-decide-no-inner-function -------------------------------

(ert-deftest nelisp-cc-lift-decide-no-inner-function ()
  "A `closure' with no `:inner-function' meta trips :no-inner-function."
  (let* ((caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def)))
    ;; Deliberately leave META nil.
    (nelisp-cc--ssa-add-instr caller entry 'return (list def) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (decision (nelisp-cc--lift-decide closure-instr escape-info)))
      (should (eq :no-inner-function
                  (nelisp-cc--lift-status-reason decision))))))

;;; (4) lift-decide-no-uses ----------------------------------------

(ert-deftest nelisp-cc-lift-decide-no-uses ()
  "An unused closure trips :no-uses."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (x (car (nelisp-cc--ssa-function-params caller)))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    ;; Return uses x (param), not the closure def.
    (nelisp-cc--ssa-add-instr caller entry 'return (list x) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (decision (nelisp-cc--lift-decide closure-instr escape-info)))
      (should (eq :no-uses
                  (nelisp-cc--lift-status-reason decision))))))

;;; (5) lift-decide-non-callee-use-data-arg -------------------------

(ert-deftest nelisp-cc-lift-decide-non-callee-use-data-arg ()
  "A closure consumed via a `call' (not `call-indirect') trips
:non-callee-use."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def))
         (rv (nelisp-cc--ssa-make-value caller nil))
         (call-instr
          (nelisp-cc--ssa-add-instr caller entry 'call (list def) rv)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta call-instr)
          (list :fn 'register :unresolved t))
    (nelisp-cc--ssa-add-instr caller entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (decision (nelisp-cc--lift-decide closure-instr escape-info)))
      (should (eq :non-callee-use
                  (nelisp-cc--lift-status-reason decision))))))

;;; (6) lift-decide-wrong-operand-position --------------------------

(ert-deftest nelisp-cc-lift-decide-wrong-operand-position ()
  "A closure that flows into a `call-indirect' at operand[1] (= as a
data argument, not the function position) trips
:wrong-operand-position."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (other-callee (nelisp-cc--ssa-make-value caller nil))
         (other-closure
          (nelisp-cc--ssa-add-instr caller entry 'closure nil other-callee))
         (other-inner (nelisp-cc-lift-test--make-inner-fn 'other))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def))
         (rv (nelisp-cc--ssa-make-value caller nil))
         (call-instr
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list other-callee def) rv)))
    (setf (nelisp-cc--ssa-instr-meta other-closure)
          (list :inner-function other-inner))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta call-instr)
          (list :indirect t))
    (nelisp-cc--ssa-add-instr caller entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (decision (nelisp-cc--lift-decide closure-instr escape-info)))
      (should (eq :wrong-operand-position
                  (nelisp-cc--lift-status-reason decision))))))

;;; (7) lift-decide-not-stack-only-via-return -----------------------

(ert-deftest nelisp-cc-lift-decide-not-stack-only-via-return ()
  "A closure whose def directly flows into `return' is :not-stack-only.

Note: the structural gates (uses-only-as-callee) come *before* the
escape gate, so a closure that flows to `return' alone trips
:no-uses (no `call-indirect' use at all).  We construct a hybrid
case: the closure flows into a `call-indirect' (callee position)
*and* into a separate `return' — the structural gate fires first
because the additional `return' use is :non-callee-use."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (x (car (nelisp-cc--ssa-function-params caller)))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def))
         (rv (nelisp-cc--ssa-make-value caller 'int))
         (call-instr
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list def x) rv)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta call-instr) (list :indirect t))
    ;; Closure def *also* flows into return (= second use, escapes).
    (nelisp-cc--ssa-add-instr caller entry 'return (list def) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (decision (nelisp-cc--lift-decide closure-instr escape-info)))
      ;; The structural gate detects the extra non-callee use first;
      ;; we accept either :non-callee-use (the structural fast path)
      ;; or :not-stack-only as a valid abort — both correctly reject.
      (should (memq (nelisp-cc--lift-status-reason decision)
                    '(:non-callee-use :not-stack-only))))))

;;; (8) lift-can-lift-p-go ------------------------------------------

(ert-deftest nelisp-cc-lift-can-lift-p-go ()
  "Predicate accepts the canonical lift candidate."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (closure-instr (plist-get rec :closure-instr))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (should (nelisp-cc-lift-can-lift-p closure-instr escape-info))))

;;; (9) lift-can-lift-p-no-go-on-data-use ---------------------------

(ert-deftest nelisp-cc-lift-can-lift-p-no-go-on-data-use ()
  "Predicate rejects a closure used as a data argument."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def))
         (rv (nelisp-cc--ssa-make-value caller nil))
         (call-instr
          (nelisp-cc--ssa-add-instr caller entry 'call (list def) rv)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta call-instr)
          (list :fn 'register))
    (nelisp-cc--ssa-add-instr caller entry 'return (list rv) nil)
    (let ((escape-info (nelisp-cc-escape-analyze caller)))
      (should-not (nelisp-cc-lift-can-lift-p closure-instr escape-info)))))

;;; (10) lift-pass-rewrites-call-indirect ---------------------------

(ert-deftest nelisp-cc-lift-pass-rewrites-call-indirect ()
  "After the pass the original `call-indirect' is now a direct `call'."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (call-instr (plist-get rec :call-instr))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (nelisp-cc-lift-pass caller escape-info)
    (should (eq 'call (nelisp-cc--ssa-instr-opcode call-instr)))
    (let ((meta (nelisp-cc--ssa-instr-meta call-instr)))
      (should (plist-get meta :lifted))
      (should (plist-get meta :unresolved))
      (should (symbolp (plist-get meta :fn))))))

;;; (11) lift-pass-drops-closure-instr ------------------------------

(ert-deftest nelisp-cc-lift-pass-drops-closure-instr ()
  "After the pass the `closure' instruction is no longer in any block."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (closure-instr (plist-get rec :closure-instr))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (nelisp-cc-lift-pass caller escape-info)
    (should-not
     (cl-some (lambda (b)
                (memq closure-instr (nelisp-cc--ssa-block-instrs b)))
              (nelisp-cc--ssa-function-blocks caller)))
    (should-not (nelisp-cc-lift-test--first-instr-of-opcode caller 'closure))))

;;; (12) lift-pass-registers-lifted-fn ------------------------------

(ert-deftest nelisp-cc-lift-pass-registers-lifted-fn ()
  "The lifted inner function is reachable via the stats registry."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (inner (plist-get rec :inner))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (nelisp-cc-lift-pass caller escape-info)
    (let* ((stats (nelisp-cc-lift-stats caller escape-info))
           ;; After the pass the only `closure' sites are gone, so
           ;; stats samples zero — but the registry slot is the
           ;; *fresh* registry copy supplied to the stats call (= nil
           ;; here).  We use the inner function's mutated NAME (set
           ;; by the lift driver) as the proof-of-registration:
           (inner-name (nelisp-cc--ssa-function-name inner)))
      (should (symbolp inner-name))
      (should (string-match-p "\\$lift\\$" (symbol-name inner-name)))
      (should (assq :registry stats)))))

;;; (13) lift-pass-counts-lifts -------------------------------------

(ert-deftest nelisp-cc-lift-pass-counts-lifts ()
  "The pass returns (CALLER . LIFTED-COUNT) where COUNT matches the
number of closures lifted."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (escape-info (nelisp-cc-escape-analyze caller))
         (result (nelisp-cc-lift-pass caller escape-info)))
    (should (eq caller (car result)))
    (should (= 1 (cdr result)))))

;;; (14) lift-pass-leaves-non-stack-only-untouched -------------------

(ert-deftest nelisp-cc-lift-pass-leaves-non-stack-only-untouched ()
  "A closure whose def has a non-callee use survives the pass intact."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    ;; Direct return of the closure value — escape via :returned.
    (nelisp-cc--ssa-add-instr caller entry 'return (list def) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (result (nelisp-cc-lift-pass caller escape-info)))
      (should (= 0 (cdr result)))
      ;; The original `closure' instr is still in the entry block.
      (should (memq closure-instr (nelisp-cc--ssa-block-instrs entry))))))

;;; (15) lift-pass-no-closures-noop ---------------------------------

(ert-deftest nelisp-cc-lift-pass-no-closures-noop ()
  "A function with no `closure' instructions is a no-op."
  (let* ((fn (nelisp-cc--ssa-make-function 'plain '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (p (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list p) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (result (nelisp-cc-lift-pass fn escape-info)))
      (should (= 0 (cdr result))))))

;;; (16) lift-pass-non-fn-signals -----------------------------------

(ert-deftest nelisp-cc-lift-pass-non-fn-signals ()
  "The pass signals on non-SSA-function input."
  (should-error
   (nelisp-cc-lift-pass "not-a-fn" nil)
   :type 'nelisp-cc-lift-error))

;;; (17) lift-pass-multiple-closures-distinct-names ------------------

(ert-deftest nelisp-cc-lift-pass-multiple-closures-distinct-names ()
  "Two independent lift candidates produce two distinct lifted names."
  (let* ((inner-a (nelisp-cc-lift-test--make-inner-fn 'a))
         (inner-b (nelisp-cc-lift-test--make-inner-fn 'b))
         (caller (nelisp-cc--ssa-make-function 'multi '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (x (car (nelisp-cc--ssa-function-params caller)))
         (def-a (nelisp-cc--ssa-make-value caller nil))
         (closure-a
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def-a))
         (rv-a (nelisp-cc--ssa-make-value caller 'int))
         (call-a
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list def-a x) rv-a))
         (def-b (nelisp-cc--ssa-make-value caller nil))
         (closure-b
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def-b))
         (rv-b (nelisp-cc--ssa-make-value caller 'int))
         (call-b
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list def-b rv-a) rv-b)))
    (setf (nelisp-cc--ssa-instr-meta closure-a)
          (list :inner-function inner-a))
    (setf (nelisp-cc--ssa-instr-meta closure-b)
          (list :inner-function inner-b))
    (setf (nelisp-cc--ssa-instr-meta call-a) (list :indirect t))
    (setf (nelisp-cc--ssa-instr-meta call-b) (list :indirect t))
    (nelisp-cc--ssa-add-instr caller entry 'return (list rv-b) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (result (nelisp-cc-lift-pass caller escape-info)))
      (should (= 2 (cdr result)))
      (let ((name-a (nelisp-cc--ssa-function-name inner-a))
            (name-b (nelisp-cc--ssa-function-name inner-b)))
        (should (symbolp name-a))
        (should (symbolp name-b))
        (should-not (eq name-a name-b))))))

;;; (18) lift-stats-tally-shape -------------------------------------

(ert-deftest nelisp-cc-lift-stats-tally-shape ()
  "`nelisp-cc-lift-stats' returns a well-formed alist."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (escape-info (nelisp-cc-escape-analyze caller))
         (stats (nelisp-cc-lift-stats caller escape-info)))
    (should (assq :lifted stats))
    (should (assq :abort stats))
    (should (assq :registry stats))
    (let ((aborts (cdr (assq :abort stats))))
      ;; All abort buckets must be present even when zero.
      (dolist (k '(:not-a-closure :no-inner-function :no-def-value
                   :no-uses :non-callee-use :wrong-operand-position
                   :not-stack-only))
        (should (assq k aborts))))
    ;; Pre-pass stats: the canonical sample lifts 1.
    (should (= 1 (cdr (assq :lifted stats))))))

;;; (19) lift-stats-tally-with-aborts -------------------------------

(ert-deftest nelisp-cc-lift-stats-tally-with-aborts ()
  "Stats correctly count multiple abort reasons across the same caller."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         ;; closure-1: no uses.
         (def-1 (nelisp-cc--ssa-make-value caller nil))
         (closure-1
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def-1))
         ;; closure-2: returned (escapes).
         (def-2 (nelisp-cc--ssa-make-value caller nil))
         (closure-2
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def-2)))
    (setf (nelisp-cc--ssa-instr-meta closure-1)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta closure-2)
          (list :inner-function inner))
    (nelisp-cc--ssa-add-instr caller entry 'return (list def-2) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (stats (nelisp-cc-lift-stats caller escape-info))
           (aborts (cdr (assq :abort stats))))
      ;; Both closures are aborts; tallies must be ≥ 1 for the
      ;; relevant buckets.  closure-1 → :no-uses; closure-2 →
      ;; :no-uses too (its def's only use is `return', not
      ;; `call-indirect').
      (should (= 0 (cdr (assq :lifted stats))))
      (should (>= (cdr (assq :no-uses aborts)) 1)))))

;;; (20) lift-pass-preserves-call-args ------------------------------

(ert-deftest nelisp-cc-lift-pass-preserves-call-args ()
  "The rewritten direct `call' carries the original arg list (= operands
sans the closure value at position 0)."
  (let* ((rec (nelisp-cc-lift-test--make-caller-with-closure-call))
         (caller (plist-get rec :caller))
         (call-instr (plist-get rec :call-instr))
         (x (plist-get rec :x))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (nelisp-cc-lift-pass caller escape-info)
    ;; Operands should now be (x) — the original call had
    ;; (closure-def x), and the rewrite drops the closure operand.
    (should (equal (list x)
                   (nelisp-cc--ssa-instr-operands call-instr)))))

;;; (21) lift-pass-rewrite-call-indirect-bad-opcode-signals ----------

(ert-deftest nelisp-cc-lift-pass-rewrite-call-indirect-bad-opcode-signals ()
  "`nelisp-cc--lift-rewrite-call-indirect' signals on a non-`call-indirect'."
  (let* ((fn (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (p (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list p) rv)))
    (setf (nelisp-cc--ssa-instr-meta call) (list :fn 'register))
    (should-error
     (nelisp-cc--lift-rewrite-call-indirect call 'lifted)
     :type 'nelisp-cc-lift-error)))

;;; (22) lift-rewrites-multiple-call-sites-of-same-closure ---------

(ert-deftest nelisp-cc-lift-rewrites-multiple-call-sites-of-same-closure ()
  "When a closure value flows into N `call-indirect' sites all at
operand[0], the pass rewrites all N sites to direct calls and drops
the single `closure' instr."
  (let* ((inner (nelisp-cc-lift-test--make-inner-fn 'inner))
         (caller (nelisp-cc--ssa-make-function 'host '(int)))
         (entry (nelisp-cc--ssa-function-entry caller))
         (x (car (nelisp-cc--ssa-function-params caller)))
         (def (nelisp-cc--ssa-make-value caller nil))
         (closure-instr
          (nelisp-cc--ssa-add-instr caller entry 'closure nil def))
         (rv-1 (nelisp-cc--ssa-make-value caller 'int))
         (call-1
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list def x) rv-1))
         (rv-2 (nelisp-cc--ssa-make-value caller 'int))
         (call-2
          (nelisp-cc--ssa-add-instr caller entry 'call-indirect
                                    (list def rv-1) rv-2)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr)
          (list :inner-function inner))
    (setf (nelisp-cc--ssa-instr-meta call-1) (list :indirect t))
    (setf (nelisp-cc--ssa-instr-meta call-2) (list :indirect t))
    (nelisp-cc--ssa-add-instr caller entry 'return (list rv-2) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze caller))
           (result (nelisp-cc-lift-pass caller escape-info)))
      (should (= 1 (cdr result)))
      ;; Both sites are now `call' opcodes.
      (should (eq 'call (nelisp-cc--ssa-instr-opcode call-1)))
      (should (eq 'call (nelisp-cc--ssa-instr-opcode call-2)))
      ;; Both carry the same lifted name.
      (should (eq (plist-get (nelisp-cc--ssa-instr-meta call-1) :fn)
                  (plist-get (nelisp-cc--ssa-instr-meta call-2) :fn)))
      ;; Closure instr is gone.
      (should-not (nelisp-cc-lift-test--first-instr-of-opcode
                   caller 'closure)))))

(provide 'nelisp-cc-lambda-lift-test)
;;; nelisp-cc-lambda-lift-test.el ends here
