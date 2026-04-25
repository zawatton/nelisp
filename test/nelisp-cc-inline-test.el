;;; nelisp-cc-inline-test.el --- ERT for nelisp-cc-inline (Phase 7.7.2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for the Doc 42 §3.2 simple-inline pass in
;; `src/nelisp-cc-inline.el'.  Builds SSA functions directly through
;; the Phase 7.1 scaffold builders so the tests stay focused on the
;; inliner's decision logic and substitution semantics.
;;
;; Tests:
;;   1.  inline-leaf-no-side-effects     — single-block leaf gets inlined
;;   2.  inline-respects-cost-threshold  — body > threshold rejected
;;   3.  inline-skip-recursive           — self-recursive callee rejected
;;   4.  inline-skip-multi-block         — multi-block callee rejected
;;   5.  inline-skip-not-leaf            — callee with `call' rejected
;;   6.  inline-skip-escaped-args        — escape :heap-required arg rejected
;;   7.  inline-skip-unresolved          — registry miss rejected
;;   8.  inline-skip-no-callee-name      — `call' meta lacks :fn rejected
;;   9.  inline-skip-no-return           — leaf without return rejected
;;  10.  inline-multiple-call-sites      — N callsites all inlined
;;  11.  inline-cost-counts-instructions — cost helper sums all blocks
;;  12.  inline-can-inline-p-go          — predicate accepts canonical case
;;  13.  inline-stats-tally              — stats reflects go + per-reason
;;  14.  inline-preserves-ssa-verifier   — verifier passes after splice
;;  15.  inline-rebinds-call-def         — original call def points at copy
;;  16.  inline-arity-mismatch-signals   — wrong arg count signals
;;  17.  inline-call-without-def-skipped — no-def call has no `copy' added

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)
(require 'nelisp-cc-inline)

;;; helpers --------------------------------------------------------

(defun nelisp-cc-inline-test--make-leaf-add-fn ()
  "Build callee `(lambda (a b) (return (add a b)))'.

Returns the SSA function — single block, two params, one `add', one
`return'.  Eligible for the simple inliner."
  (let* ((fn (nelisp-cc--ssa-make-function 'leaf-add '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (a (car (nelisp-cc--ssa-function-params fn)))
         (b (cadr (nelisp-cc--ssa-function-params fn)))
         (sum (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'add (list a b) sum)
    (nelisp-cc--ssa-add-instr fn entry 'return (list sum) nil)
    fn))

(defun nelisp-cc-inline-test--make-caller-with-call (callee-name &optional discard-result)
  "Build caller that calls CALLEE-NAME with two arguments and uses the result.

The caller signature is `(lambda (x y) (LEAF-ADD x y))'.  When
DISCARD-RESULT is non-nil the call result is unused (no-def call).
Returns (FN CALL-INSTR RV-OR-NIL)."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (y (cadr (nelisp-cc--ssa-function-params fn)))
         (rv (and (not discard-result) (nelisp-cc--ssa-make-value fn 'int)))
         (instr (nelisp-cc--ssa-add-instr fn entry 'call (list x y) rv)))
    (setf (nelisp-cc--ssa-instr-meta instr)
          (list :fn callee-name :unresolved t))
    ;; Add a `return' so the verifier sees a well-formed function.
    (when rv
      (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil))
    (list fn instr rv)))

(defun nelisp-cc-inline-test--registry (callee)
  "Return a callee-registry with CALLEE under its NAME."
  (list (cons (nelisp-cc--ssa-function-name callee) callee)))

;;; (1) inline-leaf-no-side-effects --------------------------------

(ert-deftest nelisp-cc-inline-leaf-no-side-effects ()
  "A canonical 1-callsite leaf-add is inlined — call drops, body splices."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'leaf-add))
         (caller (car caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (result (nelisp-cc-inline-pass caller escape-info registry)))
    (should (eq caller (car result)))
    (should (= 1 (cdr result)))
    ;; The original `call' instruction is gone.
    (let ((opcodes (mapcar #'nelisp-cc--ssa-instr-opcode
                           (nelisp-cc--ssa-block-instrs
                            (nelisp-cc--ssa-function-entry caller)))))
      (should-not (memq 'call opcodes))
      ;; The inlined `add' is now in the caller body.
      (should (memq 'add opcodes))
      ;; A `copy' replaced the call's result def.
      (should (memq 'copy opcodes)))))

;;; (2) inline-respects-cost-threshold -----------------------------

(ert-deftest nelisp-cc-inline-respects-cost-threshold ()
  "A callee body larger than the threshold is rejected with :size-cap-exceeded."
  (let* ((nelisp-cc-inline-size-threshold 1) ; force the abort path
         (callee (nelisp-cc-inline-test--make-leaf-add-fn)) ; cost = 2
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'leaf-add))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--inline-decide
                    call-instr escape-info registry caller))
         (result (nelisp-cc-inline-pass caller escape-info registry)))
    (should (eq :abort (nelisp-cc--inline-status-verdict decision)))
    (should (eq :size-cap-exceeded (nelisp-cc--inline-status-reason decision)))
    (should (= 0 (cdr result)))))

;;; (3) inline-skip-recursive --------------------------------------

(ert-deftest nelisp-cc-inline-skip-recursive ()
  "A callee that calls itself is rejected with :recursive."
  (let* ((callee (let* ((fn (nelisp-cc--ssa-make-function 'rec '(int)))
                        (entry (nelisp-cc--ssa-function-entry fn))
                        (x (car (nelisp-cc--ssa-function-params fn)))
                        (rv (nelisp-cc--ssa-make-value fn 'int))
                        (call (nelisp-cc--ssa-add-instr
                               fn entry 'call (list x) rv)))
                   (setf (nelisp-cc--ssa-instr-meta call)
                         (list :fn 'rec :unresolved t))
                   (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
                   fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'rec))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--inline-decide
                    call-instr escape-info registry caller))
         (result (nelisp-cc-inline-pass caller escape-info registry)))
    (should (eq :recursive (nelisp-cc--inline-status-reason decision)))
    (should (= 0 (cdr result)))))

;;; (4) inline-skip-multi-block ------------------------------------

(ert-deftest nelisp-cc-inline-skip-multi-block ()
  "A multi-block callee is rejected with :multi-block-callee."
  (let* ((callee (let* ((fn (nelisp-cc--ssa-make-function 'two-blk '(int)))
                        (entry (nelisp-cc--ssa-function-entry fn))
                        (next  (nelisp-cc--ssa-make-block fn "next"))
                        (x (car (nelisp-cc--ssa-function-params fn))))
                   (nelisp-cc--ssa-link-blocks entry next)
                   (nelisp-cc--ssa-add-instr fn entry 'jump nil nil)
                   (nelisp-cc--ssa-add-instr fn next 'return (list x) nil)
                   fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'two-blk))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--inline-decide
                    call-instr escape-info registry caller)))
    ;; arity mismatch — caller passes 2 args, callee takes 1; that's
    ;; OK because the abort fires *before* we'd splice.  We exercise
    ;; the predicate path only.
    (should (eq :multi-block-callee
                (nelisp-cc--inline-status-reason decision)))))

;;; (5) inline-skip-not-leaf ---------------------------------------

(ert-deftest nelisp-cc-inline-skip-not-leaf ()
  "A callee that contains its own `call' (non-self) is rejected with :not-leaf."
  (let* ((callee (let* ((fn (nelisp-cc--ssa-make-function 'mid '(int)))
                        (entry (nelisp-cc--ssa-function-entry fn))
                        (x (car (nelisp-cc--ssa-function-params fn)))
                        (tmp (nelisp-cc--ssa-make-value fn 'int))
                        (c (nelisp-cc--ssa-add-instr
                            fn entry 'call (list x) tmp)))
                   (setf (nelisp-cc--ssa-instr-meta c)
                         (list :fn 'other :unresolved t))
                   (nelisp-cc--ssa-add-instr fn entry 'return (list tmp) nil)
                   fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'mid))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--inline-decide
                    call-instr escape-info registry caller)))
    (should (eq :not-leaf (nelisp-cc--inline-status-reason decision)))))

;;; (6) inline-skip-escaped-args -----------------------------------

(ert-deftest nelisp-cc-inline-skip-escaped-args ()
  "When an arg is :heap-required per escape-info the inline is aborted.

We construct a caller where the escape pass marks the call's operand
as :heap-required (here, by feeding it through a `closure' opcode
before the `call').  The simple inliner then rejects the call with
:escaped-arg."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (fn (nelisp-cc--ssa-make-function 'caller-esc '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (y (cadr (nelisp-cc--ssa-function-params fn)))
         ;; First, capture x in a closure so escape analysis marks it
         ;; :heap-required.
         (clo (nelisp-cc--ssa-make-value fn 'closure))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (progn
                 (nelisp-cc--ssa-add-instr fn entry 'closure (list x) clo)
                 (let ((c (nelisp-cc--ssa-add-instr
                           fn entry 'call (list x y) rv)))
                   (setf (nelisp-cc--ssa-instr-meta c)
                         (list :fn 'leaf-add :unresolved t))
                   c))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((registry (nelisp-cc-inline-test--registry callee))
           (escape-info (nelisp-cc-escape-analyze fn))
           (decision (nelisp-cc--inline-decide
                      call escape-info registry fn))
           (result (nelisp-cc-inline-pass fn escape-info registry)))
      (should (eq :escaped-arg (nelisp-cc--inline-status-reason decision)))
      (should (= 0 (cdr result))))))

;;; (7) inline-skip-unresolved -------------------------------------

(ert-deftest nelisp-cc-inline-skip-unresolved ()
  "When the callee name is not in the registry the call is left untouched."
  (let* ((caller-spec (nelisp-cc-inline-test--make-caller-with-call 'missing))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--inline-decide
                    call-instr escape-info nil caller))
         (result (nelisp-cc-inline-pass caller escape-info nil)))
    (should (eq :unresolved-callee (nelisp-cc--inline-status-reason decision)))
    (should (= 0 (cdr result)))))

;;; (8) inline-skip-no-callee-name ---------------------------------

(ert-deftest nelisp-cc-inline-skip-no-callee-name ()
  "A `call' instruction without :fn meta is rejected with :no-callee-name."
  (let* ((fn (nelisp-cc--ssa-make-function 'nameless '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (decision (nelisp-cc--inline-decide call escape-info nil fn)))
      (should (eq :no-callee-name
                  (nelisp-cc--inline-status-reason decision))))))

;;; (9) inline-skip-no-return --------------------------------------

(ert-deftest nelisp-cc-inline-skip-no-return ()
  "A leaf callee without a `return' instruction is rejected with :no-return.

This guards against the corner case where a leaf body does work but
never names its result (e.g. the post-codegen stub before the return
rewriter has run).  The inliner has nothing to bind to the caller's
def site so it must abort."
  (let* ((callee (let* ((fn (nelisp-cc--ssa-make-function 'no-ret '(int)))
                        (entry (nelisp-cc--ssa-function-entry fn))
                        (x (car (nelisp-cc--ssa-function-params fn)))
                        (def (nelisp-cc--ssa-make-value fn 'int)))
                   (nelisp-cc--ssa-add-instr fn entry 'add (list x x) def)
                   fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'no-ret))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (decision (nelisp-cc--inline-decide
                    call-instr escape-info registry caller)))
    (should (eq :no-return (nelisp-cc--inline-status-reason decision)))))

;;; (10) inline-multiple-call-sites --------------------------------

(ert-deftest nelisp-cc-inline-multiple-call-sites ()
  "When a caller has 3 callsites all of the same eligible callee, all 3 inline."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (fn (nelisp-cc--ssa-make-function 'caller-many '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (y (cadr (nelisp-cc--ssa-function-params fn))))
    (dotimes (_ 3)
      (let* ((rv (nelisp-cc--ssa-make-value fn 'int))
             (c (nelisp-cc--ssa-add-instr fn entry 'call (list x y) rv)))
        (setf (nelisp-cc--ssa-instr-meta c)
              (list :fn 'leaf-add :unresolved t))))
    (let* ((registry (nelisp-cc-inline-test--registry callee))
           (escape-info (nelisp-cc-escape-analyze fn))
           (result (nelisp-cc-inline-pass fn escape-info registry))
           (opcodes (mapcar #'nelisp-cc--ssa-instr-opcode
                            (nelisp-cc--ssa-block-instrs entry))))
      (should (= 3 (cdr result)))
      (should-not (memq 'call opcodes))
      ;; 3 inlined `add' instructions + 3 `copy' rebinds.
      (should (= 3 (cl-count 'add opcodes)))
      (should (= 3 (cl-count 'copy opcodes))))))

;;; (11) inline-cost-counts-instructions ---------------------------

(ert-deftest nelisp-cc-inline-cost-counts-instructions ()
  "`nelisp-cc-inline-cost' sums every instruction in every block."
  (let* ((fn (nelisp-cc--ssa-make-function 'multi '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (next  (nelisp-cc--ssa-make-block fn "next"))
         (x (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-link-blocks entry next)
    (nelisp-cc--ssa-add-instr fn entry 'jump nil nil)
    (nelisp-cc--ssa-add-instr fn next 'add (list x x) (nelisp-cc--ssa-make-value fn 'int))
    (nelisp-cc--ssa-add-instr fn next 'return (list x) nil)
    (should (= 3 (nelisp-cc-inline-cost fn)))))

;;; (12) inline-can-inline-p-go ------------------------------------

(ert-deftest nelisp-cc-inline-can-inline-p-go ()
  "The predicate accepts a canonical leaf inline and rejects the rest."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'leaf-add))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (should (nelisp-cc-inline-can-inline-p
             call-instr escape-info registry caller))
    (should-not (nelisp-cc-inline-can-inline-p
                 call-instr escape-info nil caller))))

;;; (13) inline-stats-tally ----------------------------------------

(ert-deftest nelisp-cc-inline-stats-tally ()
  "`nelisp-cc-inline-stats' produces a per-reason histogram."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (fn (nelisp-cc--ssa-make-function 'mixed '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (y (cadr (nelisp-cc--ssa-function-params fn))))
    ;; Two call sites: one resolved, one unresolved.
    (let* ((rv1 (nelisp-cc--ssa-make-value fn 'int))
           (c1 (nelisp-cc--ssa-add-instr fn entry 'call (list x y) rv1)))
      (setf (nelisp-cc--ssa-instr-meta c1)
            (list :fn 'leaf-add :unresolved t)))
    (let* ((rv2 (nelisp-cc--ssa-make-value fn 'int))
           (c2 (nelisp-cc--ssa-add-instr fn entry 'call (list x y) rv2)))
      (setf (nelisp-cc--ssa-instr-meta c2)
            (list :fn 'missing :unresolved t)))
    (let* ((registry (nelisp-cc-inline-test--registry callee))
           (escape-info (nelisp-cc-escape-analyze fn))
           (stats (nelisp-cc-inline-stats fn escape-info registry)))
      (should (= 1 (cdr (assq :go stats))))
      (let ((aborts (cdr (assq :abort stats))))
        (should (= 1 (cdr (assq :unresolved-callee aborts))))))))

;;; (14) inline-preserves-ssa-verifier -----------------------------

(ert-deftest nelisp-cc-inline-preserves-ssa-verifier ()
  "After the splice the SSA verifier still passes on the caller."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'leaf-add))
         (caller (car caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (nelisp-cc-inline-pass caller escape-info registry)
    (should (eq t (nelisp-cc--ssa-verify-function caller)))))

;;; (15) inline-rebinds-call-def -----------------------------------

(ert-deftest nelisp-cc-inline-rebinds-call-def ()
  "The original call's def value is rebound to the inserted `copy' instruction."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'leaf-add))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (rv (nth 2 caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller)))
    (nelisp-cc-inline-pass caller escape-info registry)
    ;; rv's def-point used to be the call instr; now it is the copy.
    (let ((dp (nelisp-cc--ssa-value-def-point rv)))
      (should-not (eq dp call-instr))
      (should (eq 'copy (nelisp-cc--ssa-instr-opcode dp))))))

;;; (16) inline-arity-mismatch-signals -----------------------------

(ert-deftest nelisp-cc-inline-arity-mismatch-signals ()
  "When the callee's param count differs from the call's args, splice signals."
  (let* ((callee (let* ((fn (nelisp-cc--ssa-make-function 'one-arg '(int)))
                        (entry (nelisp-cc--ssa-function-entry fn))
                        (x (car (nelisp-cc--ssa-function-params fn))))
                   (nelisp-cc--ssa-add-instr fn entry 'return (list x) nil)
                   fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call 'one-arg))
         (caller (car caller-spec))
         (call-instr (cadr caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (blk (nelisp-cc--ssa-instr-block call-instr)))
    (should-error
     (nelisp-cc--inline-splice-into caller blk call-instr callee)
     :type 'nelisp-cc-inline-error)))

;;; (17) inline-call-without-def-skipped ---------------------------

(ert-deftest nelisp-cc-inline-call-without-def-skipped ()
  "When the original `call' has no def we still inline the body but skip `copy'."
  (let* ((callee (nelisp-cc-inline-test--make-leaf-add-fn))
         (caller-spec (nelisp-cc-inline-test--make-caller-with-call
                       'leaf-add t))
         (caller (car caller-spec))
         (registry (nelisp-cc-inline-test--registry callee))
         (escape-info (nelisp-cc-escape-analyze caller))
         (result (nelisp-cc-inline-pass caller escape-info registry))
         (opcodes (mapcar #'nelisp-cc--ssa-instr-opcode
                          (nelisp-cc--ssa-block-instrs
                           (nelisp-cc--ssa-function-entry caller)))))
    (should (= 1 (cdr result)))
    (should (memq 'add opcodes))
    (should-not (memq 'copy opcodes))
    (should-not (memq 'call opcodes))))

(provide 'nelisp-cc-inline-test)
;;; nelisp-cc-inline-test.el ends here
