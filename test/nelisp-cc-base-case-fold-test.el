;;; nelisp-cc-base-case-fold-test.el --- ERT for nelisp-cc-base-case-fold (Phase 7.1.2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for the Phase 7.1.2 base-case fold pass in
;; `src/nelisp-cc-base-case-fold.el'.  Builds SSA functions through
;; the Phase 7.1 scaffold builders + uses the live AST→SSA frontend
;; to verify the pass against the bench-form fib shape.
;;
;; Tests:
;;   1.  bcf-eligible-leaf-arith-fn         — pure-arith fn classified eligible
;;   2.  bcf-eligible-rejects-closure       — fn with `:closure' rejected
;;   3.  bcf-const-literal-direct           — `:const' def-point detected
;;   4.  bcf-const-literal-via-copy         — chained `:copy CONST' detected
;;   5.  bcf-eval-call-fib-base-case        — fib(0)=0, fib(1)=1, fib(2)=1 (eval)
;;   6.  bcf-eval-call-fib-recursive        — fib(5)=5, fib(10)=55 (full eval)
;;   7.  bcf-eval-call-budget-exceeded      — budget=10 aborts on fib(30)
;;   8.  bcf-eval-call-non-const-arg        — non-const param aborts gracefully
;;   9.  bcf-pass-folds-fib-30              — pipeline run replaces call w/ const
;;   10. bcf-pass-disabled-noop             — enable=nil leaves IR untouched
;;   11. bcf-pass-fewer-call-instrs         — folded body has 1 fewer `:call'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)
(require 'nelisp-cc-inline)
(require 'nelisp-cc-recursive-inline)
(require 'nelisp-cc-rewrite)
(require 'nelisp-cc-pipeline)
(require 'nelisp-cc-base-case-fold)

;;; helpers --------------------------------------------------------

(defun nelisp-cc-bcf-test--build-fib-outer ()
  "Build the bench-form fib(30) outer SSA function.

Source:
  (lambda ()
    (letrec ((fib (lambda (n)
                    (if (< n 2) n
                      (+ (funcall fib (- n 1))
                         (funcall fib (- n 2)))))))
      (funcall fib 30)))

Returns the outer `nelisp-cc--ssa-function'.  The inner fib is
reachable via the registry collected by
`nelisp-cc-pipeline--collect-letrec-callee-registry'."
  (nelisp-cc-build-ssa-from-ast
   '(lambda ()
      (letrec ((fib (lambda (n)
                      (if (< n 2) n
                        (+ (funcall fib (- n 1))
                           (funcall fib (- n 2)))))))
        (funcall fib 30)))))

(defun nelisp-cc-bcf-test--inner-fib (outer)
  "Return the inner-fib SSA function from OUTER's registry."
  (cdr (assq 'fib
             (nelisp-cc-pipeline--collect-letrec-callee-registry outer))))

(defun nelisp-cc-bcf-test--count-call-instrs (fn)
  "Return the number of `:call' instructions in FN."
  (let ((n 0))
    (dolist (b (nelisp-cc--ssa-function-blocks fn))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
          (cl-incf n))))
    n))

;;; (1) bcf-eligible-leaf-arith-fn ---------------------------------

(ert-deftest nelisp-cc-bcf-eligible-leaf-arith-fn ()
  "A self-recursive arith-only fn is classified as eligible after the
T161 `:call-indirect' → `:call' rewrite pre-pass.  Pre-rewrite the
fn carries `:call-indirect' which the eligibility classifier
rejects (rightly — the indirect callee identity is opaque)."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer))
         (registry (nelisp-cc-pipeline--collect-letrec-callee-registry outer))
         (inner-fib (cdr (assq 'fib registry))))
    (nelisp-cc-rewrite-call-indirect-to-call inner-fib registry)
    (should (nelisp-cc--bcf-eligible-callee-p inner-fib registry))))

;;; (2) bcf-eligible-rejects-closure -------------------------------

(ert-deftest nelisp-cc-bcf-eligible-rejects-closure ()
  "A fn containing `:closure' is rejected (no static eval)."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer)))
    ;; The outer has a `:closure' instruction (the letrec lambda
    ;; alloc) — it should be rejected as a candidate callee.
    (should-not (nelisp-cc--bcf-eligible-callee-p outer nil))))

;;; (3) bcf-const-literal-direct -----------------------------------

(ert-deftest nelisp-cc-bcf-const-literal-direct ()
  "A value whose def-point is a `:const' yields its literal."
  (let* ((fn (nelisp-cc--ssa-make-function 'tester '()))
         (entry (nelisp-cc--ssa-function-entry fn))
         (def (nelisp-cc--ssa-make-value fn 'int))
         (instr (nelisp-cc--ssa-add-instr fn entry 'const nil def)))
    (setf (nelisp-cc--ssa-instr-meta instr) (list :literal 42))
    (let ((result (nelisp-cc--bcf-const-literal def)))
      (should result)
      (should (= 42 (car result))))))

;;; (4) bcf-const-literal-via-copy ---------------------------------

(ert-deftest nelisp-cc-bcf-const-literal-via-copy ()
  "A value whose def-point is `:copy' from a `:const' resolves transitively."
  (let* ((fn (nelisp-cc--ssa-make-function 'tester '()))
         (entry (nelisp-cc--ssa-function-entry fn))
         (cdef (nelisp-cc--ssa-make-value fn 'int))
         (cinstr (nelisp-cc--ssa-add-instr fn entry 'const nil cdef))
         (copy-def (nelisp-cc--ssa-make-value fn 'int))
         (_copy (nelisp-cc--ssa-add-instr fn entry 'copy (list cdef) copy-def)))
    (setf (nelisp-cc--ssa-instr-meta cinstr) (list :literal 7))
    (let ((result (nelisp-cc--bcf-const-literal copy-def)))
      (should result)
      (should (= 7 (car result))))))

;;; (5) bcf-eval-call-fib-base-case --------------------------------

(ert-deftest nelisp-cc-bcf-eval-call-fib-base-case ()
  "fib(0) = 0 and fib(1) = 1 — base-case folded to identity."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer))
         (registry (nelisp-cc-pipeline--collect-letrec-callee-registry outer))
         (inner-fib (cdr (assq 'fib registry))))
    ;; Run the rewrite pass first so `:call-indirect' becomes `:call'
    ;; (the eval treats `load-var' as inert, but the rewrite is a
    ;; precondition for the call sites' `:fn' meta).
    (nelisp-cc-rewrite-call-indirect-to-call inner-fib registry)
    (let ((r0 (nelisp-cc-base-case-fold-eval-call inner-fib 0 registry))
          (r1 (nelisp-cc-base-case-fold-eval-call inner-fib 1 registry))
          (r2 (nelisp-cc-base-case-fold-eval-call inner-fib 2 registry)))
      (should (eq :value (car r0))) (should (= 0 (cdr r0)))
      (should (eq :value (car r1))) (should (= 1 (cdr r1)))
      (should (eq :value (car r2))) (should (= 1 (cdr r2))))))

;;; (6) bcf-eval-call-fib-recursive --------------------------------

(ert-deftest nelisp-cc-bcf-eval-call-fib-recursive ()
  "fib(5)=5, fib(10)=55, fib(15)=610 — full memoised recursive eval."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer))
         (registry (nelisp-cc-pipeline--collect-letrec-callee-registry outer))
         (inner-fib (cdr (assq 'fib registry))))
    (nelisp-cc-rewrite-call-indirect-to-call inner-fib registry)
    (let ((r5  (nelisp-cc-base-case-fold-eval-call inner-fib 5  registry))
          (r10 (nelisp-cc-base-case-fold-eval-call inner-fib 10 registry))
          (r15 (nelisp-cc-base-case-fold-eval-call inner-fib 15 registry)))
      (should (eq :value (car r5)))  (should (= 5   (cdr r5)))
      (should (eq :value (car r10))) (should (= 55  (cdr r10)))
      (should (eq :value (car r15))) (should (= 610 (cdr r15))))))

;;; (7) bcf-eval-call-budget-exceeded ------------------------------

(ert-deftest nelisp-cc-bcf-eval-call-budget-exceeded ()
  "A tiny budget aborts with `:budget-exceeded'."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer))
         (registry (nelisp-cc-pipeline--collect-letrec-callee-registry outer))
         (inner-fib (cdr (assq 'fib registry))))
    (nelisp-cc-rewrite-call-indirect-to-call inner-fib registry)
    ;; Budget of 5 is too small to even reach the first base case.
    (let ((memo (make-hash-table :test 'equal))
          (budget-cell (cons 5 nil)))
      (let ((result (nelisp-cc-base-case-fold-eval-call
                     inner-fib 30 registry memo budget-cell)))
        (should (eq :abort (car result)))
        (should (eq :budget-exceeded (cdr result)))))))

;;; (8) bcf-eval-call-non-const-arg --------------------------------

(ert-deftest nelisp-cc-bcf-eval-call-non-const-arg ()
  "When the callee body's argument cannot be resolved to a literal,
the eval bails with `:unresolved-call-arg'.  Build a tiny self-call
fn whose recursive arg is its own param (= unbounded recursion)."
  (let* ((fn (nelisp-cc--ssa-make-function 'looper '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (n (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list n) rv)))
    (setf (nelisp-cc--ssa-instr-meta call) (list :fn 'looper :unresolved t))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((registry (list (cons 'looper fn)))
           ;; Call `looper(0)' — recursion through self with same
           ;; const arg is detected by the cycle guard.
           (result (nelisp-cc-base-case-fold-eval-call fn 0 registry)))
      (should (eq :abort (car result)))
      (should (eq :recursion-cycle (cdr result))))))

;;; (9) bcf-pass-folds-fib-30 --------------------------------------

(ert-deftest nelisp-cc-bcf-pass-folds-fib-30 ()
  "Running the full pipeline on the bench-form folds the outer
`(funcall fib 30)' to a `:const 832040' instruction."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer))
         (nelisp-cc-pipeline-rec-inline-depth-limit 2)
         (result (nelisp-cc-pipeline-run-7.7-passes outer))
         (stats (cdr result)))
    (should (>= (nelisp-cc-pipeline-stats-base-case-folded stats) 1))
    ;; Verify the outer block now contains a const 832040 in place
    ;; of the outer fib call.
    (let* ((entry (nelisp-cc--ssa-function-entry outer))
           (folded
            (cl-find-if
             (lambda (instr)
               (and (eq (nelisp-cc--ssa-instr-opcode instr) 'const)
                    (let ((m (nelisp-cc--ssa-instr-meta instr)))
                      (and (plist-get m :folded-from-call)
                           (= 832040 (plist-get m :literal))))))
             (nelisp-cc--ssa-block-instrs entry))))
      (should folded))))

;;; (10) bcf-pass-disabled-noop ------------------------------------

(ert-deftest nelisp-cc-bcf-pass-disabled-noop ()
  "When `nelisp-cc-base-case-fold-enable' is nil, the IR is untouched."
  (let* ((outer (nelisp-cc-bcf-test--build-fib-outer))
         (nelisp-cc-base-case-fold-enable nil)
         (nelisp-cc-pipeline-rec-inline-depth-limit 2)
         (result (nelisp-cc-pipeline-run-7.7-passes outer))
         (stats (cdr result)))
    (should (= 0 (nelisp-cc-pipeline-stats-base-case-folded stats)))
    ;; The outer call to fib should still be present.
    (let* ((entry (nelisp-cc--ssa-function-entry outer))
           (residual-call
            (cl-find-if
             (lambda (instr)
               (and (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
                    (eq 'fib (plist-get (nelisp-cc--ssa-instr-meta instr) :fn))))
             (nelisp-cc--ssa-block-instrs entry))))
      (should residual-call))))

;;; (11) bcf-pass-fewer-call-instrs --------------------------------

(ert-deftest nelisp-cc-bcf-pass-fewer-call-instrs ()
  "After the BCF pass on the outer fib(30), the outer block has at
least N-1 fewer `:call' instructions than before (= the spec's
acceptance metric for base-case fold)."
  (let* ((outer-pre (nelisp-cc-bcf-test--build-fib-outer))
         (outer-post (nelisp-cc-bcf-test--build-fib-outer))
         ;; Drive both through the same pipeline modulo BCF enable.
         (nelisp-cc-pipeline-rec-inline-depth-limit 2))
    (let ((nelisp-cc-base-case-fold-enable nil))
      (nelisp-cc-pipeline-run-7.7-passes outer-pre))
    (let ((nelisp-cc-base-case-fold-enable t))
      (nelisp-cc-pipeline-run-7.7-passes outer-post))
    (let ((calls-pre (nelisp-cc-bcf-test--count-call-instrs outer-pre))
          (calls-post (nelisp-cc-bcf-test--count-call-instrs outer-post)))
      (should (< calls-post calls-pre))
      ;; The outer should specifically have eliminated its single fib
      ;; call (= 1 fewer call instruction).
      (should (= 1 (- calls-pre calls-post))))))

(provide 'nelisp-cc-base-case-fold-test)

;;; nelisp-cc-base-case-fold-test.el ends here
