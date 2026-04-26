;;; nelisp-cc-pipeline-test.el --- T158 SSA pass pipeline ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T158 — ERT for `nelisp-cc-pipeline-run-7.7-passes' (the codegen
;; integration of the four Phase 7.7 SSA passes).
;;
;; Test list:
;;   1.  pipeline-disabled-is-noop          — flag nil → zero stats
;;   2.  pipeline-empty-stats-on-leaf       — `(lambda (x) x)` no-ops
;;   3.  pipeline-fires-on-named-call-site  — synthetic registry inlines
;;   4.  pipeline-letrec-registry-built     — letrec lambdas indexed
;;   5.  pipeline-runtime-passes-stats      — runtime threads stats
;;   6.  pipeline-bench-form-shape          — fib bench-form documents
;;                                            the call-indirect blocker

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-pipeline)
(require 'nelisp-cc-runtime)

(ert-deftest nelisp-cc-pipeline-disabled-is-noop ()
  "When `nelisp-cc-enable-7.7-passes' is nil the driver returns zero stats."
  (let* ((nelisp-cc-enable-7.7-passes nil)
         (fn (nelisp-cc-build-ssa-from-ast '(lambda (x) x)))
         (result (nelisp-cc-pipeline-run-7.7-passes fn))
         (stats (cdr result)))
    (should (eq fn (car result)))
    (should (= 0 (nelisp-cc-pipeline-stats-simple-inlined stats)))
    (should (= 0 (nelisp-cc-pipeline-stats-rec-inlined stats)))
    (should (= 0 (nelisp-cc-pipeline-stats-lifted stats)))
    (should (= 0 (nelisp-cc-pipeline-stats-registry-size stats)))))

(ert-deftest nelisp-cc-pipeline-empty-stats-on-leaf ()
  "On `(lambda (x) x)` the driver runs but every count stays at 0."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x) x)))
         (result (nelisp-cc-pipeline-run-7.7-passes fn))
         (stats (cdr result)))
    (should (eq fn (car result)))
    (should (= 0 (nelisp-cc-pipeline-stats-simple-inlined stats)))
    (should (= 0 (nelisp-cc-pipeline-stats-rec-inlined stats)))
    (should (= 0 (nelisp-cc-pipeline-stats-lifted stats)))
    ;; No `:closure' instruction, so the registry is empty.
    (should (= 0 (nelisp-cc-pipeline-stats-registry-size stats)))
    ;; SSA still verifies — pipeline must preserve invariants.
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

(ert-deftest nelisp-cc-pipeline-letrec-registry-built ()
  "`letrec' forms with named lambdas populate the inliner registry."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (letrec ((helper (lambda (x) x)))
                   (funcall helper 1)))))
         (registry (nelisp-cc-pipeline--collect-letrec-callee-registry fn)))
    (should (= 1 (length registry)))
    (should (eq 'helper (caar registry)))
    (should (nelisp-cc--ssa-function-p (cdar registry)))))

(ert-deftest nelisp-cc-pipeline-runtime-threads-stats ()
  "`nelisp-cc-runtime-compile-and-allocate' surfaces `:pipeline-stats'."
  (let* ((nelisp-cc-runtime-exec-mode 'simulator)
         (result (nelisp-cc-runtime-compile-and-allocate
                  '(lambda (x) x) 'x86_64))
         (stats (plist-get result :pipeline-stats)))
    (should (nelisp-cc-pipeline-stats-p stats))
    (should (= 0 (nelisp-cc-pipeline-stats-simple-inlined stats)))))

(ert-deftest nelisp-cc-pipeline-fires-on-named-call-site ()
  "A synthetic `(lambda (x y) (LEAF-ADD x y))` inlines under the driver
when LEAF-ADD is in the registry."
  ;; Manually build caller + callee — same pattern as
  ;; `nelisp-cc-inline-test--make-leaf-add-fn'.
  (let* ((callee
          (let* ((fn (nelisp-cc--ssa-make-function 'leaf-add '(int int)))
                 (entry (nelisp-cc--ssa-function-entry fn))
                 (a (car (nelisp-cc--ssa-function-params fn)))
                 (b (cadr (nelisp-cc--ssa-function-params fn)))
                 (sum (nelisp-cc--ssa-make-value fn 'int)))
            (nelisp-cc--ssa-add-instr fn entry 'add (list a b) sum)
            (nelisp-cc--ssa-add-instr fn entry 'return (list sum) nil)
            fn))
         (caller
          (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int int)))
                 (entry (nelisp-cc--ssa-function-entry fn))
                 (x (car (nelisp-cc--ssa-function-params fn)))
                 (y (cadr (nelisp-cc--ssa-function-params fn)))
                 (rv (nelisp-cc--ssa-make-value fn 'int))
                 (call (nelisp-cc--ssa-add-instr fn entry 'call (list x y) rv)))
            (setf (nelisp-cc--ssa-instr-meta call)
                  (list :fn 'leaf-add :unresolved t))
            (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
            fn))
         ;; The pipeline's auto-registry walks `:closure' instructions only;
         ;; for the synthetic case we inject the registry by binding a
         ;; private helper that returns the desired alist.
         (cl-letf-bound nil))
    (cl-letf (((symbol-function 'nelisp-cc-pipeline--collect-letrec-callee-registry)
               (lambda (_fn) (list (cons 'leaf-add callee)))))
      (let* ((result (nelisp-cc-pipeline-run-7.7-passes caller))
             (stats (cdr result)))
        (should (eq caller (car result)))
        (should (= 1 (nelisp-cc-pipeline-stats-simple-inlined stats)))
        (should (= 1 (nelisp-cc-pipeline-stats-registry-size stats)))
        ;; Caller's `call' opcode is now gone; replaced by `add' + `copy'.
        (let ((opcodes (mapcar #'nelisp-cc--ssa-instr-opcode
                               (nelisp-cc--ssa-block-instrs
                                (nelisp-cc--ssa-function-entry caller)))))
          (should-not (memq 'call opcodes))
          (should (memq 'add opcodes)))))))

(ert-deftest nelisp-cc-pipeline-bench-fib-form-shape ()
  "Document the bench-form blocker — fib's recursion is `call-indirect`,
NOT a named `call`, so the simple/recursive inliner cannot fire on
the bench form as-is.  The lambda-lift ALSO declines because the
closure is stored to a letrec slot (escape verdict rejects the
`:stack-only' gate).  This test pins the negative outcome so any
future change that flips it surfaces in CI rather than silently."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (letrec ((fib (lambda (n)
                                 (if (< n 2) n
                                   (+ (funcall fib (- n 1))
                                      (funcall fib (- n 2)))))))
                   (funcall fib 30)))))
         (result (nelisp-cc-pipeline-run-7.7-passes fn))
         (stats (cdr result)))
    ;; Registry HAS the fib entry.
    (should (>= (nelisp-cc-pipeline-stats-registry-size stats) 1))
    ;; But the inliner doesn't fire because every recursion site is
    ;; `:call-indirect', not `:call' with `:fn fib' meta.
    (should (= 0 (nelisp-cc-pipeline-stats-simple-inlined stats)))
    (should (= 0 (nelisp-cc-pipeline-stats-rec-inlined stats)))))

(provide 'nelisp-cc-pipeline-test)

;;; nelisp-cc-pipeline-test.el ends here
