;;; nelisp-cc-rewrite-test.el --- T161 :call-indirect rewrite ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T161 — ERT for `nelisp-cc-rewrite-call-indirect-to-call' (the
;; pre-inliner pass that rewrites `:call-indirect' sites whose callee
;; resolves through a `:load-var' OR a `:store-var :letrec-init t' to a
;; direct `:call' with `:fn NAME' meta).
;;
;; Test list:
;;   1.  rewrite-self-ref-load-var      — inner-body fib recursion
;;   2.  rewrite-letrec-store-outer     — outer-lambda funcall path
;;   3.  rewrite-skip-non-load-var      — opaque callee untouched
;;   4.  rewrite-skip-unregistered      — load-var name not in registry
;;   5.  rewrite-pipeline-fib-non-zero  — bench-form fib drives ≥1 inline
;;   6.  rewrite-preserves-arity        — argument operands intact

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-rewrite)
(require 'nelisp-cc-pipeline)

;;; helpers --------------------------------------------------------

(defun nelisp-cc-rewrite-test--make-load-var-call-indirect ()
  "Build a synthetic SSA: `(load-var fib) → (call-indirect fib 1)`.
Returns (FN . SITE-INSTR)."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (one (nelisp-cc--ssa-make-value fn 'int))
         (load-def (nelisp-cc--ssa-make-value fn nil))
         (load-instr (nelisp-cc--ssa-add-instr fn entry 'load-var nil load-def))
         (rv (nelisp-cc--ssa-make-value fn nil))
         (call (nelisp-cc--ssa-add-instr fn entry 'call-indirect
                                         (list load-def one) rv)))
    (setf (nelisp-cc--ssa-instr-meta load-instr) (list :name 'fib))
    (setf (nelisp-cc--ssa-instr-meta call) (list :indirect t :funcall t))
    (nelisp-cc--ssa-add-instr fn entry 'const nil one)
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (cons fn call)))

(defun nelisp-cc-rewrite-test--make-store-var-letrec-init-call-indirect ()
  "Build synthetic SSA: store-var :letrec-init → call-indirect via store def.
Returns (FN . SITE-INSTR)."
  (let* ((fn (nelisp-cc--ssa-make-function 'outer nil))
         (entry (nelisp-cc--ssa-function-entry fn))
         ;; Materialise a placeholder `closure' value (def of a `:closure' instr).
         (closure-def (nelisp-cc--ssa-make-value fn nil))
         (closure-instr (nelisp-cc--ssa-add-instr fn entry 'closure nil closure-def))
         ;; Letrec-init store: operand = closure-def, def = store-def.
         (store-def (nelisp-cc--ssa-make-value fn nil))
         (store-instr (nelisp-cc--ssa-add-instr fn entry 'store-var
                                                (list closure-def) store-def))
         (arg (nelisp-cc--ssa-make-value fn 'int))
         (rv (nelisp-cc--ssa-make-value fn nil))
         (call (nelisp-cc--ssa-add-instr fn entry 'call-indirect
                                         (list store-def arg) rv)))
    (setf (nelisp-cc--ssa-instr-meta closure-instr) (list :inner-function nil))
    (setf (nelisp-cc--ssa-instr-meta store-instr)
          (list :name 'helper :letrec-init t))
    (setf (nelisp-cc--ssa-instr-meta call) (list :indirect t :funcall t))
    (nelisp-cc--ssa-add-instr fn entry 'const nil arg)
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (cons fn call)))

;;; tests ----------------------------------------------------------

(ert-deftest nelisp-cc-rewrite-self-ref-load-var ()
  "`call-indirect' whose op[0] is `load-var fib' is rewritten to direct call.
Registry contains `fib' → eligibility OK."
  (let* ((built (nelisp-cc-rewrite-test--make-load-var-call-indirect))
         (fn (car built))
         (site (cdr built))
         (registry (list (cons 'fib fn)))
         (result (nelisp-cc-rewrite-call-indirect-to-call fn registry)))
    (should (eq fn (car result)))
    (should (= 1 (cdr result)))
    ;; Site is now opcode `call' with `:fn fib' meta.
    (should (eq 'call (nelisp-cc--ssa-instr-opcode site)))
    (let ((meta (nelisp-cc--ssa-instr-meta site)))
      (should (eq 'fib (plist-get meta :fn)))
      (should (eq t (plist-get meta :rewritten-from-call-indirect))))
    ;; First operand (closure) was dropped.
    (should (= 1 (length (nelisp-cc--ssa-instr-operands site))))))

(ert-deftest nelisp-cc-rewrite-letrec-store-outer ()
  "`call-indirect' whose op[0] is a letrec-init store def is rewritten."
  (let* ((built (nelisp-cc-rewrite-test--make-store-var-letrec-init-call-indirect))
         (fn (car built))
         (site (cdr built))
         (registry (list (cons 'helper fn)))
         (result (nelisp-cc-rewrite-call-indirect-to-call fn registry)))
    (should (= 1 (cdr result)))
    (should (eq 'call (nelisp-cc--ssa-instr-opcode site)))
    (should (eq 'helper (plist-get (nelisp-cc--ssa-instr-meta site) :fn)))))

(ert-deftest nelisp-cc-rewrite-skip-non-load-var ()
  "`call-indirect' whose op[0] is opaque (e.g. a `:param') stays untouched."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (arg (nelisp-cc--ssa-make-value fn 'int))
         (rv (nelisp-cc--ssa-make-value fn nil))
         (call (nelisp-cc--ssa-add-instr fn entry 'call-indirect
                                         (list param arg) rv))
         (registry (list (cons 'foo fn)))
         (result (nelisp-cc-rewrite-call-indirect-to-call fn registry)))
    (setf (nelisp-cc--ssa-instr-meta call) (list :indirect t :funcall t))
    (nelisp-cc--ssa-add-instr fn entry 'const nil arg)
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (should (= 0 (cdr result)))
    (should (eq 'call-indirect (nelisp-cc--ssa-instr-opcode call)))))

(ert-deftest nelisp-cc-rewrite-skip-unregistered ()
  "`load-var' name not in registry → site stays `call-indirect'."
  (let* ((built (nelisp-cc-rewrite-test--make-load-var-call-indirect))
         (fn (car built))
         (site (cdr built))
         (registry (list (cons 'other-name fn)))
         (result (nelisp-cc-rewrite-call-indirect-to-call fn registry)))
    (should (= 0 (cdr result)))
    (should (eq 'call-indirect (nelisp-cc--ssa-instr-opcode site)))))

(ert-deftest nelisp-cc-rewrite-pipeline-fib-non-zero ()
  "Bench-form fib drives at least one rewrite + downstream inline.
T158 baseline = simple=0 rec=0 lifted=0 rewrote=0;
T161 expectation = rewrote-call-indirect ≥ 1 (= the inner self-call sites)."
  (let* ((nelisp-cc-enable-7.7-passes t)
         (fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (letrec ((fib (lambda (n)
                                 (if (< n 2) n
                                   (+ (funcall fib (- n 1))
                                      (funcall fib (- n 2)))))))
                   (funcall fib 30)))))
         (result (nelisp-cc-pipeline-run-7.7-passes fn))
         (stats (cdr result)))
    (should (>= (nelisp-cc-pipeline-stats-rewrote-call-indirect stats) 1))
    (should (>= (nelisp-cc-pipeline-stats-registry-size stats) 1))
    ;; SSA still verifies post-rewrite + downstream passes.
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

(ert-deftest nelisp-cc-rewrite-preserves-arity ()
  "Argument operands (= rest after operand[0]) survive verbatim post-rewrite."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (a (car (nelisp-cc--ssa-function-params fn)))
         (b (cadr (nelisp-cc--ssa-function-params fn)))
         (load-def (nelisp-cc--ssa-make-value fn nil))
         (load-instr (nelisp-cc--ssa-add-instr fn entry 'load-var nil load-def))
         (rv (nelisp-cc--ssa-make-value fn nil))
         (call (nelisp-cc--ssa-add-instr fn entry 'call-indirect
                                         (list load-def a b) rv)))
    (setf (nelisp-cc--ssa-instr-meta load-instr) (list :name 'fib))
    (setf (nelisp-cc--ssa-instr-meta call) (list :indirect t :funcall t))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((registry (list (cons 'fib fn)))
           (result (nelisp-cc-rewrite-call-indirect-to-call fn registry)))
      (should (= 1 (cdr result)))
      (let ((ops (nelisp-cc--ssa-instr-operands call)))
        (should (= 2 (length ops)))
        (should (eq a (nth 0 ops)))
        (should (eq b (nth 1 ops)))))))

(provide 'nelisp-cc-rewrite-test)

;;; nelisp-cc-rewrite-test.el ends here
