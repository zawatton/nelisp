;;; nelisp-bytecode-test.el --- ERT for Phase 3b bytecode VM  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3b.1 seeded the module with data shapes and stubs.  Phase 3b.2
;; ships the first five opcodes (RETURN / CONST / STACK-REF / DROP /
;; DUP) and a minimal compiler that handles self-evaluating atoms and
;; `(quote X)'.  Tests below split into:
;;
;;   - data shape (3b.1 carry-over)
;;   - opcode table (5 ops wired)
;;   - compiler + run round-trip (literal + quoted forms)
;;   - handwritten bcl exercising each opcode + error paths
;;
;; See docs/design/08-bytecode-vm.org §3.3b.2.

;;; Code:

(require 'ert)
(require 'nelisp-bytecode)

(defun nelisp-bc-test--code (ops)
  "Assemble OPS (list of (OP ARG...) or bare OP symbol) into a vector.
Each symbolic op is replaced with its opcode byte via
`nelisp-bc-opcode'; numeric args are inserted verbatim."
  (let ((acc nil))
    (dolist (item ops)
      (cond
       ((symbolp item)
        (push (nelisp-bc-opcode item) acc))
       ((and (consp item) (symbolp (car item)))
        (push (nelisp-bc-opcode (car item)) acc)
        (dolist (arg (cdr item)) (push arg acc)))
       (t (error "bad test code item: %S" item))))
    (apply #'vector (nreverse acc))))

;;; Data shape (from 3b.1) -------------------------------------------

(ert-deftest nelisp-bc-skeleton-make-and-predicate ()
  (let ((bcl (nelisp-bc-make nil '(x) [] [] 0 0)))
    (should (nelisp-bcl-p bcl))
    (should-not (nelisp-bcl-p '(nelisp-closure nil () nil)))
    (should-not (nelisp-bcl-p nil))
    (should-not (nelisp-bcl-p 42))))

(ert-deftest nelisp-bc-skeleton-accessors ()
  (let* ((env '((a . 1)))
         (params '(x &optional y))
         (consts [foo "bar" 3])
         (code [0 1 2])
         (bcl (nelisp-bc-make env params consts code 4 #b101)))
    (should (equal (nelisp-bc-env bcl) env))
    (should (equal (nelisp-bc-params bcl) params))
    (should (equal (nelisp-bc-consts bcl) consts))
    (should (equal (nelisp-bc-code bcl) code))
    (should (=     (nelisp-bc-stack-depth bcl) 4))
    (should (=     (nelisp-bc-special-mask bcl) 5))))

(ert-deftest nelisp-bc-skeleton-error-hierarchy ()
  (should (memq 'nelisp-bc-error
                (get 'nelisp-bc-unimplemented 'error-conditions))))

;;; Opcode table (3b.2) ----------------------------------------------

(ert-deftest nelisp-bc-3b2-opcode-table ()
  (should (= (nelisp-bc-opcode 'RETURN)    0))
  (should (= (nelisp-bc-opcode 'CONST)     1))
  (should (= (nelisp-bc-opcode 'STACK-REF) 2))
  (should (= (nelisp-bc-opcode 'DROP)      3))
  (should (= (nelisp-bc-opcode 'DUP)       4))
  (should-error (nelisp-bc-opcode 'NOPE) :type 'nelisp-bc-error))

(ert-deftest nelisp-bc-3b2-opcode-arg-bytes ()
  (should (= (aref nelisp-bc--opcode-arg-bytes 0) 0)) ; RETURN
  (should (= (aref nelisp-bc--opcode-arg-bytes 1) 1)) ; CONST
  (should (= (aref nelisp-bc--opcode-arg-bytes 2) 1)) ; STACK-REF
  (should (= (aref nelisp-bc--opcode-arg-bytes 3) 0)) ; DROP
  (should (= (aref nelisp-bc--opcode-arg-bytes 4) 0))) ; DUP

;;; Compiler + run round-trip ----------------------------------------

(defun nelisp-bc-test--eval (form)
  "Compile FORM with the Phase 3b.2 compiler and run it on the VM."
  (nelisp-bc-run (nelisp-bc-compile form)))

(ert-deftest nelisp-bc-3b2-compile-self-evaluating ()
  (should (eq       (nelisp-bc-test--eval nil) nil))
  (should (eq       (nelisp-bc-test--eval t) t))
  (should (eq       (nelisp-bc-test--eval :k) :k))
  (should (=        (nelisp-bc-test--eval 42) 42))
  (should (equal    (nelisp-bc-test--eval 3.14) 3.14))
  (should (equal    (nelisp-bc-test--eval "hi") "hi"))
  (should (equal    (nelisp-bc-test--eval [1 2 3]) [1 2 3])))

(ert-deftest nelisp-bc-3b2-compile-quote ()
  (should (eq    (nelisp-bc-test--eval '(quote foo)) 'foo))
  (should (equal (nelisp-bc-test--eval '(quote (a b c))) '(a b c))))

(ert-deftest nelisp-bc-3b2-compile-unimplemented ()
  (should-error (nelisp-bc-compile '(+ 1 2))
                :type 'nelisp-bc-unimplemented))

(ert-deftest nelisp-bc-3b2-compile-captures-env ()
  ;; ENV is carried verbatim — not yet consulted, just preserved.
  (let* ((env '((a . 1) (b . 2)))
         (bcl (nelisp-bc-compile 42 env)))
    (should (equal (nelisp-bc-env bcl) env))
    (should (=     (nelisp-bc-run bcl) 42))))

;;; Handwritten bcl — each opcode ------------------------------------

(ert-deftest nelisp-bc-3b2-handwritten-const-return ()
  (let ((bcl (nelisp-bc-make nil nil [:payload]
                             (nelisp-bc-test--code '((CONST 0) RETURN))
                             1 0)))
    (should (eq (nelisp-bc-run bcl) :payload))))

(ert-deftest nelisp-bc-3b2-handwritten-drop ()
  ;; CONST 0, CONST 1, DROP, RETURN -> first constant
  (let ((bcl (nelisp-bc-make nil nil ["keep" "discard"]
                             (nelisp-bc-test--code
                              '((CONST 0) (CONST 1) DROP RETURN))
                             2 0)))
    (should (equal (nelisp-bc-run bcl) "keep"))))

(ert-deftest nelisp-bc-3b2-handwritten-dup ()
  (let ((bcl (nelisp-bc-make nil nil [99]
                             (nelisp-bc-test--code
                              '((CONST 0) DUP DROP RETURN))
                             2 0)))
    (should (= (nelisp-bc-run bcl) 99))))

(ert-deftest nelisp-bc-3b2-handwritten-stack-ref ()
  ;; CONST 0, CONST 1, STACK-REF 1, RETURN -> the first constant
  (let ((bcl (nelisp-bc-make nil nil [first second]
                             (nelisp-bc-test--code
                              '((CONST 0) (CONST 1) (STACK-REF 1) RETURN))
                             3 0)))
    (should (eq (nelisp-bc-run bcl) 'first))))

;;; Error paths -------------------------------------------------------

(ert-deftest nelisp-bc-3b2-run-rejects-args ()
  ;; 3b.2 has no BIND op; passing ARGS must surface the limitation.
  (let ((bcl (nelisp-bc-compile 0)))
    (should-error (nelisp-bc-run bcl '(1))
                  :type 'nelisp-bc-unimplemented)))

(ert-deftest nelisp-bc-3b2-run-rejects-non-bcl ()
  (should-error (nelisp-bc-run '(nelisp-closure nil () 0))
                :type 'nelisp-bc-error))

(ert-deftest nelisp-bc-3b2-unknown-opcode ()
  (let ((bcl (nelisp-bc-make nil nil [] [200] 1 0)))
    (should-error (nelisp-bc-run bcl) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b2-return-on-empty-stack ()
  (let ((bcl (nelisp-bc-make nil nil []
                             (nelisp-bc-test--code '(RETURN))
                             1 0)))
    (should-error (nelisp-bc-run bcl) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b2-fall-off-end ()
  ;; CODE ends without RETURN.
  (let ((bcl (nelisp-bc-make nil nil [1]
                             (nelisp-bc-test--code '((CONST 0)))
                             1 0)))
    (should-error (nelisp-bc-run bcl) :type 'nelisp-bc-error)))

(provide 'nelisp-bytecode-test)
;;; nelisp-bytecode-test.el ends here
