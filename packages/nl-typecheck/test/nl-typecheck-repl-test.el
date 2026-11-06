;;; nl-typecheck-repl-test.el --- REPL layers 1-2 tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nl-typecheck-repl)

(defconst nl-typecheck-repl-test--def
  '(defun clash-fn (n) (declare (ftype (function (integer) integer))) (* n 2)))

(defmacro nl-typecheck-repl-test--session (&rest body)
  `(let ((nelisp-typecheck-strict nil) (nl-typecheck-repl--sigs nil)
         (nl-typecheck-repl--defs nil) (nl-typecheck-repl--callers nil)
         (nl-typecheck-repl--count 0))
     ,@body))

(defun nl-typecheck-repl-test--out (form)
  "Return (OUTPUT . RESULT-OR-ERROR-SYMBOL) of checking FORM."
  (let ((res nil))
    (let ((out (with-output-to-string
                 (setq res (condition-case err
                               (nl-typecheck-repl-check-input form)
                             (nl-typecheck-rejected 'rejected)
                             (error (car err)))))))
      (cons out res))))

(ert-deftest nl-typecheck-repl-input-accumulated-environment ()
  (nl-typecheck-repl-test--session
   (nl-typecheck-repl-test--out nl-typecheck-repl-test--def)
   (let ((r (nl-typecheck-repl-test--out '(clash-fn "s"))))
     (should (eq (cdr r) t))
     (should (string-match-p "^<repl>:2:[0-9]+: error: argument 1 of clash-fn" (car r))))
   (let ((r (nl-typecheck-repl-test--out '(clash-fn 3))))
     (should (equal (car r) ""))
     (should (eq (cdr r) t)))))

(ert-deftest nl-typecheck-repl-input-strict-refusal ()
  (nl-typecheck-repl-test--session
   (nl-typecheck-repl-test--out nl-typecheck-repl-test--def)
   (let* ((nelisp-typecheck-strict t)
          (r (nl-typecheck-repl-test--out '(clash-fn "s"))))
     (should (eq (cdr r) 'rejected))
     (should (string-match-p "input 2 refused" (car r))))))

(ert-deftest nl-typecheck-repl-input-mode-toggle ()
  (let ((nelisp-repl-input-check-function nil) (nelisp-typecheck-repl-mode nil))
    (should (nelisp-typecheck-repl-mode 1))
    (should (eq nelisp-repl-input-check-function #'nl-typecheck-repl-check-input))
    (should-not (nelisp-typecheck-repl-mode 0))
    (should-not nelisp-repl-input-check-function)))

(ert-deftest nl-typecheck-repl-input-negative-control ()
  (nl-typecheck-repl-test--session
   (let ((nelisp-repl-input-check-function #'nl-typecheck-repl-check-input)
         (form '(progn (setq probe-flag t) (clash-fn "s"))))
     (eval nl-typecheck-repl-test--def t)
     (nl-typecheck-repl-test--out nl-typecheck-repl-test--def)
     (makunbound 'probe-flag)
     ;; strict: refused, side effect absent
     (let ((nelisp-typecheck-strict t))
       (with-output-to-string
         (should-error (when (funcall nelisp-repl-input-check-function form)
                         (eval form t))
                       :type 'nl-typecheck-rejected)))
     (should-not (boundp 'probe-flag))
     ;; not strict: evaluated
     (let ((nelisp-typecheck-strict nil))
       (with-output-to-string
         (when (funcall nelisp-repl-input-check-function form)
           (condition-case nil (eval form t) (error nil)))))
     (should (boundp 'probe-flag))
     (makunbound 'probe-flag))))

(ert-deftest nl-typecheck-redef-negative-control ()
  (nl-typecheck-repl-test--session
   (with-output-to-string
     (nl-typecheck-repl-check-input
      '(defun rd-callee (n) (declare (ftype (function (integer) integer))) n))
     (nl-typecheck-repl-check-input '(defun rd-ok (x) (rd-callee x)))
     (nl-typecheck-repl-check-input '(defun rd-bad () (rd-callee 5))))
   (let ((out (with-output-to-string
                (nl-typecheck-repl-check-input
                 '(defun rd-callee (s) (declare (ftype (function (string) integer))) (length s))))))
     (should (string-match-p "rd-bad" out))
     (should-not (string-match-p "rd-ok" out))
     (should (= (length nl-typecheck-repl-last-redef) 1))
     (should (eq (plist-get (car nl-typecheck-repl-last-redef) :callee) 'rd-callee)))))

(ert-deftest nl-typecheck-redef-type-change-reports-literal-caller ()
  (nl-typecheck-repl-test--session
   (with-output-to-string
     (nl-typecheck-repl-check-input
      '(defun rt-callee (n) (declare (ftype (function (integer) integer))) n))
     (nl-typecheck-repl-check-input '(defun rt-caller () (rt-callee 3))))
   (with-output-to-string
     (nl-typecheck-repl-check-input
      '(defun rt-callee (s) (declare (ftype (function (string) integer))) (length s))))
   (should (= (length nl-typecheck-repl-last-redef) 1))
   ;; unchanged signature: nothing re-reported
   (with-output-to-string
     (nl-typecheck-repl-check-input
      '(defun rt-callee (s) (declare (ftype (function (string) integer))) (length s))))
   (should-not nl-typecheck-repl-last-redef)))

(provide 'nl-typecheck-repl-test)
;;; nl-typecheck-repl-test.el ends here
