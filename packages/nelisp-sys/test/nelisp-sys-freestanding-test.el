;;; nelisp-sys-freestanding-test.el --- ERT tests for the freestanding spike -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 132.7: freestanding executable mode is DESIGNED but not shipped.
;; These tests pin the boundary: the experimental freestanding fixture
;; parses and type-checks, but the hosted-C-ABI MVP backend deliberately
;; refuses to lower its `sys:exit' entry (a clear error, not a miscompile).

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-check)
(require 'nelisp-sys-backend)

(defconst nelisp-sys-freestanding-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defun nelisp-sys-freestanding-test--fixture-forms ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "fixtures/freestanding-main.nl"
                       nelisp-sys-freestanding-test--dir))
    (goto-char (point-min))
    (let ((forms '()))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest nelisp-sys-freestanding-fixture-parses ()
  "The experimental freestanding fixture parses to a _start defun."
  (let* ((forms (nelisp-sys-freestanding-test--fixture-forms))
         (mod (nelisp-sys-frontend-parse-module forms))
         (items (nelisp-sys-ast-prop mod :items)))
    (should (= 1 (length items)))
    (should (eq 'defun (nelisp-sys-ast-kind (car items))))
    (should (eq '_start (nelisp-sys-ast-prop (car items) :name)))))

(ert-deftest nelisp-sys-freestanding-typechecks ()
  "The freestanding entry type-checks (sys:exit is void/noreturn)."
  (let ((mod (nelisp-sys-frontend-parse-module
              (nelisp-sys-freestanding-test--fixture-forms))))
    (should (nelisp-sys-check-module mod))))

(ert-deftest nelisp-sys-freestanding-codegen-deferred ()
  "The hosted MVP backend refuses to lower the freestanding entry (sys:exit)."
  (should-error
   (nelisp-sys-backend-lower-module
    (nelisp-sys-frontend-parse-module
     (nelisp-sys-freestanding-test--fixture-forms))
    "x86_64-unknown-linux-gnu")
   :type 'nelisp-sys-backend-error))

;;; nelisp-sys-freestanding-test.el ends here
