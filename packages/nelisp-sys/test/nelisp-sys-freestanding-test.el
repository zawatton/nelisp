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
(require 'nelisp-sys-driver)
(require 'nelisp-sys-adapter-nelisp)

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

(ert-deftest nelisp-sys-freestanding-lowers-exit ()
  "Doc 133 Phase 7: sys:exit now lowers to the AOT (exit ...) form.
The _start entry `(sys:exit 0)' lowers to `(defun _start () (exit 0))',
the AOT program shape the standalone-binary emitter consumes."
  (should (equal '(defun _start () (exit 0))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   (nelisp-sys-freestanding-test--fixture-forms))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-freestanding-exit0-runs ()
  "Doc 133 Phase 7 e2e: the nelisp-sys `_start' fixture `(sys:exit 0)'
compiles to a standalone native binary that runs and exits 0.
First nelisp-sys -> native executable e2e (self-host verification path)."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-exec")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           (nelisp-sys-freestanding-test--fixture-forms) path)
          (should (file-executable-p path))
          (should (= 0 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

;;; nelisp-sys-freestanding-test.el ends here
