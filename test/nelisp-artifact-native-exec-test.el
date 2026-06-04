;;; nelisp-artifact-native-exec-test.el --- Doc 142 general native exec tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side proof tests for Doc 142's builtin-calling native-exec path.

;;; Code:

(require 'ert)
(require 'nelisp-artifact)

(defun nelisp-artifact-native-exec-test--linux-x86_64-p ()
  "Return non-nil when the host matches the current native proof lane."
  (and (eq system-type 'gnu/linux)
       (string-match-p "x86_64" (or system-configuration ""))))

(ert-deftest nelisp-artifact/native-exec-general-builtin-call1 ()
  "Builtin-calling `.neln' defuns execute through the host proof harness.
The current proof lane covers the hidden boundary-slot setup plus the
`nl_alloc_symbol' / `nelisp_aot_builtin_call1' runtime shims on
x86_64 Linux."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-exec-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-let-if (x)
  (let ((y (1+ x)))
    (if (> x 0) (1+ y) (1- y))))
(defun nat-ng-compare (x y)
  (if (> x y) (1+ x) (1- y)))
(defun nat-ng-multi (a b c)
  (if (> a b) (1+ c) (1- c)))
(provide 'nat-ng)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-let-if" '(5))
                     (nat-ng-let-if 5)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-let-if" '(-2))
                     (nat-ng-let-if -2)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-compare" '(9 4))
                     (nat-ng-compare 9 4)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-compare" '(1 4))
                     (nat-ng-compare 1 4)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-multi" '(9 4 7))
                     (nat-ng-multi 9 4 7)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-multi" '(1 4 7))
                     (nat-ng-multi 1 4 7))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-builtin-calln-eq ()
  "A vararg builtin calln defun executes through the host proof harness."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-exec-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-eq-flag (x y)
  (eq x y))
(provide 'nat-ng-eq)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-eq-flag" '(5 5))
                     1))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-eq-flag" '(-2 7))
                     0)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'nelisp-artifact-native-exec-test)

;;; nelisp-artifact-native-exec-test.el ends here
