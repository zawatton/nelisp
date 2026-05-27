;;; nelisp-sys-abi-meta-test.el --- ERT tests for nelisp-sys ABI metadata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 132.4 gate: the emitted ABI summary matches the source exports and
;; the target descriptor.

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-abi-meta)

(defun nelisp-sys-abi-meta-test--module (forms)
  (nelisp-sys-frontend-parse-module forms))

(ert-deftest nelisp-sys-abi-meta-exports-only-exported ()
  (let* ((m (nelisp-sys-abi-meta-test--module
             '((sys:defun add ((a i32) (b i32)) i32 (:abi c :export "nl_add") (+ a b))
               (sys:defun helper ((x i32)) i32 (:private) x))))
         (exports (nelisp-sys-abi-meta-exports m)))
    (should (= 1 (length exports)))
    (should (equal '("nl_add" :args (i32 i32) :ret i32) (car exports)))))

(ert-deftest nelisp-sys-abi-meta-export-name-defaults-to-fn-name ()
  (let* ((m (nelisp-sys-abi-meta-test--module
             '((sys:defun frob ((p (ptr u8))) void (:export "frob") (seq)))))
         (exports (nelisp-sys-abi-meta-exports m)))
    (should (equal '("frob" :args ((ptr u8)) :ret void) (car exports)))))

(ert-deftest nelisp-sys-abi-meta-module-summary-linux-x86_64 ()
  (let* ((m (nelisp-sys-abi-meta-test--module
             '((sys:defun add ((a i32) (b i32)) i32 (:abi c :export "nl_add") (+ a b)))))
         (meta (nelisp-sys-abi-meta-module m "x86_64-unknown-linux-gnu")))
    (should (string= "x86_64-unknown-linux-gnu" (plist-get meta :target)))
    (should (eq 'elf64 (plist-get meta :object)))
    (should (eq 'sysv-amd64 (plist-get meta :c-abi)))
    (should (= 64 (plist-get meta :pointer-width)))
    (should (equal '(("nl_add" :args (i32 i32) :ret i32))
                   (plist-get meta :exports)))))

(ert-deftest nelisp-sys-abi-meta-summary-cross-target ()
  (let* ((m (nelisp-sys-abi-meta-test--module
             '((sys:defun a () void (:export "a") (seq)))))
         (win (nelisp-sys-abi-meta-module m "x86_64-pc-windows-msvc")))
    (should (eq 'pe-coff (plist-get win :object)))
    (should (eq 'win64 (plist-get win :c-abi)))))

(ert-deftest nelisp-sys-abi-meta-to-string-roundtrips ()
  (let* ((m (nelisp-sys-abi-meta-test--module
             '((sys:defun add ((a i32) (b i32)) i32 (:export "nl_add") (+ a b)))))
         (meta (nelisp-sys-abi-meta-module m "x86_64-unknown-linux-gnu"))
         (s (nelisp-sys-abi-meta-to-string meta)))
    (should (stringp s))
    (should (equal meta (car (read-from-string s))))))

;;; nelisp-sys-abi-meta-test.el ends here
