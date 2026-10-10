;;; nelisp-build-digest-test.el --- Bounded identity and hashing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)

(defun nelisp-build-digest-reference (bytes)
  "Lisp reference for the reader accessor, given its 32 rodata BYTES.
Return nil for an unstamped image. The production accessor never accepts
caller-supplied bytes or exposes the rodata address."
  (unless (= (length bytes) 32) (error "Build digest needs 32 bytes"))
  (unless (equal bytes (make-string 32 0))
    (let ((index 0) (hex ""))
      (while (< index 32)
        (setq hex (concat hex (format "%02x" (aref bytes index)))
              index (1+ index)))
      hex)))

(defun nelisp-build-digest-test--runtime-form (name)
  "Read one canonical runtime definition without loading the build toolchain."
  (with-temp-buffer
    (insert-file-contents (or (getenv "NELISP_SHA_SOURCE")
                              "scripts/nelisp-standalone-build.el"))
    (goto-char (point-min))
    (search-forward (concat "(defun " (symbol-name name) " "))
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(ert-deftest nelisp-build-digest-reference-unstamped-and-high-bytes ()
  (should-not (nelisp-build-digest-reference (make-string 32 0)))
  (should (equal (nelisp-build-digest-reference (make-string 32 255))
                 (make-string 64 ?f))))

(ert-deftest nelisp-build-digest-string-copy-has-constant-stack ()
  "Exercise the actual copy source on a host without native tail-call elimination.
The previous recursive form exceeds max-lisp-eval-depth before copying 1 MiB."
  (let* ((form (cl-subst 'progn 'seq
                         (nelisp-build-digest-test--runtime-form 'm5_sha_copy)))
         (copy (eval (cons 'lambda (cddr form)) t))
         (bytes (make-string 1048576 ?a))
         (written 0) (max-lisp-eval-depth 200))
    (cl-letf (((symbol-function 'm5_byte_at) (lambda (string index) (aref string index)))
              ((symbol-function 'ptr-write-u8)
               (lambda (_address index value)
                 (unless (and (= index written) (= value ?a)) (error "Copy mismatch"))
                 (setq written (1+ written))))
              ;; Retain the recursive call name for the against-the-bug control.
              ((symbol-function 'm5_sha_copy) copy))
      (funcall copy 0 bytes 0 (length bytes))
      (should (= written (length bytes))))))

(provide 'nelisp-build-digest-test)

(ert-deftest nelisp-build-digest-windows-buffer-hash-uses-bounded-native-input ()
  "Windows file-buffer hashes must not need a /tmp subprocess."
  (let* ((form (with-temp-buffer
                 (insert-file-contents (or (getenv "NELISP_HASH_PRELUDE")
                                           "scripts/nelisp-stdlib-prelude.el"))
                 (goto-char (point-min))
                 (search-forward "(defun secure-hash ")
                 (goto-char (match-beginning 0))
                 (read (current-buffer))))
         (hash (eval (cons 'lambda (cddr form)) t))
         (host-hash (symbol-function 'secure-hash))
         (system-type 'windows-nt) (calls 0))
    (cl-letf (((symbol-function 'nelisp--target-os-code) (lambda () 2))
              ((symbol-function 'nelisp--sha256)
               (lambda (bytes)
                 (should (stringp bytes))
                 (should (<= (string-bytes bytes) 1048576))
                 (setq calls (1+ calls))
                 (funcall host-hash 'sha256 bytes)))
              ((symbol-function 'nelisp--secure-hash-helper)
               (lambda (&rest _) (ert-fail "Windows SHA must not spawn a helper"))))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert (make-string 1048576 97))
        (should (equal (funcall hash 'sha256 (current-buffer))
                       "9bc1b2a288b26af7257a36277ae3816a7d4f16e89c1e7e77d0a5c48bad62b360"))
        (should (equal (funcall hash 'sha256 (current-buffer) 2 5)
                       (funcall host-hash 'sha256 "aaa"))))
      (should (= calls 2)))))
