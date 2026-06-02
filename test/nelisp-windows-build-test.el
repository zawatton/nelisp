;;; nelisp-windows-build-test.el --- ERT tests for Windows smoke build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for the native Windows PE32+ smoke build entry.  These
;; tests do not execute the generated EXEs; they verify that the same manifest
;; used by tools/windows-selfhost-test.ps1 can emit every smoke artifact on any
;; host with Emacs.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir))))
       (lisp-dir (and repo-root (expand-file-name "lisp" repo-root)))
       (scripts-dir (and repo-root (expand-file-name "scripts" repo-root))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir))
  (when (and scripts-dir (file-directory-p scripts-dir))
    (add-to-list 'load-path scripts-dir)))

(require 'nelisp-windows-build)

(defun nelisp-windows-build-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest nelisp-windows-build-smoke-specs-are-stable ()
  "The Windows real-machine smoke manifest covers the expected four PE specs."
  (should (equal nelisp-windows-build-smoke-specs
                 '((exit42 . minimal-exit-42)
                   (virtualalloc . virtualalloc-exit-42)
                   (arena . virtualalloc-arena-exit-42)
                   (writefile-stdout . writefile-stdout-exit-42))))
  (dolist (entry nelisp-windows-build-smoke-specs)
    (should (eq (nelisp-windows-build--normalize-spec (car entry))
                (cdr entry)))
    (should (eq (nelisp-windows-build--normalize-spec
                 (symbol-name (car entry)))
                (cdr entry)))))

(ert-deftest nelisp-windows-build-smoke-exes-emits-all-pe-images ()
  "The Windows smoke build entry can emit every real-machine smoke EXE."
  (let ((out-dir (make-temp-file "nelisp-windows-smoke-" t)))
    (unwind-protect
        (let ((paths (nelisp-windows-build-smoke-exes out-dir)))
          (should (equal (mapcar #'file-name-nondirectory paths)
                         '("nelisp-windows-exit42.exe"
                           "nelisp-windows-virtualalloc.exe"
                           "nelisp-windows-arena.exe"
                           "nelisp-windows-writefile-stdout.exe")))
          (dolist (path paths)
            (let ((bytes (nelisp-windows-build-test--read-file-bytes path)))
              (should (> (length bytes) #x400))
              (should (equal (substring bytes 0 2)
                             (unibyte-string #x4d #x5a))))))
      (when (file-directory-p out-dir)
        (delete-directory out-dir t)))))

(provide 'nelisp-windows-build-test)

;;; nelisp-windows-build-test.el ends here
