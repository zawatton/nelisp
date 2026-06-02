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

(defconst nelisp-windows-build-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for Windows build-entry tests.")

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (or nelisp-windows-build-test--repo-root
                      (and test-dir
                           (file-name-directory
                            (directory-file-name test-dir)))))
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

(defun nelisp-windows-build-test--read-file-text (path)
  "Return PATH as plain text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest nelisp-windows-build-smoke-specs-are-stable ()
  "The Windows real-machine smoke manifest covers the expected PE specs."
  (should (equal nelisp-windows-build-smoke-specs
                 '((exit42 . minimal-exit-42)
                   (virtualalloc . virtualalloc-exit-42)
                   (virtualprotect-free . virtualprotect-free-exit-42)
                   (arena . virtualalloc-arena-exit-42)
                   (writefile-stdout . writefile-stdout-exit-42)
                   (getcommandline . getcommandline-exit-42)
                   (commandlinetoargv . commandlinetoargv-exit-42)
                   (wsastartup . wsastartup-exit-42))))
  (dolist (entry nelisp-windows-build-smoke-specs)
    (should (eq (nelisp-windows-build--normalize-spec (car entry))
                (cdr entry)))
    (should (eq (nelisp-windows-build--normalize-spec
                 (symbol-name (car entry)))
                (cdr entry)))))

(ert-deftest nelisp-windows-build-powershell-smoke-selector-matches-manifest ()
  "The PowerShell real-machine script exposes the same smoke names."
  (let* ((script-path (expand-file-name "tools/windows-selfhost-test.ps1"
                                        nelisp-windows-build-test--repo-root))
         (script (nelisp-windows-build-test--read-file-text script-path)))
    (should (string-match-p "\\[string\\[\\]\\]\\$Smoke" script))
    (should (string-match-p "-Smoke virtualalloc" script))
    (dolist (entry nelisp-windows-build-smoke-specs)
      (should (string-match-p
               (regexp-quote (format "Name = \"%s\"" (car entry)))
               script)))))

(ert-deftest nelisp-windows-build-smoke-exes-emits-all-pe-images ()
  "The Windows smoke build entry can emit every real-machine smoke EXE."
  (let ((out-dir (make-temp-file "nelisp-windows-smoke-" t)))
    (unwind-protect
        (let ((paths (nelisp-windows-build-smoke-exes out-dir)))
          (should (equal (mapcar #'file-name-nondirectory paths)
                         '("nelisp-windows-exit42.exe"
                           "nelisp-windows-virtualalloc.exe"
                           "nelisp-windows-virtualprotect-free.exe"
                           "nelisp-windows-arena.exe"
                           "nelisp-windows-writefile-stdout.exe"
                           "nelisp-windows-getcommandline.exe"
                           "nelisp-windows-commandlinetoargv.exe"
                           "nelisp-windows-wsastartup.exe")))
          (dolist (path paths)
            (let ((bytes (nelisp-windows-build-test--read-file-bytes path)))
              (should (> (length bytes) #x400))
              (should (equal (substring bytes 0 2)
                             (unibyte-string #x4d #x5a))))))
      (when (file-directory-p out-dir)
        (delete-directory out-dir t)))))

(provide 'nelisp-windows-build-test)

;;; nelisp-windows-build-test.el ends here
