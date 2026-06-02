;;; nelisp-windows-build.el --- native Windows x86_64 smoke build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Builds native Windows x86_64 PE32+ smoke executables from the pure-elisp
;; PE writer.  These are deliberately small de-risk artifacts for Doc 138:
;; ExitProcess, VirtualAlloc, arena metadata, and stdout HANDLE I/O via
;; GetStdHandle + WriteFile.
;;
;; Run on a Windows machine with Emacs installed via:
;;
;;   .\tools\windows-selfhost-test.ps1
;;
;; The generated EXEs require no external compiler, linker, CRT, or signing.

;;; Code:

(require 'nelisp-pe-write)

(defconst nelisp-windows-build--this-file
  (or load-file-name buffer-file-name)
  "Absolute path of this build script.")

(defconst nelisp-windows-build--repo-root
  (file-name-directory
   (directory-file-name (file-name-directory nelisp-windows-build--this-file)))
  "Repo root, the parent directory of scripts/.")

(defconst nelisp-windows-build--default-out-dir
  (expand-file-name "target/windows-smoke" nelisp-windows-build--repo-root)
  "Default directory for generated Windows smoke EXEs.")

(defconst nelisp-windows-build--exit42-out
  (expand-file-name "target/nelisp-windows-exit42.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single ExitProcess(42) smoke EXE.")

(defconst nelisp-windows-build-smoke-specs
  '((exit42 . minimal-exit-42)
    (virtualalloc . virtualalloc-exit-42)
    (arena . virtualalloc-arena-exit-42)
    (writefile-stdout . writefile-stdout-exit-42))
  "Named Windows PE32+ smoke specs used by the real-machine script.")

(defun nelisp-windows-build--normalize-spec (spec)
  "Return a `nelisp-pe-write-exe-binary' spec from SPEC.
SPEC may be a symbol, a string naming a symbol, or a smoke name from
`nelisp-windows-build-smoke-specs'."
  (let* ((sym (cond
               ((symbolp spec) spec)
               ((stringp spec) (intern spec))
               (t (error "nelisp-windows-build: invalid spec %S" spec))))
         (mapped (cdr (assq sym nelisp-windows-build-smoke-specs))))
    (or mapped sym)))

(defun nelisp-windows-build-exe (spec out-path)
  "Build Windows PE32+ smoke executable SPEC to OUT-PATH.
Returns OUT-PATH."
  (let ((resolved (nelisp-windows-build--normalize-spec spec)))
    (make-directory (file-name-directory (expand-file-name out-path)) t)
    (nelisp-pe-write-exe-binary out-path resolved)
    (message "nelisp-windows-build: wrote %s (%S)" out-path resolved)
    out-path))

(defun nelisp-windows-build-smoke-exes (&optional out-dir)
  "Build all Windows smoke EXEs into OUT-DIR.
OUT-DIR defaults to `target/windows-smoke'.  Returns the generated paths."
  (let ((dir (or out-dir nelisp-windows-build--default-out-dir))
        (paths nil))
    (make-directory dir t)
    (dolist (entry nelisp-windows-build-smoke-specs)
      (let* ((name (symbol-name (car entry)))
             (path (expand-file-name (concat "nelisp-windows-" name ".exe") dir)))
        (push (nelisp-windows-build-exe (car entry) path) paths)))
    (nreverse paths)))

(defun nelisp-windows-build-from-env ()
  "Batch entry for PowerShell scripts.
Reads NELISP_WINDOWS_SPEC and NELISP_WINDOWS_OUT, then writes one EXE."
  (let ((spec (getenv "NELISP_WINDOWS_SPEC"))
        (out (getenv "NELISP_WINDOWS_OUT")))
    (unless (and spec (> (length spec) 0))
      (error "NELISP_WINDOWS_SPEC is required"))
    (unless (and out (> (length out) 0))
      (error "NELISP_WINDOWS_OUT is required"))
    (nelisp-windows-build-exe spec out)))

(defun nelisp-windows-build-exit42 (&optional out-path)
  "Batch entry: build the minimal Windows ExitProcess(42) smoke EXE."
  (let ((out (or out-path nelisp-windows-build--exit42-out)))
    (make-directory (file-name-directory (expand-file-name out)) t)
    (nelisp-pe-write-exe-binary out 'minimal-exit-42)
    (message "nelisp-windows-build: wrote %s (ExitProcess 42)" out)
    out))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
