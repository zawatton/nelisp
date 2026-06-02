;;; nelisp-windows-build.el --- native Windows x86_64 smoke build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Builds native Windows x86_64 PE32+ smoke executables from the pure-elisp
;; PE writer.  These are deliberately small de-risk artifacts for Doc 138:
;; ExitProcess, VirtualAlloc, VirtualProtect / VirtualFree, arena metadata,
;; stdout HANDLE I/O via GetStdHandle + WriteFile, and CRT-free command-line
;; discovery via GetCommandLineW / CommandLineToArgvW, and Winsock startup via
;; WS2_32.dll!WSAStartup, plus CreateProcessW child launch / wait and
;; CreateThread launch / join.
;;
;; Run on a Windows machine with Emacs installed via:
;;
;;   .\tools\windows-selfhost-test.ps1
;;
;; Or build a single smoke EXE with one of:
;;
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-exit42
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-virtualalloc
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-virtualprotect-free
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-arena
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-writefile-stdout
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-getcommandline
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-commandlinetoargv
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-wsastartup
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-createprocess
;;   emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build -f nelisp-windows-build-createthread
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

(defconst nelisp-windows-build--virtualalloc-out
  (expand-file-name "target/nelisp-windows-virtualalloc.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single VirtualAlloc smoke EXE.")

(defconst nelisp-windows-build--virtualprotect-free-out
  (expand-file-name "target/nelisp-windows-virtualprotect-free.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single VirtualProtect / VirtualFree smoke EXE.")

(defconst nelisp-windows-build--arena-out
  (expand-file-name "target/nelisp-windows-arena.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single VirtualAlloc arena smoke EXE.")

(defconst nelisp-windows-build--writefile-stdout-out
  (expand-file-name "target/nelisp-windows-writefile-stdout.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single WriteFile stdout smoke EXE.")

(defconst nelisp-windows-build--getcommandline-out
  (expand-file-name "target/nelisp-windows-getcommandline.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single GetCommandLineW smoke EXE.")

(defconst nelisp-windows-build--wsastartup-out
  (expand-file-name "target/nelisp-windows-wsastartup.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single WSAStartup smoke EXE.")

(defconst nelisp-windows-build--commandlinetoargv-out
  (expand-file-name "target/nelisp-windows-commandlinetoargv.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single CommandLineToArgvW smoke EXE.")

(defconst nelisp-windows-build--createprocess-out
  (expand-file-name "target/nelisp-windows-createprocess.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single CreateProcessW smoke EXE.")

(defconst nelisp-windows-build--createthread-out
  (expand-file-name "target/nelisp-windows-createthread.exe"
                    nelisp-windows-build--repo-root)
  "Default path for the single CreateThread smoke EXE.")

(defconst nelisp-windows-build-smoke-specs
  '((exit42 . minimal-exit-42)
    (virtualalloc . virtualalloc-exit-42)
    (virtualprotect-free . virtualprotect-free-exit-42)
    (arena . virtualalloc-arena-exit-42)
    (writefile-stdout . writefile-stdout-exit-42)
    (getcommandline . getcommandline-exit-42)
    (commandlinetoargv . commandlinetoargv-exit-42)
    (wsastartup . wsastartup-exit-42)
    (createprocess . createprocess-wait-exit-42)
    (createthread . createthread-wait-exit-42))
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

(defun nelisp-windows-build--batch-smoke (spec out-path label)
  "Build one batch smoke SPEC to OUT-PATH and describe it with LABEL."
  (make-directory (file-name-directory (expand-file-name out-path)) t)
  (nelisp-pe-write-exe-binary out-path spec)
  (message "nelisp-windows-build: wrote %s (%s)" out-path label)
  out-path)

(defun nelisp-windows-build-exit42 (&optional out-path)
  "Batch entry: build the minimal Windows ExitProcess(42) smoke EXE."
  (nelisp-windows-build--batch-smoke
   'minimal-exit-42
   (or out-path nelisp-windows-build--exit42-out)
   "ExitProcess 42"))

(defun nelisp-windows-build-virtualalloc (&optional out-path)
  "Batch entry: build the Windows VirtualAlloc smoke EXE."
  (nelisp-windows-build--batch-smoke
   'virtualalloc-exit-42
   (or out-path nelisp-windows-build--virtualalloc-out)
   "VirtualAlloc + ExitProcess 42"))

(defun nelisp-windows-build-virtualprotect-free (&optional out-path)
  "Batch entry: build the Windows VirtualProtect / VirtualFree smoke EXE."
  (nelisp-windows-build--batch-smoke
   'virtualprotect-free-exit-42
   (or out-path nelisp-windows-build--virtualprotect-free-out)
   "VirtualAlloc + VirtualProtect + VirtualFree + ExitProcess 42"))

(defun nelisp-windows-build-arena (&optional out-path)
  "Batch entry: build the Windows VirtualAlloc arena smoke EXE."
  (nelisp-windows-build--batch-smoke
   'virtualalloc-arena-exit-42
   (or out-path nelisp-windows-build--arena-out)
   "VirtualAlloc arena metadata + ExitProcess 42"))

(defun nelisp-windows-build-writefile-stdout (&optional out-path)
  "Batch entry: build the Windows WriteFile stdout smoke EXE."
  (nelisp-windows-build--batch-smoke
   'writefile-stdout-exit-42
   (or out-path nelisp-windows-build--writefile-stdout-out)
   "GetStdHandle + WriteFile stdout + ExitProcess 42"))

(defun nelisp-windows-build-getcommandline (&optional out-path)
  "Batch entry: build the Windows GetCommandLineW smoke EXE."
  (nelisp-windows-build--batch-smoke
   'getcommandline-exit-42
   (or out-path nelisp-windows-build--getcommandline-out)
   "GetCommandLineW + ExitProcess 42"))

(defun nelisp-windows-build-wsastartup (&optional out-path)
  "Batch entry: build the Windows WSAStartup smoke EXE."
  (nelisp-windows-build--batch-smoke
   'wsastartup-exit-42
   (or out-path nelisp-windows-build--wsastartup-out)
   "WSAStartup + ExitProcess 42"))

(defun nelisp-windows-build-commandlinetoargv (&optional out-path)
  "Batch entry: build the Windows CommandLineToArgvW smoke EXE."
  (nelisp-windows-build--batch-smoke
   'commandlinetoargv-exit-42
   (or out-path nelisp-windows-build--commandlinetoargv-out)
   "GetCommandLineW + CommandLineToArgvW + ExitProcess 42"))

(defun nelisp-windows-build-createprocess (&optional out-path)
  "Batch entry: build the Windows CreateProcessW smoke EXE."
  (nelisp-windows-build--batch-smoke
   'createprocess-wait-exit-42
   (or out-path nelisp-windows-build--createprocess-out)
   "CreateProcessW + wait child exit 42"))

(defun nelisp-windows-build-createthread (&optional out-path)
  "Batch entry: build the Windows CreateThread smoke EXE."
  (nelisp-windows-build--batch-smoke
   'createthread-wait-exit-42
   (or out-path nelisp-windows-build--createthread-out)
   "CreateThread + join thread exit 42"))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
