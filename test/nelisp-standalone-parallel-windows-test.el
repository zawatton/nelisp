;;; nelisp-standalone-parallel-windows-test.el --- tests for Windows parallel build runner  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for tools/build-standalone-parallel.ps1.  These tests do not
;; require Windows; they verify that the PowerShell runner drives the same
;; pure-elisp chunk compiler as the POSIX parallel build script.

;;; Code:

(require 'ert)

(defconst nelisp-standalone-parallel-windows-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for Windows standalone parallel build tests.")

(defun nelisp-standalone-parallel-windows-test--read-file-text (path)
  "Return PATH as plain text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest nelisp-standalone-parallel-windows-script-exists ()
  "The native PowerShell standalone parallel build entry exists."
  (let ((script-path
         (expand-file-name "tools/build-standalone-parallel.ps1"
                           nelisp-standalone-parallel-windows-test--repo-root)))
    (should (file-exists-p script-path))
    (should (file-readable-p script-path))))

(ert-deftest nelisp-standalone-parallel-windows-script-drives-chunks ()
  "The PowerShell runner starts N chunk workers with per-worker environment."
  (let* ((script-path
          (expand-file-name "tools/build-standalone-parallel.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (string-match-p "\\[int\\]\\$Jobs" script))
    (should (string-match-p "Start-Job" script))
    (should (string-match-p "\\$env:NELISP_CHUNK_IDX" script))
    (should (string-match-p "\\$env:NELISP_CHUNK_N" script))
    (should (string-match-p "nelisp-standalone-compile-chunk" script))
    (should (string-match-p "target\\\\standalone-parallel" script))))

(ert-deftest nelisp-standalone-parallel-windows-script-links-serially ()
  "After all chunk workers pass, the PowerShell runner links once serially."
  (let* ((script-path
          (expand-file-name "tools/build-standalone-parallel.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (string-match-p "\\[switch\\]\\$CompileOnly" script))
    (should (string-match-p "\\[parallel\\] linking (serial)" script))
    (should (string-match-p "-f nelisp-standalone-build" script))
    (should (string-match-p "\\[parallel\\] PASS: standalone parallel build completed"
                            script))))

(ert-deftest nelisp-standalone-parallel-windows-script-stays-zero-rust ()
  "The Windows runner must remain an Emacs/pure-elisp build path."
  (let* ((script-path
          (expand-file-name "tools/build-standalone-parallel.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (string-match-p "does not invoke Rust" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-windows-verify-runs-script ()
  "The Windows cross-platform verifier includes the parallel build runner."
  (let* ((script-path
          (expand-file-name "scripts/verify-cross-platform.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (string-match-p "\\$Emacs = \\$env:EMACS" script))
    (should (string-match-p "standalone parallel build (zero-Rust)" script))
    (should (string-match-p "tools\\\\build-standalone-parallel.ps1" script))
    (should (string-match-p "-Jobs \\$ParallelJobs" script))))

(provide 'nelisp-standalone-parallel-windows-test)

;;; nelisp-standalone-parallel-windows-test.el ends here
