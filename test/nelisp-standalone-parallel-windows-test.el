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
    (should (string-match-p "\\[string\\]\\$Target = \"windows-x86_64\"" script))
    (should (string-match-p "Start-Job" script))
    (should (string-match-p "\\$env:NELISP_CHUNK_IDX" script))
    (should (string-match-p "\\$env:NELISP_CHUNK_N" script))
    (should (string-match-p "\\$env:NELISP_STANDALONE_TARGET" script))
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
    (should (string-match-p "\\[parallel\\] target" script))
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

(ert-deftest nelisp-standalone-parallel-windows-script-hides-result-objects ()
  "Worker result objects are used for status but not printed as user output."
  (let* ((script-path
          (expand-file-name "tools/build-standalone-parallel.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (string-match-p "\\$ResultCandidates = @(\\$Output | Where-Object"
                            script))
    (should (string-match-p "-not (\\$_ -is \\[pscustomobject\\]" script))
    (should-not (string-match-p
                 (regexp-quote "$Output | ForEach-Object { Write-Host $_ }")
                 script))))

(ert-deftest nelisp-standalone-parallel-windows-verify-runs-script ()
  "The Windows cross-platform verifier includes the parallel build runner."
  (let* ((script-path
          (expand-file-name "scripts/verify-cross-platform.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (string-match-p "\\[string\\]\\$Emacs = \\$env:EMACS" script))
    (should (string-match-p "function Invoke-Checked" script))
    (should (string-match-p "Windows OS compatibility ERT smoke" script))
    (should (string-match-p "windows-os-compat-test.ps1" script))
    (should (string-match-p "Windows x86_64 PE32\\+ self-host smoke" script))
    (should (string-match-p "windows-selfhost-test.ps1" script))
    (should (string-match-p "standalone parallel build (zero-Rust)" script))
    (should (string-match-p "tools\\\\build-standalone-parallel.ps1" script))
    (should (string-match-p "-Jobs \\$ParallelJobs" script))
    (should (string-match-p "Windows standalone eval native smoke" script))
    (should (string-match-p "windows-standalone-eval-test.ps1" script))
    (should (string-match-p "Windows standalone cache identity smoke" script))
    (should (string-match-p "windows-standalone-cache-identity-test.ps1" script))
    (should (string-match-p "Windows standalone reader native smoke" script))
    (should (string-match-p "windows-standalone-reader-test.ps1" script))
    (should (string-match-p "\\[switch\\]\\$IncludeTarball" script))
    (should (string-match-p "Windows standalone tarball smoke" script))
    (should (string-match-p "build-standalone-tarball.ps1" script))
    (should (string-match-p "verify-standalone-tarball.ps1" script))
    (should (string-match-p "function Invoke-WindowsStandaloneInstallSmoke" script))
    (should (string-match-p "Windows standalone installer smoke" script))
    (should (string-match-p "release\\\\stage-d-v3.0\\\\install-v3.ps1" script))
    (should (string-match-p "installed bin\\\\nelisp.exe eval -> 42" script))))

(ert-deftest nelisp-standalone-parallel-windows-eval-smoke-script ()
  "The Windows native standalone eval smoke builds and executes the PE EXE."
  (let* ((script-path
          (expand-file-name "tools/windows-standalone-eval-test.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (string-match-p "\\$env:NELISP_STANDALONE_TARGET = \"windows-x86_64\""
                            script))
    (should (string-match-p "nelisp-standalone-build" script))
    (should (string-match-p "target\\\\nelisp-standalone-eval.exe" script))
    (should (string-match-p "\\[windows-standalone-eval\\] PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-windows-reader-smoke-script ()
  "The Windows native standalone reader smoke builds and executes the PE EXE."
  (let* ((script-path
          (expand-file-name "tools/windows-standalone-reader-test.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (string-match-p "\\$env:NELISP_STANDALONE_TARGET = \"windows-x86_64\""
                            script))
    (should (string-match-p "\\$env:NELISP_SRC = \\$Source" script))
    (should (string-match-p "nelisp-standalone-build-reader" script))
    (should (string-match-p "target\\\\nelisp.exe" script))
    (should (string-match-p "\\[windows-standalone-reader\\] PASS" script))
    (should (string-match-p "Invoke-ReaderFileSmoke" script))
    (should (string-match-p "file arg with spaces" script))
    (should (string-match-p "unicode file arg" script))
    (should (string-match-p "\\$Exe --help" script))
    (should (string-match-p "\\$Exe eval" script))
    (should (string-match-p "\\$Exe -e" script))
    (should (string-match-p "\\$Exe load" script))
    (should (string-match-p "\\$Exe dump-runtime-image" script))
    (should (string-match-p "\\$Exe eval-runtime-image" script))
    (should (string-match-p "\\$Exe exec-runtime-image" script))
    (should (string-match-p "function Invoke-ReaderWithInput" script))
    (should (string-match-p "StandardInputEncoding = \\[System.Text.Encoding\\]::ASCII"
                            script))
    (should (string-match-p "Arguments @(\"repl\", \"--no-prompt\")" script))
    (should (string-match-p "Arguments @(\"repl\", \"--no-prompt\", \"--no-print\")"
                            script))
    (should (string-match-p "\\$Exe repl --bad" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-windows-cache-identity-script ()
  "The cache identity smoke compares fresh and cached PE output hashes."
  (let* ((script-path
          (expand-file-name "tools/windows-standalone-cache-identity-test.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (script (nelisp-standalone-parallel-windows-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (string-match-p "\\$env:NELISP_STANDALONE_TARGET = \"windows-x86_64\""
                            script))
    (should (string-match-p "windows-x86_64-arena-\\*" script))
    (should (string-match-p "Get-FileHash -Algorithm SHA256" script))
    (should (string-match-p "\\$FreshHash -ne \\$CachedHash" script))
    (should (string-match-p "\\[windows-standalone-cache\\] PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-windows-tarball-scripts ()
  "The Windows tarball scripts build and verify the short nelisp.exe CLI."
  (let* ((build-path
          (expand-file-name "tools/build-standalone-tarball.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (verify-path
          (expand-file-name "tools/verify-standalone-tarball.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (install-path
          (expand-file-name "release/stage-d-v3.0/install-v3.ps1"
                            nelisp-standalone-parallel-windows-test--repo-root))
         (build (nelisp-standalone-parallel-windows-test--read-file-text
                 build-path))
         (verify (nelisp-standalone-parallel-windows-test--read-file-text
                  verify-path))
         (install (nelisp-standalone-parallel-windows-test--read-file-text
                   install-path)))
    (should (file-exists-p build-path))
    (should (file-exists-p verify-path))
    (should (file-exists-p install-path))
    (should (string-match-p "\\[string\\]\\$Target = \"windows-x86_64\"" build))
    (should (string-match-p "\\$env:NELISP_STANDALONE_TARGET = \\$Target" build))
    (should (string-match-p "nelisp-standalone-build-reader" build))
    (should (string-match-p "bin\\\\nelisp.exe" build))
    (should (string-match-p "Get-FileHash -Algorithm SHA256" build))
    (should (string-match-p "tar -czf" build))
    (should (string-match-p "15 MB cap" build))
    (should (string-match-p "\\[string\\]\\$Target = \"windows-x86_64\"" verify))
    (should (string-match-p "tar -xzf" verify))
    (should (string-match-p "bin\\\\nelisp.exe eval" verify))
    (should (string-match-p "repl --no-prompt" verify))
    (should (string-match-p "Windows standalone tarball OK" verify))
    (should (string-match-p "\\[string\\]\\$Target = \"windows-x86_64\"" install))
    (should (string-match-p "Get-FileHash -Algorithm SHA256" install))
    (should (string-match-p "tar -xzf \\$TarPath -C \\$Prefix --strip-components=1" install))
    (should (string-match-p "installed bin\\\\nelisp.exe missing" install))
    (should (string-match-p "anvil-\" \\+ \\$Version" install))
    (should-not (string-match-p "\\_<cargo\\_>" build))
    (should-not (string-match-p "\\_<rustc\\_>" build))
    (should-not (string-match-p "\\_<cargo\\_>" verify))
    (should-not (string-match-p "\\_<rustc\\_>" verify))
    (should-not (string-match-p "\\_<cargo\\_>" install))
    (should-not (string-match-p "\\_<rustc\\_>" install))))

(provide 'nelisp-standalone-parallel-windows-test)

;;; nelisp-standalone-parallel-windows-test.el ends here
