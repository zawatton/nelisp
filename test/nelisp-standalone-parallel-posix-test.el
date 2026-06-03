;;; nelisp-standalone-parallel-posix-test.el --- tests for POSIX standalone runners  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for the POSIX standalone parallel and Linux/macOS smoke runners.
;; These tests do not require macOS; they verify that the shell scripts drive the
;; pure-Elisp standalone builder with explicit target selection.

;;; Code:

(require 'ert)

(defconst nelisp-standalone-parallel-posix-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for POSIX standalone runner tests.")

(defun nelisp-standalone-parallel-posix-test--read-file-text (path)
  "Return PATH as plain text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-standalone-parallel-posix-test--count-darwin-os-tests (text)
  "Return the number of ERT tests in TEXT whose names contain darwin."
  (let ((pos 0)
        (count 0))
    (while (string-match "(ert-deftest[ \t\n]+[^ \t\n)]*darwin" text pos)
      (setq count (1+ count))
      (setq pos (match-end 0)))
    count))

(ert-deftest nelisp-standalone-parallel-posix-script-targets-macos ()
  "The POSIX parallel runner accepts an explicit macOS standalone target."
  (let* ((script-path
          (expand-file-name "tools/build-standalone-parallel.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "--target" script))
    (should (string-match-p "macos-aarch64" script))
    (should (string-match-p "NELISP_STANDALONE_TARGET=\"\\$TARGET\"" script))
    (should (string-match-p "NELISP_CHUNK_IDX=\"\\$i\"" script))
    (should (string-match-p "nelisp-standalone-compile-chunk" script))
    (should (string-match-p "nelisp-standalone-build" script))
    (should (string-match-p "\\[parallel\\] PASS: standalone parallel build completed"
                            script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-cache-identity-script ()
  "The macOS cache identity smoke compares fresh and cached Mach-O output."
  (let* ((script-path
          (expand-file-name "tools/macos-standalone-cache-identity-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "NELISP_STANDALONE_TARGET=macos-aarch64" script))
    (should (string-match-p "target/standalone-units/macos-aarch64" script))
    (should (string-match-p "sha256" script))
    (should (string-match-p "\\$FRESH_HASH\" != \"\\$CACHED_HASH" script))
    (should (string-match-p "\\[macos-standalone-cache\\] PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-linux-cache-identity-script ()
  "The Linux cache identity smoke compares fresh and cached ELF output."
  (let* ((script-path
          (expand-file-name "tools/linux-standalone-cache-identity-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "NELISP_STANDALONE_TARGET=linux-x86_64" script))
    (should (string-match-p "target/standalone-units/linux-x86_64" script))
    (should (string-match-p "sha256" script))
    (should (string-match-p "\\$FRESH_HASH\" != \"\\$CACHED_HASH" script))
    (should (string-match-p "\\[linux-standalone-cache\\] PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-linux-reader-script ()
  "The Linux reader smoke uses the short target/nelisp CLI and REPL exit command."
  (let* ((script-path
          (expand-file-name "tools/linux-standalone-reader-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "NELISP_STANDALONE_TARGET=linux-x86_64" script))
    (should (string-match-p "target/nelisp" script))
    (should (string-match-p "file arg with spaces" script))
    (should (string-match-p "unicode file arg" script))
    (should (string-match-p "dump-runtime-image" script))
    (should (string-match-p "eval-runtime-image" script))
    (should (string-match-p "exec-runtime-image missing form" script))
    (should (string-match-p "(exit)" script))
    (should (string-match-p "repl --no-print" script))
    (should (string-match-p "repl bad option" script))
    (should (string-match-p "\\[linux-standalone-reader\\] all PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-linux-selfhost-script ()
  "The Linux self-host runner exposes the same selectable smoke harness shape."
  (let* ((script-path
          (expand-file-name "tools/selfhost-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "--emacs" script))
    (should (string-match-p "--smoke" script))
    (should (string-match-p "--list" script))
    (should (string-match-p "SMOKE_NAMES=(fact)" script))
    (should (string-match-p "target/linux-smoke" script))
    (should (string-match-p "nelisp-linux-selfhost-\\$name" script))
    (should (string-match-p "\\[selfhost\\] PASS: \\$name -> exit 120"
                            script))
    (should (string-match-p "\\[selfhost\\] all PASS - Linux x86_64 self-host smoke OK"
                            script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-macos-reader-script ()
  "The macOS reader smoke exercises the same short CLI and REPL surface."
  (let* ((script-path
          (expand-file-name "tools/macos-standalone-reader-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "NELISP_STANDALONE_TARGET=macos-aarch64" script))
    (should (string-match-p "target/nelisp" script))
    (should (string-match-p "file arg with spaces" script))
    (should (string-match-p "unicode file arg" script))
    (should (string-match-p "dump-runtime-image" script))
    (should (string-match-p "eval-runtime-image" script))
    (should (string-match-p "exec-runtime-image missing form" script))
    (should (string-match-p "(exit)" script))
    (should (string-match-p "repl --no-print" script))
    (should (string-match-p "repl bad option" script))
    (should (string-match-p "\\[macos-standalone-reader\\] all PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-linux-os-compat-script ()
  "The Linux OS compatibility runner exposes the non-Windows stdlib selector."
  (let* ((script-path
          (expand-file-name "tools/linux-os-compat-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (string-match-p "SELECTOR=\"non-windows\"" script))
    (should (string-match-p "EXPECTED_COUNT=12" script))
    (should (string-match-p "EXPECTED_COUNT=318" script))
    (should (string-match-p "test/nelisp-stdlib-os-test.el" script))
    (should (string-match-p "\\[linux-os\\] PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-macos-os-compat-script ()
  "The macOS OS compatibility runner exposes the Darwin stdlib selector."
  (let* ((script-path
          (expand-file-name "tools/macos-os-compat-test.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (test-path
          (expand-file-name "test/nelisp-stdlib-os-test.el"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path))
         (test-text (nelisp-standalone-parallel-posix-test--read-file-text
                     test-path))
         (count (nelisp-standalone-parallel-posix-test--count-darwin-os-tests
                 test-text)))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (should (= count 34))
    (should (string-match-p "NELISP_MACOS_OS_SELECTOR" script))
    (should (string-match-p "test/nelisp-stdlib-os-test.el" script))
    (should (string-match-p "EXPECTED_COUNT=34" script))
    (should (string-match-p "EXPECTED_COUNT=17" script))
    (should (string-match-p "ert-run-tests-batch-and-exit" script))
    (should (string-match-p "\\[macos-os\\] all PASS" script))
    (should-not (string-match-p "\\_<cargo\\_>" script))
    (should-not (string-match-p "\\_<rustc\\_>" script))))

(ert-deftest nelisp-standalone-parallel-posix-macos-verify-runs-parity-gates ()
  "The POSIX cross-platform verifier includes macOS standalone parity gates."
  (let* ((script-path
          (expand-file-name "scripts/verify-cross-platform.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (string-match-p (regexp-quote "[ \"$(uname -s)\" = \"Darwin\" ]")
                            script))
    (should (string-match-p "--parallel-jobs" script))
    (should (string-match-p "--skip-native-smokes" script))
    (should (string-match-p "--include-tarball" script))
    (should (string-match-p "macOS arm64 Mach-O self-host smoke" script))
    (should (string-match-p "tools/macos-selfhost-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "macOS OS compatibility ERT smoke" script))
    (should (string-match-p "tools/macos-os-compat-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "macOS standalone parallel build" script))
    (should (string-match-p "tools/build-standalone-parallel.sh --jobs \"\\$PARALLEL_JOBS\" --target macos-aarch64"
                            script))
    (should (string-match-p "tools/build-standalone-parallel.sh --jobs \"\\$PARALLEL_JOBS\" --target macos-aarch64 --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "macOS standalone eval native smoke" script))
    (should (string-match-p "tools/macos-standalone-eval-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "macOS standalone cache identity smoke" script))
    (should (string-match-p "tools/macos-standalone-cache-identity-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "macOS standalone reader native smoke" script))
    (should (string-match-p "tools/macos-standalone-reader-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "macOS standalone tarball smoke" script))
    (should (string-match-p "tools/build-standalone-tarball.sh stage-d-v3.0 macos-aarch64 --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "tools/verify-standalone-tarball.sh stage-d-v3.0 macos-aarch64"
                            script))))

(ert-deftest nelisp-standalone-parallel-posix-linux-verify-runs-parity-gates ()
  "The POSIX cross-platform verifier includes Linux standalone parity gates."
  (let* ((script-path
          (expand-file-name "scripts/verify-cross-platform.sh"
                            nelisp-standalone-parallel-posix-test--repo-root))
         (script (nelisp-standalone-parallel-posix-test--read-file-text
                  script-path)))
    (should (string-match-p "--parallel-jobs" script))
    (should (string-match-p "--skip-native-smokes" script))
    (should (string-match-p "--include-tarball" script))
    (should (string-match-p "Linux OS compatibility ERT smoke" script))
    (should (string-match-p "tools/linux-os-compat-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "Linux x86_64 ELF self-host smoke" script))
    (should (string-match-p "tools/selfhost-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "Linux standalone parallel build" script))
    (should (string-match-p "tools/build-standalone-parallel.sh --jobs \"\\$PARALLEL_JOBS\" --target linux-x86_64"
                            script))
    (should (string-match-p "tools/build-standalone-parallel.sh --jobs \"\\$PARALLEL_JOBS\" --target linux-x86_64 --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "Linux standalone eval native smoke" script))
    (should (string-match-p "tools/linux-standalone-eval-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "Linux standalone cache identity smoke" script))
    (should (string-match-p "tools/linux-standalone-cache-identity-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "Linux standalone reader native smoke" script))
    (should (string-match-p "tools/linux-standalone-reader-test.sh --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "Linux standalone tarball smoke" script))
    (should (string-match-p "tools/build-standalone-tarball.sh stage-d-v3.0 linux-x86_64 --emacs \"\\$EMACS\""
                            script))
    (should (string-match-p "tools/verify-standalone-tarball.sh stage-d-v3.0 linux-x86_64"
                            script))))

(provide 'nelisp-standalone-parallel-posix-test)

;;; nelisp-standalone-parallel-posix-test.el ends here
