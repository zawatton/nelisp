;;; nelisp-standalone-parallel-posix-test.el --- tests for POSIX standalone runners  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for the POSIX standalone parallel and macOS smoke runners.
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
    (should (= count 13))
    (should (string-match-p "NELISP_MACOS_OS_SELECTOR" script))
    (should (string-match-p "test/nelisp-stdlib-os-test.el" script))
    (should (string-match-p "EXPECTED_COUNT=13" script))
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
    (should (string-match-p "macOS arm64 Mach-O self-host smoke" script))
    (should (string-match-p "tools/macos-selfhost-test.sh" script))
    (should (string-match-p "macOS OS compatibility ERT smoke" script))
    (should (string-match-p "tools/macos-os-compat-test.sh" script))
    (should (string-match-p "macOS standalone parallel build" script))
    (should (string-match-p "tools/build-standalone-parallel.sh --jobs 2 --target macos-aarch64"
                            script))
    (should (string-match-p "macOS standalone eval native smoke" script))
    (should (string-match-p "tools/macos-standalone-eval-test.sh" script))
    (should (string-match-p "macOS standalone cache identity smoke" script))
    (should (string-match-p "tools/macos-standalone-cache-identity-test.sh" script))
    (should (string-match-p "macOS standalone reader native smoke" script))
    (should (string-match-p "tools/macos-standalone-reader-test.sh" script))))

(provide 'nelisp-standalone-parallel-posix-test)

;;; nelisp-standalone-parallel-posix-test.el ends here
