;;; nelisp-stdlib-os-test.el --- ERT for Doc 53 Phase 1 OS surface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 53 Phase 1 — POSIX OS surface (Minimal-5: open / read / write /
;; close / exit) end-to-end through the real `bin/nelisp' binary.
;;
;; Tests shell out to `target/release/nelisp eval EXPR' (= same pattern
;; as `test/nelisp-runtime-test.el') because the host-Emacs simulator
;; in `src/nelisp-eval.el' does not register `nelisp--syscall*'
;; primitives — only the Rust evaluator does, and that is what Phase 1
;; verifies.  Tests skip when the binary is missing so a `make test'
;; without prior `cargo build --release --bin nelisp' degrades quietly.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-os-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir)))
  "Absolute path to the dev/nelisp worktree, or nil when undeterminable.")

(defconst nelisp-stdlib-os-test--bin
  (and nelisp-stdlib-os-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-os-test--repo-root))
  "Path to the build-tool `nelisp' binary built by `cargo build --release --bin nelisp'.")

(defun nelisp-stdlib-os-test--skip-unless-built ()
  "Skip the current ERT unless the build-tool nelisp binary is present."
  (unless (and nelisp-stdlib-os-test--bin
               (file-executable-p nelisp-stdlib-os-test--bin))
    (ert-skip
     (format "build-tool nelisp binary missing — run `cargo build --release --bin nelisp' (looked at %s)"
             nelisp-stdlib-os-test--bin))))

(defun nelisp-stdlib-os-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-os-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

;;; Tests --------------------------------------------------------------

(ert-deftest nelisp-stdlib-os-test/supported-on-linux ()
  "On Linux, `nelisp--syscall-supported-p' must report t (= Path A enabled)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval "(nelisp--syscall-supported-p)")))
    (should (eq (car r) 0))
    (should (string-match-p "^t" (cdr r)))))

(ert-deftest nelisp-stdlib-os-test/round-trip-write-then-read ()
  "open(O_WRONLY|O_CREAT|O_TRUNC) → write → close → open(O_RDONLY) → read → close
preserves bytes verbatim through `nelisp-os-*' wrappers."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((tmp (make-temp-file "nelisp-os-test-"))
         (expr (format "(progn
  (require (quote nelisp-stdlib-os))
  (let ((wfd (nelisp-os-open %S
               (logior nelisp-os-O-WRONLY nelisp-os-O-CREAT nelisp-os-O-TRUNC)
               420)))
    (nelisp-os-write wfd \"hello\\n\")
    (nelisp-os-close wfd))
  (let* ((rfd (nelisp-os-open %S nelisp-os-O-RDONLY 0))
         (data (nelisp-os-read rfd 1024)))
    (nelisp-os-close rfd)
    (nelisp-os-write 1 data)
    (nelisp-os-exit 0)))"
                       tmp tmp)))
    (unwind-protect
        (let ((r (nelisp-stdlib-os-test--eval expr)))
          (should (eq (car r) 0))
          (should (equal (cdr r) "hello\n")))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-stdlib-os-test/open-nonexistent-signals ()
  "Opening a non-existent path with O_RDONLY signals `nelisp-os-error'
(non-zero exit from the subprocess + error text on stderr/stdout)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn
              (require (quote nelisp-stdlib-os))
              (nelisp-os-open \"/tmp/nelisp-os-does-not-exist-zzz\"
                              nelisp-os-O-RDONLY 0))")))
    (should-not (eq (car r) 0))
    (should (string-match-p "nelisp-os-error" (cdr r)))))

(ert-deftest nelisp-stdlib-os-test/exit-with-explicit-code ()
  "`nelisp-os-exit 42' must propagate exit code 42 through exit_group(2)."
  (nelisp-stdlib-os-test--skip-unless-built)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((r (nelisp-stdlib-os-test--eval
            "(progn (require (quote nelisp-stdlib-os)) (nelisp-os-exit 42))")))
    (should (eq (car r) 42))))

(provide 'nelisp-stdlib-os-test)

;;; nelisp-stdlib-os-test.el ends here
