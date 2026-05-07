;;; nelisp-stdlib-prn-test.el --- ERT tests for elisp Sexp printer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7 Stage 7.1.2 (Doc 64) — exercise the bundled
;; `lisp/nelisp-stdlib-prn.el' Sexp printer / serializer through the
;; real `target/release/nelisp' binary.
;;
;; Subprocess pattern: same as `test/nelisp-stdlib-os-test.el'.
;; `(princ (prin1-to-string OBJ))' funnels the readable form into
;; stdout; we then string-match.  This bypasses the host-Emacs eval
;; simulator in `src/nelisp-eval.el' (which would delegate
;; `prin1-to-string' to host Emacs and not exercise our impl).

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-prn-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir)))
  "Absolute path to the dev/nelisp worktree, or nil when undeterminable.")

(defconst nelisp-stdlib-prn-test--bin
  (and nelisp-stdlib-prn-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-prn-test--repo-root))
  "Path to the build-tool `nelisp' binary.")

(defun nelisp-stdlib-prn-test--skip-unless-built ()
  (unless (and nelisp-stdlib-prn-test--bin
               (file-executable-p nelisp-stdlib-prn-test--bin))
    (ert-skip
     (format "build-tool nelisp binary missing — run `cargo build --release --bin nelisp' (looked at %s)"
             nelisp-stdlib-prn-test--bin))))

(defun nelisp-stdlib-prn-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-prn-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(defun nelisp-stdlib-prn-test--prin1 (expr-string)
  "Return stdout of `(princ (prin1-to-string EXPR))' for EXPR-STRING.
Wraps in `(progn ... nil)' so the eval-mode's auto-print of the return
value is the literal `nil', which we then strip from the suffix along
with the trailing newline."
  (let* ((wrapper (format "(progn (princ (prin1-to-string %s)) nil)"
                          expr-string))
         (r (nelisp-stdlib-prn-test--eval wrapper)))
    (should (eq (car r) 0))
    (let ((out (string-trim-right (cdr r))))
      ;; `(progn ... nil)' returns nil → binary auto-prints `nil\n'.
      (if (string-suffix-p "nil" out)
          (substring out 0 (- (length out) 3))
        out))))

;;; Atoms ------------------------------------------------------------

(ert-deftest nelisp-prn-binary/nil ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "nil") "nil")))

(ert-deftest nelisp-prn-binary/t ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "t") "t")))

(ert-deftest nelisp-prn-binary/int ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "42") "42"))
  (should (string= (nelisp-stdlib-prn-test--prin1 "0") "0"))
  (should (string= (nelisp-stdlib-prn-test--prin1 "-7") "-7")))

(ert-deftest nelisp-prn-binary/float-keeps-dot ()
  ;; `1.0' must print as `1.0' (= round-trip safe), not `1' (which
  ;; would re-read as Int).  Matches the prior Rust Display behaviour.
  (nelisp-stdlib-prn-test--skip-unless-built)
  (let ((s (nelisp-stdlib-prn-test--prin1 "1.0")))
    (should (or (string-match-p "[.eE]" s)
                (string= s "inf") (string= s "-inf") (string= s "NaN")))))

(ert-deftest nelisp-prn-binary/float-decimal ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "1.5") "1.5")))

(ert-deftest nelisp-prn-binary/symbol ()
  ;; `(quote hello)' evaluates to the bare SYMBOL `hello', so
  ;; prin1 prints `hello' (no abbreviation prefix — that path lives
  ;; in `nelisp-prn-binary/quote-abbrev', which feeds the cons
  ;; `(quote foo)' as data via `(list (quote quote) (quote foo))').
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "(quote hello)") "hello"))
  (should (string= (nelisp-stdlib-prn-test--prin1 "(quote a-b-c)") "a-b-c")))

;;; Strings ----------------------------------------------------------

(ert-deftest nelisp-prn-binary/string-readable ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "\"abc\"") "\"abc\"")))

(ert-deftest nelisp-prn-binary/string-empty ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "\"\"") "\"\"")))

(ert-deftest nelisp-prn-binary/string-escape-newline ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "\"a\\nb\"") "\"a\\nb\"")))

(ert-deftest nelisp-prn-binary/string-escape-tab ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "\"a\\tb\"") "\"a\\tb\"")))

(ert-deftest nelisp-prn-binary/string-escape-backslash ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "\"a\\\\b\"") "\"a\\\\b\"")))

;;; Cons / lists -----------------------------------------------------

(ert-deftest nelisp-prn-binary/cons-proper ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "(list 1 2 3)") "(1 2 3)")))

(ert-deftest nelisp-prn-binary/cons-dotted ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "(cons 1 2)") "(1 . 2)")))

(ert-deftest nelisp-prn-binary/cons-nested ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1
                    "(list (list (quote a)) (quote b) (list (quote c) (quote d)))")
                   "((a) b (c d))")))

;;; Reader-macro abbrev ----------------------------------------------

(ert-deftest nelisp-prn-binary/quote-abbrev ()
  ;; `'foo' = `(quote foo)' on the data side; printer must abbreviate.
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1
                    "(list (quote quote) (quote foo))")
                   "'foo")))

(ert-deftest nelisp-prn-binary/function-abbrev ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1
                    "(list (quote function) (quote foo))")
                   "#'foo")))

(ert-deftest nelisp-prn-binary/backquote-abbrev ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1
                    "(list (quote backquote) (quote foo))")
                   "`foo")))

;;; Vectors ----------------------------------------------------------

(ert-deftest nelisp-prn-binary/vector ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "(vector 1 2 3)") "[1 2 3]")))

(ert-deftest nelisp-prn-binary/vector-empty ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (should (string= (nelisp-stdlib-prn-test--prin1 "(vector)") "[]")))

;;; format %S routes through prin1-to-string ------------------------

(ert-deftest nelisp-prn-binary/format-spec-S ()
  (nelisp-stdlib-prn-test--skip-unless-built)
  (let ((r (nelisp-stdlib-prn-test--eval
            "(princ (format \"%S/%S/%S\" \"abc\" (quote sym) (list 1 2)))")))
    (should (eq (car r) 0))
    (should (string-match-p "\"abc\"/sym/(1 2)" (cdr r)))))

(provide 'nelisp-stdlib-prn-test)

;;; nelisp-stdlib-prn-test.el ends here
