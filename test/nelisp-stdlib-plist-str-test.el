;;; nelisp-stdlib-plist-str-test.el --- Doc 86 §86.1.e Tier 2 simple  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 86 §86.1.e (2026-05-10) — IEEE-754 byte-exact verification of
;; the `format' float-conversion body builder migrated to elisp on
;; top of the new `nl_jit_format_float' trampoline + parity smoke-
;; tests for `concat' / `make-string' (= the two simpler trampolines
;; that share `:trampoline-unary' shape).
;;
;; Subprocess pattern: same as `test/nelisp-stdlib-prn-test.el'.  We
;; invoke `target/release/nelisp eval EXPR' to exercise the actual
;; binary's bake-image bootstrap path; the host-Emacs ERT runner only
;; collects exit codes + stdout, so we can probe NeLisp internal
;; functions (= `nelisp--format-float-body') that have no host-side
;; counterpart.
;;
;; Strategy (= per Doc 87 §3.5 risk note): `nelisp--format-float-body'
;; must produce the exact same bytes as the Rust
;; `format!("{:.*}", PREC, X)' emit it replaced.  Fixtures below probe
;; IEEE-754 round-half-to-even / scientific notation / %g shortest-
;; form selection across the boundaries the pre-§86.1.e Rust impl
;; covered.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-plist-str-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir)))
  "Absolute path to the dev/nelisp worktree, or nil when undeterminable.")

(defconst nelisp-stdlib-plist-str-test--bin
  (and nelisp-stdlib-plist-str-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-plist-str-test--repo-root))
  "Path to the build-tool `nelisp' binary.")

(defun nelisp-stdlib-plist-str-test--skip-unless-built ()
  (unless (and nelisp-stdlib-plist-str-test--bin
               (file-executable-p nelisp-stdlib-plist-str-test--bin))
    (ert-skip
     (format "build-tool nelisp binary missing — run `cargo build --release --bin nelisp' (looked at %s)"
             nelisp-stdlib-plist-str-test--bin))))

(defun nelisp-stdlib-plist-str-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-plist-str-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(defun nelisp-stdlib-plist-str-test--princ (expr-string)
  "Return the trimmed stdout of `(princ EXPR)' for EXPR-STRING.
Wraps in `(progn ... nil)' so the eval-mode's auto-print of the
return value is the literal `nil', which we strip from the suffix
along with the trailing newline."
  (let* ((wrapper (format "(progn (princ %s) nil)" expr-string))
         (r (nelisp-stdlib-plist-str-test--eval wrapper)))
    (should (eq (car r) 0))
    (let ((out (string-trim-right (cdr r))))
      (if (string-suffix-p "nil" out)
          (substring out 0 (- (length out) 3))
        out))))

;; --- `concat' parity (= flat int list → string) -----------------

(ert-deftest nelisp-stdlib-format-concat-empty ()
  "Empty arg list returns empty string."
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ "(concat)") "")))

(ert-deftest nelisp-stdlib-format-concat-strings ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(concat \"a\" \"b\" \"c\")") "abc"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(concat \"\" \"x\" \"\")") "x"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(concat \"ab\" nil \"cd\")") "abcd")))

(ert-deftest nelisp-stdlib-format-concat-int-list ()
  "Lists of int codepoints are appended as chars."
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(concat (list ?a ?b ?c))") "abc"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(concat \"x\" (list ?y ?z))") "xyz")))

;; --- `make-string' parity --------------------------------------

(ert-deftest nelisp-stdlib-format-make-string-zero ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(make-string 0 ?x)") "")))

(ert-deftest nelisp-stdlib-format-make-string-repeats ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(make-string 3 ?A)") "AAA"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(make-string 5 ?\\s)") "     ")))

(ert-deftest nelisp-stdlib-format-make-string-rejects-negative ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (let* ((r (nelisp-stdlib-plist-str-test--eval
             "(condition-case err (progn (make-string -1 ?a) :ok) (error :err))")))
    (should (eq (car r) 0))
    (should (string-match-p ":err" (cdr r)))))

;; --- `nelisp--format-float-body' IEEE-754 ---------------------

(ert-deftest nelisp-stdlib-format-float-f-default-prec ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?f 6 1.5)") "1.500000"))
  ;; PREC 0 with 1.5 yields the unsigned, undecorated body.  Rust's
  ;; `format!("{:.0}", 1.5)' is "2" (= round-half-to-even).
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?f 0 1.5)") "2"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?f 2 0.0)") "0.00")))

(ert-deftest nelisp-stdlib-format-float-e ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?e 2 12345.0)") "1.23e4"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?E 2 12345.0)") "1.23E4")))

(ert-deftest nelisp-stdlib-format-float-via-format ()
  ;; End-to-end: `format' should dispatch to `nelisp--format-float-body'
  ;; and produce the same bytes as the pre-§86.1.e Rust impl.
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(format \"%.2f\" 3.14159)") "3.14"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(format \"%.3e\" 1.5e3)") "1.500e3")))

(ert-deftest nelisp-stdlib-format-float-int-promotion ()
  ;; Integer x is promoted to f64 in the elisp wrapper before the
  ;; trampoline call.
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?f 2 5)") "5.00"))
  (should (string= (nelisp-stdlib-plist-str-test--princ
                    "(nelisp--format-float-body ?f 0 7)") "7")))

(ert-deftest nelisp-stdlib-format-float-rejects-bad-types ()
  (nelisp-stdlib-plist-str-test--skip-unless-built)
  (let* ((wrapper "(condition-case err
                     (progn (nelisp--format-float-body ?f -1 1.5) :ok)
                     (error :err))")
         (r (nelisp-stdlib-plist-str-test--eval wrapper)))
    (should (eq (car r) 0))
    (should (string-match-p ":err" (cdr r)))))

(provide 'nelisp-stdlib-plist-str-test)

;;; nelisp-stdlib-plist-str-test.el ends here
