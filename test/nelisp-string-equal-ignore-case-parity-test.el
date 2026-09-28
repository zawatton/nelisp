;;; nelisp-string-equal-ignore-case-parity-test.el --- compare-strings type/bounds parity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `string-equal-ignore-case' (vendor/staged-emacs-lisp/subr.el) is a thin
;; wrapper around `compare-strings', so the type/bounds errors it surfaces
;; come entirely from the primitive.  `(string-equal-ignore-case "e" '(1 2
;; . 3))' answered (wrong-type-argument stringp (1 2 . 3)) on host Emacs but
;; (wrong-type-argument listp 3) on the standalone (found via the
;; emacs-parity corpus, test/nelisp-shadow-differential-cases.el), because
;; the standalone's `compare-strings' (scripts/nelisp-stdlib-prelude.el) did
;; no argument checking of its own and went straight to `length'/`aref' on
;; STR1/STR2, so whatever `length' signalled for a bad STR2 leaked through
;; instead of Emacs's own CHECK_STRING (Emacs 31.1 src/fns.c
;; Fcompare_strings).  Fixed 2026-09-28 by adding the same checks Emacs's C
;; primitive makes, in the same order: STR1 stringp, then STR2 stringp,
;; then STR1's START1/END1 (type + `args-out-of-range', matching
;; `validate_subarray', same file) fully before STR2's START2/END2 is even
;; looked at.  IGNORE-CASE also switched from `downcase' to `upcase' to
;; match Fcompare_strings.
;;
;; Run this file with BOTH host Emacs and the standalone (`--load') and
;; diff the single printed line -- same convention as
;; test/nelisp-file-attributes-parity-test.el and the emacs-parity corpus.
;; No fixture directory or environment variable is needed; every case here
;; is a literal.

;;; Code:

(princ
 (format "%S\n"
         (list
          ;; The reported divergence: STR2 is an improper list.  Emacs
          ;; checks STR1 (a real string, fine) then STR2 -- CHECK_STRING
          ;; fails on STR2 itself, not on whatever `length' would make of
          ;; it.
          (condition-case e (string-equal-ignore-case "e" '(1 2 . 3)) (error e))
          ;; STR1 itself is not a string: Emacs checks STR1 before STR2,
          ;; so the error names STR1 even though STR2 is also bad.
          (condition-case e (compare-strings 1 0 nil '(9) 0 nil) (error e))
          ;; STR1 is fine, STR2 is not: this is the case that leaked
          ;; `length''s own error before the fix.
          (condition-case e (compare-strings "e" 0 nil '(1 2 . 3) 0 nil) (error e))
          ;; A non-integer, non-nil START/END signals wrong-type-argument
          ;; integerp, not whatever the bad value does when used as an
          ;; index.
          (condition-case e (compare-strings "abc" "x" nil "abc" 0 nil) (error e))
          (condition-case e (compare-strings "abc" 0 "x" "abc" 0 nil) (error e))
          ;; STR1's bad range is reported (and its error takes priority)
          ;; even when STR2's range is fine.
          (condition-case e (compare-strings "abc" 5 nil "abc" 0 nil) (error e))
          (condition-case e (compare-strings "abc" 0 5 "abc" 0 nil) (error e))
          (condition-case e (compare-strings "abc" 2 1 "abc" 0 nil) (error e))
          ;; STR2's bad range surfaces only once STR1's own START1/END1
          ;; pair is fully valid.
          (condition-case e (compare-strings "abc" 0 nil "abc" 5 nil) (error e))
          ;; A too-large positive END is silently clamped to the string's
          ;; length rather than treated as out of range.
          (compare-strings "abc" 0 99 "abc" 0 nil)
          ;; Negative START/END count from the end of the string, exactly
          ;; like `substring'.
          (compare-strings "abc" -2 nil "abc" 0 nil)
          (compare-strings "abc" 0 -1 "abcd" 0 nil)
          ;; Ordinary equality/ordering, case-sensitive and
          ;; case-insensitive (IGNORE-CASE upcases, per Fcompare_strings).
          (compare-strings "abc" 0 nil "abc" 0 nil)
          (compare-strings "abd" 0 nil "abc" 0 nil)
          (compare-strings "ABC" 0 nil "abc" 0 nil t)
          (string-equal-ignore-case "AB" "ab")
          (string-equal-ignore-case "AB" "abc"))))

;;; nelisp-string-equal-ignore-case-parity-test.el ends here
