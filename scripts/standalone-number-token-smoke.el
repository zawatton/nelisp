;;; standalone-number-token-smoke.el --- reader number-token classification -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run under the standalone runtime, not host Emacs:
;;
;;     nelisp --load scripts/standalone-number-token-smoke.el
;;
;; A token that starts with a digit is a number only when it matches integer
;; or float syntax exactly.  Anything else is a symbol.  Two places in this
;; tree decide that -- `nelisp_reader_classify_step' in the native lexer, which
;; is what `load' goes through, and `nelisp--rd-numeric-token-p' in the stdlib
;; prelude, which is what `read-from-string' goes through -- and on 2026-08-19
;; both were wrong and wrong in DIFFERENT ways: `(quote 7.1.4)' read as `(quote
;; 7)' through one and as nothing at all through the other, where a host reads
;; the symbol 7.1.4 and consumes all five characters.
;;
;; That cost `src/nelisp-cc-arm64.el', which has `:phase '7.1.4' in it: the
;; load stopped there, `load' returned t anyway, the file's own `(provide ...)'
;; never ran, `require' answered `file-missing' for a file that is on
;; load-path, and the native compiler was reported unavailable.
;;
;; So the table is checked against BOTH readers, and the answers in it are not
;; a judgement -- they are what `read-from-string' returns on a host Emacs,
;; measured 2026-08-19.  Adding a row means measuring it there first.

;;; Code:

(let ((cases '(("7.1.4" . sym) ("1e2e3" . sym) ("1e2.3" . sym) ("12e" . sym)
               ("1+" . sym) ("1-" . sym) ("0x10" . sym) ("..." . sym)
               ("1.2e3" . num) ("1e+2" . num) (".5" . num) ("1.5e-3" . num)
               ("1." . num) ("42" . num) ("-7" . num) ("+5" . num)))
      (bad 0))
  (while cases
    (let* ((c (car cases))
           (tok (car c))
           (want (cdr c))
           (r (read-from-string tok))
           (got (if (numberp (car r)) 'num 'sym))
           (native (nelisp--read-all-from-string-native tok))
           (ngot (if (numberp (car native)) 'num 'sym)))
      ;; The prelude reader: right value class, and the whole token consumed.
      (unless (eq got want)
        (setq bad (1+ bad))
        (princ (concat "MISMATCH read-from-string " tok "\n")))
      (unless (= (cdr r) (length tok))
        (setq bad (1+ bad))
        (princ (concat "MISMATCH consumed " tok "\n")))
      ;; The native reader: same answer, and it read something at all.  An
      ;; empty return is how this defect presented -- the parser gave up and
      ;; the caller could not tell that from end of input.
      (unless native
        (setq bad (1+ bad))
        (princ (concat "MISMATCH native-reader-empty " tok "\n")))
      (unless (eq ngot want)
        (setq bad (1+ bad))
        (princ (concat "MISMATCH native-reader " tok "\n"))))
    (setq cases (cdr cases)))
  (princ (concat "NUMBER-TOKEN-SMOKE cases=16 mismatches="
                 (number-to-string bad) "\n")))
nil
;;; standalone-number-token-smoke.el ends here
