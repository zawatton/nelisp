;;; recursion-guard-smoke.el --- deep recursion must signal, not die -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Regression for the 2026-08-16 recalibration of `rec_max'.
;;
;; The `excessive-lisp-nesting' guard in `nelisp_eval_call' was always
;; implemented, but rec_max (300000) sat ABOVE the real native ceiling
;; of the eval stack, so recursion deeper than the ceiling exhausted
;; the stack before the guard could fire: the process died with a
;; silent exit 127 and no diagnosis.  The ceiling was measured at
;; ~136k rec levels -- a self-recursive elisp function survived depth
;; 65000 and SIGSEGVed by 72000 -- against a comment claiming ~404k.
;; Doc 152 Stage 3 roots eval transients, so this non-tail probe now also
;; holds root-depth=3N+6.  The root region was enlarged 1 MiB -> 4 MiB
;; (32768 -> 131072 entries) so that budget stays well clear of rec_max;
;; rec_max stays 100000, ~74% of the measured native-stack ceiling.
;;
;; This runs on the standalone binary and asserts both halves:
;;   1. recursion inside the budget still returns normally, and
;;   2. recursion past it signals a CATCHABLE `excessive-lisp-nesting'
;;      instead of killing the process.
;;
;; Run it through `make standalone-reader-recursion-guard-smoke'.

;;; Code:

(defun nelisp-rec-guard-probe (n)
  "Return N by counting down one recursive call at a time."
  (if (= n 0) 0 (+ 1 (nelisp-rec-guard-probe (- n 1)))))

(let ((shallow (nelisp-rec-guard-probe 1000))
      (deep (condition-case e
                (progn (nelisp-rec-guard-probe 200000) 'no-error)
              (error (car e)))))
  (if (and (equal shallow 1000)
           (eq deep 'excessive-lisp-nesting))
      (princ "[recursion-guard-smoke] PASS: shallow=1000 deep=excessive-lisp-nesting\n")
    (princ (format "[recursion-guard-smoke] FAIL: shallow=%S deep=%S\n"
                   shallow deep))
    (kill-emacs 1)))

;;; recursion-guard-smoke.el ends here
