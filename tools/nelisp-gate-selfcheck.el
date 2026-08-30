;;; nelisp-gate-selfcheck.el --- does each gate actually look at anything -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Every gate prints a verdict.  A verdict answers "was what I looked at
;; clean" -- it does not answer "did I look at anything", and on 2026-08-19
;; three separate checks were green while seeing nothing:
;;
;;   * the checked-allocator soak compared rounds that ran in SEPARATE
;;     processes, so a leak could not make it fail;
;;   * tools/nelisp-emacs-compat.el read quoted data as definitions and
;;     counted 1,771 names that do not exist;
;;   * the shadow differential compared the standalone against itself, so a
;;     defect present in both halves was invisible.
;;
;; `GATE-COUNT checked=N findings=M' is the second answer.  This runs each
;; gate and requires N inside a band -- catching the case where a scan stops
;; finding its inputs, which no verdict can report.
;;
;; It deliberately does NOT check `findings': that is the gate's own job and
;; its own baseline.  Two numbers, two jobs.
;;
;; A gate can also answer neither question on purpose: `emacs-parity' prints
;; a reasoned `GATE-SKIP ...' line, with no GATE-COUNT, on a host outside its
;; pinned Emacs major (Makefile, "Version-pinned since fix/emacs-parity-
;; version-guard").  Measured on GitHub Actions run 32608413695 (ubuntu/29.4
;; lane, 2026-08-23): that reasoned skip made THIS gate fail -- the band scan
;; saw no GATE-COUNT line and, not knowing the difference, reported it the
;; same as a gate that silently found nothing.  A reasoned skip is population
;; examined (the row is still counted) but is not a finding; only a gate that
;; prints NEITHER a count NOR a skip reason stays one -- `tools/ai/README.md'
;; states the same rule ("a reasoned skip is neither pass nor fail") for the
;; shell-side wrapper this file mirrors on the Elisp side.
;;
;; Run: make gate-selfcheck

;;; Code:

(require 'cl-lib)

(defconst nelisp-gate-selfcheck--bands "tools/gate-count-bands.txt")

(defun nelisp-gate-selfcheck--bands ()
  (let ((rows nil))
    (with-temp-buffer
      (insert-file-contents nelisp-gate-selfcheck--bands)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                   (line-end-position)))))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (when (string-match "\\`\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)\\'" line)
              (push (list (match-string 1 line)
                          (string-to-number (match-string 2 line))
                          (string-to-number (match-string 3 line)))
                    rows))))
        (forward-line 1)))
    (nreverse rows)))

(defun nelisp-gate-selfcheck--checked (target)
  "Run TARGET and classify what its output said about itself.

Returns `(count . N)' when a `GATE-COUNT checked=N' line is present,
`(skip . REASON)' when TARGET instead printed a reasoned `GATE-SKIP
REASON' line and no GATE-COUNT, or nil when it printed neither -- the
same output this already scanned for GATE-COUNT, so no second run of
TARGET is needed.  Skip is checked first, matching the precedence
`tools/ai/nelisp-ai.sh cmd_gate' uses for the same two lines: a gate
that explains why it did not run does not also owe a count."
  (with-temp-buffer
    ;; The gate's own exit status is ignored on purpose: a gate that FAILS
    ;; still reports how much it looked at, and that is what is wanted here.
    (call-process "make" nil t nil target)
    (goto-char (point-min))
    (cond
     ((re-search-forward "^GATE-SKIP \\(.*\\)$" nil t)
      (cons 'skip (match-string 1)))
     ((progn
        (goto-char (point-min))
        (re-search-forward "^GATE-COUNT checked=\\([0-9]+\\)" nil t))
      (cons 'count (string-to-number (match-string 1))))
     (t nil))))

(defun nelisp-gate-selfcheck-run ()
  (let ((bands (nelisp-gate-selfcheck--bands))
        (bad nil))
    (when (null bands)
      (princ (format "gate-selfcheck: FAIL (no bands in %s)\n" nelisp-gate-selfcheck--bands))
      (kill-emacs 1))
    (dolist (row bands)
      (let* ((target (nth 0 row)) (lo (nth 1 row)) (hi (nth 2 row))
             (result (nelisp-gate-selfcheck--checked target)))
        (cond
         ((null result)
          (princ (format "%-20s NO GATE-COUNT LINE\n" target))
          (push (list target nil lo hi) bad))
         ((eq (car result) 'skip)
          ;; Reasoned skip: the population was still examined (this row
          ;; stays IN `checked' below, via `(length bands)'), but a gate
          ;; that said why it did not run is not a finding -- excluded
          ;; from the band check entirely, not compared to [lo, hi].
          (princ (format "%-20s SKIP (reasoned) -- excluded from band check: %s\n"
                         target (cdr result))))
         (t
          (let ((n (cdr result)))
            (if (or (< n lo) (> n hi))
                (progn
                  (princ (format "%-20s checked=%-8d OUTSIDE [%d, %d]\n" target n lo hi))
                  (push (list target n lo hi) bad))
              (princ (format "%-20s checked=%-8d ok [%d, %d]\n" target n lo hi))))))))
    (princ (format "GATE-COUNT checked=%d findings=%d\n" (length bands) (length bad)))
    (if bad
        (progn
          (princ (format "gate-selfcheck: FAIL (%d gate(s) looked at an unexpected amount -- a scan that stops finding its inputs still prints PASS, which is the case this catches)\n"
                         (length bad)))
          (kill-emacs 1))
      (princ (format "gate-selfcheck: PASS (%d gates report a plausible scan size)\n"
                     (length bands))))))

(nelisp-gate-selfcheck-run)

;;; nelisp-gate-selfcheck.el ends here
