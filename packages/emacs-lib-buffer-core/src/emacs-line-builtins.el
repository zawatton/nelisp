;;; emacs-line-builtins.el --- Line / column primitive derivations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase J (Track A, 2026-05-03) — Layer 2.
;;
;; Provides the unprefixed line / column primitives that the Phase
;; 11.A' note in `emacs-stub.el' explicitly carved out as "no L1.5
;; line-iterator yet": `bobp', `eobp', `bolp', `eolp',
;; `beginning-of-line', `end-of-line', `line-beginning-position',
;; `line-end-position', `forward-line', `line-number-at-pos'.
;;
;; Strategy: derive everything from `nelisp-ec-buffer-substring' +
;; point/point-min/point-max manipulation rather than introducing new
;; L1.5 substrate.  The substrate already exposes enough buffer-text
;; access to compute line boundaries by scanning for `\n' bytes; we
;; just keep that scan inside L2.  This avoids cross-layer edits and
;; matches the same "L2 derivation on top of nelisp-ec-*" pattern used
;; by `emacs-buffer-builtins.el' (Phase 9) for its non-bridge forms.
;;
;; Function definitions use a host-aware install gate: host Emacs keeps
;; its C builtins, while standalone NeLisp overwrites any bootstrap
;; stubs with these line/column derivations.
;;
;; Phase J also deletes the no-op stubs that this file supersedes
;; from `emacs-stub.el' (= the same load-order shadowing risk that
;; Phases 11.A' / 11.B' / 11.C'' fixed for the buffer / search /
;; keymap / frame / window sides).

;;; Code:

(require 'nelisp-emacs-compat)
(require 'emacs-buffer-builtins)

;;;; --- private helpers --------------------------------------------------

(defun emacs-line-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(defun emacs-line--bol-pos (&optional pos)
  "Return position of beginning of line containing POS (default = point).
Does not move point."
  (let* ((p (or pos (nelisp-ec-point)))
         (bm (nelisp-ec-point-min)))
    (if (<= p bm)
        bm
      (let* ((s (nelisp-ec-buffer-substring bm p))
             (n (length s))
             (i (- n 1))
             (found nil))
        (while (and (>= i 0) (null found))
          (when (eq (aref s i) ?\n)
            (setq found i))
          (setq i (- i 1)))
        (if found (+ bm found 1) bm)))))

(defun emacs-line--eol-pos (&optional pos)
  "Return position of end of line containing POS (= just before \\n or
point-max).  Does not move point."
  (let* ((p (or pos (nelisp-ec-point)))
         (em (nelisp-ec-point-max)))
    (if (>= p em)
        em
      (let* ((s (nelisp-ec-buffer-substring p em))
             (n (length s))
             (i 0)
             (found nil))
        (while (and (< i n) (null found))
          (when (eq (aref s i) ?\n)
            (setq found i))
          (setq i (+ i 1)))
        (if found (+ p found) em)))))

;;;; --- bobp / eobp ------------------------------------------------------

(when (emacs-line-builtins--install-function-p 'bobp)
  (defun bobp ()
    "Phase J polyfill: t when point is at point-min."
    (= (nelisp-ec-point) (nelisp-ec-point-min))))

(when (emacs-line-builtins--install-function-p 'eobp)
  (defun eobp ()
    "Phase J polyfill: t when point is at point-max."
    (= (nelisp-ec-point) (nelisp-ec-point-max))))

;;;; --- eolp / bolp ------------------------------------------------------

(when (emacs-line-builtins--install-function-p 'bolp)
  (defun bolp ()
    "Phase J polyfill: t when point is at start of a line.
True at point-min (= no preceding char) or when the previous char is \\n."
    (or (bobp)
        (let* ((pt (nelisp-ec-point))
               (s (nelisp-ec-buffer-substring (- pt 1) pt)))
          (and (> (length s) 0) (eq (aref s 0) ?\n))))))

(when (emacs-line-builtins--install-function-p 'eolp)
  (defun eolp ()
    "Phase J polyfill: t when point is at end of a line.
True at point-max (= no following char) or when the next char is \\n."
    (or (eobp)
        (let* ((pt (nelisp-ec-point))
               (s (nelisp-ec-buffer-substring pt (+ pt 1))))
          (and (> (length s) 0) (eq (aref s 0) ?\n))))))

;;;; --- line-beginning-position / line-end-position ---------------------

(when (emacs-line-builtins--install-function-p 'line-beginning-position)
  (defun line-beginning-position (&optional n)
    "Phase J polyfill: position of beginning of (current + N - 1)-th line.
N = 1 (default) = current line; N > 1 = forward; N < 1 = backward.
Does not move point."
    (let ((offset (1- (or n 1))))
      (if (= offset 0)
          (emacs-line--bol-pos)
        (save-excursion
          (forward-line offset)
          (nelisp-ec-point))))))

(when (emacs-line-builtins--install-function-p 'line-end-position)
  (defun line-end-position (&optional n)
    "Phase J polyfill: position of end of (current + N - 1)-th line.
N = 1 (default) = current line.  Does not move point."
    (let ((offset (1- (or n 1))))
      (if (= offset 0)
          (emacs-line--eol-pos)
        (save-excursion
          (forward-line offset)
          (emacs-line--eol-pos))))))

;;;; --- beginning-of-line / end-of-line ---------------------------------

(when (emacs-line-builtins--install-function-p 'beginning-of-line)
  (defun beginning-of-line (&optional n)
    "Phase J polyfill: move point to start of (current + N - 1)-th line.
Returns the new point.  Bound to C-a."
    (interactive "p")
    (nelisp-ec-goto-char (line-beginning-position n))))

(when (emacs-line-builtins--install-function-p 'end-of-line)
  (defun end-of-line (&optional n)
    "Phase J polyfill: move point to end of (current + N - 1)-th line.
Returns the new point.  Bound to C-e."
    (interactive "p")
    (nelisp-ec-goto-char (line-end-position n))))

;;;; --- forward-line ----------------------------------------------------

(when (emacs-line-builtins--install-function-p 'forward-line)
  (defun forward-line (&optional n)
    "Phase J polyfill: move point forward N lines (= -N backward).
N defaults to 1.  Returns the count of lines NOT moved — 0 on full
success, positive when hitting EOB, negative when hitting BOB.
Matches Emacs C semantics where moving past the last `\\n' onto the
final line (or to point-max from a line with no trailing newline)
counts as 1 line consumed."
    (let ((count (or n 1)))
      (cond
       ((= count 0)
        (nelisp-ec-goto-char (emacs-line--bol-pos))
        0)
       ((> count 0)
        (let ((c count) (done nil))
          (while (and (> c 0) (not done))
            (let ((em (nelisp-ec-point-max)))
              (if (= (nelisp-ec-point) em)
                  (setq done t)
                (let ((eol (emacs-line--eol-pos)))
                  (cond
                   ((< eol em)
                    (nelisp-ec-goto-char (+ eol 1))
                    (setq c (- c 1)))
                   (t
                    (nelisp-ec-goto-char em)
                    (setq c (- c 1))))))))
          c))
       (t
        (let ((c (- count)) (remaining 0))
          (nelisp-ec-goto-char (emacs-line--bol-pos))
          (while (> c 0)
            (cond
             ((= (nelisp-ec-point) (nelisp-ec-point-min))
              (setq remaining (- c))
              (setq c 0))
             (t
              (nelisp-ec-goto-char (- (nelisp-ec-point) 1))
              (nelisp-ec-goto-char (emacs-line--bol-pos))
              (setq c (- c 1)))))
          remaining))))))

;;;; --- line-number-at-pos ---------------------------------------------

(when (emacs-line-builtins--install-function-p 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos absolute)
    "Phase J polyfill: 1-based line number of POS (default = point).
ABSOLUTE is accepted for API parity but ignored — the substrate has
no narrowing-aware absolute-vs-relative distinction."
    (ignore absolute)
    (let* ((p (or pos (nelisp-ec-point)))
           (bm (nelisp-ec-point-min)))
      (if (<= p bm)
          1
        (let* ((s (nelisp-ec-buffer-substring bm p))
               (n (length s))
               (i 0)
               (count 1))
          (while (< i n)
            (when (eq (aref s i) ?\n)
              (setq count (+ count 1)))
            (setq i (+ i 1)))
          count)))))

;;;; --- next-line / previous-line (Doc 51 Track B) ---------------------------

(when (emacs-line-builtins--install-function-p 'next-line)
  (defun next-line (&optional n _try-vscroll)
    "Doc 51 Track B (2026-05-04) MVP `next-line'.
Move point N lines down, preserving the current column where
possible (= clamps to end-of-line on shorter targets).  N
defaults to 1; negative N moves up.  Bound to C-n / <down>."
    (interactive "p")
    (let* ((n (or n 1))
           (col (- (nelisp-ec-point) (emacs-line--bol-pos))))
      (forward-line n)
      (let* ((bol (emacs-line--bol-pos))
             (eol (emacs-line--eol-pos))
             (target (+ bol col)))
        (nelisp-ec-goto-char (min target eol))))))

(when (emacs-line-builtins--install-function-p 'previous-line)
  (defun previous-line (&optional n _try-vscroll)
    "Doc 51 Track B (2026-05-04) MVP `previous-line'.
Forwarder to `next-line' with negated count.  Bound to C-p / <up>."
    (interactive "p")
    (next-line (- (or n 1)))))

(provide 'emacs-line-builtins)

;;; emacs-line-builtins.el ends here
