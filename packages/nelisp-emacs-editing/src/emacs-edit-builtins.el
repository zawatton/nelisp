;;; emacs-edit-builtins.el --- Editing commands + kill-ring (Track E MVP)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track E (2026-05-03) — Layer 2.
;;
;; The β-stage editing-command surface that the user-facing edit cycle
;; needs once the command loop dispatches keys to commands:
;;
;;   - `self-insert-command' / `newline' / `delete-backward-char'
;;   - kill-ring infra: `kill-ring' / `kill-new' / `copy-region-as-kill'
;;     / `kill-region' / `kill-line' / `yank'
;;   - word motion: `forward-word' / `backward-word' (= ASCII alnum
;;     boundary — syntax-table integration deferred to a future phase
;;     once `nelisp-ec' grows a syntax-class accessor).
;;
;; Out of scope (= deferred):
;;
;;   - `buffer-undo-list' / `undo' / `primitive-undo': will be a
;;     dedicated Phase once we agree on the record-list shape.
;;   - `transpose-chars' / `open-line' / `indent-*': depend on mode
;;     hooks or selection state we don't yet model.
;;
;; Each definition is gated on `unless (fboundp ...)' / `unless
;; (boundp ...)' so loading inside a host Emacs is a cheap no-op.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'emacs-buffer-builtins)
(require 'emacs-line-builtins)

;;;; --- last-command-event placeholder ---------------------------------

;; Real Emacs sets `last-command-event' inside the command loop;
;; for now we only care that the variable exists so callers (=
;; `self-insert-command' default arg) don't void-variable.

(unless (boundp 'last-command-event)
  (defvar last-command-event nil
    "Phase E placeholder for the command-loop-set last input event."))

;;;; --- character insertion --------------------------------------------

(unless (fboundp 'self-insert-command)
  (defun self-insert-command (&optional n char)
    "Phase E polyfill: insert CHAR (or `last-command-event') N times.
N defaults to 1.  CHAR may be an integer or a single-char string;
when nil, falls back to `last-command-event'."
    (let* ((c (or char last-command-event))
           (count (or n 1))
           (s (cond
               ((null c)
                (signal 'error '("self-insert-command: no char to insert")))
               ((stringp c) c)
               ((integerp c) (string c))
               (t (signal 'wrong-type-argument
                          (list 'character-or-string c))))))
      (let ((i 0))
        (while (< i count)
          (nelisp-ec-insert s)
          (setq i (+ i 1))))
      nil)))

(unless (fboundp 'newline)
  (defun newline (&optional n interactive)
    "Phase E polyfill: insert N newlines (default 1)."
    (ignore interactive)
    (let ((c (or n 1)) (i 0))
      (while (< i c)
        (nelisp-ec-insert "\n")
        (setq i (+ i 1))))
    nil))

(unless (fboundp 'delete-backward-char)
  (defun delete-backward-char (n &optional killflag)
    "Phase E polyfill: delete N characters backward.
KILLFLAG (= prefix-arg-driven `kill-region' route) is accepted for
API parity but ignored in MVP."
    (ignore killflag)
    (nelisp-ec-delete-char (- n))))

;;;; --- kill ring -----------------------------------------------------

(unless (boundp 'kill-ring)
  (defvar kill-ring nil
    "Phase E placeholder: list of killed strings, most recent first."))

(unless (boundp 'kill-ring-max)
  (defvar kill-ring-max 60
    "Phase E placeholder: maximum length of `kill-ring'."))

(unless (boundp 'kill-ring-yank-pointer)
  (defvar kill-ring-yank-pointer nil
    "Phase E placeholder: cdr-pointer into `kill-ring' for `yank-pop'.
Set to `kill-ring' on each fresh kill."))

(defun emacs-edit--trim-kill-ring ()
  "Truncate `kill-ring' to `kill-ring-max' entries."
  (let ((c kill-ring) (i 1))
    (while (and c (< i kill-ring-max))
      (setq c (cdr c))
      (setq i (+ i 1)))
    (when c (setcdr c nil))))

(unless (fboundp 'kill-new)
  (defun kill-new (string &optional replace)
    "Phase E polyfill: prepend STRING to `kill-ring'.
With REPLACE non-nil, mutate the head entry instead of pushing."
    (when (and (stringp string) (> (length string) 0))
      (cond
       ((and replace kill-ring)
        (setcar kill-ring string))
       (t
        (setq kill-ring (cons string kill-ring))
        (emacs-edit--trim-kill-ring)))
      (setq kill-ring-yank-pointer kill-ring))
    string))

(unless (fboundp 'copy-region-as-kill)
  (defun copy-region-as-kill (start end &optional region)
    "Phase E polyfill: push the START..END region onto `kill-ring'."
    (ignore region)
    (let ((s (min start end))
          (e (max start end)))
      (kill-new (nelisp-ec-buffer-substring s e)))
    nil))

(unless (fboundp 'kill-region)
  (defun kill-region (start end &optional region)
    "Phase E polyfill: push the START..END region to `kill-ring' AND delete it."
    (ignore region)
    (let ((s (min start end))
          (e (max start end)))
      (kill-new (nelisp-ec-buffer-substring s e))
      (nelisp-ec-delete-region s e))
    nil))

(unless (fboundp 'kill-line)
  (defun kill-line (&optional arg)
    "Phase E polyfill: kill from point to end of line.
At EOL (= no chars to kill on the current line) and not at EOB,
kills the trailing `\\n' so successive `kill-line' calls collapse
the cursor toward EOB.  ARG (= multi-line variant) deferred."
    (ignore arg)
    (let* ((start (nelisp-ec-point))
           (em (nelisp-ec-point-max))
           (eol (emacs-line--eol-pos)))
      (cond
       ((= start eol)
        ;; At EOL — kill the \n itself if any.
        (when (< eol em)
          (kill-region start (+ eol 1))))
       (t
        (kill-region start eol))))))

(unless (fboundp 'yank)
  (defun yank (&optional arg)
    "Phase E polyfill: insert the most recent kill at point.
ARG selects which kill-ring entry: 1 (default) = head; N>1 = N-th
older entry; `-' = `yank-pop'-style (deferred).  Negative / `-'
arg currently coerced to head-yank."
    (let ((idx (cond
                ((null arg) 0)
                ((integerp arg) (max 0 (- arg 1)))
                (t 0)))
          (entry nil))
      (let ((c kill-ring))
        (while (and c (> idx 0))
          (setq c (cdr c))
          (setq idx (- idx 1)))
        (setq entry (and c (car c)))
        (setq kill-ring-yank-pointer (or c kill-ring)))
      (when entry
        (nelisp-ec-insert entry))
      nil)))

;;;; --- word motion (ASCII alnum) -------------------------------------

(defun emacs-edit--word-char-p (ch)
  "Phase E MVP: ASCII alnum-or-underscore = word constituent."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun emacs-edit--char-at (pos)
  "Return the char at POS, or nil when out of buffer range."
  (let* ((bm (nelisp-ec-point-min))
         (em (nelisp-ec-point-max)))
    (when (and (>= pos bm) (< pos em))
      (let ((s (nelisp-ec-buffer-substring pos (+ pos 1))))
        (and (> (length s) 0) (aref s 0))))))

(unless (fboundp 'forward-word)
  (defun forward-word (&optional arg)
    "Phase E polyfill: ASCII alnum word motion.
ARG > 0: move forward ARG words.  ARG < 0: move backward.
Returns t when at least one boundary was crossed, nil otherwise."
    (let* ((count (or arg 1))
           (sign (if (>= count 0) 1 -1))
           (n (abs count))
           (moved 0))
      (cond
       ((= sign 1)
        (while (> n 0)
          (let ((p (nelisp-ec-point))
                (em (nelisp-ec-point-max))
                (start-p nil))
            (while (and (< p em)
                        (not (emacs-edit--word-char-p
                              (emacs-edit--char-at p))))
              (setq p (+ p 1)))
            (setq start-p p)
            (while (and (< p em)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at p)))
              (setq p (+ p 1)))
            (when (> p start-p) (setq moved (+ moved 1)))
            (nelisp-ec-goto-char p)
            (setq n (- n 1)))))
       (t
        (while (> n 0)
          (let ((p (nelisp-ec-point))
                (bm (nelisp-ec-point-min))
                (start-p nil))
            (while (and (> p bm)
                        (not (emacs-edit--word-char-p
                              (emacs-edit--char-at (- p 1)))))
              (setq p (- p 1)))
            (setq start-p p)
            (while (and (> p bm)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at (- p 1))))
              (setq p (- p 1)))
            (when (< p start-p) (setq moved (+ moved 1)))
            (nelisp-ec-goto-char p)
            (setq n (- n 1))))))
      (> moved 0))))

(unless (fboundp 'backward-word)
  (defun backward-word (&optional arg)
    "Phase E polyfill: equivalent to `(forward-word (- ARG))'."
    (forward-word (- (or arg 1)))))

(provide 'emacs-edit-builtins)

;;; emacs-edit-builtins.el ends here
