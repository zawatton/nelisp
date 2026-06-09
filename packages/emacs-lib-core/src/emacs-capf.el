;;; emacs-capf.el --- Minimal completion-at-point for nelisp-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 11 M6 (Package/Major-mode Promotion): a minimal in-buffer
;; `completion-at-point' baseline -- the CAPF/completion style that must
;; exist before any company/eglot-style claims.  Major modes register an
;; in-buffer completion source on `completion-at-point-functions'; this
;; runs the first one that applies and completes the text before point.
;;
;; Completion tables are lists of candidate strings (the common case).
;; Function / alist / hash-table tables are intentionally out of scope for
;; this baseline.

;;; Code:

(require 'nelisp-emacs-compat)

(defvar completion-at-point-functions nil
  "Special hook for in-buffer completion.
Each function takes no arguments and returns either nil (it does not apply
here) or a list (START END COLLECTION . PROPS), where START..END is the
buffer region to complete and COLLECTION is a list of candidate strings.")

(defun emacs-capf--candidates (text collection)
  "Return the COLLECTION strings that have TEXT as a prefix."
  (let (out)
    (dolist (c collection)
      (when (and (stringp c) (string-prefix-p text c))
        (push c out)))
    (nreverse out)))

(defun emacs-capf--common-prefix (strings)
  "Return the longest common string prefix of STRINGS."
  (if (null strings)
      ""
    (let ((prefix (car strings)))
      (dolist (s (cdr strings))
        (let ((i 0)
              (max (min (length prefix) (length s))))
          (while (and (< i max) (eq (aref prefix i) (aref s i)))
            (setq i (1+ i)))
          (setq prefix (substring prefix 0 i))))
      prefix)))

(defun emacs-capf--replace-region (start end string)
  "Replace the buffer text START..END with STRING."
  (nelisp-ec-delete-region start end)
  (nelisp-ec-goto-char start)
  (nelisp-ec-insert string))

;;;###autoload
(defun completion-at-point ()
  "Complete the text before point via `completion-at-point-functions'.

Runs the hook functions until one returns a (START END COLLECTION) form,
then completes the region START..END against COLLECTION:
- exactly one match  -> insert it, return t;
- several matches    -> extend to their longest common prefix (when that
  grows the text) and return the list of matches;
- no match           -> return nil.
Returns nil when no hook function applies."
  (interactive)
  (let ((fns completion-at-point-functions)
        (res nil))
    (while (and fns (not res))
      (setq res (funcall (car fns)))
      (setq fns (cdr fns)))
    (when (and (consp res) (nth 2 res))
      (let* ((start (nth 0 res))
             (end (nth 1 res))
             (collection (nth 2 res))
             (text (nelisp-ec-buffer-substring start end))
             (cands (emacs-capf--candidates text collection)))
        (cond
         ((null cands) nil)
         ((= 1 (length cands))
          (emacs-capf--replace-region start end (car cands))
          t)
         (t
          (let ((cp (emacs-capf--common-prefix cands)))
            (when (> (length cp) (length text))
              (emacs-capf--replace-region start end cp)))
          cands))))))

(provide 'emacs-capf)

;;; emacs-capf.el ends here
