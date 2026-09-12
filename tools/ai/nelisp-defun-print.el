;;; nelisp-defun-print.el --- print one defun's source, by name -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The generated evalport/combiner modules under `lisp/' hold their whole
;; body as a single quoted form on ONE line --
;; `nelisp-cc-evalport-combiner-apply.el' is five lines and 40 KB.  Opening
;; one to read a single helper costs the whole file; on 2026-09-12 that
;; happened once and took a large part of a session's context to read four
;; defuns out of it.  This prints exactly the defun asked for.
;;
;; Balanced with the REAL reader (`forward-sexp' in `emacs-lisp-mode'), never
;; a paren count: these files contain `?\(' character literals and parens
;; inside strings, and a counting scan stops in the wrong place.  The same
;; rule as bisecting a vendor file -- see `AI.md'.
;;
;; Driven by `tools/ai/nelisp-ai.sh defun NAME [ROOT...]'.  Reads
;; NELISP_DEFUN_NAME and NELISP_DEFUN_ROOTS (space separated) so nothing has
;; to survive a second round of shell quoting.

;;; Code:

(defun nelisp-defun-print--files (root)
  "Every .el file under ROOT, or ROOT itself when it is a file."
  (cond ((file-directory-p root)
         (directory-files-recursively root "\\.el\\'"))
        ((file-readable-p root) (list root))
        (t nil)))

(defun nelisp-defun-print--in-file (name file)
  "Print NAME's defun from FILE if it is there.  Return non-nil when printed."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "(defun " (regexp-quote name) "[ \n(]") nil t)
      (goto-char (match-beginning 0))
      (let ((start (point)))
        ;; A truncated or malformed tail should report what it found rather
        ;; than fail the whole lookup.
        (condition-case nil (forward-sexp 1) (error (goto-char (point-max))))
        (princ (format "=== %s  (%s, %d chars) ===\n" name file (- (point) start)))
        (princ (buffer-substring-no-properties start (point)))
        (princ "\n")
        t))))

(defun nelisp-defun-print-batch ()
  "Print the first defun named by NELISP_DEFUN_NAME under NELISP_DEFUN_ROOTS."
  (let* ((name (getenv "NELISP_DEFUN_NAME"))
         (roots (split-string (or (getenv "NELISP_DEFUN_ROOTS") "") " " t))
         (found nil))
    (unless (and name (> (length name) 0))
      (error "nelisp-defun-print: NELISP_DEFUN_NAME is unset"))
    (dolist (root roots)
      (dolist (file (nelisp-defun-print--files root))
        (unless found
          (setq found (nelisp-defun-print--in-file name file)))))
    (unless found
      (princ (format "nelisp-ai.sh defun: %s not found under %s\n"
                     name (mapconcat #'identity roots " "))))))

(provide 'nelisp-defun-print)

;;; nelisp-defun-print.el ends here
