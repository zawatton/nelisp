;;; fxir-analysis-query.el --- One-row M0 source query -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Repository root usage:
;; emacs -Q --batch -L lisp -l tools/ai/fxir-analysis-query.el -- ROW [CODE]
;; ROW and optional CODE contain one printed datum each, not executable Lisp.
;; Output is a schema-1 plist; exit 0 means valid structure, never admission.

(require 'nelisp-cc-fxir-analysis)

(defun nelisp-fxir-analysis-query--read (path)
  "Read exactly one bounded datum at PATH without evaluating it."
  (unless (and (file-regular-p path)
               (<= (file-attribute-size (file-attributes path)) 1000000))
    (error "Expected a regular datum file of at most 1000000 bytes"))
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((datum (read (current-buffer))))
      (forward-comment (buffer-size))
      (unless (eobp) (error "Expected exactly one datum"))
      datum)))

(let* ((args (if (equal (car command-line-args-left) "--")
                 (cdr command-line-args-left) command-line-args-left)))
  (unless (memq (length args) '(1 2)) (error "Usage: -- ROW [CODE]"))
  (let* ((report (nelisp-cc-fxir-analysis-row
                  (nelisp-fxir-analysis-query--read (car args))
                  (when (cadr args) (nelisp-fxir-analysis-query--read (cadr args)))))
         (print-length nil) (print-level nil))
    (princ (prin1-to-string report)) (terpri)
    (kill-emacs (if (plist-get report :valid) 0 2))))
