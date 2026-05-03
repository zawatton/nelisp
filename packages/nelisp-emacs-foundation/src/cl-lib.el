;;; cl-lib.el --- nelisp-emacs intercepting cl-lib shim  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track O (2026-05-04) — Layer 2 cl-lib intercept shim.
;;
;; Why this exists: the upstream `vendor/emacs-lisp/emacs-lisp/cl-lib.el'
;; uses several reader features (= `\(' string escape on docstring
;; arglist hints, `,' outside backquote, etc.) that nelisp's reader
;; rejects.  Under host Emacs `cl-lib' is preloaded so `(require
;; 'cl-lib)' is a no-op and our shim never executes.  Under nelisp the
;; shim wins because `src/' precedes `vendor/' on the load-path.
;;
;; We deliberately do NOT mirror every cl-lib symbol — only the
;; subset our Layer-2 substrate touches (= the `MISSING' list from
;; the audit script run as part of Track O).  Most of cl-lib is
;; already covered by `emacs-cl-macros.el' (cl-defun, cl-loop,
;; cl-defstruct, …); this file adds the remaining 3-4 helpers and
;; declares the `cl-lib' feature.
;;
;; If a future substrate change pulls in another cl-lib symbol that
;; isn't here, the right fix is to either (a) add a polyfill here,
;; or (b) port the symbol into `emacs-cl-macros.el'.

;;; Code:

(provide 'cl-lib)

;; Pull in the existing prefixed subset (cl-loop / cl-defun /
;; cl-defstruct / cl-letf / cl-flet / cl-block / cl-some / cl-every /
;; cl-position / cl-find / cl-remove-if{,-not} / cl-delete-* /
;; cl-union / cl-intersection / cl-sort / cl-case / cl-pushnew / etc.)
(require 'emacs-cl-macros)

;;;; --- helpers not in emacs-cl-macros --------------------------------

(unless (fboundp 'cl-subseq)
  (defun cl-subseq (sequence start &optional end)
    "Return the subsequence of SEQUENCE from START to END.
If END is nil, copy SEQUENCE from START to end.  Mirrors the
classic Common Lisp shape used by the Layer-2 substrate (=
`emacs-window.el' tree-rebuild paths)."
    (cond
     ((listp sequence)
      (let* ((rest (nthcdr start sequence))
             (len (if end (- end start) (length rest))))
        (let (out (i 0))
          (while (and rest (< i len))
            (push (car rest) out)
            (setq rest (cdr rest))
            (setq i (1+ i)))
          (nreverse out))))
     ((stringp sequence)
      (substring sequence start end))
     ((vectorp sequence)
      (let* ((len (length sequence))
             (e (or end len))
             (out (make-vector (- e start) nil)))
        (let ((i start) (j 0))
          (while (< i e)
            (aset out j (aref sequence i))
            (setq i (1+ i) j (1+ j))))
        out))
     (t (signal 'wrong-type-argument (list 'sequencep sequence))))))

(unless (fboundp 'cl-remove)
  (defun cl-remove (item sequence)
    "Return SEQUENCE with all occurrences of ITEM removed (`equal' test).
Always returns a fresh list (= callers in `emacs-window.el' rely on
this for sibling-list immutability)."
    (cond
     ((listp sequence)
      (let (out)
        (dolist (x sequence)
          (unless (equal item x) (push x out)))
        (nreverse out)))
     ((stringp sequence)
      (apply #'string
             (cl-loop for c across sequence
                      unless (equal item c) collect c)))
     ((vectorp sequence)
      (apply #'vector
             (cl-loop for x across sequence
                      unless (equal item x) collect x)))
     (t (signal 'wrong-type-argument (list 'sequencep sequence))))))

(unless (fboundp 'cl-find-if-not)
  (defun cl-find-if-not (predicate sequence)
    "Return the first element of SEQUENCE for which PREDICATE is nil."
    (catch 'found
      (cond
       ((listp sequence)
        (dolist (x sequence)
          (unless (funcall predicate x) (throw 'found x))))
       ((stringp sequence)
        (let ((i 0) (n (length sequence)))
          (while (< i n)
            (let ((c (aref sequence i)))
              (unless (funcall predicate c) (throw 'found c)))
            (setq i (1+ i)))))
       ((vectorp sequence)
        (let ((i 0) (n (length sequence)))
          (while (< i n)
            (let ((x (aref sequence i)))
              (unless (funcall predicate x) (throw 'found x)))
            (setq i (1+ i))))))
      nil)))

;;;; --- introspection -------------------------------------------------

(defconst cl-lib-version "1.0-nemacs-shim"
  "Version of the nelisp-emacs cl-lib shim (= NOT upstream cl-lib).")

;;; cl-lib.el ends here
