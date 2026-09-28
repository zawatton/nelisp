;;; nelisp-declare-standalone-probe.el --- `declare' semantics probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loaded by test/nelisp-declare-standalone-smoke.sh on both stock Emacs
;; (`--batch -Q') and the standalone binary; the two outputs must be
;; identical.  The standalone's native `defun' / `defmacro' dispatch used to
;; strip every `(declare ...)' clause, so the properties GNU byte-run.el
;; records through `defun-declarations-alist' / `macro-declarations-alist'
;; never appeared, and a redefined `defun' was never consulted for a
;; top-level form.  Each line prints what GNU Emacs records.

;;; Code:

(require 'inline)

(defun nds-cm (form _x) form)

(defun nds-f1 (x)
  "Doc."
  (declare (compiler-macro nds-cm) (indent 1) (pure t)
           (side-effect-free t) (doc-string 2) (speed 3)
           (completion ignore) (interactive-only "use x")
           (important-return-value t))
  (+ x 1))
(princ (format "F1 %S %S\n" (symbol-plist 'nds-f1) (nds-f1 1)))

(defun nds-f2 (a &optional b)
  (declare (compiler-macro (lambda (form) form)))
  (list a b))
(princ (format "F2 %S %S %S\n" (get 'nds-f2 'compiler-macro)
               (fboundp 'nds-f2--anon-cmacro) (nds-f2 1)))

(defun nds-f3 (x) (declare (obsolete nds-f1 "30.1")) x)
(princ (format "F3 %S\n" (get 'nds-f3 'byte-obsolete-info)))

;; `(declare ...)' as the whole body: the function still returns nil.
(defun nds-f4 () (declare (pure t)))
(princ (format "F4 %S %S\n" (symbol-plist 'nds-f4) (nds-f4)))

;; Not at top level.
(let ((n 1))
  (defun nds-f5 (x) (declare (side-effect-free error-free)) (+ x n)))
(princ (format "F5 %S %S\n" (symbol-plist 'nds-f5) (nds-f5 1)))

(defmacro nds-m1 (x &rest body)
  "Doc."
  (declare (indent 1) (debug (form body)) (no-font-lock-keyword t))
  `(progn ,x ,@body))
(princ (format "M1 %S %S\n" (symbol-plist 'nds-m1) (nds-m1 1 2)))

(defsubst nds-s1 (x) (declare (side-effect-free t)) (* x 2))
(princ (format "S1 %S %S\n" (get 'nds-s1 'side-effect-free) (nds-s1 4)))

;; `define-inline' registers its inliner through `declare'.
(define-inline nds-inl (x) (inline-quote (+ ,x 1)))
(princ (format "INL %S %S\n" (function-get 'nds-inl 'compiler-macro)
               (nds-inl 2)))

;; A redefined `defun' is what a top-level `defun' form runs.
(defvar nds-seen nil)
(let ((orig (symbol-function 'defun)))
  (unwind-protect
      (progn
        (fset 'defun (cons 'macro
                           (lambda (name &rest rest)
                             (setq nds-seen (cons name nds-seen))
                             (apply (cdr orig) name rest))))
        (eval '(defun nds-f6 (x) (* x 3)) t))
    (fset 'defun orig)))
(defun nds-f7 (x) x)
(princ (format "REDEF %S %S %S\n" nds-seen (nds-f6 2) (nds-f7 5)))

;; The standalone's bootstrap declaration tables (used before byte-run.el
;; loads) must record what GNU's handlers record.  On stock Emacs the same
;; forms run with GNU's own tables.
(defmacro nds-with-bootstrap-tables (&rest body)
  (if (boundp 'nelisp--bootstrap-defun-declarations)
      `(let ((defun-declarations-alist nelisp--bootstrap-defun-declarations)
             (macro-declarations-alist nelisp--bootstrap-macro-declarations))
         ,@body)
    `(progn ,@body)))
(nds-with-bootstrap-tables
 (eval '(defun nds-b1 (x)
          (declare (compiler-macro nds-cm) (indent 1) (pure t)
                   (side-effect-free t) (doc-string 2) (speed 3)
                   (completion ignore) (interactive-only "use x")
                   (important-return-value t) (obsolete nds-f1 "30.1"))
          x)
       t)
 (eval '(defmacro nds-b2 (x)
          (declare (indent 1) (debug (form)) (no-font-lock-keyword t))
          x)
       t))
(princ (format "B1 %S\n" (symbol-plist 'nds-b1)))
(princ (format "B2 %S\n" (symbol-plist 'nds-b2)))

;; The bootstrap `defun' macro's declaration forms (standalone only; stock
;; Emacs has no such function, so run the forms GNU's `defun' expansion
;; carries).  Compare what they record, not their spelling: the bootstrap
;; writes with `put' because `function-put' does not exist yet there.
(eval (cons 'progn
            (if (fboundp 'nelisp--declaration-forms)
                (nds-with-bootstrap-tables
                 (nelisp--declaration-forms
                  'nds-b3 '(x) '("doc" (declare (pure t) (indent 2)) x)
                  'defun))
              (cdr (cdr (macroexpand-1
                         '(defun nds-b3 (x) "doc"
                            (declare (pure t) (indent 2)) x))))))
      t)
(princ (format "BF %S\n" (symbol-plist 'nds-b3)))

;;; nelisp-declare-standalone-probe.el ends here
