;;; nelisp-self-host-test.el --- Phase 2 self-host probe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2 self-host probe: every NeLisp source file in `src/' parses
;; with the NeLisp reader and every top-level form evaluates against
;; NeLisp's evaluator without errors.  After the probe runs, the
;; defuns / defmacros / defvars from those files have all been
;; installed as NeLisp closures / macros / globals — i.e., NeLisp
;; carries a complete copy of itself in its own tables, ready for
;; the cycle-1 / cycle-2 fixpoint comparison once the host C stack
;; can be sidestepped (Phase 3+ trampoline).
;;
;; What this does NOT yet do: actually call into those installed
;; closures with deep recursion.  Doing so loops back through the
;; host evaluator (NeLisp eval -> apply -> body -> NeLisp eval -> …)
;; and exhausts the host C stack at modest depths — a fundamental
;; limitation of an interpreter-on-interpreter without trampoline,
;; tracked in the docs as the cycle-1=cycle-2 work.

;;; Code:

(require 'ert)
(require 'nelisp)

;; CI-smoke gating rationale: the self-host probe parses every NeLisp
;; source file with the NeLisp reader and runs each top-level form
;; through the NeLisp evaluator.  The interpreter-on-interpreter walk
;; consumes deep host C / Emacs Lisp stack frames — local development
;; hosts run with `max-lisp-eval-depth' >= 13000 (Emacs 30 default
;; under interactive use) and finish cleanly, while the CI smoke runners
;; (Emacs 29.4 / 30.1 in `--batch -Q') start with the unraised default
;; (1600) and bubble an `error' (not the `excessive-lisp-nesting' the
;; probe's `condition-case' is prepared for) out of the C-installed
;; macros loop.  We bind a generous depth ceiling for the probe so the
;; legitimate nested walk does not depth-trap on a fresh Emacs.
(defmacro nelisp-self-host-test--with-deep-stack (&rest body)
  "Run BODY with `max-lisp-eval-depth' / `max-specpdl-size' raised.
Mirrors the local-dev host environment so the probe's top-level
recursion does not hit a CI-only depth ceiling."
  (declare (indent 0) (debug t))
  `(let ((max-lisp-eval-depth (max 13000 max-lisp-eval-depth))
         (max-specpdl-size (max 13000 max-specpdl-size)))
     ,@body))

(defconst nelisp-self-host-test--source-dir
  (expand-file-name
   "../src"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the NeLisp source tree.")

(defconst nelisp-self-host-test--files
  '("nelisp-read.el" "nelisp-eval.el" "nelisp-macro.el"
    "nelisp-load.el" "nelisp.el")
  "NeLisp source files in dependency order.")

(defun nelisp-self-host-test--eval-file (path)
  "Read every form in PATH and evaluate it through `nelisp-eval'.
Return the count of forms processed.

Defun / defvar / defmacro / define-error etc install regardless of
whether their body would later run successfully — the install side
effect happens during dispatch, before the closure body executes.
That makes this probe safe to run on top-level call forms whose
fully-installed bodies would recurse through the host C stack
(`excessive-lisp-nesting'); we record the would-be call as the
Phase 3 trampoline target rather than a probe failure."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (let ((forms (nelisp-read-all (buffer-string)))
          (count 0))
      (dolist (form forms)
        (setq count (1+ count))
        (condition-case err
            (nelisp-eval form)
          (excessive-lisp-nesting nil)
          (error
           (signal (car err) (cons (list 'in-form (car-safe form))
                                   (cdr err))))))
      count)))

(ert-deftest nelisp-self-host-all-files-load ()
  "Every NeLisp source file parses and evaluates cleanly through NeLisp."
  (nelisp-self-host-test--with-deep-stack
    (nelisp--reset)
    (let ((total 0))
      (dolist (rel nelisp-self-host-test--files)
        (let* ((path (expand-file-name rel nelisp-self-host-test--source-dir))
               (n (nelisp-self-host-test--eval-file path)))
          (should (> n 0))
          (setq total (+ total n))))
      ;; All 5 files together produce well over 100 forms.
      (should (> total 100)))))

(ert-deftest nelisp-self-host-installs-evaluator ()
  "After self-host load, NeLisp's own evaluator functions are present
as NeLisp closures (not host functions) in `nelisp--functions'."
  (nelisp-self-host-test--with-deep-stack
    (nelisp--reset)
    (dolist (rel nelisp-self-host-test--files)
      (nelisp-self-host-test--eval-file
       (expand-file-name rel nelisp-self-host-test--source-dir)))
    (dolist (sym '(nelisp-eval nelisp-eval-form nelisp--apply
                               nelisp--apply-closure nelisp--lookup
                               nelisp--eval-let nelisp--eval-cond
                               nelisp--eval-condition-case))
      (let ((fn (gethash sym nelisp--functions nelisp--unbound)))
        (should-not (eq fn nelisp--unbound))
        (should (nelisp--closure-p fn))))))

(ert-deftest nelisp-self-host-installs-reader ()
  "After self-host load, the reader functions are NeLisp closures too."
  (nelisp-self-host-test--with-deep-stack
    (nelisp--reset)
    (dolist (rel nelisp-self-host-test--files)
      (nelisp-self-host-test--eval-file
       (expand-file-name rel nelisp-self-host-test--source-dir)))
    (dolist (sym '(nelisp-read nelisp-read-from-string nelisp-read-all
                               nelisp-read--sexp nelisp-read--list
                               nelisp-read--string nelisp-read--bq-expand))
      (let ((fn (gethash sym nelisp--functions nelisp--unbound)))
        (should-not (eq fn nelisp--unbound))
        (should (nelisp--closure-p fn))))))

(ert-deftest nelisp-self-host-installs-macro-system ()
  "After self-host load, `defmacro' dispatch and `macroexpand' family
are NeLisp closures."
  (nelisp-self-host-test--with-deep-stack
    (nelisp--reset)
    (dolist (rel nelisp-self-host-test--files)
      (nelisp-self-host-test--eval-file
       (expand-file-name rel nelisp-self-host-test--source-dir)))
    (dolist (sym '(nelisp--eval-defmacro nelisp-macroexpand
                                         nelisp-macroexpand-1
                                         nelisp-macroexpand-all))
      (let ((fn (gethash sym nelisp--functions nelisp--unbound)))
        (should-not (eq fn nelisp--unbound))
        (should (nelisp--closure-p fn))))))

(ert-deftest nelisp-self-host-installed-globals ()
  "Hash tables and the unbound sentinel are visible after self-host."
  (nelisp-self-host-test--with-deep-stack
    (nelisp--reset)
    (dolist (rel nelisp-self-host-test--files)
      (nelisp-self-host-test--eval-file
       (expand-file-name rel nelisp-self-host-test--source-dir)))
    (dolist (sym '(nelisp--functions nelisp--globals
                                     nelisp--specials nelisp--macros
                                     nelisp--unbound))
      (let ((v (gethash sym nelisp--globals nelisp--unbound)))
        (should-not (eq v nelisp--unbound))))))

(provide 'nelisp-self-host-test)

;;; nelisp-self-host-test.el ends here
