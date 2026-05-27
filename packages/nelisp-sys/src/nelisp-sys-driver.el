;;; nelisp-sys-driver.el --- Front-end facade + analysis pipeline for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The top-level facade.  Requiring `nelisp-sys-driver' pulls in the whole
;; front end (types, target model, layout, AST, frontend, type checker, and
;; the ownership/borrow/unsafe safety passes) and exposes a single analysis
;; entry point that runs every check in order.
;;
;; Native code generation (Stage 130.3+ / 132.3) is layered on top of this
;; via the backend + adapter, required lazily by the compile entry points so
;; that pure-analysis use never needs a toolchain.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-target)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-abi-layout)
(require 'nelisp-sys-check)
(require 'nelisp-sys-ownership)
(require 'nelisp-sys-borrow)
(require 'nelisp-sys-unsafe)

;; Backend is required lazily by the compile entry points.
(declare-function nelisp-sys-backend-emit-object "nelisp-sys-backend"
                  (module output-path target))

;;; Analysis.

(defun nelisp-sys-check-all (module)
  "Run every static check over MODULE in order.
Type checking first (it resolves types and catches structural errors),
then the ownership, borrow, and unsafe/effect passes.  Signals the
first pass's error (a `nelisp-sys-error' subtype) on failure; returns t
when MODULE passes all gates."
  (nelisp-sys-check-module module)
  (nelisp-sys-ownership-check-module module)
  (nelisp-sys-borrow-check-module module)
  (nelisp-sys-unsafe-check-module module)
  t)

(defun nelisp-sys-check-all-collect (module)
  "Run every static check over MODULE, returning a list of diagnostics.
Returns type diagnostics first if any (the AST may be semantically
unsound for the later passes); otherwise the combined ownership, borrow,
and unsafe diagnostics.  Nil means MODULE passes all gates."
  (or (nelisp-sys-check-collect module)
      (append (nelisp-sys-ownership-check-collect module)
              (nelisp-sys-borrow-check-collect module)
              (nelisp-sys-unsafe-check-collect module))))

(defun nelisp-sys-analyze (forms)
  "Parse FORMS into a module and run all static checks.
Returns t on success; signals on the first parse/check error."
  (nelisp-sys-check-all (nelisp-sys-frontend-parse-module forms)))

(defun nelisp-sys-analyze-collect (forms)
  "Parse FORMS and run all checks, returning a diagnostics list (nil = clean).
Parse errors are reported as a single (E-SYS-PARSE MESSAGE FORM) entry."
  (condition-case err
      (nelisp-sys-check-all-collect (nelisp-sys-frontend-parse-module forms))
    (nelisp-sys-parse-error
     (list (list 'E-SYS-PARSE (car err) (plist-get (cdr err) :form))))))

;;; Compilation (native codegen layered on top; backend required lazily).

(defun nelisp-sys-compile-object (forms output-path &optional target)
  "Analyze FORMS and compile the module to a native object at OUTPUT-PATH.
TARGET is a triple string (defaults to the host).  Requires the backend
\(and therefore the NeLisp toolchain) lazily so analysis-only callers do
not pay for it.  Returns OUTPUT-PATH."
  (let ((module (nelisp-sys-frontend-parse-module forms)))
    (nelisp-sys-check-all module)
    (require 'nelisp-sys-backend)
    (nelisp-sys-backend-emit-object
     module output-path (or target (nelisp-sys-target-triple
                                     (nelisp-sys-target-host))))))

(provide 'nelisp-sys-driver)

;;; nelisp-sys-driver.el ends here
