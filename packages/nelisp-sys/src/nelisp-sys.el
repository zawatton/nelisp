;;; nelisp-sys.el --- Typed C-replacement systems subset for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; nelisp-sys is a typed, statically checked systems-programming subset
;; hosted on the NeLisp Phase 47 toolchain.  It is NOT ordinary dynamic
;; Elisp: source written in the `sys:' family of forms is type checked,
;; ownership/borrow checked, and lowered to native object code through a
;; thin adapter over the existing NeLisp compiler/assembler/linker.
;;
;; Design references:
;;   docs/design/130-nelisp-sys-extractable-c-replacement-subset.org
;;   docs/design/131-nelisp-sys-semantics-and-safety-contract.org
;;   docs/design/132-nelisp-sys-abi-and-platform-model.org
;;
;; Boundary rule (Doc 130): only `nelisp-sys-adapter-nelisp.el' is allowed
;; to call private NeLisp compiler/asm/linker internals.  Every other
;; module talks to the backend through the adapter contract so the package
;; stays extractable into its own repository later.
;;
;; This file is the public aggregator: requiring `nelisp-sys' pulls in the
;; whole front end (types, target model, layout, AST, frontend, checkers)
;; but NOT the backend adapter, which is required explicitly by the driver
;; so that pure-analysis use (type/borrow checking) never needs a toolchain.

;;; Code:

(require 'cl-lib)

(defconst nelisp-sys-version "0.1.0-dev"
  "Semantic version of the nelisp-sys package.

The version is independent from NeLisp core (Doc 130 extraction
criterion 15: the release cadence differs from NeLisp core).")

(define-error 'nelisp-sys-error "nelisp-sys error")

(defun nelisp-sys-version ()
  "Return the nelisp-sys package version string."
  nelisp-sys-version)

;; Front-end modules are required lazily as they are implemented.  During
;; the scaffold stage (130.1) only this aggregator and the adapter stub
;; exist; later phases add `(require 'nelisp-sys-types)' etc. here.

(provide 'nelisp-sys)

;;; nelisp-sys.el ends here
