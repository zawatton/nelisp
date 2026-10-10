;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'nelisp-artifact)
;; Compile raw reader-DSL entry points, as the static reader builder does.
;; Artifact compilation ordinarily selects the boxed Elisp parameter ABI.
(let ((compile-unit (symbol-function 'nelisp-aot-compile-to-link-unit)))
  (cl-letf (((symbol-function 'nelisp-aot-compile-to-link-unit)
             (lambda (&rest args)
               (let ((nelisp-aot-compiler--runtime-entry-params nil))
                 (apply compile-unit args)))))
    (nelisp-artifact-compile-file
     "test/fixtures/aot-evalorder-functions.el" (getenv "AOT_ORDER_ARTIFACT")
     nil nil nil nil nil 'neln 'required)))
