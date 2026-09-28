;;; byte-compile-lambda.wrapper.el --- S6.9 corpus wrapper -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Wraps `byte-compile-lambda', which needs `byte-compile--\#$' and the
;; rest of `byte-compile-close-variables''s bindings; it calls
;; `byte-compile-top-level' itself, which re-establishes depth/output/
;; constants, so those need not be pre-bound (see README.md).  Unlike the
;; other wrappers, this one returns `byte-compile-lambda''s own return
;; value directly: on success a byte-code function object, which Emacs's
;; `equal' (unlike `eq') compares structurally.
;;
;; The defvar+macro below must live in THIS file, not a shared one loaded
;; via a separate `load' call: a bodyless `(defvar SYM)' only marks SYM as
;; dynamically-scoped for the rest of the file it is evaluated in, so a
;; `let' written in a different file that merely `load's this macro's
;; definition would bind SYM lexically instead (confirmed empirically:
;; factoring this into a shared s6-corpus-env.el loaded by `load' from each
;; wrapper file produced `void-variable byte-compile--\#$').

(defvar byte-compile--for-effect)
(defvar byte-compile-free-references)
(defvar byte-compile-free-assignments)
(defvar byte-compile--\#$)

(defmacro s6-corpus--byte-compile-env (&rest body)
  "Run BODY with bytecomp.el's own top-level compilation state bound."
  `(let ((byte-compile--for-effect nil)
         (byte-compile-constants nil)
         (byte-compile-variables nil)
         (byte-compile-tag-number 0)
         (byte-compile-depth 0)
         (byte-compile-maxdepth 0)
         (byte-compile--lexical-environment nil)
         (byte-compile-reserved-constants 0)
         (byte-compile-output nil)
         (byte-compile-jump-tables nil)
         (byte-compile-free-references nil)
         (byte-compile-free-assignments nil)
         (byte-compile-macro-environment (copy-alist byte-compile-initial-macro-environment))
         (byte-compile-function-environment nil)
         (byte-compile-bound-variables nil)
         (byte-compile-lexical-variables nil)
         (byte-compile-const-variables nil)
         (byte-compile--\#$ nil)
         (byte-native-compiling nil))
     ,@body))

(defun s6-corpus--byte-compile-lambda (fun &optional reserved-csts)
  (s6-corpus--byte-compile-env
   (byte-compile-lambda fun reserved-csts)))

;;; byte-compile-lambda.wrapper.el ends here
