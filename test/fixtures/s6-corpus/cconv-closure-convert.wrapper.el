;;; cconv-closure-convert.wrapper.el --- S6.6 corpus wrapper -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; `cconv-closure-convert' let-binds its own working state, but the
;; lambda/closure paths of `cconv-convert' it calls push onto
;; `byte-compile-lexical-variables', which cconv.el only declares with a
;; bodyless `defvar'; bytecomp.el gives it the global value nil.  The host
;; oracle always has that value (its driver loads `comp', which loads
;; bytecomp), while a NeLisp process that has loaded only cconv does not
;; -- a fresh `emacs -Q --batch' with only cconv loaded signals
;; `void-variable' there too.  This file supplies bytecomp's own default
;; identically in all three phases (a no-op on the host) and calls the
;; function by symbol so each phase's own definition runs.

(defvar byte-compile-lexical-variables nil)

(defun s6-corpus--cconv-closure-convert (&rest args)
  (apply #'cconv-closure-convert args))

;;; cconv-closure-convert.wrapper.el ends here
