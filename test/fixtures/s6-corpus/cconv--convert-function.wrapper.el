;;; cconv--convert-function.wrapper.el --- S6.7 corpus wrapper -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Wraps `cconv--convert-function', which needs `cconv-freevars-alist' (a
;; list of (BODY . FREE-VARS) pairs that `cconv-convert' normally maintains
;; during its own recursion; `cconv--convert-function' pops the first
;; entry and asserts its car is `equal' to the BODY argument) and
;; `cconv-var-classification' (see README.md).  Calls
;; `cconv--convert-function' by symbol so the calling phase's own
;; definition runs underneath.

(defvar cconv-freevars-alist)
(defvar cconv-var-classification)
(defvar cconv--dynbound-variables)

(defun s6-corpus--cconv--convert-function
    (freevars-alist args body env parentform &optional docstring)
  (let ((cconv-freevars-alist freevars-alist)
        (cconv-var-classification nil)
        (cconv--dynbound-variables nil))
    (cconv--convert-function args body env parentform docstring)))

;;; cconv--convert-function.wrapper.el ends here
