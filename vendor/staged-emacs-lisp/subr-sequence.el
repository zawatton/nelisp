;;; subr-sequence.el --- selected GNU Emacs sequence forms -*- lexical-binding:t; -*-

;; Copyright (C) 1985-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; Source: GNU Emacs 31.1, lisp/subr.el, upstream commit
;; a360712c9d272d950d8d8255ef74570f7e90b7d9.
;; The forms below are extracted verbatim from the pinned source file whose
;; SHA-256 is a51d2fb5d52749133cc95904d24aaceb6887a591b0f6c07ee255c1e876d20b96.

;;; Code:

(defun internal--effect-free-fun-arg-p (x)
  ;; FIXME: Rename it to `macroexp-FOO-p' and give it a proper docstring
  ;; which explains the finer difference with `macroexp-copyable-p'
  ;; (and maybe adjust the docstring of `macroexp-copyable-p' accordingly).
  (or (closurep x) (memq (car-safe x) '(function quote))))

(defun drop-while (pred list)
  "Skip initial elements of LIST satisfying PRED and return the rest."
  (declare (compiler-macro
            (lambda (form)
              (let* ((tail (make-symbol "tail")))
                (if (not (internal--effect-free-fun-arg-p pred))
                    ;; Don't inline since it would just duplicate the code
                    ;; without allowing any more optimizations.
                    form
                  `(let ((,tail ,list))
                     (while (and ,tail (funcall ,pred (car ,tail)))
                       (setq ,tail (cdr ,tail)))
                     ,tail))))))
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(defun member-if (pred list)
  "Non-nil if PRED is true for at least one element in LIST.
Returns the suffix of LIST starting with the first element that
satisfies PRED, or nil if none do.

Compatibility note: this function replaces `cl-member-if' but does not
support the latter's `:key KEY-FN' argument.  It is better to compose
any KEY-FN into PRED.  For example, you can replace

    (cl-member-if #\\='foo items :key #\\='bar)

with

    (member-if (lambda (x) (foo (bar x))) items)"
  (declare (compiler-macro
            (lambda (form)
              (if (not (internal--effect-free-fun-arg-p pred))
                  ;; Don't inline since it would just duplicate the code
                  ;; without allowing any more optimizations.
                  form
                (let* ((x (make-symbol "x")))
                  `(drop-while (lambda (,x)
                                 (not (funcall ,pred ,x)))
                               ,list))))))
  (drop-while (lambda (x) (not (funcall pred x))) list))

(defalias 'any #'member-if)
