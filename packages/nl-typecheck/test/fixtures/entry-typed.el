;;; entry-typed.el --- fixture: typed module for entry checks -*- lexical-binding: t; nelisp-typed: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun entry-add (a b)
  "Add the integers A and B."
  (declare (ftype (function (integer integer) integer)))
  (+ a b))

(defun entry-greet (name &optional suffix)
  "Greet NAME, optionally followed by SUFFIX."
  (declare (ftype (function (string &optional (or null string)) string)))
  (concat "hi " name (or suffix "")))

(defun entry-label (x)
  "Label X, an integer or a string."
  (declare (ftype (function ((or integer string)) string)))
  (format "<%s>" x))

(defun entry-untyped (x)
  "Untyped: never checked."
  (list x))

(provide 'entry-typed)
;;; entry-typed.el ends here
