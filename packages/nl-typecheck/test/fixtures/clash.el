;;; clash.el --- fixture: a definite declared-type clash -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun clash-double (n)
  "Double the integer N."
  (declare (ftype (function (integer) integer)))
  (* n 2))

(defun clash-caller ()
  "Call `clash-double' with a string literal."
  (clash-double "three"))

(provide 'clash)
;;; clash.el ends here
