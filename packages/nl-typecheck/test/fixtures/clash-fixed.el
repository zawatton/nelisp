;;; clash.el --- fixture: the clash fixed -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun clash-double (n)
  "Double the integer N."
  (declare (ftype (function (integer) integer)))
  (* n 2))

(defun clash-caller ()
  "Call `clash-double' with an integer."
  (clash-double 3))

(provide 'clash)
;;; clash.el ends here
