;;; typed-warning.el --- fixture: a warning in a typed module -*- lexical-binding: t; nelisp-typed: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun typed-warning-first (n)
  "Take the car of a let-bound integer N (an inferred clash)."
  (let ((x 5))
    (car x)))

(provide 'typed-warning)
;;; typed-warning.el ends here
