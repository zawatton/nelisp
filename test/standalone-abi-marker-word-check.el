;;; standalone-abi-marker-word-check.el -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(with-temp-buffer
  (insert "abcdef")
  (let ((m (make-marker)))
    (set-marker m 3)
    (unless (equal (list (+ m 0) (- m 1) (* m 2) (/ m 3)
                         (max m 1) (min m 4))
                   '(3 2 6 1 3 3))
      (error "numeric marker WORD changed"))
    (garbage-collect)
    (unless (= (+ 0 m 1) 4) (error "marker lost at GC"))
    (set-marker m nil)
    (unless (condition-case nil (progn (+ m 0) nil) (error t))
      (error "detached marker accepted"))))
(princ "ABI-MARKER-WORD checks=8 PASS\n")
