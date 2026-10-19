;;; standalone-abi-capture-walk-check.el --- Capture traversal parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(let ((checks 0))
  ;; Enough bindings to exercise hash-backed frames, shadowing, and a shared
  ;; captured cell. The optional formal must be excluded from the filter.
  (unless (equal
           (eval '(let ((a0 0) (a1 1) (a2 2) (a3 3) (a4 4) (a5 5)
                        (a6 6) (a7 7) (a8 8) (a9 9) (a10 10) (a11 11)
                        (a12 12) (a13 13) (a14 14) (a15 15))
                    (let ((a0 41))
                      (let ((f (lambda (&optional delta)
                                 (setq a0 (+ a0 (or delta 0)))
                                 (+ a0 a15))))
                        (list (funcall f) (funcall f 2)
                              (progn (garbage-collect) (funcall f)))))) t)
           '(56 58 58))
    (error "wide capture traversal changed"))
  (setq checks (1+ checks))
  ;; Rest formals and repeated free references exercise both filter walks.
  (unless (equal
           (eval '(let ((free (list 'saved)))
                    (let ((f (lambda (head &rest rest) (list free head rest free))))
                      (garbage-collect)
                      (funcall f 'first 'second 'third))) t)
           '((saved) first (second third) (saved)))
    (error "rest lambda filter changed"))
  (setq checks (1+ checks))
  ;; A lexical scope boundary must stop an outer binding.
  (unless (equal
           (eval '(let ((hidden 'outer))
                    (eval '(let ((f (lambda () hidden)))
                             (garbage-collect) (funcall f))
                          '((hidden . inner)))) t)
           'inner)
    (error "explicit capture scope changed"))
  (setq checks (1+ checks))
  (princ (format "ABI-CAPTURE-WALK checks=%d PASS\n" checks)))
(kill-emacs 0)
