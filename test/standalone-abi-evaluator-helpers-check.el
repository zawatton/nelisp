;;; standalone-abi-evaluator-helpers-check.el --- G4 support ABI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(let ((checks 0))
  ;; The same heap object travels through body evaluation, captures and throw.
  (dolist (value (list nil t -19 0 (list 'helper) (vector 'helper)
                       (concat "helper-" "value")))
    (let ((captured value))
      (unless (eq value (funcall (lambda () (garbage-collect) captured)))
        (error "lambda capture changed"))
      (unless (eq value (catch 'helper-tag
                          ((lambda () (garbage-collect)
                             (throw 'helper-tag captured)))))
        (error "throw identity changed"))
      (setq checks (+ checks 2))))
  ;; Grow the lexical frame backing while preserving the outer captured cell.
  (let ((outer (list 'outer)))
    (let ((form '(progn (garbage-collect) outer)) (i 0))
      (while (< i 80)
        (setq form (list 'let (list (list (intern (format "abi-helper-%d" i)) i)) form))
        (setq i (+ i 1)))
      (unless (eq (eval form (list (cons 'outer outer))) outer)
        (error "frame growth lost the outer cell"))
      (setq checks (+ checks 1))))
  ;; Exercise mirror insertion and update through fresh value/function cells.
  (let ((symbol (make-symbol "abi-helper-mirror")) (value (list 'mirror)))
    (set symbol value)
    (fset symbol (lambda () value))
    (put symbol 'helper value)
    (garbage-collect)
    (unless (and (eq (symbol-value symbol) value)
                 (eq (funcall symbol) value) (eq (get symbol 'helper) value))
      (error "mirror entry changed"))
    (set symbol 23)
    (unless (= (symbol-value symbol) 23) (error "mirror update changed"))
    (setq checks (+ checks 2)))
  (unless (null ((lambda ()))) (error "empty body changed"))
  (setq checks (+ checks 1))
  (princ (format "ABI-EVALUATOR-HELPERS checks=%d PASS\n" checks)))
