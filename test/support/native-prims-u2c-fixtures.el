;;; native-prims-u2c-fixtures.el --- Exact numeric/predicate fixtures -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defconst native-prims-u2c-family
  '((57 symbolp 1) (58 consp 1) (59 stringp 1) (60 listp 1) (61 eq 2) (63 not 1)
    (83 1- 1) (84 1+ 1) (85 = 2) (86 > 2) (87 < 2) (88 <= 2) (89 >= 2)
    (90 - 2) (91 - 1) (92 + 2) (93 max 2) (94 min 2) (95 * 2)
    (164 nconc 2) (165 / 2) (166 % 2)
    (167 numberp 1) (168 integerp 1)))
(defun native-prims-u2c-function (row)
  "Build the precise opcode without public calls or optimizer substitutions."
  (let ((argc (nth 2 row)))
    (make-byte-code (+ argc (lsh argc 8)) (if (= (car row) 92)
                        ;; Select the general F1 lane without changing ADD's
                        ;; existing single-opcode arithmetic admission.
                        (unibyte-string 92 137 63 136 135)
                      (unibyte-string (car row) 135)) [] (1+ argc))))
(defun native-prims-u2c-marker ()
  "Return a fresh positioned marker."
  (let ((marker (make-marker))) (set-marker marker 1) marker))
(defun native-prims-u2c-reset () nil)
(defun native-prims-u2c-cases (opcode)
  "Return fresh numeric, type, marker, zero-divisor and mutation operands."
  (cond
   ((= opcode 164)
    (list (list nil '(b)) (list (list 'a) (list 'b))
          (list (cons 'a 'tail) (cons 'b 'last)) (list (list 'a) 7)
          (list 7 nil) (list 'bad '(b))))
   ((= opcode 61)
    (let ((same (cons 'a 'tail)))
      (list (list same same) (list (cons 'a 'tail) (cons 'a 'tail))
            (list nil nil) (list 7 7) (list 'a 'b))))
   ((memq opcode '(57 58 59 60 63 167 168))
    (mapcar #'list (list nil t 'symbol 7 2.5 9223372036854775808
                        "text" [a b] (cons 'a 'tail) (native-prims-u2c-marker))))
   ((memq opcode '(83 84 91))
    (mapcar #'list (list 7 -7 2.5 9223372036854775808
                        (native-prims-u2c-marker) nil 'bad (cons 'bad 'tail))))
   (t
    (list (list 7 2) (list -7 2) (list 7 -2) (list 7 2.0)
          (list 2.5 7) (list 7 0) (list 0 0) (list 7 0.0)
          (list 9223372036854775808 3) (list 3 9223372036854775808)
          (list 9223372036854775808 9223372036854775808)
          (list (native-prims-u2c-marker) 2) (list 2 (native-prims-u2c-marker))
          (list nil 2) (list 2 nil) (list 'bad 'other)
          (list (cons 'bad 'tail) 2) (list 2 (cons 'bad 'tail))))))
(defun native-prims-u2c-normalize (value)
  "Normalize independent markers and IEEE special values for comparison."
  (cond ((markerp value) (list 'marker (marker-position value)
                               (and (marker-buffer value) t)))
        ((floatp value) (list 'float (format "%s" value)))
        ((consp value) (cons (native-prims-u2c-normalize (car value))
                            (native-prims-u2c-normalize (cdr value))))
        (t value)))
(defun native-prims-u2c-observe (function args)
  "Observe exact condition/data, mutations and result identity."
  (let* ((answer (condition-case err (list 'value (apply function args))
                   (error (cons 'signal err))))
         (result (cadr answer))
         (identity (when (eq (car answer) 'value)
                     (list (eq result (car args)) (eq result (cadr args))))))
    (list (native-prims-u2c-normalize answer)
          (native-prims-u2c-normalize args) identity)))
(defun native-prims-u2c-observe-cyclic-tail (function empty-first)
  "Observe a cyclic second operand without recursively printing it."
  (let ((cycle (list 'self)) (first (unless empty-first (list 'head))))
    (setcdr cycle cycle)
    (let ((result (funcall function first cycle)))
      (list (eq result (if empty-first cycle first))
            (if empty-first (eq (cdr result) cycle) (eq (cdr first) cycle))
            (eq (cdr cycle) cycle)))))
(provide 'native-prims-u2c-fixtures)
