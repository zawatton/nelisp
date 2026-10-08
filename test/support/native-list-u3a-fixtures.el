;;; native-list-u3a-fixtures.el --- List opcode parity corpus -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defun native-list-u3a-function (opcode count)
  "Build a valid list fixture.  GNU LISTN has an unsigned eight-bit count."
  (unless (and (memq opcode '(67 68 69 70 175))
               (integerp count) (<= 0 count 255)
               (or (= opcode 175) (= count (- opcode 66))))
    (error "Invalid list opcode/count: %S/%S" opcode count))
  (if (> count 4)
      ;; Repeated references exercise maximum depth without exceeding the
      ;; independent authenticated-root admission limit. Keep a distinct first
      ;; and last value to expose operand-order and aliasing errors.
      (make-byte-code 514
                      (concat (apply #'unibyte-string (make-list (- count 2) 137))
                              (unibyte-string 175 count 135)) [] count)
    (make-byte-code (+ count (* count 256))
                    (if (= opcode 175) (unibyte-string opcode count 135)
                      (unibyte-string opcode 135)) [] (max 1 count))))
(defun native-list-u3a-fixtures ()
  "Return opcode/count rows, including both ends of the encoded count range."
  '((67 1) (68 2) (69 3) (70 4) (175 0) (175 1) (175 4) (175 33) (175 255)))
(defun native-list-u3a-args (count)
  (let ((values (list (cons 'left 'tail) (vector 'right) "third" nil)))
    (if (> count 4) (list (car values) (cadr values))
      (cl-subseq values 0 count))))
(defun native-list-u3a-expected-values (arguments count)
  (if (> count 4) (cons (car arguments) (make-list (1- count) (cadr arguments)))
    arguments))
(defun native-list-u3a-mutation-error ()
  "Mutate, allocate a list, then signal before the final mutation."
  (make-byte-code 514 (unibyte-string 1 192 160 136 1 1 68 136
                                    193 64 136 1 194 160 136 135)
                  [changed 7 forbidden] 4))
(defun native-list-u3a-join-function ()
  "Select a boxed value at a diamond join, then build a long list."
  (make-byte-code 257
                  (concat (unibyte-string 131 7 0 192 130 8 0 193)
                          (apply #'unibyte-string (make-list 32 137))
                          (unibyte-string 175 33 135)) [left right] 33))
(provide 'native-list-u3a-fixtures)
