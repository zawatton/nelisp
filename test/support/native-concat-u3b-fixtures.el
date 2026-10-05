;;; native-concat-u3b-fixtures.el --- Sequence opcode parity corpus -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(defun native-concat-u3b-function (opcode count)
  "Build sequence bytecode with the GNU unsigned eight-bit count boundary."
  (unless (and (memq opcode '(80 81 82 176 177)) (integerp count) (<= 0 count 255)
               (or (memq opcode '(176 177)) (= count (- opcode 78))))
    (error "Invalid sequence opcode/count: %S/%S" opcode count))
  (if (> count 4)
      (make-byte-code 514
                      (concat (apply #'unibyte-string (make-list (- count 2) 137))
                              (unibyte-string opcode count 135)) [] count)
    (make-byte-code (+ count (* count 256))
                    (if (memq opcode '(176 177)) (unibyte-string opcode count 135)
                      (unibyte-string opcode 135)) [] (max 1 count))))
(defun native-concat-u3b-fixtures ()
  '((80 2) (81 3) (82 4) (176 0) (176 1) (176 4) (176 33) (176 255)
    (177 0) (177 1) (177 4) (177 33) (177 255)))
(defun native-concat-u3b-args (opcode count)
  (cl-subseq (if (> count 4) '("L" "r")
              (if (= opcode 177) '("L" 955 "尾" "R")
                '("L" [955] (23614) "R"))) 0 (if (> count 4) 2 count)))
(defun native-concat-u3b-values (args count)
  (if (> count 4) (cons (car args) (make-list (1- count) (cadr args))) args))
(defun native-concat-u3b-observe (fn args opcode)
  "Return value, byte mode and buffer/point effects, including signal data."
  (with-temp-buffer
    (insert "<>") (goto-char 2)
    (let ((value (condition-case err (apply fn args) (error err))))
      (list value (and (stringp value) (multibyte-string-p value))
            (buffer-string) (point) opcode))))
(defun native-concat-u3b-mutation-error (opcode)
  "Mutate a cell, perform sequence effects, fail, then forbid a second mutation."
  (make-byte-code 257
                  (concat (unibyte-string 137 192 160 136 193 194)
                          (if (= opcode 177) (unibyte-string 177 2) (unibyte-string 80))
                          (unibyte-string 136 137 195 160 136 135))
                  [changed "first" bad forbidden] 3))
(defun native-concat-u3b-chain ()
  "Two allocating concat operations keep the first result live across GC."
  (make-byte-code 514 (unibyte-string 1 1 80 80 135) [] 4))
(provide 'native-concat-u3b-fixtures)
