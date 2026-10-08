;;; native-buffer-u4a-fixtures.el --- Buffer opcode parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defconst native-buffer-u4a-family
  '((96 point 0) (98 goto-char 1) (99 insert 1) (100 point-max 0)
    (101 point-min 0) (102 char-after 1) (103 following-char 0)
    (104 previous-char 0) (105 current-column 0) (106 indent-to 1)))
(defun native-buffer-u4a-function (row)
  (let ((n (nth 2 row)))
    (make-byte-code (+ n (* n 256)) (unibyte-string (car row) 135) [] (max 1 n))))
(defun native-buffer-u4a-cases (opcode)
  (pcase opcode
    (98 '((2) (-100) (100) (marker) (unset-marker) (bad) (nil) (2.5)))
    (99 '(("中é") (955) ("") (bad) (nil) (-1) (4194304)))
    (102 '((nil) (2) (3) (1) (0) (100) (marker) (unset-marker) (bad) (2.5)))
    (106 '((0) (12) (-1) (bad) (nil) (2.5)))
    (_ '(nil))))
(defun native-buffer-u4a-observe (function arguments position narrow)
  "Observe result/error, text, bounds, point and both marker insertion types."
  (with-temp-buffer
    (insert "aé中\n\tb")
    (when narrow (narrow-to-region 2 6))
    (goto-char position)
    (let* ((left (copy-marker (point))) (right (copy-marker (point) t))
           (args (mapcar (lambda (value)
                           (cond ((eq value 'marker) left)
                                 ((eq value 'unset-marker) (make-marker))
                                 (t value))) arguments))
           (result (condition-case err (apply function args) (error err))))
      (when (markerp result) (setq result (list 'marker (eq result left) (marker-position result))))
      (list result (buffer-string) (point) (point-min) (point-max)
            (marker-position left) (marker-position right)))))
(defun native-buffer-u4a-mutation-error ()
  "Insert, fail at CHAR-AFTER, then refuse a later insertion."
  (make-byte-code 514 (unibyte-string 1 99 136 137 102 136 192 99 136 135)
                  ["forbidden"] 3))
(defun native-buffer-u4a-range-error ()
  "Insert before SUBSTRING reports exact args-out-of-range data."
  (make-byte-code 257 (unibyte-string 99 136 193 194 195 79 136 192 99 136 196 135)
                  ["forbidden" "a" 0 9 finished] 3))
(defun native-buffer-u4a-errors-function ()
  "One body tests both errors after insertion, amortizing native compilation."
  (make-byte-code 514 (unibyte-string 1 99 136 1 192 193 79 136
                                    137 102 136 194 99 136 135)
                  [0 9 "forbidden"] 5))
(provide 'native-buffer-u4a-fixtures)
