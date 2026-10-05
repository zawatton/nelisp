;;; native-buffer-u4b-fixtures.el --- Buffer opcode parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(defconst native-buffer-u4b-family
  '((108 eolp 0) (109 eobp 0) (110 bolp 0) (111 bobp 0)
    (112 current-buffer 0) (113 set-buffer 1) (116 interactive-p 0 dynamic)
    (117 forward-char 1) (118 forward-word 1)))
(defun native-buffer-u4b-function (row)
  (let ((n (nth 2 row)))
    (make-byte-code (+ n (* n 256)) (unibyte-string (car row) 135) [] (max 1 n))))
(defun native-buffer-u4b-cases (opcode)
  (pcase opcode
    (113 '((self) (other) (other-name) (missing) (killed) (bad) (nil) (2.5)))
    ((or 117 118) '((nil) (0) (1) (-1) (2) (-2) (100) (-100) (bad) (2.5) (marker) (unset-marker)))
    (_ '(nil))))
(defun native-buffer-u4b-observe (function arguments position narrow)
  "Observe identity, exact errors, text, bounds, point and both marker types."
  (with-temp-buffer
    (insert "aé中\n\tb")
    (when narrow (narrow-to-region 2 6))
    (goto-char position)
    (let* ((self (current-buffer))
           (other (generate-new-buffer " *u4b-other*"))
           (dead (generate-new-buffer " *u4b-dead*"))
           (left (copy-marker (point))) (right (copy-marker (point) t)))
      (kill-buffer dead)
      (unwind-protect
          (let* ((args (mapcar (lambda (v)
                                (pcase v ('self self) ('other other)
                                  ('other-name (buffer-name other))
                                  ('missing " *u4b-missing*") ('killed dead)
                                  ('marker left) ('unset-marker (make-marker)) (_ v))) arguments))
                 (result (condition-case e (apply function args) (error e))))
            ;; Error data may carry a marker from this invocation's buffer.
            ;; Preserve its position and operand identity before that buffer dies.
            (cl-labels ((normalize (x)
                          (cond ((markerp x) (list 'marker (eq x left) (marker-position x)))
                                ((bufferp x) (cond ((eq x self) 'self) ((eq x other) 'other) (t 'unknown)))
                                ((consp x) (cons (normalize (car x)) (normalize (cdr x))))
                                (t x))))
              (setq result (normalize result)))
            (let ((selected (if (eq (current-buffer) self) 'self 'other)))
              (set-buffer self)
              (list result selected (buffer-string) (point) (point-min) (point-max)
                    (marker-position left) (marker-position right))))
        (set-buffer self)
        (kill-buffer other)))))
(defun native-buffer-u4b-effect-function ()
  "Insert before a motion error; a subsequent insertion must never run."
  (make-byte-code 257 (unibyte-string 192 99 136 137 117 136 193 99 136 135)
                  ["先" "forbidden"] 2))
(provide 'native-buffer-u4b-fixtures)
