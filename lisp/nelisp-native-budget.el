;;; nelisp-native-budget.el --- Lifetime native mapping reservations -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Reservations are conservative and never refunded, including failed maps.
;; Lisp heap and shared-library dependencies are outside this code budget.
;;; Code:
(define-error 'nelisp-native-budget-exhausted "Native code mapping budget exhausted")
(defvar nelisp-native-budget-handle-limit 512)
(defvar nelisp-native-budget-byte-limit (* 128 1024 1024))
(defvar nelisp-native-budget--handles 0)
(defvar nelisp-native-budget--bytes 0)
(defun nelisp-native-budget-check (bytes)
  "Refuse a new compilation/mapping when BYTES cannot fit its lifetime budget."
  (unless (and (integerp bytes) (> bytes 0)
               (< nelisp-native-budget--handles nelisp-native-budget-handle-limit)
               (<= (+ bytes nelisp-native-budget--bytes) nelisp-native-budget-byte-limit))
    (signal 'nelisp-native-budget-exhausted (list bytes)))
  t)
(defun nelisp-native-budget-reserve (bytes)
  "Reserve one mapping owner and BYTES before mapping any code."
  (nelisp-native-budget-check bytes)
  (setq nelisp-native-budget--handles (1+ nelisp-native-budget--handles)
        nelisp-native-budget--bytes (+ bytes nelisp-native-budget--bytes)))
(defun nelisp-native-budget--uint (bytes offset width)
  "Read a bounded little-endian unsigned integer from BYTES."
  (unless (<= (+ offset width) (length bytes)) (error "Truncated ELF header"))
  (let ((value 0))
    (dotimes (i width)
      (setq value (+ value (ash (aref bytes (+ offset i)) (* 8 i)))))
    value))
(defun nelisp-native-budget-elf-bytes (file)
  "Count page-rounded PT_LOAD extents in an authenticated ELF64 FILE."
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file) (buffer-string))) (total 0))
    (unless (and (>= (length bytes) 64) (equal (substring bytes 0 6) "\177ELF\2\1"))
      (error "Native budget requires little-endian ELF64"))
    (let ((offset (nelisp-native-budget--uint bytes 32 8))
          (width (nelisp-native-budget--uint bytes 54 2))
          (count (nelisp-native-budget--uint bytes 56 2)))
      (unless (and (>= width 56) (> count 0) (<= count 4096)
                   (<= (+ offset (* width count)) (length bytes)))
        (error "Invalid ELF program headers"))
      (dotimes (i count)
        (let ((start (+ offset (* i width))))
          (when (= 1 (nelisp-native-budget--uint bytes start 4))
            (let ((address (nelisp-native-budget--uint bytes (+ start 16) 8))
                  (size (nelisp-native-budget--uint bytes (+ start 40) 8)))
              (setq total (+ total (* 4096 (/ (+ (mod address 4096) size 4095) 4096)))))))))
    (unless (> total 0) (error "ELF has no PT_LOAD code reservation"))
    total))
(provide 'nelisp-native-budget)
;;; nelisp-native-budget.el ends here
