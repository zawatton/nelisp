;;; native-corpus-u10-state.el --- Compile/load mutation witnesses -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defun u10-value-snapshot (value)
  "Copy the finite fixture's contents, including mutable switch tables."
  (cond
   ((stringp value) (list :string (multibyte-string-p value) (copy-sequence value)))
   ((hash-table-p value)
    (let (entries)
      (maphash (lambda (key item)
                 (push (list (u10-value-snapshot key) (u10-value-snapshot item)) entries)) value)
      (list :hash-table (hash-table-test value) (hash-table-count value) entries)))
   ((consp value) (list :cons (u10-value-snapshot (car value)) (u10-value-snapshot (cdr value))))
   ((vectorp value) (list :vector (mapcar #'u10-value-snapshot (append value nil))))
   (t value)))

(defun u10-function-state (function)
  "Retain identities and independent content witnesses after VM setup."
  (let (fields)
    (dotimes (index (length function))
      (push (u10-value-snapshot (aref function index)) fields))
    (list function (aref function 1) (aref function 2) (nreverse fields))))

(defun u10-function-state-check (function state)
  "Refuse function replacement and in-place code/constant/metadata mutation."
  (unless (and (eq function (nth 0 state))
               (eq (aref function 1) (nth 1 state))
               (eq (aref function 2) (nth 2 state))
               (equal (nth 3 (u10-function-state function)) (nth 3 state)))
    (error "U10 compile/load mutated original byte code"))
  t)

(provide 'native-corpus-u10-state)
