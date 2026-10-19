;;; standalone-abi-slot-producer-word-check.el --- Slot producer parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun abi-slot-gc-value (value) (garbage-collect) value)
(defun abi-slot-arguments (a b &optional c &rest rest) (list a b c rest))
(defun abi-slot-callout (value)
  (let ((first (abi-slot-gc-value value)))
    (abi-slot-gc-value nil)
    first))

(let ((checked 0))
  (dolist (value (list nil t 0 -7 1.25 "type" 'abi-slot-tag '(a . b)
                      [nil t -7] 2305843009213693952
                      9223372036854775807 -9223372036854775808))
    ;; Zero data slots still own the permanent type WORD. Heap types must
    ;; remain valid after the source record itself becomes unreachable.
    (dolist (count '(0 3))
      (let* ((r (make-record value count nil)) (saved (aref r 0)))
        (unless (equal saved value) (error "make-record type changed"))
        (unless (= (length r) (1+ count)) (error "record length changed"))
        (garbage-collect)
        (unless (equal (aref r 0) value) (error "record type lost at GC"))
        (setq r nil)
        (garbage-collect)
        (unless (equal saved value) (error "escaped record type lost at GC"))
        (setq checked (+ checked 4))))
    (let ((r (record value nil t -7)))
      (unless (equal (aref r 0) value) (error "record type changed"))
      (unless (equal (list (aref r 1) (aref r 2) (aref r 3)) '(nil t -7))
        (error "record data indexes changed"))
      (when (fboundp 'nelisp--record-expand)
        (unless (equal (nelisp--record-expand r)
                       (cons (cond ((vectorp value) (append value nil))
                                   (t value)) '(nil t -7)))
          (error "record expansion type changed")))
      (setq checked (+ checked 2)))
    (unless (equal (abi-slot-callout value) value) (error "callout result reused"))
    ;; The first argument survives evaluation of later arguments and GC.
    ;; Exercise optional/rest arities and a VM call from another VM call.
    (unless (equal (abi-slot-arguments (abi-slot-gc-value value)
                                     (abi-slot-gc-value t) nil value -7)
                   (list value t nil (list value -7)))
      (error "evaluated argument WORDs changed"))
    (unless (equal (apply #'abi-slot-arguments (list value t)) (list value t nil nil))
      (error "optional argument WORDs changed"))
    (setq checked (+ checked 3)))
  ;; This globals record predates startup: its type WORD must relocate from
  ;; the automatic cold image. A2 crashes here; newly allocated records pass.
  (when (fboundp 'nelisp--env-capture-roots)
    (unless (symbolp (aref (car (nelisp--env-capture-roots)) 0))
      (error "restored globals type WORD did not relocate")))
  (setq checked (1+ checked))
  ;; Also exercise source-level buffer providers; the bytecode corpus covers
  ;; their generic native aref gateway separately.
  (unless (equal (with-temp-buffer
                   (insert "hello") (goto-char (point-min)) (forward-char 2)
                   (list (point) (buffer-string) (char-after)))
                 '(3 "hello" 108))
    (error "cold record type WORD did not relocate"))
  (setq checked (1+ checked))
  (let ((events nil))
    (condition-case nil
        (abi-slot-arguments (progn (setq events (cons 1 events)) nil)
                            (progn (setq events (cons 2 events)) (error "stop"))
                            (setq events (cons 3 events)))
      (error nil))
    (unless (equal events '(2 1)) (error "argument evaluation repeated or reordered"))
    (setq checked (1+ checked)))
  (when (boundp 'nelisp--fx-table)
    (dolist (fn '(abi-slot-gc-value abi-slot-arguments abi-slot-callout))
      (unless (vectorp (gethash (nthcdr 3 (symbol-function fn)) nelisp--fx-table))
        (error "slot producer fixture was not compiled: %S" fn))))
  (princ (format "ABI-SLOT-PRODUCER checked=%d PASS\n" checked)))
nil
