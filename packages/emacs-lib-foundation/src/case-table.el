;;; case-table.el --- lightweight case-table support for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Minimal pure-Elisp case-table and char-table substrate.  The standalone
;; NeLisp bootstrap has nil stubs for many char-table names; this file
;; replaces them with an ASCII-oriented implementation sufficient for the
;; vendor case-table API and early i18n/bootstrap loads.

;;; Code:

(defconst case-table--size 256
  "Number of character slots in the lightweight char-table.")

(defconst case-table--extra-slots 3
  "Number of extra slots carried by a lightweight char-table.")

(defun case-table--standalone-p ()
  "Return non-nil under standalone NeLisp."
  (not (boundp 'emacs-version)))

(defun case-table--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this facade."
  (if (case-table--standalone-p)
      t
    (not (fboundp symbol))))

(defun case-table--extra-index (slot)
  "Return vector index for extra SLOT."
  (+ case-table--size slot))

(defun case-table--identity-table ()
  "Return a fresh lightweight char-table with identity entries."
  (let ((table (make-vector (+ case-table--size case-table--extra-slots) nil))
        (i 0))
    (while (< i case-table--size)
      (aset table i i)
      (setq i (1+ i)))
    table))

(defun case-table--copy-vector (vector)
  "Return a shallow copy of VECTOR."
  (let* ((len (length vector))
         (copy (make-vector len nil))
         (i 0))
    (while (< i len)
      (aset copy i (aref vector i))
      (setq i (1+ i)))
    copy))

(defun case-table--make-char-table (&optional _subtype init)
  "Make a lightweight char-table filled with INIT or identity mappings."
  (if init
      (make-vector (+ case-table--size case-table--extra-slots) init)
    (case-table--identity-table)))

(defun case-table--char-table-p (object)
  "Return non-nil when OBJECT is a lightweight char-table."
  (and (vectorp object)
       (= (length object) (+ case-table--size case-table--extra-slots))))

(defun case-table--char-table-range (table range)
  "Return TABLE entry at RANGE."
  (cond
   ((integerp range) (aref table range))
   ((eq range t) nil)
   ((consp range) (aref table (car range)))
   (t nil)))

(defun case-table--set-char-table-range (table range value)
  "Set TABLE RANGE to VALUE."
  (cond
   ((integerp range)
    (aset table range value))
   ((consp range)
    (let ((i (car range))
          (end (cdr range)))
      (while (<= i end)
        (aset table i value)
        (setq i (1+ i)))))
   ((eq range t)
    nil))
  value)

(defun case-table--char-table-extra-slot (table slot)
  "Return extra SLOT from TABLE."
  (aref table (case-table--extra-index slot)))

(defun case-table--set-char-table-extra-slot (table slot value)
  "Set extra SLOT in TABLE to VALUE."
  (aset table (case-table--extra-index slot) value))

(defun case-table--get-extra-slot (table slot)
  "Return extra SLOT from TABLE, lightweight or host."
  (if (case-table--char-table-p table)
      (case-table--char-table-extra-slot table slot)
    (char-table-extra-slot table slot)))

(defun case-table--put-extra-slot (table slot value)
  "Set extra SLOT in TABLE, lightweight or host, to VALUE."
  (if (case-table--char-table-p table)
      (case-table--set-char-table-extra-slot table slot value)
    (set-char-table-extra-slot table slot value)))

(defun case-table--map-char-table (function table)
  "Call FUNCTION for every non-nil character entry in TABLE."
  (let ((i 0))
    (while (< i case-table--size)
      (let ((value (aref table i)))
        (when value
          (funcall function i value)))
      (setq i (1+ i)))))

(when (case-table--standalone-p)
  (fset 'make-char-table #'case-table--make-char-table)
  (fset 'char-table-p #'case-table--char-table-p)
  (fset 'char-table-range #'case-table--char-table-range)
  (fset 'set-char-table-range #'case-table--set-char-table-range)
  (fset 'char-table-extra-slot #'case-table--char-table-extra-slot)
  (fset 'set-char-table-extra-slot #'case-table--set-char-table-extra-slot)
  (fset 'map-char-table #'case-table--map-char-table)
  (fset 'set-char-table-parent (lambda (&rest _) nil)))

(when (case-table--install-function-p 'make-char-table)
  (defalias 'make-char-table #'case-table--make-char-table))

(when (case-table--install-function-p 'char-table-p)
  (defalias 'char-table-p #'case-table--char-table-p))

(when (case-table--install-function-p 'char-table-range)
  (defalias 'char-table-range #'case-table--char-table-range))

(when (case-table--install-function-p 'set-char-table-range)
  (defalias 'set-char-table-range #'case-table--set-char-table-range))

(when (case-table--install-function-p 'char-table-extra-slot)
  (defalias 'char-table-extra-slot #'case-table--char-table-extra-slot))

(when (case-table--install-function-p 'set-char-table-extra-slot)
  (defalias 'set-char-table-extra-slot #'case-table--set-char-table-extra-slot))

(when (case-table--install-function-p 'map-char-table)
  (defalias 'map-char-table #'case-table--map-char-table))

(defvar case-table--standard (case-table--identity-table)
  "Standard lightweight case table.")

(defvar case-table--current case-table--standard
  "Current lightweight case table.")

(defvar case-table--standard-syntax-table (case-table--identity-table)
  "Placeholder syntax table for case-table mutation helpers.")

(when (case-table--standalone-p)
  (fset 'standard-case-table (lambda () case-table--standard))
  (fset 'current-case-table (lambda () case-table--current))
  (fset 'set-standard-case-table
        (lambda (table)
          (setq case-table--standard table
                case-table--current table)
          (case-table--ensure-extra-slots table)
          table))
  (fset 'set-case-table
        (lambda (table)
          (setq case-table--current table)
          (case-table--ensure-extra-slots table)
          table))
  (fset 'standard-syntax-table
        (lambda () case-table--standard-syntax-table))
  (fset 'modify-syntax-entry
        (lambda (_char _newentry &optional _table) nil)))

(when (case-table--install-function-p 'standard-case-table)
  (defun standard-case-table ()
    "Return the standard lightweight case table."
    case-table--standard))

(when (case-table--install-function-p 'current-case-table)
  (defun current-case-table ()
    "Return the current lightweight case table."
    case-table--current))

(when (case-table--install-function-p 'set-standard-case-table)
  (defun set-standard-case-table (table)
    "Set the standard lightweight case table to TABLE."
    (setq case-table--standard table
          case-table--current table)
    (case-table--ensure-extra-slots table)
    table))

(when (case-table--install-function-p 'set-case-table)
  (defun set-case-table (table)
    "Set the current lightweight case table to TABLE."
    (setq case-table--current table)
    (case-table--ensure-extra-slots table)
    table))

(when (case-table--install-function-p 'standard-syntax-table)
  (defun standard-syntax-table ()
    "Return the lightweight standard syntax table placeholder."
    case-table--standard-syntax-table))

(when (case-table--install-function-p 'modify-syntax-entry)
  (defun modify-syntax-entry (_char _newentry &optional _table)
    "Accept syntax mutations for compatibility."
    nil))

(defun case-table--ensure-extra-slots (case-table)
  "Ensure CASE-TABLE has up/canon/eqv extra slots."
  (unless (case-table--get-extra-slot case-table 0)
    (let ((up (case-table--identity-table))
          (i 0))
      (while (< i case-table--size)
        (let ((down (aref case-table i)))
          (when (and (integerp down)
                     (>= down 0)
                     (< down case-table--size))
            (aset up down i)))
        (setq i (1+ i)))
      (case-table--put-extra-slot case-table 0 up)))
  (unless (case-table--get-extra-slot case-table 1)
    (case-table--put-extra-slot case-table 1 (case-table--identity-table)))
  (unless (case-table--get-extra-slot case-table 2)
    (case-table--put-extra-slot case-table 2 (case-table--identity-table)))
  case-table)

(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (message "case-table: lightweight ASCII table"))

(defun case-table-get-table (case-table table)
  "Return TABLE from CASE-TABLE.
TABLE can be `down', `up', `eqv', or `canon'."
  (let ((slot (cdr (assq table '((up . 0) (canon . 1) (eqv . 2))))))
    (cond
     ((eq table 'down) case-table)
     ((or (case-table--standalone-p)
          (case-table--char-table-p case-table))
      (case-table--ensure-extra-slots case-table)
      (case-table--get-extra-slot case-table slot))
     (t
      (or (case-table--get-extra-slot case-table slot)
          (let ((old (standard-case-table)))
            (unwind-protect
                (progn
                  (set-standard-case-table case-table)
                  (case-table--get-extra-slot case-table slot))
              (unless (eq case-table old)
                (set-standard-case-table old)))))))))

(defun get-upcase-table (case-table)
  "Return the upcase table of CASE-TABLE."
  (case-table-get-table case-table 'up))

(defun copy-case-table (case-table)
  "Return a shallow copy of CASE-TABLE with derived slots invalidated."
  (let ((copy (if (case-table--standalone-p)
                  (case-table--copy-vector case-table)
                (copy-sequence case-table)))
        (up (case-table--get-extra-slot case-table 0)))
    (when up
      (case-table--put-extra-slot
       copy 0
       (if (case-table--standalone-p)
           (case-table--copy-vector up)
         (copy-sequence up))))
    (case-table--put-extra-slot copy 1 nil)
    (case-table--put-extra-slot copy 2 nil)
    copy))

(defun set-case-syntax-delims (l r table)
  "Make L and R non-case-converting delimiters in TABLE."
  (aset table l l)
  (aset table r r)
  (let ((up (case-table-get-table table 'up)))
    (aset up l l)
    (aset up r r))
  (case-table--put-extra-slot table 1 nil)
  (case-table--put-extra-slot table 2 nil)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
                       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
                       (standard-syntax-table)))

(defun set-case-syntax-pair (uc lc table)
  "Make UC and LC an inter-case-converting pair in TABLE."
  (aset table uc lc)
  (aset table lc lc)
  (let ((up (case-table-get-table table 'up)))
    (aset up uc uc)
    (aset up lc uc))
  (case-table--put-extra-slot table 1 nil)
  (case-table--put-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-upcase-syntax (uc lc table)
  "Make UC an upcase character for LC in TABLE."
  (aset table lc lc)
  (let ((up (case-table-get-table table 'up)))
    (aset up uc uc)
    (aset up lc uc))
  (case-table--put-extra-slot table 1 nil)
  (case-table--put-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-downcase-syntax (uc lc table)
  "Make LC a downcase character for UC in TABLE."
  (aset table uc lc)
  (aset table lc lc)
  (let ((up (case-table-get-table table 'up)))
    (aset up uc uc))
  (case-table--put-extra-slot table 1 nil)
  (case-table--put-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-case-syntax (c syntax table)
  "Make C case-invariant with SYNTAX in TABLE."
  (aset table c c)
  (let ((up (case-table-get-table table 'up)))
    (aset up c c))
  (case-table--put-extra-slot table 1 nil)
  (case-table--put-extra-slot table 2 nil)
  (modify-syntax-entry c syntax (standard-syntax-table)))

(provide 'case-table)

;;; case-table.el ends here
