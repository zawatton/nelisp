;;; charprop.el --- lightweight character property registry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Pure-Elisp substrate for the standard `charprop' feature.  Emacs'
;; generated file mostly registers Unicode property names with lazy table
;; filenames; the large data tables stay outside the daily-driver path.

;;; Code:

(require 'case-table)

(defvar charprop--registry nil
  "Lightweight char-code property registry.
Each entry has the shape (PROPERTY TABLE DOCSTRING OVERRIDES).")

(defun charprop--standalone-p ()
  "Return non-nil under standalone NeLisp."
  (not (boundp 'emacs-version)))

(defun charprop--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this facade."
  (if (charprop--standalone-p)
      t
    (not (fboundp symbol))))

(defun charprop--entry (property)
  "Return registry entry for PROPERTY."
  (assq property charprop--registry))

(defun charprop--sync-public-alist ()
  "Synchronize `char-code-property-alist' with the lightweight registry."
  (when (or (charprop--standalone-p)
            (not (boundp 'char-code-property-alist)))
    (setq char-code-property-alist
          (mapcar (lambda (entry)
                    (list (car entry) (nth 1 entry) (nth 2 entry)))
                  (nreverse (copy-sequence charprop--registry))))))

(defun charprop--define-char-code-property (property table &optional docstring)
  "Register PROPERTY with TABLE and optional DOCSTRING."
  (let ((entry (charprop--entry property)))
    (if entry
        (setcdr entry (list table docstring (nth 3 entry)))
      (push (list property table docstring nil) charprop--registry)))
  (charprop--sync-public-alist)
  property)

(defun charprop--ensure-property (property)
  "Return registry entry for PROPERTY, creating an empty one when needed."
  (or (charprop--entry property)
      (progn
        (charprop--define-char-code-property property nil nil)
        (charprop--entry property))))

(defun charprop--lookup-table (table char)
  "Return TABLE value for CHAR, or nil when TABLE is lazy/unknown."
  (cond
   ((null table) nil)
   ((stringp table) nil)
   ((hash-table-p table) (gethash char table))
   ((char-table-p table) (char-table-range table char))
   ((and (vectorp table) (< char (length table))) (aref table char))
   ((consp table) (cdr (assq char table)))
   (t nil)))

(defun charprop--get-char-code-property (char property)
  "Return PROPERTY value for CHAR."
  (let ((entry (charprop--entry property)))
    (when entry
      (let ((override (assq char (nth 3 entry))))
        (if override
            (cdr override)
          (charprop--lookup-table (nth 1 entry) char))))))

(defun charprop--put-char-code-property (char property value)
  "Set PROPERTY for CHAR to VALUE."
  (let* ((entry (charprop--ensure-property property))
         (overrides (nth 3 entry))
         (cell (assq char overrides)))
    (if cell
        (setcdr cell value)
      (setcar (nthcdr 3 entry) (cons (cons char value) overrides))))
  value)

(defun charprop--unicode-property-table-internal (property)
  "Return loaded table for Unicode PROPERTY, or nil for lazy tables."
  (let ((entry (charprop--entry property)))
    (when entry
      (let ((table (nth 1 entry)))
        (and (not (stringp table)) table)))))

(defun charprop--char-code-property-description (property value)
  "Return a lightweight description for PROPERTY VALUE."
  (cond
   ((null value) nil)
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (format "%S" value))))

(defun charprop--install ()
  "Install lightweight char-code property functions when needed."
  (when (charprop--standalone-p)
    (fset 'define-char-code-property #'charprop--define-char-code-property)
    (fset 'get-char-code-property #'charprop--get-char-code-property)
    (fset 'put-char-code-property #'charprop--put-char-code-property)
    (fset 'unicode-property-table-internal
          #'charprop--unicode-property-table-internal)
    (fset 'char-code-property-description
          #'charprop--char-code-property-description))
  (when (charprop--install-function-p 'define-char-code-property)
    (defalias 'define-char-code-property
      #'charprop--define-char-code-property))
  (when (charprop--install-function-p 'get-char-code-property)
    (defalias 'get-char-code-property #'charprop--get-char-code-property))
  (when (charprop--install-function-p 'put-char-code-property)
    (defalias 'put-char-code-property #'charprop--put-char-code-property))
  (when (charprop--install-function-p 'unicode-property-table-internal)
    (defalias 'unicode-property-table-internal
      #'charprop--unicode-property-table-internal))
  (when (charprop--install-function-p 'char-code-property-description)
    (defalias 'char-code-property-description
      #'charprop--char-code-property-description)))

(charprop--install)

(charprop--define-char-code-property 'name "uni-name.el"
  "Unicode character name.")
(charprop--define-char-code-property 'general-category "uni-category.el"
  "Unicode general category.")
(charprop--define-char-code-property 'canonical-combining-class
  "uni-combining.el" "Unicode canonical combining class.")
(charprop--define-char-code-property 'bidi-class "uni-bidi.el"
  "Unicode bidi class.")
(charprop--define-char-code-property 'decomposition "uni-decomposition.el"
  "Unicode decomposition mapping.")
(charprop--define-char-code-property 'decimal-digit-value "uni-decimal.el"
  "Unicode numeric value (decimal digit).")
(charprop--define-char-code-property 'digit-value "uni-digit.el"
  "Unicode numeric value (digit).")
(charprop--define-char-code-property 'numeric-value "uni-numeric.el"
  "Unicode numeric value (numeric).")
(charprop--define-char-code-property 'mirrored "uni-mirrored.el"
  "Unicode bidi mirrored flag.")
(charprop--define-char-code-property 'mirroring "uni-mirrored.el"
  "Unicode bidi-mirroring characters.")
(charprop--define-char-code-property 'old-name "uni-old-name.el"
  "Unicode old names.")
(charprop--define-char-code-property 'iso-10646-comment "uni-comment.el"
  "Unicode ISO 10646 comment.")
(charprop--define-char-code-property 'uppercase "uni-uppercase.el"
  "Unicode simple uppercase mapping.")
(charprop--define-char-code-property 'lowercase "uni-lowercase.el"
  "Unicode simple lowercase mapping.")
(charprop--define-char-code-property 'titlecase "uni-titlecase.el"
  "Unicode simple titlecase mapping.")
(charprop--define-char-code-property 'special-uppercase
  "uni-special-uppercase.el" "Unicode unconditional special uppercase mapping.")
(charprop--define-char-code-property 'special-lowercase
  "uni-special-lowercase.el" "Unicode unconditional special lowercase mapping.")
(charprop--define-char-code-property 'special-titlecase
  "uni-special-titlecase.el" "Unicode unconditional special titlecase mapping.")
(charprop--define-char-code-property 'paired-bracket "uni-brackets.el"
  "Unicode bidi paired-bracket characters.")
(charprop--define-char-code-property 'bracket-type "uni-brackets.el"
  "Unicode bidi paired-bracket type.")

(provide 'charprop)

;;; charprop.el ends here
