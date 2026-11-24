;;; charprop.el --- Load the generated GNU Unicode property registry -*- lexical-binding: t; -*-

;;; Code:

(require 'emacs-cc-chartab-1)

;; The standalone image does not preload mule-cmds.el.  Keep its character
;; property API here so the generated registry and tables can load verbatim.
(when (emacs-char-table--standalone-p)
  (put 'char-code-property-table 'char-table-extra-slots 5)

  (defvar char-code-property-table (make-char-table 'char-code-property-table)
    "Property lists for character properties outside `char-code-property-alist'.")

  (defun define-char-code-property (name table &optional docstring)
    "Define NAME with a Unicode property TABLE or the file that defines it.
Optional DOCSTRING documents the property."
    (unless (symbolp name)
      (error "Not a symbol: %s" name))
    (when (and (vectorp table) (= (length table) 73)
               (eq (aref table 2) 'char-code-property-table))
      (setq table (emacs-cc-chartab-1--read-table table)))
    (if (char-table-p table)
        (unless (and (eq (char-table-subtype table) 'char-code-property-table)
                     (eq (char-table-extra-slot table 0) name))
          (error "Invalid char-table: %s" table))
      (unless (stringp table)
        (error "Not a char-table nor a file name: %s" table)))
    (let ((entry (assq name char-code-property-alist)))
      (unless (and (stringp table) (char-table-p (cdr entry)))
        (if entry
            (setcdr entry table)
          (push (cons name table) char-code-property-alist))
        (put name 'char-code-property-documentation docstring))))

  (defun get-char-code-property (char property)
    "Return the value of CHAR's PROPERTY."
    (let ((table (unicode-property-table-internal property)))
      (if table
          (let ((decoder (char-table-extra-slot table 1)))
            (if (functionp decoder)
                (funcall decoder char (aref table char) table)
              (get-unicode-property-internal table char)))
        (plist-get (aref char-code-property-table char) property))))

  (defun put-char-code-property (char property value)
    "Store VALUE for CHAR's PROPERTY and return VALUE."
    (let ((table (unicode-property-table-internal property)))
      (if table
          (let ((encoder (char-table-extra-slot table 2)))
            (if (functionp encoder)
                (funcall encoder char value table)
              (put-unicode-property-internal table char value)))
        (aset char-code-property-table char
              (plist-put (aref char-code-property-table char) property value))))
    value)

  (defun char-code-property-description (property value)
    "Return a description of PROPERTY's VALUE, or nil if it has none."
    (let ((table (unicode-property-table-internal property)))
      (when table
        (let ((describe (char-table-extra-slot table 3)))
          (when (functionp describe)
            (funcall describe value)))))))

(unless (featurep 'charprop)
  (load "international/charprop.el" nil 'no-message t t))

;;; charprop.el ends here
