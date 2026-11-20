;;; emacs-cc-category-1.el --- category.c compatibility -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-category-1--docs-for (table)
  "Return TABLE's category docstrings from char-table extra-slot 0."
  (let ((docs (emacs-char-table-extra-slot table 0)))
    (unless (and (vectorp docs) (= (length docs) 95))
      (signal 'wrong-type-argument (list 'category-docstrings docs)))
    (when (and (eq table (emacs-char-table-category-table))
               (null (aref docs 0)))
      (aset docs 0
            "space for indent\nThis character counts as a space for indentation purposes."))
    docs))

(defun emacs-cc-category-1--category-p (category)
  (and (integerp category) (<= ?\s category) (<= category ?~)))

(defun emacs-cc-category-1--table (table)
  (let ((table (or table (emacs-char-table-category-table))))
    (unless (emacs-char-table-category-table-p table)
      (signal 'wrong-type-argument (list 'category-table-p table)))
    table))

(unless (fboundp 'category-docstring)
  (defun category-docstring (category &optional table)
    "Return the documentation string of CATEGORY, as defined in TABLE.
TABLE should be a category table and defaults to the current buffer's
category table."
    (unless (emacs-cc-category-1--category-p category)
      (signal 'wrong-type-argument (list 'categoryp category)))
    (let* ((table (emacs-cc-category-1--table table))
           (docs (emacs-cc-category-1--docs-for table)))
      (or (aref docs (- category ?\s))
          (and (= category ?A) "2-byte alnum\nAlphanumeric characters of 2-byte character sets")))) )

(unless (fboundp 'category-set-mnemonics)
  (defun category-set-mnemonics (category-set)
    "Return a string containing mnemonics of the categories in CATEGORY-SET.
CATEGORY-SET is a bool-vector, and the categories `in' it are those
that are indexes where t occurs in the bool-vector.
The return value is a string containing those same categories."
    (unless (and (bool-vector-p category-set)
                 (= (length category-set) 128))
      (signal 'wrong-type-argument (list 'categorysetp category-set)))
    (let (chars)
      (dotimes (i 128)
        (when (aref category-set i) (push i chars)))
      (apply #'string (nreverse chars)))))

(unless (fboundp 'define-category)
  (defun define-category (category docstring &optional table)
    "Define CATEGORY as a category which is described by DOCSTRING.
CATEGORY should be an ASCII printing character in the range ` ' to `~'.
DOCSTRING is the category description.
The category is defined only in category table TABLE, which defaults to
the current buffer's category table."
    (unless (emacs-cc-category-1--category-p category)
      (signal 'wrong-type-argument (list 'categoryp category)))
    (unless (stringp docstring)
      (signal 'wrong-type-argument (list 'stringp docstring)))
    (let* ((table (emacs-cc-category-1--table table))
           (docs (emacs-cc-category-1--docs-for table))
           (index (- category ?\s))
           (old (aref docs index)))
      (when old
        (signal 'error (list (format "Category ‘%c’ is already defined" category))))
      (aset docs index docstring))
    nil))

(unless (fboundp 'get-unused-category)
  (defun get-unused-category (&optional table)
    "Return a category which is not yet defined in TABLE.
If no category remains available, return nil.
The optional argument TABLE specifies which category table to modify;
it defaults to the current buffer's category table."
    (let* ((table (emacs-cc-category-1--table table))
           (docs (emacs-cc-category-1--docs-for table))
           (c ?\s))
      (while (and (<= c ?~) (aref docs (- c ?\s))) (setq c (1+ c)))
      (and (<= c ?~) c))))

(unless (fboundp 'make-category-set)
  (defun make-category-set (categories)
    "Return a newly created category-set which contains CATEGORIES.
CATEGORIES is a string of category mnemonics.
The value is a bool-vector which has t at the indices corresponding to
those categories."
    (unless (stringp categories)
      (signal 'wrong-type-argument (list 'stringp categories)))
    (when (multibyte-string-p categories)
      (dotimes (i (length categories))
        (when (> (aref categories i) 127)
          (error "Multibyte string in ‘make-category-set’"))))
    (let ((set (make-bool-vector 128 nil)))
      (dotimes (i (length categories))
        (let ((category (aref categories i)))
          (unless (emacs-cc-category-1--category-p category)
            (signal 'wrong-type-argument (list 'categoryp category)))
          (aset set category t)))
      set)))

;; The prelude's category primitives expect native char-tables.  When the
;; emacs-lib vector char-tables are installed, point them at the emacs-lib
;; standard and current tables so define-category and friends operate on the
;; same objects `category-table' and `standard-category-table' return.
(when (and (fboundp 'nelisp--category-standard)
           (fboundp 'emacs-char-table-standard-category-table))
  (defvar emacs-cc-category-1--standard-docs-p nil)
  (defun nelisp--category-standard ()
    "Return the emacs-lib standard category table with GNU's docstrings."
    (let ((table (emacs-char-table-standard-category-table)))
      (unless emacs-cc-category-1--standard-docs-p
        (setq emacs-cc-category-1--standard-docs-p t)
        (when (boundp 'nelisp--unicode-category-docstrings)
          (let ((docs (emacs-char-table-extra-slot table 0)) (i 0))
            (while (< i (length nelisp--unicode-category-docstrings))
              (aset docs i (aref nelisp--unicode-category-docstrings i))
              (setq i (1+ i))))))
      table))
  (defun nelisp--category-current ()
    "Return the emacs-lib current category table."
    (nelisp--category-standard)
    (emacs-char-table-category-table)))

(provide 'emacs-cc-category-1)
;;; emacs-cc-category-1.el ends here
