;;; emacs-parity-abbrev.el --- real obarray-backed abbrev tables -*- lexical-binding: t; -*-

;; `define-abbrev' was undefined on the standalone substrate (void-function
;; errors from python-mode / other modes building their abbrev tables), and
;; `define-abbrev-table' was only a stub that set the table symbol to the raw
;; definitions list -- not a usable abbrev table.

;; Real Emacs models an abbrev table as an obarray whose interned symbols
;; carry the abbrev: the symbol VALUE is the expansion, its FUNCTION is the
;; expansion hook, and its plist holds the abbrev properties.  The substrate
;; provides `obarray-make' (returns a vector-backed obarray, verified) and a
;; table-aware `intern' / `intern-soft', so we implement the real thing over
;; those primitives -- a genuine abbrev table, not a stub.

(when (and (fboundp 'obarray-make) (fboundp 'intern))

  (defun define-abbrev (table name expansion &optional hook &rest props)
    "Define abbrev NAME in TABLE, expanding into EXPANSION and calling HOOK.
Stores the abbrev as an interned symbol of TABLE (an obarray): value =
EXPANSION, function = HOOK, plist = PROPS.  Returns NAME."
    (let ((sym (intern (if (symbolp name) (symbol-name name) name) table)))
      (set sym expansion)
      (when hook (fset sym hook))
      (setplist sym props)
      name))

  ;; Replace the stub with a real obarray-backed table (unconditional: the
  ;; baked `define-abbrev-table' stub stores a list, which no abbrev consumer
  ;; can use).
  (defun define-abbrev-table (tablename &optional definitions _docstring &rest _props)
    "Define TABLENAME (a symbol) as an abbrev table (an obarray).
Populate it from DEFINITIONS, each a `define-abbrev' argument list
\(NAME EXPANSION [HOOK PROPS...]).  Returns TABLENAME."
    (let ((table (if (and (boundp tablename)
                          (vectorp (symbol-value tablename)))
                     (symbol-value tablename)
                   (obarray-make))))
      (set tablename table)
      (dolist (d definitions)
        (when (consp d)
          (apply #'define-abbrev table d)))
      tablename))

  (unless (fboundp 'abbrev-table-p)
    (defun abbrev-table-p (object)
      "Return non-nil if OBJECT is an abbrev table (an obarray)."
      (and (vectorp object) (not (stringp object)))))

  (unless (fboundp 'make-abbrev-table)
    (defun make-abbrev-table (&optional _props)
      "Return a new, empty abbrev table (an obarray)."
      (obarray-make))))

(provide 'emacs-parity-abbrev)
;;; emacs-parity-abbrev.el ends here
