;;; emacs-cc-casetab-1.el --- casetab.c primitives -*- lexical-binding: t; -*-

(unless (fboundp 'case-table-p)
  (defun case-table-p (object)
    "Return t if OBJECT is a case table.
See `set-case-table' for more information on these data structures."
    (and (char-table-p object)
         (eq (char-table-subtype object) 'case-table))))

(provide 'emacs-cc-casetab-1)
;;; emacs-cc-casetab-1.el ends here
