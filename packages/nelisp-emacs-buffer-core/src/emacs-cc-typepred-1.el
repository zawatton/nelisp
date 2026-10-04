;;; emacs-cc-typepred-1.el --- C-core type predicates -*- lexical-binding: t; -*-

(unless (fboundp 'integer-or-marker-p)
  (defun integer-or-marker-p (object)
    "Return non-nil when OBJECT is an integer or marker."
    (or (integerp object) (markerp object))))

(unless (fboundp 'number-or-marker-p)
  (defun number-or-marker-p (object)
    "Return non-nil when OBJECT is a number or marker."
    (or (numberp object) (markerp object))))

(unless (fboundp 'vector-or-char-table-p)
  (defun vector-or-char-table-p (object)
    "Return non-nil when OBJECT is a vector or character table."
    (or (vectorp object) (char-table-p object))))

(provide 'emacs-cc-typepred-1)
;;; emacs-cc-typepred-1.el ends here
