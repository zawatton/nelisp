;;; emacs-cc-subr-metadata-1.el --- Builtin callable metadata -*- lexical-binding: t; -*-

(unless (fboundp 'subr-native-lambda-list)
  (defun subr-native-lambda-list (subr)
    "Return t for a builtin without native-compiled Lisp argument metadata."
    (unless (subrp subr)
      (signal 'wrong-type-argument (list 'subrp subr)))
    t))

(unless (fboundp 'subr-type)
  (defun subr-type (subr)
    "Return the Lisp type metadata of SUBR, nil for a C-style builtin."
    (unless (subrp subr)
      (signal 'wrong-type-argument (list 'subrp subr)))
    nil))

(provide 'emacs-cc-subr-metadata-1)
;;; emacs-cc-subr-metadata-1.el ends here
