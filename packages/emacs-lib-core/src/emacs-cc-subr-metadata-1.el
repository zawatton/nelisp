;;; emacs-cc-subr-metadata-1.el --- Builtin callable metadata -*- lexical-binding: t; -*-

(unless (fboundp 'subr-name)
  (defun subr-name (subr)
    "Return the name carried by the builtin callable SUBR."
    (unless (subrp subr)
      (signal 'wrong-type-argument (list 'subrp subr)))
    ;; The existing tagged-object printer reads the builtin name field.
    ;; Use that representation instead of adding a native metadata entry.
    (let ((text (nelisp--repr subr)))
      (substring text 7 (1- (length text))))))

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
