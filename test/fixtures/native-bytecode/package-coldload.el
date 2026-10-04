;;; package-coldload.el --- two-function package cold-load fixture -*- lexical-binding: t; -*-

(defvar nelisp-native-package-coldload-count 0)
(setq nelisp-native-package-coldload-count
      (1+ nelisp-native-package-coldload-count))

(defun nelisp-native-package-default (value &optional supplied)
  (or supplied value))

(defun nelisp-native-package-optional-value (value &optional supplied)
  supplied)

(defun nelisp-native-package-identity (value)
  value)

(provide 'nelisp-native-package-coldload-fixture)
;;; package-coldload.el ends here
