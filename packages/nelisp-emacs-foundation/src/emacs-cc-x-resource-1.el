;;; emacs-cc-x-resource-1.el --- batch X resource primitive -*- lexical-binding: t; -*-

;; This standalone runtime has no initialized window system.  GNU Emacs' X
;; primitive signals this exact error before inspecting resource arguments.
(unless (fboundp 'x-get-resource)
  (defun x-get-resource (attribute class &optional component subclass)
    "Look up ATTRIBUTE of CLASS in the X resource database.
COMPONENT and SUBCLASS specify the optional instance path.

In the standalone batch runtime no X display is initialized, so match the
GNU primitive's no-display error."
    (ignore attribute class component subclass)
    (signal 'error (list "Window system is not in use or not initialized"))))

(provide 'emacs-cc-x-resource-1)
;;; emacs-cc-x-resource-1.el ends here
