;;; emacs-cc-xml-1.el --- xml.c C-core primitives -*- lexical-binding: t; -*-

(unless (fboundp 'libxml-available-p)
  (defun libxml-available-p ()
    "Return t if libxml2 support is available in this instance of Emacs."
    ;; Availability is represented by GNU Emacs's optional libxml feature.
    (and (featurep 'libxml) t)))

(provide 'emacs-cc-xml-1)
