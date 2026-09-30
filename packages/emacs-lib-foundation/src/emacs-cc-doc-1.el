;;; emacs-cc-doc-1.el --- documentation primitives -*- lexical-binding: t; -*-

;; Pure-Lisp compatibility for the small doc.c surface exercised in batch.

(defvar text-quoting-style nil
  "Preferred style for quoting text, or nil for automatic selection.")

(unless (fboundp 'internal-subr-documentation)
  (defun internal-subr-documentation (function)
    "Return the raw documentation info of a C primitive.

(fn FUNCTION)"
    ;; GNU batch builds report that no raw C documentation is available.
    (ignore function)
    t))

(unless (fboundp 'Snarf-documentation)
  (defun Snarf-documentation (filename)
    "Used during Emacs initialization to scan the `etc/DOC...' file.

(fn FILENAME)"
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (if (string-empty-p filename)
        (signal 'error (list "DOC file invalid at position 0"))
      nil)))

(unless (fboundp 'text-quoting-style)
  (defun text-quoting-style ()
    "Return the current effective text quoting style.

(fn)"
    (let ((style (and (boundp 'text-quoting-style)
                      (symbol-value 'text-quoting-style))))
      (if (memq style '(grave straight curve))
          style
        'curve))))

(provide 'emacs-cc-doc-1)
;;; emacs-cc-doc-1.el ends here
