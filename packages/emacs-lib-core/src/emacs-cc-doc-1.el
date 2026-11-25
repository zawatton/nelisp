;;; emacs-cc-doc-1.el --- documentation primitives -*- lexical-binding: t; -*-

;; Pure-Lisp compatibility for the small doc.c surface exercised in batch.

(defvar text-quoting-style nil
  "Preferred style for quoting text, or nil for automatic selection.")

(defvar emacs-cc-doc-1--subr-docs nil
  "Hash table: C primitive name -> its docstring from `doc-directory''s DOC.")

(defun emacs-cc-doc-1--subr-docs ()
  "Return (building once) the table of the DOC file's function entries.
GNU's etc/DOC holds each primitive as \\037F<name>\\n<docstring>, the
docstring ending with its (fn ...) usage line."
  (or emacs-cc-doc-1--subr-docs
      (setq emacs-cc-doc-1--subr-docs
            (let ((table (make-hash-table :test 'equal))
                  (file (and (boundp 'doc-directory) (stringp doc-directory)
                             (expand-file-name "DOC" doc-directory))))
              (when (and file (file-readable-p file))
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (search-forward "\037F" nil t)
                    (let ((name (buffer-substring-no-properties
                                 (point) (line-end-position))))
                      (forward-line 1)
                      (let ((start (point))
                            (end (if (search-forward "\037" nil t)
                                     (1- (point))
                                   (point-max))))
                        (puthash name (buffer-substring-no-properties start end)
                                 table)
                        (goto-char end))))))
              table))))

(unless (fboundp 'internal-subr-documentation)
  (defun internal-subr-documentation (function)
    "Return the raw documentation info of a C primitive.

(fn FUNCTION)"
    ;; GNU answers the docstring's offset in etc/DOC, which `documentation'
    ;; then reads; this answers the docstring read from the vendored DOC
    ;; itself, or t (no documentation) for a primitive DOC does not list.
    (let ((name (and (fboundp 'subr-name) (subrp function)
                     (subr-name function))))
      (or (and name (gethash name (emacs-cc-doc-1--subr-docs)))
          t))))

(unless (fboundp 'Snarf-documentation)
  (defun Snarf-documentation (filename)
    "Used during Emacs initialization to scan the `etc/DOC...' file.

(fn FILENAME)"
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (if (string-empty-p filename)
        (signal 'error (list "DOC file invalid at position 0"))
      nil)))

(provide 'emacs-cc-doc-1)
;;; emacs-cc-doc-1.el ends here
