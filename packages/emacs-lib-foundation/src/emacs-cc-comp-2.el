;;; emacs-cc-comp-2.el --- GNU comp.c primitives in Lisp -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'native-elisp-load)
  (defun native-elisp-load (filename &optional late-load)
    "Load native elisp code FILENAME.
LATE-LOAD has to be non-nil when loading for deferred compilation."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (ignore late-load)
    (cond
     ((string-empty-p filename)
      (signal 'native-lisp-file-inconsistent (list filename)))
     ((not (file-exists-p filename))
      (signal 'native-lisp-load-failed
              (list "file does not exists" filename)))
     ((file-directory-p filename)
      (signal 'native-lisp-load-failed
              (list filename (concat filename ": cannot read file data: Is a directory"))))
     (t
      (signal 'native-lisp-load-failed
              (list filename (concat filename ": file too short")))))))

(provide 'emacs-cc-comp-2)

;;; emacs-cc-comp-2.el ends here
