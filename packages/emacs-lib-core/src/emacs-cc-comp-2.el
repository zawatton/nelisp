;;; emacs-cc-comp-2.el --- GNU comp.c primitives in Lisp -*- lexical-binding: t; -*-

;;; Code:

(defvar nelisp-eln-registration-isolated-namespace)

(unless (fboundp 'native-elisp-load)
  (defun native-elisp-load (filename &optional late-load)
    "Load native elisp code FILENAME.
LATE-LOAD has to be non-nil when loading for deferred compilation."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (cond
     ((string-empty-p filename)
      (signal 'native-lisp-file-inconsistent (list filename)))
     ((not (file-exists-p filename))
      (signal 'native-lisp-load-failed
              (list "file does not exists" filename)))
     ((file-directory-p filename)
      (signal 'native-lisp-load-failed
              (list filename (concat filename ": cannot read file data: Is a directory"))))
     ((with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename nil 0 4)
        (equal (buffer-string) (unibyte-string 127 ?E ?L ?F)))
      (when late-load
        (signal 'native-lisp-load-failed (list filename "Late load is unsupported")))
      (require 'nelisp-eln-registration)
      ;; Admit the complete unit before changing any existing definition.
      (let* ((load-file-name filename)
             (nelisp-eln-registration-isolated-namespace
              (nelisp-eln-registration-make-isolated-namespace))
             (registration (nelisp-eln-registration-load filename))
             (first (plist-get registration :callable))
             (second (plist-get registration :callable2)))
        (defalias (plist-get registration :name) first)
        (when second
          (defalias (plist-get registration :name2) second))
        (or second first)))
     (t
      (signal 'native-lisp-load-failed
              (list filename (concat filename ": file too short")))))))

(provide 'emacs-cc-comp-2)

;;; emacs-cc-comp-2.el ends here
