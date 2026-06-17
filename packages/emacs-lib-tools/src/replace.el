;;; replace.el --- occur/replace facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-replace-install "emacs-replace")

(defun replace--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar replace--standalone-p (replace--standalone-runtime-p))

(defun replace--host-load-standard ()
  "Load host Emacs's standard replace library (shim dir removed from `load-path')."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "replace" nil t))))

(if replace--standalone-p
    (progn
      ;; Bind the standard `occur' / `replace-regexp' / `how-many' / line-filter
      ;; command names to the `emacs-replace' implementations.
      (require 'emacs-replace)
      (emacs-replace-install))
  (replace--host-load-standard))

(provide 'replace)

;;; replace.el ends here
