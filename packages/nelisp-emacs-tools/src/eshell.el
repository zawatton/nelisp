;;; eshell.el --- eshell facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defun eshell--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar eshell--standalone-p (eshell--standalone-runtime-p))

(defun eshell--host-load-standard ()
  "Load host Emacs's standard eshell library (shim dir removed from `load-path')."
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
      (load "eshell" nil t))))

(if eshell--standalone-p
    ;; Bind the standard `eshell' command (and `eshell/*' built-ins) by loading
    ;; the minimal `emacs-eshell' implementation.
    (require 'emacs-eshell)
  (eshell--host-load-standard))

(provide 'eshell)

;;; eshell.el ends here
