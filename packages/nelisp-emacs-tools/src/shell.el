;;; shell.el --- shell facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-shell-install "emacs-shell")

(defun shell--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar shell--standalone-p (shell--standalone-runtime-p))

(defun shell--host-load-standard ()
  "Load host Emacs's standard shell library (shim dir removed from `load-path')."
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
      (load "shell" nil t))))

(if shell--standalone-p
    (progn
      ;; Bind the standard `shell' command name to the comint-based
      ;; `emacs-shell' implementation.
      (require 'emacs-shell)
      (emacs-shell-install))
  (shell--host-load-standard))

(provide 'shell)

;;; shell.el ends here
