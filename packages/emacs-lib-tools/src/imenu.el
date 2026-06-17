;;; imenu.el --- imenu facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-imenu-install "emacs-imenu")

(defun imenu--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar imenu--standalone-p (imenu--standalone-runtime-p))

(defun imenu--host-load-standard ()
  "Load host Emacs's standard imenu library (shim dir removed from `load-path')."
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
      (load "imenu" nil t))))

(if imenu--standalone-p
    (progn
      ;; Bind the standard `imenu' command name to the `emacs-imenu'
      ;; read-only symbol-index implementation.
      (require 'emacs-imenu)
      (emacs-imenu-install))
  (imenu--host-load-standard))

(provide 'imenu)

;;; imenu.el ends here
