;;; compile.el --- compile/grep facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-compile-install "emacs-compile")

(defun compile--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar compile--standalone-p (compile--standalone-runtime-p))

(defun compile--host-load-standard ()
  "Load host Emacs's standard compile library (shim dir removed from `load-path')."
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
      (load "compile" nil t))))

(if compile--standalone-p
    (progn
      ;; Bind the standard `compile' / `grep' / `next-error' / `previous-error'
      ;; command names to the `emacs-compile' read-only implementations.
      (require 'emacs-compile)
      (emacs-compile-install))
  (compile--host-load-standard))

(provide 'compile)

;;; compile.el ends here
