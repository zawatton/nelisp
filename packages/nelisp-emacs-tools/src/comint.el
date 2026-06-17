;;; comint.el --- comint facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-comint-install "emacs-comint")

(defun comint--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar comint--standalone-p (comint--standalone-runtime-p))

(defun comint--host-load-standard ()
  "Load host Emacs's standard comint library (shim dir removed from `load-path')."
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
      (load "comint" nil t))))

(if comint--standalone-p
    (progn
      ;; Bind the standard `comint-*' / `make-comint-in-buffer' command names
      ;; to the `emacs-comint' machinery implementations.
      (require 'emacs-comint)
      (emacs-comint-install))
  (comint--host-load-standard))

(provide 'comint)

;;; comint.el ends here
