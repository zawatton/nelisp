;;; man.el --- man facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-man-install "emacs-man")

(defun man--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar man--standalone-p (man--standalone-runtime-p))

(defun man--host-load-standard ()
  "Load host Emacs's standard man library (shim dir removed from `load-path')."
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
      (load "man" nil t))))

(if man--standalone-p
    (progn
      ;; Bind the standard `man' / `woman' command names to the `emacs-man'
      ;; viewer implementation.
      (require 'emacs-man)
      (emacs-man-install))
  (man--host-load-standard))

(provide 'man)

;;; man.el ends here
