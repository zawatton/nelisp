;;; ielm.el --- Lightweight IELM shim for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function ielm-input-handler "emacs-ielm")

(defun ielm--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar ielm--standalone-p (ielm--standalone-runtime-p))

(defun ielm--host-load-standard ()
  "Load host Emacs's standard ielm library."
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
      (load "ielm" nil t))))

(if ielm--standalone-p
    (progn
      (require 'emacs-ielm)
      (unless (fboundp 'ielm-send-input)
        (defalias 'ielm-send-input #'ielm-input-handler)))
  (ielm--host-load-standard))

(provide 'ielm)

;;; ielm.el ends here
