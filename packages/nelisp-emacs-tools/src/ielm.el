;;; ielm.el --- Lightweight IELM shim for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defvar ielm--standalone-p (not (boundp 'emacs-version)))

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
