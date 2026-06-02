;;; project.el --- Lightweight project shim for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defvar project--standalone-p (not (boundp 'emacs-version)))

(defun project--host-load-standard ()
  "Load host Emacs's standard project library."
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
      (load "project" nil t))))

(if project--standalone-p
    (require 'emacs-project)
  (project--host-load-standard))

(provide 'project)

;;; project.el ends here
