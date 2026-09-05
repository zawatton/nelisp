;;; minibuffer.el --- Lightweight minibuffer shim for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defvar minibuffer--standalone-p
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))
      (not (stringp emacs-version)))
  "Non-nil under standalone NeLisp.
Standalone NeLisp binds `emacs-version' to a real string too (for vendor
compatibility), so neither disjunct after the first fires there; detect
the standalone path by a NeLisp-only primitive instead, matching
`emacs-char-table--standalone-p' in `emacs-char-table.el'.")

(defun minibuffer--host-load-standard ()
  "Load host Emacs's standard minibuffer library."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory (or (and (boundp 'load-file-name) load-file-name)
                         (and (boundp 'buffer-file-name) buffer-file-name)
                         default-directory)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "minibuffer" nil t))))

(if minibuffer--standalone-p
    (progn
      (require 'emacs-minibuffer-builtins)
      (unless (fboundp 'minibuffer-complete)
        (defun minibuffer-complete ()
          "Complete minibuffer contents in the lightweight reader."
          (interactive)
          nil))
      (unless (fboundp 'minibuffer-complete-and-exit)
        (defun minibuffer-complete-and-exit ()
          "Accept the current lightweight minibuffer contents."
          (interactive)
          (exit-minibuffer))))
  (minibuffer--host-load-standard))

(provide 'minibuffer)

;;; minibuffer.el ends here
