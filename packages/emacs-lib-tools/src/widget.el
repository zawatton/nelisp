;;; widget.el --- Widget loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defun widget--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defun widget--vendor-file ()
  "Return the vendored `widget.el' path for standalone loads, or nil."
  (require 'nelisp-emacs-vendor)
  (nelisp-emacs-vendor-file "widget.el"))

(defun widget--host-load-standard ()
  "Load host Emacs's standard `widget' library."
  (let ((shim-dir (expand-file-name
                   (file-name-as-directory
                    (file-name-directory
                     (or (and (boundp 'load-file-name) load-file-name)
                         (and (boundp 'buffer-file-name) buffer-file-name)
                         default-directory)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (expand-file-name (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "widget" nil t))))

(if (widget--standalone-runtime-p)
    (let ((file (widget--vendor-file)))
      (load file nil t))
  (widget--host-load-standard))

;;; widget.el ends here
