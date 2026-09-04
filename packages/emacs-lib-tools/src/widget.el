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
  (let* ((root (or (and (boundp 'nelisp-emacs-vendor-root)
                        nelisp-emacs-vendor-root)
                   (let ((here (or (and (boundp 'load-file-name) load-file-name)
                                   (and (boundp 'buffer-file-name) buffer-file-name))))
                     (and here
                          (expand-file-name "../vendor"
                                            (file-name-directory here))))))
         (file (and root (expand-file-name "emacs-lisp/widget.el" root))))
    (and file (file-readable-p file) file)))

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
      (when file
        (load file nil t)))
  (widget--host-load-standard))

;;; widget.el ends here
