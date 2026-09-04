;;; url.el --- URL loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defun url--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defun url--vendor-file ()
  "Return the vendored `url.el' path for standalone loads, or nil."
  (let* ((root (or (and (boundp 'nelisp-emacs-vendor-root)
                        nelisp-emacs-vendor-root)
                   (let ((here (or (and (boundp 'load-file-name) load-file-name)
                                   (and (boundp 'buffer-file-name) buffer-file-name))))
                     (and here
                          (expand-file-name "../vendor"
                                            (file-name-directory here))))))
         (file (and root (expand-file-name "emacs-lisp/url.el" root))))
    (and file (file-readable-p file) file)))

(defun url--host-load-standard ()
  "Load host Emacs's standard `url' library."
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
      (load "url" nil t))))

(if (url--standalone-runtime-p)
    (let ((file (url--vendor-file)))
      (when file
        (load file nil t)))
  (url--host-load-standard))

;;; url.el ends here
