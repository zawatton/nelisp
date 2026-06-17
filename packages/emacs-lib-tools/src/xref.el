;;; xref.el --- xref facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(declare-function emacs-xref-install "emacs-xref")

(defun xref--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar xref--standalone-p (xref--standalone-runtime-p))

(defun xref--host-load-standard ()
  "Load host Emacs's standard xref library (shim dir removed from `load-path')."
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
      (load "xref" nil t))))

(if xref--standalone-p
    (progn
      ;; Bind the standard `xref-find-definitions' / `xref-pop-marker-stack'
      ;; command names to the `emacs-xref' read-only implementations.
      (require 'emacs-xref)
      (emacs-xref-install))
  (xref--host-load-standard))

(provide 'xref)

;;; xref.el ends here
