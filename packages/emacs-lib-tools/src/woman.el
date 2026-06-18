;;; woman.el --- woman facade loader for NeLisp  -*- lexical-binding: t; -*-

;;; Commentary:

;; The standalone viewer uses the `man' program for both `man' and `woman'
;; (the pure-Elisp nroff formatter is a later target), so on the reader this
;; facade just loads `emacs-man', which installs `woman'.

;;; Code:

(declare-function emacs-man-install "emacs-man")

(defun woman--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader.
`emacs-version' is bound under nemacs too, so also probe for reader
primitives (mirrors `files--standalone-runtime-p')."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar woman--standalone-p (woman--standalone-runtime-p))

(defun woman--host-load-standard ()
  "Load host Emacs's standard woman library (shim dir removed from `load-path')."
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
      (load "woman" nil t))))

(if woman--standalone-p
    (progn
      (require 'emacs-man)
      (emacs-man-install))
  (woman--host-load-standard))

(provide 'woman)

;;; woman.el ends here
