;;; image-loader.el --- lisp-image restore helper for nemacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Runtime-facing wrapper around `emacs-dump-load'.  The `.nli' files
;; restored here contain Lisp-visible state only: features, selected
;; defvars, and optional buffer contents.  Function bindings and the
;; runtime heap must already exist, or be restored later by a lower
;; level NeLisp image primitive.

;;; Code:

(require 'emacs-dump)

(defvar image-loader-file nil
  "Default `.nli' file loaded by `image-loader-load-batch'.")

(defvar image-loader-restore-buffers t
  "Non-nil means `image-loader-load' restores persisted buffers by default.")

(defvar image-loader-last-loaded-file nil
  "Absolute path of the most recently loaded `.nli' file.")

(defvar image-loader-last-image-info nil
  "Summary plist for the most recently loaded `.nli' file.")

;;;###autoload
(defun image-loader-load (path &rest restore-buffers-arg)
  "Load the `.nli' lisp-image at PATH.
When RESTORE-BUFFERS is non-nil, restore persisted buffer contents.
When RESTORE-BUFFERS is omitted, use `image-loader-restore-buffers'.
Returns the loaded image plist."
  (let* ((restore-buffers (if restore-buffers-arg
                              (car restore-buffers-arg)
                            image-loader-restore-buffers))
         (file (expand-file-name path))
         (image (emacs-dump-load
                 file
                 restore-buffers)))
    (setq image-loader-last-loaded-file file
          image-loader-last-image-info (emacs-dump-image-info file))
    image))

;;;###autoload
(defun image-loader-load-if-readable (path &optional restore-buffers)
  "Load PATH when it is a readable `.nli' file, otherwise return nil."
  (when (and path (file-readable-p path))
    (image-loader-load path restore-buffers)))

;;;###autoload
(defun image-loader-load-batch (&optional path)
  "Batch entry point for loading a `.nli' lisp-image.
The image path is chosen from PATH, `image-loader-file', or the
first leftover command-line argument.  Prints a short summary and
returns the loaded image plist."
  (let* ((file (or path
                   image-loader-file
                   (and (boundp 'command-line-args-left)
                        (car command-line-args-left)))))
    (unless file
      (signal 'wrong-number-of-arguments (list 'image-loader-load-batch 0)))
    (let ((image (image-loader-load file)))
      (princ (format "image-loader image=%s features=%d defvars=%d buffers=%d\n"
                     (expand-file-name file)
                     (length (plist-get image :features))
                     (length (plist-get image :defvars))
                     (length (plist-get image :buffers))))
      image)))

(defun image-loader-info (path)
  "Return a summary plist for the `.nli' lisp-image at PATH."
  (emacs-dump-image-info path))

(provide 'image-loader)

;;; image-loader.el ends here
