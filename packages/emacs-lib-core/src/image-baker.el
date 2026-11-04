;;; image-baker.el --- batch lisp-image baker for nemacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Small orchestration layer over `nemacs-loadup' + `emacs-dump'.
;; This produces a Lisp-readable `.nli' image after the normal loadup
;; path has run.  It intentionally does not try to freeze the runtime
;; heap, obarray, function bindings, or native pointers; those require
;; a NeLisp runtime restore primitive.

;;; Code:

(require 'nemacs-loadup)

(defvar image-baker-output-file
  (expand-file-name "build/nemacs-loadup.nli"
                    (expand-file-name
                     ".."
                     (file-name-directory
                      (or load-file-name buffer-file-name default-directory))))
  "Default `.nli' output path used by `image-baker-bake-batch'.")

(defvar image-baker-extra-buffer-names nil
  "Extra buffer names captured while baking an image.")

(defun image-baker--ensure-output-directory (path)
  "Create PATH's parent directory when it is non-empty."
  (let ((dir (file-name-directory (expand-file-name path))))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))))

;;;###autoload
(defun image-baker-bake (&optional output extra-buffer-names)
  "Run `nemacs-init' in batch mode and write a lisp-image to OUTPUT.
OUTPUT defaults to `image-baker-output-file'.  EXTRA-BUFFER-NAMES
temporarily extends `emacs-dump-extra-buffer-names'.  Returns the
image plist written by `emacs-dump-save'."
  (let ((path (expand-file-name (or output image-baker-output-file)))
        (emacs-dump-extra-buffer-names
         (append extra-buffer-names image-baker-extra-buffer-names
                 emacs-dump-extra-buffer-names)))
    (unless nemacs-initialized
      (nemacs-init t))
    (image-baker--ensure-output-directory path)
    (emacs-dump-save path)))

;;;###autoload
(defun image-baker-bake-batch (&optional output)
  "Batch entry point for building a `.nli' lisp-image.
The output path is chosen from OUTPUT, `image-baker-output-file',
or the first leftover command-line argument.  Prints a short
summary and returns the image plist."
  (let* ((path (expand-file-name
                (or output
                    (and (boundp 'command-line-args-left)
                         (car command-line-args-left))
                    image-baker-output-file)))
         (image (image-baker-bake path)))
    (princ (format "image-baker image=%s features=%d defvars=%d buffers=%d\n"
                   path
                   (length (plist-get image :features))
                   (length (plist-get image :defvars))
                   (length (plist-get image :buffers))))
    image))

(defun image-baker-image-info (path)
  "Return a summary plist for the baked image at PATH."
  (nemacs-dump-info path))

(provide 'image-baker)

;;; image-baker.el ends here
