;;; nemacs-loaddefs.el --- Autoload (loaddefs) generation for nelisp-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 11 M7 (Runtime Image, Autoload, and Vendor Callable Promotion):
;; generate `autoload' forms from `;;;###autoload' cookies so vendored
;; libraries can be declared lazily (paired with the lazy `autoload' in
;; emacs-eval.el) and startup stays bounded -- the file's body is only
;; loaded when one of its autoloaded commands is first called.

;;; Code:

(defun nemacs-loaddefs--form-autoload (form file-base)
  "Return an `autoload' form for top-level FORM defined in FILE-BASE, or nil.
Only `defun' / `defmacro' forms produce an autoload."
  (when (and (consp form) (memq (car form) '(defun defmacro)))
    (let* ((name (nth 1 form))
           (doc (let ((d (nth 3 form))) (and (stringp d) d)))
           (body (nthcdr 3 form))
           (interactive
            (let (found)
              (dolist (f body)
                (when (and (consp f) (eq (car f) 'interactive))
                  (setq found t)))
              found))
           (macrop (eq (car form) 'defmacro)))
      `(autoload ',name ,file-base ,doc ,interactive ,(and macrop 'macro)))))

(defun nemacs-loaddefs-generate-for-file (file)
  "Return the list of `autoload' forms for `;;;###autoload' cookies in FILE.
A cookie on its own line is associated with the next top-level form read
after it.  Comments and blank lines between the cookie and the form are
skipped by the Lisp reader."
  (let ((file-base (file-name-base file))
        (forms nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^;;;###autoload[ \t]*$" nil t)
        (forward-line 1)
        (let ((form (condition-case nil (read (current-buffer)) (error nil))))
          (when form
            (let ((al (nemacs-loaddefs--form-autoload form file-base)))
              (when al (push al forms)))))))
    (nreverse forms)))

(defun nemacs-loaddefs-generate (files)
  "Return all `autoload' forms for the `;;;###autoload' cookies in FILES."
  (let (out)
    (dolist (file files)
      (setq out (append out (nemacs-loaddefs-generate-for-file file))))
    out))

(provide 'nemacs-loaddefs)

;;; nemacs-loaddefs.el ends here
