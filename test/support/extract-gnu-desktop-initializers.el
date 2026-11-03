;;; extract-gnu-desktop-initializers.el --- Exact autoload variable declarations -*- lexical-binding: t; -*-
(require 'seq)
(let* ((root (getenv "NELISP_GNU_BUILD_SOURCE_ROOT"))
       (source (expand-file-name "lisp/desktop.el" root))
       (names '(desktop-buffer-mode-handlers desktop-minor-mode-handlers)) forms)
  (unless (equal emacs-version "31.1") (error "GNU31 extractor required"))
  (with-temp-buffer
    (insert-file-contents-literally source)
    (unless (equal (secure-hash 'sha256 (current-buffer))
                   "da58e8c7bc9e33990d5a7642155c018b6c619d24131e8fdf05518850f77398a2")
      (error "GNU desktop source pin differs"))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (or (and (eq (car-safe form) 'defvar) (memq (cadr form) names))
                      (and (eq (car-safe form) 'put)
                           (eq (car-safe (cadr form)) 'quote)
                           (memq (cadr (cadr form)) names)
                           (equal (caddr form) '(quote risky-local-variable))))
              (push form forms))))
      (end-of-file nil)))
  (dolist (name names)
    (unless (and (= 1 (length (seq-filter
                              (lambda (form) (and (eq (car form) 'defvar) (eq (cadr form) name))) forms)))
                 (= 1 (length (seq-filter
                              (lambda (form) (and (eq (car form) 'put) (eq (cadr (cadr form)) name))) forms))))
      (error "Desktop declaration/property missing or repeated: %S" name)))
  (with-temp-file (getenv "NELISP_GNU_DESKTOP_FORMS_OUTPUT")
    (prin1 (nreverse forms) (current-buffer)) (terpri (current-buffer))))
