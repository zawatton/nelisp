;;; gnu-semantics-shadow.el --- Bound GNU variable deferral -*- lexical-binding: t; -*-

(defun gs-shadow-target-p (form)
  (and (consp form)
       (or (and (eq (car form) 'defvar)
                (eq (cadr form) 'internal--text-quoting-flag))
           (gs-shadow-target-p (car form))
           (gs-shadow-target-p (cdr form)))))

(let* ((root (expand-file-name "../.."
                               (file-name-directory (or load-file-name buffer-file-name))))
       (source (expand-file-name
                (or (getenv "GNU_SEMANTICS_PRELUDE_SOURCE")
                    "scripts/nelisp-stdlib-prelude.el") root))
       (declaration
        (with-temp-buffer
          (insert-file-contents source)
          (goto-char (point-min))
          (let ((form nil))
            (while (not (gs-shadow-target-p form))
              (setq form (read (current-buffer))))
            form)))
       (original-defvar (symbol-function 'defvar))
       (was-bound (boundp 'internal--text-quoting-flag))
       (original-value (and was-bound (symbol-value 'internal--text-quoting-flag)))
       (calls 0))
  (unwind-protect
      (progn
        (set 'internal--text-quoting-flag t)
        (fset 'defvar
              (cons 'macro (lambda (name &rest _args)
                             (setq calls (1+ calls))
                             (list 'quote name))))
        (eval declaration t)
        (prin1 (list 'shadow-existing-bound-variable calls
                     (eq (symbol-value 'internal--text-quoting-flag) t)))
        (terpri))
    (fset 'defvar original-defvar)
    (if was-bound (set 'internal--text-quoting-flag original-value)
      (makunbound 'internal--text-quoting-flag))))

(kill-emacs 0)

;;; gnu-semantics-shadow.el ends here
