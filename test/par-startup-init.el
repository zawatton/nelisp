;;; par-startup-init.el --- GNU startup parity fixture -*- lexical-binding: t; -*-

;; Build fixture files with (par-startup-init-fixture HOME), then run from
;; that HOME.  Put HOME/site on the initial load path (EMACSLOADPATH on
;; GNU), set PAR_STARTUP_INIT_CASE to the row name, and load this file
;; as a command-line action after startup.  Compare against .gnu.

(defvar par-startup-init-trace nil)

(defun par-startup-init-write (root name text)
  "Write fixture Lisp TEXT at NAME under ROOT."
  (let ((file (expand-file-name name root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";; -*- lexical-binding: t; -*-\n" text "\n"))))

(defun par-startup-init-fixture (home)
  "Create an isolated startup fixture under HOME."
  (par-startup-init-write
   home "site/site-start.el"
   "(defvar par-startup-init-trace nil)
(push 'site par-startup-init-trace)
(add-hook 'before-init-hook (lambda () (push 'before par-startup-init-trace)))
(add-hook 'after-init-hook (lambda () (push 'after par-startup-init-trace)))
(add-hook 'emacs-startup-hook (lambda () (push 'startup par-startup-init-trace)))
(add-hook 'window-setup-hook (lambda () (push 'window par-startup-init-trace)))")
  (par-startup-init-write home "site/default.el"
                         "(push 'default par-startup-init-trace)")
  (dolist (dir '(".emacs.d" "alternate" "xdg/emacs"))
    (let ((root (expand-file-name dir home)))
      (par-startup-init-write
       root "early-init.el"
       "(defvar par-startup-init-trace nil)
(push 'early par-startup-init-trace)
(when (getenv \"PAR_STARTUP_INHIBIT_DEFAULT\") (setq inhibit-default-init t))")
      (par-startup-init-write
       root "init.el"
       "(push 'init par-startup-init-trace)
(setq custom-file (expand-file-name \"custom.el\" user-emacs-directory))
(load custom-file nil t t)")
      (par-startup-init-write root "custom.el"
                             "(push 'custom par-startup-init-trace)")
      (par-startup-init-write root "elpa/startup-row-1.0/startup-row-pkg.el"
                             "(define-package \"startup-row\" \"1.0\" \"Startup fixture\")")
      (par-startup-init-write root "elpa/startup-row-1.0/startup-row-autoloads.el"
                             "(push 'package par-startup-init-trace)")))
  (par-startup-init-write home "dot-emacs.el"
                         "(push 'dot-emacs par-startup-init-trace)"))

(defun par-startup-init-path (path)
  "Make PATH relative to HOME for stable parity output."
  (let ((home (file-name-as-directory (getenv "HOME"))))
    (if (and (stringp path) (string-prefix-p home path))
        (concat "$HOME/" (substring path (length home)))
      path)))

(when-let* ((row (getenv "PAR_STARTUP_INIT_CASE")))
  (prin1
   (list row
         (reverse par-startup-init-trace)
         (par-startup-init-path user-init-file)
         (par-startup-init-path early-init-file)
         (par-startup-init-path user-emacs-directory)
         (par-startup-init-path custom-file)
         (and (bound-and-true-p after-init-time) t)))
  (terpri))
