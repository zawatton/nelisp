;;; files.el --- tiny daily-driver file facade  -*- lexical-binding: t; -*-

;;; Code:

(defvar files--standalone-p (not (boundp 'emacs-version)))
(defvar files--current-file-name nil)

(defun files--ensure-buffer-substrate ()
  (require 'files-standalone-buffer)
  t)

(defun files--call (function &rest args)
  (files--ensure-buffer-substrate)
  (apply function args))

(defun files--wrap (function)
  (list 'lambda '(&rest args)
        (list 'apply (list 'quote 'files--call)
              (list 'quote function)
              'args)))

(defun files--install (public target)
  (when files--standalone-p
    (fset public (files--wrap target))))

(defun files--install-nullary (public target)
  (when files--standalone-p
    (fset public
          (list 'lambda '(&rest _args)
                (list 'files--call (list 'quote target))))))

(defun files--buffer-file-name (&optional _buffer)
  files--current-file-name)

(defun files--set-visited-file-name (filename &optional _no-query _along)
  (setq files--current-file-name filename))

(when files--standalone-p
  (defun make-sparse-keymap (&optional _prompt) (list 'keymap)))

(when files--standalone-p
  (defun keymapp (object) (and (consp object) (eq (car object) 'keymap))))

(when files--standalone-p
  (defun define-key (keymap key def &optional _remove)
    (setcdr keymap (cons (cons key def) (cdr keymap))) def))

(when files--standalone-p
  (defun lookup-key (keymap key &optional _accept-default)
    (cdr (assoc key (cdr keymap)))))

(when files--standalone-p (defvar ctl-x-map (make-sparse-keymap)))
(when files--standalone-p (defvar ctl-x-4-map (make-sparse-keymap)))
(when files--standalone-p (defvar ctl-x-5-map (make-sparse-keymap)))

(files--install 'buffer-file-name 'files--buffer-file-name)
(files--install 'set-visited-file-name 'files--set-visited-file-name)
(files--install 'find-file 'files-standalone-find-file)
(files--install 'find-file-noselect 'files-standalone-find-file-noselect)
(files--install 'find-file-read-only 'files-standalone-find-file-read-only)
(files--install 'find-alternate-file 'files-standalone-find-alternate-file)
(files--install 'find-file-other-window 'files-standalone-find-file)
(files--install 'find-file-other-frame 'files-standalone-find-file)
(files--install 'write-file 'files-standalone-write-file)
(files--install 'insert-file 'files-standalone-insert-file)
(files--install 'list-directory 'files-standalone-list-directory)
(files--install-nullary 'save-buffer 'files-standalone-save-buffer)
(files--install-nullary 'save-some-buffers 'files-standalone-save-some-buffers)

(when files--standalone-p (define-key ctl-x-map "\C-f" 'find-file))
(when files--standalone-p (define-key ctl-x-map "\C-r" 'find-file-read-only))
(when files--standalone-p (define-key ctl-x-map "\C-v" 'find-alternate-file))
(when files--standalone-p (define-key ctl-x-map "\C-s" 'save-buffer))
(when files--standalone-p (define-key ctl-x-map "\C-w" 'write-file))
(when files--standalone-p (define-key ctl-x-map "i" 'insert-file))
(when files--standalone-p (define-key ctl-x-4-map "f" 'find-file-other-window))
(when files--standalone-p (define-key ctl-x-5-map "f" 'find-file-other-frame))

(provide 'files)

;;; files.el ends here
