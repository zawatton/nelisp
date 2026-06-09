;;; files.el --- tiny daily-driver file facade  -*- lexical-binding: t; -*-

;;; Code:

(defun files--standalone-runtime-p ()
  "Return non-nil when the lightweight file facade should install wrappers."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar files--standalone-p (files--standalone-runtime-p))
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

(defun files--lazy-wrapper (target)
  (list 'lambda '(&rest args)
        '(require 'files-standalone-buffer)
        (list 'apply (list 'quote target) 'args)))

(defun files--install-nullary (public target)
  (when files--standalone-p
    (fset public
          (list 'lambda '(&rest _args)
                (list 'files--call (list 'quote target))))))

(defun files--lazy-nullary-wrapper (target)
  (list 'lambda '(&rest _args)
        '(require 'files-standalone-buffer)
        (list 'funcall (list 'quote target))))

(defun files--install-lazy (public target)
  (when files--standalone-p
    (fset public (files--lazy-wrapper target))))

(defun files--install-lazy-nullary (public target)
  (when files--standalone-p
    (fset public (files--lazy-nullary-wrapper target))))

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
(when files--standalone-p
  (fset 'find-file
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-find-file args))))
(when files--standalone-p
  (fset 'find-file-noselect
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-find-file-noselect args))))
(when files--standalone-p
  (fset 'find-file-read-only
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-find-file-read-only args))))
(when files--standalone-p
  (fset 'find-alternate-file
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-find-alternate-file args))))
(when files--standalone-p
  (fset 'find-file-other-window
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-find-file args))))
(when files--standalone-p
  (fset 'find-file-other-frame
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-find-file args))))
(when files--standalone-p
  (fset 'write-file
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-write-file args))))
(when files--standalone-p
  (fset 'insert-file
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-insert-file args))))
(when files--standalone-p
  (fset 'list-directory
        '(lambda (&rest args)
           (require 'files-standalone-buffer)
           (apply 'files-standalone-list-directory args))))
(when files--standalone-p
  (fset 'save-buffer
        '(lambda (&rest _args)
           (require 'files-standalone-buffer)
           (funcall 'files-standalone-save-buffer))))
(when files--standalone-p
  (fset 'save-some-buffers
        '(lambda (&rest _args)
           (require 'files-standalone-buffer)
           (funcall 'files-standalone-save-some-buffers))))

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
