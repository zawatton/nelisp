;;; nemacs-gnu-startup.el --- NeLisp adapter for GNU startup.el -*- lexical-binding: t; -*-

;; This file follows startup.el in the standalone bootstrap image.

(require 'emacs-vars)

(defvar frame-initial-frame nil)
(defvar face-font-rescale-alist nil)
(defvar face-ignored-fonts nil)
(defvar scalable-fonts-allowed nil)
(defvar menu-bar-mode nil)
(defvar tool-bar-mode nil)
(defcustom abbrev-file-name (locate-user-emacs-file "abbrev_defs" ".abbrev_defs")
  "Default file containing the user's abbreviations."
  :type 'file :group 'abbrev)
(defvar abbrevs-changed nil)
(defvar inhibit-x-resources nil)
(defvar package-enable-at-startup t)
(defvar package--builtin-versions nil
  "Built-in package versions supplied by the startup image.")
(defcustom package-user-dir (locate-user-emacs-file "elpa")
  "Directory containing this user's installed Emacs Lisp packages."
  :type 'directory :group 'applications)
(defcustom package-quickstart-file (locate-user-emacs-file "package-quickstart.el")
  "File containing cached package activation forms."
  :type 'file :group 'applications)
(defcustom package-directory-list
  (let (dirs)
    (dolist (dir load-path (nreverse dirs))
      (when (and (stringp dir)
                 (equal (file-name-nondirectory (directory-file-name dir)) "site-lisp"))
        (push (expand-file-name "elpa" dir) dirs))))
  "Directories containing site-installed Emacs Lisp packages."
  :type '(repeat directory) :group 'applications)

(unless (fboundp 'quietly-read-abbrev-file)
  (autoload 'quietly-read-abbrev-file "abbrev"))

;; The frontend and the runtime have already selected the locale coding
;; system.  GNU's Mule language-environment negotiation remains deferred.
(unless (fboundp 'set-locale-environment)
  (defun set-locale-environment (&optional _locale) nil))

;; GNU batch startup has no display connection or initial frame to realize.
;; Interactive NeLisp frontends realize their own frame after this phase.
(unless (fboundp 'frame-initialize)
  (defun frame-initialize () nil))
(unless (fboundp 'window-system-initialization)
  (defun window-system-initialization (&optional _display) nil))
(unless (fboundp 'frame-notice-user-settings)
  (defun frame-notice-user-settings () nil))

;; The TUI frontend supplies its own palette and terminal initialization.
(unless (fboundp 'tty-register-default-colors)
  (defun tty-register-default-colors () nil))
(unless (fboundp 'tty-run-terminal-initialization)
  (defun tty-run-terminal-initialization (_frame &optional _type _run-hook) nil))

;; In this image GNU's window-system-specific command line is the tty one.
(defalias 'handle-args-function #'tty-handle-args)

(defconst nemacs-gnu-startup--delayed-settings
  '(user-mail-address auto-save-list-file-prefix tutorial-directory
    user-lisp-directory user-lisp-ignored-directories
    abbrev-file-name package-user-dir package-quickstart-file package-directory-list))

;; The legacy loadup supplies a fallback before GNU startup.el is loaded.
;; Use GNU's default for scratch buffers once its Custom declaration exists.
(custom-reevaluate-setting 'initial-scratch-message)

(defun nemacs-gnu-startup--before-init ()
  "Publish the chosen init directory and run the legacy package-slot hook."
  (setq nemacs-user-emacs-directory (expand-file-name user-emacs-directory))
  (when init-file-user
    (run-hooks 'nemacs-package-activation-hook)))

(defun nemacs-gnu-startup--finish (_args)
  "Return control to the NeLisp frontend for action options and display."
  (throw 'nemacs-gnu-startup-finished t))

(defun nemacs-gnu-startup-run (batch-p arguments init-user)
  "Use GNU startup with startup ARGUMENTS and the frontend's INIT-USER gate.
BATCH-P follows GNU batch behavior.  An explicit -u in ARGUMENTS can
opt into loading init even in batch.  The frontend owns action options."
  (let* ((override (getenv "NEMACS_USER_EMACS_DIRECTORY"))
         (override (and override (not (equal override "")) override))
         (noninteractive batch-p)
         (initial-window-system nil)
         (internal--text-quoting-flag internal--text-quoting-flag)
         (command-line-processed nil)
         (command-line-args
          (append (list (or invocation-name "nemacs"))
                  (when (and (not batch-p) (null init-user)) '("-q"))
                  (when (and (not batch-p) init-user) (list "-u" init-user))
                  (when override (list "--init-directory" override))
                  arguments nil))
         (inhibit-startup-hooks (or batch-p inhibit-startup-hooks))
         (custom-delayed-init-variables
          (reverse nemacs-gnu-startup--delayed-settings))
         (original-command-line-1 (symbol-function 'command-line-1))
         (original-init-loader (symbol-function 'startup--load-user-init-file))
         (original-quote-setup (symbol-function 'startup--setup-quote-display)))
    ;; GNU captures the environment before Lisp startup.  The reader has
    ;; already reconstructed it for this process, including image starts.
    (setq initial-environment (copy-sequence process-environment)
          charset-map-path (list (expand-file-name "charsets" data-directory))
          user-init-file nil
          early-init-file nil
          init-file-had-error nil)
    (add-hook 'before-init-hook #'nemacs-gnu-startup--before-init)
    (unwind-protect
        (progn
          (fset 'command-line-1 #'nemacs-gnu-startup--finish)
          ;; No frontend is realized yet.  Keep NeLisp's own startup
          ;; encoding instead of inspecting a GNU terminal glyph table.
          (unless batch-p (fset 'startup--setup-quote-display #'ignore))
          ;; The frontend fixture override confines init loads to its
          ;; directory.  The normal GNU path may fall back to ~/.emacs.
          (when override
            (fset 'startup--load-user-init-file
                  (lambda (filename &optional alternate defaults)
                    (funcall original-init-loader
                             (if defaults #'ignore filename)
                             alternate defaults))))
          (catch 'nemacs-gnu-startup-finished
            (normal-top-level)))
      (fset 'command-line-1 original-command-line-1)
      (unless batch-p (fset 'startup--setup-quote-display original-quote-setup))
      (when override (fset 'startup--load-user-init-file original-init-loader))
      (remove-hook 'before-init-hook #'nemacs-gnu-startup--before-init))
    (setq nemacs-user-emacs-directory (expand-file-name user-emacs-directory))))

(provide 'nemacs-gnu-startup)
;;; nemacs-gnu-startup.el ends here
