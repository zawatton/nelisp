;;; nemacs-gnu-startup.el --- NeLisp adapter for GNU startup.el -*- lexical-binding: t; -*-

;; This file follows startup.el in the standalone bootstrap image.

(require 'emacs-vars)
(require 'emacs-parity-subdirs)
(require 'emacs-file-name-handler)
(emacs-fnh-install-quoting)
(emacs-parity-subdirs-install)

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

;; The frontend and the runtime have already initialized their editor
;; substrate.  The physical window/terminal is managed by the frontend.
(unless (fboundp 'window-system-initialization)
  (defun window-system-initialization (&optional _display) nil))

;; The TUI frontend supplies its own palette and terminal initialization.
(unless (fboundp 'tty-register-default-colors)
  (defun tty-register-default-colors () nil))
(unless (fboundp 'tty-run-terminal-initialization)
  (defun tty-run-terminal-initialization (_frame &optional _type _run-hook) nil))

(defconst nemacs-gnu-startup--delayed-settings
  '(user-mail-address auto-save-list-file-prefix tutorial-directory
    user-lisp-directory user-lisp-ignored-directories
    abbrev-file-name package-user-dir package-quickstart-file package-directory-list))

(defvar nemacs-daemon-name nil
  "Name requested by the launcher, or t for the default daemon.")
(defvar nemacs-gnu-startup--frontend nil)

;; These are normally supplied by C before Lisp startup.  The terminal
;; and server backends are selected by the launcher.
(defun nemacs-gnu-startup-daemon-p ()
  nemacs-daemon-name)

(defun nemacs-gnu-startup--server-start ()
  "Start the NeLisp listener at the GNU daemon startup slot."
  (unless (and server-process (process-live-p server-process))
    (unless (file-directory-p server-socket-dir)
      (make-directory server-socket-dir t)
      (set-file-modes server-socket-dir #o700))
    (emacs-server-client-polyfills-install)
    (nemacs-server-start)))

(defun nemacs-gnu-startup--display-splash (&optional _displayable-buffers)
  "Display the NeLisp splash in the frame selected by GNU startup."
  (when-let* ((buffer (emacs-startup-screen-create)))
    (switch-to-buffer buffer)))

(defun nemacs-gnu-startup--prepare-frame ()
  "Expose the initial editor frame before processing user init."
  (when (eq nemacs-gnu-startup--frontend 'xcb)
    (setq frame-initial-frame (selected-frame)
          emacs-display-system 'x)
    (modify-frame-parameters
     frame-initial-frame
     (list '(window-system . x) (cons 'display (getenv "DISPLAY"))))))

(defun nemacs-gnu-startup--apply-frame-settings ()
  "Apply user frame parameters to NeLisp's initial GUI frame."
  (when frame-initial-frame
    (modify-frame-parameters frame-initial-frame default-frame-alist)
    (modify-frame-parameters frame-initial-frame initial-frame-alist)))

(defun nemacs-gnu-startup-run (batch-p arguments &optional frontend)
  "Run GNU startup with ARGUMENTS and a NeLisp FRONTEND.
BATCH-P is the noninteractive bit normally set by emacs.c."
  (let* ((override (getenv "NEMACS_USER_EMACS_DIRECTORY"))
         (default-path (delete-dups (copy-sequence load-path)))
         (search-path (getenv "EMACSLOADPATH"))
         (nemacs-gnu-startup--frontend frontend))
    (setq user-real-login-name (or (user-login-name (user-real-uid)) "unknown")
          user-login-name (or (getenv "LOGNAME") (getenv "USER")
                              (user-login-name (user-uid)) "unknown")
          noninteractive batch-p
          initial-window-system (and (not batch-p) (eq frontend 'xcb) 'x)
          command-line-args
          (append (list (or invocation-name "nemacs"))
                  (when (and override (not (equal override "")))
                    (list "--init-directory" override))
                  arguments)
          initial-environment (copy-sequence process-environment)
          charset-map-path (list (expand-file-name "charsets" data-directory))
          custom-delayed-init-variables
          (delete-dups (append (reverse nemacs-gnu-startup--delayed-settings)
                               (and (listp custom-delayed-init-variables)
                                    custom-delayed-init-variables))))
    (setq load-path
          (if search-path
              (apply #'append
                     (mapcar (lambda (dir) (if dir (list dir) default-path))
                             (parse-colon-path search-path)))
            default-path))
    (when nemacs-daemon-name
      (require 'emacs-server-client-polyfills)
      (require 'server)
      (setq server-use-tcp nil
            server-socket-dir
            (or (getenv "NEMACS_SERVER_SOCKET_DIR") server-socket-dir
                (expand-file-name (format "emacs%d" (user-uid))
                                  (or (getenv "TMPDIR") "/tmp"))))
      (defalias 'server-start #'nemacs-gnu-startup--server-start))
    (defalias 'daemonp #'nemacs-gnu-startup-daemon-p)
    ;; GNU applies these operations to the frame it creates.  NeLisp's
    ;; platform frontend owns the corresponding terminal and display work.
    (when initial-window-system
      (load "term/common-win" nil t)
      (cl-defmethod handle-args-function (args &context (window-system x))
        (x-handle-args args))
      (provide 'x-win))
    (defalias 'frame-initialize #'nemacs-gnu-startup--prepare-frame)
    (when initial-window-system
      (defalias 'frame-notice-user-settings
        #'nemacs-gnu-startup--apply-frame-settings))
    (defalias 'startup--setup-quote-display #'ignore)
    (defalias 'display-startup-screen #'nemacs-gnu-startup--display-splash)
    (if noninteractive
        (condition-case-unless-debug err
            (normal-top-level)
          (error
           (message "%s" (error-message-string err))
           (kill-emacs 255)))
      (normal-top-level))))

(provide 'nemacs-gnu-startup)
;;; nemacs-gnu-startup.el ends here
