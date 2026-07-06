;;; nemacs-runtime-image-preload.el --- NeLisp runtime-image preload forms  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared preload entry points for `make bake-*-runtime-image'.  Keep the
;; Makefile recipes as small as possible so new runtime-image lanes can be
;; added without duplicating the load-path/bootstrap form.

;;; Code:

;; Source-v1 runtime image replay preserves top-level `setq' more reliably
;; than `defconst', while functions below need these variables after boot.
(setq nemacs-runtime-image-preload--script-directory
      (file-name-directory (or load-file-name buffer-file-name "")))

(setq nemacs-runtime-image-package-load-paths
      '("packages/nelisp-emacs-buffer-core/lazy"
        "packages/nelisp-emacs-buffer-core/lisp"
        "packages/nelisp-emacs-core/lazy"
        "packages/nelisp-emacs-core/lisp"
        "packages/nelisp-emacs-editing/lisp"
        "packages/nelisp-emacs-facade/lisp"
        "packages/nelisp-emacs-foundation/lisp"
        "packages/nelisp-emacs-io/lazy"
        "packages/nelisp-emacs-io/lisp"
        "packages/nelisp-emacs-special-buffers/lisp"
        "packages/nelisp-emacs-text-core/lazy"
        "packages/nelisp-emacs-text-core/lisp"
        "packages/nelisp-emacs-textmodes-stub/lisp"
        "packages/nelisp-emacs-app-gui/lisp"))

(defun nemacs-runtime-image-setup-paths (repo-root)
  "Install REPO-ROOT's package/app scaffold and vendor paths for a bake."
  (unless (boundp 'load-path)
    (defvar load-path nil))
  (setq nelisp-emacs-vendor-root (concat repo-root "/vendor"))
  (setq load-path
        (append (mapcar (lambda (path) (concat repo-root "/" path))
                        nemacs-runtime-image-package-load-paths)
                (list (concat repo-root "/vendor/emacs-lisp")
                      (concat repo-root "/vendor/emacs-lisp/emacs-lisp")
                      (concat repo-root "/vendor/emacs-lisp/vc")
                      ;; Doc 37 (task #16): vendor Tramp's own
                      ;; directory plus `parse-time''s home under
                      ;; calendar/.  Path-only -- nothing here `require's
                      ;; Tramp into the baked image (Doc 37 risk #10);
                      ;; `nemacs-tramp-setup' calls `require' dynamically
                      ;; after session start.
                      (concat repo-root "/vendor/emacs-lisp/net")
                      (concat repo-root "/vendor/emacs-lisp/calendar"))
                load-path))
  t)

(defun nemacs-runtime-image-load-bootstrap (bootstrap-file)
  "Load BOOTSTRAP-FILE when it is non-empty."
  (when (and bootstrap-file
             (not (string= bootstrap-file "")))
    (load bootstrap-file nil 'no-message t t))
  t)

(defun nemacs-runtime-image-preload-batch (repo-root bootstrap-file)
  "Preload the batch nemacs entry into the current NeLisp image."
  (nemacs-runtime-image-setup-paths repo-root)
  (nemacs-runtime-image-load-bootstrap bootstrap-file)
  (nemacs-runtime-image-preload--install-process-core)
  (require 'nemacs-main)
  (load (concat repo-root "/scripts/nemacs-runtime-frame-tab-preload.el")
        nil 'no-message t t)
  t)

(defun nemacs-runtime-image-preload-interactive (repo-root bootstrap-file)
  "Preload the interactive TUI entry into the current NeLisp image."
  (nemacs-runtime-image-preload-batch repo-root bootstrap-file)
  (require 'emacs-tui-backend)
  (require 'emacs-tui-event)
  (require 'emacs-redisplay-core)
  (nemacs-main--init-keymap)
  (nemacs-main--prepare-tui-state)
  t)

(defun nemacs-runtime-image-preload-vendor-core (repo-root bootstrap-file)
  "Preload the daily-driver vendor core lane into the current image.

This currently bakes lightweight daily-driver feature surfaces after
the Layer-2 substrate is loaded.  It is intentionally separate from
the default batch image while the vendor surface is still expanding."
  (nemacs-runtime-image-preload-batch repo-root bootstrap-file)
  (nemacs-runtime-image-preload-vendor-core-extension)
  t)

(defun nemacs-runtime-image-preload-vendor-core-extension ()
  "Extend an already-baked base runtime image with vendor core modules."
  (nemacs-runtime-image-preload--install-files-core)
  (nemacs-runtime-image-preload--install-simple-core)
  (nemacs-runtime-image-preload--install-dired-core)
  (nemacs-runtime-image-preload--install-help-core)
  (nemacs-runtime-image-preload--install-elisp-core)
  (nemacs-runtime-image-preload--install-ielm-core)
  (nemacs-runtime-image-preload--install-isearch-core)
  (nemacs-runtime-image-preload--install-minibuffer-core)
  (nemacs-runtime-image-preload--install-project-core)
  (nemacs-runtime-image-preload--install-process-core)
  (nemacs-runtime-image-preload--install-frame-core)
  (nemacs-runtime-image-preload--install-tab-core)
  (nemacs-runtime-image-preload--install-support-core)
  (nemacs-runtime-image-preload--install-utility-i18n-core)
  t)

(defun nemacs-runtime-image-preload--install-files-core ()
  "Install the minimal `files' daily-driver surface without source `load'."
  (unless (featurep 'files)
    (unless (fboundp 'make-sparse-keymap)
      (fset 'make-sparse-keymap (lambda (&optional _prompt) (list 'keymap))))
    (unless (fboundp 'define-key)
      (fset 'define-key
            (lambda (keymap key def &optional _remove)
              (setcdr keymap (cons (cons key def) (cdr keymap)))
              def)))
    (unless (fboundp 'lookup-key)
      (fset 'lookup-key
            (lambda (keymap key &optional _accept-default)
              (cdr (assoc key (cdr keymap))))))
    (unless (boundp 'ctl-x-map)
      (defvar ctl-x-map (make-sparse-keymap)))
    (unless (boundp 'ctl-x-4-map)
      (defvar ctl-x-4-map (make-sparse-keymap)))
    (unless (boundp 'ctl-x-5-map)
      (defvar ctl-x-5-map (make-sparse-keymap)))
    (unless (boundp 'files--current-file-name)
      (defvar files--current-file-name nil))
    (unless (boundp 'files--buffer-file-names)
      (defvar files--buffer-file-names nil))
    (unless (boundp 'files--buffer-string)
      (defvar files--buffer-string ""))
    (unless (boundp 'files--buffer-strings)
      (defvar files--buffer-strings nil))
    (unless (boundp 'files--point)
      (defvar files--point 1))
    (unless (boundp 'files--buffer-points)
      (defvar files--buffer-points nil))
    (unless (boundp 'files--buffer-modified-p)
      (defvar files--buffer-modified-p nil))
    (unless (boundp 'files--buffer-modified-flags)
      (defvar files--buffer-modified-flags nil))
    (dolist (pair '((buffer-file-name . files--buffer-file-name)
                    (set-visited-file-name . files--set-visited-file-name)
                    (find-file . files-standalone-find-file)
                    (find-file-noselect . files-standalone-find-file-noselect)
                    (find-file-read-only . files-standalone-find-file-read-only)
                    (find-alternate-file . files-standalone-find-alternate-file)
                    (find-file-other-window . files-standalone-find-file)
                    (find-file-other-frame . files-standalone-find-file)
                    (save-buffer . files-standalone-save-buffer)
                    (save-some-buffers . files-standalone-save-some-buffers)
                    (write-file . files-standalone-write-file)
                    (insert-file . files-standalone-insert-file)
                    (list-directory . files-standalone-list-directory)))
      (nemacs-runtime-image-preload--install-file-command
       (car pair) (cdr pair)))
    (define-key ctl-x-map "\C-f" 'find-file)
    (define-key ctl-x-map "\C-r" 'find-file-read-only)
    (define-key ctl-x-map "\C-v" 'find-alternate-file)
    (define-key ctl-x-map "\C-s" 'save-buffer)
    (define-key ctl-x-map "\C-w" 'write-file)
    (define-key ctl-x-map "i" 'insert-file)
    (define-key ctl-x-4-map "f" 'find-file-other-window)
    (define-key ctl-x-5-map "f" 'find-file-other-frame)
    (provide 'files))
  t)

(defun nemacs-runtime-image-preload--install-file-command (public target)
  "Install PUBLIC as a lazy wrapper around TARGET.
The wrapper is quoted data instead of a closure so source-v1 images can
replay it in runtimes with minimal closure support."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                '(require 'files-standalone-buffer)
                (list 'apply (list 'quote target) 'args)))))

(defun nemacs-runtime-image-preload--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (condition-case nil
           (symbol-function symbol)
         (error nil))))

(defun nemacs-runtime-image-preload--install-dired-command (public)
  "Install PUBLIC as a lazy wrapper around `emacs-dired-min'."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                '(require 'emacs-dired-min)
                (list 'apply (list 'quote public) 'args)))))

(defun nemacs-runtime-image-preload--install-dired-core ()
  "Install the minimal `dired' daily-driver surface without source `load'."
  (unless (featurep 'dired)
    (dolist (symbol '(dired
                      dired-mode
                      dired-find-file
                      dired-next-line
                      dired-previous-line
                      dired-up-directory))
      (nemacs-runtime-image-preload--install-dired-command symbol))
    (provide 'dired))
  t)

(defun nemacs-runtime-image-preload--install-help-command (public)
  "Install PUBLIC as a lazy wrapper around `emacs-help'."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                '(require 'emacs-help)
                (list 'apply (list 'quote public) 'args)))))

(defun nemacs-runtime-image-preload--install-help-core ()
  "Install the minimal help daily-driver surface without source `load'."
  (unless (featurep 'help-mode)
    (dolist (symbol '(help-mode
                      help-go-back
                      help-go-forward))
      (nemacs-runtime-image-preload--install-help-command symbol))
    (provide 'help-mode))
  (unless (featurep 'help-fns)
    (dolist (symbol '(describe-function
                      describe-variable
                      describe-symbol
                      describe-key))
      (nemacs-runtime-image-preload--install-help-command symbol))
    (provide 'help-fns))
  t)

(defun nemacs-runtime-image-preload--install-module-command
    (public feature target)
  "Install PUBLIC as a lazy wrapper requiring FEATURE and calling TARGET."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                (list 'require (list 'quote feature))
                (list 'apply (list 'quote target) 'args)))))

(defun nemacs-runtime-image-preload--install-elisp-core ()
  "Install the minimal `lisp-mode' daily-driver surface."
  (unless (featurep 'lisp-mode)
    (nemacs-runtime-image-preload--install-module-command
     'emacs-lisp-mode 'lisp-mode 'emacs-mode-emacs-lisp-mode)
    (nemacs-runtime-image-preload--install-module-command
     'lisp-mode 'lisp-mode 'emacs-mode-emacs-lisp-mode)
    (nemacs-runtime-image-preload--install-module-command
     'eval-defun 'emacs-elisp-eval 'eval-defun)
    (unless (fboundp 'indent-sexp)
      (fset 'indent-sexp '(lambda (&optional _endpos) nil)))
    (provide 'lisp-mode))
  t)

(defun nemacs-runtime-image-preload--install-ielm-core ()
  "Install the minimal `ielm' daily-driver surface."
  (unless (featurep 'ielm)
    (nemacs-runtime-image-preload--install-module-command
     'ielm 'emacs-ielm 'ielm)
    (nemacs-runtime-image-preload--install-module-command
     'ielm-send-input 'emacs-ielm 'ielm-input-handler)
    (provide 'ielm))
  t)

(defun nemacs-runtime-image-preload--install-isearch-core ()
  "Install the minimal `isearch' daily-driver surface."
  (unless (featurep 'isearch)
    (nemacs-runtime-image-preload--install-module-command
     'isearch-forward 'emacs-isearch 'isearch-forward)
    (nemacs-runtime-image-preload--install-module-command
     'isearch-backward 'emacs-isearch 'isearch-backward)
    (unless (fboundp 'isearch-forward-regexp)
      (fset 'isearch-forward-regexp
            '(lambda (&optional no-recursive-edit)
               (require 'emacs-isearch)
               (isearch-forward t no-recursive-edit))))
    (provide 'isearch))
  t)

(defun nemacs-runtime-image-preload--install-minibuffer-core ()
  "Install the minimal `minibuffer' daily-driver surface."
  (unless (featurep 'minibuffer)
    (nemacs-runtime-image-preload--install-module-command
     'completing-read 'emacs-minibuffer-builtins 'completing-read)
    (unless (fboundp 'minibuffer-complete)
      (fset 'minibuffer-complete '(lambda () nil)))
    (unless (fboundp 'minibuffer-complete-and-exit)
      (fset 'minibuffer-complete-and-exit
            '(lambda ()
               (require 'emacs-minibuffer-builtins)
               (exit-minibuffer))))
    (provide 'minibuffer))
  t)

(defun nemacs-runtime-image-preload--install-project-core ()
  "Install the minimal `project' daily-driver surface."
  (unless (featurep 'project)
    (dolist (symbol '(project-current
                      project-find-file
                      project-switch-project))
      (nemacs-runtime-image-preload--install-module-command
       symbol 'emacs-project symbol))
    (provide 'project))
  t)

(defun nemacs-runtime-image-preload--install-process-core ()
  "Install process API facades without source `load'.

The source-v1 runtime image records `setq' and `fset' reliably, while
full source `defun' forms may not replay into a fresh standalone reader.
Keep this surface as data lambdas so runtime images expose the
`emacs-process' boundary even before NeLisp's real subprocess primitives are
bound.  When `nelisp-process-*' or legacy `nelisp-*' delegates exist they are
used; otherwise synchronous calls return a failure status instead of aborting
image startup."
  (let* ((base (cond
                ((boundp 'nemacs-runtime-image-preload--script-directory)
                 nemacs-runtime-image-preload--script-directory)
                ((or load-file-name buffer-file-name)
                 (file-name-directory (or load-file-name buffer-file-name)))
                (t nil)))
         (preload (and base
                       (expand-file-name
                        "nemacs-runtime-process-preload.el" base))))
    (when (and preload (file-readable-p preload)
               ;; In host Emacs the native subr `make-process' already
               ;; exists; loading the source-v1 preload would clobber it
               ;; (and the other unprefixed process primitives) with wrapper
               ;; lambdas that re-enter `emacs-process-*' and recurse.  Only
               ;; load the preload when there is no native subr to preserve
               ;; (= a real standalone reader / runtime image).
               (not (and (fboundp 'make-process)
                         (fboundp 'subrp)
                         (subrp (symbol-function 'make-process)))))
      (load preload nil 'no-message)))
  (unless (boundp 'shell-file-name)
    (defvar shell-file-name "/bin/sh"))
  (unless (boundp 'shell-command-switch)
    (defvar shell-command-switch "-c"))
  (unless (boundp 'emacs-process-shell-file-name)
    (defvar emacs-process-shell-file-name "/bin/sh"))
  (unless (boundp 'emacs-process-shell-command-switch)
    (defvar emacs-process-shell-command-switch "-c"))
  (unless (boundp 'emacs-process-call-process-region-input-file)
    (defvar emacs-process-call-process-region-input-file
      "/tmp/nemacs-call-process-region-input"))
  (unless (boundp 'emacs-process-shell-command-on-region-output-file)
    (defvar emacs-process-shell-command-on-region-output-file
      "/tmp/nemacs-shell-command-on-region-output"))
  (unless (boundp 'emacs-process--fallback-tag)
    (defvar emacs-process--fallback-tag 'emacs-process-fallback))
  (unless (boundp 'emacs-process--fallback-processes)
    (defvar emacs-process--fallback-processes nil))
  (unless (boundp 'emacs-process--fallback-next-pid)
    (defvar emacs-process--fallback-next-pid 10000))
  (unless (fboundp 'emacs-process--fallback-process-p)
    (fset 'emacs-process--fallback-process-p
          '(lambda (object)
             (if (vectorp object)
                 (if (<= 10 (length object))
                     (eq (aref object 0) emacs-process--fallback-tag)
                   nil)
               nil))))
  (unless (fboundp 'emacs-process--fallback-plist-get)
    (fset 'emacs-process--fallback-plist-get
          '(lambda (plist prop)
             (let ((value nil)
                   (found nil))
               (while (if plist (not found) nil)
                 (if (eq (car plist) prop)
                     (progn
                       (setq value (car (cdr plist)))
                       (setq found t))
                   (setq plist (cdr (cdr plist)))))
               value))))
  (unless (fboundp 'emacs-process--fallback-buffer)
    (fset 'emacs-process--fallback-buffer
          '(lambda (buffer)
             (if (if (stringp buffer) (fboundp 'get-buffer-create) nil)
                 (get-buffer-create buffer)
               buffer))))
  (unless (fboundp 'emacs-process--fallback-sentinel-event)
    (fset 'emacs-process--fallback-sentinel-event
          '(lambda (status)
             (if (if (integerp status) (= status 0) nil)
                 "finished\n"
               (concat "exited abnormally with code "
                       (number-to-string status)
                       "\n")))))
  (unless (fboundp 'emacs-process--fallback-make-process)
    (fset 'emacs-process--fallback-make-process
          '(lambda (&rest plist)
             (let ((name (or (emacs-process--fallback-plist-get plist :name)
                             "process"))
                   (buffer (emacs-process--fallback-buffer
                            (emacs-process--fallback-plist-get plist :buffer)))
                   (command (emacs-process--fallback-plist-get plist :command))
                   (sentinel (emacs-process--fallback-plist-get
                              plist :sentinel))
                   (filter (emacs-process--fallback-plist-get plist :filter))
                   (pid emacs-process--fallback-next-pid)
                   (process nil)
                   (status 1))
               (setq process
                     (vector emacs-process--fallback-tag name buffer command
                             'run nil filter sentinel nil pid))
               (setq emacs-process--fallback-next-pid
                     (+ emacs-process--fallback-next-pid 1))
               (setq emacs-process--fallback-processes
                     (cons process emacs-process--fallback-processes))
               (setq status
                     (if (if (consp command) (car command) nil)
                         (apply 'call-process
                                (car command) nil buffer nil (cdr command))
                       1))
               (aset process 4 'exit)
               (aset process 5 status)
               (if (functionp sentinel)
                   (funcall sentinel process
                            (emacs-process--fallback-sentinel-event status))
                 nil)
               process))))
  (unless (fboundp 'emacs-process-call-process)
    (fset 'emacs-process-call-process
          '(lambda (&rest args)
             (cond
              ((fboundp 'nelisp-process-call-process)
               (apply 'nelisp-process-call-process args))
              ((fboundp 'nelisp-call-process)
               (apply 'nelisp-call-process args))
              (t 1)))))
  (unless (fboundp 'call-process)
    (fset 'call-process
          '(lambda (&rest args)
             (apply 'emacs-process-call-process args))))
  (unless (fboundp 'emacs-process-call-process-region)
    (fset 'emacs-process-call-process-region
          '(lambda (start end program &optional delete destination display
                          &rest args)
             (cond
              ((fboundp 'nelisp-process-call-process-region)
               (apply 'nelisp-process-call-process-region
                      start end program delete destination display args))
              ((fboundp 'nelisp-call-process-region)
               (apply 'nelisp-call-process-region
                      start end program delete destination display args))
              ((and (fboundp 'buffer-substring-no-properties)
                    (fboundp 'nl-write-file))
               (nl-write-file
                emacs-process-call-process-region-input-file
                (buffer-substring-no-properties start end))
               (when (and delete (fboundp 'delete-region))
                 (delete-region start end))
               (apply 'call-process
                      program
                      emacs-process-call-process-region-input-file
                      destination
                      display
                      args))
              (t 1)))))
  (unless (fboundp 'call-process-region)
    (fset 'call-process-region
          '(lambda (&rest args)
             (apply 'emacs-process-call-process-region args))))
  (unless (fboundp 'emacs-process-start-process)
    (fset 'emacs-process-start-process
          '(lambda (name buffer program &rest program-args)
             (emacs-process--fallback-make-process
              :name name
              :buffer buffer
              :command (cons program program-args)))))
  (unless (fboundp 'start-process)
    (fset 'start-process
          '(lambda (&rest args)
             (apply 'emacs-process-start-process args))))
  (unless (fboundp 'emacs-process-make-process)
    (fset 'emacs-process-make-process
          '(lambda (&rest plist)
             (apply 'emacs-process--fallback-make-process plist))))
  (unless (fboundp 'make-process)
    (fset 'make-process
          '(lambda (&rest plist)
             (apply 'emacs-process-make-process plist))))
  (unless (fboundp 'emacs-process-processp)
    (fset 'emacs-process-processp
          '(lambda (object)
             (emacs-process--fallback-process-p object))))
  (unless (fboundp 'processp)
    (fset 'processp
          '(lambda (object)
             (emacs-process-processp object))))
  (unless (fboundp 'emacs-process-process-list)
    (fset 'emacs-process-process-list
          '(lambda ()
             (let ((items emacs-process--fallback-processes)
                   (result nil))
               (while items
                 (if (aref (car items) 8)
                     nil
                   (setq result (cons (car items) result)))
                 (setq items (cdr items)))
               result))))
  (unless (fboundp 'process-list)
    (fset 'process-list
          '(lambda ()
             (emacs-process-process-list))))
  (unless (fboundp 'emacs-process-process-status)
    (fset 'emacs-process-process-status
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 4)
               nil))))
  (unless (fboundp 'process-status)
    (fset 'process-status
          '(lambda (process)
             (emacs-process-process-status process))))
  (unless (fboundp 'emacs-process-process-exit-status)
    (fset 'emacs-process-process-exit-status
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (or (aref process 5) 0)
               0))))
  (unless (fboundp 'process-exit-status)
    (fset 'process-exit-status
          '(lambda (process)
             (emacs-process-process-exit-status process))))
  (unless (fboundp 'emacs-process-process-buffer)
    (fset 'emacs-process-process-buffer
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 2)
               nil))))
  (unless (fboundp 'process-buffer)
    (fset 'process-buffer
          '(lambda (process)
             (emacs-process-process-buffer process))))
  (unless (fboundp 'emacs-process-process-name)
    (fset 'emacs-process-process-name
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 1)
               ""))))
  (unless (fboundp 'process-name)
    (fset 'process-name
          '(lambda (process)
             (emacs-process-process-name process))))
  (unless (fboundp 'emacs-process-process-command)
    (fset 'emacs-process-process-command
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 3)
               nil))))
  (unless (fboundp 'process-command)
    (fset 'process-command
          '(lambda (process)
             (emacs-process-process-command process))))
  (unless (fboundp 'emacs-process-process-live-p)
    (fset 'emacs-process-process-live-p
          '(lambda (process)
             (memq (process-status process)
                   '(run open listen connect stop)))))
  (unless (fboundp 'process-live-p)
    (fset 'process-live-p
          '(lambda (process)
             (emacs-process-process-live-p process))))
  (unless (fboundp 'emacs-process-process-id)
    (fset 'emacs-process-process-id
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 9)
               nil))))
  (unless (fboundp 'process-id)
    (fset 'process-id
          '(lambda (process)
             (emacs-process-process-id process))))
  (unless (fboundp 'emacs-process-process-mark)
    (fset 'emacs-process-process-mark
          '(lambda (process)
             nil)))
  (unless (fboundp 'process-mark)
    (fset 'process-mark
          '(lambda (process)
             (emacs-process-process-mark process))))
  (unless (fboundp 'emacs-process-set-process-filter)
    (fset 'emacs-process-set-process-filter
          '(lambda (process filter)
             (if (emacs-process--fallback-process-p process)
                 (progn
                   (aset process 6 filter)
                   filter)
               nil))))
  (unless (fboundp 'set-process-filter)
    (fset 'set-process-filter
          '(lambda (process filter)
             (emacs-process-set-process-filter process filter))))
  (unless (fboundp 'emacs-process-set-process-sentinel)
    (fset 'emacs-process-set-process-sentinel
          '(lambda (process sentinel)
             (if (emacs-process--fallback-process-p process)
                 (progn
                   (aset process 7 sentinel)
                   sentinel)
               nil))))
  (unless (fboundp 'set-process-sentinel)
    (fset 'set-process-sentinel
          '(lambda (process sentinel)
             (emacs-process-set-process-sentinel process sentinel))))
  (unless (fboundp 'emacs-process-accept-process-output)
    (fset 'emacs-process-accept-process-output
          '(lambda (&optional process seconds millisec just-this-one)
             nil)))
  (unless (fboundp 'accept-process-output)
    (fset 'accept-process-output
          '(lambda (&optional process seconds millisec just-this-one)
             (emacs-process-accept-process-output
              process seconds millisec just-this-one))))
  (unless (fboundp 'emacs-process-signal-process)
    (fset 'emacs-process-signal-process
          '(lambda (process-or-pid signum)
             (if (emacs-process--fallback-process-p process-or-pid)
                 (progn
                   (aset process-or-pid 4 'signal)
                   (aset process-or-pid 5 1)
                   process-or-pid)
               nil))))
  (unless (fboundp 'signal-process)
    (fset 'signal-process
          '(lambda (process-or-pid signum)
             (emacs-process-signal-process process-or-pid signum))))
  (unless (fboundp 'emacs-process-kill-process)
    (fset 'emacs-process-kill-process
          '(lambda (process)
             (signal-process process 'KILL))))
  (unless (fboundp 'kill-process)
    (fset 'kill-process
          '(lambda (process)
             (emacs-process-kill-process process))))
  (unless (fboundp 'emacs-process-process-send-string)
    (fset 'emacs-process-process-send-string
          '(lambda (process string)
             nil)))
  (unless (fboundp 'process-send-string)
    (fset 'process-send-string
          '(lambda (process string)
             (emacs-process-process-send-string process string))))
  (unless (fboundp 'emacs-process-process-send-eof)
    (fset 'emacs-process-process-send-eof
          '(lambda (&optional process)
             nil)))
  (unless (fboundp 'process-send-eof)
    (fset 'process-send-eof
          '(lambda (&optional process)
             (emacs-process-process-send-eof process))))
  (unless (fboundp 'emacs-process-delete-process)
    (fset 'emacs-process-delete-process
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (progn
                   (aset process 8 t)
                   process)
               nil))))
  (unless (fboundp 'delete-process)
    (fset 'delete-process
          '(lambda (process)
             (emacs-process-delete-process process))))
  (unless (fboundp 'emacs-process-shell-command)
    (fset 'emacs-process-shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (call-process emacs-process-shell-file-name
                           nil
                           (if output-buffer output-buffer t)
                           nil
                           emacs-process-shell-command-switch
                           command))))
  (unless (fboundp 'shell-command)
    (fset 'shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (emacs-process-shell-command
              command output-buffer error-buffer))))
  (unless (fboundp 'emacs-process-shell-command-on-region)
    (fset 'emacs-process-shell-command-on-region
          '(lambda (start end command &optional output-buffer replace-flag
                          error-buffer display-error-buffer
                          region-noncontiguous-p)
             (let ((destination
                    (if replace-flag
                        emacs-process-shell-command-on-region-output-file
                      (if output-buffer
                          output-buffer
                        emacs-process-shell-command-on-region-output-file)))
                   (status 1))
               (when (fboundp 'nl-write-file)
                 (nl-write-file destination ""))
               (setq status
                     (call-process-region
                      start end emacs-process-shell-file-name
                      nil destination nil
                      emacs-process-shell-command-switch command))
               (when (and replace-flag
                          (fboundp 'delete-region)
                          (fboundp 'insert))
                 (let ((text (if (fboundp 'rdf) (rdf destination) "")))
                   (delete-region start end)
                   (insert text)))
               status))))
  (unless (fboundp 'shell-command-on-region)
    (fset 'shell-command-on-region
          '(lambda (start end command &optional output-buffer replace-flag
                          error-buffer display-error-buffer
                          region-noncontiguous-p)
             (emacs-process-shell-command-on-region
              start end command output-buffer replace-flag error-buffer
              display-error-buffer region-noncontiguous-p))))
  (unless (fboundp 'emacs-process-async-shell-command)
    (fset 'emacs-process-async-shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (make-process
              :name (concat "async-shell-command<" command ">")
              :buffer (or output-buffer "*Async Shell Command*")
              :command (list emacs-process-shell-file-name
                             emacs-process-shell-command-switch
                             command)))))
  (unless (fboundp 'async-shell-command)
    (fset 'async-shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (emacs-process-async-shell-command
              command output-buffer error-buffer))))
  (unless (fboundp 'emacs-process-shell-command-to-string)
    (fset 'emacs-process-shell-command-to-string
          '(lambda (command)
             (if (fboundp 'with-temp-buffer)
                 (with-temp-buffer
                   (call-process emacs-process-shell-file-name
                                 nil t nil
                                 emacs-process-shell-command-switch
                                 command)
                   (buffer-string))
               ""))))
  (unless (fboundp 'shell-command-to-string)
    (fset 'shell-command-to-string
          '(lambda (command)
             (emacs-process-shell-command-to-string command))))
  (provide 'emacs-process)
  (provide 'emacs-process-builtins)
  t)

(defun nemacs-runtime-image-preload--install-frame-core ()
  "Install the minimal frame daily-driver surface without source `load'."
  (unless (boundp 'emacs-frame--runtime-id-counter)
    (defvar emacs-frame--runtime-id-counter 0))
  (unless (boundp 'emacs-frame--runtime-registry)
    (defvar emacs-frame--runtime-registry nil))
  (unless (boundp 'emacs-frame--runtime-selected-frame)
    (defvar emacs-frame--runtime-selected-frame nil))
  (unless (fboundp 'emacs-frame--runtime-cell)
    (fset 'emacs-frame--runtime-cell
          '(lambda (frame key)
             (assq key frame))))
  (unless (fboundp 'emacs-frame--runtime-ref)
    (fset 'emacs-frame--runtime-ref
          '(lambda (frame key)
             (cdr (emacs-frame--runtime-cell frame key)))))
  (unless (fboundp 'emacs-frame--runtime-set)
    (fset 'emacs-frame--runtime-set
          '(lambda (frame key value)
             (let ((cell (emacs-frame--runtime-cell frame key)))
               (if cell
                   (setcdr cell value)
                 (setq frame (cons (cons key value) frame)))
               value))))
  (unless (fboundp 'emacs-frame--runtime-frame-object-p)
    (fset 'emacs-frame--runtime-frame-object-p
          '(lambda (object)
             (and (consp object)
                  (assq 'nemacs-frame object)))))
  (unless (fboundp 'emacs-frame--runtime-live-p)
    (fset 'emacs-frame--runtime-live-p
          '(lambda (object)
             (and (emacs-frame--runtime-frame-object-p object)
                  (not (emacs-frame--runtime-ref object 'dead))))))
  (unless (fboundp 'emacs-frame--runtime-make-frame-object)
    (fset 'emacs-frame--runtime-make-frame-object
          '(lambda (&optional parameters)
             (setq emacs-frame--runtime-id-counter
                   (+ emacs-frame--runtime-id-counter 1))
             (let* ((id emacs-frame--runtime-id-counter)
                    (frame (list (cons 'nemacs-frame t)
                                 (cons 'id id)
                                 (cons 'backend 'stub)
                                 (cons 'name (concat "F" (number-to-string id)))
                                 (cons 'width 80)
                                 (cons 'height 24)
                                 (cons 'pixel-width 640)
                                 (cons 'pixel-height 384)
                                 (cons 'left 0)
                                 (cons 'top 0)
                                 (cons 'visible t)
                                 (cons 'parameters nil)
                                 (cons 'dead nil))))
               (when parameters
                 (emacs-frame--runtime-apply-parameters frame parameters))
               frame))))
  (unless (fboundp 'emacs-frame--runtime-ensure-initial)
    (fset 'emacs-frame--runtime-ensure-initial
          '(lambda ()
             (unless (emacs-frame--runtime-live-p
                      emacs-frame--runtime-selected-frame)
               (let ((frame (emacs-frame--runtime-make-frame-object nil)))
                 (setq emacs-frame--runtime-registry (list frame))
                 (setq emacs-frame--runtime-selected-frame frame)))
             emacs-frame--runtime-selected-frame)))
  (unless (fboundp 'emacs-frame--runtime-get)
    (fset 'emacs-frame--runtime-get
          '(lambda (&optional frame)
             (let ((target (if frame
                               frame
                             (emacs-frame--runtime-ensure-initial))))
               (if (emacs-frame--runtime-live-p target)
                   target
                 (emacs-frame--runtime-ensure-initial))))))
  (unless (fboundp 'emacs-frame--runtime-put-parameter)
    (fset 'emacs-frame--runtime-put-parameter
          '(lambda (frame key value)
             (let* ((pcell (emacs-frame--runtime-cell frame 'parameters))
                    (plist (cdr pcell))
                    (existing (assq key plist)))
               (if existing
                   (setcdr existing value)
                 (setcdr pcell (cons (cons key value) plist)))
               value))))
  (unless (fboundp 'emacs-frame--runtime-apply-parameters)
    (fset 'emacs-frame--runtime-apply-parameters
          '(lambda (frame parameters)
             (dolist (pair parameters)
               (let ((key (car pair))
                     (value (cdr pair)))
                 (cond
                  ((eq key 'width)
                   (emacs-frame--runtime-set frame 'width value)
                   (emacs-frame--runtime-set frame 'pixel-width (* value 8)))
                  ((eq key 'height)
                   (emacs-frame--runtime-set frame 'height value)
                   (emacs-frame--runtime-set frame 'pixel-height (* value 16)))
                  ((eq key 'left)
                   (emacs-frame--runtime-set frame 'left value))
                  ((eq key 'top)
                   (emacs-frame--runtime-set frame 'top value))
                  ((eq key 'name)
                   (emacs-frame--runtime-set frame 'name value))
                  ((eq key 'visibility)
                   (emacs-frame--runtime-set frame 'visible value)))
                 (emacs-frame--runtime-put-parameter frame key value)))
             frame)))
  (unless (fboundp 'emacs-frame-reset)
    (fset 'emacs-frame-reset
          '(lambda ()
             (setq emacs-frame--runtime-id-counter 0)
             (setq emacs-frame--runtime-registry nil)
             (setq emacs-frame--runtime-selected-frame nil)
             nil)))
  (unless (fboundp 'framep)
    (fset 'framep
          '(lambda (object)
             (and (emacs-frame--runtime-frame-object-p object)
                  (emacs-frame--runtime-ref object 'backend)))))
  (unless (fboundp 'frame-live-p)
    (fset 'frame-live-p
          '(lambda (object)
             (and (emacs-frame--runtime-live-p object)
                  (emacs-frame--runtime-ref object 'backend)))))
  (unless (fboundp 'selected-frame)
    (fset 'selected-frame
          '(lambda ()
             (emacs-frame--runtime-ensure-initial))))
  (unless (fboundp 'frame-list)
    (fset 'frame-list
          '(lambda ()
             (emacs-frame--runtime-ensure-initial)
             (let ((live nil))
               (dolist (frame emacs-frame--runtime-registry)
                 (when (emacs-frame--runtime-live-p frame)
                   (setq live (append live (list frame)))))
               live))))
  (unless (fboundp 'make-frame)
    (fset 'make-frame
          '(lambda (&optional parameters)
             (emacs-frame--runtime-ensure-initial)
             (let ((frame
                    (emacs-frame--runtime-make-frame-object parameters)))
               (setq emacs-frame--runtime-registry
                     (append emacs-frame--runtime-registry (list frame)))
               frame))))
  (unless (fboundp 'delete-frame)
    (fset 'delete-frame
          '(lambda (&optional frame force)
             (let ((target (emacs-frame--runtime-get frame)))
               (if (<= (length (frame-list)) 1)
                   nil
                 (emacs-frame--runtime-set target 'dead t)
                 (when (eq target emacs-frame--runtime-selected-frame)
                   (setq emacs-frame--runtime-selected-frame
                         (car (frame-list))))
                 nil)))))
  (unless (fboundp 'delete-other-frames)
    (fset 'delete-other-frames
          '(lambda (&optional frame)
             (let ((keep (emacs-frame--runtime-get frame)))
               (dolist (candidate (frame-list))
                 (unless (eq candidate keep)
                   (delete-frame candidate)))
               nil))))
  (unless (fboundp 'window-frame)
    (fset 'window-frame
          '(lambda (&optional window)
             (if (framep window)
                 window
               (selected-frame)))))
  (unless (fboundp 'frame-width)
    (fset 'frame-width
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'width))))
  (unless (fboundp 'frame-height)
    (fset 'frame-height
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'height))))
  (unless (fboundp 'frame-char-width)
    (fset 'frame-char-width '(lambda (&optional frame) 8)))
  (unless (fboundp 'frame-char-height)
    (fset 'frame-char-height '(lambda (&optional frame) 16)))
  (unless (fboundp 'frame-pixel-width)
    (fset 'frame-pixel-width
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'pixel-width))))
  (unless (fboundp 'frame-pixel-height)
    (fset 'frame-pixel-height
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'pixel-height))))
  (unless (fboundp 'set-frame-size)
    (fset 'set-frame-size
          '(lambda (frame cols lines &optional pixelwise)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'width cols)
               (emacs-frame--runtime-set target 'height lines)
               (emacs-frame--runtime-set target 'pixel-width (* cols 8))
               (emacs-frame--runtime-set target 'pixel-height (* lines 16))
               nil))))
  (unless (fboundp 'set-frame-position)
    (fset 'set-frame-position
          '(lambda (frame x y)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'left x)
               (emacs-frame--runtime-set target 'top y)
               nil))))
  (unless (fboundp 'frame-parameters)
    (fset 'frame-parameters
          '(lambda (&optional frame)
             (let ((target (emacs-frame--runtime-get frame)))
               (append
                (list (cons 'width (emacs-frame--runtime-ref target 'width))
                      (cons 'height (emacs-frame--runtime-ref target 'height))
                      (cons 'pixel-width
                            (emacs-frame--runtime-ref target 'pixel-width))
                      (cons 'pixel-height
                            (emacs-frame--runtime-ref target 'pixel-height))
                      (cons 'left (emacs-frame--runtime-ref target 'left))
                      (cons 'top (emacs-frame--runtime-ref target 'top))
                      (cons 'name (emacs-frame--runtime-ref target 'name))
                      (cons 'visibility
                            (emacs-frame--runtime-ref target 'visible)))
                (emacs-frame--runtime-ref target 'parameters))))))
  (unless (fboundp 'frame-parameter)
    (fset 'frame-parameter
          '(lambda (frame parameter)
             (cdr (assq parameter (frame-parameters frame))))))
  (unless (fboundp 'set-frame-parameter)
    (fset 'set-frame-parameter
          '(lambda (frame parameter value)
             (emacs-frame--runtime-apply-parameters
              (emacs-frame--runtime-get frame)
              (list (cons parameter value)))
             value)))
  (unless (fboundp 'modify-frame-parameters)
    (fset 'modify-frame-parameters
          '(lambda (frame alist)
             (emacs-frame--runtime-apply-parameters
              (emacs-frame--runtime-get frame) alist)
             nil)))
  (unless (fboundp 'frame-visible-p)
    (fset 'frame-visible-p
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'visible))))
  (unless (fboundp 'make-frame-visible)
    (fset 'make-frame-visible
          '(lambda (&optional frame)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'visible t)
               target))))
  (unless (fboundp 'make-frame-invisible)
    (fset 'make-frame-invisible
          '(lambda (&optional frame force)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'visible nil)
               target))))
  (unless (fboundp 'raise-frame)
    (fset 'raise-frame
          '(lambda (&optional frame)
             (emacs-frame--runtime-get frame))))
  (unless (fboundp 'lower-frame)
    (fset 'lower-frame
          '(lambda (&optional frame)
             (emacs-frame--runtime-get frame))))
  (unless (fboundp 'select-frame)
    (fset 'select-frame
          '(lambda (frame &optional norecord)
             (when (emacs-frame--runtime-live-p frame)
               (setq emacs-frame--runtime-selected-frame frame))
             emacs-frame--runtime-selected-frame)))
  (unless (fboundp 'frame-focus)
    (fset 'frame-focus
          '(lambda (&optional frame)
             (if frame
                 (and (eq frame emacs-frame--runtime-selected-frame) frame)
               emacs-frame--runtime-selected-frame))))
  (unless (fboundp 'frame-windows)
    (fset 'frame-windows '(lambda (&optional frame) nil)))
  (unless (fboundp 'display-pixel-width)
    (fset 'display-pixel-width '(lambda (&optional display) 1024)))
  (unless (fboundp 'display-pixel-height)
    (fset 'display-pixel-height '(lambda (&optional display) 768)))
  (provide 'frame)
  (provide 'emacs-frame-builtins)
  t)

(defun nemacs-runtime-image-preload--install-tab-core ()
  "Install the minimal tab-bar/tab-line daily-driver surface."
  (unless (boundp 'tab-bar-mode)
    (defvar tab-bar-mode nil))
  (unless (boundp 'tab-bar--tabs)
    (defvar tab-bar--tabs nil))
  (unless (boundp 'tab-bar--selected-index)
    (defvar tab-bar--selected-index 0))
  (unless (boundp 'tab-line-mode)
    (defvar tab-line-mode nil))
  (unless (boundp 'global-tab-line-mode)
    (defvar global-tab-line-mode nil))
  (unless (boundp 'tab-line-format)
    (defvar tab-line-format nil))
  (unless (fboundp 'tab-bar--ensure-tabs)
    (fset 'tab-bar--ensure-tabs
          '(lambda ()
             (unless tab-bar--tabs
               (setq tab-bar--tabs
                     (list (list (cons 'name "1")
                                 (cons 'explicit-name nil))))
               (setq tab-bar--selected-index 0))
             tab-bar--tabs)))
  (unless (fboundp 'tab-bar-tabs)
    (fset 'tab-bar-tabs
          '(lambda (&optional frame)
             (tab-bar--ensure-tabs))))
  (unless (fboundp 'tab-bar-current-tab)
    (fset 'tab-bar-current-tab
          '(lambda (&optional frame)
             (nth tab-bar--selected-index (tab-bar--ensure-tabs)))))
  (unless (fboundp 'tab-bar-current-tab-index)
    (fset 'tab-bar-current-tab-index
          '(lambda (&optional frame)
             (tab-bar--ensure-tabs)
             tab-bar--selected-index)))
  (unless (fboundp 'tab-bar-new-tab)
    (fset 'tab-bar-new-tab
          '(lambda (&optional arg)
             (tab-bar--ensure-tabs)
             (let ((tab (list (cons 'name
                                    (number-to-string
                                     (+ (length tab-bar--tabs) 1)))
                              (cons 'explicit-name nil))))
               (setq tab-bar--tabs (append tab-bar--tabs (list tab)))
               (setq tab-bar--selected-index
                     (- (length tab-bar--tabs) 1))
               tab))))
  (unless (fboundp 'tab-bar-select-tab)
    (fset 'tab-bar-select-tab
          '(lambda (tab-number)
             (tab-bar--ensure-tabs)
             (let ((index (- tab-number 1)))
               (when (and (>= index 0) (< index (length tab-bar--tabs)))
                 (setq tab-bar--selected-index index))
               (tab-bar-current-tab)))))
  (unless (fboundp 'tab-bar-switch-to-next-tab)
    (fset 'tab-bar-switch-to-next-tab
          '(lambda (&optional arg)
             (tab-bar--ensure-tabs)
             (let ((count (length tab-bar--tabs))
                   (index (+ tab-bar--selected-index (or arg 1))))
               (while (< index 0)
                 (setq index (+ index count)))
               (while (>= index count)
                 (setq index (- index count)))
               (setq tab-bar--selected-index index)
               (tab-bar-current-tab)))))
  (unless (fboundp 'tab-bar-switch-to-prev-tab)
    (fset 'tab-bar-switch-to-prev-tab
          '(lambda (&optional arg)
             (tab-bar-switch-to-next-tab (- 0 (or arg 1))))))
  (unless (fboundp 'tab-bar-close-tab)
    (fset 'tab-bar-close-tab
          '(lambda (&optional tab-number)
             (tab-bar--ensure-tabs)
             (if (<= (length tab-bar--tabs) 1)
                 (tab-bar-current-tab)
               (let ((index (if tab-number
                                (- tab-number 1)
                              tab-bar--selected-index))
                     (i 0)
                     (new-tabs nil))
                 (dolist (tab tab-bar--tabs)
                   (unless (= i index)
                     (setq new-tabs (append new-tabs (list tab))))
                   (setq i (+ i 1)))
                 (setq tab-bar--tabs new-tabs)
                 (when (>= tab-bar--selected-index
                           (length tab-bar--tabs))
                   (setq tab-bar--selected-index
                         (- (length tab-bar--tabs) 1)))
                 (tab-bar-current-tab))))))
  (unless (fboundp 'tab-bar-rename-tab)
    (fset 'tab-bar-rename-tab
          '(lambda (name &optional tab-number)
             (tab-bar--ensure-tabs)
             (let* ((index (if tab-number
                               (- tab-number 1)
                             tab-bar--selected-index))
                    (tab (nth index tab-bar--tabs))
                    (name-cell (assq 'name tab))
                    (explicit-cell (assq 'explicit-name tab)))
               (when name-cell
                 (setcdr name-cell name))
               (when explicit-cell
                 (setcdr explicit-cell t))
               tab))))
  (unless (fboundp 'tab-bar-mode)
    (fset 'tab-bar-mode
          '(lambda (&optional arg)
             (setq tab-bar-mode
                   (if arg
                       (> arg 0)
                     (not tab-bar-mode)))
             (tab-bar--ensure-tabs)
             tab-bar-mode)))
  (unless (fboundp 'tab-bar-height)
    (fset 'tab-bar-height
          '(lambda (&optional frame)
             (if tab-bar-mode 1 0))))
  (unless (fboundp 'tab-new)
    (fset 'tab-new '(lambda (&optional arg) (tab-bar-new-tab arg))))
  (unless (fboundp 'tab-close)
    (fset 'tab-close '(lambda (&optional tab-number)
                        (tab-bar-close-tab tab-number))))
  (unless (fboundp 'tab-next)
    (fset 'tab-next '(lambda (&optional arg)
                       (tab-bar-switch-to-next-tab arg))))
  (unless (fboundp 'tab-previous)
    (fset 'tab-previous '(lambda (&optional arg)
                           (tab-bar-switch-to-prev-tab arg))))
  (unless (fboundp 'tab-select)
    (fset 'tab-select '(lambda (tab-number)
                         (tab-bar-select-tab tab-number))))
  (unless (fboundp 'tab-rename)
    (fset 'tab-rename '(lambda (name &optional tab-number)
                         (tab-bar-rename-tab name tab-number))))
  (unless (fboundp 'tab-line-mode)
    (fset 'tab-line-mode
          '(lambda (&optional arg)
             (setq tab-line-mode
                   (if arg
                       (> arg 0)
                     (not tab-line-mode)))
             (setq tab-line-format
                   (and tab-line-mode '(:eval (buffer-name))))
             tab-line-mode)))
  (unless (fboundp 'global-tab-line-mode)
    (fset 'global-tab-line-mode
          '(lambda (&optional arg)
             (setq global-tab-line-mode
                   (if arg
                       (> arg 0)
                     (not global-tab-line-mode)))
             global-tab-line-mode)))
  (unless (fboundp 'window-tab-line-height)
    (fset 'window-tab-line-height
          '(lambda (&optional window)
             (if (or tab-line-mode global-tab-line-mode) 1 0))))
  (unless (fboundp 'tab-line-tabs-buffer-list)
    (fset 'tab-line-tabs-buffer-list
          '(lambda ()
             (if (fboundp 'buffer-list)
                 (buffer-list)
               nil))))
  (unless (fboundp 'tab-line-tabs-window-buffers)
    (fset 'tab-line-tabs-window-buffers
          '(lambda ()
             (tab-line-tabs-buffer-list))))
  (unless (fboundp 'tab-line-tabs-fixed-window-buffers)
    (fset 'tab-line-tabs-fixed-window-buffers
          '(lambda ()
             (tab-line-tabs-buffer-list))))
  (unless (fboundp 'tab-line-tab-name-buffer)
    (fset 'tab-line-tab-name-buffer
          '(lambda (buffer &optional buffers)
             (if (fboundp 'buffer-name)
                 (buffer-name buffer)
               ""))))
  (provide 'tab-bar)
  (provide 'tab-line)
  t)

(defun nemacs-runtime-image-preload--install-simple-core ()
  "Install the minimal `simple' daily-driver surface without source `load'."
  (unless (featurep 'simple)
    (unless (boundp 'max-mini-window-lines)
      (defvar max-mini-window-lines 1))
    (unless (boundp 'indent-line-function)
      (defvar indent-line-function nil))
    (unless (fboundp 'open-line)
      (fset 'open-line
            (lambda (&optional n)
              (let ((count (or n 1))
                    (pos (point)))
                (while (> count 0)
                  (newline)
                  (setq count (1- count)))
                (goto-char pos)))))
    (unless (fboundp 'quoted-insert)
      (fset 'quoted-insert
            (lambda (&optional arg)
              (let ((count (or arg 1))
                    (char (read-char)))
                (while (> count 0)
                  (self-insert-command 1 char)
                  (setq count (1- count)))))))
    (unless (fboundp 'indent-for-tab-command)
      (fset 'indent-for-tab-command
            (lambda (&optional _arg)
              (if (and (boundp 'indent-line-function)
                       (functionp indent-line-function))
                  (funcall indent-line-function)
                (self-insert-command 1 9)))))
    (provide 'simple))
  t)

(defun nemacs-runtime-image-preload--install-subr-x-core ()
  "Install common `subr-x' support helpers without source `load'."
  (unless (featurep 'subr-x)
    (unless (fboundp 'internal--thread-argument)
      (defmacro internal--thread-argument (first &rest forms)
        (let ((value (car forms))
              (tail (cdr forms)))
          (while tail
            (let ((form (car tail)))
              (setq value
                    (cond
                     ((consp form)
                      (if first
                          (cons (car form) (cons value (cdr form)))
                        (append form (list value))))
                     (first (list form value))
                     (t (list form value)))))
            (setq tail (cdr tail)))
          value)))
    (unless (fboundp 'thread-first)
      (defmacro thread-first (&rest forms)
        (declare (indent 0))
        (cons 'internal--thread-argument (cons t forms))))
    (unless (fboundp 'thread-last)
      (defmacro thread-last (&rest forms)
        (declare (indent 0))
        (cons 'internal--thread-argument (cons nil forms))))
    (unless (fboundp 'hash-table-empty-p)
      (fset 'hash-table-empty-p (lambda (hash-table)
                                  (= (hash-table-count hash-table) 0))))
    (unless (fboundp 'hash-table-keys)
      (fset 'hash-table-keys
            (lambda (hash-table)
              (let (keys)
                (maphash (lambda (key _value) (push key keys)) hash-table)
                keys))))
    (unless (fboundp 'hash-table-values)
      (fset 'hash-table-values
            (lambda (hash-table)
              (let (values)
                (maphash (lambda (_key value) (push value values)) hash-table)
                values))))
    (unless (fboundp 'string-remove-prefix)
      (fset 'string-remove-prefix
            (lambda (prefix string)
              (if (string-prefix-p prefix string)
                  (substring string (length prefix))
                string))))
    (unless (fboundp 'string-remove-suffix)
      (fset 'string-remove-suffix
            (lambda (suffix string)
              (if (string-suffix-p suffix string)
                  (substring string 0 (- (length string) (length suffix)))
                string))))
    (unless (fboundp 'string-replace)
      (fset 'string-replace
            (lambda (from-string to-string in-string)
              (if (= (length from-string) 0)
                  in-string
                (let ((start 0)
                      pieces
                      pos)
                  (while (setq pos (string-search from-string in-string start))
                    (push (substring in-string start pos) pieces)
                    (push to-string pieces)
                    (setq start (+ pos (length from-string))))
                  (push (substring in-string start) pieces)
                  (apply #'concat (nreverse pieces)))))))
    (unless (fboundp 'string-limit)
      (fset 'string-limit
            (lambda (string length &optional end _coding-system)
              (cond
               ((<= (length string) length) string)
               (end (substring string (- (length string) length)))
               (t (substring string 0 length))))))
    (unless (fboundp 'string-pad)
      (fset 'string-pad
            (lambda (string length &optional padding start)
              (let ((pad-length (- length (length string))))
                (if (<= pad-length 0)
                    string
                  (let ((pad (make-string pad-length (or padding ?\s))))
                    (if start (concat pad string) (concat string pad))))))))
    (unless (fboundp 'proper-list-p)
      (fset 'proper-list-p
            (lambda (object)
              (let ((tail object)
                    (len 0))
                (while (consp tail)
                  (setq len (1+ len)
                        tail (cdr tail)))
                (and (null tail) len)))))
    (unless (fboundp 'mapcan)
      (fset 'mapcan
            (lambda (function sequence &rest more-sequences)
              (apply #'nconc (apply #'mapcar function sequence more-sequences)))))
    (provide 'subr-x))
  t)

(defun nemacs-runtime-image-preload--seq-list (sequence)
  "Return SEQUENCE as a list."
  (cond
   ((listp sequence) sequence)
   ((vectorp sequence) (append sequence nil))
   ((stringp sequence)
    (let ((i 0)
          (n (length sequence))
          out)
      (while (< i n)
        (push (aref sequence i) out)
        (setq i (1+ i)))
      (nreverse out)))
   (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

(defun nemacs-runtime-image-preload--install-seq-core ()
  "Install common `seq' support helpers without source `load'."
  (unless (featurep 'seq)
    (unless (fboundp 'seqp)
      (fset 'seqp (lambda (object)
                    (or (listp object) (stringp object) (vectorp object)))))
    (unless (fboundp 'seq-length)
      (fset 'seq-length #'length))
    (unless (fboundp 'seq-elt)
      (fset 'seq-elt #'elt))
    (unless (fboundp 'seq-map)
      (fset 'seq-map #'mapcar))
    (unless (fboundp 'seq-filter)
      (fset 'seq-filter
            (lambda (predicate sequence)
              (let (out)
                (dolist (elt (nemacs-runtime-image-preload--seq-list sequence)
                             (nreverse out))
                  (when (funcall predicate elt)
                    (push elt out)))))))
    (unless (fboundp 'seq-remove)
      (fset 'seq-remove
            (lambda (predicate sequence)
              (seq-filter (lambda (elt) (not (funcall predicate elt)))
                          sequence))))
    (unless (fboundp 'seq-find)
      (fset 'seq-find
            (lambda (predicate sequence &optional default)
              (catch 'found
                (dolist (elt (nemacs-runtime-image-preload--seq-list sequence))
                  (when (funcall predicate elt)
                    (throw 'found elt)))
                default))))
    (unless (fboundp 'seq-some)
      (fset 'seq-some
            (lambda (predicate sequence)
              (catch 'found
                (dolist (elt (nemacs-runtime-image-preload--seq-list sequence))
                  (let ((value (funcall predicate elt)))
                    (when value (throw 'found value))))
                nil))))
    (unless (fboundp 'seq-every-p)
      (fset 'seq-every-p
            (lambda (predicate sequence)
              (not (seq-some (lambda (elt) (not (funcall predicate elt)))
                             sequence)))))
    (unless (fboundp 'seq-reduce)
      (fset 'seq-reduce
            (lambda (function sequence initial-value)
              (let ((acc initial-value))
                (dolist (elt (nemacs-runtime-image-preload--seq-list sequence)
                             acc)
                  (setq acc (funcall function acc elt)))))))
    (unless (fboundp 'seq-uniq)
      (fset 'seq-uniq
            (lambda (sequence &optional testfn)
              (let ((test (or testfn #'equal))
                    out)
                (dolist (elt (nemacs-runtime-image-preload--seq-list sequence)
                             (nreverse out))
                  (unless (seq-some (lambda (seen) (funcall test elt seen))
                                    out)
                    (push elt out)))))))
    (unless (fboundp 'seq-concatenate)
      (fset 'seq-concatenate
            (lambda (type &rest sequences)
              (let ((list (apply #'append
                                 (mapcar #'nemacs-runtime-image-preload--seq-list
                                         sequences))))
                (cond
                 ((eq type 'list) list)
                 ((eq type 'vector) (apply #'vector list))
                 ((eq type 'string) (apply #'string list))
                 (t list))))))
    (provide 'seq))
  t)

(defun nemacs-runtime-image-preload--map-pairs (map)
  "Return MAP as key/value cons pairs."
  (cond
   ((hash-table-p map)
    (let (pairs)
      (maphash (lambda (key value) (push (cons key value) pairs)) map)
      pairs))
   ((listp map) map)
   ((vectorp map)
    (let ((i 0)
          pairs)
      (while (< i (length map))
        (push (cons i (aref map i)) pairs)
        (setq i (1+ i)))
      (nreverse pairs)))
   (t nil)))

(defun nemacs-runtime-image-preload--install-map-core ()
  "Install common `map' support helpers without source `load'."
  (unless (featurep 'map)
    (unless (fboundp 'mapp)
      (fset 'mapp (lambda (object)
                    (or (listp object) (hash-table-p object) (vectorp object)))))
    (unless (fboundp 'map-elt)
      (fset 'map-elt
            (lambda (map key &optional default)
              (cond
               ((hash-table-p map) (gethash key map default))
               ((vectorp map) (if (and (integerp key) (< key (length map)))
                                  (aref map key)
                                default))
               ((listp map) (let ((cell (assoc key map)))
                              (if cell (cdr cell) default)))
               (t default)))))
    (unless (fboundp 'map-keys)
      (fset 'map-keys
            (lambda (map)
              (mapcar #'car (nemacs-runtime-image-preload--map-pairs map)))))
    (unless (fboundp 'map-values)
      (fset 'map-values
            (lambda (map)
              (mapcar #'cdr (nemacs-runtime-image-preload--map-pairs map)))))
    (unless (fboundp 'map-pairs)
      (fset 'map-pairs #'nemacs-runtime-image-preload--map-pairs))
    (unless (fboundp 'map-apply)
      (fset 'map-apply
            (lambda (function map)
              (mapcar (lambda (pair) (funcall function (car pair) (cdr pair)))
                      (nemacs-runtime-image-preload--map-pairs map)))))
    (unless (fboundp 'map-do)
      (fset 'map-do
            (lambda (function map)
              (dolist (pair (nemacs-runtime-image-preload--map-pairs map))
                (funcall function (car pair) (cdr pair)))
              nil)))
    (unless (fboundp 'map-empty-p)
      (fset 'map-empty-p
            (lambda (map)
              (cond
               ((hash-table-p map) (= (hash-table-count map) 0))
               (t (= (length map) 0))))))
    (unless (fboundp 'map-contains-key)
      (fset 'map-contains-key
            (lambda (map key)
              (not (eq (map-elt map key :nemacs-missing) :nemacs-missing)))))
    (unless (fboundp 'map-merge)
      (fset 'map-merge
            (lambda (&rest maps)
              (let (out)
                (dolist (map maps out)
                  (dolist (pair (nemacs-runtime-image-preload--map-pairs map))
                    (let ((cell (assoc (car pair) out)))
                      (if cell
                          (setcdr cell (cdr pair))
                        (push (cons (car pair) (cdr pair)) out)))))))))
    (unless (fboundp 'map-merge-with)
      (fset 'map-merge-with
            (lambda (function &rest maps)
              (let (out)
                (dolist (map maps out)
                  (dolist (pair (nemacs-runtime-image-preload--map-pairs map))
                    (let ((cell (assoc (car pair) out)))
                      (if cell
                          (setcdr cell (funcall function (cdr cell) (cdr pair)))
                        (push (cons (car pair) (cdr pair)) out)))))))))
    (unless (fboundp 'map-into)
      (fset 'map-into (lambda (map _type) map)))
    (unless (fboundp 'map-put!)
      (fset 'map-put!
            (lambda (map key value)
              (cond
               ((hash-table-p map) (puthash key value map))
               ((vectorp map) (aset map key value))
               ((listp map) (let ((cell (assoc key map)))
                              (if cell (setcdr cell value)
                                (push (cons key value) map)))))
              map)))
    (unless (fboundp 'map-insert)
      (fset 'map-insert
            (lambda (map key value)
              (cons (cons key value) map))))
    (provide 'map))
  t)

(defun nemacs-runtime-image-preload--lisp-char-at (pos)
  "Return character at POS, or nil when POS is outside the buffer."
  (when (and (>= pos (point-min)) (< pos (point-max)))
    (let ((string (buffer-substring-no-properties pos (1+ pos))))
      (and (> (length string) 0) (aref string 0)))))

(defun nemacs-runtime-image-preload--lisp-space-char-p (char)
  "Return non-nil when CHAR is simple whitespace."
  (memq char '(?\s ?\t ?\n ?\r)))

(defun nemacs-runtime-image-preload--lisp-open-char-p (char)
  "Return non-nil when CHAR opens a list."
  (memq char '(?\( ?\[ ?\{)))

(defun nemacs-runtime-image-preload--lisp-close-char-p (char)
  "Return non-nil when CHAR closes a list."
  (memq char '(?\) ?\] ?\})))

(defun nemacs-runtime-image-preload--lisp-matching-close (open)
  "Return the close delimiter matching OPEN."
  (cdr (assq open '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))))

(defun nemacs-runtime-image-preload--lisp-matching-open (close)
  "Return the open delimiter matching CLOSE."
  (cdr (assq close '((?\) . ?\() (?\] . ?\[) (?\} . ?\{)))))

(defun nemacs-runtime-image-preload--lisp-symbol-char-p (char)
  "Return non-nil when CHAR can be part of a lightweight Lisp atom."
  (and char
       (not (nemacs-runtime-image-preload--lisp-space-char-p char))
       (not (memq char '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\" ?\;)))))

(defun nemacs-runtime-image-preload--lisp-prefix-char-p (char)
  "Return non-nil when CHAR prefixes the next sexp."
  (memq char '(?\' ?` ?,)))

(defun nemacs-runtime-image-preload--lisp-skip-forward-trivia (&optional limit)
  "Move point over whitespace and line comments up to LIMIT."
  (let ((pos (point))
        (end (or limit (point-max))))
    (catch 'done
      (while (< pos end)
        (let ((char (nemacs-runtime-image-preload--lisp-char-at pos)))
          (cond
           ((nemacs-runtime-image-preload--lisp-space-char-p char)
            (setq pos (1+ pos)))
           ((eq char ?\;)
            (while (and (< pos end)
                        (not (eq (nemacs-runtime-image-preload--lisp-char-at
                                  pos)
                                 ?\n)))
              (setq pos (1+ pos))))
           (t
            (throw 'done nil))))))
    (goto-char (min pos end))))

(defun nemacs-runtime-image-preload--lisp-skip-backward-trivia (&optional limit)
  "Move point backward over whitespace down to LIMIT."
  (let ((pos (point))
        (start (or limit (point-min))))
    (while (and (> pos start)
                (nemacs-runtime-image-preload--lisp-space-char-p
                 (nemacs-runtime-image-preload--lisp-char-at (1- pos))))
      (setq pos (1- pos)))
    (goto-char (max pos start))))

(defun nemacs-runtime-image-preload--lisp-scan-string-forward (pos limit)
  "Return position after string at POS, or nil before LIMIT."
  (let ((p (1+ pos))
        found)
    (catch 'done
      (while (< p limit)
        (let ((char (nemacs-runtime-image-preload--lisp-char-at p)))
          (cond
           ((eq char ?\\)
            (setq p (+ p 2)))
           ((eq char ?\")
            (setq found (1+ p))
            (throw 'done nil))
           (t
            (setq p (1+ p)))))))
    found))

(defun nemacs-runtime-image-preload--lisp-scan-atom-forward (pos limit)
  "Return position after atom at POS before LIMIT."
  (let ((p pos))
    (while (and (< p limit)
                (nemacs-runtime-image-preload--lisp-symbol-char-p
                 (nemacs-runtime-image-preload--lisp-char-at p)))
      (setq p (1+ p)))
    p))

(defun nemacs-runtime-image-preload--lisp-scan-one-forward-at (pos limit)
  "Return end position of one sexp starting at POS, or nil."
  (let ((char (nemacs-runtime-image-preload--lisp-char-at pos)))
    (cond
     ((null char) nil)
     ((nemacs-runtime-image-preload--lisp-space-char-p char)
      (save-excursion
        (goto-char pos)
        (nemacs-runtime-image-preload--lisp-skip-forward-trivia limit)
        (nemacs-runtime-image-preload--lisp-scan-one-forward-at
         (point) limit)))
     ((eq char ?\;)
      (save-excursion
        (goto-char pos)
        (nemacs-runtime-image-preload--lisp-skip-forward-trivia limit)
        (nemacs-runtime-image-preload--lisp-scan-one-forward-at
         (point) limit)))
     ((nemacs-runtime-image-preload--lisp-prefix-char-p char)
      (nemacs-runtime-image-preload--lisp-scan-one-forward-at
       (1+ pos) limit))
     ((and (eq char ?#)
           (< (1+ pos) limit)
           (nemacs-runtime-image-preload--lisp-prefix-char-p
            (nemacs-runtime-image-preload--lisp-char-at (1+ pos))))
      (nemacs-runtime-image-preload--lisp-scan-one-forward-at
       (+ pos 2) limit))
     ((eq char ?\")
      (nemacs-runtime-image-preload--lisp-scan-string-forward pos limit))
     ((nemacs-runtime-image-preload--lisp-open-char-p char)
      (let ((close (nemacs-runtime-image-preload--lisp-matching-close char))
            (p (1+ pos))
            done)
        (catch 'done
          (while (< p limit)
            (let ((current
                   (nemacs-runtime-image-preload--lisp-char-at p)))
              (cond
               ((eq current close)
                (setq done (1+ p))
                (throw 'done nil))
               ((nemacs-runtime-image-preload--lisp-close-char-p current)
                (throw 'done nil))
               ((or (nemacs-runtime-image-preload--lisp-open-char-p current)
                    (eq current ?\")
                    (nemacs-runtime-image-preload--lisp-prefix-char-p current)
                    (and (eq current ?#)
                         (< (1+ p) limit)
                         (nemacs-runtime-image-preload--lisp-prefix-char-p
                          (nemacs-runtime-image-preload--lisp-char-at
                           (1+ p)))))
                (let ((next
                       (nemacs-runtime-image-preload--lisp-scan-one-forward-at
                        p limit)))
                  (unless next
                    (throw 'done nil))
                  (setq p next)))
               ((eq current ?\;)
                (while (and (< p limit)
                            (not
                             (eq (nemacs-runtime-image-preload--lisp-char-at
                                  p)
                                 ?\n)))
                  (setq p (1+ p))))
               (t
                (setq p (1+ p)))))))
        done))
     ((nemacs-runtime-image-preload--lisp-close-char-p char)
      nil)
     (t
      (nemacs-runtime-image-preload--lisp-scan-atom-forward pos limit)))))

(defun nemacs-runtime-image-preload--lisp-scan-sexp-forward (&optional limit)
  "Move over one sexp and return point, or nil on failure."
  (let ((start (point))
        (end (or limit (point-max))))
    (nemacs-runtime-image-preload--lisp-skip-forward-trivia end)
    (let ((next
           (nemacs-runtime-image-preload--lisp-scan-one-forward-at
            (point) end)))
      (if next
          (progn (goto-char next) next)
        (goto-char start)
        nil))))

(defun nemacs-runtime-image-preload--lisp-scan-string-backward (pos limit)
  "Return start of string ending before POS, or nil."
  (let ((p (- pos 2))
        found)
    (catch 'done
      (while (>= p limit)
        (let ((char (nemacs-runtime-image-preload--lisp-char-at p)))
          (when (eq char ?\")
            (setq found p)
            (throw 'done nil)))
        (setq p (1- p))))
    found))

(defun nemacs-runtime-image-preload--lisp-scan-list-backward (pos limit)
  "Return start of list ending before POS, or nil."
  (let* ((close (nemacs-runtime-image-preload--lisp-char-at (1- pos)))
         (open (nemacs-runtime-image-preload--lisp-matching-open close))
         (depth 1)
         (p (1- pos))
         found)
    (catch 'done
      (while (> p limit)
        (setq p (1- p))
        (let ((char (nemacs-runtime-image-preload--lisp-char-at p)))
          (cond
           ((eq char close)
            (setq depth (1+ depth)))
           ((eq char open)
            (setq depth (1- depth))
            (when (= depth 0)
              (setq found p)
              (throw 'done nil)))))))
    found))

(defun nemacs-runtime-image-preload--lisp-scan-atom-backward (pos limit)
  "Return start of atom ending at POS."
  (let ((p (1- pos)))
    (while (and (> p limit)
                (nemacs-runtime-image-preload--lisp-symbol-char-p
                 (nemacs-runtime-image-preload--lisp-char-at (1- p))))
      (setq p (1- p)))
    (while (and (> p limit)
                (nemacs-runtime-image-preload--lisp-prefix-char-p
                 (nemacs-runtime-image-preload--lisp-char-at (1- p))))
      (setq p (1- p)))
    (when (and (> p limit)
               (eq (nemacs-runtime-image-preload--lisp-char-at (1- p)) ?#))
      (setq p (1- p)))
    p))

(defun nemacs-runtime-image-preload--lisp-scan-sexp-backward (&optional limit)
  "Move backward over one sexp and return point, or nil on failure."
  (let ((start (point))
        (minpos (or limit (point-min))))
    (nemacs-runtime-image-preload--lisp-skip-backward-trivia minpos)
    (let* ((end (point))
           (char (and (> end minpos)
                      (nemacs-runtime-image-preload--lisp-char-at (1- end))))
           (prev
            (cond
             ((null char) nil)
             ((nemacs-runtime-image-preload--lisp-close-char-p char)
              (nemacs-runtime-image-preload--lisp-scan-list-backward
               end minpos))
             ((eq char ?\")
              (nemacs-runtime-image-preload--lisp-scan-string-backward
               end minpos))
             ((nemacs-runtime-image-preload--lisp-symbol-char-p char)
              (nemacs-runtime-image-preload--lisp-scan-atom-backward
               end minpos))
             (t (1- end)))))
      (if prev
          (progn (goto-char prev) prev)
        (goto-char start)
        nil))))

(defun nemacs-runtime-image-preload--lisp-forward-sexp
    (&optional arg interactive)
  "Move forward across ARG balanced expressions."
  (let ((count (or arg 1)))
    (cond
     ((and forward-sexp-function (not interactive))
      (funcall forward-sexp-function count))
     ((= count 0) nil)
     ((> count 0)
      (while (> count 0)
        (unless (nemacs-runtime-image-preload--lisp-scan-sexp-forward)
          (signal 'scan-error (list "No next sexp" (point) (point-max))))
        (setq count (1- count))))
     (t
      (while (< count 0)
        (unless (nemacs-runtime-image-preload--lisp-scan-sexp-backward)
          (signal 'scan-error
                  (list "No previous sexp" (point-min) (point))))
        (setq count (1+ count)))))))

(defun nemacs-runtime-image-preload--lisp-backward-sexp
    (&optional arg interactive)
  "Move backward across ARG balanced expressions."
  (nemacs-runtime-image-preload--lisp-forward-sexp
   (- (or arg 1)) interactive))

(defun nemacs-runtime-image-preload--lisp-forward-list
    (&optional arg _interactive)
  "Move forward across ARG parenthesized groups."
  (nemacs-runtime-image-preload--lisp-forward-sexp (or arg 1)))

(defun nemacs-runtime-image-preload--lisp-backward-list
    (&optional arg _interactive)
  "Move backward across ARG parenthesized groups."
  (nemacs-runtime-image-preload--lisp-forward-sexp (- (or arg 1))))

(defun nemacs-runtime-image-preload--lisp-down-list (&optional arg _interactive)
  "Move forward into ARG nested lists."
  (let ((count (or arg 1)))
    (while (> count 0)
      (let ((pos (point))
            (end (point-max))
            found)
        (catch 'done
          (while (< pos end)
            (when (nemacs-runtime-image-preload--lisp-open-char-p
                   (nemacs-runtime-image-preload--lisp-char-at pos))
              (setq found (1+ pos))
              (throw 'done nil))
            (setq pos (1+ pos))))
        (unless found
          (signal 'scan-error (list "No containing list" (point) end)))
        (goto-char found))
      (setq count (1- count)))))

(defun nemacs-runtime-image-preload--lisp-up-list
    (&optional arg _escape-strings _no-syntax-crossing)
  "Move forward out of ARG containing lists."
  (let ((count (or arg 1)))
    (while (> count 0)
      (let ((pos (point))
            (end (point-max))
            found)
        (catch 'done
          (while (< pos end)
            (let ((char (nemacs-runtime-image-preload--lisp-char-at pos)))
              (cond
               ((eq char ?\")
                (setq pos
                      (or (nemacs-runtime-image-preload--lisp-scan-string-forward
                           pos end)
                          end)))
               ((nemacs-runtime-image-preload--lisp-close-char-p char)
                (setq found (1+ pos))
                (throw 'done nil))
               (t
                (setq pos (1+ pos)))))))
        (unless found
          (signal 'scan-error (list "No containing list" (point) end)))
        (goto-char found))
      (setq count (1- count)))))

(defun nemacs-runtime-image-preload--lisp-backward-up-list
    (&optional arg _escape-strings _no-syntax-crossing)
  "Move backward out of ARG containing lists."
  (let ((count (or arg 1)))
    (while (> count 0)
      (let ((pos (point))
            (minpos (point-min))
            found)
        (catch 'done
          (while (> pos minpos)
            (setq pos (1- pos))
            (when (nemacs-runtime-image-preload--lisp-open-char-p
                   (nemacs-runtime-image-preload--lisp-char-at pos))
              (setq found pos)
              (throw 'done nil))))
        (unless found
          (signal 'scan-error (list "No containing list" minpos (point))))
        (goto-char found))
      (setq count (1- count)))))

(defun nemacs-runtime-image-preload--lisp-kill-sexp
    (&optional arg _interactive)
  "Kill ARG sexps after point."
  (let ((start (point)))
    (nemacs-runtime-image-preload--lisp-forward-sexp (or arg 1))
    (kill-region start (point))))

(defun nemacs-runtime-image-preload--lisp-beginning-of-defun-once ()
  "Move to the previous top-level form opener."
  (let ((pos (point))
        found)
    (catch 'done
      (while (> pos (point-min))
        (setq pos (1- pos))
        (when (and (eq (nemacs-runtime-image-preload--lisp-char-at pos)
                       ?\()
                   (or (= pos (point-min))
                       (eq (nemacs-runtime-image-preload--lisp-char-at
                            (1- pos))
                           ?\n)))
          (setq found pos)
          (throw 'done nil))))
    (goto-char (or found (point-min)))))

(defun nemacs-runtime-image-preload--lisp-beginning-of-defun (&optional arg)
  "Move to the beginning of ARG top-level forms."
  (let ((count (or arg 1)))
    (cond
     (beginning-of-defun-function
      (funcall beginning-of-defun-function count))
     ((>= count 0)
      (while (> count 0)
        (nemacs-runtime-image-preload--lisp-beginning-of-defun-once)
        (setq count (1- count))))
     (t
      (nemacs-runtime-image-preload--lisp-end-of-defun (- count))))))

(defun nemacs-runtime-image-preload--lisp-end-of-defun
    (&optional arg _interactive)
  "Move to the end of ARG top-level forms."
  (let ((count (or arg 1)))
    (cond
     (end-of-defun-function
      (funcall end-of-defun-function count))
     ((>= count 0)
      (while (> count 0)
        (nemacs-runtime-image-preload--lisp-beginning-of-defun 1)
        (nemacs-runtime-image-preload--lisp-forward-sexp 1)
        (when end-of-defun-moves-to-eol
          (end-of-line))
        (setq count (1- count))))
     (t
      (nemacs-runtime-image-preload--lisp-beginning-of-defun (- count))))))

(defun nemacs-runtime-image-preload--lisp-mark (&optional force)
  "Return fallback mark position."
  (if (or lisp--mark force)
      lisp--mark
    (signal 'mark-inactive nil)))

(defun nemacs-runtime-image-preload--lisp-set-mark (pos)
  "Set fallback mark to POS."
  (setq lisp--mark pos
        mark-active t)
  nil)

(defun nemacs-runtime-image-preload--lisp-push-mark
    (&optional location _nomsg activate)
  "Set fallback mark to LOCATION or point."
  (nemacs-runtime-image-preload--lisp-set-mark (or location (point)))
  (setq mark-active (or activate mark-active))
  nil)

(defun nemacs-runtime-image-preload--lisp-mark-sexp
    (&optional arg _allow-extend)
  "Set mark ARG sexps from point."
  (nemacs-runtime-image-preload--lisp-push-mark
   (save-excursion
     (nemacs-runtime-image-preload--lisp-forward-sexp (or arg 1))
     (point))
   nil t))

(defun nemacs-runtime-image-preload--lisp-mark-defun
    (&optional arg _interactive)
  "Set mark around ARG defuns."
  (let ((start (save-excursion
                 (nemacs-runtime-image-preload--lisp-beginning-of-defun 1)
                 (point)))
        (end (save-excursion
               (nemacs-runtime-image-preload--lisp-end-of-defun (or arg 1))
               (point))))
    (goto-char start)
    (nemacs-runtime-image-preload--lisp-push-mark end nil t)))

(defun nemacs-runtime-image-preload--lisp-insert-pair
    (&optional arg open close)
  "Insert OPEN and CLOSE around point or ARG following sexps."
  (let* ((open-char (or open ?\())
         (pair (or close (cdr (assq open-char insert-pair-alist))))
         (close-char (cond
                      ((consp pair) (cdr pair))
                      ((integerp pair) pair)
                      (t ?\)))))
    (insert (string open-char))
    (let ((mid (point)))
      (when arg
        (nemacs-runtime-image-preload--lisp-forward-sexp
         (prefix-numeric-value arg)))
      (insert (string close-char))
      (unless arg
        (goto-char mid)))))

(defun nemacs-runtime-image-preload--lisp-delete-pair (&optional arg)
  "Delete ARG pairs around point."
  (let ((count (or arg 1)))
    (while (> count 0)
      (let* ((open-pos (point))
             (open (nemacs-runtime-image-preload--lisp-char-at open-pos))
             (end (and (nemacs-runtime-image-preload--lisp-open-char-p open)
                       (nemacs-runtime-image-preload--lisp-scan-one-forward-at
                        open-pos (point-max)))))
        (unless end
          (signal 'scan-error (list "No pair at point" open-pos (point-max))))
        (delete-region (1- end) end)
        (delete-region open-pos (1+ open-pos)))
      (setq count (1- count)))))

(defun nemacs-runtime-image-preload--lisp-check-parens-range (start end)
  "Return nil when START..END has balanced delimiters, else error."
  (let ((pos start)
        stack)
    (while (< pos end)
      (let ((char (nemacs-runtime-image-preload--lisp-char-at pos)))
        (cond
         ((eq char ?\")
          (let ((next
                 (nemacs-runtime-image-preload--lisp-scan-string-forward
                  pos end)))
            (unless next
              (signal 'scan-error
                      (list "Unmatched string quote" pos end)))
            (setq pos (1- next))))
         ((eq char ?\;)
          (while (and (< pos end)
                      (not (eq (nemacs-runtime-image-preload--lisp-char-at
                                pos)
                               ?\n)))
            (setq pos (1+ pos))))
         ((nemacs-runtime-image-preload--lisp-open-char-p char)
          (setq stack (cons char stack)))
         ((nemacs-runtime-image-preload--lisp-close-char-p char)
          (let ((open
                 (nemacs-runtime-image-preload--lisp-matching-open char)))
            (unless (and stack (eq (car stack) open))
              (signal 'scan-error
                      (list "Unmatched closing delimiter" pos end)))
            (setq stack (cdr stack))))))
      (setq pos (1+ pos)))
    (when stack
      (signal 'scan-error (list "Unmatched opening delimiter" start end))))
  nil)

(defun nemacs-runtime-image-preload--lisp-check-parens ()
  "Signal `scan-error' when the current buffer has unmatched delimiters."
  (nemacs-runtime-image-preload--lisp-check-parens-range
   (point-min) (point-max)))

(defun nemacs-runtime-image-preload--install-lisp-core ()
  "Install lightweight Lisp editing helpers without source `load'."
  (unless (featurep 'lisp)
    (unless (boundp 'defun-prompt-regexp)
      (set 'defun-prompt-regexp nil))
    (unless (boundp 'parens-require-spaces)
      (set 'parens-require-spaces t))
    (unless (boundp 'forward-sexp-function)
      (set 'forward-sexp-function nil))
    (unless (boundp 'beginning-of-defun-function)
      (set 'beginning-of-defun-function nil))
    (unless (boundp 'end-of-defun-function)
      (set 'end-of-defun-function nil))
    (unless (boundp 'end-of-defun-moves-to-eol)
      (set 'end-of-defun-moves-to-eol t))
    (unless (boundp 'narrow-to-defun-include-comments)
      (set 'narrow-to-defun-include-comments nil))
    (unless (boundp 'insert-pair-alist)
      (set 'insert-pair-alist
           '((?\( ?\( . ?\)) (?\[ ?\[ . ?\]) (?\{ ?\{ . ?\})
             (?\" ?\" . ?\") (?\' ?\' . ?\'))))
    (unless (boundp 'delete-pair-blink-delay)
      (set 'delete-pair-blink-delay nil))
    (unless (boundp 'lisp--mark) (set 'lisp--mark nil))
    (unless (boundp 'mark-active) (set 'mark-active nil))
    (unless (fboundp 'buffer-end)
      (fset 'buffer-end
            (lambda (arg)
              (if (> (or arg 1) 0) (point-max) (point-min)))))
    (unless (fboundp 'forward-sexp-default-function)
      (fset 'forward-sexp-default-function
            (lambda (&optional arg) (forward-sexp arg))))
    (unless (fboundp 'forward-sexp)
      (fset 'forward-sexp
            #'nemacs-runtime-image-preload--lisp-forward-sexp))
    (unless (fboundp 'backward-sexp)
      (fset 'backward-sexp
            #'nemacs-runtime-image-preload--lisp-backward-sexp))
    (unless (fboundp 'forward-list)
      (fset 'forward-list
            #'nemacs-runtime-image-preload--lisp-forward-list))
    (unless (fboundp 'backward-list)
      (fset 'backward-list
            #'nemacs-runtime-image-preload--lisp-backward-list))
    (unless (fboundp 'down-list)
      (fset 'down-list #'nemacs-runtime-image-preload--lisp-down-list))
    (unless (fboundp 'up-list)
      (fset 'up-list #'nemacs-runtime-image-preload--lisp-up-list))
    (unless (fboundp 'backward-up-list)
      (fset 'backward-up-list
            #'nemacs-runtime-image-preload--lisp-backward-up-list))
    (unless (fboundp 'kill-sexp)
      (fset 'kill-sexp #'nemacs-runtime-image-preload--lisp-kill-sexp))
    (unless (fboundp 'backward-kill-sexp)
      (fset 'backward-kill-sexp
            (lambda (&optional arg interactive)
              (nemacs-runtime-image-preload--lisp-kill-sexp
               (- (or arg 1)) interactive))))
    (unless (fboundp 'kill-backward-up-list)
      (fset 'kill-backward-up-list
            (lambda (&optional arg)
              (let ((end (point)))
                (nemacs-runtime-image-preload--lisp-backward-up-list
                 (or arg 1))
                (kill-region (point) end)))))
    (unless (fboundp 'beginning-of-defun)
      (fset 'beginning-of-defun
            #'nemacs-runtime-image-preload--lisp-beginning-of-defun))
    (unless (fboundp 'beginning-of-defun-raw)
      (fset 'beginning-of-defun-raw
            #'nemacs-runtime-image-preload--lisp-beginning-of-defun))
    (unless (fboundp 'beginning-of-defun-comments)
      (fset 'beginning-of-defun-comments
            #'nemacs-runtime-image-preload--lisp-beginning-of-defun))
    (unless (fboundp 'end-of-defun)
      (fset 'end-of-defun
            #'nemacs-runtime-image-preload--lisp-end-of-defun))
    (unless (fboundp 'mark)
      (fset 'mark #'nemacs-runtime-image-preload--lisp-mark))
    (unless (fboundp 'set-mark)
      (fset 'set-mark #'nemacs-runtime-image-preload--lisp-set-mark))
    (unless (fboundp 'push-mark)
      (fset 'push-mark #'nemacs-runtime-image-preload--lisp-push-mark))
    (unless (fboundp 'mark-sexp)
      (fset 'mark-sexp #'nemacs-runtime-image-preload--lisp-mark-sexp))
    (unless (fboundp 'mark-defun)
      (fset 'mark-defun #'nemacs-runtime-image-preload--lisp-mark-defun))
    (unless (fboundp 'narrow-to-defun)
      (fset 'narrow-to-defun
            (lambda (&optional include-comments)
              (ignore include-comments narrow-to-defun-include-comments)
              (let ((start (save-excursion
                             (beginning-of-defun 1)
                             (point)))
                    (end (save-excursion
                           (end-of-defun 1)
                           (point))))
                (narrow-to-region start end)))))
    (unless (fboundp 'insert-pair)
      (fset 'insert-pair
            #'nemacs-runtime-image-preload--lisp-insert-pair))
    (unless (fboundp 'insert-parentheses)
      (fset 'insert-parentheses
            (lambda (&optional arg)
              (nemacs-runtime-image-preload--lisp-insert-pair
               arg ?\( ?\)))))
    (unless (fboundp 'delete-pair)
      (fset 'delete-pair
            #'nemacs-runtime-image-preload--lisp-delete-pair))
    (unless (fboundp 'raise-sexp)
      (fset 'raise-sexp
            (lambda (&optional n)
              (let ((start (save-excursion
                             (backward-up-list 1)
                             (point)))
                    (end (save-excursion
                           (up-list 1)
                           (point)))
                    (sexp-start (point))
                    sexp-end text)
                (forward-sexp (or n 1))
                (setq sexp-end (point)
                      text (buffer-substring-no-properties
                            sexp-start sexp-end))
                (delete-region start end)
                (insert text)))))
    (unless (fboundp 'move-past-close-and-reindent)
      (fset 'move-past-close-and-reindent
            (lambda ()
              (when (nemacs-runtime-image-preload--lisp-close-char-p
                     (nemacs-runtime-image-preload--lisp-char-at (point)))
                (forward-char 1))
              nil)))
    (unless (fboundp 'check-parens)
      (fset 'check-parens
            #'nemacs-runtime-image-preload--lisp-check-parens))
    (unless (fboundp 'field-complete)
      (fset 'field-complete
            (lambda (_table &optional _predicate)
              (when (fboundp 'completion-at-point)
                (completion-at-point)))))
    (unless (fboundp 'lisp-complete-symbol)
      (fset 'lisp-complete-symbol
            (lambda (&optional _predicate)
              (when (fboundp 'completion-at-point)
                (completion-at-point)))))
    (provide 'lisp))
  t)

(defconst nemacs-runtime-image-preload--case-table-size 256
  "Number of character slots in the runtime-image case-table facade.")

(defconst nemacs-runtime-image-preload--case-table-extra-slots 3
  "Number of extra slots in the runtime-image case-table facade.")

(defun nemacs-runtime-image-preload--case-table-extra-index (slot)
  "Return vector index for extra SLOT."
  (+ nemacs-runtime-image-preload--case-table-size slot))

(defun nemacs-runtime-image-preload--case-table-identity-table ()
  "Return a fresh lightweight char-table with identity entries."
  (let ((table (make-vector
                (+ nemacs-runtime-image-preload--case-table-size
                   nemacs-runtime-image-preload--case-table-extra-slots)
                nil))
        (i 0))
    (while (< i nemacs-runtime-image-preload--case-table-size)
      (aset table i i)
      (setq i (1+ i)))
    table))

(defun nemacs-runtime-image-preload--case-table-copy-vector (vector)
  "Return a shallow copy of VECTOR."
  (let* ((len (length vector))
         (copy (make-vector len nil))
         (i 0))
    (while (< i len)
      (aset copy i (aref vector i))
      (setq i (1+ i)))
    copy))

(defun nemacs-runtime-image-preload--case-table-make-char-table
    (&optional _subtype init)
  "Make a lightweight char-table filled with INIT or identity mappings."
  (if init
      (make-vector
       (+ nemacs-runtime-image-preload--case-table-size
          nemacs-runtime-image-preload--case-table-extra-slots)
       init)
    (nemacs-runtime-image-preload--case-table-identity-table)))

(defun nemacs-runtime-image-preload--case-table-char-table-p (object)
  "Return non-nil when OBJECT is a lightweight char-table."
  (and (vectorp object)
       (= (length object)
          (+ nemacs-runtime-image-preload--case-table-size
             nemacs-runtime-image-preload--case-table-extra-slots))))

(defun nemacs-runtime-image-preload--case-table-char-table-range
    (table range)
  "Return TABLE entry at RANGE."
  (cond
   ((integerp range) (aref table range))
   ((eq range t) nil)
   ((consp range) (aref table (car range)))
   (t nil)))

(defun nemacs-runtime-image-preload--case-table-set-char-table-range
    (table range value)
  "Set TABLE RANGE to VALUE."
  (cond
   ((integerp range)
    (aset table range value))
   ((consp range)
    (let ((i (car range))
          (end (cdr range)))
      (while (<= i end)
        (aset table i value)
        (setq i (1+ i)))))
   ((eq range t)
    nil))
  value)

(defun nemacs-runtime-image-preload--case-table-extra-slot
    (table slot)
  "Return extra SLOT from lightweight TABLE."
  (aref table (nemacs-runtime-image-preload--case-table-extra-index slot)))

(defun nemacs-runtime-image-preload--case-table-set-extra-slot
    (table slot value)
  "Set extra SLOT in lightweight TABLE to VALUE."
  (aset table
        (nemacs-runtime-image-preload--case-table-extra-index slot)
        value))

(defun nemacs-runtime-image-preload--case-table-map
    (function table)
  "Call FUNCTION for every non-nil character entry in TABLE."
  (let ((i 0))
    (while (< i nemacs-runtime-image-preload--case-table-size)
      (let ((value (aref table i)))
        (when value
          (funcall function i value)))
      (setq i (1+ i)))))

(defun nemacs-runtime-image-preload--case-table-ensure-extra-slots
    (case-table)
  "Ensure CASE-TABLE has up/canon/eqv extra slots."
  (unless (char-table-extra-slot case-table 0)
    (let ((up (nemacs-runtime-image-preload--case-table-identity-table))
          (i 0))
      (while (< i nemacs-runtime-image-preload--case-table-size)
        (let ((down (aref case-table i)))
          (when (and (integerp down)
                     (>= down 0)
                     (< down nemacs-runtime-image-preload--case-table-size))
            (aset up down i)))
        (setq i (1+ i)))
      (set-char-table-extra-slot case-table 0 up)))
  (unless (char-table-extra-slot case-table 1)
    (set-char-table-extra-slot
     case-table 1
     (nemacs-runtime-image-preload--case-table-identity-table)))
  (unless (char-table-extra-slot case-table 2)
    (set-char-table-extra-slot
     case-table 2
     (nemacs-runtime-image-preload--case-table-identity-table)))
  case-table)

(defun nemacs-runtime-image-preload--case-table-get-table
    (case-table table)
  "Return TABLE from CASE-TABLE."
  (let ((slot (cdr (assq table '((up . 0) (canon . 1) (eqv . 2))))))
    (cond
     ((eq table 'down) case-table)
     (slot
      (nemacs-runtime-image-preload--case-table-ensure-extra-slots
       case-table)
      (char-table-extra-slot case-table slot))
     (t nil))))

(defun nemacs-runtime-image-preload--case-table-copy (case-table)
  "Return a shallow copy of CASE-TABLE with derived slots invalidated."
  (let ((copy
         (nemacs-runtime-image-preload--case-table-copy-vector case-table))
        (up (char-table-extra-slot case-table 0)))
    (when up
      (set-char-table-extra-slot
       copy 0
       (nemacs-runtime-image-preload--case-table-copy-vector up)))
    (set-char-table-extra-slot copy 1 nil)
    (set-char-table-extra-slot copy 2 nil)
    copy))

(defun nemacs-runtime-image-preload--case-table-set-standard (table)
  "Set the standard lightweight case table to TABLE."
  (setq case-table--standard table
        case-table--current table)
  (nemacs-runtime-image-preload--case-table-ensure-extra-slots table)
  table)

(defun nemacs-runtime-image-preload--case-table-set-current (table)
  "Set the current lightweight case table to TABLE."
  (setq case-table--current table)
  (nemacs-runtime-image-preload--case-table-ensure-extra-slots table)
  table)

(defun nemacs-runtime-image-preload--case-table-set-delims (l r table)
  "Make L and R non-case-converting delimiters in TABLE."
  (aset table l l)
  (aset table r r)
  (let ((up (case-table-get-table table 'up)))
    (aset up l l)
    (aset up r r))
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  nil)

(defun nemacs-runtime-image-preload--case-table-set-pair (uc lc table)
  "Make UC and LC an inter-case-converting pair in TABLE."
  (aset table uc lc)
  (aset table lc lc)
  (let ((up (case-table-get-table table 'up)))
    (aset up uc uc)
    (aset up lc uc))
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  nil)

(defun nemacs-runtime-image-preload--case-table-set-upcase (uc lc table)
  "Make UC an upcase character for LC in TABLE."
  (aset table lc lc)
  (let ((up (case-table-get-table table 'up)))
    (aset up uc uc)
    (aset up lc uc))
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  nil)

(defun nemacs-runtime-image-preload--case-table-set-downcase (uc lc table)
  "Make LC a downcase character for UC in TABLE."
  (aset table uc lc)
  (aset table lc lc)
  (let ((up (case-table-get-table table 'up)))
    (aset up uc uc))
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  nil)

(defun nemacs-runtime-image-preload--case-table-set-syntax
    (char _syntax table)
  "Make CHAR case-invariant in TABLE."
  (aset table char char)
  (let ((up (case-table-get-table table 'up)))
    (aset up char char))
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  nil)

(defun nemacs-runtime-image-preload--install-case-table-core ()
  "Install lightweight case-table helpers without source `load'."
  (unless (featurep 'case-table)
    (unless (fboundp 'make-char-table)
      (fset 'make-char-table
            #'nemacs-runtime-image-preload--case-table-make-char-table))
    (unless (fboundp 'char-table-p)
      (fset 'char-table-p
            #'nemacs-runtime-image-preload--case-table-char-table-p))
    (unless (fboundp 'char-table-range)
      (fset 'char-table-range
            #'nemacs-runtime-image-preload--case-table-char-table-range))
    (unless (fboundp 'set-char-table-range)
      (fset 'set-char-table-range
            #'nemacs-runtime-image-preload--case-table-set-char-table-range))
    (unless (fboundp 'char-table-extra-slot)
      (fset 'char-table-extra-slot
            #'nemacs-runtime-image-preload--case-table-extra-slot))
    (unless (fboundp 'set-char-table-extra-slot)
      (fset 'set-char-table-extra-slot
            #'nemacs-runtime-image-preload--case-table-set-extra-slot))
    (unless (fboundp 'map-char-table)
      (fset 'map-char-table #'nemacs-runtime-image-preload--case-table-map))
    (unless (fboundp 'set-char-table-parent)
      (fset 'set-char-table-parent (lambda (&rest _args) nil)))
    (unless (boundp 'case-table--standard)
      (defvar case-table--standard (make-char-table 'case-table)))
    (unless (boundp 'case-table--current)
      (defvar case-table--current case-table--standard))
    (unless (boundp 'case-table--standard-syntax-table)
      (defvar case-table--standard-syntax-table
        (make-char-table 'syntax-table)))
    (nemacs-runtime-image-preload--case-table-ensure-extra-slots
     case-table--standard)
    (unless (fboundp 'standard-case-table)
      (fset 'standard-case-table (lambda () case-table--standard)))
    (unless (fboundp 'current-case-table)
      (fset 'current-case-table (lambda () case-table--current)))
    (unless (fboundp 'set-standard-case-table)
      (fset 'set-standard-case-table
            #'nemacs-runtime-image-preload--case-table-set-standard))
    (unless (fboundp 'set-case-table)
      (fset 'set-case-table
            #'nemacs-runtime-image-preload--case-table-set-current))
    (unless (fboundp 'standard-syntax-table)
      (fset 'standard-syntax-table
            (lambda () case-table--standard-syntax-table)))
    (unless (fboundp 'modify-syntax-entry)
      (fset 'modify-syntax-entry (lambda (&rest _args) nil)))
    (unless (fboundp 'describe-buffer-case-table)
      (fset 'describe-buffer-case-table
            (lambda ()
              (interactive)
              (message "case-table: lightweight ASCII table"))))
    (unless (fboundp 'case-table-get-table)
      (fset 'case-table-get-table
            #'nemacs-runtime-image-preload--case-table-get-table))
    (unless (fboundp 'get-upcase-table)
      (fset 'get-upcase-table
            (lambda (case-table)
              (case-table-get-table case-table 'up))))
    (unless (fboundp 'copy-case-table)
      (fset 'copy-case-table
            #'nemacs-runtime-image-preload--case-table-copy))
    (unless (fboundp 'set-case-syntax-delims)
      (fset 'set-case-syntax-delims
            #'nemacs-runtime-image-preload--case-table-set-delims))
    (unless (fboundp 'set-case-syntax-pair)
      (fset 'set-case-syntax-pair
            #'nemacs-runtime-image-preload--case-table-set-pair))
    (unless (fboundp 'set-upcase-syntax)
      (fset 'set-upcase-syntax
            #'nemacs-runtime-image-preload--case-table-set-upcase))
    (unless (fboundp 'set-downcase-syntax)
      (fset 'set-downcase-syntax
            #'nemacs-runtime-image-preload--case-table-set-downcase))
    (unless (fboundp 'set-case-syntax)
      (fset 'set-case-syntax
            #'nemacs-runtime-image-preload--case-table-set-syntax))
    (provide 'case-table))
  t)

(defun nemacs-runtime-image-preload--install-cdl-core ()
  "Install lightweight `cdl' command names without source `load'."
  (unless (featurep 'cdl)
    (unless (fboundp 'cdl-get-file)
      (fset 'cdl-get-file
            (lambda (&rest args)
              (apply #'call-process "ncdump" nil t nil args))))
    (unless (fboundp 'cdl-put-region)
      (fset 'cdl-put-region
            (lambda (start end file)
              (call-process-region start end "ncgen" nil nil nil "-o" file))))
    (provide 'cdl))
  t)

(defun nemacs-runtime-image-preload--range-normalize (range)
  "Normalize RANGE for the direct runtime-image `range' facade."
  (if (listp (cdr-safe range))
      range
    (list range)))

(defun nemacs-runtime-image-preload--range-denormalize (range)
  "Return a single span when RANGE contains exactly one cons span."
  (if (and (consp (car range))
           (null (cdr range)))
      (car range)
    range))

(defun nemacs-runtime-image-preload--range-span-start (span)
  "Return first number in SPAN."
  (if (consp span) (car span) span))

(defun nemacs-runtime-image-preload--range-span-end (span)
  "Return last number in SPAN."
  (if (consp span) (cdr span) span))

(defun nemacs-runtime-image-preload--range-insert-sorted-unique
    (number numbers)
  "Insert NUMBER into sorted NUMBERS unless it is already present."
  (cond
   ((null numbers) (list number))
   ((= number (car numbers)) numbers)
   ((< number (car numbers)) (cons number numbers))
   (t (cons (car numbers)
            (nemacs-runtime-image-preload--range-insert-sorted-unique
             number (cdr numbers))))))

(defun nemacs-runtime-image-preload--range-numbers (range)
  "Expand RANGE into a sorted list of unique numbers."
  (let (numbers)
    (dolist (span (nemacs-runtime-image-preload--range-normalize range))
      (cond
       ((numberp span)
        (setq numbers
              (nemacs-runtime-image-preload--range-insert-sorted-unique
               span numbers)))
       ((consp span)
        (let ((number (car span))
              (end (cdr span)))
          (while (<= number end)
            (setq numbers
                  (nemacs-runtime-image-preload--range-insert-sorted-unique
                   number numbers))
            (setq number (1+ number)))))))
    numbers))

(defun nemacs-runtime-image-preload--range-number-member-p
    (number numbers)
  "Return non-nil when NUMBER appears in sorted NUMBERS."
  (catch 'done
    (while numbers
      (cond
       ((= number (car numbers))
        (throw 'done t))
       ((< number (car numbers))
        (throw 'done nil)))
      (setq numbers (cdr numbers)))
    nil))

(defun nemacs-runtime-image-preload--range-compress-list (numbers)
  "Convert sorted NUMBERS to a compact range list."
  (let ((numbers (copy-sequence numbers))
        result first last)
    (while numbers
      (let ((number (car numbers)))
        (cond
         ((null first)
          (setq first number
                last number))
         ((= number last)
          nil)
         ((= number (1+ last))
          (setq last number))
         (t
          (push (if (= first last) first (cons first last)) result)
          (setq first number
                last number))))
      (setq numbers (cdr numbers)))
    (when first
      (push (if (= first last) first (cons first last)) result))
    (nreverse result)))

(defun nemacs-runtime-image-preload--install-range-core ()
  "Install lightweight `range' helpers without source `load'."
  (unless (featurep 'range)
    (unless (fboundp 'range-normalize)
      (fset 'range-normalize
            '(lambda (range)
               (nemacs-runtime-image-preload--range-normalize range))))
    (unless (fboundp 'range-denormalize)
      (fset 'range-denormalize
            '(lambda (range)
               (nemacs-runtime-image-preload--range-denormalize range))))
    (unless (fboundp 'range-compress-list)
      (fset 'range-compress-list
            '(lambda (numbers)
               (nemacs-runtime-image-preload--range-compress-list numbers))))
    (unless (fboundp 'range-uncompress)
      (fset 'range-uncompress
            '(lambda (ranges)
               (nemacs-runtime-image-preload--range-numbers ranges))))
    (unless (fboundp 'range-concat)
      (fset 'range-concat
            '(lambda (range1 range2)
               (range-compress-list
                (let ((numbers
                       (nemacs-runtime-image-preload--range-numbers range1)))
                  (dolist (number
                           (nemacs-runtime-image-preload--range-numbers range2)
                           numbers)
                    (setq numbers
                          (nemacs-runtime-image-preload--range-insert-sorted-unique
                           number numbers))))))))
    (unless (fboundp 'range-add-list)
      (fset 'range-add-list
            '(lambda (ranges list)
               (range-concat ranges (range-compress-list list)))))
    (unless (fboundp 'range-difference)
      (fset 'range-difference
            '(lambda (range1 range2)
               (let ((remove
                      (nemacs-runtime-image-preload--range-numbers range2))
                     result)
                 (dolist (number
                          (nemacs-runtime-image-preload--range-numbers range1))
                   (unless
                       (nemacs-runtime-image-preload--range-number-member-p
                        number remove)
                     (push number result)))
                 (range-compress-list (nreverse result))))))
    (unless (fboundp 'range-remove)
      (fset 'range-remove
            '(lambda (range1 range2)
               (range-difference range1 range2))))
    (unless (fboundp 'range-intersection)
      (fset 'range-intersection
            '(lambda (range1 range2)
               (let ((right
                      (nemacs-runtime-image-preload--range-numbers range2))
                     result)
                 (dolist (number
                          (nemacs-runtime-image-preload--range-numbers range1))
                   (when
                       (nemacs-runtime-image-preload--range-number-member-p
                        number right)
                     (push number result)))
                 (range-denormalize
                  (range-compress-list (nreverse result)))))))
    (unless (fboundp 'range-member-p)
      (fset 'range-member-p
            '(lambda (number ranges)
               (catch 'done
                 (dolist
                     (span
                      (nemacs-runtime-image-preload--range-normalize ranges))
                   (let ((start
                          (nemacs-runtime-image-preload--range-span-start
                           span))
                         (end
                          (nemacs-runtime-image-preload--range-span-end
                           span)))
                     (cond
                      ((and (<= start number) (<= number end))
                       (throw 'done t))
                      ((> start number)
                       (throw 'done nil)))))
                 nil))))
    (unless (fboundp 'range-list-intersection)
      (fset 'range-list-intersection
            '(lambda (list ranges)
               (let (result)
                 (dolist (number list)
                   (when (range-member-p number ranges)
                     (push number result)))
                 (nreverse result)))))
    (unless (fboundp 'range-list-difference)
      (fset 'range-list-difference
            '(lambda (list ranges)
               (let (result)
                 (dolist (number list)
                   (unless (range-member-p number ranges)
                     (push number result)))
                 (nreverse result)))))
    (unless (fboundp 'range-length)
      (fset 'range-length
            '(lambda (range)
               (length
                (nemacs-runtime-image-preload--range-numbers range)))))
    (unless (fboundp 'range-map)
      (fset 'range-map
            '(lambda (func range)
               (dolist
                   (number
                    (nemacs-runtime-image-preload--range-numbers range))
                 (funcall func number)))))
    (provide 'range))
  t)

(defun nemacs-runtime-image-preload--regi-line-string ()
  "Return the current line without the trailing newline."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun nemacs-runtime-image-preload--regi-predicate-match-p
    (pred negate-p case-fold-search-value)
  "Return non-nil when PRED matches the current line."
  (let* ((case-fold-search case-fold-search-value)
         (value (eval pred))
         (matched
          (cond
           ((stringp value) (looking-at value))
           (t value))))
    (if negate-p (not matched) matched)))

(defun nemacs-runtime-image-preload--regi-handle-result
    (result working-frame current-frame)
  "Return (DONE-P WORKING-FRAME CURRENT-FRAME STEP) for RESULT."
  (let ((done-p nil)
        (step 1))
    (when (consp result)
      (let ((frame-cell (assq 'frame result))
            (step-cell (assq 'step result)))
        (when frame-cell
          (setq working-frame (cdr frame-cell)))
        (when step-cell
          (setq step (cdr step-cell)))
        (when (memq 'continue result)
          (setq current-frame (cdr current-frame)))
        (when (memq 'abort result)
          (setq done-p t))))
    (unless (and (consp result) (memq 'continue result))
      (setq current-frame working-frame))
    (list done-p working-frame current-frame step)))

(defun nemacs-runtime-image-preload--regi-frame-specials (frame)
  "Return (BEGIN END EVERY WORKING-FRAME) for FRAME."
  (let (begin-tag end-tag every-tag working-frame)
    (dolist (entry frame)
      (let ((pred (car entry))
            (func (cadr entry)))
        (cond
         ((eq pred 'begin) (setq begin-tag func))
         ((eq pred 'end)   (setq end-tag func))
         ((eq pred 'every) (setq every-tag func))
         (t                (push entry working-frame)))))
    (list begin-tag end-tag every-tag (nreverse working-frame))))

(defun nemacs-runtime-image-preload--regi-pos (&optional position col-p)
  "Return point or column at a line-relative POSITION."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'boi)
      (beginning-of-line)
      (let ((end (line-end-position)))
        (while (and (< (point) end)
                    (let ((char
                           (aref (buffer-substring-no-properties
                                  (point) (1+ (point)))
                                 0)))
                      (or (= char ?\s) (= char ?\t))))
          (forward-char 1))))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (end-of-line)))
    (if col-p
        (- (point) (line-beginning-position))
      (point))))

(defun nemacs-runtime-image-preload--regi-mapcar
    (predlist func &optional negate-p case-fold-search-p)
  "Build a regi frame from PREDLIST and FUNC."
  (let (frame)
    (dolist (pred predlist (nreverse frame))
      (let ((entry (list pred func)))
        (when (or negate-p case-fold-search-p)
          (setq entry (append entry (list negate-p))))
        (when case-fold-search-p
          (setq entry (append entry (list case-fold-search-p))))
        (push entry frame)))))

(defun nemacs-runtime-image-preload--regi-interpret
    (frame &optional start end)
  "Interpret regi FRAME over the current buffer."
  (save-excursion
    (save-restriction
      (when (and start end)
        (let ((lo (min start end))
              (hi (max start end)))
          (narrow-to-region
           (save-excursion
             (goto-char lo)
             (line-beginning-position))
           (save-excursion
             (goto-char hi)
             (forward-line 1)
             (point)))))
      (goto-char (point-min))
      (let* ((specials
              (nemacs-runtime-image-preload--regi-frame-specials frame))
             (begin-tag (nth 0 specials))
             (end-tag (nth 1 specials))
             (every-tag (nth 2 specials))
             (working-frame (nth 3 specials))
             (current-frame working-frame)
             done-p)
        (when begin-tag
          (eval begin-tag))
        (while (and (not done-p) (not (eobp)))
          (cond
           ((null current-frame)
            (setq current-frame working-frame)
            (forward-line 1))
           (t
            (let* ((entry (car current-frame))
                   (pred (nth 0 entry))
                   (func (nth 1 entry))
                   (negate-p (nth 2 entry))
                   (case-fold-search-value (nth 3 entry)))
              (cond
               ((nemacs-runtime-image-preload--regi-predicate-match-p
                 pred negate-p case-fold-search-value)
                (let* ((curline
                        (nemacs-runtime-image-preload--regi-line-string))
                       (curframe current-frame)
                       (curentry entry)
                       (result (eval func))
                       (state
                        (nemacs-runtime-image-preload--regi-handle-result
                         result working-frame current-frame)))
                  (setq done-p (nth 0 state)
                        working-frame (nth 1 state)
                        current-frame (nth 2 state))
                  (unless (and (consp result) (memq 'continue result))
                    (forward-line (nth 3 state)))))
               (t
                (setq current-frame (cdr current-frame)))))))
          (when every-tag
            (eval every-tag)))
        (when end-tag
          (eval end-tag))))))

(defun nemacs-runtime-image-preload--install-regi-core ()
  "Install lightweight `regi' interpreter without source `load'."
  (unless (featurep 'regi)
    (unless (boundp 'curline) (defvar curline nil))
    (unless (boundp 'curframe) (defvar curframe nil))
    (unless (boundp 'curentry) (defvar curentry nil))
    (unless (fboundp 'regi-pos)
      (fset 'regi-pos #'nemacs-runtime-image-preload--regi-pos))
    (unless (fboundp 'regi-mapcar)
      (fset 'regi-mapcar #'nemacs-runtime-image-preload--regi-mapcar))
    (unless (fboundp 'regi-interpret)
      (fset 'regi-interpret #'nemacs-runtime-image-preload--regi-interpret))
    (provide 'regi))
  t)

(defun nemacs-runtime-image-preload--install-support-core ()
  "Install lightweight support facades without source load."
  (nemacs-runtime-image-preload--install-subr-x-core)
  (nemacs-runtime-image-preload--install-seq-core)
  (nemacs-runtime-image-preload--install-map-core)
  (nemacs-runtime-image-preload--install-lisp-core)
  (nemacs-runtime-image-preload--install-case-table-core)
  (nemacs-runtime-image-preload--install-cdl-core)
  (nemacs-runtime-image-preload--install-range-core)
  (nemacs-runtime-image-preload--install-regi-core)
  t)

(defun nemacs-runtime-image-preload--hex-digit-value (char)
  "Return hexadecimal value for CHAR."
  (cond
   ((and (<= ?0 char) (<= char ?9)) (- char ?0))
   ((and (<= ?a char) (<= char ?f)) (+ 10 (- char ?a)))
   ((and (<= ?A char) (<= char ?F)) (+ 10 (- char ?A)))
   (t (error "Invalid hexadecimal digit `%c'" char))))

(defun nemacs-runtime-image-preload--install-hex-util-core ()
  "Install the lightweight `hex-util' surface without source `load'."
  (unless (featurep 'hex-util)
    (unless (fboundp 'decode-hex-string)
      (fset 'decode-hex-string
            (lambda (string)
              (let* ((len (length string))
                     (dst (make-string (/ len 2) 0))
                     (idx 0)
                     (pos 0))
                (while (< pos len)
                  (aset dst idx
                        (+ (* (nemacs-runtime-image-preload--hex-digit-value
                               (aref string pos))
                              16)
                           (nemacs-runtime-image-preload--hex-digit-value
                            (aref string (1+ pos)))))
                  (setq idx (1+ idx)
                        pos (+ pos 2)))
                dst))))
    (unless (fboundp 'encode-hex-string)
      (fset 'encode-hex-string
            (lambda (string)
              (let* ((digits "0123456789abcdef")
                     (len (length string))
                     (dst (make-string (* len 2) 0))
                     (idx 0)
                     (pos 0))
                (while (< pos len)
                  (let ((char (aref string pos)))
                    (aset dst idx (aref digits (/ char 16)))
                    (setq idx (1+ idx))
                    (aset dst idx (aref digits (% char 16)))
                    (setq idx (1+ idx)
                          pos (1+ pos))))
                dst))))
    (provide 'hex-util))
  t)

(defun nemacs-runtime-image-preload--install-map-ynp-core ()
  "Install the lightweight `map-ynp' prompt surface without source `load'."
  (unless (featurep 'map-ynp)
    (unless (boundp 'read-answer-short)
      (defvar read-answer-short 'auto))
    (unless (boundp 'read-answer-map--memoize)
      (defvar read-answer-map--memoize nil))
    (unless (fboundp 'map-y-or-n-p)
      (fset 'map-y-or-n-p
            (lambda (prompter actor list &optional _help _action-alist
                              _no-cursor-in-echo-area)
              (let ((actions 0))
                (dolist (object list actions)
                  (let ((prompt (if (stringp prompter)
                                    (format prompter object)
                                  (funcall prompter object))))
                    (when (and prompt
                               (or (not (stringp prompt))
                                   (y-or-n-p prompt)))
                      (funcall actor object)
                      (setq actions (1+ actions)))))))))
    (unless (fboundp 'read-answer)
      (fset 'read-answer
            (lambda (question answers)
              (let ((input (read-from-minibuffer
                            (format "%s(%s) "
                                    question
                                    (mapconcat #'car answers ", ")))))
                (or (car (assoc input answers))
                    (car (car answers)))))))
    (provide 'map-ynp))
  t)

(defun nemacs-runtime-image-preload--install-charprop-core ()
  "Install the lightweight `charprop' property surface without source `load'."
  (unless (featurep 'charprop)
    (unless (boundp 'charprop--registry)
      (defvar charprop--registry nil))
    (unless (fboundp 'define-char-code-property)
      (fset 'define-char-code-property
            (lambda (property table &optional docstring)
              (let ((entry (assq property charprop--registry)))
                (if entry
                    (setcdr entry (list table docstring nil))
                  (push (list property table docstring nil)
                        charprop--registry)))
              property)))
    (unless (fboundp 'get-char-code-property)
      (fset 'get-char-code-property
            (lambda (char property)
              (let ((entry (assq property charprop--registry)))
                (when entry
                  (let ((table (cadr entry)))
                    (cond
                     ((hash-table-p table) (gethash char table))
                     ((and (vectorp table) (< char (length table)))
                      (aref table char))
                     ((consp table) (cdr (assq char table)))
                     (t nil))))))))
    (unless (fboundp 'put-char-code-property)
      (fset 'put-char-code-property
            (lambda (char property value)
              (let ((entry (assq property charprop--registry)))
                (unless entry
                  (define-char-code-property property nil nil)
                  (setq entry (assq property charprop--registry)))
                (let ((cell (assq char (nth 3 entry))))
                  (if cell
                      (setcdr cell value)
                    (setcar (nthcdr 3 entry)
                            (cons (cons char value) (nth 3 entry))))))
              value)))
    (unless (fboundp 'unicode-property-table-internal)
      (fset 'unicode-property-table-internal
            (lambda (property)
              (let ((entry (assq property charprop--registry)))
                (and entry (not (stringp (cadr entry))) (cadr entry))))))
    (unless (fboundp 'char-code-property-description)
      (fset 'char-code-property-description
            (lambda (_property value)
              (cond
               ((null value) nil)
               ((symbolp value) (symbol-name value))
               ((stringp value) value)
               (t (format "%S" value))))))
    (provide 'charprop))
  t)

(defun nemacs-runtime-image-preload--install-charscript-core ()
  "Install the lightweight `charscript' table surface without source `load'."
  (unless (featurep 'charscript)
    (unless (boundp 'char-script-table)
      (defvar char-script-table
        (if (fboundp 'make-char-table)
            (make-char-table 'char-script-table nil)
          [])))
    (provide 'charscript))
  t)

(defun nemacs-runtime-image-preload--install-emoji-labels-core ()
  "Install representative `emoji-labels' data without source `load'."
  (unless (featurep 'emoji-labels)
    (unless (boundp 'emoji--labels)
      (defvar emoji--labels '(("Smileys" ("smiling" "😀" "🙂")))))
    (unless (boundp 'emoji--names)
      (defvar emoji--names
        (let ((table (make-hash-table :test 'equal)))
          (puthash "😀" "grinning face" table)
          (puthash "🙂" "slightly smiling face" table)
          table)))
    (unless (boundp 'emoji--derived)
      (defvar emoji--derived (make-hash-table :test 'equal)))
    (provide 'emoji-labels))
  t)

(defun nemacs-runtime-image-preload--install-iso-transl-core ()
  "Install representative `iso-transl' keymaps without source `load'."
  (unless (featurep 'iso-transl)
    (unless (boundp 'key-translation-map)
      (defvar key-translation-map (make-sparse-keymap)))
    (unless (boundp 'iso-transl-ctl-x-8-map)
      (defvar iso-transl-ctl-x-8-map (make-sparse-keymap)))
    (unless (boundp 'iso-transl-char-map)
      (defvar iso-transl-char-map
        '(("E" . [#x20ac])
          ("Y" . [#x00a5])
          ("C" . [#x00a9])
          ("R" . [#x00ae])
          ("a" . [#x00e1])
          ("e" . [#x00e9]))))
    (unless (boundp 'iso-transl-language-alist)
      (defvar iso-transl-language-alist
        '(("French" ("C" . [#x00c7]) ("c" . [#x00e7]))
          ("German" ("A" . [#x00c4]) ("O" . [#x00d6])
           ("U" . [#x00dc]) ("s" . [#x00df])))))
    (unless (fboundp 'iso-transl-define-keys)
      (fset 'iso-transl-define-keys
            (lambda (alist)
              (dolist (entry alist)
                (define-key iso-transl-ctl-x-8-map (car entry) (cdr entry))))))
    (unless (fboundp 'iso-transl-set-language)
      (fset 'iso-transl-set-language
            (lambda (lang)
              (let ((entry (assoc lang iso-transl-language-alist)))
                (unless entry
                  (error "Unknown iso-transl language: %s" lang))
                (iso-transl-define-keys (cdr entry))))))
    (define-key key-translation-map "\C-x8" iso-transl-ctl-x-8-map)
    (iso-transl-define-keys iso-transl-char-map)
    (provide 'iso-transl))
  t)

(defun nemacs-runtime-image-preload--install-translation-table-core
    (feature decode encode)
  "Provide FEATURE and register DECODE/ENCODE translation-table symbols."
  (unless (featurep feature)
    (put decode 'translation-table (make-hash-table :test 'equal))
    (put encode 'translation-table (make-hash-table :test 'equal))
    (provide feature))
  t)

(defun nemacs-runtime-image-preload--install-fontset-core ()
  "Install the lightweight `fontset' helper surface without source `load'."
  (unless (featurep 'fontset)
    (unless (boundp 'font-encoding-alist)
      (defvar font-encoding-alist '(("ascii-0$" . ascii))))
    (unless (boundp 'script-representative-chars)
      (defvar script-representative-chars
        '((latin ?A ?Z ?a ?z) (emoji #x1f600))))
    (unless (boundp 'fontset-alias-alist)
      (defvar fontset-alias-alist nil))
    (unless (boundp 'standard-fontset-spec)
      (defvar standard-fontset-spec
        "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-standard"))
    (unless (fboundp 'x-decompose-font-name)
      (fset 'x-decompose-font-name
            (lambda (_pattern) (make-vector 12 nil))))
    (unless (fboundp 'x-compose-font-name)
      (fset 'x-compose-font-name
            (lambda (fields &optional _reduce)
              (concat "-" (mapconcat (lambda (field) (or field "*"))
                                     (append fields nil) "-")))))
    (unless (fboundp 'set-font-encoding)
      (fset 'set-font-encoding
            (lambda (pattern charset)
              (push (cons pattern charset) font-encoding-alist)
              charset)))
    (unless (fboundp 'fontset-name-p)
      (fset 'fontset-name-p
            (lambda (fontset)
              (and (stringp fontset)
                   (string-match-p "fontset-" fontset)))))
    (unless (fboundp 'fontset-plain-name)
      (fset 'fontset-plain-name (lambda (fontset) fontset)))
    (unless (fboundp 'generate-fontset-menu)
      (fset 'generate-fontset-menu (lambda () (list "Fontset"))))
    (unless (fboundp 'setup-default-fontset)
      (fset 'setup-default-fontset (lambda () standard-fontset-spec)))
    (unless (fboundp 'create-default-fontset)
      (fset 'create-default-fontset (lambda () standard-fontset-spec)))
    (provide 'fontset))
  t)

(defun nemacs-runtime-image-preload--install-idna-mapping-core ()
  "Install the lightweight `idna-mapping' vector without source `load'."
  (unless (featurep 'idna-mapping)
    (unless (boundp 'idna-mapping-table)
      (defvar idna-mapping-table
        (let ((table (make-vector #x110000 nil))
              (i ?A))
          (while (<= i ?Z)
            (aset table i (char-to-string (+ ?a (- i ?A))))
            (setq i (1+ i)))
          (aset table #x7f t)
          (aset table #x00ad 'ignored)
          (aset table #x212a "k")
          table)))
    (provide 'idna-mapping))
  t)

(defun nemacs-runtime-image-preload--install-ja-dic-utl-core ()
  "Install the lightweight `ja-dic-utl' helper surface without source `load'."
  (unless (featurep 'ja-dic-utl)
    (unless (boundp 'skkdic-okuri-ari) (defvar skkdic-okuri-ari nil))
    (unless (boundp 'skkdic-okuri-nasi) (defvar skkdic-okuri-nasi nil))
    (unless (boundp 'skkdic-prefix) (defvar skkdic-prefix nil))
    (unless (boundp 'skkdic-postfix) (defvar skkdic-postfix nil))
    (unless (boundp 'skkdic-okurigana-table)
      (defvar skkdic-okurigana-table '((#x3042 . ?a) (#x304b . ?k))))
    (unless (fboundp 'skkdic-merge-head-and-tail)
      (fset 'skkdic-merge-head-and-tail
            (lambda (heads tails _postfix)
              (let (out)
                (dolist (head heads)
                  (dolist (tail tails)
                    (push (concat head tail) out)))
                (nreverse out)))))
    (unless (fboundp 'skkdic-lookup-key)
      (fset 'skkdic-lookup-key
            (lambda (_seq _len &optional _postfix _prefer-noun) nil)))
    (provide 'ja-dic-utl))
  t)

(defun nemacs-runtime-image-preload--install-utility-i18n-core ()
  "Install lightweight utility/i18n vendor-core surfaces without source load."
  (nemacs-runtime-image-preload--install-hex-util-core)
  (nemacs-runtime-image-preload--install-map-ynp-core)
  (nemacs-runtime-image-preload--install-charprop-core)
  (nemacs-runtime-image-preload--install-charscript-core)
  (nemacs-runtime-image-preload--install-emoji-labels-core)
  (nemacs-runtime-image-preload--install-iso-transl-core)
  (nemacs-runtime-image-preload--install-translation-table-core
   'cp51932 'cp51932-decode 'cp51932-encode)
  (nemacs-runtime-image-preload--install-translation-table-core
   'eucjp-ms 'eucjp-ms-decode 'eucjp-ms-encode)
  (nemacs-runtime-image-preload--install-fontset-core)
  (nemacs-runtime-image-preload--install-idna-mapping-core)
  (nemacs-runtime-image-preload--install-ja-dic-utl-core)
  t)

;; Source-v1 runtime images replay top-level `setq' / `fset' reliably, while
;; helper `defun' forms are not a dependable runtime surface.  In standalone
;; bakes, force the frame/tab installers past earlier stub `fboundp' results so
;; the GUI-neutral API cells are recorded in the base image.  Host Emacs keeps
;; its native frame/tab functions because `nelisp--write-stdout-bytes' is not
;; present there.
(defun nemacs-runtime-image-preload--force-install-frame-tab-core ()
  "Force-install frame/tab API cells for standalone runtime-image bakes."
  (setq nemacs-runtime-image-preload--saved-fboundp (symbol-function 'fboundp))
  (fset 'fboundp '(lambda (_symbol) nil))
  (nemacs-runtime-image-preload--install-frame-core)
  (nemacs-runtime-image-preload--install-tab-core)
  (fset 'fboundp nemacs-runtime-image-preload--saved-fboundp)
  t)

(when (fboundp 'nelisp--write-stdout-bytes)
  (nemacs-runtime-image-preload--force-install-frame-tab-core))

;; Task #17 (M1) — Magit bridge extension.  Mirrors
;; `nemacs-runtime-image-preload-vendor-core-extension' above: a thin
;; dispatcher that hands off to the real logic owned by
;; `src/nelisp-emacs-magit-bridge.el' (per CLAUDE.md/AGENTS.md, session/image
;; wiring stays thin; the reusable "bring the real vendor Magit chain into a
;; NeLisp session" behavior belongs in a `src/' module, not here).
(defun nemacs-runtime-image-preload-magit-extension (repo-root)
  "Extend an already-baked base runtime image with the real vendor Magit chain.
REPO-ROOT is the repository root (matches the other preload entry points'
REPO-ROOT convention); the Magit bridge bundle and its own preconditions
are resolved relative to it."
  (load (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo-root)
        nil 'no-message t t)
  (setq nelisp-emacs-magit-bridge-repo-root repo-root)
  (nelisp-emacs-magit-bridge-load)
  t)

(provide 'nemacs-runtime-image-preload)

;;; nemacs-runtime-image-preload.el ends here
