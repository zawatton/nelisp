;;; packages.el --- Genuine GNU/Magit consumers -*- lexical-binding: t; -*-
(defvar nelisp-gui-packages-state-file nil)
(defvar nelisp-gui-packages-sequence 0)
(defvar nelisp-gui-packages-load-steps nil)
(defvar nelisp-gui-packages-trace-id 0)

(defun nelisp-gui-packages-load-step (name function)
  "Time a genuine source dependency or package, retaining failed-step timing."
  (let ((start (float-time)))
    (princ (format "GUI-PACKAGE-STEP-BEGIN|name=%s|\n" name))
    (unwind-protect
        (funcall function)
      (let ((elapsed (- (float-time) start)))
        (push `((name . ,name) (seconds . ,elapsed))
              nelisp-gui-packages-load-steps)
        (princ (format "GUI-PACKAGE-STEP-END|name=%s|seconds=%.6f|\n"
                       name elapsed))))))

(defun nelisp-gui-packages-observe ()
  "Write observations with buffer identity captured before temporary output."
  (let* ((name (buffer-name)) (mode (symbol-name major-mode))
         (pos (point)) (text (buffer-string))
         (file (or buffer-file-name ""))
         (visible (buffer-name (window-buffer (selected-window))))
         (messages (if (get-buffer "*Messages*")
                       (with-current-buffer "*Messages*" (buffer-string)) ""))
         (section (when (and (fboundp 'magit-current-section)
                             (derived-mode-p 'magit-mode))
                    (magit-current-section)))
         (hidden (and section (oref section hidden))))
    (setq nelisp-gui-packages-sequence (1+ nelisp-gui-packages-sequence))
    (with-temp-file nelisp-gui-packages-state-file
      (insert (json-encode
               `((sequence . ,nelisp-gui-packages-sequence)
                 (buffer . ,name) (window_buffer . ,visible) (mode . ,mode)
                 (point . ,pos) (file . ,file) (text . ,text)
                 (messages . ,messages) (section_hidden . ,(and hidden t))))))
    (princ (format "GUI-PACKAGE-STATE|sequence=%d|buffer=%S|mode=%S|point=%d|\n"
                   nelisp-gui-packages-sequence name mode pos))))

(defun nelisp-gui-packages-trace (name original &rest args)
  "Record package call progress without changing arguments or results."
  (let ((start (float-time))
        (id (setq nelisp-gui-packages-trace-id (1+ nelisp-gui-packages-trace-id))))
    (princ (format "GUI-PACKAGE-CALL-BEGIN|id=%d|name=%S|time=%.6f|%s\n"
                   id name start
                   (if (memq name '(process-file call-process))
                       (format "program=%S|argv=%S|" (car args) (nthcdr 4 args)) "")))
    (unwind-protect
        (condition-case failure
            (apply original args)
          (error
           (princ (format "K3-CALL-ERROR|name=%S|condition=%S|\n" name (car failure)))
           (signal (car failure) (cdr failure))))
      (princ (format "GUI-PACKAGE-CALL-END|id=%d|name=%S|seconds=%.6f|\n"
                     id name (- (float-time) start))))))

(defun nelisp-gui-packages-observe-minibuffer (&rest _ignored)
  "Observe the shared live reader without replacing package commands."
  ;; Full text is already captured in state JSON.  The frontend's optional
  ;; %S dump also traverses text-property values, including Magit's circular
  ;; section graph; the native circular-object printer cannot finish it.
  ;; Disable that duplicate diagnostic before input, keeping buffer properties
  ;; and the ordinary dispatcher/paint callback intact.
  (setq nelisp-gui-frontend--trace-text nil)
  (when (and (> emacs-minibuffer--depth 0) emacs-minibuffer--buffers)
   (let* ((buffer (car emacs-minibuffer--buffers))
         (text (nelisp-ec-with-current-buffer buffer (nelisp-ec-buffer-string))))
    (setq nelisp-gui-packages-sequence (1+ nelisp-gui-packages-sequence))
    (with-temp-file nelisp-gui-packages-state-file
      (insert (json-encode `((sequence . ,nelisp-gui-packages-sequence)
                             (minibuffer . t) (text . ,text)))))
    (princ (format "GUI-PACKAGE-MINIBUFFER|text=%S|\n" text)))))

(defun nelisp-gui-packages-fixture ()
  "Load genuine packages into the ordinary image, then await real keys."
  (let* ((root (getenv "NELISP_GUI_PACKAGES_ROOT"))
         (package (getenv "NELISP_GUI_PACKAGE"))
         (paths (with-temp-buffer
                  (insert-file-contents (concat root "/load-path.json"))
                  (let ((json-array-type 'list)) (json-read))))
         (start (float-time))
         (elapsed nil)
         (failure nil))
    (setq load-path (append paths load-path)
          nelisp-gui-packages-state-file (getenv "NELISP_GUI_PACKAGE_STATE")
          default-directory (concat root (if (equal package "magit") "/repo/" "/tree/")))
    (setq temporary-file-directory (concat root "/tmp/"))
    ;; Opt-in profiling transport executes genuine Git and preserves its
    ;; argv/stdio/status; normal fixtures keep their ordinary executable.
    (when (getenv "NELISP_GUI_PACKAGE_GIT_BIN")
      (setq exec-path (cons (getenv "NELISP_GUI_PACKAGE_GIT_BIN") exec-path)))
    (princ (format "GUI-PACKAGE-LOAD-BEGIN|package=%s|time=%.6f|\n" package start))
    (condition-case err
        (progn
          (nelisp-gui-packages-load-step
           "gnu-preloaded" (lambda () (load (concat root "/vendor/gnu-preloaded.el") nil t t)))
          ;; These are preloaded by GNU Emacs, but the daily-driver image has
          ;; a smaller file facade.  Load the genuine GNU dependency as well.
          (nelisp-gui-packages-load-step
           "gnu-files" (lambda () (load (concat root "/vendor/gnu/files.el") nil t t)))
          (nelisp-gui-packages-load-step "gnu-uniquify" (lambda () (require 'uniquify)))
          (nelisp-gui-packages-load-step "gnu-files-x" (lambda () (require 'files-x)))
          ;; GNU file/completion commands share this preloaded dependency.
          ;; The real M-x reader needs it for every package, including Org.
          (nelisp-gui-packages-load-step
           "gnu-minibuffer" (lambda () (load (concat root "/vendor/gnu/minibuffer.el") nil t t)))
          (nelisp-gui-packages-load-step
           "gnu-epa-hook" (lambda () (load (concat root "/vendor/gnu/epa-hook.el") nil t t)))
          (nelisp-gui-packages-load-step
           "gnu-map-ynp" (lambda () (load (concat root "/vendor/gnu/emacs-lisp/map-ynp.el") nil t t)))
          (cond
         ((equal package "dired")
          (nelisp-gui-packages-load-step
           "dired" (lambda () (load (concat root "/vendor/gnu/dired.el") nil t t))))
         ((equal package "magit")
          ;; GNU loadup preloads this parent mode before Magit defines its
          ;; repository-list keymap.  Keep the genuine mode, not an empty map.
          (nelisp-gui-packages-load-step
           "gnu-tabulated-list" (lambda () (require 'tabulated-list)))
          (nelisp-gui-packages-load-step
           "gnu-isearch" (lambda () (load (concat root "/vendor/gnu/isearch.el") nil t t)))
          ;; The native backquote expander drops GNU derived.el's map setup.
          ;; Use the shared mode provider's explicit-list expansion before
          ;; evaluating genuine package definitions.  Their commands, maps,
          ;; bodies, parent modes and hooks remain the package's own.
          (when (fboundp 'nelisp--write-stdout-bytes)
            (defalias 'define-derived-mode
              (symbol-function 'emacs-mode-define-derived-mode)))
          (nelisp-gui-packages-load-step "magit" (lambda ()
                     ;; Packages are being loaded by the startup fixture.  GNU
                     ;; 31 defers global-mode Custom initialization until the
                     ;; source file finishes; preserve that startup ordering.
                     (let ((after-init-time nil)) (require 'magit)))))
         ((equal package "org-agenda")
          ;; Org's genuine link/widget companions use GNU's preloaded TTY
          ;; color definitions even when no network request is made.
          (nelisp-gui-packages-load-step
           "gnu-tty-colors" (lambda () (load (concat root "/vendor/gnu/term/tty-colors.el") nil t t)))
          (nelisp-gui-packages-load-step "org-agenda" (lambda () (require 'org-agenda)))
          ;; The minimal bootstrap loop checks UNTIL before a preceding DO.
          ;; Org's real dispatcher must run its key reader before that test.
          ;; Load the genuine GNU macro provider, retaining the already loaded
          ;; package's structure definitions and every real package command.
          (nelisp-gui-packages-load-step
           "gnu-cl-macs" (lambda () (load (concat root "/vendor/gnu/emacs-lisp/cl-macs.el") nil t t))))
         (t (error "Unknown real package: %s" package))))
      (error (setq failure err)))
    (setq elapsed (- (float-time) start))
    (with-temp-file (concat nelisp-gui-packages-state-file ".load.json")
      (insert (json-encode
               `((package . ,package) (seconds . ,elapsed)
                 (steps . ,(vconcat (reverse nelisp-gui-packages-load-steps)))
                 (error . ,(format "%S" failure))))))
    (princ (format "GUI-PACKAGE-LOAD|package=%s|seconds=%.6f|error=%S|\n"
                   package elapsed failure))
    (when failure (signal (car failure) (cdr failure)))
    ;; GNU packages use the public native buffer API.  Explicitly finish the
    ;; legacy scratch handoff through the shared library's public boundary.
    (nelisp-ec-clear-current-buffer)
    (let ((buffer (get-buffer-create "*Package fixture*")))
      (set-buffer buffer)
      (set-window-buffer (selected-window) buffer)
      (setq default-directory (concat root (if (equal package "magit") "/repo/" "/tree/")))
      (insert "Real package fixture ready\n")
      (setq header-line-format nil mode-line-format '(" %b ")))
    (setq org-agenda-files (list (concat root "/agenda.org"))
          enable-local-variables nil enable-dir-local-variables nil
          org-agenda-span 'day org-agenda-start-day (getenv "NELISP_GUI_PACKAGE_DATE")
          org-agenda-window-setup 'current-window
          magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
          ;; Use with-editor's real shell transport.  Package Git commands
          ;; must not start a TCP Emacsclient server in this local fixture.
          with-editor-emacsclient-executable nil)
    (when (getenv "NELISP_GUI_PACKAGE_TRACE")
      (dolist (function (append
                         (and (equal package "magit")
                              (append magit-status-sections-hook magit-status-headers-hook
                                      '(magit-status magit-status-setup-buffer magit-setup-buffer-internal
                                        magit-refresh-buffer magit-status-refresh-buffer magit-mode
                                        magit-status-mode magit-git-insert magit-git-string
                                        magit-insert-section--create magit-insert-section--finish
                                        process-file emacs-process--standalone-run
                                        nelisp-gui-packages-observe json-encode
                                        nelisp-gui-frontend--dispatch nelisp-gui-frontend--paint
                                        nelisp-gui-pango-paint emacs-redisplay-redisplay-window
                                        emacs-redisplay--snapshot-fingerprint emacs-redisplay--snapshot-line-spans
                                        emacs-redisplay--viewport-text emacs-redisplay--source-entries
                                        emacs-redisplay--display-tokens emacs-redisplay--token-rows
                                        emacs-redisplay--redisplay-window-rebuild)))
                         '(dired dired-noselect dired-internal-noselect
                          dired-readin dired-mode dired-insert-directory
                          insert-directory call-process file-attributes
                          file-attribute-size delete-file insert-directory-clean
                          dired-insert-set-properties
                          dired-build-subdir-alist dired-get-buffer-create
                          dired-sort-other dired-readin-insert)))
        (when (fboundp function)
          (advice-add function :around
                      (apply-partially #'nelisp-gui-packages-trace function)))))
    (global-set-key (kbd "C-x d") 'dired)
    (global-set-key (kbd "M-x") 'execute-extended-command)
    (when (boundp 'nemacs-main--global-keymap)
      (nemacs-main--init-keymap)
      (emacs-keymap-define-key nemacs-main--global-keymap (kbd "C-x d") 'dired)
      (emacs-keymap-define-key nemacs-main--global-keymap (kbd "M-x") 'execute-extended-command))
    ;; The frontend installs its paint callback after this fixture returns.
    ;; Observe the live reader before it waits for each event, preserving that
    ;; callback and the real recursive command-loop dispatch.
    (advice-add 'emacs-command-loop-read-event :before
                #'nelisp-gui-packages-observe-minibuffer)
    ;; Record the completed real dispatcher before the shared reader waits.
    ;; The gate then sends `a'; it never calls agenda commands itself.
    (when (equal package "org-agenda")
      (advice-add 'read-char-exclusive :before
                  (lambda (&rest _args) (nelisp-gui-packages-observe))))
    (add-hook 'post-command-hook #'nelisp-gui-packages-observe)
    (nelisp-gui-packages-observe)
    (when (getenv "NELISP_GUI_PACKAGE_SNAPSHOT")
      (garbage-collect)
      (unless (> (nelisp--arena-dump-image-stream (getenv "NELISP_GUI_PACKAGE_SNAPSHOT")) 0)
        (error "Package profiling snapshot failed"))
      (princ "GUI-PACKAGE-SNAPSHOT-READY|\n"))
    (princ "GUI-PACKAGE-FIXTURE-READY|\n")))
(provide 'nelisp-gui-packages-fixture)
