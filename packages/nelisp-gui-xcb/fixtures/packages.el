;;; packages.el --- Genuine GNU/Magit consumers -*- lexical-binding: t; -*-
(defvar nelisp-gui-packages-state-file nil)
(defvar nelisp-gui-packages-sequence 0)
(defvar nelisp-gui-packages-load-steps nil)
(defvar nelisp-gui-packages-trace-id 0)
(defvar nelisp-gui-packages-image-ready nil)
(defvar nelisp-gui-packages-profile-functions nil)
(defvar nelisp-gui-packages-loading-image nil)
(defvar nelisp-gui-packages-common-ready nil)
(defun nelisp-gui-packages-capture-library-provider (name)
  "Copy a shared provider's definition, excluding runtime function objects."
  (let* ((function (symbol-function name))
         (macro (and (consp function) (eq (car function) 'macro)))
         (definition (if macro (cdr function) function))
         (closure (eq (car definition) 'closure)))
    (unless (memq (car definition) '(lambda closure))
      (error "Library provider has no source definition: %S" name))
    (when (and closure (not (member (cadr definition) '(nil (t)))))
      (error "Library provider captures lexical state: %S" name))
    (list name nil (get name 'compiler-macro)
          (cons (if macro 'defmacro 'defun)
                (cons name (copy-tree (if closure (cddr definition) (cdr definition))))))))
(defvar nelisp-gui-packages-library-providers
  (when (fboundp 'nelisp--write-stdout-bytes)
    (mapcar #'nelisp-gui-packages-capture-library-provider '(cl-typep cl-symbol-macrolet)))
  "Exact shared source forms and their current restored definitions.")
(defun nelisp-gui-packages-restore-library-providers ()
  "Re-evaluate the exact shared type and symbol-place definitions.
GNU cl-typep passes unsupported &cl-defs arguments to deftype expanders;
GNU cl-symbol-macrolet requires the host macroexpand-all environment.
Source forms avoid retaining function objects across GNU redefinitions."
  (dolist (entry nelisp-gui-packages-library-providers)
    (eval (nth 3 entry) t)
    (setcar (cdr entry) (symbol-function (car entry)))
    (put (car entry) 'compiler-macro (nth 2 entry))))

(defun nelisp-gui-packages-load-note (text)
  "Keep image source-load progress off the strict completion-marker stream."
  (if nelisp-gui-packages-loading-image
      (let ((file (getenv "NELISP_GUI_PACKAGE_LOAD_LOG")))
        (when file (write-region text nil file t 'silent)))
    (princ text)))

(defun nelisp-gui-packages-load-step (name function)
  "Time a genuine source dependency or package, retaining failed-step timing."
  (let ((start (float-time)))
    (nelisp-gui-packages-load-note
     (format "GUI-PACKAGE-STEP-BEGIN|name=%s|\n" name))
    (unwind-protect
        ;; Match GNU loadup: nested source evaluation is a live load.
        (let ((load-in-progress t)) (funcall function))
      (let ((elapsed (- (float-time) start)))
        (push `((name . ,name) (seconds . ,elapsed))
              nelisp-gui-packages-load-steps)
        (nelisp-gui-packages-load-note
         (format "GUI-PACKAGE-STEP-END|name=%s|seconds=%.6f|\n"
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
           (princ (format "GUI-PACKAGE-CALL-ERROR|name=%S|condition=%S|\n" name (car failure)))
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

(defun nelisp-gui-packages-configure (root)
  "Configure live paths without reloading dumped package definitions."
  (let ((paths (with-temp-buffer
                 (insert-file-contents (concat root "/load-path.json"))
                 (let ((json-array-type 'list)) (json-read)))))
    ;; Remove the image's private vendor paths before adding this session's.
    (setq load-path (append paths
                           (cl-remove-if
                            (lambda (path) (string-match-p "/gui-packages-image/fixture/vendor/" path))
                            load-path))))
  (setq temporary-file-directory (concat root "/tmp/"))
  ;; GNU org-persist gives each -Q session its own temporary directory at
  ;; load time.  A dumped Org still holds the image build's directory,
  ;; which that process removed on exit; repeat the per-session step.
  (when (and (featurep 'org-persist)
             (bound-and-true-p org-persist--disable-when-emacs-Q)
             (not user-init-file))
    (setq org-persist-directory (make-temp-file "org-persist-" 'dir))))

(defun nelisp-gui-packages-fingerprint-value (symbol)
  "Return SYMBOL's default value with per-session temporary paths named.
Two processes never share org-persist's -Q directory, so compare its role."
  (let ((value (default-value symbol)))
    (if (and (eq symbol 'org-persist-directory)
             (stringp value)
             (string-prefix-p (expand-file-name "org-persist-" temporary-file-directory)
                              (expand-file-name value)))
        'per-session-temporary-directory
      value)))

(defun nelisp-gui-packages-load (root package)
  "Load genuine sources headlessly, keeping package commands unchanged."
  (nelisp-gui-packages-configure root)
  (unless (and nelisp-gui-packages-loading-image nelisp-gui-packages-common-ready)
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
    (setq nelisp-gui-packages-common-ready t))
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
           "gnu-cl-macs" (lambda () (load (concat root "/vendor/gnu/emacs-lisp/cl-macs.el") nil t t)))
          ;; GNU cl-macs.el just replaced the shared cl-typep provider.  Org's
          ;; modules define EIEIO classes next (ol-eww -> eww -> vtable), and
          ;; GNU cl-typep calls deftype expanders with unsupported arguments:
          ;; (wrong-number-of-arguments lambda 0) in eieio-defclass-internal.
          (when nelisp-gui-packages-loading-image
            (nelisp-gui-packages-restore-library-providers))
          ;; Org loads its default modules lazily on the first real org-mode.
          ;; They belong in the package heap too; do not run a mode or open
          ;; the session's agenda file while building the headless image.
          (when nelisp-gui-packages-loading-image
            (nelisp-gui-packages-load-step "org-modules" #'org-load-modules-maybe)
            ;; First-file initialization and sexp agenda scanning otherwise
            ;; require these genuine GNU dependencies in the GUI.  Keep
            ;; their source interpretation in the headless package preload.
            (dolist (feature '(font-lock jit-lock diary-lib))
              (nelisp-gui-packages-load-step
               (concat "org-dependency-" (symbol-name feature))
               (apply-partially #'require feature)))
            ;; The default citation activation processor is otherwise first
            ;; required by org-set-font-lock-defaults in a live org-mode.
            (when org-cite-activate-processor
              (nelisp-gui-packages-load-step
               "org-cite-processor"
               (lambda () (org-cite-try-load-processor org-cite-activate-processor))))))
         (t (error "Unknown real package: %s" package)))
  (unless nelisp-gui-packages-loading-image
    (nelisp-gui-packages-restore-library-providers))
  t)

(defun nelisp-gui-packages-preload ()
  "Preload all S5.2 packages without modes, fixture buffers or transport."
  (let ((nelisp-gui-packages-loading-image t)
        (root (getenv "NELISP_GUI_PACKAGES_ROOT")))
    (dolist (package '("dired" "magit" "org-agenda"))
      (nelisp-gui-packages-load root package)))
  ;; Finish GNU source loading before replacing compatibility providers.
  (nelisp-gui-packages-restore-library-providers)
  (setq nelisp-gui-packages-load-steps nil
        nelisp-gui-packages-image-ready t)
  (nelisp-gui-skk-evil-assert-headless))

(defun nelisp-gui-packages-assert-image ()
  "Require the dumped packages without repairing missing image state."
  (unless (and nelisp-gui-packages-image-ready
               (featurep 'dired) (featurep 'magit) (featurep 'org-agenda))
    (error "S5.2 packages missing from image"))
  (nelisp-gui-skk-evil-assert-headless))

(defun nelisp-gui-packages-fixture ()
  "Configure dumped genuine packages, then await real keys."
  (let* ((root (getenv "NELISP_GUI_PACKAGES_ROOT"))
         (package (getenv "NELISP_GUI_PACKAGE"))
         (start (float-time))
         (elapsed nil)
         (failure nil))
    (setq nelisp-gui-packages-state-file (getenv "NELISP_GUI_PACKAGE_STATE")
          default-directory (concat root (if (equal package "magit") "/repo/" "/tree/")))
    (nelisp-gui-packages-configure root)
    ;; Opt-in profiling transport executes genuine Git and preserves its
    ;; argv/stdio/status; normal fixtures keep their ordinary executable.
    (when (getenv "NELISP_GUI_PACKAGE_GIT_BIN")
      (setq exec-path (cons (getenv "NELISP_GUI_PACKAGE_GIT_BIN") exec-path)))
    (princ (format "GUI-PACKAGE-LOAD-BEGIN|package=%s|time=%.6f|\n" package start))
    (condition-case err
        (progn
          (if nelisp-gui-packages-image-ready
              (nelisp-gui-packages-assert-image)
            (nelisp-gui-packages-load root package)))
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
      (dolist (function (delete-dups (append
                         (and (equal package "magit")
                              (append nelisp-gui-packages-profile-functions
                                      magit-status-sections-hook magit-status-headers-hook
                                      '(magit-status magit-status-setup-buffer magit-setup-buffer-internal
                                        magit-refresh-buffer magit-status-refresh-buffer magit-mode
                                        magit-status-mode magit-git-insert magit-git-string
                                        magit-insert-section--create magit-insert-section--finish
                                        magit-process-file magit-git-wash magit-start-process
                                        magit-process-sentinel magit-refresh
                                        accept-process-output emacs-process-dispatch-pending
                                        emacs-process--native-maybe-fire-sentinel
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
                          dired-sort-other dired-readin-insert))))
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
(defvar nelisp-gui-packages-fingerprint-symbols nil)

(defun nelisp-gui-packages-fingerprint (file)
  "Write deterministic features, keymaps, hooks and package Custom values.
Render complete values with circular references enabled.  Include
all named keymaps/hooks, including shared maps modified by package loading."
  (let ((symbols nil) (state nil) (print-circle t) (print-length nil) (print-level nil)
        (trace (getenv "NELISP_GUI_FINGERPRINT_TRACE")))
    ;; The fixed reader cannot enumerate its global obarray.  The probe
    ;; inventory comes from GNU package loading plus the exact bundle sources.
    (unless nelisp-gui-packages-fingerprint-symbols (error "Missing fingerprint inventory"))
    (dolist (symbol nelisp-gui-packages-fingerprint-symbols)
       (when (boundp symbol)
         (let ((name (symbol-name symbol)))
           (when (or (keymapp (symbol-value symbol))
                     (string-suffix-p "-hook" name)
                     (string-suffix-p "-functions" name)
                     (get symbol 'custom-type))
             (push symbol symbols)))))
    (dolist (symbol '(minor-mode-map-alist minor-mode-overriding-map-alist
                      emulation-mode-map-alists overriding-local-map
                      overriding-terminal-local-map input-method-alist load-path))
      (when (and (boundp symbol) (not (memq symbol symbols))) (push symbol symbols)))
    (when trace (princ (format "GUI-FINGERPRINT|scanned=%d|\n" (length symbols))))
    (setq symbols (sort symbols (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (when trace (princ "GUI-FINGERPRINT|sorted|\n"))
    (dolist (symbol symbols)
      (push (list symbol (nelisp-gui-packages-fingerprint-value symbol)) state))
    (setq state (cons (cons 'features (sort (copy-sequence features)
                                          (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
                      (nreverse state)))
    ;; The fixed reader's strings already hold UTF-8 bytes.  Write the
    ;; complete graph directly rather than editing a temporary buffer.
    (let ((text (concat (prin1-to-string state) "\n")))
      (if (fboundp 'nl-write-file)
          (nl-write-file file (string-as-unibyte text))
        (write-region text nil file nil 'silent)))
    (princ (format "GUI-PACKAGE-FINGERPRINT|variables=%d|\n" (length symbols)))))

(provide 'nelisp-gui-packages-fixture)
