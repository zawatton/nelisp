;;; nemacs-loadup.el --- nemacs bootstrap entry point  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track J (2026-05-03) — Layer 2.
;;
;; The bootstrap glue that turns NeLisp + the Layer 2 Emacs C-core
;; ports into a runnable `nemacs' (= NeLisp-cored Emacs).
;; Mirrors Emacs's classic `loadup.el' role: pulls the dependency
;; graph in order, sets up the initial buffer, fires the startup
;; hook, and signals readiness.
;;
;; Use:
;;
;;   nemacs --batch -l nemacs-loadup -f nemacs-init [...]
;;
;; or from elisp:
;;
;;   (require 'nemacs-loadup)
;;   (nemacs-init)
;;
;; The bootstrap is idempotent — `nemacs-init' guards on
;; `nemacs-initialized' and signals if called twice; tests reset
;; via `nemacs-uninit'.
;;
;; Out of scope for this MVP:
;;   - command-line option parsing (= --eval / --load / -l etc.)
;;   - terminal init (= the TUI backend wakes up via
;;     `emacs-tui-event-init', driven separately)
;;
;; Track L (2026-05-03): dump file save/load is wired here as
;; `nemacs-save-dump' / `nemacs-load-dump' helpers; the underlying
;; engine lives in `emacs-dump.el'.

;;; Code:

(require 'emacs-init)
(require 'emacs-dump)
(require 'nemacs-init-transport)

;;;; --- version + hook surface ----------------------------------------

(defconst nemacs-version "0.1.0-mvp"
  "Current nemacs version.  Format: SEMVER + suffix tag.")

(defvar nemacs-startup-hook nil
  "Hook run by `nemacs-init' after the bootstrap completes.")

(defvar nemacs-package-activation-hook nil
  "Hook run at the package activation slot between early-init and init.
This hook runs after the built-in package.el activation attempt, so tests or
frontends can observe the same startup slot without owning package semantics.")

(defvar nemacs-initialized nil
  "Non-nil once `nemacs-init' has run.  Reset by `nemacs-uninit'.")

(defvar nemacs--initial-buffer nil
  "The `*scratch*'-equivalent buffer created during bootstrap.")

(defvar nemacs-user-emacs-directory nil
  "Directory used by nemacs for early-init.el and init.el discovery.
When nil, `nemacs-init' resolves it from `NEMACS_USER_EMACS_DIRECTORY',
then from real Emacs ~/.emacs.d versus XDG precedence.")

(defvar nemacs-init-file-loaded nil
  "Non-nil after `nemacs-init' has completed user init discovery.")

(defvar nemacs-init-file-error nil
  "Cons cell (FILE . ERROR-STRING) for the last non-fatal init load error.")

(unless (boundp 'early-init-file)
  (defvar early-init-file nil
    "Compatibility variable naming the loaded early-init.el file."))

(unless (boundp 'user-init-file)
  (defvar user-init-file nil
    "Compatibility variable naming the loaded user init file."))

(unless (boundp 'init-file-had-error)
  (defvar init-file-had-error nil
    "Non-nil when user init loading caught a non-fatal error."))

(unless (boundp 'init-file-user)
  (defvar init-file-user ""
    "Compatibility init gate.  Nil means do not load user init files."))

(unless (boundp 'after-init-hook)
  (defvar after-init-hook nil
    "Hook run after user init loading."))

(unless (boundp 'before-init-hook)
  (defvar before-init-hook nil
    "Hook run before user init loading."))

(unless (boundp 'initial-scratch-message)
  (defvar initial-scratch-message
    ";; This buffer is for text that is not saved, and for Lisp evaluation.\n;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n"
    "Initial contents inserted into a newly-created *scratch* buffer."))

(unless (boundp 'inhibit-startup-screen)
  (defvar inhibit-startup-screen nil
    "Compatibility gate for the startup splash screen.
Non-nil suppresses it (Emacs's `-Q' implies this).  `nemacs-loadup' only
declares the variable so the CLI/options-plist wiring in `bin/nemacs' and
`nemacs-main' has a stable place to set it; the splash-screen owner that
consults this variable is `emacs-startup-screen'
(`emacs-startup-screen-use-p')."))

(unless (boundp 'initial-major-mode)
  (defvar initial-major-mode 'lisp-interaction-mode
    "Major mode used for the initial *scratch* buffer."))

(unless (boundp 'default-frame-alist)
  (defvar default-frame-alist nil
    "Default frame parameter alist."))

(unless (boundp 'initial-frame-alist)
  (defvar initial-frame-alist nil
    "Initial frame parameter alist."))

(when (or (fboundp 'nl-write-file)
          (fboundp 'nelisp--write-stdout-bytes)
          (not (boundp 'emacs-version)))
  (defmacro push (item place)
    "Standalone compatibility macro: cons ITEM onto PLACE.
PLACE may be a symbol (`setq') or a generalized place the substrate's
`setf' polyfill handles (real Emacs `push' accepts any gv place; a
symbol-only expansion emits `(setq (cdr ...) ...)', which the
standalone evaluator aborts on flagless)."
    (if (symbolp place)
        (list 'setq place (list 'cons item place))
      (list 'setf place (list 'cons item place)))))

(define-error 'nemacs-error "nemacs bootstrap error")
(define-error 'nemacs-already-initialized
  "nemacs already initialized" 'nemacs-error)

;;;; --- bootstrap -----------------------------------------------------

(defun nemacs--ensure-scratch-buffer ()
  "Return the bootstrap's initial buffer, creating it if absent."
  (let ((buf (or nemacs--initial-buffer
                 (and (fboundp 'nelisp-ec-generate-new-buffer)
                      (setq nemacs--initial-buffer
                            (or (and (boundp 'nelisp-ec--buffers)
                                     (cdr (assoc "*scratch*" nelisp-ec--buffers)))
                                (nelisp-ec-generate-new-buffer "*scratch*")))))))
    (when buf
      (when (and (fboundp 'nelisp-ec-set-buffer)
                 (or (not (fboundp 'nelisp-ec-current-buffer))
                     (not (eq (nelisp-ec-current-buffer) buf))))
        (nelisp-ec-set-buffer buf))
      (when (boundp 'buffer-read-only)
        (setq buffer-read-only nil))
      (when (and (fboundp 'nelisp-ec-buffer-string)
                 (fboundp 'nelisp-ec-insert)
                 (equal (nelisp-ec-buffer-string) "")
                 (stringp initial-scratch-message)
                 (> (length initial-scratch-message) 0))
        (nelisp-ec-insert initial-scratch-message)))
    buf))

(defun nemacs--home-directory ()
  "Return the startup home directory used for init discovery."
  (or (and (fboundp 'getenv) (getenv "HOME"))
      "~"))

(defun nemacs--directory-with-slash (dir)
  "Return DIR with a trailing slash when DIR is a non-empty string."
  (when (and (stringp dir) (> (length dir) 0))
    (if (string= (substring dir (1- (length dir))) "/")
        dir
      (concat dir "/"))))

(defun nemacs--xdg-config-emacs-directory (home)
  "Return the XDG Emacs config directory for HOME."
  (nemacs--directory-with-slash
   (concat (or (and (fboundp 'getenv) (getenv "XDG_CONFIG_HOME"))
               (concat home "/.config"))
           "/emacs")))

(defun nemacs-resolve-user-emacs-directory ()
  "Resolve the nemacs user init directory.
`NEMACS_USER_EMACS_DIRECTORY' is a test/frontend-safe override.  Without
it, and without a cached `nemacs-user-emacs-directory', prefer a
~/.nemacs.d directory when one exists on disk: that is the historical M15
GUI-bridge wrapped-init lane's directory (scripts/nemacs-wrap-init.el),
previously hardcoded independently by the GUI launcher instead of going
through this resolver.  Loader reconcile Phase 3 makes this resolver the
single source every driver/launcher defers to instead: the GUI launcher,
`bin/nemacs' (nelisp driver), and `bin/nemacs-server' now all either read
~/.nemacs.d through this same precedence or explicitly set
`NEMACS_USER_EMACS_DIRECTORY' when they need a different directory,
instead of hardcoding their own directory constant.  Without a
~/.nemacs.d directory, fall back to real Emacs precedence: ~/.emacs.d
when it exists, otherwise XDG when it exists, otherwise default to
~/.emacs.d."
  (or (nemacs--directory-with-slash
       (and (fboundp 'getenv) (getenv "NEMACS_USER_EMACS_DIRECTORY")))
      nemacs-user-emacs-directory
      (let* ((home (nemacs--home-directory))
             (legacy-nemacs-dir (nemacs--directory-with-slash
                                 (concat home "/.nemacs.d")))
             (dot-emacs-dir (nemacs--directory-with-slash
                             (concat home "/.emacs.d")))
             (legacy-dotfile (concat home "/.emacs"))
             (xdg-dir (nemacs--xdg-config-emacs-directory home)))
        (cond
         ((and (fboundp 'file-directory-p)
               (file-directory-p legacy-nemacs-dir))
          legacy-nemacs-dir)
         ((and (fboundp 'file-directory-p)
               (file-directory-p dot-emacs-dir))
          dot-emacs-dir)
         ((and (fboundp 'file-exists-p)
               (file-exists-p legacy-dotfile))
          dot-emacs-dir)
         ((and (fboundp 'file-directory-p)
               (file-directory-p xdg-dir))
          xdg-dir)
         (t dot-emacs-dir)))))

(defun nemacs--init-file-readable-p (path)
  "Return non-nil when PATH names a loadable init file."
  (and (stringp path)
       (fboundp 'file-exists-p)
       (file-exists-p path)))

(defun nemacs--load-init-file (path kind)
  "Load init PATH for KIND, catching errors like Emacs startup."
  (condition-case err
      (progn
        (load path nil t)
        (cond
         ((eq kind 'early-init) (setq early-init-file path))
         ((eq kind 'init) (setq user-init-file path)))
        (when (fboundp 'message)
          (message "Loaded %s" path))
        t)
    (error
     (setq init-file-had-error t
           nemacs-init-file-error (cons path (error-message-string err)))
     (when (fboundp 'message)
       (message "init error: %s - %s" path (error-message-string err)))
     nil)))

(defun nemacs--package-startup-enabled-p ()
  "Return non-nil when startup package activation should run."
  (if (boundp 'package-enable-at-startup)
      package-enable-at-startup
    t))

(defun nemacs--package-already-activated-p ()
  "Return non-nil when package.el has already run startup activation."
  (and (boundp 'package--activated) package--activated))

(defun nemacs-activate-packages-at-startup ()
  "Run package.el activation at the Emacs startup package slot.
Activation is skipped when `package-enable-at-startup' is nil, or when
package.el has already activated packages.  Errors are logged through
`message' and do not prevent init.el from loading."
  (when (and (nemacs--package-startup-enabled-p)
             (not (nemacs--package-already-activated-p)))
    (condition-case err
        (progn
          (require 'package)
          (when (nemacs--package-startup-enabled-p)
            (package-activate-all)))
      (error
       (when (fboundp 'message)
         (message "package activation error: %s" (error-message-string err)))
       nil))))

(defun nemacs-candidate-init-files ()
  "Return candidate user init files in load order.
When `NEMACS_USER_EMACS_DIRECTORY' is set, only that fixture directory's
init.el is considered, so tests never fall through to the user's real home."
  (let* ((override (and (fboundp 'getenv)
                        (getenv "NEMACS_USER_EMACS_DIRECTORY")))
         (dir (nemacs-resolve-user-emacs-directory)))
    (if (and override (> (length override) 0))
        (list (concat dir "init.el"))
      (let ((home (nemacs--home-directory)))
        (list (concat dir "init.el")
              (concat home "/.emacs.el")
              (concat home "/.emacs"))))))

(defun nemacs--macro-less-runtime-p ()
  "Return non-nil when the running driver may need pre-lowered init forms.
Same standalone-vs-host guard used across `src/' (e.g. `emacs-fns.el'
require polyfill, and this file's own `push' compat macro above): a
real host Emacs macro-expands every user construct normally, so a raw
`load' of early-init.el/init.el is always sufficient there.  The
standalone NeLisp reader may not reliably macro-expand every construct
a real third-party package uses (Doc reconcile plan §1), which is what
`nemacs-wrap-init' host-side pre-lowering exists for (loader reconcile
Phase 2)."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))
      (not (stringp emacs-version))))

(defun nemacs--init-wrapped-transport-path ()
  "Return the wrapped-init transport path under `nemacs-user-emacs-directory'.
`nemacs-wrap-init' (scripts/nemacs-wrap-init.el, run ahead of time under
a full host Emacs) writes its OUT file here, plus the `-packages' /
`-pkgs-lowered' companions `nemacs-init-transport-consume' also reads.
`bin/nemacs' (nelisp driver) and `gui/bin/nemacs' now run the generator
themselves ahead of dispatch (loader reconcile Phase 3); a standalone
session with no such transport (host Emacs unavailable, or nothing to
wrap) keeps consuming whatever is already present on disk, same as
Phase 2."
  (concat nemacs-user-emacs-directory "nemacs-init-wrapped"))

(defun nemacs--load-user-init-via-transport ()
  "Consume a pre-lowered wrapped-init transport when one is present.
Returns non-nil when the transport was found and applied (whether
freshly loaded this call or already applied for the same mtime by an
earlier call in this process), so the caller should skip the raw
`load' fallback; returns nil when there is no transport to consume
\(no wrapper file, or the runtime lacks the primitives
`nemacs-init-transport-consume' needs), so the caller should fall back
to the Phase 1 raw-load lane unchanged."
  (and (nemacs--macro-less-runtime-p)
       (fboundp 'nemacs-init-transport-consume)
       (let ((wrapper (nemacs--init-wrapped-transport-path)))
         (nemacs-init-transport-consume wrapper (concat wrapper "-report")))))

(defun nemacs-load-user-init-files ()
  "Load early-init.el, run the package slot, then load init.el.
The loader is session-owned; frontend wrappers should delegate here instead
of reimplementing init discovery.

Under the standalone macro-less runtime, a pre-lowered wrapped-init
transport (written by `nemacs-wrap-init', scripts/nemacs-wrap-init.el)
takes priority over the raw early-init.el/init.el `load' when one is
present at `nemacs--init-wrapped-transport-path' -- this is the loader
reconcile Phase 2 lane (`nemacs-init-transport-consume',
src/nemacs-init-transport.el).  A real host Emacs, and a standalone
session with no such transport, keep the Phase 1 raw-load path
unchanged."
  (setq nemacs-user-emacs-directory (nemacs-resolve-user-emacs-directory))
  (when (boundp 'user-emacs-directory)
    (setq user-emacs-directory nemacs-user-emacs-directory))
  (unless (null init-file-user)
    (let* ((early (concat nemacs-user-emacs-directory "early-init.el"))
           (early-readable (nemacs--init-file-readable-p early))
           found-init)
      (catch 'done
        (dolist (path (nemacs-candidate-init-files))
          (when (nemacs--init-file-readable-p path)
            (setq found-init path)
            (throw 'done t))))
      (if (nemacs--load-user-init-via-transport)
          (progn
            (when early-readable
              (setq early-init-file early))
            (when found-init
              (setq user-init-file found-init))
            (nemacs-activate-packages-at-startup)
            (when (fboundp 'run-hooks)
              (run-hooks 'nemacs-package-activation-hook)))
        (progn
          (when early-readable
            (nemacs--load-init-file early 'early-init))
          (nemacs-activate-packages-at-startup)
          (when (fboundp 'run-hooks)
            (run-hooks 'nemacs-package-activation-hook))
          (when found-init
            (nemacs--load-init-file found-init 'init))))))
  (setq nemacs-init-file-loaded t))

(defun nemacs--apply-initial-major-mode ()
  "Apply `initial-major-mode' to *scratch*, falling back honestly."
  (let ((mode (and (symbolp initial-major-mode) initial-major-mode)))
    (cond
     ((and mode (fboundp mode))
      (condition-case nil
          (funcall mode)
        (error
         (when (fboundp 'emacs-mode-fundamental-mode)
           (emacs-mode-fundamental-mode)))))
     ((and (eq mode 'lisp-interaction-mode)
           (fboundp 'emacs-mode-fundamental-mode))
      ;; `lisp-interaction-mode' is not implemented in the local substrate yet.
      (emacs-mode-fundamental-mode))
     ((fboundp 'emacs-mode-fundamental-mode)
      (emacs-mode-fundamental-mode)))))

(defun nemacs--maybe-startup-screen ()
  "Create and select the startup splash screen when its gate allows it.
The splash owner is `emacs-startup-screen' (an editor-semantics module
loaded by app entry points such as `nemacs-main'); when it is absent
this step is a no-op.  `emacs-startup-screen-use-p' suppresses the
splash for `inhibit-startup-screen' non-nil, a loaded user init file,
or CLI file arguments.  Returns the splash buffer when selected."
  (when (and (fboundp 'emacs-startup-screen-use-p)
             (fboundp 'emacs-startup-screen-select)
             (emacs-startup-screen-use-p))
    (emacs-startup-screen-select)))

(defun nemacs--report-banner (batch-p)
  "Emit the readiness banner.  No-op under BATCH-P."
  (unless batch-p
    (when (fboundp 'message)
      (message "nemacs %s ready (Layer 2 / Doc 51)" nemacs-version)))
  nil)

(defun nemacs-init (&optional batch-p)
  "Run the nemacs bootstrap sequence.

When BATCH-P is non-nil, suppresses interactive output.  Idempotent
guard: calling `nemacs-init' twice signals
`nemacs-already-initialized' rather than re-running.

Steps:
  1. Load early-init.el when present.
  2. Run the package activation hook point.
  3. Load init.el when present.
  4. Run `after-init-hook', create *scratch*.
  5. Interactive only: show the startup splash screen when the
     `emacs-startup-screen' gate allows it, then run
     `nemacs-startup-hook'.
  6. Mark `nemacs-initialized' = t.

Returns the symbol `ready' on success."
  (when nemacs-initialized
    (signal 'nemacs-already-initialized nil))
  ;; Step 0 — initialise the standalone-mode dispatch scaffold so
  ;; `emacs-standalone-active-p' returns a stable value for the rest
  ;; of bootstrap.
  (when (fboundp 'emacs-standalone-init)
    (emacs-standalone-init))
  (when (fboundp 'run-hooks)
    (run-hooks 'before-init-hook))
  (nemacs-load-user-init-files)
  (when (fboundp 'run-hooks)
    (run-hooks 'after-init-hook))
  (let ((buf (nemacs--ensure-scratch-buffer)))
    (when (and buf (fboundp 'nelisp-ec-set-buffer))
      (nelisp-ec-set-buffer buf)))
  (nemacs--apply-initial-major-mode)
  ;; Batch bootstraps never show the splash (Emacs `noninteractive'
  ;; semantics); this also keeps every existing `(nemacs-init t)' caller
  ;; on today's *scratch* initial buffer.
  (unless batch-p
    (nemacs--maybe-startup-screen))
  (when (fboundp 'run-hooks)
    (run-hooks 'nemacs-startup-hook))
  (setq nemacs-initialized t)
  (nemacs--report-banner batch-p)
  'ready)

(defun nemacs-uninit ()
  "Reset the bootstrap so `nemacs-init' can run again.
Test-only helper.  Returns nil."
  (setq nemacs-initialized nil
        nemacs--initial-buffer nil
        nemacs-init-file-loaded nil
        nemacs-init-file-error nil
        init-file-had-error nil
        early-init-file nil
        user-init-file nil)
  (when (fboundp 'emacs-standalone-uninit)
    (emacs-standalone-uninit))
  nil)

;;;; --- introspection -------------------------------------------------

(defun nemacs-status ()
  "Return a plist describing the bootstrap state.

Keys:
  :version          — `nemacs-version'
  :initialized      — `nemacs-initialized'
  :initial-buffer   — `nemacs--initial-buffer' (= the scratch buffer)
  :major-mode       — current substrate major-mode (= via Track H)
  :feature-count    — number of features `featurep'-true.

Useful for smoke-testing the boot order from elisp."
  (list :version        nemacs-version
        :initialized    nemacs-initialized
        :initial-buffer nemacs--initial-buffer
        :major-mode     (and (fboundp 'emacs-mode-major-mode)
                             (emacs-mode-major-mode))
        :feature-count  (and (boundp 'features) (length features))))

;;;; --- dump helpers (Track L wiring) ---------------------------------

(defun nemacs-save-dump (path)
  "Write a lisp-image dump of the running session to PATH.
Returns the image plist that was written."
  (emacs-dump-save path))

(defun nemacs-load-dump (path &optional restore-buffers)
  "Load a lisp-image dump from PATH and re-establish bindings.
When RESTORE-BUFFERS is non-nil, also recreates the persisted
buffers' contents.  Returns the loaded image plist."
  (emacs-dump-load path restore-buffers))

(defun nemacs-dump-info (path)
  "Return a summary plist of the dump at PATH (without applying it)."
  (emacs-dump-image-info path))

(provide 'nemacs-loadup)

;;; nemacs-loadup.el ends here
