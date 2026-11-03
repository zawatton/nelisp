;;; nemacs-library-package-smoke.el --- batch package loader smoke -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT loads host `cl-lib' internals before tests run.  Package-path smoke must
;; let the generated package scaffold provide `cl-lib', so this batch proof
;; avoids ERT and checks package group loaders directly.

;;; Code:

(defconst nemacs-library-package-smoke--forbidden-features
  '(emacs-init
    image-baker
    nelisp-emacs
    nemacs-main
    nemacs-gtk-frontend
    nemacs-editor-transport
    nemacs-gtk-view-menu
    nemacs-gui-file-bridge-runtime
    emacs-tui-backend
    emacs-tui-event
    emacs-project
    emacs-dump
    image-loader
    nemacs-loaddefs
    emacs-elisp-eval
    emacs-ielm
    emacs-redisplay-core
    files-standalone-buffer
    emacs-redisplay
    emacs-font-lock-builtins
    emacs-elisp-mode)
  "Features that individual package group loaders must not pull in.")

(defconst nemacs-library-package-smoke--forbidden-files
  '("emacs-init.el"
    "image-baker.el"
    "nelisp-emacs.el"
    "nemacs-main.el"
    "nemacs-gtk-frontend.el"
    "nemacs-editor-transport.el"
    "nemacs-gtk-view-menu.el"
    "nemacs-gui-file-bridge-runtime.el"
    "emacs-tui-backend.el"
    "emacs-tui-event.el"
    "emacs-project.el"
    "emacs-dump.el"
    "image-loader.el"
    "nemacs-loaddefs.el"
    "emacs-elisp-eval.el"
    "emacs-ielm.el"
    "lisp-mode.el"
    "emacs-redisplay-core.el"
    "files-standalone-buffer.el"
    "emacs-redisplay.el"
    "emacs-font-lock-builtins.el"
    "emacs-elisp-mode.el")
  "Files that individual package group loaders must not pull in.")

(defun nemacs-library-package-smoke--feature ()
  "Return the package group feature requested by the batch target."
  (let ((name (getenv "NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE")))
    (unless (and name (not (string= name "")))
      (error "NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE is required"))
    (intern name)))

(defun nemacs-library-package-smoke--loaded-file-p (basename)
  "Return the loaded path whose nondirectory name is BASENAME, or nil."
  (let (loaded)
    (dolist (entry load-history loaded)
      (let ((file (car entry)))
        (when (and (stringp file)
                   (string= (file-name-nondirectory file) basename))
          (setq loaded file))))))

(defun nemacs-library-package-smoke--symbol-value-or-nil (symbol)
  "Return SYMBOL's value, or nil when SYMBOL is unbound."
  (and (boundp symbol) (symbol-value symbol)))

(defun nemacs-library-package-smoke--feature-manifest (feature)
  "Return FEATURE's member manifest, or a singleton fallback."
  (cond
   ((eq feature 'emacs-foundation)
    (nemacs-library-package-smoke--symbol-value-or-nil
     'emacs-foundation-features))
   ((eq feature 'emacs-text-core)
    (nemacs-library-package-smoke--symbol-value-or-nil
     'emacs-text-core-features))
   ((eq feature 'emacs-buffer-core)
    (nemacs-library-package-smoke--symbol-value-or-nil
     'emacs-buffer-core-features))
   ((eq feature 'emacs-editing)
    (nemacs-library-package-smoke--symbol-value-or-nil
     'emacs-editing-features))
   ((eq feature 'emacs-io)
    (nemacs-library-package-smoke--symbol-value-or-nil
     'emacs-io-features))
   ((eq feature 'emacs-core)
    (nemacs-library-package-smoke--symbol-value-or-nil
     'emacs-core-features))
   (t (list feature))))

(defun nemacs-library-package-smoke--assert (condition message &rest args)
  "Signal an error unless CONDITION is non-nil.
MESSAGE and ARGS are passed to `format'."
  (unless condition
    (error "%s" (apply #'format message args))))

(defun nemacs-library-package-smoke-batch ()
  "Run package loader smoke proof for `NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE'."
  (let ((feature (nemacs-library-package-smoke--feature)))
    (require feature)
    (nemacs-library-package-smoke--assert
     (featurep feature)
     "required feature was not provided: %S" feature)
    (dolist (member (nemacs-library-package-smoke--feature-manifest feature))
      (nemacs-library-package-smoke--assert
       (featurep member)
       "package member feature was not provided: %S via %S" member feature))
    (dolist (forbidden nemacs-library-package-smoke--forbidden-features)
      (nemacs-library-package-smoke--assert
       (not (featurep forbidden))
       "package group %S loaded forbidden feature %S" feature forbidden))
    (dolist (file nemacs-library-package-smoke--forbidden-files)
      (nemacs-library-package-smoke--assert
       (not (nemacs-library-package-smoke--loaded-file-p file))
       "package group %S loaded forbidden file %s" feature file))
    (princ (format "nemacs-library-package-smoke: feature=%S ok\n" feature))))

(provide 'nemacs-library-package-smoke)

;;; nemacs-library-package-smoke.el ends here
