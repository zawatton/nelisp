;;; nelisp-emacs-package-smoke-test.el --- Package group require smoke  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst nelisp-emacs-package-smoke-test--forbidden-features
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

(defconst nelisp-emacs-package-smoke-test--forbidden-files
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

(defun nelisp-emacs-package-smoke-test--feature ()
  "Return the package group feature requested by the batch target."
  (let ((name (getenv "NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE")))
    (unless (and name (not (string= name "")))
      (error "NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE is required"))
    (intern name)))

(defun nelisp-emacs-package-smoke-test--loaded-file-p (basename)
  "Return the loaded path whose nondirectory name is BASENAME, or nil."
  (let (loaded)
    (dolist (entry load-history loaded)
      (let ((file (car entry)))
        (when (and (stringp file)
                   (string= (file-name-nondirectory file) basename))
          (setq loaded file))))))

(defun nelisp-emacs-package-smoke-test--feature-manifest (feature)
  "Return FEATURE's member manifest, or a singleton fallback."
  (cond
   ((eq feature 'emacs-foundation) emacs-foundation-features)
   ((eq feature 'emacs-text-core) emacs-text-core-features)
   ((eq feature 'emacs-buffer-core) emacs-buffer-core-features)
   ((eq feature 'emacs-editing) emacs-editing-features)
   ((eq feature 'emacs-io) emacs-io-features)
   ((eq feature 'emacs-core) emacs-core-features)
   (t (list feature))))

(ert-deftest nelisp-emacs-package-smoke-test/require-package-group-from-src-only ()
  (let ((feature (nelisp-emacs-package-smoke-test--feature)))
    (require feature)
    (should (featurep feature))
    (dolist (member (nelisp-emacs-package-smoke-test--feature-manifest feature))
      (should (featurep member)))))

(ert-deftest nelisp-emacs-package-smoke-test/package-group-does-not-load-app-or-frontends ()
  (require (nelisp-emacs-package-smoke-test--feature))
  (dolist (feature nelisp-emacs-package-smoke-test--forbidden-features)
    (should-not (featurep feature)))
  (dolist (file nelisp-emacs-package-smoke-test--forbidden-files)
    (should-not (nelisp-emacs-package-smoke-test--loaded-file-p file))))

;;; nelisp-emacs-package-smoke-test.el ends here
