;;; nemacs-next.el --- Product contract for nemacs-next  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; `nemacs-next' is an application consumer of the reusable nelisp-emacs
;; libraries.  This module intentionally contains product/session contract
;; data only.  Editor semantics remain in the library packages.

;;; Code:

(require 'nelisp-emacs)

(defconst nemacs-next-protocol-version 0
  "Current nemacs-next frontend/session protocol version.")

(defconst nemacs-next-required-library-packages
  '(foundation
    text-core
    buffer-core
    editing
    io
    special-buffers
    core)
  "Reusable `nelisp-emacs' package groups required by nemacs-next.
The app consumes these packages through the facade contract instead of
requiring app/bootstrap or legacy GUI modules.")

(defconst nemacs-next-optional-library-packages
  '(textmodes-stub)
  "Reusable package groups that are useful but not core session blockers.")

(defconst nemacs-next-frontend-owned-surfaces
  '(native-window
    renderer
    raw-input-decoding
    keyboard-input
    text-rendering
    font-discovery
    ime
    clipboard
    menu-rendering
    toolbar-rendering
    tabs
    native-dialogs
    window-chrome)
  "Surfaces owned by the nemacs-next GUI frontend.")

(defconst nemacs-next-session-owned-surfaces
  '(buffer-state
    point-and-mark
    undo
    keymap-lookup
    command-dispatch
    minibuffer
    completion
    file-commands
    process-objects
    package-loading
    user-init
    hooks
    faces)
  "Surfaces owned by the persistent editor session and reusable libraries.")

(defconst nemacs-next-client-message-types
  '(hello input command resize open clipboard shutdown)
  "Protocol V0 message types sent from frontend to editor session.")

(defconst nemacs-next-session-message-types
  '(hello snapshot delta minibuffer echo menu request error)
  "Protocol V0 message types sent from editor session to frontend.")

(defun nemacs-next--manifest-package-names ()
  "Return package names exposed by the `nelisp-emacs' library facade."
  (if (fboundp 'nelisp-emacs-library-package-names)
      (nelisp-emacs-library-package-names)
    nil))

(defun nemacs-next-missing-required-packages ()
  "Return required library package groups missing from the facade manifest."
  (let ((available (nemacs-next--manifest-package-names))
        missing)
    (dolist (package nemacs-next-required-library-packages (nreverse missing))
      (unless (memq package available)
        (setq missing (cons package missing))))))

(defun nemacs-next-session-plan ()
  "Return the current product/session contract as a plist."
  (list :protocol-version nemacs-next-protocol-version
        :required-library-packages
        (copy-sequence nemacs-next-required-library-packages)
        :optional-library-packages
        (copy-sequence nemacs-next-optional-library-packages)
        :frontend-owned-surfaces
        (copy-sequence nemacs-next-frontend-owned-surfaces)
        :session-owned-surfaces
        (copy-sequence nemacs-next-session-owned-surfaces)
        :client-message-types
        (copy-sequence nemacs-next-client-message-types)
        :session-message-types
        (copy-sequence nemacs-next-session-message-types)
        :missing-required-library-packages
        (nemacs-next-missing-required-packages)))

(provide 'nemacs-next)

;;; nemacs-next.el ends here
