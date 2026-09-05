;;; emacs-parity-core-vars.el --- core var fills -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k19: the marker-leak fix made declared-but-unbound core vars signal
;; void-variable (instead of leaking the sentinel as a value).  Fill the safe
;; core ones with real stock values, guarded with `unless boundp'.
;;
;; DEFERRED (empty shim would shadow the package's real map): `isearch-mode-map',
;; `grep-mode-map', `minibuffer-local-completion-map', `mode-line-major-mode-keymap',
;; `vc-hg-log-view-mode-map', `vc-git-log-view-mode-map', `org-org-menu',
;; `evil-command-window-mode-map', `diff-hl-stage-diff-mode-map',
;; `compilation-minor-mode-map', `treemacs-header-projects-button',
;; `sh-mode-syntax-table' -- read while unbound but defined by their package.

;;; Code:

;; --- version (kills the ~141 `emacs-version' cascade) ---
(unless (boundp 'emacs-version) (defvar emacs-version "30.1"))
(unless (boundp 'emacs-major-version) (defvar emacs-major-version 30))
(unless (boundp 'emacs-minor-version) (defvar emacs-minor-version 1))
(unless (boundp 'emacs-build-time) (defvar emacs-build-time nil))
(unless (boundp 'emacs-build-system) (defvar emacs-build-system "nemacs"))

;; --- batch/display context ---
(unless (boundp 'window-system) (defvar window-system nil))
(unless (boundp 'invocation-directory) (defvar invocation-directory "/usr/bin/"))
(unless (boundp 'invocation-name) (defvar invocation-name "emacs"))
(unless (boundp 'user-full-name)
  (defvar user-full-name (or (getenv "USER") "user")))
(unless (boundp 'enable-multibyte-characters) (defvar enable-multibyte-characters t))

;; --- cc-mode char classes / features (stock cc-defs values; used inside [...]) ---
(unless (boundp 'c-alpha) (defvar c-alpha "[:alpha:]"))
(unless (boundp 'c-alnum) (defvar c-alnum "[:alnum:]"))
(unless (boundp 'c-upper) (defvar c-upper "[:upper:]"))
(unless (boundp 'c-lower) (defvar c-lower "[:lower:]"))
(unless (boundp 'c-use-category) (defvar c-use-category nil))
(unless (boundp 'c-emacs-features)
  (defvar c-emacs-features '(pps-extended-state category-properties syntax-properties)))
(unless (boundp 'objc-font-lock-extra-types) (defvar objc-font-lock-extra-types nil))
(unless (boundp 'cc-imenu-c++-generic-expression) (defvar cc-imenu-c++-generic-expression nil))
(unless (boundp 'cc-imenu-java-type-spec-regexp) (defvar cc-imenu-java-type-spec-regexp nil))

;; --- misc core ---
(unless (boundp 'stipple-pixmap) (defvar stipple-pixmap nil))
(unless (boundp 'lisp-mode-symbol-regexp)
  (defvar lisp-mode-symbol-regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"))
(unless (boundp 'completion-styles-alist) (defvar completion-styles-alist nil))
(unless (boundp 'completion-ignored-extensions)
  (defvar completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".obj")))
(unless (boundp 'face-attribute-name-alist) (defvar face-attribute-name-alist nil))
(unless (boundp 'frameset-filter-alist) (defvar frameset-filter-alist nil))
(unless (boundp 'emacs-easy-mmode--standalone-p) (defvar emacs-easy-mmode--standalone-p t))
(unless (boundp 'flycheck-this-emacs-executable) (defvar flycheck-this-emacs-executable nil))

;; --- mode-line value var (stock default is nil; a VALUE var, not a keymap,
;; so filling it cannot shadow a package's real map like the DEFERRED entries
;; above).  `global-mode-string' is read by mode-line construction in many
;; packages and signalled void-variable in the full-init audit. ---
(unless (boundp 'global-mode-string) (defvar global-mode-string nil))

;; --- minor-mode state vars (read before their define-minor-mode sets them) ---
(unless (boundp 'evil-mode) (defvar evil-mode nil))
(unless (boundp 'mouse-wheel-mode) (defvar mouse-wheel-mode nil))
(unless (boundp 'auto-compression-mode) (defvar auto-compression-mode nil))

;; --- input method state (headless: no input method active) ---
(unless (boundp 'current-input-method)
  (defvar current-input-method nil
    "The current input method for reading text, or nil for none."))
(unless (boundp 'current-input-method-title)
  (defvar current-input-method-title nil
    "Mode-line title of the current input method, or nil."))

;; T79: the rest of GNU's input-method/mule-cmds.el variable surface.
;; This runtime never registers a LEIM/Quail input method, so
;; `input-method-alist' stays permanently empty and every value below is
;; the real stock GNU default (`keyboard.c'/`mule-cmds.el'), not an
;; invented headless placeholder -- confirmed against host Emacs 31.1
;; (`emacs -Q --batch'), including the surprising real C default for
;; `input-method-function' (the function `list', not nil: applying it to
;; a just-read character returns a one-element list containing that same
;; character unchanged, which is GNU's built-in identity no-op for "no
;; input method is translating this keystroke").  `evil-core.el's
;; `evil-local-mode' reads `deactivate-current-input-method-function'
;; directly in a `when' guard while switching evil states; that bare
;; variable reference is what signalled `void-variable' before this fix.
(unless (boundp 'default-input-method)
  (defvar default-input-method nil
    "Default input method for multilingual text (a string), or nil.
This is the input method activated automatically by the command
`toggle-input-method'."))
(unless (boundp 'input-method-alist)
  (defvar input-method-alist nil
    "Alist of input method names vs how to use them.
Each element has the form (INPUT-METHOD LANGUAGE-ENV ACTIVATE-FUNC TITLE
DESCRIPTION ARGS...); see `register-input-method'.  No LEIM/Quail input
method is ever registered in this headless runtime, so this alist stays
empty -- `register-input-method', if a package calls it, still appends
to it normally."))
(put 'input-method-alist 'risky-local-variable t)
(unless (boundp 'input-method-function)
  (defvar input-method-function 'list
    "If non-nil, the function that implements the current input method.
It is called with one argument, a just-read character; it should return
a list of zero or more events to use as input.  The real GNU default
(with no input method active) is the function `list', which trivially
returns the character unchanged as a one-element list -- not nil."))
(unless (boundp 'input-method-verbose-flag)
  (defvar input-method-verbose-flag 'default
    "A flag to control extra guidance given by input methods.
Stock GNU default is the symbol `default'; irrelevant without an
active input method, but declared with its real value for parity."))
(unless (boundp 'input-method-highlight-flag)
  (defvar input-method-highlight-flag t
    "Non-nil means input methods highlight partially-entered text.
Stock GNU default is t; irrelevant without an active input method, but
declared with its real value for parity."))
(unless (boundp 'input-method-activate-hook)
  (defvar input-method-activate-hook nil
    "Normal hook run just after an input method is activated."))
(unless (boundp 'input-method-deactivate-hook)
  (defvar input-method-deactivate-hook nil
    "Normal hook run just after an input method is deactivated."))
(unless (boundp 'deactivate-current-input-method-function)
  (defvar deactivate-current-input-method-function nil
    "Function to call for deactivating the current input method.
Every input method should set this to an appropriate value when
activated; this function is then called with no argument.  It is set
back to nil by `deactivate-input-method'."))
(unless (boundp 'describe-current-input-method-function)
  (defvar describe-current-input-method-function nil
    "Function to call for describing the current input method.
This function is called with no argument."))

;; --- input method commands (headless: same "no LEIM/Quail method is
;; ever registered" constraint as the variables above) ---
;;
;; `activate-input-method'/`deactivate-input-method' mirror GNU's real
;; `mule-cmds.el' control flow: deactivating when no method is active is
;; an honest no-op that leaves `current-input-method' at nil; activating
;; a concrete (non-nil) INPUT-METHOD looks it up in `input-method-alist',
;; which is always empty here, so it signals exactly as GNU does for an
;; unregistered name -- confirmed against host Emacs 31.1
;; (`(activate-input-method "no-such-method")' => `(error "Can't
;; activate input method \\='no-such-method\\='")').
(unless (fboundp 'activate-input-method)
  (defun activate-input-method (input-method)
    "Switch to input method INPUT-METHOD for the current buffer.
If some other input method is already active, turn it off first.
If INPUT-METHOD is nil, deactivate any current input method."
    (when (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
    (when (and current-input-method
               (not (string= current-input-method input-method)))
      (deactivate-input-method))
    (unless (or current-input-method (null input-method))
      (let ((slot (assoc input-method input-method-alist)))
        (unless slot
          (error "Can't activate input method `%s'" input-method))
        (setq current-input-method-title nil)
        (let ((func (nth 2 slot)))
          (cond
           ((functionp func) (apply func input-method (nthcdr 5 slot)))
           ((and (consp func) (symbolp (car func)) (symbolp (cdr func)))
            (require (cdr func))
            (apply (car func) input-method (nthcdr 5 slot)))
           (t (error "Can't activate input method `%s'" input-method))))
        (setq current-input-method input-method)
        (or (stringp current-input-method-title)
            (setq current-input-method-title (nth 3 slot)))
        (run-hooks 'input-method-activate-hook)
        (force-mode-line-update)))
    nil))
(unless (fboundp 'deactivate-input-method)
  (defun deactivate-input-method ()
    "Turn off the current input method."
    (when current-input-method
      (setq input-method-function nil
            current-input-method-title nil)
      (when deactivate-current-input-method-function
        (funcall deactivate-current-input-method-function))
      (run-hooks 'input-method-deactivate-hook)
      (setq current-input-method nil)
      (force-mode-line-update))
    nil))
;; Guarded like every other definition in this file, so host Emacs
;; (where `set-input-method' is already a real function, defined in
;; `mule-cmds.el') keeps its own primitive when this file is `require'd
;; directly, e.g. by the host ERT parity tests.  `src/emacs-stub.el'
;; also carries a headless
;; no-op stub for `set-input-method'
;; (`(defun set-input-method (&rest _) nil)'); this file is placed
;; ahead of it in the standalone bootstrap bundle (`core-vars' is
;; forced immediately before `emacs-vars.el', itself far earlier than
;; `emacs-stub.el' -- see `scripts/build-nelisp-bootstrap.el'), so the
;; real implementation below defines first and the later stub's own
;; `unless fboundp' guard then skips -- verified empirically on the
;; standalone (`(set-input-method "no-such-method")' signals, it does
;; not silently return nil).
(unless (fboundp 'set-input-method)
  (defun set-input-method (input-method &optional _interactive)
    "Select and activate input method INPUT-METHOD for the current buffer.
This also sets the default input method to the one you specify.
If INPUT-METHOD is nil, this function turns off the input method."
    (activate-input-method input-method)
    (setq default-input-method input-method)
    default-input-method))

;; --- headless display-hint functions -----------------------------------
;; The batch substrate has no frame/redisplay, so these C display primitives
;; are correctly inert here: `force-window-update' only schedules a redisplay
;; and `set-window-fringes' sets display-only geometry; both return nil like
;; the C originals do for a non-interactive frame.  They surfaced as
;; void-function in the full-init audit (mode/ui setup calling them at load).
;; Guarded with `fboundp' so a real GUI build keeps its native primitives.
(unless (fboundp 'force-window-update)
  (defun force-window-update (&optional _object)
    "Headless no-op: no frame redisplay to force in the batch substrate."
    nil))
(unless (fboundp 'set-window-fringes)
  (defun set-window-fringes (&rest _args)
    "Headless no-op: fringe geometry is display-only in the batch substrate."
    nil))

(provide 'emacs-parity-core-vars)

;;; emacs-parity-core-vars.el ends here
