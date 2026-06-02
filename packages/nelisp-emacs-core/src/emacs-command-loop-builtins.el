;;; emacs-command-loop-builtins.el --- Unprefixed command-loop bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track B Phase B.1 (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* command-loop names to the
;; substrate in `emacs-command-loop.el', mirroring the bridge pattern
;; used by Track C / D / E / J / L1 / 11.C''.
;;
;; Function definitions use a host-aware install gate: host Emacs keeps
;; its C builtins, while standalone NeLisp overwrites any bootstrap
;; stubs with the real substrate functions.  Variables are still gated
;; on `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridged today (B.1 = foundation only):
;;
;;   - Functions: read-event / read-char / read-command /
;;     this-command-keys / this-command-keys-vector /
;;     this-single-command-keys / this-single-command-raw-keys /
;;     clear-this-command-keys
;;
;;   - Variables: this-command / last-command / real-this-command /
;;     last-command-event / last-input-event / last-nonmenu-event /
;;     unread-command-events / quit-flag / inhibit-quit /
;;     throw-on-input
;;
;; B.2 (2026-05-03) added: read-key-sequence / read-key-sequence-vector.
;; B.3 (2026-05-03) added: call-interactively / funcall-interactively /
;;                          command-execute / prefix-arg / current-prefix-arg.
;; B.4 (2026-05-03) added: command-loop-1 / top-level / recursive-edit /
;;                          recursion-depth / pre-command-hook /
;;                          post-command-hook.
;; B.5 (2026-05-03) added: execute-extended-command / universal-argument /
;;                          digit-argument / negative-argument.
;; B.6 (2026-05-03) added: keyboard-quit / exit-recursive-edit + real
;;                          recursive-edit / top-level / recursion-depth.
;;                          `command-loop-1' wraps each step in a
;;                          quit-catch so C-g aborts the command,
;;                          not the loop.
;;
;; Deferred to subsequent phases:
;;   B.4: command-loop-1 / top-level
;;   B.5: execute-extended-command / prefix-arg / current-prefix-arg
;;   B.6: keyboard-quit / recursive-edit / abort-recursive-edit

;;; Code:

(require 'emacs-command-loop)

;;;; --- function bridges ----------------------------------------------

(defun emacs-command-loop-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-command-loop-builtins--install-function-p 'read-event)
  (defalias 'read-event #'emacs-command-loop-read-event))

(when (emacs-command-loop-builtins--install-function-p 'read-char)
  (defalias 'read-char #'emacs-command-loop-read-char))

(when (emacs-command-loop-builtins--install-function-p 'read-command)
  (defalias 'read-command #'emacs-command-loop-read-command))

(when (emacs-command-loop-builtins--install-function-p 'this-command-keys)
  (defalias 'this-command-keys #'emacs-command-loop-this-command-keys))

(when (emacs-command-loop-builtins--install-function-p 'this-command-keys-vector)
  (defalias 'this-command-keys-vector
    #'emacs-command-loop-this-command-keys-vector))

(when (emacs-command-loop-builtins--install-function-p 'this-single-command-keys)
  ;; MVP: no menu-event distinction; same as `this-command-keys'.
  (defalias 'this-single-command-keys
    #'emacs-command-loop-this-command-keys))

(when (emacs-command-loop-builtins--install-function-p 'this-single-command-raw-keys)
  (defalias 'this-single-command-raw-keys
    #'emacs-command-loop-this-command-keys-vector))

(when (emacs-command-loop-builtins--install-function-p 'clear-this-command-keys)
  (defalias 'clear-this-command-keys
    #'emacs-command-loop-clear-this-command-keys))

(when (emacs-command-loop-builtins--install-function-p 'read-key-sequence)
  (defalias 'read-key-sequence
    #'emacs-command-loop-read-key-sequence))

(when (emacs-command-loop-builtins--install-function-p 'read-key-sequence-vector)
  (defalias 'read-key-sequence-vector
    #'emacs-command-loop-read-key-sequence-vector))

(when (emacs-command-loop-builtins--install-function-p 'call-interactively)
  (defalias 'call-interactively #'emacs-command-loop-call-interactively))

(when (emacs-command-loop-builtins--install-function-p 'funcall-interactively)
  (defalias 'funcall-interactively #'emacs-command-loop-funcall-interactively))

(when (emacs-command-loop-builtins--install-function-p 'command-execute)
  (defalias 'command-execute #'emacs-command-loop-command-execute))

(when (emacs-command-loop-builtins--install-function-p 'command-loop-1)
  (defalias 'command-loop-1 #'emacs-command-loop-1))

(when (emacs-command-loop-builtins--install-function-p 'top-level)
  (defalias 'top-level #'emacs-command-loop-top-level))

(when (emacs-command-loop-builtins--install-function-p 'recursive-edit)
  (defalias 'recursive-edit #'emacs-command-loop-recursive-edit))

(when (emacs-command-loop-builtins--install-function-p 'recursion-depth)
  (defalias 'recursion-depth #'emacs-command-loop-recursion-depth))

(when (emacs-command-loop-builtins--install-function-p 'execute-extended-command)
  (defalias 'execute-extended-command
    #'emacs-command-loop-execute-extended-command))

(when (emacs-command-loop-builtins--install-function-p 'universal-argument)
  (defalias 'universal-argument #'emacs-command-loop-universal-argument))

(when (emacs-command-loop-builtins--install-function-p 'digit-argument)
  (defalias 'digit-argument #'emacs-command-loop-digit-argument))

(when (emacs-command-loop-builtins--install-function-p 'negative-argument)
  (defalias 'negative-argument #'emacs-command-loop-negative-argument))

(when (emacs-command-loop-builtins--install-function-p 'keyboard-quit)
  (defalias 'keyboard-quit #'emacs-command-loop-keyboard-quit))

(when (emacs-command-loop-builtins--install-function-p 'exit-recursive-edit)
  (defalias 'exit-recursive-edit
    #'emacs-command-loop-exit-recursive-edit))

(defvar emacs-command-loop--sigint-handler-installed-p nil
  "Non-nil after the pure-Elisp SIGINT compatibility handler is installed.")

(when (emacs-command-loop-builtins--install-function-p 'install-sigint-handler)
  (defun install-sigint-handler ()
    "Install the pure-Elisp SIGINT compatibility handler.

Standalone NeLisp may provide this as a runtime builtin.  When it does
not, the Elisp fallback records installation and exposes the same
idempotent success value expected by `nemacs-main'."
    (setq emacs-command-loop--sigint-handler-installed-p t)
    t))

(when (emacs-command-loop-builtins--install-function-p '_sigint-handler-installed-p)
  (defun _sigint-handler-installed-p ()
    "Return non-nil when `install-sigint-handler' has been called."
    emacs-command-loop--sigint-handler-installed-p))

(when (emacs-command-loop-builtins--install-function-p 'set-quit-flag)
  (defun set-quit-flag ()
    "Set the command-loop quit flag and return t."
    (when (not (boundp 'emacs-version))
      (setq quit-flag t))
    (setq emacs-command-loop--quit-flag t)
    t))

(when (emacs-command-loop-builtins--install-function-p 'clear-quit-flag)
  (defun clear-quit-flag ()
    "Clear the command-loop quit flag and return nil."
    (when (not (boundp 'emacs-version))
      (setq quit-flag nil))
    (setq emacs-command-loop--quit-flag nil)
    nil))

(when (emacs-command-loop-builtins--install-function-p 'quit-flag-pending-p)
  (defun quit-flag-pending-p ()
    "Return non-nil when a quit is pending."
    (or emacs-command-loop--quit-flag
        (and (not (boundp 'emacs-version)) quit-flag))))

;; Real Emacs binds `recursive-edit' / `abort-recursive-edit' as C
;; primitives — same gating pattern.  Track C already aliased
;; `abort-recursive-edit' to the minibuffer cancel routine; the
;; minibuffer's quit-signal will still propagate through the
;; command-loop's condition-case so this is consistent.

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'this-command)
  (defvar this-command nil
    "Phase B.1 bridge: the command being executed.  See
`emacs-command-loop--this-command'."))

(unless (boundp 'last-command)
  (defvar last-command nil
    "Phase B.1 bridge: the previously executed command."))

(unless (boundp 'real-this-command)
  (defvar real-this-command nil
    "Phase B.1 bridge: the command actually dispatched (= pre-remap)."))

(unless (boundp 'last-command-event)
  (defvar last-command-event nil
    "Phase B.1 bridge: last event of the key sequence triggering the
current command."))

(unless (boundp 'last-input-event)
  (defvar last-input-event nil
    "Phase B.1 bridge: most recent event read by `read-event'."))

(unless (boundp 'last-nonmenu-event)
  (defvar last-nonmenu-event nil
    "Phase B.1 bridge: most recent input event NOT from a menu bar."))

(unless (boundp 'unread-command-events)
  (defvar unread-command-events nil
    "Phase B.1 bridge: standard alternative event-injection queue.
The substrate `emacs-command-loop-read-event' drains this as a
secondary source after `emacs-command-loop--unread-events' is empty."))

(unless (boundp 'quit-flag)
  (defvar quit-flag nil
    "Phase B.1 bridge: non-nil when quit was requested."))

(unless (boundp 'inhibit-quit)
  (defvar inhibit-quit nil
    "Phase B.1 bridge: non-nil suppresses `quit-flag' processing."))

(unless (boundp 'throw-on-input)
  (defvar throw-on-input nil
    "Phase B.1 bridge: tag to `throw' to on next input read."))

(unless (boundp 'prefix-arg)
  (defvar prefix-arg nil
    "Phase B.3 bridge: prefix arg pending for the next `call-interactively'."))

(unless (boundp 'current-prefix-arg)
  (defvar current-prefix-arg nil
    "Phase B.3 bridge: prefix arg of the command currently executing."))

(unless (boundp 'pre-command-hook)
  (defvar pre-command-hook nil
    "Phase B.4 bridge: hook run before each command-loop dispatch."))

(unless (boundp 'post-command-hook)
  (defvar post-command-hook nil
    "Phase B.4 bridge: hook run after each command-loop dispatch."))

(provide 'emacs-command-loop-builtins)

;;; emacs-command-loop-builtins.el ends here
