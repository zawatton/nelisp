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
;; Each definition is gated on `unless (fboundp ...)' / `unless
;; (boundp ...)' so loading inside a host Emacs is a cheap no-op:
;; the C builtins win and the substrate is exercised separately by
;; ERTs that call the prefixed names directly.
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
;;
;; Deferred to subsequent phases:
;;   B.4: command-loop-1 / top-level
;;   B.5: execute-extended-command / prefix-arg / current-prefix-arg
;;   B.6: keyboard-quit / recursive-edit / abort-recursive-edit

;;; Code:

(require 'emacs-command-loop)

;;;; --- function bridges ----------------------------------------------

(unless (fboundp 'read-event)
  (defalias 'read-event #'emacs-command-loop-read-event))

(unless (fboundp 'read-char)
  (defalias 'read-char #'emacs-command-loop-read-char))

(unless (fboundp 'read-command)
  (defalias 'read-command #'emacs-command-loop-read-command))

(unless (fboundp 'this-command-keys)
  (defalias 'this-command-keys #'emacs-command-loop-this-command-keys))

(unless (fboundp 'this-command-keys-vector)
  (defalias 'this-command-keys-vector
    #'emacs-command-loop-this-command-keys-vector))

(unless (fboundp 'this-single-command-keys)
  ;; MVP: no menu-event distinction; same as `this-command-keys'.
  (defalias 'this-single-command-keys
    #'emacs-command-loop-this-command-keys))

(unless (fboundp 'this-single-command-raw-keys)
  (defalias 'this-single-command-raw-keys
    #'emacs-command-loop-this-command-keys-vector))

(unless (fboundp 'clear-this-command-keys)
  (defalias 'clear-this-command-keys
    #'emacs-command-loop-clear-this-command-keys))

(unless (fboundp 'read-key-sequence)
  (defalias 'read-key-sequence
    #'emacs-command-loop-read-key-sequence))

(unless (fboundp 'read-key-sequence-vector)
  (defalias 'read-key-sequence-vector
    #'emacs-command-loop-read-key-sequence-vector))

(unless (fboundp 'call-interactively)
  (defalias 'call-interactively #'emacs-command-loop-call-interactively))

(unless (fboundp 'funcall-interactively)
  (defalias 'funcall-interactively #'emacs-command-loop-funcall-interactively))

(unless (fboundp 'command-execute)
  (defalias 'command-execute #'emacs-command-loop-command-execute))

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

(provide 'emacs-command-loop-builtins)

;;; emacs-command-loop-builtins.el ends here
