;;; emacs-process-builtins.el --- Process bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track I (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed process API to the substrate in
;; `emacs-process.el'.  Same `unless (fboundp ...)' / `unless
;; (boundp ...)' gating as every other Track bridge — under host
;; Emacs the C builtins win and our substrate's `delegate-p' check
;; routes through the host binding.
;;
;; Bridged today:
;;   - call-process / call-process-region
;;   - start-process / make-process
;;   - processp / process-list / process-status /
;;     process-exit-status / process-buffer / process-name
;;   - process-send-string / process-send-eof / delete-process
;;   - shell-command / shell-command-to-string
;;   - shell-file-name / shell-command-switch
;;
;; Deferred:
;;   - filter / sentinel callbacks
;;   - process-coding-system handling
;;   - network processes

;;; Code:

(require 'emacs-process)

;;;; --- function bridges ----------------------------------------------

(unless (fboundp 'call-process)
  (defalias 'call-process #'emacs-process-call-process))

(unless (fboundp 'call-process-region)
  (defalias 'call-process-region #'emacs-process-call-process-region))

(unless (fboundp 'start-process)
  (defalias 'start-process #'emacs-process-start-process))

(unless (fboundp 'make-process)
  (defalias 'make-process #'emacs-process-make-process))

(unless (fboundp 'processp)
  (defalias 'processp #'emacs-process-processp))

(unless (fboundp 'process-list)
  (defalias 'process-list #'emacs-process-process-list))

(unless (fboundp 'process-status)
  (defalias 'process-status #'emacs-process-process-status))

(unless (fboundp 'process-exit-status)
  (defalias 'process-exit-status #'emacs-process-process-exit-status))

(unless (fboundp 'process-buffer)
  (defalias 'process-buffer #'emacs-process-process-buffer))

(unless (fboundp 'process-name)
  (defalias 'process-name #'emacs-process-process-name))

(unless (fboundp 'process-command)
  (defalias 'process-command #'emacs-process-process-command))

(unless (fboundp 'process-live-p)
  (defalias 'process-live-p #'emacs-process-process-live-p))

(unless (fboundp 'process-id)
  (defalias 'process-id #'emacs-process-process-id))

(unless (fboundp 'process-mark)
  (defalias 'process-mark #'emacs-process-process-mark))

(unless (fboundp 'set-process-filter)
  (defalias 'set-process-filter #'emacs-process-set-process-filter))

(unless (fboundp 'set-process-sentinel)
  (defalias 'set-process-sentinel #'emacs-process-set-process-sentinel))

(unless (fboundp 'accept-process-output)
  (defalias 'accept-process-output #'emacs-process-accept-process-output))

(unless (fboundp 'signal-process)
  (defalias 'signal-process #'emacs-process-signal-process))

(unless (fboundp 'kill-process)
  (defalias 'kill-process #'emacs-process-kill-process))

(unless (fboundp 'process-send-string)
  (defalias 'process-send-string #'emacs-process-process-send-string))

(unless (fboundp 'process-send-eof)
  (defalias 'process-send-eof #'emacs-process-process-send-eof))

(unless (fboundp 'delete-process)
  (defalias 'delete-process #'emacs-process-delete-process))

(unless (fboundp 'shell-command)
  (defalias 'shell-command #'emacs-process-shell-command))

(unless (fboundp 'shell-command-to-string)
  (defalias 'shell-command-to-string
    #'emacs-process-shell-command-to-string))

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'shell-file-name)
  (defvar shell-file-name "/bin/sh"
    "Track I bridge: path to the shell used by `shell-command'."))

(unless (boundp 'shell-command-switch)
  (defvar shell-command-switch "-c"
    "Track I bridge: the shell flag that invokes a single command."))

(provide 'emacs-process-builtins)

;;; emacs-process-builtins.el ends here
