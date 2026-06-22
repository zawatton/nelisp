;;; emacs-process-builtins.el --- Process bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track I (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed process API to the substrate in
;; `emacs-process.el'.  Function definitions use a host-aware install
;; gate: host Emacs keeps its C builtins, while standalone NeLisp
;; overwrites bootstrap stubs with the pure-Elisp process substrate.
;; Variables are still gated on `unless (boundp ...)' so host-owned
;; special variables win.
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

(defun emacs-process-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-process-builtins--install-function-p 'call-process)
  (defalias 'call-process #'emacs-process-call-process))

(when (emacs-process-builtins--install-function-p 'call-process-region)
  (defalias 'call-process-region #'emacs-process-call-process-region))

(when (emacs-process-builtins--install-function-p 'start-process)
  (defalias 'start-process #'emacs-process-start-process))

(when (emacs-process-builtins--install-function-p 'make-process)
  (defalias 'make-process #'emacs-process-make-process))

(when (emacs-process-builtins--install-function-p 'processp)
  (defalias 'processp #'emacs-process-processp))

(when (emacs-process-builtins--install-function-p 'process-list)
  (defalias 'process-list #'emacs-process-process-list))

(when (emacs-process-builtins--install-function-p 'process-status)
  (defalias 'process-status #'emacs-process-process-status))

(when (emacs-process-builtins--install-function-p 'process-exit-status)
  (defalias 'process-exit-status #'emacs-process-process-exit-status))

(when (emacs-process-builtins--install-function-p 'process-buffer)
  (defalias 'process-buffer #'emacs-process-process-buffer))

(when (emacs-process-builtins--install-function-p 'process-name)
  (defalias 'process-name #'emacs-process-process-name))

(when (emacs-process-builtins--install-function-p 'process-command)
  (defalias 'process-command #'emacs-process-process-command))

(when (emacs-process-builtins--install-function-p 'process-live-p)
  (defalias 'process-live-p #'emacs-process-process-live-p))

(when (emacs-process-builtins--install-function-p 'process-id)
  (defalias 'process-id #'emacs-process-process-id))

(when (emacs-process-builtins--install-function-p 'process-mark)
  (defalias 'process-mark #'emacs-process-process-mark))

(when (emacs-process-builtins--install-function-p 'set-process-filter)
  (defalias 'set-process-filter #'emacs-process-set-process-filter))

(when (emacs-process-builtins--install-function-p 'set-process-sentinel)
  (defalias 'set-process-sentinel #'emacs-process-set-process-sentinel))

(when (emacs-process-builtins--install-function-p 'accept-process-output)
  (defalias 'accept-process-output #'emacs-process-accept-process-output))

(when (emacs-process-builtins--install-function-p 'signal-process)
  (defalias 'signal-process #'emacs-process-signal-process))

(when (emacs-process-builtins--install-function-p 'kill-process)
  (defalias 'kill-process #'emacs-process-kill-process))

(when (emacs-process-builtins--install-function-p 'process-send-string)
  (defalias 'process-send-string #'emacs-process-process-send-string))

(when (emacs-process-builtins--install-function-p 'process-send-eof)
  (defalias 'process-send-eof #'emacs-process-process-send-eof))

(when (emacs-process-builtins--install-function-p 'delete-process)
  (defalias 'delete-process #'emacs-process-delete-process))

(when (emacs-process-builtins--install-function-p 'shell-command)
  (defalias 'shell-command #'emacs-process-shell-command))

(when (emacs-process-builtins--install-function-p 'shell-command-to-string)
  (defalias 'shell-command-to-string
    #'emacs-process-shell-command-to-string))

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'shell-file-name)
  (defvar shell-file-name "/bin/sh"
    "Track I bridge: path to the shell used by `shell-command'."))

(unless (boundp 'shell-command-switch)
  (defvar shell-command-switch "-c"
    "Track I bridge: the shell flag that invokes a single command."))


;;;; --- A19 follow-up: filter/sentinel getters + plist/buffer/query/region --
(when (emacs-process-builtins--install-function-p 'process-filter)
  (defalias 'process-filter #'emacs-process-process-filter))
(when (emacs-process-builtins--install-function-p 'process-sentinel)
  (defalias 'process-sentinel #'emacs-process-process-sentinel))
(when (emacs-process-builtins--install-function-p 'set-process-buffer)
  (defalias 'set-process-buffer #'emacs-process-set-process-buffer))
(when (emacs-process-builtins--install-function-p 'process-plist)
  (defalias 'process-plist #'emacs-process-process-plist))
(when (emacs-process-builtins--install-function-p 'set-process-plist)
  (defalias 'set-process-plist #'emacs-process-set-process-plist))
(when (emacs-process-builtins--install-function-p 'process-query-on-exit-flag)
  (defalias 'process-query-on-exit-flag
    #'emacs-process-process-query-on-exit-flag))
(when (emacs-process-builtins--install-function-p 'set-process-query-on-exit-flag)
  (defalias 'set-process-query-on-exit-flag
    #'emacs-process-set-process-query-on-exit-flag))
(when (emacs-process-builtins--install-function-p 'process-send-region)
  (defalias 'process-send-region #'emacs-process-process-send-region))

(provide 'emacs-process-builtins)

;;; emacs-process-builtins.el ends here
