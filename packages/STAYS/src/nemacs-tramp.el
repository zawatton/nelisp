;;; nemacs-tramp.el --- ssh-only Tramp lane glue (task #16)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 37 (2026-07) -- Layer 2 (IO/runtime adapters).
;;
;; `nemacs-tramp' bridges the vendored Emacs Tramp
;; (`vendor/emacs-lisp/net/tramp*.el', version 2.7.1.30.1) into a
;; nelisp-emacs image.  Naming note: this file is NOT the NeLisp
;; trampoline evaluator (`vendor/nelisp/packages/nelisp-tramp/', public
;; API `nelisp-tramp-eval') -- that is an unrelated internal evaluator
;; dispatch mechanism.  This module is the Emacs-compatible
;; `/ssh:host:/path' remote-file lane (roadmap Doc 11 M11, task #16).
;;
;; Scope (2026-06-10 user decision, Doc 11 M11): ssh/scp only.  Dired
;; full integration, multi-hop (`/ssh:a|ssh:b:/path'), interactive
;; pty/password prompting, and every other Tramp method (`smb', `adb',
;; `sudo', `su', `rsh', `telnet', `rclone', `fuse', `crypt', `container',
;; `archive', `ftp', ...) are explicitly out of scope.  `nemacs-tramp-setup'
;; only requires `tramp-sh' (not the other backend files) and installs a
;; guard, `nemacs-tramp--guard-vec', that signals
;; `nemacs-tramp-unsupported-method' for anything outside
;; `nemacs-tramp-supported-methods' -- because `tramp-sh' registers
;; itself as the *default* foreign handler (`tramp-register-foreign-file-name-handler'
;; with predicate `#'identity', tramp-sh.el's very last form), it would
;; otherwise silently accept every method `tramp-sh.el' itself defines
;; (`scp', `rsync', `rsh', `telnet', `su', `sudo', ...), not just ssh.
;;
;; `nemacs-tramp-setup' is idempotent and is never called from
;; `nemacs-main'/`nemacs-loadup' (Doc 37 risk #10: do not bake Tramp into
;; a baked runtime image's defun replay).  Host callers call it
;; explicitly (e.g. from `init.el', or a test); the standalone reader is
;; expected to call it after session start, once dynamic `require' of a
;; vendor package is available.

;;; Code:

(require 'emacs-file-name-handler)

(define-error 'nemacs-tramp-unsupported-method
  "Unsupported Tramp method for the nelisp-emacs ssh-only lane")

(defconst nemacs-tramp-supported-methods '("ssh" "scp")
  "Tramp methods the ssh-only lane accepts.
Everything else signals `nemacs-tramp-unsupported-method' through the
`tramp-dissect-file-name' guard installed by `nemacs-tramp-setup'.")

(defvar nemacs-tramp--setup-done nil
  "Non-nil once `nemacs-tramp-setup' has configured this session.")

(defun nemacs-tramp--vendor-net-root ()
  "Return the `vendor/emacs-lisp/' directory this source tree ships.
Derived from this file's own location (falling back to
`locate-library') so the lane works whether it is loaded from `src/'
directly or from a package-scaffold copy."
  (let* ((here (or load-file-name buffer-file-name
                   (and (fboundp 'locate-library)
                        (locate-library "nemacs-tramp"))))
         (src-dir (and here (file-name-directory here))))
    (and src-dir (expand-file-name "../vendor/emacs-lisp/" src-dir))))

(defun nemacs-tramp--add-load-path-dir (root sub)
  "Add ROOT/SUB to `load-path' once, when the directory exists."
  (let ((dir (expand-file-name sub root)))
    (when (and (file-directory-p dir)
               (not (member dir load-path)))
      (add-to-list 'load-path dir))))

(defun nemacs-tramp--add-load-path ()
  "Add the vendor `net/' and `calendar/' directories Tramp needs.
`tramp-compat' pulls in `parse-time', which lives under
`vendor/emacs-lisp/calendar/' rather than alongside the other Tramp
dependencies (`ansi-color', `auth-source', `format-spec', `xdg',
`help-mode', `ls-lisp', which are directly under `vendor/emacs-lisp/'
already on `load-path' in a normal nelisp-emacs image)."
  (let ((root (nemacs-tramp--vendor-net-root)))
    (when root
      (nemacs-tramp--add-load-path-dir root "net")
      (nemacs-tramp--add-load-path-dir root "calendar"))))

(defun nemacs-tramp--guard-vec (vec)
  "Signal `nemacs-tramp-unsupported-method' unless VEC uses a supported method.
Installed as `:filter-return' advice on `tramp-dissect-file-name' so
every Tramp entry point (the generic `tramp-file-name-handler' as well
as each backend's `with-parsed-tramp-file-name') is guarded at the same
single point, instead of letting an unregistered/unloaded backend fail
with an obscure void-function or connection error partway through."
  (when (and (fboundp 'tramp-file-name-p) (tramp-file-name-p vec))
    (let ((method (and (fboundp 'tramp-file-name-method)
                       (tramp-file-name-method vec))))
      (when (and method (not (member method nemacs-tramp-supported-methods)))
        (signal 'nemacs-tramp-unsupported-method (list method)))))
  vec)

(defun nemacs-tramp--install-guard ()
  "Install the ssh/scp-only method guard, once."
  (when (and (fboundp 'tramp-dissect-file-name)
             (fboundp 'advice-add)
             (not (advice-member-p #'nemacs-tramp--guard-vec
                                    'tramp-dissect-file-name)))
    (advice-add 'tramp-dissect-file-name :filter-return
                #'nemacs-tramp--guard-vec)))

(defun nemacs-tramp--configure ()
  "Set the ssh-only lane's Tramp defaults (Doc 37 Sec 3 M1 completion gate).
`tramp-process-connection-type' nil forces a pipe instead of a pty (Doc
37 risk #4: no interactive pty/password support in this lane -- key-based
auth only).  `auth-sources' nil keeps Tramp from touching
`~/.authinfo'/`~/.netrc' (Doc 37 risk #9).  `tramp-use-connection-share'
is the modern name of the option historically called
`tramp-use-ssh-controlmaster-options'; setting it nil avoids leaving
ControlMaster sockets behind in this lane."
  (when (boundp 'auth-sources) (setq auth-sources nil))
  (when (boundp 'tramp-default-method) (setq tramp-default-method "ssh"))
  (when (boundp 'tramp-process-connection-type)
    (setq tramp-process-connection-type nil))
  (when (boundp 'tramp-use-connection-share)
    (setq tramp-use-connection-share nil))
  (when (boundp 'tramp-verbose) (setq tramp-verbose 3)))

;;;###autoload
(defun nemacs-tramp-setup ()
  "Load and configure the vendored ssh-only Tramp lane.
Adds the vendor `net/'+`calendar/' directories to `load-path', requires
`tramp' and `tramp-sh' (only -- no other backend), installs the
ssh/scp-only method guard, and sets the ssh-only lane defaults.
Idempotent: safe to call more than once.  Safe under host Emacs too --
if Tramp is already loaded there, the `require's are no-ops and this
only adds the vendor copy's directories and this lane's defaults on
top."
  (unless nemacs-tramp--setup-done
    (nemacs-tramp--add-load-path)
    (require 'tramp)
    (require 'tramp-sh)
    (nemacs-tramp--install-guard)
    (nemacs-tramp--configure)
    (setq nemacs-tramp--setup-done t))
  t)

(provide 'nemacs-tramp)

;;; nemacs-tramp.el ends here
