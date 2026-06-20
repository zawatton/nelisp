;;; emacs-callproc.el --- NeLisp port of Emacs C core callproc.c env API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Ports `getenv' / `setenv' / `process-environment' from Emacs C
;; core's `callproc.c'.  The bridge first honors the elisp
;; `process-environment' overlay, then falls through to a NeLisp
;; syscall/runtime getenv primitive when one is bound.

;;; Code:

(defvar process-environment nil
  "List of `KEY=VALUE' strings used as the elisp environment overlay.")

(declare-function nl-syscall-getenv "nelisp-runtime" (variable))
(declare-function nelisp-sys-getenv "nelisp-sys" (variable))
(declare-function rdf "nelisp-runtime" (file))

(defconst emacs-callproc--sys-getenv-functions
  '(nelisp-sys-getenv nl-syscall-getenv)
  "Candidate NeLisp runtime getenv functions, in preferred order.")

(defvar emacs-callproc--sys-getenv-active nil
  "Non-nil while `emacs-callproc--sys-getenv' is inside a backend call.")

(defun emacs-callproc--lookup-process-environment (variable)
  "Return VARIABLE from `process-environment', or nil when absent."
  (let ((cur process-environment)
        (prefix (concat variable "="))
        (prefix-len 0)
        (found nil)
        (result nil))
    (setq prefix-len (length prefix))
    (while (and cur (not found))
      (let ((entry (car cur)))
        (if (and (stringp entry)
                 (>= (length entry) prefix-len)
                 (equal (substring entry 0 prefix-len) prefix))
            (progn (setq result (substring entry prefix-len))
                   (setq found t))))
      (setq cur (cdr cur)))
    result))

(defun emacs-callproc--sys-getenv (variable)
  "Return VARIABLE from a NeLisp getenv primitive, or nil if unavailable."
  (unless emacs-callproc--sys-getenv-active
    (let ((emacs-callproc--sys-getenv-active t))
      (catch 'done
        (dolist (fn emacs-callproc--sys-getenv-functions)
          (when (fboundp fn)
            (let ((value (condition-case nil
                             (funcall fn variable)
                           (error nil))))
              (when (stringp value)
                (throw 'done value)))))
        nil))))

(defun emacs-callproc-getenv (variable &optional frame)
  "Look VARIABLE up in the elisp overlay, then the NeLisp runtime env."
  (ignore frame)
  (or (emacs-callproc--lookup-process-environment variable)
      (emacs-callproc--sys-getenv variable)))

;; On the standalone reader the prelude already defines a `getenv' that
;; reads its own (empty) `nelisp--environment', so an `unless fboundp'
;; guard would keep that broken lookup even though we seed
;; `process-environment' from /proc/self/environ below.  Install our
;; `process-environment'-based getenv whenever the reader file primitive
;; `rdf' is present (= standalone); on host Emacs (`rdf' unbound) only
;; when getenv is missing, so the C builtin is never clobbered.
(when (or (not (fboundp 'getenv)) (fboundp 'rdf))
  (defun getenv (variable &optional frame)
    "Polyfill: look VARIABLE up in the NeLisp-compatible environment."
    (emacs-callproc-getenv variable frame)))

;; Same reader/host split as getenv above: keep setenv writing to the
;; `process-environment' overlay that our getenv reads.
(when (or (not (fboundp 'setenv)) (fboundp 'rdf))
  (defun setenv (variable &optional value substitute-env-vars)
    "Polyfill: prepend `VARIABLE=VALUE' to `process-environment'.
Returns VALUE.  When VALUE is nil this removes the entry (= matches
Emacs C semantics).  SUBSTITUTE-ENV-VARS is accepted for arglist
parity and ignored (= no `$VAR' interpolation in the polyfill)."
    (ignore substitute-env-vars)
    ;; Strip any existing entry for VARIABLE.
    (let ((prefix (concat variable "="))
          (prefix-len 0)
          (acc nil)
          (cur process-environment))
      (setq prefix-len (length prefix))
      (while cur
        (let ((entry (car cur)))
          (unless (and (>= (length entry) prefix-len)
                       (equal (substring entry 0 prefix-len) prefix))
            (setq acc (cons entry acc))))
        (setq cur (cdr cur)))
      ;; Reverse acc back to original order, then prepend new entry.
      (let ((forward nil))
        (while acc
          (setq forward (cons (car acc) forward))
          (setq acc (cdr acc)))
        (setq process-environment
              (if value
                  (cons (concat variable "=" value) forward)
                forward))))
    value))


;;;; --- standalone-reader environment seeding --------------------------
;;
;; On nemacs `process-environment' starts empty and there is no native
;; getenv backend wired (`nelisp-sys-getenv' / `nl-syscall-getenv' are
;; unbound, and nothing captures the startup envp), so `getenv' --
;; hence `executable-find's PATH walk -- would always see an empty
;; environment.  The reader does expose a working file reader (`rdf'),
;; so we seed `process-environment' once from Linux `/proc/self/environ'
;; (NUL-separated `KEY=VALUE' entries).  Inert on host Emacs, where
;; `process-environment' is already populated and `rdf' is unbound.

(defconst emacs-callproc--proc-self-environ "/proc/self/environ"
  "Linux pseudo-file exposing the process environment (NUL-separated).")

(defun emacs-callproc--split-on-nul (string)
  "Split STRING on NUL (?\\0) bytes, dropping empty substrings."
  (let ((parts nil)
        (start 0)
        (i 0)
        (len (length string)))
    (while (< i len)
      (when (eq (aref string i) 0)
        (when (> i start)
          (setq parts (cons (substring string start i) parts)))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (when (> len start)
      (setq parts (cons (substring string start len) parts)))
    (nreverse parts)))

(defun emacs-callproc--read-proc-environ ()
  "Return the OS environment as a list of `KEY=VALUE' strings, or nil.
Reads Linux `/proc/self/environ' through the standalone reader's `rdf'
primitive.  Returns nil on host Emacs, non-Linux, or when `rdf' or the
pseudo-file is unavailable."
  (when (fboundp 'rdf)
    (let ((raw (condition-case nil
                   (rdf emacs-callproc--proc-self-environ)
                 (error nil))))
      (and (stringp raw) (> (length raw) 0)
           (emacs-callproc--split-on-nul raw)))))

(defun emacs-callproc-populate-process-environment ()
  "Seed `process-environment' from the OS env when it is empty.
No-op when `process-environment' already has entries (= host Emacs) or
no source is available.  Returns the (possibly updated) value."
  (when (null process-environment)
    (let ((env (emacs-callproc--read-proc-environ)))
      (when env
        (setq process-environment env))))
  process-environment)

;; Seed at load time: harmless on host, effective on the standalone reader.
(emacs-callproc-populate-process-environment)


(provide 'emacs-callproc)

;;; emacs-callproc.el ends here
