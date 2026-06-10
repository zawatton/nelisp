;;; nemacs-server-loop.el --- standalone emacsclient server driver -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; M14 server/emacsclient lane — boot vendor server.el on the
;; standalone NeLisp reader and serve real `emacsclient -e EXPR'
;; round-trips over a local UNIX socket.
;;
;; The caller MUST set these before loading this file (the reader's
;; `getenv' is a stub that returns nil, so configuration travels in a
;; small generated preamble — same pattern as the anvil-runtime
;; daemon launcher):
;;
;;   (setq nemacs-server-loop-root "/abs/path/to/nelisp-emacs")
;;   (setq nemacs-server-loop-name "nemacs")            ; server-name
;;   (setq nemacs-server-loop-dir "/tmp/nemacs-server") ; socket dir
;;
;; Launch:  nelisp --eval '(load "PREAMBLE.el" nil t)'
;;          where the preamble ends with (load ".../nemacs-server-loop.el" nil t)
;;
;; CAUTION (reader semantics): a missing-function call hard-aborts the
;; enclosing top-level form and cannot be caught by `condition-case'.
;; The serve loop is therefore the LAST form of this file; an abort
;; ends the process rather than wedging it half-alive.

;;; Code:

(if (boundp 'nemacs-server-loop-root)
    nil
  (nelisp--write-stderr-line
   "nemacs-server-loop: set nemacs-server-loop-root before loading")
  (exit 2))

(load (concat nemacs-server-loop-root "/src/emacs-stub.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-stub-bulk.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-network-syscall-shim.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-network-ffi.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-process-events.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-eventloop.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-server-polyfills.el") nil t)
(load (concat nemacs-server-loop-root "/src/emacs-server-client-polyfills.el") nil t)
(load (concat nemacs-server-loop-root "/vendor/emacs-lisp/server.el") nil t)
(emacs-server-client-polyfills-install)

(setq server-name (if (boundp 'nemacs-server-loop-name)
                      nemacs-server-loop-name
                    "nemacs"))
(setq server-use-tcp nil)
(setq server-socket-dir (if (boundp 'nemacs-server-loop-dir)
                            nemacs-server-loop-dir
                          "/tmp/nemacs-server"))

;; M18: apply the user's wrapped init (load-path requires resolved to
;; absolute loads by scripts/nemacs-wrap-init.el) so emacsclient evals
;; see the user's packages.  The marker calls are satisfied by these
;; counters; the GUI bridge owns the report file, so the server only
;; tallies for its own log line.
(defvar nemacs-init--applied 0)
(defvar nemacs-init--last-load-path-dir nil)
(defun nemacs-init--begin (n _hint) n)
(defun nemacs-init--ok (n)
  (setq nemacs-init--applied (+ nemacs-init--applied 1))
  n)
(if (file-exists-p "/tmp/nemacs-init-wrapped")
    (progn
      (load "/tmp/nemacs-init-wrapped" nil t)
      (nelisp--write-stderr-line
       (concat "nemacs-server-loop: user init applied forms="
               (number-to-string nemacs-init--applied))))
  nil)

(nemacs-server-start)
(nelisp--write-stderr-line
 (concat "nemacs-server-loop: listening on " (server--file-name)))

(let ((alive t))
  (while alive
    (accept-process-output nil 0 200)))

;;; nemacs-server-loop.el ends here
