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

;; Loader reconcile Phase 3: apply the user's wrapped init (load-path
;; requires resolved to absolute loads by scripts/nemacs-wrap-init.el) so
;; emacsclient evals see the user's packages, through the shared
;; per-form marker state/consume orchestration in
;; nemacs-init-transport.el (src/nemacs-init-transport.el) instead of a
;; hand-rolled, incomplete duplicate of it (the previous defuns here
;; tallied `applied' but never defined `nemacs-init--note-file' /
;; `nemacs-init--file-loaded-p', so any wrapped form using a resolved
;; `require' would hard-abort under the CAUTION above).  The GUI bridge
;; owns the report file for its own transport dir; this loop only reads
;; `nemacs-init--applied' afterward for its own log line.
(load (concat nemacs-server-loop-root "/src/nemacs-init-transport.el") nil t)
(if (nemacs-init-transport-consume "/tmp/nemacs-init-wrapped" nil)
    (nelisp--write-stderr-line
     (concat "nemacs-server-loop: user init applied forms="
             (number-to-string nemacs-init--applied)))
  nil)

(nemacs-server-start)
(nelisp--write-stderr-line
 (concat "nemacs-server-loop: listening on " (server--file-name)))

(let ((alive t))
  (while alive
    (accept-process-output nil 0 200)))

;;; nemacs-server-loop.el ends here
