;;; nelisp-repl-development.el --- REPL development entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; One explicit entry point for the opt-in REPL development helpers.  The
;; individual modules own their APIs; this file only loads them and reports
;; the available commands, so it cannot create a require cycle.

;;; Code:

(require 'nelisp-repl-session)
(require 'nelisp-repl-code)
(require 'nelisp-repl-gc)

(defun nelisp-repl-help ()
  "Return the available REPL development commands and their purposes."
  '((nelisp-repl-session-call . "Call a function with failure recording.")
    (nelisp-repl-session-retry . "Explicitly retry one recorded failure.")
    (nelisp-repl-session-failures . "Read recorded failures.")
    (nelisp-repl-session-clear . "Release retained session history and arguments.")
    (nelisp-repl-session-record . "Register an explicit readable replay form.")
    (nelisp-repl-session-record-setting . "Register a setting for replay.")
    (nelisp-repl-session-record-load . "Register a source file for replay.")
    (nelisp-repl-session-export . "Export registered replay forms.")
    (nelisp-artifact-reload-source-file . "Reload a source definition in the REPL.")
    (nelisp-repl-code-info . "Inspect live code provenance.")
    (nelisp-repl-code-forget . "Release retained provenance records.")
    (nelisp-repl-gc-snapshot . "Inspect REPL GC development status.")
    (nelisp-repl-gc-collect . "Explicitly request a REPL GC collection.")
    (nelisp-repl-gc-compare . "Compare two GC snapshots.")))

(provide 'nelisp-repl-development)

;;; nelisp-repl-development.el ends here
