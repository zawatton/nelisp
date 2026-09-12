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
    (nelisp-repl-gc-compare . "Compare two GC snapshots.")
    ;; Native unit replacement, Linux x86_64 opt-in build only.  Listed here
    ;; because `nelisp-repl-code-info' answers for Lisp definitions and says
    ;; nothing about a published native unit; someone who found this help
    ;; through the Lisp side would otherwise have no way to learn the native
    ;; side exists.  These names are `fboundp' only when
    ;; `nelisp-native-unit-development' has been required.
    (nelisp-native-unit-code-info
     . "Provenance of the native code a unit is running now.")
    (nelisp-native-unit-status
     . "A native unit's live generation, identity and export addresses.")
    (nelisp-native-unit-resources
     . "Account for retained native mappings and what was reclaimed.")
    (nelisp-native-unit-reclaim
     . "Release what is provably reclaimable; report refusals with reasons.")
    (nelisp-native-unit-rebuild-and-reload
     . "Recompile a native source and publish it, bounded and cancellable.")
    (nelisp-native-callsite-reachability
     . "Which callers a replacement of NAME would actually reach.")))

(provide 'nelisp-repl-development)

;;; nelisp-repl-development.el ends here
