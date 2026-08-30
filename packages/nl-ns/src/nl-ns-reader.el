;;; nl-ns-reader.el --- Doc 189 Phase 0: reader-time namespace resolution -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 189 ("nl-ns, advisory to enforced") Phase 0 only.  This file does
;; not touch `nl-ns.el' or `nl-ns-in.el' -- it sits beside them, reusing
;; `nl-ns-define''s already-shipped `:members' / `:prefix' registry
;; (`nl-ns-member-list', `nl-ns-qualify', both public) to drive the new
;; opt-in reader hook `nelisp-read-namespace-resolve' added to
;; `src/nelisp-read.el' by the same change that added this file.
;;
;; What this buys over `nl-ns-in.el's existing macro-rewrite: reading
;; `wrap' inside an active resolution interns the qualified symbol
;; `text-wrap' directly -- the short name `wrap' is never interned at
;; all, so it never becomes an entry in the one shared intern table
;; that `nl-ns.el's own README calls "the flat obarray".  `nl-ns-in.el'
;; still reads `wrap' as an ordinary symbol first and rewrites the
;; already-read form afterward; this file's mechanism resolves before
;; that symbol is ever created.
;;
;; Opt-in, by construction: `nelisp-read-namespace-resolve' defaults to
;; nil everywhere.  The only place in this tree that ever binds it to
;; something else is `nl-ns-read-with-resolution' below.  No existing
;; reader entry point (`nelisp-read', `nelisp-read-from-string',
;; `nelisp-read-all', `nelisp-load', `nelisp-eval-buffer', the
;; standalone runtime's own `--load'/`--eval'/REPL paths) calls this
;; macro, so every one of them is completely unaffected by this file
;; existing -- this is Phase 0's "zero effect on unannotated code"
;; contract, satisfied by construction rather than by a claim to verify
;; later (see docs/design/189-nl-ns-enforced-namespaces.org §4 Phase 0
;; and §5).
;;
;; Public API:
;;
;;   `nl-ns-read-with-resolution' -- macro, dynamically bind + eval BODY
;;                                   (NAME unevaluated, like `nl-ns-in')
;;   `nl-ns-read-eval-in'         -- function, NAME evaluated + THUNK called
;;   `nl-ns-read-in'              -- resolution-aware `nelisp-read'
;;   `nl-ns-read-from-string-in'  -- resolution-aware `nelisp-read-from-string'
;;   `nl-ns-read-all-in'          -- resolution-aware `nelisp-read-all'
;;
;; A file or package opts in to Phase 0 by calling one of the three
;; `-in' entry points above (naming its own declared namespace) instead
;; of the plain `nelisp-read*' function -- "opt-in per file/package" per
;; the design doc's Phase 0 contract.  Wiring an opted-in file into the
;; production loader (`nelisp-load.el', `require') is explicitly out of
;; scope for Phase 0: see docs/design/189-nl-ns-enforced-namespaces.org
;; §5 "Reader entry-point coverage" for the accepted-gap accounting that
;; follows from this file being the only caller of the hook today.

;;; Code:

(require 'nl-prelude)
(require 'nl-ns-in)
(require 'nelisp-read)

(defun nl-ns-read--table (name)
  "Build a resolution table for namespace NAME.
Keys are NAME's declared member names as strings (the shape a reader
token arrives in, before it is ever a symbol); values are the already-
interned, namespace-qualified symbols `nl-ns-qualify' produces.  Errors
the same way `nl-ns-in' does when NAME was never declared: `nl-ns-
member-list' below is itself the validation (it signals via nl-ns-in's
private `nl-ns--entry', which this file -- deliberately, per Doc 189
§1 Extends -- never calls directly, only through nl-ns-in.el's public
surface)."
  (let* ((members (nl-ns-member-list name))
         (table (make-hash-table :test 'equal
                                  :size (max 1 (length members)))))
    (dolist (member members)
      (puthash (symbol-name member) (nl-ns-qualify name member) table))
    table))

(defun nl-ns-read-eval-in (name thunk)
  "Call THUNK with reader-time short-name resolution bound to NAME.
NAME is evaluated as an ordinary value (unlike `nl-ns-in', it is not
quoted syntax), so callers may pass a namespace chosen at run time --
e.g. a loader mapping a file path to a namespace.  THUNK is a function
of no arguments; its dynamic extent is exactly where resolution is
active.  This is the shared primitive behind `nl-ns-read-with-resolution'
and the `nl-ns-read*-in' wrappers below; nothing else in this tree
binds `nelisp-read-namespace-resolve' to a non-nil value."
  (let ((nelisp-read-namespace-resolve (nl-ns-read--table name)))
    (funcall thunk)))

(defmacro nl-ns-read-with-resolution (name &rest body)
  "Evaluate BODY with reader-time short-name resolution bound to NAME.
NAME is unevaluated, matching the calling convention `nl-ns-in' uses:
write `(nl-ns-read-with-resolution tns ...)', not a quoted NAME.

While BODY runs, `nelisp-read-namespace-resolve' -- consulted by
`nelisp-read--atom' before it interns a plain-identifier token -- maps
each of NAME's declared `:members' to its namespace-qualified symbol.
A token that is not a declared member of NAME reads exactly as it does
today.  Reading anything outside this form is unaffected."
  (declare (indent 1))
  `(nl-ns-read-eval-in ',name (lambda () ,@body)))

(defun nl-ns-read-in (name str)
  "Read one sexp from STR with namespace NAME's members resolved.
NAME is evaluated here; `nl-ns-read-with-resolution' instead quotes it.
Resolution-aware counterpart to `nelisp-read'."
  (nl-ns-read-eval-in name (lambda () (nelisp-read str))))

(defun nl-ns-read-from-string-in (name str &optional start)
  "Read one sexp from STR at START with NAME's members resolved.
NAME is evaluated here; `nl-ns-read-with-resolution' instead quotes it.
Resolution-aware counterpart to `nelisp-read-from-string'."
  (nl-ns-read-eval-in name (lambda () (nelisp-read-from-string str start))))

(defun nl-ns-read-all-in (name str)
  "Read every top-level sexp in STR with NAME's members resolved.
NAME is evaluated here; `nl-ns-read-with-resolution' instead quotes it.
Resolution-aware counterpart to `nelisp-read-all'."
  (nl-ns-read-eval-in name (lambda () (nelisp-read-all str))))

(provide 'nl-ns-reader)

;;; nl-ns-reader.el ends here
