;;; json.el --- minimal json.el for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-compat: `(require 'json)' dies `file-missing:
;; json' on the standalone binary -- there is no `json' anywhere on its
;; default `load-path'.  `json-parse-string', `json-parse-buffer',
;; `json-encode', `json-read-from-string', `json-read' and
;; `json-available-p' were not defined at all before this file, which is
;; what the nelisp-agent host-only test corpus's 6 `json-parse-string' /
;; 3 `json-encode' / 2 `json-parse-buffer' / 2 `json-serialize' call
;; sites hit.  This directory is appended LAST to the standalone's
;; default `load-path' (any native/prelude definition always wins) and
;; is NOT on any host `-L' path `make test' uses (`make -n test | grep
;; -c standalone-compat' is 0), so this never shadows real Emacs's own
;; `json' for the host suite.
;;
;; `json-serialize' is the one exception to "native/prelude always
;; wins": `scripts/nelisp-stdlib-prelude.el' already defines it
;; unconditionally at boot (a hand-rolled encoder written for
;; anvil-pkg-state's JSON shape, predating this file), so it is already
;; `fboundp' by the time anything here loads.  That prelude definition
;; never learned `:null-object'/`:false-object' -- it hardcodes exactly
;; `:null'/`:json-false' as null/false and ignores its own `_keys' rest
;; argument entirely (never threading it into its recursive
;; self-calls), so any other sentinel it is asked to serialize, at any
;; depth, signals `(wrong-type-argument json-value-p SENTINEL)'.
;; Measured 2026-09-20: `(json-serialize :json-null :null-object
;; :json-null :false-object :json-false)' on the standalone signals
;; exactly that, where real Emacs 31.1 returns "null".  Below,
;; `json-serialize' is therefore defined UNCONDITIONALLY (no `unless
;; (fboundp ...)' guard, unlike every other function in this file),
;; overriding the prelude's definition once `(require 'json)' has run,
;; and delegates to `nelisp-json-serialize' (`packages/nelisp-json/src/
;; nelisp-json.el'), which now threads `:null-object'/`:false-object'
;; through every encoder (array/vector/hash-table/alist/plist) at any
;; depth, matching real Emacs's `json-serialize' keyword semantics.
;; This also incidentally fixes the nil-valued-hash-entry abort the
;; prelude comment describes: `nelisp-json-encode' already encodes a
;; nil hash value as JSON null instead of erroring.  Callers that never
;; require `json' still get the prelude's original, narrower
;; definition -- unchanged and out of this segment's scope (edits to
;; `scripts/nelisp-stdlib-prelude.el' are forbidden here).
;;
;; Built on `packages/nelisp-json/src/nelisp-json.el', which already
;; implements a pure-Lisp JSON parser/encoder with the SAME keyword names
;; (`:object-type'/`:array-type'/`:null-object'/`:false-object') the real
;; `json-parse-string'/`json-parse-buffer' use, and the same defaults
;; (hash-table with STRING keys, vector, `:null', `:false') -- see its
;; own header.  This file is therefore almost entirely thin wrappers.
;;
;; Known, pre-existing, deliberate divergence (not introduced here, and
;; out of this segment's scope to fix -- see memory
;; `feedback_nelisp_json_parse_string_keys_are_strings'): with
;; `:object-type 'alist', `nelisp-json-parse-string' returns STRING keys
;; (`(("a" . 1))'), where real Emacs's native `json-parse-string' interns
;; SYMBOL keys (`((a . 1))').  The default `hash-table' object type is
;; NOT affected -- both use string keys there, verified against Emacs
;; 31.1 in the differential this segment's report includes.  `json-read'/
;; `json-read-from-string' below (old json.el API, object-type defaults
;; to alist) work around this one case with an explicit string->symbol
;; rekey since that old API has no caller-visible `:object-type' to leave
;; as-is; `json-parse-string'/`json-parse-buffer' (new API) pass
;; `:object-type 'alist' straight through when a caller asks for it, STILL
;; STRING-KEYED, because silently changing what an explicit caller
;; argument produces would be a worse surprise than documenting the gap.
;;
;; `json-parse-buffer'/`json-read' (buffer-position variants) have a
;; smaller, also-documented gap: `nelisp-json-parse-string' only accepts
;; a complete string and only reports "where trailing data starts" AFTER
;; skipping trailing whitespace, while real Emacs's C parser stops
;; immediately after the value, before any trailing whitespace.  Both
;; land at the same place once a caller does its own
;; `skip-chars-forward' + `(point) = (point-max)' trailing-garbage check
;; (verified against the two real call sites in
;; ../nelisp-agent/lisp/nl-agent-mcp-config.el and
;; ../nelisp-agent/lisp/nl-agent-recurrent-config.el, the latter of which
;; does exactly that check) -- only a caller that inspects `point'
;; immediately after the call, before doing its own whitespace skip,
;; would see a different (further-advanced) position than real Emacs.
;; Reaching into `nelisp-json''s own `--'-private single-value parser to
;; get an exact match would cross a namespace boundary `ns-inventory'
;; tracks (`ns-private-escape', already at its 767 baseline) for a
;; distinction no real caller here observes; not done.

;;; Code:

(require 'nelisp-json)

(unless (get 'json-error 'error-conditions)
  (define-error 'json-error "JSON error"))
(unless (get 'json-parse-error 'error-conditions)
  (define-error 'json-parse-error "JSON parse error" 'json-error))

;; json.el's classic configuration variables, with Emacs 31.1's defaults
;; (measured: json-false :json-false, json-null nil, json-object-type alist,
;; json-array-type vector, json-key-type nil).  Old-style callers read them
;; as free variables -- ../nelisp-agent/test/mcp-modern-server-fixture.el:83
;; builds `("isError" . ,json-false)' -- so they must be bound, not only
;; accepted as keywords by the parse/serialize wrappers above.  Defined
;; here, before any function below reads them, so `json-encode' (further
;; down) does not reference them as free variables at byte-compile time.
(defvar json-false :json-false)
(defvar json-null nil)
(defvar json-object-type 'alist)
(defvar json-array-type 'vector)
(defvar json-key-type nil)

(defun standalone-compat-json--translate-error (err)
  "Re-signal ERR (a caught `nelisp-json-parse-error' condition object,
i.e. `(nelisp-json-parse-error MESSAGE JSON-STRING POS)') as the
standard `json-parse-error' real Emacs callers would expect to catch,
keeping the same message/data."
  (signal 'json-parse-error (cdr err)))

(defun standalone-compat-json--read-one (args)
  "Parse one JSON value starting at point in the current buffer and
advance point past it, leaving any further buffer content unread --
`json-parse-buffer'/`json-read' semantics.  ARGS are
`nelisp-json-parse-string' keyword arguments.  See the trailing-
whitespace caveat in the Commentary above."
  ;; `buffer-substring' (not `-no-properties'): measured 2026-09-19,
  ;; `buffer-substring-no-properties' on the standalone binary always
  ;; returns "" regardless of BEG/END (a pre-existing runtime gap,
  ;; unrelated to ert/json/subr-x/`load-file-name'/`generate-new-buffer-
  ;; name' and out of this segment's scope to fix -- `buffer-substring'
  ;; itself works correctly and text properties are irrelevant to a
  ;; string handed straight to `nelisp-json-parse-string').
  (let ((text (buffer-substring (point) (point-max)))
        (start (point)))
    (condition-case outer-err
        (prog1 (apply #'nelisp-json-parse-string text args)
          (goto-char (point-max)))
      (nelisp-json-parse-error
       (let ((pos (nth 3 outer-err)))
         (unless (integerp pos)
           (standalone-compat-json--translate-error outer-err))
         (condition-case nil
             (prog1 (apply #'nelisp-json-parse-string (substring text 0 pos) args)
               (goto-char (+ start pos)))
           (error (standalone-compat-json--translate-error outer-err))))))))

(unless (fboundp 'json-parse-string)
  (defun json-parse-string (string &rest args)
    "Parse STRING as JSON.  ARGS: `:object-type'/`:array-type'/
`:null-object'/`:false-object', same names/defaults as real Emacs.
See the Commentary above for the one known keyed-object divergence."
    (condition-case err
        (apply #'nelisp-json-parse-string string args)
      (nelisp-json-parse-error (standalone-compat-json--translate-error err)))))

(unless (fboundp 'json-parse-buffer)
  (defun json-parse-buffer (&rest args)
    "Parse one JSON value at point in the current buffer.  ARGS as
`json-parse-string'.  See the Commentary above for the buffer-position
caveat."
    (standalone-compat-json--read-one args)))

;; Deliberately unconditional -- see the Commentary above.  This is the
;; only definition in this file that overrides an already-`fboundp'
;; standalone symbol rather than deferring to it.
(defun json-serialize (object &rest args)
  "Serialize OBJECT to a JSON string.  ARGS: `:null-object'/
`:false-object', same names/defaults as real Emacs.  Overrides the
prelude's own limited `json-serialize' (see Commentary above) once
`json' has been required."
  (apply #'nelisp-json-serialize object args))

(defconst standalone-compat-json--old-api-args
  (list :object-type 'alist :array-type 'array
        :null-object nil :false-object :json-false)
  "Defaults old `json.el' (`json-read'/`json-read-from-string') used:
object = alist, array = vector, JSON null = nil, JSON false = `:json-false'.
Matches real Emacs's own historical defaults for these two functions,
which is a different default set than `json-parse-string''s.")

(defun standalone-compat-json--alist-keys->symbols (value)
  "Recursively re-key an alist-shaped VALUE from string keys (what
`nelisp-json-parse-string' produces for `:object-type (quote alist)', a
documented divergence -- see the Commentary above) to symbol keys,
matching old `json-read''s actual default `json-object-type' behavior.
VALUE's arrays are vectors (`:array-type 'array' above), so a Lisp list
here is unambiguously an alist, never an array."
  (cond
   ((vectorp value)
    (let ((out (copy-sequence value)) (i 0) (n (length value)))
      (while (< i n)
        (aset out i (standalone-compat-json--alist-keys->symbols (aref value i)))
        (setq i (1+ i)))
      out))
   ((and (consp value) (consp (car value)))
    (mapcar (lambda (pair)
              (cons (if (stringp (car pair)) (intern (car pair)) (car pair))
                    (standalone-compat-json--alist-keys->symbols (cdr pair))))
            value))
   (t value)))

(unless (fboundp 'json-read-from-string)
  (defun json-read-from-string (string)
    "Old `json.el' API: parse STRING with the historical alist/vector/
nil/`:json-false' defaults."
    (standalone-compat-json--alist-keys->symbols
     (condition-case err
         (apply #'nelisp-json-parse-string string standalone-compat-json--old-api-args)
       (nelisp-json-parse-error (standalone-compat-json--translate-error err))))))

(unless (fboundp 'json-read)
  (defun json-read ()
    "Old `json.el' API: parse one JSON value at point in the current
buffer with the historical alist/vector/nil/`:json-false' defaults."
    (standalone-compat-json--alist-keys->symbols
     (standalone-compat-json--read-one standalone-compat-json--old-api-args))))

(unless (fboundp 'json-encode)
  (defun json-encode (object)
    "Encode OBJECT as a JSON string.  Old `json.el' never accepted
keyword arguments to `json-encode' -- it read the package-global
`json-null'/`json-false' variables instead, and real Emacs's `json.el'
honors whatever they are currently bound to at encode time (verified
against Emacs 31.1: rebinding `json-null'/`json-false' changes what
`json-encode' treats as null/false).  Pass their current values through
to `nelisp-json-encode' as `:null-object'/`:false-object' so a caller
that lets-binds them sees the same behavior.  One caveat, out of this
segment's scope: `nelisp-json-encode' always treats bare `nil' as JSON
null (the documented divergence in `nelisp-json.el''s own commentary),
where real Emacs only does that while `json-null' is nil (its default);
rebinding `json-null' away from nil makes real Emacs encode `nil' as
`{}' instead, which this delegate does not reproduce."
    (nelisp-json-encode object
                        :null-object json-null
                        :false-object json-false)))

(unless (fboundp 'json-insert)
  (defun json-insert (object &rest _args)
    "Insert OBJECT, JSON-encoded, at point."
    (insert (nelisp-json-encode object))))

(unless (fboundp 'json-available-p)
  (defun json-available-p ()
    "Always t here: this file only loads when JSON support is present."
    t))

(provide 'json)

;;; json.el ends here
