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
;; FIXED (segment C1, then segment D2): `nelisp-json-parse-string' used
;; to return STRING keys for `:object-type 'alist' (`(("a" . 1))'), a
;; documented divergence from real Emacs's native `json-parse-string',
;; which interns SYMBOL keys (`((a . 1))') -- see memory
;; `feedback_nelisp_json_parse_string_keys_are_strings'.  Segment C1
;; papered over this here, in a thin wrapper
;; (`standalone-compat-json--parse') around the then-unfixed
;; `nelisp-json-parse-string' call, by interning the alist keys itself
;; after the fact.  Segment D2 fixed `nelisp-json-parse-string' itself
;; (`packages/nelisp-json/src/nelisp-json.el') to return interned symbol
;; keys for `:object-type 'alist' directly, recursively, including
;; through nested objects and JSON arrays -- matching real Emacs, which
;; gives no way to ask `json-parse-string' for anything OTHER than
;; symbol keys when `:object-type' is `alist'.  The C1 workaround wrapper
;; is therefore gone from this file: `json-parse-string'/`json-parse-
;; buffer' below now call `nelisp-json-parse-string' directly with no
;; rekeying step for the modern API.  The default `hash-table' object
;; type is untouched (already string-keyed, matching Emacs) and `plist'
;; is untouched (already keyword-keyed, matching Emacs); verified against
;; Emacs 31.1.
;;
;; The `standalone-compat-json--rekey'/`--key-as'/`--resolve-key-type'
;; machinery below is NOT a workaround and stays: it implements the old
;; `json-read'/`json-read-from-string' API's own `json-key-type' feature
;; (a real Emacs behavior, not a nelisp-json gap -- `json-key-type' lets
;; a caller override the object-type-implied key representation, e.g.
;; ask for string keys while `json-object-type' is `alist') and its
;; `json-object-type' dynamic-variable dispatch (`alist'/`plist'/
;; `hash-table', chosen at call time, unlike the new API's `:object-type'
;; keyword argument).  That is this compat file's own responsibility to
;; provide, independent of whatever `nelisp-json-parse-string' returns
;; for a fixed `:object-type'.
;;
;; `json-read'/`json-read-from-string' (old API) go through the same
;; `standalone-compat-json--rekey' machinery, but via the dynamic
;; `json-object-type'/`json-key-type' variables real Emacs's old API
;; actually reads (measured against Emacs 31.1: `json-object-type' lets
;; a caller ask for `alist'/`plist'/`hash-table', and `json-key-type',
;; when non-nil, OVERRIDES the object-type-implied default key
;; representation for any of the three) -- previously this file always
;; parsed with a hardcoded `:object-type 'alist' regardless of the
;; current `json-object-type' binding, so `(let ((json-object-type
;; 'hash-table)) (json-read-from-string ...))' silently still produced
;; an alist.  `json-array-type' is intentionally left at real Emacs's
;; own default (`vector'), matching this file's pre-existing scope
;; limit; overriding it to `list' is not done (see the note on
;; `standalone-compat-json--rekey' below for why that would need
;; shape-based, not object-type-based, dispatch).
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

(defun standalone-compat-json--key-as (key target)
  "Convert a JSON object KEY -- a string, a plain symbol, or a keyword,
whatever `nelisp-json-parse-string' happened to hand back for the
object-type it was asked for -- to TARGET (`string', `symbol', or
`keyword').  Idempotent: converting a key already in TARGET's form
(e.g. a keyword key run through `keyword' again) is a no-op."
  (let ((name (cond ((stringp key) key)
                     ((keywordp key) (substring (symbol-name key) 1))
                     ((symbolp key) (symbol-name key))
                     (t (format "%s" key)))))
    (pcase target
      ('string name)
      ('symbol (intern name))
      ('keyword (intern (concat ":" name)))
      (_ key))))

(defun standalone-compat-json--rekey (value object-type target)
  "Recursively convert every JSON object key inside VALUE, which was
parsed with `:object-type' OBJECT-TYPE (`alist', `plist', or
`hash-table'; JSON arrays are always vectors -- this file never asks
`nelisp-json-parse-string' for `:array-type (quote list)'), to TARGET
(`string', `symbol', or `keyword'), matching real Emacs's
`json-key-type' semantics.  OBJECT-TYPE disambiguates an object from a
JSON array with no shape-guessing: with `:object-type (quote alist)' a
cons is always an object member list (arrays are vectors), and with
`:object-type (quote plist)' a cons is always a flat KEY VALUE KEY
VALUE... list -- there is no case where the parsed shape is ambiguous
for a fixed OBJECT-TYPE.  Leaves scalars (and, for a given OBJECT-TYPE,
values that are not that shape) alone."
  (cond
   ((vectorp value)
    (let ((out (copy-sequence value)) (i 0) (n (length value)))
      (while (< i n)
        (aset out i (standalone-compat-json--rekey
                     (aref value i) object-type target))
        (setq i (1+ i)))
      out))
   ((eq object-type 'hash-table)
    (if (hash-table-p value)
        (let ((out (make-hash-table :test 'equal)))
          (maphash (lambda (k v)
                     (puthash (standalone-compat-json--key-as k target)
                              (standalone-compat-json--rekey
                               v object-type target)
                              out))
                   value)
          out)
      value))
   ((eq object-type 'plist)
    (if (consp value)
        (let (out (rest value))
          (while rest
            (push (standalone-compat-json--key-as (car rest) target) out)
            (push (standalone-compat-json--rekey
                   (cadr rest) object-type target)
                  out)
            (setq rest (cddr rest)))
          (nreverse out))
      value))
   ((eq object-type 'alist)
    (if (consp value)
        (mapcar (lambda (pair)
                  (cons (standalone-compat-json--key-as (car pair) target)
                        (standalone-compat-json--rekey
                         (cdr pair) object-type target)))
                value)
      value))
   (t value)))

(defun standalone-compat-json--resolve-key-type (object-type key-type)
  "Real Emacs's `json-key-type' default-guessing rule (its own
docstring, verified against Emacs 31.1): an explicit KEY-TYPE wins;
otherwise `hash-table' -> `string', `alist' -> `symbol', `plist' ->
`keyword'."
  (or key-type
      (pcase object-type
        ('hash-table 'string)
        ('alist 'symbol)
        ('plist 'keyword)
        (_ 'string))))

(defun standalone-compat-json--read-one (args)
  "Parse one JSON value starting at point in the current buffer and
advance point past it, leaving any further buffer content unread --
`json-parse-buffer'/`json-read' semantics.  ARGS are
`nelisp-json-parse-string' keyword arguments -- passed straight through,
with no rekeying step, since `nelisp-json-parse-string' now returns the
correct key representation for every `:object-type' itself (see the
Commentary above).  See the trailing-whitespace caveat in the
Commentary above."
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
`:object-type (quote alist)' returns SYMBOL keys, matching real Emacs --
see the Commentary above.  A thin wrapper around
`nelisp-json-parse-string', which already returns the correct key
representation for every `:object-type'; this only translates the
error condition."
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

(defun standalone-compat-json--old-api-args ()
  "Build `nelisp-json-parse-string' keyword arguments for the classic
`json-read'/`json-read-from-string' API from the CURRENT dynamic
bindings of `json-object-type'/`json-null'/`json-false' -- real Emacs's
old API reads all three dynamically (verified against Emacs 31.1:
let-binding any of them changes what a `json-read-from-string' call in
its extent produces), where this file previously hardcoded
`:object-type 'alist' regardless of the live `json-object-type'
binding.  `:array-type' stays `array' (`json-array-type''s own real-
Emacs default is `vector'), matching this file's pre-existing scope
limit; see `standalone-compat-json--rekey''s docstring for why
`json-array-type' set to `list' is not handled.  `json-key-type' is
applied afterwards, as a separate rekey pass (see
`standalone-compat-json--old-api-read'), not folded in here."
  (list :object-type json-object-type
        :array-type 'array
        :null-object json-null
        :false-object json-false))

(defun standalone-compat-json--old-api-read (parsed-value)
  "Apply the current `json-key-type' (or its per-`json-object-type'
default, via `standalone-compat-json--resolve-key-type') to
PARSED-VALUE, which was parsed with the current `json-object-type'.
Safe to run on an `alist' result even though `nelisp-json-parse-string'
already returns symbol keys for it (the common case, `json-object-type'
defaulting to `alist'): `standalone-compat-json--key-as' accepts a key
already in symbol form and converts it again, a no-op unless
`json-key-type' asks for something else."
  (standalone-compat-json--rekey
   parsed-value json-object-type
   (standalone-compat-json--resolve-key-type json-object-type json-key-type)))

(unless (fboundp 'json-read-from-string)
  (defun json-read-from-string (string)
    "Old `json.el' API: parse STRING using the current
`json-object-type'/`json-array-type'/`json-null'/`json-false'/
`json-key-type' bindings (defaults: alist/vector/nil/`:json-false'/nil),
matching real Emacs's dynamic-variable-driven behavior -- see
`standalone-compat-json--old-api-args'."
    (standalone-compat-json--old-api-read
     (condition-case err
         (apply #'nelisp-json-parse-string string
                (standalone-compat-json--old-api-args))
       (nelisp-json-parse-error (standalone-compat-json--translate-error err))))))

(unless (fboundp 'json-read)
  (defun json-read ()
    "Old `json.el' API: parse one JSON value at point in the current
buffer using the current dynamic-variable bindings; see
`json-read-from-string'."
    (standalone-compat-json--old-api-read
     (standalone-compat-json--read-one
      (standalone-compat-json--old-api-args)))))

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
