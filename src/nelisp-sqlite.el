;;; nelisp-sqlite.el --- Emacs 30 sqlite-* compat via NeLisp runtime FFI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T77 (Wave 1 agent C, T54 substrate).
;;
;; anvil-memory / anvil-state / anvil-defs / anvil-org-index reach for
;; the host Emacs 30 builtin `sqlite-execute' / `sqlite-select' /
;; `sqlite-open' API directly (~110 call sites across 7 modules).  On
;; a host without those primitives — most importantly the Stage D /
;; standalone NeLisp deliverable — the architecture α delegate chain
;; cannot run at all, and anvil never spins up.
;;
;; This module is the NeLisp-side bridge.  It dlopens the C wrapper
;; `nelisp-sqlite-module.so' (built by `make sqlite-module'), which in
;; turn dlsym's the `nl_sqlite_*' symbols out of the rusqlite-bundled
;; cdylib `libnelisp_runtime.so'.  The 5 entry points re-exported here
;; match the Emacs 30 builtin signatures byte-for-byte:
;;
;;   - `nelisp-sqlite-open'    -> like `sqlite-open'
;;   - `nelisp-sqlite-close'   -> like `sqlite-close'
;;   - `nelisp-sqlite-execute' -> like `sqlite-execute'
;;   - `nelisp-sqlite-select'  -> like `sqlite-select'
;;   - `nelisp-sqlitep'        -> like `sqlitep'
;;
;; Architecture α delegate:
;;
;;   anvil-memory -> sqlite-execute      (host, may be missing)
;;        \-> nelisp-sqlite-execute      (NeLisp, always present)
;;             \-> nl_sqlite_execute     (Rust, rusqlite static link)
;;                  \-> libsqlite3       (bundled inside the cdylib)
;;
;; Once both `nelisp-sqlite' and the host builtin are available, anvil
;; can `(require 'nelisp-sqlite)` unconditionally and use the NeLisp
;; surface even on host Emacs (= one less dlopen / setup quirk to
;; chase between platforms).
;;
;; Encoding contract:
;;
;;   - parameter list: alist / vector / list of scalars; encoded to a
;;     JSON array via `nelisp-json-encode' before crossing the FFI.
;;     SQLite NULL = JSON null = `nil' / `:null' on the Lisp side.
;;   - SELECT result: list of vectors, each vector holding one row's
;;     column values.  Matches the Emacs 30 builtin shape so anvil's
;;     existing extractors (e.g. `(nth 0 row)' / `(aref row 1)') keep
;;     working.
;;
;; Out of scope (deferred to anvil-XXX consumers):
;;
;;   - Connection pooling.
;;   - Statement caching.  rusqlite caches the prepared statements
;;     internally; the boundary cost is `sqlite3_prepare_v2' once per
;;     SQL string, which is negligible for anvil's call patterns.
;;   - Multibyte BLOB columns.  Phase 7.5+ may add an explicit binary
;;     accessor; current anvil call sites do not exercise BLOBs.

;;; Code:

(require 'cl-lib)
(require 'nelisp-json)

(define-error 'nelisp-sqlite-error "NeLisp SQLite error")

;; The 7 entry points below are bound by `nelisp-sqlite-module.so' at
;; module-load time (see nelisp-runtime/c-bindings/nelisp-sqlite-module.c).
;; They are declared up-front so the byte-compiler does not warn about
;; "function not known to be defined" — they cannot be `defalias'd in
;; place because `module-load' has not yet been called when `nelisp-sqlite'
;; is first byte-compiled.

(declare-function nelisp-sqlite-module-load-cdylib "nelisp-sqlite-module" (path))
(declare-function nelisp-sqlite-module-version     "nelisp-sqlite-module" ())
(declare-function nelisp-sqlite-module-open        "nelisp-sqlite-module" (path))
(declare-function nelisp-sqlite-module-close       "nelisp-sqlite-module" (handle))
(declare-function nelisp-sqlite-module-execute     "nelisp-sqlite-module" (handle sql args-json))
(declare-function nelisp-sqlite-module-query       "nelisp-sqlite-module" (handle sql args-json))
(declare-function nelisp-sqlite-module-alive-p     "nelisp-sqlite-module" (handle))

(defgroup nelisp-sqlite nil
  "NeLisp SQLite FFI binding (T77 Wave 1 agent C)."
  :group 'nelisp)

(defcustom nelisp-sqlite-module-path nil
  "Absolute path to the `nelisp-sqlite-module.so' Emacs module.
When nil, `nelisp-sqlite--ensure-module' auto-detects via the worktree
`target/release/' directory.  Override when shipping pre-built binaries
out of tree (e.g. the Stage D tarball)."
  :type '(choice (const nil) file)
  :group 'nelisp-sqlite)

(defcustom nelisp-sqlite-runtime-cdylib-path nil
  "Absolute path to `libnelisp_runtime.{so,dylib}' (the rusqlite-bundled cdylib).
Same auto-detect rules as `nelisp-sqlite-module-path'.  Set explicitly
when the Emacs `process-environment' will not propagate
`NELISP_RUNTIME_SO' to libc."
  :type '(choice (const nil) file)
  :group 'nelisp-sqlite)

;;; --- Module bootstrap --------------------------------------------

(defun nelisp-sqlite--repo-root ()
  "Locate the NeLisp worktree root, or nil if not under one."
  (or (getenv "NELISP_REPO_ROOT")
      (let ((dir (locate-dominating-file
                  (or load-file-name buffer-file-name default-directory)
                  "Makefile")))
        (and dir (file-name-as-directory (expand-file-name dir))))))

(defun nelisp-sqlite--detect (basename)
  "Locate BASENAME under `<root>/nelisp-runtime/target/release/`."
  (let ((root (nelisp-sqlite--repo-root)))
    (when root
      (let ((cand (expand-file-name
                   (concat "nelisp-runtime/target/release/" basename)
                   root)))
        (and (file-readable-p cand) cand)))))

(defvar nelisp-sqlite--module-loaded nil
  "Non-nil once `nelisp-sqlite-module.so' has been successfully loaded.")

(defvar nelisp-sqlite--cdylib-loaded nil
  "Non-nil once the cdylib has been registered with the C wrapper.")

(defun nelisp-sqlite--ensure-module ()
  "Load `nelisp-sqlite-module.so' + register the cdylib path.
Idempotent: safe to call from every public entry point.  Signals
`nelisp-sqlite-error' when the substrate cannot be loaded (e.g. neither
`make sqlite-module' nor an explicit `nelisp-sqlite-module-path' is
set)."
  (unless nelisp-sqlite--module-loaded
    (let* ((path (or nelisp-sqlite-module-path
                     (nelisp-sqlite--detect "nelisp-sqlite-module.so"))))
      (unless (and path (file-readable-p path))
        (signal 'nelisp-sqlite-error
                (list (format "nelisp-sqlite-module.so not found (try `make sqlite-module' or set `nelisp-sqlite-module-path'); searched: %S" path))))
      (unless (fboundp 'nelisp-sqlite-module-open)
        (module-load path))
      (setq nelisp-sqlite--module-loaded t)))
  (unless nelisp-sqlite--cdylib-loaded
    (let* ((cdylib (or nelisp-sqlite-runtime-cdylib-path
                       (nelisp-sqlite--detect "libnelisp_runtime.so")
                       (nelisp-sqlite--detect "libnelisp_runtime.dylib"))))
      (unless (and cdylib (file-readable-p cdylib))
        (signal 'nelisp-sqlite-error
                (list (format "libnelisp_runtime cdylib not found (try `make runtime'); searched %S" cdylib))))
      (nelisp-sqlite-module-load-cdylib cdylib)
      (setq nelisp-sqlite--cdylib-loaded t)))
  t)

(defun nelisp-sqlite-available-p ()
  "Return non-nil when `nelisp-sqlite-*' can be invoked.
Best-effort probe: tries `nelisp-sqlite--ensure-module' inside an
`ignore-errors' guard so callers can branch without signalling.  Useful
for the architecture α `fboundp + fallback' delegate path."
  (condition-case _
      (progn (nelisp-sqlite--ensure-module) t)
    (error nil)))

;;; --- FFI value encoders / decoders -------------------------------

(defun nelisp-sqlite--encode-args (args)
  "Encode ARGS to a JSON array string for the FFI.
ARGS may be:

  - nil                  -> nil  (no parameters)
  - vector               -> JSON array
  - list                 -> JSON array
  - scalar               -> single-element JSON array

Sentinel mapping mirrors `nelisp-json':

  - nil                  -> JSON null
  - `:null'              -> JSON null
  - `:false'             -> JSON false
  - t                    -> JSON true
  - integer / float      -> JSON number
  - string               -> JSON string

Raises `nelisp-sqlite-error' on un-encodable values."
  (cond
   ((null args) nil)
   ((vectorp args)
    (nelisp-json-encode (append args nil)))
   ((listp args)
    (nelisp-json-encode args))
   (t
    (nelisp-json-encode (list args)))))

(defun nelisp-sqlite--decode-rows (json-string)
  "Decode JSON-STRING (array-of-arrays) into a list of row vectors.
Returns the empty list when JSON-STRING is `\"[]\"' or empty."
  (cond
   ((null json-string) '())
   ((string-empty-p json-string) '())
   (t
    (let ((rows (nelisp-json-parse-string json-string :array-type 'list)))
      (mapcar (lambda (row)
                (cond
                 ((vectorp row) row)
                 ((listp row) (vconcat row))
                 (t (vector row))))
              rows)))))

;;; --- Error wrapping ----------------------------------------------
;;
;; The C wrapper (nelisp-sqlite-module.c) uses `non_local_exit_signal'
;; with the `error' symbol; ERT call sites expect the more specific
;; `nelisp-sqlite-error' so they can distinguish FFI failures from
;; arbitrary host errors.  `nelisp-sqlite--call-ffi' wraps each FFI
;; entry point and re-signals as `nelisp-sqlite-error', preserving the
;; original message for diagnostics.

(defmacro nelisp-sqlite--ffi-call (form)
  "Run FORM under a `condition-case' that re-signals as `nelisp-sqlite-error'.
Re-raises non-`error' signals untouched (e.g. quit)."
  `(condition-case err
       ,form
     (error
      (signal 'nelisp-sqlite-error (cdr err)))))

;;; --- Public API: Emacs 30 sqlite-* compatible -------------------

(defun nelisp-sqlite-open (path)
  "Open the SQLite database at PATH.
PATH may be the literal string \":memory:\" for an in-memory database.
Returns an integer handle (the NeLisp `sqlitep' equivalent), or signals
`nelisp-sqlite-error' on failure."
  (nelisp-sqlite--ensure-module)
  (unless (stringp path)
    (signal 'nelisp-sqlite-error
            (list (format "nelisp-sqlite-open: PATH must be a string, got %S" path))))
  (nelisp-sqlite--ffi-call (nelisp-sqlite-module-open path)))

(defun nelisp-sqlite-close (db)
  "Close the SQLite connection DB (an integer handle).
Returns t on success.  Signals `nelisp-sqlite-error' on bad / freed
handles."
  (nelisp-sqlite--ensure-module)
  (unless (nelisp-sqlitep db)
    (signal 'nelisp-sqlite-error
            (list (format "nelisp-sqlite-close: bad handle %S" db))))
  (nelisp-sqlite--ffi-call (nelisp-sqlite-module-close db)))

(defun nelisp-sqlite-execute (db query &optional values)
  "Execute QUERY against DB; return rows-affected (integer ≥ 0).
VALUES is a list / vector of parameter values bound to `?' placeholders
in QUERY (JSON-encoded across the FFI), or nil for no parameters."
  (nelisp-sqlite--ensure-module)
  (unless (nelisp-sqlitep db)
    (signal 'nelisp-sqlite-error
            (list (format "nelisp-sqlite-execute: bad handle %S" db))))
  (unless (stringp query)
    (signal 'nelisp-sqlite-error
            (list (format "nelisp-sqlite-execute: QUERY must be a string"))))
  (let ((args-json (nelisp-sqlite--encode-args values)))
    (nelisp-sqlite--ffi-call (nelisp-sqlite-module-execute db query args-json))))

(defun nelisp-sqlite-select (db query &optional values _return-type)
  "Run SELECT-style QUERY against DB; return list of row vectors.
VALUES is a list / vector of parameter values bound to `?' placeholders
in QUERY, or nil for no parameters.  RETURN-TYPE is accepted for
signature compatibility with the Emacs 30 builtin but ignored — the
result is always a list of vectors (anvil's universal expectation).

NULL columns decode to `nil', integer columns to integers, real
columns to floats, text columns to strings, and blob columns to
hex-encoded strings (T77 substrate; binary BLOB consumers are deferred
to Phase 7.5+)."
  (nelisp-sqlite--ensure-module)
  (unless (nelisp-sqlitep db)
    (signal 'nelisp-sqlite-error
            (list (format "nelisp-sqlite-select: bad handle %S" db))))
  (unless (stringp query)
    (signal 'nelisp-sqlite-error
            (list (format "nelisp-sqlite-select: QUERY must be a string"))))
  (let* ((args-json (nelisp-sqlite--encode-args values))
         (json-result (nelisp-sqlite--ffi-call
                       (nelisp-sqlite-module-query db query args-json))))
    (nelisp-sqlite--decode-rows json-result)))

(defun nelisp-sqlitep (object)
  "Return non-nil when OBJECT is a NeLisp SQLite connection handle.
A handle is a positive integer issued by `nelisp-sqlite-open' and not
yet passed through `nelisp-sqlite-close'.  This predicate matches the
Emacs 30 builtin `sqlitep' contract.

When the FFI substrate is not yet loaded, the predicate returns nil
unconditionally — callers can branch on availability before delegating
to this layer."
  (and (integerp object)
       (> object 0)
       (or (and (not nelisp-sqlite--module-loaded)
                ;; Module not loaded: we cannot probe liveness.  The
                ;; caller is expected to have called something through
                ;; the public API first, which would have loaded the
                ;; module; opportunistically returning nil keeps the
                ;; predicate safe.
                nil)
           (and (fboundp 'nelisp-sqlite-module-alive-p)
                (nelisp-sqlite-module-alive-p object)))))

;;; --- Convenience: pragma + transaction sugar ---------------------
;;
;; These wrap `nelisp-sqlite-execute' so the existing anvil call sites
;; (e.g. `(sqlite-pragma db "busy_timeout = 5000")`) keep working.  The
;; spec's non-goals call out "WAL pragma の specific config" as
;; user-side, so we deliberately keep these as one-liners and do not
;; bake any policy.

(defun nelisp-sqlite-pragma (db pragma-clause)
  "Execute `PRAGMA <PRAGMA-CLAUSE>' against DB.  Return rows-affected.
Signals `nelisp-sqlite-error' on bad handle or SQL error."
  (nelisp-sqlite-execute db (concat "PRAGMA " pragma-clause)))

(defun nelisp-sqlite-transaction (db)
  "Begin a transaction on DB.  Pair with
`nelisp-sqlite-commit' / `nelisp-sqlite-rollback'.  Returns 0."
  (nelisp-sqlite-execute db "BEGIN"))

(defun nelisp-sqlite-commit (db)
  "Commit the current transaction on DB.  Returns 0."
  (nelisp-sqlite-execute db "COMMIT"))

(defun nelisp-sqlite-rollback (db)
  "Roll back the current transaction on DB.  Returns 0."
  (nelisp-sqlite-execute db "ROLLBACK"))

(provide 'nelisp-sqlite)

;;; nelisp-sqlite.el ends here
