;;; nelisp-defs-index.el --- SQLite index of elisp definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of NeLisp.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 6.5.1 (Doc 25 §3) — MVP port of `anvil-defs.el' (Doc 11
;; SHIPPED, 964 LOC) to NeLisp.  This MVP is the rebuild + search
;;縦串 (handoff §3.3, §1.1 必達条件 1-5).
;;
;; Persistent SQLite index of every top-level Elisp definition,
;; queried by name to answer "where is X defined?" in 1 MCP hop
;; rather than the 3-5-hop Grep + Read loop.
;;
;; Schema (DDL identical to anvil-defs schema v1; refs/features
;; tables are CREATE-only in MVP — populated in Phase 6.5.2 when
;; the deep walker lands):
;;
;;   schema_meta (version)
;;   file        (id, path, mtime, size, indexed_at)
;;   defs        (id, file_id, kind, name, line, end_line,
;;                arity_min, arity_max, docstring_head, obsolete_p)
;;   refs        (id, file_id, name, line, context, kind)
;;   features    (id, file_id, feature, kind in {'requires','provides'})
;;
;; The DB lives at `nelisp-defs-index-db-path' (default XDG cache,
;; Doc 25 §2.2 B).  Deleting it is safe — full rebuild is cheap
;; (sub-second on project scale).
;;
;; Scanner: `insert-file-contents' + `read' loop, no dependency on
;; `anvil-sexp'.  Top-level form heads in
;; `nelisp-defs-index--defining-forms' produce a `defs' row; every
;; form (defining or not) is then walked recursively to produce
;; `refs' (call / quote / symbol references) and `features'
;; (require / provide edges).  All references coming from one
;; top-level form share that form's start line — anvil-defs makes
;; the same simplification because `read' gives form positions, not
;; sub-form positions, without re-tokenising.
;;
;; Phase history:
;;   6.5.1 (Doc 25 §3) — top-level scanner only, refs/features empty
;;   6.5.2/6.5.3 (Doc 25 §3) — deep walker + refs/features INSERT,
;;     +4 public API (-references / -signature / -who-requires /
;;     -index-status)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; --- group / config -----------------------------------------------------

(defgroup nelisp-defs-index nil
  "SQLite-backed Elisp definition index for NeLisp."
  :group 'nelisp
  :prefix "nelisp-defs-index-")

(defvar nelisp-defs-index-db-path
  (let* ((xdg (or (getenv "XDG_CACHE_HOME")
                  (expand-file-name ".cache" (getenv "HOME"))))
         (dir (expand-file-name "nelisp" xdg)))
    (expand-file-name "nelisp-defs.db" dir))
  "Path to the SQLite database file backing `nelisp-defs-index'.
Defaults to XDG cache (Emacs 非依存)。Stage D launcher の
NELISP_CACHE_DIR で indirection 可能。
defvar (not defcustom) per Doc 25 §2.4 B.")

(defvar nelisp-defs-index-paths nil
  "List of project roots to index.
When nil, `nelisp-defs-index-rebuild' falls back to the git ancestor
of `default-directory'.  defvar per Doc 25 §2.4 B.")

(defvar nelisp-defs-index-exclude-patterns
  '("/\\.git/"
    "/\\.claude/"
    "/node_modules/"
    "/tests/fixtures/"
    "/worktrees/"
    "/dist/"
    "/build/"
    "\\.elc\\'")
  "Regexps for files that should not be indexed.")

(defconst nelisp-defs-index-schema-version 1
  "Integer migration key written into `schema_meta.version'.
Phase 6.5.x stays at v1; mismatch triggers drop-and-rebuild
(Doc 25 §2.3 B).")

(defconst nelisp-defs-index--defining-forms
  '(defun defmacro defsubst cl-defun cl-defmacro
     defvar defvar-local defcustom defconst
     define-minor-mode define-derived-mode
     define-globalized-minor-mode
     cl-defgeneric cl-defmethod cl-defstruct
     cl-deftype defalias defclass)
  "Top-level form heads recognised as definitions in MVP.
Phase 6.5.2 may add more (e.g. defgroup, define-error).")

(defconst nelisp-defs-index--function-defining-forms
  '(defun defmacro defsubst cl-defun cl-defmacro
     cl-defgeneric cl-defmethod
     define-minor-mode define-derived-mode)
  "Subset of `nelisp-defs-index--defining-forms' that has an arglist
in the standard (OP NAME ARGLIST ...) shape, for arity extraction.")

;;;; --- backend / db helpers ----------------------------------------------

(defvar nelisp-defs-index--db nil
  "Open SQLite handle, or nil when `nelisp-defs-index-enable' has not run.")

(defcustom nelisp-defs-index-busy-timeout-ms 10000
  "Milliseconds the SQLite engine waits on `SQLITE_BUSY' before erroring.
Tier 2 fix (T59 finding #2): WAL alone does not serialise concurrent
writers; `PRAGMA busy_timeout' lets a competing writer wait for the
lock instead of failing immediately."
  :type 'integer
  :group 'nelisp-defs-index)

(defcustom nelisp-defs-index-busy-retry-count 1
  "Extra `BEGIN IMMEDIATE' retries on `SQLITE_BUSY' (Tier 2 fix #2)."
  :type 'integer
  :group 'nelisp-defs-index)

(defun nelisp-defs-index--require-sqlite ()
  "Signal `user-error' unless built-in sqlite (Emacs 29+) is available."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "nelisp-defs-index: built-in sqlite not available (Emacs 29+ required)")))

(defun nelisp-defs-index--apply-pragmas (db)
  "Apply the pragmas every freshly-opened DB needs.
WAL + busy_timeout + foreign_keys are required for the Tier 2 fix
(atomic shadow swap + concurrent writer + FK cascade)."
  (sqlite-execute db "PRAGMA journal_mode = WAL")
  (sqlite-execute db
                  (format "PRAGMA busy_timeout = %d"
                          (max 0 nelisp-defs-index-busy-timeout-ms)))
  (sqlite-execute db "PRAGMA foreign_keys = ON"))

(defun nelisp-defs-index--open-fresh (path)
  "Open a fresh DB handle at PATH with pragmas + DDL applied.
Used both by the cached `nelisp-defs-index--open' and by the
shadow rebuild path (see `nelisp-defs-index-rebuild')."
  (nelisp-defs-index--require-sqlite)
  (let ((dir (file-name-directory path)))
    (unless (file-directory-p dir) (make-directory dir t)))
  (let ((db (sqlite-open path)))
    (nelisp-defs-index--apply-pragmas db)
    (nelisp-defs-index--apply-ddl db)
    db))

(defun nelisp-defs-index--open ()
  "Open DB at `nelisp-defs-index-db-path' and cache the handle."
  (unless nelisp-defs-index--db
    (setq nelisp-defs-index--db
          (nelisp-defs-index--open-fresh nelisp-defs-index-db-path)))
  nelisp-defs-index--db)

(defun nelisp-defs-index--close ()
  "Close the cached DB handle (if any)."
  (when (and nelisp-defs-index--db (sqlitep nelisp-defs-index--db))
    (ignore-errors (sqlite-close nelisp-defs-index--db)))
  (setq nelisp-defs-index--db nil))

(defun nelisp-defs-index--invalidate-connection ()
  "Drop the cached connection so the next `--db' call re-opens.
Used after a shadow swap so readers pick up the new file."
  (nelisp-defs-index--close))

(defun nelisp-defs-index--db ()
  "Return the live DB handle, opening one on first call."
  (or nelisp-defs-index--db (nelisp-defs-index--open)))

(defun nelisp-defs-index--busy-error-p (err)
  "Return non-nil when ERR is an SQLITE_BUSY signal.
Emacs surfaces SQLite errors as either `(sqlite-error ...)' or as
`(error \"... busy ...\")' depending on build; check both forms."
  (let ((msg (cond
              ((and (consp err) (stringp (cadr err))) (cadr err))
              ((and (consp err) (stringp (car err))) (car err))
              (t (format "%S" err)))))
    (and (stringp msg)
         (or (string-match-p "busy" msg)
             (string-match-p "BUSY" msg)
             (string-match-p "locked" msg)))))

(defmacro nelisp-defs-index--with-immediate-tx (db &rest body)
  "Run BODY inside a `BEGIN IMMEDIATE' / COMMIT / ROLLBACK on DB.
Retries up to `nelisp-defs-index-busy-retry-count' times when the
initial BEGIN raises SQLITE_BUSY (the busy_timeout pragma already
covers in-flight contention; this retry is for the wait-and-fail
edge that occurs when another process holds the WAL writer lock
across multiple timeout windows).  Tier 2 fix #2."
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db"))
        (attempts (make-symbol "attempts"))
        (started (make-symbol "started"))
        (err-sym (make-symbol "err")))
    `(let ((,db-sym ,db)
           (,attempts (1+ (max 0 nelisp-defs-index-busy-retry-count)))
           (,started nil))
       (catch 'nelisp-defs-index--tx-done
         (while (> ,attempts 0)
           (cl-decf ,attempts)
           (condition-case ,err-sym
               (progn
                 (sqlite-execute ,db-sym "BEGIN IMMEDIATE")
                 (setq ,started t)
                 (throw 'nelisp-defs-index--tx-done nil))
             (error
              (if (and (> ,attempts 0)
                       (nelisp-defs-index--busy-error-p ,err-sym))
                  (sleep-for 0.05)
                (signal (car ,err-sym) (cdr ,err-sym)))))))
       (unless ,started
         (error "nelisp-defs-index: BEGIN IMMEDIATE never succeeded"))
       (condition-case ,err-sym
           (prog1 (progn ,@body)
             (sqlite-execute ,db-sym "COMMIT"))
         (error
          (ignore-errors (sqlite-execute ,db-sym "ROLLBACK"))
          (signal (car ,err-sym) (cdr ,err-sym)))))))

(defmacro nelisp-defs-index--with-transaction (db &rest body)
  "Run BODY inside a BEGIN / COMMIT / ROLLBACK on DB.
Doc 25 §2.5 B: inline re-definition (not re-export of
`nelisp-state--with-transaction') for module independence;
`feedback_sqlite_with_transaction_not_portable.md' は手書き
BEGIN/COMMIT を要求。Tier 2 fix #2 で BEGIN IMMEDIATE 経路を
`nelisp-defs-index--with-immediate-tx' へ分離。"
  (declare (indent 1) (debug t))
  `(nelisp-defs-index--with-immediate-tx ,db ,@body))

;;;; --- schema / ddl ------------------------------------------------------

(defconst nelisp-defs-index--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS file (
       id          INTEGER PRIMARY KEY,
       path        TEXT UNIQUE NOT NULL,
       mtime       INTEGER NOT NULL,
       size        INTEGER NOT NULL,
       indexed_at  INTEGER NOT NULL)"

    "CREATE TABLE IF NOT EXISTS defs (
       id              INTEGER PRIMARY KEY,
       file_id         INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       kind            TEXT NOT NULL,
       name            TEXT NOT NULL,
       line            INTEGER NOT NULL,
       end_line        INTEGER,
       arity_min       INTEGER,
       arity_max       INTEGER,
       docstring_head  TEXT,
       obsolete_p      INTEGER NOT NULL DEFAULT 0)"

    "CREATE INDEX IF NOT EXISTS idx_defs_name ON defs(name)"
    "CREATE INDEX IF NOT EXISTS idx_defs_file ON defs(file_id)"
    "CREATE INDEX IF NOT EXISTS idx_defs_kind_name ON defs(kind, name)"

    "CREATE TABLE IF NOT EXISTS refs (
       id       INTEGER PRIMARY KEY,
       file_id  INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       name     TEXT NOT NULL,
       line     INTEGER NOT NULL,
       context  TEXT,
       kind     TEXT NOT NULL)"

    "CREATE INDEX IF NOT EXISTS idx_refs_name ON refs(name)"
    "CREATE INDEX IF NOT EXISTS idx_refs_file ON refs(file_id)"

    "CREATE TABLE IF NOT EXISTS features (
       id       INTEGER PRIMARY KEY,
       file_id  INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       feature  TEXT NOT NULL,
       kind     TEXT NOT NULL)"

    "CREATE INDEX IF NOT EXISTS idx_features_feat_kind ON features(feature, kind)")
  "DDL applied by `nelisp-defs-index--apply-ddl'.")

(defun nelisp-defs-index--stored-schema-version (db)
  "Return the integer schema_meta.version stored in DB, or nil."
  (condition-case _err
      (car-safe (car-safe
                 (sqlite-select db "SELECT version FROM schema_meta")))
    (error nil)))

(defun nelisp-defs-index--drop-all-tables (db)
  "Drop every nelisp-defs-index-owned table from DB.
Doc 25 §2.3 B: schema mismatch → drop-and-rebuild instead of
per-version migration (cheap on project scale)."
  (dolist (tbl '("defs" "refs" "features" "file" "schema_meta"))
    (sqlite-execute db (format "DROP TABLE IF EXISTS %s" tbl))))

(defun nelisp-defs-index--apply-ddl (db)
  "Apply DDL and schema version row to DB.
Pragmas are applied separately by `nelisp-defs-index--apply-pragmas'
so the open path can re-use the same incantation for the live DB
and the shadow rebuild file.  On stored vs code schema mismatch,
drop owned tables first."
  (let ((stored (nelisp-defs-index--stored-schema-version db)))
    (when (and stored (not (= stored nelisp-defs-index-schema-version)))
      (message "nelisp-defs-index: schema mismatch (db=%s code=%s); dropping"
               stored nelisp-defs-index-schema-version)
      (nelisp-defs-index--drop-all-tables db)))
  (dolist (stmt nelisp-defs-index--ddl)
    (sqlite-execute db stmt))
  (sqlite-execute
   db "INSERT OR IGNORE INTO schema_meta(version) VALUES (?1)"
   (list nelisp-defs-index-schema-version)))

;;;; --- file discovery ---------------------------------------------------

(defun nelisp-defs-index--excluded-p (path)
  "Return non-nil when PATH matches one of `nelisp-defs-index-exclude-patterns'."
  (cl-some (lambda (re) (string-match-p re path))
           nelisp-defs-index-exclude-patterns))

(defun nelisp-defs-index--git-ls-files (root)
  "Return relative .el paths under ROOT via `git ls-files', or nil on failure.
handoff §9 R3: subprocess failure falls back to
`directory-files-recursively'."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory root))
           (rc (condition-case _
                   (call-process "git" nil t nil
                                 "ls-files" "--cached" "--others"
                                 "--exclude-standard"
                                 "*.el")
                 (error 1))))
      (when (and (integerp rc) (= rc 0))
        (split-string (buffer-string) "\n" t)))))

(defun nelisp-defs-index--project-root ()
  "Return git ancestor of `default-directory', or `default-directory'.
Pure subprocess call; no `project.el' dep so the helper stays usable
under `emacs --batch -Q'."
  (with-temp-buffer
    (let ((rc (condition-case _
                  (call-process "git" nil t nil
                                "rev-parse" "--show-toplevel")
                (error 1))))
      (if (and (integerp rc) (= rc 0))
          (file-name-as-directory (string-trim (buffer-string)))
        (file-name-as-directory (expand-file-name default-directory))))))

(defun nelisp-defs-index--collect-files (&optional paths)
  "Return absolute .el paths under PATHS (default `nelisp-defs-index-paths').
Falls back to the nearest git ancestor of `default-directory' when both
are unset.  Tries `git ls-files' first, then `directory-files-recursively'."
  (let* ((roots (or paths nelisp-defs-index-paths
                    (list (nelisp-defs-index--project-root))))
         (acc nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (when (file-directory-p abs)
          (let ((rels (nelisp-defs-index--git-ls-files abs)))
            (if rels
                (dolist (rel rels)
                  (let ((full (expand-file-name rel abs)))
                    (when (and (file-readable-p full)
                               (not (nelisp-defs-index--excluded-p full)))
                      (push full acc))))
              (dolist (f (directory-files-recursively abs "\\.el\\'" nil))
                (unless (nelisp-defs-index--excluded-p f)
                  (push (expand-file-name f) acc))))))))
    (nreverse (delete-dups acc))))

;;;; --- walker (Phase 6.5.2) ---------------------------------------------

(defconst nelisp-defs-index--walker-skip-symbols
  '(nil t)
  "Symbols too pervasive to record as references.
No project ever asks \"who calls nil?\".")

(defconst nelisp-defs-index--special-forms
  '(;; lexical / control flow
    let let* letrec lambda quote function progn prog1 prog2
    if cond when unless while
    save-excursion save-current-buffer save-restriction save-match-data
    condition-case unwind-protect catch throw
    and or not
    ;; binding / mutation
    setq setq-default setf setq-local
    push pop cl-incf cl-decf cl-pushnew
    add-to-list dolist dotimes
    ;; defining forms (head appears as op of the top-level sexp; the
    ;; deep walker should not record `(call defun)' once it recurses).
    defun defmacro defsubst defvar defvar-local defcustom defconst
    defgroup define-error define-minor-mode define-derived-mode
    define-globalized-minor-mode
    cl-defun cl-defmacro cl-defgeneric cl-defmethod cl-defstruct
    cl-deftype defalias defclass
    ;; module edges (already handled by dedicated branches but listed
    ;; so they don't double-emit a stray `call' ref).
    require provide)
  "Special forms / definers whose head is not a real call site.
Tier 2 fix #6: filtering these out cuts walker noise so refs only
contain meaningful function-style call edges plus `quote' / bare
`symbol' / module edges.")

(defconst nelisp-defs-index--binder-shapes
  '((let . bindings-and-body)
    (let* . bindings-and-body)
    (letrec . bindings-and-body)
    (cl-let . bindings-and-body)
    (cl-let* . bindings-and-body)
    (lambda . arglist-and-body)
    (defun . name-arglist-and-body)
    (defmacro . name-arglist-and-body)
    (defsubst . name-arglist-and-body)
    (cl-defun . name-arglist-and-body)
    (cl-defmacro . name-arglist-and-body)
    (cl-defgeneric . name-arglist-and-body)
    (cl-defmethod . name-arglist-and-body)
    (defvar . name-and-body)
    (defvar-local . name-and-body)
    (defcustom . name-and-body)
    (defconst . name-and-body)
    (condition-case . cc-var-and-body)
    (dolist . loop-and-body)
    (dotimes . loop-and-body))
  "Per-special-form arglist shape rules used by the deep walker.
Maps OP → SHAPE.  SHAPE controls which positional sub-forms are
treated as binding *names* (skipped from the ref output) versus
arbitrary forms (recursed into).  Tier 2 fix #6.")

(defun nelisp-defs-index--walker-ignored-p (sym)
  "Return non-nil when SYM should not be emitted by the walker."
  (or (null sym)
      (keywordp sym)
      (memq sym nelisp-defs-index--walker-skip-symbols)))

(defun nelisp-defs-index--collect-binding-names (bindings)
  "Return the binding-name symbols from a `let'-style BINDINGS list.
Each binding is `(NAME)' or `(NAME INIT)' or a bare NAME symbol."
  (let (names)
    (while (consp bindings)
      (let ((b (car bindings)))
        (cond
         ((symbolp b) (push b names))
         ((and (consp b) (symbolp (car b))) (push (car b) names))))
      (setq bindings (cdr bindings)))
    (nreverse names)))

(defun nelisp-defs-index--collect-arglist-names (arglist)
  "Return the binding-name symbols from a `defun'-style ARGLIST.
Skips lambda-list keywords (&optional / &rest / &key / ...) and
keeps both bare-symbol params and `(NAME INIT)' / `(NAME INIT SVAR)'
shapes (cl-arglist).  Tier 2 fix #6."
  (let (names)
    (while (consp arglist)
      (let ((a (car arglist)))
        (cond
         ((and (symbolp a)
               (string-prefix-p "&" (symbol-name a)))
          nil)
         ((symbolp a) (push a names))
         ((and (consp a) (symbolp (car a))) (push (car a) names))))
      (setq arglist (cdr arglist)))
    (nreverse names)))

(defun nelisp-defs-index--walk-each (xs fn)
  "Apply FN to each element of XS, tolerating improper / dotted lists.
`dolist' signals on the dotted tail; reader output may include
literal alists like `(:key . \"val\")', so the walker must not
rely on the proper-list invariant."
  (while (consp xs)
    (funcall fn (car xs))
    (setq xs (cdr xs)))
  (when xs (funcall fn xs)))

(defun nelisp-defs-index--walk-binder (op tail context line emit)
  "Walk a binder special form `(OP . TAIL)' according to its shape rule.
OP must be a key in `nelisp-defs-index--binder-shapes'.  Binding
names are skipped from the ref output; binding init forms and the
body are recursed into normally.  Returns non-nil when handled."
  (let ((shape (cdr (assq op nelisp-defs-index--binder-shapes)))
        (binders (nelisp-defs-index--collect-binding-names tail))
        (arglister (lambda (al)
                     (nelisp-defs-index--collect-arglist-names al))))
    (ignore binders arglister)
    (pcase shape
      ('bindings-and-body
       ;; (let ((NAME INIT) ...) BODY...)
       (let* ((bindings (car-safe tail))
              (body (cdr-safe tail))
              (skip (nelisp-defs-index--collect-binding-names bindings)))
         ;; Walk inits (cdr of each binding) only.
         (nelisp-defs-index--walk-each
          bindings
          (lambda (b)
            (cond
             ((symbolp b) nil)             ; bare NAME, no init
             ((consp b)
              (nelisp-defs-index--walk-each
               (cdr b)
               (lambda (init)
                 (nelisp-defs-index--walk-form-refs
                  init context line emit (cons 'skip-set skip))))))))
         ;; Walk body forms.
         (nelisp-defs-index--walk-each
          body
          (lambda (sub)
            (nelisp-defs-index--walk-form-refs
             sub context line emit (cons 'skip-set skip))))
         t))
      ('arglist-and-body
       ;; (lambda ARGLIST BODY...)
       (let* ((arglist (car-safe tail))
              (body (cdr-safe tail))
              (skip (nelisp-defs-index--collect-arglist-names arglist)))
         (nelisp-defs-index--walk-each
          body
          (lambda (sub)
            (nelisp-defs-index--walk-form-refs
             sub context line emit (cons 'skip-set skip))))
         t))
      ('name-arglist-and-body
       ;; (defun NAME ARGLIST [DOC] BODY...)
       (let* ((arglist (car-safe (cdr-safe tail)))
              (body (cdr-safe (cdr-safe tail)))
              ;; Drop a leading docstring so it doesn't get walked as
              ;; a `symbol' ref (strings aren't symbols, but still — be
              ;; explicit so future changes don't regress).
              (body (if (and (consp body) (stringp (car body)))
                        (cdr body) body))
              (skip (nelisp-defs-index--collect-arglist-names arglist)))
         (nelisp-defs-index--walk-each
          body
          (lambda (sub)
            (nelisp-defs-index--walk-form-refs
             sub context line emit (cons 'skip-set skip))))
         t))
      ('name-and-body
       ;; (defvar NAME [INIT [DOC]])
       (let* ((init (car-safe (cdr-safe tail))))
         (when init
           (nelisp-defs-index--walk-form-refs init context line emit nil))
         t))
      ('cc-var-and-body
       ;; (condition-case VAR BODY HANDLER...)
       (let* ((var (car-safe tail))
              (rest (cdr-safe tail))
              (skip (when (symbolp var) (list var))))
         (nelisp-defs-index--walk-each
          rest
          (lambda (sub)
            (nelisp-defs-index--walk-form-refs
             sub context line emit (cons 'skip-set skip))))
         t))
      ('loop-and-body
       ;; (dolist (VAR LIST [RESULT]) BODY...) / (dotimes (VAR N) BODY)
       (let* ((header (car-safe tail))
              (body (cdr-safe tail))
              (var (and (consp header) (car header)))
              (header-rest (and (consp header) (cdr header)))
              (skip (when (symbolp var) (list var))))
         (nelisp-defs-index--walk-each
          header-rest
          (lambda (sub)
            (nelisp-defs-index--walk-form-refs
             sub context line emit nil)))
         (nelisp-defs-index--walk-each
          body
          (lambda (sub)
            (nelisp-defs-index--walk-form-refs
             sub context line emit (cons 'skip-set skip))))
         t))
      (_ nil))))

(defun nelisp-defs-index--walk-form-refs (sexp context line emit
                                               &optional skip-cell)
  "Walk SEXP recursively, calling EMIT for each reference / feature edge.
EMIT is (KIND NAME LINE CONTEXT).  CONTEXT is the enclosing def's
name (string) or nil.  LINE is the top-level form's starting line.
SKIP-CELL is `(skip-set . SYMBOLS)' or nil; symbols in SYMBOLS are
not emitted as bare-`symbol' refs (binder names propagated by
`--walk-binder').  Recorded:
  - call sites: (OP ARGS...) with OP a plain symbol that is not a
    special form / definer (Tier 2 fix #6)
  - quote sites: \\='X / #\\='X (via `quote' / `function' heads)
  - require / provide edges (folded into the `features' table by
    the caller)
  - bare value-position symbols (excluding active binders)"
  (let ((skip-set (and (consp skip-cell) (cdr skip-cell))))
    (cond
     ((consp sexp)
      (let ((op (car sexp)))
        (cond
         ;; (quote X) / (function X) on a bare symbol — no recursion.
         ((and (memq op '(quote function))
               (consp (cdr sexp))
               (symbolp (cadr sexp))
               (not (nelisp-defs-index--walker-ignored-p (cadr sexp))))
          (funcall emit 'quote (symbol-name (cadr sexp)) line context))
         ;; (require 'SYM ...)
         ((and (eq op 'require)
               (consp (cdr sexp))
               (consp (cadr sexp))
               (eq (car (cadr sexp)) 'quote)
               (symbolp (cadr (cadr sexp))))
          (funcall emit 'require (symbol-name (cadr (cadr sexp)))
                   line context))
         ;; (provide 'SYM ...)
         ((and (eq op 'provide)
               (consp (cdr sexp))
               (consp (cadr sexp))
               (eq (car (cadr sexp)) 'quote)
               (symbolp (cadr (cadr sexp))))
          (funcall emit 'provide (symbol-name (cadr (cadr sexp)))
                   line context))
         ;; Binder special forms — custom shape walker (Tier 2 fix #6).
         ((and (symbolp op)
               (assq op nelisp-defs-index--binder-shapes))
          (nelisp-defs-index--walk-binder op (cdr sexp) context line emit))
         ;; Other special forms — skip head ref, recurse args (Tier 2 #6).
         ((and (symbolp op)
               (memq op nelisp-defs-index--special-forms))
          (nelisp-defs-index--walk-each
           (cdr sexp)
           (lambda (sub)
             (nelisp-defs-index--walk-form-refs
              sub context line emit skip-cell))))
         ;; (OP ARGS...) — record a call on OP, then recurse args.
         ((and (symbolp op)
               (not (nelisp-defs-index--walker-ignored-p op)))
          (funcall emit 'call (symbol-name op) line context)
          (nelisp-defs-index--walk-each
           (cdr sexp)
           (lambda (sub)
             (nelisp-defs-index--walk-form-refs
              sub context line emit skip-cell))))
         ;; non-symbol head (e.g. ((lambda (x) x) 1)) / dotted alists.
         (t
          (nelisp-defs-index--walk-each
           sexp
           (lambda (sub)
             (nelisp-defs-index--walk-form-refs
              sub context line emit skip-cell)))))))
     ;; Bare value-position symbol reference.
     ((and (symbolp sexp)
           (not (nelisp-defs-index--walker-ignored-p sexp))
           (not (memq sexp skip-set)))
      (funcall emit 'symbol (symbol-name sexp) line context)))))

;;;; --- scanner -----------------------------------------------------------

(defun nelisp-defs-index--first-line (s)
  "Return the first line of S, trimmed and clipped to 160 chars."
  (when (and s (stringp s))
    (let* ((nl (string-search "\n" s))
           (first (if nl (substring s 0 nl) s))
           (trim (string-trim first)))
      (if (> (length trim) 160) (substring trim 0 160) trim))))

(defun nelisp-defs-index--extract-docstring (sexp)
  "Return the docstring of SEXP, or nil.
Looks at the third element for defun-likes, fourth for defvar-likes."
  (when (consp sexp)
    (let ((op (car sexp))
          (rest (cddr sexp)))
      (cond
       ((memq op '(defun defmacro defsubst cl-defun cl-defmacro
                    cl-defgeneric cl-defmethod
                    define-minor-mode define-derived-mode))
        (when (and (consp rest) (stringp (cadr rest)))
          (cadr rest)))
       ((memq op '(defvar defvar-local defcustom defconst))
        (when (and (consp rest) (consp (cdr rest)) (stringp (cadr rest)))
          (cadr rest)))))))

(defun nelisp-defs-index--extract-arity (arglist)
  "Return (MIN . MAX) arity for ARGLIST.  MAX is nil when &rest present."
  (let ((min 0) (max 0) (stage 'req) (ret nil))
    (catch 'done
      (dolist (a arglist)
        (cond
         ((eq a '&optional) (setq stage 'opt))
         ((memq a '(&rest &body &key))
          (setq ret (cons min nil))
          (throw 'done nil))
         ((memq a '(&allow-other-keys &aux)) nil)
         ((eq stage 'req) (cl-incf min) (cl-incf max))
         ((eq stage 'opt) (cl-incf max))))
      (setq ret (cons min max)))
    ret))

(defun nelisp-defs-index--arglist (sexp)
  "Return the ARGLIST of a defun-like SEXP, or nil.
Skips shapes whose third slot is not statically an arglist
(define-minor-mode etc.)."
  (let* ((op (car sexp))
         (tail (cdr sexp))
         (second (car-safe tail)))
    (cond
     ((not (memq op nelisp-defs-index--function-defining-forms)) nil)
     ((memq op '(define-minor-mode define-derived-mode
                  define-globalized-minor-mode))
      nil)
     ((eq op 'cl-defmethod)
      (let ((rest (cdr-safe tail)))
        (while (and rest (keywordp (car rest)))
          (setq rest (cdr rest)))
        (let ((candidate (car-safe rest)))
          (when (listp candidate) candidate))))
     (t
      (let ((third (car-safe (cdr-safe tail))))
        (when (and (symbolp second) (listp third))
          third))))))

(defun nelisp-defs-index--scan-file (path)
  "Parse PATH and return (:defs LIST :refs LIST :features LIST).
Each :defs entry is a plist (:kind :name :line :end-line :arity-min
:arity-max :docstring-head :obsolete-p).  Each :refs entry is
(:name :line :context :kind), each :features entry is (:feature
:kind in {\"requires\",\"provides\"}).  Phase 6.5.2: top-level
defining forms feed `defs', the deep walker feeds `refs' /
`features' for every form (defining or not)."
  (let ((defs nil) (refs nil) (features nil))
    (with-temp-buffer
      (insert-file-contents path)
      ;; `read' on a buffer needs an elisp-friendly syntax table for
      ;; `?' / `'' to behave; the default `fundamental-mode' table
      ;; works for the subset we need.
      (goto-char (point-min))
      (let ((continue t))
        (while continue
          (let ((start-pos (point))
                (start-line (line-number-at-pos))
                (sexp nil))
            (condition-case _err
                (setq sexp (read (current-buffer)))
              (end-of-file (setq continue nil) (setq sexp nil))
              (error
               ;; Malformed top-level form — skip a token and keep going.
               (goto-char start-pos)
               (forward-line 1)
               (setq sexp nil)))
            (when (and continue sexp (consp sexp))
              (let* ((op (car sexp))
                     (name-sym (and (consp (cdr sexp)) (cadr sexp)))
                     (defining (and (memq op nelisp-defs-index--defining-forms)
                                    (symbolp name-sym)
                                    name-sym))
                     (context-str (and defining (symbol-name name-sym))))
                (when defining
                  (let* ((end-line (line-number-at-pos))
                         (docstring (nelisp-defs-index--extract-docstring sexp))
                         (arglist (nelisp-defs-index--arglist sexp))
                         (arity (and arglist
                                     (nelisp-defs-index--extract-arity arglist))))
                    (push (list :kind (symbol-name op)
                                :name (symbol-name name-sym)
                                :line start-line
                                :end-line end-line
                                :arity-min (car-safe arity)
                                :arity-max (cdr-safe arity)
                                :docstring-head
                                (nelisp-defs-index--first-line docstring)
                                :obsolete-p 0)
                          defs)))
                (let ((emit
                       (lambda (ekind ename eline ecntx)
                         (cond
                          ((memq ekind '(require provide))
                           (push (list :feature ename
                                       :kind (pcase ekind
                                               ('require "requires")
                                               ('provide "provides")))
                                 features))
                          (t
                           (push (list :name ename
                                       :line eline
                                       :context ecntx
                                       :kind (symbol-name ekind))
                                 refs))))))
                  (nelisp-defs-index--walk-form-refs
                   sexp context-str start-line emit))))))))
    (list :defs (nreverse defs)
          :refs (nreverse refs)
          :features (nreverse features))))

;;;; --- ingest ------------------------------------------------------------

(defun nelisp-defs-index--file-id (db path)
  "Return the `file.id' for PATH (inserting / updating the row)."
  (let* ((attrs (file-attributes path 'integer))
         (mtime (and attrs (float-time (file-attribute-modification-time attrs))))
         (size (and attrs (file-attribute-size attrs))))
    (sqlite-execute
     db
     "INSERT INTO file(path,mtime,size,indexed_at) VALUES (?1,?2,?3,?4)
        ON CONFLICT(path) DO UPDATE SET
          mtime=excluded.mtime, size=excluded.size, indexed_at=excluded.indexed_at"
     (list path (truncate (or mtime 0)) (or size 0) (truncate (float-time))))
    (car-safe (car-safe
               (sqlite-select db "SELECT id FROM file WHERE path = ?1"
                              (list path))))))

(defun nelisp-defs-index--delete-file-rows (db file-id)
  "Delete every indexed row for FILE-ID, leaving the file row itself."
  (sqlite-execute db "DELETE FROM defs WHERE file_id = ?1" (list file-id))
  (sqlite-execute db "DELETE FROM refs WHERE file_id = ?1" (list file-id))
  (sqlite-execute db "DELETE FROM features WHERE file_id = ?1" (list file-id)))

(defun nelisp-defs-index--ingest-file (db path)
  "Scan PATH and write its rows into DB (replacing prior state).
Returns plist (:defs N :refs N :features N)."
  (let* ((scan (nelisp-defs-index--scan-file path))
         (file-id (nelisp-defs-index--file-id db path)))
    (nelisp-defs-index--with-transaction db
      (nelisp-defs-index--delete-file-rows db file-id)
      (dolist (d (plist-get scan :defs))
        (sqlite-execute
         db
         "INSERT INTO defs(file_id,kind,name,line,end_line,
            arity_min,arity_max,docstring_head,obsolete_p)
          VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9)"
         (list file-id
               (plist-get d :kind)
               (plist-get d :name)
               (plist-get d :line)
               (plist-get d :end-line)
               (plist-get d :arity-min)
               (plist-get d :arity-max)
               (plist-get d :docstring-head)
               (plist-get d :obsolete-p))))
      (dolist (r (plist-get scan :refs))
        (sqlite-execute
         db
         "INSERT INTO refs(file_id,name,line,context,kind)
          VALUES (?1,?2,?3,?4,?5)"
         (list file-id
               (plist-get r :name)
               (plist-get r :line)
               (plist-get r :context)
               (plist-get r :kind))))
      (dolist (f (plist-get scan :features))
        (sqlite-execute
         db
         "INSERT INTO features(file_id,feature,kind) VALUES (?1,?2,?3)"
         (list file-id
               (plist-get f :feature)
               (plist-get f :kind)))))
    (list :defs (length (plist-get scan :defs))
          :refs (length (plist-get scan :refs))
          :features (length (plist-get scan :features)))))

;;;; --- public API --------------------------------------------------------

;;;###autoload
(defun nelisp-defs-index-enable ()
  "Open the backing SQLite DB.  Phase 6.5.1 registers no MCP tools here
— anvil.el's `anvil-defs' module calls the API directly via fboundp
delegation (architecture α)."
  (interactive)
  (nelisp-defs-index--open)
  nil)

(defun nelisp-defs-index-disable ()
  "Close the backing SQLite DB.  Idempotent."
  (interactive)
  (nelisp-defs-index--close)
  nil)

(defun nelisp-defs-index--build-into (shadow-path paths)
  "Build a fresh index at SHADOW-PATH from PATHS.  Returns counts plist.
Helper for `nelisp-defs-index-rebuild'.  Tier 2 fix #1 + #2: the
shadow file is created from scratch (so it never observes a
partial mid-build state from a previous failure) and uses
BEGIN IMMEDIATE so concurrent readers/writers respect busy_timeout."
  (when (file-exists-p shadow-path) (delete-file shadow-path))
  (let* ((db (nelisp-defs-index--open-fresh shadow-path))
         (files (nelisp-defs-index--collect-files paths))
         (file-count 0)
         (def-count 0)
         (ref-count 0)
         (feat-count 0)
         (failures 0))
    (unwind-protect
        (progn
          (dolist (f files)
            (condition-case err
                (let ((r (nelisp-defs-index--ingest-file db f)))
                  (cl-incf file-count)
                  (cl-incf def-count (plist-get r :defs))
                  (cl-incf ref-count (plist-get r :refs))
                  (cl-incf feat-count (plist-get r :features)))
              (error
               (cl-incf failures)
               (message "nelisp-defs-index: skipping %s: %S" f err))))
          (list :files file-count
                :defs def-count
                :refs ref-count
                :features feat-count
                :failures failures))
      (ignore-errors (sqlite-close db)))))

;;;###autoload
(defun nelisp-defs-index-rebuild (&optional paths)
  "Re-index every .el file under PATHS (default `nelisp-defs-index-paths').
Returns plist (:files N :defs N :refs N :features N :failures N
:duration-ms N).

Tier 2 fix #1 (T59): build the index into a `<db>.rebuild' shadow
file and rename it over the live DB on success.  Concurrent readers
keep seeing the previous fully-populated DB until the swap commits;
on build failure the shadow is unlinked and the live DB stays
unchanged (no silent data loss)."
  (let* ((t0 (current-time))
         (db-path nelisp-defs-index-db-path)
         (shadow-path (concat db-path ".rebuild"))
         result)
    (unwind-protect
        (progn
          (setq result
                (nelisp-defs-index--build-into shadow-path paths))
          ;; Drop our cached connection before swapping so the next
          ;; --db call opens the freshly-renamed file.
          (nelisp-defs-index--invalidate-connection)
          (let ((dir (file-name-directory db-path)))
            (unless (file-directory-p dir) (make-directory dir t)))
          (rename-file shadow-path db-path t)
          (setq shadow-path nil)
          (append result
                  (list :duration-ms
                        (truncate (* 1000 (float-time
                                           (time-subtract
                                            (current-time) t0)))))))
      ;; Failure path: unlink the shadow if it still exists.
      (when (and shadow-path (file-exists-p shadow-path))
        (ignore-errors (delete-file shadow-path))))))

;;;###autoload
(defun nelisp-defs-index-search (name &optional opts)
  "Return definition records whose NAME matches.
OPTS plist:
  :kind STRING-OR-LIST  - restrict to def.kind (e.g. \"defun\" or
                          (\"defun\" \"cl-defun\"))
  :fuzzy BOOLEAN        - LIKE %NAME% match instead of exact
  :limit INTEGER        - default 50

Each row is a plist with :kind :name :file :line :end-line :arity-min
:arity-max :docstring-head :obsolete-p."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "nelisp-defs-index-search: NAME must be a non-empty string, got %S"
                name))
  (let* ((db (nelisp-defs-index--db))
         (kind (plist-get opts :kind))
         (fuzzy (plist-get opts :fuzzy))
         (limit (or (plist-get opts :limit) 50))
         (kind-list (cond
                     ((null kind) nil)
                     ((listp kind)
                      (mapcar (lambda (k)
                                (if (symbolp k) (symbol-name k) k))
                              kind))
                     ((symbolp kind) (list (symbol-name kind)))
                     (t (list kind))))
         (kind-sql (cond
                    ((null kind-list) "")
                    (t (format " AND kind IN (%s)"
                               (mapconcat (lambda (_) "?")
                                          kind-list ",")))))
         (sql (format
               "SELECT d.kind, d.name, f.path, d.line, d.end_line,
                       d.arity_min, d.arity_max, d.docstring_head,
                       d.obsolete_p
                FROM defs d JOIN file f ON d.file_id = f.id
                WHERE %s%s
                ORDER BY d.name, f.path
                LIMIT ?"
               (if fuzzy "d.name LIKE ?" "d.name = ?")
               kind-sql))
         (params (append
                  (list (if fuzzy (format "%%%s%%" name) name))
                  kind-list
                  (list limit))))
    (mapcar (lambda (row)
              (list :kind (nth 0 row)
                    :name (nth 1 row)
                    :file (nth 2 row)
                    :line (nth 3 row)
                    :end-line (nth 4 row)
                    :arity-min (nth 5 row)
                    :arity-max (nth 6 row)
                    :docstring-head (nth 7 row)
                    :obsolete-p (nth 8 row)))
            (sqlite-select db sql params))))

;;;###autoload
(defun nelisp-defs-index-references (symbol &optional opts)
  "Return reference records for SYMBOL.
SYMBOL may be a string or symbol.

OPTS plist:
  :kind STRING-OR-LIST  - filter ref.kind (call / quote / symbol)
  :limit INTEGER        - default 500

Each row is a plist with :name :file :line :context :kind."
  (let ((name (cond
               ((symbolp symbol)
                (and symbol (symbol-name symbol)))
               (t symbol))))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error
       "nelisp-defs-index-references: SYMBOL must be a non-empty string or symbol, got %S"
       symbol))
    (let* ((db (nelisp-defs-index--db))
           (kind (plist-get opts :kind))
           (limit (or (plist-get opts :limit) 500))
           (kind-list (cond
                       ((null kind) nil)
                       ((listp kind)
                        (mapcar (lambda (k)
                                  (if (symbolp k) (symbol-name k) k))
                                kind))
                       ((symbolp kind) (list (symbol-name kind)))
                       (t (list kind))))
           (kind-sql (cond
                      ((null kind-list) "")
                      (t (format " AND r.kind IN (%s)"
                                 (mapconcat (lambda (_) "?")
                                            kind-list ",")))))
           (sql (format
                 "SELECT r.name, f.path, r.line, r.context, r.kind
                  FROM refs r JOIN file f ON r.file_id = f.id
                  WHERE r.name = ?%s
                  ORDER BY f.path, r.line
                  LIMIT ?"
                 kind-sql))
           (params (append (list name) kind-list (list limit))))
      (mapcar (lambda (row)
                (list :name (nth 0 row)
                      :file (nth 1 row)
                      :line (nth 2 row)
                      :context (nth 3 row)
                      :kind (nth 4 row)))
              (sqlite-select db sql params)))))

;;;###autoload
(defun nelisp-defs-index-signature (symbol)
  "Return a signature plist for SYMBOL, or nil if not indexed.
When multiple definitions exist the first encountered is returned.
Result plist: :name :kind :arity-min :arity-max :file :line
:docstring-head."
  (let* ((name (cond
                ((symbolp symbol) (and symbol (symbol-name symbol)))
                (t symbol)))
         (hits (and (stringp name) (not (string-empty-p name))
                    (nelisp-defs-index-search name '(:limit 1)))))
    (when hits
      (let ((h (car hits)))
        (list :name (plist-get h :name)
              :kind (plist-get h :kind)
              :arity-min (plist-get h :arity-min)
              :arity-max (plist-get h :arity-max)
              :file (plist-get h :file)
              :line (plist-get h :line)
              :docstring-head (plist-get h :docstring-head))))))

;;;###autoload
(defun nelisp-defs-index-who-requires (feature)
  "Return a list of file paths that contain `(require ''FEATURE)'.
FEATURE may be a string or symbol."
  (let ((feat (cond
               ((symbolp feature) (and feature (symbol-name feature)))
               (t feature))))
    (unless (and (stringp feat) (not (string-empty-p feat)))
      (user-error
       "nelisp-defs-index-who-requires: FEATURE must be a non-empty string or symbol, got %S"
       feature))
    (let ((db (nelisp-defs-index--db)))
      (mapcar #'car
              (sqlite-select
               db
               "SELECT f.path FROM features ff JOIN file f ON ff.file_id = f.id
                WHERE ff.feature = ?1 AND ff.kind = 'requires'
                ORDER BY f.path"
               (list feat))))))

;;;###autoload
(defun nelisp-defs-index-status ()
  "Return DB statistics plist:
(:db-path :schema-version :files :defs :refs :features :db-bytes)."
  (let* ((db (nelisp-defs-index--db))
         (count (lambda (sql)
                  (or (car-safe (car-safe (sqlite-select db sql))) 0)))
         (size (when (file-exists-p nelisp-defs-index-db-path)
                 (file-attribute-size
                  (file-attributes nelisp-defs-index-db-path)))))
    (list :db-path nelisp-defs-index-db-path
          :schema-version nelisp-defs-index-schema-version
          :files (funcall count "SELECT COUNT(*) FROM file")
          :defs (funcall count "SELECT COUNT(*) FROM defs")
          :refs (funcall count "SELECT COUNT(*) FROM refs")
          :features (funcall count "SELECT COUNT(*) FROM features")
          :db-bytes (or size 0))))

(provide 'nelisp-defs-index)
;;; nelisp-defs-index.el ends here
