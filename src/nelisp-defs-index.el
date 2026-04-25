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
;; Scanner is the inline simple variant (Doc 25 §2.6 B): no
;; dependency on `anvil-sexp', just `insert-file-contents' + `read'
;; loop with car-symbol pattern match for defun-likes.  Phase 6.5.2
;; will swap in the deep walker for refs/features.

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

(defun nelisp-defs-index--require-sqlite ()
  "Signal `user-error' unless built-in sqlite (Emacs 29+) is available."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "nelisp-defs-index: built-in sqlite not available (Emacs 29+ required)")))

(defun nelisp-defs-index--open ()
  "Open DB at `nelisp-defs-index-db-path' and cache the handle."
  (nelisp-defs-index--require-sqlite)
  (unless nelisp-defs-index--db
    (let ((dir (file-name-directory nelisp-defs-index-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq nelisp-defs-index--db (sqlite-open nelisp-defs-index-db-path))
    (nelisp-defs-index--apply-ddl nelisp-defs-index--db))
  nelisp-defs-index--db)

(defun nelisp-defs-index--close ()
  "Close the cached DB handle (if any)."
  (when (and nelisp-defs-index--db (sqlitep nelisp-defs-index--db))
    (ignore-errors (sqlite-close nelisp-defs-index--db)))
  (setq nelisp-defs-index--db nil))

(defun nelisp-defs-index--db ()
  "Return the live DB handle, opening one on first call."
  (or nelisp-defs-index--db (nelisp-defs-index--open)))

(defmacro nelisp-defs-index--with-transaction (db &rest body)
  "Run BODY inside a BEGIN / COMMIT / ROLLBACK on DB.
Doc 25 §2.5 B: inline re-definition (not re-export of
`nelisp-state--with-transaction') for module independence;
`feedback_sqlite_with_transaction_not_portable.md' は手書き
BEGIN/COMMIT を要求。"
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db")))
    `(let ((,db-sym ,db))
       (sqlite-execute ,db-sym "BEGIN")
       (condition-case err
           (prog1 (progn ,@body)
             (sqlite-execute ,db-sym "COMMIT"))
         (error
          (ignore-errors (sqlite-execute ,db-sym "ROLLBACK"))
          (signal (car err) (cdr err)))))))

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
  "Apply DDL, pragmas, and schema version row to DB.
On stored vs code schema mismatch, drop owned tables first."
  (sqlite-execute db "PRAGMA journal_mode = WAL")
  (sqlite-execute db "PRAGMA foreign_keys = ON")
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
  "Parse PATH and return (:defs LIST).
Each entry is a plist (:kind STR :name STR :line INT :end-line INT
:arity-min INT-or-nil :arity-max INT-or-nil :docstring-head STR-or-nil
:obsolete-p 0).  MVP: top-level forms only via `read' loop."
  (let ((defs nil))
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
                     (name-sym (and (consp (cdr sexp)) (cadr sexp))))
                (when (and (memq op nelisp-defs-index--defining-forms)
                           (symbolp name-sym)
                           name-sym)
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
                          defs)))))))))
    (list :defs (nreverse defs))))

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
Returns plist (:defs N)."
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
               (plist-get d :obsolete-p)))))
    (list :defs (length (plist-get scan :defs)))))

;;;; --- public API --------------------------------------------------------

;;;###autoload
(defun nelisp-defs-index-enable ()
  "Open the backing SQLite DB.  Phase 6.5.1 registers no MCP tools here
— `nelisp-tools.el' calls the API directly via deftool handlers."
  (interactive)
  (nelisp-defs-index--open)
  nil)

(defun nelisp-defs-index-disable ()
  "Close the backing SQLite DB.  Idempotent."
  (interactive)
  (nelisp-defs-index--close)
  nil)

;;;###autoload
(defun nelisp-defs-index-rebuild (&optional paths)
  "Re-index every .el file under PATHS (default `nelisp-defs-index-paths').
Returns plist (:files N :defs N :duration-ms N)."
  (let* ((db (nelisp-defs-index--db))
         (t0 (current-time))
         (files (nelisp-defs-index--collect-files paths))
         (file-count 0)
         (def-count 0))
    (nelisp-defs-index--with-transaction db
      (sqlite-execute db "DELETE FROM file")
      (sqlite-execute db "DELETE FROM defs")
      (sqlite-execute db "DELETE FROM refs")
      (sqlite-execute db "DELETE FROM features"))
    (dolist (f files)
      (condition-case _
          (let ((r (nelisp-defs-index--ingest-file db f)))
            (cl-incf file-count)
            (cl-incf def-count (plist-get r :defs)))
        (error nil)))
    (list :files file-count
          :defs def-count
          :duration-ms (truncate (* 1000 (float-time
                                          (time-subtract
                                           (current-time) t0)))))))

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

(provide 'nelisp-defs-index)
;;; nelisp-defs-index.el ends here
