;;; nelisp-state.el --- NeLisp persistent KV store (SQLite)  -*- lexical-binding: t; -*-

;; Phase 5-F.1.1 — anvil-state (Doc 08, 377 LOC) 1:1 port to NeLisp.
;; Doc 17 §2 LOCKED 2026-04-25 全 default 採用。
;;
;; anvil-state との差分:
;;   - DB path default = XDG ~/.cache/nelisp/nelisp-state.db (§2.2 B)
;;     (anvil-state は ~/.emacs.d/anvil-state.db、Emacs 前提。NeLisp の
;;     Stage D launcher からも使えるよう Emacs 非依存 path に)
;;   - schema に source TEXT 列追加 (§2.3 B、anvil/nelisp 混在時の区別用)
;;   - cl-defun → plist optional 方式 (§2.4 B)
;;     anvil-state の (cl-defun anvil-state-set (key val &key ns ttl)) に
;;     対応して (nelisp-state-set KEY VAL &optional OPTS) where OPTS =
;;     (:ns NS :ttl TTL)。呼び出し側は (nelisp-state-set "k" 'v '(:ns "foo"))

(require 'subr-x)

;;; Customization

(defgroup nelisp-state nil
  "NeLisp persistent KV store."
  :group 'nelisp
  :prefix "nelisp-state-")

(defvar nelisp-state-db-path
  (let* ((xdg (or (getenv "XDG_CACHE_HOME")
                  (expand-file-name ".cache" (getenv "HOME"))))
         (dir (expand-file-name "nelisp" xdg)))
    (expand-file-name "nelisp-state.db" dir))
  "Path to the SQLite database file backing `nelisp-state'.
Defaults to XDG cache location (Emacs 非依存)。Stage D launcher の
ANVIL_CACHE_DIR 環境変数で override 可能。
defvar (not defcustom) because NeLisp self-host customize 層未実装。")

(defconst nelisp-state-schema-version 1
  "Current schema version of the nelisp-state database.
Bump on incompatible changes; add a migration branch in
`nelisp-state--migrate' at the same time.")

(defconst nelisp-state-default-namespace "default"
  "Namespace used when the caller omits :ns.")

(defconst nelisp-state-source-tag "nelisp"
  "Default value for the kv.source column (§2.3 B).
anvil-state が書くときは \"anvil\"、future merge 時に区別可能にする。")

;;; Internal state

(defvar nelisp-state--db nil
  "Open SQLite handle, or nil when `nelisp-state-enable' has not run yet.")

;;; Backend plumbing

(defun nelisp-state--require-sqlite ()
  "Signal `user-error' unless built-in sqlite (Emacs 29+) is available."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "nelisp-state: built-in sqlite not available (Emacs 29+ required)")))

(defun nelisp-state--open ()
  "Open the database at `nelisp-state-db-path' and cache the handle."
  (nelisp-state--require-sqlite)
  (unless nelisp-state--db
    (let ((dir (file-name-directory nelisp-state-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq nelisp-state--db (sqlite-open nelisp-state-db-path))
    (nelisp-state--ensure-schema nelisp-state--db))
  nelisp-state--db)

(defun nelisp-state--close ()
  "Close the cached DB handle (if any)."
  (when (and nelisp-state--db (sqlitep nelisp-state--db))
    (ignore-errors (sqlite-close nelisp-state--db)))
  (setq nelisp-state--db nil))

(defun nelisp-state--db ()
  "Return the live DB handle, opening one on first call."
  (or nelisp-state--db (nelisp-state--open)))

(defmacro nelisp-state--with-transaction (db &rest body)
  "Run BODY inside a BEGIN / COMMIT transaction on DB.
§2.5 B: anvil-state と同じ defmacro port。memory
`feedback_sqlite_with_transaction_not_portable.md' 準拠で
`with-sqlite-transaction' は使わず手書き BEGIN/COMMIT。"
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

;;; Schema

(defconst nelisp-state--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS kv (
       ns          TEXT NOT NULL,
       k           TEXT NOT NULL,
       v           TEXT NOT NULL,
       created_at  INTEGER NOT NULL,
       updated_at  INTEGER NOT NULL,
       expires_at  INTEGER,
       source      TEXT NOT NULL DEFAULT 'nelisp',
       PRIMARY KEY (ns, k))"

    "CREATE INDEX IF NOT EXISTS idx_kv_expires
       ON kv(expires_at) WHERE expires_at IS NOT NULL")
  "DDL statements executed by `nelisp-state--ensure-schema'.
§2.3 B: source 列追加 (anvil-state との差分)。")

(defun nelisp-state--ensure-schema (db)
  "Create the schema on DB when missing and run migrations.
Phase 1 only knows schema v1."
  (dolist (stmt nelisp-state--ddl)
    (sqlite-execute db stmt))
  (let* ((row (car (sqlite-select
                    db "SELECT version FROM schema_meta LIMIT 1")))
         (current (and row (car row))))
    (cond
     ((null current)
      (sqlite-execute db "INSERT INTO schema_meta(version) VALUES (?1)"
                      (list nelisp-state-schema-version)))
     ((= current nelisp-state-schema-version)
      nil)
     (t
      (nelisp-state--migrate db current nelisp-state-schema-version)))))

(defun nelisp-state--migrate (_db from to)
  "Migrate schema from FROM to TO. Phase 1 has no upgrades defined."
  (error "nelisp-state: no migration path from schema v%d to v%d" from to))

;;; Serialization (§2.6 B: prin1/read + round-trip validation)

(defun nelisp-state--serialize (val)
  "Return a printed, re-readable representation of VAL.
Signals `user-error' when VAL contains non-readable objects."
  (let ((printed (let ((print-length nil)
                       (print-level nil)
                       (print-circle t))
                   (prin1-to-string val))))
    (condition-case err
        (let ((round (car (read-from-string printed))))
          (ignore round)
          printed)
      (error
       (user-error
        "nelisp-state: value is not readable (%s): %S"
        (error-message-string err) val)))))

(defun nelisp-state--deserialize (text)
  "Restore the Lisp value stored as TEXT."
  (car (read-from-string text)))

;;; Key helpers

(defun nelisp-state--check-string (name val)
  "Signal `user-error' unless VAL is a non-empty string."
  (unless (and (stringp val) (not (string-empty-p val)))
    (user-error "nelisp-state: %s must be a non-empty string, got %S"
                name val)))

(defun nelisp-state--ns (opts)
  "Pick the namespace out of OPTS plist.
Defaults to `nelisp-state-default-namespace' when :ns is absent."
  (let ((ns (or (plist-get opts :ns) nelisp-state-default-namespace)))
    (nelisp-state--check-string ":ns" ns)
    ns))

(defun nelisp-state--expires-at (opts)
  "Return an absolute unix-timestamp for :ttl in OPTS plist, or nil."
  (let ((ttl (plist-get opts :ttl)))
    (cond
     ((null ttl) nil)
     ((and (integerp ttl) (> ttl 0))
      (+ (truncate (float-time)) ttl))
     ((and (numberp ttl) (> ttl 0))
      (+ (truncate (float-time)) (truncate ttl)))
     (t
      (user-error
       "nelisp-state: :ttl must be a positive number of seconds, got %S"
       ttl)))))

;;; Public API (§2.1 B: full port、§2.4 B: plist optional)

;;;###autoload
(defun nelisp-state-set (key val &optional opts)
  "Store VAL under KEY in namespace (from OPTS :ns, default \"default\").

OPTS plist:
  :ns STRING  - Namespace (conventionally the module name).
  :ttl NUMBER - Seconds until expiry. nil means never.

VAL must round-trip through `prin1' / `read'; non-readable values
raise `user-error' before the DB is touched. Returns VAL."
  (nelisp-state--check-string "KEY" key)
  (let* ((db (nelisp-state--db))
         (ns* (nelisp-state--ns opts))
         (now (truncate (float-time)))
         (exp (nelisp-state--expires-at opts))
         (serialized (nelisp-state--serialize val)))
    (sqlite-execute
     db
     "INSERT INTO kv(ns, k, v, created_at, updated_at, expires_at, source)
        VALUES (?1, ?2, ?3, ?4, ?4, ?5, ?6)
        ON CONFLICT(ns, k) DO UPDATE SET
          v = excluded.v,
          updated_at = excluded.updated_at,
          expires_at = excluded.expires_at,
          source = excluded.source"
     (list ns* key serialized now exp nelisp-state-source-tag))
    val))

;;;###autoload
(defun nelisp-state-get (key &optional opts)
  "Return the value stored under KEY in namespace, or :default from OPTS.

OPTS plist:
  :ns STRING      - Namespace (default \"default\").
  :default VALUE  - Returned when KEY is absent or expired.

Expired rows are deleted lazily during the GET that notices them."
  (nelisp-state--check-string "KEY" key)
  (let* ((db (nelisp-state--db))
         (ns* (nelisp-state--ns opts))
         (default (plist-get opts :default))
         (now (truncate (float-time)))
         (row (car (sqlite-select
                    db
                    "SELECT v, expires_at FROM kv
                       WHERE ns = ?1 AND k = ?2 LIMIT 1"
                    (list ns* key)))))
    (cond
     ((null row) default)
     ((and (nth 1 row) (<= (nth 1 row) now))
      (sqlite-execute db "DELETE FROM kv WHERE ns = ?1 AND k = ?2"
                      (list ns* key))
      default)
     (t (nelisp-state--deserialize (nth 0 row))))))

;;;###autoload
(defun nelisp-state-delete (key &optional opts)
  "Remove KEY from namespace (OPTS :ns). Return t when a row was deleted."
  (nelisp-state--check-string "KEY" key)
  (let* ((db (nelisp-state--db))
         (ns* (nelisp-state--ns opts)))
    (sqlite-execute db "DELETE FROM kv WHERE ns = ?1 AND k = ?2"
                    (list ns* key))
    (let ((changed (car (car (sqlite-select db "SELECT changes()")))))
      (and (integerp changed) (> changed 0)))))

;;;###autoload
(defun nelisp-state-delete-ns (ns)
  "Remove every row in namespace NS. Returns the deletion count."
  (nelisp-state--check-string "NS" ns)
  (let ((db (nelisp-state--db)))
    (nelisp-state--with-transaction db
      (sqlite-execute db "DELETE FROM kv WHERE ns = ?1" (list ns))
      (or (car (car (sqlite-select db "SELECT changes()"))) 0))))

;;;###autoload
(defun nelisp-state-list-ns ()
  "Return the sorted list of namespaces currently holding rows."
  (mapcar #'car
          (sqlite-select
           (nelisp-state--db)
           "SELECT DISTINCT ns FROM kv ORDER BY ns")))

;;;###autoload
(defun nelisp-state-list-keys (&optional opts)
  "Return the sorted list of keys under namespace (OPTS :ns).
OPTS :limit N returns at most N keys. Expired rows are skipped so
callers see only live keys; run `nelisp-state-vacuum' to physically
remove them. OPTS :ns is required — enumerating the entire DB
would cross tenant boundaries and is not supported."
  (let ((ns (plist-get opts :ns))
        (limit (plist-get opts :limit)))
    (unless ns
      (error "nelisp-state-list-keys: :ns is required"))
    (nelisp-state--check-string "NS" ns)
    (let* ((db (nelisp-state--db))
           (now (truncate (float-time)))
           (sql (if limit
                    "SELECT k FROM kv
                       WHERE ns = ?1
                         AND (expires_at IS NULL OR expires_at > ?2)
                       ORDER BY k
                       LIMIT ?3"
                  "SELECT k FROM kv
                       WHERE ns = ?1
                         AND (expires_at IS NULL OR expires_at > ?2)
                       ORDER BY k"))
           (params (if limit (list ns now limit) (list ns now))))
      (mapcar #'car (sqlite-select db sql params)))))

;;;###autoload
(defun nelisp-state-count (&optional ns)
  "Return the number of live rows (optionally in NS only).
Expired rows are counted until `nelisp-state-vacuum' physically
removes them."
  (let* ((db (nelisp-state--db))
         (row (if ns
                  (progn
                    (nelisp-state--check-string "NS" ns)
                    (car (sqlite-select
                          db "SELECT COUNT(*) FROM kv WHERE ns = ?1"
                          (list ns))))
                (car (sqlite-select db "SELECT COUNT(*) FROM kv")))))
    (or (car row) 0)))

;;;###autoload
(defun nelisp-state-vacuum ()
  "Delete expired rows and run SQLite VACUUM.
Returns a plist with :expired (rows removed) and :size-before
/ :size-after byte counts of the DB file."
  (let* ((db (nelisp-state--db))
         (now (truncate (float-time)))
         (size-before (nth 7 (file-attributes nelisp-state-db-path)))
         (expired 0))
    (nelisp-state--with-transaction db
      (sqlite-execute
       db
       "DELETE FROM kv WHERE expires_at IS NOT NULL AND expires_at <= ?1"
       (list now))
      (setq expired (or (car (car (sqlite-select db "SELECT changes()")))
                        0)))
    (sqlite-execute db "VACUUM")
    (list :expired expired
          :size-before size-before
          :size-after (nth 7 (file-attributes nelisp-state-db-path)))))

;;; Lifecycle (§2.9 B: enable/disable 明示)

;;;###autoload
(defun nelisp-state-enable ()
  "Initialize the nelisp-state DB. No MCP tools are registered here.
Phase 5-F.1.2 で MCP tool expose 予定。"
  (nelisp-state--open)
  nil)

(defun nelisp-state-disable ()
  "Close the nelisp-state DB handle. Safe to call repeatedly."
  (nelisp-state--close)
  nil)

(provide 'nelisp-state)
;;; nelisp-state.el ends here
