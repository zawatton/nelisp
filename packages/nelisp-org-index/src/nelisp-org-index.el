;;; nelisp-org-index.el --- SQLite index of org headlines  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of NeLisp.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 6.6.1 (Doc 26 §3.6.1) — MVP substrate of an org headline
;; index for NeLisp.  Twin module of `nelisp-defs-index' (Phase 6.5)
;; for org files: rebuild + search縦串.
;;
;; Persistent SQLite index of every top-level headline across a tree
;; of *.org files, queried by title substring + tag + outline depth +
;; file path so a single MCP hop answers "where is this headline?"
;; without opening every org file.
;;
;; Schema (DDL compatible with anvil-org-index Doc 02 v1; advanced
;; tables — fts5, scheduled / deadline filters — DEFER to Phase
;; 6.6.3):
;;
;;   schema_meta (version)
;;   file        (id, path, mtime, size, indexed_at)
;;   headline    (id, file_id, parent_id, level, title, todo,
;;                priority, line_start, line_end, position, org_id)
;;   tag         (headline_id, tag)            PRIMARY KEY (h_id, tag)
;;   property    (headline_id, key, value)     PRIMARY KEY (h_id, key)
;;
;; The DB lives at `nelisp-org-index-db-path' (default XDG cache, Doc
;; 26 §2.4).  Deleting it is safe — full rebuild is cheap (1500 files
;; / 71k headlines indexes in ~3s on host elisp per anvil-org-index
;; production data).
;;
;; Scanner: `insert-file-contents' + line walk (no `org-mode' load),
;; modeled on `anvil-org-index--scan-buffer'.  Handles TODO keywords,
;; priority cookies, trailing tags, :PROPERTIES: drawer, and
;; SCHEDULED / DEADLINE / CLOSED planning lines (planning lines are
;; parsed but stored only on the headline plist; the columns exist
;; in the schema for forward compatibility with Phase 6.6.3).
;;
;; architecture α 準備 (Doc 26 §0):
;;   This module is standalone.  After Phase 6.6.x ships, the anvil
;;   side `anvil-org-index' will delegate its pure helpers
;;   (--collect-files / --scan-buffer / --excluded-p / search query
;;   composition) here via `fboundp' guard + legacy fallback (the
;;   same pattern used for anvil-defs / anvil-http).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; --- group / config -----------------------------------------------------

(defgroup nelisp-org-index nil
  "SQLite-backed org headline index for NeLisp."
  :group 'nelisp
  :prefix "nelisp-org-index-")

(defvar nelisp-org-index-db-path
  (let* ((xdg (or (getenv "XDG_CACHE_HOME")
                  (expand-file-name ".cache" (getenv "HOME"))))
         (dir (expand-file-name "nelisp" xdg)))
    (expand-file-name "nelisp-org-index.db" dir))
  "Path to the SQLite database file backing `nelisp-org-index'.
Defaults to XDG cache; Stage D launcher's NELISP_CACHE_DIR can
indirect.  defvar (not defcustom) per Doc 26 §2.6.")

(defvar nelisp-org-index-paths nil
  "List of project roots whose *.org files should be indexed.
When nil, `nelisp-org-index-rebuild' falls back to the git
ancestor of `default-directory'.  defvar per Doc 26 §2.6.")

(defvar nelisp-org-index-exclude-patterns
  '("/\\.git/"
    "/\\.claude/"
    "/node_modules/"
    "/tests/fixtures/"
    "/worktrees/"
    "/dist/"
    "/build/"
    "\\.org_archive\\'")
  "Regexps for *.org files that should not be indexed.")

(defvar nelisp-org-index-todo-keywords
  '("TODO" "DONE" "NEXT" "WAIT" "WAITING" "SOMEDAY" "CANCELLED"
    "MEMO" "NOTE" "PROJ" "PROJECT" "SCHEDULED" "INPROGRESS")
  "TODO keywords recognised when parsing headline lines.
Anything matching `\\\\=`[A-Z]+\\\\=`' is candidate; only members
of this list are stored as the `todo' column.")

(defconst nelisp-org-index-schema-version 2
  "Integer migration key written into `schema_meta.version'.
Tier 2 fix bumps to v2 (parent_id FK + FTS5 trigram).  Schema
mismatch on open triggers drop-and-rebuild.")

;;;; --- backend / db helpers ----------------------------------------------

(defvar nelisp-org-index--db nil
  "Open SQLite handle, or nil when `nelisp-org-index-enable' has not run.")

(defcustom nelisp-org-index-busy-timeout-ms 10000
  "Milliseconds the SQLite engine waits on `SQLITE_BUSY' before erroring.
Tier 2 fix (T59 finding #2): WAL alone does not serialise concurrent
writers."
  :type 'integer
  :group 'nelisp-org-index)

(defcustom nelisp-org-index-busy-retry-count 1
  "Extra `BEGIN IMMEDIATE' retries on `SQLITE_BUSY' (Tier 2 fix #2)."
  :type 'integer
  :group 'nelisp-org-index)

(defcustom nelisp-org-index-fts-tokenizer 'auto
  "Tokenizer used when (re-)creating the headlines FTS5 table.
Tier 2 fix (T59 finding #4 + #5): adds CJK-friendly substring
search.  Values:

  `auto'      probe the SQLite build; trigram when available.
  `trigram'   force the trigram tokenizer (CJK-friendly).
  `unicode61' force the unicode61 tokenizer (host-Emacs default)."
  :type '(choice (const :tag "Auto (trigram if available)" auto)
                 (const :tag "Trigram (SQLite 3.34+)" trigram)
                 (const :tag "unicode61" unicode61))
  :group 'nelisp-org-index)

(defun nelisp-org-index--require-sqlite ()
  "Signal `user-error' unless built-in sqlite (Emacs 29+) is available."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "nelisp-org-index: built-in sqlite not available (Emacs 29+ required)")))

(defun nelisp-org-index--apply-pragmas (db)
  "Apply the pragmas every freshly-opened DB needs (WAL + busy + FK)."
  (sqlite-execute db "PRAGMA journal_mode = WAL")
  (sqlite-execute db
                  (format "PRAGMA busy_timeout = %d"
                          (max 0 nelisp-org-index-busy-timeout-ms)))
  (sqlite-execute db "PRAGMA foreign_keys = ON"))

(defun nelisp-org-index--open-fresh (path)
  "Open a fresh DB handle at PATH with pragmas + DDL + FTS schema."
  (nelisp-org-index--require-sqlite)
  (let ((dir (file-name-directory path)))
    (unless (file-directory-p dir) (make-directory dir t)))
  (let ((db (sqlite-open path)))
    (nelisp-org-index--apply-pragmas db)
    (nelisp-org-index--apply-ddl db)
    (nelisp-org-index--ensure-fts db)
    db))

(defun nelisp-org-index--open ()
  "Open DB at `nelisp-org-index-db-path' and cache the handle."
  (unless nelisp-org-index--db
    (setq nelisp-org-index--db
          (nelisp-org-index--open-fresh nelisp-org-index-db-path)))
  nelisp-org-index--db)

(defun nelisp-org-index--close ()
  "Close the cached DB handle (if any)."
  (when (and nelisp-org-index--db (sqlitep nelisp-org-index--db))
    (ignore-errors (sqlite-close nelisp-org-index--db)))
  (setq nelisp-org-index--db nil))

(defun nelisp-org-index--invalidate-connection ()
  "Drop the cached connection so the next `--db' call re-opens.
Used after a shadow swap so readers pick up the new file."
  (nelisp-org-index--close))

(defun nelisp-org-index--db ()
  "Return the live DB handle, opening one on first call."
  (or nelisp-org-index--db (nelisp-org-index--open)))

(defun nelisp-org-index--busy-error-p (err)
  "Return non-nil when ERR is an SQLITE_BUSY signal."
  (let ((msg (cond
              ((and (consp err) (stringp (cadr err))) (cadr err))
              ((and (consp err) (stringp (car err))) (car err))
              (t (format "%S" err)))))
    (and (stringp msg)
         (or (string-match-p "busy" msg)
             (string-match-p "BUSY" msg)
             (string-match-p "locked" msg)))))

(defmacro nelisp-org-index--with-immediate-tx (db &rest body)
  "Run BODY inside a `BEGIN IMMEDIATE' / COMMIT / ROLLBACK on DB.
Retries up to `nelisp-org-index-busy-retry-count' times on
SQLITE_BUSY.  Tier 2 fix #2."
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db"))
        (attempts (make-symbol "attempts"))
        (started (make-symbol "started"))
        (err-sym (make-symbol "err")))
    `(let ((,db-sym ,db)
           (,attempts (1+ (max 0 nelisp-org-index-busy-retry-count)))
           (,started nil))
       (catch 'nelisp-org-index--tx-done
         (while (> ,attempts 0)
           (cl-decf ,attempts)
           (condition-case ,err-sym
               (progn
                 (sqlite-execute ,db-sym "BEGIN IMMEDIATE")
                 (setq ,started t)
                 (throw 'nelisp-org-index--tx-done nil))
             (error
              (if (and (> ,attempts 0)
                       (nelisp-org-index--busy-error-p ,err-sym))
                  (sleep-for 0.05)
                (signal (car ,err-sym) (cdr ,err-sym)))))))
       (unless ,started
         (error "nelisp-org-index: BEGIN IMMEDIATE never succeeded"))
       (condition-case ,err-sym
           (prog1 (progn ,@body)
             (sqlite-execute ,db-sym "COMMIT"))
         (error
          (ignore-errors (sqlite-execute ,db-sym "ROLLBACK"))
          (signal (car ,err-sym) (cdr ,err-sym)))))))

(defmacro nelisp-org-index--with-transaction (db &rest body)
  "Run BODY inside a BEGIN IMMEDIATE / COMMIT / ROLLBACK on DB.
Tier 2 fix #2: thin wrapper over `--with-immediate-tx'."
  (declare (indent 1) (debug t))
  `(nelisp-org-index--with-immediate-tx ,db ,@body))

;;;; --- schema / ddl ------------------------------------------------------

(defconst nelisp-org-index--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS file (
       id          INTEGER PRIMARY KEY,
       path        TEXT UNIQUE NOT NULL,
       mtime       INTEGER NOT NULL,
       size        INTEGER NOT NULL,
       indexed_at  INTEGER NOT NULL)"

    ;; Tier 2 fix #3: parent_id now carries a self-referencing FK so
    ;; future incremental update / watcher paths can't leak orphans.
    "CREATE TABLE IF NOT EXISTS headline (
       id          INTEGER PRIMARY KEY,
       file_id     INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       parent_id   INTEGER REFERENCES headline(id) ON DELETE CASCADE,
       level       INTEGER NOT NULL,
       title       TEXT NOT NULL,
       todo        TEXT,
       priority    TEXT,
       line_start  INTEGER NOT NULL,
       line_end    INTEGER,
       position    INTEGER NOT NULL,
       org_id      TEXT)"

    "CREATE INDEX IF NOT EXISTS idx_headline_file ON headline(file_id)"
    "CREATE INDEX IF NOT EXISTS idx_headline_parent ON headline(parent_id)"
    "CREATE INDEX IF NOT EXISTS idx_headline_todo ON headline(todo)"
    "CREATE INDEX IF NOT EXISTS idx_headline_level ON headline(level)"
    ;; Tier 2 fix #4: title B-tree dropped — search uses LIKE %x% so
    ;; the prefix index never gets used.  FTS5 (--ensure-fts) handles
    ;; substring relevance.

    "CREATE TABLE IF NOT EXISTS tag (
       headline_id INTEGER NOT NULL REFERENCES headline(id) ON DELETE CASCADE,
       tag         TEXT NOT NULL,
       PRIMARY KEY (headline_id, tag))"

    "CREATE INDEX IF NOT EXISTS idx_tag_tag ON tag(tag)"

    "CREATE TABLE IF NOT EXISTS property (
       headline_id INTEGER NOT NULL REFERENCES headline(id) ON DELETE CASCADE,
       key         TEXT NOT NULL,
       value       TEXT NOT NULL,
       PRIMARY KEY (headline_id, key))"

    "CREATE INDEX IF NOT EXISTS idx_property_key ON property(key)")
  "DDL applied by `nelisp-org-index--apply-ddl'.")

(defun nelisp-org-index--stored-schema-version (db)
  "Return the integer schema_meta.version stored in DB, or nil."
  (condition-case _err
      (car-safe (car-safe
                 (sqlite-select db "SELECT version FROM schema_meta")))
    (error nil)))

(defun nelisp-org-index--drop-all-tables (db)
  "Drop every nelisp-org-index-owned table from DB.
Schema mismatch → drop-and-rebuild (cheap on project scale).
Drops the FTS5 virtual table first so its shadow tables go too."
  (dolist (tbl '("headlines_fts" "property" "tag" "headline"
                 "file" "schema_meta"))
    (sqlite-execute db (format "DROP TABLE IF EXISTS %s" tbl))))

(defun nelisp-org-index--apply-ddl (db)
  "Apply DDL and schema version row to DB.
Pragmas live in `nelisp-org-index--apply-pragmas'.  On stored vs
code schema mismatch, drop owned tables first."
  (let ((stored (nelisp-org-index--stored-schema-version db)))
    (when (and stored (not (= stored nelisp-org-index-schema-version)))
      (message "nelisp-org-index: schema mismatch (db=%s code=%s); dropping"
               stored nelisp-org-index-schema-version)
      (nelisp-org-index--drop-all-tables db)))
  (dolist (stmt nelisp-org-index--ddl)
    (sqlite-execute db stmt))
  (sqlite-execute
   db "INSERT OR IGNORE INTO schema_meta(version) VALUES (?1)"
   (list nelisp-org-index-schema-version)))

;;;; --- FTS5 (Tier 2 fix #4 + #5) -----------------------------------------

(defun nelisp-org-index--sqlite-supports-trigram-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer."
  (condition-case nil
      (progn
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE nelisp_org_trigram_probe
            USING fts5(x, tokenize='trigram')")
        (sqlite-execute db "DROP TABLE nelisp_org_trigram_probe")
        t)
    (error nil)))

(defun nelisp-org-index--resolve-tokenizer (db)
  "Return the tokenizer symbol DB should use, given the user setting."
  (pcase nelisp-org-index-fts-tokenizer
    ('trigram   'trigram)
    ('unicode61 'unicode61)
    ('auto (if (nelisp-org-index--sqlite-supports-trigram-p db)
               'trigram
             'unicode61))
    (other (user-error
            "nelisp-org-index: invalid `nelisp-org-index-fts-tokenizer': %S"
            other))))

(defvar nelisp-org-index--last-tokenizer nil
  "Tokenizer used for the most recently opened FTS5 table.
Set by `nelisp-org-index--ensure-fts' so callers / tests can verify
which path the build took (trigram vs unicode61).")

(defun nelisp-org-index--tokenizer-for (db)
  "Return the tokenizer symbol active for DB (open or freshly resolved)."
  (or nelisp-org-index--last-tokenizer
      (nelisp-org-index--resolve-tokenizer db)))

(defun nelisp-org-index--ensure-fts (db)
  "Create the headlines FTS5 virtual table if it does not exist.
Uses `external content' so the FTS rows are kept in sync with the
backing `headline' table by triggers — `--ensure-fts-triggers'
installs them."
  (let* ((tok (nelisp-org-index--resolve-tokenizer db))
         (tok-sql (pcase tok
                    ('trigram   ", tokenize='trigram'")
                    ('unicode61 "")))
         (existing (sqlite-select
                    db
                    "SELECT name FROM sqlite_master
                       WHERE type='table' AND name='headlines_fts'")))
    (unless existing
      (sqlite-execute
       db
       (concat
        "CREATE VIRTUAL TABLE headlines_fts USING fts5("
        " title,"
        " content='headline',"
        " content_rowid='id'"
        tok-sql ")")))
    (setq nelisp-org-index--last-tokenizer tok)
    (nelisp-org-index--ensure-fts-triggers db)
    tok))

(defun nelisp-org-index--ensure-fts-triggers (db)
  "Install FTS5 sync triggers on `headline' so inserts/updates/deletes
keep `headlines_fts' in lock-step.  Idempotent."
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS headline_ai AFTER INSERT ON headline BEGIN
      INSERT INTO headlines_fts(rowid, title) VALUES (new.id, new.title);
    END")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS headline_ad AFTER DELETE ON headline BEGIN
      INSERT INTO headlines_fts(headlines_fts, rowid, title)
        VALUES ('delete', old.id, old.title);
    END")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS headline_au AFTER UPDATE ON headline BEGIN
      INSERT INTO headlines_fts(headlines_fts, rowid, title)
        VALUES ('delete', old.id, old.title);
      INSERT INTO headlines_fts(rowid, title) VALUES (new.id, new.title);
    END"))

;;;; --- file discovery ---------------------------------------------------

(defun nelisp-org-index--excluded-p (path)
  "Return non-nil when PATH matches `nelisp-org-index-exclude-patterns'."
  (cl-some (lambda (re) (string-match-p re path))
           nelisp-org-index-exclude-patterns))

(defun nelisp-org-index--git-ls-files (root)
  "Return relative .org paths under ROOT via `git ls-files', or nil on failure.
Falls back to `directory-files-recursively' upstream when the git
subprocess fails."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory root))
           (rc (condition-case _
                   (call-process "git" nil t nil
                                 "ls-files" "--cached" "--others"
                                 "--exclude-standard"
                                 "*.org")
                 (error 1))))
      (when (and (integerp rc) (= rc 0))
        (split-string (buffer-string) "\n" t)))))

(defun nelisp-org-index--project-root ()
  "Return git ancestor of `default-directory', or `default-directory'.
Pure subprocess call; no `project.el' dep so the helper stays
usable under `emacs --batch -Q'."
  (with-temp-buffer
    (let ((rc (condition-case _
                  (call-process "git" nil t nil
                                "rev-parse" "--show-toplevel")
                (error 1))))
      (if (and (integerp rc) (= rc 0))
          (file-name-as-directory (string-trim (buffer-string)))
        (file-name-as-directory (expand-file-name default-directory))))))

(defun nelisp-org-index--collect-files (&optional paths)
  "Return absolute *.org paths under PATHS (default `nelisp-org-index-paths').
Falls back to the nearest git ancestor of `default-directory' when
both are unset.  Tries `git ls-files' first, then
`directory-files-recursively'."
  (let* ((roots (or paths nelisp-org-index-paths
                    (list (nelisp-org-index--project-root))))
         (acc nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (when (file-directory-p abs)
          (let ((rels (nelisp-org-index--git-ls-files abs)))
            (if rels
                (dolist (rel rels)
                  (let ((full (expand-file-name rel abs)))
                    (when (and (file-readable-p full)
                               (not (nelisp-org-index--excluded-p full)))
                      (push full acc))))
              (dolist (f (directory-files-recursively abs "\\.org\\'" nil))
                (unless (nelisp-org-index--excluded-p f)
                  (push (expand-file-name f) acc))))))))
    (nreverse (delete-dups acc))))

;;;; --- scanner -----------------------------------------------------------

(defun nelisp-org-index--parse-headline-title (raw level)
  "Parse RAW headline body (no leading stars) at LEVEL into a plist.
Extracts TODO keyword, priority cookie [#A], trailing tags
:t1:t2:, and the clean title.  Mirrors
`anvil-org-index--parse-headline-title'."
  (let ((s raw) todo prio tags)
    (when (string-match "\\`\\([A-Z]+\\(?:-[A-Z]+\\)?\\)[ \t]+\\(.*\\)\\'" s)
      (let ((kw (match-string 1 s)))
        (when (member kw nelisp-org-index-todo-keywords)
          (setq todo kw
                s (match-string 2 s)))))
    (when (string-match "\\`\\[#\\([A-Z]\\)\\][ \t]+\\(.*\\)\\'" s)
      (setq prio (match-string 1 s)
            s (match-string 2 s)))
    (when (string-match "[ \t]+\\(:[A-Za-z0-9_@#%:]+:\\)[ \t]*\\'" s)
      ;; Capture match data before `split-string' clobbers it.
      (let ((beg (match-beginning 0))
            (tagstr (match-string 1 s)))
        (setq tags (split-string tagstr ":" t))
        (setq s (substring s 0 beg))))
    (list :level level
          :todo todo
          :priority prio
          :tags tags
          :title (string-trim s))))

(defun nelisp-org-index--scan-buffer ()
  "Scan the current buffer as org text and return a flat headline list.
Each entry is a plist (:level :title :todo :priority :tags
:position :line-start :line-end :org-id :properties).  Headlines
are emitted in document order; their `:line-end' is set to the
line just before the next headline (or the buffer's last line)."
  (save-excursion
    (goto-char (point-min))
    (let ((results nil)
          (current nil)
          (in-properties nil)
          (line 1))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (text (buffer-substring-no-properties bol eol)))
          (cond
           ;; headline
           ((string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)\\'" text)
            (when current
              (setq current (plist-put current :line-end (1- line)))
              (push current results))
            (let* ((level (length (match-string 1 text)))
                   (rest (match-string 2 text))
                   (parsed (nelisp-org-index--parse-headline-title
                            rest level)))
              (setq current (append parsed
                                    (list :position bol
                                          :line-start line
                                          :org-id nil
                                          :properties nil))
                    in-properties nil)))
           ;; :PROPERTIES: start
           ((and current
                 (string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'" text))
            (setq in-properties t))
           ;; :END:
           ((and current in-properties
                 (string-match-p "\\`[ \t]*:END:[ \t]*\\'" text))
            (setq in-properties nil))
           ;; property line
           ((and current in-properties
                 (string-match
                  "\\`[ \t]*:\\([^:\n\t ]+\\):[ \t]+\\(.*\\)\\'" text))
            (let ((key (match-string 1 text))
                  (val (string-trim (match-string 2 text))))
              (setq current
                    (plist-put current :properties
                               (cons (cons key val)
                                     (plist-get current :properties))))
              (when (string-equal (upcase key) "ID")
                (setq current (plist-put current :org-id val))))))
          (goto-char (1+ eol))
          (cl-incf line)))
      (when current
        (setq current (plist-put current :line-end (1- line)))
        (push current results))
      (nreverse results))))

(defun nelisp-org-index--scan-file (path)
  "Return a flat headline plist list for PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (nelisp-org-index--scan-buffer)))

;;;; --- ingest ------------------------------------------------------------

(defun nelisp-org-index--insert-file-row (db path)
  "Delete the existing FILE row for PATH if any, then insert fresh.
Return the new file id."
  (let* ((attrs (file-attributes path 'integer))
         (mtime (and attrs (truncate
                            (float-time
                             (file-attribute-modification-time attrs)))))
         (size (and attrs (file-attribute-size attrs)))
         (now (truncate (float-time))))
    (sqlite-execute db "DELETE FROM file WHERE path = ?1" (list path))
    (sqlite-execute
     db
     "INSERT INTO file(path,mtime,size,indexed_at) VALUES (?1,?2,?3,?4)"
     (list path (or mtime 0) (or size 0) now))
    (car-safe (car-safe
               (sqlite-select db "SELECT last_insert_rowid()")))))

(defun nelisp-org-index--insert-headline (db file-id parent-id hl)
  "Insert one HL plist under FILE-ID with optional PARENT-ID.
Return the new headline id."
  (sqlite-execute
   db
   "INSERT INTO headline
      (file_id, parent_id, level, title, todo, priority,
       line_start, line_end, position, org_id)
    VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)"
   (list file-id
         parent-id
         (plist-get hl :level)
         (or (plist-get hl :title) "")
         (plist-get hl :todo)
         (plist-get hl :priority)
         (plist-get hl :line-start)
         (plist-get hl :line-end)
         (plist-get hl :position)
         (plist-get hl :org-id)))
  (let ((hid (car-safe (car-safe
                        (sqlite-select db "SELECT last_insert_rowid()")))))
    (dolist (tag (plist-get hl :tags))
      (sqlite-execute
       db "INSERT OR IGNORE INTO tag(headline_id,tag) VALUES (?1,?2)"
       (list hid tag)))
    (dolist (kv (plist-get hl :properties))
      (sqlite-execute
       db "INSERT OR REPLACE INTO property(headline_id,key,value)
           VALUES (?1,?2,?3)"
       (list hid (car kv) (cdr kv))))
    hid))

(defun nelisp-org-index--ingest-file (db path)
  "Re-index PATH into DB (replacing prior rows).
Returns plist (:headlines N)."
  (let* ((headlines (nelisp-org-index--scan-file path))
         (file-id (nelisp-org-index--insert-file-row db path))
         (stack nil)
         (count 0))
    (dolist (hl headlines)
      (let ((level (plist-get hl :level)))
        (while (and stack (>= (caar stack) level))
          (pop stack))
        (let* ((parent-id (and stack (cdar stack)))
               (hid (nelisp-org-index--insert-headline
                     db file-id parent-id hl)))
          (push (cons level hid) stack)
          (cl-incf count))))
    (list :headlines count)))

;;;; --- public API --------------------------------------------------------

;;;###autoload
(defun nelisp-org-index-enable ()
  "Open the backing SQLite DB.  Idempotent.
Phase 6.6.1 registers no MCP tools here — anvil.el's `anvil-org-index'
module calls the API directly via fboundp delegation (architecture α)."
  (interactive)
  (nelisp-org-index--open)
  nil)

(defun nelisp-org-index-disable ()
  "Close the backing SQLite DB.  Idempotent."
  (interactive)
  (nelisp-org-index--close)
  nil)

(defun nelisp-org-index--build-into (shadow-path paths)
  "Build a fresh org index at SHADOW-PATH from PATHS.  Returns counts plist.
Tier 2 fix #1 + #2: shadow file is created from scratch with full
schema + FTS5 + busy_timeout so writers respect concurrency."
  (when (file-exists-p shadow-path) (delete-file shadow-path))
  (let* ((db (nelisp-org-index--open-fresh shadow-path))
         (files (nelisp-org-index--collect-files paths))
         (file-count 0)
         (head-count 0)
         (failures 0))
    (unwind-protect
        (progn
          (dolist (f files)
            (condition-case err
                (nelisp-org-index--with-transaction db
                  (let ((r (nelisp-org-index--ingest-file db f)))
                    (cl-incf file-count)
                    (cl-incf head-count (plist-get r :headlines))))
              (error
               (cl-incf failures)
               (message "nelisp-org-index: skipping %s: %S" f err))))
          (list :files file-count
                :headlines head-count
                :failures failures))
      (ignore-errors (sqlite-close db)))))

;;;###autoload
(defun nelisp-org-index-rebuild (&optional paths)
  "Re-index every *.org file under PATHS (default `nelisp-org-index-paths').
Returns plist (:files N :headlines N :failures N :duration-ms N).

Tier 2 fix #1 (T59): build the index into a `<db>.rebuild' shadow
file and rename it over the live DB on success.  Concurrent readers
keep seeing the previous fully-populated DB until the swap commits;
on build failure the shadow is unlinked and the live DB stays
unchanged (no silent data loss)."
  (let* ((t0 (current-time))
         (db-path nelisp-org-index-db-path)
         (shadow-path (concat db-path ".rebuild"))
         result)
    (unwind-protect
        (progn
          (setq result
                (nelisp-org-index--build-into shadow-path paths))
          (nelisp-org-index--invalidate-connection)
          (let ((dir (file-name-directory db-path)))
            (unless (file-directory-p dir) (make-directory dir t)))
          (rename-file shadow-path db-path t)
          (setq shadow-path nil)
          (append result
                  (list :duration-ms
                        (truncate (* 1000 (float-time
                                           (time-subtract
                                            (current-time) t0)))))))
      (when (and shadow-path (file-exists-p shadow-path))
        (ignore-errors (delete-file shadow-path))))))

(defun nelisp-org-index--fts-quote (term)
  "Wrap TERM in double quotes for FTS5 MATCH, escaping embedded quotes."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" term) "\""))

;;;###autoload
(cl-defun nelisp-org-index-search
    (name &key file tag depth limit fts)
  "Return headline records whose title matches NAME.
NAME is matched as either an FTS5 query (when the headlines FTS5
table is available — Tier 2 fix #4 + #5) or as a SQL LIKE %NAME%
substring fallback.

Keyword arguments:
  :file STRING   - SQL LIKE pattern on `file.path' (auto-wrap with
                   %..% when no % present).
  :tag STRING    - exact tag match (single tag; AND of multiple is
                   Phase 6.6.3).
  :depth INTEGER - exact outline level filter.
  :limit INTEGER - default 200.
  :fts SYMBOL    - one of `auto' (default) / `on' / `off'.  `auto'
                   uses FTS5 when the table exists *and* NAME has
                   no SQL `%' wildcards; `off' forces the legacy
                   LIKE path; `on' demands FTS5 (errors if missing).

Each row is a plist with :file :line :level :title :tags :todo
:priority :org-id."
  (unless (stringp name)
    (user-error
     "nelisp-org-index-search: NAME must be a string, got %S" name))
  (let* ((db (nelisp-org-index--db))
         (lim (or limit 200))
         (mode (or fts 'auto))
         (has-fts (sqlite-select
                   db
                   "SELECT 1 FROM sqlite_master
                      WHERE type='table' AND name='headlines_fts'"))
         (use-fts (cond
                   ((eq mode 'off) nil)
                   ((eq mode 'on)
                    (unless has-fts
                      (user-error
                       "nelisp-org-index-search: :fts on but headlines_fts missing"))
                    (not (string-empty-p name)))
                   (t                    ; 'auto
                    (and has-fts
                         (not (string-empty-p name))
                         (not (string-match-p "%" name))))))
         (file-pat (and file
                        (stringp file)
                        (not (string-empty-p file))
                        (if (string-match-p "%" file)
                            file
                          (format "%%%s%%" file)))))
    (cond
     (use-fts
      (let* ((where (list))
             (args (list))
             (match-arg (nelisp-org-index--fts-quote name)))
        (when file-pat
          (push "f.path LIKE ?" where)
          (push file-pat args))
        (when (and tag (stringp tag) (not (string-empty-p tag)))
          (push "EXISTS (SELECT 1 FROM tag t WHERE t.headline_id = h.id AND t.tag = ?)" where)
          (push tag args))
        (when (and depth (integerp depth))
          (push "h.level = ?" where)
          (push depth args))
        (let* ((extra-sql
                (if where
                    (concat " AND "
                            (mapconcat #'identity (nreverse where) " AND "))
                  ""))
               (sql (format
                     "SELECT f.path, h.line_start, h.level, h.title,
                             h.todo, h.priority, h.org_id,
                             (SELECT GROUP_CONCAT(t.tag, ',')
                                FROM tag t WHERE t.headline_id = h.id)
                      FROM headlines_fts fts
                      JOIN headline h ON h.id = fts.rowid
                      JOIN file f ON h.file_id = f.id
                      WHERE headlines_fts MATCH ?%s
                      ORDER BY rank, f.path, h.position
                      LIMIT ?"
                     extra-sql))
               (params (append (list match-arg)
                               (nreverse args)
                               (list lim))))
          (condition-case err
              (mapcar
               (lambda (row)
                 (list :file (nth 0 row)
                       :line (nth 1 row)
                       :level (nth 2 row)
                       :title (nth 3 row)
                       :todo (nth 4 row)
                       :priority (nth 5 row)
                       :org-id (nth 6 row)
                       :tags (let ((concat (nth 7 row)))
                               (and concat (split-string concat "," t)))))
               (sqlite-select db sql params))
            (error
             ;; FTS5 query syntax errors fall back to LIKE so callers
             ;; never see "fts5: syntax error" for free-form input.
             (message "nelisp-org-index-search: FTS5 fell back to LIKE: %S" err)
             (nelisp-org-index--search-like
              db name file-pat tag depth lim))))))
     (t
      (nelisp-org-index--search-like
       db name file-pat tag depth lim)))))

(defun nelisp-org-index--search-like (db name file-pat tag depth lim)
  "Legacy LIKE-based search.  Backward-compat fallback path.
DB is the live handle; the other args are normalised by
`nelisp-org-index-search'."
  (let* ((title-pat (cond
                     ((string-empty-p name) "%")
                     ((string-match-p "%" name) name)
                     (t (format "%%%s%%" name))))
         (where (list "h.title LIKE ?"))
         (args (list title-pat)))
    (when file-pat
      (push "f.path LIKE ?" where)
      (push file-pat args))
    (when (and tag (stringp tag) (not (string-empty-p tag)))
      (push "EXISTS (SELECT 1 FROM tag t WHERE t.headline_id = h.id AND t.tag = ?)" where)
      (push tag args))
    (when (and depth (integerp depth))
      (push "h.level = ?" where)
      (push depth args))
    (let* ((sql (format
                 "SELECT f.path, h.line_start, h.level, h.title,
                         h.todo, h.priority, h.org_id,
                         (SELECT GROUP_CONCAT(t.tag, ',')
                            FROM tag t WHERE t.headline_id = h.id)
                  FROM headline h JOIN file f ON h.file_id = f.id
                  WHERE %s
                  ORDER BY f.path, h.position
                  LIMIT ?"
                 (mapconcat #'identity (nreverse where) " AND ")))
           (params (append (nreverse args) (list lim))))
      (mapcar
       (lambda (row)
         (list :file (nth 0 row)
               :line (nth 1 row)
               :level (nth 2 row)
               :title (nth 3 row)
               :todo (nth 4 row)
               :priority (nth 5 row)
               :org-id (nth 6 row)
               :tags (let ((concat (nth 7 row)))
                       (and concat (split-string concat "," t)))))
       (sqlite-select db sql params)))))

;;;###autoload
(defun nelisp-org-index-status ()
  "Return DB statistics plist:
\(:db-path :schema-version :files :headlines :tags :db-bytes)."
  (let* ((db (nelisp-org-index--db))
         (count (lambda (sql)
                  (or (car-safe (car-safe (sqlite-select db sql))) 0)))
         (size (when (file-exists-p nelisp-org-index-db-path)
                 (file-attribute-size
                  (file-attributes nelisp-org-index-db-path)))))
    (list :db-path nelisp-org-index-db-path
          :schema-version nelisp-org-index-schema-version
          :files (funcall count "SELECT COUNT(*) FROM file")
          :headlines (funcall count "SELECT COUNT(*) FROM headline")
          :tags (funcall count "SELECT COUNT(DISTINCT tag) FROM tag")
          :db-bytes (or size 0))))

(provide 'nelisp-org-index)
;;; nelisp-org-index.el ends here
