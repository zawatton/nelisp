;;; anvil-standalone-db.el --- Standalone Anvil SQLite DB tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin standalone DB tools for Anvil.  SQLite access is deliberately
;; routed through the Emacs-compatible `sqlite-*' API provided by
;; nelisp-emacs (`emacs-sqlite-ffi.el' under NeLisp standalone).

;;; Code:

(defconst anvil-standalone-db-memory-prefix "anvil-memory:db:")
(defconst anvil-standalone-db-worklog-prefix "anvil-worklog:db:")

(defconst anvil-standalone-db-memory-types
  '("user" "feedback" "project" "reference" "memo"))

(defvar anvil-standalone-db--last-error nil)

(defun anvil-standalone-db--error (message)
  "Signal MESSAGE while retaining it for standalone JSON error output."
  (setq anvil-standalone-db--last-error message)
  (error message))

(defun anvil-standalone-db--getenv (name)
  (and (fboundp 'getenv) (getenv name)))

(defun anvil-standalone-db--notes-dir ()
  (expand-file-name
   (or (anvil-standalone-db--getenv "ANVIL_NOTES_DIR")
       (anvil-standalone-db--getenv "PWD")
       default-directory
       ".")))

(defun anvil-standalone-db--memory-db-path ()
  (expand-file-name ".anvil-memory/anvil-memory-index.db"
                    (anvil-standalone-db--notes-dir)))

(defun anvil-standalone-db--worklog-db-path ()
  (expand-file-name ".anvil-worklog/anvil-worklog-index.db"
                    (anvil-standalone-db--notes-dir)))

(defun anvil-standalone-db--ensure-sqlite ()
  (unless (and (fboundp 'sqlite-open)
               (fboundp 'sqlite-execute)
               (fboundp 'sqlite-select))
    (anvil-standalone-db--error
     "sqlite-* API is unavailable; load nelisp-emacs emacs-sqlite-ffi first"))
  (when (and (fboundp 'emacs-ffi-available-p)
             (not (emacs-ffi-available-p)))
    (anvil-standalone-db--error
     "FFI backend is unavailable; nl-ffi-call is not provided by this NeLisp build"))
  (when (and (fboundp 'sqlite-available-p)
             (not (sqlite-available-p)))
    (anvil-standalone-db--error
     "sqlite-* API is present but SQLite is unavailable")))

(defun anvil-standalone-db--open (path)
  (anvil-standalone-db--ensure-sqlite)
  (let ((dir (file-name-directory path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (sqlite-open (expand-file-name path)))

(defun anvil-standalone-db--close (db)
  (when (and db (fboundp 'sqlite-close))
    (sqlite-close db)))

(defun anvil-standalone-db--trim-right (s)
  (replace-regexp-in-string "[ \t\n\r]+\\'" "" (or s "")))

(defun anvil-standalone-db--sha1 (s)
  (cond
   ((fboundp 'secure-hash) (secure-hash 'sha1 s))
   ((fboundp 'nelisp-hash-sha1) (nelisp-hash-sha1 s))
   (t (error "SHA1 support is unavailable; load nelisp-secure-hash"))))

(defun anvil-standalone-db--now ()
  (if (fboundp 'float-time)
      (truncate (float-time))
    0))

(defun anvil-standalone-db--today ()
  (if (fboundp 'format-time-string)
      (format-time-string "%Y-%m-%d")
    "1970-01-01"))

(defun anvil-standalone-db--hostname ()
  (or (anvil-standalone-db--getenv "COMPUTERNAME")
      (anvil-standalone-db--getenv "HOSTNAME")
      "unknown"))

(defun anvil-standalone-db--machine ()
  (concat (format "%s" system-type) "-" (anvil-standalone-db--hostname)))

(defun anvil-standalone-db--year-from-date (date)
  (and date (>= (length date) 4)
       (string-to-number (substring date 0 4))))

(defun anvil-standalone-db--type-valid-p (type)
  (member type anvil-standalone-db-memory-types))

(defun anvil-standalone-db--type-prefix-present-p (name)
  (let ((types anvil-standalone-db-memory-types)
        (found nil))
    (while (and types (not found))
      (setq found (string-prefix-p (concat (car types) "_") name))
      (setq types (cdr types)))
    found))

(defun anvil-standalone-db--memory-name (name type)
  (if (anvil-standalone-db--type-prefix-present-p name)
      name
    (concat type "_" name)))

(defun anvil-standalone-db--display-name (name)
  (let ((types anvil-standalone-db-memory-types)
        (s name))
    (while types
      (let ((prefix (concat (car types) "_")))
        (when (string-prefix-p prefix s)
          (setq s (substring s (length prefix)))))
      (setq types (cdr types)))
    (replace-regexp-in-string "_" " " s)))

(defun anvil-standalone-db--memory-schema (db)
  (sqlite-execute db "PRAGMA journal_mode=WAL")
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS memory_meta (
      file TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      created INTEGER NOT NULL,
      last_accessed INTEGER,
      access_count INTEGER DEFAULT 0,
      validity_prior REAL DEFAULT 0.9,
      ttl_policy TEXT,
      decay_score REAL DEFAULT 0.0,
      tags TEXT)")
  (sqlite-execute
   db "CREATE INDEX IF NOT EXISTS memory_meta_type_idx ON memory_meta(type)")
  (sqlite-execute
   db
   "CREATE VIRTUAL TABLE IF NOT EXISTS memory_body_fts
      USING fts5(file UNINDEXED, body)"))

(defun anvil-standalone-db--worklog-schema (db)
  (sqlite-execute db "PRAGMA journal_mode=WAL")
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS worklog_entry (
      file TEXT NOT NULL,
      start_line INTEGER NOT NULL,
      end_line INTEGER NOT NULL,
      machine TEXT NOT NULL,
      year INTEGER NOT NULL,
      date TEXT NOT NULL,
      title TEXT NOT NULL,
      body TEXT NOT NULL,
      digest TEXT NOT NULL,
      scanned_at INTEGER NOT NULL,
      PRIMARY KEY (file, start_line))")
  (sqlite-execute
   db "CREATE INDEX IF NOT EXISTS worklog_entry_date_idx ON worklog_entry(date)")
  (sqlite-execute
   db "CREATE INDEX IF NOT EXISTS worklog_entry_machine_idx ON worklog_entry(machine)")
  (sqlite-execute
   db
   "CREATE VIRTUAL TABLE IF NOT EXISTS worklog_fts
      USING fts5(file UNINDEXED, start_line UNINDEXED,
                 machine UNINDEXED, date UNINDEXED, title, body)"))

(defun anvil-standalone-db--row-memory (row body)
  (let* ((file (nth 0 row))
         (synthetic (string-prefix-p anvil-standalone-db-memory-prefix file))
         (name (if synthetic
                   (substring file (length anvil-standalone-db-memory-prefix))
                 (file-name-sans-extension (file-name-nondirectory file)))))
    (list :file file
          :name name
          :type (nth 1 row)
          :created (nth 2 row)
          :last-accessed (nth 3 row)
          :access-count (nth 4 row)
          :validity-prior (nth 5 row)
          :ttl-policy (nth 6 row)
          :decay-score (nth 7 row)
          :tags (nth 8 row)
          :description (anvil-standalone-db--display-name name)
          :body body)))

(defun anvil-standalone-db--row-worklog (row)
  (list :file (nth 0 row)
        :start-line (nth 1 row)
        :end-line (nth 2 row)
        :machine (nth 3 row)
        :year (nth 4 row)
        :date (nth 5 row)
        :title (nth 6 row)
        :body (nth 7 row)
        :digest (nth 8 row)))

(defun anvil-standalone-db--json-escape (s)
  (let ((i 0) (n (length s)) (out ""))
    (while (< i n)
      (let ((c (aref s i)))
        (setq out
              (concat out
                      (cond
                       ((= c ?\") "\\\"")
                       ((= c ?\\) "\\\\")
                       ((= c ?\n) "\\n")
                       ((= c ?\r) "\\r")
                       ((= c ?\t) "\\t")
                       (t (char-to-string c)))))
        (setq i (+ i 1))))
    out))

(defun anvil-standalone-db--json (x)
  (cond
   ((null x) "null")
   ((eq x t) "true")
   ((stringp x) (concat "\"" (anvil-standalone-db--json-escape x) "\""))
   ((numberp x) (number-to-string x))
   ((and (consp x) (keywordp (car x)))
    (let ((parts nil) (rest x))
      (while rest
        (let ((k (substring (symbol-name (car rest)) 1))
              (v (cadr rest)))
          (push (concat (anvil-standalone-db--json k)
                        ":"
                        (anvil-standalone-db--json v))
                parts))
        (setq rest (cddr rest)))
      (concat "{" (mapconcat #'identity (nreverse parts) ",") "}")))
   ((listp x)
    (concat "[" (mapconcat #'anvil-standalone-db--json x ",") "]"))
   (t (anvil-standalone-db--json (format "%s" x)))))

(defun anvil-standalone-db--arg (args key)
  (let ((rest args) (value nil))
    (while rest
      (when (equal (car rest) key)
        (setq value (cadr rest)))
      (setq rest (cdr rest)))
    value))

(defun anvil-standalone-db--int-arg (args key default)
  (let ((s (anvil-standalone-db--arg args key)))
    (if (and s (not (string-empty-p s))) (string-to-number s) default)))

(defun anvil-standalone-db--params (args)
  (let ((rest args) (values nil))
    (while rest
      (when (equal (car rest) "--param")
        (push (cadr rest) values)
        (setq rest (cdr rest)))
      (setq rest (cdr rest)))
    (nreverse values)))

(defun anvil-standalone-db--read-text-option (args key file-key)
  (let ((text (anvil-standalone-db--arg args key))
        (file (anvil-standalone-db--arg args file-key)))
    (cond
     (text text)
     (file
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))
     (t nil))))

(defun anvil-standalone-db-memory-add (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--memory-db-path))))
         result)
    (unwind-protect
        (progn
          (anvil-standalone-db--memory-schema db)
          (let* ((name (or (anvil-standalone-db--arg args "--name")
                           (error "memory-add requires --name")))
                 (type (or (anvil-standalone-db--arg args "--type")
                           (error "memory-add requires --type")))
                 (body (or (anvil-standalone-db--read-text-option
                            args "--body" "--body-file")
                           (error "memory-add requires --body or --body-file"))))
            (unless (anvil-standalone-db--type-valid-p type)
              (error "invalid memory type: %s" type))
            (let* ((basename (anvil-standalone-db--memory-name name type))
                   (file (concat anvil-standalone-db-memory-prefix basename))
                   (exists (sqlite-select
                            db "SELECT 1 FROM memory_meta WHERE file = ?1 LIMIT 1"
                            (list file)))
                   (created (anvil-standalone-db--int-arg
                             args "--created" (anvil-standalone-db--now)))
                   (ttl (or (anvil-standalone-db--arg args "--ttl-policy") type))
                   (tags (anvil-standalone-db--arg args "--tags"))
                   (body* (anvil-standalone-db--trim-right body))
                   (digest (anvil-standalone-db--sha1 body*)))
              (when exists
                (error "memory already exists: %s" basename))
              (sqlite-execute
               db
               "INSERT INTO memory_meta
                  (file, type, created, ttl_policy, tags)
                VALUES (?1, ?2, ?3, ?4, ?5)"
               (list file type created ttl tags))
              (sqlite-execute
               db "INSERT INTO memory_body_fts(file, body) VALUES (?1, ?2)"
               (list file body*))
              (setq result
                    (list :file file :name basename :type type
                          :description (or (anvil-standalone-db--arg
                                            args "--description")
                                           (anvil-standalone-db--display-name
                                            basename))
                          :created created :digest digest)))))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-memory-get (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--memory-db-path))))
         (key (or (anvil-standalone-db--arg args "--file")
                  (anvil-standalone-db--arg args "--name")
                  (error "memory-get requires --file or --name")))
         result)
    (unwind-protect
        (let* ((file (if (string-prefix-p anvil-standalone-db-memory-prefix key)
                         key
                       (concat anvil-standalone-db-memory-prefix key)))
               (rows (sqlite-select
                      db
                      "SELECT file, type, created, last_accessed, access_count,
                              validity_prior, ttl_policy, decay_score, tags
                         FROM memory_meta WHERE file = ?1 LIMIT 1"
                      (list file))))
          (when rows
            (setq result
                  (anvil-standalone-db--row-memory
                   (car rows)
                   (or (caar (sqlite-select
                              db
                              "SELECT body FROM memory_body_fts
                                WHERE file = ?1 LIMIT 1"
                              (list file)))
                       "")))))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-memory-list (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--memory-db-path))))
         (type (anvil-standalone-db--arg args "--type"))
         (limit (anvil-standalone-db--int-arg args "--limit" 20))
         result)
    (unwind-protect
        (let* ((sql (concat
                     "SELECT file, type, created, last_accessed, access_count,
                             validity_prior, ttl_policy, decay_score, tags
                        FROM memory_meta"
                     (if type " WHERE type = ?1" "")
                     " ORDER BY created DESC LIMIT "
                     (number-to-string limit)))
               (rows (if type
                         (sqlite-select db sql (list type))
                       (sqlite-select db sql))))
          (setq result
                (mapcar (lambda (row) (anvil-standalone-db--row-memory row nil))
                        rows)))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-memory-search (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--memory-db-path))))
         (query (or (anvil-standalone-db--arg args "--query")
                    (error "memory-search requires --query")))
         (type (anvil-standalone-db--arg args "--type"))
         (limit (anvil-standalone-db--int-arg args "--limit" 20))
         result)
    (unwind-protect
        (let* ((sql (concat
                     "SELECT m.file, m.type, m.created, m.last_accessed,
                             m.access_count, m.validity_prior, m.ttl_policy,
                             m.decay_score, m.tags, memory_body_fts.body
                        FROM memory_body_fts
                        JOIN memory_meta m ON m.file = memory_body_fts.file
                       WHERE memory_body_fts MATCH ?1"
                     (if type " AND m.type = ?2" "")
                     " ORDER BY rank LIMIT "
                     (number-to-string limit)))
               (rows (if type
                         (sqlite-select db sql (list query type))
                       (sqlite-select db sql (list query)))))
          (setq result
                (mapcar (lambda (row)
                          (anvil-standalone-db--row-memory
                           (list (nth 0 row) (nth 1 row) (nth 2 row)
                                 (nth 3 row) (nth 4 row) (nth 5 row)
                                 (nth 6 row) (nth 7 row) (nth 8 row))
                           (nth 9 row)))
                        rows)))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-worklog-add (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--worklog-db-path))))
         result)
    (unwind-protect
        (progn
          (anvil-standalone-db--worklog-schema db)
          (let* ((title (or (anvil-standalone-db--arg args "--title")
                            (error "worklog-add requires --title")))
                 (body (or (anvil-standalone-db--read-text-option
                            args "--body" "--body-file")
                           (error "worklog-add requires --body or --body-file")))
                 (date (or (anvil-standalone-db--arg args "--date")
                           (anvil-standalone-db--today)))
                 (machine (or (anvil-standalone-db--arg args "--machine")
                              (anvil-standalone-db--machine)))
                 (year (anvil-standalone-db--int-arg
                        args "--year" (anvil-standalone-db--year-from-date date)))
                 (file (format "%s%s:%d"
                               anvil-standalone-db-worklog-prefix machine year))
                 (start (or (caar (sqlite-select
                                   db
                                   "SELECT COALESCE(MAX(start_line), 0) + 1
                                      FROM worklog_entry WHERE file = ?1"
                                   (list file)))
                            1))
                 (body* (anvil-standalone-db--trim-right body))
                 (digest (anvil-standalone-db--sha1 body*))
                 (now (anvil-standalone-db--now)))
            (sqlite-execute db "BEGIN")
            (condition-case err
                (progn
                  (sqlite-execute
                   db
                   "INSERT INTO worklog_entry
                      (file, start_line, end_line, machine, year, date,
                       title, body, digest, scanned_at)
                    VALUES (?1, ?2, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)"
                   (list file start machine year date title body* digest now))
                  (sqlite-execute
                   db
                   "INSERT INTO worklog_fts
                      (file, start_line, machine, date, title, body)
                    VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
                   (list file start machine date title body*))
                  (sqlite-execute db "COMMIT"))
              (error
               (ignore-errors (sqlite-execute db "ROLLBACK"))
               (signal (car err) (cdr err))))
            (setq result
                  (list :file file :start-line start :machine machine
                        :year year :date date :digest digest))))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-worklog-list (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--worklog-db-path))))
         (limit (anvil-standalone-db--int-arg args "--limit" 20))
         result)
    (unwind-protect
        (let ((rows (sqlite-select
                     db
                     (concat
                      "SELECT file, start_line, end_line, machine, year,
                              date, title, body, digest
                         FROM worklog_entry
                        ORDER BY date DESC, start_line ASC LIMIT "
                      (number-to-string limit)))))
          (setq result (mapcar #'anvil-standalone-db--row-worklog rows)))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-worklog-search (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--worklog-db-path))))
         (query (or (anvil-standalone-db--arg args "--query")
                    (error "worklog-search requires --query")))
         (limit (anvil-standalone-db--int-arg args "--limit" 20))
         result)
    (unwind-protect
        (let ((rows (sqlite-select
                     db
                     (concat
                      "SELECT e.file, e.start_line, e.end_line, e.machine,
                              e.year, e.date, e.title, e.body, e.digest
                         FROM worklog_fts
                         JOIN worklog_entry e
                           ON worklog_fts.file = e.file
                          AND worklog_fts.start_line = e.start_line
                        WHERE worklog_fts MATCH ?1
                        ORDER BY rank LIMIT "
                      (number-to-string limit))
                     (list query))))
          (setq result (mapcar #'anvil-standalone-db--row-worklog rows)))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-worklog-get (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (anvil-standalone-db--worklog-db-path))))
         (digest (anvil-standalone-db--arg args "--digest"))
         (file (anvil-standalone-db--arg args "--file"))
         (start (anvil-standalone-db--int-arg args "--start-line" nil))
         result)
    (unwind-protect
        (let ((rows
               (if digest
                   (sqlite-select
                    db
                    "SELECT file, start_line, end_line, machine, year,
                            date, title, body, digest
                       FROM worklog_entry WHERE digest = ?1 LIMIT 1"
                    (list digest))
                 (progn
                   (unless (and file start)
                     (error "worklog-get requires --digest or --file plus --start-line"))
                   (sqlite-select
                    db
                    "SELECT file, start_line, end_line, machine, year,
                            date, title, body, digest
                       FROM worklog_entry
                      WHERE file = ?1 AND start_line = ?2 LIMIT 1"
                    (list file start))))))
          (when rows
            (setq result (anvil-standalone-db--row-worklog (car rows)))))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-sqlite-select (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (error "sqlite-select requires --db"))))
         (sql (or (anvil-standalone-db--arg args "--sql")
                  (error "sqlite-select requires --sql")))
         (params (anvil-standalone-db--params args))
         result)
    (unwind-protect
        (setq result
              (if params
                  (sqlite-select db sql params)
                (sqlite-select db sql)))
      (anvil-standalone-db--close db))
    result))

(defun anvil-standalone-db-sqlite-execute (args)
  (let* ((db (anvil-standalone-db--open
              (or (anvil-standalone-db--arg args "--db")
                  (error "sqlite-execute requires --db"))))
         (sql (or (anvil-standalone-db--arg args "--sql")
                  (error "sqlite-execute requires --sql")))
         (params (anvil-standalone-db--params args)))
    (unwind-protect
        (progn
          (if params
              (sqlite-execute db sql params)
            (sqlite-execute db sql))
          (list :ok t))
      (anvil-standalone-db--close db))))

(defun anvil-standalone-db-dispatch (args)
  (let ((command (car args))
        (rest (cdr args)))
    (cond
     ((equal command "memory-add") (anvil-standalone-db-memory-add rest))
     ((equal command "memory-get") (anvil-standalone-db-memory-get rest))
     ((equal command "memory-list") (anvil-standalone-db-memory-list rest))
     ((equal command "memory-search") (anvil-standalone-db-memory-search rest))
     ((equal command "worklog-add") (anvil-standalone-db-worklog-add rest))
     ((equal command "worklog-get") (anvil-standalone-db-worklog-get rest))
     ((equal command "worklog-list") (anvil-standalone-db-worklog-list rest))
     ((equal command "worklog-search") (anvil-standalone-db-worklog-search rest))
     ((equal command "sqlite-select") (anvil-standalone-db-sqlite-select rest))
     ((equal command "sqlite-execute") (anvil-standalone-db-sqlite-execute rest))
     (t (error "unknown standalone-db command: %s" command)))))

(defun anvil-standalone-db-main (args out-file)
  (let ((json
         (condition-case err
             (anvil-standalone-db--json
              (anvil-standalone-db-dispatch args))
           (error
            (anvil-standalone-db--json
             (list :status "error"
                   :error (or anvil-standalone-db--last-error
                              (format "%S" err))))))))
    (if out-file
        (let ((text (concat json "\n")))
          (cond
           ((fboundp 'wrf)
            (wrf out-file text))
           ((fboundp 'write-region)
            (write-region text nil out-file nil 'silent))
           (t
            (with-temp-file out-file
              (insert text)))))
      json)))

(provide 'anvil-standalone-db)

;;; anvil-standalone-db.el ends here
