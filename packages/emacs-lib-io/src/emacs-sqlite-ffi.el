;;; emacs-sqlite-ffi.el --- Emacs sqlite-* API over the reader's nl-ffi-call  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Layer 2 IO adapter: the Emacs 29+ `sqlite-*' surface (`sqlite-open',
;; `sqlite-execute', `sqlite-select' with its `full' / `set' return
;; types, `sqlite-next' / `sqlite-more-p' / `sqlite-finalize',
;; `sqlite-pragma', the transaction trio, `sqlitep', `sqlite-version'),
;; implemented on the NeLisp standalone reader's name-dispatch FFI.
;;
;; v2 (2026-09-04, NeLisp v1.2.0).  The previous version of this file
;; spoke the Rust-era `nl-ffi-call LIBRARY FUNCTION SIGNATURE ARGS...'
;; contract against `libnelisp_runtime.so', a JSON-shaped query
;; protocol and a shared-library path resolver -- all of which went with
;; the Rust runtime (nelisp b76cc5ea, 2026-06-02).  The v1.2.0 reader
;; instead exposes `(nl-ffi-call NAME ARG...)' over a declarative table
;; baked at build time; the `sqlite3_*' rows live in
;; nelisp/scripts/nelisp-standalone-build.el under the SONAME
;; `libsqlite3.so.0' (mapped to the inbox `winsqlite3.dll' on
;; windows-x86_64; live under NELISP_READER_DYNAMIC on Linux).  See Doc
;; 138 of that repository, "SQLite arm".
;;
;; Marshalling, as the FFI table documents it: handles (sqlite3*,
;; sqlite3_stmt*) and C strings are i64 addresses; a string is copied
;; NUL-terminated into `alloc-bytes' memory; out parameters are an
;; 8-byte `alloc-bytes' slot read back with `ptr-read-u64'; text columns
;; are read over `sqlite3_column_bytes' and decoded as UTF-8.  The bulk
;; byte moves use the reader's `ptr-write-bytes' / `ptr-read-bytes' when
;; present (v1.2.0 + Doc 138 SQLite arm) and fall back to a per-byte
;; `ptr-write-u8' / `ptr-read-u8' walk; the UTF-8 step uses
;; `string-as-unibyte' / `string-as-multibyte', which are native on the
;; reader where `encode-coding-string' / `decode-coding-string' are
;; identities.  `alloc-bytes' has no free, so each call leaks its
;; scratch for the process lifetime.
;;
;; Host Emacs: this file is inert.  Loading it makes no FFI call, and
;; `emacs-sqlite-ffi-install' refuses to shadow a `sqlite-open' that is
;; a C subr.  The install runs automatically at load only when the FFI
;; backend is present and no real `sqlite-open' is.

;;; Code:

;; Reader builtins (nelisp/scripts/nelisp-standalone-build.el); absent
;; under host Emacs, which is why every use sits behind `fboundp'.
(declare-function nl-ffi-call "ext:nelisp-standalone-build" (name &rest args))
(declare-function alloc-bytes "ext:nelisp-standalone-build" (size align))
(declare-function ptr-read-u8 "ext:nelisp-standalone-build" (address offset))
(declare-function ptr-write-u8 "ext:nelisp-standalone-build" (address offset byte))
(declare-function ptr-read-u64 "ext:nelisp-standalone-build" (address offset))
(declare-function ptr-read-bytes "ext:nelisp-standalone-build" (address n))
(declare-function ptr-write-bytes "ext:nelisp-standalone-build" (address string))

;;;; --- constants --------------------------------------------------------

(defconst emacs-sqlite-ffi--tag 'emacs-sqlite-ffi-db
  "Head symbol of the database object `(TAG HANDLE FILE)'.")

(defconst emacs-sqlite-ffi--stmt-tag 'emacs-sqlite-ffi-stmt
  "Head symbol of the statement object returned for RETURN-TYPE `set'.
Shape: `(TAG STMT-HANDLE COLUMN-NAMES DB EOF-CELL)'.")

(defconst emacs-sqlite-ffi--open-flags 6
  "SQLITE_OPEN_READWRITE (2) | SQLITE_OPEN_CREATE (4).")

(defconst emacs-sqlite-ffi--ok 0 "SQLITE_OK.")
(defconst emacs-sqlite-ffi--row 100 "SQLITE_ROW.")
(defconst emacs-sqlite-ffi--done 101 "SQLITE_DONE.")
(defconst emacs-sqlite-ffi--transient -1
  "SQLITE_TRANSIENT, the destructor sentinel (void*)-1, as an i64.")

(defconst emacs-sqlite-ffi--type-integer 1 "SQLITE_INTEGER.")
(defconst emacs-sqlite-ffi--type-float 2 "SQLITE_FLOAT.")
(defconst emacs-sqlite-ffi--type-text 3 "SQLITE_TEXT.")
(defconst emacs-sqlite-ffi--type-blob 4 "SQLITE_BLOB.")

;; Emacs's own condition names, so callers' `condition-case' clauses
;; written for host Emacs keep matching here.
(unless (get 'sqlite-error 'error-conditions)
  (put 'sqlite-error 'error-conditions '(sqlite-error error))
  (put 'sqlite-error 'error-message "SQLite error"))
(unless (get 'sqlite-locked-error 'error-conditions)
  (put 'sqlite-locked-error 'error-conditions
       '(sqlite-locked-error sqlite-error error))
  (put 'sqlite-locked-error 'error-message "SQLite locked error"))

;;;; --- backend presence --------------------------------------------------

(defun emacs-sqlite-ffi-backend-p ()
  "Return non-nil when the reader's FFI and pointer primitives are present.
Says nothing about whether the sqlite3 rows are in the table -- see
`emacs-sqlite-ffi-available-p' for that."
  (and (fboundp 'nl-ffi-call)
       (fboundp 'alloc-bytes)
       (fboundp 'ptr-read-u64)
       (fboundp 'ptr-read-u8)
       (fboundp 'ptr-write-u8)))

(defun emacs-sqlite-ffi-available-p ()
  "Return non-nil when `nl-ffi-call' resolves the sqlite3 rows.
An unknown name answers nil on the dynamic reader and signals on the
static one; both mean \"no backend\"."
  (and (emacs-sqlite-ffi-backend-p)
       (condition-case nil
           (let ((p (nl-ffi-call "sqlite3_libversion")))
             (and (integerp p) (> p 0)))
         (error nil))))

;;;; --- marshalling -------------------------------------------------------

(defun emacs-sqlite-ffi--utf8-bytes (string)
  "Return STRING's UTF-8 bytes as a unibyte string.
A string whose byte length equals its character length is pure ASCII
or already raw bytes and is returned as is.  `multibyte-string-p' is
not consulted: the reader answers t for every string."
  (cond
   ((= (string-bytes string) (length string)) string)
   ((fboundp 'string-as-unibyte) (funcall 'string-as-unibyte string))
   (t (encode-coding-string string 'utf-8 t))))

(defun emacs-sqlite-ffi--write-bytes (address bytes)
  "Copy the unibyte string BYTES to ADDRESS; return its length."
  (if (fboundp 'ptr-write-bytes)
      (ptr-write-bytes address bytes)
    (let ((n (length bytes))
          (i 0))
      (while (< i n)
        (ptr-write-u8 address i (aref bytes i))
        (setq i (1+ i)))
      n)))

(defun emacs-sqlite-ffi--cstring (string)
  "Copy STRING NUL-terminated as UTF-8 into fresh memory; return its address."
  (let* ((bytes (emacs-sqlite-ffi--utf8-bytes string))
         (n (length bytes))
         (p (alloc-bytes (1+ n) 1)))
    (emacs-sqlite-ffi--write-bytes p bytes)
    (ptr-write-u8 p n 0)
    p))

(defconst emacs-sqlite-ffi--read-chunk 512
  "Bytes gathered per `unibyte-string' call in `emacs-sqlite-ffi--read-bytes'.
Keeps the `apply' argument list bounded on multi-kilobyte text columns.")

(defun emacs-sqlite-ffi--read-bytes (address n)
  "Return the N bytes at ADDRESS as a unibyte string."
  (cond
   ((<= n 0) "")
   ((fboundp 'ptr-read-bytes) (ptr-read-bytes address n))
   (t
    (let ((parts nil)
          (i 0))
      (while (< i n)
        (let ((chunk nil)
              (end (min n (+ i emacs-sqlite-ffi--read-chunk))))
          (while (< i end)
            (push (ptr-read-u8 address i) chunk)
            (setq i (1+ i)))
          (push (apply #'unibyte-string (nreverse chunk)) parts)))
      (apply #'concat (nreverse parts))))))

(defun emacs-sqlite-ffi--decode (unibyte)
  "Decode the unibyte UTF-8 string UNIBYTE into a multibyte string."
  (if (fboundp 'string-as-multibyte)
      (funcall 'string-as-multibyte unibyte)
    (decode-coding-string unibyte 'utf-8 t)))

(defun emacs-sqlite-ffi--read-cstring (address)
  "Return the NUL-terminated UTF-8 string at ADDRESS, or nil for NULL."
  (if (or (null address) (= address 0))
      nil
    (let ((n 0))
      (while (/= (ptr-read-u8 address n) 0)
        (setq n (1+ n)))
      (emacs-sqlite-ffi--decode (emacs-sqlite-ffi--read-bytes address n)))))

;;;; --- database object ---------------------------------------------------

(defun emacs-sqlite-ffi-sqlitep (object)
  "Return non-nil when OBJECT is a database opened by this adapter."
  (and (consp object)
       (eq (car object) emacs-sqlite-ffi--tag)
       (consp (cdr object))))

(defun emacs-sqlite-ffi--handle (db)
  "Return DB's live sqlite3* handle, signalling when DB is not one."
  (unless (emacs-sqlite-ffi-sqlitep db)
    (signal 'wrong-type-argument (list 'sqlitep db)))
  (let ((h (nth 1 db)))
    (when (or (null h) (= h 0))
      (signal 'sqlite-error (list "Database is closed" (nth 2 db))))
    h))

(defun emacs-sqlite-ffi--errmsg (handle)
  "Return sqlite3_errmsg for HANDLE as a string."
  (or (emacs-sqlite-ffi--read-cstring (nl-ffi-call "sqlite3_errmsg" handle))
      "unknown sqlite error"))

(defun emacs-sqlite-ffi--signal (handle what)
  "Signal `sqlite-error' with the message for HANDLE and WHAT (a statement)."
  (signal 'sqlite-error (list (emacs-sqlite-ffi--errmsg handle) what)))

(defun emacs-sqlite-ffi-open (&optional file)
  "Open the database FILE (`:memory:' when nil or empty) read-write, creating it."
  (let* ((name (if (and (stringp file) (> (length file) 0))
                   (expand-file-name file)
                 ":memory:"))
         (slot (alloc-bytes 8 8))
         (rc (nl-ffi-call "sqlite3_open_v2"
                          (emacs-sqlite-ffi--cstring name)
                          slot emacs-sqlite-ffi--open-flags 0))
         (h (ptr-read-u64 slot 0)))
    (unless (= rc emacs-sqlite-ffi--ok)
      (let ((msg (and (/= h 0) (emacs-sqlite-ffi--errmsg h))))
        (when (/= h 0)
          (nl-ffi-call "sqlite3_close_v2" h))
        (signal 'sqlite-error
                (list (or msg (format "sqlite3_open_v2 failed (rc=%d)" rc))
                      name))))
    (list emacs-sqlite-ffi--tag h name)))

(defun emacs-sqlite-ffi-close (db)
  "Close DB.  Closing twice is harmless."
  (unless (emacs-sqlite-ffi-sqlitep db)
    (signal 'wrong-type-argument (list 'sqlitep db)))
  (let ((h (nth 1 db)))
    (when (and h (/= h 0))
      (nl-ffi-call "sqlite3_close_v2" h)
      (setcar (cdr db) 0)))
  nil)

;;;; --- statements --------------------------------------------------------

(defun emacs-sqlite-ffi--prepare (handle sql)
  "Compile SQL on HANDLE; return the sqlite3_stmt* or nil for an empty SQL."
  (let* ((slot (alloc-bytes 8 8))
         (rc (nl-ffi-call "sqlite3_prepare_v2" handle
                          (emacs-sqlite-ffi--cstring sql) -1 slot 0))
         (stmt (ptr-read-u64 slot 0)))
    (unless (= rc emacs-sqlite-ffi--ok)
      (emacs-sqlite-ffi--signal handle sql))
    (and (/= stmt 0) stmt)))

(defun emacs-sqlite-ffi--bind (handle stmt values)
  "Bind VALUES (a list or vector) to STMT's parameters, 1-based."
  (let ((i 1))
    (dolist (v (if (vectorp values) (append values nil) values))
      (let ((rc (cond
                 ((null v) (nl-ffi-call "sqlite3_bind_null" stmt i))
                 ((integerp v) (nl-ffi-call "sqlite3_bind_int64" stmt i v))
                 ((floatp v) (nl-ffi-call "sqlite3_bind_double" stmt i v))
                 ((stringp v)
                  (let ((bytes (emacs-sqlite-ffi--utf8-bytes v)))
                    (nl-ffi-call "sqlite3_bind_text" stmt i
                                 (emacs-sqlite-ffi--cstring bytes)
                                 (length bytes)
                                 emacs-sqlite-ffi--transient)))
                 (t (nl-ffi-call "sqlite3_finalize" stmt)
                    (signal 'wrong-type-argument
                            (list '(or null integer float string) v))))))
        (unless (= rc emacs-sqlite-ffi--ok)
          (nl-ffi-call "sqlite3_finalize" stmt)
          (emacs-sqlite-ffi--signal handle (list 'bind i v))))
      (setq i (1+ i)))))

(defun emacs-sqlite-ffi--column (stmt index)
  "Return column INDEX of STMT's current row as a Lisp value."
  (let ((type (nl-ffi-call "sqlite3_column_type" stmt index)))
    (cond
     ((= type emacs-sqlite-ffi--type-integer)
      (nl-ffi-call "sqlite3_column_int64" stmt index))
     ((= type emacs-sqlite-ffi--type-float)
      (nl-ffi-call "sqlite3_column_double" stmt index))
     ((= type emacs-sqlite-ffi--type-text)
      (let ((p (nl-ffi-call "sqlite3_column_text" stmt index))
            (n (nl-ffi-call "sqlite3_column_bytes" stmt index)))
        (if (or (= p 0) (<= n 0))
            ""
          (emacs-sqlite-ffi--decode (emacs-sqlite-ffi--read-bytes p n)))))
     ((= type emacs-sqlite-ffi--type-blob)
      (let ((p (nl-ffi-call "sqlite3_column_blob" stmt index))
            (n (nl-ffi-call "sqlite3_column_bytes" stmt index)))
        (if (or (= p 0) (<= n 0))
            ""
          (emacs-sqlite-ffi--read-bytes p n))))
     (t nil))))

(defun emacs-sqlite-ffi--row (stmt ncols)
  "Return STMT's current row as a list of NCOLS values."
  (let ((acc nil)
        (i 0))
    (while (< i ncols)
      (push (emacs-sqlite-ffi--column stmt i) acc)
      (setq i (1+ i)))
    (nreverse acc)))

(defun emacs-sqlite-ffi--column-names (stmt ncols)
  "Return STMT's NCOLS column names as strings."
  (let ((acc nil)
        (i 0))
    (while (< i ncols)
      (push (or (emacs-sqlite-ffi--read-cstring
                 (nl-ffi-call "sqlite3_column_name" stmt i))
                "")
            acc)
      (setq i (1+ i)))
    (nreverse acc)))

(defun emacs-sqlite-ffi--step (handle stmt what)
  "Step STMT; return `row', `done', or signal with WHAT for context."
  (let ((rc (nl-ffi-call "sqlite3_step" stmt)))
    (cond
     ((= rc emacs-sqlite-ffi--row) 'row)
     ((= rc emacs-sqlite-ffi--done) 'done)
     (t (nl-ffi-call "sqlite3_finalize" stmt)
        (emacs-sqlite-ffi--signal handle what)))))

;;;; --- Emacs API ---------------------------------------------------------

(defun emacs-sqlite-ffi-execute (db query &optional values)
  "Execute QUERY on DB with VALUES bound; return the number of rows changed.
Rows a statement may return (e.g. RETURNING) are drained and dropped,
as `sqlite-execute' does."
  (let* ((h (emacs-sqlite-ffi--handle db))
         (stmt (emacs-sqlite-ffi--prepare h query)))
    (if (null stmt)
        0
      (emacs-sqlite-ffi--bind h stmt values)
      (while (eq (emacs-sqlite-ffi--step h stmt query) 'row))
      (nl-ffi-call "sqlite3_finalize" stmt)
      (nl-ffi-call "sqlite3_changes" h))))

(defun emacs-sqlite-ffi-select (db query &optional values return-type)
  "Run QUERY on DB with VALUES bound; return rows per RETURN-TYPE.
nil: a list of rows (each a list of values).  `full': that list with the
column-name list consed in front.  `set': a statement object to walk
with `sqlite-next' / `sqlite-more-p' / `sqlite-columns' and release with
`sqlite-finalize'."
  (let* ((h (emacs-sqlite-ffi--handle db))
         (stmt (emacs-sqlite-ffi--prepare h query)))
    (if (null stmt)
        (if (eq return-type 'set) nil nil)
      (emacs-sqlite-ffi--bind h stmt values)
      (let* ((ncols (nl-ffi-call "sqlite3_column_count" stmt))
             (names (emacs-sqlite-ffi--column-names stmt ncols)))
        (if (eq return-type 'set)
            (list emacs-sqlite-ffi--stmt-tag stmt names db (list nil))
          (let ((rows nil))
            (while (eq (emacs-sqlite-ffi--step h stmt query) 'row)
              (push (emacs-sqlite-ffi--row stmt ncols) rows))
            (nl-ffi-call "sqlite3_finalize" stmt)
            (setq rows (nreverse rows))
            (if (eq return-type 'full)
                (cons names rows)
              rows)))))))

(defun emacs-sqlite-ffi--stmt-p (object)
  "Return non-nil when OBJECT is a `set' statement from this adapter."
  (and (consp object) (eq (car object) emacs-sqlite-ffi--stmt-tag)))

(defun emacs-sqlite-ffi-more-p (set)
  "Return non-nil while SET has not reached its end.
As in Emacs this is true until a `sqlite-next' call steps past the last
row, so an empty result still answers t before the first `sqlite-next'."
  (unless (emacs-sqlite-ffi--stmt-p set)
    (signal 'wrong-type-argument (list 'sqlite-set set)))
  (not (car (nth 4 set))))

(defun emacs-sqlite-ffi-next (set)
  "Return SET's next row as a list, or nil once it is exhausted."
  (unless (emacs-sqlite-ffi--stmt-p set)
    (signal 'wrong-type-argument (list 'sqlite-set set)))
  (let ((stmt (nth 1 set))
        (eof (nth 4 set)))
    (if (or (car eof) (= stmt 0))
        nil
      (let ((h (emacs-sqlite-ffi--handle (nth 3 set))))
        (if (eq (emacs-sqlite-ffi--step h stmt 'next) 'row)
            (emacs-sqlite-ffi--row stmt (length (nth 2 set)))
          (setcar eof t)
          nil)))))

(defun emacs-sqlite-ffi-columns (set)
  "Return SET's column names."
  (unless (emacs-sqlite-ffi--stmt-p set)
    (signal 'wrong-type-argument (list 'sqlite-set set)))
  (nth 2 set))

(defun emacs-sqlite-ffi-finalize (set)
  "Release SET's statement.  Finalizing twice is harmless."
  (unless (emacs-sqlite-ffi--stmt-p set)
    (signal 'wrong-type-argument (list 'sqlite-set set)))
  (let ((stmt (nth 1 set)))
    (when (/= stmt 0)
      (nl-ffi-call "sqlite3_finalize" stmt)
      (setcar (cdr set) 0)
      (setcar (nth 4 set) t)))
  nil)

(defun emacs-sqlite-ffi-pragma (db pragma)
  "Execute PRAGMA (a clause such as \"journal_mode = wal\") on DB; return t."
  (emacs-sqlite-ffi-execute db (concat "PRAGMA " pragma))
  t)

(defun emacs-sqlite-ffi-transaction (db)
  "Begin a transaction on DB; return t."
  (emacs-sqlite-ffi-execute db "BEGIN")
  t)

(defun emacs-sqlite-ffi-commit (db)
  "Commit DB's open transaction; return t."
  (emacs-sqlite-ffi-execute db "COMMIT")
  t)

(defun emacs-sqlite-ffi-rollback (db)
  "Roll back DB's open transaction; return t."
  (emacs-sqlite-ffi-execute db "ROLLBACK")
  t)

(defun emacs-sqlite-ffi-version ()
  "Return the linked SQLite library's version string."
  (emacs-sqlite-ffi--read-cstring (nl-ffi-call "sqlite3_libversion")))

;;;; --- install -----------------------------------------------------------

(defconst emacs-sqlite-ffi--aliases
  '((sqlite-available-p . emacs-sqlite-ffi-available-p)
    (sqlite-open        . emacs-sqlite-ffi-open)
    (sqlite-close       . emacs-sqlite-ffi-close)
    (sqlite-execute     . emacs-sqlite-ffi-execute)
    (sqlite-select      . emacs-sqlite-ffi-select)
    (sqlite-next        . emacs-sqlite-ffi-next)
    (sqlite-more-p      . emacs-sqlite-ffi-more-p)
    (sqlite-columns     . emacs-sqlite-ffi-columns)
    (sqlite-finalize    . emacs-sqlite-ffi-finalize)
    (sqlite-pragma      . emacs-sqlite-ffi-pragma)
    (sqlite-transaction . emacs-sqlite-ffi-transaction)
    (sqlite-commit      . emacs-sqlite-ffi-commit)
    (sqlite-rollback    . emacs-sqlite-ffi-rollback)
    (sqlitep            . emacs-sqlite-ffi-sqlitep)
    (sqlite-version     . emacs-sqlite-ffi-version))
  "Emacs name -> adapter implementation installed by `emacs-sqlite-ffi-install'.")

(defun emacs-sqlite-ffi--host-sqlite-p ()
  "Return non-nil when a real (C subr) `sqlite-open' is present."
  (and (fboundp 'sqlite-open)
       (subrp (symbol-function 'sqlite-open))))

(defun emacs-sqlite-ffi-install (&optional force)
  "Install the `sqlite-*' Emacs API on top of this adapter.
Does nothing, returning nil, without the FFI backend or when the host
already has a C `sqlite-open' -- unless FORCE.  Returns t when the
aliases were installed."
  (when (and (emacs-sqlite-ffi-backend-p)
             (or force (not (emacs-sqlite-ffi--host-sqlite-p))))
    (dolist (pair emacs-sqlite-ffi--aliases)
      (defalias (car pair) (cdr pair)))
    t))

;; Layer 2's `emacs-sqlite.el' forwarders to the retired `nelisp-sqlite-*'
;; names are what the reader carries at this point; replace them.  Host
;; Emacs keeps its subrs.
(emacs-sqlite-ffi-install)

(provide 'emacs-sqlite-ffi)
;;; emacs-sqlite-ffi.el ends here
