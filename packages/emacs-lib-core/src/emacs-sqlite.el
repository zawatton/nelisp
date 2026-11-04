;;; emacs-sqlite.el --- NeLisp port of Emacs sqlite.c name-bridging  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Use the standalone runtime's existing SQLite FFI table.  All public
;; definitions remain guarded so host Emacs keeps its C primitives.
;; Database handles and prepared statements are allocated only on demand.

;;; Code:

(defun emacs-sqlite--cstring (string)
  "Copy STRING as a NUL-terminated UTF-8 buffer."
  (let* ((bytes (encode-coding-string string 'utf-8 t))
         (size (length bytes))
         (pointer (alloc-bytes (1+ size) 1))
         (index 0))
    (while (< index size)
      (ptr-write-u8 pointer index (aref bytes index))
      (setq index (1+ index)))
    (ptr-write-u8 pointer size 0)
    pointer))

(defun emacs-sqlite--read (pointer size &optional textp)
  "Read SIZE bytes from POINTER, decoding UTF-8 when TEXTP is non-nil."
  (let ((bytes nil) (index 0))
    (while (< index size)
      (setq bytes (cons (ptr-read-u8 pointer index) bytes))
      (setq index (1+ index)))
    ;; Construct bytes directly: standalone's string-as-unibyte can retain
    ;; the fixed character width of an ASCII string when used before aset.
    (setq bytes (apply #'unibyte-string (nreverse bytes)))
    (if textp (decode-coding-string bytes 'utf-8 t) bytes)))

(defun emacs-sqlite--read-cstring (pointer)
  "Read the NUL-terminated UTF-8 string at POINTER."
  (let ((size 0))
    (unless (= pointer 0)
      (while (/= (ptr-read-u8 pointer size) 0)
        (setq size (1+ size))))
    (emacs-sqlite--read pointer size t)))

(defun emacs-sqlite--object-p (object)
  "Return non-nil if OBJECT wraps a SQLite database."
  (and (vectorp object) (= (length object) 2)
       (eq (aref object 0) 'emacs-sqlite-ffi)
       (integerp (aref object 1))))

(defun emacs-sqlite--handle (db)
  "Validate DB and return its live native database handle."
  (unless (emacs-sqlite--object-p db)
    (signal 'wrong-type-argument (list 'sqlitep db)))
  (when (= (aref db 1) 0)
    (signal 'sqlite-error '("Database closed")))
  (aref db 1))

(defun emacs-sqlite--errmsg (handle)
  "Return the current SQLite error message for HANDLE."
  (emacs-sqlite--read-cstring (nl-ffi-call "sqlite3_errmsg" handle)))

(defun emacs-sqlite--error (handle code)
  "Signal the GNU SQLite error corresponding to HANDLE and CODE."
  ;; The fixed FFI table does not expose sqlite3_errstr or extended_errcode.
  ;; These primary result descriptions are SQLite's stable public messages.
  (let* ((primary (logand code 255))
         (description
          (nth primary
               '("not an error" "SQL logic error" "unknown error"
                 "access permission denied" "query aborted" "database is locked"
                 "database table is locked" "out of memory" "attempt to write a readonly database"
                 "interrupted" "disk I/O error" "database disk image is malformed"
                 "unknown operation" "database or disk is full" "unable to open database file"
                 "locking protocol" "unknown error" "database schema has changed"
                 "string or blob too big" "constraint failed" "datatype mismatch"
                 "bad parameter or other API misuse" "unknown error" "authorization denied"
                 "unknown error" "column index out of range" "file is not a database"))))
    (signal 'sqlite-error
            (list (list (or description "unknown error")
                        (emacs-sqlite--errmsg handle) primary code)))))

(defun emacs-sqlite--prepare (handle query)
  "Prepare the first statement of QUERY on HANDLE."
  (unless (stringp query)
    (signal 'wrong-type-argument (list 'stringp query)))
  (let ((slot (alloc-bytes 8 8)))
    (ptr-write-u64 slot 0 0)
    (let ((code (nl-ffi-call "sqlite3_prepare_v2" handle
                             (emacs-sqlite--cstring query) -1 slot 0)))
      (unless (= code 0) (emacs-sqlite--error handle code)))
    (ptr-read-u64 slot 0)))

(defun emacs-sqlite--bind (handle statement values)
  "Bind list or vector VALUES to STATEMENT on HANDLE."
  (unless (or (listp values) (vectorp values))
    (signal 'sqlite-error '("VALUES must be a list or a vector")))
  (let ((size (length values)) (index 0) (tail values))
    (while (< index size)
      (let* ((value (if (vectorp values) (aref values index) (car tail)))
             (position (1+ index))
             (code
              (cond
               ((null value) (nl-ffi-call "sqlite3_bind_null" statement position))
               ((eq value t) (nl-ffi-call "sqlite3_bind_int64" statement position 1))
               ((integerp value) (nl-ffi-call "sqlite3_bind_int64" statement position value))
               ((floatp value) (nl-ffi-call "sqlite3_bind_double" statement position value))
               ((stringp value)
                (let ((bytes (encode-coding-string value 'utf-8 t)))
                  (nl-ffi-call "sqlite3_bind_text" statement position
                               (emacs-sqlite--cstring value) (length bytes) -1)))
               (t (signal 'sqlite-error '("invalid argument"))))))
        (unless (= code 0)
          (signal 'sqlite-error (list (emacs-sqlite--errmsg handle)))))
      (unless (vectorp values) (setq tail (cdr tail)))
      (setq index (1+ index)))))

(defun emacs-sqlite--column (statement column)
  "Read COLUMN from the current row of STATEMENT."
  (let ((type (nl-ffi-call "sqlite3_column_type" statement column)))
    (cond
     ((= type 5) nil)
     ((= type 1) (nl-ffi-call "sqlite3_column_int64" statement column))
     ((= type 2) (nl-ffi-call "sqlite3_column_double" statement column))
     (t
      (let* ((textp (= type 3))
             (pointer (nl-ffi-call (if textp "sqlite3_column_text" "sqlite3_column_blob")
                                   statement column))
             (size (nl-ffi-call "sqlite3_column_bytes" statement column)))
        (emacs-sqlite--read pointer size textp))))))

(defun emacs-sqlite--query (db query values return-type executep)
  "Run QUERY on DB, binding VALUES and returning rows or a change count.
RETURN-TYPE controls column names; EXECUTEP enables change counts."
  (let* ((handle (emacs-sqlite--handle db))
         (statement (emacs-sqlite--prepare handle query)))
    (unwind-protect
        (progn
          (when values (emacs-sqlite--bind handle statement values))
          ;; The iterator primitives live in another package and currently
          ;; reject all set objects.  Do not substitute eager rows for a set.
          (when (eq return-type 'set)
            (signal 'sqlite-error '("SQLite result sets require iterator support")))
          (if (= statement 0)
              (if executep (signal 'sqlite-error '("not an error"))
                (if (eq return-type 'full) '(nil) nil))
            (let ((columns (nl-ffi-call "sqlite3_column_count" statement))
                  (names nil) (rows nil) (index 0) code)
              (when (eq return-type 'full)
                (while (< index columns)
                  (setq names
                        (cons (emacs-sqlite--read-cstring
                               (nl-ffi-call "sqlite3_column_name" statement index)) names))
                  (setq index (1+ index)))
                (setq names (nreverse names)))
              (setq code (nl-ffi-call "sqlite3_step" statement))
              (while (= code 100)
                (let ((row nil))
                  (setq index 0)
                  (while (< index columns)
                    (setq row (cons (emacs-sqlite--column statement index) row))
                    (setq index (1+ index)))
                  (setq rows (cons (nreverse row) rows)))
                (setq code (nl-ffi-call "sqlite3_step" statement)))
              (when (and executep (/= code 101))
                (signal 'sqlite-error (list (emacs-sqlite--errmsg handle))))
              (cond
               ((and executep (= columns 0)) (nl-ffi-call "sqlite3_changes" handle))
               ((eq return-type 'full) (cons names (nreverse rows)))
               (t (nreverse rows))))))
      (unless (= statement 0) (nl-ffi-call "sqlite3_finalize" statement)))))

(defun emacs-sqlite--exec (db query)
  "Run QUERY on DB and return whether SQLite accepted it."
  (let ((handle (emacs-sqlite--handle db)))
    (= (nl-ffi-call "sqlite3_exec" handle (emacs-sqlite--cstring query) 0 0 0) 0)))

(unless (fboundp 'sqlite-open)
  (defun sqlite-open (&optional file readonly disable-uri)
    "Open FILE as a SQLite database, or an in-memory database when nil.
READONLY opens an existing file without writing; DISABLE-URI disables URIs."
    (unless (or (null file) (stringp file))
      (signal 'wrong-type-argument (list 'stringp file)))
    (let ((slot (alloc-bytes 8 8)))
      (ptr-write-u64 slot 0 0)
      (let* ((flags (+ (if readonly 1 6) (if disable-uri 0 64)))
             (code (nl-ffi-call "sqlite3_open_v2"
                                (emacs-sqlite--cstring (or file ":memory:")) slot flags 0))
             (handle (ptr-read-u64 slot 0)))
        (if (= code 0) (vector 'emacs-sqlite-ffi handle)
          (unless (= handle 0) (nl-ffi-call "sqlite3_close_v2" handle))
          nil)))))

(unless (fboundp 'sqlite-close)
  (defun sqlite-close (db)
    "Close the SQLite database DB and return t, including repeated closes."
    (unless (emacs-sqlite--object-p db)
      (signal 'wrong-type-argument (list 'sqlitep db)))
    (unless (= (aref db 1) 0)
      (let ((code (nl-ffi-call "sqlite3_close_v2" (aref db 1))))
        (unless (= code 0) (emacs-sqlite--error (aref db 1) code)))
      (aset db 1 0))
    t))

(unless (fboundp 'sqlite-execute)
  (defun sqlite-execute (db query &optional values)
    "Execute QUERY on DB, binding VALUES; return affected rows or query rows."
    (emacs-sqlite--query db query values nil t)))

(unless (fboundp 'sqlite-select)
  (defun sqlite-select (db query &optional values return-type)
    "Return QUERY's rows from DB, binding optional VALUES.
With RETURN-TYPE `full', prepend the column names.
Result-set mode requires iterator support from the SQLite core package."
    (emacs-sqlite--query db query values return-type nil)))

(unless (fboundp 'sqlitep)
  (defun sqlitep (object)
    "Return t if OBJECT is a SQLite database, including a closed database."
    (and (emacs-sqlite--object-p object) t)))

(unless (fboundp 'sqlite-pragma)
  (defun sqlite-pragma (db pragma-clause)
    "Execute PRAGMA-CLAUSE in DB and return whether it succeeded."
    (emacs-sqlite--handle db)
    (unless (stringp pragma-clause)
      (signal 'wrong-type-argument (list 'stringp pragma-clause)))
    (emacs-sqlite--exec db (concat "PRAGMA " pragma-clause))))

(unless (fboundp 'sqlite-transaction)
  (defun sqlite-transaction (db)
    "Begin a transaction on DB and return whether it succeeded."
    (emacs-sqlite--exec db "BEGIN TRANSACTION")))

(unless (fboundp 'sqlite-commit)
  (defun sqlite-commit (db)
    "Commit the transaction on DB and return whether it succeeded."
    (emacs-sqlite--exec db "COMMIT TRANSACTION")))

(unless (fboundp 'sqlite-rollback)
  (defun sqlite-rollback (db)
    "Roll back the transaction on DB and return whether it succeeded."
    (emacs-sqlite--exec db "ROLLBACK TRANSACTION")))

(unless (fboundp 'sqlite-available-p)
  (defun sqlite-available-p ()
    "Return t when the runtime's SQLite FFI is available."
    (and (fboundp 'nl-ffi-call)
         (condition-case nil
             (let ((version (nl-ffi-call "sqlite3_libversion")))
               (and (integerp version) (/= version 0) t))
           (error nil)))))

(unless (fboundp 'sqlite-supports-trigram-p)
  (defun sqlite-supports-trigram-p (&optional db)
    "NeLisp does not yet expose a trigram-presence probe; return nil so
callers fall back to the unicode61 / porter tokenizer."
    (ignore db)
    nil))


(provide 'emacs-sqlite)

;;; emacs-sqlite.el ends here
