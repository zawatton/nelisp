;;; emacs-sqlite-ffi.el --- sqlite-* over NeLisp's direct SQLite FFI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Commentary:

;; Implement Emacs's sqlite-* surface with the sqlite3_* functions linked into
;; NeLisp standalone's fixed `nl-ffi-call' table.  Host Emacs definitions are
;; guarded and therefore retain their built-in implementation.

;;; Code:

(declare-function nl-ffi-call "ext:nelisp-runtime" (name &rest args))
(declare-function alloc-bytes "ext:nelisp-runtime" (nbytes align))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (pointer offset))
(declare-function ptr-read-u64 "ext:nelisp-runtime" (pointer offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (pointer offset value))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (pointer offset value))

(defconst emacs-sqlite-ffi--ok 0)
(defconst emacs-sqlite-ffi--row-code 100)
(defconst emacs-sqlite-ffi--done 101)

(defun emacs-sqlite-ffi--call (name &rest args)
  "Call supported SQLite function NAME with ARGS."
  (let ((signature
         (cond
          ((equal name "sqlite3_open_v2")
           (list '(:pointer :pointer :sint32 :pointer) :sint32))
          ((equal name "sqlite3_libversion")
           (list nil :pointer))
          ((equal name "sqlite3_close_v2")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_errmsg")
           (list '(:pointer) :pointer))
          ((equal name "sqlite3_prepare_v2")
           (list '(:pointer :pointer :sint32 :pointer :pointer) :sint32))
          ((equal name "sqlite3_step")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_changes")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_finalize")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_bind_int64")
           (list '(:pointer :sint32 :sint64) :sint32))
          ((equal name "sqlite3_errcode")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_extended_errcode")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_errstr")
           (list '(:sint32) :pointer))
          ((equal name "sqlite3_column_count")
           (list '(:pointer) :sint32))
          ((equal name "sqlite3_column_type")
           (list '(:pointer :sint32) :sint32))
          ((equal name "sqlite3_column_int64")
           (list '(:pointer :sint32) :sint64))
          ((equal name "sqlite3_column_text")
           (list '(:pointer :sint32) :pointer))
          ((equal name "sqlite3_column_bytes")
           (list '(:pointer :sint32) :sint32))
          ((equal name "sqlite3_column_name")
           (list '(:pointer :sint32) :pointer))
          ((equal name "sqlite3_exec")
           (list '(:pointer :pointer :pointer :pointer :pointer) :sint32)))))
    (if (null signature)
        (apply #'nl-ffi-call name args)
      (let ((fixed-result nil)
            (fixed-error nil))
        (condition-case err
            (setq fixed-result (apply #'nl-ffi-call name args))
          (nelisp-unsupported-primitive
           (setq fixed-error err)))
        (if (null fixed-error)
            fixed-result
          (let ((path (nl-ffi-loader--resolve-soname
                       "libsqlite3.so.0" nil)))
            (if (null path)
                (if fixed-error
                    (signal (car fixed-error) (cdr fixed-error))
                  fixed-result)
              (ffi:library path)
              (let ((nl-ffi--library-order (list path)))
                (nl-ffi--invoke name name
                                (car signature) (cadr signature) args)))))))))

(defun emacs-sqlite-ffi--encode (string)
  "Encode STRING as UTF-8 bytes when coding support is available."
  (if (fboundp 'encode-coding-string)
      (encode-coding-string string 'utf-8 t)
    string))

(defun emacs-sqlite-ffi--cstring (string)
  "Copy STRING to a fresh NUL-terminated native buffer."
  (let* ((bytes (emacs-sqlite-ffi--encode string))
         (size (length bytes))
         (buffer (alloc-bytes (1+ size) 1))
         (index 0))
    (while (< index size)
      (ptr-write-u8 buffer index (aref bytes index))
      (setq index (1+ index)))
    (ptr-write-u8 buffer size 0)
    buffer))

(defun emacs-sqlite-ffi--read (pointer size &optional textp)
  "Read SIZE bytes from POINTER; decode UTF-8 when TEXTP is non-nil."
  (let ((bytes "")
        (index 0))
    ;; The base standalone reader exposes byte-addressed pointer reads but
    ;; does not require the optional nl-ffi buffer convenience functions.
    (while (< index size)
      (setq bytes
            (concat bytes (unibyte-string (ptr-read-u8 pointer index))))
      (setq index (1+ index)))
    (if (and textp (fboundp 'decode-coding-string))
        (decode-coding-string bytes 'utf-8 t)
      bytes)))

(defun emacs-sqlite-ffi--read-cstring (pointer)
  "Read a NUL-terminated string from POINTER."
  (if (= pointer 0) ""
    (let ((size 0))
      (while (not (= (ptr-read-u8 pointer size) 0))
        (setq size (1+ size)))
      (emacs-sqlite-ffi--read pointer size t))))

(defun emacs-sqlite-ffi--object (handle)
  "Wrap native HANDLE as an opaque SQLite object."
  (vector 'emacs-sqlite-ffi handle))

(defun emacs-sqlite-ffi--object-p (object)
  "Return non-nil when OBJECT is an SQLite compatibility object."
  (and (vectorp object) (= (length object) 2)
       (eq (aref object 0) 'emacs-sqlite-ffi)
       (integerp (aref object 1))))

(defun emacs-sqlite-ffi--handle (database)
  "Return DATABASE's live native handle."
  (unless (emacs-sqlite-ffi--object-p database)
    (signal 'wrong-type-argument (list 'sqlitep database)))
  (let ((handle (aref database 1)))
    (when (= handle 0) (error "SQLite database is closed"))
    handle))

(defun emacs-sqlite-ffi--errmsg (handle)
  "Return SQLite's current error message for HANDLE."
  (emacs-sqlite-ffi--read-cstring
   (emacs-sqlite-ffi--call "sqlite3_errmsg" handle)))

(defun emacs-sqlite-ffi--check (code handle operation)
  "Return CODE if successful, otherwise report OPERATION on HANDLE."
  (unless (= code emacs-sqlite-ffi--ok)
    (error "%s: %s (SQLite code %d)"
           operation (emacs-sqlite-ffi--errmsg handle) code))
  code)

(defun emacs-sqlite-ffi--prepare (handle query)
  "Prepare QUERY on HANDLE and return the native statement."
  (unless (stringp query)
    (signal 'wrong-type-argument (list 'stringp query)))
  (let ((slot (alloc-bytes 8 8)))
    (ptr-write-u64 slot 0 0)
    (let ((code (emacs-sqlite-ffi--call "sqlite3_prepare_v2" handle
                                        (emacs-sqlite-ffi--cstring query)
                                        -1 slot 0)))
      (unless (= code emacs-sqlite-ffi--ok)
        (let* ((primary (emacs-sqlite-ffi--call "sqlite3_errcode" handle))
               (extended (emacs-sqlite-ffi--call
                          "sqlite3_extended_errcode" handle))
               (description
                (emacs-sqlite-ffi--read-cstring
                 (emacs-sqlite-ffi--call "sqlite3_errstr" primary)))
               (message (emacs-sqlite-ffi--errmsg handle)))
          (signal 'sqlite-error
                  (list (list description message primary extended)))))
    (let ((statement (ptr-read-u64 slot 0)))
      (when (= statement 0)
        (error "sqlite3_prepare_v2 returned no statement"))
      statement))))

(defun emacs-sqlite-ffi--value-count (values)
  "Return the number of VALUES in a list or vector."
  (cond ((null values) 0)
        ((or (listp values) (vectorp values)) (length values))
        (t (signal 'wrong-type-argument (list 'sequencep values)))))

(defun emacs-sqlite-ffi--value-at (values index)
  "Return zero-based INDEX from list or vector VALUES."
  (if (vectorp values) (aref values index) (nth index values)))

(defun emacs-sqlite-ffi--bind-one (handle statement index value)
  "Bind VALUE at one-based INDEX in STATEMENT on HANDLE."
  (let ((code
         (cond
          ((null value)
           (emacs-sqlite-ffi--call "sqlite3_bind_null" statement index))
          ((integerp value)
           (emacs-sqlite-ffi--call "sqlite3_bind_int64"
                                   statement index value))
          ((floatp value)
           (emacs-sqlite-ffi--call "sqlite3_bind_double"
                                   statement index value))
          ((stringp value)
           (let* ((bytes (emacs-sqlite-ffi--encode value))
                  (pointer (emacs-sqlite-ffi--cstring bytes)))
             (emacs-sqlite-ffi--call "sqlite3_bind_text"
                                     statement index pointer
                                     (length bytes) -1)))
          (t (signal 'wrong-type-argument
                     (list '(or null integer float string) value))))))
    (emacs-sqlite-ffi--check code handle "sqlite3_bind")))

(defun emacs-sqlite-ffi--bind-values (handle statement values)
  "Bind list or vector VALUES to STATEMENT on HANDLE."
  (let ((index 0) (count (emacs-sqlite-ffi--value-count values)))
    (while (< index count)
      (emacs-sqlite-ffi--bind-one
       handle statement (1+ index)
       (emacs-sqlite-ffi--value-at values index))
      (setq index (1+ index)))))

(defun emacs-sqlite-ffi--column-string (statement column textp)
  "Read text or blob COLUMN from STATEMENT."
  (let ((size (emacs-sqlite-ffi--call
               "sqlite3_column_bytes" statement column))
        (pointer (emacs-sqlite-ffi--call
                  (if textp "sqlite3_column_text" "sqlite3_column_blob")
                  statement column)))
    (if (= pointer 0) nil
      (emacs-sqlite-ffi--read pointer size textp))))

(defun emacs-sqlite-ffi--column-value (statement column)
  "Decode COLUMN from the current row of STATEMENT."
  (let ((type (emacs-sqlite-ffi--call
               "sqlite3_column_type" statement column)))
    (cond
     ((= type 5) nil)
     ((= type 1)
      (emacs-sqlite-ffi--call "sqlite3_column_int64" statement column))
     ((= type 2)
      (emacs-sqlite-ffi--call "sqlite3_column_double" statement column))
     ((= type 3) (emacs-sqlite-ffi--column-string statement column t))
     ((= type 4) (emacs-sqlite-ffi--column-string statement column nil))
     (t (error "Unknown SQLite column type %d" type)))))

(defun emacs-sqlite-ffi--row (statement column-count)
  "Decode the current STATEMENT row."
  (let ((column 0) (row nil))
    (while (< column column-count)
      (setq row (cons (emacs-sqlite-ffi--column-value statement column) row))
      (setq column (1+ column)))
    (nreverse row)))

(defun emacs-sqlite-ffi--column-names (statement column-count)
  "Return COLUMN-COUNT column names for STATEMENT."
  (let ((column 0) (names nil))
    (while (< column column-count)
      (setq names
            (cons (emacs-sqlite-ffi--read-cstring
                   (emacs-sqlite-ffi--call
                    "sqlite3_column_name" statement column))
                  names))
      (setq column (1+ column)))
    (nreverse names)))

(unless (fboundp 'sqlite-available-p)
  (defun sqlite-available-p ()
    "Return non-nil when the standalone SQLite FFI is usable."
    (and (fboundp 'nl-ffi-call)
         (condition-case nil
             (let ((database (sqlite-open nil)))
               (sqlite-close database)
               t)
           (error nil)))))

(unless (fboundp 'sqlite-open)
  (defun sqlite-open (&optional file _readonly _disable-uri)
    "Open FILE as an SQLite database, or an in-memory database when nil."
    (unless (or (null file) (stringp file))
      (signal 'wrong-type-argument (list 'stringp file)))
    (let ((slot (alloc-bytes 8 8)))
      (ptr-write-u64 slot 0 0)
      (let* ((code (emacs-sqlite-ffi--call
                    "sqlite3_open_v2"
                    (emacs-sqlite-ffi--cstring (or file ":memory:"))
                    slot 6 0))
             (handle (ptr-read-u64 slot 0)))
        (if (= code emacs-sqlite-ffi--ok)
            (emacs-sqlite-ffi--object handle)
          (when (not (= handle 0))
            (emacs-sqlite-ffi--call "sqlite3_close_v2" handle))
          nil)))))

(unless (fboundp 'sqlite-close)
  (defun sqlite-close (database)
    "Close DATABASE and return non-nil on success."
    (unless (emacs-sqlite-ffi--object-p database)
      (signal 'wrong-type-argument (list 'sqlitep database)))
    (let ((handle (aref database 1)))
      (if (= handle 0)
          t
        (let ((code (emacs-sqlite-ffi--call "sqlite3_close_v2" handle)))
          (emacs-sqlite-ffi--check code handle "sqlite-close")
          (aset database 1 0)
          t)))))

(unless (fboundp 'sqlitep)
  (defun sqlitep (object)
    "Return non-nil when OBJECT is an SQLite compatibility object."
    (and (emacs-sqlite-ffi--object-p object) t)))

(unless (fboundp 'sqlite-execute)
  (defun sqlite-execute (database query &optional values)
    "Execute non-select QUERY on DATABASE, binding optional VALUES."
    (let* ((handle (emacs-sqlite-ffi--handle database))
           (statement (emacs-sqlite-ffi--prepare handle query)))
      (unwind-protect
          (progn
            (emacs-sqlite-ffi--bind-values handle statement values)
            (let ((code (emacs-sqlite-ffi--call
                         "sqlite3_step" statement)))
              ;; Pragmas such as journal_mode return rows even through
              ;; Emacs's execute API.  Consume them before checking DONE.
              (while (= code emacs-sqlite-ffi--row-code)
                (setq code
                      (emacs-sqlite-ffi--call "sqlite3_step" statement)))
              (unless (= code emacs-sqlite-ffi--done)
                (signal 'sqlite-error
                        (list (emacs-sqlite-ffi--errmsg handle))))
              (emacs-sqlite-ffi--call "sqlite3_changes" handle)))
        (emacs-sqlite-ffi--call "sqlite3_finalize" statement)))))

(unless (fboundp 'sqlite-select)
  (defun sqlite-select (database query &optional values return-type)
    "Select rows from DATABASE, optionally binding VALUES.
With RETURN-TYPE `full', prepend the list of column names."
    (unless (memq return-type '(nil full))
      (error "sqlite-select: unsupported return type %S" return-type))
    (let* ((handle (emacs-sqlite-ffi--handle database))
           (statement (emacs-sqlite-ffi--prepare handle query)))
      (unwind-protect
          (progn
            (emacs-sqlite-ffi--bind-values handle statement values)
            (let* ((column-count
                    (emacs-sqlite-ffi--call
                     "sqlite3_column_count" statement))
                   (names (and (eq return-type 'full)
                               (emacs-sqlite-ffi--column-names
                                statement column-count)))
                   (rows nil)
                   (code (emacs-sqlite-ffi--call
                          "sqlite3_step" statement)))
              (while (= code emacs-sqlite-ffi--row-code)
                (setq rows
                      (cons (emacs-sqlite-ffi--row statement column-count)
                            rows))
                (setq code
                      (emacs-sqlite-ffi--call "sqlite3_step" statement)))
              (setq rows (nreverse rows))
              (if names (cons names rows) rows)))
        (emacs-sqlite-ffi--call "sqlite3_finalize" statement)))))

(provide 'emacs-sqlite-ffi)

;;; emacs-sqlite-ffi.el ends here
