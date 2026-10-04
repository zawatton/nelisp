;;; emacs-cc-sqlite-1.el --- SQLite C primitive fallbacks -*- lexical-binding: t; -*-

;; SQLite result sets are native C objects and cannot be represented by the
;; standalone's current SQLite database-only FFI.

;; This library is optional.  A missing provider must not leave callable
;; wrappers that fail later with an unrelated void-function error.  Errors
;; raised while loading a present provider are intentionally not suppressed.
(require 'emacs-sqlite-ffi nil t)

(when (and (featurep 'emacs-sqlite-ffi) (fboundp 'nl-ffi-call))
    ;; The application compatibility wrappers can be present without their
    ;; `nelisp-sqlite-*' provider in a library-only standalone.
    (unless (fboundp 'nelisp-sqlite-open)
      (defun nelisp-sqlite-open (file)
        (unless (or (null file) (stringp file))
          (signal 'wrong-type-argument (list 'stringp file)))
        (let ((slot (alloc-bytes 8 8)))
          (ptr-write-u64 slot 0 0)
          (let* ((code (emacs-sqlite-ffi--call
                        "sqlite3_open_v2"
                        (emacs-sqlite-ffi--cstring (or file ":memory:"))
                        slot 6 0))
                 (handle (ptr-read-u64 slot 0)))
            (if (= code 0) (emacs-sqlite-ffi--object handle)
              (error "sqlite-open: %s (SQLite code %d)"
                     (if (= handle 0) "unable to open database"
                       (emacs-sqlite-ffi--errmsg handle)) code))))))
    (unless (fboundp 'nelisp-sqlite-close)
      (defun nelisp-sqlite-close (db)
        (let* ((handle (emacs-sqlite-ffi--handle db))
               (code (emacs-sqlite-ffi--call "sqlite3_close_v2" handle)))
          (emacs-sqlite-ffi--check code handle "sqlite-close")
          (aset db 1 0)
          t)))
    (unless (fboundp 'nelisp-sqlitep)
      (defun nelisp-sqlitep (object)
        (emacs-sqlite-ffi--object-p object))))

(unless (fboundp 'sqlite-columns)
  (defun sqlite-columns (set)
    "Return the column names of SET."
    (if (null set)
        (signal 'wrong-type-argument (list 'sqlitep set))
      (signal 'sqlite-error (list "Invalid set object")))))

(unless (fboundp 'sqlite-execute-batch)
  (defun sqlite-execute-batch (db statements)
    "Execute multiple SQL STATEMENTS in DB.
STATEMENTS is a string containing 0 or more SQL statements."
    (unless (and (featurep 'emacs-sqlite-ffi) (fboundp 'nl-ffi-call))
      (signal 'sqlite-error (list "SQLite support is not available")))
    (unless (stringp statements)
      (signal 'wrong-type-argument (list 'stringp statements)))
    (let* ((handle (emacs-sqlite-ffi--handle db))
           (code (emacs-sqlite-ffi--call
                  "sqlite3_exec" handle
                  (emacs-sqlite-ffi--cstring statements) 0 0 0)))
      (unless (= code 0)
        (signal 'sqlite-error (list (emacs-sqlite-ffi--errmsg handle))))
      t)))

(unless (fboundp 'sqlite-finalize)
  (defun sqlite-finalize (set)
    "Mark this SET as being finished.
This will free the resources held by SET."
    (if (null set)
        (signal 'wrong-type-argument (list 'sqlitep set))
      (signal 'sqlite-error (list "Invalid set object")))))

(unless (fboundp 'sqlite-load-extension)
  (defun sqlite-load-extension (db module)
    "Load an SQlite MODULE into DB.
MODULE should be the name of an SQlite module's file, a
shared library in the system-dependent format and having a
system-dependent file-name extension.

Only modules on Emacs's list of allowed modules can be loaded."
    (unless (and (featurep 'emacs-sqlite-ffi) (fboundp 'nl-ffi-call))
      (signal 'sqlite-error (list "SQLite support is not available")))
    (emacs-sqlite-ffi--handle db)
    (unless (stringp module)
      (signal 'wrong-type-argument (list 'stringp module)))
    (signal 'sqlite-error (list "Module name not on allowlist"))))

 (unless (fboundp 'sqlite-more-p)
  (defun sqlite-more-p (set)
    "Say whether there are any further results in SET."
    (if (null set)
        (signal 'wrong-type-argument (list 'sqlitep set))
      (signal 'sqlite-error (list "Invalid set object")))))

(unless (fboundp 'sqlite-next)
  (defun sqlite-next (set)
    "Return the next result set from SET.
Return nil when the statement has finished executing successfully."
    (if (null set)
        (signal 'wrong-type-argument (list 'sqlitep set))
      (signal 'sqlite-error (list "Invalid set object")))))

(unless (fboundp 'sqlite-version)
  (defun sqlite-version ()
    "Return the version string of the SQLite library.
Signal an error if SQLite support is not available."
    (cond
     ((fboundp 'nelisp-sqlite-version) (nelisp-sqlite-version))
     ((and (featurep 'emacs-sqlite-ffi) (fboundp 'nl-ffi-call))
      (emacs-sqlite-ffi--read-cstring
       (emacs-sqlite-ffi--call "sqlite3_libversion")))
     (t (signal 'sqlite-error (list "SQLite support is not available"))))))

(provide 'emacs-cc-sqlite-1)

;;; emacs-cc-sqlite-1.el ends here
