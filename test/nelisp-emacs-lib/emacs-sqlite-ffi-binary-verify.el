;;; emacs-sqlite-ffi-binary-verify.el --- Standalone SQLite smoke -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with a dynamically linked NeLisp standalone reader.  This exercises
;; public sqlite-* compatibility against the system libsqlite3.

;;; Code:

(let* ((prefixes '("src/" "../src/" "test/../src/"))
       (source
        (catch 'found
          (dolist (prefix prefixes)
            (when (file-exists-p (concat prefix "emacs-sqlite-ffi.el"))
              (throw 'found (concat prefix "emacs-sqlite-ffi.el")))))))
  (unless source
    (error "SQLITE-FFI-VERIFY: cannot locate emacs-sqlite-ffi.el"))
  (load source nil t))

(unless (sqlite-available-p)
  (error "SQLITE-FFI-VERIFY: SQLite is unavailable"))

(let ((database (sqlite-open nil)))
  (unless (sqlitep database)
    (error "SQLITE-FFI-VERIFY: sqlitep rejected open database"))
  (unless (= (sqlite-execute
              database
              "create table sample (n integer, r real, s text, z text)")
             0)
    (error "SQLITE-FFI-VERIFY: create changed rows"))
  (unless (= (sqlite-execute
              database
              "insert into sample values (?2, ?3, ?1, ?4)"
              '("text" 42 2.5 nil))
             1)
    (error "SQLITE-FFI-VERIFY: insert changed-row count"))
  (unless (equal (sqlite-select database
                                "select n, r, s, z from sample where n = ?"
                                [42])
                 '((42 2.5 "text" nil)))
    (error "SQLITE-FFI-VERIFY: row decoding or binding failed"))
  (unless (equal (sqlite-select database
                                "select s, n from sample" nil 'full)
                 '(("s" "n") ("text" 42)))
    (error "SQLITE-FFI-VERIFY: full row shape failed"))
  (sqlite-close database)
  (unless (sqlitep database)
    (error "SQLITE-FFI-VERIFY: sqlitep rejected closed database")))

(princ "SQLITE-FFI-VERIFY: PASS\n")

;;; emacs-sqlite-ffi-binary-verify.el ends here
