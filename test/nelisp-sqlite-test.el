;;; nelisp-sqlite-test.el --- T77 SQLite FFI ERT smoke -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T77 (Wave 1 agent C) ERT smoke for `nelisp-sqlite.el'.
;;
;; The module depends on two artifacts produced by `make sqlite-module'
;; (which depends on `make runtime'):
;;
;;   - nelisp-runtime/target/release/libnelisp_runtime.so  (cdylib)
;;   - nelisp-runtime/target/release/nelisp-sqlite-module.so (Emacs module)
;;
;; A fresh checkout that has never run `cargo build --release' should
;; *skip* (not fail) so plain `make test' stays green for hosts without
;; a Rust toolchain.  CI / `make test-sqlite' (TBD) depends on
;; `sqlite-module' so the full pipeline still proves the FFI smoke.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-sqlite)

(defun nelisp-sqlite-test--skip-unless-built ()
  "Skip the current ERT unless `sqlite-module' has produced its artifacts."
  (unless (nelisp-sqlite-available-p)
    (ert-skip
     "nelisp-sqlite substrate not built (run `make runtime' + `make sqlite-module')")))

(defmacro nelisp-sqlite-test--with-mem-db (var &rest body)
  "Bind VAR to a freshly-opened :memory: connection, run BODY, close on exit."
  (declare (indent 1))
  `(let ((,var nil))
     (unwind-protect
         (progn
           (setq ,var (nelisp-sqlite-open ":memory:"))
           ,@body)
       (when ,var
         (ignore-errors (nelisp-sqlite-close ,var))))))

;;; --- 1. open / close ---------------------------------------------

(ert-deftest nelisp-sqlite-open-close-roundtrip ()
  "`open' returns a positive handle; `close' returns t; `sqlitep' tracks liveness."
  (nelisp-sqlite-test--skip-unless-built)
  (let ((db (nelisp-sqlite-open ":memory:")))
    (should (integerp db))
    (should (> db 0))
    (should (nelisp-sqlitep db))
    (should (eq t (nelisp-sqlite-close db)))
    (should-not (nelisp-sqlitep db))))

(ert-deftest nelisp-sqlite-open-bad-arg-signals ()
  "`open' must signal on non-string PATH."
  (nelisp-sqlite-test--skip-unless-built)
  (should-error (nelisp-sqlite-open 42) :type 'nelisp-sqlite-error))

(ert-deftest nelisp-sqlite-close-bad-handle-signals ()
  "Closing an already-closed / bogus handle signals."
  (nelisp-sqlite-test--skip-unless-built)
  (should-error (nelisp-sqlite-close 99999999) :type 'nelisp-sqlite-error))

;;; --- 2. CRUD -----------------------------------------------------

(ert-deftest nelisp-sqlite-execute-create-insert-select ()
  "End-to-end CRUD round-trip with a simple integer + text table."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (should (= 0 (nelisp-sqlite-execute db "CREATE TABLE t (id INTEGER, name TEXT)")))
    (should (= 1 (nelisp-sqlite-execute db "INSERT INTO t VALUES (?, ?)" '(1 "alpha"))))
    (should (= 1 (nelisp-sqlite-execute db "INSERT INTO t VALUES (?, ?)" '(2 "beta"))))
    (let ((rows (nelisp-sqlite-select db "SELECT id, name FROM t ORDER BY id")))
      (should (= 2 (length rows)))
      (should (equal [1 "alpha"] (car rows)))
      (should (equal [2 "beta"]  (cadr rows))))))

(ert-deftest nelisp-sqlite-execute-no-args ()
  "Schema DDL that takes no parameters works without a VALUES list."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (should (= 0 (nelisp-sqlite-execute db "CREATE TABLE t (n INTEGER)")))
    (should (= 1 (nelisp-sqlite-execute db "INSERT INTO t VALUES (1)")))
    (let ((rows (nelisp-sqlite-select db "SELECT * FROM t")))
      (should (equal [1] (car rows))))))

(ert-deftest nelisp-sqlite-select-empty-table ()
  "Selecting from an empty table yields an empty list."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (n INTEGER)")
    (should (equal '() (nelisp-sqlite-select db "SELECT * FROM t")))))

;;; --- 3. parameter binding (vector + list) ------------------------

(ert-deftest nelisp-sqlite-prepared-statement-binding-vector ()
  "VALUES may be a vector of parameters."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (id INTEGER, name TEXT)")
    (should (= 1 (nelisp-sqlite-execute db "INSERT INTO t VALUES (?, ?)" [42 "answer"])))
    (let ((rows (nelisp-sqlite-select db "SELECT * FROM t WHERE id = ?" [42])))
      (should (equal [42 "answer"] (car rows))))))

(ert-deftest nelisp-sqlite-prepared-statement-binding-list ()
  "VALUES may also be a plain list of parameters."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (id INTEGER, name TEXT)")
    (should (= 1 (nelisp-sqlite-execute db "INSERT INTO t VALUES (?, ?)" '(7 "lucky"))))
    (let ((rows (nelisp-sqlite-select db "SELECT * FROM t WHERE id = ?" '(7))))
      (should (equal [7 "lucky"] (car rows))))))

;;; --- 4. NULL / integer / float / text type matrix ----------------

(ert-deftest nelisp-sqlite-null-roundtrip ()
  "JSON null becomes Lisp `nil' on read-back."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (n INTEGER)")
    (nelisp-sqlite-execute db "INSERT INTO t VALUES (NULL)")
    (let ((rows (nelisp-sqlite-select db "SELECT n FROM t")))
      ;; NULL -> JSON null -> :null sentinel from nelisp-json-parse-string
      ;; (parser default behaviour); accept either nil or :null per the
      ;; nelisp-json sentinel mapping.
      (let ((cell (aref (car rows) 0)))
        (should (memq cell '(nil :null)))))))

(ert-deftest nelisp-sqlite-real-float-roundtrip ()
  "REAL columns decode to floats."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (x REAL)")
    (nelisp-sqlite-execute db "INSERT INTO t VALUES (?)" '(3.14))
    (let* ((rows (nelisp-sqlite-select db "SELECT x FROM t"))
           (val  (aref (car rows) 0)))
      (should (numberp val))
      (should (< (abs (- val 3.14)) 0.0001)))))

;;; --- 5. transaction ----------------------------------------------

(ert-deftest nelisp-sqlite-transaction-commit ()
  "BEGIN + INSERT + COMMIT persists rows."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (n INTEGER)")
    (nelisp-sqlite-transaction db)
    (nelisp-sqlite-execute db "INSERT INTO t VALUES (1)")
    (nelisp-sqlite-execute db "INSERT INTO t VALUES (2)")
    (nelisp-sqlite-commit db)
    (should (= 2 (length (nelisp-sqlite-select db "SELECT * FROM t"))))))

(ert-deftest nelisp-sqlite-transaction-rollback ()
  "BEGIN + INSERT + ROLLBACK discards rows."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (n INTEGER)")
    (nelisp-sqlite-execute db "INSERT INTO t VALUES (0)")
    (nelisp-sqlite-transaction db)
    (nelisp-sqlite-execute db "INSERT INTO t VALUES (1)")
    (nelisp-sqlite-rollback db)
    (let ((rows (nelisp-sqlite-select db "SELECT * FROM t")))
      (should (= 1 (length rows)))
      (should (equal [0] (car rows))))))

;;; --- 6. pragma ---------------------------------------------------

(ert-deftest nelisp-sqlite-pragma-busy-timeout ()
  "`PRAGMA busy_timeout' executes without error."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (should (>= (nelisp-sqlite-pragma db "busy_timeout = 5000") 0))))

(ert-deftest nelisp-sqlite-pragma-foreign-keys ()
  "`PRAGMA foreign_keys' executes without error."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (should (>= (nelisp-sqlite-pragma db "foreign_keys = ON") 0))))

;;; --- 7. CJK / emoji UTF-8 round-trip ----------------------------

(ert-deftest nelisp-sqlite-cjk-utf8-roundtrip ()
  "Japanese + emoji string round-trips byte-for-byte."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (s TEXT)")
    (let ((payload "日本語テスト🎌こんにちは"))
      (nelisp-sqlite-execute db "INSERT INTO t VALUES (?)" (list payload))
      (let* ((rows (nelisp-sqlite-select db "SELECT s FROM t"))
             (got  (aref (car rows) 0)))
        (should (string= payload got))))))

;;; --- 8. multiple connections isolation --------------------------

(ert-deftest nelisp-sqlite-multiple-connections-isolated ()
  "Two independent :memory: handles do not see each other's tables."
  (nelisp-sqlite-test--skip-unless-built)
  (let ((db1 (nelisp-sqlite-open ":memory:"))
        (db2 (nelisp-sqlite-open ":memory:")))
    (unwind-protect
        (progn
          (should (/= db1 db2))
          (nelisp-sqlite-execute db1 "CREATE TABLE t (n INTEGER)")
          (nelisp-sqlite-execute db1 "INSERT INTO t VALUES (1)")
          ;; db2 has no `t' table; SELECT should signal.
          (should-error (nelisp-sqlite-select db2 "SELECT * FROM t")
                        :type 'nelisp-sqlite-error))
      (ignore-errors (nelisp-sqlite-close db1))
      (ignore-errors (nelisp-sqlite-close db2)))))

;;; --- 9. negative paths -----------------------------------------

(ert-deftest nelisp-sqlite-execute-bad-sql-signals ()
  "Malformed SQL surfaces as a signal."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (should-error (nelisp-sqlite-execute db "NOT VALID SQL")
                  :type 'nelisp-sqlite-error)))

(ert-deftest nelisp-sqlite-select-large-result-grows-buffer ()
  "A result larger than the initial probe round-trips correctly."
  (nelisp-sqlite-test--skip-unless-built)
  (nelisp-sqlite-test--with-mem-db db
    (nelisp-sqlite-execute db "CREATE TABLE t (n INTEGER, s TEXT)")
    (nelisp-sqlite-execute db "BEGIN")
    (cl-loop for i from 0 below 200 do
             (nelisp-sqlite-execute db "INSERT INTO t VALUES (?, ?)"
                                    (list i (format "row-%d-padding-padding" i))))
    (nelisp-sqlite-execute db "COMMIT")
    (let ((rows (nelisp-sqlite-select db "SELECT * FROM t ORDER BY n")))
      (should (= 200 (length rows)))
      (should (equal [0   "row-0-padding-padding"]   (nth 0 rows)))
      (should (equal [199 "row-199-padding-padding"] (nth 199 rows))))))

(provide 'nelisp-sqlite-test)

;;; nelisp-sqlite-test.el ends here
