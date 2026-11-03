;;; census-process-02.el --- canonical probes  -*- lexical-binding: t; -*-

(process-id
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-id p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (delete-process p) (process-id p)) (delete-process p)))
 (condition-case e (process-id 17) (error e)))

(process-list
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (not (null (memq p (process-list)))) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (delete-process p) (not (null (memq p (process-list))))) (delete-process p)))
 (condition-case e (process-list 17) (error (car e))))

(process-mark
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (let ((m (process-mark p))) (list (markerp m) (marker-position m))) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (with-temp-buffer (insert "abc") (set-process-buffer p (current-buffer)) (let ((m (process-mark p))) (list (marker-position m) (eq (marker-buffer m) (current-buffer))))) (delete-process p)))
 (condition-case e (process-mark 17) (error e)))

(process-name
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-name p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (delete-process p) (process-name p)) (delete-process p)))
 (condition-case e (process-name 17) (error e)))

(process-plist
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-plist p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-plist p '(mode binary count 2)) (process-plist p)) (delete-process p)))
 (condition-case e (process-plist 17) (error e)))

(process-query-on-exit-flag
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-query-on-exit-flag p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-query-on-exit-flag p t) (process-query-on-exit-flag p)) (delete-process p)))
 (condition-case e (process-query-on-exit-flag 17) (error e)))

(process-send-eof
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (eq (process-send-eof p) p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (process-send-eof p) (process-status p)) (delete-process p)))
 (condition-case e (process-send-eof 17) (error e)))

(process-send-region
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (with-temp-buffer (insert "abc") (process-send-region p (point-min) (point-max))) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (with-temp-buffer (process-send-region p (point-min) (point-max))) (delete-process p)))
 (condition-case e (process-send-region 17 1 1) (error e)))

(process-send-string
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-send-string p "abc") (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-send-string p "") (delete-process p)))
 (condition-case e (process-send-string 17 "x") (error e)))

(process-sentinel
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (eq (process-sentinel p) 'internal-default-process-sentinel) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-sentinel p 'ignore) (eq (process-sentinel p) 'ignore)) (delete-process p)))
 (condition-case e (process-sentinel 17) (error e)))

(process-status
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (process-status p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (delete-process p) (process-status p)) (delete-process p)))
 (condition-case e (process-status 17) (error e)))

(set-process-buffer
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (with-temp-buffer (set-process-buffer p (current-buffer)) (eq (process-buffer p) (current-buffer))) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-buffer p nil) (process-buffer p)) (delete-process p)))
 (condition-case e (set-process-buffer 17 nil) (error e)))

(set-process-filter
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-filter p 'ignore) (eq (process-filter p) 'ignore)) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-filter p nil) (eq (process-filter p) 'internal-default-process-filter)) (delete-process p)))
 (condition-case e (set-process-filter 17 nil) (error e)))

(set-process-plist
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-plist p '(key 7)) (process-plist p)) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-plist p nil) (process-plist p)) (delete-process p)))
 (condition-case e (set-process-plist 17 nil) (error e)))

(set-process-query-on-exit-flag
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-query-on-exit-flag p t) (process-query-on-exit-flag p)) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t))) (unwind-protect (progn (set-process-query-on-exit-flag p nil) (process-query-on-exit-flag p)) (delete-process p)))
 (condition-case e (set-process-query-on-exit-flag 17 nil) (error e)))

(sqlite-available-p
 (sqlite-available-p)
 (condition-case e (sqlite-available-p t) (error (car e))))

(sqlite-close
 (let ((db (sqlite-open))) (unwind-protect (sqlite-close db) (ignore-errors (sqlite-close db))))
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-close db) (sqlite-close db)) (ignore-errors (sqlite-close db))))
 (condition-case e (sqlite-close 17) (error e)))

(sqlite-commit
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-transaction db) (sqlite-commit db)) (sqlite-close db)))
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-execute db "CREATE TABLE t (x INTEGER)") (sqlite-transaction db) (sqlite-execute db "INSERT INTO t VALUES (7)") (sqlite-commit db) (sqlite-select db "SELECT x FROM t")) (sqlite-close db)))
 (condition-case e (sqlite-commit 17) (error e)))

(sqlite-execute
 (let ((db (sqlite-open))) (unwind-protect (sqlite-execute db "CREATE TABLE t (x INTEGER)") (sqlite-close db)))
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-execute db "CREATE TABLE t (x INTEGER)") (sqlite-execute db "INSERT INTO t VALUES (?)" [7])) (sqlite-close db)))
 (condition-case e (sqlite-execute 17 "SELECT 1") (error e)))

(sqlite-open
 (let ((db (sqlite-open))) (unwind-protect (sqlitep db) (sqlite-close db)))
 (let ((db (sqlite-open nil nil t))) (unwind-protect (sqlite-select db "SELECT 7") (sqlite-close db)))
 (condition-case e (sqlite-open 17) (error e)))

(sqlite-pragma
 (let ((db (sqlite-open))) (unwind-protect (sqlite-pragma db "user_version") (sqlite-close db)))
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-pragma db "user_version=7") (sqlite-pragma db "user_version") (sqlite-select db "PRAGMA user_version")) (sqlite-close db)))
 (condition-case e (sqlite-pragma 17 "user_version") (error e)))

(sqlite-rollback
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-transaction db) (sqlite-rollback db)) (sqlite-close db)))
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-execute db "CREATE TABLE t (x INTEGER)") (sqlite-transaction db) (sqlite-execute db "INSERT INTO t VALUES (7)") (sqlite-rollback db) (sqlite-select db "SELECT x FROM t")) (sqlite-close db)))
 (condition-case e (sqlite-rollback 17) (error e)))

(sqlite-select
 (let ((db (sqlite-open))) (unwind-protect (sqlite-select db "SELECT 7, 'abc'") (sqlite-close db)))
 (let ((db (sqlite-open))) (unwind-protect (sqlite-select db "SELECT ? AS n" [7] 'full) (sqlite-close db)))
 (condition-case e (sqlite-select 17 "SELECT 1") (error e)))

(sqlite-transaction
 (let ((db (sqlite-open))) (unwind-protect (sqlite-transaction db) (sqlite-close db)))
 (let ((db (sqlite-open))) (unwind-protect (progn (sqlite-transaction db) (sqlite-execute db "CREATE TABLE t (x INTEGER)") (sqlite-rollback db) (sqlite-select db "SELECT name FROM sqlite_master WHERE name = 't'")) (sqlite-close db)))
 (condition-case e (sqlite-transaction 17) (error e)))

(sqlitep
 (let ((db (sqlite-open))) (unwind-protect (sqlitep db) (sqlite-close db)))
 (mapcar #'sqlitep '(nil 17 "db"))
 (condition-case e (sqlitep) (error (car e))))

(treesit-available-p
 (treesit-available-p)
 (condition-case e (treesit-available-p t) (error (car e))))

(thread-join
 (condition-case e (apply 'thread-join '(17 nil t extra)) (error (car e)))
 (condition-case e (apply 'thread-join '(17 nil t)) (error (car e))))
