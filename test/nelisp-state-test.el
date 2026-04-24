;;; nelisp-state-test.el --- ERT tests for nelisp-state  -*- lexical-binding: t; -*-

;; Phase 5-F.1.1 — anvil-state-test.el 移植 + NeLisp 固有 (cl-defun → plist 互換)。

(require 'ert)
(require 'nelisp-state)

(defvar nelisp-state-test--tmpdir nil)

(defmacro nelisp-state-test--with-fresh-db (&rest body)
  "Bind `nelisp-state-db-path' to a fresh temp file and reset state.
Ensures tests do not leak rows between runs."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-state-test--tmpdir
           (make-temp-file "nelisp-state-test-" t))
          (nelisp-state-db-path
           (expand-file-name "test-state.db" nelisp-state-test--tmpdir)))
     (unwind-protect
         (progn
           (nelisp-state--close)
           ,@body)
       (nelisp-state--close)
       (when (file-directory-p nelisp-state-test--tmpdir)
         (delete-directory nelisp-state-test--tmpdir t)))))

;;;; enable / disable / schema

(ert-deftest nelisp-state-test-enable-creates-db-file ()
  "`nelisp-state-enable' creates DB file under XDG cache default."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (should-not (file-exists-p nelisp-state-db-path))
    (nelisp-state-enable)
    (should (file-exists-p nelisp-state-db-path))))

(ert-deftest nelisp-state-test-disable-is-idempotent ()
  "`nelisp-state-disable' can be called repeatedly without signal."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-enable)
    (nelisp-state-disable)
    (nelisp-state-disable)
    (should-not nelisp-state--db)))

(ert-deftest nelisp-state-test-schema-has-source-column ()
  "§2.3 B: schema に source 列が含まれる (anvil-state との差分)。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-enable)
    (let* ((cols (sqlite-select
                  (nelisp-state--db)
                  "PRAGMA table_info(kv)"))
           (names (mapcar (lambda (row) (nth 1 row)) cols)))
      (should (member "source" names)))))

;;;; set / get round trip

(ert-deftest nelisp-state-test-set-get-basic ()
  "`nelisp-state-set' → `nelisp-state-get' で値が round trip する。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k1" 42)
    (should (= 42 (nelisp-state-get "k1")))
    (nelisp-state-set "k2" "hello")
    (should (string= "hello" (nelisp-state-get "k2")))
    (nelisp-state-set "k3" '(1 2 3))
    (should (equal '(1 2 3) (nelisp-state-get "k3")))
    (nelisp-state-set "k4" '(:plist "value"))
    (should (equal '(:plist "value") (nelisp-state-get "k4")))))

(ert-deftest nelisp-state-test-get-missing-returns-default ()
  "absent KEY returns :default from OPTS plist (default nil)。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (should-not (nelisp-state-get "missing"))
    (should (eq 'fallback (nelisp-state-get "missing" '(:default fallback))))))

(ert-deftest nelisp-state-test-set-overwrites ()
  "既存 key への set は update (insert 新規ではなく更新)。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k" 1)
    (nelisp-state-set "k" 2)
    (should (= 2 (nelisp-state-get "k")))
    (should (= 1 (nelisp-state-count)))))

;;;; namespace

(ert-deftest nelisp-state-test-ns-isolation ()
  "異なる :ns 下の同 key が独立して保持される。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k" "a-val" '(:ns "a"))
    (nelisp-state-set "k" "b-val" '(:ns "b"))
    (should (string= "a-val" (nelisp-state-get "k" '(:ns "a"))))
    (should (string= "b-val" (nelisp-state-get "k" '(:ns "b"))))
    (should-not (nelisp-state-get "k"))))  ; :ns default ≠ "a" / "b"

(ert-deftest nelisp-state-test-ns-default ()
  ":ns を省略すると `nelisp-state-default-namespace' が使われる。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k" 1)
    (should (= 1 (nelisp-state-get "k" `(:ns ,nelisp-state-default-namespace))))))

;;;; TTL

(ert-deftest nelisp-state-test-ttl-future-survives ()
  "未来 TTL の row は getできる。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k" 'alive '(:ttl 3600))
    (should (eq 'alive (nelisp-state-get "k")))))

(ert-deftest nelisp-state-test-ttl-zero-rejected ()
  ":ttl 0 は user-error (正数のみ許容)。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (should-error (nelisp-state-set "k" 1 '(:ttl 0))
                  :type 'user-error)))

(ert-deftest nelisp-state-test-ttl-expired-lazily-deleted ()
  "expires_at <= now の row は次 get 時に削除され :default を返す。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-enable)
    ;; Manually insert a row that is already expired (can't set past TTL via API)
    (let ((db (nelisp-state--db))
          (now (truncate (float-time))))
      (sqlite-execute
       db
       "INSERT INTO kv(ns, k, v, created_at, updated_at, expires_at, source)
          VALUES ('default', 'expired', ?1, ?2, ?2, ?3, 'nelisp')"
       (list (prin1-to-string 'gone) now (- now 10))))
    (should (eq 'fallback (nelisp-state-get "expired" '(:default fallback))))
    (should (= 0 (nelisp-state-count)))))

;;;; delete / delete-ns

(ert-deftest nelisp-state-test-delete-returns-t-when-removed ()
  "delete returns t on hit、nil on miss。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k" 1)
    (should (eq t (nelisp-state-delete "k")))
    (should-not (nelisp-state-delete "k"))))

(ert-deftest nelisp-state-test-delete-ns-removes-all ()
  "delete-ns removes all rows in a namespace、返り値は削除件数。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k1" 1 '(:ns "a"))
    (nelisp-state-set "k2" 2 '(:ns "a"))
    (nelisp-state-set "k3" 3 '(:ns "b"))
    (should (= 2 (nelisp-state-delete-ns "a")))
    (should-not (nelisp-state-get "k1" '(:ns "a")))
    (should (= 3 (nelisp-state-get "k3" '(:ns "b"))))))

;;;; list / count

(ert-deftest nelisp-state-test-list-ns-sorted ()
  "list-ns は sorted distinct namespace を返す。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "x" 1 '(:ns "zeta"))
    (nelisp-state-set "x" 1 '(:ns "alpha"))
    (nelisp-state-set "x" 1 '(:ns "mu"))
    (should (equal '("alpha" "mu" "zeta") (nelisp-state-list-ns)))))

(ert-deftest nelisp-state-test-list-keys-requires-ns ()
  "list-keys は :ns なしで error (tenant boundary)。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (should-error (nelisp-state-list-keys) :type 'error)
    (should-error (nelisp-state-list-keys '(:limit 5)) :type 'error)))

(ert-deftest nelisp-state-test-list-keys-with-limit ()
  ":limit が効く、結果は sorted。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (dotimes (i 5) (nelisp-state-set (format "k%d" i) i '(:ns "test")))
    (should (equal '("k0" "k1" "k2") (nelisp-state-list-keys '(:ns "test" :limit 3))))
    (should (= 5 (length (nelisp-state-list-keys '(:ns "test")))))))

(ert-deftest nelisp-state-test-count-with-and-without-ns ()
  "count total / count NS 両方動く。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k1" 1 '(:ns "a"))
    (nelisp-state-set "k2" 2 '(:ns "a"))
    (nelisp-state-set "k3" 3 '(:ns "b"))
    (should (= 3 (nelisp-state-count)))
    (should (= 2 (nelisp-state-count "a")))
    (should (= 1 (nelisp-state-count "b")))
    (should (= 0 (nelisp-state-count "c")))))

;;;; vacuum

(ert-deftest nelisp-state-test-vacuum-removes-expired ()
  "vacuum は expired row を物理削除、:expired カウントを返す。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-enable)
    (let ((db (nelisp-state--db))
          (now (truncate (float-time))))
      (sqlite-execute
       db
       "INSERT INTO kv(ns, k, v, created_at, updated_at, expires_at, source)
          VALUES ('default', 'x', ?1, ?2, ?2, ?3, 'nelisp')"
       (list (prin1-to-string 1) now (- now 100)))
      (sqlite-execute
       db
       "INSERT INTO kv(ns, k, v, created_at, updated_at, expires_at, source)
          VALUES ('default', 'y', ?1, ?2, ?2, NULL, 'nelisp')"
       (list (prin1-to-string 2) now)))
    (let ((result (nelisp-state-vacuum)))
      (should (= 1 (plist-get result :expired))))
    (should (= 1 (nelisp-state-count)))))

;;;; serialization / source column

(ert-deftest nelisp-state-test-serialize-rejects-non-readable ()
  "non-readable (buffer) 値は set 前に user-error。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-enable)  ; triggers DB open so the error is pre-DB
    (with-temp-buffer
      (should-error (nelisp-state-set "k" (current-buffer))
                    :type 'user-error))))

(ert-deftest nelisp-state-test-source-tag-stored ()
  "書き込み時に source 列に `nelisp' tag が入る。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-state-test--with-fresh-db
    (nelisp-state-set "k" 1)
    (let ((row (car (sqlite-select
                     (nelisp-state--db)
                     "SELECT source FROM kv WHERE k = 'k' LIMIT 1"))))
      (should (string= "nelisp" (nth 0 row))))))

(provide 'nelisp-state-test)
;;; nelisp-state-test.el ends here
