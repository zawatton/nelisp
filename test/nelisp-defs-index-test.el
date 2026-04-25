;;; nelisp-defs-index-test.el --- ERT tests for nelisp-defs-index  -*- lexical-binding: t; -*-

;; Phase 6.5.1 (Doc 25 §2.7 B) — MVP scope ERT.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-defs-index)

(defvar nelisp-defs-index-test--tmpdir nil)
(defvar nelisp-defs-index-test--sandbox-root nil
  "Per-test sandbox root, dynamic-bound by the with-fresh-db macro
so the file-writing helper resolves it without explicit threading.")

(defmacro nelisp-defs-index-test--with-fresh-db (&rest body)
  "Bind `nelisp-defs-index-db-path' + `-paths' to a fresh temp tree.
Ensures tests do not leak rows between runs and stay independent
of whatever .el files happen to exist around the cwd."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-defs-index-test--tmpdir
           (make-temp-file "nelisp-defs-index-test-" t))
          (nelisp-defs-index-db-path
           (expand-file-name "test-defs.db" nelisp-defs-index-test--tmpdir))
          (nelisp-defs-index-test--sandbox-root
           (expand-file-name "src" nelisp-defs-index-test--tmpdir))
          (nelisp-defs-index-paths
           (list nelisp-defs-index-test--sandbox-root)))
     (make-directory nelisp-defs-index-test--sandbox-root t)
     (unwind-protect
         (progn
           (nelisp-defs-index--close)
           ,@body)
       (nelisp-defs-index--close)
       (when (file-directory-p nelisp-defs-index-test--tmpdir)
         (delete-directory nelisp-defs-index-test--tmpdir t)))))

(defun nelisp-defs-index-test--write-fixture (rel content)
  "Write CONTENT to REL under the per-test sandbox root."
  (let ((path (expand-file-name rel nelisp-defs-index-test--sandbox-root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defconst nelisp-defs-index-test--fixture-a
  "(defun foo-fn (x &optional y) \"A foo function.\\nLine 2.\" (+ x (or y 0)))
(defmacro foo-mac (a b) (list 'progn a b))
(defvar foo-var nil \"A foo variable.\")
(defcustom foo-custom 42 \"Custom value.\" :type 'integer)
(cl-defun foo-cl (a &key b) (list a b))
(defconst foo-const :tag \"A constant.\")
")

(defconst nelisp-defs-index-test--fixture-b
  "(defun bar-fn () \"Bar.\" t)
(defun shared-name () \"In B.\" t)
")

(defconst nelisp-defs-index-test--fixture-c-with-comment
  ";;; comment-only.el --- header comment

;; not a defining form
(provide 'comment-only)
")

;;;; --- enable / schema ---------------------------------------------------

(ert-deftest nelisp-defs-index-test-enable-creates-db-file ()
  "`nelisp-defs-index-enable' creates the DB file under XDG cache default."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (should-not (file-exists-p nelisp-defs-index-db-path))
    (nelisp-defs-index-enable)
    (should (file-exists-p nelisp-defs-index-db-path))))

(ert-deftest nelisp-defs-index-test-disable-is-idempotent ()
  "`nelisp-defs-index-disable' tolerates being called twice."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-enable)
    (nelisp-defs-index-disable)
    (nelisp-defs-index-disable)
    (should-not nelisp-defs-index--db)))

(ert-deftest nelisp-defs-index-test-schema-tables-present ()
  "All 5 tables exist after enable."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-enable)
    (let* ((rows (sqlite-select
                  (nelisp-defs-index--db)
                  "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"))
           (names (mapcar #'car rows)))
      (dolist (tbl '("defs" "features" "file" "refs" "schema_meta"))
        (should (member tbl names))))))

(ert-deftest nelisp-defs-index-test-schema-defs-columns ()
  "PRAGMA table_info(defs) reports the 10 expected columns."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-enable)
    (let* ((cols (sqlite-select
                  (nelisp-defs-index--db)
                  "PRAGMA table_info(defs)"))
           (names (mapcar (lambda (row) (nth 1 row)) cols)))
      (dolist (col '("id" "file_id" "kind" "name" "line" "end_line"
                     "arity_min" "arity_max" "docstring_head" "obsolete_p"))
        (should (member col names))))))

;;;; --- rebuild -----------------------------------------------------------

(ert-deftest nelisp-defs-index-test-rebuild-positive-counts ()
  "`rebuild' returns plist with files / defs / duration counts > 0."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-test--write-fixture "a.el"
                                           nelisp-defs-index-test--fixture-a)
    (nelisp-defs-index-test--write-fixture "b.el"
                                           nelisp-defs-index-test--fixture-b)
    (let ((r (nelisp-defs-index-rebuild)))
      (should (= 2 (plist-get r :files)))
      ;; fixture-a = 6 forms (foo-fn / foo-mac / foo-var / foo-custom
      ;; / foo-cl / foo-const), fixture-b = 2 forms.
      (should (= 8 (plist-get r :defs)))
      (should (integerp (plist-get r :duration-ms))))))

(ert-deftest nelisp-defs-index-test-rebuild-skips-non-el-and-comments ()
  "Files without defining forms add nothing to the index."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-test--write-fixture
     "comment-only.el" nelisp-defs-index-test--fixture-c-with-comment)
    (nelisp-defs-index-test--write-fixture
     "not-elisp.txt" "anything")
    (let ((r (nelisp-defs-index-rebuild)))
      ;; Only comment-only.el is collected (not the .txt), and it has
      ;; zero defining forms but does contain a `provide' (Phase 6.5.2
      ;; will record it; MVP just notes 0 defs).
      (should (= 1 (plist-get r :files)))
      (should (= 0 (plist-get r :defs))))))

;;;; --- search ------------------------------------------------------------

(ert-deftest nelisp-defs-index-test-search-known-symbol ()
  "Exact-name search returns the row with kind / file / line populated."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-test--write-fixture
     "a.el" nelisp-defs-index-test--fixture-a)
    (nelisp-defs-index-rebuild)
    (let* ((hits (nelisp-defs-index-search "foo-fn"))
           (h (car hits)))
      (should (= 1 (length hits)))
      (should (string= "defun" (plist-get h :kind)))
      (should (string= "foo-fn" (plist-get h :name)))
      (should (string-suffix-p "a.el" (plist-get h :file)))
      (should (= 1 (plist-get h :line)))
      (should (= 1 (plist-get h :arity-min)))
      (should (= 2 (plist-get h :arity-max)))
      (should (string= "A foo function." (plist-get h :docstring-head))))))

(ert-deftest nelisp-defs-index-test-search-missing-returns-empty ()
  "Searching for a name with no rows returns nil."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-test--write-fixture
     "a.el" nelisp-defs-index-test--fixture-a)
    (nelisp-defs-index-rebuild)
    (should-not (nelisp-defs-index-search "definitely-missing"))))

(ert-deftest nelisp-defs-index-test-search-kind-filter ()
  ":kind restricts to the matching form head."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-test--write-fixture
     "a.el" nelisp-defs-index-test--fixture-a)
    (nelisp-defs-index-rebuild)
    (let ((only-defun (nelisp-defs-index-search "foo-fn" '(:kind "defun")))
          (only-defmacro (nelisp-defs-index-search "foo-fn" '(:kind "defmacro"))))
      (should (= 1 (length only-defun)))
      (should-not only-defmacro))))

(ert-deftest nelisp-defs-index-test-search-fuzzy ()
  ":fuzzy enables substring matching across multiple defining forms."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (nelisp-defs-index-test--write-fixture
     "a.el" nelisp-defs-index-test--fixture-a)
    (nelisp-defs-index-rebuild)
    (let ((hits (nelisp-defs-index-search "foo" '(:fuzzy t :limit 100))))
      ;; All 6 fixture-a forms start with "foo-".
      (should (= 6 (length hits))))))

(ert-deftest nelisp-defs-index-test-search-validates-name ()
  "Empty or non-string NAME signals user-error before touching the DB."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (should-error (nelisp-defs-index-search "") :type 'user-error)
    (should-error (nelisp-defs-index-search nil) :type 'user-error)))

(ert-deftest nelisp-defs-index-test-rebuild-replaces-prior-state ()
  "Re-running rebuild against fewer files purges stale rows."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-defs-index-test--with-fresh-db
    (let ((path-a (nelisp-defs-index-test--write-fixture
                   "a.el" nelisp-defs-index-test--fixture-a))
          (path-b (nelisp-defs-index-test--write-fixture
                   "b.el" nelisp-defs-index-test--fixture-b)))
      (nelisp-defs-index-rebuild)
      (should (nelisp-defs-index-search "bar-fn"))
      (delete-file path-b)
      (ignore path-a)
      (let ((r (nelisp-defs-index-rebuild)))
        (should (= 1 (plist-get r :files))))
      (should-not (nelisp-defs-index-search "bar-fn"))
      (should (nelisp-defs-index-search "foo-fn")))))

(provide 'nelisp-defs-index-test)
;;; nelisp-defs-index-test.el ends here
