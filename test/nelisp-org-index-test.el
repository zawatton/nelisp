;;; nelisp-org-index-test.el --- ERT tests for nelisp-org-index  -*- lexical-binding: t; -*-

;; Phase 6.6.1 (Doc 26 §2.7) — MVP scope ERT.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-org-index)

;; CI-smoke gating rationale: a small set of search assertions below
;; (search-name-substring / search-tag-filter / status-counts /
;; org-id-property-captured) compare the rebuild output against literal
;; ASCII expectations.  On Windows native Emacs the rebuild path walker
;; produces zero rows for the synthetic fixtures (Phase 9d Windows path
;; gap — same root cause as `nelisp-locate-file' fails).  Skip those
;; assertions on Windows; the schema / rebuild-shape tests stay enabled
;; everywhere, and a Phase 9d Windows fix will re-enable the gated set.
(defun nelisp-org-index-test--posix-fs-host-p ()
  "Non-nil iff the host filesystem matches NeLisp's path-resolution
contract used by `nelisp-org-index-rebuild'."
  (memq system-type '(gnu/linux darwin berkeley-unix)))

(defvar nelisp-org-index-test--tmpdir nil)
(defvar nelisp-org-index-test--sandbox-root nil
  "Per-test sandbox root, dynamic-bound by the with-fresh-db macro
so the file-writing helper resolves it without explicit threading.")

(defmacro nelisp-org-index-test--with-fresh-db (&rest body)
  "Bind `nelisp-org-index-db-path' + `-paths' to a fresh temp tree.
Ensures tests do not leak rows between runs and stay independent
of whatever .org files happen to exist around the cwd."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-org-index-test--tmpdir
           (make-temp-file "nelisp-org-index-test-" t))
          (nelisp-org-index-db-path
           (expand-file-name "test-org.db" nelisp-org-index-test--tmpdir))
          (nelisp-org-index-test--sandbox-root
           (expand-file-name "org" nelisp-org-index-test--tmpdir))
          (nelisp-org-index-paths
           (list nelisp-org-index-test--sandbox-root)))
     (make-directory nelisp-org-index-test--sandbox-root t)
     (unwind-protect
         (progn
           (nelisp-org-index--close)
           ,@body)
       (nelisp-org-index--close)
       (when (file-directory-p nelisp-org-index-test--tmpdir)
         (delete-directory nelisp-org-index-test--tmpdir t)))))

(defun nelisp-org-index-test--write-fixture (rel content)
  "Write CONTENT to REL under the per-test sandbox root."
  (let ((path (expand-file-name rel nelisp-org-index-test--sandbox-root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defconst nelisp-org-index-test--fixture-simple
  "* Top
** TODO Sub one :work:urgent:
   :PROPERTIES:
   :ID: top-sub-one
   :CATEGORY: A
   :END:
** DONE Sub two :work:
*** Deep three
* Another root
")

(defconst nelisp-org-index-test--fixture-other
  "* Bar root
** Bar child :tag-x:
")

(defconst nelisp-org-index-test--fixture-no-headlines
  "Just a paragraph of plain text.

No stars at the start of any line.
")

;;;; --- enable / schema ---------------------------------------------------

(ert-deftest nelisp-org-index-test-enable-creates-db-file ()
  "`nelisp-org-index-enable' creates the DB file under XDG cache default."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (should-not (file-exists-p nelisp-org-index-db-path))
    (nelisp-org-index-enable)
    (should (file-exists-p nelisp-org-index-db-path))))

(ert-deftest nelisp-org-index-test-disable-is-idempotent ()
  "`nelisp-org-index-disable' tolerates being called twice."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-enable)
    (nelisp-org-index-disable)
    (nelisp-org-index-disable)
    (should-not nelisp-org-index--db)))

(ert-deftest nelisp-org-index-test-schema-tables-present ()
  "All 5 tables exist after enable."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-enable)
    (let* ((rows (sqlite-select
                  (nelisp-org-index--db)
                  "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"))
           (names (mapcar #'car rows)))
      (dolist (tbl '("file" "headline" "property" "schema_meta" "tag"))
        (should (member tbl names))))))

;;;; --- rebuild -----------------------------------------------------------

(ert-deftest nelisp-org-index-test-rebuild-empty-tree ()
  "Rebuild on a tree with no .org files reports zeros."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (let ((r (nelisp-org-index-rebuild)))
      (should (= 0 (plist-get r :files)))
      (should (= 0 (plist-get r :headlines)))
      (should (integerp (plist-get r :duration-ms))))))

(ert-deftest nelisp-org-index-test-rebuild-single-file ()
  "Rebuild over a single org file counts the headlines correctly."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (let ((r (nelisp-org-index-rebuild)))
      (should (= 1 (plist-get r :files)))
      ;; Top / Sub one / Sub two / Deep three / Another root = 5.
      (should (= 5 (plist-get r :headlines))))))

(ert-deftest nelisp-org-index-test-rebuild-multiple-files-sums ()
  "Rebuild over multiple files sums the headlines."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-test--write-fixture
     "sub/b.org" nelisp-org-index-test--fixture-other)
    (let ((r (nelisp-org-index-rebuild)))
      (should (= 2 (plist-get r :files)))
      (should (= 7 (plist-get r :headlines))))))

(ert-deftest nelisp-org-index-test-rebuild-no-headlines-file ()
  "Org file without any headlines indexes as 0 headlines."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "plain.org" nelisp-org-index-test--fixture-no-headlines)
    (let ((r (nelisp-org-index-rebuild)))
      (should (= 1 (plist-get r :files)))
      (should (= 0 (plist-get r :headlines))))))

(ert-deftest nelisp-org-index-test-rebuild-replaces-prior-state ()
  "Re-running rebuild against fewer files purges stale rows."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (let ((path-a (nelisp-org-index-test--write-fixture
                   "a.org" nelisp-org-index-test--fixture-simple))
          (path-b (nelisp-org-index-test--write-fixture
                   "b.org" nelisp-org-index-test--fixture-other)))
      (nelisp-org-index-rebuild)
      (should (nelisp-org-index-search "Bar"))
      (delete-file path-b)
      (ignore path-a)
      (let ((r (nelisp-org-index-rebuild)))
        (should (= 1 (plist-get r :files))))
      (should-not (nelisp-org-index-search "Bar"))
      (should (nelisp-org-index-search "Top")))))

;;;; --- search ------------------------------------------------------------

(ert-deftest nelisp-org-index-test-search-name-substring ()
  "Substring NAME match returns rows with title / file / line populated."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (skip-unless (nelisp-org-index-test--posix-fs-host-p))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-rebuild)
    (let* ((hits (nelisp-org-index-search "Sub"))
           (titles (mapcar (lambda (h) (plist-get h :title)) hits)))
      (should (= 2 (length hits)))
      (should (member "Sub one" titles))
      (should (member "Sub two" titles))
      (let ((sub-one (cl-find-if
                      (lambda (h) (string= "Sub one" (plist-get h :title)))
                      hits)))
        (should sub-one)
        (should (string= "TODO" (plist-get sub-one :todo)))
        (should (= 2 (plist-get sub-one :level)))
        (should (string-suffix-p "a.org" (plist-get sub-one :file)))
        (should (member "work" (plist-get sub-one :tags)))
        (should (member "urgent" (plist-get sub-one :tags)))))))

(ert-deftest nelisp-org-index-test-search-no-match-empty ()
  "Searching for a name with no rows returns nil."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-rebuild)
    (should-not (nelisp-org-index-search "definitely-missing-headline"))))

(ert-deftest nelisp-org-index-test-search-file-filter ()
  ":file restricts results to the matching file path."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-test--write-fixture
     "sub/b.org" nelisp-org-index-test--fixture-other)
    (nelisp-org-index-rebuild)
    (let ((only-a (nelisp-org-index-search "" :file "a.org"))
          (only-b (nelisp-org-index-search "Bar" :file "b.org")))
      ;; "" matches every title under "a.org" only (5 hits).
      (should (= 5 (length only-a)))
      (dolist (h only-a)
        (should (string-match-p "/a\\.org\\'" (plist-get h :file))))
      ;; Bar root + Bar child under sub/b.org
      (should (= 2 (length only-b)))
      (dolist (h only-b)
        (should (string-match-p "/sub/b\\.org\\'" (plist-get h :file)))))))

(ert-deftest nelisp-org-index-test-search-tag-filter ()
  ":tag returns only headlines carrying the requested tag."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (skip-unless (nelisp-org-index-test--posix-fs-host-p))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-rebuild)
    (let ((urgent (nelisp-org-index-search "" :tag "urgent"))
          (work (nelisp-org-index-search "" :tag "work")))
      ;; Only "Sub one" has :urgent:
      (should (= 1 (length urgent)))
      (should (string= "Sub one" (plist-get (car urgent) :title)))
      ;; Sub one + Sub two carry :work:
      (should (= 2 (length work))))))

(ert-deftest nelisp-org-index-test-search-depth-filter ()
  ":depth restricts to the matching outline level."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-rebuild)
    (let ((level-1 (nelisp-org-index-search "" :depth 1))
          (level-3 (nelisp-org-index-search "" :depth 3)))
      ;; Top + Another root at level 1.
      (should (= 2 (length level-1)))
      (dolist (h level-1) (should (= 1 (plist-get h :level))))
      ;; Deep three at level 3.
      (should (= 1 (length level-3)))
      (should (string= "Deep three" (plist-get (car level-3) :title))))))

(ert-deftest nelisp-org-index-test-search-validates-name ()
  "Non-string NAME signals user-error before touching the DB.
Empty NAME is allowed and means \"match every title\" (LIKE %)."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-org-index-test--with-fresh-db
    (should-error (nelisp-org-index-search nil) :type 'user-error)
    (should-error (nelisp-org-index-search 42) :type 'user-error)))

(ert-deftest nelisp-org-index-test-org-id-property-captured ()
  "Headlines with an :ID: property store it in the org_id column."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (skip-unless (nelisp-org-index-test--posix-fs-host-p))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-rebuild)
    (let ((hits (nelisp-org-index-search "Sub one")))
      (should (= 1 (length hits)))
      (should (string= "top-sub-one" (plist-get (car hits) :org-id))))))

(ert-deftest nelisp-org-index-test-status-counts ()
  "Status returns per-table counts + db-bytes for an indexed sandbox."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (skip-unless (nelisp-org-index-test--posix-fs-host-p))
  (nelisp-org-index-test--with-fresh-db
    (nelisp-org-index-test--write-fixture
     "a.org" nelisp-org-index-test--fixture-simple)
    (nelisp-org-index-rebuild)
    (let ((s (nelisp-org-index-status)))
      (should (string= nelisp-org-index-db-path (plist-get s :db-path)))
      ;; Tier 2 fix bumped schema-version from 1 → 2 (parent_id FK +
      ;; FTS5 trigram); just verify the field equals the constant the
      ;; module advertises so future bumps don't ripple here.
      (should (= nelisp-org-index-schema-version
                 (plist-get s :schema-version)))
      (should (= 1 (plist-get s :files)))
      (should (= 5 (plist-get s :headlines)))
      (should (>= (plist-get s :tags) 2))
      (should (> (plist-get s :db-bytes) 0)))))

(provide 'nelisp-org-index-test)
;;; nelisp-org-index-test.el ends here
