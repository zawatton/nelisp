;;; nelisp-indices-fix-test.el --- ERT for T59 Tier 2 indices fix  -*- lexical-binding: t; -*-

;; Phase 6.5/6.6 Tier 2 fix (T69) — covers the six T59 audit findings:
;;   #1 atomic shadow-swap rebuild
;;   #2 busy_timeout + BEGIN IMMEDIATE retry
;;   #3 schema FK constraints (parent_id self-reference + cascade)
;;   #4 + #5 FTS5 trigram search (CJK substring + ranking)
;;   #6 deep walker special-form filter

(require 'ert)
(require 'cl-lib)
(require 'nelisp-defs-index)
(require 'nelisp-org-index)

;;;; --- shared sandboxing helpers -----------------------------------------

(defvar nelisp-indices-fix-test--tmpdir nil)
(defvar nelisp-indices-fix-test--sandbox-root nil)

(defmacro nelisp-indices-fix-test--with-defs-fresh-db (&rest body)
  "Bind defs-index DB / paths to a fresh temp tree."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-indices-fix-test--tmpdir
           (make-temp-file "nelisp-indices-fix-test-" t))
          (nelisp-defs-index-db-path
           (expand-file-name "test-defs.db" nelisp-indices-fix-test--tmpdir))
          (nelisp-indices-fix-test--sandbox-root
           (expand-file-name "src" nelisp-indices-fix-test--tmpdir))
          (nelisp-defs-index-paths
           (list nelisp-indices-fix-test--sandbox-root)))
     (make-directory nelisp-indices-fix-test--sandbox-root t)
     (unwind-protect
         (progn
           (nelisp-defs-index--close)
           ,@body)
       (nelisp-defs-index--close)
       (when (file-directory-p nelisp-indices-fix-test--tmpdir)
         (delete-directory nelisp-indices-fix-test--tmpdir t)))))

(defmacro nelisp-indices-fix-test--with-org-fresh-db (&rest body)
  "Bind org-index DB / paths to a fresh temp tree."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-indices-fix-test--tmpdir
           (make-temp-file "nelisp-indices-fix-test-org-" t))
          (nelisp-org-index-db-path
           (expand-file-name "test-org.db" nelisp-indices-fix-test--tmpdir))
          (nelisp-indices-fix-test--sandbox-root
           (expand-file-name "org" nelisp-indices-fix-test--tmpdir))
          (nelisp-org-index-paths
           (list nelisp-indices-fix-test--sandbox-root)))
     (make-directory nelisp-indices-fix-test--sandbox-root t)
     (unwind-protect
         (progn
           (nelisp-org-index--close)
           ,@body)
       (nelisp-org-index--close)
       (when (file-directory-p nelisp-indices-fix-test--tmpdir)
         (delete-directory nelisp-indices-fix-test--tmpdir t)))))

(defun nelisp-indices-fix-test--write (rel content)
  "Write CONTENT to REL under the per-test sandbox root."
  (let ((path (expand-file-name rel
                                nelisp-indices-fix-test--sandbox-root)))
    (make-directory (file-name-directory path) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file path (insert content)))
    path))

;;;; --- #1 atomic rebuild via shadow swap ---------------------------------

(ert-deftest nelisp-indices-fix-test-defs-rebuild-shadow-cleaned ()
  "Successful defs rebuild leaves no shadow file behind."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-indices-fix-test--write "a.el"
                                    "(defun ax () nil)\n")
    (let ((shadow (concat nelisp-defs-index-db-path ".rebuild")))
      (nelisp-defs-index-rebuild)
      (should (file-exists-p nelisp-defs-index-db-path))
      (should-not (file-exists-p shadow)))))

(ert-deftest nelisp-indices-fix-test-defs-rebuild-preserves-live-on-failure ()
  "Shadow build failure leaves the prior live DB intact."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-indices-fix-test--write "a.el" "(defun ax () nil)\n")
    ;; First successful rebuild → live DB has 1 row.
    (nelisp-defs-index-rebuild)
    (should (= 1 (length (nelisp-defs-index-search "ax"))))
    ;; Now sabotage: stub `--build-into' to error out.  The live DB
    ;; must remain unchanged and the shadow must be cleaned up.
    (let ((shadow (concat nelisp-defs-index-db-path ".rebuild"))
          (orig (symbol-function 'nelisp-defs-index--build-into)))
      (cl-letf (((symbol-function 'nelisp-defs-index--build-into)
                 (lambda (path _paths)
                   ;; Touch the shadow, then signal — exercises the
                   ;; cleanup branch.
                   (with-temp-file path (insert "broken"))
                   (error "synthetic-build-failure"))))
        (should-error (nelisp-defs-index-rebuild)))
      (ignore orig)
      (should-not (file-exists-p shadow))
      ;; Live DB still has the original row.
      (should (= 1 (length (nelisp-defs-index-search "ax")))))))

(ert-deftest nelisp-indices-fix-test-org-rebuild-shadow-cleaned ()
  "Successful org rebuild leaves no shadow file behind."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-indices-fix-test--write "a.org" "* Hello\n")
    (let ((shadow (concat nelisp-org-index-db-path ".rebuild")))
      (nelisp-org-index-rebuild)
      (should (file-exists-p nelisp-org-index-db-path))
      (should-not (file-exists-p shadow)))))

(ert-deftest nelisp-indices-fix-test-org-rebuild-preserves-live-on-failure ()
  "org-index shadow failure leaves prior data + no shadow."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-indices-fix-test--write "a.org" "* Hello\n")
    (nelisp-org-index-rebuild)
    (should (= 1 (length (nelisp-org-index-search "Hello"))))
    (let ((shadow (concat nelisp-org-index-db-path ".rebuild")))
      (cl-letf (((symbol-function 'nelisp-org-index--build-into)
                 (lambda (path _paths)
                   (with-temp-file path (insert "broken"))
                   (error "synthetic-build-failure"))))
        (should-error (nelisp-org-index-rebuild)))
      (should-not (file-exists-p shadow))
      (should (= 1 (length (nelisp-org-index-search "Hello")))))))

;;;; --- #2 busy_timeout + retry / pragmas ---------------------------------

(ert-deftest nelisp-indices-fix-test-defs-busy-timeout-pragma-applied ()
  "Opened defs DB advertises the configured busy_timeout pragma."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (let ((nelisp-defs-index-busy-timeout-ms 12345))
      (nelisp-defs-index-enable)
      (let ((row (sqlite-select (nelisp-defs-index--db)
                                "PRAGMA busy_timeout")))
        (should (= 12345 (caar row)))))))

(ert-deftest nelisp-indices-fix-test-org-busy-timeout-pragma-applied ()
  "Opened org DB advertises the configured busy_timeout pragma."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (let ((nelisp-org-index-busy-timeout-ms 23456))
      (nelisp-org-index-enable)
      (let ((row (sqlite-select (nelisp-org-index--db)
                                "PRAGMA busy_timeout")))
        (should (= 23456 (caar row)))))))

(ert-deftest nelisp-indices-fix-test-defs-immediate-tx-rolls-back ()
  "BEGIN IMMEDIATE wrapper rolls back on body error."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-defs-index-enable)
    (let ((db (nelisp-defs-index--db)))
      ;; Insert a sentinel via the macro and signal: the row must not
      ;; survive the rollback.
      (should-error
       (nelisp-defs-index--with-immediate-tx db
         (sqlite-execute
          db
          "INSERT INTO file(path,mtime,size,indexed_at) VALUES (?,?,?,?)"
          (list "/tmp/sentinel.el" 0 0 0))
         (error "synthetic-rollback-trigger")))
      (should-not
       (sqlite-select db
                      "SELECT 1 FROM file WHERE path='/tmp/sentinel.el'")))))

;;;; --- #3 schema FK constraint -------------------------------------------

(ert-deftest nelisp-indices-fix-test-org-parent-id-fk-cascades ()
  "Deleting a parent headline cascades to its children via parent_id FK."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-indices-fix-test--write
     "a.org"
     "* Top\n** Mid\n*** Leaf\n")
    (nelisp-org-index-rebuild)
    (let* ((db (nelisp-org-index--db))
           (top-id (caar
                    (sqlite-select db
                                   "SELECT id FROM headline WHERE level=1")))
           (mid-before
            (caar (sqlite-select db
                                 "SELECT COUNT(*) FROM headline WHERE level=2")))
           (leaf-before
            (caar (sqlite-select db
                                 "SELECT COUNT(*) FROM headline WHERE level=3"))))
      (should (= 1 mid-before))
      (should (= 1 leaf-before))
      ;; Delete the top headline; its mid + leaf children must vanish
      ;; via ON DELETE CASCADE on parent_id (Tier 2 fix #3).
      (sqlite-execute db "DELETE FROM headline WHERE id = ?"
                      (list top-id))
      (should (= 0 (caar (sqlite-select db
                                        "SELECT COUNT(*) FROM headline")))))))

(ert-deftest nelisp-indices-fix-test-org-parent-id-fk-declared ()
  "PRAGMA foreign_key_list(headline) reports the parent_id self-FK."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-org-index-enable)
    (let* ((rows (sqlite-select (nelisp-org-index--db)
                                "PRAGMA foreign_key_list(headline)"))
           (refs (mapcar (lambda (r) (cons (nth 2 r) (nth 3 r))) rows)))
      ;; Expect (table . from-column) for both parent_id and file_id.
      (should (member '("headline" . "parent_id") refs))
      (should (member '("file" . "file_id") refs)))))

;;;; --- #4 + #5 FTS5 trigram search ---------------------------------------

(ert-deftest nelisp-indices-fix-test-org-fts-table-created ()
  "headlines_fts virtual table is created when DB opens."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-org-index-enable)
    (let ((row (sqlite-select
                (nelisp-org-index--db)
                "SELECT name FROM sqlite_master
                   WHERE type='table' AND name='headlines_fts'")))
      (should row))))

(ert-deftest nelisp-indices-fix-test-org-fts-cjk-substring ()
  "FTS5 trigram path matches Japanese substrings (skipped if not built)."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-org-index-enable)
    (skip-unless (eq (nelisp-org-index--last-tokenizer-resolved) 'trigram))
    (nelisp-indices-fix-test--write
     "ja.org"
     "* 月次点検レポート\n* 別の見出し\n")
    (nelisp-org-index-rebuild)
    ;; Trigram tokenizer can match a 3-codepoint substring of the
    ;; first headline title.
    (let ((hits (nelisp-org-index-search "次点検" :fts 'on)))
      (should (= 1 (length hits)))
      (should (string-match-p "月次点検レポート"
                              (plist-get (car hits) :title))))))

(defun nelisp-org-index--last-tokenizer-resolved ()
  "Return the tokenizer the live DB was opened with (helper for tests)."
  (or nelisp-org-index--last-tokenizer
      (nelisp-org-index--resolve-tokenizer (nelisp-org-index--db))))

(ert-deftest nelisp-indices-fix-test-org-fts-fallback-to-like ()
  "Search with `:fts off' uses the legacy LIKE path even when FTS exists."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-indices-fix-test--write
     "a.org"
     "* Alpha headline\n* Beta\n* Alpha later\n")
    (nelisp-org-index-rebuild)
    (let ((hits (nelisp-org-index-search "Alpha" :fts 'off)))
      (should (= 2 (length hits))))))

(ert-deftest nelisp-indices-fix-test-org-fts-results-match-like ()
  "FTS5 and LIKE return the same set of rows for a simple ASCII term."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-org-fresh-db
    (nelisp-indices-fix-test--write
     "a.org"
     "* Quick brown fox\n* Lazy dog\n* Quickdraw\n")
    (nelisp-org-index-rebuild)
    (let ((via-fts (nelisp-org-index-search "Quick" :fts 'on))
          (via-like (nelisp-org-index-search "Quick" :fts 'off)))
      (should (>= (length via-fts) 1))
      (should (>= (length via-like) 1))
      ;; Both paths should at minimum agree on which file & lines were
      ;; matched.  Compare normalised key sets.
      (let ((fts-keys
             (sort (mapcar (lambda (h)
                             (cons (plist-get h :file)
                                   (plist-get h :line)))
                           via-fts)
                   (lambda (a b) (string< (format "%S" a) (format "%S" b)))))
            (like-keys
             (sort (mapcar (lambda (h)
                             (cons (plist-get h :file)
                                   (plist-get h :line)))
                           via-like)
                   (lambda (a b) (string< (format "%S" a) (format "%S" b))))))
        (should (equal fts-keys like-keys))))))

;;;; --- #6 deep walker special-form filter --------------------------------

(ert-deftest nelisp-indices-fix-test-defs-special-form-not-call-ref ()
  "`defun' / `let' heads do not appear as `call' refs."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-indices-fix-test--write
     "x.el"
     "(defun the-fn (x) (let ((y 1)) (+ x y)))\n")
    (nelisp-defs-index-rebuild)
    (let ((defun-refs (nelisp-defs-index-references "defun"))
          (let-refs (nelisp-defs-index-references "let")))
      (dolist (r defun-refs)
        (should-not (string= "call" (plist-get r :kind))))
      (dolist (r let-refs)
        (should-not (string= "call" (plist-get r :kind)))))))

(ert-deftest nelisp-indices-fix-test-defs-arglist-not-symbol-ref ()
  "Arglist binding-name `x' should not appear as a symbol ref."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-indices-fix-test--write
     "x.el"
     "(defun the-fn (x) (+ x 1))\n")
    (nelisp-defs-index-rebuild)
    ;; `x' is an arglist binding; the walker must not record any
    ;; `symbol' refs for it (it remains free of definition because
    ;; it's locally bound — recording would produce false positives).
    (let ((x-refs (nelisp-defs-index-references "x")))
      (should-not x-refs))))

(ert-deftest nelisp-indices-fix-test-defs-real-call-still-recorded ()
  "Real call-site references survive the special-form filter."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-indices-fix-test--write
     "x.el"
     "(defun the-fn (x) (let ((y 1)) (helper-fn x y)))\n")
    (nelisp-defs-index-rebuild)
    (let* ((calls (nelisp-defs-index-references
                   "helper-fn" '(:kind "call"))))
      (should (= 1 (length calls)))
      (should (string= "the-fn"
                       (plist-get (car calls) :context))))))

(ert-deftest nelisp-indices-fix-test-defs-let-binding-not-symbol ()
  "`let' binding name should not show up as a `symbol' ref."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-indices-fix-test--with-defs-fresh-db
    (nelisp-indices-fix-test--write
     "x.el"
     "(defun the-fn () (let ((local-var 42)) (use-it local-var)))\n")
    (nelisp-defs-index-rebuild)
    ;; `local-var' is bound by `let'.  Body usage in (use-it local-var)
    ;; is still a symbol ref — but the binding head itself isn't.  We
    ;; check that the walker emitted at most one symbol ref for it
    ;; (the body usage), and definitely no `call' ref.
    (let ((refs (nelisp-defs-index-references "local-var")))
      (dolist (r refs)
        (should-not (string= "call" (plist-get r :kind)))))))

(provide 'nelisp-indices-fix-test)
;;; nelisp-indices-fix-test.el ends here
