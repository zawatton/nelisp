;;; nelisp-dev-audit-test.el --- ERT tests for nelisp-dev-audit  -*- lexical-binding: t; -*-

;; Phase 7+ replan-gate scanner ERT.  Tests run against the actual
;; docs/design tree in the repo (Doc 27-32 v2 LOCKED snapshot).

(require 'ert)
(require 'cl-lib)
(require 'nelisp-dev-audit)

(defun nelisp-dev-audit-test--design-dir ()
  "Resolve the docs/design dir from the test file's location.

`load-file-name' is set during -l, but is nil under
`ert-run-tests-batch-and-exit'.  Fall back to `find-library-name'
on a feature provided by this test file, then to the cwd."
  (let* ((this (or load-file-name
                   buffer-file-name
                   (ignore-errors
                     (find-library-name "nelisp-dev-audit-test"))))
         (test-dir (and this (file-name-directory this)))
         (root (file-name-as-directory
                (expand-file-name
                 ".."
                 (or test-dir default-directory)))))
    (expand-file-name "docs/design/" root)))

(defmacro nelisp-dev-audit-test--with-design-dir (&rest body)
  "Bind `nelisp-dev-audit-design-dir' to the repo design tree."
  (declare (indent 0) (debug t))
  `(let ((nelisp-dev-audit-design-dir
          (nelisp-dev-audit-test--design-dir)))
     ,@body))

;;;; 1. scan-design-docs finds 6 docs (27/28/29/30/31/32) ----------

(ert-deftest nelisp-dev-audit-scan-design-docs-finds-6-docs ()
  "Phase 7+ doc inventory matches `nelisp-dev-audit--doc-numbers'."
  (nelisp-dev-audit-test--with-design-dir
   (let* ((docs (nelisp-dev-audit--scan-design-docs))
          (ids  (mapcar (lambda (d) (plist-get d :doc)) docs)))
     (should (equal (sort (copy-sequence ids) #'<) '(27 28 29 30 31 32)))
     (should (= (length docs) 6)))))

;;;; 2. Doc 28 v2 LOCKED state detected -----------------------------

(ert-deftest nelisp-dev-audit-doc28-locked-v2-detected ()
  "Doc 28 frontmatter is LOCKED + version v2 + date 2026-04-25."
  (nelisp-dev-audit-test--with-design-dir
   (let* ((docs (nelisp-dev-audit--scan-design-docs))
          (doc28 (cl-find 28 docs :key (lambda (d) (plist-get d :doc)))))
     (should doc28)
     (should (eq (plist-get doc28 :state) 'LOCKED))
     (should (equal (plist-get doc28 :state-version) "v2"))
     (should (equal (plist-get doc28 :state-date) "2026-04-25")))))

;;;; 3. Phase 7.0 SHIPPED stamp inside Doc 27 ---------------------

(ert-deftest nelisp-dev-audit-doc27-shipped-stamp-7.0 ()
  "Doc 27 §3 carries a SHIPPED stamp for Phase 7.0 (syscall stub)."
  (nelisp-dev-audit-test--with-design-dir
   (let* ((docs (nelisp-dev-audit--scan-design-docs))
          (doc27 (cl-find 27 docs :key (lambda (d) (plist-get d :doc))))
          (stamps (nelisp-dev-audit--scan-shipped-stamps doc27))
          (sp7.0 (assoc "7.0" stamps)))
     (should doc27)
     (should sp7.0)
     (should (plist-get (cdr sp7.0) :date))
     ;; The stamp says SHIPPED 2026-04-25 in the doc body
     (should (string-match-p
              "\\`2026-" (plist-get (cdr sp7.0) :date))))))

;;;; 4. Doc 28 gate-W4 at week 4 with no implementation = pending --

(ert-deftest nelisp-dev-audit-replan-gate-week-4-doc28-pending ()
  "Doc 28 gate-W4 fires `pending' when sub-phase 7.1.1 is absent.

The actual repo tree carries no Phase 7.1.1 SHIPPED stamp yet.
With current-week=nil we expect a 'pending result (week未到来扱い)."
  (nelisp-dev-audit-test--with-design-dir
   (let* ((docs (nelisp-dev-audit--scan-design-docs))
          (res (nelisp-dev-audit-evaluate-replan-gate 28 nil docs)))
     (should (eq (plist-get res :gate) 'gate-W4))
     (should (eq (plist-get res :status) 'pending))
     (should (member "7.1.1"
                     (plist-get res :missing-sub-phases))))))

;;;; 5. report buffer contains expected sections -------------------

(ert-deftest nelisp-dev-audit-report-buffer-content ()
  "`nelisp-dev-audit-report' renders the 4 standard sections."
  ;; CI-smoke gate: on Windows native Emacs the report renderer
  ;; transitively invokes a subprocess (git log walker) that reads
  ;; from stdin; in `--batch -Q' mode stdin is closed so the call
  ;; signals `(end-of-file "Error reading from stdin")'.  The renderer
  ;; works fine on POSIX hosts; skip the assertion on Windows until
  ;; the reader path is made stdin-free.
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (nelisp-dev-audit-test--with-design-dir
   (unwind-protect
       (let ((buf (nelisp-dev-audit-report 'all)))
         (with-current-buffer buf
           (let ((s (buffer-string)))
             (should (string-match-p "Doc state summary" s))
             (should (string-match-p "Sub-phase SHIPPED stamps" s))
             (should (string-match-p "Replan gate evaluation" s))
             (should (string-match-p "Outstanding TODO" s))
             ;; Doc state table contains all 6 doc rows.
             (dolist (n '(27 28 29 30 31 32))
               (should (string-match-p
                        (format "| %2d  |" n) s))))))
     (when (get-buffer nelisp-dev-audit-buffer-name)
       (kill-buffer nelisp-dev-audit-buffer-name)))))

(provide 'nelisp-dev-audit-test)
;;; nelisp-dev-audit-test.el ends here
