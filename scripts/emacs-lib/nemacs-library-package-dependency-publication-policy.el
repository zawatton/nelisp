;;; nemacs-library-package-dependency-publication-policy.el --- package dependency publication policy gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that reusable package dependency exceptions are explicit enough for
;; publication review.  Package-owned lazy companions must be shipped in the
;; generated package scaffold, host Emacs features must be allowlisted, and
;; vendored nelisp dependencies must be declared separately from normal package
;; metadata dependencies.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-library-package-dependency-publication-policy-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-dependency-publication-policy-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-dependency-publication-policy-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-dependency-publication-policy-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-dependency-publication-policy-repo-root)
  "Package scaffold TSV.")

(defvar nemacs-library-package-dependency-publication-policy-deps
  (expand-file-name "build/nemacs-library-package-deps.tsv"
                    nemacs-library-package-dependency-publication-policy-repo-root)
  "Package dependency TSV.")

(defvar nemacs-library-package-dependency-publication-policy-output
  (expand-file-name "build/nemacs-library-package-dependency-publication-policy.tsv"
                    nemacs-library-package-dependency-publication-policy-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-dependency-publication-policy-summary-output
  (expand-file-name "build/nemacs-library-package-dependency-publication-policy.org"
                    nemacs-library-package-dependency-publication-policy-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-dependency-publication-policy--host-features
  '(("keymap" .
     ("host-emacs-provided-feature"
      "host Emacs keymap feature; not bundled in reusable package metadata"))
    ("pp" .
     ("host-emacs-provided-feature"
      "host Emacs pretty-printer feature; not bundled in reusable package metadata")))
  "Allowlisted host feature publication policies.")

(defconst nemacs-library-package-dependency-publication-policy--vendor-features
  '(("nelisp-process" .
     ("vendored-nelisp-package-dependency"
      "vendor/nelisp/packages/nelisp-process"
      "vendored nelisp package; release lock is verified by the vendor lock gate")))
  "Allowlisted vendored package publication policies.")

(defun nemacs-library-package-dependency-publication-policy--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-dependency-publication-policy--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat
   #'nemacs-library-package-dependency-publication-policy--tsv-cell
   cells "\t"))

(defun nemacs-library-package-dependency-publication-policy--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-dependency-publication-policy--read-tsv (file)
  "Return rows from TSV FILE without the header."
  (let (rows)
    (unless (file-readable-p file)
      (error "missing readable TSV: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string= line "")
            (push (split-string line "\t") rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun nemacs-library-package-dependency-publication-policy--repo-readable-p
    (file)
  "Return non-nil if FILE is readable relative to the repository root."
  (file-readable-p
   (expand-file-name
    file nemacs-library-package-dependency-publication-policy-repo-root)))

(defun nemacs-library-package-dependency-publication-policy--repo-directory-p
    (directory)
  "Return non-nil if DIRECTORY exists relative to the repository root."
  (file-directory-p
   (expand-file-name
    directory nemacs-library-package-dependency-publication-policy-repo-root)))

(defun nemacs-library-package-dependency-publication-policy--feature-file-name
    (feature)
  "Return the expected Elisp file name for FEATURE."
  (concat feature ".el"))

(defun nemacs-library-package-dependency-publication-policy--scaffold-lazy-rows
    (package feature scaffold-rows)
  "Return lazy scaffold rows for PACKAGE and FEATURE from SCAFFOLD-ROWS."
  (let ((expected-file
         (nemacs-library-package-dependency-publication-policy--feature-file-name
          feature))
        found)
    (dolist (row scaffold-rows (nreverse found))
      (when (and (string= (nth 0 row) "file")
                 (string= (nth 1 row) package)
                 (string= (nth 2 row) "lazy")
                 (string= (file-name-nondirectory (nth 3 row))
                          expected-file)
                 (string= (file-name-nondirectory (nth 4 row))
                          expected-file))
        (push row found)))))

(defun nemacs-library-package-dependency-publication-policy--deps-relation-p
    (feature relation deps-rows)
  "Return non-nil if FEATURE has RELATION in DEPS-ROWS."
  (cl-some
   (lambda (row)
     (and (string= (nth 3 row) feature)
          (string= (nth 7 row) relation)))
   deps-rows))

(defun nemacs-library-package-dependency-publication-policy--ok-row
    (category package feature policy evidence details)
  "Return an ok policy row."
  (list category package feature policy "ok" evidence details))

(defun nemacs-library-package-dependency-publication-policy--fail-row
    (category package feature policy evidence details)
  "Return a failure policy row."
  (list category package feature policy "fail" evidence details))

(defun nemacs-library-package-dependency-publication-policy--lazy-checks
    (metadata-rows scaffold-rows)
  "Return lazy companion publication policy checks."
  (let (rows)
    (dolist (metadata metadata-rows)
      (let ((package (nth 0 metadata)))
        (dolist (feature
                 (nemacs-library-package-dependency-publication-policy--split-list
                  (nth 11 metadata)))
          (let* ((scaffold-rows
                  (nemacs-library-package-dependency-publication-policy--scaffold-lazy-rows
                   package feature scaffold-rows))
                 (scaffold-row (car scaffold-rows))
                 (source (nth 3 scaffold-row))
                 (target (nth 4 scaffold-row))
                 (target-prefix (format "packages/%s/lazy/" package))
                 (jis-note (if (string= feature "nelisp-coding-jis-tables")
                               "generated JIS table is shipped as lazy package companion"
                             "package-owned lazy companion")))
            (push
             (cond
              ((not (= (length scaffold-rows) 1))
               (nemacs-library-package-dependency-publication-policy--fail-row
                "lazy-companion" package feature
                "packaged-lazy-companion"
                (format "matches=%d" (length scaffold-rows))
                "metadata lazy feature must have exactly one lazy scaffold file"))
              ((not (and (string-prefix-p target-prefix target)
                         (nemacs-library-package-dependency-publication-policy--repo-readable-p
                          source)
                         (nemacs-library-package-dependency-publication-policy--repo-readable-p
                          target)))
               (nemacs-library-package-dependency-publication-policy--fail-row
                "lazy-companion" package feature
                "packaged-lazy-companion"
                (format "source=%s target=%s" source target)
                "lazy companion source and target must be readable and staged under package lazy/"))
              (t
               (nemacs-library-package-dependency-publication-policy--ok-row
                "lazy-companion" package feature
                "packaged-lazy-companion"
                target
                jis-note)))
             rows)))))
    rows))

(defun nemacs-library-package-dependency-publication-policy--host-checks
    (metadata-rows deps-rows)
  "Return host feature publication policy checks."
  (let (rows)
    (dolist (metadata metadata-rows)
      (let ((package (nth 0 metadata)))
        (dolist (feature
                 (nemacs-library-package-dependency-publication-policy--split-list
                  (nth 12 metadata)))
          (let ((policy
                 (cdr (assoc feature
                             nemacs-library-package-dependency-publication-policy--host-features)))
                (has-relation
                 (nemacs-library-package-dependency-publication-policy--deps-relation-p
                  feature "host-feature" deps-rows)))
            (push
             (cond
              ((null policy)
               (nemacs-library-package-dependency-publication-policy--fail-row
                "host-feature" package feature
                "host-feature-allowlist"
                "missing"
                "host feature must be allowlisted with publication rationale"))
              ((not has-relation)
               (nemacs-library-package-dependency-publication-policy--fail-row
                "host-feature" package feature
                (car policy)
                "missing host-feature dependency relation"
                "metadata host feature must match dependency graph relation"))
              (t
               (nemacs-library-package-dependency-publication-policy--ok-row
                "host-feature" package feature
                (car policy)
                "dependency relation host-feature"
                (cadr policy))))
             rows)))))
    rows))

(defun nemacs-library-package-dependency-publication-policy--vendor-checks
    (metadata-rows deps-rows)
  "Return vendored package publication policy checks."
  (let (rows)
    (dolist (metadata metadata-rows)
      (let ((package (nth 0 metadata)))
        (dolist (feature
                 (nemacs-library-package-dependency-publication-policy--split-list
                  (nth 13 metadata)))
          (let* ((policy
                  (cdr (assoc feature
                              nemacs-library-package-dependency-publication-policy--vendor-features)))
                 (policy-name (car policy))
                 (directory (cadr policy))
                 (details (caddr policy))
                 (has-relation
                  (nemacs-library-package-dependency-publication-policy--deps-relation-p
                   feature "vendor-package" deps-rows)))
            (push
             (cond
              ((null policy)
               (nemacs-library-package-dependency-publication-policy--fail-row
                "vendor-package" package feature
                "vendor-package-allowlist"
                "missing"
                "vendored package must be allowlisted with publication rationale"))
              ((not has-relation)
               (nemacs-library-package-dependency-publication-policy--fail-row
                "vendor-package" package feature
                policy-name
                "missing vendor-package dependency relation"
                "metadata vendored package must match dependency graph relation"))
              ((not (nemacs-library-package-dependency-publication-policy--repo-directory-p
                     directory))
               (nemacs-library-package-dependency-publication-policy--fail-row
                "vendor-package" package feature
                policy-name
                directory
                "vendored package directory is missing"))
              (t
               (nemacs-library-package-dependency-publication-policy--ok-row
                "vendor-package" package feature
                policy-name
                directory
                details)))
             rows)))))
    rows))

(defun nemacs-library-package-dependency-publication-policy--unexpected-checks
    (deps-rows)
  "Return checks for dependency relations that are not publication-ready."
  (let ((blocked '("external-or-host"
                   "unmanifested-reusable"
                   "lazy-unmanifested-reusable"
                   "app-or-frontend"))
        rows)
    (dolist (relation blocked)
      (let ((count 0))
        (dolist (row deps-rows)
          (when (string= (nth 7 row) relation)
            (setq count (1+ count))))
        (push
         (if (= count 0)
             (nemacs-library-package-dependency-publication-policy--ok-row
              "dependency-relation" "all-packages" relation
              "no-unpublished-dependency-relation"
              "count=0"
              "dependency graph has no relation requiring publication triage")
           (nemacs-library-package-dependency-publication-policy--fail-row
            "dependency-relation" "all-packages" relation
            "no-unpublished-dependency-relation"
            (format "count=%d" count)
            "dependency graph still has unpublished dependency relations"))
         rows)))
    rows))

(defun nemacs-library-package-dependency-publication-policy--build-rows ()
  "Return dependency publication policy rows."
  (let* ((metadata-rows
          (nemacs-library-package-dependency-publication-policy--read-tsv
           nemacs-library-package-dependency-publication-policy-metadata))
         (scaffold-rows
          (nemacs-library-package-dependency-publication-policy--read-tsv
           nemacs-library-package-dependency-publication-policy-scaffold))
         (deps-rows
          (nemacs-library-package-dependency-publication-policy--read-tsv
           nemacs-library-package-dependency-publication-policy-deps)))
    (sort
     (append
      (nemacs-library-package-dependency-publication-policy--lazy-checks
       metadata-rows scaffold-rows)
      (nemacs-library-package-dependency-publication-policy--host-checks
       metadata-rows deps-rows)
      (nemacs-library-package-dependency-publication-policy--vendor-checks
       metadata-rows deps-rows)
      (nemacs-library-package-dependency-publication-policy--unexpected-checks
       deps-rows))
     (lambda (a b)
       (string< (mapconcat #'identity (list (nth 0 a) (nth 1 a) (nth 2 a)) "\t")
                (mapconcat #'identity (list (nth 0 b) (nth 1 b) (nth 2 b)) "\t"))))))

(defun nemacs-library-package-dependency-publication-policy--write-tsv
    (rows)
  "Write dependency publication policy ROWS to TSV output."
  (make-directory
   (file-name-directory
    nemacs-library-package-dependency-publication-policy-output)
   t)
  (with-temp-file nemacs-library-package-dependency-publication-policy-output
    (insert
     (nemacs-library-package-dependency-publication-policy--row
      "category" "package_id" "feature" "policy" "status" "evidence" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-dependency-publication-policy--row row)
       "\n"))))

(defun nemacs-library-package-dependency-publication-policy--count-category
    (category rows)
  "Return number of ROWS in CATEGORY."
  (cl-count-if
   (lambda (row) (string= (nth 0 row) category))
   rows))

(defun nemacs-library-package-dependency-publication-policy--write-summary
    (rows)
  "Write dependency publication policy ROWS to Org summary output."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 4 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory
      nemacs-library-package-dependency-publication-policy-summary-output)
     t)
    (with-temp-file nemacs-library-package-dependency-publication-policy-summary-output
      (insert "#+TITLE: nemacs library package dependency publication policy\n\n")
      (insert "* Summary\n\n")
      (insert (format "- checks: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n" fail))
      (insert
       (format "- lazy companion rows: %d\n"
               (nemacs-library-package-dependency-publication-policy--count-category
                "lazy-companion" rows)))
      (insert
       (format "- host feature rows: %d\n"
               (nemacs-library-package-dependency-publication-policy--count-category
                "host-feature" rows)))
      (insert
       (format "- vendor package rows: %d\n\n"
               (nemacs-library-package-dependency-publication-policy--count-category
                "vendor-package" rows)))
      (insert "* Policy\n\n")
      (insert "- Lazy companions are shipped inside the owning reusable package under =lazy/= and are not package metadata dependencies.\n")
      (insert "- Host features must be allowlisted as provided by host Emacs.\n")
      (insert "- Vendored nelisp dependencies must be allowlisted separately from generated facade package dependencies.\n")
      (insert "- Unknown external, app/frontend, and unmanifested reusable relations must remain empty.\n\n")
      (insert "* Checks\n\n")
      (insert "| Category | Package | Feature | Status | Policy |\n")
      (insert "|----------+---------+---------+--------+--------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | =%s= | %s | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 2 row)
                 (nth 4 row)
                 (nth 3 row))))
      (insert "\n* Notes\n\n")
      (insert "- This target records dependency publication policy and classification.\n")
      (insert "- Vendored package release locks and lazy companion closure metadata are verified by dedicated gates.\n"))))

;;;###autoload
(defun nemacs-library-package-dependency-publication-policy-batch ()
  "Generate and verify dependency publication policy artifacts."
  (let* ((rows
          (nemacs-library-package-dependency-publication-policy--build-rows))
         (failures 0))
    (unless rows
      (error "empty dependency publication policy rows"))
    (nemacs-library-package-dependency-publication-policy--write-tsv rows)
    (nemacs-library-package-dependency-publication-policy--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 4 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-dependency-publication-policy: checks=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-package-dependency-publication-policy-output
      nemacs-library-package-dependency-publication-policy-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-dependency-publication-policy)

;;; nemacs-library-package-dependency-publication-policy.el ends here
