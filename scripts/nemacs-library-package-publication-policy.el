;;; nemacs-library-package-publication-policy.el --- package publication policy gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate and verify the draft publication policy for reusable package
;; archives.  This policy separates local development smokes from external
;; release requirements: local archives are unsigned draft artifacts, while
;; public release artifacts must retain checksums and detached signatures.

;;; Code:

(require 'subr-x)

(defvar nemacs-library-package-publication-policy-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-publication-policy-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-publication-policy-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-publication-policy-checksum
  (expand-file-name "build/nemacs-library-package-archive-checksum.tsv"
                    nemacs-library-package-publication-policy-repo-root)
  "Package archive checksum TSV.")

(defvar nemacs-library-package-publication-policy-index
  (expand-file-name "build/nemacs-library-package-archive-index.tsv"
                    nemacs-library-package-publication-policy-repo-root)
  "Package archive index TSV.")

(defvar nemacs-library-package-publication-policy-output
  (expand-file-name "build/nemacs-library-package-publication-policy.tsv"
                    nemacs-library-package-publication-policy-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-publication-policy-summary-output
  (expand-file-name "build/nemacs-library-package-publication-policy.org"
                    nemacs-library-package-publication-policy-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-publication-policy--signature-policy
  "external-release-detached-signatures-required"
  "Policy for published package archive signatures.")

(defconst nemacs-library-package-publication-policy--local-signature-policy
  "local-development-archives-unsigned"
  "Policy for local development package archive signatures.")

(defconst nemacs-library-package-publication-policy--checksum-policy
  "retain-sha256-for-every-tarball"
  "Policy for release checksum retention.")

(defun nemacs-library-package-publication-policy--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-publication-policy--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-publication-policy--tsv-cell
             cells "\t"))

(defun nemacs-library-package-publication-policy--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-publication-policy--read-tsv (file)
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

(defun nemacs-library-package-publication-policy--find-row
    (package rows column)
  "Return row for PACKAGE from ROWS comparing COLUMN."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth column row))
        (setq found row)))))

(defun nemacs-library-package-publication-policy--relative-readable-p
    (file)
  "Return non-nil if FILE is readable relative to the repository."
  (file-readable-p
   (expand-file-name file
                     nemacs-library-package-publication-policy-repo-root)))

(defun nemacs-library-package-publication-policy--ok-row
    (check subject policy evidence)
  "Return an ok policy row for CHECK, SUBJECT, POLICY, and EVIDENCE."
  (list check subject policy "ok" evidence ""))

(defun nemacs-library-package-publication-policy--fail-row
    (check subject policy evidence details)
  "Return a failure policy row for CHECK, SUBJECT, POLICY, EVIDENCE, DETAILS."
  (list check subject policy "fail" evidence details))

(defun nemacs-library-package-publication-policy--version-checks
    (metadata-rows)
  "Return version policy checks for METADATA-ROWS."
  (let* ((versions
          (sort (delete-dups (mapcar (lambda (row) (nth 1 row))
                                     (copy-sequence metadata-rows)))
                #'string<))
         (version (car versions))
         rows)
    (push
     (if (and version
              (string-match-p
               "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'"
               version)
              (= (length versions) 1))
         (nemacs-library-package-publication-policy--ok-row
          "shared-version"
          "all-packages"
          "single-semver-draft-version"
          (format "version=%s packages=%d" version (length metadata-rows)))
       (nemacs-library-package-publication-policy--fail-row
        "shared-version"
        "all-packages"
        "single-semver-draft-version"
        (format "versions=%s" (string-join versions ","))
        "packages must share one X.Y.Z draft version"))
     rows)
    (dolist (row metadata-rows)
      (let* ((package (nth 0 row))
             (row-version (nth 1 row))
             (install-dir (nth 15 row))
             (expected (format "%s-%s" package row-version)))
        (push
         (if (string= install-dir expected)
             (nemacs-library-package-publication-policy--ok-row
              "install-dir-version"
              package
              "package-version-install-dir"
              install-dir)
           (nemacs-library-package-publication-policy--fail-row
            "install-dir-version"
            package
            "package-version-install-dir"
            install-dir
            (format "expected %s" expected)))
         rows)))
    rows))

(defun nemacs-library-package-publication-policy--dependency-checks
    (metadata-rows)
  "Return dependency policy checks for METADATA-ROWS."
  (let (rows)
    (dolist (row metadata-rows)
      (let* ((package (nth 0 row))
             (requires
              (nemacs-library-package-publication-policy--split-list
               (nth 9 row)))
             missing)
        (dolist (dep requires)
          (unless (nemacs-library-package-publication-policy--find-row
                   dep metadata-rows 0)
            (push dep missing)))
        (push
         (if missing
             (nemacs-library-package-publication-policy--fail-row
              "dependency-version-source"
              package
              "metadata-owned-package-dependency-versions"
              (string-join requires ",")
              (format "missing dependency metadata: %s"
                      (string-join (sort missing #'string<) ",")))
           (nemacs-library-package-publication-policy--ok-row
            "dependency-version-source"
            package
            "metadata-owned-package-dependency-versions"
            (if requires
                (string-join requires ",")
              "none")))
         rows)))
    rows))

(defun nemacs-library-package-publication-policy--checksum-checks
    (metadata-rows checksum-rows index-rows)
  "Return checksum and archive coverage checks."
  (let (rows)
    (dolist (metadata metadata-rows)
      (let* ((package (nth 0 metadata))
             (version (nth 1 metadata))
             (checksum-row
              (nemacs-library-package-publication-policy--find-row
               package checksum-rows 0))
             (index-row
              (nemacs-library-package-publication-policy--find-row
               package index-rows 0)))
        (push
         (cond
          ((null checksum-row)
           (nemacs-library-package-publication-policy--fail-row
            "checksum-retention" package
            nemacs-library-package-publication-policy--checksum-policy
            "missing" "missing checksum row"))
          ((not (and (string= version (nth 1 checksum-row))
                     (string= "yes" (nth 6 checksum-row))
                     (string= "ok" (nth 7 checksum-row))
                     (= (length (nth 3 checksum-row)) 64)
                     (nemacs-library-package-publication-policy--relative-readable-p
                      (nth 2 checksum-row))))
           (nemacs-library-package-publication-policy--fail-row
            "checksum-retention" package
            nemacs-library-package-publication-policy--checksum-policy
            (format "version=%s deterministic=%s status=%s sha=%s archive=%s"
                    (or (nth 1 checksum-row) "")
                    (or (nth 6 checksum-row) "")
                    (or (nth 7 checksum-row) "")
                    (or (nth 3 checksum-row) "")
                    (or (nth 2 checksum-row) ""))
            "checksum row must match metadata, be deterministic, and point to readable archive"))
          (t
           (nemacs-library-package-publication-policy--ok-row
            "checksum-retention" package
            nemacs-library-package-publication-policy--checksum-policy
            (format "%s %s" (nth 2 checksum-row) (nth 3 checksum-row)))))
         rows)
        (push
         (cond
          ((null index-row)
           (nemacs-library-package-publication-policy--fail-row
            "archive-index-retention" package
            "published-index-must-reference-retained-archive"
            "missing" "missing archive index row"))
          ((not (and (string= version (nth 1 index-row))
                     (string= "ok" (nth 7 index-row))
                     (string= "tar" (nth 6 index-row))
                     (nemacs-library-package-publication-policy--relative-readable-p
                      (nth 4 index-row))))
           (nemacs-library-package-publication-policy--fail-row
            "archive-index-retention" package
            "published-index-must-reference-retained-archive"
            (format "version=%s kind=%s status=%s archive=%s"
                    (or (nth 1 index-row) "")
                    (or (nth 6 index-row) "")
                    (or (nth 7 index-row) "")
                    (or (nth 4 index-row) ""))
            "archive index row must match metadata and point to readable archive"))
          (t
           (nemacs-library-package-publication-policy--ok-row
            "archive-index-retention" package
            "published-index-must-reference-retained-archive"
            (nth 4 index-row))))
         rows)))
    rows))

(defun nemacs-library-package-publication-policy--signature-checks ()
  "Return signature policy rows."
  (list
   (nemacs-library-package-publication-policy--ok-row
    "local-signature-policy"
    "development-archives"
    nemacs-library-package-publication-policy--local-signature-policy
    "local package smokes disable signature verification")
   (nemacs-library-package-publication-policy--ok-row
    "release-signature-policy"
    "public-archives"
    nemacs-library-package-publication-policy--signature-policy
    "public release must add detached signatures for archive-contents and tarballs")))

(defun nemacs-library-package-publication-policy--build-rows ()
  "Return publication policy rows."
  (let* ((metadata-rows
          (nemacs-library-package-publication-policy--read-tsv
           nemacs-library-package-publication-policy-metadata))
         (checksum-rows
          (nemacs-library-package-publication-policy--read-tsv
           nemacs-library-package-publication-policy-checksum))
         (index-rows
          (nemacs-library-package-publication-policy--read-tsv
           nemacs-library-package-publication-policy-index)))
    (sort
     (append
      (nemacs-library-package-publication-policy--version-checks
       metadata-rows)
      (nemacs-library-package-publication-policy--dependency-checks
       metadata-rows)
      (nemacs-library-package-publication-policy--checksum-checks
       metadata-rows checksum-rows index-rows)
      (nemacs-library-package-publication-policy--signature-checks))
     (lambda (a b)
       (string< (mapconcat #'identity (list (nth 0 a) (nth 1 a)) "\t")
                (mapconcat #'identity (list (nth 0 b) (nth 1 b)) "\t"))))))

(defun nemacs-library-package-publication-policy--write-tsv (rows)
  "Write publication policy ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-publication-policy-output) t)
  (with-temp-file nemacs-library-package-publication-policy-output
    (insert
     (nemacs-library-package-publication-policy--row
      "check" "subject" "policy" "status" "evidence" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-publication-policy--row row)
       "\n"))))

(defun nemacs-library-package-publication-policy--write-summary (rows)
  "Write publication policy ROWS to Org summary output."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 3 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory
      nemacs-library-package-publication-policy-summary-output)
     t)
    (with-temp-file nemacs-library-package-publication-policy-summary-output
      (insert "#+TITLE: nemacs library package publication policy\n\n")
      (insert "* Summary\n\n")
      (insert (format "- checks: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n\n" fail))
      (insert "* Policy\n\n")
      (insert "- Local package archives are draft development artifacts and remain unsigned.\n")
      (insert "- Public release archives must retain SHA-256 checksums for every tarball.\n")
      (insert "- Public release archives must add detached signatures for =archive-contents= and every tarball.\n")
      (insert "- While facade packages are versioned together, all package metadata must share one =X.Y.Z= version.\n\n")
      (insert "* Checks\n\n")
      (insert "| Check | Subject | Status | Policy |\n")
      (insert "|-------+---------+--------+--------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 3 row)
                 (nth 2 row))))
      (insert "\n* Notes\n\n")
      (insert "- This target records publication policy; it does not create signatures.\n")
      (insert "- Signature artifact targets and strict release verification are covered by =nemacs-library-package-signature-policy=.\n"))))

;;;###autoload
(defun nemacs-library-package-publication-policy-batch ()
  "Generate and verify package publication policy artifacts."
  (let* ((rows
          (nemacs-library-package-publication-policy--build-rows))
         (failures 0))
    (unless rows
      (error "empty package publication policy rows"))
    (nemacs-library-package-publication-policy--write-tsv rows)
    (nemacs-library-package-publication-policy--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 3 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-publication-policy: checks=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-package-publication-policy-output
      nemacs-library-package-publication-policy-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-publication-policy)

;;; nemacs-library-package-publication-policy.el ends here
