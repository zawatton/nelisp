;;; nemacs-library-package-release-bundle-smoke.el --- smoke release bundle archive -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify a retained package release bundle before treating it as a
;; publishable package archive.  The smoke checks retained manifest hashes,
;; then installs generated packages from the bundle's archives/ directory via
;; `package-refresh-contents' and `package-install'.

;;; Code:

(require 'package)

(defvar nemacs-library-package-release-bundle-smoke-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-release-bundle-smoke-package
  (getenv "NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_PACKAGE")
  "Package id to smoke-test.")

(defvar nemacs-library-package-release-bundle-smoke-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-release-bundle-smoke-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-release-bundle-smoke-manifest
  (expand-file-name "build/nemacs-library-package-release-bundle-manifest.tsv"
                    nemacs-library-package-release-bundle-smoke-repo-root)
  "Release bundle manifest TSV.")

(defvar nemacs-library-package-release-bundle-smoke-bundle-root
  (expand-file-name "build/nemacs-library-package-release-bundle"
                    nemacs-library-package-release-bundle-smoke-repo-root)
  "Release bundle root.")

(defvar nemacs-library-package-release-bundle-smoke-install-root
  (expand-file-name "build/nemacs-library-package-release-bundle-smoke/install"
                    nemacs-library-package-release-bundle-smoke-repo-root)
  "Temporary package-user-dir root.")

(defvar nemacs-library-package-release-bundle-smoke-output
  (expand-file-name "build/nemacs-library-package-release-bundle-smoke.tsv"
                    nemacs-library-package-release-bundle-smoke-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-release-bundle-smoke-summary-output
  (expand-file-name "build/nemacs-library-package-release-bundle-smoke.org"
                    nemacs-library-package-release-bundle-smoke-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-release-bundle-smoke--archive-name
  "nelisp-emacs-release-bundle"
  "Local package archive name used by release bundle smoke.")

(defun nemacs-library-package-release-bundle-smoke--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-release-bundle-smoke--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-release-bundle-smoke--tsv-cell
             cells "\t"))

(defun nemacs-library-package-release-bundle-smoke--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-release-bundle-smoke--read-tsv (file)
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

(defun nemacs-library-package-release-bundle-smoke--find-row
    (package rows)
  "Return metadata row for PACKAGE from ROWS."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth 0 row))
        (setq found row)))))

(defun nemacs-library-package-release-bundle-smoke--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file
                    nemacs-library-package-release-bundle-smoke-repo-root))

(defun nemacs-library-package-release-bundle-smoke--hash-file (file)
  "Return SHA-256 digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-release-bundle-smoke--file-bytes (file)
  "Return byte size for FILE."
  (number-to-string (nth 7 (file-attributes file))))

(defun nemacs-library-package-release-bundle-smoke--verify-manifest ()
  "Verify retained release bundle manifest rows.
Return a plist with retained and pending counts."
  (let ((rows
         (nemacs-library-package-release-bundle-smoke--read-tsv
          nemacs-library-package-release-bundle-smoke-manifest))
        (retained 0)
        (pending 0)
        (archive-artifacts 0)
        (failures 0))
    (dolist (row rows)
      (let* ((artifact-type (nth 0 row))
             (bundle-file (nth 3 row))
             (expected-bytes (nth 4 row))
             (expected-sha256 (nth 5 row))
             (retained-p (string= (nth 7 row) "yes"))
             (status (nth 9 row))
             (path
              (nemacs-library-package-release-bundle-smoke--absolute
               bundle-file)))
        (cond
         ((string= status "fail")
          (setq failures (1+ failures)))
         (retained-p
          (setq retained (1+ retained))
          (when (member artifact-type '("archive-index" "package-tarball"))
            (setq archive-artifacts (1+ archive-artifacts)))
          (unless (file-readable-p path)
            (error "manifest retained file is not readable: %s" bundle-file))
          (unless (string= expected-bytes
                           (nemacs-library-package-release-bundle-smoke--file-bytes
                            path))
            (error "manifest byte mismatch for %s" bundle-file))
          (unless (string= expected-sha256
                           (nemacs-library-package-release-bundle-smoke--hash-file
                            path))
            (error "manifest sha256 mismatch for %s" bundle-file)))
         ((string= status "pending")
          (setq pending (1+ pending))))))
    (unless (eq failures 0)
      (error "release bundle manifest contains %d failure rows" failures))
    (unless (eq archive-artifacts 9)
      (error "release bundle retained archive artifact count mismatch: %d"
             archive-artifacts))
    (list :retained retained
          :pending pending
          :archive-artifacts archive-artifacts)))

(defun nemacs-library-package-release-bundle-smoke--source-leaks ()
  "Return loaded files from repo src/ or generated packages/ scaffold."
  (let ((src-prefix
         (file-truename
          (expand-file-name "src/"
                            nemacs-library-package-release-bundle-smoke-repo-root)))
        (packages-prefix
         (file-truename
          (expand-file-name "packages/"
                            nemacs-library-package-release-bundle-smoke-repo-root)))
        leaks)
    (dolist (entry load-history)
      (let ((file (car entry)))
        (when (stringp file)
          (let ((true (file-truename file)))
            (when (or (string-prefix-p src-prefix true)
                      (string-prefix-p packages-prefix true))
              (push true leaks))))))
    (sort (delete-dups leaks) #'string<)))

(defun nemacs-library-package-release-bundle-smoke--installed-dependencies
    (package)
  "Return installed dependencies for PACKAGE from `package-alist'."
  (let* ((cell (assq (intern package) package-alist))
         (desc (cadr cell))
         dependencies)
    (dolist (req (and desc (package-desc-reqs desc)))
      (push (symbol-name (car req)) dependencies))
    (sort (delete-dups dependencies) #'string<)))

(defun nemacs-library-package-release-bundle-smoke--archive-location ()
  "Return local package archive location for the release bundle."
  (file-name-as-directory
   (expand-file-name
    "archives"
    nemacs-library-package-release-bundle-smoke-bundle-root)))

(defun nemacs-library-package-release-bundle-smoke--write-single-row (row)
  "Write a single smoke ROW."
  (make-directory
   (file-name-directory nemacs-library-package-release-bundle-smoke-output)
   t)
  (with-temp-file nemacs-library-package-release-bundle-smoke-output
    (insert
     (nemacs-library-package-release-bundle-smoke--row
      "package_id" "status" "loader_feature" "declared_dependencies"
      "installed_dependencies" "bundle_archive_location" "package_user_dir"
      "member_features" "manifest_retained" "manifest_pending"
      "manifest_archive_artifacts" "source_leaks")
     "\n")
    (insert
     (apply #'nemacs-library-package-release-bundle-smoke--row row)
     "\n")))

(defun nemacs-library-package-release-bundle-smoke--run-one (package)
  "Run release bundle consumer smoke for PACKAGE and return a row."
  (let* ((manifest
          (nemacs-library-package-release-bundle-smoke--verify-manifest))
         (metadata-rows
          (nemacs-library-package-release-bundle-smoke--read-tsv
           nemacs-library-package-release-bundle-smoke-metadata))
         (metadata
          (nemacs-library-package-release-bundle-smoke--find-row
           package metadata-rows))
         (loader (nth 8 metadata))
         (declared-dependencies
          (nemacs-library-package-release-bundle-smoke--split-list
           (nth 9 metadata)))
         (members
          (nemacs-library-package-release-bundle-smoke--split-list
           (nth 10 metadata)))
         (archive-location
          (nemacs-library-package-release-bundle-smoke--archive-location)))
    (unless metadata
      (error "unknown package: %s" package))
    (unless (file-readable-p
             (expand-file-name "archive-contents" archive-location))
      (error "missing archive-contents in release bundle: %s"
             archive-location))
    (when (file-directory-p
           nemacs-library-package-release-bundle-smoke-install-root)
      (delete-directory
       nemacs-library-package-release-bundle-smoke-install-root t))
    (make-directory nemacs-library-package-release-bundle-smoke-install-root t)
    (setq package-user-dir
          (expand-file-name
           "elpa"
           nemacs-library-package-release-bundle-smoke-install-root))
    (setq package-archives
          `((,nemacs-library-package-release-bundle-smoke--archive-name
             . ,archive-location)))
    (setq package-check-signature nil)
    (setq package-unsigned-archives
          (list nemacs-library-package-release-bundle-smoke--archive-name))
    (setq package-selected-packages nil)
    (setq package-archive-contents nil)
    (package-initialize)
    (package-refresh-contents)
    (unless (assq (intern package) package-archive-contents)
      (error "package not found in release bundle archive contents: %s"
             package))
    (package-install (intern package) t)
    (package-initialize)
    (package-activate (intern package))
    (require (intern loader))
    (unless (featurep (intern loader))
      (error "loader feature was not provided: %s" loader))
    (dolist (member members)
      (unless (featurep (intern member))
        (error "member feature was not provided: %s via %s"
               member loader)))
    (let ((leaks
           (nemacs-library-package-release-bundle-smoke--source-leaks)))
      (when leaks
        (error "package %s loaded repo source/scaffold files: %s"
               package
               (mapconcat #'identity leaks ",")))
      (list package
            "ok"
            loader
            (mapconcat #'identity declared-dependencies ",")
            (mapconcat
             #'identity
             (nemacs-library-package-release-bundle-smoke--installed-dependencies
              package)
             ",")
            archive-location
            package-user-dir
            (mapconcat #'identity members ",")
            (plist-get manifest :retained)
            (plist-get manifest :pending)
            (plist-get manifest :archive-artifacts)
            ""))))

;;;###autoload
(defun nemacs-library-package-release-bundle-smoke-batch ()
  "Run release bundle consumer smoke for one package."
  (unless (and nemacs-library-package-release-bundle-smoke-package
               (not (string= nemacs-library-package-release-bundle-smoke-package
                             "")))
    (error "NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_PACKAGE is required"))
  (let ((row
         (nemacs-library-package-release-bundle-smoke--run-one
          nemacs-library-package-release-bundle-smoke-package)))
    (nemacs-library-package-release-bundle-smoke--write-single-row row)
    (princ
     (format
      "nemacs-library-package-release-bundle-smoke: package=%s status=ok output=%s\n"
      nemacs-library-package-release-bundle-smoke-package
      nemacs-library-package-release-bundle-smoke-output))))

(defun nemacs-library-package-release-bundle-smoke--write-summary (rows)
  "Write aggregate release bundle smoke summary for ROWS."
  (let ((ok 0)
        (fail 0)
        (retained "0")
        (pending "0")
        (archive-artifacts "0"))
    (dolist (row rows)
      (if (string= (nth 1 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail)))
      (setq retained (nth 8 row))
      (setq pending (nth 9 row))
      (setq archive-artifacts (nth 10 row)))
    (make-directory
     (file-name-directory
      nemacs-library-package-release-bundle-smoke-summary-output)
     t)
    (with-temp-file
        nemacs-library-package-release-bundle-smoke-summary-output
      (insert "#+TITLE: nemacs library package release bundle smoke\n\n")
      (insert "* Summary\n\n")
      (insert (format "- packages: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n" fail))
      (insert (format "- manifest retained files: %s\n" retained))
      (insert (format "- manifest pending files: %s\n" pending))
      (insert
       (format "- manifest retained archive artifacts: %s\n\n"
               archive-artifacts))
      (insert "* Packages\n\n")
      (insert "| Package | Status | Loader | Declared dependencies | Installed dependencies |\n")
      (insert "|---------+--------+--------+-----------------------+------------------------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | %s | =%s= | =%s= | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 2 row)
                 (nth 3 row)
                 (nth 4 row))))
      (insert "\n* Notes\n\n")
      (insert "- Retained manifest rows are verified by byte size and SHA-256 before package installation.\n")
      (insert "- Each package is installed via =package-refresh-contents= and =package-install= from the retained release bundle archives directory.\n")
      (insert "- Source leaks record repo =src/= or generated =packages/= scaffold files loaded during smoke.\n"))))

;;;###autoload
(defun nemacs-library-package-release-bundle-smoke-summary-batch ()
  "Write aggregate release bundle consumer smoke summary."
  (let ((rows
         (nemacs-library-package-release-bundle-smoke--read-tsv
          nemacs-library-package-release-bundle-smoke-output)))
    (nemacs-library-package-release-bundle-smoke--write-summary rows)
    (princ
     (format
      "nemacs-library-package-release-bundle-smoke-summary: packages=%d summary=%s\n"
      (length rows)
      nemacs-library-package-release-bundle-smoke-summary-output))))

(provide 'nemacs-library-package-release-bundle-smoke)

;;; nemacs-library-package-release-bundle-smoke.el ends here
