;;; nemacs-library-package-lazy-metadata.el --- lazy companion metadata -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate external-facing metadata for package-owned lazy companions.  Normal
;; `define-package' metadata describes eager loader dependencies; this artifact
;; records which additional package, host, and vendored dependencies are needed
;; when a consumer opts into a package-shipped lazy feature.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nemacs-library-package-deps)

(defvar nemacs-library-package-lazy-metadata-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-lazy-metadata-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-lazy-metadata-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-lazy-metadata-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-lazy-metadata-repo-root)
  "Package scaffold TSV.")

(defvar nemacs-library-package-lazy-metadata-output
  (expand-file-name "build/nemacs-library-package-lazy-metadata.tsv"
                    nemacs-library-package-lazy-metadata-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-lazy-metadata-summary-output
  (expand-file-name "build/nemacs-library-package-lazy-metadata.org"
                    nemacs-library-package-lazy-metadata-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-lazy-metadata--blocked-relations
  '("app-or-frontend"
    "external-or-host"
    "lazy-unmanifested-reusable"
    "unmanifested-reusable")
  "Dependency relation buckets that are not publishable lazy metadata.")

(defun nemacs-library-package-lazy-metadata--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-lazy-metadata--package-id (name)
  "Return facade package id for package NAME."
  (format "nelisp-emacs-%s"
          (nemacs-library-package-lazy-metadata--symbol-name name)))

(defun nemacs-library-package-lazy-metadata--sort-strings (values)
  "Return sorted unique string VALUES."
  (sort (delete-dups
         (mapcar #'nemacs-library-package-lazy-metadata--symbol-name
                 (copy-sequence values)))
        #'string<))

(defun nemacs-library-package-lazy-metadata--join (values)
  "Return VALUES as a comma-separated stable string."
  (string-join
   (nemacs-library-package-lazy-metadata--sort-strings values)
   ","))

(defun nemacs-library-package-lazy-metadata--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-lazy-metadata--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-lazy-metadata--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-lazy-metadata--tsv-cell cells "\t"))

(defun nemacs-library-package-lazy-metadata--read-tsv (file)
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

(defun nemacs-library-package-lazy-metadata--scaffold-lazy-row
    (package feature scaffold-rows)
  "Return scaffold lazy row for PACKAGE and FEATURE."
  (let ((expected (concat feature ".el"))
        found)
    (dolist (row scaffold-rows found)
      (when (and (string= (nth 0 row) "file")
                 (string= (nth 1 row) package)
                 (string= (nth 2 row) "lazy")
                 (string= (file-name-nondirectory (nth 3 row)) expected)
                 (string= (file-name-nondirectory (nth 4 row)) expected))
        (setq found row)))))

(defun nemacs-library-package-lazy-metadata--metadata-table (metadata-rows)
  "Return package metadata table from METADATA-ROWS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row metadata-rows table)
      (puthash (nth 0 row) row table))))

(defun nemacs-library-package-lazy-metadata--package-requires
    (package metadata-table)
  "Return direct package requirements for PACKAGE from METADATA-TABLE."
  (let ((row (gethash package metadata-table)))
    (and row
         (nemacs-library-package-lazy-metadata--split-list (nth 9 row)))))

(defun nemacs-library-package-lazy-metadata--package-closure
    (packages metadata-table)
  "Return transitive package dependency closure for PACKAGES."
  (let ((seen nil)
        (queue (copy-sequence packages)))
    (while queue
      (let ((package (pop queue)))
        (unless (member package seen)
          (push package seen)
          (dolist (dep
                   (nemacs-library-package-lazy-metadata--package-requires
                    package metadata-table))
            (unless (member dep seen)
              (push dep queue))))))
    (nemacs-library-package-lazy-metadata--sort-strings seen)))

(defun nemacs-library-package-lazy-metadata--string-difference
    (left right)
  "Return strings in LEFT that are not in RIGHT."
  (cl-remove-if
   (lambda (item) (member item right))
   (copy-sequence left)))

(defun nemacs-library-package-lazy-metadata--feature-package-map
    (packages &optional lazy)
  "Return feature -> facade package name map for PACKAGES.
When LAZY is non-nil, map lazy companion features."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (package packages table)
      (let ((name (car package))
            (loader (nth 2 package))
            (features (if lazy (nth 4 package) (nth 3 package))))
        (unless lazy
          (puthash loader name table))
        (dolist (feature features)
          (puthash feature name table))))))

(defun nemacs-library-package-lazy-metadata--classify
    (required-feature from-package require-scope provide-map feature-packages
                      lazy-feature-packages)
  "Classify REQUIRED-FEATURE required by FROM-PACKAGE."
  (let* ((target (gethash required-feature provide-map))
         (to-file (car target))
         (to-owner (or (cadr target) "EXTERNAL"))
         (lazy-target-package
          (gethash required-feature lazy-feature-packages))
         (to-package (or (gethash required-feature feature-packages)
                         lazy-target-package))
         (relation
          (nemacs-library-package-deps--relation
           required-feature from-package to-package to-owner require-scope
           lazy-target-package)))
    (list required-feature
          relation
          require-scope
          to-package
          to-owner
          to-file)))

(defun nemacs-library-package-lazy-metadata--inc (plist key)
  "Increment integer value for KEY in PLIST and return PLIST."
  (plist-put plist key (1+ (or (plist-get plist key) 0))))

(defun nemacs-library-package-lazy-metadata--row-for-lazy
    (package-id feature source target metadata-table provide-map
                feature-packages lazy-feature-packages)
  "Return lazy metadata row for PACKAGE-ID FEATURE SOURCE TARGET."
  (let* ((from-name
          (intern (string-remove-prefix "nelisp-emacs-" package-id)))
         (own-package-requires
          (nemacs-library-package-lazy-metadata--package-requires
           package-id metadata-table))
         (direct-package-requires nil)
         (top-level-package-requires nil)
         (lazy-runtime-package-requires nil)
         (same-package-features nil)
         (same-package-lazy-features nil)
         (host-features nil)
         (vendor-features nil)
         (app-or-frontend-features nil)
         (unknown-features nil)
         (blocked-counts nil)
         (readable-source
          (file-readable-p
           (expand-file-name source
                             nemacs-library-package-lazy-metadata-repo-root)))
         (readable-target
          (file-readable-p
           (expand-file-name target
                             nemacs-library-package-lazy-metadata-repo-root))))
    (when readable-source
      (dolist (required (nemacs-library-package-deps--requires-in-file source))
        (let* ((required-feature (car required))
               (scope (cdr required))
               (classified
                (nemacs-library-package-lazy-metadata--classify
                 required-feature from-name scope provide-map
                 feature-packages lazy-feature-packages))
               (relation (nth 1 classified))
               (to-package (nth 3 classified))
               (target-package-id
                (and to-package
                     (nemacs-library-package-lazy-metadata--package-id
                      to-package))))
          (cond
           ((and target-package-id
                 (string= target-package-id package-id)
                 (string= relation "lazy-manifest-package"))
            (push required-feature same-package-lazy-features))
           ((and target-package-id
                 (string= target-package-id package-id))
            (push required-feature same-package-features))
           ((and target-package-id
                 (member relation '("manifest-package"
                                    "lazy-manifest-package")))
            (push target-package-id direct-package-requires)
            (if (string= scope "top-level")
                (push target-package-id top-level-package-requires)
              (push target-package-id lazy-runtime-package-requires)))
           ((string= relation "host-feature")
            (push required-feature host-features))
           ((string= relation "vendor-package")
            (push required-feature vendor-features))
           ((string= relation "app-or-frontend")
            (push required-feature app-or-frontend-features)
            (setq blocked-counts
                  (nemacs-library-package-lazy-metadata--inc
                   blocked-counts :app-or-frontend)))
           (t
            (push required-feature unknown-features)
            (setq blocked-counts
                  (nemacs-library-package-lazy-metadata--inc
                   blocked-counts (intern (concat ":" relation)))))))))
    (let* ((direct-package-requires
            (nemacs-library-package-lazy-metadata--sort-strings
             direct-package-requires))
           (additional-package-requires
            (nemacs-library-package-lazy-metadata--string-difference
             direct-package-requires own-package-requires))
           (closure
            (nemacs-library-package-lazy-metadata--package-closure
             (append (list package-id)
                     own-package-requires
                     additional-package-requires)
             metadata-table))
           (status
            (if (and readable-source readable-target
                     (null app-or-frontend-features)
                     (null unknown-features))
                "ok"
              "fail"))
           (details
            (cond
             ((not readable-source) "lazy source file is not readable")
             ((not readable-target) "lazy scaffold target file is not readable")
             ((or app-or-frontend-features unknown-features)
              (format "blocked-relations=%s"
                      (prin1-to-string blocked-counts)))
             (t "lazy dependency closure metadata is publishable"))))
      (list package-id
            feature
            source
            target
            (nemacs-library-package-lazy-metadata--join
             direct-package-requires)
            (nemacs-library-package-lazy-metadata--join
             top-level-package-requires)
            (nemacs-library-package-lazy-metadata--join
             lazy-runtime-package-requires)
            (nemacs-library-package-lazy-metadata--join
             additional-package-requires)
            (nemacs-library-package-lazy-metadata--join closure)
            (nemacs-library-package-lazy-metadata--join
             same-package-features)
            (nemacs-library-package-lazy-metadata--join
             same-package-lazy-features)
            (nemacs-library-package-lazy-metadata--join host-features)
            (nemacs-library-package-lazy-metadata--join vendor-features)
            (nemacs-library-package-lazy-metadata--join
             app-or-frontend-features)
            (nemacs-library-package-lazy-metadata--join unknown-features)
            status
            details))))

(defun nemacs-library-package-lazy-metadata--build-rows ()
  "Return lazy companion metadata rows."
  (let* ((metadata-rows
          (nemacs-library-package-lazy-metadata--read-tsv
           nemacs-library-package-lazy-metadata-metadata))
         (metadata-table
          (nemacs-library-package-lazy-metadata--metadata-table
           metadata-rows))
         (scaffold-rows
          (nemacs-library-package-lazy-metadata--read-tsv
           nemacs-library-package-lazy-metadata-scaffold))
         (ownership (nemacs-library-package-deps--ownership))
         (provide-map (nemacs-library-package-deps--provide-map ownership))
         (packages (nemacs-library-package-deps--manifest-rows))
         (feature-packages
          (nemacs-library-package-lazy-metadata--feature-package-map
           packages))
         (lazy-feature-packages
          (nemacs-library-package-lazy-metadata--feature-package-map
           packages t))
         rows)
    (dolist (metadata metadata-rows)
      (let ((package-id (nth 0 metadata)))
        (dolist (feature
                 (nemacs-library-package-lazy-metadata--split-list
                  (nth 11 metadata)))
          (let* ((scaffold
                  (nemacs-library-package-lazy-metadata--scaffold-lazy-row
                   package-id feature scaffold-rows))
                 (source (or (nth 3 scaffold) ""))
                 (target (or (nth 4 scaffold) "")))
            (push
             (nemacs-library-package-lazy-metadata--row-for-lazy
              package-id feature source target metadata-table provide-map
              feature-packages lazy-feature-packages)
             rows)))))
    (sort
     (nreverse rows)
     (lambda (a b)
       (string< (mapconcat #'identity (list (nth 0 a) (nth 1 a)) "\t")
                (mapconcat #'identity (list (nth 0 b) (nth 1 b)) "\t"))))))

(defun nemacs-library-package-lazy-metadata--write-tsv (rows)
  "Write lazy metadata ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-lazy-metadata-output)
   t)
  (with-temp-file nemacs-library-package-lazy-metadata-output
    (insert
     (nemacs-library-package-lazy-metadata--row
      "package_id" "lazy_feature" "source_file" "target_file"
      "direct_package_requires" "top_level_package_requires"
      "lazy_runtime_package_requires" "additional_package_requires"
      "package_dependency_closure" "same_package_features"
      "same_package_lazy_features" "host_features" "vendor_package_features"
      "app_or_frontend_features" "unknown_features" "status" "details")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-lazy-metadata--row row)
              "\n"))))

(defun nemacs-library-package-lazy-metadata--non-empty-count
    (rows column)
  "Return count of ROWS with a non-empty value at COLUMN."
  (cl-count-if
   (lambda (row)
     (not (string= (nth column row) "")))
   rows))

(defun nemacs-library-package-lazy-metadata--edge-count (rows column)
  "Return comma-separated edge count across ROWS at COLUMN."
  (let ((count 0))
    (dolist (row rows count)
      (setq count
            (+ count
               (length
                (nemacs-library-package-lazy-metadata--split-list
                 (nth column row))))))))

(defun nemacs-library-package-lazy-metadata--write-summary (rows)
  "Write lazy metadata ROWS to Org summary output."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 15 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory
      nemacs-library-package-lazy-metadata-summary-output)
     t)
    (with-temp-file nemacs-library-package-lazy-metadata-summary-output
      (insert "#+TITLE: nemacs library package lazy companion metadata\n\n")
      (insert "* Summary\n\n")
      (insert (format "- lazy feature rows: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n" fail))
      (insert
       (format "- rows with additional package requirements: %d\n"
               (nemacs-library-package-lazy-metadata--non-empty-count
                rows 7)))
      (insert
       (format "- direct package requirement edges: %d\n"
               (nemacs-library-package-lazy-metadata--edge-count
                rows 4)))
      (insert
       (format "- additional package requirement edges: %d\n\n"
               (nemacs-library-package-lazy-metadata--edge-count
                rows 7)))
      (insert "* Policy\n\n")
      (insert "- Lazy companions are package-shipped features, not eager =package-requires=.\n")
      (insert "- =additional_package_requires= records package dependencies needed only when a lazy feature is used.\n")
      (insert "- =package_dependency_closure= records the package closure a consumer must have available for that lazy feature.\n")
      (insert "- App/frontend, unmanifested reusable, and unknown external relations fail this gate.\n\n")
      (insert "* Lazy Features\n\n")
      (insert "| Package | Lazy feature | Status | Additional package requires | Closure |\n")
      (insert "|---------+--------------+--------+-----------------------------+---------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | =%s= | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 15 row)
                 (nth 7 row)
                 (nth 8 row)))))))

;;;###autoload
(defun nemacs-library-package-lazy-metadata-batch ()
  "Generate and verify lazy companion package metadata."
  (let* ((rows (nemacs-library-package-lazy-metadata--build-rows))
         (failures 0))
    (unless rows
      (error "empty lazy metadata rows"))
    (nemacs-library-package-lazy-metadata--write-tsv rows)
    (nemacs-library-package-lazy-metadata--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 15 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-lazy-metadata: rows=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-package-lazy-metadata-output
      nemacs-library-package-lazy-metadata-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-lazy-metadata)

;;; nemacs-library-package-lazy-metadata.el ends here
