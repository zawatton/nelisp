;;; nemacs-library-package-index-smoke.el --- smoke package archive index -*- lexical-binding: t; -*-

;;; Commentary:

;; Install generated packages through a local package archive index.  This
;; exercises package discovery via `archive-contents', dependency resolution,
;; tar download from a file archive, package activation, and loader require.

;;; Code:

(require 'package)

(defvar nemacs-library-package-index-smoke-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-index-smoke-package
  (getenv "NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_PACKAGE")
  "Package id to smoke-test.")

(defvar nemacs-library-package-index-smoke-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-index-smoke-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-index-smoke-archive-root
  (expand-file-name "build/nemacs-library-package-archives"
                    nemacs-library-package-index-smoke-repo-root)
  "Directory containing local package archive contents.")

(defvar nemacs-library-package-index-smoke-install-root
  (expand-file-name "build/nemacs-library-package-index-smoke/install"
                    nemacs-library-package-index-smoke-repo-root)
  "Temporary package-user-dir root.")

(defvar nemacs-library-package-index-smoke-output
  (expand-file-name "build/nemacs-library-package-index-smoke.tsv"
                    nemacs-library-package-index-smoke-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-index-smoke-summary-output
  (expand-file-name "build/nemacs-library-package-index-smoke.org"
                    nemacs-library-package-index-smoke-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-index-smoke--archive-name
  "nelisp-emacs"
  "Local package archive name used by index smoke.")

(defun nemacs-library-package-index-smoke--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-index-smoke--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-index-smoke--tsv-cell cells "\t"))

(defun nemacs-library-package-index-smoke--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-index-smoke--read-tsv (file)
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

(defun nemacs-library-package-index-smoke--find-row (package rows)
  "Return metadata row for PACKAGE from ROWS."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth 0 row))
        (setq found row)))))

(defun nemacs-library-package-index-smoke--source-leaks ()
  "Return loaded files from repo src/ or generated packages/ scaffold."
  (let ((src-prefix
         (file-truename
          (expand-file-name "src/" nemacs-library-package-index-smoke-repo-root)))
        (packages-prefix
         (file-truename
          (expand-file-name "packages/" nemacs-library-package-index-smoke-repo-root)))
        leaks)
    (dolist (entry load-history)
      (let ((file (car entry)))
        (when (stringp file)
          (let ((true (file-truename file)))
            (when (or (string-prefix-p src-prefix true)
                      (string-prefix-p packages-prefix true))
              (push true leaks))))))
    (sort (delete-dups leaks) #'string<)))

(defun nemacs-library-package-index-smoke--installed-dependencies
    (package)
  "Return installed dependencies for PACKAGE from `package-alist'."
  (let* ((cell (assq (intern package) package-alist))
         (desc (cadr cell))
         dependencies)
    (dolist (req (and desc (package-desc-reqs desc)))
      (push (symbol-name (car req)) dependencies))
    (sort (delete-dups dependencies) #'string<)))

(defun nemacs-library-package-index-smoke--write-single-row (row)
  "Write a single smoke ROW."
  (make-directory
   (file-name-directory nemacs-library-package-index-smoke-output) t)
  (with-temp-file nemacs-library-package-index-smoke-output
    (insert
     (nemacs-library-package-index-smoke--row
      "package_id" "status" "loader_feature" "declared_dependencies"
      "installed_dependencies" "archive_location" "package_user_dir"
      "member_features" "source_leaks")
     "\n")
    (insert
     (apply #'nemacs-library-package-index-smoke--row row)
     "\n")))

(defun nemacs-library-package-index-smoke--archive-location ()
  "Return local archive location for the generated package archive root."
  ;; `package--with-response-buffer' accepts http(s) URLs or absolute local
  ;; file names.  For local archive smoke, pass an absolute directory name.
  (file-name-as-directory
   (expand-file-name nemacs-library-package-index-smoke-archive-root)))

(defun nemacs-library-package-index-smoke--run-one (package)
  "Run package archive index smoke for PACKAGE and return a row."
  (let* ((metadata-rows
          (nemacs-library-package-index-smoke--read-tsv
           nemacs-library-package-index-smoke-metadata))
         (metadata
          (nemacs-library-package-index-smoke--find-row
           package metadata-rows))
         (loader (nth 8 metadata))
         (declared-dependencies
          (nemacs-library-package-index-smoke--split-list (nth 9 metadata)))
         (members
          (nemacs-library-package-index-smoke--split-list (nth 10 metadata)))
         (archive-location
          (nemacs-library-package-index-smoke--archive-location)))
    (unless metadata
      (error "unknown package: %s" package))
    (unless (file-readable-p
             (expand-file-name "archive-contents"
                               nemacs-library-package-index-smoke-archive-root))
      (error "missing archive-contents in %s"
             nemacs-library-package-index-smoke-archive-root))
    (when (file-directory-p nemacs-library-package-index-smoke-install-root)
      (delete-directory nemacs-library-package-index-smoke-install-root t))
    (make-directory nemacs-library-package-index-smoke-install-root t)
    (setq package-user-dir
          (expand-file-name "elpa"
                            nemacs-library-package-index-smoke-install-root))
    (setq package-archives
          `((,nemacs-library-package-index-smoke--archive-name
             . ,archive-location)))
    (setq package-check-signature nil)
    (setq package-unsigned-archives
          (list nemacs-library-package-index-smoke--archive-name))
    (setq package-selected-packages nil)
    (setq package-archive-contents nil)
    (package-initialize)
    (package-refresh-contents)
    (unless (assq (intern package) package-archive-contents)
      (error "package not found in refreshed archive contents: %s" package))
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
    (let ((leaks (nemacs-library-package-index-smoke--source-leaks)))
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
             (nemacs-library-package-index-smoke--installed-dependencies
              package)
             ",")
            archive-location
            package-user-dir
            (mapconcat #'identity members ",")
            ""))))

;;;###autoload
(defun nemacs-library-package-index-smoke-batch ()
  "Run package archive index smoke for one package."
  (unless (and nemacs-library-package-index-smoke-package
               (not (string= nemacs-library-package-index-smoke-package "")))
    (error "NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_PACKAGE is required"))
  (let ((row
         (nemacs-library-package-index-smoke--run-one
          nemacs-library-package-index-smoke-package)))
    (nemacs-library-package-index-smoke--write-single-row row)
    (princ
     (format "nemacs-library-package-index-smoke: package=%s status=ok output=%s\n"
             nemacs-library-package-index-smoke-package
             nemacs-library-package-index-smoke-output))))

(defun nemacs-library-package-index-smoke--write-summary (rows)
  "Write aggregate index smoke summary for ROWS."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 1 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory nemacs-library-package-index-smoke-summary-output)
     t)
    (with-temp-file nemacs-library-package-index-smoke-summary-output
      (insert "#+TITLE: nemacs library package archive index smoke\n\n")
      (insert "* Summary\n\n")
      (insert (format "- packages: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n\n" fail))
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
      (insert "- Each package is installed via =package-refresh-contents= and =package-install= from a local file archive.\n")
      (insert "- This smoke validates archive discovery and dependency resolution, not only direct tar installation.\n")
      (insert "- Source leaks record repo =src/= or generated =packages/= scaffold files loaded during smoke.\n"))))

;;;###autoload
(defun nemacs-library-package-index-smoke-summary-batch ()
  "Write aggregate package archive index smoke summary."
  (let ((rows
         (nemacs-library-package-index-smoke--read-tsv
          nemacs-library-package-index-smoke-output)))
    (nemacs-library-package-index-smoke--write-summary rows)
    (princ
     (format
      "nemacs-library-package-index-smoke-summary: packages=%d summary=%s\n"
      (length rows)
      nemacs-library-package-index-smoke-summary-output))))

(provide 'nemacs-library-package-index-smoke)

;;; nemacs-library-package-index-smoke.el ends here
