;;; nemacs-library-package-archive-smoke.el --- smoke install package archives -*- lexical-binding: t; -*-

;;; Commentary:

;; Install generated package tar archives through `package-install-file' and
;; require the package loader feature from the installed package-user-dir.

;;; Code:

(require 'package)

(defvar nemacs-library-package-archive-smoke-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-archive-smoke-package
  (getenv "NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_PACKAGE")
  "Package id to smoke-test.")

(defvar nemacs-library-package-archive-smoke-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-archive-smoke-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-archive-smoke-archives
  (expand-file-name "build/nemacs-library-package-archive.tsv"
                    nemacs-library-package-archive-smoke-repo-root)
  "Package archive TSV.")

(defvar nemacs-library-package-archive-smoke-install-root
  (expand-file-name "build/nemacs-library-package-archive-smoke/install"
                    nemacs-library-package-archive-smoke-repo-root)
  "Temporary package-user-dir root.")

(defvar nemacs-library-package-archive-smoke-output
  (expand-file-name "build/nemacs-library-package-archive-smoke.tsv"
                    nemacs-library-package-archive-smoke-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-archive-smoke-summary-output
  (expand-file-name "build/nemacs-library-package-archive-smoke.org"
                    nemacs-library-package-archive-smoke-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-archive-smoke--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-archive-smoke--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-archive-smoke--tsv-cell cells "\t"))

(defun nemacs-library-package-archive-smoke--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-archive-smoke--read-tsv (file)
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

(defun nemacs-library-package-archive-smoke--find-row
    (package rows column)
  "Return row for PACKAGE from ROWS comparing COLUMN."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth column row))
        (setq found row)))))

(defun nemacs-library-package-archive-smoke--closure-visit
    (package metadata-rows state)
  "Visit PACKAGE dependency closure using METADATA-ROWS and STATE."
  (let ((seen (car state))
        (ordered (cdr state)))
    (unless (member package seen)
      (let ((row
             (nemacs-library-package-archive-smoke--find-row
              package metadata-rows 0)))
        (unless row
          (error "unknown package dependency: %s" package))
        (setcar state (cons package seen))
        (dolist (dependency
                 (nemacs-library-package-archive-smoke--split-list
                  (nth 9 row)))
          (nemacs-library-package-archive-smoke--closure-visit
           dependency metadata-rows state))
        (setq ordered (cdr state))
        (setcdr state (cons package ordered))))))

(defun nemacs-library-package-archive-smoke--closure
    (package metadata-rows)
  "Return dependency closure for PACKAGE from METADATA-ROWS."
  (let ((state (cons nil nil)))
    (nemacs-library-package-archive-smoke--closure-visit
     package metadata-rows state)
    (nreverse (cdr state))))

(defun nemacs-library-package-archive-smoke--archive-file
    (package archive-rows)
  "Return archive file for PACKAGE from ARCHIVE-ROWS."
  (let ((row
         (nemacs-library-package-archive-smoke--find-row
          package archive-rows 0)))
    (unless row
      (error "missing archive row for package: %s" package))
    (expand-file-name (nth 4 row)
                      nemacs-library-package-archive-smoke-repo-root)))

(defun nemacs-library-package-archive-smoke--source-leaks ()
  "Return loaded files from repo src/ or generated packages/ scaffold."
  (let ((src-prefix
         (file-truename
          (expand-file-name "src/" nemacs-library-package-archive-smoke-repo-root)))
        (packages-prefix
         (file-truename
          (expand-file-name "packages/" nemacs-library-package-archive-smoke-repo-root)))
        leaks)
    (dolist (entry load-history)
      (let ((file (car entry)))
        (when (stringp file)
          (let ((true (file-truename file)))
            (when (or (string-prefix-p src-prefix true)
                      (string-prefix-p packages-prefix true))
              (push true leaks))))))
    (sort (delete-dups leaks) #'string<)))

(defun nemacs-library-package-archive-smoke--write-single-row (row)
  "Write a single smoke ROW."
  (make-directory
   (file-name-directory nemacs-library-package-archive-smoke-output) t)
  (with-temp-file nemacs-library-package-archive-smoke-output
    (insert
     (nemacs-library-package-archive-smoke--row
      "package_id" "status" "loader_feature" "dependency_closure"
      "archives" "package_user_dir" "member_features" "source_leaks")
     "\n")
    (insert
     (apply #'nemacs-library-package-archive-smoke--row row)
     "\n")))

(defun nemacs-library-package-archive-smoke--run-one (package)
  "Run archive install smoke for PACKAGE and return a row."
  (let* ((metadata-rows
          (nemacs-library-package-archive-smoke--read-tsv
           nemacs-library-package-archive-smoke-metadata))
         (archive-rows
          (nemacs-library-package-archive-smoke--read-tsv
           nemacs-library-package-archive-smoke-archives))
         (metadata
          (nemacs-library-package-archive-smoke--find-row
           package metadata-rows 0))
         (closure
          (nemacs-library-package-archive-smoke--closure
           package metadata-rows))
         (loader (nth 8 metadata))
         (members
          (nemacs-library-package-archive-smoke--split-list
           (nth 10 metadata)))
         archives)
    (unless metadata
      (error "unknown package: %s" package))
    (when (file-directory-p nemacs-library-package-archive-smoke-install-root)
      (delete-directory nemacs-library-package-archive-smoke-install-root t))
    (make-directory nemacs-library-package-archive-smoke-install-root t)
    (setq package-user-dir
          (expand-file-name "elpa"
                            nemacs-library-package-archive-smoke-install-root))
    (setq package-archives nil)
    (setq package-check-signature nil)
    (setq package-selected-packages nil)
    (package-initialize)
    (dolist (name closure)
      (let ((archive
             (nemacs-library-package-archive-smoke--archive-file
              name archive-rows)))
        (unless (file-readable-p archive)
          (error "missing readable archive for %s: %s" name archive))
        (push archive archives)
        (package-install-file archive)
        (package-initialize)))
    (package-activate (intern package))
    (require (intern loader))
    (unless (featurep (intern loader))
      (error "loader feature was not provided: %s" loader))
    (dolist (member members)
      (unless (featurep (intern member))
        (error "member feature was not provided: %s via %s"
               member loader)))
    (let ((leaks (nemacs-library-package-archive-smoke--source-leaks)))
      (when leaks
        (error "package %s loaded repo source/scaffold files: %s"
               package
               (mapconcat #'identity leaks ",")))
      (list package
            "ok"
            loader
            (mapconcat #'identity closure ",")
            (mapconcat #'identity (nreverse archives) ",")
            package-user-dir
            (mapconcat #'identity members ",")
            ""))))

;;;###autoload
(defun nemacs-library-package-archive-smoke-batch ()
  "Run archive install smoke for one package."
  (unless (and nemacs-library-package-archive-smoke-package
               (not (string= nemacs-library-package-archive-smoke-package "")))
    (error "NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_PACKAGE is required"))
  (let ((row
         (nemacs-library-package-archive-smoke--run-one
          nemacs-library-package-archive-smoke-package)))
    (nemacs-library-package-archive-smoke--write-single-row row)
    (princ
     (format "nemacs-library-package-archive-smoke: package=%s status=ok output=%s\n"
             nemacs-library-package-archive-smoke-package
             nemacs-library-package-archive-smoke-output))))

(defun nemacs-library-package-archive-smoke--write-summary (rows)
  "Write aggregate archive smoke summary for ROWS."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 1 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory nemacs-library-package-archive-smoke-summary-output)
     t)
    (with-temp-file nemacs-library-package-archive-smoke-summary-output
      (insert "#+TITLE: nemacs library package archive install smoke\n\n")
      (insert "* Summary\n\n")
      (insert (format "- packages: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n\n" fail))
      (insert "* Packages\n\n")
      (insert "| Package | Status | Loader | Dependency closure |\n")
      (insert "|---------+--------+--------+--------------------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | %s | =%s= | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 2 row)
                 (nth 3 row))))
      (insert "\n* Notes\n\n")
      (insert "- Each package is installed from generated tar archives through =package-install-file=.\n")
      (insert "- Dependency closure archives are installed before the requested package.\n")
      (insert "- Source leaks record repo =src/= or generated =packages/= scaffold files loaded during smoke.\n"))))

;;;###autoload
(defun nemacs-library-package-archive-smoke-summary-batch ()
  "Write aggregate archive smoke summary."
  (let ((rows
         (nemacs-library-package-archive-smoke--read-tsv
          nemacs-library-package-archive-smoke-output)))
    (nemacs-library-package-archive-smoke--write-summary rows)
    (princ
     (format
      "nemacs-library-package-archive-smoke-summary: packages=%d summary=%s\n"
      (length rows)
      nemacs-library-package-archive-smoke-summary-output))))

(provide 'nemacs-library-package-archive-smoke)

;;; nemacs-library-package-archive-smoke.el ends here
