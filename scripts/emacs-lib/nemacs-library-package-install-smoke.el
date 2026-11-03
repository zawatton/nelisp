;;; nemacs-library-package-install-smoke.el --- smoke package-style installs -*- lexical-binding: t; -*-

;;; Commentary:

;; Smoke-test one generated reusable package from package-style install
;; directories.  This script intentionally reads generated TSV artifacts and
;; package/app scaffold copies; it does not require the source facade.

;;; Code:

(defvar nemacs-library-package-install-smoke-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-install-smoke-package
  (getenv "NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_PACKAGE")
  "Package id to smoke-test.")

(defvar nemacs-library-package-install-smoke-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-install-smoke-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-install-smoke-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-install-smoke-repo-root)
  "Package scaffold TSV.")

(defvar nemacs-library-package-install-smoke-install-root
  (expand-file-name "build/nemacs-library-package-install-smoke/install"
                    nemacs-library-package-install-smoke-repo-root)
  "Temporary install root.")

(defvar nemacs-library-package-install-smoke-output
  (expand-file-name "build/nemacs-library-package-install-smoke.tsv"
                    nemacs-library-package-install-smoke-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-install-smoke-summary-output
  (expand-file-name "build/nemacs-library-package-install-smoke.org"
                    nemacs-library-package-install-smoke-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-install-smoke--defined-package nil
  "Captured generated package metadata form.")

(defun define-package (&rest args)
  "Capture generated package metadata ARGS."
  (setq nemacs-library-package-install-smoke--defined-package args))

(defun nemacs-library-package-install-smoke--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-install-smoke--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-install-smoke--tsv-cell cells "\t"))

(defun nemacs-library-package-install-smoke--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-install-smoke--read-tsv (file)
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

(defun nemacs-library-package-install-smoke--metadata-rows ()
  "Return package metadata rows."
  (nemacs-library-package-install-smoke--read-tsv
   nemacs-library-package-install-smoke-metadata))

(defun nemacs-library-package-install-smoke--scaffold-rows ()
  "Return package scaffold file rows."
  (let (rows)
    (dolist (row (nemacs-library-package-install-smoke--read-tsv
                  nemacs-library-package-install-smoke-scaffold))
      (when (string= (nth 0 row) "file")
        (push row rows)))
    (nreverse rows)))

(defun nemacs-library-package-install-smoke--find-metadata (package rows)
  "Return metadata row for PACKAGE from ROWS."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth 0 row))
        (setq found row)))))

(defun nemacs-library-package-install-smoke--closure-visit
    (package metadata-rows state)
  "Visit PACKAGE dependency closure using METADATA-ROWS and STATE.
STATE is a cons cell (SEEN . ORDERED)."
  (let ((seen (car state))
        (ordered (cdr state)))
    (unless (member package seen)
      (let ((row
             (nemacs-library-package-install-smoke--find-metadata
              package metadata-rows)))
        (unless row
          (error "unknown package dependency: %s" package))
        (setcar state (cons package seen))
        (dolist (dependency
                 (nemacs-library-package-install-smoke--split-list
                  (nth 9 row)))
          (nemacs-library-package-install-smoke--closure-visit
           dependency metadata-rows state))
        (setq ordered (cdr state))
        (setcdr state (cons package ordered))))))

(defun nemacs-library-package-install-smoke--closure
    (package metadata-rows)
  "Return dependency closure for PACKAGE from METADATA-ROWS."
  (let ((state (cons nil nil)))
    (nemacs-library-package-install-smoke--closure-visit
     package metadata-rows state)
    (nreverse (cdr state))))

(defun nemacs-library-package-install-smoke--copy-file (source target)
  "Copy SOURCE to TARGET, creating parent directories."
  (make-directory (file-name-directory target) t)
  (copy-file source target t))

(defun nemacs-library-package-install-smoke--install-dir
    (package metadata-rows)
  "Return install directory for PACKAGE using METADATA-ROWS."
  (let ((row
         (nemacs-library-package-install-smoke--find-metadata
          package metadata-rows)))
    (unless row
      (error "unknown package: %s" package))
    (expand-file-name (nth 15 row)
                      nemacs-library-package-install-smoke-install-root)))

(defun nemacs-library-package-install-smoke--prepare-install
    (packages metadata-rows scaffold-rows)
  "Prepare install dirs for PACKAGES using METADATA-ROWS and SCAFFOLD-ROWS."
  (when (file-directory-p nemacs-library-package-install-smoke-install-root)
    (delete-directory nemacs-library-package-install-smoke-install-root t))
  (make-directory nemacs-library-package-install-smoke-install-root t)
  (dolist (package packages)
    (let* ((metadata
            (nemacs-library-package-install-smoke--find-metadata
             package metadata-rows))
           (install-dir
            (nemacs-library-package-install-smoke--install-dir
             package metadata-rows))
           (package-file
            (expand-file-name
             (nth 14 metadata)
             nemacs-library-package-install-smoke-repo-root)))
      (unless metadata
        (error "unknown package: %s" package))
      (nemacs-library-package-install-smoke--copy-file
       package-file
       (expand-file-name (file-name-nondirectory package-file)
                         install-dir))
      (dolist (row scaffold-rows)
        (when (string= package (nth 1 row))
          (let ((source
                 (expand-file-name
                  (nth 4 row)
                  nemacs-library-package-install-smoke-repo-root)))
            (nemacs-library-package-install-smoke--copy-file
             source
             (expand-file-name (file-name-nondirectory source)
                               install-dir))))))))

(defun nemacs-library-package-install-smoke--load-package-metadata
    (package metadata-rows)
  "Load generated `*-pkg.el' for PACKAGE and validate METADATA-ROWS."
  (let* ((row
          (nemacs-library-package-install-smoke--find-metadata
           package metadata-rows))
         (package-file
          (expand-file-name
           (file-name-nondirectory (nth 14 row))
           (nemacs-library-package-install-smoke--install-dir
            package metadata-rows))))
    (setq nemacs-library-package-install-smoke--defined-package nil)
    (load package-file nil t)
    (unless nemacs-library-package-install-smoke--defined-package
      (error "package metadata did not call define-package: %s" package))
    (unless (string= package
                     (car nemacs-library-package-install-smoke--defined-package))
      (error "package metadata name mismatch: %s" package))
    (unless (string= (nth 1 row)
                     (cadr nemacs-library-package-install-smoke--defined-package))
      (error "package metadata version mismatch: %s" package))
    package-file))

(defun nemacs-library-package-install-smoke--source-leaks ()
  "Return loaded files from repo src/ or packages/ scaffold paths."
  (let ((src-prefix
         (file-truename
          (expand-file-name "src/" nemacs-library-package-install-smoke-repo-root)))
        (packages-prefix
         (file-truename
          (expand-file-name "packages/" nemacs-library-package-install-smoke-repo-root)))
        leaks)
    (dolist (entry load-history)
      (let ((file (car entry)))
        (when (stringp file)
          (let ((true (file-truename file)))
            (when (or (string-prefix-p src-prefix true)
                      (string-prefix-p packages-prefix true))
              (push true leaks))))))
    (sort (delete-dups leaks) #'string<)))

(defun nemacs-library-package-install-smoke--write-single-row (row)
  "Write a single smoke ROW to output."
  (make-directory
   (file-name-directory nemacs-library-package-install-smoke-output) t)
  (with-temp-file nemacs-library-package-install-smoke-output
    (insert
     (nemacs-library-package-install-smoke--row
      "package_id" "status" "loader_feature" "dependency_closure"
      "install_dirs" "metadata_file" "member_features" "source_leaks")
     "\n")
    (insert
     (apply #'nemacs-library-package-install-smoke--row row)
     "\n")))

(defun nemacs-library-package-install-smoke--run-one (package)
  "Run install smoke for PACKAGE and return a TSV row."
  (let* ((metadata-rows
          (nemacs-library-package-install-smoke--metadata-rows))
         (scaffold-rows
          (nemacs-library-package-install-smoke--scaffold-rows))
         (metadata
          (nemacs-library-package-install-smoke--find-metadata
           package metadata-rows))
         (closure
          (nemacs-library-package-install-smoke--closure
           package metadata-rows))
         (install-dirs nil)
         (metadata-file nil)
         (loader (nth 8 metadata))
         (members
          (nemacs-library-package-install-smoke--split-list
           (nth 10 metadata))))
    (unless metadata
      (error "unknown package: %s" package))
    (nemacs-library-package-install-smoke--prepare-install
     closure metadata-rows scaffold-rows)
    (dolist (name closure)
      (push
       (nemacs-library-package-install-smoke--install-dir name metadata-rows)
       install-dirs))
    (setq install-dirs (nreverse install-dirs))
    (dolist (dir install-dirs)
      (add-to-list 'load-path dir t))
    (setq metadata-file
          (nemacs-library-package-install-smoke--load-package-metadata
           package metadata-rows))
    (require (intern loader))
    (unless (featurep (intern loader))
      (error "loader feature was not provided: %s" loader))
    (dolist (member members)
      (unless (featurep (intern member))
        (error "member feature was not provided: %s via %s"
               member loader)))
    (let ((leaks (nemacs-library-package-install-smoke--source-leaks)))
      (when leaks
        (error "package %s loaded repo source/scaffold files: %s"
               package
               (mapconcat #'identity leaks ",")))
      (list package
            "ok"
            loader
            (mapconcat #'identity closure ",")
            (mapconcat #'identity install-dirs ",")
            metadata-file
            (mapconcat #'identity members ",")
            ""))))

;;;###autoload
(defun nemacs-library-package-install-smoke-batch ()
  "Run one package-style install smoke."
  (unless (and nemacs-library-package-install-smoke-package
               (not (string= nemacs-library-package-install-smoke-package "")))
    (error "NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_PACKAGE is required"))
  (let ((row
         (nemacs-library-package-install-smoke--run-one
          nemacs-library-package-install-smoke-package)))
    (nemacs-library-package-install-smoke--write-single-row row)
    (princ
     (format "nemacs-library-package-install-smoke: package=%s status=ok output=%s\n"
             nemacs-library-package-install-smoke-package
             nemacs-library-package-install-smoke-output))))

(defun nemacs-library-package-install-smoke--write-summary (rows)
  "Write install smoke summary for ROWS."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 1 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory nemacs-library-package-install-smoke-summary-output)
     t)
    (with-temp-file nemacs-library-package-install-smoke-summary-output
      (insert "#+TITLE: nemacs library package install smoke\n\n")
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
      (insert "- Each row is produced by a separate =emacs -Q= process through a package-style install root.\n")
      (insert "- Install directories flatten eager and lazy scaffold files so package activation can load without =src/=.\n")
      (insert "- Source leaks record repo =src/= or generated =packages/= scaffold files loaded during smoke.\n"))))

;;;###autoload
(defun nemacs-library-package-install-smoke-summary-batch ()
  "Write aggregate package install smoke summary."
  (let ((rows
         (nemacs-library-package-install-smoke--read-tsv
          nemacs-library-package-install-smoke-output)))
    (nemacs-library-package-install-smoke--write-summary rows)
    (princ
     (format
      "nemacs-library-package-install-smoke-summary: packages=%d summary=%s\n"
      (length rows)
      nemacs-library-package-install-smoke-summary-output))))

(provide 'nemacs-library-package-install-smoke)

;;; nemacs-library-package-install-smoke.el ends here
