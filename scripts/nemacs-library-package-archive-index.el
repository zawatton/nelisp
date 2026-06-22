;;; nemacs-library-package-archive-index.el --- generate package archive index -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate an `archive-contents' file next to the draft tar archives so
;; Emacs package.el can discover the packages through a normal archive index.

;;; Code:

(require 'package)

(defvar nemacs-library-package-archive-index-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-archive-index-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-archive-index-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-archive-index-archives
  (expand-file-name "build/nemacs-library-package-archive.tsv"
                    nemacs-library-package-archive-index-repo-root)
  "Package archive TSV.")

(defvar nemacs-library-package-archive-index-root
  (expand-file-name "build/nemacs-library-package-archives"
                    nemacs-library-package-archive-index-repo-root)
  "Directory containing package tar archives and archive-contents.")

(defvar nemacs-library-package-archive-index-output
  (expand-file-name "build/nemacs-library-package-archive-index.tsv"
                    nemacs-library-package-archive-index-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-archive-index-summary-output
  (expand-file-name "build/nemacs-library-package-archive-index.org"
                    nemacs-library-package-archive-index-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-archive-index--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-archive-index--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-archive-index--tsv-cell cells "\t"))

(defun nemacs-library-package-archive-index--split-list (value)
  "Split comma-separated VALUE."
  (if (or (null value) (string= value ""))
      nil
    (split-string value "," t)))

(defun nemacs-library-package-archive-index--read-tsv (file)
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

(defun nemacs-library-package-archive-index--find-row
    (package rows column)
  "Return row for PACKAGE from ROWS comparing COLUMN."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth column row))
        (setq found row)))))

(defun nemacs-library-package-archive-index--dependency-form
    (requires metadata-rows)
  "Return archive-contents dependency form for REQUIRES using METADATA-ROWS."
  (mapcar
   (lambda (package)
     (let ((metadata
            (nemacs-library-package-archive-index--find-row
             package metadata-rows 0)))
       (unless metadata
         (error "missing dependency metadata for %s" package))
       (list (intern package)
             (version-to-list (nth 1 metadata)))))
   (nemacs-library-package-archive-index--split-list requires)))

(defun nemacs-library-package-archive-index--extras (metadata)
  "Return package archive extras from METADATA."
  (let ((keywords (nemacs-library-package-archive-index--split-list
                   (nth 3 metadata)))
        (url (nth 4 metadata))
        (license (nth 5 metadata))
        (maintainer (nth 6 metadata))
        (authors (nth 7 metadata)))
    `((:keywords . ,keywords)
      (:url . ,url)
      (:license . ,license)
      (:maintainer . ,maintainer)
      (:authors . (,authors)))))

(defun nemacs-library-package-archive-index--archive-entry
    (metadata metadata-rows)
  "Return archive-contents entry for METADATA using METADATA-ROWS."
  (let ((package (intern (nth 0 metadata)))
        (version (version-to-list (nth 1 metadata)))
        (summary (nth 2 metadata))
        (requires (nth 9 metadata)))
    (cons package
          (vector version
                  (nemacs-library-package-archive-index--dependency-form
                   requires metadata-rows)
                  summary
                  'tar
                  (nemacs-library-package-archive-index--extras metadata)))))

(defun nemacs-library-package-archive-index--validate-archive (metadata archives)
  "Return archive row for METADATA from ARCHIVES and validate file presence."
  (let* ((package (nth 0 metadata))
         (archive-row
          (nemacs-library-package-archive-index--find-row package archives 0))
         (archive
          (and archive-row
               (expand-file-name (nth 4 archive-row)
                                 nemacs-library-package-archive-index-repo-root))))
    (unless archive-row
      (error "missing archive row for %s" package))
    (unless (file-readable-p archive)
      (error "missing readable archive for %s: %s" package archive))
    archive-row))

(defun nemacs-library-package-archive-index--archive-contents-path ()
  "Return generated archive-contents path."
  (expand-file-name "archive-contents"
                    nemacs-library-package-archive-index-root))

(defun nemacs-library-package-archive-index--write-archive-contents
    (metadata-rows)
  "Write archive-contents for METADATA-ROWS."
  (make-directory nemacs-library-package-archive-index-root t)
  (let ((print-length nil)
        (print-level nil)
        (path (nemacs-library-package-archive-index--archive-contents-path)))
    (with-temp-file path
      (prin1 (cons 1
                   (mapcar
                    (lambda (metadata)
                      (nemacs-library-package-archive-index--archive-entry
                       metadata metadata-rows))
                    metadata-rows))
             (current-buffer))
      (insert "\n"))
    path))

(defun nemacs-library-package-archive-index--build-rows ()
  "Generate archive index files and return report rows."
  (let ((metadata-rows
         (nemacs-library-package-archive-index--read-tsv
          nemacs-library-package-archive-index-metadata))
        (archive-rows
         (nemacs-library-package-archive-index--read-tsv
          nemacs-library-package-archive-index-archives))
        rows)
    (dolist (metadata metadata-rows)
      (let* ((archive-row
              (nemacs-library-package-archive-index--validate-archive
               metadata archive-rows))
             (package (nth 0 metadata))
             (requires (nth 9 metadata)))
        (push (list package
                    (nth 1 metadata)
                    (nth 8 metadata)
                    requires
                    (nth 4 archive-row)
                    (nth 7 archive-row)
                    "tar"
                    "ok"
                    "")
              rows)))
    (let ((archive-contents
           (nemacs-library-package-archive-index--write-archive-contents
            metadata-rows)))
      (setq rows (sort (nreverse rows)
                       (lambda (a b) (string< (car a) (car b)))))
      (list rows archive-contents))))

(defun nemacs-library-package-archive-index--write-tsv (rows)
  "Write index ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-archive-index-output) t)
  (with-temp-file nemacs-library-package-archive-index-output
    (insert
     (nemacs-library-package-archive-index--row
      "package_id" "version" "loader_feature" "package_requires"
      "archive_file" "archive_bytes" "kind" "status" "details")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-archive-index--row row)
              "\n"))))

(defun nemacs-library-package-archive-index--write-summary
    (rows archive-contents)
  "Write index ROWS and ARCHIVE-CONTENTS summary."
  (make-directory
   (file-name-directory nemacs-library-package-archive-index-summary-output)
   t)
  (with-temp-file nemacs-library-package-archive-index-summary-output
    (insert "#+TITLE: nemacs library package archive index\n\n")
    (insert "* Summary\n\n")
    (insert (format "- packages: %d\n" (length rows)))
    (insert (format "- archive-contents: =%s=\n"
                    (file-relative-name
                     archive-contents
                     nemacs-library-package-archive-index-repo-root)))
    (insert "- archive-version: 1\n\n")
    (insert "* Packages\n\n")
    (insert "| Package | Loader | Requires | Kind | Archive |\n")
    (insert "|---------+--------+----------+------+---------|\n")
    (dolist (row rows)
      (insert
       (format "| =%s= | =%s= | =%s= | =%s= | =%s= |\n"
               (nth 0 row)
               (nth 2 row)
               (nth 3 row)
               (nth 6 row)
               (nth 4 row))))
    (insert "\n* Notes\n\n")
    (insert "- This index is consumed by Emacs package.el through =package-refresh-contents=.\n")
    (insert "- Use =make nemacs-library-package-index-smoke= to validate install through archive discovery.\n")))

;;;###autoload
(defun nemacs-library-package-archive-index-batch ()
  "Generate package archive index artifacts."
  (let* ((result (nemacs-library-package-archive-index--build-rows))
         (rows (car result))
         (archive-contents (cadr result)))
    (unless rows
      (error "empty package archive index rows"))
    (nemacs-library-package-archive-index--write-tsv rows)
    (nemacs-library-package-archive-index--write-summary
     rows archive-contents)
    (princ
     (format
      "nemacs-library-package-archive-index: packages=%d archive-contents=%s output=%s summary=%s\n"
      (length rows)
      archive-contents
      nemacs-library-package-archive-index-output
      nemacs-library-package-archive-index-summary-output))))

(provide 'nemacs-library-package-archive-index)

;;; nemacs-library-package-archive-index.el ends here
