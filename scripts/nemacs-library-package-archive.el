;;; nemacs-library-package-archive.el --- build package archive drafts -*- lexical-binding: t; -*-

;;; Commentary:

;; Build archive-like multi-file package tarballs from the generated package
;; metadata and reusable package scaffold.  The tarballs are draft artifacts
;; for install smoke; they are not a publication claim.

;;; Code:

(defvar nemacs-library-package-archive-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-archive-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-archive-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-archive-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-archive-repo-root)
  "Package scaffold TSV.")

(defvar nemacs-library-package-archive-root
  (expand-file-name "build/nemacs-library-package-archives"
                    nemacs-library-package-archive-repo-root)
  "Directory for generated package tar archives.")

(defvar nemacs-library-package-archive-staging-root
  (expand-file-name "build/nemacs-library-package-archive-staging"
                    nemacs-library-package-archive-repo-root)
  "Directory for archive staging trees.")

(defvar nemacs-library-package-archive-output
  (expand-file-name "build/nemacs-library-package-archive.tsv"
                    nemacs-library-package-archive-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-archive-summary-output
  (expand-file-name "build/nemacs-library-package-archive.org"
                    nemacs-library-package-archive-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-archive--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-archive--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-archive--tsv-cell cells "\t"))

(defun nemacs-library-package-archive--read-tsv (file)
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

(defun nemacs-library-package-archive--metadata-rows ()
  "Return package metadata rows."
  (nemacs-library-package-archive--read-tsv
   nemacs-library-package-archive-metadata))

(defun nemacs-library-package-archive--scaffold-rows ()
  "Return reusable package scaffold file rows."
  (let (rows)
    (dolist (row (nemacs-library-package-archive--read-tsv
                  nemacs-library-package-archive-scaffold))
      (when (string= (nth 0 row) "file")
        (push row rows)))
    (nreverse rows)))

(defun nemacs-library-package-archive--copy-file (source target)
  "Copy SOURCE to TARGET, creating parent directories."
  (make-directory (file-name-directory target) t)
  (copy-file source target t))

(defun nemacs-library-package-archive--file-size (file)
  "Return size of FILE in bytes."
  (file-attribute-size (file-attributes file)))

(defun nemacs-library-package-archive--relative (file)
  "Return FILE relative to repository root."
  (file-relative-name file nemacs-library-package-archive-repo-root))

(defun nemacs-library-package-archive--ensure-unique-basename
    (basename seen package)
  "Signal if BASENAME already appears in SEEN for PACKAGE."
  (when (member basename seen)
    (error "duplicate archive basename for %s: %s" package basename))
  (cons basename seen))

(defun nemacs-library-package-archive--stage-package
    (metadata scaffold-rows)
  "Stage package METADATA using SCAFFOLD-ROWS and return (DIR FILE-COUNT)."
  (let* ((package (nth 0 metadata))
         (install-dir (nth 15 metadata))
         (stage-dir (expand-file-name
                     install-dir
                     nemacs-library-package-archive-staging-root))
         (package-file (expand-file-name
                        (nth 14 metadata)
                        nemacs-library-package-archive-repo-root))
         (readme (expand-file-name
                  (format "packages/%s/README.org" package)
                  nemacs-library-package-archive-repo-root))
         (seen nil)
         (count 0))
    (when (file-directory-p stage-dir)
      (delete-directory stage-dir t))
    (make-directory stage-dir t)
    (setq seen
          (nemacs-library-package-archive--ensure-unique-basename
           (file-name-nondirectory package-file) seen package))
    (nemacs-library-package-archive--copy-file
     package-file
     (expand-file-name (file-name-nondirectory package-file) stage-dir))
    (setq count (1+ count))
    (when (file-readable-p readme)
      (setq seen
            (nemacs-library-package-archive--ensure-unique-basename
             "README.org" seen package))
      (nemacs-library-package-archive--copy-file
       readme
       (expand-file-name "README.org" stage-dir))
      (setq count (1+ count)))
    (dolist (row scaffold-rows)
      (when (string= package (nth 1 row))
        (let* ((source (expand-file-name
                        (nth 4 row)
                        nemacs-library-package-archive-repo-root))
               (basename (file-name-nondirectory source)))
          (setq seen
                (nemacs-library-package-archive--ensure-unique-basename
                 basename seen package))
          (nemacs-library-package-archive--copy-file
           source
           (expand-file-name basename stage-dir))
          (setq count (1+ count)))))
    (list stage-dir count)))

(defun nemacs-library-package-archive--tar-package (metadata)
  "Create tar archive for staged package METADATA and return archive path."
  (let* ((install-dir (nth 15 metadata))
         (archive (expand-file-name
                   (format "%s.tar" install-dir)
                   nemacs-library-package-archive-root))
         (tar (or (executable-find "tar")
                  (error "tar executable is required"))))
    (when (file-exists-p archive)
      (delete-file archive))
    (make-directory (file-name-directory archive) t)
    (unless (= 0 (call-process tar nil nil nil
                               "--sort=name"
                               "--owner=0"
                               "--group=0"
                               "--numeric-owner"
                               "--mtime=@0"
                               "-C"
                               nemacs-library-package-archive-staging-root
                               "-cf"
                               archive
                               install-dir))
      (error "tar failed for %s" install-dir))
    archive))

(defun nemacs-library-package-archive--build-rows ()
  "Build package archive tarballs and return report rows."
  (let ((metadata-rows (nemacs-library-package-archive--metadata-rows))
        (scaffold-rows (nemacs-library-package-archive--scaffold-rows))
        rows)
    (when (file-directory-p nemacs-library-package-archive-root)
      (delete-directory nemacs-library-package-archive-root t))
    (when (file-directory-p nemacs-library-package-archive-staging-root)
      (delete-directory nemacs-library-package-archive-staging-root t))
    (make-directory nemacs-library-package-archive-root t)
    (make-directory nemacs-library-package-archive-staging-root t)
    (dolist (metadata metadata-rows)
      (let* ((package (nth 0 metadata))
             (version (nth 1 metadata))
             (loader (nth 8 metadata))
             (requires (nth 9 metadata))
             (staged (nemacs-library-package-archive--stage-package
                      metadata scaffold-rows))
             (stage-dir (car staged))
             (file-count (cadr staged))
             (archive (nemacs-library-package-archive--tar-package metadata)))
        (push
         (list package
               version
               loader
               requires
               (nemacs-library-package-archive--relative archive)
               (nemacs-library-package-archive--relative stage-dir)
               file-count
               (nemacs-library-package-archive--file-size archive)
               "ok"
               "")
         rows)))
    (sort (nreverse rows)
          (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-package-archive--write-tsv (rows)
  "Write archive ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-archive-output) t)
  (with-temp-file nemacs-library-package-archive-output
    (insert
     (nemacs-library-package-archive--row
      "package_id" "version" "loader_feature" "package_requires"
      "archive_file" "staging_dir" "file_count" "archive_bytes"
      "status" "details")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-archive--row row) "\n"))))

(defun nemacs-library-package-archive--write-summary (rows)
  "Write archive ROWS to Org summary output."
  (let ((ok 0)
        (bytes 0))
    (dolist (row rows)
      (when (string= (nth 8 row) "ok")
        (setq ok (1+ ok)))
      (setq bytes (+ bytes (string-to-number (format "%s" (nth 7 row))))))
    (make-directory
     (file-name-directory nemacs-library-package-archive-summary-output)
     t)
    (with-temp-file nemacs-library-package-archive-summary-output
      (insert "#+TITLE: nemacs library package archive drafts\n\n")
      (insert "* Summary\n\n")
      (insert (format "- packages: %d\n" (length rows)))
      (insert (format "- archives: %d\n" ok))
      (insert (format "- total-bytes: %d\n\n" bytes))
      (insert "* Archives\n\n")
      (insert "| Package | Loader | Files | Bytes | Archive |\n")
      (insert "|---------+--------+-------+-------+---------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | %s | =%s= |\n"
                 (nth 0 row)
                 (nth 2 row)
                 (nth 6 row)
                 (nth 7 row)
                 (nth 4 row))))
      (insert "\n* Notes\n\n")
      (insert "- Archives are generated from package metadata and scaffold files.\n")
      (insert "- Archive layout is =PACKAGE-VERSION/= with flat Elisp files plus README and =PACKAGE-pkg.el=.\n")
      (insert "- Use =make nemacs-library-package-archive-smoke= to validate install from these tar files.\n"))))

;;;###autoload
(defun nemacs-library-package-archive-batch ()
  "Build package archive draft tarballs."
  (let ((rows (nemacs-library-package-archive--build-rows)))
    (unless rows
      (error "empty package archive rows"))
    (nemacs-library-package-archive--write-tsv rows)
    (nemacs-library-package-archive--write-summary rows)
    (princ
     (format
      "nemacs-library-package-archive: archives=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-archive-output
      nemacs-library-package-archive-summary-output))))

(provide 'nemacs-library-package-archive)

;;; nemacs-library-package-archive.el ends here
