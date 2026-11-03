;;; nemacs-library-package-archive-checksum.el --- verify package archive reproducibility -*- lexical-binding: t; -*-

;;; Commentary:

;; Rebuild package archives into isolated roots and compare SHA-256 checksums
;; with the primary archive output.  This proves the draft package archives are
;; deterministic enough for later signing/checksum publication work.

;;; Code:

(require 'nemacs-library-package-archive)

(defvar nemacs-library-package-archive-checksum-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-archive-checksum-metadata
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-archive-checksum-repo-root)
  "Package metadata TSV.")

(defvar nemacs-library-package-archive-checksum-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-archive-checksum-repo-root)
  "Package scaffold TSV.")

(defvar nemacs-library-package-archive-checksum-archives
  (expand-file-name "build/nemacs-library-package-archive.tsv"
                    nemacs-library-package-archive-checksum-repo-root)
  "Primary package archive TSV.")

(defvar nemacs-library-package-archive-checksum-rebuild-root
  (expand-file-name "build/nemacs-library-package-archive-checksum"
                    nemacs-library-package-archive-checksum-repo-root)
  "Root for isolated rebuild archive outputs.")

(defvar nemacs-library-package-archive-checksum-output
  (expand-file-name "build/nemacs-library-package-archive-checksum.tsv"
                    nemacs-library-package-archive-checksum-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-archive-checksum-summary-output
  (expand-file-name "build/nemacs-library-package-archive-checksum.org"
                    nemacs-library-package-archive-checksum-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-archive-checksum--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-archive-checksum--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-archive-checksum--tsv-cell cells "\t"))

(defun nemacs-library-package-archive-checksum--read-tsv (file)
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

(defun nemacs-library-package-archive-checksum--hash-file (file)
  "Return SHA-256 hex digest for FILE."
  (unless (file-readable-p file)
    (error "missing readable archive file: %s" file))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-archive-checksum--find-row
    (package rows)
  "Return row for PACKAGE from ROWS."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth 0 row))
        (setq found row)))))

(defun nemacs-library-package-archive-checksum--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file nemacs-library-package-archive-checksum-repo-root))

(defun nemacs-library-package-archive-checksum--rebuild (name)
  "Rebuild archives in isolated rebuild root NAME and return archive rows."
  (let ((nemacs-library-package-archive-repo-root
         nemacs-library-package-archive-checksum-repo-root)
        (nemacs-library-package-archive-metadata
         nemacs-library-package-archive-checksum-metadata)
        (nemacs-library-package-archive-scaffold
         nemacs-library-package-archive-checksum-scaffold)
        (nemacs-library-package-archive-root
         (expand-file-name
          (format "%s/archives" name)
          nemacs-library-package-archive-checksum-rebuild-root))
        (nemacs-library-package-archive-staging-root
         (expand-file-name
          (format "%s/staging" name)
          nemacs-library-package-archive-checksum-rebuild-root)))
    (nemacs-library-package-archive--build-rows)))

(defun nemacs-library-package-archive-checksum--build-rows ()
  "Return checksum verification rows."
  (let* ((primary-rows
          (nemacs-library-package-archive-checksum--read-tsv
           nemacs-library-package-archive-checksum-archives))
         (rebuild-1
          (nemacs-library-package-archive-checksum--rebuild "rebuild-1"))
         (rebuild-2
          (nemacs-library-package-archive-checksum--rebuild "rebuild-2"))
         rows)
    (dolist (primary primary-rows)
      (let* ((package (nth 0 primary))
             (version (nth 1 primary))
             (archive-file (nth 4 primary))
             (primary-hash
              (nemacs-library-package-archive-checksum--hash-file
               (nemacs-library-package-archive-checksum--absolute
                archive-file)))
             (row-1
              (nemacs-library-package-archive-checksum--find-row
               package rebuild-1))
             (row-2
              (nemacs-library-package-archive-checksum--find-row
               package rebuild-2)))
        (unless row-1
          (error "missing rebuild-1 row for %s" package))
        (unless row-2
          (error "missing rebuild-2 row for %s" package))
        (let* ((hash-1
                (nemacs-library-package-archive-checksum--hash-file
                 (nemacs-library-package-archive-checksum--absolute
                  (nth 4 row-1))))
               (hash-2
                (nemacs-library-package-archive-checksum--hash-file
                 (nemacs-library-package-archive-checksum--absolute
                  (nth 4 row-2))))
               (ok (and (string= primary-hash hash-1)
                        (string= primary-hash hash-2))))
          (push
           (list package
                 version
                 archive-file
                 primary-hash
                 hash-1
                 hash-2
                 (if ok "yes" "no")
                 (if ok "ok" "fail")
                 (if ok "" "primary/rebuild checksum mismatch"))
           rows))))
    (sort (nreverse rows)
          (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-package-archive-checksum--write-tsv (rows)
  "Write checksum ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-archive-checksum-output) t)
  (with-temp-file nemacs-library-package-archive-checksum-output
    (insert
     (nemacs-library-package-archive-checksum--row
      "package_id" "version" "archive_file" "primary_sha256"
      "rebuild1_sha256" "rebuild2_sha256" "deterministic"
      "status" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-archive-checksum--row row)
       "\n"))))

(defun nemacs-library-package-archive-checksum--write-summary (rows)
  "Write checksum ROWS to Org summary output."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 7 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory
      nemacs-library-package-archive-checksum-summary-output)
     t)
    (with-temp-file nemacs-library-package-archive-checksum-summary-output
      (insert "#+TITLE: nemacs library package archive checksum\n\n")
      (insert "* Summary\n\n")
      (insert (format "- packages: %d\n" (length rows)))
      (insert (format "- deterministic: %d\n" ok))
      (insert (format "- failures: %d\n\n" fail))
      (insert "* Packages\n\n")
      (insert "| Package | Status | Deterministic | SHA-256 |\n")
      (insert "|---------+--------+---------------+---------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | %s | %s | =%s= |\n"
                 (nth 0 row)
                 (nth 7 row)
                 (nth 6 row)
                 (nth 3 row))))
      (insert "\n* Notes\n\n")
      (insert "- This verifier rebuilds every archive twice in isolated roots.\n")
      (insert "- A package passes only when primary, rebuild-1, and rebuild-2 SHA-256 digests match.\n"))))

;;;###autoload
(defun nemacs-library-package-archive-checksum-batch ()
  "Verify package archive checksums are reproducible."
  (let* ((rows (nemacs-library-package-archive-checksum--build-rows))
         (failures 0))
    (unless rows
      (error "empty package archive checksum rows"))
    (nemacs-library-package-archive-checksum--write-tsv rows)
    (nemacs-library-package-archive-checksum--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 7 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-archive-checksum: packages=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-package-archive-checksum-output
      nemacs-library-package-archive-checksum-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-archive-checksum)

;;; nemacs-library-package-archive-checksum.el ends here
