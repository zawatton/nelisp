;;; nemacs-library-package-metadata.el --- generate package metadata drafts -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate package archive style metadata for the reusable package scaffold.
;; This writes review artifacts and `*-pkg.el' files under packages/PACKAGE/.
;; The files are still draft metadata; src/ remains the source of truth.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nemacs-library-package-descriptors)

(defvar nemacs-library-package-metadata-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-metadata-output
  (expand-file-name "build/nemacs-library-package-metadata.tsv"
                    nemacs-library-package-metadata-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-metadata-summary-output
  (expand-file-name "build/nemacs-library-package-metadata.org"
                    nemacs-library-package-metadata-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-metadata--url
  "https://github.com/zawatton/nelisp-emacs"
  "Package metadata URL.")

(defconst nemacs-library-package-metadata--license "GPL-3.0-or-later"
  "Package metadata license.")

(defconst nemacs-library-package-metadata--maintainer "zawatton"
  "Package metadata maintainer.")

(defconst nemacs-library-package-metadata--summary-by-package
  '((nelisp-emacs-foundation
     . "Reusable nelisp-emacs compatibility primitives and core Lisp helpers.")
    (nelisp-emacs-text-core
     . "Reusable nelisp-emacs text, coding, and regexp helpers.")
    (nelisp-emacs-buffer-core
     . "Reusable nelisp-emacs buffer, line, search, and compatibility helpers.")
    (nelisp-emacs-editing
     . "Reusable nelisp-emacs editing and undo helpers.")
    (nelisp-emacs-io
     . "Reusable nelisp-emacs file, process, dump, image, and loaddefs helpers.")
    (nelisp-emacs-special-buffers
     . "Reusable nelisp-emacs special buffer helpers.")
    (nelisp-emacs-core
     . "Reusable nelisp-emacs command, frame, mode, help, and window helpers.")
    (nelisp-emacs-textmodes-stub
     . "Reusable nelisp-emacs text mode compatibility stubs."))
  "Package summaries keyed by package id.")

(defun nemacs-library-package-metadata--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-metadata--join (values)
  "Return VALUES as a comma-separated stable string."
  (mapconcat #'identity
             (sort (delete-dups
                    (mapcar #'nemacs-library-package-metadata--symbol-name
                            (copy-sequence values)))
                   #'string<)
             ","))

(defun nemacs-library-package-metadata--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-metadata--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-metadata--tsv-cell cells "\t"))

(defun nemacs-library-package-metadata--summary (package-id)
  "Return metadata summary for PACKAGE-ID."
  (or (cdr (assq package-id
                 nemacs-library-package-metadata--summary-by-package))
      (format "Reusable nelisp-emacs package %s." package-id)))

(defun nemacs-library-package-metadata--keywords (owner)
  "Return keyword strings for OWNER."
  (list "emacs" "lisp" "nelisp" "compatibility"
        (downcase (nemacs-library-package-metadata--symbol-name owner))))

(defun nemacs-library-package-metadata--package-file (package-id)
  "Return package metadata file path for PACKAGE-ID."
  (format "packages/%s/%s-pkg.el" package-id package-id))

(defun nemacs-library-package-metadata--install-dir (package-id)
  "Return package-style install directory name for PACKAGE-ID."
  (format "%s-%s"
          package-id
          nemacs-library-package-descriptors-version))

(defun nemacs-library-package-metadata--rows ()
  "Return package metadata rows."
  (let (rows)
    (dolist (descriptor (nemacs-library-package-descriptors--descriptor-rows))
      (let* ((package-id (nth 1 descriptor))
             (owner (nth 2 descriptor))
             (loader (nth 3 descriptor))
             (requires (mapcar #'nemacs-library-package-descriptors--package-id
                               (nth 6 descriptor)))
             (members (nth 4 descriptor))
             (lazy (nth 7 descriptor))
             (host (nth 9 descriptor))
             (vendor (nth 10 descriptor))
             (keywords (nemacs-library-package-metadata--keywords owner)))
        (push
         (list package-id
               nemacs-library-package-descriptors-version
               (nemacs-library-package-metadata--summary package-id)
               keywords
               nemacs-library-package-metadata--url
               nemacs-library-package-metadata--license
               nemacs-library-package-metadata--maintainer
               nemacs-library-package-metadata--maintainer
               loader
               requires
               members
               lazy
               host
               vendor
               (nemacs-library-package-metadata--package-file package-id)
               (nemacs-library-package-metadata--install-dir package-id))
         rows)))
    (sort (nreverse rows)
          (lambda (a b)
            (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun nemacs-library-package-metadata--package-requires-form (requires)
  "Return package descriptor dependency form for REQUIRES."
  (if requires
      (mapcar
       (lambda (package-id)
         (list package-id
               nemacs-library-package-descriptors-version))
       requires)
    nil))

(defun nemacs-library-package-metadata--write-package-file (row)
  "Write `define-package' metadata file for ROW."
  (let* ((package-id (nth 0 row))
         (version (nth 1 row))
         (summary (nth 2 row))
         (keywords (nth 3 row))
         (url (nth 4 row))
         (license (nth 5 row))
         (maintainer (nth 6 row))
         (authors (nth 7 row))
         (requires (nth 9 row))
         (relative (nth 14 row))
         (target (expand-file-name relative
                                   nemacs-library-package-metadata-repo-root)))
    (make-directory (file-name-directory target) t)
    (with-temp-file target
      (insert ";;; " (file-name-nondirectory relative)
              " --- package metadata -*- lexical-binding: t; no-byte-compile: t; -*-\n\n")
      (insert ";; Generated by scripts/nemacs-library-package-metadata.el.\n")
      (insert ";; Source of truth remains src/ and the package descriptor tooling.\n\n")
      (insert "(define-package\n")
      (insert "  " (prin1-to-string
                    (nemacs-library-package-metadata--symbol-name package-id))
              "\n")
      (insert "  " (prin1-to-string version) "\n")
      (insert "  " (prin1-to-string summary) "\n")
      (insert "  '"
              (prin1-to-string
               (nemacs-library-package-metadata--package-requires-form
                requires))
              "\n")
      (insert "  :keywords '" (prin1-to-string keywords) "\n")
      (insert "  :url " (prin1-to-string url) "\n")
      (insert "  :maintainer " (prin1-to-string maintainer) "\n")
      (insert "  :authors '" (prin1-to-string (list authors)) "\n")
      (insert "  :license " (prin1-to-string license) ")\n\n")
      (insert ";;; " (file-name-nondirectory relative) " ends here\n"))))

(defun nemacs-library-package-metadata--write-tsv (rows output)
  "Write metadata ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-metadata--row
      "package_id" "version" "summary" "keywords" "url" "license"
      "maintainer" "authors" "loader_feature" "package_requires"
      "member_features" "lazy_features" "host_features"
      "vendor_package_features" "package_file" "install_dir")
     "\n")
    (dolist (row rows)
      (insert
       (nemacs-library-package-metadata--row
        (nth 0 row)
        (nth 1 row)
        (nth 2 row)
        (nemacs-library-package-metadata--join (nth 3 row))
        (nth 4 row)
        (nth 5 row)
        (nth 6 row)
        (nth 7 row)
        (nth 8 row)
        (nemacs-library-package-metadata--join (nth 9 row))
        (nemacs-library-package-metadata--join (nth 10 row))
        (nemacs-library-package-metadata--join (nth 11 row))
        (nemacs-library-package-metadata--join (nth 12 row))
        (nemacs-library-package-metadata--join (nth 13 row))
        (nth 14 row)
        (nth 15 row))
       "\n"))))

(defun nemacs-library-package-metadata--write-summary (rows output)
  "Write metadata ROWS summary to Org OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert "#+TITLE: nemacs library package metadata drafts\n\n")
    (insert "* Summary\n\n")
    (insert (format "- packages: %d\n" (length rows)))
    (insert (format "- version: %s\n"
                    nemacs-library-package-descriptors-version))
    (insert "- metadata files: generated under =packages/PACKAGE/PACKAGE-pkg.el=\n")
    (insert "- license: =GPL-3.0-or-later=\n")
    (insert (format "- url: =%s=\n\n"
                    nemacs-library-package-metadata--url))
    (insert "* Packages\n\n")
    (insert "| Package | Loader | Requires | Keywords | Metadata file |\n")
    (insert "|---------+--------+----------+----------+---------------|\n")
    (dolist (row rows)
      (insert
       (format "| =%s= | =%s= | =%s= | =%s= | =%s= |\n"
               (nth 0 row)
               (nth 8 row)
               (nemacs-library-package-metadata--join (nth 9 row))
               (nemacs-library-package-metadata--join (nth 3 row))
               (nth 14 row))))
    (insert "\n* Notes\n\n")
    (insert "- Generated metadata is package-archive style review input, not a published archive claim.\n")
    (insert "- Per-package install smoke flattens scaffold files into package-style install directories and loads these metadata files.\n")
    (insert "- Update descriptor ownership and dependencies first; regenerate metadata rather than editing generated =*-pkg.el= files.\n")))

;;;###autoload
(defun nemacs-library-package-metadata-batch ()
  "Write package metadata draft artifacts and generated `*-pkg.el' files."
  (let ((rows (nemacs-library-package-metadata--rows)))
    (unless rows
      (error "empty nelisp-emacs package metadata rows"))
    (dolist (row rows)
      (nemacs-library-package-metadata--write-package-file row))
    (nemacs-library-package-metadata--write-tsv
     rows nemacs-library-package-metadata-output)
    (nemacs-library-package-metadata--write-summary
     rows nemacs-library-package-metadata-summary-output)
    (princ
     (format
      "nemacs-library-package-metadata: packages=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-metadata-output
      nemacs-library-package-metadata-summary-output))))

(provide 'nemacs-library-package-metadata)

;;; nemacs-library-package-metadata.el ends here
