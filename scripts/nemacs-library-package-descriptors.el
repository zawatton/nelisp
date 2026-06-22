;;; nemacs-library-package-descriptors.el --- draft package descriptors -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate review artifacts that describe the current reusable facade
;; packages as package-descriptor drafts.  The output is intentionally not a
;; final package archive format yet; it is a stable extraction planning view
;; derived from the public facade manifest and generated dependency edges.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-emacs)
(require 'nemacs-library-package-deps)

(defvar nemacs-library-package-descriptors-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-descriptors-output
  (expand-file-name "build/nemacs-library-package-descriptors.tsv"
                    nemacs-library-package-descriptors-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-descriptors-summary-output
  (expand-file-name "build/nemacs-library-package-descriptors.org"
                    nemacs-library-package-descriptors-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-descriptors-version "0.1.0"
  "Draft package version used in descriptor artifacts.")

(defun nemacs-library-package-descriptors--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-descriptors--sort-strings (values)
  "Return VALUES as sorted unique strings."
  (sort (delete-dups
         (mapcar #'nemacs-library-package-descriptors--symbol-name
                 (copy-sequence values)))
        #'string<))

(defun nemacs-library-package-descriptors--sort-symbols (values)
  "Return VALUES as sorted unique symbols."
  (sort (delete-dups (copy-sequence values))
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))))

(defun nemacs-library-package-descriptors--join (values)
  "Return VALUES as a comma-separated stable string."
  (mapconcat #'identity
             (nemacs-library-package-descriptors--sort-strings values)
             ","))

(defun nemacs-library-package-descriptors--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-descriptors--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-descriptors--tsv-cell cells "\t"))

(defun nemacs-library-package-descriptors--package-id (name)
  "Return package archive id for facade package NAME."
  (intern (format "nelisp-emacs-%s" name)))

(defun nemacs-library-package-descriptors--package-files
    (package provide-map)
  "Return source files for PACKAGE using PROVIDE-MAP."
  (let (files)
    (dolist (feature (cons (nth 2 package) (nth 3 package)))
      (let ((entry (gethash feature provide-map)))
        (when entry
          (cl-pushnew (car entry) files :test #'equal))))
    (sort files #'string<)))

(defun nemacs-library-package-descriptors--package-deps (name rows)
  "Return manifest package dependencies for package NAME from ROWS."
  (let (deps)
    (dolist (row rows)
      (when (and (eq (nth 0 row) name)
                 (equal (nth 7 row) "manifest-package")
                 (not (equal (nth 4 row) "")))
        (cl-pushnew (nth 4 row) deps :test #'eq)))
    (sort deps (lambda (a b)
                 (string< (symbol-name a) (symbol-name b))))))

(defun nemacs-library-package-descriptors--feature-deps
    (name relation rows)
  "Return required features for package NAME with RELATION from ROWS."
  (let (features)
    (dolist (row rows)
      (when (and (eq (nth 0 row) name)
                 (equal (nth 7 row) relation))
        (cl-pushnew (nth 3 row) features :test #'eq)))
    (sort features (lambda (a b)
                     (string< (symbol-name a) (symbol-name b))))))

(defun nemacs-library-package-descriptors--files-for-relation
    (name relation rows)
  "Return target files for package NAME dependency RELATION from ROWS."
  (let (files)
    (dolist (row rows)
      (when (and (eq (nth 0 row) name)
                 (equal (nth 7 row) relation)
                 (not (equal (nth 6 row) "")))
        (cl-pushnew (nth 6 row) files :test #'equal)))
    (sort files #'string<)))

(defun nemacs-library-package-descriptors--files-for-features
    (features provide-map)
  "Return source files for FEATURES using PROVIDE-MAP."
  (let (files)
    (dolist (feature features)
      (let ((entry (gethash feature provide-map)))
        (when entry
          (cl-pushnew (car entry) files :test #'equal))))
    (sort files #'string<)))

(defun nemacs-library-package-descriptors--descriptor-rows ()
  "Return package descriptor draft rows."
  (let* ((ownership (nemacs-library-package-deps--ownership))
         (provide-map (nemacs-library-package-deps--provide-map ownership))
         (packages (nemacs-library-package-deps--manifest-rows))
         (deps (nemacs-library-package-deps--rows))
         rows)
    (dolist (package packages)
      (let* ((name (car package))
             (owner (nth 1 package))
             (loader (nth 2 package))
             (members (nth 3 package))
             (declared-lazy (nth 4 package))
             (requires
              (nemacs-library-package-descriptors--package-deps name deps))
             (inferred-lazy
              (nemacs-library-package-descriptors--feature-deps
               name "lazy-manifest-package" deps))
             (lazy
              (nemacs-library-package-descriptors--sort-symbols
               (append declared-lazy inferred-lazy)))
             (lazy-files
              (nemacs-library-package-descriptors--files-for-features
               lazy provide-map))
             (host
              (nemacs-library-package-descriptors--feature-deps
               name "host-feature" deps))
             (vendor
              (nemacs-library-package-descriptors--feature-deps
               name "vendor-package" deps))
             (unknown-external
              (nemacs-library-package-descriptors--feature-deps
               name "external-or-host" deps))
             (files
              (nemacs-library-package-descriptors--package-files
               package provide-map)))
        (push (list name
                    (nemacs-library-package-descriptors--package-id name)
                    owner
                    loader
                    members
                    files
                    requires
                    lazy
                    lazy-files
                    host
                    vendor
                    unknown-external)
              rows)))
    (nreverse rows)))

(defun nemacs-library-package-descriptors--write-tsv (rows output)
  "Write descriptor ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-descriptors--row
      "name" "package_id" "owner" "loader_feature" "version"
      "member_features" "source_files" "package_requires"
      "lazy_features" "lazy_source_files" "host_features"
      "vendor_package_features" "unknown_external_features")
     "\n")
    (dolist (row rows)
      (insert
       (nemacs-library-package-descriptors--row
        (nth 0 row)
        (nth 1 row)
        (nth 2 row)
        (nth 3 row)
        nemacs-library-package-descriptors-version
        (nemacs-library-package-descriptors--join (nth 4 row))
        (nemacs-library-package-descriptors--join (nth 5 row))
        (nemacs-library-package-descriptors--join
         (mapcar #'nemacs-library-package-descriptors--package-id
                 (nth 6 row)))
        (nemacs-library-package-descriptors--join (nth 7 row))
        (nemacs-library-package-descriptors--join (nth 8 row))
        (nemacs-library-package-descriptors--join (nth 9 row))
        (nemacs-library-package-descriptors--join (nth 10 row))
        (nemacs-library-package-descriptors--join (nth 11 row)))
       "\n"))))

(defun nemacs-library-package-descriptors--write-summary (rows output)
  "Write descriptor ROWS to Org OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert "#+TITLE: nemacs library package descriptor drafts\n\n")
    (insert (format "* Summary\n\n- packages: %d\n- draft-version: %s\n\n"
                    (length rows)
                    nemacs-library-package-descriptors-version))
    (insert "* Packages\n\n")
    (dolist (row rows)
      (insert (format "** %s\n\n" (nth 1 row)))
      (insert (format "- facade-name: =%s=\n" (nth 0 row)))
      (insert (format "- owner: =%s=\n" (nth 2 row)))
      (insert (format "- loader-feature: =%s=\n" (nth 3 row)))
      (insert (format "- member-features: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (nth 4 row))))
      (insert (format "- source-files: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (nth 5 row))))
      (insert (format "- package-requires: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (mapcar
                        #'nemacs-library-package-descriptors--package-id
                        (nth 6 row)))))
      (insert (format "- lazy-features: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (nth 7 row))))
      (insert (format "- lazy-source-files: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (nth 8 row))))
      (insert (format "- host-features: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (nth 9 row))))
      (insert (format "- vendor-package-features: =%s=\n"
                      (nemacs-library-package-descriptors--join
                       (nth 10 row))))
      (insert (format "- unknown-external-features: =%s=\n\n"
                      (nemacs-library-package-descriptors--join
                       (nth 11 row)))))
    (insert "* Notes\n\n")
    (insert "- Generated from `nelisp-emacs-library-package-manifest' and source `require' edges.\n")
    (insert "- `package-requires' contains only facade package dependencies.\n")
    (insert "- `host-features' are expected from host Emacs.\n")
    (insert "- `vendor-package-features' are expected from vendored packages outside the facade set.\n")
    (insert "- `unknown-external-features' need classification before package publication.\n")
    (insert "- `lazy-features' are declared lazy package companions and stay outside eager package membership unless a load-time decision changes.\n")
    (insert "- This is a packaging draft, not a published package archive descriptor.\n")))

;;;###autoload
(defun nemacs-library-package-descriptors-batch ()
  "Write library package descriptor draft artifacts."
  (let ((rows (nemacs-library-package-descriptors--descriptor-rows)))
    (unless rows
      (error "empty nelisp-emacs library package descriptor set"))
    (nemacs-library-package-descriptors--write-tsv
     rows nemacs-library-package-descriptors-output)
    (nemacs-library-package-descriptors--write-summary
     rows nemacs-library-package-descriptors-summary-output)
    (princ
     (format
      "nemacs-library-package-descriptors: packages=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-descriptors-output
      nemacs-library-package-descriptors-summary-output))))

(provide 'nemacs-library-package-descriptors)

;;; nemacs-library-package-descriptors.el ends here
