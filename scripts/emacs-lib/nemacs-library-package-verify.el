;;; nemacs-library-package-verify.el --- verify package extraction artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that generated package descriptor and physical layout drafts are
;; internally consistent enough to be used as package extraction inputs.
;; The consumer package guide and package-scoped API inventory are also
;; checked so generated external-facing artifacts cannot drift from the
;; descriptor source of truth.  The consumer API catalog is checked against
;; both of those generated inputs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nemacs-library-api-promotion-queue)
(require 'nemacs-library-package-api)
(require 'nemacs-library-package-catalog)
(require 'nemacs-library-package-descriptors)
(require 'nemacs-library-package-guide)
(require 'nemacs-library-package-layout)

(defvar nemacs-library-package-verify-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-verify-output
  (expand-file-name "build/nemacs-library-package-verify.tsv"
                    nemacs-library-package-verify-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-verify-summary-output
  (expand-file-name "build/nemacs-library-package-verify.org"
                    nemacs-library-package-verify-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-verify--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-verify--join (values)
  "Return VALUES as a stable comma-separated string."
  (mapconcat #'identity
             (sort (delete-dups
                    (mapcar #'nemacs-library-package-verify--symbol-name
                            (copy-sequence values)))
                   #'string<)
             ","))

(defun nemacs-library-package-verify--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-verify--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-verify--tsv-cell cells "\t"))

(defun nemacs-library-package-verify--relative-exists-p (relative)
  "Return non-nil if RELATIVE exists under repository root."
  (file-exists-p
   (expand-file-name relative nemacs-library-package-verify-repo-root)))

(defun nemacs-library-package-verify--duplicates (values)
  "Return duplicate VALUES using `equal'."
  (let (seen duplicates)
    (dolist (value values)
      (if (member value seen)
          (cl-pushnew value duplicates :test #'equal)
        (push value seen)))
    (nreverse duplicates)))

(defun nemacs-library-package-verify--layout-key (row)
  "Return descriptor/layout comparison key for ROW."
  (list (nth 0 row) (nth 4 row) (nth 5 row)))

(defun nemacs-library-package-verify--descriptor-layout-keys (descriptors)
  "Return expected layout keys from DESCRIPTORS."
  (let (keys)
    (dolist (descriptor descriptors)
      (let ((package-id (nth 1 descriptor)))
        (dolist (source (nth 5 descriptor))
          (push (list package-id "eager" source) keys))
        (dolist (source (nth 8 descriptor))
          (push (list package-id "lazy" source) keys))))
    (sort keys
          (lambda (a b)
            (string<
             (mapconcat #'nemacs-library-package-verify--symbol-name a "\t")
             (mapconcat #'nemacs-library-package-verify--symbol-name b "\t"))))))

(defun nemacs-library-package-verify--missing (expected actual)
  "Return EXPECTED entries not present in ACTUAL."
  (let (missing)
    (dolist (entry expected)
      (unless (member entry actual)
        (push entry missing)))
    (nreverse missing)))

(defun nemacs-library-package-verify--target-prefix-p (row)
  "Return non-nil if layout ROW target path matches package/role prefix."
  (let* ((package-id (nth 0 row))
         (role (nth 4 row))
         (target (nth 6 row))
         (dir (if (equal role "lazy") "lazy" "lisp")))
    (string-prefix-p
     (format "packages/%s/%s/" package-id dir)
     target)))

(defun nemacs-library-package-verify--guide-row (descriptor)
  "Return expected guide comparison row from DESCRIPTOR."
  (let ((requires
         (mapcar #'nemacs-library-package-descriptors--package-id
                 (nth 6 descriptor))))
    (list (nth 1 descriptor)
          (nth 0 descriptor)
          (nth 2 descriptor)
          (nth 3 descriptor)
          (nemacs-library-package-verify--join requires)
          (nemacs-library-package-verify--join (nth 4 descriptor))
          (nemacs-library-package-verify--join (nth 7 descriptor))
          (nemacs-library-package-verify--join (nth 9 descriptor))
          (nemacs-library-package-verify--join (nth 10 descriptor))
          (length (nth 5 descriptor))
          (length (nth 8 descriptor)))))

(defun nemacs-library-package-verify--guide-comparison-row (row)
  "Return comparable guide ROW using the guide tool's row shape."
  (list (nth 0 row)
        (nth 1 row)
        (nth 2 row)
        (nth 3 row)
        (nemacs-library-package-verify--join (nth 4 row))
        (nemacs-library-package-verify--join (nth 5 row))
        (nemacs-library-package-verify--join (nth 6 row))
        (nemacs-library-package-verify--join (nth 7 row))
        (nemacs-library-package-verify--join (nth 8 row))
        (nth 9 row)
        (nth 10 row)))

(defun nemacs-library-package-verify--package-api-row-valid-p
    (package-ids row)
  "Return non-nil if ROW belongs to PACKAGE-IDS and is not adapter API."
  (and (member (nth 0 row) package-ids)
       (not (equal (nth 7 row) "adapter-or-app"))))

(defun nemacs-library-package-verify--unique-strings (values)
  "Return sorted unique string VALUES."
  (sort (delete-dups (copy-sequence values)) #'string<))

(defun nemacs-library-package-verify--descriptor-api-files
    (descriptors api-rows)
  "Return descriptor files that have public API rows.
DESCRIPTORS supplies package source files.  API-ROWS comes from the broader
public API inventory."
  (let ((descriptor-files (make-hash-table :test 'equal))
        files)
    (dolist (descriptor descriptors)
      (dolist (file (append (nth 5 descriptor) (nth 8 descriptor)))
        (puthash file t descriptor-files)))
    (dolist (row api-rows)
      (let ((file (nth 4 row)))
        (when (gethash file descriptor-files)
          (push file files))))
    (nemacs-library-package-verify--unique-strings files)))

(defun nemacs-library-package-verify--package-api-files (package-api)
  "Return source files represented by PACKAGE-API rows."
  (nemacs-library-package-verify--unique-strings
   (mapcar (lambda (row) (nth 9 row)) package-api)))

(defun nemacs-library-package-verify--package-api-package-ids
    (package-api)
  "Return package ids represented by PACKAGE-API rows."
  (sort (delete-dups (mapcar (lambda (row) (nth 0 row)) package-api))
        (lambda (a b)
          (string< (symbol-name a) (symbol-name b)))))

(defun nemacs-library-package-verify--catalog-guide-row (guide-row)
  "Return expected catalog guide fields from GUIDE-ROW."
  (list (nth 0 guide-row)
        (nth 1 guide-row)
        (nth 2 guide-row)
        (nth 3 guide-row)
        (nemacs-library-package-verify--join (nth 4 guide-row))
        (nemacs-library-package-verify--join (nth 5 guide-row))
        (nemacs-library-package-verify--join (nth 6 guide-row))
        (nemacs-library-package-verify--join (nth 7 guide-row))
        (nemacs-library-package-verify--join (nth 8 guide-row))))

(defun nemacs-library-package-verify--catalog-guide-comparison-row
    (catalog-row)
  "Return comparable guide fields from CATALOG-ROW."
  (list (nth 0 catalog-row)
        (nth 1 catalog-row)
        (nth 2 catalog-row)
        (nth 3 catalog-row)
        (nemacs-library-package-verify--join (nth 4 catalog-row))
        (nemacs-library-package-verify--join (nth 5 catalog-row))
        (nemacs-library-package-verify--join (nth 6 catalog-row))
        (nemacs-library-package-verify--join (nth 7 catalog-row))
        (nemacs-library-package-verify--join (nth 8 catalog-row))))

(defun nemacs-library-package-verify--package-api-class-counts
    (package-api class)
  "Return unique symbol count rows for PACKAGE-API rows of CLASS."
  (let ((table (make-hash-table :test 'equal))
        rows)
    (dolist (row package-api)
      (when (equal (nth 7 row) class)
        (puthash (nth 0 row)
                 (cons (nth 11 row) (gethash (nth 0 row) table))
                 table)))
    (maphash (lambda (package-id symbols)
               (push
                (list package-id
                      (length
                       (sort (delete-dups (copy-sequence symbols))
                             #'string<)))
                rows))
             table)
    (sort rows
          (lambda (a b)
            (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun nemacs-library-package-verify--catalog-class-counts
    (catalog column)
  "Return count rows from CATALOG using count COLUMN."
  (let (rows)
    (dolist (row catalog)
      (when (/= (nth column row) 0)
        (push (list (nth 0 row) (nth column row)) rows)))
    (sort rows
          (lambda (a b)
            (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun nemacs-library-package-verify--package-api-symbols
    (package-api class)
  "Return symbol-list rows for PACKAGE-API rows of CLASS."
  (let ((table (make-hash-table :test 'equal))
        rows)
    (dolist (row package-api)
      (when (equal (nth 7 row) class)
        (puthash (nth 0 row)
                 (cons (nth 11 row) (gethash (nth 0 row) table))
                 table)))
    (maphash (lambda (package-id symbols)
               (push
                (list package-id
                      (nemacs-library-package-verify--join symbols))
                rows))
             table)
    (sort rows
          (lambda (a b)
            (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun nemacs-library-package-verify--catalog-symbols
    (catalog column)
  "Return symbol-list rows from CATALOG using symbol-list COLUMN."
  (let (rows)
    (dolist (row catalog)
      (when (nth column row)
        (push
         (list (nth 0 row)
               (nemacs-library-package-verify--join (nth column row)))
         rows)))
    (sort rows
          (lambda (a b)
            (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun nemacs-library-package-verify--package-api-promotion-key (row)
  "Return promotion comparison key for package API ROW."
  (list (nth 0 row) (nth 7 row) (nth 9 row) (nth 10 row) (nth 11 row)))

(defun nemacs-library-package-verify--promotion-row-key (row)
  "Return promotion comparison key for promotion ROW."
  (list (nth 0 row) (nth 5 row) (nth 7 row) (nth 8 row) (nth 9 row)))

(defun nemacs-library-package-verify--promotion-candidate-row-p (row)
  "Return non-nil if package API ROW should appear in promotion queue."
  (member (nth 7 row) '("public-prefixed" "compat-global")))

(defun nemacs-library-package-verify--promotion-status-valid-p (row)
  "Return non-nil if promotion ROW has a known status."
  (member (nth 10 row)
          '("stable-contract" "promote-ready" "loader-manifest"
            "needs-doc" "needs-test" "needs-review"
            "compat-shim-covered" "compat-shim-review"
            "compat-shim-helper-covered" "compat-shim-helper-internal"
            "compat-shim-helper-review"
            "validation-helper-covered" "validation-helper-internal"
            "validation-helper-review")))

(defun nemacs-library-package-verify--stable-api-keys ()
  "Return stable API comparison keys from the facade manifest."
  (let (keys)
    (dolist (entry (nelisp-emacs-library-stable-api-manifest))
      (let ((package-id (plist-get (cdr entry) :package-id)))
        (dolist (spec (plist-get (cdr entry) :symbols))
          (push (list package-id (nth 1 spec)) keys))))
    (sort keys
          (lambda (a b)
            (string<
             (mapconcat #'nemacs-library-package-verify--symbol-name a "\t")
             (mapconcat #'nemacs-library-package-verify--symbol-name b "\t"))))))

(defun nemacs-library-package-verify--stable-lazy-api-keys ()
  "Return stable lazy API comparison keys from the facade manifest."
  (let (keys)
    (dolist (entry (nelisp-emacs-library-stable-lazy-api-manifest))
      (let ((package-id (plist-get (cdr entry) :package-id)))
        (dolist (spec (plist-get (cdr entry) :symbols))
          (push (list package-id (nth 1 spec)) keys))))
    (sort keys
          (lambda (a b)
            (string<
             (mapconcat #'nemacs-library-package-verify--symbol-name a "\t")
             (mapconcat #'nemacs-library-package-verify--symbol-name b "\t"))))))

(defun nemacs-library-package-verify--promotion-stable-keys (promotion)
  "Return stable-contract comparison keys from PROMOTION rows."
  (let (keys)
    (dolist (row promotion)
      (when (equal (nth 10 row) "stable-contract")
        (push (list (nth 0 row) (intern (nth 9 row))) keys)))
    (sort keys
          (lambda (a b)
            (string<
             (mapconcat #'nemacs-library-package-verify--symbol-name a "\t")
             (mapconcat #'nemacs-library-package-verify--symbol-name b "\t"))))))

(defun nemacs-library-package-verify--package-api-symbol-keys (package-api)
  "Return package/symbol keys represented by PACKAGE-API."
  (let (keys)
    (dolist (row package-api)
      (push (list (nth 0 row) (intern (nth 11 row))) keys))
    (sort (delete-dups keys)
          (lambda (a b)
            (string<
             (mapconcat #'nemacs-library-package-verify--symbol-name a "\t")
             (mapconcat #'nemacs-library-package-verify--symbol-name b "\t"))))))

(defun nemacs-library-package-verify--stable-api-symbols-resolve-p ()
  "Return non-nil if all stable API symbols resolve by kind."
  (cl-every
   (lambda (entry)
     (cl-every
      (lambda (spec)
        (pcase (nth 0 spec)
          ('function (fboundp (nth 1 spec)))
          ('macro (macrop (symbol-function (nth 1 spec))))
          ('variable (boundp (nth 1 spec)))
          ('feature (featurep (nth 1 spec)))
          (_ nil)))
      (plist-get (cdr entry) :symbols)))
   (nelisp-emacs-library-stable-api-manifest)))

(defun nemacs-library-package-verify--stable-lazy-api-symbols-resolve-p ()
  "Return non-nil if all stable lazy API symbols resolve after lazy require."
  (cl-every
   (lambda (entry)
     (let ((feature (plist-get (cdr entry) :feature)))
       (and feature
            (require feature nil t)
            (cl-every
             (lambda (spec)
               (pcase (nth 0 spec)
                 ('function (fboundp (nth 1 spec)))
                 ('macro (macrop (symbol-function (nth 1 spec))))
                 ('variable (boundp (nth 1 spec)))
                 ('feature (featurep (nth 1 spec)))
                 (_ nil)))
             (plist-get (cdr entry) :symbols)))))
   (nelisp-emacs-library-stable-lazy-api-manifest)))

(defun nemacs-library-package-verify--status-row
    (check subject status details)
  "Return one verification row for CHECK/SUBJECT/STATUS/DETAILS."
  (list check subject status details))

(defmacro nemacs-library-package-verify--add
    (rows check subject ok details)
  "Push one verification row onto ROWS."
  `(push (nemacs-library-package-verify--status-row
          ,check ,subject (if ,ok "ok" "fail") ,details)
         ,rows))

(defun nemacs-library-package-verify--rows ()
  "Return package verification rows."
  (let* ((descriptors (nemacs-library-package-descriptors--descriptor-rows))
         (package-api (nemacs-library-package-api--rows))
         (catalog (nemacs-library-package-catalog--rows))
         (promotion (nemacs-library-api-promotion-queue--rows))
         (public-api (nemacs-public-api-inventory--rows))
         (layout (nemacs-library-package-layout--rows))
         (guide (nemacs-library-package-guide--rows))
         (package-names (mapcar #'car descriptors))
         (package-ids (mapcar #'cadr descriptors))
         (expected-api-files
          (nemacs-library-package-verify--descriptor-api-files
           descriptors public-api))
         (actual-api-files
          (nemacs-library-package-verify--package-api-files package-api))
         (actual-api-package-ids
          (nemacs-library-package-verify--package-api-package-ids package-api))
         (loader-features (mapcar (lambda (row) (nth 3 row)) descriptors))
         (source-files (apply #'append (mapcar (lambda (row) (nth 5 row))
                                               descriptors)))
         (lazy-source-files (apply #'append (mapcar (lambda (row) (nth 8 row))
                                                    descriptors)))
         (package-requires (apply #'append (mapcar (lambda (row) (nth 6 row))
                                                   descriptors)))
         (unknown-externals (apply #'append (mapcar (lambda (row) (nth 11 row))
                                                    descriptors)))
         (expected-keys
          (nemacs-library-package-verify--descriptor-layout-keys descriptors))
         (actual-keys (sort (mapcar #'nemacs-library-package-verify--layout-key
                                    layout)
                            (lambda (a b)
                              (string<
                               (mapconcat
                               #'nemacs-library-package-verify--symbol-name
                                a "\t")
                               (mapconcat
                                #'nemacs-library-package-verify--symbol-name
                                b "\t")))))
         (expected-guide
          (sort (mapcar #'nemacs-library-package-verify--guide-row
                        descriptors)
                (lambda (a b)
                  (string<
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name a "\t")
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name b "\t")))))
         (actual-guide
          (sort (mapcar #'nemacs-library-package-verify--guide-comparison-row
                        guide)
                (lambda (a b)
                  (string<
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name a "\t")
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name b "\t")))))
         (expected-catalog-guide
          (sort (mapcar #'nemacs-library-package-verify--catalog-guide-row
                        guide)
                (lambda (a b)
                  (string<
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name a "\t")
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name b "\t")))))
         (actual-catalog-guide
          (sort (mapcar
                 #'nemacs-library-package-verify--catalog-guide-comparison-row
                 catalog)
                (lambda (a b)
                  (string<
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name a "\t")
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name b "\t")))))
         (expected-promotion-keys
          (sort (mapcar
                 #'nemacs-library-package-verify--package-api-promotion-key
                 (cl-remove-if-not
                  #'nemacs-library-package-verify--promotion-candidate-row-p
                  package-api))
                (lambda (a b)
                  (string<
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name a "\t")
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name b "\t")))))
         (actual-promotion-keys
          (sort (mapcar
                 #'nemacs-library-package-verify--promotion-row-key
                 promotion)
                (lambda (a b)
                  (string<
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name a "\t")
                   (mapconcat
                    #'nemacs-library-package-verify--symbol-name b "\t")))))
         (stable-api-keys
          (nemacs-library-package-verify--stable-api-keys))
         (stable-lazy-api-keys
          (nemacs-library-package-verify--stable-lazy-api-keys))
         (promotion-stable-keys
          (nemacs-library-package-verify--promotion-stable-keys promotion))
         (package-api-symbol-keys
          (nemacs-library-package-verify--package-api-symbol-keys
           package-api))
         (rows nil))
    (nemacs-library-package-verify--add rows "package-count" "descriptors" descriptors
           (format "packages=%d" (length descriptors)))
      (let ((duplicates (nemacs-library-package-verify--duplicates
                         package-ids)))
        (nemacs-library-package-verify--add rows "unique-package-ids" "descriptors" (null duplicates)
             (nemacs-library-package-verify--join duplicates)))
      (let ((duplicates (nemacs-library-package-verify--duplicates
                         loader-features)))
        (nemacs-library-package-verify--add rows "unique-loader-features" "descriptors" (null duplicates)
             (nemacs-library-package-verify--join duplicates)))
      (let ((missing
             (cl-remove-if
              #'nemacs-library-package-verify--relative-exists-p
              source-files)))
        (nemacs-library-package-verify--add rows "source-files-exist" "descriptors" (null missing)
             (nemacs-library-package-verify--join missing)))
      (let ((missing
             (cl-remove-if
              #'nemacs-library-package-verify--relative-exists-p
              lazy-source-files)))
        (nemacs-library-package-verify--add rows "lazy-source-files-exist" "descriptors" (null missing)
             (nemacs-library-package-verify--join missing)))
      (let ((missing (cl-remove-if
                      (lambda (package) (member package package-names))
                      package-requires)))
        (nemacs-library-package-verify--add rows "package-requires-resolve" "descriptors" (null missing)
             (nemacs-library-package-verify--join missing)))
      (dolist (descriptor descriptors)
        (let ((lazy-count (length (nth 7 descriptor)))
              (lazy-file-count (length (nth 8 descriptor))))
          (nemacs-library-package-verify--add rows "lazy-feature-file-count"
               (nemacs-library-package-verify--symbol-name
                (nth 1 descriptor))
               (= lazy-count lazy-file-count)
               (format "lazy-features=%d lazy-source-files=%d"
                       lazy-count lazy-file-count))))
      (nemacs-library-package-verify--add rows "unknown-external-features" "descriptors"
           (null unknown-externals)
           (nemacs-library-package-verify--join unknown-externals))
      (let ((missing (nemacs-library-package-verify--missing
                      expected-keys actual-keys)))
        (nemacs-library-package-verify--add rows "layout-covers-descriptors" "layout" (null missing)
             (mapconcat
              (lambda (key)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           key ":"))
              missing ",")))
      (let ((extra (nemacs-library-package-verify--missing
                    actual-keys expected-keys)))
        (nemacs-library-package-verify--add rows "layout-has-no-extra-sources" "layout" (null extra)
             (mapconcat
              (lambda (key)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           key ":"))
              extra ",")))
      (let ((duplicates (nemacs-library-package-verify--duplicates
                         (mapcar (lambda (row) (nth 6 row)) layout))))
        (nemacs-library-package-verify--add rows "layout-targets-unique" "layout" (null duplicates)
             (nemacs-library-package-verify--join duplicates)))
      (let ((bad-targets
             (cl-remove-if #'nemacs-library-package-verify--target-prefix-p
                           layout)))
        (nemacs-library-package-verify--add rows "layout-target-prefixes" "layout" (null bad-targets)
             (nemacs-library-package-verify--join
              (mapcar (lambda (row) (nth 6 row)) bad-targets))))
      (nemacs-library-package-verify--add rows "guide-count" "guide" (= (length guide) (length descriptors))
           (format "guide=%d descriptors=%d"
                   (length guide)
                   (length descriptors)))
      (let ((missing (nemacs-library-package-verify--missing
                      expected-guide actual-guide)))
        (nemacs-library-package-verify--add rows "guide-covers-descriptors" "guide" (null missing)
             (mapconcat
              (lambda (row)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           row ":"))
              missing ",")))
      (let ((extra (nemacs-library-package-verify--missing
                    actual-guide expected-guide)))
        (nemacs-library-package-verify--add rows "guide-has-no-extra-rows" "guide" (null extra)
             (mapconcat
              (lambda (row)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           row ":"))
              extra ",")))
      (nemacs-library-package-verify--add rows "package-api-count" "package-api" package-api
           (format "rows=%d" (length package-api)))
      (let ((missing (nemacs-library-package-verify--missing
                      package-ids actual-api-package-ids)))
        (nemacs-library-package-verify--add rows "package-api-covers-packages" "package-api" (null missing)
             (nemacs-library-package-verify--join missing)))
      (let ((missing (nemacs-library-package-verify--missing
                      expected-api-files actual-api-files)))
        (nemacs-library-package-verify--add rows "package-api-covers-api-files" "package-api" (null missing)
             (nemacs-library-package-verify--join missing)))
      (let ((extra (nemacs-library-package-verify--missing
                    actual-api-files expected-api-files)))
        (nemacs-library-package-verify--add rows "package-api-has-no-extra-files" "package-api" (null extra)
             (nemacs-library-package-verify--join extra)))
      (let ((invalid
             (cl-remove-if
              (lambda (row)
                (nemacs-library-package-verify--package-api-row-valid-p
                 package-ids row))
              package-api)))
        (nemacs-library-package-verify--add rows "package-api-scoped-to-packages" "package-api" (null invalid)
             (mapconcat
              (lambda (row)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           (list (nth 0 row) (nth 7 row) (nth 11 row))
                           ":"))
              invalid ",")))
      (nemacs-library-package-verify--add rows "catalog-count" "catalog" (= (length catalog) (length guide))
           (format "catalog=%d guide=%d"
                   (length catalog)
                   (length guide)))
      (let ((missing (nemacs-library-package-verify--missing
                      expected-catalog-guide actual-catalog-guide)))
        (nemacs-library-package-verify--add rows "catalog-covers-guide" "catalog" (null missing)
             (mapconcat
              (lambda (row)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           row ":"))
              missing ",")))
      (let ((extra (nemacs-library-package-verify--missing
                    actual-catalog-guide expected-catalog-guide)))
        (nemacs-library-package-verify--add rows "catalog-has-no-extra-guide-rows" "catalog" (null extra)
             (mapconcat
              (lambda (row)
                (mapconcat #'nemacs-library-package-verify--symbol-name
                           row ":"))
              extra ",")))
      (let ((expected (nemacs-library-package-verify--package-api-class-counts
                       package-api "public-prefixed"))
            (actual (nemacs-library-package-verify--catalog-class-counts
                     catalog 9)))
        (nemacs-library-package-verify--add rows "catalog-public-counts-match" "catalog"
             (and (null (nemacs-library-package-verify--missing
                         expected actual))
                  (null (nemacs-library-package-verify--missing
                         actual expected)))
             (format "expected=%s actual=%s"
                     (nemacs-library-package-verify--join
                      (mapcar (lambda (row)
                                (format "%s:%s" (car row) (cadr row)))
                              expected))
                     (nemacs-library-package-verify--join
                      (mapcar (lambda (row)
                                (format "%s:%s" (car row) (cadr row)))
                              actual)))))
      (let ((expected (nemacs-library-package-verify--package-api-class-counts
                       package-api "compat-global"))
            (actual (nemacs-library-package-verify--catalog-class-counts
                     catalog 10)))
        (nemacs-library-package-verify--add rows "catalog-compat-counts-match" "catalog"
             (and (null (nemacs-library-package-verify--missing
                         expected actual))
                  (null (nemacs-library-package-verify--missing
                         actual expected)))
             (format "expected=%s actual=%s"
                     (nemacs-library-package-verify--join
                      (mapcar (lambda (row)
                                (format "%s:%s" (car row) (cadr row)))
                              expected))
                     (nemacs-library-package-verify--join
                      (mapcar (lambda (row)
                                (format "%s:%s" (car row) (cadr row)))
                              actual)))))
      (let ((expected (nemacs-library-package-verify--package-api-class-counts
                       package-api "private-helper"))
            (actual (nemacs-library-package-verify--catalog-class-counts
                     catalog 11)))
        (nemacs-library-package-verify--add rows "catalog-private-counts-match" "catalog"
             (and (null (nemacs-library-package-verify--missing
                         expected actual))
                  (null (nemacs-library-package-verify--missing
                         actual expected)))
             (format "expected=%s actual=%s"
                     (nemacs-library-package-verify--join
                      (mapcar (lambda (row)
                                (format "%s:%s" (car row) (cadr row)))
                              expected))
                     (nemacs-library-package-verify--join
                      (mapcar (lambda (row)
                                (format "%s:%s" (car row) (cadr row)))
                              actual)))))
      (let ((expected (nemacs-library-package-verify--package-api-symbols
                       package-api "public-prefixed"))
            (actual (nemacs-library-package-verify--catalog-symbols
                     catalog 12)))
        (nemacs-library-package-verify--add rows "catalog-public-symbols-match" "catalog"
             (and (null (nemacs-library-package-verify--missing
                         expected actual))
                  (null (nemacs-library-package-verify--missing
                         actual expected)))
             "public-prefixed symbol lists match package API"))
      (let ((expected (nemacs-library-package-verify--package-api-symbols
                       package-api "compat-global"))
            (actual (nemacs-library-package-verify--catalog-symbols
                     catalog 13)))
        (nemacs-library-package-verify--add rows "catalog-compat-symbols-match" "catalog"
             (and (null (nemacs-library-package-verify--missing
                         expected actual))
                  (null (nemacs-library-package-verify--missing
                         actual expected)))
             "compat-global symbol lists match package API"))
      (nemacs-library-package-verify--add rows
          "promotion-count" "promotion" promotion
          (format "rows=%d" (length promotion)))
      (let ((missing (nemacs-library-package-verify--missing
                      expected-promotion-keys actual-promotion-keys)))
        (nemacs-library-package-verify--add rows
            "promotion-covers-api-candidates" "promotion" (null missing)
            (mapconcat
             (lambda (row)
               (mapconcat #'nemacs-library-package-verify--symbol-name
                          row ":"))
             missing ",")))
      (let ((extra (nemacs-library-package-verify--missing
                    actual-promotion-keys expected-promotion-keys)))
        (nemacs-library-package-verify--add rows
            "promotion-has-no-extra-candidates" "promotion" (null extra)
            (mapconcat
             (lambda (row)
               (mapconcat #'nemacs-library-package-verify--symbol-name
                          row ":"))
             extra ",")))
      (let ((invalid
             (cl-remove-if
              #'nemacs-library-package-verify--promotion-status-valid-p
              promotion)))
        (nemacs-library-package-verify--add rows
            "promotion-statuses-valid" "promotion" (null invalid)
            (nemacs-library-package-verify--join
             (mapcar (lambda (row) (nth 10 row)) invalid))))
      (let ((private
             (cl-remove-if-not
              (lambda (row) (equal (nth 5 row) "private-helper"))
              promotion)))
        (nemacs-library-package-verify--add rows
            "promotion-excludes-private" "promotion" (null private)
            (nemacs-library-package-verify--join
             (mapcar (lambda (row) (nth 9 row)) private))))
      (nemacs-library-package-verify--add rows
          "stable-api-count" "stable-api" stable-api-keys
          (format "symbols=%d" (length stable-api-keys)))
      (nemacs-library-package-verify--add rows
          "stable-api-symbols-resolve" "stable-api"
          (nemacs-library-package-verify--stable-api-symbols-resolve-p)
          "stable API symbols resolve after requiring facade")
      (let ((missing (nemacs-library-package-verify--missing
                      stable-api-keys package-api-symbol-keys)))
        (nemacs-library-package-verify--add rows
            "stable-api-covered-by-package-api" "stable-api" (null missing)
            (mapconcat
             (lambda (row)
               (mapconcat #'nemacs-library-package-verify--symbol-name
                          row ":"))
             missing ",")))
      (let ((missing (nemacs-library-package-verify--missing
                      stable-api-keys promotion-stable-keys)))
        (nemacs-library-package-verify--add rows
            "stable-api-covered-by-promotion" "stable-api" (null missing)
            (mapconcat
             (lambda (row)
               (mapconcat #'nemacs-library-package-verify--symbol-name
                          row ":"))
             missing ",")))
      (nemacs-library-package-verify--add rows
          "stable-lazy-api-count" "stable-lazy-api" stable-lazy-api-keys
          (format "symbols=%d" (length stable-lazy-api-keys)))
      (nemacs-library-package-verify--add rows
          "stable-lazy-api-symbols-resolve" "stable-lazy-api"
          (nemacs-library-package-verify--stable-lazy-api-symbols-resolve-p)
          "stable lazy API symbols resolve after requiring their lazy feature")
      (let ((missing (nemacs-library-package-verify--missing
                      stable-lazy-api-keys package-api-symbol-keys)))
        (nemacs-library-package-verify--add rows
            "stable-lazy-api-covered-by-package-api" "stable-lazy-api"
            (null missing)
            (mapconcat
             (lambda (row)
               (mapconcat #'nemacs-library-package-verify--symbol-name
                          row ":"))
             missing ",")))
      (let ((missing (nemacs-library-package-verify--missing
                      stable-lazy-api-keys promotion-stable-keys)))
        (nemacs-library-package-verify--add rows
            "stable-lazy-api-covered-by-promotion" "stable-lazy-api"
            (null missing)
            (mapconcat
             (lambda (row)
               (mapconcat #'nemacs-library-package-verify--symbol-name
                          row ":"))
             missing ",")))
      (nreverse rows)))

(defun nemacs-library-package-verify--write-tsv (rows output)
  "Write verification ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-verify--row
      "check" "subject" "status" "details")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-verify--row row) "\n"))))

(defun nemacs-library-package-verify--write-summary (rows output)
  "Write verification ROWS summary to OUTPUT."
  (let ((failures (cl-remove-if-not
                   (lambda (row) (equal (nth 2 row) "fail"))
                   rows)))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library package verification\n\n")
      (insert (format "* Summary\n\n- checks: %d\n- failures: %d\n\n"
                      (length rows)
                      (length failures)))
      (insert "* Checks\n\n")
      (insert "| Check | Subject | Status | Details |\n")
      (insert "|-------+---------+--------+---------|\n")
      (dolist (row rows)
        (insert (format "| %s | %s | %s | %s |\n"
                        (nth 0 row)
                        (nth 1 row)
                        (nth 2 row)
                        (nth 3 row))))
      (insert "\n* Notes\n\n")
      (insert "- This verifies descriptor, guide, API inventory, catalog, promotion queue, and layout consistency; it does not move files.\n")
      (insert "- `unknown-external-features' must stay empty before package extraction.\n")
      (insert "- Layout rows must exactly cover descriptor eager and lazy source files.\n")
      (insert "- Guide rows must exactly cover descriptor package ids, loader features, dependencies, and consumer-facing feature lists.\n")
      (insert "- Package API rows must cover descriptor package ids and descriptor files that define API rows.\n")
      (insert "- Package API rows must not include files outside descriptor package sources or adapter/app API.\n")
      (insert "- Catalog rows must match guide package fields and package API symbol/count rows.\n")
      (insert "- Promotion queue rows must exactly cover public-prefixed and compat-global package API candidates and exclude private helpers.\n")
      (insert "- Stable API manifest rows must resolve, appear in package API, and classify as stable-contract in the promotion queue.\n")
      (insert "- Stable lazy API manifest rows must resolve after requiring their lazy feature, appear in package API, and classify as stable-contract in the promotion queue.\n"))))

;;;###autoload
(defun nemacs-library-package-verify-batch ()
  "Write library package verification artifacts and fail on errors."
  (let* ((rows (nemacs-library-package-verify--rows))
         (failures (cl-remove-if-not
                    (lambda (row) (equal (nth 2 row) "fail"))
                    rows)))
    (nemacs-library-package-verify--write-tsv
     rows nemacs-library-package-verify-output)
    (nemacs-library-package-verify--write-summary
     rows nemacs-library-package-verify-summary-output)
    (princ
     (format
      "nemacs-library-package-verify: checks=%d failures=%d output=%s summary=%s\n"
      (length rows)
      (length failures)
      nemacs-library-package-verify-output
      nemacs-library-package-verify-summary-output))
    (when failures
      (kill-emacs 1))))

(provide 'nemacs-library-package-verify)

;;; nemacs-library-package-verify.el ends here
