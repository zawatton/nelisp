;;; nemacs-library-package-deps.el --- export facade package dependencies -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a package-group dependency view from the public `nelisp-emacs'
;; facade manifest and source `require' forms.  This is review input for the
;; library-first extraction phase: package descriptors can be derived from
;; the generated edges instead of hand-copying dependency assumptions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-emacs)

(defvar nemacs-library-package-deps-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-deps-ownership-doc
  (expand-file-name "docs/design/18-library-package-ownership-inventory.org"
                    nemacs-library-package-deps-repo-root)
  "Doc 18 ownership inventory path.")

(defvar nemacs-library-package-deps-output
  (expand-file-name "build/nemacs-library-package-deps.tsv"
                    nemacs-library-package-deps-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-deps-summary-output
  (expand-file-name "build/nemacs-library-package-deps-summary.org"
                    nemacs-library-package-deps-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-deps-migration-queue-output
  (expand-file-name "build/nemacs-library-package-migration-queue.tsv"
                    nemacs-library-package-deps-repo-root)
  "TSV output path for unmanifested reusable dependency candidates.")

(defvar nemacs-library-package-deps-migration-queue-summary-output
  (expand-file-name "build/nemacs-library-package-migration-queue.org"
                    nemacs-library-package-deps-repo-root)
  "Org output path for unmanifested reusable dependency candidates.")

(defconst nemacs-library-package-deps--reusable-groups
  '("FND" "TXT" "BUF" "CORE" "IO" "DSP" "FEAT" "PKG")
  "Doc 18 ownership groups considered reusable package owners.")

(defconst nemacs-library-package-deps--external-feature-relations
  '((keymap . "host-feature")
    (pp . "host-feature")
    (nelisp-process . "vendor-package"))
  "Known non-repository dependency relation by required feature.")

(defun nemacs-library-package-deps--external-feature-relation (feature)
  "Return known external dependency relation for FEATURE, or nil."
  (cdr (assq feature nemacs-library-package-deps--external-feature-relations)))

(defun nemacs-library-package-deps--primary-group (group)
  "Return primary ownership group from GROUP."
  (car (split-string group "/" t)))

(defun nemacs-library-package-deps--relative (path)
  "Return PATH relative to repository root."
  (file-relative-name path nemacs-library-package-deps-repo-root))

(defun nemacs-library-package-deps--ownership ()
  "Return a hash table mapping repo-relative paths to primary owner group."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents nemacs-library-package-deps-ownership-doc)
      (goto-char (point-min))
      (while (re-search-forward "^| =\\([^=]+\\)= | \\([^ |]+\\)" nil t)
        (let* ((item (match-string 1))
               (group (nemacs-library-package-deps--primary-group
                       (match-string 2)))
               (relative
                (cond
                 ((string-prefix-p "gui/" item) item)
                 ((string-prefix-p "src/" item) item)
                 ((string-suffix-p ".el" item) (concat "src/" item))
                 (t item))))
          (puthash relative group table))))
    table))

(defun nemacs-library-package-deps--elisp-files ()
  "Return repository Elisp files relevant to package dependency analysis."
  (sort
   (append
    (directory-files-recursively
     (expand-file-name "src" nemacs-library-package-deps-repo-root)
     "\\.el\\'")
    (let ((gui (expand-file-name "gui" nemacs-library-package-deps-repo-root)))
      (and (file-directory-p gui)
           (directory-files-recursively gui "\\.el\\'"))))
   #'string<))

(defun nemacs-library-package-deps--file-group (ownership relative)
  "Return ownership group for RELATIVE using OWNERSHIP."
  (or (gethash relative ownership)
      (and (string-prefix-p "gui/" relative) "GUI")
      "UNOWNED"))

(defun nemacs-library-package-deps--read-forms (file)
  "Return top-level forms read from FILE."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (push form forms)))
        (end-of-file nil)))
    (nreverse forms)))

(defun nemacs-library-package-deps--quoted-symbol (form)
  "Return quoted symbol represented by FORM, or nil."
  (cond
   ((and (consp form)
         (eq (car form) 'quote)
         (symbolp (cadr form)))
    (cadr form))
   (t nil)))

(defun nemacs-library-package-deps--form-provide (form)
  "Return provided feature in FORM, or nil."
  (and (consp form)
       (eq (car form) 'provide)
       (nemacs-library-package-deps--quoted-symbol (cadr form))))

(defun nemacs-library-package-deps--require-entry (feature scope)
  "Return a dependency entry for FEATURE with SCOPE."
  (cons feature scope))

(defun nemacs-library-package-deps--collect-requires (form scope)
  "Return quoted feature dependency entries found in FORM with SCOPE."
  (let (entries)
    (cond
     ((and (consp form) (eq (car form) 'quote))
      nil)
     ((and (consp form) (eq (car form) 'require))
      (let ((feature (nemacs-library-package-deps--quoted-symbol (cadr form))))
        (when feature
          (push (nemacs-library-package-deps--require-entry feature scope)
                entries))))
     ((consp form)
      (while (consp form)
        (let ((part (car form)))
          (setq entries
                (append entries
                        (nemacs-library-package-deps--collect-requires
                         part scope))))
        (setq form (cdr form)))
      (when form
        (setq entries
              (append entries
                      (nemacs-library-package-deps--collect-requires
                       form scope))))))
    entries))

(defun nemacs-library-package-deps--provide-map (ownership)
  "Return feature -> (RELATIVE GROUP) map using OWNERSHIP."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (file (nemacs-library-package-deps--elisp-files))
      (let* ((relative (nemacs-library-package-deps--relative file))
             (group (nemacs-library-package-deps--file-group
                     ownership relative)))
        (dolist (form (nemacs-library-package-deps--read-forms file))
          (let ((feature (nemacs-library-package-deps--form-provide form)))
            (when (and feature (not (gethash feature table)))
              (puthash feature (list relative group) table))))))
    table))

(defun nemacs-library-package-deps--manifest-rows ()
  "Return facade package manifest rows."
  (mapcar
   (lambda (entry)
     (let ((name (car entry))
           (plist (cdr entry)))
       (list name
             (plist-get plist :owner)
             (plist-get plist :feature)
             (plist-get plist :features)
             (plist-get plist :lazy-features))))
   (nelisp-emacs-library-package-manifest)))

(defun nemacs-library-package-deps--feature-package-map
    (packages &optional lazy)
  "Return feature -> package NAME map for PACKAGES.
When LAZY is non-nil, map lazy companion features instead of eager package
loader and member features."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (package packages)
      (let ((name (car package))
            (feature (nth 2 package))
            (features (if lazy (nth 4 package) (nth 3 package))))
        (unless lazy
          (puthash feature name table))
        (dolist (member features)
          (puthash member name table))))
    table))

(defun nemacs-library-package-deps--package-files (package provide-map)
  "Return files relevant to PACKAGE using PROVIDE-MAP."
  (let ((features (cons (nth 2 package) (nth 3 package)))
        files)
    (dolist (feature features)
      (let ((entry (gethash feature provide-map)))
        (when entry
          (cl-pushnew (car entry) files :test #'equal))))
    (sort files #'string<)))

(defun nemacs-library-package-deps--requires-in-file (relative)
  "Return required feature entries found in RELATIVE."
  (let ((file (expand-file-name relative nemacs-library-package-deps-repo-root))
        entries)
    (dolist (form (nemacs-library-package-deps--read-forms file))
      (setq entries
            (append entries
                    (if (and (consp form) (eq (car form) 'require))
                        (nemacs-library-package-deps--collect-requires
                         form "top-level")
                      (nemacs-library-package-deps--collect-requires
                       form "lazy")))))
    (sort (delete-dups entries)
          (lambda (a b)
            (string< (format "%s\t%s" (car a) (cdr a))
                     (format "%s\t%s" (car b) (cdr b)))))))

(defun nemacs-library-package-deps--relation
    (required-feature from-package to-package to-group scope lazy-target)
  "Return dependency relation from FROM-PACKAGE to TO-PACKAGE/TO-GROUP/SCOPE.
REQUIRED-FEATURE is the feature requested by the source file.
LAZY-TARGET is non-nil when the required feature is declared as a lazy
companion in the facade package manifest."
  (cond
   ((and lazy-target (equal scope "lazy")) "lazy-manifest-package")
   ((and to-package (eq from-package to-package)) "same-package")
   (to-package "manifest-package")
   ((and (equal scope "lazy")
         (member to-group nemacs-library-package-deps--reusable-groups))
    "lazy-unmanifested-reusable")
   ((member to-group nemacs-library-package-deps--reusable-groups)
    "unmanifested-reusable")
   ((member to-group '("APP" "GUI"))
    "app-or-frontend")
   ((nemacs-library-package-deps--external-feature-relation required-feature))
   (t "external-or-host")))

(defun nemacs-library-package-deps--rows ()
  "Return package dependency rows."
  (let* ((ownership (nemacs-library-package-deps--ownership))
         (provide-map (nemacs-library-package-deps--provide-map ownership))
         (packages (nemacs-library-package-deps--manifest-rows))
         (feature-packages
         (nemacs-library-package-deps--feature-package-map packages))
         (lazy-feature-packages
          (nemacs-library-package-deps--feature-package-map packages t))
         rows seen)
    (dolist (package packages)
      (let ((from-package (car package))
            (from-owner (format "%s" (cadr package))))
        (dolist (relative (nemacs-library-package-deps--package-files
                           package provide-map))
          (dolist (required (nemacs-library-package-deps--requires-in-file
                             relative))
            (let* ((required-feature (car required))
                   (require-scope (cdr required))
                   (target (gethash required-feature provide-map))
                   (to-file (car target))
                   (to-owner (or (cadr target) "EXTERNAL"))
                   (lazy-target-package
                    (gethash required-feature lazy-feature-packages))
                   (to-package (or (gethash required-feature feature-packages)
                                   lazy-target-package))
                   (relation
                    (nemacs-library-package-deps--relation
                     required-feature from-package to-package to-owner require-scope
                     lazy-target-package))
                   (key (list from-package relative required-feature
                              require-scope)))
              (unless (member key seen)
                (push key seen)
                (push (list from-package from-owner relative required-feature
                            (or to-package "")
                            to-owner (or to-file "") relation require-scope)
                      rows)))))))
    (sort (nreverse rows)
          (lambda (a b)
            (string< (mapconcat (lambda (cell) (format "%s" cell)) a "\t")
                     (mapconcat (lambda (cell) (format "%s" cell)) b "\t"))))))

(defun nemacs-library-package-deps--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-deps--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-deps--tsv-cell cells "\t"))

(defun nemacs-library-package-deps--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-library-package-deps--sorted-counts (table)
  "Return TABLE counts sorted by key."
  (let (items)
    (maphash (lambda (k v) (push (cons k v) items)) table)
    (sort items (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-package-deps--symbol-name (value)
  "Return VALUE as a stable string, treating symbols by name."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-deps--sorted-strings (values)
  "Return sorted string representation of VALUES without duplicates."
  (sort (delete-dups (mapcar #'nemacs-library-package-deps--symbol-name
                             (copy-sequence values)))
        #'string<))

(defun nemacs-library-package-deps--join (values)
  "Return VALUES as a comma-separated stable string."
  (mapconcat #'identity
             (nemacs-library-package-deps--sorted-strings values)
             ","))

(defun nemacs-library-package-deps--migration-kind
    (incoming-packages incoming-owners target-owner)
  "Return migration queue kind for INCOMING-PACKAGES/OWNERS and TARGET-OWNER."
  (cond
   ((> (length (nemacs-library-package-deps--sorted-strings
                incoming-packages))
       1)
    "shared-utility-candidate")
   ((cl-every (lambda (owner) (equal owner target-owner)) incoming-owners)
    "same-owner-hidden-member")
   (t "cross-owner-hidden-dependency")))

(defun nemacs-library-package-deps--migration-queue (rows)
  "Return aggregated migration queue rows from package dependency ROWS."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (row rows)
      (when (equal (nth 7 row) "unmanifested-reusable")
        (let* ((feature (nth 3 row))
               (entry (or (gethash feature table)
                          (let ((new (list :feature feature
                                           :target-owner (nth 5 row)
                                           :target-file (nth 6 row)
                                           :incoming-packages nil
                                           :incoming-owners nil
                                           :incoming-files nil
                                           :edge-count 0)))
                            (puthash feature new table)
                            new))))
          (plist-put entry :incoming-packages
                     (cons (nth 0 row) (plist-get entry :incoming-packages)))
          (plist-put entry :incoming-owners
                     (cons (nth 1 row) (plist-get entry :incoming-owners)))
          (plist-put entry :incoming-files
                     (cons (nth 2 row) (plist-get entry :incoming-files)))
          (plist-put entry :edge-count
                     (1+ (plist-get entry :edge-count))))))
    (let (queue)
      (maphash
       (lambda (_feature entry)
         (let* ((incoming-packages (plist-get entry :incoming-packages))
                (incoming-owners (plist-get entry :incoming-owners))
                (target-owner (plist-get entry :target-owner))
                (kind
                 (nemacs-library-package-deps--migration-kind
                  incoming-packages incoming-owners target-owner)))
           (push
            (list (plist-get entry :feature)
                  target-owner
                  (plist-get entry :target-file)
                  (plist-get entry :edge-count)
                  (nemacs-library-package-deps--join incoming-packages)
                  (nemacs-library-package-deps--join incoming-owners)
                  (nemacs-library-package-deps--join
                   (plist-get entry :incoming-files))
                  kind)
            queue)))
       table)
      (sort queue
            (lambda (a b)
              (string<
               (mapconcat (lambda (cell) (format "%s" cell)) a "\t")
               (mapconcat (lambda (cell) (format "%s" cell)) b "\t")))))))

(defun nemacs-library-package-deps--write-tsv (rows output)
  "Write ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-deps--row
      "from_package" "from_owner" "from_file" "required_feature"
      "to_package" "to_owner" "to_file" "relation" "require_scope")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-deps--row row) "\n"))))

(defun nemacs-library-package-deps--write-summary (rows output)
  "Write ROWS summary to OUTPUT."
  (let ((by-relation (make-hash-table :test 'equal))
        (by-edge (make-hash-table :test 'equal)))
    (dolist (row rows)
      (nemacs-library-package-deps--inc by-relation (nth 7 row))
      (when (and (not (equal (nth 4 row) ""))
                 (not (equal (format "%s" (nth 0 row))
                             (format "%s" (nth 4 row)))))
        (nemacs-library-package-deps--inc
         by-edge
         (format "%s -> %s" (nth 0 row) (nth 4 row)))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library package dependency summary\n\n")
      (insert (format "* Edges\n\n- rows: %d\n\n" (length rows)))
      (insert "** By relation\n\n")
      (insert "| Relation | Count |\n|----------+-------|\n")
      (dolist (item (nemacs-library-package-deps--sorted-counts by-relation))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n** Manifest package edges\n\n")
      (insert "| Edge | Count |\n|------+-------|\n")
      (dolist (item (nemacs-library-package-deps--sorted-counts by-edge))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Notes\n\n")
      (insert "- `same-package' means the required feature belongs to the same facade package.\n")
      (insert "- `manifest-package' means the required feature belongs to another facade package.\n")
      (insert "- `lazy-manifest-package' means the required feature is declared as a lazy package companion.\n")
      (insert "- `unmanifested-reusable' means Doc 18 owns the feature but the facade manifest does not name it directly.\n")
      (insert "- `lazy-unmanifested-reusable' means a nested/lazy `require' reaches a reusable feature that should not necessarily be eager-loaded by a package loader.\n")
      (insert "- `host-feature' means the dependency is expected from host Emacs.\n")
      (insert "- `vendor-package' means the dependency is expected from a vendored package outside this facade set.\n")
      (insert "- `external-or-host' means no repository provider or known external classification was found.\n")
      (insert "- `app-or-frontend' rows are review risks for reusable packages.\n"))))

(defun nemacs-library-package-deps--write-migration-queue-tsv (queue output)
  "Write migration QUEUE to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-deps--row
      "candidate_feature" "owner" "provider_file" "edge_count"
      "incoming_packages" "incoming_owners" "incoming_files" "queue")
     "\n")
    (dolist (row queue)
      (insert (apply #'nemacs-library-package-deps--row row) "\n"))))

(defun nemacs-library-package-deps--write-migration-queue-summary
    (queue output)
  "Write migration QUEUE summary to OUTPUT."
  (let ((by-kind (make-hash-table :test 'equal)))
    (dolist (row queue)
      (nemacs-library-package-deps--inc by-kind (nth 7 row)))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library package migration queue\n\n")
      (insert
       (format
        "* Unmanifested reusable candidates\n\n- candidates: %d\n\n"
        (length queue)))
      (insert "** By queue\n\n")
      (insert "| Queue | Count |\n|-------+-------|\n")
      (dolist (item (nemacs-library-package-deps--sorted-counts by-kind))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n** Candidates\n\n")
      (insert
       "| Feature | Owner | Edges | Incoming packages | Queue |\n")
      (insert
       "|---------+-------+-------+-------------------+-------|\n")
      (dolist (row queue)
        (insert
         (format "| %s | %s | %d | %s | %s |\n"
                 (nemacs-library-package-deps--symbol-name (nth 0 row))
                 (nth 1 row)
                 (nth 3 row)
                 (nth 4 row)
                 (nth 7 row))))
      (insert "\n* Queue meanings\n\n")
      (insert "- `same-owner-hidden-member' means the dependency is within one Doc 18 owner group but not yet named in the facade package manifest.\n")
      (insert "- `cross-owner-hidden-dependency' means a facade package reaches a reusable feature owned by another group without an explicit manifest package edge.\n")
      (insert "- `shared-utility-candidate' means more than one facade package reaches the feature, so it may deserve its own package or an explicit shared package dependency.\n")
      (insert "- Declared lazy companions are reported as `lazy-manifest-package' in the dependency TSV and intentionally excluded from this eager membership queue.\n")
      (insert "- Undeclared lazy reusable dependencies remain `lazy-unmanifested-reusable' for review without forcing eager package membership.\n"))))

;;;###autoload
(defun nemacs-library-package-deps-batch ()
  "Write library package dependency artifacts."
  (let* ((rows (nemacs-library-package-deps--rows))
         (queue (nemacs-library-package-deps--migration-queue rows)))
    (nemacs-library-package-deps--write-tsv
     rows nemacs-library-package-deps-output)
    (nemacs-library-package-deps--write-summary
     rows nemacs-library-package-deps-summary-output)
    (nemacs-library-package-deps--write-migration-queue-tsv
     queue nemacs-library-package-deps-migration-queue-output)
    (nemacs-library-package-deps--write-migration-queue-summary
     queue nemacs-library-package-deps-migration-queue-summary-output)
    (princ
     (format
      "nemacs-library-package-deps: rows=%d migration-candidates=%d output=%s summary=%s migration-queue=%s\n"
      (length rows)
      (length queue)
      nemacs-library-package-deps-output
      nemacs-library-package-deps-summary-output
      nemacs-library-package-deps-migration-queue-output))))

(provide 'nemacs-library-package-deps)

;;; nemacs-library-package-deps.el ends here
