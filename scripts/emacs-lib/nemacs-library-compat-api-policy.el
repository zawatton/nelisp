;;; nemacs-library-compat-api-policy.el --- stable API compatibility policy -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a policy artifact for the stable package API surface exposed by
;; the `nelisp-emacs' facade.  The public API inventory classifies the broader
;; source tree as public-prefixed, compat-global, private-helper, or adapter.
;; This gate narrows that classification to the versioned stable eager and
;; stable lazy API manifests that external consumers can query.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-emacs)
(require 'nemacs-public-api-inventory)

(defvar nemacs-library-compat-api-policy-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-compat-api-policy-output
  (expand-file-name "build/nemacs-library-compat-api-policy.tsv"
                    nemacs-library-compat-api-policy-repo-root)
  "TSV output path.")

(defvar nemacs-library-compat-api-policy-summary-output
  (expand-file-name "build/nemacs-library-compat-api-policy.org"
                    nemacs-library-compat-api-policy-repo-root)
  "Org summary output path.")

(defun nemacs-library-compat-api-policy--symbol-name (value)
  "Return VALUE as a stable symbol name string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-compat-api-policy--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-compat-api-policy--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-compat-api-policy--tsv-cell cells "\t"))

(defun nemacs-library-compat-api-policy--stable-symbol-entry (entry)
  "Return (KIND SYMBOL) from stable manifest symbol ENTRY."
  (cond
   ((and (consp entry) (symbolp (car entry)) (symbolp (cadr entry)))
    (list (car entry) (cadr entry)))
   ((symbolp entry)
    (list 'symbol entry))
   (t
    (list 'unknown entry))))

(defun nemacs-library-compat-api-policy--inventory-table ()
  "Return public API inventory rows keyed by symbol string."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row (nemacs-public-api-inventory--rows) table)
      (let ((symbol (nth 6 row)))
        (unless (gethash symbol table)
          (puthash symbol row table))))))

(defun nemacs-library-compat-api-policy--policy-for-class (class)
  "Return consumer policy for API CLASS."
  (cond
   ((string= class "public-prefixed")
    "preferred-stable-library-api")
   ((string= class "compat-global")
    "stable-emacs-compatibility-global")
   ((string= class "missing-inventory")
    "stable-symbol-must-exist-in-public-api-inventory")
   ((string= class "private-helper")
    "private-helper-not-stable-api")
   ((string= class "adapter-or-app")
    "app-adapter-not-stable-library-api")
   (t "unknown-stable-api-class")))

(defun nemacs-library-compat-api-policy--status-for-class (class)
  "Return status for stable API CLASS."
  (if (member class '("public-prefixed" "compat-global"))
      "ok"
    "fail"))

(defun nemacs-library-compat-api-policy--details-for-class (class role)
  "Return details for stable API CLASS and ROLE."
  (cond
   ((string= class "public-prefixed")
    (format "%s stable API uses a project/library prefix" role))
   ((string= class "compat-global")
    (format "%s stable API intentionally exposes an Emacs-compatible global name"
            role))
   ((string= class "missing-inventory")
    "stable API symbol was not found in generated public API inventory")
   ((string= class "private-helper")
    "private helper names must not be promoted to stable consumer API")
   ((string= class "adapter-or-app")
    "app/frontend adapter names must not be promoted to reusable stable API")
   (t "unknown stable API class")))

(defun nemacs-library-compat-api-policy--rows-for-manifest
    (role manifest inventory)
  "Return policy rows for stable API ROLE MANIFEST using INVENTORY."
  (let (rows)
    (dolist (entry manifest rows)
      (let* ((package (car entry))
             (plist (cdr entry))
             (package-id (plist-get plist :package-id))
             (feature (or (plist-get plist :feature) ""))
             (symbols (plist-get plist :symbols)))
        (dolist (symbol-entry symbols)
          (pcase-let* ((`(,kind ,symbol)
                        (nemacs-library-compat-api-policy--stable-symbol-entry
                         symbol-entry))
                       (symbol-name
                        (nemacs-library-compat-api-policy--symbol-name
                         symbol))
                       (inventory-row (gethash symbol-name inventory))
                       (class (or (nth 2 inventory-row)
                                  "missing-inventory"))
                       (source-file (or (nth 4 inventory-row) ""))
                       (line (or (nth 5 inventory-row) "")))
            (push
             (list role
                   package-id
                   package
                   feature
                   kind
                   symbol-name
                   class
                   (nemacs-library-compat-api-policy--policy-for-class
                    class)
                   (nemacs-library-compat-api-policy--status-for-class
                    class)
                   source-file
                   line
                   (nemacs-library-compat-api-policy--details-for-class
                    class role))
             rows)))))))

(defun nemacs-library-compat-api-policy--rows ()
  "Return stable API compatibility policy rows."
  (let ((inventory (nemacs-library-compat-api-policy--inventory-table)))
    (sort
     (append
      (nemacs-library-compat-api-policy--rows-for-manifest
       "stable-eager"
       (nelisp-emacs-library-stable-api-manifest)
       inventory)
      (nemacs-library-compat-api-policy--rows-for-manifest
       "stable-lazy"
       (nelisp-emacs-library-stable-lazy-api-manifest)
       inventory))
     (lambda (a b)
       (string< (mapconcat (lambda (cell) (format "%s" cell)) a "\t")
                (mapconcat (lambda (cell) (format "%s" cell)) b "\t"))))))

(defun nemacs-library-compat-api-policy--write-tsv (rows)
  "Write policy ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-compat-api-policy-output) t)
  (with-temp-file nemacs-library-compat-api-policy-output
    (insert
     (nemacs-library-compat-api-policy--row
      "role" "package_id" "package" "feature" "kind" "symbol"
      "class" "consumer_policy" "status" "source_file" "line" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-compat-api-policy--row row)
       "\n"))))

(defun nemacs-library-compat-api-policy--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-library-compat-api-policy--sorted-counts (table)
  "Return TABLE counts sorted by key."
  (let (items)
    (maphash (lambda (key value) (push (cons key value) items)) table)
    (sort items (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-compat-api-policy--write-summary (rows)
  "Write policy ROWS to Org summary output."
  (let ((by-class (make-hash-table :test 'equal))
        (by-role-class (make-hash-table :test 'equal))
        (failures 0))
    (dolist (row rows)
      (nemacs-library-compat-api-policy--inc by-class (nth 6 row))
      (nemacs-library-compat-api-policy--inc
       by-role-class (format "%s/%s" (nth 0 row) (nth 6 row)))
      (when (string= (nth 8 row) "fail")
        (setq failures (1+ failures))))
    (make-directory
     (file-name-directory
      nemacs-library-compat-api-policy-summary-output)
     t)
    (with-temp-file nemacs-library-compat-api-policy-summary-output
      (insert "#+TITLE: nemacs library compatibility API policy\n\n")
      (insert "* Summary\n\n")
      (insert (format "- stable API rows: %d\n" (length rows)))
      (insert (format "- failures: %d\n\n" failures))
      (insert "* Counts by class\n\n")
      (insert "| Class | Count |\n|-------+-------|\n")
      (dolist (item (nemacs-library-compat-api-policy--sorted-counts
                     by-class))
        (insert (format "| =%s= | %d |\n" (car item) (cdr item))))
      (insert "\n* Counts by role/class\n\n")
      (insert "| Role/Class | Count |\n|------------+-------|\n")
      (dolist (item (nemacs-library-compat-api-policy--sorted-counts
                     by-role-class))
        (insert (format "| =%s= | %d |\n" (car item) (cdr item))))
      (insert "\n* Consumer Policy\n\n")
      (insert "- =public-prefixed= stable symbols are the preferred reusable library surface.\n")
      (insert "- =compat-global= stable symbols are intentional Emacs-compatible global shims.\n")
      (insert "- Stable lazy symbols require the entry's lazy feature before use.\n")
      (insert "- =private-helper=, =adapter-or-app=, and inventory-missing symbols fail this gate.\n\n")
      (insert "* Notes\n\n")
      (insert "- This gate is generated from the facade stable API query functions and the public API inventory.\n")
      (insert "- It does not promote advisory inventory rows; only symbols already in the stable manifests are covered.\n"))))

;;;###autoload
(defun nemacs-library-compat-api-policy-batch ()
  "Generate and verify stable API compatibility policy artifacts."
  (let* ((rows (nemacs-library-compat-api-policy--rows))
         (failures 0))
    (unless rows
      (error "empty stable API compatibility policy rows"))
    (nemacs-library-compat-api-policy--write-tsv rows)
    (nemacs-library-compat-api-policy--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 8 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-compat-api-policy: rows=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-compat-api-policy-output
      nemacs-library-compat-api-policy-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-compat-api-policy)

;;; nemacs-library-compat-api-policy.el ends here
