;;; nemacs-runtime-image-input-inventory.el --- runtime image input inventory -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a static inventory for the runtime-image bake/smoke input surface.
;; Runtime-image bakes should resolve reusable and app entry modules from the
;; package/app scaffold load-paths; direct src/ rows are tracked as debt.

;;; Code:

(require 'cl-lib)

(defvar nemacs-runtime-image-input-inventory-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-runtime-image-input-inventory-output
  (expand-file-name "build/nemacs-runtime-image-input-inventory.tsv"
                    nemacs-runtime-image-input-inventory-repo-root)
  "TSV output path.")

(defvar nemacs-runtime-image-input-inventory-summary-output
  (expand-file-name "build/nemacs-runtime-image-input-inventory.org"
                    nemacs-runtime-image-input-inventory-repo-root)
  "Org summary output path.")

(defvar nemacs-runtime-image-input-inventory-package-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-runtime-image-input-inventory-repo-root)
  "Reusable package scaffold index.")

(defvar nemacs-runtime-image-input-inventory-app-scaffold
  (expand-file-name "build/nemacs-library-app-scaffold.tsv"
                    nemacs-runtime-image-input-inventory-repo-root)
  "App/frontend scaffold index.")

(defconst nemacs-runtime-image-input-inventory--inputs
  '((base file runtime-process-preload "scripts/nemacs-runtime-process-preload.el")
    (base file runtime-frame-tab-preload "scripts/nemacs-runtime-frame-tab-preload.el")
    (base file runtime-image-preload "scripts/nemacs-runtime-image-preload.el")
    (base generated bootstrap "build/nemacs-bootstrap.el")
    (base load-path app-gui-lisp "packages/nelisp-emacs-app-gui/lisp")
    (base load-path buffer-core-lazy "packages/nelisp-emacs-buffer-core/lazy")
    (base load-path buffer-core-lisp "packages/nelisp-emacs-buffer-core/lisp")
    (base load-path core-lazy "packages/nelisp-emacs-core/lazy")
    (base load-path core-lisp "packages/nelisp-emacs-core/lisp")
    (base load-path editing-lisp "packages/nelisp-emacs-editing/lisp")
    (base load-path facade-lisp "packages/nelisp-emacs-facade/lisp")
    (base load-path foundation-lisp "packages/nelisp-emacs-foundation/lisp")
    (base load-path io-lazy "packages/nelisp-emacs-io/lazy")
    (base load-path io-lisp "packages/nelisp-emacs-io/lisp")
    (base load-path special-buffers-lisp "packages/nelisp-emacs-special-buffers/lisp")
    (base load-path text-core-lazy "packages/nelisp-emacs-text-core/lazy")
    (base load-path text-core-lisp "packages/nelisp-emacs-text-core/lisp")
    (base load-path textmodes-stub-lisp "packages/nelisp-emacs-textmodes-stub/lisp")
    (base load-path vendor-root "vendor/emacs-lisp")
    (base load-path vendor-elisp-root "vendor/emacs-lisp/emacs-lisp")
    (base load-path vendor-vc-root "vendor/emacs-lisp/vc")
    (base feature emacs-init "src/emacs-init.el")
    (base feature nemacs-loadup "src/nemacs-loadup.el")
    (base feature nemacs-main "src/nemacs-main.el")
    (base feature emacs-dump "src/emacs-dump.el")
    (base lazy-feature image-loader "src/image-loader.el")
    (interactive feature emacs-tui-backend "src/emacs-tui-backend.el")
    (interactive feature emacs-tui-event "src/emacs-tui-event.el")
    (interactive feature emacs-redisplay-core "src/emacs-redisplay-core.el")
    (vendor-core lazy-feature files-standalone-buffer "src/files-standalone-buffer.el")
    (vendor-core lazy-feature emacs-dired-min "src/emacs-dired-min.el")
    (vendor-core lazy-feature emacs-help "src/emacs-help.el")
    (vendor-core lazy-feature lisp-mode "src/lisp-mode.el")
    (vendor-core lazy-feature emacs-elisp-eval "src/emacs-elisp-eval.el")
    (vendor-core lazy-feature emacs-ielm "src/emacs-ielm.el")
    (vendor-core lazy-feature emacs-isearch "src/emacs-isearch.el")
    (vendor-core lazy-feature emacs-minibuffer-builtins "src/emacs-minibuffer-builtins.el")
    (vendor-core lazy-feature emacs-project "src/emacs-project.el"))
  "Known runtime-image bake and runtime-lazy feature inputs.")

(defun nemacs-runtime-image-input-inventory--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-runtime-image-input-inventory--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-runtime-image-input-inventory--tsv-cell cells "\t"))

(defun nemacs-runtime-image-input-inventory--relative-readable-p (relative)
  "Return non-nil when RELATIVE exists under the repo root."
  (file-readable-p
   (expand-file-name relative nemacs-runtime-image-input-inventory-repo-root)))

(defun nemacs-runtime-image-input-inventory--scaffold-row (source index)
  "Return scaffold row plist for SOURCE in INDEX, or nil."
  (when (and source (file-readable-p index))
    (with-temp-buffer
      (insert-file-contents index)
      (let (row)
        (goto-char (point-min))
        (forward-line 1)
        (while (and (not row) (not (eobp)))
          (let ((fields (split-string
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         "\t")))
            (when (and (equal (nth 0 fields) "file")
                       (equal (nth 3 fields) source))
              (setq row (list :id (nth 1 fields)
                              :role (nth 2 fields)
                              :target (nth 4 fields)))))
          (forward-line 1))
        row))))

(defun nemacs-runtime-image-input-inventory--package-row (source)
  "Return package scaffold row for SOURCE, or nil."
  (nemacs-runtime-image-input-inventory--scaffold-row
   source nemacs-runtime-image-input-inventory-package-scaffold))

(defun nemacs-runtime-image-input-inventory--app-row (source)
  "Return app scaffold row for SOURCE, or nil."
  (nemacs-runtime-image-input-inventory--scaffold-row
   source nemacs-runtime-image-input-inventory-app-scaffold))

(defun nemacs-runtime-image-input-inventory--classify (kind source)
  "Return classification plist for runtime input KIND and SOURCE."
  (let ((package (nemacs-runtime-image-input-inventory--package-row source))
        (app (nemacs-runtime-image-input-inventory--app-row source)))
    (cond
     (package (list :class "package-scaffold"
                    :owner (plist-get package :id)
                    :role (plist-get package :role)
                    :target (plist-get package :target)))
     (app (list :class "app-scaffold"
                :owner (plist-get app :id)
                :role (plist-get app :role)
                :target (plist-get app :target)))
     ((eq kind 'generated)
      (list :class "generated-runtime-artifact"
            :owner "runtime" :role "generated" :target source))
     ((eq kind 'load-path)
      (cond
       ((string-prefix-p "packages/nelisp-emacs-app-" source)
        (list :class "app-scaffold-load-path"
              :owner "APP" :role "load-path" :target source))
       ((string-prefix-p "packages/nelisp-emacs-" source)
        (list :class "package-scaffold-load-path"
              :owner "packages" :role "load-path" :target source))
       ((string-prefix-p "vendor/" source)
        (list :class "vendor-load-path"
              :owner "vendor" :role "load-path" :target source))
       (t
        (list :class "temporary-src-load-path"
              :owner "runtime" :role "compat-load-path" :target source))))
     ((and source (string-prefix-p "scripts/" source))
      (list :class "runtime-script"
            :owner "runtime" :role "preload" :target source))
     ((and source (string-prefix-p "src/" source))
      (list :class "temporary-src-input"
            :owner "runtime" :role "source-fallback" :target source))
     (t (list :class "unknown" :owner "" :role "" :target "")))))

(defun nemacs-runtime-image-input-inventory--entry-row (entry)
  "Return TSV row data for ENTRY."
  (pcase-let ((`(,lane ,kind ,name ,source) entry))
    (let* ((status (nemacs-runtime-image-input-inventory--classify kind source))
           (exists (or (memq kind '(feature lazy-feature))
                       (nemacs-runtime-image-input-inventory--relative-readable-p
                        source))))
      (list (symbol-name lane)
            (symbol-name kind)
            (symbol-name name)
            source
            (plist-get status :class)
            (plist-get status :owner)
            (plist-get status :role)
            (plist-get status :target)
            (if exists "yes" "no")))))

(defun nemacs-runtime-image-input-inventory--rows ()
  "Return all inventory rows."
  (mapcar #'nemacs-runtime-image-input-inventory--entry-row
          nemacs-runtime-image-input-inventory--inputs))

(defun nemacs-runtime-image-input-inventory--counts (rows column)
  "Return alist of counts in ROWS by COLUMN index."
  (let (counts)
    (dolist (row rows)
      (let* ((key (nth column row))
             (cell (assoc key counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (push (cons key 1) counts))))
    (sort counts (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-runtime-image-input-inventory--write-tsv (rows)
  "Write ROWS to the configured TSV output."
  (make-directory (file-name-directory
                   nemacs-runtime-image-input-inventory-output)
                  t)
  (with-temp-buffer
    (insert (nemacs-runtime-image-input-inventory--row
             "lane" "kind" "name" "source" "class" "owner" "role"
             "target" "exists")
            "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-runtime-image-input-inventory--row row) "\n"))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max)
                    nemacs-runtime-image-input-inventory-output
                    nil 'silent))))

(defun nemacs-runtime-image-input-inventory--write-summary (rows)
  "Write Org summary for ROWS."
  (make-directory (file-name-directory
                   nemacs-runtime-image-input-inventory-summary-output)
                  t)
  (with-temp-buffer
    (insert "#+TITLE: nemacs runtime image input inventory\n\n")
    (insert "* Summary\n\n")
    (insert (format "- rows: %d\n" (length rows)))
    (insert (format "- unknown rows: %d\n"
                    (cl-count "unknown" rows :key (lambda (row) (nth 4 row))
                              :test #'equal)))
    (insert (format "- missing files: %d\n"
                    (cl-count "no" rows :key (lambda (row) (nth 8 row))
                              :test #'equal)))
    (insert "\n* Counts By Class\n\n")
    (insert "| Class | Rows |\n|-------+------|\n")
    (dolist (count (nemacs-runtime-image-input-inventory--counts rows 4))
      (insert (format "| =%s= | %d |\n" (car count) (cdr count))))
    (insert "\n* Counts By Lane\n\n")
    (insert "| Lane | Rows |\n|------+------|\n")
    (dolist (count (nemacs-runtime-image-input-inventory--counts rows 0))
      (insert (format "| =%s= | %d |\n" (car count) (cdr count))))
    (insert "\n* Inputs\n\n")
    (insert "| Lane | Kind | Name | Source | Class | Owner | Role | Target | Exists |\n")
    (insert "|------+------+------+--------+-------+-------+------+--------+--------|\n")
    (dolist (row rows)
      (insert
       (format "| =%s= | =%s= | =%s= | =%s= | =%s= | =%s= | =%s= | =%s= | %s |\n"
               (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row)
               (nth 4 row) (nth 5 row) (nth 6 row) (nth 7 row)
               (nth 8 row))))
    (insert "\n* Notes\n\n")
    (insert "- =package-scaffold= rows are reusable runtime inputs already mapped by package extraction.\n")
    (insert "- =package-scaffold-load-path= and =app-scaffold-load-path= rows keep runtime-image bakes on scaffold paths.\n")
    (insert "- =app-scaffold= rows stay outside reusable package descriptors.\n")
    (insert "- =temporary-src-input= and =temporary-src-load-path= rows are explicit compatibility debt and should remain zero.\n")
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max)
                    nemacs-runtime-image-input-inventory-summary-output
                    nil 'silent))))

;;;###autoload
(defun nemacs-runtime-image-input-inventory-batch ()
  "Generate runtime image input inventory artifacts."
  (let* ((rows (nemacs-runtime-image-input-inventory--rows))
         (unknown (cl-count "unknown" rows
                            :key (lambda (row) (nth 4 row))
                            :test #'equal))
         (missing (cl-count "no" rows
                            :key (lambda (row) (nth 8 row))
                            :test #'equal)))
    (nemacs-runtime-image-input-inventory--write-tsv rows)
    (nemacs-runtime-image-input-inventory--write-summary rows)
    (princ
     (format "nemacs-runtime-image-input-inventory: rows=%d unknown=%d missing=%d output=%s summary=%s\n"
             (length rows)
             unknown
             missing
             nemacs-runtime-image-input-inventory-output
             nemacs-runtime-image-input-inventory-summary-output))
    (when (or (> unknown 0) (> missing 0))
      (kill-emacs 1))))

(provide 'nemacs-runtime-image-input-inventory)

;;; nemacs-runtime-image-input-inventory.el ends here
