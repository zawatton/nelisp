;;; audit-vendor-classify.el --- classify vendored Emacs Lisp files  -*- lexical-binding: t; -*-

;; Static inventory helper for Doc 03 Phase 0.
;;
;; This script intentionally does not load vendor files.  It scans their
;; source text for blocker signatures so the migration can be driven by a
;; repeatable CSV rather than by ad hoc "try a file and see" sessions.

;;; Code:

(require 'cl-lib)

(defvar vendor-audit-repo-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by `vendor-audit-batch'.")

(defvar vendor-audit-vendor-root
  (expand-file-name "vendor/emacs-lisp" vendor-audit-repo-root)
  "Vendored Emacs Lisp root.")

(defvar vendor-audit-output-file
  (expand-file-name "docs/design/03-vendor-inventory.csv" vendor-audit-repo-root)
  "CSV output path for the vendor inventory.")

(defun vendor-audit--csv-field (value)
  "Return VALUE encoded as one CSV field."
  (let ((s (format "%s" (or value ""))))
    (concat "\""
            (replace-regexp-in-string
             "\"" "\"\""
             (replace-regexp-in-string "[\n\r]+" " " s))
            "\"")))

(defun vendor-audit--csv-row (fields)
  "Return FIELDS encoded as a CSV row."
  (mapconcat #'vendor-audit--csv-field fields ","))

(defun vendor-audit--file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun vendor-audit--matches-p (regexp text)
  "Return non-nil when REGEXP matches TEXT."
  (string-match-p regexp text))

(defun vendor-audit--requires (text)
  "Return a sorted list of feature names required by TEXT.
This is a lightweight source scan.  It captures common
`(require \\='foo)' and `(require \"foo\")' shapes without trying to
fully parse Emacs Lisp."
  (let (features)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "(require[ \t\n']+\\(?:'\\|\\(?:quote[ \t\n]+\\)\\)?\\([^][()\"' \t\n]+\\)" nil t)
        (push (match-string 1) features))
      (goto-char (point-min))
      (while (re-search-forward "(require[ \t\n]+\"\\([^\"]+\\)\"" nil t)
        (push (match-string 1) features)))
    (sort (delete-dups features) #'string<)))

(defun vendor-audit--classify (relative text)
  "Return plist classification for RELATIVE path with source TEXT."
  (let* ((raw-apostrophe (vendor-audit--matches-p "\\?'" text))
         (function-quote (vendor-audit--matches-p "#'" text))
         (autoload (or (vendor-audit--matches-p ";;;###autoload" text)
                       (vendor-audit--matches-p "(autoload[ \t\n]" text)))
         (dump-or-loaddefs (or (string-match-p "\\(?:loaddefs\\|ldefs-boot\\|cus-load\\|finder-inf\\)\\.el\\'" relative)
                               (vendor-audit--matches-p "dump-emacs\\|pdmp\\|preloaded" text)))
         (native-or-module (or (vendor-audit--matches-p "(module-load[ \t\n]" text)
                               (vendor-audit--matches-p "native-comp\\|comp-native" text)))
         (class (cond
                 (native-or-module "E")
                 ((or autoload dump-or-loaddefs) "D")
                 (function-quote "C")
                 (raw-apostrophe "B")
                 (t "A"))))
    (list :path relative
          :class class
          :raw-apostrophe raw-apostrophe
          :function-quote function-quote
          :autoload autoload
          :dump-or-loaddefs dump-or-loaddefs
          :native-or-module native-or-module
          :requires (vendor-audit--requires text))))

(defun vendor-audit--row (entry)
  "Return CSV row for ENTRY."
  (vendor-audit--csv-row
   (list (plist-get entry :path)
         (plist-get entry :class)
         (if (plist-get entry :raw-apostrophe) "1" "0")
         (if (plist-get entry :function-quote) "1" "0")
         (if (plist-get entry :autoload) "1" "0")
         (if (plist-get entry :dump-or-loaddefs) "1" "0")
         (if (plist-get entry :native-or-module) "1" "0")
         (mapconcat #'identity (plist-get entry :requires) ";"))))

(defun vendor-audit-batch ()
  "Write the Doc 03 Phase 0 vendor inventory CSV.
Returns a plist summary and prints it for shell logs."
  (let* ((files (sort (directory-files-recursively vendor-audit-vendor-root "\\.el\\'")
                      #'string<))
         (entries
          (mapcar
           (lambda (file)
             (vendor-audit--classify
              (file-relative-name file vendor-audit-vendor-root)
              (vendor-audit--file-string file)))
           files))
         (counts (let ((table (make-hash-table :test 'equal)))
                   (dolist (entry entries)
                     (cl-incf (gethash (plist-get entry :class) table 0)))
                   table)))
    (make-directory (file-name-directory vendor-audit-output-file) t)
    (with-temp-file vendor-audit-output-file
      (insert (vendor-audit--csv-row
               '("path" "class" "raw_apostrophe_char" "function_quote"
                 "autoload" "dump_or_loaddefs" "native_or_module" "requires"))
              "\n")
      (dolist (entry entries)
        (insert (vendor-audit--row entry) "\n")))
    (let ((summary (list :files (length entries)
                         :class-a (gethash "A" counts 0)
                         :class-b (gethash "B" counts 0)
                         :class-c (gethash "C" counts 0)
                         :class-d (gethash "D" counts 0)
                         :class-e (gethash "E" counts 0)
                         :output vendor-audit-output-file)))
      (princ (format "vendor-audit-summary=%S\n" summary))
      summary)))

(provide 'audit-vendor-classify)

;;; audit-vendor-classify.el ends here
