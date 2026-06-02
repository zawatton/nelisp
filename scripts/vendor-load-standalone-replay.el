;;; vendor-load-standalone-replay.el --- standalone vendor load replay  -*- lexical-binding: t; -*-

;;; Code:

(defvar vendor-load-standalone-reader nil
  "Path to target/nelisp-standalone-reader.")

(defvar vendor-load-standalone-bootstrap nil
  "Path to the generated nemacs bootstrap bundle.")

(defvar vendor-load-standalone-prelude nil
  "Path to the standalone reader stdlib prelude.")

(defvar vendor-load-standalone-files nil
  "Whitespace-separated string or list of vendor files to load.")

(defvar vendor-load-standalone-proof-form
  "(fboundp (quote emacs-keymap-define-key-after))"
  "Raw Lisp form that must be true after load replay.
The default proves that the bootstrap bundle was actually evaluated,
not merely skipped before the final sentinel.")

(defvar vendor-load-standalone-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defun vendor-load-standalone--files ()
  "Return normalized absolute vendor file list."
  (cond
   ((stringp vendor-load-standalone-files)
    (mapcar #'expand-file-name
            (split-string vendor-load-standalone-files "[ \t\n]+" t)))
   ((listp vendor-load-standalone-files)
    (mapcar #'expand-file-name vendor-load-standalone-files))
   (t nil)))

(defun vendor-load-standalone--load-paths ()
  "Return the load paths needed for standalone vendor replay."
  (list (expand-file-name "src" vendor-load-standalone-repo-root)
        (expand-file-name "scripts" vendor-load-standalone-repo-root)
        (expand-file-name "vendor/emacs-lisp" vendor-load-standalone-repo-root)
        (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                          vendor-load-standalone-repo-root)
        (expand-file-name "vendor/emacs-lisp/vc"
                          vendor-load-standalone-repo-root)))

(defun vendor-load-standalone--write-program (files output)
  "Write a standalone-reader load replay program for FILES to OUTPUT.
The load forms are deliberately top-level forms.  The standalone reader
currently handles large file loads reliably at top level, while wrapping
the same sequence in one large `progn' can leave the success sentinel
unreached.  The final value is 42 only when
`vendor-load-standalone-proof-form' evaluates non-nil after the loads."
  (with-temp-file output
    (insert ";;; standalone vendor load replay probe -*- lexical-binding: t; -*-\n")
    (insert (format "(setq nelisp-emacs-vendor-root %S)\n"
                    (expand-file-name "vendor" vendor-load-standalone-repo-root)))
    (insert (format "(setq load-path %S)\n"
                    (vendor-load-standalone--load-paths)))
    (when vendor-load-standalone-prelude
      (insert (format "(load %S nil (quote no-message) t t)\n"
                      vendor-load-standalone-prelude)))
    (insert (format "(load %S nil (quote no-message) t t)\n"
                    vendor-load-standalone-bootstrap))
    (dolist (file files)
      (insert (format "(load %S nil (quote no-message) t t)\n" file)))
    ;; The standalone reader currently reports Lisp errors as exit 0.
    ;; A successful replay must therefore prove a post-load binding and
    ;; then reach this explicit sentinel.
    (insert (format "(if %s 42 13)\n"
                    vendor-load-standalone-proof-form))))

(defun vendor-load-standalone--run (files)
  "Run standalone reader on a generated replay program for FILES."
  (let ((tmp (make-temp-file "nemacs-vendor-load-standalone-" nil ".el"))
        (start (float-time))
        exit elapsed)
    (unwind-protect
        (progn
          (vendor-load-standalone--write-program files tmp)
          (setq exit (call-process vendor-load-standalone-reader nil nil nil tmp))
          (setq elapsed (- (float-time) start))
          (list exit elapsed))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun vendor-load-standalone-batch ()
  "Load vendor files through standalone-reader using host-side diagnostics."
  (unless (and vendor-load-standalone-reader
               (file-executable-p vendor-load-standalone-reader))
    (error "vendor-load-standalone-reader is not executable: %S"
           vendor-load-standalone-reader))
  (unless (and vendor-load-standalone-bootstrap
               (file-readable-p vendor-load-standalone-bootstrap))
    (error "vendor-load-standalone-bootstrap is not readable: %S"
           vendor-load-standalone-bootstrap))
  (unless (and vendor-load-standalone-prelude
               (file-readable-p vendor-load-standalone-prelude))
    (error "vendor-load-standalone-prelude is not readable: %S"
           vendor-load-standalone-prelude))
  (let ((files (vendor-load-standalone--files)))
    (unless files
      (error "vendor-load-standalone-files is empty"))
    (dolist (file files)
      (unless (file-readable-p file)
        (error "vendor load file is not readable: %S" file)))
    (princ (format "vendor-load-standalone files=%S proof=%s status=start\n"
                   files vendor-load-standalone-proof-form))
    (pcase-let ((`(,exit ,elapsed) (vendor-load-standalone--run files)))
      (if (and (numberp exit) (= exit 42))
          (princ (format "vendor-load-standalone files=%S proof=%s status=done elapsed=%S exit=%S\n"
                         files vendor-load-standalone-proof-form elapsed exit))
        (princ (format "vendor-load-standalone files=%S proof=%s status=fail elapsed=%S exit=%S expected=42\n"
                       files vendor-load-standalone-proof-form elapsed exit))
        (kill-emacs 1)))))

(provide 'vendor-load-standalone-replay)

;;; vendor-load-standalone-replay.el ends here
