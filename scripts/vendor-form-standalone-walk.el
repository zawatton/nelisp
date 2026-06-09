;;; vendor-form-standalone-walk.el --- host-driven standalone vendor form walk  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar vendor-form-standalone-reader nil
  "Path to target/nelisp or a compatible standalone reader binary.")

(defvar vendor-form-standalone-bootstrap nil
  "Path to the generated nemacs bootstrap bundle.")

(defvar vendor-form-standalone-prelude nil
  "Path to the standalone reader stdlib prelude, or nil.")

(defvar vendor-form-standalone-file nil
  "Vendor file to evaluate form-by-form.")

(defvar vendor-form-standalone-preload-files nil
  "Whitespace-separated string or list of files to load before form walking.")

(defvar vendor-form-standalone-start-index 1
  "One-based top-level form index to start reporting.")

(defvar vendor-form-standalone-limit 0
  "Maximum number of reported forms.  Zero means no limit.")

(defvar vendor-form-standalone-print-every 1
  "Print progress for every Nth reported form.")

(defvar vendor-form-standalone-normalize-floats nil
  "When non-nil, rewrite float literals in vendor probe forms to integers.

This is a diagnostic escape hatch for standalone-reader crashes while
installing function bodies that contain float-literal comparisons.  The
default nil path evaluates the raw vendor source.")

(defvar vendor-form-standalone-short-load-name-files
  '("org-element-ast.el"
    "org-footnote.el"
    "org-list.el"
    "org-entities.el"
    "org-macro.el")
  "Basenames whose replay `load-file-name' is shortened to the basename.")

(defvar vendor-form-standalone-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defun vendor-form-standalone--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun vendor-form-standalone--head (form)
  "Return a short head for FORM."
  (cond
   ((consp form) (car form))
   ((symbolp form) form)
   (t (type-of form))))

(defun vendor-form-standalone--forms (file)
  "Return top-level form descriptors for FILE.
Each descriptor is a plist with :index, :pos, :end, :head, and :text."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((index 0)
          forms)
      (goto-char (point-min))
      (condition-case err
          (while t
            (let ((pos (point))
                  form end)
              (setq form (read (current-buffer)))
              (setq end (point))
              (setq index (1+ index))
              (push (list :index index
                          :pos (1- pos)
                          :end (1- end)
                          :head (vendor-form-standalone--head form)
                          :text (buffer-substring-no-properties pos end))
                    forms)))
        (end-of-file nil)
        (error
         (error "read error in %s near index %d: %S" file (1+ index) err)))
      (nreverse forms))))

(defun vendor-form-standalone--print-p (reported)
  "Return non-nil when REPORTED form progress should be printed."
  (or (<= vendor-form-standalone-print-every 1)
      (= (% reported vendor-form-standalone-print-every) 0)))

(defun vendor-form-standalone--normalize-floats (form)
  "Return FORM with float literals replaced by truncated integers."
  (cond
   ((floatp form) (truncate form))
   ((consp form)
    (cons (vendor-form-standalone--normalize-floats (car form))
          (vendor-form-standalone--normalize-floats (cdr form))))
   ((vectorp form)
    (apply #'vector
           (mapcar #'vendor-form-standalone--normalize-floats
                   (append form nil))))
   (t form)))

(defun vendor-form-standalone--form-text (form &optional current-file)
  "Return source text for FORM, applying diagnostic rewrites if enabled."
  (let ((text (plist-get form :text)))
    (condition-case nil
        (with-temp-buffer
          (let* ((raw (if vendor-form-standalone-normalize-floats
                          (vendor-form-standalone--normalize-floats
                           (read text))
                        (read text)))
                 (rewritten
                  (let ((standalone-source-normalize-current-file
                         (or current-file
                             standalone-source-normalize-current-file)))
                    (standalone-source-normalize-top-level-forms raw))))
            (dolist (top-level rewritten)
              (insert (standalone-source-normalize-form-to-string top-level))
              (insert "\n")))
          (buffer-string))
      (error text))))

(defun vendor-form-standalone--eval-source-form (source)
  "Return a standalone form that evaluates SOURCE through NeLisp's reader."
  (format "(nelisp--eval-source-string %S)\n" source))

(defun vendor-form-standalone--preload-files ()
  "Return normalized absolute preload file list."
  (cond
   ((stringp vendor-form-standalone-preload-files)
    (mapcar #'expand-file-name
            (split-string vendor-form-standalone-preload-files
                          "[ \t\n]+" t)))
   ((listp vendor-form-standalone-preload-files)
    (mapcar #'expand-file-name vendor-form-standalone-preload-files))
   (t nil)))

(defun vendor-form-standalone--load-form (file)
  "Return a top-level standalone form that loads FILE."
  (let ((runtime-file (vendor-form-standalone--runtime-file-name file)))
    (concat
     (format "(setq load-file-name %S)\n" runtime-file)
     (format "(setq buffer-file-name %S)\n" runtime-file)
     (mapconcat #'vendor-form-standalone--eval-source-form
                (standalone-source-normalize-file-to-form-strings file)
                ""))))

(defun vendor-form-standalone--runtime-file-name (file)
  "Return FILE's runtime name for standalone form replay."
  (let ((basename (file-name-nondirectory file)))
    (if (member basename vendor-form-standalone-short-load-name-files)
        basename
      file)))

(defun vendor-form-standalone--load-paths ()
  "Return the load paths needed for standalone vendor form probes."
  (list (expand-file-name "src" vendor-form-standalone-repo-root)
        (expand-file-name "scripts" vendor-form-standalone-repo-root)
        (expand-file-name "vendor/emacs-lisp" vendor-form-standalone-repo-root)
        (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                          vendor-form-standalone-repo-root)
        (expand-file-name "vendor/emacs-lisp/vc"
                          vendor-form-standalone-repo-root)))

(defun vendor-form-standalone--write-program
    (bootstrap vendor-file forms upto output &optional preload-files)
  "Write standalone-reader program for FORMS through one-based UPTO."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file output
      (insert ";;; standalone vendor form walk probe -*- lexical-binding: t; -*-\n")
      (insert (format "(setq nelisp-emacs-vendor-root %S)\n"
                      (expand-file-name "vendor" vendor-form-standalone-repo-root)))
      (insert (format "(setq load-path '%S)\n"
                      (vendor-form-standalone--load-paths)))
      (when vendor-form-standalone-prelude
        (dolist (source (standalone-source-normalize-file-to-form-strings
                         vendor-form-standalone-prelude))
          (insert (vendor-form-standalone--eval-source-form source))))
      (insert (vendor-form-standalone--read-file bootstrap))
      (insert "\n")
      (dolist (file preload-files)
        (insert (vendor-form-standalone--load-form file)))
      (let ((runtime-file (vendor-form-standalone--runtime-file-name
                           vendor-file)))
        (insert (format "(setq load-file-name %S)\n" runtime-file))
        (insert (format "(setq buffer-file-name %S)\n" runtime-file)))
      (let ((standalone-source-normalize-current-file
             (file-name-nondirectory vendor-file)))
        (dolist (form forms)
          (let ((index (plist-get form :index)))
            (when (<= index upto)
              (let ((text (vendor-form-standalone--form-text
                           form
                           standalone-source-normalize-current-file)))
                (unless (string-empty-p text)
                  (insert text)
                  (insert "\n")))))))
      ;; The standalone reader currently reports Lisp errors as exit 0.
      ;; A successful probe must therefore reach this explicit sentinel.
      (insert "\n(exit 42)\n"))))

(defun vendor-form-standalone--run-prefix (forms upto)
  "Run standalone reader on FORMS through one-based UPTO."
  (let ((tmp (make-temp-file "nemacs-vendor-form-standalone-" nil ".el"))
        (start (float-time))
        exit elapsed)
    (unwind-protect
        (progn
          (vendor-form-standalone--write-program
           vendor-form-standalone-bootstrap
           vendor-form-standalone-file
           forms upto tmp
           (vendor-form-standalone--preload-files))
          (setq exit
                (call-process vendor-form-standalone-reader nil nil nil
                              "--load" tmp))
          (setq elapsed (- (float-time) start))
          (list exit elapsed))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun vendor-form-standalone-batch ()
  "Evaluate vendor forms through standalone-reader using host-side diagnostics."
  (unless (and vendor-form-standalone-reader
               (file-executable-p vendor-form-standalone-reader))
    (error "vendor-form-standalone-reader is not executable: %S"
           vendor-form-standalone-reader))
  (unless (and vendor-form-standalone-bootstrap
               (file-readable-p vendor-form-standalone-bootstrap))
    (error "vendor-form-standalone-bootstrap is not readable: %S"
           vendor-form-standalone-bootstrap))
  (when (and vendor-form-standalone-prelude
             (not (file-readable-p vendor-form-standalone-prelude)))
    (error "vendor-form-standalone-prelude is not readable: %S"
           vendor-form-standalone-prelude))
  (unless (and vendor-form-standalone-file
               (file-readable-p vendor-form-standalone-file))
    (error "vendor-form-standalone-file is not readable: %S"
           vendor-form-standalone-file))
  (dolist (file (vendor-form-standalone--preload-files))
    (unless (file-readable-p file)
      (error "vendor-form-standalone-preload file is not readable: %S"
             file)))
  (let* ((forms (vendor-form-standalone--forms vendor-form-standalone-file))
         (start (max 1 vendor-form-standalone-start-index))
         (limit vendor-form-standalone-limit)
         (reported 0)
         (evaluated 0)
         (failed nil))
    (princ (format "vendor-form-standalone-read file=%S status=done forms=%d normalize-floats=%S\n"
                   vendor-form-standalone-file
                   (length forms)
                   vendor-form-standalone-normalize-floats))
    (cl-loop for form in forms
             for index = (plist-get form :index)
             while (and (not failed)
                        (or (<= limit 0) (< reported limit)))
             when (>= index start)
             do (setq reported (1+ reported))
             and do (when (vendor-form-standalone--print-p reported)
                      (princ (format "vendor-form-standalone index=%d status=start pos=%d end=%d head=%S\n"
                                     index
                                     (plist-get form :pos)
                                     (plist-get form :end)
                                     (plist-get form :head))))
             and do (pcase-let ((`(,exit ,elapsed)
                                  (vendor-form-standalone--run-prefix
                                   forms index)))
                      (if (and (numberp exit) (= exit 42))
                          (progn
                            (setq evaluated (1+ evaluated))
                            (when (vendor-form-standalone--print-p reported)
                              (princ (format "vendor-form-standalone index=%d status=done pos=%d head=%S elapsed=%S exit=%S\n"
                                             index
                                             (plist-get form :end)
                                             (plist-get form :head)
                                             elapsed exit))))
                        (setq failed t)
                        (princ (format "vendor-form-standalone index=%d status=fail pos=%d head=%S elapsed=%S exit=%S expected=42\n"
                                       index
                                       (plist-get form :pos)
                                       (plist-get form :head)
                                       elapsed exit)))))
    (princ (format "vendor-form-standalone-summary file=%S forms=%d evaluated=%d status=%s\n"
                   vendor-form-standalone-file
                   (length forms)
                   evaluated
                   (if failed "fail" "done")))
    (when failed
      (kill-emacs 1))))

(provide 'vendor-form-standalone-walk)

;;; vendor-form-standalone-walk.el ends here
