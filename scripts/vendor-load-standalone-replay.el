;;; vendor-load-standalone-replay.el --- standalone vendor load replay  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar vendor-load-standalone-reader nil
  "Path to target/nelisp or a compatible standalone reader binary.")

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

(defvar vendor-load-standalone-write-status nil
  "Non-nil means write per-file status markers during load replay.

The status writes are diagnostic-only.  Keep them off by default because the
standalone runtime is still sensitive to the extra top-level file writes in
large vendor replays.")

(defvar vendor-load-standalone-debug-program nil
  "When non-nil, write the generated replay program to this file and keep it.")

(defvar vendor-load-standalone-short-load-name-files
  '("org-element-ast.el"
    "org-footnote.el"
    "org-list.el"
    "org-entities.el"
    "org-macro.el")
  "Basenames whose replay `load-file-name' is shortened to the basename.
These files do not inspect their load path at runtime, and avoiding another
large absolute path string keeps late vendor replay forms within the current
standalone reader envelope.")

(defvar vendor-load-standalone-direct-source-files
  '("org-macro.el")
  "Basenames whose normalized forms are inserted directly during load replay.
Most files are fed through `nelisp--eval-source-string' one form at a time.
`org-macro.el' contains a NUL delimiter string that is stable as direct source
but can crash the standalone reader when nested inside another source string.")

(defvar vendor-load-standalone-omitted-runtime-file-name-files
  '("oc-bibtex.el"
    "thunk.el"
    "env.el"
    "fileloop.el")
  "Basenames whose replay omits `load-file-name' and `buffer-file-name'.
Late vendor add-ons can trip the standalone reader by assigning even short
filename strings after a large prefix.  These files do not need their runtime
filename for the callable surface covered by replay.")

(defun vendor-load-standalone--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun vendor-load-standalone--eval-source-form (source)
  "Return a standalone form that evaluates SOURCE through NeLisp's reader."
  (format "(nelisp--eval-source-string %S)\n" source))

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

(defun vendor-load-standalone--load-status-form (status-file status)
  "Return standalone form writing STATUS to STATUS-FILE."
  (format "(nl-write-file %S %S)\n" status-file status))

(defun vendor-load-standalone--runtime-file-name (file)
  "Return FILE's runtime name for standalone load replay."
  (let ((basename (file-name-nondirectory file)))
    (if (member basename vendor-load-standalone-short-load-name-files)
        basename
      file)))

(defun vendor-load-standalone--omit-runtime-file-name-p (file)
  "Return non-nil when replay should not bind runtime filename for FILE."
  (member (file-name-nondirectory file)
          vendor-load-standalone-omitted-runtime-file-name-files))

(defun vendor-load-standalone--form-string (form)
  "Return FORM printed for the standalone replay program."
  (with-temp-buffer
    (let ((print-quoted nil))
      (prin1 form (current-buffer)))
    (buffer-string)))

(defun vendor-load-standalone--proof-gate-forms (form)
  "Return standalone top-level forms that gate success on FORM.

Large standalone replays are sensitive to deeply nested final expressions.
Keep proof checks as top-level forms and make the final successful sentinel a
plain top-level `(exit 42)'."
  (cond
   ((and (consp form) (eq (car form) 'progn))
    (let ((body (cdr form)))
      (cond
       ((null body)
        (vendor-load-standalone--proof-gate-forms t))
       ((null (cdr body))
        (vendor-load-standalone--proof-gate-forms (car body)))
       (t
        (append (mapcar (lambda (subform)
                          (concat (vendor-load-standalone--form-string subform)
                                  "\n"))
                        (butlast body))
                (vendor-load-standalone--proof-gate-forms
                 (car (last body))))))))
   ((and (consp form) (eq (car form) 'and))
    (if (null (cdr form))
        (vendor-load-standalone--proof-gate-forms t)
      (apply #'append
             (mapcar #'vendor-load-standalone--proof-gate-forms
                     (cdr form)))))
   (t
    (list
     (format "(setq vendor-standalone-proof-ok %s)\n"
             (vendor-load-standalone--form-string form))
     "(if vendor-standalone-proof-ok nil (exit 13))\n"))))

(defun vendor-load-standalone--proof-forms ()
  "Return standalone top-level proof forms plus the success sentinel."
  (condition-case nil
      (append
       (vendor-load-standalone--proof-gate-forms
        (read vendor-load-standalone-proof-form))
       '("(exit 42)\n"))
    (error
     (list
      (format "(setq vendor-standalone-proof-ok %s)\n"
              vendor-load-standalone-proof-form)
      "(if vendor-standalone-proof-ok nil (exit 13))\n"
      "(exit 42)\n"))))

(defun vendor-load-standalone--count-proof-form-p (form file-count)
  "Return non-nil when FORM only proves replay count for FILE-COUNT files."
  (cond
   ((eq form t)
    t)
   ((and (consp form) (eq (car form) 'and))
    (and (cdr form)
         (cl-every
          (lambda (subform)
            (vendor-load-standalone--count-proof-form-p subform file-count))
          (cdr form))))
   ((and (consp form)
         (eq (car form) '=)
         (equal (cadr form) 'vendor-standalone-load-ok-count)
         (null (cdddr form)))
    (let ((rhs (caddr form)))
      (or (equal rhs 'vendor-standalone-load-file-count)
          (and (integerp rhs) (= rhs file-count)))))
   (t nil)))

(defun vendor-load-standalone--count-only-proof-p (file-count)
  "Return non-nil when the configured proof is only FILE-COUNT equality."
  (condition-case nil
      (vendor-load-standalone--count-proof-form-p
       (read vendor-load-standalone-proof-form)
       file-count)
    (error nil)))

(defun vendor-load-standalone--write-program (files output &optional status-file)
  "Write a standalone-reader load replay program for FILES to OUTPUT.
The load forms are deliberately top-level forms.  The standalone reader
currently handles large file loads reliably at top level, while wrapping
the same sequence in one large `progn' can leave the success sentinel
unreached.  The final value is 42 only when
`vendor-load-standalone-proof-form' evaluates non-nil after the loads."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file output
      (insert ";;; standalone vendor load replay probe -*- lexical-binding: t; -*-\n")
      (insert (format "(setq nelisp-emacs-vendor-root %S)\n"
                      (expand-file-name "vendor" vendor-load-standalone-repo-root)))
      (insert (format "(setq load-path '%S)\n"
                      (vendor-load-standalone--load-paths)))
      (when vendor-load-standalone-prelude
        (dolist (source (standalone-source-normalize-file-to-form-strings
                         vendor-load-standalone-prelude))
          (insert (vendor-load-standalone--eval-source-form source))))
      (insert (vendor-load-standalone--read-file
               vendor-load-standalone-bootstrap))
      (unless (bolp)
        (insert "\n"))
      (insert (format "(setq vendor-standalone-load-file-count %d)\n"
                      (length files)))
      (insert "(setq vendor-standalone-load-ok-count 0)\n")
      (let ((count-only-proof
             (vendor-load-standalone--count-only-proof-p (length files))))
        (dolist (file files)
          (let ((sources (standalone-source-normalize-file-to-form-strings file)))
            (when sources
              (when status-file
                (insert (vendor-load-standalone--load-status-form
                         status-file
                         (concat "start:" (file-name-nondirectory file)))))
              (unless (vendor-load-standalone--omit-runtime-file-name-p file)
                (let ((runtime-file (vendor-load-standalone--runtime-file-name file)))
                  (insert (format "(setq load-file-name %S)\n" runtime-file))
                  (insert (format "(setq buffer-file-name %S)\n" runtime-file))))
              (dolist (source sources)
                (if (member (file-name-nondirectory file)
                            vendor-load-standalone-direct-source-files)
                    (progn
                      (insert source)
                      (insert "\n"))
                  (insert (vendor-load-standalone--eval-source-form source))))
              (when status-file
                (insert (vendor-load-standalone--load-status-form
                         status-file
                         (concat "ok:" (file-name-nondirectory file))))))))
        (if count-only-proof
            ;; For count-only replay, reaching this sentinel after every
            ;; generated file form is the proof.  Avoid late variable lookups
            ;; that currently perturb very large standalone programs.
            (insert "(exit 42)\n")
          ;; Reaching this form already proves every generated load form above
          ;; completed.  Updating this counter once avoids extra top-level
          ;; forms that currently perturb large standalone replays.
          (insert "(setq vendor-standalone-load-ok-count vendor-standalone-load-file-count)\n")
          ;; The standalone reader currently reports Lisp errors as exit 0.
          ;; A successful replay must therefore prove a post-load binding and
          ;; then reach this explicit sentinel.
          (dolist (form (vendor-load-standalone--proof-forms))
            (insert form)))))))

(defun vendor-load-standalone--run (files)
  "Run standalone reader on a generated replay program for FILES."
  (let ((tmp (or vendor-load-standalone-debug-program
                 (make-temp-file "nemacs-vendor-load-standalone-" nil ".el")))
        (status-file (and vendor-load-standalone-write-status
                          (make-temp-file "nemacs-vendor-load-status-")))
        (start (float-time))
        exit elapsed status)
    (unwind-protect
        (progn
          (vendor-load-standalone--write-program files tmp status-file)
          (setq exit
                (call-process vendor-load-standalone-reader nil nil nil
                              "--load" tmp))
          (setq elapsed (- (float-time) start))
          (setq status (when (and status-file
                                   (file-readable-p status-file))
                         (replace-regexp-in-string
                          "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
                          (vendor-load-standalone--read-file status-file))))
          (list exit elapsed status))
      (when (and (not vendor-load-standalone-debug-program)
                 (file-exists-p tmp))
        (delete-file tmp))
      (when (and status-file (file-exists-p status-file))
        (delete-file status-file)))))

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
    (pcase-let ((`(,exit ,elapsed ,status) (vendor-load-standalone--run files)))
      (if (and (numberp exit) (= exit 42))
          (princ (format "vendor-load-standalone files=%S proof=%s status=done elapsed=%S exit=%S load-status=%S\n"
                         files vendor-load-standalone-proof-form elapsed exit status))
        (princ (format "vendor-load-standalone files=%S proof=%s status=fail elapsed=%S exit=%S expected=42 load-status=%S\n"
                       files vendor-load-standalone-proof-form elapsed exit status))
        (kill-emacs 1)))))

(provide 'vendor-load-standalone-replay)

;;; vendor-load-standalone-replay.el ends here
