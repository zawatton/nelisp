;;; build-nelisp-bootstrap.el --- generate standalone bootstrap bundle  -*- lexical-binding: t; -*-

;;; Commentary:

;; Build helper for the NeLisp driver cold-start path.
;;
;; NeLisp v2 can run the runtime mostly as Elisp, but cold boot still
;; pays for many small source loads.  This script asks host Emacs to
;; load `nemacs-main', reads the resulting local `load-history', and
;; concatenates the participating src/*.el files in dependency order.
;; The generated file is still plain Elisp; it is a preload bundle, not
;; a bytecode/pdump replacement.

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar nelisp-bootstrap-output-file
  (expand-file-name "build/nemacs-bootstrap.el"
                    (expand-file-name ".." (file-name-directory
                                            (or load-file-name
                                                buffer-file-name))))
  "Output path for the generated NeLisp bootstrap bundle.")

(defvar nelisp-bootstrap-repl-output-file nil
  "Output path for the generated NeLisp REPL bootstrap input.
When nil, derive it from `nelisp-bootstrap-output-file' by replacing the
final `.el' suffix with `.repl'.")

(defvar nelisp-bootstrap-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root used by the bundle generator.")

(defvar nelisp-bootstrap-extra-files
  '("cl-lib.el"
    "seq.el"
    "map.el"
    "json.el"
    "range.el"
    "let-alist.el"
    "thunk.el"
    "generator.el"
    "rx.el"
    "emacs-tui-backend.el"
    "emacs-redisplay-core.el"
    "emacs-tui-event.el")
  "Local src files that host Emacs may not load but standalone NeLisp needs.")

(defvar nelisp-bootstrap-late-extra-files
  '("lisp.el"
    "emacs-fileio.el"
    "case-table.el"
    "emacs-process-events.el"
    "regi.el"
    "files-standalone-buffer.el"
    "emacs-syntax-table.el"
    "emacs-font-lock.el"
    "emacs-font-lock-builtins.el"
    ;; dev daily-driver surfaces: symbol index + jump-to-definition +
    ;; compile/grep + next-error.  Greenfield implementations first, then
    ;; the facade loaders that install the standard command names on the
    ;; standalone reader.
    "emacs-imenu.el"
    "emacs-xref.el"
    "imenu.el"
    "xref.el"
    "emacs-compile.el"
    "compile.el"
    "emacs-vc.el"
    "vc.el"
    "emacs-comint.el"
    "comint.el"
    "emacs-replace.el"
    "replace.el"
    "emacs-isearch.el"
    "isearch.el"
    "emacs-ielm.el"
    "ielm.el"
    "emacs-project.el"
    "project.el"
    "emacs-shell.el"
    "shell.el"
    "emacs-eshell.el"
    "eshell.el"
    "emacs-man.el"
    "man.el"
    "woman.el"
    "emacs-calc.el"
    "calc.el"
    ;; directory browser: greenfield `emacs-dired-min' (defines the `dired'
    ;; command on top of nelisp-ec-directory-files / -file-attributes) then
    ;; the `dired' feature facade.  Wired once the standalone reader's
    ;; readdir/stat syscalls return real entries (Doc 142 gate-5).
    "emacs-dired-min.el"
    "dired.el")
  "Local src files inserted after buffer/face substrates are available.")

(defvar nelisp-bootstrap-repl-direct-character-limit 0
  "Minimum printed form size emitted directly in generated REPL input.

Large forms are already normalized before this stage.  Emitting them as direct
REPL forms avoids an extra nested source-string read in the persistent
standalone evaluator while preserving the same evaluated form.  A value of 0
emits every bootstrap form directly.")

(defun nelisp-bootstrap--src-dir ()
  "Return the absolute src directory."
  (file-name-as-directory
   (expand-file-name "src" nelisp-bootstrap-repo-root)))

(defun nelisp-bootstrap--vendor-dir ()
  "Return the absolute vendor directory."
  (file-name-as-directory
   (expand-file-name "vendor" nelisp-bootstrap-repo-root)))

(defun nelisp-bootstrap--source-file (file)
  "Return local src source file for FILE, or nil.
FILE may be either the source `.el' path or the byte-compiled `.elc'
path recorded in `load-history'."
  (let ((src (nelisp-bootstrap--src-dir))
        (abs (and (stringp file) (expand-file-name file))))
    (and abs
         (string-prefix-p src abs)
         (cond
          ((and (string-suffix-p ".el" abs)
                (file-readable-p abs))
           abs)
          ((string-suffix-p ".elc" abs)
           (let ((source (substring abs 0 -1)))
             (and (file-readable-p source) source)))))))

(defun nelisp-bootstrap--collect-loaded-src-files ()
  "Return loaded local src files in dependency-first order."
  (let (files)
    (dolist (entry load-history)
      (let ((file (car-safe entry)))
        (let ((source (nelisp-bootstrap--source-file file)))
          (when source
            (push (expand-file-name source) files)))))
    (delete-dups files)))

(defun nelisp-bootstrap--insert-after (file anchor files)
  "Insert FILE after ANCHOR in FILES, unless FILE is already present."
  (let ((file (expand-file-name file))
        (anchor (expand-file-name anchor)))
    (cond
     ((member file files) files)
     ((not (member anchor files)) (cons file files))
     (t
      (let (out rest done)
        (setq rest files)
        (while rest
          (push (car rest) out)
          (when (equal (car rest) anchor)
            (push file out)
            (setq done t))
          (setq rest (cdr rest)))
        (unless done
          (push file out))
        (nreverse out))))))

(defun nelisp-bootstrap--complete-file-list (files)
  "Add standalone-only source files to FILES in a dependency-safe spot."
  (let ((src (nelisp-bootstrap--src-dir))
        (out files)
        (anchor (expand-file-name "emacs-cl-macros.el"
                                  (nelisp-bootstrap--src-dir))))
    (dolist (name nelisp-bootstrap-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (nelisp-bootstrap--insert-after file anchor out))
          (setq anchor file))))
    (setq anchor (expand-file-name "emacs-faces-builtins.el" src))
    (unless (member anchor out)
      (setq anchor (expand-file-name "emacs-faces.el" src)))
    (dolist (name nelisp-bootstrap-late-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (nelisp-bootstrap--insert-after file anchor out))
          (setq anchor file))))
    ;; Systemic fix (Doc 22 A19): load emacs-stub-bulk LAST so its bulk
    ;; no-op stubs only fill names still void after every real
    ;; implementation has loaded.  Loaded early, the stubs shadow real
    ;; impls gated with `unless (fboundp ...)' (e.g. mapcan / regexp-opt).
    (let ((bulk (expand-file-name "emacs-stub-bulk.el" src)))
      (when (member bulk out)
        (setq out (append (delete bulk out) (list bulk)))))
    out))

(defun nelisp-bootstrap--write-bundle (files output)
  "Write FILES into OUTPUT as one lexical-binding Elisp bundle."
  (make-directory (file-name-directory output) t)
  ;; NeLisp's current `load' prefers OUTPUT.elc even when OUTPUT ends in
  ;; ".el".  Remove stale byte-compiled companions so the bootstrap
  ;; bundle stays a plain-Elisp preload file.
  (let ((compiled (concat output "c")))
    (when (file-exists-p compiled)
      (delete-file compiled)))
  (with-temp-buffer
    (insert ";;; nemacs-bootstrap.el --- generated NeLisp bootstrap bundle  -*- lexical-binding: t; -*-\n")
    (insert ";;; Generated by scripts/build-nelisp-bootstrap.el; do not edit.\n\n")
    (dolist (file files)
      (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
        (insert "\n;;; >>> " rel "\n")
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "\n;;; <<< " rel "\n")))
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

(defun nelisp-bootstrap--read-forms-from-file (file)
  "Return top-level forms read from FILE."
  (standalone-source-normalize-read-forms-from-file file))

(defun nelisp-bootstrap--one-line-string-literal (string)
  "Return STRING as an Elisp string literal that fits on one line."
  (let ((literal (let ((print-quoted nil))
                   (prin1-to-string string))))
    (setq literal (replace-regexp-in-string "\n" "\\\\n" literal t t))
    (setq literal (replace-regexp-in-string "\r" "\\\\r" literal t t))
    literal))

(defun nelisp-bootstrap--standalone-repl-form (form)
  "Return FORM normalized for the standalone-reader REPL bootstrap.

The standalone prelude currently ignores definition docstrings.  Dropping
those unused arguments keeps generated REPL bootstrap input smaller and
avoids retaining large docstring literals in the persistent evaluator."
  (cond
   ((and (consp form)
         (memq (car form) '(defun defmacro))
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (append (list (nth 0 form) (nth 1 form) (nth 2 form))
            (nthcdr 4 form)))
   ((and (consp form)
         (memq (car form) '(defvar defconst))
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list (nth 0 form) (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'defvar-local)
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list 'defvar (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'defcustom)
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list 'defvar (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'cl-defstruct)
         (>= (length form) 3)
         (stringp (nth 2 form)))
    (append (list (nth 0 form) (nth 1 form))
            (nthcdr 3 form)))
   (t form)))

(defun nelisp-bootstrap--function-headed-list-p (object)
  "Return non-nil when OBJECT contains a list headed by symbol `function'."
  (cond
   ((consp object)
    (or (eq (car object) 'function)
        (nelisp-bootstrap--function-headed-list-p (car object))
        (nelisp-bootstrap--function-headed-list-p (cdr object))))
   (t nil)))

(defun nelisp-bootstrap--quoted-defun-lambda-list-risk-p (object)
  "Return non-nil when OBJECT contains a defun whose arglist prints as `#''."
  (cond
   ((and (consp object)
         (memq (car object) '(defun defmacro))
         (nelisp-bootstrap--function-headed-list-p (nth 2 object)))
    t)
   ((and (consp object)
         (eq (car object) 'quote))
    nil)
   ((consp object)
    (or (nelisp-bootstrap--quoted-defun-lambda-list-risk-p (car object))
        (nelisp-bootstrap--quoted-defun-lambda-list-risk-p (cdr object))))
   (t nil)))

(defun nelisp-bootstrap--direct-repl-form-p (rel form &optional form-string)
  "Return non-nil when FORM from REL should be emitted as a direct REPL form.

FORM-STRING, when non-nil, is the printed form used for size-based emission."
  (or (member rel '("src/nelisp-text-buffer.el"
                    "src/nelisp-emacs-compat.el"))
      (and (consp form)
           (eq (car form) 'cl-defstruct))
      (and form-string
           (> (length form-string)
              nelisp-bootstrap-repl-direct-character-limit))))

(defun nelisp-bootstrap--repl-form-string (form)
  "Return FORM printed for `nelisp--eval-source-string'."
  (let ((print-escape-newlines t)
        ;; Host `prin1' abbreviates any list headed by `function' as `#'...'.
        ;; That is correct for quoted function forms, but invalid inside a
        ;; lambda list such as `(defun maphash (function table) ...)'.
        (print-quoted
         (not (nelisp-bootstrap--quoted-defun-lambda-list-risk-p form))))
    (prin1-to-string form)))

(defun nelisp-bootstrap--write-repl-bundle (files output)
  "Write FILES into OUTPUT as standalone-reader REPL input.

The standalone reader's persistent development surface is the REPL.
Most forms go through `nelisp--eval-source-string', matching the source
loader path.  Buffer substrate files are emitted as direct REPL forms
because their `cl-defstruct' accessors and setters must be installed in
the live REPL context for immediate redefinition."
  (make-directory (file-name-directory output) t)
  (with-temp-buffer
    (insert ";;; nemacs-bootstrap.repl --- generated NeLisp bootstrap REPL input\n")
    (insert ";;; Generated by scripts/build-nelisp-bootstrap.el; do not edit.\n")
    (dolist (file files)
      (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
        (insert "\n;;; >>> " rel "\n")
        (dolist (source-form (nelisp-bootstrap--read-forms-from-file file))
          (let* ((form (nelisp-bootstrap--standalone-repl-form source-form))
                 (form-string (nelisp-bootstrap--repl-form-string form)))
            (if (nelisp-bootstrap--direct-repl-form-p rel form form-string)
                (progn
                  (insert "(progn ")
                  (insert form-string)
                  (insert " nil)\n"))
              (insert "(progn (nelisp--eval-source-string ")
              (insert (nelisp-bootstrap--one-line-string-literal
                       form-string))
              (insert ") nil)\n"))))
        (insert ";;; <<< " rel "\n")))
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

(defun nelisp-bootstrap-build-batch ()
  "Generate `nelisp-bootstrap-output-file' and print a short summary."
  (let* ((src (nelisp-bootstrap--src-dir))
         (vendor (nelisp-bootstrap--vendor-dir))
         (nelisp-emacs-vendor-root (directory-file-name vendor)))
    (add-to-list 'load-path src)
    (add-to-list 'load-path (expand-file-name "emacs-lisp" vendor) t)
    (add-to-list 'load-path (expand-file-name "emacs-lisp/emacs-lisp" vendor) t)
    (require 'nemacs-main)
    (let* ((files (nelisp-bootstrap--complete-file-list
                   (nelisp-bootstrap--collect-loaded-src-files)))
           (output (expand-file-name nelisp-bootstrap-output-file))
           (repl-output
            (expand-file-name
             (or nelisp-bootstrap-repl-output-file
                 (concat (file-name-sans-extension output) ".repl")))))
      (nelisp-bootstrap--write-bundle files output)
      (nelisp-bootstrap--write-repl-bundle files repl-output)
      (princ (format "nelisp-bootstrap bundle=%s repl=%s files=%d\n"
                     output repl-output (length files))))))

(provide 'build-nelisp-bootstrap)

;;; build-nelisp-bootstrap.el ends here
