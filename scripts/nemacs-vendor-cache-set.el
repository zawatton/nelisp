;;; nemacs-vendor-cache-set.el --- Doc 142 vendor .nelc cache set -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nemacs-vendor-cache)

(defconst nemacs-vendor-cache-set--org-macs-drop-reason
  "Dropped from the cached set: `nelisp-artifact-compile-file' fails to read \
`org-macs.el' with `nelisp-read-error' \"unknown string escape\".")

(defvar nemacs-vendor-cache-set-root-override nil
  "Override root for set-cache artifacts.
Nil means `build/nelisp-artifacts/vendor-set' under the repo root.")

(defvar nemacs-vendor-cache-set-batch-source-overrides nil
  "Alist mapping entry names to source path overrides for batch tests.")

(defvar nemacs-vendor-cache-set-batch-root-override nil
  "Batch test cache root override.")

(defun nemacs-vendor-cache-set-repo-root ()
  "Return the repository root."
  (nemacs-vendor-cache-repo-root))

(defun nemacs-vendor-cache-set-root ()
  "Return the build root for vendor set artifacts."
  (expand-file-name (or nemacs-vendor-cache-set-root-override
                        "build/nelisp-artifacts/vendor-set")
                    (nemacs-vendor-cache-set-repo-root)))

(defun nemacs-vendor-cache-set--bind-root ()
  "Bind the single-file cache helpers to the set cache root."
  (setq nemacs-vendor-cache-root-override
        (or nemacs-vendor-cache-set-batch-root-override
            nemacs-vendor-cache-set-root-override
            (nemacs-vendor-cache-set-root))))

(defun nemacs-vendor-cache-set-vendor-root ()
  "Return the top-level vendor Elisp directory."
  (expand-file-name "vendor/emacs-lisp" (nemacs-vendor-cache-set-repo-root)))

(defun nemacs-vendor-cache-set-org-root ()
  "Return the vendored Org directory."
  (expand-file-name "org" (nemacs-vendor-cache-set-vendor-root)))

(defun nemacs-vendor-cache-set-emacs-lisp-root ()
  "Return the vendored `emacs-lisp/' directory."
  (expand-file-name "emacs-lisp" (nemacs-vendor-cache-set-vendor-root)))

(defun nemacs-vendor-cache-set-cl-lib-path ()
  "Return the vendored `cl-lib.el' path."
  (expand-file-name "cl-lib.el" (nemacs-vendor-cache-set-emacs-lisp-root)))

(defun nemacs-vendor-cache-set-load-paths ()
  "Return load paths for vendor-set compile/load operations."
  (delete-dups
   (append
    (list (nemacs-vendor-cache-set-vendor-root)
          (nemacs-vendor-cache-set-org-root)
          (nemacs-vendor-cache-set-emacs-lisp-root))
    (nemacs-vendor-cache-load-paths))))

(defun nemacs-vendor-cache-set-preloads ()
  "Return the small preload set folded into every cache key."
  (list (nemacs-vendor-cache-set-cl-lib-path)))

(defun nemacs-vendor-cache-set--source-path (name relative-path)
  "Return source path for NAME, defaulting to RELATIVE-PATH."
  (expand-file-name
   (or (cdr (assq name nemacs-vendor-cache-set-batch-source-overrides))
       relative-path)
   (nemacs-vendor-cache-set-repo-root)))

(defun nemacs-vendor-cache-set-format-spec-entry ()
  "Return the `format-spec.el' cache entry."
  (list :name 'format-spec
        :source-path (nemacs-vendor-cache-set--source-path
                      'format-spec
                      "vendor/emacs-lisp/format-spec.el")
        :requested-feature 'format-spec
        :dependencies nil
        :preloads (nemacs-vendor-cache-set-preloads)
        :load-paths (nemacs-vendor-cache-set-load-paths)
        :proof-function 'nemacs-vendor-cache-proof-format-spec))

(defun nemacs-vendor-cache-set-org-version-entry ()
  "Return the `org-version.el' cache entry."
  (list :name 'org-version
        :source-path (nemacs-vendor-cache-set--source-path
                      'org-version
                      "vendor/emacs-lisp/org/org-version.el")
        :requested-feature 'org-version
        :dependencies nil
        :preloads (nemacs-vendor-cache-set-preloads)
        :load-paths (nemacs-vendor-cache-set-load-paths)
        :proof-function 'nemacs-vendor-cache-set-proof-org-version))

(defun nemacs-vendor-cache-set-org-macs-entry ()
  "Return the `org-macs.el' candidate entry."
  (list :name 'org-macs
        :source-path (nemacs-vendor-cache-set--source-path
                      'org-macs
                      "vendor/emacs-lisp/org/org-macs.el")
        :requested-feature 'org-macs
        :dependencies '(format-spec org-version)
        :preloads (nemacs-vendor-cache-set-preloads)
        :load-paths (nemacs-vendor-cache-set-load-paths)
        :proof-function 'nemacs-vendor-cache-set-proof-org-macs))

(defun nemacs-vendor-cache-set-candidate-entries ()
  "Return the candidate dependency-ordered vendor chain."
  (list (nemacs-vendor-cache-set-format-spec-entry)
        (nemacs-vendor-cache-set-org-version-entry)
        (nemacs-vendor-cache-set-org-macs-entry)))

(defun nemacs-vendor-cache-set-dropped-candidates ()
  "Return dropped candidate metadata."
  (list (list :name 'org-macs
              :reason nemacs-vendor-cache-set--org-macs-drop-reason)))

(defun nemacs-vendor-cache-set-default-entries ()
  "Return the chosen cacheable vendor set for this host."
  ;; `org-macs.el' source-loads, but current NeLisp artifact compilation
  ;; cannot read it due an unsupported string escape in the file.
  (list (nemacs-vendor-cache-set-format-spec-entry)
        (nemacs-vendor-cache-set-org-version-entry)))

(defun nemacs-vendor-cache-set-proof-org-version ()
  "Return the proof tuple for `org-version.el'."
  (list (featurep 'org-version)
        (fboundp 'org-release)
        (org-release)))

(defun nemacs-vendor-cache-set-proof-org-macs ()
  "Return a stable proof tuple for `org-macs.el'."
  (list (featurep 'org-macs)
        (fboundp 'org-string-nw-p)
        (org-string-nw-p " x ")))

(defun nemacs-vendor-cache-set--proof (entry)
  "Run ENTRY's proof function."
  (funcall (plist-get entry :proof-function)))

(defun nemacs-vendor-cache-set--entry-by-name (entries name)
  "Return the entry named NAME from ENTRIES."
  (cl-find name entries :key (lambda (entry) (plist-get entry :name))))

(defun nemacs-vendor-cache-set-dependency-order (&optional entries)
  "Return ENTRIES sorted by dependencies."
  (let ((entries (or entries (nemacs-vendor-cache-set-default-entries)))
        ordered
        active
        done)
    (cl-labels
        ((visit (entry)
           (let ((name (plist-get entry :name)))
             (when (memq name active)
               (error "dependency cycle involving %S" name))
             (unless (memq name done)
               (push name active)
               (dolist (dep-name (plist-get entry :dependencies))
                 (let ((dep (nemacs-vendor-cache-set--entry-by-name entries dep-name)))
                   (unless dep
                     (error "missing dependency %S for %S" dep-name name))
                   (visit dep)))
               (setq active (delq name active))
               (push name done)
               (push entry ordered)))))
      (dolist (entry entries)
        (visit entry))
      (nreverse ordered))))

(defun nemacs-vendor-cache-set--unique-preloads (entries)
  "Return a stable unique preload list for ENTRIES."
  (let (paths)
    (dolist (entry entries)
      (dolist (path (plist-get entry :preloads))
        (unless (member path paths)
          (push path paths))))
    (nreverse paths)))

(defun nemacs-vendor-cache-set--load-preloads (entries)
  "Load ENTRIES preload files into the host."
  (dolist (path (nemacs-vendor-cache-set--unique-preloads entries))
    (load path nil t)))

(defun nemacs-vendor-cache-set--read-tracker (tracked-paths reads-box)
  "Return advice that records TRACKED-PATHS reads into READS-BOX."
  (lambda (original path &rest args)
    (let* ((truename (ignore-errors (file-truename path)))
           (name (and truename (cdr (assoc truename tracked-paths)))))
      (when name
        (push (list :name name :path truename) (car reads-box))))
    (apply original path args)))

(defun nemacs-vendor-cache-set--read-events-for (events name)
  "Return tracked source read EVENTS for NAME."
  (let (paths)
    (dolist (event events)
      (when (eq (plist-get event :name) name)
        (push (plist-get event :path) paths)))
    (nreverse paths)))

(defun nemacs-vendor-cache-set--run-entry (entry source-read-events)
  "Load ENTRY through the cache layer and collect proof metadata."
  (let* ((name (plist-get entry :name))
         (start (float-time))
         (meta (nemacs-vendor-cache-load-file entry))
         (elapsed (- (float-time) start))
         (record (plist-get meta :record)))
    (list :name name
          :source-path (plist-get entry :source-path)
          :tuple (nemacs-vendor-cache-set--proof entry)
          :mode (plist-get meta :mode)
          :cache-status (plist-get meta :cache-status)
          :compiled (plist-get meta :compiled)
          :compile-error (plist-get meta :compile-error)
          :elapsed elapsed
          :key (plist-get record :key)
          :artifact-path (plist-get record :artifact-path)
          :manifest-path (plist-get record :manifest-path)
          :artifact-exists (file-readable-p (plist-get record :artifact-path))
          :manifest-exists (file-readable-p (plist-get record :manifest-path))
          :source-read-paths
          (nemacs-vendor-cache-set--read-events-for
           source-read-events
           name))))

(defun nemacs-vendor-cache-set-run (&optional entries)
  "Run the vendor set cache proof for ENTRIES."
  (let* ((entries (nemacs-vendor-cache-set-dependency-order
                   (or entries (nemacs-vendor-cache-set-default-entries))))
         (tracked-paths
          (mapcar (lambda (entry)
                    (cons (file-truename (plist-get entry :source-path))
                          (plist-get entry :name)))
                  entries))
         (reads-box (list nil))
         (artifact-reader
          (nemacs-vendor-cache-set--read-tracker tracked-paths reads-box))
         (insert-reader
          (nemacs-vendor-cache-set--read-tracker tracked-paths reads-box))
         (results nil)
         (aggregate-elapsed 0.0))
    (let ((nemacs-vendor-cache-root-override nil))
      (nemacs-vendor-cache-set--bind-root)
      (make-directory nemacs-vendor-cache-root-override t)
      (nelisp--reset)
      (setq nelisp-artifact--loaded nil)
      (unwind-protect
          (progn
            (advice-add 'nelisp-artifact--read-file-as-string :around artifact-reader)
            (advice-add 'insert-file-contents :around insert-reader)
            (advice-add 'insert-file-contents-literally :around insert-reader)
            (nemacs-vendor-cache-set--load-preloads entries)
            (dolist (entry entries)
              (let ((result (nemacs-vendor-cache-set--run-entry
                             entry (car reads-box))))
                (setq aggregate-elapsed (+ aggregate-elapsed
                                           (plist-get result :elapsed)))
                (push result results)))
            (setq results (nreverse results))
            (list :entries results
                  :aggregate-proof
                  (mapcar (lambda (result)
                            (list (plist-get result :name)
                                  (plist-get result :tuple)))
                          results)
                  :aggregate-elapsed aggregate-elapsed
                  :source-read-events (nreverse (car reads-box))
                  :selected-set (mapcar (lambda (entry) (plist-get entry :name))
                                        entries)
                  :dropped-candidates (nemacs-vendor-cache-set-dropped-candidates)
                  :preloads (nemacs-vendor-cache-set--unique-preloads entries)
                  :cache-root nemacs-vendor-cache-root-override))
        (advice-remove 'insert-file-contents-literally insert-reader)
        (advice-remove 'insert-file-contents insert-reader)
        (advice-remove 'nelisp-artifact--read-file-as-string artifact-reader)))))

(defun nemacs-vendor-cache-set-emacs-binary ()
  "Return the current Emacs binary path."
  (nemacs-vendor-cache-emacs-binary))

(defun nemacs-vendor-cache-set--batch-args ()
  "Return batch Emacs arguments for vendor-cache-set subprocesses."
  (list "-Q" "--batch"
        "-L" (expand-file-name "scripts" (nemacs-vendor-cache-set-repo-root))
        "-L" nemacs-vendor-cache--nelisp-src-dir
        "-L" nemacs-vendor-cache--nelisp-lisp-dir
        "-l" (expand-file-name "scripts/nemacs-vendor-cache-set.el"
                               (nemacs-vendor-cache-set-repo-root))))

(defun nemacs-vendor-cache-set--binding-args (bindings)
  "Return `--eval' pairs for BINDINGS."
  (let (args)
    (dolist (binding bindings)
      (setq args
            (append args
                    (list "--eval"
                          (format "(setq %s '%S)"
                                  (car binding)
                                  (cdr binding))))))
    args))

(defun nemacs-vendor-cache-set-run-subprocess (entrypoint &optional bindings)
  "Run ENTRYPOINT in a fresh batch Emacs with optional BINDINGS."
  (let* ((buffer (generate-new-buffer " *nemacs-vendor-cache-set*"))
         (args (append (nemacs-vendor-cache-set--batch-args)
                       (nemacs-vendor-cache-set--binding-args bindings)
                       (list "-f" (symbol-name entrypoint))))
         status)
    (unwind-protect
        (with-current-buffer buffer
          (setq status (apply #'call-process
                              (nemacs-vendor-cache-set-emacs-binary)
                              nil t nil args))
          (unless (and (integerp status) (zerop status))
            (error "Vendor cache set subprocess %s failed (exit=%S):\n%s"
                   entrypoint status (buffer-string)))
          (goto-char (point-min))
          (read (current-buffer)))
      (kill-buffer buffer))))

(defun nemacs-vendor-cache-set-batch-proof ()
  "Batch entry point printing set cache proof."
  (prin1 (nemacs-vendor-cache-set-run))
  (terpri))

(provide 'nemacs-vendor-cache-set)

;;; nemacs-vendor-cache-set.el ends here
