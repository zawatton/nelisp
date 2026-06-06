;;; nemacs-artifact-gate5.el --- Doc 142 Gate 5 proof helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defconst nemacs-artifact-gate5--nelisp-root
  ;; Prefer NEMACS_NELISP_ROOT (set by the Makefile), then a vendored
  ;; vendor/nelisp snapshot beside this repo, then the legacy sibling
  ;; checkout. Keeps gate5 decoupled from a frequently-rebuilt ../nelisp.
  (or (getenv "NEMACS_NELISP_ROOT")
      (let ((vendored
             (expand-file-name
              "vendor/nelisp"
              (file-name-directory
               (directory-file-name
                (file-name-directory
                 (or load-file-name buffer-file-name default-directory)))))))
        (and (file-directory-p vendored) vendored))
      "/home/madblack-21/Cowork/Notes/dev/nelisp"))
(defconst nemacs-artifact-gate5--nelisp-src-dir
  (expand-file-name "src" nemacs-artifact-gate5--nelisp-root))
(defconst nemacs-artifact-gate5--nelisp-lisp-dir
  (expand-file-name "lisp" nemacs-artifact-gate5--nelisp-root))
(defconst nemacs-artifact-gate5--feature 'format-spec)

(defvar nemacs-artifact-gate5-artifact-path nil
  "Artifact path used by `nemacs-artifact-gate5-batch-artifact-proof'.")

(defun nemacs-artifact-gate5-repo-root ()
  "Return the repository root."
  (let* ((origin (or load-file-name buffer-file-name default-directory))
         (path (expand-file-name origin))
         (dir (if (file-directory-p path)
                  path
                (file-name-directory path))))
    (if (file-exists-p (expand-file-name "vendor/emacs-lisp/format-spec.el" dir))
        dir
      (expand-file-name ".." dir))))

(defun nemacs-artifact-gate5-source-path ()
  "Return the real vendor source file used for Gate 5."
  (expand-file-name "vendor/emacs-lisp/format-spec.el"
                    (nemacs-artifact-gate5-repo-root)))

(defun nemacs-artifact-gate5-cache-root ()
  "Return the build cache root for Gate 5 artifacts."
  (expand-file-name "build/nelisp-artifacts"
                    (nemacs-artifact-gate5-repo-root)))

(defun nemacs-artifact-gate5-load-paths ()
  "Return the NeLisp tool paths required by the Gate 5 helpers."
  (list nemacs-artifact-gate5--nelisp-lisp-dir
        nemacs-artifact-gate5--nelisp-src-dir))

(defun nemacs-artifact-gate5--read-file (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun nemacs-artifact-gate5--read-host-forms (path)
  "Read every top-level host Elisp form from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let (forms form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (push form forms))
        (end-of-file nil))
      (nreverse forms))))

(defun nemacs-artifact-gate5--defun-names (forms)
  "Return top-level defun names from FORMS."
  (let (names)
    (dolist (form forms)
      (when (and (consp form)
                 (eq (car form) 'defun)
                 (symbolp (nth 1 form)))
        (push (nth 1 form) names)))
    (nreverse names)))

(defun nemacs-artifact-gate5--proof-tuple ()
  "Return the Gate 5 proof tuple."
  (list (featurep nemacs-artifact-gate5--feature)
        (fboundp 'format-spec)
        (format-spec "%a" (list (cons ?a "x")))))

(defun nemacs-artifact-gate5--reset-host-state (function-names)
  "Remove the Gate 5 bindings for FUNCTION-NAMES from the host."
  (setq features (delq nemacs-artifact-gate5--feature features))
  (dolist (name function-names)
    (when (fboundp name)
      (fmakunbound name))))

(defun nemacs-artifact-gate5-source-proof ()
  "Replay the vendor source through host read/eval and return its proof."
  (let* ((forms (nemacs-artifact-gate5--read-host-forms
                 (nemacs-artifact-gate5-source-path)))
         (function-names (nemacs-artifact-gate5--defun-names forms))
         (before-feature nil)
         (before-fboundp nil))
    (nemacs-artifact-gate5--reset-host-state function-names)
    (setq before-feature (featurep nemacs-artifact-gate5--feature)
          before-fboundp (fboundp 'format-spec))
    (dolist (form forms)
      (eval form t))
    (list :tuple (nemacs-artifact-gate5--proof-tuple)
          :before-feature before-feature
          :before-fboundp before-fboundp)))

(defun nemacs-artifact-gate5-cache-key ()
  "Return the Gate 5 cache key.
The key pins the source content plus the NeLisp artifact format,
manifest format, compiler format, requested feature, preloads, and
tool load paths."
  (require 'nelisp-artifact)
  (secure-hash
   'sha256
   (prin1-to-string
    (list :source-sha256
          (secure-hash 'sha256
                       (nemacs-artifact-gate5--read-file
                        (nemacs-artifact-gate5-source-path)))
          :kind 'nelc
          :artifact-format nelisp-artifact--format
          :manifest-format nelisp-artifact--manifest-format
          :compiler (nelisp-artifact--compiler-plist)
          :runtime-abi nelisp-artifact--runtime-abi
          :requested-feature nemacs-artifact-gate5--feature
          :preloads nil
          :load-paths (nemacs-artifact-gate5-load-paths)))))

(defun nemacs-artifact-gate5-cache-record ()
  "Return the cache record plist for the current Gate 5 source."
  (let* ((key (nemacs-artifact-gate5-cache-key))
         (root (nemacs-artifact-gate5-cache-root)))
    (list :key key
          :artifact-path (expand-file-name (concat "nelc/" key ".nelc") root)
          :manifest-path (expand-file-name (concat "manifests/" key ".manifest.el")
                                           root))))

(defun nemacs-artifact-gate5-compile-cache-record ()
  "Compile the real vendor file into the Gate 5 cache record."
  (require 'nelisp-artifact)
  (let* ((record (nemacs-artifact-gate5-cache-record))
         (artifact-path (plist-get record :artifact-path))
         (manifest-path (plist-get record :manifest-path)))
    (make-directory (file-name-directory artifact-path) t)
    (make-directory (file-name-directory manifest-path) t)
    (nelisp-artifact-compile-file
     (nemacs-artifact-gate5-source-path)
     artifact-path
     manifest-path
     nil
     (nemacs-artifact-gate5-load-paths)
     nil
     nemacs-artifact-gate5--feature
     'nelc)
    record))

(defun nemacs-artifact-gate5-make-load-pair (record)
  "Return a temporary sibling artifact+manifest pair for RECORD.
The cache record stays in `build/nelisp-artifacts/', while the load pair
matches `nelisp-artifact-load-file''s current sibling-manifest contract."
  (let* ((dir (make-temp-file "nemacs-gate5-load-" t))
         (artifact-path (expand-file-name
                         (file-name-nondirectory (plist-get record :artifact-path))
                         dir))
         (manifest-path (concat artifact-path ".manifest.el")))
    (copy-file (plist-get record :artifact-path) artifact-path t)
    (copy-file (plist-get record :manifest-path) manifest-path t)
    (list :dir dir
          :artifact-path artifact-path
          :manifest-path manifest-path)))

(defun nemacs-artifact-gate5--artifact-function-names (artifact-path)
  "Return defun names materialized by ARTIFACT-PATH."
  (require 'nelisp-artifact)
  (let* ((payload (nelisp-artifact--read-payload artifact-path))
         (module (plist-get payload :module-init))
         names)
    (dolist (item module)
      (cond
       ((and (consp item)
             (eq (car item) :fn)
             (symbolp (nth 1 item)))
        (push (nth 1 item) names))
       ((and (consp item)
             (eq (car item) :eval))
        (let ((form (nth 1 item)))
          (when (and (consp form)
                     (eq (car form) 'defun)
                     (symbolp (nth 1 form)))
            (push (nth 1 form) names))))))
    (nreverse (delete-dups names))))

(defun nemacs-artifact-gate5--materialize-host-from-artifact (artifact-path)
  "Install ARTIFACT-PATH's module onto host function/value cells."
  (require 'nelisp-artifact)
  (require 'nelisp-bytecode)
  (let* ((payload (nelisp-artifact--read-payload artifact-path))
         (module (plist-get payload :module-init))
         (features (plist-get payload :features)))
    (dolist (item module)
      (cond
       ((and (consp item)
             (eq (car item) :fn))
        (nelisp-bc--defun-from-elc (nth 1 item) (nth 2 item)))
       ((and (consp item)
             (eq (car item) :eval))
        (eval (nth 1 item) t))
       (t
        (eval item t))))
    (dolist (feature features)
      (unless (featurep feature)
        (provide feature)))))

(defun nemacs-artifact-gate5-artifact-proof (artifact-path)
  "Load only ARTIFACT-PATH, expose it to host Elisp, and return its proof."
  (require 'nelisp-artifact)
  (let* ((function-names
          (nemacs-artifact-gate5--artifact-function-names artifact-path))
         (source-truename
          (file-truename (nemacs-artifact-gate5-source-path)))
         (source-read-paths nil)
         (before-feature nil)
         (before-fboundp nil)
         (content-reader
          (lambda (original path &rest args)
            (let ((truename (ignore-errors (file-truename path))))
              (when (and truename (equal truename source-truename))
                (push truename source-read-paths)))
            (apply original path args)))
         (insert-reader
          (lambda (original path &rest args)
            (let ((truename (ignore-errors (file-truename path))))
              (when (and truename (equal truename source-truename))
                (push truename source-read-paths)))
            (apply original path args))))
    (nemacs-artifact-gate5--reset-host-state function-names)
    (setq before-feature (featurep nemacs-artifact-gate5--feature)
          before-fboundp (fboundp 'format-spec))
    (nelisp--reset)
    (setq nelisp-artifact--loaded nil)
    (unwind-protect
        (progn
          (advice-add 'nelisp-artifact--read-file-as-string :around content-reader)
          (advice-add 'insert-file-contents :around insert-reader)
          (advice-add 'insert-file-contents-literally :around insert-reader)
          (nelisp-artifact-load-file artifact-path)
          (nemacs-artifact-gate5--materialize-host-from-artifact artifact-path)
          (list :tuple (nemacs-artifact-gate5--proof-tuple)
                :before-feature before-feature
                :before-fboundp before-fboundp
                :source-read-paths (nreverse source-read-paths)))
      (advice-remove 'insert-file-contents-literally insert-reader)
      (advice-remove 'insert-file-contents insert-reader)
      (advice-remove 'nelisp-artifact--read-file-as-string content-reader))))

(defun nemacs-artifact-gate5-emacs-binary ()
  "Return the current Emacs binary path."
  (expand-file-name invocation-name invocation-directory))

(defun nemacs-artifact-gate5--batch-args ()
  "Return the base Emacs batch arguments for Gate 5 subprocesses."
  (list "-Q" "--batch"
        "-L" (expand-file-name "scripts" (nemacs-artifact-gate5-repo-root))
        "-L" nemacs-artifact-gate5--nelisp-src-dir
        "-L" nemacs-artifact-gate5--nelisp-lisp-dir
        "-l" (expand-file-name "scripts/nemacs-artifact-gate5.el"
                               (nemacs-artifact-gate5-repo-root))))

(defun nemacs-artifact-gate5--binding-args (bindings)
  "Return `--eval' pairs for BINDINGS."
  (let (args)
    (dolist (binding bindings)
      (setq args
            (append args
                    (list "--eval"
                          (format "(setq %s %S)"
                                  (car binding)
                                  (cdr binding))))))
    args))

(defun nemacs-artifact-gate5-run-subprocess (entrypoint &optional bindings)
  "Run ENTRYPOINT in a fresh batch Emacs with optional BINDINGS."
  (let* ((buffer (generate-new-buffer " *nemacs-gate5*"))
         (args (append (nemacs-artifact-gate5--batch-args)
                       (nemacs-artifact-gate5--binding-args bindings)
                       (list "-f" (symbol-name entrypoint))))
         status)
    (unwind-protect
        (with-current-buffer buffer
          (setq status (apply #'call-process
                              (nemacs-artifact-gate5-emacs-binary)
                              nil t nil args))
          (unless (and (integerp status) (zerop status))
            (error "Gate 5 subprocess %s failed (exit=%S):\n%s"
                   entrypoint status (buffer-string)))
          (goto-char (point-min))
          (read (current-buffer)))
      (kill-buffer buffer))))

(defun nemacs-artifact-gate5-batch-source-proof ()
  "Batch entry point printing the source replay proof."
  (prin1 (nemacs-artifact-gate5-source-proof))
  (terpri))

(defun nemacs-artifact-gate5-batch-artifact-proof ()
  "Batch entry point printing the artifact replay proof."
  (unless (and (stringp nemacs-artifact-gate5-artifact-path)
               (file-readable-p nemacs-artifact-gate5-artifact-path))
    (error "nemacs-artifact-gate5-artifact-path is not readable: %S"
           nemacs-artifact-gate5-artifact-path))
  (prin1 (nemacs-artifact-gate5-artifact-proof
          nemacs-artifact-gate5-artifact-path))
  (terpri))

(provide 'nemacs-artifact-gate5)

;;; nemacs-artifact-gate5.el ends here
