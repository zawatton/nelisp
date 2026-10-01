;;; doc211-source-roots.el --- resolve imported package source roots -*- lexical-binding: t; -*-

(require 'nelisp-pkg)

(defvar doc211-source-root-dirs-cache nil
  "Cached source roots keyed by package root and manifest fingerprint.")

(defun doc211-source-root--fingerprint (root)
  "Return a cheap fingerprint for source-root inputs below ROOT."
  (let ((packages-dir (expand-file-name "packages" root)))
    (mapcar
     (lambda (dir)
       (let* ((manifest (expand-file-name "manifest.el" dir))
              (attributes (file-attributes manifest 'string))
              (src (expand-file-name "src" dir))
              (sources (and (file-directory-p src)
                            (directory-files src t "\\.el\\'"))))
         (list dir
               (file-directory-p src)
               (and attributes
                    (list (file-attribute-modification-time attributes)
                          (file-attribute-status-change-time attributes)
                          (file-attribute-size attributes)
                          (file-attribute-inode-number attributes)))
               (mapcar
                (lambda (file)
                  (let ((attrs (file-attributes file 'string)))
                    (list file
                          (file-attribute-modification-time attrs)
                          (file-attribute-status-change-time attrs)
                          (file-attribute-size attrs)
                          (file-attribute-inode-number attrs))))
                sources))))
     (and (file-directory-p packages-dir)
          (cl-remove-if-not
           #'file-directory-p
           (directory-files packages-dir t "\\`[^.]"))))))

(defun doc211-source-root-dirs-invalidate (&optional root)
  "Invalidate cached roots for ROOT, or clear all roots when nil."
  (setq doc211-source-root-dirs-cache
        (if root
            (assoc-delete-all (expand-file-name root)
                              doc211-source-root-dirs-cache)
          nil)))

(defun doc211-source-root-dirs (&optional root)
  "Return ordered package source roots and the moved test root below ROOT.

The cache is local to this Emacs invocation.  Set
`DOC211_SOURCE_ROOTS_SNAPSHOT' only for read-only batch consumers to reuse the
first resolution without repeating its filesystem fingerprint."
  (let* ((root (expand-file-name (or root default-directory)))
         (cached (assoc root doc211-source-root-dirs-cache))
         (snapshot (getenv "DOC211_SOURCE_ROOTS_SNAPSHOT"))
         (fingerprint (unless (and cached snapshot)
                        (doc211-source-root--fingerprint root))))
    (or (and cached
             (or snapshot (equal fingerprint (cadr cached)))
             (copy-sequence (caddr cached)))
        (let* ((fingerprint (or fingerprint
                                (doc211-source-root--fingerprint root)))
               (packages (nelisp-pkg-scan root))
               (resolution (nelisp-pkg-resolve packages))
               (names (append (plist-get resolution :order)
                              (plist-get resolution :cycles)))
               (dirs nil))
          (dolist (name names)
            (let* ((pkg (cl-find name packages
                                 :key (lambda (entry) (plist-get entry :name))
                                 :test #'equal))
                   (src (expand-file-name "src" (plist-get pkg :dir))))
              (when (file-directory-p src) (push src dirs))))
          (setq dirs
                (append (nreverse dirs)
                        (list (expand-file-name "test/nelisp-emacs-lib" root))))
          (setq doc211-source-root-dirs-cache
                (cons (list root fingerprint dirs)
                      (assoc-delete-all root doc211-source-root-dirs-cache)))
          dirs))))

(when (getenv "DOC211_SOURCE_ROOTS_PRINT")
  (dolist (dir (doc211-source-root-dirs))
    (princ (concat (file-relative-name dir default-directory) "\n"))))

;;; doc211-source-roots.el ends here
