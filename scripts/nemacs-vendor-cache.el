;;; nemacs-vendor-cache.el --- Doc 142 vendor .nelc cache layer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nelisp-bytecode)

(defconst nemacs-vendor-cache--nelisp-root
  "/home/madblack-21/Cowork/Notes/dev/nelisp")
(defconst nemacs-vendor-cache--nelisp-src-dir
  (expand-file-name "src" nemacs-vendor-cache--nelisp-root))
(defconst nemacs-vendor-cache--nelisp-lisp-dir
  (expand-file-name "lisp" nemacs-vendor-cache--nelisp-root))
(defconst nemacs-vendor-cache--index-format
  'nemacs-vendor-cache-index-v1)
(defconst nemacs-vendor-cache--kind 'nelc)

(defvar nemacs-vendor-cache-root-override nil
  "Override root for vendor cache artifacts.
Nil means `build/nelisp-artifacts' under the repo root.")

(defvar nemacs-vendor-cache-batch-source-path nil
  "Batch test source path override.")

(defun nemacs-vendor-cache-repo-root ()
  "Return the repository root."
  (let* ((origin (or load-file-name buffer-file-name default-directory))
         (path (expand-file-name origin))
         (dir (if (file-directory-p path)
                  path
                (file-name-directory path))))
    (if (file-exists-p (expand-file-name "vendor/emacs-lisp/format-spec.el" dir))
        dir
      (expand-file-name ".." dir))))

(defun nemacs-vendor-cache-root ()
  "Return the build root for vendor artifacts."
  (expand-file-name (or nemacs-vendor-cache-root-override
                        "build/nelisp-artifacts")
                    (nemacs-vendor-cache-repo-root)))

(defun nemacs-vendor-cache-index-path ()
  "Return the derived cache index path."
  (expand-file-name "index.el" (nemacs-vendor-cache-root)))

(defun nemacs-vendor-cache-load-paths ()
  "Return NeLisp tool load paths used for cache compilation."
  (list nemacs-vendor-cache--nelisp-lisp-dir
        nemacs-vendor-cache--nelisp-src-dir))

(defun nemacs-vendor-cache-format-spec-entry (&optional source-path)
  "Return the dependency-free cache entry for format-spec.el."
  (list :name 'format-spec
        :source-path (expand-file-name
                      (or source-path
                          (expand-file-name "vendor/emacs-lisp/format-spec.el"
                                            (nemacs-vendor-cache-repo-root))))
        :requested-feature 'format-spec
        :preloads nil
        :load-paths (nemacs-vendor-cache-load-paths)
        :proof-function 'nemacs-vendor-cache-proof-format-spec))

(defun nemacs-vendor-cache-default-entries ()
  "Return the current minimal dependency-free vendor set."
  (list (nemacs-vendor-cache-format-spec-entry)))

(defun nemacs-vendor-cache--read-file (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun nemacs-vendor-cache--read-lisp-file (path)
  "Read one top-level Lisp object from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

(defun nemacs-vendor-cache--write-lisp-file (path object)
  "Write OBJECT to PATH with a trailing newline."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (prin1-to-string object) "\n")
                  nil path nil 'silent)))

(defun nemacs-vendor-cache--read-host-forms (path)
  "Read top-level host Elisp forms from PATH."
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

(defun nemacs-vendor-cache--defun-names (forms)
  "Return top-level defun names from FORMS."
  (let (names)
    (dolist (form forms)
      (when (and (consp form)
                 (eq (car form) 'defun)
                 (symbolp (nth 1 form)))
        (push (nth 1 form) names)))
    (nreverse names)))

(defun nemacs-vendor-cache--extract-provided-feature (form)
  "Return the feature symbol provided by FORM, or nil."
  (when (and (consp form)
             (eq (car form) 'provide))
    (let ((arg (nth 1 form)))
      (cond
       ((symbolp arg) arg)
       ((and (consp arg)
             (eq (car arg) 'quote)
             (symbolp (nth 1 arg)))
        (nth 1 arg))))))

(defun nemacs-vendor-cache--collect-features (forms)
  "Return provided features from FORMS."
  (let (features)
    (dolist (form forms)
      (let ((feature (nemacs-vendor-cache--extract-provided-feature form)))
        (when (and feature (not (memq feature features)))
          (push feature features))))
    (nreverse features)))

(defun nemacs-vendor-cache--source-metadata-from-forms (entry forms contents)
  "Build metadata for ENTRY from FORMS and CONTENTS."
  (let* ((source-path (expand-file-name (plist-get entry :source-path)))
         (attrs (file-attributes source-path))
         (size (if (fboundp 'file-attribute-size)
                   (file-attribute-size attrs)
                 (nth 7 attrs)))
         (mtime (if (fboundp 'file-attribute-modification-time)
                    (file-attribute-modification-time attrs)
                  (nth 5 attrs))))
    (list :path source-path
          :truename (file-truename source-path)
          :sha256 (secure-hash 'sha256 contents)
          :size size
          :mtime mtime
          :features (nemacs-vendor-cache--collect-features forms)
          :top-level-count (length forms)
          :defun-names (nemacs-vendor-cache--defun-names forms))))

(defun nemacs-vendor-cache--entry-descriptor (entry)
  "Return the source-independent cache descriptor for ENTRY."
  (list :source-truename (file-truename (plist-get entry :source-path))
        :kind nemacs-vendor-cache--kind
        :artifact-format nelisp-artifact--format
        :manifest-format nelisp-artifact--manifest-format
        :compiler (nelisp-artifact--compiler-plist)
        :runtime-abi nelisp-artifact--runtime-abi
        :requested-feature (plist-get entry :requested-feature)
        :preload-paths (mapcar #'expand-file-name (plist-get entry :preloads))
        :load-paths (mapcar #'expand-file-name (plist-get entry :load-paths))))

(defun nemacs-vendor-cache--index-id (entry)
  "Return the derived index key for ENTRY."
  (secure-hash 'sha256
               (prin1-to-string
                (nemacs-vendor-cache--entry-descriptor entry))))

(defun nemacs-vendor-cache--read-index ()
  "Read the derived index plist."
  (let ((path (nemacs-vendor-cache-index-path)))
    (if (file-readable-p path)
        (let ((index (nemacs-vendor-cache--read-lisp-file path)))
          (if (eq (plist-get index :format) nemacs-vendor-cache--index-format)
              index
            (list :format nemacs-vendor-cache--index-format :entries nil)))
      (list :format nemacs-vendor-cache--index-format :entries nil))))

(defun nemacs-vendor-cache--write-index (index)
  "Persist INDEX."
  (nemacs-vendor-cache--write-lisp-file (nemacs-vendor-cache-index-path) index))

(defun nemacs-vendor-cache--index-get (entry)
  "Return the cached index entry for ENTRY."
  (cdr (assoc (nemacs-vendor-cache--index-id entry)
              (plist-get (nemacs-vendor-cache--read-index) :entries))))

(defun nemacs-vendor-cache--index-put (entry state cache-key)
  "Store STATE and CACHE-KEY for ENTRY in the derived index."
  (let* ((id (nemacs-vendor-cache--index-id entry))
         (index (nemacs-vendor-cache--read-index))
         (entries (plist-get index :entries))
         (record (list :cache-key cache-key
                       :state (list :path (plist-get state :path)
                                    :truename (plist-get state :truename)
                                    :sha256 (plist-get state :sha256)
                                    :size (plist-get state :size)
                                    :mtime (plist-get state :mtime)
                                    :features (plist-get state :features)
                                    :top-level-count (plist-get state :top-level-count)
                                    :defun-names (plist-get state :defun-names)))))
    (setf (alist-get id entries nil nil #'equal) record)
    (nemacs-vendor-cache--write-index
     (list :format nemacs-vendor-cache--index-format
           :entries entries))))

(defun nemacs-vendor-cache--cached-state-current-p (entry cached-state)
  "Return non-nil when CACHED-STATE still matches ENTRY on disk."
  (when cached-state
    (let* ((path (plist-get entry :source-path))
           (attrs (file-attributes path))
           (size (if (fboundp 'file-attribute-size)
                     (file-attribute-size attrs)
                   (nth 7 attrs)))
           (mtime (if (fboundp 'file-attribute-modification-time)
                      (file-attribute-modification-time attrs)
                    (nth 5 attrs))))
      (and (equal size (plist-get cached-state :size))
           (equal mtime (plist-get cached-state :mtime))
           (equal (file-truename path) (plist-get cached-state :truename))))))

(defun nemacs-vendor-cache--read-source-state (entry)
  "Read ENTRY source and return metadata plus FORMS."
  (let* ((contents (nemacs-vendor-cache--read-file (plist-get entry :source-path)))
         (forms (nemacs-vendor-cache--read-host-forms (plist-get entry :source-path)))
         (meta (nemacs-vendor-cache--source-metadata-from-forms entry forms contents)))
    (list :meta meta :forms forms :from-index nil)))

(defun nemacs-vendor-cache--source-state (entry &optional force-read)
  "Return current source state for ENTRY.
When FORCE-READ is non-nil, bypass the derived index."
  (let* ((index-record (unless force-read
                         (nemacs-vendor-cache--index-get entry)))
         (cached-state (plist-get index-record :state)))
    (if (and cached-state
             (nemacs-vendor-cache--cached-state-current-p entry cached-state))
        (list :meta cached-state
              :forms nil
              :from-index t
              :cached-key (plist-get index-record :cache-key))
      (let ((state (nemacs-vendor-cache--read-source-state entry)))
        (plist-put state :cached-key (plist-get index-record :cache-key))
        state))))

(defun nemacs-vendor-cache--preload-records (entry)
  "Return preload records for ENTRY."
  (mapcar (lambda (path)
            (let ((full (expand-file-name path)))
              (list :path full
                    :sha256 (secure-hash 'sha256
                                         (nemacs-vendor-cache--read-file full)))))
          (plist-get entry :preloads)))

(defun nemacs-vendor-cache-cache-key (entry source-state &optional preload-records)
  "Return the Doc 142 cache key for ENTRY and SOURCE-STATE."
  (secure-hash
   'sha256
   (prin1-to-string
    (list :source-sha256 (plist-get source-state :sha256)
          :kind nemacs-vendor-cache--kind
          :artifact-format nelisp-artifact--format
          :manifest-format nelisp-artifact--manifest-format
          :compiler (nelisp-artifact--compiler-plist)
          :runtime-abi nelisp-artifact--runtime-abi
          :requested-feature (plist-get entry :requested-feature)
          :preloads (or preload-records
                        (nemacs-vendor-cache--preload-records entry))
          :load-paths (mapcar #'expand-file-name
                              (plist-get entry :load-paths))))))

(defun nemacs-vendor-cache-cache-record (entry source-state &optional preload-records)
  "Return the cache record for ENTRY and SOURCE-STATE."
  (let* ((preload-records (or preload-records
                              (nemacs-vendor-cache--preload-records entry)))
         (key (nemacs-vendor-cache-cache-key entry source-state preload-records))
         (root (nemacs-vendor-cache-root)))
    (list :key key
          :preloads preload-records
          :artifact-path (expand-file-name (concat "nelc/" key ".nelc") root)
          :manifest-path (expand-file-name (concat "manifests/" key ".manifest.el")
                                           root)
          :sibling-manifest-path
          (expand-file-name (concat "nelc/" key ".nelc.manifest.el") root))))

(defun nemacs-vendor-cache--expected-manifest (entry source-state record artifact-content)
  "Return the exact authoritative manifest expected for ENTRY."
  (list :format nelisp-artifact--manifest-format
        :kind nemacs-vendor-cache--kind
        :artifact-format nelisp-artifact--format
        :artifact-class nelisp-artifact--artifact-class
        :runtime-abi nelisp-artifact--runtime-abi
        :artifact-sha256 (secure-hash 'sha256 artifact-content)
        :nelisp-version (if (boundp 'nelisp--cli-version)
                            nelisp--cli-version
                          "unknown")
        :target (or (and (boundp 'system-configuration) system-configuration)
                    "unknown")
        :source (list :path (plist-get source-state :path)
                      :truename (plist-get source-state :truename)
                      :sha256 (plist-get source-state :sha256)
                      :size (plist-get source-state :size)
                      :mtime (plist-get source-state :mtime))
        :preloads (plist-get record :preloads)
        :load-path (mapcar #'expand-file-name (plist-get entry :load-paths))
        :features (plist-get source-state :features)
        :top-level-count (plist-get source-state :top-level-count)
        :compiler (nelisp-artifact--compiler-plist)
        :entry (list :type 'module-init
                     :id (file-name-nondirectory (plist-get source-state :path)))))

(defun nemacs-vendor-cache--manifest-valid-p (entry source-state record)
  "Return non-nil when RECORD is a valid authoritative cache hit for ENTRY."
  (let ((artifact-path (plist-get record :artifact-path))
        (manifest-path (plist-get record :manifest-path)))
    (when (and (file-readable-p artifact-path)
               (file-readable-p manifest-path))
      (let* ((artifact-content (nemacs-vendor-cache--read-file artifact-path))
             (manifest (nemacs-vendor-cache--read-lisp-file manifest-path))
             (expected (nemacs-vendor-cache--expected-manifest
                        entry source-state record artifact-content)))
        (equal manifest expected)))))

(defun nemacs-vendor-cache--sync-sibling-manifest (record)
  "Mirror RECORD's authoritative manifest beside the artifact for loading."
  (let ((manifest-path (plist-get record :manifest-path))
        (sibling-path (plist-get record :sibling-manifest-path)))
    ;; `nelisp-artifact-load-file' currently insists on a sibling manifest.
    ;; Keep the manifest under `manifests/' authoritative and mirror it here.
    (make-directory (file-name-directory sibling-path) t)
    (copy-file manifest-path sibling-path t)))

(defun nemacs-vendor-cache--artifact-function-names (artifact-path)
  "Return defun names materialized by ARTIFACT-PATH."
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

(defun nemacs-vendor-cache--materialize-host-from-artifact (artifact-path)
  "Install ARTIFACT-PATH's definitions onto the host function/value cells."
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

(defun nemacs-vendor-cache--load-artifact-into-host (record)
  "Load RECORD's artifact via `.nelc' and reflect it into host cells."
  (nemacs-vendor-cache--sync-sibling-manifest record)
  (nelisp-artifact-load-file (plist-get record :artifact-path))
  (nemacs-vendor-cache--materialize-host-from-artifact
   (plist-get record :artifact-path)))

(defun nemacs-vendor-cache--eval-source-into-host (entry)
  "Read and eval ENTRY source through host Elisp."
  (dolist (form (nemacs-vendor-cache--read-host-forms (plist-get entry :source-path)))
    (eval form t)))

(defun nemacs-vendor-cache--compile-record (entry record)
  "Compile ENTRY into RECORD."
  (make-directory (file-name-directory (plist-get record :artifact-path)) t)
  (make-directory (file-name-directory (plist-get record :manifest-path)) t)
  (nelisp-artifact-compile-file
   (plist-get entry :source-path)
   (plist-get record :artifact-path)
   (plist-get record :manifest-path)
   nil
   (plist-get entry :load-paths)
   (plist-get entry :preloads)
   (plist-get entry :requested-feature)
   nemacs-vendor-cache--kind))

(defun nemacs-vendor-cache-ensure-record (entry &optional force-read)
  "Return the cache record metadata for ENTRY.
This compiles a missing or invalid cache entry and updates the derived index."
  (let* ((state-wrap (nemacs-vendor-cache--source-state entry force-read))
         (state (plist-get state-wrap :meta))
         (record (nemacs-vendor-cache-cache-record entry state))
         (cached-key (plist-get state-wrap :cached-key))
         (had-previous-cache cached-key)
         (hit (nemacs-vendor-cache--manifest-valid-p entry state record)))
    (if hit
        (progn
          (nemacs-vendor-cache--index-put entry state (plist-get record :key))
          (list :cache-status 'hit
                :compiled nil
                :record record
                :source-state state))
      (let* ((fresh-wrap (if force-read
                             state-wrap
                           (nemacs-vendor-cache--source-state entry t)))
             (fresh-state (plist-get fresh-wrap :meta))
             (fresh-record (nemacs-vendor-cache-cache-record entry fresh-state)))
        (nemacs-vendor-cache--compile-record entry fresh-record)
        (nemacs-vendor-cache--index-put entry fresh-state (plist-get fresh-record :key))
        (list :cache-status (if had-previous-cache 'recompiled 'miss)
              :compiled t
              :record fresh-record
              :source-state fresh-state)))))

(defun nemacs-vendor-cache-load-file (entry)
  "Load ENTRY via a valid `.nelc' artifact, else source-eval and compile."
  (let* ((state-wrap (nemacs-vendor-cache--source-state entry))
         (state (plist-get state-wrap :meta))
         (record (nemacs-vendor-cache-cache-record entry state))
         (hit (nemacs-vendor-cache--manifest-valid-p entry state record)))
    (if hit
        (progn
          (nemacs-vendor-cache--index-put entry state (plist-get record :key))
          (nemacs-vendor-cache--load-artifact-into-host record)
          (list :mode 'artifact
                :cache-status 'hit
                :compiled nil
                :record record
                :source-state state))
      (let* ((fresh-wrap (nemacs-vendor-cache--source-state entry t))
             (fresh-state (plist-get fresh-wrap :meta))
             (fresh-record (nemacs-vendor-cache-cache-record entry fresh-state))
             (had-previous-cache (plist-get state-wrap :cached-key))
             (compile-error nil))
        (nemacs-vendor-cache--eval-source-into-host entry)
        (condition-case err
            (progn
              (nemacs-vendor-cache--compile-record entry fresh-record)
              (nemacs-vendor-cache--index-put entry fresh-state
                                              (plist-get fresh-record :key)))
          (error
           (setq compile-error err)))
        (list :mode 'source
              :cache-status (if had-previous-cache 'recompiled 'miss)
              :compiled (null compile-error)
              :compile-error compile-error
              :record fresh-record
              :source-state fresh-state)))))

(defun nemacs-vendor-cache-build-default-set ()
  "Compile or reuse the current dependency-free default vendor set."
  (mapcar #'nemacs-vendor-cache-ensure-record
          (nemacs-vendor-cache-default-entries)))

(defun nemacs-vendor-cache-proof-format-spec ()
  "Return the proof tuple for format-spec.el."
  (list (featurep 'format-spec)
        (fboundp 'format-spec)
        (format-spec "%a" (list (cons ?a "x")))))

(defun nemacs-vendor-cache--proof (entry)
  "Run ENTRY's proof function."
  (funcall (plist-get entry :proof-function)))

(defun nemacs-vendor-cache--reset-host-state (entry &optional state)
  "Remove ENTRY's feature and defun bindings from the host."
  (let* ((feature (plist-get entry :requested-feature))
         (names (or (plist-get state :defun-names)
                    (plist-get (plist-get (nemacs-vendor-cache--source-state entry t)
                                          :meta)
                               :defun-names))))
    (setq features (delq feature features))
    (dolist (name names)
      (when (fboundp name)
        (fmakunbound name)))))

(defun nemacs-vendor-cache-source-proof (entry)
  "Replay ENTRY source and return its proof plist."
  (let* ((state (plist-get (nemacs-vendor-cache--source-state entry t) :meta))
         (feature (plist-get entry :requested-feature))
         before-feature before-fboundp)
    (nemacs-vendor-cache--reset-host-state entry state)
    (setq before-feature (featurep feature)
          before-fboundp (fboundp feature))
    (nemacs-vendor-cache--eval-source-into-host entry)
    (list :tuple (nemacs-vendor-cache--proof entry)
          :before-feature before-feature
          :before-fboundp before-fboundp)))

(defun nemacs-vendor-cache-load-proof (entry)
  "Load ENTRY through the cache layer and return its proof plist."
  (let* ((state (plist-get (nemacs-vendor-cache--source-state entry) :meta))
         (feature (plist-get entry :requested-feature))
         (source-truename (file-truename (plist-get entry :source-path)))
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
            (apply original path args)))
         (elapsed 0.0)
         load-meta)
    (nemacs-vendor-cache--reset-host-state entry state)
    (setq before-feature (featurep feature)
          before-fboundp (fboundp feature))
    (nelisp--reset)
    (setq nelisp-artifact--loaded nil)
    (unwind-protect
        (progn
          (advice-add 'nelisp-artifact--read-file-as-string :around content-reader)
          (advice-add 'insert-file-contents :around insert-reader)
          (advice-add 'insert-file-contents-literally :around insert-reader)
          (let ((start (float-time)))
            (setq load-meta (nemacs-vendor-cache-load-file entry)
                  elapsed (- (float-time) start)))
          (list :tuple (nemacs-vendor-cache--proof entry)
                :before-feature before-feature
                :before-fboundp before-fboundp
                :source-read-paths (nreverse source-read-paths)
                :elapsed elapsed
                :mode (plist-get load-meta :mode)
                :cache-status (plist-get load-meta :cache-status)
                :compiled (plist-get load-meta :compiled)
                :compile-error (plist-get load-meta :compile-error)
                :key (plist-get (plist-get load-meta :record) :key)
                :artifact-path (plist-get (plist-get load-meta :record) :artifact-path)
                :manifest-path (plist-get (plist-get load-meta :record) :manifest-path)))
      (advice-remove 'insert-file-contents-literally insert-reader)
      (advice-remove 'insert-file-contents insert-reader)
      (advice-remove 'nelisp-artifact--read-file-as-string content-reader))))

(defun nemacs-vendor-cache-emacs-binary ()
  "Return the current Emacs binary path."
  (expand-file-name invocation-name invocation-directory))

(defun nemacs-vendor-cache--batch-args ()
  "Return batch Emacs arguments for vendor-cache subprocesses."
  (list "-Q" "--batch"
        "-L" (expand-file-name "scripts" (nemacs-vendor-cache-repo-root))
        "-L" nemacs-vendor-cache--nelisp-src-dir
        "-L" nemacs-vendor-cache--nelisp-lisp-dir
        "-l" (expand-file-name "scripts/nemacs-vendor-cache.el"
                               (nemacs-vendor-cache-repo-root))))

(defun nemacs-vendor-cache--binding-args (bindings)
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

(defun nemacs-vendor-cache-run-subprocess (entrypoint &optional bindings)
  "Run ENTRYPOINT in a fresh batch Emacs with optional BINDINGS."
  (let* ((buffer (generate-new-buffer " *nemacs-vendor-cache*"))
         (args (append (nemacs-vendor-cache--batch-args)
                       (nemacs-vendor-cache--binding-args bindings)
                       (list "-f" (symbol-name entrypoint))))
         status)
    (unwind-protect
        (with-current-buffer buffer
          (setq status (apply #'call-process
                              (nemacs-vendor-cache-emacs-binary)
                              nil t nil args))
          (unless (and (integerp status) (zerop status))
            (error "Vendor cache subprocess %s failed (exit=%S):\n%s"
                   entrypoint status (buffer-string)))
          (goto-char (point-min))
          (read (current-buffer)))
      (kill-buffer buffer))))

(defun nemacs-vendor-cache-batch-source-proof ()
  "Batch entry point printing source replay proof."
  (prin1 (nemacs-vendor-cache-source-proof
          (nemacs-vendor-cache-format-spec-entry
           nemacs-vendor-cache-batch-source-path)))
  (terpri))

(defun nemacs-vendor-cache-batch-load-proof ()
  "Batch entry point printing cache-layer proof."
  (prin1 (nemacs-vendor-cache-load-proof
          (nemacs-vendor-cache-format-spec-entry
           nemacs-vendor-cache-batch-source-path)))
  (terpri))

(provide 'nemacs-vendor-cache)

;;; nemacs-vendor-cache.el ends here
