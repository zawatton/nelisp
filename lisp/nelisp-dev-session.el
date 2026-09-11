;;; nelisp-dev-session.el --- bounded JSON session adapter -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'nelisp-dev-protocol)
(require 'nelisp-repl-session)

;; This is an interchange manifest, rather than an Elisp data file.
(defconst nelisp-dev-session-version "1")
(defconst nelisp-dev-session-max-records 128)
(defconst nelisp-dev-session-max-form-bytes 65536)
(defconst nelisp-dev-session-max-file-bytes (* 16 1024 1024))
(defconst nelisp-dev-session-max-manifest-bytes (* 1024 1024))
(defconst nelisp-dev-session-max-environment-chars 1024)

(defun nelisp-dev-session--error (kind &rest data)
  (append (list :kind kind) data))

(defun nelisp-dev-session--context-get (context key)
  "Read KEY from a keyword plist or a string-key JSON alist CONTEXT."
  (let ((keyword (intern (concat ":" key)))
        (colon (concat ":" key)))
    (cond
     ((and (listp context) (keywordp (car context)))
     (plist-get context keyword))
     ((listp context)
      (or (cdr (assoc key context))
          (cdr (assoc (replace-regexp-in-string "-" "_" key) context))
          (cdr (assoc colon context)))))))

(defun nelisp-dev-session--request-name (request)
  (cond
   ((symbolp request) (symbol-name request))
   ((stringp request) request)
   ((and (listp request) (stringp (cdr (assoc "operation" request))))
    (cdr (assoc "operation" request)))
   (t nil)))

(defun nelisp-dev-session--request-argument (request key)
  (let ((arguments (and (listp request) (cdr (assoc "arguments" request)))))
    (and (listp arguments)
         (or (cdr (assoc key arguments))
             (cdr (assoc (replace-regexp-in-string "-" "_" key) arguments))))))

(defun nelisp-dev-session--effective-context (request context)
  "Copy trusted context and add only supported request arguments.
Session identity and root are intentionally taken from CONTEXT, never from
request arguments supplied by a command line client."
  (let ((effective (list :session-id
                         (nelisp-dev-session--context-get context "session-id")
                         :root (nelisp-dev-session--context-get context "root"))))
    (dolist (key '("recipe" "manifest" "effects-policy"))
      (let ((value (nelisp-dev-session--request-argument request key)))
        (when value
          (setq effective
                (plist-put effective (intern (concat ":" key)) value)))))
    ;; Direct adapter callers may provide these keys in context as well.
    (dolist (key '("recipe" "manifest" "effects-policy"))
      (unless (nelisp-dev-session--request-argument request key)
        (let ((value (nelisp-dev-session--context-get context key)))
          (when value
            (setq effective
                  (plist-put effective (intern (concat ":" key)) value))))))
    (dolist (key '(:recipe :manifest))
      (when (stringp (plist-get effective key))
        (setq effective
              (plist-put effective key
                         (expand-file-name (plist-get effective key)
                                           (plist-get effective :root))))))
    effective))

(defun nelisp-dev-session--file-size (path)
  (when (file-regular-p path)
    (file-attribute-size (file-attributes path 'string))))

(defun nelisp-dev-session--sha256 (path &optional limit)
  "Hash PATH within LIMIT bytes, refusing a file that grows while read."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq limit (or limit nelisp-dev-session-max-file-bytes))
    (insert-file-contents-literally path nil 0 (1+ limit))
    (when (> (buffer-size) limit) (error "Session hash input exceeds byte limit"))
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-dev-session--sha256-p (value)
  (and (stringp value)
       (string-match-p "\\`[0-9a-fA-F]\\{64\\}\\'" value)))

(defun nelisp-dev-session--bounded-text-p (value)
  (and (stringp value)
       (<= (string-bytes value) nelisp-dev-session-max-environment-chars)))

(defun nelisp-dev-session--relative-path-p (value)
  (and (stringp value) (> (length value) 0)
       (not (file-name-absolute-p value))
       (not (string-match-p "\\0" value))))

(defun nelisp-dev-session--json-get (object key)
  (cdr (assoc key object)))

(defun nelisp-dev-session--json-object-p (object)
  (and (proper-list-p object)
       (cl-every (lambda (pair) (and (consp pair) (stringp (car pair)))) object)
       (= (length object) (length (delete-dups (mapcar #'car object))))))

(defun nelisp-dev-session--relative (path directory)
  (let ((relative (file-relative-name (expand-file-name path)
                                      (expand-file-name directory))))
    (unless (nelisp-dev-session--relative-path-p relative)
      (error "Session path is not relative: %S" path))
    relative))

(defun nelisp-dev-session--path-collision-p (path-a path-b)
  (equal (expand-file-name path-a) (expand-file-name path-b)))

(defun nelisp-dev-session--record-sources (records)
  "Check the live registry and return source descriptors and errors."
  (let ((sources nil) (errors nil) (count 0))
    (if (not (listp records))
        (list :sources nil :errors
              (list (nelisp-dev-session--error :records-schema)))
      (dolist (record records)
        (setq count (1+ count))
        (cond
         ((not (listp record))
          (push (nelisp-dev-session--error :record-schema) errors))
         ((eq (plist-get record :kind) :form)
          (let ((form (plist-get record :form)))
            (unless (and (stringp form)
                         (<= (string-bytes form)
                             nelisp-dev-session-max-form-bytes))
              (push (nelisp-dev-session--error :form-schema) errors))))
         ((eq (plist-get record :kind) :load)
          (let* ((path (plist-get record :path))
                 (expected (plist-get record :sha256))
                 (size (and (stringp path) (nelisp-dev-session--file-size path))))
            (cond
             ((not (and (stringp path) (file-regular-p path)
                        (file-readable-p path)))
              (push (nelisp-dev-session--error :source-missing :path path)
                    errors))
             ((not (and (integerp size)
                        (<= size nelisp-dev-session-max-file-bytes)))
              (push (nelisp-dev-session--error :source-too-large :path path)
                    errors))
             ((not (nelisp-dev-session--sha256-p expected))
              (push (nelisp-dev-session--error :source-sha-schema :path path)
                    errors))
             ((not (equal (downcase expected)
                          (downcase (nelisp-dev-session--sha256 path))))
              (push (nelisp-dev-session--error :source-sha-mismatch :path path)
                    errors))
             (t (push (list :path path :sha256 (downcase expected) :bytes size)
                      sources)))))
         (t (push (nelisp-dev-session--error :record-schema) errors))))
      (when (> count nelisp-dev-session-max-records)
        (push (nelisp-dev-session--error :record-limit
                                          :limit nelisp-dev-session-max-records)
              errors))
      (list :sources (nreverse sources) :errors (nreverse errors)))))

(defun nelisp-dev-session--environment ()
  (let ((system (format "%s" system-type)))
    (list (cons "system_type"
                (substring system 0
                           (min (length system)
                                nelisp-dev-session-max-environment-chars)))
          (cons "emacs_version"
                (substring emacs-version
                           0 (min (length emacs-version)
                                  nelisp-dev-session-max-environment-chars))))))

(defun nelisp-dev-session--write-json (path data)
  (make-directory (file-name-directory (expand-file-name path)) t)
  (let ((json-encoding-pretty-print nil)
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert (json-encode data) "\n"))))

(defun nelisp-dev-session--manifest-data
    (session-id recipe manifest records sources)
  (let ((directory (file-name-directory (expand-file-name manifest))))
    (list
     (cons "schema_version" nelisp-dev-session-version)
     (cons "session_id" session-id)
     (cons "recipe" (nelisp-dev-session--relative recipe directory))
     (cons "recipe_sha256" (nelisp-dev-session--sha256 recipe))
     (cons "recipe_bytes" (nelisp-dev-session--file-size recipe))
     (cons "sources"
           (vconcat
            (mapcar (lambda (source)
                      (list (cons "path"
                                  (nelisp-dev-session--relative
                                   (plist-get source :path) directory))
                            (cons "sha256" (plist-get source :sha256))
                            (cons "bytes" (plist-get source :bytes))))
                    sources)))
     (cons "limitations"
           ["explicit-records-only"
            "validation-does-not-load-or-evaluate"
            "replay-requires-an-explicit-effects-policy"])
     (cons "record_count" (length records))
     (cons "environment" (nelisp-dev-session--environment)))))

(defun nelisp-dev-session--export (context)
  (let* ((session-id (nelisp-dev-session--context-get context "session-id"))
         (recipe (nelisp-dev-session--context-get context "recipe"))
         (manifest (nelisp-dev-session--context-get context "manifest"))
         ;; Export always snapshots the registry owned by the REPL session.
         (records (copy-tree nelisp-repl-session--records)))
    (cond
     ((or (null session-id) (eq session-id :null))
      (list :status :rejected :phase :session
            :errors (list (nelisp-dev-session--error :nil-session))))
     ((not (and (stringp recipe) (stringp manifest)
                (> (length recipe) 0) (> (length manifest) 0)))
      (list :status :rejected :phase :arguments
            :errors (list (nelisp-dev-session--error :path-schema))))
     ((> (length records) nelisp-dev-session-max-records)
      (list :status :rejected :phase :records
            :errors (list (nelisp-dev-session--error
                           :record-limit :limit nelisp-dev-session-max-records))))
     (t
      (let* ((sources-result (nelisp-dev-session--record-sources records))
             (errors (plist-get sources-result :errors))
             (sources (plist-get sources-result :sources))
             (recipe (expand-file-name recipe))
             (manifest (expand-file-name manifest))
             (collision
              (or (nelisp-dev-session--path-collision-p recipe manifest)
                  (cl-some
                   (lambda (source)
                     (or (nelisp-dev-session--path-collision-p
                          recipe (plist-get source :path))
                         (nelisp-dev-session--path-collision-p
                          manifest (plist-get source :path))))
                   sources))))
        (if collision
            (list :status :rejected :phase :arguments
                  :errors (list (nelisp-dev-session--error :path-collision)))
          (if errors
            (list :status :rejected :phase :records :errors errors)
            (condition-case err
              (progn
                (make-directory (file-name-directory recipe) t)
                (let ((nelisp-repl-session--records records))
                  (nelisp-repl-session-export recipe))
                (nelisp-dev-session--write-json
                 manifest
                 (nelisp-dev-session--manifest-data
                  session-id recipe manifest records sources))
                (list :status :exported :version nelisp-dev-session-version
                      :recipe recipe :manifest manifest
                      :record-count (length records)
                      :source-count (length sources)))
            (error
             (list :status :rejected :phase :export
                   :errors (list (nelisp-dev-session--error
                   :export-error :message
                                  (error-message-string err)))))))))))))

(defun nelisp-dev-session--read-json (manifest)
  (cond
   ((not (and (file-regular-p manifest) (file-readable-p manifest)))
    (list :errors (list (nelisp-dev-session--error :manifest-missing
                                                   :path manifest))))
   ((let ((size (nelisp-dev-session--file-size manifest)))
      (or (not (integerp size))
          (> size nelisp-dev-session-max-manifest-bytes)))
    (list :errors (list (nelisp-dev-session--error :manifest-too-large))))
   (t
    (condition-case err
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents manifest nil 0
                                  (1+ nelisp-dev-session-max-manifest-bytes)))
          (when (> (buffer-size) nelisp-dev-session-max-manifest-bytes)
            (error "Manifest grew beyond input budget"))
          (list :data
                (nelisp-dev-protocol-string-keys
                 (json-parse-string (buffer-string) :object-type 'alist
                                    :array-type 'array :null-object :null
                                    :false-object :false))))
      (error
       (list :errors (list (nelisp-dev-session--error
                            :manifest-read :message
                            (error-message-string err)))))))))

(defun nelisp-dev-session--validate-file (directory descriptor kind)
  (if (not (nelisp-dev-session--json-object-p descriptor))
      (list (nelisp-dev-session--error :descriptor-schema :kind kind))
   (let* ((path (nelisp-dev-session--json-get descriptor "path"))
         (sha (nelisp-dev-session--json-get descriptor "sha256"))
         (bytes (nelisp-dev-session--json-get descriptor "bytes"))
         (absolute (and (nelisp-dev-session--relative-path-p path)
                        (expand-file-name path directory)))
         (size (and absolute (nelisp-dev-session--file-size absolute)))
         errors)
    (unless (nelisp-dev-session--relative-path-p path)
      (push (nelisp-dev-session--error :path-schema :kind kind :path path)
            errors))
    (unless (nelisp-dev-session--sha256-p sha)
      (push (nelisp-dev-session--error :sha-schema :kind kind :path path)
            errors))
    (unless (and (integerp bytes) (<= 0 bytes nelisp-dev-session-max-file-bytes))
      (push (nelisp-dev-session--error :size-schema :kind kind :path path)
            errors))
    ;; Keep independent failures: a tampered file can have both a size and a
    ;; digest mismatch, and validation must report both rather than hiding one.
    (when (and absolute (not (file-regular-p absolute)))
      (push (nelisp-dev-session--error :missing :kind kind :path path) errors))
    (when (and (integerp size) (> size nelisp-dev-session-max-file-bytes))
      (push (nelisp-dev-session--error :too-large :kind kind :path path) errors))
    (when (and (integerp size) (integerp bytes) (/= size bytes))
      (push (nelisp-dev-session--error :size-mismatch :kind kind :path path
                                       :expected bytes :actual size) errors))
    (when (and absolute (file-regular-p absolute) (integerp size)
               (<= size nelisp-dev-session-max-file-bytes)
               (nelisp-dev-session--sha256-p sha)
               (not (equal (downcase sha)
                           (downcase (nelisp-dev-session--sha256 absolute)))))
      (push (nelisp-dev-session--error :sha-mismatch :kind kind :path path)
            errors))
    (nreverse errors))))

(defun nelisp-dev-session--validate (context)
  "Validate a JSON manifest without loading or evaluating any referenced file."
  (let* ((manifest-value (nelisp-dev-session--context-get context "manifest"))
         (manifest (and (stringp manifest-value)
                        (expand-file-name manifest-value)))
         (read-result (and manifest (nelisp-dev-session--read-json manifest)))
         (errors (or (plist-get read-result :errors)
                     (unless manifest
                       (list (nelisp-dev-session--error :manifest-schema)))))
         (data (plist-get read-result :data)))
    (if errors
        (list :status :invalid :errors errors)
      (if (not (nelisp-dev-session--json-object-p data))
          (list :status :invalid
                :errors (list (nelisp-dev-session--error :manifest-schema)))
        (let* ((directory (file-name-directory manifest))
               (version (nelisp-dev-session--json-get data "schema_version"))
               (session-id (nelisp-dev-session--json-get data "session_id"))
               (recipe (nelisp-dev-session--json-get data "recipe"))
               (recipe-sha (nelisp-dev-session--json-get data "recipe_sha256"))
               (recipe-bytes (nelisp-dev-session--json-get data "recipe_bytes"))
               (sources (nelisp-dev-session--json-get data "sources"))
               (limitations (nelisp-dev-session--json-get data "limitations"))
               (environment (nelisp-dev-session--json-get data "environment")))
          (dolist (required '("schema_version" "session_id" "recipe" "recipe_sha256"
                              "recipe_bytes" "sources" "limitations" "record_count" "environment"))
            (unless (assoc required data)
              (push (nelisp-dev-session--error :required-field :field required)
                    errors)))
          (unless (equal version nelisp-dev-session-version)
            (push (nelisp-dev-session--error :unknown-version :value version)
                  errors))
          (unless (and (stringp session-id) (> (length session-id) 0))
            (push (nelisp-dev-session--error :session-id-schema) errors))
          (unless (nelisp-dev-session--relative-path-p recipe)
            (push (nelisp-dev-session--error :recipe-schema :path recipe) errors))
          (unless (vectorp sources)
            (push (nelisp-dev-session--error :sources-schema) errors))
          (when (and (vectorp sources)
                     (> (length sources) nelisp-dev-session-max-records))
            (push (nelisp-dev-session--error :source-limit
                                              :limit nelisp-dev-session-max-records)
                  errors))
          (unless (vectorp limitations)
            (push (nelisp-dev-session--error :limitations-schema) errors))
          (unless (and (vectorp limitations) (<= (length limitations) 128))
            (push (nelisp-dev-session--error :limitations-limit) errors))
          (let ((count (nelisp-dev-session--json-get data "record_count")))
            (unless (and (integerp count) (<= 0 count nelisp-dev-session-max-records))
              (push (nelisp-dev-session--error :record-count-schema) errors)))
          (when (and (vectorp limitations) (<= (length limitations) 128))
            (dotimes (i (length limitations))
              (unless (nelisp-dev-session--bounded-text-p (aref limitations i))
                (push (nelisp-dev-session--error :limitation-schema) errors))))
          (unless (and (nelisp-dev-session--json-object-p environment)
                       (nelisp-dev-session--bounded-text-p
                        (nelisp-dev-session--json-get environment "system_type"))
                       (nelisp-dev-session--bounded-text-p
                        (nelisp-dev-session--json-get environment "emacs_version")))
            (push (nelisp-dev-session--error :environment-schema) errors))
          (when recipe
            (setq errors
                  (append (nelisp-dev-session--validate-file
                           directory
                           (list (cons "path" recipe)
                                 (cons "sha256" recipe-sha)
                                 (cons "bytes" recipe-bytes))
                           :recipe)
                          errors)))
          (when (and (vectorp sources) (<= (length sources) nelisp-dev-session-max-records))
            (dotimes (i (length sources))
              (let ((source (aref sources i)))
                (if (listp source)
                    (setq errors
                          (append (nelisp-dev-session--validate-file
                                   directory source :source)
                                  errors))
                  (push (nelisp-dev-session--error :source-schema) errors)))))
          (if errors
              (list :status :invalid :errors (nreverse errors))
            (list :status :valid :version version :manifest manifest
                  :session-id session-id :source-count (length sources))))))))

(defun nelisp-dev-session--replay (context)
  (let ((policy (nelisp-dev-session--context-get context "effects-policy"))
        (validation (nelisp-dev-session--validate context)))
    (cond
     ((not (member policy '("explicit-only" :explicit-only explicit-only)))
      (list :status :unsupported :phase :policy
            :reason :explicit-effects-policy-required :validation validation))
     ((not (eq (plist-get validation :status) :valid))
      (list :status :rejected :phase :validate :validation validation))
     (t
      (list :status :unsupported :phase :replay
            :reason :explicit-effects-replayer-unavailable
            :validation validation)))))

(defun nelisp-dev-session--wire-error (error-data)
  (let ((kind (plist-get error-data :kind))
        (path (plist-get error-data :path))
        (message (plist-get error-data :message)))
    (append (list (cons "code" (if (symbolp kind)
                                    (symbol-name kind)
                                  "session-error")))
            (and path (list (cons "path" path)))
            (and message (list (cons "message" message))))))

(defun nelisp-dev-session--wire-data (result)
  (let ((data nil))
    (dolist (key '(:version :recipe :manifest :record-count :source-count
                   :session-id))
      (let ((value (plist-get result key)))
        (when value
          (push (cons (substring (symbol-name key) 1) value) data))))
    (nreverse data)))

(defun nelisp-dev-session--wire-result (operation request-id result)
  (let* ((internal (plist-get result :status))
         (status (cond ((memq internal '(:exported :valid)) "ok")
                       ((memq internal '(:unsupported)) "unsupported")
                       ((memq internal '(:invalid :rejected)) "failed")
                       (t "failed")))
         (errors (plist-get result :errors))
         (diagnostics (vconcat (mapcar #'nelisp-dev-session--wire-error errors)))
         (summary (list (cons "errors" (length errors)))))
    (nelisp-dev-protocol-envelope
     operation request-id status nil summary diagnostics
     (nelisp-dev-session--wire-data result)
     (if (eq internal :unsupported)
         (vector (format "%s" (or (plist-get result :reason)
                                   "operation is not implemented")))
       ["Validation checks registered file hashes and sizes only."
         "Native runtime, build options and dependency compatibility remain unverified."
         "No recipe was executed; replay requires a separate supported operation."]))))

(defun nelisp-dev-session-dispatch (request context)
  "Dispatch a bounded development session REQUEST using JSON CONTEXT.

REQUEST may be an operation string or JSON request alist.  CONTEXT may be a
keyword plist or a string-key alist.  Export uses the live REPL registry and
rejects a nil session id.  Validation only reads and hashes files."
  (let* ((name (nelisp-dev-session--request-name request))
         (request-id (and (listp request) (cdr (assoc "request_id" request))))
         (effective (nelisp-dev-session--effective-context request context))
         (result (cond
                  ((equal name "session.export")
                   (nelisp-dev-session--export effective))
                  ((equal name "session.validate")
                   (nelisp-dev-session--validate effective))
                  ((equal name "session.replay")
                   (nelisp-dev-session--replay effective))
                  (t (list :status :unsupported :reason :unknown-request)))))
    (nelisp-dev-session--wire-result (or name "unknown") request-id result)))

(provide 'nelisp-dev-session)
;;; nelisp-dev-session.el ends here
