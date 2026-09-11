;;; nelisp-dev-reload.el --- process-local native GC reload plans -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Private records own staged handles; reports contain only copied identities.
(require 'cl-lib)
(require 'nelisp-dev)
(require 'nelisp-runtime-development)

(defconst nelisp-dev-reload--ttl 900)
(defconst nelisp-dev-reload--max-plans 64)
(defvar nelisp-dev-reload--session-id nil)
(defvar nelisp-dev-reload--plans (make-hash-table :test #'equal))
(defconst nelisp-dev-reload--limitations
  ["Single-use, process-local allocator/GC unit; no arbitrary native caller closure or heap migration."
   "Compiler-directory contents are checked conservatively; host compiler libraries and external dynamic loads are not hermetic."
   "Filesystem checks are observations, not locks; do not edit inputs concurrently with build or publication."
   "Expired or cleared plans lose authority; mapped code remains until process exit."])

(defun nelisp-dev-reload--digest (value)
  (let ((print-length nil) (print-level nil) (print-circle nil))
    (secure-hash 'sha256 (prin1-to-string value))))

(defun nelisp-dev-reload-context (&optional root)
  "Create a live native GC context, refusing processes without the opt-in runtime."
  (unless (and (eq system-type 'gnu/linux)
               (eq (plist-get (nelisp-runtime-reload-status) :status) 'ready))
    (error "NELISP-DEV-NATIVE-UNAVAILABLE: opt-in Linux runtime required"))
  (unless nelisp-dev-reload--session-id
    (setq nelisp-dev-reload--session-id
          (nelisp-dev-reload--digest (list (current-time) (random)))))
  (list :root (file-name-as-directory (expand-file-name (or root default-directory)))
        :target "native-linux-x86_64" :live-session t
        :session-id (copy-sequence nelisp-dev-reload--session-id)
        :adapters (list (cons "reload.plan" #'nelisp-dev-reload-plan-dispatch)
                        (cons "reload.apply" #'nelisp-dev-reload-apply-dispatch))))

(defun nelisp-dev-reload-clear ()
  "Revoke all local plans and contexts without publishing or unmapping code."
  (let ((count (hash-table-count nelisp-dev-reload--plans)))
    (clrhash nelisp-dev-reload--plans)
    (setq nelisp-dev-reload--session-id nil)
    count))

(defun nelisp-dev-reload--purge ()
  (let ((now (float-time)) expired)
    (maphash (lambda (id record)
               (when (<= (plist-get record :expires) now) (push id expired)))
             nelisp-dev-reload--plans)
    (dolist (id expired) (remhash id nelisp-dev-reload--plans))))

(defun nelisp-dev-reload--sha256-file (path)
  "Hash literal file bytes with a 16 MiB bound; missing files are errors."
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "NELISP-DEV-STALE-PLAN: unreadable input %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (1+ (* 16 1024 1024)))
    (when (> (- (point-max) (point-min)) (* 16 1024 1024))
      (error "NELISP-DEV-INPUT-LIMIT: file exceeds 16 MiB"))
    (secure-hash 'sha256 (buffer-string))))

(defun nelisp-dev-reload--inputs (root)
  "Identify the compiler search directories, not a general dependency closure."
  (let ((total 0) paths)
    (dolist (directory '("lisp" "src" "scripts"))
      (dolist (path (directory-files (expand-file-name directory root) t "\\.elc?\\'"))
        (unless (file-regular-p path)
          (error "NELISP-DEV-STALE-PLAN: nonregular compiler input"))
        (setq total (+ total (file-attribute-size (file-attributes path))))
        (push path paths)
        (when (or (> (length paths) 1024) (> total (* 32 1024 1024)))
          (error "NELISP-DEV-INPUT-LIMIT: compiler set exceeds 1024 files or 32 MiB"))))
    (mapcar (lambda (path)
              (cons (substring path (length (file-name-as-directory (expand-file-name root))))
                    (nelisp-dev-reload--sha256-file path)))
            (sort paths #'string<))))

(defun nelisp-dev-reload--options ()
  "Digest the compiler's named environment inputs without exposing their values."
  (nelisp-dev-reload--digest
   (mapcar (lambda (name) (cons name (getenv name)))
           '("EMACS" "PATH" "EMACSLOADPATH" "TMPDIR"
             "NELISP_STANDALONE_TARGET" "NELISP_LINUX_ARENA_SIZE"
             "NELISP_RUNTIME_RELOAD" "NELISP_TCO" "NELISP_T104_FLAT_DISPATCH"
             "NELISP_READER_DYNAMIC" "NELISP_SRC" "NELISP_CHUNK_IDX"
             "NELISP_CHUNK_N" "NELISP_FORM_A" "NELISP_FORM_B" "NELISP_FORM_OP"))))

(defun nelisp-dev-reload--runtime ()
  (let* ((binary (nelisp-native-load--running-binary-sha256))
         (abi (nelisp-runtime-reload-contract-hash))
         (state (nelisp-runtime-reload-status)))
    (unless (and (eq (plist-get state :status) 'ready)
                 (integerp (plist-get state :generation))
                 (stringp binary) (stringp abi))
      (error "NELISP-DEV-STALE-PLAN: native identity unavailable"))
    (list :binary binary :abi abi :generation (plist-get state :generation))))

(defun nelisp-dev-reload--guard (request context operation keys)
  (unless (and (equal (plist-get context :target) "native-linux-x86_64")
               (eq (plist-get context :live-session) t)
               (stringp nelisp-dev-reload--session-id)
               (equal (plist-get context :session-id) nelisp-dev-reload--session-id))
    (error "NELISP-DEV-LIVE-SESSION-REQUIRED: current native context required"))
  (unless (and (nelisp-dev--valid-request-p request)
               (equal (cdr (assoc "operation" request)) operation)
               (member (cdr (assoc "target" request)) '(:null "native-linux-x86_64"))
               (or (eq (cdr (assoc "session_id" request)) :null)
                   (equal (cdr (assoc "session_id" request)) nelisp-dev-reload--session-id))
               (cl-every (lambda (pair) (member (car pair) keys))
                         (cdr (assoc "arguments" request))))
    (error "NELISP-DEV-INVALID-REQUEST: unsupported reload arguments"))
  (let ((cursor (cdr (assoc "cursor" (cdr (assoc "limits" request))))))
    (when (and cursor (not (eq cursor :null)))
      (error "NELISP-DEV-INVALID-REQUEST: reload cannot use cursors")))
  (unless (equal (nelisp-dev--arg request "effects_policy") "explicit-only")
    (error "NELISP-DEV-EFFECTS-POLICY-REQUIRED: explicit-only required")))

(defun nelisp-dev-reload--copy-report (value)
  "Copy report strings as well as containers, including compiled shared constants."
  (cond ((stringp value) (copy-sequence value))
        ((consp value) (cons (nelisp-dev-reload--copy-report (car value))
                            (nelisp-dev-reload--copy-report (cdr value))))
        ((vectorp value) (vconcat (mapcar #'nelisp-dev-reload--copy-report value)))
        (t value)))

(defun nelisp-dev-reload--result (operation request context status runtime data diagnostics)
  (nelisp-dev-reload--copy-report
   (nelisp-dev-protocol-envelope
   operation (nelisp-dev--request-id request) status
   (list (cons "target" "native-linux-x86_64")
         (cons "session_id" (if (stringp (plist-get context :session-id))
                               (copy-sequence (plist-get context :session-id)) :null))
         (cons "source_revision" :null) (cons "source_content_hash" :null)
         (cons "runtime_artifact_hash"
               (if (plist-get runtime :binary)
                   (copy-sequence (plist-get runtime :binary)) :null))
         (cons "generation" (or (plist-get runtime :generation) :null)))
    nil diagnostics data nelisp-dev-reload--limitations)))

(defun nelisp-dev-reload--failure (operation request context err)
  (let* ((message (error-message-string err))
         (code (if (string-match "^\\(NELISP-DEV-[A-Z-]+\\):" message)
                   (match-string 1 message) "NELISP-DEV-RELOAD-FAILED")))
    (nelisp-dev-reload--result
     operation request context "failed" nil nil
     (vector (list (cons "code" code)
                   (cons "message" (substring message 0 (min 512 (length message)))))))))

(defun nelisp-dev-reload-plan-dispatch (request context)
  "Build and validate an unpublished allocator/GC candidate."
  (condition-case err
      (progn
        (nelisp-dev-reload--guard request context "reload.plan"
                                  '("unit" "atomicity" "effects_policy"))
        (unless (and (equal (nelisp-dev--arg request "unit") "allocator-gc")
                     (equal (nelisp-dev--arg request "atomicity") "runtime-unit"))
          (error "NELISP-DEV-UNSUPPORTED-SCOPE: only allocator-gc/runtime-unit is supported"))
        (nelisp-dev-reload--purge)
        (when (>= (hash-table-count nelisp-dev-reload--plans) nelisp-dev-reload--max-plans)
          (error "NELISP-DEV-PLAN-LIMIT: clear or expire a plan first"))
        (let* ((root (file-name-as-directory (expand-file-name (plist-get context :root))))
               (session (copy-sequence nelisp-dev-reload--session-id))
               (runtime (nelisp-dev-reload--runtime))
               (options (nelisp-dev-reload--options))
               (inputs (nelisp-dev-reload--inputs root))
               (staged (nelisp-runtime-build-and-stage root)))
          (unless (eq (plist-get staged :status) 'staged)
            (error "NELISP-DEV-PLAN-BUILD-FAILED: %s"
                   (or (plist-get staged :reason) (plist-get staged :phase))))
          (let* ((artifact (plist-get staged :artifact))
                 (source (plist-get staged :source))
                 (artifact-hash (nelisp-dev-reload--sha256-file artifact))
                 (source-hash (nelisp-dev-reload--sha256-file source))
                 (alloc (plist-get staged :alloc-handle))
                 (gc (plist-get staged :gc-handle))
                 (after-inputs (nelisp-dev-reload--inputs root))
                 (expires (+ (float-time) nelisp-dev-reload--ttl)))
            (unless (and (equal session nelisp-dev-reload--session-id)
                         (equal inputs after-inputs)
                         (equal options (nelisp-dev-reload--options))
                         (equal runtime (nelisp-dev-reload--runtime))
                         (equal (plist-get runtime :generation) (plist-get staged :generation))
                         (equal (plist-get runtime :binary) (plist-get staged :binary-sha256))
                         ;; Loader identity hashes the canonical manifest, not
                         ;; the complete .nelr file (which includes its header).
                         (stringp (plist-get alloc :artifact-sha256))
                         (equal (plist-get alloc :artifact-sha256)
                                (plist-get gc :artifact-sha256))
                         (equal source-hash (plist-get alloc :source-sha256))
                         alloc gc)
              (error "NELISP-DEV-STALE-PLAN: build inputs or mapped identity changed"))
            (let* ((payload (list :session session :root root :runtime runtime
                                  :inputs inputs :options options :source source
                                  :source-hash source-hash :artifact artifact
                                  :artifact-hash artifact-hash
                                  :mapped-hash (plist-get alloc :artifact-sha256)
                                  :expires expires
                                  :unit "allocator-gc" :atomicity "runtime-unit"))
                   (id (nelisp-dev-reload--digest payload)))
              (puthash id (list :payload payload :expires expires :alloc alloc :gc gc)
                       nelisp-dev-reload--plans)
              (nelisp-dev-reload--result
               "reload.plan" request context "ok" runtime
               (list (cons "schema_version" "1") (cons "plan_id" (copy-sequence id))
                     (cons "session_id" (copy-sequence session))
                     (cons "expected_generation" (plist-get runtime :generation))
                     (cons "target" "native-linux-x86_64")
                     (cons "unit" "allocator-gc") (cons "atomicity" "runtime-unit")
                     (cons "state" "validated") (cons "expires_at" expires)
                     (cons "source_set_hash" (nelisp-dev-reload--digest inputs))
                     (cons "source_files" (length inputs))
                     (cons "binary_hash" (copy-sequence (plist-get runtime :binary)))
                     (cons "abi_hash" (copy-sequence (plist-get runtime :abi)))
                     (cons "build_options_hash" (copy-sequence options))
                     (cons "artifact_hash" (copy-sequence artifact-hash))
                     (cons "source_hash" (copy-sequence source-hash))
                     (cons "limitations" (copy-sequence nelisp-dev-reload--limitations)))
               [])))))
    (error (nelisp-dev-reload--failure "reload.plan" request context err))))

(defun nelisp-dev-reload-apply-dispatch (request context)
  "Consume and publish a checked local plan; never rebuild or replay."
  (condition-case err
      (progn
        (nelisp-dev-reload--guard request context "reload.apply"
                                  '("plan_id" "effects_policy"))
        (nelisp-dev-reload--purge)
        (let* ((id (nelisp-dev--arg request "plan_id"))
               (record (and (stringp id) (gethash id nelisp-dev-reload--plans))))
          (unless record (error "NELISP-DEV-STALE-PLAN: unknown, expired or consumed plan"))
          ;; Any authorized attempt consumes authority, including an I/O error.
          (remhash id nelisp-dev-reload--plans)
          (let* ((payload (plist-get record :payload))
                 (inputs (nelisp-dev-reload--inputs (plist-get payload :root)))
                 (source-hash (nelisp-dev-reload--sha256-file (plist-get payload :source)))
                 (artifact-hash (nelisp-dev-reload--sha256-file (plist-get payload :artifact))))
            (unless (and (equal id (nelisp-dev-reload--digest payload))
                         (equal (plist-get payload :session) nelisp-dev-reload--session-id)
                         (> (plist-get record :expires) (float-time))
                         (equal inputs (plist-get payload :inputs))
                         (equal source-hash (plist-get payload :source-hash))
                         (equal artifact-hash (plist-get payload :artifact-hash))
                         (equal (plist-get payload :options) (nelisp-dev-reload--options))
                         ;; Sample runtime last, after potentially slow file I/O.
                         (equal (plist-get payload :runtime) (nelisp-dev-reload--runtime)))
              (error "NELISP-DEV-STALE-PLAN: source, artifact, options or runtime changed"))
            (let ((result (nelisp-native-load-raw-install
                           (plist-get record :alloc) (plist-get record :gc))))
              (unless (eq (plist-get result :status) 'published)
                (error "NELISP-DEV-PUBLISH-FAILED: %s"
                       (or (plist-get result :reason) (plist-get result :phase))))
              (nelisp-dev-reload--result
               "reload.apply" request context "ok"
               (plist-put (copy-sequence (plist-get payload :runtime))
                          :generation (plist-get result :generation))
               (list (cons "plan_id" (copy-sequence id)) (cons "state" "published")
                     (cons "generation" (plist-get result :generation))
                     (cons "artifact_hash" (copy-sequence artifact-hash))
                     (cons "published" ["allocator" "collector"]) (cons "replayed" :false))
               [])))))
    (error (nelisp-dev-reload--failure "reload.apply" request context err))))

(provide 'nelisp-dev-reload)
;;; nelisp-dev-reload.el ends here
