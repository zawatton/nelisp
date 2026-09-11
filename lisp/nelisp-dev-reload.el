;;; nelisp-dev-reload.el --- process-local native GC and native-unit reload plans -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Private records own staged handles; reports contain only copied identities.
(require 'cl-lib)
(require 'nelisp-dev)
(require 'nelisp-runtime-development)
(require 'nelisp-native-unit-development)
(require 'nelisp-native-unit)

(defconst nelisp-dev-reload--ttl 900)
(defconst nelisp-dev-reload--max-plans 64)
(defvar nelisp-dev-reload--session-id nil)
(defvar nelisp-dev-reload--plans (make-hash-table :test #'equal))
(defconst nelisp-dev-reload--limitations
  ["Single-use, process-local allocator/GC unit; no arbitrary native caller closure or heap migration."
   "Compiler-directory contents are checked conservatively; host compiler libraries and external dynamic loads are not hermetic."
   "Filesystem checks are observations, not locks; do not edit inputs concurrently with build or publication."
   "Expired or cleared plans lose authority; mapped code remains until process exit."])
(defconst nelisp-dev-reload--native-unit-limitations
  (vconcat
   nelisp-dev-reload--limitations
   ["Already-compiled direct callers of a native-unit export inside the running executable are not redirected by this publication; only calls made through the unit's stable entry gate observe the new generation."])
  "Limitations for `reload.plan'/`reload.apply' results scoped to `native-unit'.")

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
  "Revoke all local plans and any native-unit candidates they hold.
This never publishes or unmaps loaded code.  Return the number of plan
records dropped plus native-unit candidates revoked, so a caller that
only checks for a nonzero result also sees revoked candidates."
  (let ((count (hash-table-count nelisp-dev-reload--plans)) (revoked 0))
    (maphash (lambda (_id record)
               (when (plist-get record :candidate-id)
                 (nelisp-native-unit-discard (plist-get record :candidate-id))
                 (setq revoked (1+ revoked))))
             nelisp-dev-reload--plans)
    (clrhash nelisp-dev-reload--plans)
    (setq nelisp-dev-reload--session-id nil)
    (+ count revoked)))

(defun nelisp-dev-reload--purge ()
  "Expire plans past their TTL, discarding any native-unit candidate held.
Return the number of native-unit candidates revoked this way."
  (let ((now (float-time)) expired (revoked 0))
    (maphash (lambda (id record)
               (when (<= (plist-get record :expires) now) (push id expired)))
             nelisp-dev-reload--plans)
    (dolist (id expired)
      (let ((record (gethash id nelisp-dev-reload--plans)))
        (when (plist-get record :candidate-id)
          (nelisp-native-unit-discard (plist-get record :candidate-id))
          (setq revoked (1+ revoked))))
      (remhash id nelisp-dev-reload--plans))
    revoked))

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

(defun nelisp-dev-reload--native-compile (root path binary)
  "Compile PATH under ROOT into a fresh raw-v1 artifact for BINARY.
Return the new artifact's path.  This is a plain host-subprocess compiler
invocation; it never touches candidate/unit state, which always goes
through the `nelisp-native-unit-*' API.

Bounded through `nelisp-native-unit-development-run-bounded', which owns the
deadline, the cancellation check and the combined stdout/stderr byte budget,
and classifies the finished run from the clock rather than by re-sampling
`process-live-p'.  Before that, this function called `call-process' with no
deadline at all: a hung compiler blocked the REPL process for as long as it
cared to, there was no way to cancel it, and the whole of its output was
interpolated into the error string, so a compiler that emitted megabytes
produced a megabyte-long condition.

The helper deletes nothing, so the temp artifact is this function's to clean
up -- and on every failing path it now is.  It used to be created and then
abandoned whenever the compile failed."
  (let ((script (expand-file-name "scripts/nelisp-native-unit-compile.el" root))
        (artifact (make-temp-file "nelisp-dev-reload-native-" nil ".nelr"))
        (published nil))
    (unwind-protect
        (progn
          (unless (file-readable-p script)
            (error "NELISP-DEV-PLAN-BUILD-FAILED: no native-unit compiler in checkout"))
          (let* ((result (nelisp-native-unit-development-run-bounded
                          (or (getenv "EMACS") "emacs")
                          (list "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
                                "-L" (expand-file-name "lisp" root)
                                "-L" (expand-file-name "src" root)
                                "-L" (expand-file-name "scripts" root)
                                "-l" script "-f" "nelisp-native-unit-compile-command"
                                path artifact binary)))
                 (phase (plist-get result :phase)))
            (unless (eq phase :complete)
              (error "NELISP-DEV-PLAN-BUILD-FAILED: compiler %s (exit %S, %ss): %s"
                     (substring (symbol-name phase) 1)
                     (plist-get result :exit-code)
                     (plist-get result :elapsed-seconds)
                     (plist-get result :stderr)))
            (setq published t)
            artifact))
      (unless published
        (when (file-exists-p artifact) (delete-file artifact))))))

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

(defun nelisp-dev-reload--result (operation request context status runtime data diagnostics
                                            &optional native-unit)
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
    nil diagnostics data
    (if native-unit nelisp-dev-reload--native-unit-limitations nelisp-dev-reload--limitations))))

(defun nelisp-dev-reload--failure (operation request context err &optional native-unit)
  (let* ((message (error-message-string err))
         (code (if (string-match "^\\(NELISP-DEV-[A-Z-]+\\):" message)
                   (match-string 1 message) "NELISP-DEV-RELOAD-FAILED")))
    (nelisp-dev-reload--result
     operation request context "failed" nil nil
     (vector (list (cons "code" code)
                   (cons "message" (substring message 0 (min 512 (length message))))))
     native-unit)))

(defun nelisp-dev-reload--plan-allocator-gc (request context)
  "Build and stage an allocator/GC candidate without publishing it."
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

(defun nelisp-dev-reload--plan-native-unit (request context)
  "Compile and stage a native-unit candidate without publishing it.
SOURCE is compiled through `nelisp-dev-reload--native-compile', a plain
compiler invocation; staging and its identity checks always go through
`nelisp-native-unit-stage', never reimplemented here."
  (let* ((root (file-name-as-directory (expand-file-name (plist-get context :root))))
         (session (copy-sequence nelisp-dev-reload--session-id))
         (source-arg (nelisp-dev--arg request "source"))
         (unit-id-arg (nelisp-dev--arg request "unit_id"))
         (exports-arg (nelisp-dev--arg request "exports")))
    (unless (and (stringp source-arg) (< 0 (length source-arg) 4097))
      (error "NELISP-DEV-INVALID-REQUEST: arguments.source is required"))
    (unless (or (null unit-id-arg) (eq unit-id-arg :null) (stringp unit-id-arg))
      (error "NELISP-DEV-INVALID-REQUEST: arguments.unit_id must be a string or null"))
    (unless (or (null exports-arg) (eq exports-arg :null) (vectorp exports-arg))
      (error "NELISP-DEV-INVALID-REQUEST: arguments.exports must be an array or null"))
    (let* ((path (expand-file-name source-arg root))
           (unit-id (and (stringp unit-id-arg) unit-id-arg))
           (exports (and (vectorp exports-arg) (append exports-arg nil)))
           (runtime (nelisp-dev-reload--runtime))
           (options (nelisp-dev-reload--options))
           (inputs (nelisp-dev-reload--inputs root))
           (source-hash (nelisp-dev-reload--sha256-file path))
           (artifact (nelisp-dev-reload--native-compile root path (plist-get runtime :binary)))
           (artifact-hash (nelisp-dev-reload--sha256-file artifact))
           (after-source-hash (nelisp-dev-reload--sha256-file path))
           (after-inputs (nelisp-dev-reload--inputs root))
           (staged (nelisp-native-unit-stage artifact unit-id exports)))
      (unless (eq (plist-get staged :status) 'staged)
        (error "NELISP-DEV-PLAN-BUILD-FAILED: %s"
               (or (plist-get staged :reason) "stage rejected")))
      (let ((candidate-id (plist-get staged :candidate-id)))
        (unless (and (equal session nelisp-dev-reload--session-id)
                     (equal inputs after-inputs)
                     (equal options (nelisp-dev-reload--options))
                     (equal runtime (nelisp-dev-reload--runtime))
                     (equal source-hash after-source-hash))
          (nelisp-native-unit-discard candidate-id)
          (error "NELISP-DEV-STALE-PLAN: build inputs, session or runtime changed while staging"))
        (let* ((real-unit-id (plist-get staged :unit-id))
               (expected-generation (plist-get staged :expected-generation))
               (exports-out (plist-get staged :exports))
               (expires (+ (float-time) nelisp-dev-reload--ttl))
               (payload (list :session session :root root
                              :runtime runtime :inputs inputs :options options
                              :source path :source-hash source-hash
                              :artifact artifact :artifact-hash artifact-hash
                              :unit-id real-unit-id
                              :expected-generation expected-generation
                              :exports exports-out :expires expires
                              :unit "native-unit" :atomicity "native-unit"))
               (id (nelisp-dev-reload--digest payload)))
          (puthash id (list :payload payload :expires expires :candidate-id candidate-id)
                   nelisp-dev-reload--plans)
          (nelisp-dev-reload--result
           "reload.plan" request context "ok" runtime
           (list (cons "schema_version" "1") (cons "plan_id" (copy-sequence id))
                 (cons "session_id" (copy-sequence session))
                 (cons "expected_generation" expected-generation)
                 (cons "target" "native-linux-x86_64")
                 (cons "unit" "native-unit") (cons "atomicity" "native-unit")
                 (cons "state" "validated") (cons "expires_at" expires)
                 ;; A caller-supplied unit_id is echoed back; an absent one
                 ;; is reported as null even though staging already
                 ;; allocated a real (unpublished) unit -- the plan
                 ;; concerns "a new unit", not yet a named one.
                 (cons "unit_id" (if unit-id (copy-sequence unit-id) :null))
                 (cons "exports"
                       (vconcat (mapcar (lambda (entry)
                                          (list (cons "name" (copy-sequence (car entry)))
                                                (cons "arity" (cdr entry))))
                                        exports-out)))
                 (cons "source_hash" (copy-sequence source-hash))
                 (cons "artifact_hash" (copy-sequence artifact-hash))
                 (cons "build_options_hash" (copy-sequence options))
                 (cons "source_set_hash" (nelisp-dev-reload--digest inputs))
                 (cons "binary_hash" (copy-sequence (plist-get runtime :binary)))
                 (cons "limitations" (copy-sequence nelisp-dev-reload--native-unit-limitations)))
           [] t))))))

(defun nelisp-dev-reload-plan-dispatch (request context)
  "Build and validate an unpublished allocator/GC or native-unit candidate."
  (let ((unit (nelisp-dev--arg request "unit")))
    (condition-case err
        (progn
          (nelisp-dev-reload--guard
           request context "reload.plan"
           (if (equal unit "native-unit")
               '("unit" "atomicity" "effects_policy" "source" "unit_id" "exports")
             '("unit" "atomicity" "effects_policy")))
          (let ((atomicity (nelisp-dev--arg request "atomicity")))
            (unless (or (and (equal unit "allocator-gc") (equal atomicity "runtime-unit"))
                        (and (equal unit "native-unit") (equal atomicity "native-unit")))
              (error "NELISP-DEV-UNSUPPORTED-SCOPE: only allocator-gc/runtime-unit or native-unit/native-unit is supported")))
          (nelisp-dev-reload--purge)
          (when (>= (hash-table-count nelisp-dev-reload--plans) nelisp-dev-reload--max-plans)
            (error "NELISP-DEV-PLAN-LIMIT: clear or expire a plan first"))
          (if (equal unit "native-unit")
              (nelisp-dev-reload--plan-native-unit request context)
            (nelisp-dev-reload--plan-allocator-gc request context)))
      (error (nelisp-dev-reload--failure "reload.plan" request context err
                                         (equal unit "native-unit"))))))

(defun nelisp-dev-reload--apply-allocator-gc (id payload record request context)
  "Revalidate and publish a staged allocator/GC candidate."
  (let* ((inputs (nelisp-dev-reload--inputs (plist-get payload :root)))
         (source-hash (nelisp-dev-reload--sha256-file (plist-get payload :source)))
         (artifact-hash (nelisp-dev-reload--sha256-file (plist-get payload :artifact))))
    (unless (and (equal inputs (plist-get payload :inputs))
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
       []))))

(defun nelisp-dev-reload--apply-native-unit (id payload record request context)
  "Revalidate every precondition, then publish a staged native-unit candidate.
A revalidation failure discards the candidate via `nelisp-native-unit-discard'
so a refused plan leaves nothing publishable behind; publication itself is
always done through `nelisp-native-unit-publish', never reimplemented here."
  (let* ((candidate-id (plist-get record :candidate-id))
         (unit-id (plist-get payload :unit-id))
         (inputs (nelisp-dev-reload--inputs (plist-get payload :root)))
         (source-hash (nelisp-dev-reload--sha256-file (plist-get payload :source)))
         (artifact-hash (nelisp-dev-reload--sha256-file (plist-get payload :artifact)))
         (status (nelisp-native-unit-status unit-id)))
    (unless (and (equal inputs (plist-get payload :inputs))
                 (equal source-hash (plist-get payload :source-hash))
                 (equal artifact-hash (plist-get payload :artifact-hash))
                 (equal (plist-get payload :options) (nelisp-dev-reload--options))
                 status
                 (equal (plist-get status :generation) (plist-get payload :expected-generation))
                 ;; Sample runtime last, after potentially slow file I/O.
                 (equal (plist-get payload :runtime) (nelisp-dev-reload--runtime)))
      (nelisp-native-unit-discard candidate-id)
      (error "NELISP-DEV-STALE-PLAN: source, artifact, options, runtime or unit generation changed"))
    (let ((result (nelisp-native-unit-publish candidate-id)))
      (unless (eq (plist-get result :status) 'published)
        (error "NELISP-DEV-PUBLISH-FAILED: %s"
               (or (plist-get result :reason) "publish rejected")))
      (nelisp-dev-reload--result
       "reload.apply" request context "ok"
       (plist-put (copy-sequence (plist-get payload :runtime))
                  :generation (plist-get result :generation))
       (list (cons "plan_id" (copy-sequence id)) (cons "state" "published")
             (cons "generation" (plist-get result :generation))
             (cons "unit_id" (copy-sequence (plist-get result :unit-id)))
             (cons "artifact_hash" (copy-sequence artifact-hash))
             (cons "published" (vconcat (mapcar (lambda (entry) (copy-sequence (car entry)))
                                                (plist-get payload :exports)))))
       [] t))))

(defun nelisp-dev-reload-apply-dispatch (request context)
  "Consume and publish a checked local plan; never rebuild or replay."
  (let (native-unit)
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
            (let ((payload (plist-get record :payload)))
              (setq native-unit (equal (plist-get payload :unit) "native-unit"))
              (unless (and (equal id (nelisp-dev-reload--digest payload))
                           (equal (plist-get payload :session) nelisp-dev-reload--session-id)
                           (> (plist-get record :expires) (float-time)))
                (when (plist-get record :candidate-id)
                  (nelisp-native-unit-discard (plist-get record :candidate-id)))
                (error "NELISP-DEV-STALE-PLAN: plan identity, session or deadline invalid"))
              (if native-unit
                  (nelisp-dev-reload--apply-native-unit id payload record request context)
                (nelisp-dev-reload--apply-allocator-gc id payload record request context)))))
      (error (nelisp-dev-reload--failure "reload.apply" request context err native-unit)))))

(provide 'nelisp-dev-reload)
;;; nelisp-dev-reload.el ends here
