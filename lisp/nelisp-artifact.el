;;; nelisp-artifact.el --- Private .nelc artifact cache commands  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 142 MVP command surface for NeLisp-private `.nelc' artifacts.
;; The first implementation stores a readable module payload with a
;; distinct magic header, plus a plist sidecar manifest.  Loading the
;; artifact replays the stored module init without reopening the
;; original `.el' source file.

;;; Code:

(require 'nelisp-load)
;; Doc 142 §6.1: eligible top-level `defun' bodies are precompiled to
;; NeLisp bytecode closures (`nelisp-bcl', from `nelisp-bytecode') and
;; replayed onto the NeLisp runtime function table; non-function /
;; unsupported top-level effects fall back to `nelisp-eval' replay.
(require 'nelisp-bytecode)
(require 'nelisp-eval)
;; Loaded lazily by the §6.4 native lane; declared so the bytecode/nelc
;; path does not pull the (heavy) Phase 47 compiler at require time.
(declare-function nelisp-phase47-compile-to-object "nelisp-phase47-compiler"
                  (sexp file-path &rest keys))
(declare-function nelisp-phase47-compile-to-link-unit "nelisp-phase47-compiler"
                  (sexp &rest keys))
(declare-function nelisp-elf-write-binary "nelisp-elf-write"
                  (file-path sections))

(defconst nelisp-artifact--magic ";;; nelisp-private-nelc-v2\n")
(defconst nelisp-artifact--format 'nelisp-private-nelc-v2)
(defconst nelisp-artifact--manifest-format 'nelisp-elisp-artifact-manifest-v1)

;; Doc 142 §7: a stale or mismatched artifact must be rejected BEFORE any
;; module init code runs.  `nelisp-artifact-invalid' is the umbrella
;; condition; `nelisp-artifact-stale' is the subtype raised when the
;; recorded source hash no longer matches the on-disk source.
(define-error 'nelisp-artifact-invalid "Invalid NeLisp artifact")
(define-error 'nelisp-artifact-stale
  "Stale NeLisp artifact (source changed since compile)"
  'nelisp-artifact-invalid)

;; Doc 142 §5 cache-key participants beyond the source/artifact hashes.
;; The `.nelc' lane has no separate native ABI, so the runtime ABI is the
;; module-replay contract version.  Bump these when the replay/bytecode
;; format changes so stale caches are rejected by `nelisp-artifact--validate'.
(defconst nelisp-artifact--runtime-abi "nelisp-nelc-module-replay-v1")
(defconst nelisp-artifact--artifact-class 'bytecode)

;; Doc 142 §6.4: a `.neln' artifact carries the SAME portable bytecode
;; module (so it loads + runs everywhere, §6.3) PLUS an embedded native
;; ET_REL object (Phase 47 output) that a standalone runtime can mmap+exec
;; as an optimisation.  The native object is base64'd into the artifact's
;; `:native' section; on host the bytecode lane is used and the native
;; section is metadata only.
(defconst nelisp-artifact--native-runtime-abi "nelisp-neln-aot-v1")
(defconst nelisp-artifact--native-class 'native)
(defconst nelisp-artifact--native-object-format 'nelisp-aot-elf-v1)
(defconst nelisp-artifact--native-section-version 2)
(defconst nelisp-artifact--usage
  "usage: nelisp compile-elisp-artifact --kind nelc|neln|elc|auto --input FILE.el --output FILE.nelc|FILE.neln|FILE.elc [--manifest FILE.manifest.el] [--load-path DIR]... [--preload FILE.el]... [--feature FEATURE] [--target TARGET] [--cache-key KEY]
       nelisp exec-elisp-artifact FILE.nelc|FILE.neln|FILE.elc FORM...
       nelisp eval-elisp-artifact FILE.nelc|FILE.neln|FILE.elc FORM...
       nelisp inspect-elisp-artifact FILE.nelc|FILE.neln|FILE.elc
  (.nelc = NeLisp bytecode module; .neln = bytecode + embedded native object;
   .elc = genuine GNU Emacs byte-compiled module, Doc 142 §6.2)")

(defvar nelisp-artifact--loaded nil
  "Absolute `.nelc' paths already replayed in this process.")

(defun nelisp-artifact--write-stdout (text)
  "Write TEXT to stdout."
  (if (fboundp 'nelisp--write-stdout-bytes)
      (nelisp--write-stdout-bytes text)
    (princ text)))

(defun nelisp-artifact--write-stderr (text)
  "Write TEXT to stderr."
  (if (fboundp 'nelisp--write-stderr-line)
      (nelisp--write-stderr-line text)
    (princ (concat text "\n") 'external-debugging-output)))

(defun nelisp-artifact--print-error (msg)
  "Print MSG as a CLI error."
  (nelisp-artifact--write-stderr (concat "nelisp: " msg)))

(defun nelisp-artifact--join-forms (forms)
  "Join CLI FORMS into one source string."
  (mapconcat #'identity forms " "))

(defun nelisp-artifact--ensure-final-newline (text)
  "Return TEXT with a trailing newline."
  (if (or (= (length text) 0)
          (= (aref text (1- (length text))) ?\n))
      text
    (concat text "\n")))

(defun nelisp-artifact--read-file-as-string (path)
  "Read PATH as a string.
Artifacts and manifests are project-internal cache files read on the
dev loop, so the host C decoder (`insert-file-contents') is preferred
for speed; the pure-elisp core reader is the standalone fallback when
the host primitive is unavailable.  This matters: the pure-elisp UTF-8
decoder takes ~140ms on a 64 KB `.nelc' — acceptable for one-shot
source loading, but re-paying it on every cache hit would defeat the
whole point of the cache (the artifact would load slower than source)."
  (cond
   ((fboundp 'insert-file-contents)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (buffer-substring-no-properties (point-min) (point-max))))
   ((fboundp 'nelisp-core-read-file-as-string)
    (nelisp-core-read-file-as-string path))
   ((fboundp 'nelisp--syscall-read-file)
    (let ((source (nelisp--syscall-read-file path)))
      (unless (stringp source)
        (error "cannot read file: %s" path))
      source))
   (t (error "no file reader available for %s" path))))

(defun nelisp-artifact--write-file (path content)
  "Write CONTENT to PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent nil 'excl))
  t)

(defun nelisp-artifact--delete-if-exists (path)
  "Delete PATH when it exists."
  (when (and path (file-exists-p path))
    (delete-file path)))

(defun nelisp-artifact--make-temp-path (path suffix)
  "Return a temp path near PATH using SUFFIX."
  (let ((dir (file-name-directory path))
        (name (file-name-nondirectory path)))
    (expand-file-name
     (format ".%s.%s.%d.%d"
             name suffix
             (emacs-pid)
             (random 1000000))
     dir)))

(defun nelisp-artifact--file-size (path)
  "Return PATH size in bytes."
  (let ((attrs (file-attributes path)))
    (if (fboundp 'file-attribute-size)
        (file-attribute-size attrs)
      (nth 7 attrs))))

(defun nelisp-artifact--file-mtime (path)
  "Return PATH modification time."
  (let ((attrs (file-attributes path)))
    (if (fboundp 'file-attribute-modification-time)
        (file-attribute-modification-time attrs)
      (nth 5 attrs))))

(defun nelisp-artifact--sibling-manifest-path (artifact-path)
  "Return the sibling manifest path for ARTIFACT-PATH."
  (concat artifact-path ".manifest.el"))

(defun nelisp-artifact--read-top-level-forms (source)
  "Read every top-level form from SOURCE with the NeLisp reader."
  (let ((pos 0)
        (len (length source))
        (forms nil))
    (while (progn
             (setq pos (nelisp-read--skip-ws source pos))
             (< pos len))
      (let ((res (nelisp-read--sexp source pos)))
        (push (car res) forms)
        (setq pos (cdr res))))
    (nreverse forms)))

(defun nelisp-artifact--read-all-from-string (source)
  "Read every form from SOURCE with host `read'."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (let ((forms nil)
          (done nil))
      (while (not done)
        (skip-chars-forward " \t\r\n")
        (if (>= (point) (point-max))
            (setq done t)
          (push (read (current-buffer)) forms)))
      (nreverse forms))))

(defun nelisp-artifact--try-compile-defun (form)
  "Return (:fn NAME BCL) when FORM is a `defun' the bytecode VM accepts.
Compiling the body `(lambda ARGS . BODY)' through `nelisp-bc-compile' +
`nelisp-bc-run' yields a `nelisp-bcl' closure value; free/global symbol
references stay late-bound (resolved against `nelisp--functions' /
NeLisp variables at call time), so recursion and forward references
work once the module finishes loading.  Returns nil when the body uses
a form the VM cannot yet lower, so the caller can fall back to replay."
  (when (and (consp form)
             (eq (car form) 'defun)
             (>= (safe-length form) 3)
             (symbolp (nth 1 form))
             (listp (nth 2 form)))
    (let ((name (nth 1 form))
          (arglist (nth 2 form))
          (body (nthcdr 3 form)))
      (condition-case nil
          (let ((bcl (nelisp-bc-run
                      (nelisp-bc-compile (cons 'lambda (cons arglist body))))))
            (and (consp bcl) (eq (car bcl) 'nelisp-bcl)
                 (list :fn name bcl)))
        (error nil)))))

(defun nelisp-artifact--compile-top-level-form (form)
  "Lower FORM into a `.nelc' module instruction (Doc 142 §6.1).
An eligible top-level `defun' becomes a precompiled (:fn NAME BCL)
install; every other form (and any defun the bytecode VM cannot lower)
becomes (:eval FORM) replayed through `nelisp-eval' at load."
  (or (nelisp-artifact--try-compile-defun form)
      (list :eval form)))

(defun nelisp-artifact--extract-provided-feature (form)
  "Return the feature symbol provided by FORM, or nil."
  (when (and (consp form)
             (memq (car form) '(provide nelisp-provide))
             (consp (cdr form)))
    (let ((feature-form (nth 1 form)))
      (cond
       ((symbolp feature-form) feature-form)
       ((and (consp feature-form)
             (eq (car feature-form) 'quote)
             (symbolp (nth 1 feature-form)))
        (nth 1 feature-form))
       (t nil)))))

(defun nelisp-artifact--collect-features (forms)
  "Collect top-level provided feature symbols from FORMS."
  (let ((features nil))
    (dolist (form forms)
      (let ((feature (nelisp-artifact--extract-provided-feature form)))
        (when (and feature (not (memq feature features)))
          (setq features (append features (list feature))))))
    features))

(defun nelisp-artifact--compiler-plist ()
  "Return the Doc 142 §6.1 compiler descriptor."
  '(:frontend "nelisp-read--sexp"
    :macroexpander "elisp"
    :bytecode-version 2
    :bytecode-backend "nelisp-bcl-vm"
    :module-init-format "compiled-module-v2"
    :artifact-schema-version 3
    :native-section-version 2))

(defun nelisp-artifact--read-binary (path)
  "Read PATH as a raw unibyte byte string (no decoding)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun nelisp-artifact--target-arch (target)
  "Map a TARGET triple string to a Phase 47 arch symbol, or nil if unknown."
  (cond
   ((null target) 'x86_64)
   ((string-match-p "x86_64\\|amd64" target) 'x86_64)
   ((string-match-p "aarch64\\|arm64" target) 'arm64)
   (t nil)))

(defun nelisp-artifact--write-elf-rel-object (path unit)
  "Write ELF relocatable UNIT to PATH."
  (nelisp-elf-write-binary
   path
   (list :e-type 'rel
         :text (plist-get unit :text)
         :rodata (plist-get unit :rodata)
         :symbols (plist-get unit :symbols)
         :relocs (plist-get unit :relocs)
         :machine (plist-get unit :machine))))

(defun nelisp-artifact--native-defun-entry (entry)
  "Normalize one native defun ENTRY plist for artifact storage."
  (list :name (plist-get entry :name)
        :offset (plist-get entry :offset)
        :size (plist-get entry :size)
        :arity (plist-get entry :arity)
        :param-class (plist-get entry :param-class)
        :rt-slot-count (plist-get entry :rt-slot-count)
        :body-offset (plist-get entry :body-offset)))

(defun nelisp-artifact--native-compile-section (forms target)
  "Compile native-eligible top-level `defun's in FORMS to one ET_REL object.
Returns a `:native' section plist (Doc 142 §6.4) or nil when nothing is
eligible.  Each defun is probed individually so one unsupported body does
not sink the whole module; the eligible set is then emitted as a single
object via the pure-elisp Phase 47 compiler."
  (let ((arch (nelisp-artifact--target-arch target)))
    (when (and arch
               (or (fboundp 'nelisp-phase47-compile-to-object)
                   (require 'nelisp-phase47-compiler nil t)))
      (let ((defuns (seq-filter (lambda (f) (and (consp f) (eq (car f) 'defun)
                                                 (symbolp (nth 1 f))))
                                forms))
            (eligible nil)
            (symbols nil))
        (dolist (d defuns)
          (let ((probe (nelisp-artifact--make-temp-path "neln-probe" "o")))
            (condition-case nil
                (progn
                  (nelisp-phase47-compile-to-object d probe :arch arch :format 'elf)
                  (push d eligible)
                  (push (symbol-name (nth 1 d)) symbols))
              (error nil))
            (nelisp-artifact--delete-if-exists probe)))
        (when eligible
          (let ((obj (nelisp-artifact--make-temp-path "neln-obj" "o")))
            (unwind-protect
                (let* ((unit
                        (nelisp-phase47-compile-to-link-unit
                         (cons 'seq (nreverse eligible))
                         :arch arch :format 'elf))
                       (text-bytes (plist-get unit :text)))
                  (nelisp-artifact--write-elf-rel-object obj unit)
                  (let ((bytes (nelisp-artifact--read-binary obj)))
                    (list :native-section-version
                          nelisp-artifact--native-section-version
                          :object-format nelisp-artifact--native-object-format
                          :arch (symbol-name arch)
                          :symbols (nreverse symbols)
                          :object-size (length bytes)
                          :object-sha256 (secure-hash 'sha256 bytes)
                          :object-base64 (base64-encode-string bytes t)
                          :text-size (length text-bytes)
                          :text-base64 (base64-encode-string text-bytes t)
                          :relocs (plist-get unit :relocs)
                          :extern-symbols (plist-get unit :extern-symbols)
                          :defuns (mapcar #'nelisp-artifact--native-defun-entry
                                          (plist-get unit :defuns)))))
              (nelisp-artifact--delete-if-exists obj))))))))

(defun nelisp-artifact--artifact-payload (source-path module features
                                                      top-level-count kind native)
  "Build the serialized artifact payload for SOURCE-PATH.
KIND is `nelc' or `neln'; NATIVE is the §6.4 native section (or nil)."
  (append
   (list :format nelisp-artifact--format
         :kind kind
         :source (expand-file-name source-path)
         :module-init module
         :features features
         :top-level-count top-level-count
         :compiler (nelisp-artifact--compiler-plist))
   (when native (list :native native))
   (list :entry (list :type 'module-init
                      :id (file-name-nondirectory source-path)))))

(defun nelisp-artifact--artifact-string (payload)
  "Serialize artifact PAYLOAD to a `.nelc' string."
  (concat nelisp-artifact--magic
          (prin1-to-string payload)
          "\n"))

(defun nelisp-artifact--preload-records (preloads)
  "Return Doc 142 §5 `:preloads' records (path + sha256) for PRELOADS."
  (mapcar (lambda (p)
            (let ((abs (expand-file-name p)))
              (list :path abs
                    :sha256 (secure-hash
                             'sha256
                             (nelisp-artifact--read-file-as-string abs)))))
          preloads))

(defun nelisp-artifact--manifest-plist (source-path features top-level-count
                                                    target artifact-sha256
                                                    preload-records load-paths
                                                    kind native)
  "Build the Doc 142 v1 manifest plist.
ARTIFACT-SHA256 is the integrity hash of the serialized artifact;
PRELOAD-RECORDS and LOAD-PATHS, plus the artifact/source/compiler/ABI
fields, are the cache-key participants enforced by
`nelisp-artifact--validate' (Doc 142 §5/§7).  KIND is `nelc' / `neln';
for `neln' the artifact-class is `native', the runtime-abi is the AOT
ABI, and NATIVE metadata (object hash, symbols, arch) is recorded."
  (append
   (list :format nelisp-artifact--manifest-format
         :kind kind
         :artifact-format nelisp-artifact--format
         :artifact-class (if (eq kind 'neln)
                             nelisp-artifact--native-class
                           nelisp-artifact--artifact-class)
         :runtime-abi (if (eq kind 'neln)
                          nelisp-artifact--native-runtime-abi
                        nelisp-artifact--runtime-abi)
         :artifact-sha256 artifact-sha256
         :nelisp-version (if (boundp 'nelisp--cli-version)
                             nelisp--cli-version
                           "unknown")
         :target (or target
                     (and (boundp 'system-configuration) system-configuration)
                     "unknown")
         :source (list :path (expand-file-name source-path)
                       :truename (file-truename source-path)
                       :sha256 (secure-hash 'sha256
                                            (nelisp-artifact--read-file-as-string
                                             source-path))
                       :size (nelisp-artifact--file-size source-path)
                       :mtime (nelisp-artifact--file-mtime source-path))
         :preloads preload-records
         :load-path (mapcar #'expand-file-name load-paths)
         :features features
         :top-level-count top-level-count
         :compiler (nelisp-artifact--compiler-plist))
   (when native
     ;; manifest records native METADATA only (the bytes live in the
     ;; artifact, covered by :artifact-sha256).
     (list :native (list :native-section-version
                         (plist-get native :native-section-version)
                         :object-format (plist-get native :object-format)
                         :arch (plist-get native :arch)
                         :symbols (plist-get native :symbols)
                         :object-size (plist-get native :object-size)
                         :object-sha256 (plist-get native :object-sha256)
                         :text-size (plist-get native :text-size)
                         :relocs (plist-get native :relocs)
                         :extern-symbols (plist-get native :extern-symbols)
                         :defuns (plist-get native :defuns))))
   (list :entry (list :type 'module-init
                      :id (file-name-nondirectory source-path)))))

(defun nelisp-artifact--write-pair-atomically (artifact-path artifact-content
                                                             manifest-path manifest-content)
  "Write ARTIFACT-CONTENT and MANIFEST-CONTENT atomically enough for MVP."
  (let* ((artifact-temp (nelisp-artifact--make-temp-path artifact-path "tmp"))
         (manifest-temp (nelisp-artifact--make-temp-path manifest-path "tmp"))
         (artifact-backup (and (file-exists-p artifact-path)
                               (nelisp-artifact--make-temp-path artifact-path "bak")))
         (manifest-backup (and (file-exists-p manifest-path)
                               (nelisp-artifact--make-temp-path manifest-path "bak")))
         (artifact-installed nil)
         (manifest-installed nil))
    (unwind-protect
        (progn
          (nelisp-artifact--write-file artifact-temp artifact-content)
          (nelisp-artifact--write-file manifest-temp manifest-content)
          (when artifact-backup
            (rename-file artifact-path artifact-backup t))
          (when manifest-backup
            (rename-file manifest-path manifest-backup t))
          (rename-file artifact-temp artifact-path t)
          (setq artifact-installed t)
          (rename-file manifest-temp manifest-path t)
          (setq manifest-installed t)
          (when artifact-backup
            (delete-file artifact-backup))
          (when manifest-backup
            (delete-file manifest-backup))
          t)
      (unless (and artifact-installed manifest-installed)
        (when manifest-installed
          (nelisp-artifact--delete-if-exists manifest-path))
        (when artifact-installed
          (nelisp-artifact--delete-if-exists artifact-path))
        (when (and artifact-backup (file-exists-p artifact-backup))
          (rename-file artifact-backup artifact-path t))
        (when (and manifest-backup (file-exists-p manifest-backup))
          (rename-file manifest-backup manifest-path t))
        (nelisp-artifact--delete-if-exists artifact-temp)
        (nelisp-artifact--delete-if-exists manifest-temp)))))

(defun nelisp-artifact-compile-file (source-path artifact-path
                                                 &optional manifest-path target
                                                 load-paths preloads requested-feature
                                                 kind)
  "Compile SOURCE-PATH into ARTIFACT-PATH and MANIFEST-PATH.
KIND is `nelc' (bytecode, default) or `neln' (bytecode + an embedded
native object for the standalone runtime, Doc 142 §6.4)."
  (let* ((kind (or kind 'nelc))
         (manifest-path (or manifest-path
                            (nelisp-artifact--sibling-manifest-path artifact-path)))
         (source (nelisp-artifact--read-file-as-string source-path))
         (forms (nelisp-artifact--read-top-level-forms source))
         (module nil)
         (features nil)
         (native nil)
         (artifact-payload nil)
         (artifact-content nil)
         (manifest nil))
    (let ((load-path (append load-paths load-path))
          (nelisp-load-path (append load-paths nelisp-load-path)))
      (dolist (preload preloads)
        (load preload nil t))
      (setq module (mapcar #'nelisp-artifact--compile-top-level-form forms)))
    (setq features (nelisp-artifact--collect-features forms))
    (when (and requested-feature (not (memq requested-feature features)))
      (error "compile-elisp-artifact: source did not provide %S" requested-feature))
    (when (eq kind 'neln)
      (setq native (nelisp-artifact--native-compile-section forms target))
      (unless native
        (error "compile-elisp-artifact: --kind neln but no top-level defun could be native-compiled for %s"
               (or target "x86_64"))))
    (setq artifact-payload
          (nelisp-artifact--artifact-payload source-path module features
                                             (length forms) kind native))
    (setq artifact-content (nelisp-artifact--artifact-string artifact-payload))
    (setq manifest
          (nelisp-artifact--manifest-plist
           source-path features (length forms) target
           (secure-hash 'sha256 artifact-content)
           (nelisp-artifact--preload-records preloads)
           load-paths kind native))
    (nelisp-artifact--write-pair-atomically
     artifact-path artifact-content
     manifest-path (concat (prin1-to-string manifest) "\n"))
    manifest))

(defun nelisp-artifact--parse-payload (content artifact-path)
  "Parse the `.nelc' CONTENT string, returning its payload plist."
  (let ((prefix-len (length nelisp-artifact--magic)))
    (unless (string-prefix-p nelisp-artifact--magic content)
      (signal 'nelisp-artifact-invalid
              (list "invalid .nelc magic header" artifact-path)))
    (let ((forms (nelisp-artifact--read-all-from-string
                  (substring content prefix-len))))
      (unless (= (length forms) 1)
        (signal 'nelisp-artifact-invalid
                (list "malformed .nelc payload" artifact-path)))
      (let ((payload (car forms)))
        (unless (eq (plist-get payload :format) nelisp-artifact--format)
          (signal 'nelisp-artifact-invalid
                  (list "unsupported .nelc format"
                        (plist-get payload :format) artifact-path)))
        payload))))

(defun nelisp-artifact--read-payload (artifact-path)
  "Read and parse ARTIFACT-PATH, returning its payload plist."
  (nelisp-artifact--parse-payload
   (nelisp-artifact--read-file-as-string artifact-path)
   artifact-path))

(defun nelisp-artifact--validate (artifact-path artifact-content)
  "Reject ARTIFACT-PATH before module init if its manifest does not match.
Doc 142 §7: manifest/artifact format, the artifact integrity hash, the
runtime version, and source freshness all participate in the cache key.
The original source need NOT exist — a fresh process may load an
artifact whose source is absent — so freshness is enforced only when the
recorded source file is still present on disk.  Returns the manifest."
  (let ((manifest-path (nelisp-artifact--sibling-manifest-path artifact-path)))
    (unless (file-exists-p manifest-path)
      (signal 'nelisp-artifact-invalid
              (list "missing manifest for artifact" manifest-path)))
    (let ((manifest (nelisp-artifact-read-manifest artifact-path)))
      (unless (eq (plist-get manifest :format) nelisp-artifact--manifest-format)
        (signal 'nelisp-artifact-invalid
                (list "unsupported manifest format"
                      (plist-get manifest :format) manifest-path)))
      (unless (eq (plist-get manifest :artifact-format) nelisp-artifact--format)
        (signal 'nelisp-artifact-invalid
                (list "manifest artifact-format mismatch"
                      (plist-get manifest :artifact-format) manifest-path)))
      ;; Artifact class + runtime ABI must match (Doc 142 §5), with the
      ;; expected values selected by the artifact KIND (nelc bytecode vs
      ;; neln native).
      (let* ((kind (plist-get manifest :kind))
             (expected-class (if (eq kind 'neln)
                                 nelisp-artifact--native-class
                               nelisp-artifact--artifact-class))
             (expected-abi (if (eq kind 'neln)
                               nelisp-artifact--native-runtime-abi
                             nelisp-artifact--runtime-abi)))
        (unless (eq (plist-get manifest :artifact-class) expected-class)
          (signal 'nelisp-artifact-invalid
                  (list "artifact-class mismatch"
                        (plist-get manifest :artifact-class) manifest-path)))
        (unless (equal (plist-get manifest :runtime-abi) expected-abi)
          (signal 'nelisp-artifact-invalid
                  (list "runtime-abi mismatch"
                        (plist-get manifest :runtime-abi) manifest-path))))
      ;; Compiler format must match (Doc 142 §5: "compiler format versions
      ;; must match") — a bytecode/replay-format bump invalidates stale
      ;; caches via this check.
      (unless (equal (plist-get manifest :compiler)
                     (nelisp-artifact--compiler-plist))
        (signal 'nelisp-artifact-invalid
                (list "compiler format mismatch"
                      (plist-get manifest :compiler) manifest-path)))
      ;; Integrity: the artifact bytes must hash to the recorded value.  A
      ;; v1 manifest MUST carry the hash; a missing/forged hash is itself a
      ;; reason to reject (it cannot be silently skipped).
      (let ((want (plist-get manifest :artifact-sha256)))
        (unless (and want (stringp want))
          (signal 'nelisp-artifact-invalid
                  (list "manifest missing artifact integrity hash" manifest-path)))
        (unless (equal want (secure-hash 'sha256 artifact-content))
          (signal 'nelisp-artifact-invalid
                  (list "artifact sha256 mismatch (corrupt/truncated)"
                        artifact-path))))
      ;; Runtime version pin (skip when either side is unknown — a value
      ;; only a real production binary records).
      (let ((mv (plist-get manifest :nelisp-version))
            (cv (and (boundp 'nelisp--cli-version) nelisp--cli-version)))
        (when (and mv cv
                   (not (equal mv "unknown"))
                   (not (equal cv "unknown"))
                   (not (equal mv cv)))
          (signal 'nelisp-artifact-invalid
                  (list "nelisp-version mismatch" mv cv artifact-path))))
      ;; Source freshness: only enforced when the recorded source still
      ;; exists.  Prefer :truename (symlink-resolved) over :path.  FAST
      ;; PATH: if size AND mtime both match the manifest, treat the source
      ;; as unchanged WITHOUT re-hashing it — re-reading the whole source
      ;; on every load would defeat the cache (it is exactly the cost the
      ;; artifact is meant to avoid).  Only when size/mtime differ (or were
      ;; not recorded) do we read + sha256 to decide stale-ness precisely.
      (let* ((src (plist-get manifest :source))
             (spath (and src (or (plist-get src :truename)
                                 (plist-get src :path))))
             (swant (and src (plist-get src :sha256))))
        (when (and spath swant (file-exists-p spath))
          (let ((want-size (plist-get src :size))
                (want-mtime (plist-get src :mtime)))
            (unless (and want-size want-mtime
                         (equal want-size (nelisp-artifact--file-size spath))
                         (equal want-mtime (nelisp-artifact--file-mtime spath)))
              ;; size/mtime changed or unrecorded → hash to be sure.
              (unless (equal swant
                             (secure-hash 'sha256
                                          (nelisp-artifact--read-file-as-string spath)))
                (signal 'nelisp-artifact-stale
                        (list "source changed since compile"
                              spath artifact-path)))))))
      ;; Preload freshness: each recorded preload that still exists must
      ;; hash to its recorded value (Doc 142 §5: "preload hashes must
      ;; match").  Absent preloads are skipped, mirroring source.
      (dolist (rec (plist-get manifest :preloads))
        (let ((ppath (plist-get rec :path))
              (pwant (plist-get rec :sha256)))
          (when (and ppath pwant (file-exists-p ppath))
            (unless (equal pwant
                           (secure-hash
                            'sha256
                            (nelisp-artifact--read-file-as-string ppath)))
              (signal 'nelisp-artifact-stale
                      (list "preload changed since compile"
                            ppath artifact-path))))))
      manifest)))

(defun nelisp-artifact-load-file (artifact-path)
  "Load ARTIFACT-PATH without reopening its source `.el' file.
The sibling manifest is validated (format, integrity hash, runtime
version, source freshness) and a stale/mismatched artifact is rejected
BEFORE any module init form runs (Doc 142 §7).  A `.elc' artifact
(Doc 142 §6.2) is validated then `load'ed by host Emacs; `.nelc'/`.neln'
replay their bytecode module onto the NeLisp runtime."
  (let ((full-path (expand-file-name artifact-path)))
    (cond
     ((member full-path nelisp-artifact--loaded) nil)
     ;; §6.2 GNU Emacs .elc: validate, then host `load'.
     ((eq (nelisp-artifact--artifact-kind full-path) 'elc)
      (nelisp-artifact--validate-elc full-path)
      (load full-path nil t)
      (setq nelisp-artifact--loaded (cons full-path nelisp-artifact--loaded))
      nil)
     ;; §6.1/§6.4 .nelc/.neln: validate, then replay the bytecode module.
     (t
      (let ((content (nelisp-artifact--read-file-as-string full-path)))
        (nelisp-artifact--validate full-path content)
        (let* ((payload (nelisp-artifact--parse-payload content full-path))
               (module (plist-get payload :module-init))
               (features (plist-get payload :features))
               (last nil))
          ;; Replay the module onto the NeLisp runtime: install precompiled
          ;; bytecode closures into the function table, `nelisp-eval' the
          ;; remaining top-level effects (Doc 142 §6.1).
          (dolist (item module)
            (cond
             ((and (consp item) (eq (car item) :fn))
              (puthash (nth 1 item) (nth 2 item) nelisp--functions)
              (setq last (nth 1 item)))
             ((and (consp item) (eq (car item) :eval))
              (setq last (nelisp-eval (nth 1 item))))
             (t
              (setq last (nelisp-eval item)))))
          (dolist (feature features)
            (when (fboundp 'nelisp-provide)
              (nelisp-provide feature))
            (unless (featurep feature)
              (provide feature)))
          (setq nelisp-artifact--loaded
                (cons full-path nelisp-artifact--loaded))
          last))))))

(defun nelisp-artifact-read-manifest (artifact-path)
  "Read the sibling manifest for ARTIFACT-PATH."
  (let* ((manifest-path (nelisp-artifact--sibling-manifest-path artifact-path))
         (forms (nelisp-artifact--read-all-from-string
                 (nelisp-artifact--read-file-as-string manifest-path))))
    (unless (= (length forms) 1)
      (error "malformed manifest: %s" manifest-path))
    (car forms)))

;; ---------------------------------------------------------------------------
;; Doc 142 §6.2 — GNU Emacs-compatible `.elc' lane.
;;
;; The dev loop runs on host Emacs, so the genuine GNU Emacs byte-compiler
;; produces the artifact: a real, GNU Emacs-readable `.elc' (NOT a NeLisp
;; private text format with an `.elc' suffix).  Loading is host `load';
;; the sibling manifest carries the cache key + Emacs-version compatibility.
;; ---------------------------------------------------------------------------

(defun nelisp-artifact--byte-compile-to (source-path load-paths)
  "Byte-compile SOURCE-PATH to a GNU Emacs `.elc'; return its path.
Runs in a CLEAN `emacs -Q --batch' subprocess so NeLisp's own
byte-compiler (which shadows `byte-compile-file' to emit private NeLisp
bytecode) does not intercept it — the artifact must be a genuine GNU
Emacs-readable `.elc' (Doc 142 §6.2)."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (dest (concat (file-name-sans-extension source-path) ".elc"))
         (args (append
                (list "-Q" "--batch")
                (apply #'append
                       (mapcar (lambda (d) (list "-L" (expand-file-name d)))
                               load-paths))
                (list "--eval" "(setq byte-compile-warnings nil byte-compile-verbose nil)"
                      "-f" "batch-byte-compile" source-path))))
    (unless (and (eq 0 (apply #'call-process emacs nil nil nil args))
                 (file-exists-p dest))
      (error "byte-compile (clean Emacs subprocess) failed for %s" source-path))
    dest))

(defun nelisp-artifact--elc-manifest-plist (source-path features top-level-count
                                                        target artifact-sha256
                                                        preload-records load-paths)
  "Build the Doc 142 manifest plist for a GNU Emacs `.elc' artifact."
  (list :format nelisp-artifact--manifest-format
        :kind 'elc
        :artifact-format 'emacs-elc
        :artifact-class 'bytecode
        :runtime-abi "emacs-bytecode"
        :artifact-sha256 artifact-sha256
        :nelisp-version (if (boundp 'nelisp--cli-version)
                            nelisp--cli-version
                          "unknown")
        :target (or target
                    (and (boundp 'system-configuration) system-configuration)
                    "unknown")
        :emacs-compat (list :emacs-version emacs-version
                            :emacs-major-version emacs-major-version
                            :compatible t)
        :source (list :path (expand-file-name source-path)
                      :truename (file-truename source-path)
                      :sha256 (secure-hash 'sha256
                                           (nelisp-artifact--read-file-as-string
                                            source-path))
                      :size (nelisp-artifact--file-size source-path)
                      :mtime (nelisp-artifact--file-mtime source-path))
        :preloads preload-records
        :load-path (mapcar #'expand-file-name load-paths)
        :features features
        :top-level-count top-level-count
        :compiler (list :frontend "emacs-read"
                        :backend "emacs-byte-compile"
                        :emacs-version emacs-version)
        :entry (list :type 'module-init
                     :id (file-name-nondirectory source-path))))

(defun nelisp-artifact-compile-elc-file (source-path artifact-path
                                                     &optional manifest-path target
                                                     load-paths preloads requested-feature)
  "Compile SOURCE-PATH into a GNU Emacs-readable `.elc' at ARTIFACT-PATH.
Doc 142 §6.2: the artifact is a genuine byte-compiled file that GNU Emacs
can `load'; a sibling manifest records the cache key and Emacs version."
  (let* ((manifest-path (or manifest-path
                            (nelisp-artifact--sibling-manifest-path artifact-path)))
         (forms (nelisp-artifact--read-all-from-string
                 (nelisp-artifact--read-file-as-string source-path)))
         (features (nelisp-artifact--collect-features forms))
         (manifest-temp (nelisp-artifact--make-temp-path manifest-path "tmp"))
         (produced nil))
    (when (and requested-feature (not (memq requested-feature features)))
      (error "compile-elisp-artifact: source did not provide %S" requested-feature))
    (let ((load-path (append load-paths load-path)))
      (dolist (preload preloads)
        (load preload nil t))
      (setq produced (nelisp-artifact--byte-compile-to source-path load-paths)))
    (let* ((artifact-sha (secure-hash 'sha256
                                      (nelisp-artifact--read-binary produced)))
           (manifest (nelisp-artifact--elc-manifest-plist
                      source-path features (length forms) target artifact-sha
                      (nelisp-artifact--preload-records preloads) load-paths)))
      (unless (and (file-exists-p produced)
                   (file-equal-p produced artifact-path))
        (rename-file produced artifact-path t))
      (with-temp-file manifest-temp
        (insert (prin1-to-string manifest) "\n"))
      (rename-file manifest-temp manifest-path t)
      manifest)))

(defun nelisp-artifact--validate-elc (artifact-path)
  "Reject a `.elc' ARTIFACT-PATH before loading when its manifest does not
match (integrity, Emacs major version, source freshness).  Returns the
manifest."
  (let ((manifest-path (nelisp-artifact--sibling-manifest-path artifact-path)))
    (unless (file-exists-p manifest-path)
      (signal 'nelisp-artifact-invalid
              (list "missing manifest for artifact" manifest-path)))
    (let ((manifest (nelisp-artifact-read-manifest artifact-path)))
      ;; integrity of the binary .elc
      (let ((want (plist-get manifest :artifact-sha256)))
        (unless (and want (stringp want))
          (signal 'nelisp-artifact-invalid
                  (list "manifest missing artifact integrity hash" manifest-path)))
        (unless (equal want (secure-hash 'sha256
                                         (nelisp-artifact--read-binary artifact-path)))
          (signal 'nelisp-artifact-invalid
                  (list "elc integrity mismatch (corrupt/truncated)" artifact-path))))
      ;; .elc bytecode is tied to the Emacs major version.
      (let ((mv (plist-get (plist-get manifest :emacs-compat) :emacs-major-version)))
        (when (and mv (not (equal mv emacs-major-version)))
          (signal 'nelisp-artifact-invalid
                  (list "elc emacs-major-version mismatch" mv emacs-major-version
                        artifact-path))))
      ;; source freshness (mtime/size fast path, sha256 fallback).
      (let* ((src (plist-get manifest :source))
             (spath (and src (or (plist-get src :truename) (plist-get src :path))))
             (swant (and src (plist-get src :sha256))))
        (when (and spath swant (file-exists-p spath))
          (unless (and (equal (plist-get src :size)
                              (nelisp-artifact--file-size spath))
                       (equal (plist-get src :mtime)
                              (nelisp-artifact--file-mtime spath)))
            (unless (equal swant
                           (secure-hash 'sha256
                                        (nelisp-artifact--read-file-as-string spath)))
              (signal 'nelisp-artifact-stale
                      (list "source changed since compile" spath artifact-path))))))
      manifest)))

(defun nelisp-artifact--artifact-kind (artifact-path)
  "Return the artifact KIND (nelc/neln/elc) from its sibling manifest, or nil."
  (condition-case nil
      (plist-get (nelisp-artifact-read-manifest artifact-path) :kind)
    (error nil)))

(defun nelisp-artifact-native-exec (artifact-path symbol args)
  "Doc 142 §6.4 native EXEC: run the native SYMBOL embedded in a `.neln'.
Extracts the ET_REL object from ARTIFACT-PATH's `:native' section, links
it with a generated integer-ABI driver, runs it with integer ARGS, and
returns the int64 result.  This is the first native-execution spike: it
works for the reloc-free leaf functions Phase 47 emits today (plain C
integer ABI, no boundary slots).  The host C toolchain (cc + objcopy)
acts as the loader; an in-process standalone mmap+reloc loader for the
general boundary-ABI case is the remaining §6.4 work.

Signals an error when the toolchain is missing, the artifact has no
native object, or SYMBOL is not one of its native functions."
  (let ((cc (or (executable-find "cc") (executable-find "gcc")))
        (objcopy (executable-find "objcopy")))
    (unless (and cc objcopy)
      (error "native-exec needs cc + objcopy on PATH"))
    (let* ((payload (nelisp-artifact--read-payload artifact-path))
           (native (plist-get payload :native))
           (b64 (and native (plist-get native :object-base64)))
           (symbols (and native (plist-get native :symbols))))
      (unless b64
        (error "%s has no embedded native object" artifact-path))
      (unless (member symbol symbols)
        (error "native symbol %s not in artifact (have %S)" symbol symbols))
      (let* ((dir (make-temp-file "neln-exec-" t))
             (obj (expand-file-name "mod.o" dir))
             (obj2 (expand-file-name "mod-c.o" dir))
             (csrc (expand-file-name "drv.c" dir))
             (exe (expand-file-name "run" dir))
             (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol))
             (argc (length args)))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'binary))
                (write-region (base64-decode-string b64) nil obj nil 'silent))
              ;; ELF symbols carry the elisp name (with dashes); rename to a
              ;; C identifier so the driver can reference it.
              (unless (eq 0 (call-process objcopy nil nil nil
                                          (format "--redefine-sym=%s=%s" symbol csym)
                                          obj obj2))
                (error "objcopy symbol rename failed for %s" symbol))
              (with-temp-file csrc
                (insert "#include <stdlib.h>\n#include <stdio.h>\n")
                (insert (format "extern long %s(%s);\n" csym
                                (if (= argc 0) "void"
                                  (mapconcat (lambda (_) "long") args ","))))
                (insert "int main(int c,char**v){(void)c;")
                (let ((i 0))
                  (insert (format "printf(\"%%ld\\n\",%s(%s));return 0;}\n" csym
                                  (mapconcat (lambda (_)
                                               (setq i (1+ i))
                                               (format "atol(v[%d])" i))
                                             args ",")))))
              (unless (eq 0 (call-process cc nil nil nil "-O2" "-o" exe csrc obj2))
                (error "native link failed for %s" symbol))
              (with-temp-buffer
                (apply #'call-process exe nil t nil
                       (mapcar #'number-to-string args))
                (string-to-number (string-trim (buffer-string)))))
          (delete-directory dir t))))))

(defconst nelisp-artifact--native-boundary-slot-names
  '("out" "mirror" "frames" "scratch" "name_slot"
    "callback-slot-0" "callback-slot-1" "callback-slot-2" "callback-slot-3"
    "callback-slot-4" "callback-slot-5" "callback-slot-6" "callback-slot-7"
    "callback-slot-8" "callback-slot-9" "callback-slot-10" "callback-slot-11")
  "Object-mode hidden boundary slots for ordinary native user defuns.")

(defun nelisp-artifact--native-defun-metadata (native symbol)
  "Return SYMBOL's metadata plist from NATIVE, or nil when absent."
  (seq-find (lambda (entry)
              (equal (plist-get entry :name) symbol))
            (plist-get native :defuns)))

(defun nelisp-artifact--native-general-unsupported-externs (native)
  "Return NATIVE extern symbols not supported by the host proof harness."
  (seq-remove
   (lambda (name)
     (member name '("nl_alloc_symbol"
                    "nelisp_aot_builtin_call1"
                    "nelisp_aot_builtin_calln")))
   (plist-get native :extern-symbols)))

(defun nelisp-artifact--native-trampoline-frame-bytes (meta)
  "Return the synthetic frame size in bytes required by META."
  (let* ((arity (or (plist-get meta :arity) 0))
         (rt-slot-count (or (plist-get meta :rt-slot-count) 0))
         (rt-rounded (if (zerop rt-slot-count)
                         0
                       (if (zerop (logand rt-slot-count 1))
                           rt-slot-count
                         (1+ rt-slot-count)))))
    (+ (* 8 arity)
       (if (= 1 (logand arity 1)) 8 0)
       (* 8 rt-rounded))))

(defun nelisp-artifact--native-trampoline-slot-disp (slot-index)
  "Return the rbp-relative displacement for SLOT-INDEX."
  (- (* 8 (1+ slot-index))))

(defun nelisp-artifact--native-trampoline-asm (csym meta)
  "Return the assembly trampoline source for CSYM using META."
  (let* ((arity (or (plist-get meta :arity) 0))
         (body-offset (plist-get meta :body-offset))
         (frame-bytes (nelisp-artifact--native-trampoline-frame-bytes meta))
         (arg-regs '("rdi" "rsi" "rdx" "rcx" "r8" "r9"))
         (base-boundary-labels '("out" "mirror" "frames" "scratch" "name_slot"))
         (lines
          (list ".text"
                ".globl call_target"
                ".type call_target, @function"
                "call_target:"
                "  pushq %rbp"
                "  movq %rsp, %rbp")))
    (when (> frame-bytes 0)
      (setq lines
            (append lines
                    (list (format "  subq $%d, %%rsp" frame-bytes)))))
    (dotimes (i arity)
      (setq lines
            (append
             lines
             (list
              (format "  movq %%%s, %d(%%rbp)"
                      (nth i arg-regs)
                      (nelisp-artifact--native-trampoline-slot-disp i))))))
    (dotimes (i (length base-boundary-labels))
      (let ((slot-index (+ arity i)))
        (setq lines
              (append
               lines
               (list
                (format "  leaq neln_%s(%%rip), %%rax"
                        (nth i base-boundary-labels))
                (format "  movq %%rax, %d(%%rbp)"
                        (nelisp-artifact--native-trampoline-slot-disp
                         slot-index)))))))
    (dotimes (i 12)
      (let ((slot-index (+ arity 5 i)))
        (setq lines
              (append
               lines
               (list
                (format "  leaq neln_callback_slots+%d(%%rip), %%rax" (* i 32))
                (format "  movq %%rax, %d(%%rbp)"
                        (nelisp-artifact--native-trampoline-slot-disp
                         slot-index)))))))
    (setq lines
          (append
           lines
           (list (format "  leaq %s+%d(%%rip), %%rax" csym body-offset)
                 "  jmp *%rax"
                 ".size call_target, .-call_target")))
    (mapconcat #'identity lines "\n")))

(defun nelisp-artifact--native-driver-c (csym meta)
  "Return the C harness source for CSYM using META."
  (let* ((arity (or (plist-get meta :arity) 0))
         (extern-args (if (= arity 0)
                          "void"
                        (mapconcat (lambda (_i) "long")
                                   (number-sequence 1 arity)
                                   ", ")))
         (invoke-args (if (= arity 0)
                          ""
                        (mapconcat (lambda (i)
                                     (format "argv_vals[%d]" i))
                                   (number-sequence 0 (1- arity))
                                   ", "))))
    (concat
     "#include <stdint.h>\n"
     "#include <stdio.h>\n"
     "#include <stdlib.h>\n"
     "#include <string.h>\n"
     "\n"
     "typedef struct NelnSexp {\n"
     "  unsigned char tag;\n"
     "  unsigned char pad[7];\n"
     "  uint64_t a;\n"
     "  uint64_t b;\n"
     "  uint64_t c;\n"
     "} NelnSexp;\n"
     "\n"
     "typedef struct NelnConsBox {\n"
     "  NelnSexp car;\n"
     "  NelnSexp cdr;\n"
     "  uint64_t refcount;\n"
     "} NelnConsBox;\n"
     "\n"
     "enum {\n"
     "  NELN_TAG_NIL = 0,\n"
     "  NELN_TAG_T = 1,\n"
     "  NELN_TAG_INT = 2,\n"
     "  NELN_TAG_SYMBOL = 4,\n"
     "  NELN_TAG_CONS = 7\n"
     "};\n"
     "\n"
     "NelnSexp neln_out;\n"
     "NelnSexp neln_mirror;\n"
     "NelnSexp neln_frames;\n"
     "NelnSexp neln_scratch;\n"
     "NelnSexp neln_name_slot;\n"
     "NelnSexp neln_callback_slots[12];\n"
     "\n"
     "static const void *neln_slot_registry[64];\n"
     "static size_t neln_slot_registry_len = 0;\n"
     "\n"
     "static void neln_fail(const char *msg) {\n"
     "  fprintf(stderr, \"neln native harness: %s\\n\", msg);\n"
     "  exit(125);\n"
     "}\n"
     "\n"
     "static void neln_clear_sexp(NelnSexp *slot) {\n"
     "  memset(slot, 0, sizeof(*slot));\n"
     "}\n"
     "\n"
     "static void neln_write_nil(NelnSexp *slot) {\n"
     "  neln_clear_sexp(slot);\n"
     "  slot->tag = NELN_TAG_NIL;\n"
     "}\n"
     "\n"
     "static void neln_write_t(NelnSexp *slot) {\n"
     "  neln_clear_sexp(slot);\n"
     "  slot->tag = NELN_TAG_T;\n"
     "}\n"
     "\n"
     "static void neln_write_int(NelnSexp *slot, int64_t value) {\n"
     "  neln_clear_sexp(slot);\n"
     "  slot->tag = NELN_TAG_INT;\n"
     "  slot->a = (uint64_t)value;\n"
     "}\n"
     "\n"
     "static void neln_register_slot(const void *ptr) {\n"
     "  if (neln_slot_registry_len >= (sizeof(neln_slot_registry) / sizeof(neln_slot_registry[0]))) {\n"
     "    neln_fail(\"slot registry overflow\");\n"
     "  }\n"
     "  neln_slot_registry[neln_slot_registry_len++] = ptr;\n"
     "}\n"
     "\n"
     "static int neln_is_registered_slot(const void *ptr) {\n"
     "  size_t i;\n"
     "  for (i = 0; i < neln_slot_registry_len; i++) {\n"
     "    if (neln_slot_registry[i] == ptr) {\n"
     "      return 1;\n"
     "    }\n"
     "  }\n"
     "  return 0;\n"
     "}\n"
     "\n"
     "static void neln_reset_slots(void) {\n"
     "  size_t i;\n"
     "  neln_slot_registry_len = 0;\n"
     "  neln_write_nil(&neln_out);\n"
     "  neln_write_nil(&neln_mirror);\n"
     "  neln_write_nil(&neln_frames);\n"
     "  neln_write_nil(&neln_scratch);\n"
     "  neln_write_nil(&neln_name_slot);\n"
     "  neln_register_slot(&neln_out);\n"
     "  neln_register_slot(&neln_mirror);\n"
     "  neln_register_slot(&neln_frames);\n"
     "  neln_register_slot(&neln_scratch);\n"
     "  neln_register_slot(&neln_name_slot);\n"
     "  for (i = 0; i < 12; i++) {\n"
     "    neln_write_nil(&neln_callback_slots[i]);\n"
     "    neln_register_slot(&neln_callback_slots[i]);\n"
     "  }\n"
     "}\n"
     "\n"
     "static int64_t neln_sexp_to_int(const NelnSexp *slot) {\n"
     "  if (slot->tag != NELN_TAG_INT) {\n"
     "    neln_fail(\"expected Sexp::Int\");\n"
     "  }\n"
     "  return (int64_t)slot->a;\n"
     "}\n"
     "\n"
     "static const char *neln_symbol_name(const NelnSexp *slot) {\n"
     "  if (slot->tag != NELN_TAG_SYMBOL) {\n"
     "    neln_fail(\"expected Sexp::Symbol\");\n"
     "  }\n"
     "  return (const char *)(uintptr_t)slot->b;\n"
     "}\n"
     "\n"
     "static const NelnSexp *neln_raw_to_sexp(int64_t raw, NelnSexp *scratch_slot) {\n"
     "  const NelnSexp *ptr = (const NelnSexp *)(uintptr_t)raw;\n"
     "  if (neln_is_registered_slot(ptr)) {\n"
     "    return ptr;\n"
     "  }\n"
     "  neln_write_int(scratch_slot, raw);\n"
     "  return scratch_slot;\n"
     "}\n"
     "\n"
     "static int64_t neln_raw_to_int(int64_t raw) {\n"
     "  const NelnSexp *ptr = (const NelnSexp *)(uintptr_t)raw;\n"
     "  if (neln_is_registered_slot(ptr)) {\n"
     "    return neln_sexp_to_int(ptr);\n"
     "  }\n"
     "  return raw;\n"
     "}\n"
     "\n"
     "static void neln_clone_into(const NelnSexp *src, NelnSexp *dst) {\n"
     "  memcpy(dst, src, sizeof(*dst));\n"
     "}\n"
     "\n"
     "static int neln_eq_sexp_p(const NelnSexp *a, const NelnSexp *b) {\n"
     "  if (a->tag != b->tag) {\n"
     "    return 0;\n"
     "  }\n"
     "  switch (a->tag) {\n"
     "  case NELN_TAG_NIL:\n"
     "  case NELN_TAG_T:\n"
     "    return 1;\n"
     "  case NELN_TAG_INT:\n"
     "    return a->a == b->a;\n"
     "  case NELN_TAG_SYMBOL:\n"
     "    return strcmp(neln_symbol_name(a), neln_symbol_name(b)) == 0;\n"
     "  case NELN_TAG_CONS:\n"
     "    return a->a == b->a;\n"
     "  default:\n"
     "    return a->a == b->a && a->b == b->b && a->c == b->c;\n"
     "  }\n"
     "}\n"
     "\n"
     "NelnSexp *nl_alloc_symbol(const unsigned char *bytes_ptr, int64_t len, NelnSexp *result_slot) {\n"
     "  size_t n = (len <= 0) ? 0u : (size_t)len;\n"
     "  size_t cap = (n == 0) ? 1u : n;\n"
     "  char *buf = (char *)calloc(cap + 1u, 1u);\n"
     "  if (!buf) {\n"
     "    neln_fail(\"calloc failed in nl_alloc_symbol\");\n"
     "  }\n"
     "  if (bytes_ptr && n > 0) {\n"
     "    memcpy(buf, bytes_ptr, n);\n"
     "  }\n"
     "  neln_clear_sexp(result_slot);\n"
     "  result_slot->tag = NELN_TAG_SYMBOL;\n"
     "  result_slot->a = (uint64_t)cap;\n"
     "  result_slot->b = (uint64_t)(uintptr_t)buf;\n"
     "  result_slot->c = (uint64_t)n;\n"
     "  return result_slot;\n"
     "}\n"
     "\n"
     "NelnSexp *nelisp_aot_builtin_call1(void *mirror, void *frames, NelnSexp *name, int64_t arg, NelnSexp *out, NelnSexp *scratch) {\n"
     "  const char *builtin = neln_symbol_name(name);\n"
     "  const NelnSexp *boxed = neln_raw_to_sexp(arg, scratch);\n"
     "  (void)mirror;\n"
     "  (void)frames;\n"
     "  if (strcmp(builtin, \"1+\") == 0) {\n"
     "    neln_write_int(out, neln_raw_to_int(arg) + 1);\n"
     "    return out;\n"
     "  }\n"
     "  if (strcmp(builtin, \"1-\") == 0) {\n"
     "    neln_write_int(out, neln_raw_to_int(arg) - 1);\n"
     "    return out;\n"
     "  }\n"
     "  if (strcmp(builtin, \"car\") == 0) {\n"
     "    if (boxed->tag != NELN_TAG_CONS) {\n"
     "      neln_fail(\"car expects a cons argument\");\n"
     "    }\n"
     "    neln_clone_into(&((const NelnConsBox *)(uintptr_t)boxed->a)->car, out);\n"
     "    return out;\n"
     "  }\n"
     "  neln_fail(\"unsupported builtin1 in host proof\");\n"
     "  return out;\n"
     "}\n"
     "\n"
     "/* SysV x86_64 lowering observed in Phase 47: the first six fixed\n"
     " * parameters consume the GP argument registers, so builtin argv\n"
     " * starts in the outgoing stack area and arrives here as a0..a7.\n"
     " * Extend this fixed window if a proof test ever needs >8 builtin\n"
     " * arguments.\n"
     " */\n"
     "NelnSexp *nelisp_aot_builtin_calln(void *mirror, void *frames, NelnSexp *name, int64_t argc, NelnSexp *out, NelnSexp *scratch,\n"
     "                                   int64_t a0, int64_t a1, int64_t a2, int64_t a3,\n"
     "                                   int64_t a4, int64_t a5, int64_t a6, int64_t a7) {\n"
     "  const char *builtin = neln_symbol_name(name);\n"
     "  (void)mirror;\n"
     "  (void)frames;\n"
     "  (void)scratch;\n"
     "  if (strcmp(builtin, \"cons\") == 0) {\n"
     "    NelnSexp tmp_a;\n"
     "    NelnSexp tmp_b;\n"
     "    const NelnSexp *a;\n"
     "    const NelnSexp *b;\n"
     "    NelnConsBox *box;\n"
     "    if (argc != 2) {\n"
     "      neln_fail(\"cons expects argc=2\");\n"
     "    }\n"
     "    a = neln_raw_to_sexp(a0, &tmp_a);\n"
     "    b = neln_raw_to_sexp(a1, &tmp_b);\n"
     "    box = (NelnConsBox *)calloc(1u, sizeof(*box));\n"
     "    if (!box) {\n"
     "      neln_fail(\"calloc failed in cons\");\n"
     "    }\n"
     "    neln_clone_into(a, &box->car);\n"
     "    neln_clone_into(b, &box->cdr);\n"
     "    box->refcount = 1u;\n"
     "    neln_clear_sexp(out);\n"
     "    out->tag = NELN_TAG_CONS;\n"
     "    out->a = (uint64_t)(uintptr_t)box;\n"
     "    return out;\n"
     "  }\n"
     "  if (strcmp(builtin, \"eq\") == 0) {\n"
     "    NelnSexp tmp_a;\n"
     "    NelnSexp tmp_b;\n"
     "    const NelnSexp *a;\n"
     "    const NelnSexp *b;\n"
     "    if (argc != 2) {\n"
     "      neln_fail(\"eq expects argc=2\");\n"
     "    }\n"
     "    a = neln_raw_to_sexp(a0, &tmp_a);\n"
     "    b = neln_raw_to_sexp(a1, &tmp_b);\n"
     "    /* Host-proof subset: emit an integer flag so direct eq defuns\n"
     "     * remain decodable without the full boxed-boolean runtime lane.\n"
     "     */\n"
     "    neln_write_int(out, neln_eq_sexp_p(a, b) ? 1 : 0);\n"
     "    return out;\n"
     "  }\n"
     "  neln_fail(\"unsupported builtinn in host proof\");\n"
     "  return out;\n"
     "}\n"
     "\n"
     (format "extern NelnSexp *call_target(%s);\n" extern-args)
     "\n"
     "static int64_t neln_decode_result(NelnSexp *ret) {\n"
     "  if (!ret) {\n"
     "    neln_fail(\"native function returned NULL\");\n"
     "  }\n"
     "  if (neln_is_registered_slot(ret)) {\n"
     "    return neln_sexp_to_int(ret);\n"
     "  }\n"
     "  return (int64_t)(intptr_t)ret;\n"
     "}\n"
     "\n"
     "int main(int argc, char **argv) {\n"
     (format "  long argv_vals[%d];\n" (max 1 arity))
     "  int i;\n"
     (format "  if (argc != %d) {\n" (1+ arity))
     "    fprintf(stderr, \"usage mismatch\\n\");\n"
     "    return 2;\n"
     "  }\n"
     "  for (i = 1; i < argc; i++) {\n"
     "    argv_vals[i - 1] = strtol(argv[i], NULL, 10);\n"
     "  }\n"
     "  neln_reset_slots();\n"
     (format "  printf(\"%%ld\\n\", neln_decode_result(call_target(%s)));\n"
             invoke-args)
     "  return 0;\n"
     "}\n")))

(defun nelisp-artifact-native-exec-general (artifact-path symbol args)
  "Host-side native EXEC proof for builtin-calling `.neln' defuns.
This links the embedded object against a generated C/asm harness that
provides the `nl_alloc_symbol', `nelisp_aot_builtin_call1', and minimal
`nelisp_aot_builtin_calln' runtime shims plus a boundary-populating
trampoline, then returns the decoded integer result."
  (unless (and (eq system-type 'gnu/linux)
               (equal (or (car-safe (split-string system-configuration "-")) "")
                      "x86_64"))
    (error "native-exec-general currently requires x86_64 Linux"))
  (let ((cc (or (executable-find "cc") (executable-find "gcc")))
        (objcopy (executable-find "objcopy")))
    (unless (and cc objcopy)
      (error "native-exec-general needs cc + objcopy on PATH"))
    (let* ((payload (nelisp-artifact--read-payload artifact-path))
           (native (plist-get payload :native))
           (b64 (and native (plist-get native :object-base64)))
           (meta (and native (nelisp-artifact--native-defun-metadata native symbol)))
           (unsupported (and native
                             (nelisp-artifact--native-general-unsupported-externs
                              native))))
      (unless b64
        (error "%s has no embedded native object" artifact-path))
      (unless meta
        (error "native symbol %s not in artifact defun metadata" symbol))
      (when unsupported
        (error "native-exec-general unsupported externs: %S" unsupported))
      (unless (equal (plist-get native :arch) "x86_64")
        (error "native-exec-general only supports x86_64 native artifacts"))
      (unless (eq (plist-get meta :param-class) 'gp)
        (error "native-exec-general only supports gp/integer defuns"))
      (unless (equal (plist-get meta :arity) (length args))
        (error "native-exec-general arity mismatch for %s: expected %d, got %d"
               symbol (plist-get meta :arity) (length args)))
      (unless (integerp (plist-get meta :body-offset))
        (error "native-exec-general requires stored :body-offset metadata"))
      (let* ((dir (make-temp-file "neln-exec-general-" t))
             (obj (expand-file-name "mod.o" dir))
             (obj2 (expand-file-name "mod-c.o" dir))
             (asrc (expand-file-name "tramp.S" dir))
             (csrc (expand-file-name "drv.c" dir))
             (exe (expand-file-name "run" dir))
             (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol)))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'binary))
                (write-region (base64-decode-string b64) nil obj nil 'silent))
              (unless (eq 0 (call-process objcopy nil nil nil
                                          (format "--redefine-sym=%s=%s" symbol csym)
                                          obj obj2))
                (error "objcopy symbol rename failed for %s" symbol))
              (with-temp-file asrc
                (insert (nelisp-artifact--native-trampoline-asm csym meta) "\n"))
              (with-temp-file csrc
                (insert (nelisp-artifact--native-driver-c csym meta)))
              (unless (eq 0 (call-process cc nil nil nil "-O2" "-c" "-o"
                                          (expand-file-name "tramp.o" dir) asrc))
                (error "native trampoline assembly failed for %s" symbol))
              (unless (eq 0 (call-process cc nil nil nil "-O2" "-c" "-o"
                                          (expand-file-name "drv.o" dir) csrc))
                (error "native driver compile failed for %s" symbol))
              (unless (eq 0 (call-process cc nil nil nil "-O2" "-o" exe
                                          (expand-file-name "drv.o" dir)
                                          (expand-file-name "tramp.o" dir)
                                          obj2))
                (error "native general link failed for %s" symbol))
              (with-temp-buffer
                (apply #'call-process exe nil t nil
                       (mapcar #'number-to-string args))
                (string-to-number (string-trim (buffer-string)))))
          (delete-directory dir t))))))

(defun nelisp-artifact--eval-forms (forms &optional kind)
  "Evaluate CLI FORMS after loading an artifact, returning the last value.
For `nelc'/`neln' the module installs onto the NeLisp runtime, so FORMS
are evaluated with `nelisp-eval'.  For a `.elc' (Doc 142 §6.2) the module
is loaded into host Emacs, so FORMS are evaluated with host `eval'."
  (let ((source (nelisp-artifact--join-forms forms))
        (last nil))
    (dolist (form (nelisp-artifact--read-all-from-string source))
      (setq last (if (eq kind 'elc) (eval form t) (nelisp-eval form))))
    last))

(defun nelisp-artifact--parse-compile-args (args)
  "Parse `compile-elisp-artifact' ARGS into a plist."
  (let ((rest (cdr args))
        (kind nil)
        (input nil)
        (output nil)
        (manifest nil)
        (target nil)
        (load-paths nil)
        (preloads nil)
        (requested-feature nil))
    (while rest
      (let ((flag (car rest))
            (value (cadr rest)))
        (cond
         ((or (equal flag "--kind")
              (equal flag "--input")
              (equal flag "--output")
              (equal flag "--manifest")
              (equal flag "--target")
              (equal flag "--load-path")
              (equal flag "--preload")
              (equal flag "--feature")
              (equal flag "--cache-key"))
          (unless value
            (error "missing value for %s" flag))
          (cond
           ((equal flag "--kind") (setq kind value))
           ((equal flag "--input") (setq input value))
           ((equal flag "--output") (setq output value))
           ((equal flag "--manifest") (setq manifest value))
           ((equal flag "--target") (setq target value))
           ((equal flag "--load-path")
            (setq load-paths (append load-paths (list value))))
           ((equal flag "--preload")
            (setq preloads (append preloads (list value))))
           ((equal flag "--feature")
            (setq requested-feature (intern value))))
          (setq rest (cddr rest)))
         (t
          (error "unknown flag %s" flag)))))
    (unless (and kind input output)
      (error "compile-elisp-artifact requires --kind, --input, and --output"))
    (unless (member kind '("nelc" "neln" "elc" "auto"))
      (error "unsupported --kind %s" kind))
    ;; Resolve `auto' from the output suffix (Doc 142 §6.5).
    (let ((resolved (cond ((equal kind "auto")
                           (cond ((string-suffix-p ".neln" output) "neln")
                                 ((string-suffix-p ".elc" output) "elc")
                                 (t "nelc")))
                          (t kind))))
      (cond
       ((and (equal resolved "nelc") (not (string-suffix-p ".nelc" output)))
        (error "compile-elisp-artifact --kind nelc output must use the .nelc suffix"))
       ((and (equal resolved "neln") (not (string-suffix-p ".neln" output)))
        (error "compile-elisp-artifact --kind neln output must use the .neln suffix"))
       ((and (equal resolved "elc") (not (string-suffix-p ".elc" output)))
        (error "compile-elisp-artifact --kind elc output must use the .elc suffix")))
      (let ((expected-manifest (nelisp-artifact--sibling-manifest-path output)))
        (when (and manifest (not (equal manifest expected-manifest)))
          (error "manifest must be %s" expected-manifest))
        (list :kind resolved
              :input input
              :output output
              :manifest expected-manifest
              :target target
              :load-paths load-paths
              :preloads preloads
              :requested-feature requested-feature)))))

(defun compile-elisp-artifact (args)
  "CLI entry point for `nelisp compile-elisp-artifact'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-compile-args args))
             (kind (intern (plist-get opts :kind))))
        (if (eq kind 'elc)
            (nelisp-artifact-compile-elc-file
             (plist-get opts :input)
             (plist-get opts :output)
             (plist-get opts :manifest)
             (plist-get opts :target)
             (plist-get opts :load-paths)
             (plist-get opts :preloads)
             (plist-get opts :requested-feature))
          (nelisp-artifact-compile-file
           (plist-get opts :input)
           (plist-get opts :output)
           (plist-get opts :manifest)
           (plist-get opts :target)
           (plist-get opts :load-paths)
           (plist-get opts :preloads)
           (plist-get opts :requested-feature)
           kind))
        0)
    (error
     (nelisp-artifact--print-error
      (format "compile-elisp-artifact: %s" (error-message-string err)))
     1)))

(defun exec-elisp-artifact (args)
  "CLI entry point for `nelisp exec-elisp-artifact'."
  (let ((path (nth 1 args))
        (forms (cddr args)))
    (if (or (null path) (null forms))
        (progn
          (nelisp-artifact--print-error nelisp-artifact--usage)
          2)
      (condition-case err
          (let ((kind (nelisp-artifact--artifact-kind path)))
            (nelisp-artifact-load-file path)
            (nelisp-artifact--eval-forms forms kind)
            0)
        (error
         (nelisp-artifact--print-error
          (format "exec-elisp-artifact: artifact=%s format=%s phase=load/eval: %s"
                  path nelisp-artifact--format (error-message-string err)))
         1)))))

(defun eval-elisp-artifact (args)
  "CLI entry point for `nelisp eval-elisp-artifact'."
  (let ((path (nth 1 args))
        (forms (cddr args)))
    (if (or (null path) (null forms))
        (progn
          (nelisp-artifact--print-error nelisp-artifact--usage)
          2)
      (condition-case err
          (let ((last nil)
                (kind (nelisp-artifact--artifact-kind path)))
            (nelisp-artifact-load-file path)
            (setq last (nelisp-artifact--eval-forms forms kind))
            (nelisp-artifact--write-stdout (prin1-to-string last))
            (nelisp-artifact--write-stdout "\n")
            0)
        (error
         (nelisp-artifact--print-error
          (format "eval-elisp-artifact: artifact=%s format=%s phase=load/eval: %s"
                  path nelisp-artifact--format (error-message-string err)))
         1)))))

(defun inspect-elisp-artifact (args)
  "CLI entry point for `nelisp inspect-elisp-artifact'."
  (let ((path (nth 1 args)))
    (if (null path)
        (progn
          (nelisp-artifact--print-error nelisp-artifact--usage)
          2)
      (condition-case err
          (let ((manifest (nelisp-artifact-read-manifest path)))
            (nelisp-artifact--write-stdout (prin1-to-string manifest))
            (nelisp-artifact--write-stdout "\n")
            0)
        (error
         (nelisp-artifact--print-error
          (format "inspect-elisp-artifact: artifact=%s format=%s phase=inspect: %s"
                  path nelisp-artifact--format (error-message-string err)))
         1)))))

(provide 'nelisp-artifact)

;;; nelisp-artifact.el ends here
