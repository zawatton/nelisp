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
;; path does not pull the (heavy) AOT compiler at require time.
(declare-function nelisp-aot-compile-to-object "nelisp-aot-compiler"
                  (sexp file-path &rest keys))
(declare-function nelisp-aot-compile-to-link-unit "nelisp-aot-compiler"
                  (sexp &rest keys))
(declare-function nelisp-elf--build-rel "nelisp-elf-write" (plist))
(declare-function nelisp-elf-write-binary "nelisp-elf-write"
                  (file-path sections))
(declare-function nelisp--rd-one "nelisp-stdlib-prelude"
                  (source position length))
(declare-function nelisp--read-all-from-string-native "nelisp-standalone"
                  (source))
(declare-function nelisp--read-batch-from-string-native "nelisp-standalone"
                  (source position count))

(defvar nelisp-aot-compiler--external-native-symbols nil
  "Artifact-wide native call allowlist, dynamically bound during AOT passes.")

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
;; ET_REL object (AOT output) that a standalone runtime can mmap+exec
;; as an optimisation.  The native object is base64'd into the artifact's
;; `:native' section; on host the bytecode lane is used and the native
;; section is metadata only.
(defconst nelisp-artifact--native-runtime-abi "nelisp-neln-aot-v1")
(defconst nelisp-artifact--native-class 'native)
(defconst nelisp-artifact--native-object-format 'nelisp-aot-elf-v1)
(defconst nelisp-artifact--native-section-version 5)
(defconst nelisp-artifact--legacy-self-sized-native-section-version 4)
(defconst nelisp-artifact--legacy-compact-native-section-version 3)
(defconst nelisp-artifact--compact-reloc-format 'indexed-plt32-v1)
(defconst nelisp-artifact--layout-version 2)
(defconst nelisp-artifact--legacy-offset-layout-version 1)
(defconst nelisp-artifact--native-runtime-prefix-layout-version 2)

(defconst nelisp-artifact--supported-runtime-externs
  '("nl_alloc_symbol"
    "nl_alloc_str"
    "nl_alloc_mut_str"
    "nl_mut_str_push_byte"
    "nl_mut_str_finalize"
    "nl_vector_slot_ptr"
    "nl_word_int_payload"
    "nl_val_clone_into"
    "nelisp_env_lookup_value"
    "nl_alloc_consbox"
    "nelisp_env_set_value"
    "nl_alloc_vector"
    "nl_alloc_bytes"
    "nl_vector_set_slot"
    "nelisp_aot_builtin_call1"
    "nelisp_aot_builtin_calln"
    "nelisp_aot_errorn"
    "nelisp_aot_funcall1"
    "nelisp_aot_funcall2"
    "nelisp_aot_funcall3"
    "nelisp_aot_funcalln"
    "nelisp_aot_apply"
    "nelisp_aot_applyn"
    "nelisp_aot_listn")
  "Runtime externs supported by the standalone native resolver.")
(defconst nelisp-artifact--flat-image-cache-format
  'nelisp-flat-image-cache-v1)
(defconst nelisp-artifact--flat-image-abi
  "nelisp-flat-arena-stream-v1")
(defconst nelisp-artifact--usage
  "usage: nelisp compile-elisp-artifact --kind nelc|neln|elc|auto --input FILE.el --output FILE.nelc|FILE.neln|FILE.elc [--manifest FILE.manifest.el] [--load-path DIR]... [--preload FILE.el]... [--feature FEATURE] [--target TARGET] [--native-policy opportunistic|required] [--module-policy bytecode|eval-only] [--rewrite-defalias-late] [--profile-stages] [--profile-forms] [--cache-key KEY]
       nelisp compile-elisp-artifacts --kind nelc|neln|auto [--load-path DIR]... [--preload FILE.el]... [--target TARGET] [--native-policy opportunistic|required] [--module-policy bytecode|eval-only] [--profile-stages] [--profile-forms] FILE.el|DIR...
       nelisp compile-runtime-image --kind nelc|neln|auto --input FILE.nlri --output FILE.nelc|FILE.neln|FILE.wasm [--target TARGET] [--native-policy opportunistic|required] [--module-policy bytecode|eval-only] [--rewrite-defalias-late] [--profile-stages] [--profile-forms]
       nelisp compile-runtime-image --flat-artifact-cache --runtime NELISP --input FILE.neln --output FILE.flat.nlri [--profile-load] [--profile-load-detail]
       nelisp audit-elisp-artifacts [--required] FILE.el|FILE.neln|DIR...
       nelisp exec-elisp-artifact FILE.nelc|FILE.neln|FILE.elc FORM...
       nelisp eval-elisp-artifact FILE.nelc|FILE.neln|FILE.elc FORM...
       nelisp load-elisp-source [--auto-compile] [--kind nelc|neln] FILE.el
       nelisp eval-elisp-source [--auto-compile] [--kind nelc|neln] FILE.el FORM...
       nelisp native-exec-elisp-artifact FILE.neln SYMBOL ARG...
       nelisp inspect-elisp-artifact FILE.nelc|FILE.neln|FILE.elc
  (.nelc = NeLisp bytecode module; .neln = bytecode + embedded native object;
   .elc = genuine GNU Emacs byte-compiled module, Doc 142 §6.2)")

(defvar nelisp-artifact--loaded nil
  "Absolute `.nelc' paths already replayed in this process.")

(defvar nelisp-artifact--flat-dump-temp nil
  "GC-rooted path used while a flat image dump is being finalized.")

(defvar nelisp-artifact--flat-header-stage nil
  "Last bounded flat-header validation stage, for failure diagnostics.")

(defvar nelisp-artifact--flat-header-bytes nil
  "GC-rooted 64-byte header during standalone binary decoding.")

(defvar nelisp-artifact--flat-generation-token nil
  "Artifact/runtime identity token persisted in a flat arena image.")

(defvar nelisp-artifact--native-section-registry nil
  "Alist mapping artifact paths to serialized native sections.")

(defvar nelisp-artifact--native-runtime-mappings nil
  "Process-local native mapping cache.
This is always reset to nil before a flat arena dump; executable addresses
must never be persisted into a cold image.")

(defvar nelisp-artifact--native-artifact-linksets nil
  "Alist of artifact paths to committed transactional native linksets.")

(defvar nelisp-artifact--native-artifact-symbol-index nil
  "Alist of artifact paths to committed last-wins native symbol indexes.")

(defvar nelisp-artifact--native-link-diagnostics nil
  "Newest-first artifact native linker diagnostics.")

(defvar nelisp-artifact--native-last-preflight-duplicates nil
  "Duplicate diagnostics accumulated by the latest native preflight.")

(defvar nelisp-artifact-native-dispatch-enabled t
  "Non-nil means loaded `.neln' functions try native dispatch first.")

(defvar nelisp-artifact-native-dispatch-report nil
  "Most recent native dispatch install/call report entries.")

(defvar nelisp-artifact-native-exec-cache-enabled t
  "Non-nil means native fast exec reuses linked driver executables.")

(defvar nelisp-artifact-fast-integrity-validation t
  "Non-nil means private artifact loads may skip sha256 on exact size match.
The sibling manifest still records the full artifact sha256.  This flag only
changes the hot load path for NeLisp-private `.nelc' / `.neln' artifacts:
when the manifest's `:artifact-size' equals the on-disk artifact size, the
loader treats the artifact as intact and avoids the standalone `sha256sum'
subprocess fixed cost.  Set nil to force full sha256 validation on every load.")

(defvar nelisp-artifact-fast-private-read t
  "Non-nil means generated private artifacts use keyword-value fast readers.
The private `.nelc' / `.neln' artifact and manifest files are generated by this
module with stable plist ordering.  On standalone NeLisp, reading the entire
plist just to obtain `:module-init', `:features', and manifest metadata is a
large fixed cost.  The fast reader scans generated keyword positions and parses
only the needed values.  Fast-reader errors fail closed; set this variable to
nil explicitly to select the full plist reader.")

(defvar nelisp-artifact-module-replay-chunk-size 64
  "Number of ordinary module items per standalone replay source chunk.
When `nelisp--eval-source-string' is available, streaming artifact replay wraps
ordinary raw module items in small top-level calls and sends each chunk through
that evaluator.  Its native top-level loop reaches the existing safe
form-boundary collector after every wrapper, so already-consumed reader ASTs do
not remain live until the entire `:module-init' list finishes.  Nil or a
non-positive value keeps the direct item reader, as on host Emacs.")

(defvar nelisp-artifact--last-native-compile-report nil
  "Most recent `.neln' native compile coverage report.")

(defvar nelisp-artifact-default-native-policy 'opportunistic
  "Default native policy for `.neln' artifact compilation.
`opportunistic' means every `.el' file can produce a `.neln' artifact:
native-eligible top-level defuns are compiled, while unsupported defuns and
non-defun top-level forms keep bytecode/eval fallback.  `required' means every
top-level defun must enter the native section; otherwise compilation fails
before writing the artifact pair.")

(defvar nelisp-artifact-default-native-defun-budget 32
  "Conservative upper bound for defuns per serialized native section.
This budget is applied to `.neln' native section sharding so large sources do
not hand one giant defun batch to `nelisp-aot-compile-to-link-unit', and so
standalone replay can parse each serialized native section inside an 8 GB
process without crossing an unsafe GC boundary.  The companion 512 KiB
serialized-section budget recursively bisects a batch that still grows too
large, so files with more native-eligible defuns than this limit are sharded
into `:native-sections'.
Artifacts at or below the limit still serialize a single legacy `:native'
section for compatibility.  Raise this only when both compiler-host memory and
standalone replay memory/GC headroom are proven safe.")

(defvar nelisp-artifact-default-native-section-byte-budget (* 512 1024)
  "Serialized byte budget for one opportunistic native section.
Successful opportunistic batch compiles are measured using the stable artifact
representation that standalone replay actually reads: the UTF-8 byte length of
`(prin1-to-string SECTION)' when `string-bytes' is available, falling back to
character length otherwise.  Oversized multi-defun batches are treated like a
batch compile failure and recursively bisected; an oversized singleton defun
falls back to bytecode/source replay with a clear `:reason' entry so the
normal non-native lanes remain available.")

(defvar nelisp-artifact-default-module-policy 'bytecode
  "Default module compile policy for private `.nelc' / `.neln' artifacts.
`bytecode' preserves the normal behavior: eligible top-level defuns are lowered
to NeLisp bytecode closures and other forms replay through `nelisp-eval'.
`eval-only' skips bytecode lowering and records every top-level form as replay.
It is intended for very large bootstrap substrates where proving the cache
boundary matters before the bytecode compiler is fast enough for CI.")

(defvar nelisp-artifact-source-transform-function nil
  "Optional function that rewrites raw SOURCE before artifact compilation.
The function is called as (FUNCTION SOURCE SOURCE-PATH) and must return the
transformed source string used for top-level form parsing, module generation,
and native section compilation.  Source freshness continues to track the
original SOURCE-PATH contents so cache invalidation stays tied to the raw
input file.")

(defvar nelisp-artifact--rewrite-defalias-late nil
  "Non-nil means rewrite `defalias' forms to `nelisp--defalias-late'.
The rewrite is applied structurally after top-level forms are read, before
module and native compilation consume them.")

(defvar nelisp-artifact-cache-directory nil
  "Optional artifact cache root.
When nil, artifacts are written adjacent to SOURCE-PATH as before.  When
non-nil, `nelisp-artifact-source-artifact-path' maps each source into a stable
hashed subdirectory under this root so same-named files from different source
trees do not collide.")

(defvar nelisp-artifact-profile-stages nil
  "Non-nil means artifact compile commands emit stage timings to stderr.")

(defvar nelisp-artifact-profile-forms nil
  "Non-nil means artifact compile commands emit per-form reader timings.")

(defvar nelisp-artifact-profile-load nil
  "Non-nil means private artifact loads emit aggregate timings to stderr.")

(defvar nelisp-artifact-profile-load-detail nil
  "Non-nil means artifact load profiling also emits debug detail.
This opt-in mode includes individual parser stages, per-section timings, and
periodic replay progress.  Normal `nelisp-artifact-profile-load' output stays
bounded to native, module, and total aggregate wall times.")

(defvar nelisp-artifact-raw-eval-source-threshold nil
  "Minimum source byte length for raw eval-only `.nelc' module serialization.
Nil keeps the experimental raw-source representation disabled by default.")

(defun nelisp-artifact--profile-time ()
  "Return a monotonic-enough timestamp for artifact stage profiling."
  (if (fboundp 'float-time) (float-time) 0.0))

(defun nelisp-artifact--profile-log (stage start &optional detail)
  "Emit an artifact profile line for STAGE since START.
DETAIL, when non-nil, is appended as a compact Lisp datum."
  (when nelisp-artifact-profile-stages
    (nelisp-artifact--write-stderr
     (concat "artifact_profile stage=" stage
             " elapsed_ms="
             (number-to-string
              (* 1000.0 (- (nelisp-artifact--profile-time) start)))
             (if detail
               (concat " detail=" (prin1-to-string detail))
               "")))))

(defun nelisp-artifact--load-profile-log (stage start &optional detail)
  "Emit an artifact load profile line for STAGE since START.
DETAIL, when non-nil, is appended as a compact Lisp datum.  Normal load
profiling emits only bounded aggregate stages; debug detail is an explicit
opt-in because formatting and printing many lines distorts standalone timing."
  (when (and nelisp-artifact-profile-load
             (or nelisp-artifact-profile-load-detail
                 (member stage '("native-total" "module-total"
                                 "load-total"))))
    (nelisp-artifact--write-stderr
     (concat "artifact_load_profile stage=" stage
             " elapsed_ms="
             (number-to-string
              (* 1000.0 (- (nelisp-artifact--profile-time) start)))
             (if detail
                 (concat " detail=" (prin1-to-string detail))
               "")))))

(defun nelisp-artifact--form-profile-head (form)
  "Return a compact label for FORM in per-form artifact profiling."
  (cond
   ((and (consp form) (symbolp (car form)))
    (symbol-name (car form)))
   ((symbolp form)
    (symbol-name form))
   ((consp form)
    "list")
   ((stringp form)
    "string")
   ((integerp form)
    "integer")
   (t "atom")))

(defvar nelisp-artifact-standalone-repo-root nil
  "Optional repository root used by standalone artifact commands.
The standalone reader command path is generated from
`scripts/nelisp-standalone-build.el' and may know the source checkout root even
when `default-directory' and OS environment variables are unavailable inside the
NeLisp runtime.")

(defvar nelisp-artifact-standalone-target nil
  "Standalone build target symbol, e.g. `windows-x86_64' or `linux-x86_64'.
Baked in as a literal by `scripts/nelisp-standalone-build.el' alongside
`nelisp-artifact-standalone-repo-root'; nil/unbound outside a generated
standalone runtime.")

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

(defun nelisp-artifact--call-process-quiet (program log-file &rest args)
  "Run PROGRAM with ARGS, redirecting stdout/stderr to LOG-FILE when possible."
  (let ((sh (and (fboundp 'executable-find) (executable-find "sh"))))
    (if sh
        (apply #'call-process
               sh nil nil nil "-c"
               "program=$1; log=$2; shift 2; \"$program\" \"$@\" >\"$log\" 2>&1"
               "nelisp-quiet-call" program log-file args)
      (apply #'call-process program nil nil nil args))))

(defun nelisp-artifact--read-log-if-exists (path)
  "Return a compact log excerpt from PATH, or an empty string."
  (if (and path (file-exists-p path))
      (string-trim (nelisp-artifact--read-file-as-string path))
    ""))

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

(defun nelisp-artifact--string-search-literal (needle haystack &optional start)
  "Return the first index of NEEDLE in HAYSTACK at or after START.
This avoids depending on regexp/search helpers in standalone native-exec
hot paths."
  (if (fboundp 'nelisp--string-search)
      (let ((found (nelisp--string-search needle haystack (or start 0))))
        (and (integerp found) (>= found 0) found))
    (if (fboundp 'string-search)
        (string-search needle haystack (or start 0))
      (let* ((i (or start 0))
             (needle-len (length needle))
             (hay-len (length haystack))
             (limit (- hay-len needle-len))
             (found nil))
        (while (and (not found) (<= i limit))
          (let ((j 0)
                (ok t))
            (while (and ok (< j needle-len))
              (unless (= (aref needle j) (aref haystack (+ i j)))
                (setq ok nil))
              (setq j (1+ j)))
            (if ok
                (setq found i)
              (setq i (1+ i)))))
        found))))

(defun nelisp-artifact--string-search-char-bounded (ch source start limit)
  "Return the first index of character CH in SOURCE within [START, LIMIT).
`nelisp--string-search' is a native builtin and returns the first match at or
after START, so bounding its answer is the same predicate as walking the range
by hand -- and the walk is what costs: measured on one section's base64
payload (53,336 characters) the loop takes 3,357.3 ms against 0.587 ms for the
native search, about 248 s of `native-total' across 74 sections.  Keep the
loop for runtimes without the builtin."
  (if (fboundp 'nelisp--string-search)
      (let ((hit (nelisp--string-search (char-to-string ch) source start)))
        (and hit (< hit limit) hit))
    (let ((i start) (found nil))
      (while (and (null found) (< i limit))
        (if (= (aref source i) ch) (setq found i) (setq i (1+ i))))
      found)))

(defun nelisp-artifact--string-prefix-at-p (prefix source pos)
  "Return non-nil when PREFIX occurs in SOURCE at POS."
  (let ((i 0)
        (n (length prefix))
        (len (length source))
        (ok t))
    (while (and ok (< i n))
      (if (or (>= (+ pos i) len)
              (not (= (aref prefix i) (aref source (+ pos i)))))
          (setq ok nil)
        (setq i (1+ i))))
    ok))

(defun nelisp-artifact--canonical-integer-token-p (text)
  "Return non-nil when TEXT is exactly Emacs' canonical integer spelling."
  (let ((i 0)
        (n (and (stringp text) (length text)))
        (ok nil))
    (when (and n (> n 0))
      (when (and (> n 1) (= (aref text 0) ?-))
        (setq i 1))
      (setq ok (< i n))
      (while (and ok (< i n))
        (unless (let ((ch (aref text i)))
                  (and (>= ch ?0) (<= ch ?9)))
          (setq ok nil))
        (setq i (1+ i)))
      (when (and ok (> n 1) (= (aref text 0) ?0))
        (setq ok nil))
      (when (and ok (> n 2) (= (aref text 0) ?-) (= (aref text 1) ?0))
        (setq ok nil)))
    ok))

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

(defun nelisp-artifact--write-base64-decoded-file (base64 path)
  "Decode BASE64 into PATH.
Native artifacts embed object bytes as base64 text.  Prefer the system
`base64 -d' command when available so standalone NeLisp does not build a
large binary string through the compatibility layer."
  (let ((decoder (and (fboundp 'executable-find)
                      (executable-find "base64"))))
    (if decoder
        (let ((encoded-path (concat path ".b64")))
          (unwind-protect
              (progn
                (nelisp-artifact--delete-if-exists encoded-path)
                (write-region base64 nil encoded-path nil 'silent)
                (nelisp-artifact--delete-if-exists path)
                (unless (eq 0 (call-process
                               decoder encoded-path
                               (if (fboundp 'nelisp-process-call-process)
                                   path
                                 (list :file path))
                               nil "-d"))
                  (nelisp-artifact--delete-if-exists path)
                  (let ((coding-system-for-write 'binary))
                    (write-region (base64-decode-string base64)
                                  nil path nil 'silent)))
                t)
            (nelisp-artifact--delete-if-exists encoded-path)))
      (let ((coding-system-for-write 'binary))
        (write-region (base64-decode-string base64) nil path nil 'silent))
      t)))

(defun nelisp-artifact--write-native-object-file
    (artifact-path path &optional native-section)
  "Extract ARTIFACT-PATH's embedded native object into PATH.
Use a small external pipeline first.  This avoids pulling the whole `.neln'
payload, including the large base64 object string, through standalone
NeLisp's reader/string compatibility layer.  When NATIVE-SECTION is non-nil,
extract that already-selected serialized section; this is required for
sharded artifacts whose duplicate exports use last-section-wins semantics."
  (let ((sh (and (null native-section)
                 (fboundp 'executable-find)
                 (executable-find "sh")))
        (script "sed -n 's/.*:object-base64 \"\\([^\"]*\\)\" :text-size.*/\\1/p' \"$1\" | base64 -d > \"$2\""))
    (nelisp-artifact--delete-if-exists path)
    (if native-section
        (let ((base64 (plist-get native-section :object-base64)))
          (unless (stringp base64)
            (error "%s native section has no embedded object" artifact-path))
          (nelisp-artifact--write-base64-decoded-file base64 path))
      (if (and sh
               (eq 0 (call-process sh nil nil nil
                                   "-c" script
                                   "nelisp-artifact-object"
                                   artifact-path path))
               (file-exists-p path)
               (> (nelisp-artifact--file-size path) 0))
          t
        (nelisp-artifact--delete-if-exists path)
        (nelisp-artifact--write-base64-decoded-file
         (nelisp-artifact--read-native-object-base64 artifact-path)
         path)))))

(defun nelisp-artifact--read-native-object-base64 (artifact-path)
  "Return ARTIFACT-PATH's embedded native object base64 text.
Native execution only needs the object payload; metadata comes from the
sibling manifest.  Scanning the payload avoids reading the whole `.neln'
plist through the standalone reader."
  (let* ((content (nelisp-artifact--read-file-as-string artifact-path))
         (marker ":object-base64 \"")
         (start (nelisp-artifact--string-search-literal marker content)))
    (unless start
      (error "%s has no embedded native object" artifact-path))
    (setq start (+ start (length marker)))
    (let ((i start)
          (len (length content))
          (escaped nil)
          (done nil))
      (while (and (< i len) (not done))
        (let ((ch (aref content i)))
          (cond
           (escaped
            (setq escaped nil))
           ((= ch ?\\)
            (setq escaped t))
           ((= ch ?\")
            (setq done t)))
          (unless done
            (setq i (1+ i)))))
      (unless done
        (error "unterminated native object base64 in %s" artifact-path))
      (substring content start i))))

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

(defun nelisp-artifact--make-temp-directory (prefix)
  "Return a new temporary directory named with PREFIX.
Prefer system `mktemp -d' because standalone NeLisp's compatibility
`make-temp-file' can collide across concurrent native-exec processes."
  (let* ((base (if (and (boundp 'temporary-file-directory)
                        (stringp temporary-file-directory))
                   temporary-file-directory
                 "/tmp/"))
         (template (expand-file-name (concat prefix ".XXXXXX") base))
         (mktemp (and (fboundp 'executable-find)
                      (executable-find "mktemp"))))
    (or (and mktemp
             (condition-case nil
                 (with-temp-buffer
                   (when (eq 0 (call-process mktemp nil t nil "-d" template))
                     (let ((path (string-trim (buffer-string))))
                       (and (> (length path) 0)
                            (file-directory-p path)
                            path))))
               (error nil)))
        (make-temp-file prefix t))))

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

(defun nelisp-artifact--file-ctime (path)
  "Return PATH status-change time."
  (let ((attrs (file-attributes path)))
    (if (fboundp 'file-attribute-status-change-time)
        (file-attribute-status-change-time attrs)
      (nth 6 attrs))))

(defun nelisp-artifact--file-record (path)
  "Return a cache-key record for PATH."
  (let ((abs (expand-file-name path)))
    (list :path abs
          :truename (file-truename abs)
          :sha256 (secure-hash 'sha256
                               (nelisp-artifact--read-file-as-string abs))
          :size (nelisp-artifact--file-size abs)
          :mtime (nelisp-artifact--file-mtime abs)
          :ctime (nelisp-artifact--file-ctime abs))))

(defun nelisp-artifact--sibling-manifest-path (artifact-path)
  "Return the sibling manifest path for ARTIFACT-PATH."
  (concat artifact-path ".manifest.el"))

(defun nelisp-artifact--source-cache-root (source-path)
  "Return the cache directory for SOURCE-PATH, or nil for adjacent writes."
  (when nelisp-artifact-cache-directory
    (let* ((base (file-name-as-directory
                  (expand-file-name nelisp-artifact-cache-directory)))
           (abs (expand-file-name source-path))
           (identity (if (file-exists-p abs)
                         (file-truename abs)
                       abs))
           (hash (secure-hash 'sha256 identity)))
      (expand-file-name hash base))))

(defun nelisp-artifact--source-artifact-path-in-cache (source-path kind)
  "Return the cached artifact path for SOURCE-PATH and KIND."
  (let* ((source (expand-file-name source-path))
         (base (file-name-nondirectory source))
         (root (nelisp-artifact--source-cache-root source-path)))
    (expand-file-name (concat base "." (symbol-name kind)) root)))

(defun nelisp-artifact--read-top-level-forms-with (source reader &optional label)
  "Read every top-level form from SOURCE using READER.
When `nelisp-artifact-profile-forms' is non-nil, emit one stderr profile line
per top-level form.  LABEL identifies the source in that opt-in output."
  (let ((pos 0)
        (len (length source))
        (forms nil)
        (index 0))
    (while (progn
             (setq pos (nelisp-read--skip-ws source pos))
             (< pos len))
      (let* ((form-start pos)
             (start (nelisp-artifact--profile-time))
             (res (funcall reader source pos))
             (form (car res))
             (form-end (cdr res)))
        (when nelisp-artifact-profile-forms
          (nelisp-artifact--write-stderr
           (concat "artifact_profile_form"
                   " source=" (prin1-to-string (or label "<string>"))
                   " index=" (number-to-string index)
                   " start=" (number-to-string form-start)
                   " end=" (number-to-string form-end)
                   " elapsed_ms="
                   (number-to-string
                    (* 1000.0 (- (nelisp-artifact--profile-time) start)))
                   " head="
                   (prin1-to-string
                    (nelisp-artifact--form-profile-head form)))))
        (push (car res) forms)
        (setq pos (cdr res))
        (setq index (1+ index))))
    (nreverse forms)))

(defun nelisp-artifact--read-top-level-forms-rd-one (source &optional label)
  "Read top-level forms from SOURCE through standalone prelude `nelisp--rd-one'.
This avoids the standalone `read-from-string' START path, which currently
copies/reparses a suffix for each top-level form.  It is used only when the
runtime exposes `nelisp--rd-one'; callers wrap it in a fallback."
  (let ((pos 0)
        (len (length source))
        (forms nil)
        (index 0))
    (while (progn
             (setq pos (nelisp-read--skip-ws source pos))
             (< pos len))
      (let* ((form-start pos)
             (start (nelisp-artifact--profile-time))
             (res (nelisp--rd-one source pos len))
             (form (car res))
             (form-end (cdr res)))
        (when nelisp-artifact-profile-forms
          (nelisp-artifact--write-stderr
           (concat "artifact_profile_form"
                   " source=" (prin1-to-string (or label "<string>"))
                   " index=" (number-to-string index)
                   " start=" (number-to-string form-start)
                   " end=" (number-to-string form-end)
                   " elapsed_ms="
                   (number-to-string
                    (* 1000.0 (- (nelisp-artifact--profile-time) start)))
                   " head="
                   (prin1-to-string
                    (nelisp-artifact--form-profile-head form)))))
        (push form forms)
        (setq pos form-end)
        (setq index (1+ index))))
    (nreverse forms)))

(defun nelisp-artifact--read-top-level-forms-fallback (source &optional label)
  "Read every top-level form from SOURCE through portable readers."
  (if (fboundp 'nelisp--rd-one)
      (condition-case nil
          (nelisp-artifact--read-top-level-forms-rd-one source label)
        (error
         (if (fboundp 'read-from-string)
             (condition-case nil
                 (nelisp-artifact--read-top-level-forms-with
                  source (lambda (text pos) (read-from-string text pos)) label)
               (error
                (nelisp-artifact--read-top-level-forms-with
                 source #'nelisp-read--sexp label)))
           (nelisp-artifact--read-top-level-forms-with
            source #'nelisp-read--sexp label))))
    (if (fboundp 'read-from-string)
        (condition-case nil
            (nelisp-artifact--read-top-level-forms-with
             source (lambda (text pos) (read-from-string text pos)) label)
          (error
           (nelisp-artifact--read-top-level-forms-with
            source #'nelisp-read--sexp label)))
      (nelisp-artifact--read-top-level-forms-with
       source #'nelisp-read--sexp label))))

(defun nelisp-artifact--read-top-level-forms (source &optional label)
  "Read every top-level form from SOURCE.
Prefer the standalone native all-forms reader when it is callable and per-form
profiling is disabled.  Fall back to the portable top-level readers when
profiling needs source positions or when the native reader is unavailable."
  (if (and (not nelisp-artifact-profile-forms)
           (fboundp 'nelisp--read-all-from-string-native))
      (condition-case nil
          (nelisp--read-all-from-string-native source)
        (error
         (nelisp-artifact--read-top-level-forms-fallback source label)))
    (nelisp-artifact--read-top-level-forms-fallback source label)))

(defun nelisp-artifact--source-skip-ws-comments (source pos)
  "Return first non-whitespace/comment position in SOURCE at or after POS."
  (let ((len (length source))
        (done nil))
    (while (and (< pos len) (not done))
      (let ((ch (aref source pos)))
        (cond
         ((or (= ch ?\s) (= ch ?\t) (= ch ?\r) (= ch ?\n))
          (setq pos (1+ pos)))
         ((= ch ?\;)
          (while (and (< pos len) (not (= (aref source pos) ?\n)))
            (setq pos (1+ pos))))
         (t
          (setq done t)))))
    pos))

(defun nelisp-artifact--source-string-end (source pos)
  "Return one past the string starting at POS in SOURCE."
  (let ((len (length source))
        (i (1+ pos))
        (escaped nil)
        (done nil))
    (while (and (< i len) (not done))
      (let ((ch (aref source i)))
        (cond
         (escaped
          (setq escaped nil))
         ((= ch ?\\)
          (setq escaped t))
         ((= ch ?\")
          (setq done t))))
      (setq i (1+ i)))
    (unless done
      (error "unterminated source string"))
    i))

(defun nelisp-artifact--source-container-end (source pos)
  "Return one past the list/vector container starting at POS in SOURCE."
  (let ((len (length source))
        (i pos)
        (depth 0)
        (in-string nil)
        (atom-escaped nil)
        (escaped nil)
        (done nil))
    (while (and (< i len) (not done))
      (let ((ch (aref source i)))
        (cond
         (in-string
          (cond
           (escaped
            (setq escaped nil))
           ((= ch ?\\)
            (setq escaped t))
           ((= ch ?\")
            (setq in-string nil))))
         (atom-escaped
          (setq atom-escaped nil))
         ((= ch ?\\)
          (setq atom-escaped t))
         ((= ch ?\")
          (setq in-string t))
         ((= ch ?\;)
          (while (and (< i len) (not (= (aref source i) ?\n)))
            (setq i (1+ i))))
         ((or (= ch ?\() (= ch ?\[))
          (setq depth (1+ depth)))
         ((or (= ch ?\)) (= ch ?\]))
          (setq depth (1- depth))
          (when (= depth 0)
            (setq done t)))))
      (setq i (1+ i)))
    (unless done
      (error "unterminated source container"))
    i))

(defun nelisp-artifact--source-atom-end (source pos)
  "Return one past the atom starting at POS in SOURCE."
  (let ((len (length source))
        (i pos)
        (done nil))
    (while (and (< i len) (not done))
      (let ((ch (aref source i)))
        (if (or (= ch ?\s) (= ch ?\t) (= ch ?\r) (= ch ?\n)
                (= ch ?\;) (= ch ?\() (= ch ?\)) (= ch ?\[) (= ch ?\]))
            (setq done t)
          (setq i (1+ i)))))
    i))

(defun nelisp-artifact--source-form-end (source pos)
  "Return one past the top-level form starting at POS in SOURCE."
  (let* ((len (length source))
         (pos (nelisp-artifact--source-skip-ws-comments source pos)))
    (when (>= pos len)
      (error "no source form at end of input"))
    (let ((ch (aref source pos)))
      (cond
       ((or (= ch ?\') (= ch ?`))
        (nelisp-artifact--source-form-end source (1+ pos)))
       ((= ch ?,)
        (nelisp-artifact--source-form-end
         source
         (if (and (< (1+ pos) len) (= (aref source (1+ pos)) ?@))
             (+ pos 2)
           (1+ pos))))
       ((and (= ch ?#)
             (< (1+ pos) len)
             (= (aref source (1+ pos)) ?\'))
        (nelisp-artifact--source-form-end source (+ pos 2)))
       ((and (= ch ?#)
             (< (1+ pos) len)
             (= (aref source (1+ pos)) ?\())
        (nelisp-artifact--source-container-end source (1+ pos)))
       ((or (= ch ?\() (= ch ?\[))
        (nelisp-artifact--source-container-end source pos))
       ((= ch ?\")
        (nelisp-artifact--source-string-end source pos))
       (t
        (nelisp-artifact--source-atom-end source pos))))))

(defun nelisp-artifact--source-form-slices (source)
  "Return source substrings for each top-level form in SOURCE."
  (let ((pos 0)
        (len (length source))
        (slices nil))
    (while (progn
             (setq pos (nelisp-artifact--source-skip-ws-comments source pos))
             (< pos len))
      (let ((end (nelisp-artifact--source-form-end source pos)))
        (push (substring source pos end) slices)
        (setq pos end)))
    (nreverse slices)))

(defun nelisp-artifact--rewrite-defalias-late-binding (binding)
  "Rewrite defalias-late forms inside a LET-style BINDING."
  (cond
   ((atom binding) binding)
   ((null (cdr binding)) binding)
   (t
    (cons (car binding)
          (cons (nelisp-artifact--rewrite-defalias-late-form (cadr binding))
                (cddr binding))))))

(defun nelisp-artifact--rewrite-defalias-late-clause (clause)
  "Rewrite defalias-late forms inside a CONDITION-CASE CLAUSE."
  (if (consp clause)
      (cons (car clause)
            (mapcar #'nelisp-artifact--rewrite-defalias-late-form
                    (cdr clause)))
    clause))

(defun nelisp-artifact--rewrite-defalias-late-form (form)
  "Rewrite FORM so `defalias' becomes `nelisp--defalias-late'.
Only wrapper heads are traversed recursively.  Quoted and FUNCTION
subtrees remain untouched."
  (cond
   ((atom form) form)
   ((memq (car form) '(quote function)) form)
   ((eq (car form) 'defalias)
    (cons 'nelisp--defalias-late (cdr form)))
   ((memq (car form)
          '(progn prog1 prog2 when unless while eval-when-compile
                  eval-and-compile with-no-warnings with-suppressed-warnings))
    (cons (car form)
          (mapcar #'nelisp-artifact--rewrite-defalias-late-form
                  (cdr form))))
   ((eq (car form) 'if)
    (cons 'if
          (mapcar #'nelisp-artifact--rewrite-defalias-late-form
                  (cdr form))))
   ((eq (car form) 'condition-case)
    (append (list (car form)
                  (cadr form)
                  (nelisp-artifact--rewrite-defalias-late-form (nth 2 form)))
            (mapcar #'nelisp-artifact--rewrite-defalias-late-clause
                    (cdddr form))))
   ((memq (car form) '(let let*))
    (cons (car form)
          (cons (mapcar #'nelisp-artifact--rewrite-defalias-late-binding
                        (cadr form))
                (mapcar #'nelisp-artifact--rewrite-defalias-late-form
                        (cddr form)))))
   (t form)))

(defun nelisp-artifact--rewrite-defalias-late-forms (forms)
  "Rewrite every FORM in FORMS for late defalias staging."
  (mapcar #'nelisp-artifact--rewrite-defalias-late-form forms))

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

(defun nelisp-artifact--read-one-private-form (source label)
  "Read exactly one private artifact form from SOURCE using the NeLisp reader.
LABEL is used in error messages.  This avoids the host buffer/read
compatibility path in standalone NeLisp for `.nelc' payloads and sibling
manifests, both of which are generated by this module and use the ordinary
NeLisp-readable printed syntax."
  (let* ((pos (nelisp-read--skip-ws source 0))
         (len (length source))
         (res (and (< pos len)
                   (nelisp-read--sexp source pos))))
    (unless res
      (error "empty private artifact form: %s" label))
    (setq pos (nelisp-read--skip-ws source (cdr res)))
    (unless (>= pos len)
      (error "trailing data after private artifact form: %s" label))
    (car res)))

(defconst nelisp-artifact--missing-key :nelisp-artifact-missing-key)

(defun nelisp-artifact--read-private-keyword-value
    (source keyword label &optional missing-ok start)
  "Read KEYWORD's generated plist value from SOURCE.
KEYWORD is a keyword symbol such as `:module-init'.  LABEL is used in error
messages.  The search is intentionally simple because this is only for private
artifacts and manifests emitted by this module.  When MISSING-OK is non-nil,
return `nelisp-artifact--missing-key' instead of signaling."
  (let* ((needle (concat (symbol-name keyword) " "))
         (pos (nelisp-artifact--string-search-literal needle source start)))
    (if (null pos)
        (if missing-ok
            nelisp-artifact--missing-key
          (error "missing private artifact key %S in %s" keyword label))
      (let* ((value-pos (nelisp-read--skip-ws
                         source (+ pos (length needle))))
             (res (nelisp-read--sexp source value-pos)))
        (unless res
          (error "invalid private artifact value for %S in %s" keyword label))
        (car res)))))

(defun nelisp-artifact--private-top-level-key-position
    (source keyword label &optional start)
  "Return top-level KEYWORD position in generated private SOURCE.
This delegates to the shared O(N), single-pass private-list state machine,
which tracks strings, comments, escapes, and parenthesis depth with `aref'."
  (let* ((list-start
          (nelisp-read--skip-ws source (or start 0)))
         (positions
          (nelisp-artifact--private-list-key-positions
           source list-start (length source) (list keyword) label)))
    (cdr (assq keyword positions))))

(defun nelisp-artifact--read-private-top-level-keyword-value
    (source keyword label &optional missing-ok start)
  "Read top-level plist KEYWORD from generated private SOURCE.
Unlike the generic fast token reader, this finds KEYWORD only at depth one of
the outer plist, ignoring occurrences inside strings, comments, and nested
module values.  START points before the outer plist, normally just after the
private artifact magic header."
  (let ((key-pos
         (nelisp-artifact--private-top-level-key-position
          source keyword label start)))
    (if (null key-pos)
        (if missing-ok
            nelisp-artifact--missing-key
          (error "missing private artifact key %S in %s" keyword label))
      (let* ((value-pos
              (nelisp-read--skip-ws
               source (+ key-pos (length (symbol-name keyword)))))
             (res (nelisp-read--sexp source value-pos)))
        (unless res
          (error "invalid private artifact value for %S in %s"
                 keyword label))
        (car res)))))

(defun nelisp-artifact--private-keyword-value-pos
    (source keyword label &optional missing-ok start)
  "Return generated plist value start position for KEYWORD in SOURCE."
  (let* ((needle (concat (symbol-name keyword) " "))
         ;; A shared top-level scanner can pass the exact key position.  Do
         ;; not search again in that case: besides being redundant, a second
         ;; generic literal search can select a nested decoy if its starting
         ;; position is ever widened by a caller.
         (pos (if (and start
                       (nelisp-artifact--string-prefix-at-p
                        needle source start))
                  start
                (nelisp-artifact--string-search-literal
                 needle source start))))
    (if (null pos)
        (if missing-ok
            nil
          (error "missing private artifact key %S in %s" keyword label))
      (nelisp-read--skip-ws source (+ pos (length needle))))))

(defun nelisp-artifact--read-private-symbol-token
    (source keyword label &optional missing-ok start)
  "Read KEYWORD's generated symbol value without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok start)))
    (if (null pos)
        nelisp-artifact--missing-key
      (let ((end pos)
            (len (length source)))
        (while (and (< end len)
                    (let ((ch (aref source end)))
                      (not (or (= ch ?\s) (= ch ?\t) (= ch ?\n)
                               (= ch ?\r) (= ch ?\))))))
          (setq end (1+ end)))
        (intern (substring source pos end))))))

(defun nelisp-artifact--read-private-integer-token
    (source keyword label &optional missing-ok start)
  "Read KEYWORD's generated integer value without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok start)))
    (if (null pos)
        nelisp-artifact--missing-key
      (let ((end pos)
            (len (length source)))
        (while (and (< end len)
                    (let ((ch (aref source end)))
                      (or (and (>= ch ?0) (<= ch ?9))
                          (= ch ?-))))
          (setq end (1+ end)))
        (string-to-number (substring source pos end))))))

(defun nelisp-artifact--read-private-string-token
    (source keyword label &optional missing-ok start)
  "Read KEYWORD's generated string value without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok start)))
    (if (null pos)
        nelisp-artifact--missing-key
      (unless (= (aref source pos) ?\")
        (error "expected string value for %S in %s" keyword label))
      (let ((i (1+ pos))
            (len (length source))
            (out "")
            (escaped nil)
            (done nil))
        (while (and (< i len) (not done))
          (let ((ch (aref source i)))
            (cond
             (escaped
              (setq out (concat out (string ch))
                    escaped nil))
             ((= ch ?\\)
              (setq escaped t))
             ((= ch ?\")
              (setq done t))
             (t
              (setq out (concat out (string ch)))))
            (setq i (1+ i))))
        (unless done
          (error "unterminated string value for %S in %s" keyword label))
        out))))

(defun nelisp-artifact--read-private-symbol-list-token
    (source keyword label &optional missing-ok start)
  "Read KEYWORD's generated symbol list without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok start)))
    (if (null pos)
        nelisp-artifact--missing-key
      (let ((len (length source))
            (items nil)
            token-start)
        (cond
         ((and (<= (+ pos 3) len)
               (= (aref source pos) ?n)
               (= (aref source (1+ pos)) ?i)
               (= (aref source (+ pos 2)) ?l))
          nil)
         ((= (aref source pos) ?\()
          (setq pos (1+ pos))
          (while (progn
                   (setq pos (nelisp-read--skip-ws source pos))
                   (and (< pos len) (not (= (aref source pos) ?\)))))
            (setq token-start pos)
            (while (and (< pos len)
                        (let ((ch (aref source pos)))
                          (not (or (= ch ?\s) (= ch ?\t) (= ch ?\n)
                                   (= ch ?\r) (= ch ?\))))))
              (setq pos (1+ pos)))
            (when (= token-start pos)
              (error "invalid symbol list value for %S in %s" keyword label))
            (let* ((name (substring source token-start pos))
                   (existing
                    (and (fboundp 'intern-soft)
                         (intern-soft name))))
              (setq items
                    (cons (or existing (intern name)) items))))
          (unless (and (< pos len) (= (aref source pos) ?\)))
            (error "unterminated symbol list value for %S in %s"
                   keyword label))
          (nreverse items))
         (t
          (error "expected symbol list value for %S in %s" keyword label)))))))

(defun nelisp-artifact--plist-put-present (plist key value)
  "Return PLIST with KEY VALUE appended unless VALUE is the missing sentinel."
  (if (eq value nelisp-artifact--missing-key)
      plist
    (append plist (list key value))))

(defun nelisp-artifact--macroexpand-1-form (form)
  "Expand FORM once with the active macroexpander, or signal if unavailable."
  (let ((expander (cond
                   ((and (nelisp-artifact--standalone-runtime-p)
                         (fboundp 'nelisp-macroexpand-1))
                    #'nelisp-macroexpand-1)
                   ((fboundp 'macroexpand-1)
                    #'macroexpand-1)
                   ((fboundp 'nelisp-macroexpand-1)
                    #'nelisp-macroexpand-1)
                   (t nil))))
    (if expander
        (funcall expander form)
      (error "no available macroexpander for %S" form))))

(defun nelisp-artifact--macroexpander ()
  "Return the active macroexpander function, or nil if unavailable."
  (cond
   ((and (nelisp-artifact--standalone-runtime-p)
         (fboundp 'nelisp-macroexpand-all))
    #'nelisp-macroexpand-all)
   ((fboundp 'macroexpand-all)
    #'macroexpand-all)
   ((fboundp 'nelisp-macroexpand-all)
    #'nelisp-macroexpand-all)
   (t nil)))

(defun nelisp-artifact--macroexpand-all-form (form)
  "Fully macroexpand FORM with the active available expander."
  (let ((expander (nelisp-artifact--macroexpander)))
    (if expander
        (funcall expander form)
      (error "no available macroexpander for %S" form))))

(defun nelisp-artifact--compile-time-context-form-p (form)
  "Return non-nil when FORM should be evaluated to seed later expansion."
  (and (consp form)
       (memq (car form) '(defmacro require nelisp-require))))

(defun nelisp-artifact--apply-compile-time-context-form (form)
  "Evaluate FORM only for legitimate compile-time macro context."
  (when (nelisp-artifact--compile-time-context-form-p form)
    (condition-case nil
        (if (nelisp-artifact--standalone-runtime-p)
            (nelisp-eval form)
          (eval form))
      (error nil))
    t))

(defun nelisp-artifact--literal-symbol-form (form)
  "Return the literal symbol named by FORM, or nil."
  (cond
   ((symbolp form) form)
   ((and (consp form)
         (eq (car form) 'quote)
         (symbolp (nth 1 form)))
    (nth 1 form))
   (t nil)))

(defun nelisp-artifact--static-compile-time-condition (form)
  "Return `(known . BOOL)' when FORM can be decided without runtime side effects."
  (cond
   ((eq form t) (cons 'known t))
   ((null form) (cons 'known nil))
   ((and (consp form) (eq (car form) 'not) (= (length form) 2))
    (let ((value (nelisp-artifact--static-compile-time-condition (nth 1 form))))
      (when value
        (cons 'known (not (cdr value))))))
   ((and (consp form) (eq (car form) 'and))
    (let ((args (cdr form))
          (known t)
          (result t))
      (while (and args known result)
        (let ((value (nelisp-artifact--static-compile-time-condition (car args))))
          (if value
              (setq result (cdr value))
            (setq known nil)))
        (setq args (cdr args)))
      (when known
        (cons 'known result))))
   ((and (consp form) (eq (car form) 'or))
    (let ((args (cdr form))
          (known t)
          (result nil))
      (while (and args known (not result))
        (let ((value (nelisp-artifact--static-compile-time-condition (car args))))
          (if value
              (setq result (cdr value))
            (setq known nil)))
        (setq args (cdr args)))
      (when known
        (cons 'known result))))
   ((and (consp form)
         (= (length form) 2)
         (memq (car form) '(fboundp boundp featurep)))
    (let ((name (nelisp-artifact--literal-symbol-form (nth 1 form))))
      (when name
        (cons 'known
              (cond
               ((eq (car form) 'fboundp) (fboundp name))
               ((eq (car form) 'boundp) (boundp name))
               (t (featurep name)))))))))

(defun nelisp-artifact--apply-compile-time-context-forms (forms)
  "Apply compile-time context forms in FORMS from left to right."
  (dolist (form forms)
    (nelisp-artifact--apply-compile-time-context-tree form)))

(defun nelisp-artifact--apply-compile-time-context-tree (form)
  "Apply source-order compile-time context seeding reachable from FORM."
  (cond
   ((nelisp-artifact--compile-time-context-form-p form)
    (nelisp-artifact--apply-compile-time-context-form form))
   ((not (consp form)) nil)
   ((memq (car form) '(progn seq))
    (nelisp-artifact--apply-compile-time-context-forms (cdr form))
    t)
   ((eq (car form) 'when)
    (let ((value (and (>= (length form) 2)
                      (nelisp-artifact--static-compile-time-condition
                       (nth 1 form)))))
      (when (and value (cdr value))
        (nelisp-artifact--apply-compile-time-context-forms (nthcdr 2 form))))
    t)
   ((eq (car form) 'unless)
    (let ((value (and (>= (length form) 2)
                      (nelisp-artifact--static-compile-time-condition
                       (nth 1 form)))))
      (when (and value (not (cdr value)))
        (nelisp-artifact--apply-compile-time-context-forms (nthcdr 2 form))))
    t)
   ((eq (car form) 'if)
    (let ((value (and (>= (length form) 2)
                      (nelisp-artifact--static-compile-time-condition
                       (nth 1 form)))))
      (when value
        (if (cdr value)
            (nelisp-artifact--apply-compile-time-context-tree (nth 2 form))
          (nelisp-artifact--apply-compile-time-context-forms
           (nthcdr 3 form)))))
    t)
   (t nil)))

(defconst nelisp-artifact--top-level-function-definition-macros
  '(defsubst define-inline)
  "Top-level macro names that may normalize to a single `defun'.")

(defun nelisp-artifact--well-formed-defun-form-p (form)
  "Return non-nil when FORM is a well-formed `defun'."
  (and (consp form)
       (eq (car form) 'defun)
       (>= (safe-length form) 3)
       (symbolp (nth 1 form))
       (listp (nth 2 form))))

(defun nelisp-artifact--well-formed-defsubst-form-p (form)
  "Return non-nil when FORM is a well-formed `defsubst'."
  (and (consp form)
       (eq (car form) 'defsubst)
       (>= (safe-length form) 3)
       (symbolp (nth 1 form))
       (listp (nth 2 form))))

(defun nelisp-artifact--normalize-top-level-defun-form (form)
  "Return FORM when it is a literal or supported macro-expanded `defun'.
Only top-level `defun' and supported function-definition macros are accepted.
Well-formed top-level `defsubst' forms are normalized directly to `defun'
forms.  Supported `define-inline' forms are macroexpanded once in the current
compile-time context, and only a single well-formed `defun' expansion is
considered safe."
  (cond
   ((nelisp-artifact--well-formed-defun-form-p form) form)
   ((nelisp-artifact--well-formed-defsubst-form-p form)
    (cons 'defun (cdr form)))
   ((and (consp form)
         (eq (car form) 'define-inline))
    (condition-case nil
        (let ((expanded (nelisp-artifact--macroexpand-1-form form)))
          (and (nelisp-artifact--well-formed-defun-form-p expanded)
               expanded))
      (error nil)))
   (t nil)))

(defun nelisp-artifact--try-compile-defun (form)
  "Return (:fn NAME BCL SOURCE-DEFUN) when FORM normalizes to a `defun' the
bytecode VM accepts.  Supported top-level function-definition macros are
expanded once in the current compile-time context and then validated as a
single well-formed `defun'.  Compiling the body `(lambda ARGS . BODY)' through
`nelisp-bc-compile' +
`nelisp-bc-run' yields a `nelisp-bcl' closure value; free/global symbol
references stay late-bound (resolved against `nelisp--functions' /
NeLisp variables at call time), so recursion and forward references
work once the module finishes loading.  Returns nil when the body uses
a form the VM cannot yet lower, so the caller can fall back to replay."
  (let ((defun (nelisp-artifact--normalize-top-level-defun-form form)))
    (when defun
      (let ((name (nth 1 defun))
            (arglist (nth 2 defun))
            (body (nthcdr 3 defun)))
        (condition-case nil
            (let* ((lambda-form (cons 'lambda (cons arglist body)))
                   (expanded (nelisp-artifact--macroexpand-all-form lambda-form))
                   (bcl (nelisp-bc-run
                         (nelisp-bc-compile expanded))))
              (and (consp bcl) (eq (car bcl) 'nelisp-bcl)
                   (list :fn name bcl defun)))
          (error nil))))))

(defun nelisp-artifact--compile-top-level-form (form &optional module-policy)
  "Lower FORM into a `.nelc' module instruction (Doc 142 §6.1).
An eligible top-level `defun' or supported function-definition macro becomes
a precompiled (:fn NAME BCL SOURCE-DEFUN) install; every other form (and any
defun the bytecode VM cannot lower) becomes (:eval FORM) replayed through
`nelisp-eval' at load."
  (if (eq (nelisp-artifact--normalize-module-policy module-policy) 'eval-only)
      (list :eval form)
    (or (nelisp-artifact--try-compile-defun form)
        (list :eval form))))

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
    :artifact-schema-version 4
    :artifact-layout-version 2
    :native-section-version 5))

(defun nelisp-artifact--read-binary (path)
  "Read PATH as a raw unibyte byte string (no decoding)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun nelisp-artifact--target-arch (target)
  "Map a TARGET triple string to a AOT arch symbol, or nil if unknown."
  (cond
   ((null target) 'x86_64)
   ((string-match-p "x86_64\\|amd64" target) 'x86_64)
   ((string-match-p "aarch64\\|arm64" target) 'arm64)
   (t nil)))

(defun nelisp-artifact--runtime-image-wasm-target-p (target)
  "Return non-nil when TARGET selects the wasm runtime-image lane."
  (and (stringp target)
       (string-match-p "wasm32" target)))

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

(defun nelisp-artifact--native-object-bytes (unit)
  "Return UNIT's ET_REL bytes without round-tripping through disk."
  (nelisp-elf--build-rel
   (list :e-type 'rel
         :text (plist-get unit :text)
         :rodata (plist-get unit :rodata)
         :symbols (plist-get unit :symbols)
         :relocs (plist-get unit :relocs)
         :machine (plist-get unit :machine))))

(defun nelisp-artifact--byte-length (value)
  "Return VALUE's byte length."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (if (fboundp 'string-bytes)
      (string-bytes value)
    (length value)))

(defun nelisp-artifact--native-defun-entry (entry)
  "Normalize one native defun ENTRY plist for artifact storage."
  (list :name (plist-get entry :name)
        :offset (plist-get entry :offset)
        :size (plist-get entry :size)
        :arity (plist-get entry :arity)
        :rest-p (plist-get entry :rest-p)
        :fixed-count (plist-get entry :fixed-count)
        :param-class (plist-get entry :param-class)
        :rt-slot-count (plist-get entry :rt-slot-count)
        :body-offset (plist-get entry :body-offset)))

(defun nelisp-artifact--native-section-get (section key)
  "Return KEY from native SECTION.
Version 5 load metadata stays in its compact runtime-prefix vector so the
standalone loader does not allocate and then immediately collect a duplicate
20-cell plist for every large native section.  Compiler-side and legacy
version 2--4 sections remain ordinary plists."
  (if (and (vectorp section)
           (= (length section) 10)
           (= (aref section 0)
              nelisp-artifact--native-runtime-prefix-layout-version))
      (cond
       ((eq key :native-section-version)
        nelisp-artifact--native-section-version)
       ((eq key :runtime-prefix-char-size) (aref section 1))
       ((eq key :arch) (aref section 2))
       ((eq key :symbols) (aref section 3))
       ((eq key :text-base64) (aref section 4))
       ((eq key :reloc-format) (aref section 5))
       ((eq key :reloc-count) (aref section 6))
       ((eq key :reloc-data) (aref section 7))
       ((eq key :extern-symbols) (aref section 8))
       ((eq key :defuns) (aref section 9))
       ((eq key :runtime-end) t)
       (t nil))
    (plist-get section key)))

(defun nelisp-artifact--native-defun-metadata (native symbol)
  "Return NATIVE defun metadata for SYMBOL."
  (let ((name (if (symbolp symbol) (symbol-name symbol) symbol))
        (found nil)
        (section (nelisp-artifact--native-section-for-symbol native symbol))
        (defs nil))
    (setq defs
          (and section
               (nelisp-artifact--native-section-get section :defuns)))
    (while (and defs (not found))
      (let ((entry (car defs)))
        (when (equal (plist-get entry :name) name)
          (setq found entry)))
      (setq defs (cdr defs)))
    found))

(defun nelisp-artifact--native-function-wrapper (artifact-path symbol fallback meta)
  "Return a native-aware callable wrapper."
  (list 'nelisp-native-function
        (expand-file-name artifact-path)
        symbol
        fallback
        meta))

(defun nelisp-artifact--native-function-symbol (fn)
  "Return native wrapper FN's symbol."
  (nth 2 fn))

(defun nelisp-artifact--native-function-artifact (fn)
  "Return native wrapper FN's artifact path."
  (nth 1 fn))

(defun nelisp-artifact--native-function-fallback (fn)
  "Return native wrapper FN's fallback callable."
  (nth 3 fn))

(defun nelisp-artifact--native-function-meta (fn)
  "Return native wrapper FN's metadata plist."
  (nth 4 fn))

(defun nelisp-artifact--native-wrapper-p (fn)
  "Return non-nil when FN is a native wrapper."
  (and (consp fn) (eq (car fn) 'nelisp-native-function)))

(defun nelisp-artifact--serialized-native-section-p (native)
  "Return non-nil when NATIVE is one serialized native section plist."
  (and (listp native)
       (plist-member native :object-base64)
       (plist-member native :defuns)
       (plist-member native :symbols)))

(defun nelisp-artifact--native-section-p (native)
  "Return non-nil when NATIVE is one installable native section plist.
The manifest-side metadata used for wrapper installation strips embedded
object bytes, so this predicate only requires the section metadata needed
to rebuild wrappers and dispatch reports."
  (or
   (and (vectorp native)
        (= (length native) 10)
        (= (aref native 0)
           nelisp-artifact--native-runtime-prefix-layout-version))
   (and (listp native)
        (plist-member native :defuns)
        (plist-member native :symbols))))

(defun nelisp-artifact--native-sections-from-native (native)
  "Return a list of native section plists for NATIVE metadata.
A list of version-5 compact runtime-prefix vectors is the shape the
standalone artifact runtime cache produces; it must be recognized here or
the install loop silently iterates nothing and every artifact command runs
interpreted."
  (cond
   ((null native) nil)
   ((nelisp-artifact--native-section-p native)
    (list native))
   ((and (consp native)
         (nelisp-artifact--native-section-p (car native)))
    native)
   ((and (consp native)
         (consp (car native))
         (keywordp (caar native)))
    native)
   ((plist-member native :native-sections)
    (plist-get native :native-sections))
   ((plist-member native :native)
    (nelisp-artifact--native-sections-from-native
     (plist-get native :native)))
   (t nil)))

(defun nelisp-artifact--native-section-for-symbol (native symbol)
  "Return the last native section from NATIVE that exports SYMBOL."
  (let ((name (if (symbolp symbol) (symbol-name symbol) symbol))
        (sections (nelisp-artifact--native-sections-from-native native))
        (found nil))
    (while sections
      (let ((section (car sections)))
        (when (member
               name
               (nelisp-artifact--native-section-get section :symbols))
          (setq found section)))
      (setq sections (cdr sections)))
    found))

(defun nelisp-artifact--native-delete-alist-key (key alist)
  "Return ALIST without entries whose key is equal to KEY."
  (let ((rest alist)
        (out nil))
    (while rest
      (unless (equal (caar rest) key)
        (setq out (cons (car rest) out)))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun nelisp-artifact--native-invalidate-artifact-runtime (artifact-path)
  "Unmap and remove only ARTIFACT-PATH's committed native runtime state."
  (let* ((artifact (expand-file-name artifact-path))
         (rest nelisp-artifact--native-runtime-mappings)
         (kept nil))
    (while rest
      (let* ((entry (car rest))
             (key (car entry))
             (mapping (cdr entry)))
        (if (equal (car key) artifact)
            (when (and (fboundp 'syscall-direct)
                       (integerp (plist-get mapping :base))
                       (integerp (plist-get mapping :size)))
              (syscall-direct 11
                              (plist-get mapping :base)
                              (plist-get mapping :size)
                              0 0 0 0))
          (setq kept (cons entry kept))))
      (setq rest (cdr rest)))
    (setq nelisp-artifact--native-runtime-mappings (nreverse kept))
    (setq nelisp-artifact--native-artifact-linksets
          (nelisp-artifact--native-delete-alist-key
           artifact nelisp-artifact--native-artifact-linksets))
    (setq nelisp-artifact--native-artifact-symbol-index
          (nelisp-artifact--native-delete-alist-key
           artifact nelisp-artifact--native-artifact-symbol-index))
    artifact))

(defun nelisp-artifact--register-native-sections (artifact-path sections)
  "Register serialized SECTIONS for ARTIFACT-PATH.
An equal generation preserves its ready linkset.  Different section content
first invalidates only this artifact's owned mappings and committed indexes."
  (let* ((artifact (expand-file-name artifact-path))
         (entry (assoc artifact nelisp-artifact--native-section-registry)))
    (cond
     ((and entry (equal (cdr entry) sections)))
     (entry
      (nelisp-artifact--native-invalidate-artifact-runtime artifact)
      (setcdr entry sections))
     (t
      (setq nelisp-artifact--native-section-registry
            (cons (cons artifact sections)
                  nelisp-artifact--native-section-registry))))
    sections))

(defun nelisp-artifact--registered-native-sections (artifact-path)
  "Return serialized native sections registered for ARTIFACT-PATH."
  (cdr (assoc (expand-file-name artifact-path)
              nelisp-artifact--native-section-registry)))

(defun nelisp-artifact-clear-native-runtime-mappings ()
  "Unmap and forget every process-local native linkset and symbol index."
  (when (fboundp 'syscall-direct)
    (dolist (entry nelisp-artifact--native-runtime-mappings)
      (let ((mapping (cdr entry)))
        (when (and (integerp (plist-get mapping :base))
                   (integerp (plist-get mapping :size)))
          (syscall-direct 11
                          (plist-get mapping :base)
                          (plist-get mapping :size)
                          0 0 0 0)))))
  (setq nelisp-artifact--native-runtime-mappings nil)
  (setq nelisp-artifact--native-artifact-linksets nil)
  (setq nelisp-artifact--native-artifact-symbol-index nil))

(defun nelisp-artifact--read-serialized-native-sections
    (content artifact-path)
  "Read serialized native sections from private artifact CONTENT."
  (let* ((prefix-len (length nelisp-artifact--magic))
         (body (substring content prefix-len))
         (sections
          (nelisp-artifact--read-private-top-level-keyword-value
           body :native-sections artifact-path t 0)))
    (if (eq sections nelisp-artifact--missing-key)
        (let ((native
               (nelisp-artifact--read-private-top-level-keyword-value
                body :native artifact-path t 0)))
          (if (eq native nelisp-artifact--missing-key)
              nil
            (list (nelisp-artifact--native-section-flatten-v5 native))))
      (mapcar #'nelisp-artifact--native-section-flatten-v5 sections))))

(defconst nelisp-artifact--native-load-section-fields
  '(:native-section-version :arch :symbols :text-base64
    :relocs :reloc-format :reloc-count :reloc-data
    :extern-symbols :defuns)
  "Serialized native section fields recognized by in-process load/link.
Version 2 sections require `:relocs'.  Legacy version 3 sections require the
three compact relocation fields instead.  Version 4 uses an ordered,
self-sized runtime prefix.  Version 5 uses an independently closed runtime
plist and authoritative header offsets; both bypass this scanner.")

(defun nelisp-artifact--read-decimal-at (source pos)
  "Return (NUMBER . END) for decimal digits in SOURCE at POS.
This definition precedes native cache loading because the version 4/5 section
readers need it while the artifact command runtime cache is installing itself."
  (let ((i pos)
        (len (length source))
        (value 0)
        (have nil))
    (while (and (< i len)
                (let ((ch (aref source i)))
                  (and (>= ch ?0) (<= ch ?9))))
      (setq have t)
      (setq value (+ (* value 10) (- (aref source i) ?0)))
      (setq i (1+ i)))
    (unless have
      (error "expected decimal integer at %s" pos))
    (cons value i)))

(defun nelisp-artifact--private-string-end (source start limit label)
  "Return position after the generated string beginning at START.
Native literal search skips large base64 payloads in one operation.  Escaped
quotes are ignored; LIMIT bounds the containing generated form."
  (unless (and (< start limit) (= (aref source start) ?\"))
    (error "invalid private string in %s" label))
  (let ((search (1+ start))
        (end nil))
    (while (null end)
      (let ((quote
             (if (fboundp 'nelisp--string-search)
                 (nelisp-artifact--string-search-literal
                  "\"" source search)
               (let ((i search)
                     (found nil))
                 (while (and (< i limit) (null found))
                   (if (= (aref source i) ?\")
                       (setq found i)
                     (setq i (1+ i))))
                 found))))
        (unless (and quote (< quote limit))
          (error "unterminated private string in %s" label))
        (let ((i (1- quote))
              (slashes 0))
          (while (and (>= i start) (= (aref source i) ?\\))
            (setq slashes (1+ slashes))
            (setq i (1- i)))
          (if (= (% slashes 2) 0)
              (setq end (1+ quote))
            (setq search (1+ quote))))))
    end))

(defun nelisp-artifact--scan-private-native-load-section
    (source start limit label)
  "Return (POSITIONS . END) for one native section at START.
The section is scanned once up to its depth-zero close.  POSITIONS contains
only top-level load fields, so strings and nested diagnostic metadata cannot
masquerade as section keys."
  (unless (and (< start limit) (= (aref source start) ?\())
    (error "invalid native section in %s" label))
  (let ((i start)
        (depth 0)
         (remaining (copy-sequence
                     nelisp-artifact--native-load-section-fields))
        (positions nil)
        (end nil))
    (while (and (< i limit) (null end))
      (let ((ch (aref source i)))
        (cond
         ((= ch ?\")
          (setq i
                (1-
                 (nelisp-artifact--private-string-end
                  source i limit label))))
         ((= ch ?\;)
          (while (and (< i limit) (not (= (aref source i) ?\n)))
            (setq i (1+ i))))
         ((= ch ?\() (setq depth (1+ depth)))
         ((= ch ?\))
          (setq depth (1- depth))
          (when (= depth 0)
            (setq end (1+ i))))
         ((and remaining (= depth 1) (= ch ?:))
          (let ((token-end i))
            (while (and (< token-end limit)
                        (let ((c (aref source token-end)))
                          (not (or (= c ?\s) (= c ?\t)
                                   (= c ?\n) (= c ?\r) (= c ?\))))))
              (setq token-end (1+ token-end)))
            (let ((key (intern (substring source i token-end))))
              (when (memq key remaining)
                (setq positions (cons (cons key i) positions))
                (setq remaining (delq key remaining))))
            (setq i (1- token-end))))))
      (setq i (1+ i)))
    (unless (and end (>= depth 0))
      (error "unterminated native section in %s" label))
    (cons (nreverse positions) end)))

(defun nelisp-artifact--read-compact-reloc-data
    (source start end count label)
  "Read COUNT compact relocation triples directly from SOURCE.
START and END bound the generated parenthesized numeric field.  The result is
a flat vector with stride three.  This deliberately bypasses every generic
sexp reader so a large relocation table never becomes a plist/list AST."
  (unless (and (integerp count) (>= count 0))
    (error "invalid compact relocation count in %s: %S" label count))
  (unless (and (< start end) (= (aref source start) ?\())
    (error "invalid compact relocation data in %s" label))
  (let ((span (- end start)))
    ;; Even `(0 0 0)' needs more than three source bytes per entry.  This
    ;; cheap bound prevents a corrupt count from requesting a huge vector.
    (when (> count span)
      (error "compact relocation count exceeds source span in %s" label)))
  (let* ((slots (* count 3))
         (out (make-vector slots 0))
         (slot 0)
         (pos (1+ start)))
    (while (< slot slots)
      (setq pos (nelisp-read--skip-ws source pos))
      (when (or (>= pos end) (= (aref source pos) ?\)))
        (error "compact relocation data ended early in %s" label))
      (let ((negative nil)
            (value 0)
            (digits 0))
        (when (= (aref source pos) ?-)
          (setq negative t)
          (setq pos (1+ pos)))
        (while (and (< pos end)
                    (let ((ch (aref source pos)))
                      (and (>= ch ?0) (<= ch ?9))))
          (setq value (+ (* value 10) (- (aref source pos) ?0)))
          (setq digits (1+ digits))
          (setq pos (1+ pos)))
        (when (= digits 0)
          (error "invalid compact relocation integer in %s" label))
        (when (and (< pos end)
                   (let ((ch (aref source pos)))
                     (not (or (= ch ?\s) (= ch ?\t)
                              (= ch ?\n) (= ch ?\r) (= ch ?\))))))
          (error "invalid compact relocation delimiter in %s" label))
        (aset out slot (if negative (- value) value)))
      (setq slot (1+ slot)))
    (setq pos (nelisp-read--skip-ws source pos))
    (unless (and (< pos end) (= (aref source pos) ?\))
                 (= (1+ pos) end))
      (error "trailing compact relocation data in %s" label))
    out))

(defun nelisp-artifact--read-private-native-load-section-legacy
    (source start limit label)
  "Read legacy native load metadata from one section at START.
LIMIT bounds the containing native value.  Only
`nelisp-artifact--native-load-section-fields' enter the private reader;
diagnostic object payloads and compile reports are scanned but not
materialized.  Return (SECTION . END)."
  (let* ((scan
          (nelisp-artifact--scan-private-native-load-section
           source start limit label))
         (positions (car scan))
         (end (cdr scan))
         (common-fields
          '(:native-section-version :arch :symbols :text-base64
            :extern-symbols :defuns))
         (section nil)
         (version nil))
    (while common-fields
      (let* ((key (car common-fields))
             (key-pos (cdr (assq key positions))))
        (unless key-pos
          (error "native section lacks load field %S in %s" key label))
        (let* ((value-pos
                (nelisp-read--skip-ws
                 source (+ key-pos (length (symbol-name key)))))
               (value-end
                (if (memq key '(:arch :text-base64))
                    (nelisp-artifact--private-string-end
                     source value-pos end label)
                  (nelisp-artifact--private-field-value-end
                   source value-pos end label))))
          (setq section
                (append
                 section
                 (list key
                       (if (memq key '(:arch :text-base64))
                           (progn
                             (unless (and (< value-pos value-end)
                                          (= (aref source value-pos) ?\")
                                          (= (aref source (1- value-end)) ?\"))
                               (error
                                "invalid native string field %S in %s"
                                key label))
                             (substring source (1+ value-pos)
                                        (1- value-end)))
                         (nelisp-artifact--read-private-item
                          source value-pos value-end)))))))
      (setq common-fields (cdr common-fields)))
    (setq version (plist-get section :native-section-version))
    (cond
     ((= version 2)
      (let ((key-pos (cdr (assq :relocs positions))))
        (unless key-pos
          (error "native section lacks load field :relocs in %s" label))
        (let* ((value-pos
                (nelisp-read--skip-ws
                 source (+ key-pos (length ":relocs"))))
               (value-end
                (nelisp-artifact--private-field-value-end
                 source value-pos end label)))
          (setq section
                (append section
                        (list :relocs
                              (nelisp-artifact--read-private-item
                               source value-pos value-end)))))))
     ((= version nelisp-artifact--legacy-compact-native-section-version)
      (when (assq :relocs positions)
        (error "compact native section must omit :relocs in %s" label))
      (let* ((format-pos (cdr (assq :reloc-format positions)))
             (count-pos (cdr (assq :reloc-count positions)))
             (data-pos (cdr (assq :reloc-data positions))))
        (unless (and format-pos count-pos data-pos)
          (error "compact native section lacks relocation fields in %s" label))
        (let* ((format-value-pos
                (nelisp-read--skip-ws
                 source (+ format-pos (length ":reloc-format"))))
               (format-end
                (nelisp-artifact--private-field-value-end
                 source format-value-pos end label))
               (format
                (nelisp-artifact--read-private-item
                 source format-value-pos format-end))
               (count-value-pos
                (nelisp-read--skip-ws
                 source (+ count-pos (length ":reloc-count"))))
               (count-end
                (nelisp-artifact--private-field-value-end
                 source count-value-pos end label))
               (count
                (nelisp-artifact--read-private-item
                 source count-value-pos count-end))
               (data-value-pos
                (nelisp-read--skip-ws
                 source (+ data-pos (length ":reloc-data"))))
               (data-end
                (nelisp-artifact--private-field-value-end
                 source data-value-pos end label)))
          (unless (eq format nelisp-artifact--compact-reloc-format)
            (error "unsupported compact relocation format in %s: %S"
                   label format))
          (setq section
                (append
                 section
                 (list :reloc-format format
                       :reloc-count count
                       :reloc-data
                       (nelisp-artifact--read-compact-reloc-data
                        source data-value-pos data-end count label)))))))
     (t
      (error "unsupported native section version in %s: %S" label version)))
    (cons section end)))

(defun nelisp-artifact--private-ordered-field-value-start
    (source pos limit keyword label)
  "Return value start for ordered KEYWORD at POS before LIMIT.
This is intentionally a fixed-prefix parser, not a keyword search."
  (setq pos (nelisp-read--skip-ws source pos))
  (let* ((name (symbol-name keyword))
         (token-end (+ pos (length name))))
    (unless (and (<= token-end limit)
                 (nelisp-artifact--string-prefix-at-p name source pos)
                 (< token-end limit)
                 (let ((ch (aref source token-end)))
                   (or (= ch ?\s) (= ch ?\t)
                       (= ch ?\n) (= ch ?\r))))
      (error "native section expected ordered field %S in %s"
             keyword label))
    (let ((value-pos (nelisp-read--skip-ws source token-end)))
      (unless (< value-pos limit)
        (error "native section lacks value for %S in %s" keyword label))
      value-pos)))

(defun nelisp-artifact--read-private-native-load-section-v4-native-prefix
    (source start limit label)
  "Read a generated v4 runtime prefix with the standalone native batch reader.
The exact ` :runtime-end t' token is unambiguous in generated v4 sections:
  opaque object and diagnostic fields follow it, while the preceding base64 and
native symbol metadata cannot contain that literal token.  Parse just that
prefix as one closed plist, then use `:serialized-char-size' to skip the tail."
  (let* ((search-start (nelisp-artifact--profile-time))
         (marker " :runtime-end t")
         (marker-pos
          (nelisp-artifact--string-search-literal marker source start))
         (_search-profile
          (nelisp-artifact--load-profile-log
           "native-v4-runtime-end-search" search-start
           (list :start start :marker marker-pos)))
         (prefix-end (and marker-pos (+ marker-pos (length marker))))
         (slice-start (nelisp-artifact--profile-time))
         (prefix
          (and prefix-end
               (<= prefix-end limit)
               (concat (substring source start prefix-end) ")")))
         (_slice-profile
          (nelisp-artifact--load-profile-log
           "native-v4-prefix-slice" slice-start
           (list :chars (if prefix (length prefix) 0))))
         (read-start (nelisp-artifact--profile-time))
         (batch
          (and prefix
               (nelisp--read-batch-from-string-native prefix 0 1)))
         (_read-profile
          (nelisp-artifact--load-profile-log
           "native-v4-native-read" read-start
           (list :chars (if prefix (length prefix) 0))))
         (forms (and batch (car batch)))
         (section (and (consp forms) (car forms)))
         (serialized-size
          (and (consp section)
               (plist-get section :serialized-char-size)))
         (section-end
          (and (integerp serialized-size)
               (+ start serialized-size))))
    (unless marker-pos
      (error "native section lacks generated runtime prefix end in %s" label))
    (unless (and (consp forms) (null (cdr forms)) (consp section))
      (error "native batch reader returned invalid v4 prefix in %s" label))
    (unless (= (plist-get section :native-section-version)
               nelisp-artifact--legacy-self-sized-native-section-version)
      (error "native batch reader returned invalid v4 version in %s" label))
    (unless (and section-end
                 (> section-end start)
                 (<= section-end limit)
                 (= (aref source (1- section-end)) ?\))
                 (or (= section-end limit)
                     (let ((ch (aref source section-end)))
                       (or (= ch ?\s) (= ch ?\t)
                           (= ch ?\n) (= ch ?\r) (= ch ?\))))))
      (error "invalid native serialized character boundary in %s" label))
    (unless (and (eq (plist-get section :runtime-end) t)
                 (eq (plist-get section :reloc-format)
                     nelisp-artifact--compact-reloc-format)
                 (integerp (plist-get section :reloc-count))
                 (>= (plist-get section :reloc-count) 0))
      (error "invalid native batch runtime prefix in %s" label))
    (cons section section-end)))

(defun nelisp-artifact--read-private-native-load-section-v4-ordered
    (source start limit label)
  "Read one self-sized version 4 native section at START.
Only the ordered runtime prefix is parsed.  After `:runtime-end', the
serialized character size jumps directly over object and diagnostic fields."
  (unless (and (< start limit) (= (aref source start) ?\())
    (error "invalid native section in %s" label))
  (let ((profile-start (nelisp-artifact--profile-time))
        (pos (1+ start))
        (section-end limit)
        (section nil)
        (read-field nil))
    (setq read-field
          (lambda (keyword kind)
            (let* ((value-pos
                    (nelisp-artifact--private-ordered-field-value-start
                     source pos section-end keyword label))
                   (decimal-pair
                    (and (eq kind 'integer)
                         (let ((i value-pos)
                               (value 0)
                               (have nil))
                           (while (and (< i section-end)
                                       (let ((ch (aref source i)))
                                         (and (>= ch ?0) (<= ch ?9))))
                             (setq have t)
                             (setq value
                                   (+ (* value 10)
                                      (- (aref source i) ?0)))
                             (setq i (1+ i)))
                           (unless have
                             (error "expected native decimal field in %s"
                                    label))
                           (cons value i))))
                   (value-end
                    (cond
                     ((eq kind 'string)
                      (nelisp-artifact--private-string-end
                       source value-pos section-end label))
                     ((eq kind 'integer) (cdr decimal-pair))
                     (t
                      (nelisp-artifact--private-field-value-end
                       source value-pos section-end label))))
                   (value
                    (cond
                     ((eq kind 'string)
                      (progn
                        (unless (and (< value-pos value-end)
                                     (= (aref source value-pos) ?\")
                                     (= (aref source (1- value-end)) ?\"))
                          (error "invalid native string field %S in %s"
                                 keyword label))
                        (substring source (1+ value-pos) (1- value-end))))
                     ((eq kind 'integer) (car decimal-pair))
                     (t
                      (nelisp-artifact--read-private-item
                       source value-pos value-end)))))
              (unless (and (<= value-end section-end)
                           (or (= value-end section-end)
                               (let ((ch (aref source value-end)))
                                 (or (= ch ?\s) (= ch ?\t)
                                     (= ch ?\n) (= ch ?\r) (= ch ?\))))))
                (error "invalid native field delimiter for %S in %s"
                       keyword label))
              (setq pos value-end)
              value)))
    (let ((version (funcall read-field :native-section-version 'integer)))
      (unless (= version
                 nelisp-artifact--legacy-self-sized-native-section-version)
        (error "invalid ordered native section version in %s: %S"
               label version))
      (setq section (append section
                            (list :native-section-version version))))
    (let ((serialized-size
           (funcall read-field :serialized-char-size 'integer)))
      (unless (and (integerp serialized-size) (> serialized-size 0))
        (error "invalid native serialized character size in %s: %S"
               label serialized-size))
      (setq section-end (+ start serialized-size))
      (unless (and (> section-end start)
                   (<= section-end limit)
                   (= (aref source (1- section-end)) ?\))
                   (or (= section-end limit)
                       (let ((ch (aref source section-end)))
                         (or (= ch ?\s) (= ch ?\t)
                             (= ch ?\n) (= ch ?\r) (= ch ?\))))))
        (error "invalid native serialized character boundary in %s" label))
      (setq section
            (append section
                    (list :serialized-char-size serialized-size))))
    (nelisp-artifact--load-profile-log
     "native-v4-header" profile-start
     (list :end section-end))
    (dolist (spec '((:arch string)
                    (:symbols atom)
                    (:text-base64 string)
                    (:reloc-format atom)
                    (:reloc-count integer)))
      (let ((key (car spec)))
        (setq section
              (append section
                      (list key (funcall read-field key (cadr spec)))))
        (nelisp-artifact--load-profile-log
         "native-v4-field" profile-start (list :key key))))
    (let* ((count (plist-get section :reloc-count))
           (value-pos
            (nelisp-artifact--private-ordered-field-value-start
             source pos section-end :reloc-data label))
           (value-end
            (nelisp-artifact--private-field-value-end
             source value-pos section-end label)))
      (setq section
            (append
             section
             (list :reloc-data
                   ;; Keep this direct decimal parser inline.  The standalone
                   ;; command cache uses this loader while installing its own
                   ;; later helper defuns, so the bootstrap path cannot call
                   ;; `nelisp-artifact--read-compact-reloc-data' here.
                   (progn
                     (unless (and (integerp count) (>= count 0))
                       (error "invalid compact relocation count in %s: %S"
                              label count))
                     (if (= count 0)
                         (progn
                           (unless (and (= (- value-end value-pos) 3)
                                        (= (aref source value-pos) ?n)
                                        (= (aref source (1+ value-pos)) ?i)
                                        (= (aref source (+ value-pos 2)) ?l))
                             (error
                              "zero compact relocation data must be nil in %s"
                              label))
                           (make-vector 0 0))
                       (unless (and (< value-pos value-end)
                                    (= (aref source value-pos) ?\())
                         (error
                          "invalid compact relocation data in %s" label))
                       (when (> count (- value-end value-pos))
                         (error
                          "compact relocation count exceeds source span in %s"
                          label))
                       (let* ((slots (* count 3))
                              (out (make-vector slots 0))
                              (slot 0)
                              (data-pos (1+ value-pos)))
                         (while (< slot slots)
                           (setq data-pos
                                 (nelisp-read--skip-ws source data-pos))
                           (when (or (>= data-pos value-end)
                                     (= (aref source data-pos) ?\)))
                             (error
                              "compact relocation data ended early in %s"
                              label))
                           (let ((negative nil)
                                 (value 0)
                                 (digits 0))
                             (when (= (aref source data-pos) ?-)
                               (setq negative t)
                               (setq data-pos (1+ data-pos)))
                             (while
                                 (and (< data-pos value-end)
                                      (let ((ch (aref source data-pos)))
                                        (and (>= ch ?0) (<= ch ?9))))
                               (setq value
                                     (+ (* value 10)
                                        (- (aref source data-pos) ?0)))
                               (setq digits (1+ digits))
                               (setq data-pos (1+ data-pos)))
                             (when (= digits 0)
                               (error
                                "invalid compact relocation integer in %s"
                                label))
                             (aset out slot
                                   (if negative (- value) value)))
                           (setq slot (1+ slot)))
                         (setq data-pos
                               (nelisp-read--skip-ws source data-pos))
                         (unless (and (< data-pos value-end)
                                      (= (aref source data-pos) ?\))
                                      (= (1+ data-pos) value-end))
                           (error
                            "trailing compact relocation data in %s"
                            label))
                         out))))))
      (setq pos value-end))
    (nelisp-artifact--load-profile-log
     "native-v4-relocs" profile-start
     (list :count (plist-get section :reloc-count)))
    (dolist (key '(:extern-symbols :defuns))
      (setq section
            (append section
                    (list key (funcall read-field key 'atom)))))
    (unless (eq (funcall read-field :runtime-end 'atom) t)
      (error "native section has invalid :runtime-end marker in %s" label))
    (setq pos (nelisp-read--skip-ws source pos))
    (unless (nelisp-artifact--string-prefix-at-p
             ":object-format " source pos)
      (error "native section lacks diagnostic tail after :runtime-end in %s"
             label))
    (unless (eq (plist-get section :reloc-format)
                nelisp-artifact--compact-reloc-format)
      (error "unsupported compact relocation format in %s: %S"
             label (plist-get section :reloc-format)))
    (nelisp-artifact--load-profile-log "native-v4-prefix" profile-start)
    (cons section section-end)))

(defun nelisp-artifact--read-private-native-load-section-v4
    (source start limit label)
  "Read one generated version 4 native section at START.
Standalone uses its native batch reader for the bounded runtime prefix.  Host
Emacs and compatibility runtimes retain the ordered field parser."
  (if (fboundp 'nelisp--read-batch-from-string-native)
      (let ((profile-start (nelisp-artifact--profile-time))
            (parsed
             (nelisp-artifact--read-private-native-load-section-v4-native-prefix
              source start limit label)))
        (nelisp-artifact--load-profile-log
         "native-v4-native-prefix" profile-start
         (list :reloc-count (plist-get (car parsed) :reloc-count)))
        parsed)
    (nelisp-artifact--read-private-native-load-section-v4-ordered
     source start limit label)))

(defun nelisp-artifact--native-v5-skip-ws (source pos limit label)
  "Skip whitespace at POS without crossing LIMIT in SOURCE."
  (setq pos (nelisp-read--skip-ws source pos))
  (when (> pos limit)
    (error "version 5 prefix crossed its bound in %s" label))
  pos)

(defun nelisp-artifact--native-v5-read-string (source pos limit label)
  "Read one bounded printed string at POS and return (VALUE . END)."
  (let* ((end (nelisp-artifact--private-string-end
               source pos limit label))
         (raw-end (1- end))
         (slash
          (nelisp-artifact--string-search-char-bounded
           ?\\ source (1+ pos) raw-end)))
    (if (null slash)
        (cons (substring source (1+ pos) raw-end) end)
      (let* ((capacity (- raw-end (1+ pos)))
             (out (make-string capacity 0))
             (src (1+ pos))
             (dst 0))
        (while (< src raw-end)
          (let ((ch (aref source src)))
            (if (/= ch ?\\)
                (progn
                  (aset out dst ch)
                  (setq dst (1+ dst))
                  (setq src (1+ src)))
              (setq src (1+ src))
              (when (>= src raw-end)
                (error "truncated version 5 string escape in %s" label))
              (setq ch (aref source src))
              (aset out dst
                    (cond
                     ((= ch ?n) ?\n)
                     ((= ch ?t) ?\t)
                     ((= ch ?r) ?\r)
                     ((= ch ?b) ?\b)
                     ((= ch ?f) ?\f)
                     ((or (= ch ?\\) (= ch ?\")) ch)
                     (t
                      (error "unsupported version 5 string escape in %s"
                             label))))
              (setq dst (1+ dst))
              (setq src (1+ src)))))
        (cons (substring out 0 dst) end)))))

(defun nelisp-artifact--native-v5-read-integer
    (source pos limit label &optional signed)
  "Read one bounded integer at POS and return (VALUE . END).
When SIGNED is nil, reject negative values."
  (let ((negative nil)
        (value 0)
        (digits 0))
    (when (and signed (< pos limit) (= (aref source pos) ?-))
      (setq negative t)
      (setq pos (1+ pos)))
    (while (and (< pos limit)
                (let ((ch (aref source pos)))
                  (and (>= ch ?0) (<= ch ?9))))
      (setq value (+ (* value 10) (- (aref source pos) ?0)))
      (setq digits (1+ digits))
      (setq pos (1+ pos)))
    (when (= digits 0)
      (error "invalid version 5 integer in %s" label))
    (cons (if negative (- value) value) pos)))

(defun nelisp-artifact--native-v5-read-token (source pos limit label)
  "Read one bounded nonempty symbol token at POS."
  (let ((start pos))
    (while (and (< pos limit)
                (let ((ch (aref source pos)))
                  (not (or (= ch ?\s) (= ch ?\t)
                           (= ch ?\n) (= ch ?\r)
                           (= ch ?\() (= ch ?\))
                           (= ch ?\[) (= ch ?\])))))
      (setq pos (1+ pos)))
    (when (= start pos)
      (error "invalid version 5 symbol token in %s" label))
    (cons (substring source start pos) pos)))

(defun nelisp-artifact--native-v5-read-string-list
    (source pos limit label)
  "Read a bounded parenthesized string list at POS."
  (if (nelisp-artifact--string-prefix-at-p "nil" source pos)
      (cons nil (+ pos 3))
    (unless (and (< pos limit) (= (aref source pos) ?\())
      (error "invalid version 5 string list in %s" label))
    (let ((items nil)
          (done nil))
      (setq pos (1+ pos))
      (while (not done)
        (setq pos
              (nelisp-artifact--native-v5-skip-ws
               source pos limit label))
        (when (>= pos limit)
          (error "unterminated version 5 string list in %s" label))
        (if (= (aref source pos) ?\))
            (progn
              (setq pos (1+ pos))
              (setq done t))
          (unless (= (aref source pos) ?\")
            (error "non-string version 5 list member in %s" label))
          (let ((parsed
                 (nelisp-artifact--native-v5-read-string
                  source pos limit label)))
            (setq items (cons (car parsed) items))
            (setq pos (cdr parsed)))))
      (cons (nreverse items) pos))))

(defun nelisp-artifact--native-v5-base64-p (value)
  "Return non-nil when VALUE has canonical base64 syntax.
The Elisp loop below costs 132 us per character (measured), which is about
43% of `native-total' on the real bootstrap artifact; prefer the native
predicate when the runtime provides it.  Both apply the identical rule, and
both fail closed."
  (if (fboundp 'nelisp--base64-canonical-p)
      (nelisp--base64-canonical-p value)
  (let ((i 0)
        (len (length value))
        (padding 0)
        (ok t))
    (when (or (= len 0) (/= (% len 4) 0))
      (setq ok nil))
    (while (and ok (< i len))
      (let ((ch (aref value i)))
        (cond
         ((or (and (>= ch ?A) (<= ch ?Z))
              (and (>= ch ?a) (<= ch ?z))
              (and (>= ch ?0) (<= ch ?9))
              (= ch ?+) (= ch ?/))
          (when (> padding 0)
            (setq ok nil)))
         ((= ch ?=)
          (setq padding (1+ padding))
          (when (or (> padding 2) (< i (- len 2)))
            (setq ok nil)))
         (t (setq ok nil))))
      (setq i (1+ i)))
    ok)))

(defun nelisp-artifact--native-v5-direct-debug (stage)
  "Emit bounded direct-decoder STAGE in detailed profile mode."
  (when nelisp-artifact-profile-load-detail
    (nelisp-artifact--write-stderr
     (concat "artifact_load_profile progress=native-v5-direct"
             " stage=" stage))))

(defun nelisp-artifact--native-v5-read-defuns
    (source pos limit label)
  "Read canonical ordered version 5 defun metadata at POS."
  (unless (and (< pos limit) (= (aref source pos) ?\())
    (error "invalid version 5 defun list in %s" label))
  (let ((out nil)
        (done nil)
        (keys '((:name . string)
                (:offset . integer)
                (:size . integer)
                (:arity . integer)
                (:rest-p . atom)
                (:fixed-count . integer-or-nil)
                (:param-class . symbol)
                (:rt-slot-count . integer)
                (:body-offset . integer-or-nil))))
    (setq pos (1+ pos))
    (while (not done)
      (setq pos
            (nelisp-artifact--native-v5-skip-ws
             source pos limit label))
      (when (>= pos limit)
        (error "unterminated version 5 defun list in %s" label))
      (if (= (aref source pos) ?\))
          (progn
            (setq pos (1+ pos))
            (setq done t))
        (unless (= (aref source pos) ?\()
          (error "invalid version 5 defun entry in %s" label))
        (setq pos (1+ pos))
        (let ((entry nil)
              (rest keys))
          (while rest
            (let* ((spec (car rest))
                   (key (car spec))
                   (kind (cdr spec))
                   (value-pos
                    (nelisp-artifact--private-ordered-field-value-start
                     source pos limit key label))
                   (parsed
                    (cond
                     ((eq kind 'string)
                      (nelisp-artifact--native-v5-read-string
                       source value-pos limit label))
                     ((eq kind 'integer)
                      (nelisp-artifact--native-v5-read-integer
                       source value-pos limit label))
                     ((eq kind 'integer-or-nil)
                      (if (nelisp-artifact--string-prefix-at-p
                           "nil" source value-pos)
                          (cons nil (+ value-pos 3))
                        (nelisp-artifact--native-v5-read-integer
                         source value-pos limit label)))
                     (t
                      (nelisp-artifact--native-v5-read-token
                       source value-pos limit label))))
                   (value
                    (if (eq kind 'symbol)
                        (let ((token (car parsed)))
                          (unless (equal token "gp")
                            (error
                             "invalid version 5 defun param class in %s"
                             label))
                          'gp)
                      (car parsed))))
              (setq entry (append entry (list key value)))
              (setq pos (cdr parsed))
              (setq rest (cdr rest))))
          (setq pos
                (nelisp-artifact--native-v5-skip-ws
                 source pos limit label))
          (unless (and (< pos limit) (= (aref source pos) ?\)))
            (error "version 5 defun entry has trailing fields in %s"
                   label))
          (setq pos (1+ pos))
          (setq out (cons entry out)))))
    (cons (nreverse out) pos)))

(defun nelisp-artifact--native-v5-read-runtime-vector
    (source start limit label)
  "Directly decode one canonical bounded version 5 runtime vector."
  (unless (and (< start limit) (= (aref source start) ?\[))
    (error "invalid version 5 runtime vector in %s" label))
  (let* ((pos (1+ start))
         (layout-pair
          (nelisp-artifact--native-v5-read-integer
           source pos limit label))
         (layout (car layout-pair))
         (size-pair nil)
         (size nil)
         (end nil)
         (arch nil)
         (symbols nil)
         (text nil)
         (format nil)
         (count nil)
         (data nil)
         (externs nil)
         (defuns nil))
    (unless (= layout nelisp-artifact--native-runtime-prefix-layout-version)
      (error "unsupported version 5 runtime vector layout in %s" label))
    (setq pos
          (nelisp-artifact--native-v5-skip-ws
           source (cdr layout-pair) limit label))
    (setq size-pair
          (nelisp-artifact--native-v5-read-integer
           source pos limit label))
    (setq size (car size-pair))
    (setq end (+ start size))
    (unless (and (> size 0)
                 (<= end limit)
                 (= (aref source (1- end)) ?\]))
      (error "invalid version 5 runtime vector size in %s" label))
    (nelisp-artifact--native-v5-direct-debug "size")
    (setq pos
          (nelisp-artifact--native-v5-skip-ws
           source (cdr size-pair) end label))
    (let ((parsed
           (nelisp-artifact--native-v5-read-string source pos end label)))
      (setq arch (car parsed))
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (unless (member arch '("x86_64" "arm64"))
      (error "invalid version 5 native arch in %s" label))
    (nelisp-artifact--native-v5-direct-debug "arch")
    (let ((parsed
           (nelisp-artifact--native-v5-read-string-list
            source pos end label)))
      (setq symbols (car parsed))
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (nelisp-artifact--native-v5-direct-debug "symbols")
    (let ((parsed
           (nelisp-artifact--native-v5-read-string source pos end label)))
      (setq text (car parsed))
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (unless (nelisp-artifact--native-v5-base64-p text)
      (error "invalid version 5 text base64 in %s" label))
    (nelisp-artifact--native-v5-direct-debug "text")
    (let ((parsed
           (nelisp-artifact--native-v5-read-token source pos end label)))
      (unless (equal (car parsed)
                     (symbol-name nelisp-artifact--compact-reloc-format))
        (error "invalid version 5 relocation format in %s" label))
      (setq format nelisp-artifact--compact-reloc-format)
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (nelisp-artifact--native-v5-direct-debug "format")
    (let ((parsed
           (nelisp-artifact--native-v5-read-integer
            source pos end label)))
      (setq count (car parsed))
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (nelisp-artifact--native-v5-direct-debug "count")
    (if (= count 0)
        (progn
          (unless (nelisp-artifact--string-prefix-at-p "nil" source pos)
            (error "zero version 5 relocation data must be nil in %s"
                   label))
          (setq data (make-vector 0 0))
          (setq pos
                (nelisp-artifact--native-v5-skip-ws
                 source (+ pos 3) end label)))
      (let ((data-end pos))
        (unless (= (aref source pos) ?\()
          (error "invalid version 5 relocation data in %s" label))
        (while (and (< data-end end) (/= (aref source data-end) ?\)))
          (setq data-end (1+ data-end)))
        (when (>= data-end end)
          (error "unterminated version 5 relocation data in %s" label))
        (setq data-end (1+ data-end))
        (setq data
              (nelisp-artifact--read-compact-reloc-data
               source pos data-end count label))
        (setq pos
              (nelisp-artifact--native-v5-skip-ws
               source data-end end label))))
    (nelisp-artifact--native-v5-direct-debug "reloc-data")
    (let ((parsed
           (nelisp-artifact--native-v5-read-string-list
            source pos end label)))
      (setq externs (car parsed))
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (nelisp-artifact--native-v5-direct-debug "externs")
    (let ((slot 1)
          (slots (length data))
          (extern-count (length externs)))
      (while (< slot slots)
        (let ((index (aref data slot)))
          (unless (and (integerp index)
                       (>= index 0)
                       (< index extern-count))
            (error "version 5 relocation index out of range in %s" label)))
        (setq slot (+ slot 3))))
    (let ((parsed
           (nelisp-artifact--native-v5-read-defuns
            source pos end label)))
      (setq defuns (car parsed))
      (setq pos (nelisp-artifact--native-v5-skip-ws
                 source (cdr parsed) end label)))
    (nelisp-artifact--native-v5-direct-debug "defuns")
    (unless (= pos (1- end))
      (error "version 5 runtime vector has trailing fields in %s" label))
    (cons
     (vector layout size arch symbols text format count data externs defuns)
     end)))

(defun nelisp-artifact--read-private-native-load-section-v5
    (source start limit label)
  "Read one version 5 native section without copying its runtime prefix."
  (unless (and (< start limit) (= (aref source start) ?\())
    (error "invalid version 5 native section in %s" label))
  (let* ((profile-start (nelisp-artifact--profile-time))
         (section-origin 0)
         (pos (1+ start))
         (version-pos
          (nelisp-artifact--private-ordered-field-value-start
           source pos limit :native-section-version label))
         (version-pair (nelisp-artifact--read-decimal-at source version-pos))
         (version (car version-pair))
         (size-pos nil)
         (size-pair nil)
         (serialized-size nil)
         (section-end nil)
         (runtime-pos nil)
         (read-start nil)
         (read-result nil)
         (forms nil)
         (runtime nil)
         (runtime-end nil)
         (runtime-size nil)
         (tail-pos nil))
    (unless (= version nelisp-artifact--native-section-version)
      (error "invalid version 5 native section version in %s: %S"
             label version))
    (setq pos (cdr version-pair))
    (setq size-pos
          (nelisp-artifact--private-ordered-field-value-start
           source pos limit :serialized-char-size label))
    (setq size-pair (nelisp-artifact--read-decimal-at source size-pos))
    (setq serialized-size (car size-pair))
    (unless (> serialized-size 0)
      (error "invalid version 5 serialized character size in %s" label))
    (setq section-end (+ start serialized-size))
    (unless (and (> section-end start)
                 (<= section-end limit)
                 (= (aref source (1- section-end)) ?\))
                 (or (= section-end limit)
                     (let ((ch (aref source section-end)))
                       (or (= ch ?\s) (= ch ?\t)
                           (= ch ?\n) (= ch ?\r) (= ch ?\))))))
      (error "invalid version 5 serialized character boundary in %s" label))
    ;; Copy the section out of the artifact before decoding it.  Character
    ;; indices are resolved over the whole UTF-8 contents of SOURCE, so
    ;; decoding in place inside the 13.24 MB artifact measured 13,725.7 ms for
    ;; one 108,765 character section against 2,533.5 ms for the same section
    ;; held in its own string.  The boundary predicate above has already been
    ;; applied to the original SOURCE, including the character that must
    ;; follow the section, so nothing is validated less strictly here.
    ;; SECTION-ORIGIN restores the absolute end for the caller.
    ;; `NELISP_V5_SECTION_EXTRACT=0' turns the copy off for A/B diagnosis
    ;; against one binary: the copy is a large per-section allocation and a
    ;; decode failure was observed that depends only on when garbage
    ;; collection happens.  Any other value, or the variable being unset,
    ;; keeps the copy.
    (unless (or (and (= start 0) (= section-end limit))
                (equal (getenv "NELISP_V5_SECTION_EXTRACT") "0"))
      (setq section-origin start)
      (setq source (substring source start section-end))
      (setq size-pair (cons (car size-pair) (- (cdr size-pair) start)))
      (setq section-end (- section-end start))
      (setq start 0))
    (setq pos (cdr size-pair))
    (setq runtime-pos
          (nelisp-artifact--private-ordered-field-value-start
           source pos section-end :runtime-prefix label))
    (setq read-start (nelisp-artifact--profile-time))
    (if (= (aref source runtime-pos) ?\[)
        (condition-case direct-err
            (progn
              (setq read-result
                    (nelisp-artifact--native-v5-read-runtime-vector
                     source runtime-pos section-end label))
              (setq runtime (car read-result))
              (setq runtime-end (cdr read-result)))
          (error
           (when nelisp-artifact-profile-load-detail
             (nelisp-artifact--write-stderr
              (concat "artifact_load_profile stage=native-v5-direct-error"
                      " detail=" (prin1-to-string direct-err))))
           (signal (car direct-err) (cdr direct-err))))
      (unless (= (aref source runtime-pos) ?\()
        (error "invalid version 5 runtime prefix opener in %s" label))
      (if (fboundp 'nelisp--read-batch-from-string-native)
        (progn
          (setq read-result
                (nelisp--read-batch-from-string-native
                 source runtime-pos 1))
          (setq forms (car read-result))
          (setq runtime-end (cdr read-result))
          (unless (and (consp forms) (null (cdr forms)))
            (error "native batch reader returned invalid v5 prefix in %s"
                   label))
          (setq runtime (car forms)))
      (setq read-result (read-from-string source runtime-pos))
      (setq runtime (car read-result))
        (setq runtime-end (cdr read-result))))
    (nelisp-artifact--load-profile-log
     "native-v5-native-read" read-start
     (list :start runtime-pos :end runtime-end))
    (setq runtime-size
          (cond
           ((and (vectorp runtime)
                 (= (length runtime) 10)
                 (= (aref runtime 0)
                    nelisp-artifact--native-runtime-prefix-layout-version)
                 (integerp (aref runtime 1)))
            (aref runtime 1))
           ((and (consp runtime)
                 (eq (car runtime) :runtime-prefix-char-size)
                 (integerp (cadr runtime)))
            (cadr runtime))
           (t nil)))
    (unless (and (integerp runtime-size)
                 (> runtime-size 0)
                 (= runtime-end (+ runtime-pos runtime-size))
                 (<= runtime-end section-end)
                 (= (aref source (1- runtime-end))
                    (if (vectorp runtime) ?\] ?\))))
      (error "invalid version 5 native runtime prefix in %s" label))
    (when (consp runtime)
      (unless
          (and (eq (plist-get runtime :runtime-end) t)
               (stringp (plist-get runtime :arch))
               (listp (plist-get runtime :symbols))
               (stringp (plist-get runtime :text-base64))
               (nelisp-artifact--native-v5-base64-p
                (plist-get runtime :text-base64))
               (eq (plist-get runtime :reloc-format)
                   nelisp-artifact--compact-reloc-format)
               (integerp (plist-get runtime :reloc-count))
               (>= (plist-get runtime :reloc-count) 0)
               (let ((data (plist-get runtime :reloc-data)))
                 (and (or (listp data) (vectorp data))
                      (= (length data)
                         (* 3 (plist-get runtime :reloc-count)))))
               (listp (plist-get runtime :extern-symbols))
               (listp (plist-get runtime :defuns)))
        (error "invalid legacy version 5 runtime prefix in %s" label)))
    (setq tail-pos (nelisp-read--skip-ws source runtime-end))
    (unless (nelisp-artifact--string-prefix-at-p
             ":object-format " source tail-pos)
      (error "version 5 native section lacks object tail in %s" label))
    (nelisp-artifact--load-profile-log
     "native-v5-prefix" profile-start
     (list :chars runtime-size))
    (cons
     (if (vectorp runtime)
         runtime
       (append
        (list :native-section-version version
              :serialized-char-size serialized-size)
        runtime))
     (+ section-origin section-end))))

(defun nelisp-artifact--read-private-native-load-section
    (source start limit label)
  "Read native load metadata and return (SECTION . END).
Version 4 takes the bounded ordered-prefix path.  Version 2 and legacy version
3 retain the depth-aware compatibility scanner."
  (let* ((version-pos
          (and (< start limit)
               (= (aref source start) ?\()
               (nelisp-artifact--private-ordered-field-value-start
                source (1+ start) limit :native-section-version label)))
         (version-pair
          (and version-pos
               (let ((i version-pos)
                     (value 0)
                     (have nil))
                 (while (and (< i limit)
                             (let ((ch (aref source i)))
                               (and (>= ch ?0) (<= ch ?9))))
                   (setq have t)
                   (setq value (+ (* value 10)
                                  (- (aref source i) ?0)))
                   (setq i (1+ i)))
                 (unless have
                   (error "expected native section version in %s" label))
                 (cons value i))))
         (version (car-safe version-pair))
         (version-end (cdr-safe version-pair)))
    (nelisp-artifact--load-profile-log
     "native-section-version" (nelisp-artifact--profile-time)
     (list :version version))
    (unless (and version-end
                 (<= version-end limit)
                 (< version-end limit)
                 (let ((ch (aref source version-end)))
                   (or (= ch ?\s) (= ch ?\t)
                       (= ch ?\n) (= ch ?\r) (= ch ?\)))))
      (error "invalid native section version token in %s" label))
    (cond
     ((and (integerp version)
           (= version nelisp-artifact--native-section-version))
      (nelisp-artifact--read-private-native-load-section-v5
       source start limit label))
     ((and (integerp version)
           (= version
              nelisp-artifact--legacy-self-sized-native-section-version))
      (nelisp-artifact--read-private-native-load-section-v4
       source start limit label))
     (t
      (nelisp-artifact--read-private-native-load-section-legacy
       source start limit label)))))

(defun nelisp-artifact--generated-v4-native-key-pair (content list-start)
  "Return a literal-located generated v4 native key pair, or nil.
This standalone bootstrap proof accepts a candidate only before the generated
`:module-init' key and only when its value starts with an ordered v4 section.
Unexpected or legacy input falls back to the depth-aware top-level scanner."
  (when (fboundp 'nelisp--read-batch-from-string-native)
    (let* ((len (length content))
           (module-pos
            (nelisp-artifact--string-search-literal
             ":module-init " content list-start))
           (sections-pos
            (nelisp-artifact--string-search-literal
             ":native-sections " content list-start))
           (native-pos
            (nelisp-artifact--string-search-literal
             ":native " content list-start))
           (pair nil))
      (when (and sections-pos module-pos (< sections-pos module-pos))
        (let* ((value-pos
                (nelisp-read--skip-ws content (+ sections-pos 16)))
               (section-pos
                (and (< value-pos len)
                     (= (aref content value-pos) ?\()
                     (nelisp-read--skip-ws content (1+ value-pos)))))
          (when (and section-pos
                     (or
                      (nelisp-artifact--string-prefix-at-p
                       "(:native-section-version 5 " content section-pos)
                      (nelisp-artifact--string-prefix-at-p
                       "(:native-section-version 4 " content section-pos)))
            (setq pair (cons :native-sections sections-pos)))))
      (when (and (null pair) native-pos module-pos (< native-pos module-pos))
        (let ((value-pos
               (nelisp-read--skip-ws content (+ native-pos 7))))
          (when (and (< value-pos len)
                     (or
                      (nelisp-artifact--string-prefix-at-p
                       "(:native-section-version 5 " content value-pos)
                      (nelisp-artifact--string-prefix-at-p
                       "(:native-section-version 4 " content value-pos)))
            (setq pair (cons :native native-pos)))))
      pair)))

(defun nelisp-artifact--read-serialized-native-sections-for-load
    (content artifact-path &optional native-value-offset)
  "Read only native fields needed to load and link CONTENT.
The full serialized reader remains available to inspection and object
extraction paths.  This reader bounds every section independently and never
materializes `:object-base64', object integrity metadata, or
`:compile-report'."
  (let* ((prefix-len (length nelisp-artifact--magic))
         (scan-start (nelisp-artifact--profile-time))
         (list-start (nelisp-read--skip-ws content prefix-len))
         (offset-layout-p (integerp native-value-offset))
         (offset-key-pair
          (cond
           ((not offset-layout-p) nil)
           ((= native-value-offset 0) nil)
           ((or (< native-value-offset 0)
                (>= native-value-offset (length content)))
            (error "invalid native value offset in %s: %S"
                   artifact-path native-value-offset))
           ((and (>= native-value-offset 17)
                 (nelisp-artifact--string-prefix-at-p
                  ":native-sections " content
                  (- native-value-offset 17)))
            (cons :native-sections (- native-value-offset 17)))
           ((and (>= native-value-offset 8)
                 (nelisp-artifact--string-prefix-at-p
                  ":native " content (- native-value-offset 8)))
            (cons :native (- native-value-offset 8)))
           (t
            (error "native value offset does not follow a native key in %s"
                   artifact-path))))
         (native-key-pair
          (if offset-layout-p
              offset-key-pair
            (or
             (nelisp-artifact--generated-v4-native-key-pair
              content list-start)
             (car
              (nelisp-artifact--private-list-key-positions
               content list-start (length content)
               '((:native-sections . ":native-sections")
                 (:native . ":native"))
               artifact-path t)))))
         (sections-key
          (and (eq (car-safe native-key-pair) :native-sections)
               (cdr native-key-pair)))
         (native-key
          (and (eq (car-safe native-key-pair) :native)
               (cdr native-key-pair)))
         (key-pos (or sections-key native-key))
         (len (length content))
         (sections nil)
         (count 0))
    (nelisp-artifact--load-profile-log
     "native-key-scan" scan-start (list :key (car-safe native-key-pair)))
    (if (null key-pos)
        nil
      (let ((pos
             (if offset-layout-p
                 native-value-offset
               (nelisp-read--skip-ws
                content
                (+ key-pos
                   (length
                    (symbol-name
                     (if sections-key :native-sections :native))))))))
        (nelisp-artifact--load-profile-log
         "native-value-start" scan-start (list :pos pos))
        (if native-key
            (list
             (car
              (nelisp-artifact--read-private-native-load-section
               content pos len artifact-path)))
          (unless (and (< pos len) (= (aref content pos) ?\())
            (error "invalid :native-sections list in %s" artifact-path))
          (setq pos (1+ pos))
          (while (progn
                   (setq pos (nelisp-read--skip-ws content pos))
                   (and (< pos len) (not (= (aref content pos) ?\)))))
            (let* ((parsed
                    (nelisp-artifact--read-private-native-load-section
                     content pos len artifact-path)))
              (setq sections (cons (car parsed) sections))
              (setq pos (cdr parsed))
              (setq count (1+ count))
          (when (and nelisp-artifact-profile-load-detail
                     (= 0 (% count 100)))
                (nelisp-artifact--write-stderr
                 (concat
                  "artifact_load_profile progress=native-section"
                  " count=" (number-to-string count)
                  " pos=" (number-to-string pos))))))
          (unless (and (< pos len) (= (aref content pos) ?\)))
            (error "unterminated :native-sections list in %s" artifact-path))
          (nreverse sections))))))

(defun nelisp-artifact--native-section-index-for-symbol
    (sections symbol)
  "Return last (INDEX . SECTION) exporting SYMBOL from SECTIONS."
  (let ((name (if (symbolp symbol) (symbol-name symbol) symbol))
        (rest sections)
        (index 0)
        (found nil))
    (while rest
      (when
          (member
           name
           (nelisp-artifact--native-section-get (car rest) :symbols))
        (setq found (cons index (car rest))))
      (setq rest (cdr rest))
      (setq index (1+ index)))
    found))

(defun nelisp-artifact--serialized-native-section-for-symbol
    (artifact-path symbol &optional content)
  "Return the full serialized section exporting SYMBOL from ARTIFACT-PATH.
CONTENT may supply the already-read private artifact string.  Section lookup
uses the same last-section-wins rule as standalone native linking, so metadata
and extracted object bytes always describe the same duplicate export."
  (let* ((artifact-content
          (or content (nelisp-artifact--read-file-as-string artifact-path)))
         (sections
          (nelisp-artifact--read-serialized-native-sections
           artifact-content artifact-path))
         (indexed
          (nelisp-artifact--native-section-index-for-symbol sections symbol)))
    (cdr indexed)))

(defun nelisp-artifact--native-mapping-key (artifact-path section-index)
  "Return runtime mapping key for ARTIFACT-PATH and SECTION-INDEX."
  (cons (expand-file-name artifact-path) section-index))

(defun nelisp-artifact--native-runtime-mapping
    (artifact-path section-index)
  "Return cached mapping for ARTIFACT-PATH SECTION-INDEX."
  (cdr (assoc (nelisp-artifact--native-mapping-key
               artifact-path section-index)
              nelisp-artifact--native-runtime-mappings)))

(defun nelisp-artifact--cache-native-runtime-mapping
    (artifact-path section-index mapping)
  "Cache MAPPING for ARTIFACT-PATH SECTION-INDEX."
  (let* ((key (nelisp-artifact--native-mapping-key
               artifact-path section-index))
         (entry (assoc key nelisp-artifact--native-runtime-mappings)))
    (if entry
        (setcdr entry mapping)
      (setq nelisp-artifact--native-runtime-mappings
            (cons (cons key mapping)
                  nelisp-artifact--native-runtime-mappings)))
    mapping))

(defun nelisp-artifact--native-page-size (byte-count)
  "Round BYTE-COUNT up to a non-zero 4096-byte mapping."
  (* 4096 (max 1 (/ (+ byte-count 4095) 4096))))

(defun nelisp-artifact--native-decode-text (text-base64)
  "Decode native TEXT-BASE64 without UTF-8 re-encoding raw bytes."
  (unless (stringp text-base64)
    (error "standalone native linker missing text payload"))
  (if (fboundp 'nelisp--base64-decode-native)
      (nelisp--base64-decode-native text-base64)
    (base64-decode-string text-base64)))

(defun nelisp-artifact--native-write-jump-stub (address target)
  "Write `movabs TARGET,%rax; jmp *%rax' at ADDRESS."
  (let ((stub (unibyte-string #x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0)))
    (nelisp--ptr-copy-string-bytes address stub)
    (ptr-write-u64 address 2 target)
    address))

(defun nelisp-artifact--native-alist-set (key value alist)
  "Return ALIST with KEY mapped to VALUE, replacing an older entry."
  (let ((rest alist)
        (out nil))
    (while rest
      (unless (equal (caar rest) key)
        (setq out (cons (car rest) out)))
      (setq rest (cdr rest)))
    (cons (cons key value) (nreverse out))))

(defun nelisp-artifact--native-unique-names (names)
  "Return string NAMES without duplicates, preserving first order."
  (let ((rest names)
        (seen nil)
        (out nil))
    (while rest
      (let ((name (car rest)))
        (unless (assoc name seen)
          (setq seen (cons (cons name t) seen))
          (setq out (cons name out))))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun nelisp-artifact--native-link-diagnostic
    (artifact-path status &rest details)
  "Record native link STATUS and DETAILS for ARTIFACT-PATH."
  (let ((entry
         (append (list :artifact (expand-file-name artifact-path)
                       :status status)
                 details)))
    (setq nelisp-artifact--native-link-diagnostics
          (cons entry nelisp-artifact--native-link-diagnostics))
    entry))

(defun nelisp-artifact--native-build-symbol-index (sections)
  "Build a last-wins global defun index for SECTIONS.
Return a plist containing `:index' and duplicate diagnostics."
  (let ((rest sections)
        (section-index 0)
        (index nil)
        (duplicates nil))
    (while rest
      (let ((defs
             (nelisp-artifact--native-section-get (car rest) :defuns)))
        (while defs
          (let* ((defun (car defs))
                 (name (plist-get defun :name))
                 (offset (plist-get defun :offset))
                 (old (and (stringp name) (assoc name index)))
                 (entry
                  (list :section-index section-index
                        :offset offset
                        :defun defun)))
            (unless (stringp name)
              (error "standalone native linker invalid defun name: %S" name))
            (unless (and (integerp offset) (>= offset 0))
              (error "standalone native linker invalid defun offset: %S"
                     offset))
            (when old
              (setq duplicates
                    (cons
                     (list :symbol name
                           :previous-section
                           (plist-get (cdr old) :section-index)
                           :winner-section section-index)
                     duplicates)))
            (setq index
                  (nelisp-artifact--native-alist-set name entry index)))
          (setq defs (cdr defs))))
      (setq rest (cdr rest))
      (setq section-index (1+ section-index)))
    (list :index index :duplicates (nreverse duplicates))))

(defun nelisp-artifact--native-compact-relocs-p (section)
  "Return non-nil when SECTION carries indexed compact relocations."
  (eq (nelisp-artifact--native-section-get section :reloc-format)
      nelisp-artifact--compact-reloc-format))

(defun nelisp-artifact--native-flat-vector (values)
  "Return VALUES as a vector without a large `apply' argument frame."
  (if (vectorp values)
      values
    (let* ((len (length values))
           (out (make-vector len nil))
           (rest values)
           (index 0))
      (while rest
        (aset out index (car rest))
        (setq rest (cdr rest))
        (setq index (1+ index)))
      out)))

(defun nelisp-artifact--native-reloc-cursor (section)
  "Return a non-allocating relocation cursor for SECTION.
The cursor is `[KIND DATA EXTERNS INDEX COUNT]'.  Compact DATA is the flat
numeric vector read directly from the artifact.  Legacy DATA is a vector of
the original relocation plists, preserving their order without expanding a
compact table into plists."
  (if (nelisp-artifact--native-compact-relocs-p section)
      (let* ((raw
              (nelisp-artifact--native-section-get section :reloc-data))
             (data (nelisp-artifact--native-flat-vector raw))
             (externs-raw
              (nelisp-artifact--native-section-get
               section :extern-symbols))
             (externs (nelisp-artifact--native-flat-vector externs-raw))
             (count
              (nelisp-artifact--native-section-get section :reloc-count)))
        (unless (and (integerp count) (>= count 0)
                     (= (length data) (* count 3)))
          (error "invalid compact native relocation table"))
        (vector 'compact data externs 0 count))
    (let* ((raw (plist-get section :relocs))
           (data (nelisp-artifact--native-flat-vector raw)))
      (vector 'legacy data nil 0 (length data)))))

(defun nelisp-artifact--native-reloc-cursor-done-p (cursor)
  "Return non-nil when relocation CURSOR is exhausted."
  (>= (aref cursor 3) (aref cursor 4)))

(defun nelisp-artifact--native-reloc-cursor-offset (cursor)
  "Return current relocation offset from CURSOR."
  (let ((index (aref cursor 3))
        (data (aref cursor 1)))
    (if (eq (aref cursor 0) 'compact)
        (aref data (* index 3))
      (plist-get (aref data index) :offset))))

(defun nelisp-artifact--native-reloc-cursor-type (cursor)
  "Return current relocation type from CURSOR."
  (if (eq (aref cursor 0) 'compact)
      'plt32
    (plist-get (aref (aref cursor 1) (aref cursor 3)) :type)))

(defun nelisp-artifact--native-reloc-cursor-symbol (cursor)
  "Return current relocation symbol from CURSOR.
Compact symbol indexes are validated before indexing the extern vector."
  (if (eq (aref cursor 0) 'compact)
      (let* ((data (aref cursor 1))
             (index (aref cursor 3))
             (symbol-index (aref data (1+ (* index 3))))
             (externs (aref cursor 2)))
        (unless (and (integerp symbol-index)
                     (>= symbol-index 0)
                     (< symbol-index (length externs)))
          (error "compact native relocation symbol index out of range: %S"
                 symbol-index))
        (aref externs symbol-index))
    (plist-get (aref (aref cursor 1) (aref cursor 3)) :symbol)))

(defun nelisp-artifact--native-reloc-cursor-symbol-index (cursor)
  "Return current compact relocation symbol index, or nil for legacy data."
  (when (eq (aref cursor 0) 'compact)
    (aref (aref cursor 1) (1+ (* (aref cursor 3) 3)))))

(defun nelisp-artifact--native-reloc-cursor-addend (cursor)
  "Return current relocation addend from CURSOR."
  (if (eq (aref cursor 0) 'compact)
      (aref (aref cursor 1) (+ (* (aref cursor 3) 3) 2))
    (or (plist-get (aref (aref cursor 1) (aref cursor 3)) :addend)
        0)))

(defun nelisp-artifact--native-reloc-cursor-next (cursor)
  "Advance relocation CURSOR and return it."
  (aset cursor 3 (1+ (aref cursor 3)))
  cursor)

(defun nelisp-artifact--native-section-dependencies (section)
  "Return the unique external symbol dependencies of SECTION."
  (let ((names
         (nelisp-artifact--native-section-get section :extern-symbols)))
    (if (nelisp-artifact--native-compact-relocs-p section)
        (nelisp-artifact--native-unique-names names)
      (let ((cursor (nelisp-artifact--native-reloc-cursor section)))
        (while (not (nelisp-artifact--native-reloc-cursor-done-p cursor))
          (setq names
                (append
                 names
                 (list
                  (nelisp-artifact--native-reloc-cursor-symbol cursor))))
          (nelisp-artifact--native-reloc-cursor-next cursor))
        (nelisp-artifact--native-unique-names names)))))

(defun nelisp-artifact--native-preflight-artifact
    (artifact-path sections)
  "Validate and prepare all native SECTIONS for ARTIFACT-PATH.
Runtime externs are resolved before any mapping is owned.  Cross-section
references remain symbolic until all section base addresses are known."
  (unless sections
    (error "standalone native linker has no serialized sections"))
  (setq nelisp-artifact--native-last-preflight-duplicates nil)
  (let* ((index-result
          (nelisp-artifact--native-build-symbol-index sections))
         (symbol-index (plist-get index-result :index))
         (duplicates (plist-get index-result :duplicates))
         (runtime-targets nil)
         (prepared nil)
         (rest sections)
         (section-index 0))
    (setq nelisp-artifact--native-last-preflight-duplicates duplicates)
    (while rest
      (let* ((section (car rest))
             (arch (nelisp-artifact--native-section-get section :arch))
             (text
              (nelisp-artifact--native-decode-text
               (nelisp-artifact--native-section-get
                section :text-base64)))
             (text-size (nelisp-artifact--byte-length text))
             (dependencies
              (nelisp-artifact--native-section-dependencies section))
             (stub-base
              (+ text-size 15
                 (- (logand (+ text-size 15) 15))))
             (mapping-size
              (nelisp-artifact--native-page-size
               (+ stub-base (* (length dependencies) 16)))))
        (unless (equal arch "x86_64")
          (error "standalone native linker unsupported arch: %S" arch))
        (unless (> text-size 0)
          (error "standalone native linker missing text payload"))
        (when (nelisp-artifact--native-compact-relocs-p section)
          (let* ((externs
                  (nelisp-artifact--native-flat-vector
                   (nelisp-artifact--native-section-get
                    section :extern-symbols)))
                 (count (length externs))
                 (index 0)
                 (seen nil))
            (while (< index count)
              (let ((name (aref externs index)))
                (unless (stringp name)
                  (error
                   "standalone native linker invalid external symbol: %S"
                   name))
                (when (assoc name seen)
                  (error
                   "standalone native linker duplicate compact extern: %S"
                   name))
                (setq seen (cons (cons name t) seen)))
              (setq index (1+ index)))))
        (let ((cursor (nelisp-artifact--native-reloc-cursor section)))
          (while (not (nelisp-artifact--native-reloc-cursor-done-p cursor))
            (let* ((type
                    (nelisp-artifact--native-reloc-cursor-type cursor))
                   (offset
                    (nelisp-artifact--native-reloc-cursor-offset cursor))
                   (symbol
                    (nelisp-artifact--native-reloc-cursor-symbol cursor))
                   (addend
                    (nelisp-artifact--native-reloc-cursor-addend cursor)))
              (unless (eq type 'plt32)
                (error "standalone native linker unsupported reloc: %S"
                       type))
              (unless (and (integerp offset) (>= offset 0)
                           (<= (+ offset 4) text-size))
                (error "standalone native linker relocation offset out of range: %S"
                       offset))
              (unless (stringp symbol)
                (error "standalone native linker invalid relocation symbol: %S"
                       symbol))
              (unless (integerp addend)
                (error "standalone native linker invalid relocation addend: %S"
                       addend)))
            (nelisp-artifact--native-reloc-cursor-next cursor)))
        (let ((deps dependencies))
          (while deps
            (let ((name (car deps)))
              (unless (stringp name)
                (error
                 "standalone native linker invalid external symbol: %S"
                 name))
              (unless (assoc name symbol-index)
                (unless (assoc name runtime-targets)
                  (let ((target
                         (condition-case nil
                             (nelisp--runtime-symbol-address name)
                           (error nil))))
                    (unless (and (integerp target) (> target 0))
                      (error "standalone native linker unresolved extern: %S"
                             name))
                    (setq runtime-targets
                          (cons (cons name target) runtime-targets))))))
            (setq deps (cdr deps))))
        (setq prepared
              (cons
               (list :index section-index
                     :section section
                     :text text
                     :text-size text-size
                     :dependencies dependencies
                     :stub-base stub-base
                     :mapping-size mapping-size)
               prepared)))
      (setq rest (cdr rest))
      (setq section-index (1+ section-index)))
    (list :artifact (expand-file-name artifact-path)
          :sections (nreverse prepared)
          :symbol-index symbol-index
          :runtime-targets runtime-targets
          :duplicates duplicates)))

(defun nelisp-artifact--native-mapping-by-index (mappings section-index)
  "Return from MAPPINGS the entry for SECTION-INDEX."
  (let ((rest mappings)
        (found nil))
    (while (and rest (not found))
      (when (= (plist-get (car rest) :index) section-index)
        (setq found (car rest)))
      (setq rest (cdr rest)))
    found))

(defun nelisp-artifact--native-symbol-target
    (name symbol-index runtime-targets mappings)
  "Resolve NAME using artifact SYMBOL-INDEX or RUNTIME-TARGETS."
  (let ((global (assoc name symbol-index)))
    (if global
        (let* ((info (cdr global))
               (mapping
                (nelisp-artifact--native-mapping-by-index
                 mappings (plist-get info :section-index))))
          (unless mapping
            (error "standalone native linker missing owner mapping: %S" name))
          (+ (plist-get mapping :base) (plist-get info :offset)))
      (let ((runtime (assoc name runtime-targets)))
        (unless runtime
          (error "standalone native linker unresolved extern: %S" name))
        (cdr runtime)))))

(defun nelisp-artifact--native-unmap-owned (mappings)
  "Unmap every entry in owned MAPPINGS."
  (let ((rest mappings))
    (while rest
      (let ((mapping (car rest)))
        (when (and (integerp (plist-get mapping :base))
                   (>= (plist-get mapping :base) 4096)
                   (integerp (plist-get mapping :size))
                   (> (plist-get mapping :size) 0))
          (syscall-direct 11
                          (plist-get mapping :base)
                          (plist-get mapping :size)
                          0 0 0 0)))
      (setq rest (cdr rest)))))

(defun nelisp-artifact--native-commit-linkset
    (artifact-path plan mappings)
  "Atomically publish prepared PLAN and RX MAPPINGS for ARTIFACT-PATH."
  (let* ((artifact (expand-file-name artifact-path))
         (symbol-index (plist-get plan :symbol-index))
         (resolved-index nil)
         (mapping-cache nelisp-artifact--native-runtime-mappings)
         (rest mappings))
    (while rest
      (let* ((mapping (car rest))
             (key
              (nelisp-artifact--native-mapping-key
               artifact (plist-get mapping :index))))
        (setq mapping-cache
              (nelisp-artifact--native-alist-set
               key mapping mapping-cache)))
      (setq rest (cdr rest)))
    (setq rest symbol-index)
    (while rest
      (let* ((name (caar rest))
             (info (cdar rest))
             (mapping
              (nelisp-artifact--native-mapping-by-index
               mappings (plist-get info :section-index))))
        (setq resolved-index
              (cons
               (cons name
                     (append
                      info
                      (list :address
                            (+ (plist-get mapping :base)
                               (plist-get info :offset)))))
               resolved-index)))
      (setq rest (cdr rest)))
    ;; Publish only after every section has reached RX and all derived state
    ;; has been built locally.
    (setq nelisp-artifact--native-runtime-mappings mapping-cache)
    (setq nelisp-artifact--native-artifact-symbol-index
          (nelisp-artifact--native-alist-set
           artifact resolved-index
           nelisp-artifact--native-artifact-symbol-index))
    (let ((linkset
           (list :state 'ready
                 :mappings mappings
                 :duplicates (plist-get plan :duplicates))))
      (setq nelisp-artifact--native-artifact-linksets
            (nelisp-artifact--native-alist-set
             artifact linkset nelisp-artifact--native-artifact-linksets))
      linkset)))

(defun nelisp-artifact--native-link-artifact (artifact-path)
  "Transactionally link every native section of ARTIFACT-PATH.
All sections are mapped RW, relocated through section-local jump stubs,
changed to RX, and then committed together.  Any failure unmaps all memory
owned by this attempt and publishes no ready cache or symbol index."
  (let* ((artifact (expand-file-name artifact-path))
         (ready (cdr (assoc artifact
                            nelisp-artifact--native-artifact-linksets))))
    (if (eq (plist-get ready :state) 'ready)
        ready
      (let ((owned nil)
            (committed nil)
            (plan nil))
        (unwind-protect
            (condition-case err
                (progn
                  (setq plan
                        (nelisp-artifact--native-preflight-artifact
                         artifact
                         (nelisp-artifact--registered-native-sections
                          artifact)))
                  (let ((sections (plist-get plan :sections)))
                    (while sections
                      (let* ((prepared (car sections))
                             (size (plist-get prepared :mapping-size))
                             ;; PROT_READ|PROT_WRITE.  No executable mapping
                             ;; exists until every relocation has succeeded.
                             (base (syscall-direct 9 0 size 3 34 -1 0)))
                        (when (< base 4096)
                          (error "standalone native linker mmap failed: %S"
                                 base))
                        (let ((mapping
                               (list :index (plist-get prepared :index)
                                     :base base
                                     :size size
                                     :stub-base
                                     (plist-get prepared :stub-base)
                                     :text-size
                                     (plist-get prepared :text-size))))
                          (setq owned (cons mapping owned))
                          (nelisp--ptr-copy-string-bytes
                           base (plist-get prepared :text))))
                      (setq sections (cdr sections))))
                  (setq owned (nreverse owned))
                  (let ((sections (plist-get plan :sections)))
                    (while sections
                      (let* ((prepared (car sections))
                             (section (plist-get prepared :section))
                             (mapping
                              (nelisp-artifact--native-mapping-by-index
                               owned (plist-get prepared :index)))
                             (base (plist-get mapping :base))
                             (stub-base (plist-get mapping :stub-base))
                             (deps (plist-get prepared :dependencies))
                             (stub-index 0)
                             (stub-vector
                              (and
                               (nelisp-artifact--native-compact-relocs-p
                                section)
                               (make-vector (length deps) 0)))
                             (stubs nil))
                        (while deps
                          (let* ((name (car deps))
                                 (address
                                  (+ base stub-base (* stub-index 16)))
                                 (target
                                  (nelisp-artifact--native-symbol-target
                                   name
                                   (plist-get plan :symbol-index)
                                   (plist-get plan :runtime-targets)
                                   owned)))
                            (nelisp-artifact--native-write-jump-stub
                             address target)
                            (when stub-vector
                              (aset stub-vector stub-index address))
                            (unless stub-vector
                              (setq stubs (cons (cons name address) stubs))))
                          (setq stub-index (1+ stub-index))
                          (setq deps (cdr deps)))
                        (let ((cursor
                               (nelisp-artifact--native-reloc-cursor section)))
                          (while
                              (not
                               (nelisp-artifact--native-reloc-cursor-done-p
                                cursor))
                            (let* ((offset
                                    (nelisp-artifact--native-reloc-cursor-offset
                                     cursor))
                                   (symbol
                                    (nelisp-artifact--native-reloc-cursor-symbol
                                     cursor))
                                   (addend
                                    (nelisp-artifact--native-reloc-cursor-addend
                                     cursor))
                                   (stub
                                    (if stub-vector
                                        (let ((symbol-index
                                               (nelisp-artifact--native-reloc-cursor-symbol-index
                                                cursor)))
                                          (and
                                           (integerp symbol-index)
                                           (>= symbol-index 0)
                                           (< symbol-index
                                              (length stub-vector))
                                           (aref stub-vector symbol-index)))
                                      (cdr (assoc symbol stubs))))
                                   (displacement
                                    (and stub
                                         (- (+ stub addend)
                                            (+ base offset)))))
                              (unless stub
                                (error
                                 "standalone native linker missing local stub: %S"
                                 symbol))
                              (unless (and (>= displacement -2147483648)
                                           (<= displacement 2147483647))
                                (error
                                 "standalone native linker plt32 overflow: %S"
                                 displacement))
                              (ptr-write-u32 base offset displacement))
                            (nelisp-artifact--native-reloc-cursor-next
                             cursor))))
                      (setq sections (cdr sections))))
                  (let ((mappings owned))
                    (while mappings
                      (let* ((mapping (car mappings))
                             (rc
                              (syscall-direct
                               10
                               (plist-get mapping :base)
                               (plist-get mapping :size)
                               5 0 0 0)))
                        (unless (= rc 0)
                          (error
                           "standalone native linker mprotect RX failed: %S"
                           rc)))
                      (setq mappings (cdr mappings))))
                  (let ((linkset
                         (nelisp-artifact--native-commit-linkset
                          artifact plan owned)))
                    (setq committed t)
                    (nelisp-artifact--native-link-diagnostic
                     artifact 'ready
                     :duplicates (plist-get plan :duplicates))
                    linkset))
              (error
               (nelisp-artifact--native-link-diagnostic
                artifact 'unavailable :error err
                :duplicates
                (if plan
                    (plist-get plan :duplicates)
                  nelisp-artifact--native-last-preflight-duplicates))
               (signal (car err) (cdr err))))
          (unless committed
            (nelisp-artifact--native-unmap-owned owned)))))))

(defun nelisp-artifact--native-link-section
    (artifact-path section-index section)
  "Compatibility entry point linking SECTION through its artifact transaction."
  (unless (nelisp-artifact--registered-native-sections artifact-path)
    (nelisp-artifact--register-native-sections artifact-path (list section)))
  (nelisp-artifact--native-link-artifact artifact-path)
  (nelisp-artifact--native-runtime-mapping artifact-path section-index))

(defun nelisp-artifact--native-link-externless-section
    (artifact-path section-index section)
  "Compatibility alias for transactional native SECTION linking."
  (nelisp-artifact--native-link-section
   artifact-path section-index section))

(defun nelisp-artifact--native-body-address
  (artifact-path symbol meta)
  "Return linked native body address for SYMBOL and META."
  (let* ((artifact (expand-file-name artifact-path))
         (_linkset (nelisp-artifact--native-link-artifact artifact))
         (index (cdr (assoc artifact
                            nelisp-artifact--native-artifact-symbol-index)))
         (name (if (symbolp symbol) (symbol-name symbol) symbol))
         (entry (cdr (assoc name index)))
         (body-offset (plist-get meta :body-offset)))
    (unless entry
      (error "standalone native linker has no serialized defun for %S"
             symbol))
    (unless (integerp body-offset)
      (error "standalone native linker missing body offset for %S" symbol))
    (+ (plist-get entry :address) body-offset)))

(defun nelisp-artifact--native-call-in-process
    (artifact-path symbol meta args)
  "Execute SYMBOL from ARTIFACT-PATH through the boxed native boundary."
  (let* ((arity (plist-get meta :arity))
         (rt-slots (plist-get meta :rt-slot-count))
         (rest-p
          ;; The v5 atom decode may deliver the raw token string; treat
          ;; only a real t / "t" as rest-p so a serialized bare `nil'
          ;; ("nil" string, truthy) never enables folding on plain defuns.
          (let ((r (plist-get meta :rest-p)))
            (and r (not (equal r "nil")) t)))
         (fixed (or (plist-get meta :fixed-count)
                    (and rest-p (1- arity))))
         (args
          ;; Doc 166: a &rest callee's native body takes FIXED args plus
          ;; ONE trailing list; fold extra/exact tail args into that list
          ;; so the boundary never binds a bare element as the rest param.
          (if (and rest-p (integerp fixed) (>= (length args) fixed))
              (let ((head nil)
                    (tail args)
                    (i 0))
                (while (< i fixed)
                  (setq head (cons (car tail) head))
                  (setq tail (cdr tail))
                  (setq i (1+ i)))
                (let ((folded (cons tail nil)))
                  (while head
                    (setq folded (cons (car head) folded))
                    (setq head (cdr head)))
                  folded))
            args)))
    (unless (= arity (length args))
      (error "standalone native linker arity mismatch for %S" symbol))
    (unless (and (integerp rt-slots) (>= rt-slots 17))
      (error "standalone native linker invalid runtime slots for %S" symbol))
    (apply #'nelisp--native-call-boundary
           (append
            (list
             (nelisp-artifact--native-body-address
              artifact-path symbol meta)
             arity rt-slots)
            args))))

(defun nelisp-artifact--note-native-dispatch (entry)
  "Record one native dispatch report ENTRY."
  (setq nelisp-artifact-native-dispatch-report
        (cons entry nelisp-artifact-native-dispatch-report))
  entry)

(defun nelisp-artifact-native-dispatch-report ()
  "Return native dispatch report entries, newest last."
  (reverse nelisp-artifact-native-dispatch-report))

(defun nelisp-artifact--all-integers-p (args)
  "Return non-nil when every element of ARGS is an integer."
  (let ((rest args)
        (ok t))
    (while rest
      (unless (integerp (car rest))
        (setq ok nil))
      (setq rest (cdr rest)))
    ok))

(defun nelisp-artifact--native-simple-integer-abi-p (meta)
  "Return non-nil when META is worth trying through the direct integer ABI.
The AOT metadata may still record runtime frame slots for bookkeeping even
when the exported symbol itself is callable as a plain integer function.  The
validated CLI path already attempts this fast call first and falls back on
failure; normal native wrappers should use the same policy so hot integer
calls do not always pay the general trampoline cost."
  (or (null meta)
      (null (plist-get meta :param-class))
      (eq (plist-get meta :param-class) 'gp)
      (equal (plist-get meta :param-class) "gp")))

(defun nelisp-artifact--install-function (symbol function)
  "Install SYMBOL's NeLisp FUNCTION in both runtime tables.
`nelisp-eval' calls through `nelisp--functions', while the standalone top-level
reader/evaluator used by direct CLI source can also consult the ordinary
function cell.  Keep the function cell as a small bridge that looks up the
current hash value at call time, so later native-wrapper replacement is visible
without another `fset'."
  (puthash symbol function nelisp--functions)
  (when (fboundp 'nelisp--apply)
    (fset symbol
          `(lambda (&rest args)
             (nelisp--apply (gethash ',symbol nelisp--functions) args))))
  symbol)

(defconst nelisp-artifact--native-install-denylist
  '(nelisp-native-function-call
    nelisp-artifact--native-function-wrapper
    nelisp-artifact--native-function-artifact
    nelisp-artifact--native-function-symbol
    nelisp-artifact--native-function-fallback
    nelisp-artifact--native-function-meta
    nelisp-artifact--native-wrapper-p
    nelisp-artifact--native-call-in-process
    nelisp-artifact--native-body-address
    nelisp-artifact--native-defun-metadata
    nelisp-artifact--native-section-get
    nelisp-artifact--native-section-p
    nelisp-artifact--native-sections-from-native
    nelisp-artifact--native-section-for-symbol
    nelisp-artifact--registered-native-sections
    nelisp-artifact--register-native-sections
    nelisp-artifact--install-function
    nelisp-artifact--install-native-functions
    nelisp-artifact--note-native-dispatch
    nelisp-artifact--all-integers-p
    nelisp-artifact--native-simple-integer-abi-p
    nelisp-artifact--native-preflight-artifact
    nelisp-artifact--native-link-diagnostic
    nelisp-artifact--native-invalidate-artifact-runtime)
  "Symbols that must keep their bytecode definitions.
`nelisp-native-function-call' reaches into its own wrapper through these
functions, so wrapping any of them makes every later dispatch re-enter the
dispatcher.  Measured in the standalone: the install loop died flagless
(stack exhaustion, no condition object) the moment
`nelisp-artifact--native-function-artifact' became a wrapper.  Skipping them
costs nothing measurable -- they are called once per dispatch, not in the
hot body of any artifact command.")

(defun nelisp-artifact--install-native-functions (artifact-path native)
  "Install native wrappers from ARTIFACT-PATH and NATIVE metadata.
Each wrapper keeps the existing bytecode/interpreter fallback, so normal
NeLisp calls prefer native code when possible without losing semantic
coverage when a native executor rejects the call."
  (let ((sections (nelisp-artifact--native-sections-from-native native))
        (installed 0)
        (skipped 0)
        (preflight-error nil)
        (preflight nil)
        (registered
         (nelisp-artifact--registered-native-sections artifact-path)))
    ;; Standalone can prove resolver coverage before replacing any bytecode
    ;; function.  A real bootstrap artifact with even one unresolved extern
    ;; remains entirely on its bytecode definitions; no partial native install
    ;; is observable.
    (when (and registered (fboundp 'nelisp--native-call-boundary))
      (condition-case err
          (setq preflight
                (nelisp-artifact--native-preflight-artifact
                 artifact-path registered))
        (error
         (setq preflight-error err)
         (nelisp-artifact--native-link-diagnostic
          artifact-path 'unavailable :stage 'preflight :error err
          :duplicates nelisp-artifact--native-last-preflight-duplicates)
         (nelisp-artifact--note-native-dispatch
          (list :event 'link-preflight
                :artifact (expand-file-name artifact-path)
                :status 'unavailable
                :error err
                :duplicates
                nelisp-artifact--native-last-preflight-duplicates)))))
    (if preflight-error
        (setq skipped
              (let ((count 0)
                    (rest sections))
                (while rest
                  (setq count
                        (+ count
                           (length
                            (nelisp-artifact--native-section-get
                             (car rest) :symbols))))
                  (setq rest (cdr rest)))
                count))
      (dolist (section sections)
        (dolist
            (name
             (nelisp-artifact--native-section-get section :symbols))
          (let* ((sym (if (symbolp name) name (intern name)))
                 (current
                  (gethash sym nelisp--functions nelisp--unbound))
                 ;; Duplicate exports are last-wins, but their fallback must
                 ;; remain the original bytecode callable rather than the
                 ;; earlier native wrapper.
                 (fallback
                  (if (nelisp-artifact--native-wrapper-p current)
                      (nelisp-artifact--native-function-fallback current)
                    current))
                 (meta
                  (nelisp-artifact--native-defun-metadata section sym)))
            (if (or (eq fallback nelisp--unbound) (null meta)
                    (memq sym nelisp-artifact--native-install-denylist))
                (setq skipped (1+ skipped))
              (nelisp-artifact--install-function
               sym
               (nelisp-artifact--native-function-wrapper
                artifact-path sym fallback meta))
              (setq installed (1+ installed)))))))
    (nelisp-artifact--note-native-dispatch
     (list :event 'install
           :artifact (expand-file-name artifact-path)
           :installed installed
           :skipped skipped
           :native-ready (not preflight-error)
           :duplicates (and preflight
                            (plist-get preflight :duplicates))))
    installed))

(defun nelisp-native-function-call (fn args)
  "Call native wrapper FN with ARGS, falling back when native cannot run."
  (let ((artifact (nelisp-artifact--native-function-artifact fn))
        (symbol (symbol-name (nelisp-artifact--native-function-symbol fn)))
        (fallback (nelisp-artifact--native-function-fallback fn))
        (meta (nelisp-artifact--native-function-meta fn)))
    (if (not nelisp-artifact-native-dispatch-enabled)
        (nelisp--apply fallback args)
      (condition-case native-err
          (let ((result
                 (if (fboundp 'nelisp--native-call-boundary)
                     (nelisp-artifact--native-call-in-process
                      artifact symbol meta args)
                   (if (and (nelisp-artifact--all-integers-p args)
                            (nelisp-artifact--native-simple-integer-abi-p meta))
                       (condition-case _fast-err
                           (nelisp-artifact-native-exec-fast-simple
                            artifact symbol args)
                         (error
                          (nelisp-artifact-native-exec-general
                           artifact symbol args)))
                     (nelisp-artifact-native-exec-general
                      artifact symbol args)))))
            (nelisp-artifact--note-native-dispatch
             (list :event 'call
                   :symbol (intern symbol)
                   :mode 'native
                   :argc (length args)))
            result)
        (error
         (nelisp-artifact--note-native-dispatch
          (list :event 'call
                :symbol (intern symbol)
                :mode 'fallback
                :argc (length args)
                :reason (error-message-string native-err)))
         (nelisp--apply fallback args))))))

(defun nelisp-artifact--native-defun-forms (forms)
  "Return normalized top-level `defun' forms in FORMS."
  (let ((defuns nil))
    (dolist (form forms (nreverse defuns))
      (let ((normalized (nelisp-artifact--normalize-top-level-defun-form form)))
        (when normalized
          (push normalized defuns))))))

(defun nelisp-artifact--native-last-defun-forms (defuns)
  "Return only each symbol's source-order final definition from DEFUNS.
The returned order follows the final definitions' positions in the source.
Native coverage reports therefore also contain one entry per final binding."
  (let ((rest (reverse defuns))
        (seen nil)
        (winners nil))
    (while rest
      (let* ((defun (car rest))
             (name (nth 1 defun)))
        (unless (memq name seen)
          (setq seen (cons name seen))
          (setq winners (cons defun winners))))
      (setq rest (cdr rest)))
    winners))

(defun nelisp-artifact--normalize-native-defun-budget (budget)
  "Return a positive integer native defun BUDGET."
  (if (and (integerp budget) (> budget 0))
      budget
    (error "unsupported native defun budget: %S" budget)))

(defun nelisp-artifact--native-unsupported-report (forms reason)
  "Return a native compile report for FORMS with shared failure REASON."
  (mapcar (lambda (form)
            (list :name (symbol-name (nth 1 form))
                  :native nil
                  :reason reason))
          (nelisp-artifact--native-last-defun-forms
           (nelisp-artifact--native-defun-forms forms))))

(defun nelisp-artifact--normalize-native-policy (policy)
  "Return normalized native POLICY."
  (cond
   ((or (null policy) (eq policy 'opportunistic)
        (equal policy "opportunistic"))
    'opportunistic)
   ((or (eq policy 'required) (eq policy 'all-defuns)
        (equal policy "required") (equal policy "all-defuns"))
    'required)
   (t (error "unsupported native policy: %S" policy))))

(defun nelisp-artifact--normalize-module-policy (policy)
  "Return normalized module compile POLICY."
  (cond
   ((or (null policy) (eq policy 'bytecode) (equal policy "bytecode"))
    'bytecode)
   ((or (eq policy 'eval-only) (equal policy "eval-only")
        (eq policy 'source-replay) (equal policy "source-replay"))
    'eval-only)
   (t (error "unsupported module policy: %S" policy))))

(defun nelisp-artifact--native-report-failures (report)
  "Return REPORT entries whose `:native' value is nil."
  (let ((rest report)
        (out nil))
    (while rest
      (let ((entry (car rest)))
        (unless (plist-get entry :native)
          (setq out (append out (list entry)))))
      (setq rest (cdr rest)))
    out))

(defun nelisp-artifact--native-failures-message (failures)
  "Return a compact message for native coverage FAILURES."
  (let ((rest failures)
        (out ""))
    (while rest
      (let* ((entry (car rest))
             (name (or (plist-get entry :name) "<unknown>"))
             (reason (or (plist-get entry :reason) "not native"))
             (part (concat name " (" reason ")")))
        (setq out (if (> (length out) 0)
                      (concat out ", " part)
                    part)))
      (setq rest (cdr rest)))
    out))

(defun nelisp-artifact--enforce-native-policy (source-path kind native-policy native-report)
  "Enforce NATIVE-POLICY for SOURCE-PATH/KIND using NATIVE-REPORT."
  (let ((policy (nelisp-artifact--normalize-native-policy native-policy)))
    (when (and (eq kind 'neln) (eq policy 'required))
      (let ((failures (nelisp-artifact--native-report-failures native-report)))
        (when failures
          (signal
           'error
           (list
            (format "native policy required failed for %s: %s"
                    source-path
                    (nelisp-artifact--native-failures-message failures)))))))))

(defun nelisp-artifact--native-compiler-candidates ()
  "Return possible source paths for `nelisp-aot-compiler'."
  (let ((roots nil)
        (env-root (and (fboundp 'getenv) (getenv "NELISP_ROOT"))))
    (when (and (boundp 'nelisp-artifact-standalone-repo-root)
               nelisp-artifact-standalone-repo-root
               (stringp nelisp-artifact-standalone-repo-root)
               (> (length nelisp-artifact-standalone-repo-root) 0))
      (setq roots (append roots (list nelisp-artifact-standalone-repo-root))))
    (when (and env-root (> (length env-root) 0))
      (setq roots (append roots (list env-root))))
    (when (and (boundp 'default-directory) default-directory)
      (setq roots (append roots (list default-directory))))
    (mapcar (lambda (root)
              (expand-file-name "lisp/nelisp-aot-compiler.el" root))
            roots)))

(defun nelisp-artifact--load-native-compiler-from-path (path)
  "Load the native compiler dependency chain from compiler PATH."
  (let* ((lisp-dir (file-name-directory path))
         (load-path (cons lisp-dir load-path))
         (deps '("nelisp-asm-arm64.el"
                 "nelisp-asm-wasm.el"
                 "nelisp-asm-x86_64.el"
                 "nelisp-cc-runtime.el"
                 "nelisp-elf-write.el"
                 "nelisp-sexp-layout.el"
                 "nelisp-wasm-write.el"
                 "nelisp-aot-compiler.el")))
    (dolist (dep deps)
      (let ((dep-path (expand-file-name dep lisp-dir)))
        (when (file-exists-p dep-path)
          (load dep-path nil t))))
    (and (fboundp 'nelisp-aot-compile-to-object)
         (fboundp 'nelisp-aot-compile-to-link-unit))))

(defun nelisp-artifact--ensure-native-compiler ()
  "Ensure the native AOT compiler entry points are loaded."
  (unless (and (fboundp 'nelisp-aot-compile-to-object)
               (fboundp 'nelisp-aot-compile-to-link-unit))
    (condition-case nil
        (require 'nelisp-aot-compiler)
      (error nil)))
  (unless (and (fboundp 'nelisp-aot-compile-to-object)
               (fboundp 'nelisp-aot-compile-to-link-unit))
    (let ((candidates (nelisp-artifact--native-compiler-candidates))
          (loaded nil))
      (while (and candidates (not loaded))
        (let* ((path (car candidates))
               (dir (file-name-directory path)))
          (when (and (file-exists-p path) dir)
            (condition-case nil
                (setq loaded
                      (nelisp-artifact--load-native-compiler-from-path path))
              (error nil))))
        (setq candidates (cdr candidates)))))
  (and (fboundp 'nelisp-aot-compile-to-object)
       (fboundp 'nelisp-aot-compile-to-link-unit)))

(defun nelisp-artifact--compact-runtime-relocs (unit arch)
  "Return indexed compact runtime relocation metadata for UNIT and ARCH.
Only the exact relocation subset accepted by the in-process runtime linker is
eligible: x86_64 `plt32' patches in text whose symbols occur exactly once in
UNIT's distinct `:extern-symbols' table.  Return nil when legacy relocation
metadata must be retained."
  (let ((externs (plist-get unit :extern-symbols))
        (relocs (plist-get unit :relocs))
        (indexes nil)
        (data nil)
        (index 0)
        (eligible (eq arch 'x86_64)))
    (when eligible
      (dolist (name externs)
        (if (or (not (stringp name)) (assoc name indexes))
            (setq eligible nil)
          (setq indexes (cons (cons name index) indexes))
          (setq index (1+ index)))))
    (when eligible
      (dolist (reloc relocs)
        (let* ((section (plist-get reloc :section))
               (offset (plist-get reloc :offset))
               (type (plist-get reloc :type))
               (symbol (plist-get reloc :symbol))
               (addend (or (plist-get reloc :addend) 0))
               (indexed (and (stringp symbol) (assoc symbol indexes))))
          (if (and (memq section '(nil text))
                   (eq type 'plt32)
                   (integerp offset)
                   (integerp addend)
                   indexed)
              (setq data
                    (cons addend
                          (cons (cdr indexed)
                                (cons offset data))))
            (setq eligible nil)))))
    (and eligible
         (list :reloc-format nelisp-artifact--compact-reloc-format
               :reloc-count (length relocs)
               :reloc-data (nreverse data)))))

(defun nelisp-artifact--native-section-relocs (section)
  "Return SECTION relocations in the legacy public plist shape.
This compatibility accessor may expand compact metadata for host-side
inspection and build-time demos.  Runtime linking uses the flat relocation
cursor directly and must not call this function."
  (if (and (listp section) (plist-member section :relocs))
      (plist-get section :relocs)
    (let ((format
           (nelisp-artifact--native-section-get section :reloc-format))
          (count
           (nelisp-artifact--native-section-get section :reloc-count))
          (data
           (nelisp-artifact--native-section-get section :reloc-data))
          (externs
           (nelisp-artifact--native-section-get section :extern-symbols))
          (out nil))
      (unless (and (eq format nelisp-artifact--compact-reloc-format)
                   (integerp count) (>= count 0)
                   (= (length data) (* count 3)))
        (error "invalid compact native relocation metadata"))
      (dotimes (i count)
        (let* ((base (* i 3))
               (symbol-index (elt data (1+ base)))
               (symbol (and (integerp symbol-index)
                            (>= symbol-index 0)
                            (nth symbol-index externs))))
          (unless (stringp symbol)
            (error "compact native relocation symbol index out of range: %S"
                   symbol-index))
          (setq out
                (cons
                 (list :offset (elt data base)
                       :type 'plt32
                       :symbol symbol
                       :addend (elt data (+ base 2)))
                 out))))
      (nreverse out))))

(defun nelisp-artifact--native-section-finalize-char-size (section)
  "Set SECTION's `:serialized-char-size' to its fixed-point printed length.
The field's own decimal width contributes to the final character count."
  (let ((cell (memq :serialized-char-size section))
        (size -1)
        (actual 0))
    (unless (and cell (cdr cell))
      (error "native section lacks :serialized-char-size"))
    (while (/= size actual)
      (setq size actual)
      (setcar (cdr cell) size)
      (setq actual (length (prin1-to-string section))))
    section))

(defun nelisp-artifact--native-runtime-prefix-finalize-char-size (prefix)
  "Set PREFIX's self-referential printed character size to a fixed point."
  (let ((cell (and (listp prefix)
                   (memq :runtime-prefix-char-size prefix)))
        (vector-layout-p
         (and (vectorp prefix)
              (= (length prefix) 10)
              (= (aref prefix 0)
                 nelisp-artifact--native-runtime-prefix-layout-version)))
        (size -1)
        (actual 0))
    (unless (or (and cell (cdr cell)) vector-layout-p)
      (error "native runtime prefix lacks a supported size slot"))
    (while (/= size actual)
      (setq size actual)
      (if vector-layout-p
          (aset prefix 1 size)
        (setcar (cdr cell) size))
      (setq actual (length (prin1-to-string prefix))))
    prefix))

(defun nelisp-artifact--native-section-wire-v5 (section)
  "Return SECTION in version 5 self-sized wire form.
Compiler and manifest code keep the flat section metadata internally.  The
wire form nests the load-critical fields in one independently closed plist so
the standalone native batch reader can stop at its closing parenthesis without
copying the object/diagnostic tail."
  (if (/= (or (nelisp-artifact--native-section-get
               section :native-section-version)
              0)
          nelisp-artifact--native-section-version)
      section
    (let* ((runtime
            (nelisp-artifact--native-runtime-prefix-finalize-char-size
             (vector
              nelisp-artifact--native-runtime-prefix-layout-version
              0
              (nelisp-artifact--native-section-get section :arch)
              (nelisp-artifact--native-section-get section :symbols)
              (nelisp-artifact--native-section-get section :text-base64)
              (nelisp-artifact--native-section-get section :reloc-format)
              (nelisp-artifact--native-section-get section :reloc-count)
              (nelisp-artifact--native-section-get section :reloc-data)
              (nelisp-artifact--native-section-get
               section :extern-symbols)
              (mapcar
               #'nelisp-artifact--native-defun-entry
               (nelisp-artifact--native-section-get section :defuns)))))
           (wire
            (list :native-section-version
                  nelisp-artifact--native-section-version
                  :serialized-char-size 0
                  :runtime-prefix runtime
                  :object-format (plist-get section :object-format)
                  :object-size (plist-get section :object-size)
                  :object-sha256 (plist-get section :object-sha256)
                  :object-base64 (plist-get section :object-base64)
                  :text-size (plist-get section :text-size)
                  :compile-report (plist-get section :compile-report))))
      (nelisp-artifact--native-section-finalize-char-size wire))))

(defun nelisp-artifact--native-section-flatten-v5 (section)
  "Return parsed v5 wire SECTION with runtime fields flattened."
  (if (/= (or (nelisp-artifact--native-section-get
               section :native-section-version)
              0)
          nelisp-artifact--native-section-version)
      section
    (let ((runtime (plist-get section :runtime-prefix)))
      (unless (or
               (and (listp runtime)
                    (plist-member runtime :runtime-prefix-char-size))
               (and (vectorp runtime) (= (length runtime) 10)
                    (= (aref runtime 0)
                       nelisp-artifact--native-runtime-prefix-layout-version)))
        (error "version 5 native section lacks supported :runtime-prefix"))
      (append
       (list :native-section-version nelisp-artifact--native-section-version
             :serialized-char-size
             (nelisp-artifact--native-section-get
              section :serialized-char-size))
       (if (vectorp runtime)
           (list :runtime-prefix-char-size (aref runtime 1)
                 :arch (aref runtime 2)
                 :symbols (aref runtime 3)
                 :text-base64 (aref runtime 4)
                 :reloc-format (aref runtime 5)
                 :reloc-count (aref runtime 6)
                 :reloc-data (aref runtime 7)
                 :extern-symbols (aref runtime 8)
                 :defuns (aref runtime 9)
                 :runtime-end t)
         runtime)
       (list :object-format (plist-get section :object-format)
             :object-size (plist-get section :object-size)
             :object-sha256 (plist-get section :object-sha256)
             :object-base64 (plist-get section :object-base64)
             :text-size (plist-get section :text-size)
             :compile-report (plist-get section :compile-report))))))

(defun nelisp-artifact--native-value-flatten-v5 (native)
  "Flatten version 5 wire sections contained in NATIVE."
  (cond
   ((null native) nil)
   ((and (listp native)
         (= (or (plist-get native :native-section-version) 0)
            nelisp-artifact--native-section-version))
    (nelisp-artifact--native-section-flatten-v5 native))
   ((and (consp native) (consp (car native)))
    (mapcar #'nelisp-artifact--native-section-flatten-v5 native))
   (t native)))

(defun nelisp-artifact--native-section-plist (_obj unit arch symbols compile-report)
  "Return the serialized native section plist for UNIT.
The temp OBJ file remains available to the caller as a write/error gate,
but the embedded artifact bytes come from the in-memory link unit."
  (let* ((text-bytes (plist-get unit :text))
         (bytes (nelisp-artifact--native-object-bytes unit))
         (compact (nelisp-artifact--compact-runtime-relocs unit arch))
         (defuns (mapcar #'nelisp-artifact--native-defun-entry
                         (plist-get unit :defuns))))
    (if compact
        (append
         (list :native-section-version nelisp-artifact--native-section-version
               :arch (symbol-name arch)
               :symbols symbols
               :text-base64 (base64-encode-string text-bytes t))
         compact
         (list :extern-symbols (plist-get unit :extern-symbols)
               :defuns defuns
               :runtime-end t
               :object-format nelisp-artifact--native-object-format
               :object-size (nelisp-artifact--byte-length bytes)
               :object-sha256 (secure-hash 'sha256 bytes)
               :object-base64 (base64-encode-string bytes t)
               :text-size (nelisp-artifact--byte-length text-bytes)
               :compile-report compile-report))
      (list :native-section-version 2
            :object-format nelisp-artifact--native-object-format
            :arch (symbol-name arch)
            :symbols symbols
            :object-size (nelisp-artifact--byte-length bytes)
            :object-sha256 (secure-hash 'sha256 bytes)
            :object-base64 (base64-encode-string bytes t)
            :text-size (nelisp-artifact--byte-length text-bytes)
            :text-base64 (base64-encode-string text-bytes t)
            :relocs (plist-get unit :relocs)
            :extern-symbols (plist-get unit :extern-symbols)
            :compile-report compile-report
            :defuns defuns))))

(defun nelisp-artifact--native-compile-required-section (defuns arch)
  "Compile all DEFUNS for required native policy in one batch."
  (if (null defuns)
      (progn
        (setq nelisp-artifact--last-native-compile-report nil)
        nil)
    (let* ((eligible defuns)
         (symbols (mapcar (lambda (d) (symbol-name (nth 1 d))) eligible))
         (compile-report (mapcar (lambda (name)
                                   (list :name name :native t))
                                 symbols))
         (obj (nelisp-artifact--make-temp-path "neln-obj" "o"))
         (stage-start nil))
      (unwind-protect
          (condition-case err
              (let (unit native-section)
                (setq stage-start (nelisp-artifact--profile-time))
                (let ((nelisp-aot-compiler--external-native-symbols
                       symbols)
                      (nelisp-aot-compiler--user-el-aot-context t))
                  (setq unit
                        (nelisp-aot-compile-to-link-unit
                         (cons 'seq eligible)
                         :arch arch :format 'elf)))
                (let ((unsupported
                       (nelisp-artifact--native-unsupported-section-externs
                        (list :extern-symbols
                              (plist-get unit :extern-symbols))
                        symbols)))
                  (when unsupported
                    (error
                     "unsupported-runtime-externs: %s"
                     (mapconcat #'identity unsupported ", "))))
                (nelisp-artifact--profile-log
                 "native-required-compile"
                 stage-start
                 (list :defuns (length eligible)
                       :arch arch))
                (setq stage-start (nelisp-artifact--profile-time))
                (nelisp-artifact--write-elf-rel-object obj unit)
                (nelisp-artifact--profile-log
                 "native-required-write-object"
                 stage-start
                 (list :object obj))
                (setq nelisp-artifact--last-native-compile-report
                      compile-report)
                (setq stage-start (nelisp-artifact--profile-time))
                (setq native-section
                      (nelisp-artifact--native-section-plist
                       obj unit arch symbols compile-report))
                (nelisp-artifact--profile-log
                 "native-required-section-plist"
                 stage-start
                 (list :symbols (length symbols)
                       :object-size (plist-get native-section :object-size)
                       :text-size (plist-get native-section :text-size)))
                native-section)
            (error
             (let ((failure-report
                    (mapcar (lambda (name)
                              (list :name name
                                    :native nil
                                    :reason (error-message-string err)))
                            symbols)))
               (setq nelisp-artifact--last-native-compile-report
                     failure-report)
               nil)))
        (nelisp-artifact--delete-if-exists obj)))))

(defun nelisp-artifact--native-compile-fast-batch-section
    (defuns arch &optional report-tail)
  "Compile all DEFUNS in one opportunistic batch, or return nil on failure.
Unlike `required' policy, a failure here is not final: callers fall back to
per-defun probes so mixed native/fallback modules keep their coverage report.
The fast path avoids compiling every supported defun twice when the whole file
is already native-compatible."
  (when defuns
          (let* ((symbols (mapcar (lambda (d) (symbol-name (nth 1 d))) defuns))
           (compile-report (mapcar (lambda (name)
                                     (list :name name :native t))
                                   symbols))
           (obj (nelisp-artifact--make-temp-path "neln-obj" "o"))
           (native nil))
      (unwind-protect
          (condition-case nil
              (let ((unit (nelisp-aot-compile-to-link-unit
                           (cons 'seq defuns)
                           :arch arch :format 'elf)))
                (nelisp-artifact--write-elf-rel-object obj unit)
                (setq nelisp-artifact--last-native-compile-report
                      (append compile-report report-tail))
                (setq native
                      (nelisp-artifact--native-section-plist
                       obj unit arch symbols
                       (append compile-report report-tail))))
            (error
             (setq native nil)))
      (nelisp-artifact--delete-if-exists obj))
      native)))

(defun nelisp-artifact--native-section-serialized-byte-size (section)
  "Return SECTION's stable serialized byte size for artifact storage.
This uses the printed plist form written into `.neln' payloads so the budget
matches standalone replay's actual reader input."
  (nelisp-artifact--byte-length
   (prin1-to-string
    (nelisp-artifact--native-section-wire-v5 section))))

(defun nelisp-artifact--native-section-within-byte-budget-p (section)
  "Return non-nil when SECTION fits the opportunistic serialized byte budget."
  (<= (nelisp-artifact--native-section-serialized-byte-size section)
      nelisp-artifact-default-native-section-byte-budget))

(defun nelisp-artifact--native-compile-shard-chunk-result (chunk arch)
  "Compile CHUNK opportunistically, recursively bisecting batch failures."
  (cond
   ((null chunk)
    (list :sections nil :report nil))
   ((= (length chunk) 1)
    (let* ((result (nelisp-artifact--native-compile-single-defun-result
                    (car chunk) arch))
           (section (car (plist-get result :sections))))
      (if (or (null section)
              (nelisp-artifact--native-section-within-byte-budget-p section))
          result
        (let ((symbol (symbol-name (nth 1 (car chunk))))
              (size (nelisp-artifact--native-section-serialized-byte-size
                     section)))
          (list :sections nil
                :report
                (list
                 (list :name symbol
                       :native nil
                       :reason
                       (format
                        "serialized native section exceeds standalone replay byte budget (%d bytes > %d bytes)"
                        size
                        nelisp-artifact-default-native-section-byte-budget))))))))
   (t
    (let ((batch (nelisp-artifact--native-compile-fast-batch-section
                  chunk arch)))
      (if (and batch
               (nelisp-artifact--native-section-within-byte-budget-p batch))
          (list :sections (list batch)
                :report (plist-get batch :compile-report))
        (let* ((split (/ (length chunk) 2))
               (left nil)
               (right nil)
               (index 0))
          (dolist (defun chunk)
            (if (< index split)
                (push defun left)
              (push defun right))
            (setq index (1+ index)))
          (let* ((left-result
                  (nelisp-artifact--native-compile-shard-chunk-result
                   (nreverse left) arch))
                 (right-result
                  (nelisp-artifact--native-compile-shard-chunk-result
                   (nreverse right) arch)))
            (list :sections (append (plist-get left-result :sections)
                                    (plist-get right-result :sections))
                  :report (append (plist-get left-result :report)
                                  (plist-get right-result :report))))))))))

(defun nelisp-artifact--chunk-list (items size)
  "Split ITEMS into a list of chunks of at most SIZE elements."
  (let ((size (max 1 size))
        (chunks nil)
        (chunk nil)
        (count 0))
    (dolist (item items)
      (push item chunk)
      (setq count (1+ count))
      (when (>= count size)
        (push (nreverse chunk) chunks)
        (setq chunk nil
              count 0)))
    (when chunk
      (push (nreverse chunk) chunks))
    (nreverse chunks)))

(defun nelisp-artifact--native-compile-single-defun-result (defun arch)
  "Compile one DEFUN to a native section result plist."
  (let* ((symbol (symbol-name (nth 1 defun)))
         (obj (nelisp-artifact--make-temp-path "neln-obj" "o")))
    (unwind-protect
        (condition-case err
            (let* ((unit (nelisp-aot-compile-to-link-unit
                          (cons 'seq (list defun))
                          :arch arch :format 'elf))
                   (compile-report (list (list :name symbol :native t)))
                   (section nil))
              (nelisp-artifact--write-elf-rel-object obj unit)
              (setq section
                    (nelisp-artifact--native-section-plist
                     obj unit arch (list symbol) compile-report))
              (list :sections (list section)
                    :report compile-report))
          (error
           (list :sections nil
                 :report (list (list :name symbol
                                     :native nil
                                     :reason (error-message-string err))))))
      (nelisp-artifact--delete-if-exists obj))))

(defun nelisp-artifact--native-compile-shard-result (defuns arch)
  "Compile DEFUNS to one or more native sections, preserving report order."
  (let ((budget (nelisp-artifact--normalize-native-defun-budget
                 nelisp-artifact-default-native-defun-budget))
        (sections nil)
        (report nil))
    (dolist (chunk (nelisp-artifact--chunk-list defuns budget))
      (let ((result (nelisp-artifact--native-compile-shard-chunk-result
                     chunk arch)))
        (setq sections (append sections (plist-get result :sections))
              report (append report (plist-get result :report)))))
    (list :sections sections :report report)))

(defun nelisp-artifact--native-unsupported-section-externs
    (section native-candidates)
  "Return unsupported externs in SECTION for NATIVE-CANDIDATES."
  (let ((rest
         (nelisp-artifact--native-section-get section :extern-symbols))
        (unsupported nil))
    (while rest
      (let ((name (car rest)))
        (unless (or (member name native-candidates)
                    (member name
                            nelisp-artifact--supported-runtime-externs))
          (setq unsupported (cons name unsupported))))
      (setq rest (cdr rest)))
    (nelisp-artifact--native-unique-names (nreverse unsupported))))

(defun nelisp-artifact--native-unsupported-extern-report
    (defun unsupported)
  "Return the fallback report for DEFUN and UNSUPPORTED extern names."
  (list
   (list :name (symbol-name (nth 1 defun))
         :native nil
         :reason
         (concat "unsupported-runtime-externs: "
                 (mapconcat #'identity unsupported ", ")))))

(defun nelisp-artifact--native-compile-candidate-chunk-result
    (chunk arch native-candidates)
  "Compile candidate CHUNK, bisecting failures and unsupported externs."
  (cond
   ((null chunk)
    (list :sections nil :report nil))
   ((= (length chunk) 1)
    (let* ((nelisp-aot-compiler--external-native-symbols
            native-candidates)
           (nelisp-aot-compiler--user-el-aot-context t)
           (result
            (nelisp-artifact--native-compile-single-defun-result
             (car chunk) arch))
           (section (car (plist-get result :sections)))
           (unsupported
            (and section
                 (nelisp-artifact--native-unsupported-section-externs
                  section native-candidates))))
      (cond
       (unsupported
        (list
         :sections nil
         :report
         (nelisp-artifact--native-unsupported-extern-report
          (car chunk) unsupported)))
       ((or (null section)
            (nelisp-artifact--native-section-within-byte-budget-p section))
        result)
       (t
        (let ((symbol (symbol-name (nth 1 (car chunk))))
              (size
               (nelisp-artifact--native-section-serialized-byte-size
                section)))
          (list
           :sections nil
           :report
           (list
            (list
             :name symbol
             :native nil
             :reason
             (format
              "serialized native section exceeds standalone replay byte budget (%d bytes > %d bytes)"
              size
              nelisp-artifact-default-native-section-byte-budget)))))))))
   (t
    (let* ((nelisp-aot-compiler--external-native-symbols
            native-candidates)
           (nelisp-aot-compiler--user-el-aot-context t)
           (batch
            (nelisp-artifact--native-compile-fast-batch-section
             chunk arch))
           (unsupported
            (and batch
                 (nelisp-artifact--native-unsupported-section-externs
                  batch native-candidates))))
      (if (and batch
               (null unsupported)
               (nelisp-artifact--native-section-within-byte-budget-p batch))
          (list :sections (list batch)
                :report (plist-get batch :compile-report))
        (let* ((split (/ (length chunk) 2))
               (left nil)
               (right nil)
               (index 0))
          (dolist (defun chunk)
            (if (< index split)
                (push defun left)
              (push defun right))
            (setq index (1+ index)))
          (let* ((left-result
                  (nelisp-artifact--native-compile-candidate-chunk-result
                   (nreverse left) arch native-candidates))
                 (right-result
                  (nelisp-artifact--native-compile-candidate-chunk-result
                   (nreverse right) arch native-candidates)))
            (list
             :sections
             (append (plist-get left-result :sections)
                     (plist-get right-result :sections))
             :report
             (append (plist-get left-result :report)
                     (plist-get right-result :report))))))))))

(defun nelisp-artifact--native-compile-candidate-pass
    (defuns arch native-candidates)
  "Compile one monotonic artifact pass for NATIVE-CANDIDATES."
  (let ((budget
         (nelisp-artifact--normalize-native-defun-budget
          nelisp-artifact-default-native-defun-budget))
        (sections nil)
        (report nil))
    (dolist (chunk (nelisp-artifact--chunk-list defuns budget))
      (let ((result
             (nelisp-artifact--native-compile-candidate-chunk-result
              chunk arch native-candidates)))
        (setq sections
              (append sections (plist-get result :sections)))
        (setq report
              (append report (plist-get result :report)))))
    (list :sections sections :report report)))

(defun nelisp-artifact--native-report-entry (name report)
  "Return NAME's last entry in REPORT."
  (let ((rest report)
        (found nil))
    (while rest
      (when (equal (plist-get (car rest) :name) name)
        (setq found (car rest)))
      (setq rest (cdr rest)))
    found))

(defun nelisp-artifact--native-final-externs-valid-p
    (sections final-native-symbols)
  "Return non-nil when SECTIONS only reference final native/runtime symbols."
  (let ((rest sections)
        (valid t))
    (while rest
      (when
          (nelisp-artifact--native-unsupported-section-externs
           (car rest) final-native-symbols)
        (setq valid nil))
      (setq rest (cdr rest)))
    valid))

(defun nelisp-artifact--native-compile-fixed-point-result (defuns arch)
  "Compile DEFUNS until the artifact-wide native allowlist reaches a fixed point.
The candidate set only shrinks.  Removed definitions stay on bytecode, while
remaining callers are recompiled so calls to removed definitions use the
generic boxed builtin boundary."
  (let* ((all-names
          (mapcar (lambda (defun)
                    (symbol-name (nth 1 defun)))
                  defuns))
         (ordered-names
          (nelisp-artifact--native-unique-names all-names))
         (candidates ordered-names)
         (failures nil)
         (final-pass nil)
         (iterations 0)
         ;; Strict monotonic reduction can happen at most N times, followed
         ;; by one equality pass.
         (iteration-limit (1+ (length ordered-names)))
         (stable nil))
    (while (and (not stable) (< iterations iteration-limit))
      (setq iterations (1+ iterations))
      (let ((selected nil))
        (dolist (defun defuns)
          (when (member (symbol-name (nth 1 defun)) candidates)
            (setq selected (append selected (list defun)))))
        (let* ((pass
                (nelisp-artifact--native-compile-candidate-pass
                 selected arch candidates))
               (sections (plist-get pass :sections))
               (report (plist-get pass :report))
               (actual-set nil)
               (actual nil))
          (dolist (section sections)
            (setq actual-set
                  (append
                   actual-set
                   (nelisp-artifact--native-section-get section :symbols))))
          (setq actual-set
                (nelisp-artifact--native-unique-names actual-set))
          (dolist (name ordered-names)
            (when (member name actual-set)
              (setq actual (append actual (list name)))))
          (dolist (name actual)
            (unless (member name candidates)
              (error
               "native fixed-point became non-monotonic: %S reappeared"
               name)))
          (dolist (name candidates)
            (unless (member name actual)
              (let ((entry
                     (nelisp-artifact--native-report-entry name report)))
                (setq failures
                      (nelisp-artifact--native-alist-set
                       name
                       (or entry
                           (list :name name :native nil
                                 :reason
                                 "removed by native fixed-point"))
                       failures)))))
          (setq final-pass pass)
          (if (equal actual candidates)
              (setq stable t)
            (setq candidates actual)))))
    (unless stable
      (error "native fixed-point did not converge after %d iterations"
             iteration-limit))
    (unless
        (nelisp-artifact--native-final-externs-valid-p
         (plist-get final-pass :sections) candidates)
      (error "native fixed-point emitted unresolved final externs"))
    (let ((final-report nil))
      (dolist (name all-names)
        (setq final-report
              (append
               final-report
               (list
                (if (member name candidates)
                    (or
                     (nelisp-artifact--native-report-entry
                      name (plist-get final-pass :report))
                     (list :name name :native t))
                  (or
                   (cdr (assoc name failures))
                   (list :name name :native nil
                         :reason "removed by native fixed-point")))))))
      (list :sections (plist-get final-pass :sections)
            :report final-report
            :native-symbols candidates
            :iterations iterations))))

(defun nelisp-artifact--native-compile-section (forms target &optional native-policy)
  "Compile native-eligible top-level `defun's in FORMS to one ET_REL object.
Returns a `:native' section plist (Doc 142 §6.4) or nil when nothing is
eligible.  Opportunistic mode first tries one all-defun batch compile for the
hot path where every defun is already native-compatible; on failure it falls
back to per-defun probes so one unsupported body does not sink the whole
module."
  (let ((arch (nelisp-artifact--target-arch target))
        (policy (nelisp-artifact--normalize-native-policy native-policy))
        (stage-start nil)
        (compiler-ready nil)
        (defuns nil))
    (setq nelisp-artifact--last-native-compile-report nil)
    (cond
     ((not arch)
      (setq nelisp-artifact--last-native-compile-report
            (nelisp-artifact--native-unsupported-report
             forms (format "unsupported native target: %S" target)))
      nil)
     ((progn
        (setq stage-start (nelisp-artifact--profile-time))
        (setq compiler-ready (nelisp-artifact--ensure-native-compiler))
        (nelisp-artifact--profile-log
         "native-ensure-compiler" stage-start
         (list :ready compiler-ready))
        (not compiler-ready))
      (setq nelisp-artifact--last-native-compile-report
            (nelisp-artifact--native-unsupported-report
             forms "native compiler unavailable"))
      nil)
     (t
     (setq stage-start (nelisp-artifact--profile-time))
     (setq defuns
           (nelisp-artifact--native-last-defun-forms
            (nelisp-artifact--native-defun-forms forms)))
     (nelisp-artifact--profile-log
       "native-defun-forms" stage-start
       (list :forms (length forms) :defuns (length defuns)))
      (if (eq policy 'required)
          (nelisp-artifact--native-compile-required-section defuns arch)
        (let* ((compile-result
                (nelisp-artifact--native-compile-fixed-point-result
                 defuns arch))
               (sections (plist-get compile-result :sections))
               (compile-report (plist-get compile-result :report)))
          (setq nelisp-artifact--last-native-compile-report compile-report)
          (cond
           ((null sections) nil)
           ((= (length sections) 1) (car sections))
           (t sections))))))))

(defun nelisp-artifact--artifact-payload (source-path module features
                                                      top-level-count kind native
                                                      native-report module-policy)
  "Build the serialized artifact payload for SOURCE-PATH.
KIND is `nelc' or `neln'; NATIVE is the §6.4 native section (or nil)."
  (append
   (list :format nelisp-artifact--format
         :kind kind
         :source (expand-file-name source-path)
         :module-init module
         :features features
         :top-level-count top-level-count
         :module-policy (nelisp-artifact--normalize-module-policy module-policy)
         :compiler (nelisp-artifact--compiler-plist))
   (when native (list :native native))
   (when (eq kind 'neln) (list :native-report native-report))
   (list :entry (list :type 'module-init
                      :id (file-name-nondirectory source-path)))))

(defun nelisp-artifact--printed-list-string (items)
  "Return ITEMS printed as one generated private list."
  (let ((parts (list "("))
        (first t))
    (dolist (item items)
      (unless first
        (push " " parts))
      (push (prin1-to-string item) parts)
      (setq first nil))
    (push ")" parts)
    (apply #'concat (nreverse parts))))

(defun nelisp-artifact--raw-source-escape-char (ch)
  "Return an ASCII reader escape for non-ASCII character CH."
  (if (<= ch #xffff)
      (format "\\u%04x" ch)
    (format "\\U%08x" ch)))

(defun nelisp-artifact--raw-source-ascii (source)
  "Return SOURCE made safe for the standalone raw-source reader.

The standalone NeLisp reader currently accepts ASCII input reliably and can
read non-ASCII string contents through `\\u' / `\\U' escapes.  Preserve string
semantics by escaping non-ASCII string characters, and replace non-ASCII
comment text with spaces because comments are discarded by the reader."
  (let ((len (length source))
        (i 0)
        (in-string nil)
        (in-comment nil)
        (escape nil)
        (parts nil)
        ch)
    (while (< i len)
      (setq ch (aref source i))
      (cond
       (in-comment
        (cond
         ((= ch ?\n)
          (push (char-to-string ch) parts)
          (setq in-comment nil))
         ((< ch 128)
          (push (char-to-string ch) parts))
         (t
          (push " " parts))))
       (in-string
        (cond
         (escape
          (push (if (< ch 128)
                    (char-to-string ch)
                  (nelisp-artifact--raw-source-escape-char ch))
                parts)
          (setq escape nil))
         ((= ch ?\\)
          (push "\\" parts)
          (setq escape t))
         ((= ch ?\")
          (push "\"" parts)
          (setq in-string nil))
         ((< ch 128)
          (push (char-to-string ch) parts))
         (t
          (push (nelisp-artifact--raw-source-escape-char ch) parts))))
       ((= ch ??)
        (push "?" parts)
        (when (< (1+ i) len)
          (setq i (1+ i)
                ch (aref source i))
          (cond
           ((= ch ?\\)
            (push "\\" parts)
            (when (< (1+ i) len)
              (setq i (1+ i)
                    ch (aref source i))
              (push (if (< ch 128)
                        (char-to-string ch)
                      (nelisp-artifact--raw-source-escape-char ch))
                    parts)))
           (t
            (push (if (< ch 128)
                      (char-to-string ch)
                    (nelisp-artifact--raw-source-escape-char ch))
                  parts)))))
       ((= ch ?\")
        (push "\"" parts)
        (setq in-string t))
       ((= ch ?\;)
        (push ";" parts)
        (setq in-comment t))
       ((< ch 128)
        (push (char-to-string ch) parts))
       (t
        ;; Non-ASCII outside strings/comments would require symbol-token
        ;; escaping support in the standalone reader.  Keep raw artifacts
        ;; readable and make the unsupported case explicit.
        (error "raw eval-source contains non-ASCII outside string/comment at offset %s"
               i)))
      (setq i (1+ i)))
    (apply #'concat (nreverse parts))))

(defun nelisp-artifact--eval-source-module-string (source)
  "Return a generated raw-source `:module-init' list for eval-only SOURCE."
  (let ((ascii-source (nelisp-artifact--raw-source-ascii source)))
    (concat "((:eval-source-raw "
            (number-to-string (length ascii-source))
            "\n"
            ascii-source
            "\n))")))

(defun nelisp-artifact--artifact-string (payload &optional eval-source)
  "Serialize artifact PAYLOAD to a `.nelc' string.
The artifact remains a normal generated plist, but the large `:module-init'
list is printed item-by-item instead of sending the whole payload through one
recursive `prin1-to-string' call.  When EVAL-SOURCE is non-nil, serialize
eval-only module replay as one `(progn ...)' source item to avoid re-printing
large parsed forms."
  (let* ((kind (plist-get payload :kind))
         (native (plist-get payload :native))
         (native-sections
          (or (nelisp-artifact--native-sections-from-native
               (plist-get payload :native-sections))
              (nelisp-artifact--native-sections-from-native native)))
         (wire-native-sections
          (mapcar #'nelisp-artifact--native-section-wire-v5 native-sections))
         (native-string
          (cond
           ((> (length wire-native-sections) 1)
            (concat " :native-sections "
                    (prin1-to-string wire-native-sections)))
           (native
            (concat " :native "
                    (prin1-to-string (car wire-native-sections))))
           (wire-native-sections
            (concat " :native "
                    (prin1-to-string (car wire-native-sections))))
           (t "")))
         (native-report (plist-get payload :native-report))
         (module (plist-get payload :module-init))
         (module-start (nelisp-artifact--profile-time))
         (module-string
          (if eval-source
              (nelisp-artifact--eval-source-module-string eval-source)
            (nelisp-artifact--printed-list-string module)))
         (wrap-start nil)
         (native-offset 0)
         (module-offset 0)
         (artifact nil)
         (stable nil)
         (attempt 0))
    (nelisp-artifact--profile-log
     "artifact-module-string" module-start
     (list :eval-source (and eval-source t)
           :items (length module)
           :bytes (length module-string)))
    (setq wrap-start (nelisp-artifact--profile-time))
    (while (and (< attempt 16) (not stable))
      (let* ((prefix
              (concat
               nelisp-artifact--magic
               "(:format " (prin1-to-string (plist-get payload :format))
               " :kind " (prin1-to-string kind)
               " :layout-version "
               (number-to-string nelisp-artifact--layout-version)
               " :native-offset " (number-to-string native-offset)
               " :module-offset " (number-to-string module-offset)
               " :source " (prin1-to-string (plist-get payload :source))
               " :features " (prin1-to-string (plist-get payload :features))))
             (native-key-width
              (cond
               ((> (length wire-native-sections) 1)
                (length " :native-sections "))
               (wire-native-sections (length " :native "))
               (t 0)))
             (actual-native-offset
              (if (> native-key-width 0)
                  (+ (length prefix) native-key-width)
                0))
             (actual-module-offset
              (+ (length prefix)
                 (length native-string)
                 (length " :module-init "))))
        (if (and (= native-offset actual-native-offset)
                 (= module-offset actual-module-offset))
            (progn
              (setq artifact
                    (concat
                     prefix
                     native-string
                     " :module-init " module-string
                     " :top-level-count " (prin1-to-string
                                            (plist-get payload
                                                       :top-level-count))
                     " :module-policy " (prin1-to-string
                                          (plist-get payload :module-policy))
                     " :compiler " (prin1-to-string
                                     (plist-get payload :compiler))
                     (if (eq kind 'neln)
                         (concat " :native-report "
                                 (prin1-to-string native-report))
                       "")
                     " :entry " (prin1-to-string (plist-get payload :entry))
                     ")\n"))
              (setq stable t))
          (setq native-offset actual-native-offset)
          (setq module-offset actual-module-offset)))
      (setq attempt (1+ attempt)))
    (unless stable
      (error "artifact header offsets did not reach a fixed point"))
    (nelisp-artifact--profile-log "artifact-wrap-string" wrap-start)
    artifact))

(defun nelisp-artifact--preload-records (preloads)
  "Return Doc 142 §5 `:preloads' records (path + sha256) for PRELOADS."
  (mapcar #'nelisp-artifact--file-record preloads))

(defun nelisp-artifact--manifest-plist (source-path features top-level-count
                                                    target artifact-sha256
                                                    artifact-size
                                                    preload-records load-paths
                                                    kind native native-report
                                                    native-policy module-policy)
  "Build the Doc 142 v1 manifest plist.
ARTIFACT-SHA256 is the integrity hash of the serialized artifact;
PRELOAD-RECORDS and LOAD-PATHS, plus the artifact/source/compiler/ABI
fields, are the cache-key participants enforced by
`nelisp-artifact--validate' (Doc 142 §5/§7).  KIND is `nelc' / `neln';
for `neln' the artifact-class is `native', the runtime-abi is the AOT
ABI, and the manifest records only wrapper-install metadata (`:symbols'
and `:defuns').  Full NATIVE code, architecture, relocations, hashes, and
compile reports remain in the integrity-covered artifact payload."
  (let ((native-sections (nelisp-artifact--native-sections-from-native native)))
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
           :source (nelisp-artifact--file-record source-path)
           :artifact-size artifact-size
           :preloads preload-records
           :load-path (mapcar #'expand-file-name load-paths)
           :features features
           :top-level-count top-level-count
           :module-policy (nelisp-artifact--normalize-module-policy
                           module-policy)
           :compiler (nelisp-artifact--compiler-plist))
     (when (eq kind 'neln)
       (list :native-policy (nelisp-artifact--normalize-native-policy
                             native-policy)))
     (cond
      ((null native-sections) nil)
      ((= (length native-sections) 1)
       (list :native
             (nelisp-artifact--native-install-metadata
              (car native-sections))))
      (t
       (list :native-sections
             (mapcar #'nelisp-artifact--native-install-metadata
                     native-sections))))
     (when (eq kind 'neln)
       (list :native-report native-report))
     (list :entry (list :type 'module-init
                        :id (file-name-nondirectory source-path))))))

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
                                                 kind native-policy module-policy)
  "Compile SOURCE-PATH into ARTIFACT-PATH and MANIFEST-PATH.
KIND is `nelc' (bytecode, default) or `neln' (bytecode + an embedded
native object for the standalone runtime, Doc 142 §6.4)."
  (let* ((kind (or kind 'nelc))
         (native-policy (nelisp-artifact--normalize-native-policy
                         (or native-policy
                             nelisp-artifact-default-native-policy)))
         (module-policy (nelisp-artifact--normalize-module-policy
                         (or module-policy
                             (and (eq kind 'neln)
                                  (eq native-policy 'required)
                                  'eval-only)
                             nelisp-artifact-default-module-policy)))
         (manifest-path (or manifest-path
                             (nelisp-artifact--sibling-manifest-path artifact-path)))
         (total-start (nelisp-artifact--profile-time))
         (stage-start nil)
         (source nil)
         (transformed-source nil)
         (forms nil)
         (eval-source nil)
         (module nil)
         (features nil)
         (native nil)
         (native-report nil)
         (artifact-payload nil)
         (artifact-content nil)
         (manifest nil))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq source (nelisp-artifact--read-file-as-string source-path))
    (nelisp-artifact--profile-log
     "read-source" stage-start
     (list :bytes (length source) :source source-path))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq transformed-source
          (if nelisp-artifact-source-transform-function
              (funcall nelisp-artifact-source-transform-function
                       source source-path)
            source))
    (unless (stringp transformed-source)
      (error "source transform function must return a string: %S"
             transformed-source))
    (nelisp-artifact--profile-log
     "transform-source" stage-start
     (list :bytes (length transformed-source)
           :source source-path
           :transformed (not (equal transformed-source source))))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq forms (nelisp-artifact--read-top-level-forms
                 transformed-source source-path))
    (nelisp-artifact--profile-log
     "read-forms" stage-start
     (list :forms (length forms) :source source-path))
    (when nelisp-artifact--rewrite-defalias-late
      (setq stage-start (nelisp-artifact--profile-time))
      (setq forms (nelisp-artifact--rewrite-defalias-late-forms forms))
      (nelisp-artifact--profile-log
       "rewrite-defalias-late" stage-start
       (list :forms (length forms) :source source-path)))
    (setq stage-start (nelisp-artifact--profile-time))
    (let ((load-path (append load-paths load-path))
          (nelisp-load-path (append load-paths nelisp-load-path)))
      (dolist (preload preloads)
        (load preload nil t))
      (dolist (form forms)
        (nelisp-artifact--apply-compile-time-context-tree form)
        (push (nelisp-artifact--compile-top-level-form form module-policy)
              module))
      (setq module (nreverse module)))
    (nelisp-artifact--profile-log
     "module-build" stage-start
     (list :forms (length forms) :module-policy module-policy))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq features (nelisp-artifact--collect-features forms))
    (nelisp-artifact--profile-log
     "collect-features" stage-start
     (list :features (length features)))
    (when (and requested-feature (not (memq requested-feature features)))
      (error "compile-elisp-artifact: source did not provide %S" requested-feature))
    (when (eq kind 'neln)
      (setq stage-start (nelisp-artifact--profile-time))
      (setq native (nelisp-artifact--native-compile-section
                    forms target native-policy))
      (setq native-report nelisp-artifact--last-native-compile-report)
      (nelisp-artifact--enforce-native-policy
       source-path kind native-policy native-report)
      (nelisp-artifact--profile-log
       "native-section" stage-start
       (list :native-policy native-policy)))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq artifact-payload
          (nelisp-artifact--artifact-payload source-path module features
                                             (length forms) kind native
                                             native-report module-policy))
    (when (and (eq module-policy 'eval-only)
               (eq kind 'nelc)
               (integerp nelisp-artifact-raw-eval-source-threshold)
               (> (length transformed-source)
                  nelisp-artifact-raw-eval-source-threshold))
      (setq eval-source transformed-source))
    (setq artifact-content
          (nelisp-artifact--artifact-string artifact-payload eval-source))
    (nelisp-artifact--profile-log
     "artifact-string" stage-start
     (list :bytes (nelisp-artifact--byte-length artifact-content)))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq manifest
          (nelisp-artifact--manifest-plist
           source-path features (length forms) target
	           (secure-hash 'sha256 artifact-content)
           (nelisp-artifact--byte-length artifact-content)
	           (nelisp-artifact--preload-records preloads)
	           load-paths kind native native-report native-policy
                   module-policy))
    (nelisp-artifact--profile-log
     "manifest" stage-start
     (list :kind kind :module-policy module-policy))
    (setq stage-start (nelisp-artifact--profile-time))
    (make-directory (file-name-directory artifact-path) t)
    (nelisp-artifact--write-pair-atomically
     artifact-path artifact-content
     manifest-path (concat (prin1-to-string manifest) "\n"))
    (nelisp-artifact--profile-log
     "write" stage-start
     (list :artifact artifact-path :manifest manifest-path))
    (nelisp-artifact--profile-log
     "total" total-start
     (list :forms (length forms) :kind kind :module-policy module-policy))
    manifest))

(defun nelisp-artifact--replace-file-atomically (path content)
  "Replace PATH with CONTENT."
  (let ((temp (nelisp-artifact--make-temp-path path "tmp")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil temp nil 'silent))
          (rename-file temp path t)
          t)
      (nelisp-artifact--delete-if-exists temp))))

(defun nelisp-artifact--runtime-image-forms (image-path)
  "Return top-level forms stored in runtime IMAGE-PATH.
The source-v1 image stores one or more replayable bundles, normally as
`(progn ...)'.  For artifact compilation those bundles are flattened so
top-level `defun' forms remain visible to the `.neln' native compiler."
  (let* ((source (nelisp-artifact--read-file-as-string image-path))
         (forms (nelisp-artifact--read-all-from-string source))
         (out nil))
    (unless (string-match-p "\\`;;; nelisp-runtime-image source-v1\r?\n" source)
      (error "unsupported runtime image format: %s" image-path))
    (dolist (form forms)
      (if (and (consp form) (eq (car form) 'progn))
          (setq out (append out (cdr form)))
        (setq out (append out (list form)))))
    out))

(defun nelisp-artifact--runtime-image-source (image-path)
  "Return flattened Elisp source stored in runtime IMAGE-PATH."
  (mapconcat (lambda (form) (concat (prin1-to-string form) "\n"))
             (nelisp-artifact--runtime-image-forms image-path)
             ""))

(defun nelisp-artifact--compile-runtime-image-wasm
    (image-path artifact-path &optional target load-paths preloads requested-feature)
  "Compile runtime IMAGE-PATH to a standalone wasm ARTIFACT-PATH.
This bypasses the `.nelc' / `.neln' artifact path and emits one
self-contained `.wasm' via `nelisp-aot-compile-to-object'."
  (unless (nelisp-artifact--runtime-image-wasm-target-p target)
    (error "unsupported wasm runtime-image target: %S" target))
  (unless (nelisp-artifact--ensure-native-compiler)
    (error "native compiler unavailable"))
  (let* ((forms nil)
         (features nil)
         (program nil)
         (load-path (append load-paths load-path))
         (nelisp-load-path (append load-paths nelisp-load-path)))
    (dolist (preload preloads)
      (load preload nil t))
    (setq forms (nelisp-artifact--runtime-image-forms image-path))
    (setq features (nelisp-artifact--collect-features forms))
    (when (and requested-feature (not (memq requested-feature features)))
      (error "compile-runtime-image: source did not provide %S" requested-feature))
    (setq program (if forms (cons 'seq forms) 0))
    (nelisp-aot-compile-to-object
     program artifact-path :arch 'wasm32 :format 'wasm)
    artifact-path))

(defun nelisp-artifact-compile-runtime-image-file
    (image-path artifact-path &optional manifest-path target load-paths preloads
                requested-feature kind native-policy module-policy)
  "Compile runtime IMAGE-PATH into ARTIFACT-PATH.
The image is flattened into a temporary source file before calling
`nelisp-artifact-compile-file', preserving top-level `defun' visibility
for native `.neln' hot paths.  The final manifest records IMAGE-PATH so
stale image caches are rejected at artifact load time."
  (let* ((kind (or kind 'nelc))
         (manifest-path (or manifest-path
                            (nelisp-artifact--sibling-manifest-path artifact-path)))
         (source-temp (nelisp-artifact--make-temp-path artifact-path
                                                       "runtime-source.el"))
         (manifest nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (nelisp-artifact--runtime-image-source image-path)
                          nil source-temp nil 'silent))
          (setq manifest
                (nelisp-artifact-compile-file
	                 source-temp artifact-path manifest-path target load-paths
	                 preloads requested-feature kind native-policy
                         module-policy))
          (setq manifest
                (plist-put manifest :runtime-image
                           (nelisp-artifact--file-record image-path)))
          (setq manifest
                (plist-put manifest :entry
                           (list :type 'runtime-image
                                 :id (file-name-nondirectory image-path))))
          (nelisp-artifact--replace-file-atomically
           manifest-path (concat (prin1-to-string manifest) "\n"))
          manifest)
      (nelisp-artifact--delete-if-exists source-temp))))

(defun nelisp-artifact--parse-payload (content artifact-path)
  "Parse the `.nelc' CONTENT string, returning its payload plist."
  (let ((prefix-len (length nelisp-artifact--magic)))
    (unless (string-prefix-p nelisp-artifact--magic content)
      (signal 'nelisp-artifact-invalid
              (list "invalid .nelc magic header" artifact-path)))
    (let ((payload (nelisp-artifact--read-one-private-form
                    (substring content prefix-len) artifact-path)))
      (unless (eq (plist-get payload :format) nelisp-artifact--format)
        (signal 'nelisp-artifact-invalid
                (list "unsupported .nelc format"
                      (plist-get payload :format) artifact-path)))
      (when (plist-member payload :native)
        (setq payload
              (plist-put
               payload :native
               (nelisp-artifact--native-value-flatten-v5
                (plist-get payload :native)))))
      (when (plist-member payload :native-sections)
        (setq payload
              (plist-put
               payload :native-sections
               (nelisp-artifact--native-value-flatten-v5
                (plist-get payload :native-sections)))))
      payload)))

(defun nelisp-artifact--parse-payload-fast (content artifact-path manifest)
  "Parse CONTENT's load-critical private payload fields quickly.
MANIFEST supplies `.neln' native metadata, avoiding a full read of the payload's
`:native' value whose object bytes are not needed for normal module install."
  (let ((prefix-len (length nelisp-artifact--magic)))
    (unless (string-prefix-p nelisp-artifact--magic content)
      (signal 'nelisp-artifact-invalid
              (list "invalid .nelc magic header" artifact-path)))
    (let* ((body (substring content prefix-len))
           (format (nelisp-artifact--read-private-symbol-token
                    body :format artifact-path))
           (module (nelisp-artifact--read-private-keyword-value
                    body :module-init artifact-path))
           (features (nelisp-artifact--read-private-keyword-value
                      body :features artifact-path t))
           (kind (or (and manifest (plist-get manifest :kind))
                     (nelisp-artifact--read-private-symbol-token
                      body :kind artifact-path)))
           (native (or (and manifest (plist-get manifest :native-sections))
                       (and manifest (plist-get manifest :native)))))
      (unless (eq format nelisp-artifact--format)
        (signal 'nelisp-artifact-invalid
                (list "unsupported .nelc format" format artifact-path)))
      (list :format format
            :kind kind
            :module-init module
            :features (if (eq features nelisp-artifact--missing-key)
                          nil
                        features)
            :native native))))

(defun nelisp-artifact--private-item-end (source pos len label)
  "Return one past the generated private list item at POS in SOURCE.
LABEL is used in error messages.  This scanner is intentionally narrow: private
artifacts are generated by this module, and module items are printed list forms."
  (let ((i pos)
        (depth 0)
        (in-string nil)
        (escaped nil)
        (done nil))
    (while (and (< i len) (not done))
      (let ((ch (aref source i)))
        (cond
         (in-string
          (cond
           (escaped
            (setq escaped nil))
           ((= ch ?\\)
            (setq escaped t))
           ((= ch ?\")
            (setq in-string nil))))
         ((= ch ?\")
          (setq in-string t))
         ((= ch ?\;)
          (while (and (< i len) (not (= (aref source i) ?\n)))
            (setq i (1+ i))))
         ((= ch ?\()
          (setq depth (1+ depth)))
         ((= ch ?\))
          (setq depth (1- depth))
          (when (= depth 0)
            (setq done t)))))
      (setq i (1+ i)))
    (unless done
      (error "unterminated private artifact item in %s" label))
    i))

(defun nelisp-artifact--read-private-item (source start end)
  "Read one private artifact item from SOURCE between START and END."
  (let ((read (nelisp-read--sexp source start)))
    (unless (and (consp read)
                 (= (cdr read) end))
      (error "invalid private artifact item"))
    (car read)))

(defun nelisp-artifact--mirror-recent-function-defs ()
  "Give functions the last `:eval' item defined in the sandbox a function cell.
`:fn' items get one from `nelisp-artifact--install-function'; `:eval' items did
not, so a natively dispatched call could not reach them (measured on the
13.24 MB bootstrap: the buffer-builtins alias loop signalled
\(void-function nelisp-ec-buffer-p) while `nelisp--functions' held the name).
Only names whose function cell is still void are installed, so an existing
host, prelude, or runtime definition is never clobbered."
  (when (boundp 'nelisp--recent-function-defs)
    (let ((names nelisp--recent-function-defs))
      (setq nelisp--recent-function-defs nil)
      (while names
        (let* ((name (car names))
               ;; `gethash' defaults to nil, so a recorded name that is no
               ;; longer in the table -- e.g. a later `defmacro' took the name
               ;; over -- would install nil and POISON the entry: the sandbox
               ;; then treats the name as bound-to-nil rather than unbound and
               ;; `nelisp--apply' receives nil (measured on the 13.24 MB
               ;; bootstrap: nelisp-void-function: (nil)).  Ask for `unbound'
               ;; explicitly and skip when the name is gone.
               (fn (gethash name nelisp--functions nelisp--unbound)))
          (unless (or (eq fn nelisp--unbound) (fboundp name))
            (nelisp-artifact--install-function name fn)))
        (setq names (cdr names))))))

(defconst nelisp-artifact--mirror-skip 'nelisp-artifact--mirror-skip--skip
  "Sentinel: this sandbox value must not be mirrored natively.
An ordinary interned symbol on purpose: the defconst form is serialized
into the runtime artifact cache and re-evaluated once per namespace layer,
so an uninterned `make-symbol' value yields a DIFFERENT object per layer
and the `eq' skip test fails across them (measured: flat v6 rejected its
own artifact after closure-valued runtime variables were set to a stray
uninterned symbol during the cache self-bootstrap).")

(defun nelisp-artifact--mirror-variable-value (name value)
  "Return the native mirror for NAME's sandbox VALUE.
Sandbox function objects (`nelisp-closure' / `nelisp-bcl') now mirror
VERBATIM: `nl_apply_function' gained a dispatch-back arm that bounces
such objects to the sandbox's own `nelisp--apply', so native code can
both SEE and CALL closure-valued variables.  History, kept for the
record: verbatim mirroring alone made closure params resolve as free
variables (measured: (void-variable b) on the 13.24 MB bootstrap)
because the native apply mis-parsed the record; wrapping in a live
trampoline at mirror time hung the cache self-bootstrap (measured:
zero-output 20-minute hang).  The dispatch-back arm decides at CALL
time instead and fails with a clean void-function stash while
`nelisp--apply' is not yet natively resolvable (self-bootstrap
window), so neither failure mode can recur.  NAME is part of the
contract for future strategies."
  (ignore name)
  value)

(defun nelisp-artifact--mirror-recent-var-defs ()
  "Give sandbox `defvar'/`defconst' bindings a native global binding.
Natively dispatched module functions resolve free variables in the native
namespace, so a variable declared only in the sandbox globals is void at
call time (measured: (nelisp-unbound-variable shadow2-x) from a 3-line
artifact; on the 13.24 MB bootstrap this voided
`files--native-file-attributes' and broke `file-attributes' after replay).
Only names with no native binding are installed, so an existing host,
prelude, or runtime binding is never clobbered; the sandbox value is read
back through `nelisp-eval' so the sandbox stays the deciding layer."
  (when (boundp 'nelisp--recent-var-defs)
    (let ((names nelisp--recent-var-defs))
      (setq nelisp--recent-var-defs nil)
      (while names
        (let ((name (car names)))
          (unless (boundp name)
            (ignore-errors
              (let ((mv (nelisp-artifact--mirror-variable-value
                         name (nelisp-eval name))))
                (unless (eq mv nelisp-artifact--mirror-skip)
                  (set name mv))))))
        (setq names (cdr names))))))

(defun nelisp-artifact--replay-module-item (item)
  "Replay one module ITEM and return its resulting value."
  (cond
   ((and (consp item) (eq (car item) :fn))
    ;; Prefer the SOURCE-DEFUN through the native evaluator (measured 0.27 ms
    ;; per call versus 60.75 ms for the bytecode closure); fall back to the
    ;; serialized form when the item carries none or the source is rejected.
    (if (nth 3 item)
        (condition-case nil
            (progn
              (eval (nth 3 item))
              (puthash (nth 1 item)
                       (symbol-function (nth 1 item))
                       nelisp--functions))
          (error
           (nelisp-artifact--install-function (nth 1 item) (nth 2 item))))
      (nelisp-artifact--install-function (nth 1 item) (nth 2 item)))
    (nth 1 item))
   ((and (consp item) (eq (car item) :eval))
    (let ((form (nth 1 item))
          (value nil))
      (setq value (nelisp-eval form))
      ;; Module functions are defined by the native evaluator, so they resolve
      ;; free variables in the native namespace; a `defvar' replayed only
      ;; through `nelisp-eval' writes the sandbox globals alone and every such
      ;; read is void at call time (measured: (void-variable nat-base) from a
      ;; natively defined body).  Mirror the value the sandbox settled on
      ;; rather than re-evaluating the form: evaluating the `defvar' natively
      ;; keeps its own "assign only when unbound" state across loads, so a
      ;; reload after a source change kept the stale default (measured:
      ;; expected 1002, got 42).  The sandbox decides; the native namespace
      ;; mirrors.
      (when (and (consp form)
                 (memq (car form) '(defvar defconst))
                 (symbolp (nth 1 form)))
        (ignore-errors
          (let ((mv (nelisp-artifact--mirror-variable-value
                     (nth 1 form) (nelisp-eval (nth 1 form)))))
            (unless (eq mv nelisp-artifact--mirror-skip)
              (set (nth 1 form) mv)))))
      (nelisp-artifact--mirror-recent-function-defs)
      (nelisp-artifact--mirror-recent-var-defs)
      value))
   (t
    (nelisp-eval item))))

(defun nelisp-artifact--replay-generated-eval-source-item (content start end)
  "Replay generated eval-only source item in CONTENT from START to END.
Return (t . VALUE) when the fast path handled the item, otherwise nil."
  (let ((prefix "(:eval (progn\n"))
    (when (and (fboundp 'nelisp--eval-source-string)
               (nelisp-artifact--string-prefix-at-p prefix content start))
      (let ((source-start (+ start (length prefix)))
            (source-end (- end 3)))
        (when (and (>= source-end source-start)
                   (= (aref content source-end) ?\n)
                   (= (aref content (1+ source-end)) ?\))
                   (= (aref content (+ source-end 2)) ?\)))
          (cons t
                (nelisp--eval-source-string
                 (substring content source-start source-end))))))))

(defun nelisp-artifact--replay-raw-eval-source-item (content start)
  "Replay generated raw eval-only source item in CONTENT at START.
Return (t VALUE . END) when handled, otherwise nil."
  (let ((prefix "(:eval-source-raw "))
    (when (nelisp-artifact--string-prefix-at-p prefix content start)
      (let* ((len-start (+ start (length prefix)))
             (len-pair (nelisp-artifact--read-decimal-at content len-start))
             (source-len (car len-pair))
             (source-start (1+ (cdr len-pair)))
             (source-end (+ source-start source-len))
             (source nil)
             (last nil))
        (unless (= (aref content (cdr len-pair)) ?\n)
          (error "invalid raw eval source header"))
        (unless (and (< (+ source-end 2) (length content))
                     (= (aref content source-end) ?\n)
                     (= (aref content (1+ source-end)) ?\))
                     (= (aref content (+ source-end 2)) ?\)))
          (error "invalid raw eval source trailer"))
        (setq source (substring content source-start source-end))
        (cons t
              (cons (if (fboundp 'nelisp--eval-source-string)
                        (nelisp--eval-source-string source)
                      (let ((forms (nelisp-artifact--read-all-from-string
                                    source)))
                        (dolist (form forms)
                          (setq last (nelisp-eval form)))
                        last))
                    (+ source-end 3)))))))

(defun nelisp-artifact--module-token-end (content start limit)
  "Return the end of a generated module token in CONTENT before LIMIT."
  (let ((pos start))
    (while (and (< pos limit)
                (let ((ch (aref content pos)))
                  (not (or (= ch ?\s) (= ch ?\t)
                           (= ch ?\n) (= ch ?\r)
                           (= ch ?\() (= ch ?\))))))
      (setq pos (1+ pos)))
    pos))

(defun nelisp-artifact--module-fn-descriptor
    (content start end artifact-path)
  "Return a bounded canonical `:fn' descriptor, or nil for another item.
Canonical-looking malformed `:fn' items in CONTENT fail closed.  The returned
vector retains NAME, the bytecode-list bounds and the source DEFUN bounds: the
replay defines the function from its source with the native evaluator and keeps
the bytecode form only as a fallback."
  (when (nelisp-artifact--string-prefix-at-p "(:fn" content start)
    (let* ((kind-start (nelisp-read--skip-ws content (1+ start)))
           (kind-end
            (nelisp-artifact--module-token-end content kind-start end)))
      ;; A noncanonical legacy spelling is left to the generic replay path.
      (when (and (equal (substring content kind-start kind-end) ":fn")
                 (< kind-end end)
                 (let ((ch (aref content kind-end)))
                   (or (= ch ?\s) (= ch ?\t)
                       (= ch ?\n) (= ch ?\r))))
        (let* ((name-start (nelisp-read--skip-ws content kind-end))
               (name-end
                (nelisp-artifact--module-token-end content name-start end))
               (name-source (substring content name-start name-end))
               (name-read
                (condition-case nil
                    (nelisp-read--sexp name-source 0)
                  (error nil)))
               (bcl-start (nelisp-read--skip-ws content name-end))
               (bcl-end
                (if (and (< bcl-start end)
                         (= (aref content bcl-start) ?\())
                    (nelisp-artifact--private-item-end
                     content bcl-start end artifact-path)
                  (error "invalid canonical :fn bytecode in %s"
                         artifact-path)))
               (bcl-head-start
                (nelisp-read--skip-ws content (1+ bcl-start)))
               (bcl-head-end
                (nelisp-artifact--module-token-end
                 content bcl-head-start bcl-end))
               (source-start (nelisp-read--skip-ws content bcl-end))
               (source-end
                (if (and (< source-start end)
                         (= (aref content source-start) ?\())
                    (nelisp-artifact--private-item-end
                     content source-start end artifact-path)
                  (error "invalid canonical :fn source in %s"
                         artifact-path)))
               (source-head-start
                (nelisp-read--skip-ws content (1+ source-start)))
               (source-head-end
                (nelisp-artifact--module-token-end
                 content source-head-start source-end))
               (source-name-start
                (nelisp-read--skip-ws content source-head-end))
               (source-name-end
                (nelisp-artifact--module-token-end
                 content source-name-start source-end))
               (tail (nelisp-read--skip-ws content source-end)))
          (unless (and (< name-start name-end)
                       (consp name-read)
                       (symbolp (car name-read))
                       (= (cdr name-read) (length name-source))
                       (equal (substring content
                                         bcl-head-start bcl-head-end)
                              "nelisp-bcl")
                       (equal (substring content
                                         source-head-start source-head-end)
                              "defun")
                       (equal (substring content
                                         source-name-start source-name-end)
                              name-source)
                       (= tail (1- end))
                       (= (aref content tail) ?\)))
            (error "invalid canonical :fn item in %s" artifact-path))
          (vector :fn name-start name-end bcl-start bcl-end
                  source-start source-end))))))

(defvar nelisp-artifact-survey-failures nil
  "Non-nil while surveying: replay logs each failing item and keeps going.
Set from NELISP_ARTIFACT_SURVEY at load time.  A normal replay stops at the
first failing form, so one full pass over a large artifact yields exactly one
defect; on the 13.24 MB bootstrap that is a 74-minute cycle per defect.  In
survey mode every item is wrapped in its own handler, so a single pass returns
the whole remaining list.  Survey mode is for diagnosis only — it lets the
replay continue in a state later items may depend on, so the run's own success
or failure means nothing.")

(defun nelisp-artifact--survey-p ()
  "Return non-nil when replay should log item failures and continue."
  (or nelisp-artifact-survey-failures
      (and (fboundp 'getenv)
           (equal (getenv "NELISP_ARTIFACT_SURVEY") "1"))))

(defun nelisp-artifact--survey-wrap (source label)
  "Wrap SOURCE so a failure is reported as LABEL and does not abort the chunk."
  (concat "(condition-case nelisp-artifact--survey-err\n"
          source
          "\n  (error (nelisp-artifact--write-stderr\n"
          "          (concat \"SURVEY-FAIL " label " \"\n"
          "                  (prin1-to-string nelisp-artifact--survey-err)))))\n"))

(defun nelisp-artifact--replay-module-chunk-source (content descriptors native)
  "Return replay source for DESCRIPTORS in CONTENT.
When NATIVE is non-nil, canonical functions are defined from their source
DEFUN so the native evaluator owns them; otherwise they install from their
serialized bytecode form."
  (if (nelisp-artifact--survey-p)
      (nelisp-artifact--replay-module-chunk-source-survey content descriptors)
    (nelisp-artifact--replay-module-chunk-source-1 content descriptors native)))

(defun nelisp-artifact--replay-module-chunk-source-survey (content descriptors)
  "Return survey replay source: every item guarded, none aborting the chunk.
Functions install exactly as the ordinary native path installs them.  Only the
per-item guard differs, so a failure reported here is a failure the ordinary
replay would also hit.

The first version installed from the serialized bytecode instead, on the theory
that routing calls through `nelisp--apply' would stop a NeLisp closure reaching
a natively defined function from masking real defects.  That reasoning was stale
— the closure mismatch had already been fixed by dropping special forms from the
macro capture list — and the bytecode VM has its own constant-vector defect, so
every one of the 20 failures the first survey reported was an artifact of the
survey itself: all of them sat below 42% of `:module-init', which the ordinary
replay had already run past to 87.24%.  A survey that does not evaluate the same
way it measures reports its own bugs."
  (mapconcat
   (lambda (descriptor)
     (if (vectorp descriptor)
         (let ((name (substring content (aref descriptor 1) (aref descriptor 2))))
           (nelisp-artifact--survey-wrap
            (concat "  (progn "
                    (substring content (aref descriptor 5) (aref descriptor 6))
                    " (puthash '" name " (symbol-function '" name
                    ") nelisp--functions))")
            (concat "fn " name)))
       (nelisp-artifact--survey-wrap
        (concat "  (nelisp-artifact--replay-module-item '"
                (substring content (car descriptor) (cdr descriptor))
                ")")
        (format "eval @%d" (car descriptor)))))
   descriptors
   ""))

(defun nelisp-artifact--replay-module-chunk-source-1 (content descriptors native)
  "Return ordinary (non-survey) replay source for DESCRIPTORS in CONTENT."
  (mapconcat
   (lambda (descriptor)
     (if (vectorp descriptor)
         (if native
             ;; A serialized `nelisp-bcl' runs on the Elisp bytecode VM, which
             ;; the native evaluator then interprets -- two layers plus a macro
             ;; expansion per call (measured: 60.75 ms vs 0.27 ms for the same
             ;; body defined natively).  Emit the source DEFUN so this very
             ;; `nelisp--eval-source-string' defines it natively, then register
             ;; the resulting function object for NeLisp dispatch.
             (concat (substring content
                                (aref descriptor 5) (aref descriptor 6))
                     "\n(puthash '"
                     (substring content
                                (aref descriptor 1) (aref descriptor 2))
                     " (symbol-function '"
                     (substring content
                                (aref descriptor 1) (aref descriptor 2))
                     ") nelisp--functions)\n")
           (concat "(nelisp-artifact--install-function '"
                   (substring content
                              (aref descriptor 1) (aref descriptor 2))
                   " '"
                   (substring content
                              (aref descriptor 3) (aref descriptor 4))
                   ")\n"))
       (concat "(nelisp-artifact--replay-module-item '"
               (substring content
                          (car descriptor) (cdr descriptor))
               ")\n")))
   descriptors
   ""))

(defun nelisp-artifact--replay-module-source-chunk (content descriptors)
  "Replay module DESCRIPTORS from CONTENT at top-level boundaries.
Each descriptor is either an ordinary (START . END) span or a canonical `:fn'
vector.  Canonical functions are defined from their source DEFUN by the native
evaluator; a chunk the native evaluator rejects is replayed once more from the
serialized bytecode form, so an unsupported body still loads.  Return the last
value."
  (condition-case native-err
      (nelisp--eval-source-string
       (nelisp-artifact--replay-module-chunk-source content descriptors t))
    (error
     ;; `nelisp-artifact--write-stderr' bottoms out in a standalone-only
     ;; primitive; this path also runs under host Emacs (measured:
     ;; (void-function nelisp--write-stderr-line) from the runtime-image CLI
     ;; test), so report through `message' there.
     (let ((text (format "artifact: native chunk replay fallback: %s"
                         (error-message-string native-err))))
       (if (fboundp 'nelisp--write-stderr-line)
           (nelisp-artifact--write-stderr text)
         (message "%s" text)))
     (nelisp--eval-source-string
      (nelisp-artifact--replay-module-chunk-source content descriptors nil)))))

(defun nelisp-artifact--replay-module-streaming
    (content artifact-path &optional module-value-offset)
  "Replay CONTENT's `:module-init' list without materializing the whole list.
MODULE-VALUE-OFFSET is an authoritative versioned-layout value start."
  (let* ((pos
          (if (integerp module-value-offset)
              (progn
                (unless (and (> module-value-offset 13)
                             (< module-value-offset (length content))
                             (nelisp-artifact--string-prefix-at-p
                              ":module-init " content
                              (- module-value-offset 13)))
                  (error "invalid module value offset in %s: %S"
                         artifact-path module-value-offset))
                module-value-offset)
            (nelisp-artifact--private-keyword-value-pos
             content :module-init artifact-path)))
         (len (length content))
         (last nil)
         (count 0)
         (chunk-size nelisp-artifact-module-replay-chunk-size)
         (chunked (and (fboundp 'nelisp--eval-source-string)
                       (integerp chunk-size)
                       (> chunk-size 0)))
         (spans nil)
         (span-count 0)
         (done nil)
         end item descriptor)
    (setq pos (nelisp-read--skip-ws content pos))
    (unless (and (< pos len) (= (aref content pos) ?\())
      (error "invalid :module-init list in %s" artifact-path))
    (setq pos (1+ pos))
    (while (and (< pos len) (not done))
      (setq pos (nelisp-read--skip-ws content pos))
      (cond
       ((>= pos len)
        (error "unterminated :module-init list in %s" artifact-path))
       ((= (aref content pos) ?\))
        (setq pos len))
       (t
        (unless (= (aref content pos) ?\()
          (error "invalid :module-init item in %s" artifact-path))
        (if (nelisp-artifact--string-prefix-at-p
             "(:eval-source-raw " content pos)
            (progn
              ;; The raw-source helper evaluates immediately, so preserve
              ;; module order by draining ordinary items first.
              (when spans
                (setq last
                      (nelisp-artifact--replay-module-source-chunk
                       content (nreverse spans)))
                (setq spans nil)
                (setq span-count 0))
              (let ((raw
                     (nelisp-artifact--replay-raw-eval-source-item
                      content pos)))
                (unless raw
                  (error "invalid raw module item in %s" artifact-path))
                (setq last (cadr raw))
                (setq pos len)
                (setq done t)))
          (setq end (nelisp-artifact--private-item-end
                     content pos len artifact-path))
          (if (nelisp-artifact--string-prefix-at-p
               "(:eval (progn\n" content pos)
              (progn
                ;; This fast helper also evaluates immediately.
                (when spans
                  (setq last
                        (nelisp-artifact--replay-module-source-chunk
                         content (nreverse spans)))
                  (setq spans nil)
                  (setq span-count 0))
                (let ((fast
                       (nelisp-artifact--replay-generated-eval-source-item
                        content pos end)))
                  (unless fast
                    (error "invalid generated eval module item in %s"
                           artifact-path))
                  (setq last (cdr fast))))
            (if chunked
                (progn
                  (setq descriptor
                        (or (nelisp-artifact--module-fn-descriptor
                             content pos end artifact-path)
                            (cons pos end)))
                  (setq spans (cons descriptor spans))
                  (setq span-count (1+ span-count))
                  (when (>= span-count chunk-size)
                    (setq last
                          (nelisp-artifact--replay-module-source-chunk
                           content (nreverse spans)))
                    (setq spans nil)
                    (setq span-count 0)))
              (setq item
                    (nelisp-artifact--read-private-item content pos end))
              (setq last
                    (nelisp-artifact--replay-module-item item))))
          (setq pos end))
        (setq count (1+ count))
        (when (and nelisp-artifact-profile-load-detail
                   (= 0 (% count 100)))
          (nelisp-artifact--write-stderr
           (concat "artifact_load_profile progress=module-item"
                   " count=" (number-to-string count)
                   " pos=" (number-to-string pos))))
        )))
    (when spans
      (setq last
            (nelisp-artifact--replay-module-source-chunk
             content (nreverse spans))))
    last))

(defun nelisp-artifact--read-layout-decimal
    (content key-pos literal label)
  "Read strict nonnegative decimal after LITERAL at KEY-POS in CONTENT."
  (unless (and (integerp key-pos)
               (nelisp-artifact--string-prefix-at-p
                literal content key-pos))
    (error "invalid layout key %s in %s" literal label))
  (let* ((value-pos
          (nelisp-read--skip-ws content (+ key-pos (length literal))))
         (pair (nelisp-artifact--read-decimal-at content value-pos))
         (value (car pair))
         (end (cdr pair))
         (len (length content)))
    (unless (and (< value-pos end)
                 (>= value 0)
                 (or (= end len)
                     (let ((ch (aref content end)))
                       (or (= ch ?\s) (= ch ?\t)
                           (= ch ?\n) (= ch ?\r) (= ch ?\))))))
      (error "invalid layout decimal for %s in %s" literal label))
    value))

(defun nelisp-artifact--load-private-fast (full-path content manifest)
  "Load private artifact CONTENT using generated-key and streaming readers.
For `.neln', native install metadata is authoritative only when read from the
integrity-covered artifact itself; sidecar manifest native fields are ignored."
  (let* ((total-start (nelisp-artifact--profile-time))
         (key-start total-start)
         (prefix-len (length nelisp-artifact--magic))
         (list-start (nelisp-read--skip-ws content prefix-len))
         (content-len (length content))
         (header-end
          (if (< content-len 4096) content-len 4096))
         (header-key-start (nelisp-artifact--profile-time))
         (header-key-positions
          (nelisp-artifact--private-list-key-positions
           content list-start header-end
           '((:format . ":format")
             (:kind . ":kind")
             (:features . ":features")
             (:layout-version . ":layout-version")
             (:native-offset . ":native-offset")
             (:module-offset . ":module-offset"))
           full-path nil t))
         (_header-profile
          (nelisp-artifact--load-profile-log
           "fast-header-key-scan" header-key-start
           (list :end header-end :positions header-key-positions)))
         (key-positions
          (if (and (assq :format header-key-positions)
                   (assq :kind header-key-positions)
                   (assq :features header-key-positions))
              header-key-positions
            (let ((full-key-start (nelisp-artifact--profile-time))
                  (positions nil))
              (setq positions
                    (nelisp-artifact--private-list-key-positions
                     content list-start content-len
                     '((:format . ":format")
                       (:kind . ":kind")
                       (:features . ":features")
                       (:layout-version . ":layout-version")
                       (:native-offset . ":native-offset")
                       (:module-offset . ":module-offset"))
                     full-path))
              (nelisp-artifact--load-profile-log
               "fast-full-key-scan" full-key-start
               (list :positions positions))
              positions)))
         (format-pos
          (or (cdr (assq :format key-positions))
              (error "missing top-level :format in %s" full-path)))
         (kind-pos
          (or (cdr (assq :kind key-positions))
              (error "missing top-level :kind in %s" full-path)))
         (features-pos (cdr (assq :features key-positions)))
         (layout-pos (cdr (assq :layout-version key-positions)))
         (native-offset-pos (cdr (assq :native-offset key-positions)))
         (module-offset-pos (cdr (assq :module-offset key-positions)))
         (layout-version
          (and layout-pos
               (nelisp-artifact--read-layout-decimal
                content layout-pos ":layout-version" full-path)))
         (native-value-offset
          (and layout-pos native-offset-pos
               (nelisp-artifact--read-layout-decimal
                content native-offset-pos ":native-offset" full-path)))
         (module-value-offset
          (and layout-pos module-offset-pos
               (nelisp-artifact--read-layout-decimal
                content module-offset-pos ":module-offset" full-path)))
         (_layout-validation
          (when (or layout-pos native-offset-pos module-offset-pos)
            (unless (and (memq layout-version
                               (list nelisp-artifact--layout-version
                                     nelisp-artifact--legacy-offset-layout-version))
                         (integerp native-value-offset)
                         (integerp module-value-offset)
                         (> module-value-offset 0))
              (error "invalid or incomplete artifact layout header in %s"
                     full-path))))
         (format-read-start (nelisp-artifact--profile-time))
         (format-value-pos
          (nelisp-read--skip-ws
           content (+ format-pos 7)))
         (format
          (if (nelisp-artifact--string-prefix-at-p
               "nelisp-private-nelc-v2 " content format-value-pos)
              nelisp-artifact--format
            (nelisp-artifact--read-private-symbol-token
             content :format full-path nil format-pos)))
         (_format-profile
          (nelisp-artifact--load-profile-log
           "fast-format-token" format-read-start
           (list :pos format-pos :value format)))
         (provided-features nelisp-artifact--missing-key)
         (kind-read-start (nelisp-artifact--profile-time))
         (kind (or (plist-get manifest :kind)
                   (nelisp-artifact--read-private-symbol-token
                    content :kind full-path nil kind-pos)))
         (_kind-profile
          (nelisp-artifact--load-profile-log
           "fast-kind-token" kind-read-start
           (list :pos kind-pos :value kind)))
         (serialized-native nil)
         (last nil))
    (nelisp-artifact--load-profile-log "fast-key-read" key-start
                                       (list :kind kind))
    (unless (string-prefix-p nelisp-artifact--magic content)
      (signal 'nelisp-artifact-invalid
              (list "invalid .nelc magic header" full-path)))
    (unless (eq format nelisp-artifact--format)
      (signal 'nelisp-artifact-invalid
              (list "unsupported .nelc format" format full-path)))
    (when (eq kind 'neln)
      (let ((native-read-start (nelisp-artifact--profile-time)))
        (when (fboundp 'nelisp--native-call-boundary)
          (setq serialized-native
                (nelisp-artifact--read-serialized-native-sections-for-load
                 content full-path native-value-offset)))
        (nelisp-artifact--load-profile-log
         "native-total" native-read-start
         (list :sections (length serialized-native)))))
    (let ((replay-start (nelisp-artifact--profile-time)))
      (setq last
            (nelisp-artifact--replay-module-streaming
             content full-path module-value-offset))
      (nelisp-artifact--load-profile-log "module-total" replay-start))
    (let ((features-read-start (nelisp-artifact--profile-time)))
      (setq provided-features
            (if features-pos
                (condition-case err
                    (nelisp-artifact--read-private-symbol-list-token
                     content :features full-path nil features-pos)
                  (error
                   (error
                    "failed top-level :features read at %S (%S) in %s: %S"
                    features-pos
                    (and (< features-pos (length content))
                         (aref content features-pos))
                    full-path err)))
              nelisp-artifact--missing-key))
      (nelisp-artifact--load-profile-log
       "fast-features-token" features-read-start
       (list :pos features-pos
             :count (if (listp provided-features)
                        (length provided-features)
                      0))))
    (when serialized-native
      (nelisp-artifact--register-native-sections
       full-path serialized-native))
    (when (and (eq kind 'neln) serialized-native
               nelisp-artifact-native-dispatch-enabled)
      (let ((native-start (nelisp-artifact--profile-time)))
        (nelisp-artifact--install-native-functions
         full-path serialized-native)
        (nelisp-artifact--load-profile-log "native-install" native-start)))
    (unless (eq provided-features nelisp-artifact--missing-key)
      (let ((feature-start (nelisp-artifact--profile-time)))
        ;; Do not bind a local named `features' here.  Standalone NeLisp uses
        ;; dynamic binding, and `provide' must update the global `features'
        ;; variable rather than a loader-local traversal list.
        (dolist (feature provided-features)
          (when (fboundp 'nelisp-provide)
            (nelisp-provide feature))
          (unless (featurep feature)
            (provide feature)))
        (nelisp-artifact--load-profile-log "provide-features" feature-start
                                           (list :count (length features)))))
    (nelisp-artifact--load-profile-log "fast-total" total-start)
    last))

(defun nelisp-artifact--read-payload (artifact-path)
  "Read and parse ARTIFACT-PATH, returning its payload plist."
  (nelisp-artifact--parse-payload
   (nelisp-artifact--read-file-as-string artifact-path)
   artifact-path))

(defun nelisp-artifact--read-manifest-full (artifact-path)
  "Read ARTIFACT-PATH's sibling manifest with the full private plist reader."
  (let* ((manifest-path (nelisp-artifact--sibling-manifest-path artifact-path))
         (source (nelisp-artifact--read-file-as-string manifest-path)))
    (nelisp-artifact--read-one-private-form source manifest-path)))

(defun nelisp-artifact--native-install-metadata (section)
  "Return the wrapper-install subset of native SECTION metadata."
  (list :symbols (nelisp-artifact--native-section-get section :symbols)
        :defuns (nelisp-artifact--native-section-get section :defuns)))

(defun nelisp-artifact--private-list-key-positions
    (source start end keys label &optional first-match allow-truncated)
  "Find generated top-level KEYS in the list between START and END.
The scanner ignores strings, comments, and nested lists.  KEYS entries may be
keyword symbols or precomputed (KEYWORD . NAME) pairs; hot standalone paths
use pairs so `symbol-name' is not called inside the scan loop.  Returned
positions are key token starts.  When FIRST-MATCH is non-nil, stop after
finding the first member of KEYS.  LABEL is used when the bounded list is malformed.
When ALLOW-TRUNCATED is non-nil, END may cut through a later value; positions
already found before that bounded header limit are still returned."
  (unless (and (< start end) (= (aref source start) ?\())
    (error "invalid private metadata list in %s" label))
  (let ((i start)
        (depth 0)
        (in-string nil)
        (escaped nil)
        (remaining (copy-sequence keys))
        (positions nil))
    (while (and (< i end) remaining)
      (let ((ch (aref source i)))
        (cond
         (in-string
          (cond
           (escaped (setq escaped nil))
           ((= ch ?\\) (setq escaped t))
           ((= ch ?\") (setq in-string nil))))
         ((= ch ?\") (setq in-string t))
         ((= ch ?\;)
          (while (and (< i end) (not (= (aref source i) ?\n)))
            (setq i (1+ i))))
         ((= ch ?\() (setq depth (1+ depth)))
         ((= ch ?\)) (setq depth (1- depth)))
         ((and (= depth 1) (= ch ?:))
          (let ((token-end i))
            (while (and (< token-end end)
                        (let ((c (aref source token-end)))
                          (not (or (= c ?\s) (= c ?\t)
                                   (= c ?\n) (= c ?\r) (= c ?\))))))
              (setq token-end (1+ token-end)))
            (let ((token (substring source i token-end))
                  (rest remaining)
                  (key nil)
                  (matched nil))
              (while (and rest (null key))
                (let ((entry (car rest)))
                  (when (equal token
                               (if (consp entry)
                                   (cdr entry)
                                 (symbol-name entry)))
                    (setq matched entry)
                    (setq key (if (consp entry) (car entry) entry))))
                (setq rest (cdr rest)))
              (when key
                (setq positions (cons (cons key i) positions))
                (setq remaining
                      (and (not first-match)
                           (delq matched remaining)))))
            (setq i (1- token-end))))))
      (setq i (1+ i)))
    (when (and (not allow-truncated)
               (or in-string (< depth 0)))
      (error "malformed private metadata list in %s" label))
    (nreverse positions)))

(defun nelisp-artifact--read-private-list-field
    (source keyword key-pos end label)
  "Read bounded list field KEYWORD at KEY-POS before END in SOURCE."
  (let* ((needle (concat (symbol-name keyword) " "))
         (value-pos (nelisp-read--skip-ws
                     source (+ key-pos (length needle)))))
    (unless (< value-pos end)
      (error "missing private metadata value for %S in %s" keyword label))
    (if (= (aref source value-pos) ?\()
        (let ((value-end (nelisp-artifact--private-item-end
                          source value-pos end label)))
          (nelisp-artifact--read-private-item source value-pos value-end))
      (nelisp-artifact--read-private-symbol-token
       source keyword label nil key-pos))))

(defun nelisp-artifact--private-field-value-end
    (source value-pos limit label)
  "Return the end of one generated field value before LIMIT."
  (if (= (aref source value-pos) ?\()
      (nelisp-artifact--private-item-end source value-pos limit label)
    (let ((i value-pos))
      (while (and (< i limit)
                  (let ((ch (aref source i)))
                    (not (or (= ch ?\s) (= ch ?\t)
                             (= ch ?\n) (= ch ?\r) (= ch ?\))))))
        (setq i (1+ i)))
      i)))

(defun nelisp-artifact--read-private-native-section-install-metadata
    (source start limit label)
  "Read wrapper-install metadata and end position from section at START.
LIMIT bounds the containing `:native' or `:native-sections' value.
Large relocation and compile-report values are scanned but never passed to the
private sexp reader.  Return (METADATA . END)."
  (let* ((fast-search (fboundp 'nelisp--string-search))
         (symbols-fast
          (and fast-search
               (nelisp-artifact--string-search-literal
                ":symbols " source start)))
         (defuns-fast
          (and fast-search
               (nelisp-artifact--string-search-literal
                ":defuns " source start)))
         (positions
          (if (and symbols-fast defuns-fast
                   (< symbols-fast limit) (< defuns-fast limit))
              (list (cons :symbols symbols-fast)
                    (cons :defuns defuns-fast))
            (let ((end (nelisp-artifact--private-item-end
                        source start limit label)))
              (nelisp-artifact--private-list-key-positions
               source start end '(:symbols :defuns) label))))
         (symbols-pos (cdr (assq :symbols positions)))
         (defuns-pos (cdr (assq :defuns positions)))
         (defuns-value-pos
          (and defuns-pos
               (nelisp-read--skip-ws
                source (+ defuns-pos (length ":defuns ")))))
         (defuns-end
          (and defuns-value-pos
               (nelisp-artifact--private-field-value-end
                source defuns-value-pos limit label)))
         (section-close
          (and defuns-end (nelisp-read--skip-ws source defuns-end))))
    (unless (and symbols-pos defuns-pos)
      (error "native section lacks install metadata in %s" label))
    (unless (and section-close (< section-close limit)
                 (= (aref source section-close) ?\)))
      (error "native section has trailing metadata after :defuns in %s" label))
    (cons
     (list :symbols
           (nelisp-artifact--read-private-list-field
            source :symbols symbols-pos section-close label)
           :defuns
           (nelisp-artifact--read-private-list-field
            source :defuns defuns-pos section-close label))
     (1+ section-close))))

(defun nelisp-artifact--read-private-native-metadata
    (source keyword label &optional missing-ok start)
  "Read generated native KEYWORD metadata without one giant reader form.
Each `:native-sections' element is scanned independently, and only its
`:symbols' and `:defuns' values enter the private reader.  This keeps a
many-megabyte legacy manifest linear and bounded without materializing compile
reports or relocation data.  New manifests already contain this reduced form."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok start))
        (len (length source))
        (sections nil))
    (if (null pos)
        nelisp-artifact--missing-key
      (setq pos (nelisp-read--skip-ws source pos))
      (if (eq keyword :native)
          (car (nelisp-artifact--read-private-native-section-install-metadata
                source pos len label))
        (unless (and (< pos len) (= (aref source pos) ?\())
          (error "invalid :native-sections list in %s" label))
        (setq pos (1+ pos))
        (while (progn
                 (setq pos (nelisp-read--skip-ws source pos))
                 (and (< pos len) (not (= (aref source pos) ?\)))))
          (let* ((parsed
                  (nelisp-artifact--read-private-native-section-install-metadata
                   source pos len label))
                 (section (car parsed))
                 (end (cdr parsed)))
            (setq sections
                  (cons section sections))
            (setq pos end)))
        (unless (and (< pos len) (= (aref source pos) ?\)))
          (error "unterminated :native-sections list in %s" label))
        (nreverse sections)))))

(defun nelisp-artifact--private-top-level-key-positions (source &optional keys)
  "Return an alist of generated top-level plist keys and their source positions.
SOURCE is scanned once, ignoring strings and nested lists.  Each position is
the key token start, suitable as START for the private token readers."
  (if (and keys (fboundp 'nelisp--string-search))
      (let ((positions nil)
            (sections-pos
             (nelisp-artifact--string-search-literal
              ":native-sections " source)))
        (dolist (key keys)
          (let ((pos (nelisp-artifact--string-search-literal
                      (concat (symbol-name key) " ") source)))
            (when (and pos
                       (not (and (eq key :native)
                                 sections-pos
                                 (< sections-pos pos))))
              (setq positions (cons (cons key pos) positions)))))
        (nreverse positions))
    (let ((i 0)
        (len (length source))
        (depth 0)
        (in-string nil)
        (escaped nil)
        (positions nil))
    (while (< i len)
      (let ((ch (aref source i)))
        (cond
         (in-string
          (cond
           (escaped (setq escaped nil))
           ((= ch ?\\) (setq escaped t))
           ((= ch ?\") (setq in-string nil))))
         ((= ch ?\") (setq in-string t))
         ((= ch ?\;)
          (while (and (< i len) (not (= (aref source i) ?\n)))
            (setq i (1+ i))))
         ((= ch ?\() (setq depth (1+ depth)))
         ((= ch ?\)) (setq depth (1- depth)))
         ((and (= depth 1) (= ch ?:))
          (let ((start i))
            (while (and (< i len)
                        (let ((c (aref source i)))
                          (not (or (= c ?\s) (= c ?\t)
                                   (= c ?\n) (= c ?\r) (= c ?\))))))
              (setq i (1+ i)))
            (setq positions
                  (cons (cons (intern (substring source start i)) start)
                        positions))
            (setq i (1- i))))))
      (setq i (1+ i)))
      (nreverse positions))))

(defun nelisp-artifact--read-manifest-fast (artifact-path &optional keys)
  "Read ARTIFACT-PATH's sibling manifest via generated-key scanner.
When KEYS is non-nil, read only those top-level keys."
  (let* ((wanted-keys
          (or keys
              '(:format :kind :artifact-format :artifact-class
                :runtime-abi :artifact-sha256 :artifact-size
                :nelisp-version :target :source :runtime-image
                :preloads :load-path :features :top-level-count
                :compiler :native-policy :native :native-sections :native-report
                :emacs-compat :entry)))
         (manifest-path (nelisp-artifact--sibling-manifest-path artifact-path))
         (source (nelisp-artifact--read-file-as-string manifest-path))
         (manifest nil)
         (positions (nelisp-artifact--private-top-level-key-positions
                     source wanted-keys))
         (compiler-text (prin1-to-string (nelisp-artifact--compiler-plist))))
    (dolist (key wanted-keys)
      (let ((start (cdr (assq key positions))))
        (setq manifest
              (nelisp-artifact--plist-put-present
               manifest key
               (if (null start)
                   nelisp-artifact--missing-key
                 (cond
              ((memq key '(:format :kind :artifact-format :artifact-class
                           :native-policy))
               (nelisp-artifact--read-private-symbol-token
                source key manifest-path t start))
              ((memq key '(:runtime-abi :artifact-sha256 :nelisp-version
                           :target))
               (nelisp-artifact--read-private-string-token
                source key manifest-path t start))
              ((memq key '(:artifact-size :top-level-count))
               (nelisp-artifact--read-private-integer-token
                source key manifest-path t start))
              ((eq key :compiler)
               (if (nelisp-artifact--string-search-literal
                    (concat ":compiler " compiler-text)
                    source start)
                   (nelisp-artifact--compiler-plist)
                 (nelisp-artifact--read-private-keyword-value
                  source key manifest-path t start)))
              ((eq key :native)
               (nelisp-artifact--read-private-native-metadata
                source key manifest-path t start))
              ((eq key :native-sections)
               (nelisp-artifact--read-private-native-metadata
                source key manifest-path t start))
              (t
               (nelisp-artifact--read-private-keyword-value
                source key manifest-path t start))))))))
    manifest))

(defun nelisp-artifact--read-manifest-for-load (artifact-path)
  "Read only sidecar fields needed by private artifact load/validation.
Native wrapper metadata is deliberately omitted: fast `.neln' loading reads
the integrity-covered serialized sections from the artifact, while runtimes
without a native call boundary simply replay the portable module."
  (if nelisp-artifact-fast-private-read
      (nelisp-artifact--read-manifest-fast
           artifact-path
           '(:format :kind :artifact-format :artifact-class :runtime-abi
             :artifact-sha256 :artifact-size :nelisp-version :source
             :runtime-image :preloads :load-path :compiler))
    (nelisp-artifact--read-manifest-full artifact-path)))

(defun nelisp-artifact--activate-load-paths (paths)
  "Prepend valid manifest PATHS to global `load-path' without duplicates.
Manifest order has priority and is preserved.  Existing `load-path' entries
follow in their original order after duplicate removal.  Nil, non-string, and
empty manifest entries are ignored.  The update is intentionally persistent so
lazy `require' calls after artifact replay see the same dependency paths."
  (let ((merged nil))
    (when (listp paths)
      (dolist (path paths)
        (when (and (stringp path)
                   (> (length path) 0)
                   (not (member path merged)))
          (setq merged (cons path merged)))))
    (setq merged (nreverse merged))
    (dolist (path load-path)
      (unless (member path merged)
        (setq merged (append merged (list path)))))
    (setq load-path merged)))

(defun nelisp-artifact--validate-input-record (rec label artifact-path)
  "Validate manifest input REC freshness for LABEL and ARTIFACT-PATH."
  (let ((path (and rec (or (plist-get rec :truename)
                           (plist-get rec :path))))
        (want (and rec (plist-get rec :sha256))))
    (when (and path want (file-exists-p path))
      (let ((want-size (plist-get rec :size))
            (want-mtime (plist-get rec :mtime))
            (want-ctime (plist-get rec :ctime)))
        (unless (and want-size want-mtime want-ctime
                     (equal want-size (nelisp-artifact--file-size path))
                     (equal want-mtime (nelisp-artifact--file-mtime path))
                     (equal want-ctime (nelisp-artifact--file-ctime path)))
          (unless (equal want
                         ;; Freshness is defined by the source file's raw
                         ;; bytes.  Decoding and re-encoding here can change
                         ;; UTF-8 bytes in the standalone runtime.
                         (nelisp-artifact--sha256-file path))
            (signal 'nelisp-artifact-stale
                    (list (format "%s changed since compile" label)
                          path artifact-path))))))))

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
    (let ((manifest (nelisp-artifact--read-manifest-for-load artifact-path)))
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
      (let ((want (plist-get manifest :artifact-sha256))
            (want-size (plist-get manifest :artifact-size)))
        (unless (and want (stringp want))
          (signal 'nelisp-artifact-invalid
                  (list "manifest missing artifact integrity hash" manifest-path)))
        (unless (and nelisp-artifact-fast-integrity-validation
                     want-size
                     (equal want-size
                            (nelisp-artifact--file-size artifact-path)))
          (unless (equal want (secure-hash 'sha256 artifact-content))
            (signal 'nelisp-artifact-invalid
                    (list "artifact sha256 mismatch (corrupt/truncated)"
                          artifact-path)))))
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
      (nelisp-artifact--validate-input-record
       (plist-get manifest :runtime-image) "runtime image" artifact-path)
      (dolist (rec (plist-get manifest :preloads))
        (nelisp-artifact--validate-input-record rec "preload" artifact-path))
      ;; Source freshness uses the same raw-byte digest fallback as runtime
      ;; images and preloads.  Standalone and host mtime representations can
      ;; differ even for an unchanged file; decoding UTF-8 and hashing the
      ;; resulting character string would then produce a false stale result.
      (nelisp-artifact--validate-input-record
       (plist-get manifest :source) "source" artifact-path)
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
     ((string-suffix-p ".elc" full-path)
      (let ((manifest (nelisp-artifact--validate-elc full-path)))
        (nelisp-artifact--activate-load-paths
         (plist-get manifest :load-path))
        (load full-path nil t))
      (setq nelisp-artifact--loaded (cons full-path nelisp-artifact--loaded))
      nil)
     ;; §6.1/§6.4 .nelc/.neln: validate, then replay the bytecode module.
     (t
      (let* ((total-start (nelisp-artifact--profile-time))
             (read-start total-start)
             (content (nelisp-artifact--read-file-as-string full-path)))
        (nelisp-artifact--load-profile-log "read-artifact" read-start
                                           (list :bytes (length content)))
        (let* ((validate-start (nelisp-artifact--profile-time))
               (manifest (nelisp-artifact--validate full-path content)))
          (nelisp-artifact--activate-load-paths
           (plist-get manifest :load-path))
          (nelisp-artifact--load-profile-log "validate" validate-start)
          (if nelisp-artifact-fast-private-read
              (let ((last (nelisp-artifact--load-private-fast
                           full-path content manifest)))
                (setq nelisp-artifact--loaded
                      (cons full-path nelisp-artifact--loaded))
                (nelisp-artifact--load-profile-log "load-total"
                                                   total-start
                                                   '(:path fast))
                last)
            (let* ((payload (nelisp-artifact--parse-payload content full-path))
                   (module (plist-get payload :module-init))
                   (provided-features (plist-get payload :features))
                   (native (or (plist-get payload :native-sections)
                               (plist-get payload :native)))
                   (last nil))
              ;; Replay the module onto the NeLisp runtime: install
              ;; precompiled bytecode closures into the function table,
              ;; `nelisp-eval' the remaining top-level effects.
              (dolist (item module)
                (setq last (nelisp-artifact--replay-module-item item)))
              (when (and native nelisp-artifact-native-dispatch-enabled)
                (when (fboundp 'nelisp--native-call-boundary)
                  (nelisp-artifact--register-native-sections
                   full-path
                   (nelisp-artifact--native-sections-from-native native)))
                (nelisp-artifact--install-native-functions full-path native))
              (dolist (feature provided-features)
                (when (fboundp 'nelisp-provide)
                  (nelisp-provide feature))
                (unless (featurep feature)
                  (provide feature)))
              (setq nelisp-artifact--loaded
                    (cons full-path nelisp-artifact--loaded))
              (nelisp-artifact--load-profile-log "load-total"
                                                 total-start
                                                 '(:path full-parse))
              last))))))))

(defun nelisp-artifact-read-manifest (artifact-path)
  "Read the sibling manifest for ARTIFACT-PATH."
  (nelisp-artifact--read-manifest-full artifact-path))

(defun nelisp-artifact--read-manifest-for-inspect (artifact-path)
  "Read diagnostic manifest fields without materializing native sections."
  (if nelisp-artifact-fast-private-read
      (nelisp-artifact--read-manifest-fast
           artifact-path
           '(:format :kind :artifact-format :artifact-class :runtime-abi
             :artifact-sha256 :artifact-size :nelisp-version :target :source
             :runtime-image :preloads :load-path :features :top-level-count
             :compiler :native-policy :native-report :emacs-compat :entry))
    (nelisp-artifact--read-manifest-full artifact-path)))

(defun nelisp-artifact--read-manifest-for-audit (artifact-path)
  "Read only manifest fields needed by `audit-elisp-artifacts'."
  (if nelisp-artifact-fast-private-read
      (condition-case nil
          (nelisp-artifact--read-manifest-fast
           artifact-path
           '(:kind :source :native-report))
        (error
         (nelisp-artifact-read-manifest artifact-path)))
    (nelisp-artifact-read-manifest artifact-path)))

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
	                      :mtime (nelisp-artifact--file-mtime source-path)
	                      :ctime (nelisp-artifact--file-ctime source-path))
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
	      ;; source freshness (size/mtime/ctime fast path, sha256 fallback).
      (let* ((src (plist-get manifest :source))
             (spath (and src (or (plist-get src :truename) (plist-get src :path))))
             (swant (and src (plist-get src :sha256))))
        (when (and spath swant (file-exists-p spath))
	          (unless (and (equal (plist-get src :size)
	                              (nelisp-artifact--file-size spath))
	                       (equal (plist-get src :mtime)
	                              (nelisp-artifact--file-mtime spath))
	                       (equal (plist-get src :ctime)
	                              (nelisp-artifact--file-ctime spath)))
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

(defun nelisp-artifact--artifact-kind-from-suffix (artifact-path)
  "Return artifact kind from ARTIFACT-PATH suffix, or nil when unknown."
  (cond
   ((string-suffix-p ".neln" artifact-path) 'neln)
   ((string-suffix-p ".nelc" artifact-path) 'nelc)
   ((string-suffix-p ".elc" artifact-path) 'elc)
   (t nil)))

(defun nelisp-artifact-source-artifact-path (source-path kind)
  "Return the artifact path for SOURCE-PATH and KIND.
When `nelisp-artifact-cache-directory' is nil, this remains the historical
adjacent path.  Otherwise a stable hashed cache subdirectory is used."
  (if (nelisp-artifact--source-cache-root source-path)
      (nelisp-artifact--source-artifact-path-in-cache source-path kind)
    (concat (expand-file-name source-path) "." (symbol-name kind))))

(defun nelisp-artifact--source-artifact-candidates (source-path kinds)
  "Return adjacent artifact candidates for SOURCE-PATH in KINDS order."
  (mapcar (lambda (kind)
            (nelisp-artifact-source-artifact-path source-path kind))
          (or kinds '(neln nelc))))

(defun nelisp-artifact-load-source-file (source-path &optional kinds)
  "Load the first valid adjacent artifact for SOURCE-PATH.
Returns a plist `(:artifact PATH :value VALUE)' on hit, or nil on miss.
Invalid or stale artifacts are skipped so callers can fall back to source."
  (let ((candidates (nelisp-artifact--source-artifact-candidates
                     source-path kinds))
        (hit nil))
    (while (and candidates (not hit))
      (let ((artifact (car candidates)))
        (when (file-exists-p artifact)
          (condition-case nil
              (setq hit
                    (list :artifact artifact
                          :value (nelisp-artifact-load-file artifact)))
            (nelisp-artifact-invalid nil)
            (nelisp-artifact-stale nil))))
      (setq candidates (cdr candidates)))
    hit))

(defun nelisp-artifact-load-or-compile-source-file
    (source-path &optional kinds kind target load-paths preloads native-policy)
  "Load a fresh adjacent artifact for SOURCE-PATH, compiling on miss.
KIND defaults to `neln'.  This is the generic on-demand path used by
`nelisp-load-file' when `nelisp-load-auto-compile-artifacts' is non-nil:
all `.el' files can share the same artifact policy without each caller
special-casing native cache refresh."
  (or (nelisp-artifact-load-source-file source-path kinds)
      (when (nelisp-core-file-readable-p source-path)
        (let* ((kind (or kind 'neln))
               (artifact (nelisp-artifact-source-artifact-path source-path kind)))
          (condition-case nil
              (progn
	                (nelisp-artifact-compile-file
	                 source-path artifact nil target load-paths preloads nil kind
	                 native-policy)
                (list :artifact artifact
                      :value (nelisp-artifact-load-file artifact)))
            (error nil))))))

(defun nelisp-artifact-load-source-or-source-file
    (source-path &optional auto-compile kind target load-paths preloads native-policy)
  "Load SOURCE-PATH through the generic artifact policy, then source fallback.
Adjacent `.neln' and `.nelc' artifacts are tried first.  When AUTO-COMPILE is
non-nil, a missing/stale artifact is regenerated with KIND (default `neln')
before loading.  If no usable artifact can be loaded, the original source is
loaded directly with artifact probing disabled to avoid recursive retries."
  (or (if auto-compile
          (nelisp-artifact-load-or-compile-source-file
           source-path '(neln nelc) (or kind 'neln) target load-paths preloads
           native-policy)
        (nelisp-artifact-load-source-file source-path '(neln nelc)))
      (when (nelisp-core-file-readable-p source-path)
        (let ((nelisp-load-prefer-artifacts nil)
              (nelisp-load-auto-compile-artifacts nil))
          (list :artifact nil
                :value (nelisp-load-file source-path))))))

(defun nelisp-artifact-native-exec (artifact-path symbol args)
  "Doc 142 §6.4 native EXEC: run the native SYMBOL embedded in a `.neln'.
Extracts the ET_REL object from ARTIFACT-PATH's `:native' section, links
it with a generated integer-ABI driver, runs it with integer ARGS, and
returns the int64 result.  This is the first native-execution spike: it
works for the reloc-free leaf functions AOT emits today (plain C
integer ABI, no boundary slots).  The host C toolchain (cc + objcopy)
acts as the loader; an in-process standalone mmap+reloc loader for the
general boundary-ABI case is the remaining §6.4 work.

Signals an error when the toolchain is missing, the artifact has no
native object, or SYMBOL is not one of its native functions."
  (let ((cc (or (executable-find "cc") (executable-find "gcc")))
        (objcopy (executable-find "objcopy")))
    (unless (and cc objcopy)
      (error "native-exec needs cc + objcopy on PATH"))
    (let* ((manifest (nelisp-artifact-read-manifest artifact-path))
           (native
            (nelisp-artifact--serialized-native-section-for-symbol
             artifact-path symbol)))
      (unless (eq (plist-get manifest :kind) 'neln)
        (error "native-exec requires a .neln artifact, got %S"
               (plist-get manifest :kind)))
      (unless native
        (error "native symbol %s not in artifact %s" symbol artifact-path))
      (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec"))
             (obj (expand-file-name "mod.o" dir))
             (obj2 (expand-file-name "mod-c.o" dir))
             (csrc (expand-file-name "drv.c" dir))
             (exe (expand-file-name "run" dir))
             (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol))
             (argc (length args)))
        (unwind-protect
            (progn
              (nelisp-artifact--write-native-object-file
               artifact-path obj native)
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
                (insert (format "printf(\"%%ld\\n\",%s(%s));return 0;}\n"
                                csym
                                (mapconcat (lambda (i)
                                             (format "atol(v[%d])" i))
                                           (number-sequence 1 argc)
                                           ","))))
              (unless (eq 0 (call-process cc nil nil nil "-O2" "-o" exe csrc obj2))
                (error "native link failed for %s" symbol))
              (with-temp-buffer
                (apply #'call-process exe nil t nil
                       (mapcar #'number-to-string args))
                (string-to-number (string-trim (buffer-string)))))
          (delete-directory dir t))))))

(defun nelisp-artifact--native-exec-cache-root ()
  "Return the native exec cache root directory."
  (let* ((xdg (and (fboundp 'getenv) (getenv "XDG_CACHE_HOME")))
         (home (and (fboundp 'getenv) (getenv "HOME")))
         (base (cond
                ((and xdg (> (length xdg) 0)) xdg)
                ((and home (> (length home) 0))
                 (expand-file-name ".cache" home))
                ((and (boundp 'temporary-file-directory)
                      (stringp temporary-file-directory)
                      (> (length temporary-file-directory) 0))
                 temporary-file-directory)
                (t "/tmp"))))
    (expand-file-name "nelisp/native-exec" base)))

(defun nelisp-artifact--small-string-hash (text)
  "Return a portable decimal hash for TEXT.
This is used only for standalone native-exec cache directory names.  The
standalone `secure-hash' compatibility path may be unavailable or too slow
on the hot path, so use a small deterministic rolling hash there."
  (let ((i 0)
        (n (length text))
        (h 5381))
    (while (< i n)
      (setq h (mod (+ (* h 33) (aref text i)) 1000000007))
      (setq i (1+ i)))
    (number-to-string h)))

(defun nelisp-artifact--native-exec-file-fingerprint (artifact-path)
  "Return a cheap cache fingerprint for ARTIFACT-PATH."
  (let ((attrs (and (fboundp 'file-attributes)
                    (file-attributes (expand-file-name artifact-path)
                                     'string))))
    (if attrs
        (concat (prin1-to-string (nth 7 attrs))
                ":"
                (prin1-to-string (nth 5 attrs)))
      "unknown")))

(defun nelisp-artifact--native-exec-arg-signature (args)
  "Return a cache signature for native exec ARGS kinds."
  (mapconcat (lambda (arg)
               (cond
                ((integerp arg) "i")
                ((stringp arg) "s")
                (t "x")))
             args
             ""))

(defun nelisp-artifact--native-exec-cache-key
    (artifact-path symbol argc &optional variant arg-signature)
  "Return a stable cache key for ARTIFACT-PATH, SYMBOL, and ARGC."
  (let ((artifact (expand-file-name artifact-path)))
    ;; This runs before cache-hit detection.  Do not parse the manifest here:
    ;; standalone manifest/plist parsing is still expensive enough to erase
    ;; the benefit of a cached native driver.  Size + mtime + artifact path are
    ;; sufficient to invalidate the private dev-loop executable cache; the
    ;; validating native paths still parse the manifest on fallback/error.
    (let* ((seed (concat
                  "neln-cache|"
                  (or variant "fast")
                  "|"
                  artifact
                  "|"
                  (nelisp-artifact--native-exec-file-fingerprint artifact)
                  "|"
                  (if (symbolp symbol) (symbol-name symbol) symbol)
                  "|"
                  (number-to-string argc)
                  "|"
                  (or arg-signature ""))))
      (concat "sx-" (nelisp-artifact--small-string-hash seed)))))

(defun nelisp-artifact--native-exec-cache-exe
    (artifact-path symbol argc &optional variant arg-signature)
  "Return the cached native fast executable path for ARTIFACT-PATH."
  (expand-file-name
   "run"
   (expand-file-name
    (nelisp-artifact--native-exec-cache-key
     artifact-path symbol argc variant arg-signature)
    (nelisp-artifact--native-exec-cache-root))))

(defun nelisp-artifact--native-fast-driver-c (csym argc)
  "Return the integer ABI fast driver C source for CSYM with ARGC."
  (concat
   "#include <stdlib.h>\n#include <stdio.h>\n"
   (format "extern long %s(%s);\n" csym
           (if (= argc 0) "void"
             (mapconcat (lambda (_) "long") (make-list argc nil) ",")))
   "int main(int c,char**v){(void)c;"
   (format "printf(\"%%ld\\n\",%s(%s));return 0;}\n"
           csym
           (let ((i 0))
             (mapconcat
              (lambda (_)
                (setq i (1+ i))
                (format "atol(v[%d])" i))
              (make-list argc nil)
              ",")))))

(defun nelisp-artifact--native-exec-fast-build (artifact-path symbol argc exe)
  "Build EXE for ARTIFACT-PATH native SYMBOL with ARGC integer args."
  (let ((cc (or (executable-find "cc") (executable-find "gcc")))
        (objcopy (executable-find "objcopy"))
        (sh (executable-find "sh")))
    (unless (and cc objcopy)
      (error "native-exec fast path needs cc + objcopy on PATH"))
    (let* ((native
            (nelisp-artifact--serialized-native-section-for-symbol
             artifact-path symbol))
           (dir (nelisp-artifact--make-temp-directory "neln-exec-fast"))
           (obj (expand-file-name "mod.o" dir))
           (obj2 (expand-file-name "mod-c.o" dir))
           (csrc (expand-file-name "drv.c" dir))
           (built-exe (expand-file-name "run" dir))
           (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol)))
      (unless native
        (error "native symbol %s not in artifact %s" symbol artifact-path))
      (unwind-protect
          (progn
            (nelisp-artifact--write-native-object-file
             artifact-path obj native)
            ;; ELF symbols carry the elisp name (with dashes); rename to a
            ;; C identifier so the driver can reference it.
            (unless (eq 0
                        (if sh
                            (call-process
                             sh nil nil nil "-c"
                             "exec \"$1\" \"--redefine-sym=$2=$3\" \"$4\" \"$5\" >/dev/null 2>&1"
                             "nelisp-native-fast-objcopy"
                             objcopy symbol csym obj obj2)
                          (call-process objcopy nil nil nil
                                        (format "--redefine-sym=%s=%s"
                                                symbol csym)
                                        obj obj2)))
              (error "objcopy symbol rename failed for %s" symbol))
            (with-temp-file csrc
              (insert (nelisp-artifact--native-fast-driver-c csym argc)))
            (unless (eq 0
                        (if sh
                            (call-process
                             sh nil nil nil "-c"
                             "exec \"$1\" -O2 -o \"$2\" \"$3\" \"$4\" >/dev/null 2>&1"
                             "nelisp-native-fast-cc"
                             cc built-exe csrc obj2)
                          (call-process cc nil nil nil "-O2" "-o"
                                        built-exe csrc obj2)))
              (error "native fast link failed for %s" symbol))
            (make-directory (file-name-directory exe) t)
            (rename-file built-exe exe t)
            (nelisp-artifact--note-native-dispatch
             (list :event 'native-cache
                   :symbol (intern symbol)
                   :mode 'build
                   :exe exe))
            exe)
        (delete-directory dir t)))))

(defun nelisp-artifact--native-exec-fast-exe (artifact-path symbol argc)
  "Return a linked executable for native fast ARTIFACT-PATH/SYMBOL/ARGC."
  (let ((exe (nelisp-artifact--native-exec-cache-exe artifact-path symbol argc)))
    (if (and nelisp-artifact-native-exec-cache-enabled
             (file-exists-p exe))
        (progn
          (nelisp-artifact--note-native-dispatch
           (list :event 'native-cache
                 :symbol (intern symbol)
                 :mode 'hit
                 :exe exe))
          exe)
      (nelisp-artifact--native-exec-fast-build
       artifact-path symbol argc exe))))

(defun nelisp-artifact-native-exec-fast-simple-uncached (artifact-path symbol args)
  "Fast CLI native EXEC for externless integer-ABI `.neln' functions.
This deliberately skips manifest/plist validation and only performs the
minimum work needed by `native-exec-elisp-artifact': extract the embedded
object, rename SYMBOL for C linkage, link a small driver, and run it.
The validated `nelisp-artifact-native-exec' path remains the fallback
for diagnostics, symbol checks, and non-simple artifacts."
  (let ((cc (or (executable-find "cc") (executable-find "gcc")))
        (objcopy (executable-find "objcopy"))
        (sh (executable-find "sh")))
    (unless (and cc objcopy)
      (error "native-exec fast path needs cc + objcopy on PATH"))
    (let* ((native
            (nelisp-artifact--serialized-native-section-for-symbol
             artifact-path symbol))
           (dir (nelisp-artifact--make-temp-directory "neln-exec-fast"))
           (obj (expand-file-name "mod.o" dir))
           (obj2 (expand-file-name "mod-c.o" dir))
           (csrc (expand-file-name "drv.c" dir))
           (exe (expand-file-name "run" dir))
           (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol))
           (argc (length args)))
      (unless native
        (error "native symbol %s not in artifact %s" symbol artifact-path))
      (unwind-protect
          (progn
            (nelisp-artifact--write-native-object-file
             artifact-path obj native)
            ;; ELF symbols carry the elisp name (with dashes); rename to a
            ;; C identifier so the driver can reference it.
            (unless (eq 0
                        (if sh
                            (call-process
                             sh nil nil nil "-c"
                             "exec \"$1\" \"--redefine-sym=$2=$3\" \"$4\" \"$5\" >/dev/null 2>&1"
                             "nelisp-native-fast-objcopy"
                             objcopy symbol csym obj obj2)
                          (call-process objcopy nil nil nil
                                        (format "--redefine-sym=%s=%s"
                                                symbol csym)
                                        obj obj2)))
              (error "objcopy symbol rename failed for %s" symbol))
            (with-temp-file csrc
              (insert "#include <stdlib.h>\n#include <stdio.h>\n")
              (insert (format "extern long %s(%s);\n" csym
                              (if (= argc 0) "void"
                                (mapconcat (lambda (_) "long") args ","))))
              (insert "int main(int c,char**v){(void)c;")
              (insert (format "printf(\"%%ld\\n\",%s(%s));return 0;}\n"
                              csym
                              (mapconcat (lambda (i)
                                           (format "atol(v[%d])" i))
                                         (number-sequence 1 argc)
                                         ","))))
            (unless (eq 0
                        (if sh
                            (call-process
                             sh nil nil nil "-c"
                             "exec \"$1\" -O2 -o \"$2\" \"$3\" \"$4\" >/dev/null 2>&1"
                             "nelisp-native-fast-cc"
                             cc exe csrc obj2)
                          (call-process cc nil nil nil "-O2" "-o" exe csrc obj2)))
              (error "native fast link failed for %s" symbol))
            (with-temp-buffer
              (apply #'call-process exe nil t nil
                     (mapcar #'number-to-string args))
              (string-to-number (string-trim (buffer-string)))))
        (delete-directory dir t)))))

(defun nelisp-artifact--shell-quote (text)
  "Return POSIX single-quoted TEXT."
  (let ((i 0)
        (n (length text))
        (out "'"))
    (while (< i n)
      (let ((ch (aref text i)))
        (setq out
              (concat out
                      (if (= ch ?')
                          "'\\''"
                        (string ch)))))
      (setq i (1+ i)))
    (concat out "'")))

(defun nelisp-artifact--native-exec-run-captured-stdout (exe symbol args)
  "Run native EXE with ARGS, returning raw stdout for SYMBOL.
Standalone `call-process' buffer destinations are not reliable enough for
this hot path.  Use shell-level redirection without extra `sh -c' argv
indirection; the standalone process layer does not pass those extra args
compatibly enough for `$1' / `$@' scripts."
  (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec-run"))
         (run-out (expand-file-name "stdout" dir))
         (run-err (expand-file-name "stderr" dir))
         (argv (mapcar #'number-to-string args))
         (sh (and (fboundp 'executable-find) (executable-find "sh"))))
    (unwind-protect
        (let* ((status
                (if sh
                    (call-process
                     sh nil nil nil "-c"
                     (concat
                      (mapconcat #'nelisp-artifact--shell-quote
                                 (cons exe argv)
                                 " ")
                      " >" (nelisp-artifact--shell-quote run-out)
                      " 2>" (nelisp-artifact--shell-quote run-err)))
                  (with-temp-buffer
                    (let ((status (apply #'call-process exe nil t nil argv)))
                      (write-region (buffer-string) nil run-out)
                      (write-region "" nil run-err)
                      status))))
               (stdout (if (file-exists-p run-out)
                           (nelisp-artifact--read-file-as-string run-out)
                         ""))
               (stderr (if (file-exists-p run-err)
                           (nelisp-artifact--read-file-as-string run-err)
                         "")))
          (unless (eq status 0)
            (error "native fast run failed for %s (exit %s): %s"
                   symbol status (string-trim stderr)))
          (unless (and (stringp stdout) (> (length stdout) 0))
            (error "native fast run produced no output for %s" symbol))
          stdout)
      (delete-directory dir t))))

(defun nelisp-artifact--native-exec-run-captured (exe symbol args)
  "Run native EXE with ARGS, returning parsed stdout for SYMBOL."
  (nelisp-artifact--native-exec-parse-stdout
   (nelisp-artifact--native-exec-run-captured-stdout exe symbol args)))

(defun nelisp-artifact-native-exec-fast-simple (artifact-path symbol args)
  "Run native SYMBOL from ARTIFACT-PATH through the cached integer fast path."
  (let* ((argc (length args))
         (exe (nelisp-artifact--native-exec-fast-exe
               artifact-path symbol argc)))
    (nelisp-artifact--native-exec-run-captured exe symbol args)))

(defun nelisp-artifact-native-exec-fast-simple-stdout (artifact-path symbol args)
  "Run native SYMBOL from ARTIFACT-PATH and return raw stdout."
  (let* ((argc (length args))
         (exe (nelisp-artifact--native-exec-fast-exe
               artifact-path symbol argc)))
    (nelisp-artifact--native-exec-run-captured-stdout exe symbol args)))

(defun nelisp-artifact-native-exec-fast-simple-write-stdout
    (artifact-path symbol args)
  "Run native SYMBOL from ARTIFACT-PATH with stdout inherited by this process.
This is the standalone CLI fast path: it avoids reading the native
driver's output back into a Lisp string."
  (let* ((argc (length args))
         (exe (nelisp-artifact--native-exec-fast-exe
               artifact-path symbol argc))
         (argv (mapcar #'number-to-string args))
         (status (apply #'call-process exe nil nil nil argv)))
    (unless (eq status 0)
      (error "native fast run failed for %s (exit %s)" symbol status))
    status))

(defun nelisp-artifact--standalone-host-helper-native-exec-general
    (artifact-path symbol args)
  "Run general native exec through host Emacs when inside standalone.
Return (t . VALUE) when the helper was used successfully.  Return nil when the
helper is unavailable or fails, allowing callers to fall back to the standalone
implementation."
  (when (and (nelisp-artifact--standalone-runtime-p)
             (not (and (fboundp 'getenv)
                       (equal (getenv "NELISP_DISABLE_HOST_HELPER") "1"))))
    (let ((emacs (nelisp-artifact--host-helper-emacs))
          (sh (and (fboundp 'executable-find) (executable-find "sh"))))
      (when (and emacs sh)
        (let* ((root nelisp-artifact-standalone-repo-root)
               (out (nelisp-artifact--make-temp-path
                     "nelisp-native-helper" "out"))
               (err (nelisp-artifact--make-temp-path
                     "nelisp-native-helper" "err"))
               (arg-files nil)
               (arg-forms nil)
               (eval-form
                nil)
               (status nil)
               (result nil))
          (unwind-protect
              (progn
                (dolist (arg args)
                  (cond
                   ((integerp arg)
                    (setq arg-forms
                          (append arg-forms (list (number-to-string arg)))))
                   ((stringp arg)
                    (let ((arg-file
                           (nelisp-artifact--make-temp-path
                            "nelisp-native-helper-arg" "txt")))
                      (nelisp-artifact--write-file arg-file arg)
                      (setq arg-files (cons arg-file arg-files))
                      (setq arg-forms
                            (append
                             arg-forms
                             (list
                              (concat
                               "(nelisp-artifact--read-file-as-string "
                               (prin1-to-string arg-file)
                               ")"))))))
                   (t
                    (error "native-exec-general unsupported arg: %S" arg))))
                (setq eval-form
                      (concat
                       "(progn (setq load-prefer-newer t)"
                       " (require 'nelisp-artifact)"
                       " (prin1 (nelisp-artifact-native-exec-general "
                       (prin1-to-string artifact-path) " "
                       (prin1-to-string symbol) " "
                       "(list " (mapconcat #'identity arg-forms " ") ")"
                       ")) (terpri))"))
                (setq status
                      (call-process
                       sh nil nil nil "-c"
                       (concat
                        (nelisp-artifact--shell-quote emacs)
                        " -Q --batch"
                        " -L " (nelisp-artifact--shell-quote
                                (expand-file-name "lisp" root))
                        " -L " (nelisp-artifact--shell-quote
                                (expand-file-name "src" root))
                        " --eval "
                        (nelisp-artifact--shell-quote eval-form)
                        " >" (nelisp-artifact--shell-quote out)
                        " 2>" (nelisp-artifact--shell-quote err))))
                (if (eq status 0)
                    (progn
                      (setq result
                            (car (read-from-string
                                  (nelisp-artifact--read-file-as-string
                                   out))))
                      (cons t result))
                  (nelisp-artifact--write-stderr
                   (format "native-exec host-helper failed status=%S: %s"
                           status
                           (nelisp-artifact--read-log-if-exists err)))
                  nil))
            (nelisp-artifact--delete-if-exists out)
            (nelisp-artifact--delete-if-exists err)
            (dolist (arg-file arg-files)
              (nelisp-artifact--delete-if-exists arg-file))))))))

(defconst nelisp-artifact--native-boundary-slot-names
  '("out" "mirror" "frames" "scratch" "name_slot"
    "callback-slot-0" "callback-slot-1" "callback-slot-2" "callback-slot-3"
    "callback-slot-4" "callback-slot-5" "callback-slot-6" "callback-slot-7"
    "callback-slot-8" "callback-slot-9" "callback-slot-10" "callback-slot-11")
  "Object-mode hidden boundary slots for ordinary native user defuns.")

(defun nelisp-artifact--native-general-unsupported-externs (native)
  "Return NATIVE extern symbols not supported by the host proof harness."
  (let ((externs (plist-get native :extern-symbols)))
    (if (or (null externs)
            (and (symbolp externs)
                 (string= (symbol-name externs) "nil")))
        nil
      (seq-remove
       (lambda (name)
         (member name '("nl_alloc_symbol"
                        "nl_alloc_str"
                        "nl_alloc_mut_str"
                        "nl_mut_str_push_byte"
                        "nl_mut_str_finalize"
                        "nelisp_aot_builtin_call1"
                        "nelisp_aot_builtin_calln")))
       externs))))

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

(defun nelisp-artifact--native-driver-c (csym meta &optional args)
  "Return the C harness source for CSYM using META."
  (let* ((arity (or (plist-get meta :arity) 0))
         (arg-kinds (or (mapcar (lambda (arg)
                                  (cond
                                   ((integerp arg) 'int)
                                   ((stringp arg) 'str)
                                   (t 'unsupported)))
                                args)
                         (make-list arity 'int)))
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
     (format "/* native target: %s */\n" csym)
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
     "  NELN_TAG_STR = 5,\n"
     "  NELN_TAG_MUT_STR = 6,\n"
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
     "static void neln_write_str(NelnSexp *slot, const char *value) {\n"
     "  size_t n = value ? strlen(value) : 0u;\n"
     "  neln_clear_sexp(slot);\n"
     "  slot->tag = NELN_TAG_STR;\n"
     "  slot->a = (uint64_t)n;\n"
     "  slot->b = (uint64_t)(uintptr_t)(value ? value : \"\");\n"
     "  slot->c = (uint64_t)n;\n"
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
     "NelnSexp *nl_alloc_str(const unsigned char *bytes_ptr, int64_t len, NelnSexp *result_slot) {\n"
     "  size_t n = (len <= 0) ? 0u : (size_t)len;\n"
     "  size_t cap = (n == 0) ? 1u : n;\n"
     "  char *buf = (char *)calloc(cap + 1u, 1u);\n"
     "  if (!buf) {\n"
     "    neln_fail(\"calloc failed in nl_alloc_str\");\n"
     "  }\n"
     "  if (bytes_ptr && n > 0) {\n"
     "    memcpy(buf, bytes_ptr, n);\n"
     "  }\n"
     "  neln_clear_sexp(result_slot);\n"
     "  result_slot->tag = NELN_TAG_STR;\n"
     "  result_slot->a = (uint64_t)n;\n"
     "  result_slot->b = (uint64_t)(uintptr_t)buf;\n"
     "  result_slot->c = (uint64_t)n;\n"
     "  return result_slot;\n"
     "}\n"
     "\n"
     "NelnSexp *nl_alloc_mut_str(int64_t cap, NelnSexp *result_slot) {\n"
     "  size_t n = (cap <= 0) ? 1u : (size_t)cap;\n"
     "  char *buf = (char *)calloc(n + 1u, 1u);\n"
     "  if (!buf) {\n"
     "    neln_fail(\"calloc failed in nl_alloc_mut_str\");\n"
     "  }\n"
     "  neln_clear_sexp(result_slot);\n"
     "  result_slot->tag = NELN_TAG_MUT_STR;\n"
     "  result_slot->a = (uint64_t)n;\n"
     "  result_slot->b = (uint64_t)(uintptr_t)buf;\n"
     "  result_slot->c = 0u;\n"
     "  return result_slot;\n"
     "}\n"
     "\n"
     "void nl_mut_str_push_byte(NelnSexp *slot, int64_t byte) {\n"
     "  size_t cap;\n"
     "  size_t len;\n"
     "  char *buf;\n"
     "  if (!slot || slot->tag != NELN_TAG_MUT_STR) {\n"
     "    neln_fail(\"nl_mut_str_push_byte expects MutStr slot\");\n"
     "  }\n"
     "  cap = (size_t)slot->a;\n"
     "  len = (size_t)slot->c;\n"
     "  buf = (char *)(uintptr_t)slot->b;\n"
     "  if (len + 1u >= cap) {\n"
     "    size_t next = cap < 8u ? 8u : cap * 2u;\n"
     "    char *grown = (char *)realloc(buf, next + 1u);\n"
     "    if (!grown) {\n"
     "      neln_fail(\"realloc failed in nl_mut_str_push_byte\");\n"
     "    }\n"
     "    memset(grown + cap, 0, (next + 1u) - cap);\n"
     "    buf = grown;\n"
     "    cap = next;\n"
     "    slot->a = (uint64_t)cap;\n"
     "    slot->b = (uint64_t)(uintptr_t)buf;\n"
     "  }\n"
     "  buf[len] = (char)((unsigned char)byte);\n"
     "  slot->c = (uint64_t)(len + 1u);\n"
     "}\n"
     "\n"
     "NelnSexp *nl_mut_str_finalize(NelnSexp *slot, NelnSexp *result_slot) {\n"
     "  if (!slot || slot->tag != NELN_TAG_MUT_STR) {\n"
     "    neln_fail(\"nl_mut_str_finalize expects MutStr slot\");\n"
     "  }\n"
     "  return nl_alloc_str((const unsigned char *)(uintptr_t)slot->b, (int64_t)slot->c, result_slot);\n"
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
     "/* SysV x86_64 lowering observed in AOT: the first six fixed\n"
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
     "static int neln_print_result(NelnSexp *ret) {\n"
     "  if (!ret) {\n"
     "    printf(\"0\\n\");\n"
     "    return 0;\n"
     "  }\n"
     "  if (neln_is_registered_slot(ret)) {\n"
     "    switch (ret->tag) {\n"
     "    case NELN_TAG_NIL:\n"
     "      printf(\"nil\\n\");\n"
     "      return 0;\n"
     "    case NELN_TAG_T:\n"
     "      printf(\"t\\n\");\n"
     "      return 0;\n"
     "    case NELN_TAG_INT:\n"
     "      printf(\"%ld\\n\", (long)((int64_t)ret->a));\n"
     "      return 0;\n"
     "    case NELN_TAG_STR:\n"
     "      if (ret->b && ret->a > 0) {\n"
     "        fwrite((const void *)(uintptr_t)ret->b, 1u, (size_t)ret->a, stdout);\n"
     "      }\n"
     "      fputc('\\n', stdout);\n"
     "      return 0;\n"
     "    case NELN_TAG_SYMBOL:\n"
     "      printf(\"%s\\n\", neln_symbol_name(ret));\n"
     "      return 0;\n"
     "    default:\n"
     "      neln_fail(\"unsupported Sexp result tag\");\n"
     "    }\n"
     "  }\n"
     "  printf(\"%ld\\n\", (long)((int64_t)(intptr_t)ret));\n"
     "  return 0;\n"
     "}\n"
     "\n"
     "int main(int argc, char **argv) {\n"
     (format "  long argv_vals[%d];\n" (max 1 arity))
     (format "  NelnSexp argv_string_slots[%d];\n" (max 1 arity))
     "  int i;\n"
     (format "  if (argc != %d) {\n" (1+ arity))
     "    fprintf(stderr, \"usage mismatch\\n\");\n"
     "    return 2;\n"
     "  }\n"
     "  neln_reset_slots();\n"
     "  for (i = 1; i < argc; i++) {\n"
     "    switch (i - 1) {\n"
     (mapconcat
      (lambda (i)
        (let ((kind (nth i arg-kinds)))
          (pcase kind
            ('int
             (format "    case %d: argv_vals[%d] = strtol(argv[i], NULL, 10); break;\n"
                     i i))
            ('str
             (format "    case %d: neln_write_str(&argv_string_slots[%d], argv[i]); neln_register_slot(&argv_string_slots[%d]); argv_vals[%d] = (long)(intptr_t)&argv_string_slots[%d]; break;\n"
                     i i
                     i i i))
            (_
             (format "    case %d: fprintf(stderr, \"unsupported argument kind\\n\"); return 2;\n"
                     i)))))
      (number-sequence 0 (1- arity))
      "")
     "    default: return 2;\n"
     "    }\n"
     "  }\n"
     (format "  return neln_print_result(call_target(%s));\n"
             invoke-args)
     "}\n")))

(defun nelisp-artifact--native-exec-parse-stdout (stdout)
  "Return native exec STDOUT as an integer when canonical, else a string."
  (let ((text (if (and (stringp stdout)
                       (> (length stdout) 0)
                       (= (aref stdout (1- (length stdout))) ?\n))
                  (substring stdout 0 -1)
                stdout)))
    (if (nelisp-artifact--canonical-integer-token-p text)
        (string-to-number text)
      text)))

(defun nelisp-artifact--native-exec-general-cache-exe
    (artifact-path symbol args)
  "Return the cached general native executable path for ARGS."
  (nelisp-artifact--native-exec-cache-exe
   artifact-path symbol (length args) "general"
   (nelisp-artifact--native-exec-arg-signature args)))

(defun nelisp-artifact--native-exec-general-build
    (artifact-path symbol args exe cc objcopy)
  "Build cached general native executable EXE for ARTIFACT-PATH SYMBOL."
  (let* ((native
          (nelisp-artifact--serialized-native-section-for-symbol
           artifact-path symbol))
         (meta (and native
                    (nelisp-artifact--native-defun-metadata native symbol)))
         (unsupported (and native
                           (nelisp-artifact--native-general-unsupported-externs
                            native))))
    (unless native
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
    (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec-general"))
           (obj (expand-file-name "mod.o" dir))
           (obj2 (expand-file-name "mod-c.o" dir))
           (asrc (expand-file-name "tramp.S" dir))
           (csrc (expand-file-name "drv.c" dir))
           (build-log (expand-file-name "build.log" dir))
           (built-exe (expand-file-name "run" dir))
           (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol)))
      (unwind-protect
          (progn
            (nelisp-artifact--write-native-object-file
             artifact-path obj native)
            (unless (eq 0 (nelisp-artifact--call-process-quiet
                           objcopy build-log
                           (format "--redefine-sym=%s=%s" symbol csym)
                           obj obj2))
              (error "objcopy symbol rename failed for %s: %s"
                     symbol
                     (nelisp-artifact--read-log-if-exists build-log)))
            (with-temp-file asrc
              (insert (nelisp-artifact--native-trampoline-asm csym meta) "\n"))
            (with-temp-file csrc
              (insert (nelisp-artifact--native-driver-c csym meta args)))
            (unless (eq 0 (nelisp-artifact--call-process-quiet
                           cc build-log "-O2" "-c" "-o"
                           (expand-file-name "tramp.o" dir) asrc))
              (error "native trampoline assembly failed for %s: %s"
                     symbol
                     (nelisp-artifact--read-log-if-exists build-log)))
            (unless (eq 0 (nelisp-artifact--call-process-quiet
                           cc build-log "-O2" "-c" "-o"
                           (expand-file-name "drv.o" dir) csrc))
              (error "native driver compile failed for %s: %s"
                     symbol
                     (nelisp-artifact--read-log-if-exists build-log)))
            (unless (eq 0 (nelisp-artifact--call-process-quiet
                           cc build-log "-O2" "-o" built-exe
                           (expand-file-name "drv.o" dir)
                           (expand-file-name "tramp.o" dir)
                           obj2))
              (error "native general link failed for %s: %s"
                     symbol
                     (nelisp-artifact--read-log-if-exists build-log)))
            (make-directory (file-name-directory exe) t)
            (rename-file built-exe exe t)
            (nelisp-artifact--note-native-dispatch
             (list :event 'native-cache
                   :symbol (intern symbol)
                   :mode 'general-build
                   :exe exe))
            exe)
        (delete-directory dir t)))))

(defun nelisp-artifact--native-exec-general-exe
    (artifact-path symbol args cc objcopy)
  "Return a cached executable for general native exec."
  (let ((exe (nelisp-artifact--native-exec-general-cache-exe
              artifact-path symbol args)))
    (if (and nelisp-artifact-native-exec-cache-enabled
             (file-exists-p exe))
        (progn
          (nelisp-artifact--note-native-dispatch
           (list :event 'native-cache
                 :symbol (intern symbol)
                 :mode 'general-hit
                 :exe exe))
          exe)
      (nelisp-artifact--native-exec-general-build
       artifact-path symbol args exe cc objcopy))))

(defun nelisp-artifact--native-exec-general-run (exe symbol args sh)
  "Run cached general native EXE with ARGS and return its decoded stdout."
  (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec-general-run"))
         (run-out (expand-file-name "run.out" dir))
         (run-err (expand-file-name "run.err" dir))
         (run-args
          (mapcar (lambda (arg)
                    (cond
                     ((integerp arg) (number-to-string arg))
                     ((stringp arg) arg)
                     (t (error "native-exec-general unsupported arg: %S"
                               arg))))
                  args)))
    (unwind-protect
        (let* ((run-status
                (if sh
                    (apply #'call-process
                           sh nil nil nil "-c"
                           "exe=$1; out=$2; err=$3; shift 3; \"$exe\" \"$@\" >\"$out\" 2>\"$err\""
                           "neln-run" exe run-out run-err run-args)
                  (with-temp-buffer
                    (let ((status (apply #'call-process exe nil t nil
                                         run-args)))
                      (write-region (buffer-string) nil run-out)
                      (write-region "" nil run-err)
                      status))))
               (stdout (nelisp-artifact--read-file-as-string run-out))
               (stderr (nelisp-artifact--read-file-as-string run-err)))
          (unless (eq 0 run-status)
            (error "native general run failed for %s (exit %s): %s"
                   symbol run-status (string-trim stderr)))
          (unless (and (stringp stdout) (> (length stdout) 0))
            (error "native general run produced no output for %s"
                   symbol))
          (nelisp-artifact--native-exec-parse-stdout stdout))
      (delete-directory dir t))))

(defun nelisp-artifact-native-exec-general (artifact-path symbol args)
  "Host-side native EXEC proof for builtin-calling `.neln' defuns.
This links the embedded object against a generated C/asm harness that
provides the `nl_alloc_symbol', `nelisp_aot_builtin_call1', and minimal
`nelisp_aot_builtin_calln' runtime shims plus a boundary-populating
trampoline, then returns the decoded integer or string result."
  (let ((helper (nelisp-artifact--standalone-host-helper-native-exec-general
                 artifact-path symbol args)))
    (if helper
        (cdr helper)
      (unless (and (eq system-type 'gnu/linux)
                   (equal (or (car-safe (split-string system-configuration "-")) "")
                          "x86_64"))
        (error "native-exec-general currently requires x86_64 Linux"))
      (let ((cc (or (executable-find "cc") (executable-find "gcc")))
	    (objcopy (executable-find "objcopy"))
	    (sh (executable-find "sh")))
        (unless (and cc objcopy)
          (error "native-exec-general needs cc + objcopy on PATH"))
        (nelisp-artifact--native-exec-general-run
         (nelisp-artifact--native-exec-general-exe
          artifact-path symbol args cc objcopy)
         symbol args sh)))))

(defun nelisp-artifact--eval-forms (forms &optional kind)
  "Evaluate CLI FORMS after loading an artifact, returning the last value.
For `nelc'/`neln' the module installs onto the NeLisp runtime, so FORMS
are evaluated with `nelisp-eval'.  For a `.elc' (Doc 142 §6.2) the module
is loaded into host Emacs, so FORMS are evaluated with host `eval'."
  (let ((source (nelisp-artifact--join-forms forms))
        (last nil))
    (dolist (form (if (eq kind 'elc)
                      (nelisp-artifact--read-all-from-string source)
                    (nelisp-artifact--read-top-level-forms-fallback source)))
      (setq last (if (eq kind 'elc) (eval form t) (nelisp-eval form))))
    last))

(defun nelisp-artifact--parse-compile-args (args &optional runtime-image-p)
  "Parse `compile-elisp-artifact' ARGS into a plist.
When RUNTIME-IMAGE-P is non-nil, allow the wasm runtime-image lane to
resolve `--kind auto' or `--kind wasm' to the distinct kind value
`\"wasm\"' when `--target' selects a wasm32 triple."
  (let ((rest (cdr args))
        (kind nil)
        (input nil)
        (output nil)
        (manifest nil)
        (target nil)
        (load-paths nil)
        (preloads nil)
        (requested-feature nil)
        (native-policy nil)
        (module-policy nil)
        (rewrite-defalias-late nil)
        (profile-stages nil)
        (profile-forms nil))
    (while rest
      (let ((flag (car rest))
            (value (cadr rest)))
        (cond
         ((equal flag "--profile-stages")
          (setq profile-stages t)
          (setq rest (cdr rest)))
         ((equal flag "--profile-forms")
          (setq profile-forms t)
          (setq rest (cdr rest)))
         ((equal flag "--rewrite-defalias-late")
          (setq rewrite-defalias-late t)
          (setq rest (cdr rest)))
	         ((or (equal flag "--kind")
	              (equal flag "--input")
	              (equal flag "--output")
	              (equal flag "--manifest")
	              (equal flag "--target")
	              (equal flag "--load-path")
	              (equal flag "--preload")
	              (equal flag "--feature")
	              (equal flag "--native-policy")
	              (equal flag "--module-policy")
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
	            (setq requested-feature (intern value)))
	           ((equal flag "--native-policy")
	            (setq native-policy (nelisp-artifact--normalize-native-policy
	                                 value)))
	              ((equal flag "--module-policy")
	               (setq module-policy (nelisp-artifact--normalize-module-policy
	                                    value))))
          (setq rest (cddr rest)))
         (t
          (error "unknown flag %s" flag)))))
    (unless (and kind input output)
      (error "compile-elisp-artifact requires --kind, --input, and --output"))
    (unless (member kind (if runtime-image-p
                             '("nelc" "neln" "elc" "auto" "wasm")
                           '("nelc" "neln" "elc" "auto")))
      (error "unsupported --kind %s" kind))
    ;; Resolve `auto' from the output suffix (Doc 142 §6.5).
    (let* ((resolved (cond
                      ((and runtime-image-p
                            (nelisp-artifact--runtime-image-wasm-target-p target)
                            (member kind '("auto" "wasm")))
                       "wasm")
                      ((equal kind "auto")
                       (cond ((string-suffix-p ".neln" output) "neln")
                             ((string-suffix-p ".elc" output) "elc")
                             (t "nelc")))
                      (t kind))))
      (cond
       ((and (equal resolved "wasm") (not (string-suffix-p ".wasm" output)))
        (error "compile-elisp-artifact --kind wasm output must use the .wasm suffix"))
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
	              :requested-feature requested-feature
	              :native-policy native-policy
	              :module-policy module-policy
	              :rewrite-defalias-late rewrite-defalias-late
	              :profile-stages profile-stages
	              :profile-forms profile-forms)))))

(defun nelisp-artifact--standalone-runtime-p ()
  "Return non-nil when running inside the generated standalone CLI."
  (and (fboundp 'nelisp--write-stdout-bytes)
       (boundp 'nelisp-artifact-standalone-repo-root)
       (stringp nelisp-artifact-standalone-repo-root)
       (> (length nelisp-artifact-standalone-repo-root) 0)))

(defun nelisp-artifact--standalone-windows-p ()
  "Return non-nil when the generated standalone runtime targets Windows."
  (and (boundp 'nelisp-artifact-standalone-target)
       (memq nelisp-artifact-standalone-target '(windows-x86_64 windows-aarch64))))

(defun nelisp-artifact--standalone-windows-cmd-query (expr)
  "Run `cmd.exe /c EXPR' and return its trimmed stdout, or nil on failure.
The standalone Windows runtime's `process-environment' is never populated
from the real OS environment (nothing auto-inherits it at boot, and there is
no `--setenv' entry point on this CLI today), so `getenv'/`executable-find'
cannot see PATH or any real environment variable directly.  A CHILD process
spawned via `call-process', however, DOES inherit the real environment
(the Windows `CreateProcessW' spawn model passes `lpEnvironment = NULL',
which means \"use the calling process's environment\").  Shelling out to
`cmd.exe' is therefore the only currently-working way for standalone Windows
code to observe the real environment; kept local to the artifact
host-helper discovery below rather than a general `getenv' replacement."
  (and (fboundp 'call-process)
       (fboundp 'make-temp-file)
       (let ((out (nelisp-artifact--make-temp-path "nelisp-win-env" "txt")))
         (unwind-protect
             (let ((status (call-process "C:/Windows/System32/cmd.exe" nil
                                         (list :file out) nil "/c" expr)))
               (when (eq status 0)
                 (let ((text (nelisp-artifact--read-log-if-exists out)))
                   (when (and (stringp text) (> (length text) 0))
                     (string-trim text)))))
           (nelisp-artifact--delete-if-exists out)))))

(defun nelisp-artifact--standalone-windows-looks-like-path-p (candidate)
  "Return non-nil when CANDIDATE looks like a filesystem path.
A bare PATH-relative command name (e.g. \"emacs\") has neither a directory
separator nor a drive letter; a real path (e.g. \"C:/emacs/bin/emacs.exe\" or
a user override like \"C:/nonexistent.exe\") has at least one of these."
  (or (nelisp-artifact--string-search-literal "/" candidate)
      (nelisp-artifact--string-search-literal "\\" candidate)
      (nelisp-artifact--string-search-literal ":" candidate)))

(defun nelisp-artifact--standalone-windows-host-emacs ()
  "Locate host Emacs for the standalone Windows runtime, or nil.
`executable-find' cannot be trusted here: this runtime's polyfill (see
`scripts/nelisp-stdlib-prelude.el') does a literal PATH-search file-existence
check with no Windows executable-suffix handling, so it never resolves a bare
\"emacs\" against an on-disk \"emacs.exe\" even when `getenv' correctly sees a
PATH that contains it.  `call-process', however, spawns through
`CreateProcessW', which performs its OWN PATH + suffix search when given a
bare command name -- so returning the bare candidate string (no
`executable-find' involved) is sufficient and matches the call shape already
proven to work today (`call-process' with no wrapping `let'/`unwind-protect'
around it succeeds; see the host-helper-compile call site).  When the
candidate looks like an actual path (an explicit `NELISP_HOST_EMACS'/`EMACS'
override), verify it exists on disk first with a cheap `file-exists-p' so a
bogus override produces the clean \"unavailable\" diagnostic instead of an
attempted spawn against a path known not to exist."
  (let ((candidate (or (and (fboundp 'getenv) (getenv "NELISP_HOST_EMACS"))
                        (and (fboundp 'getenv) (getenv "EMACS"))
                        "emacs")))
    (and (stringp candidate)
         (> (length candidate) 0)
         (or (not (nelisp-artifact--standalone-windows-looks-like-path-p
                   candidate))
             (file-exists-p candidate))
         candidate)))

(defun nelisp-artifact--host-helper-emacs ()
  "Return the host Emacs executable for standalone helper builds, or nil."
  (or (and (nelisp-artifact--standalone-windows-p)
           (nelisp-artifact--standalone-windows-host-emacs))
      (let ((candidate (or (and (fboundp 'getenv)
                                (getenv "NELISP_HOST_EMACS"))
                           "emacs")))
        (and (fboundp 'executable-find)
             (executable-find candidate)))))

(defun nelisp-artifact--standalone-host-helper-disabled-p ()
  "Return non-nil when the standalone host helper is explicitly disabled."
  ;; NOTE: no `cmd.exe'-based fallback here for the same reason
  ;; `nelisp-artifact--standalone-windows-host-emacs' has none right now --
  ;; see its docstring.
  (and (fboundp 'getenv)
       (equal (getenv "NELISP_DISABLE_HOST_HELPER") "1")))

(defun nelisp-artifact--standalone-host-helper-mode (_opts kind)
  "Return the host-helper requirement for OPTS/KIND, or nil if not needed.
`required' means compilation must go through host Emacs -- the standalone
`nelc'/`neln' compiler path is not reliable on Windows standalone builds
today, so falling back to it silently would risk the old exit-with-no-output
failure mode instead of a clear error.  `preferred' means host Emacs is used
when available, but the native standalone path may still run otherwise.

NOTE: this used to also gate on `(file-exists-p (expand-file-name
\"lisp/nelisp-artifact.el\" nelisp-artifact-standalone-repo-root))' as a
sanity check that the baked repo root really points at a live checkout.  On
the standalone Windows runtime that check silently defeats the whole gate:
`file-exists-p' (built on `nelisp--syscall-stat', a separate, deeper
interpreter/syscall defect out of scope here -- see the
`handoff/syscall-stat-hang' investigation) returns nil for this exact file
even though it demonstrably exists, so `mode' always came out nil and the
native path ran unguarded.  `nelisp-artifact--standalone-runtime-p' already
requires a non-empty baked `nelisp-artifact-standalone-repo-root'; if that root
somehow does not point at a live checkout, the host-helper subprocess's own
`require\\='nelisp-artifact' will fail with a nonzero exit status, which
`nelisp-artifact--standalone-host-helper-compile' already reports as a clear
one-line diagnostic -- so dropping this redundant, broken precondition loses
no real safety.  For standalone `.neln' compiles the helper is always
preferred on non-Windows targets so opportunistic auto-AOT work leaves the
memory-heavy standalone process as early as possible."
  (and (nelisp-artifact--standalone-runtime-p)
       (cond
        ((nelisp-artifact--standalone-windows-p) 'required)
        ((eq kind 'neln)
         'preferred)
        (t nil))))

(defun nelisp-artifact--standalone-host-helper-unavailable-message (_mode kind)
  "Return a one-line diagnostic for an unavailable helper MODE/KIND."
  (let ((prefix (format "host-helper required for --kind %s on standalone %s"
                        kind
                        (if (nelisp-artifact--standalone-windows-p)
                            "Windows"
                          "runtime"))))
    (if (nelisp-artifact--standalone-host-helper-disabled-p)
        (concat prefix " but NELISP_DISABLE_HOST_HELPER=1")
      (concat prefix
              " (set NELISP_HOST_EMACS or install emacs on PATH)"))))

(defun nelisp-artifact--standalone-host-helper-quoted-list (value)
  "Return VALUE serialized as a quoted list literal for host Emacs."
  (prin1-to-string (list 'quote value)))

(defun nelisp-artifact--standalone-host-helper-compile (opts kind)
  "Compile one artifact through host Emacs when required/preferred for KIND.
Return `t' on success, a one-line diagnostic string when a helper that was
required or actually started failed (the caller must signal an error with it),
or nil when no helper was started and the caller may use the native standalone
path.  In particular, a failed PREFERRED helper is not an availability miss:
falling back after it reported a reader/compiler error can turn that error into
a truncated artifact followed by CLI exit 0."
  (let ((mode (nelisp-artifact--standalone-host-helper-mode opts kind)))
    (cond
     ((null mode) nil)
     ((nelisp-artifact--standalone-host-helper-disabled-p)
      (if (eq mode 'required)
          (nelisp-artifact--standalone-host-helper-unavailable-message mode kind)
        nil))
     (t
      (let ((emacs (nelisp-artifact--host-helper-emacs)))
        (if (null emacs)
            (if (eq mode 'required)
                (nelisp-artifact--standalone-host-helper-unavailable-message
                 mode kind)
              nil)
          (let* ((start (nelisp-artifact--profile-time))
                 (root nelisp-artifact-standalone-repo-root)
                 (log (nelisp-artifact--make-temp-path "nelisp-host-helper" "log"))
                 (eval-form
                  (if (eq kind 'elc)
                      ;; `.elc' artifacts are produced by
                      ;; `nelisp-artifact-compile-elc-file', a distinct
                      ;; function with its own (shorter) argument list -- it
                      ;; has no kind/native-policy/module-policy parameters.
                      (concat
                       "(progn (setq load-prefer-newer t)"
                       " (require 'nelisp-artifact)"
                       " (nelisp-artifact-compile-elc-file "
                       (prin1-to-string (plist-get opts :input)) " "
                       (prin1-to-string (plist-get opts :output)) " "
                       (prin1-to-string (plist-get opts :manifest)) " "
                       (prin1-to-string (plist-get opts :target)) " "
                       (nelisp-artifact--standalone-host-helper-quoted-list
                        (plist-get opts :load-paths)) " "
                       (nelisp-artifact--standalone-host-helper-quoted-list
                        (plist-get opts :preloads)) " "
                       (prin1-to-string (plist-get opts :requested-feature))
                       "))")
                    (concat
                     "(progn (setq load-prefer-newer t)"
                     " (require 'nelisp-artifact)"
                     " (let ((nelisp-artifact-profile-stages "
                     (prin1-to-string (plist-get opts :profile-stages))
                     ") (nelisp-artifact-profile-forms "
                     (prin1-to-string (plist-get opts :profile-forms))
                     ") (nelisp-artifact--rewrite-defalias-late "
                     (prin1-to-string (plist-get opts :rewrite-defalias-late))
                     ")) (nelisp-artifact-compile-file "
                     (prin1-to-string (plist-get opts :input)) " "
                     (prin1-to-string (plist-get opts :output)) " "
                     (prin1-to-string (plist-get opts :manifest)) " "
                     (prin1-to-string (plist-get opts :target)) " "
                     (nelisp-artifact--standalone-host-helper-quoted-list
                      (plist-get opts :load-paths)) " "
                     (nelisp-artifact--standalone-host-helper-quoted-list
                      (plist-get opts :preloads)) " "
                     (prin1-to-string (plist-get opts :requested-feature)) " "
                     (prin1-to-string (list 'quote kind)) " "
                     (prin1-to-string (list 'quote (plist-get opts :native-policy))) " "
                     (let ((mp (plist-get opts :module-policy)))
                       (if mp
                           (prin1-to-string (list 'quote mp))
                         "nil"))
                     ")))")))
                 (status nil))
            (unwind-protect
                (progn
                  (setq status
                        (call-process
                         emacs nil (list :file log) nil
                         "-Q" "--batch"
                         "-L" (expand-file-name "lisp" root)
                         "-L" (expand-file-name "src" root)
                         "--eval" eval-form))
                  (if (eq status 0)
                      (progn
                        (when (plist-get opts :profile-stages)
                          (let ((helper-log (nelisp-artifact--read-log-if-exists log)))
                            (when (> (length helper-log) 0)
                              (nelisp-artifact--write-stderr helper-log)))
                          (nelisp-artifact--profile-log
                           "host-helper" start
                           (list :emacs emacs :kind kind
                                 :native-policy (plist-get opts :native-policy))))
                        t)
                    (let ((msg (format "host-helper failed status=%S: %s"
                                        status
                                        (nelisp-artifact--read-log-if-exists log))))
                      ;; Once a helper was actually started, any nonzero
                      ;; status is a compile failure for both REQUIRED and
                      ;; PREFERRED modes.  Returning nil here used to run the
                      ;; standalone fallback after status=255 (including
                      ;; bootstrap reader errors); that fallback could emit a
                      ;; tiny one-form artifact and the CLI then exited 0.
                      msg)))
              (nelisp-artifact--delete-if-exists log)))))))))

(defun compile-elisp-artifact (args)
  "CLI entry point for `nelisp compile-elisp-artifact'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-compile-args args))
             (kind (intern (plist-get opts :kind))))
      (let ((nelisp-artifact-profile-stages
               (plist-get opts :profile-stages))
              (nelisp-artifact-profile-forms
               (plist-get opts :profile-forms))
              (nelisp-artifact--rewrite-defalias-late
               (plist-get opts :rewrite-defalias-late)))
          ;; NOTE: `--kind elc' used to bypass the host-helper dispatch
          ;; entirely and call the native `nelisp-artifact-compile-elc-file'
          ;; directly.  On the standalone Windows runtime that function
          ;; depends on host-Emacs-only primitives (e.g. `byte-compile-file')
          ;; that do not exist there, so it must go through the same
          ;; helper-required gate as `nelc'/`neln' -- never a silent native
          ;; attempt on Windows.
          (let ((helper-result
                 (nelisp-artifact--standalone-host-helper-compile opts kind)))
            (cond
             ;; Helper ran successfully -- nothing else to do.
             ((eq helper-result t) nil)
             ;; Helper was REQUIRED but unavailable/failed: signal a hard
             ;; error with the one-line diagnostic rather than silently
             ;; falling through to the native standalone compiler (the old
             ;; bug: exit 65/52/14 with no output on Windows).
             ((stringp helper-result) (error "%s" helper-result))
             ((eq kind 'elc)
              (nelisp-artifact-compile-elc-file
               (plist-get opts :input)
               (plist-get opts :output)
               (plist-get opts :manifest)
               (plist-get opts :target)
               (plist-get opts :load-paths)
               (plist-get opts :preloads)
               (plist-get opts :requested-feature)))
             (t
              (nelisp-artifact-compile-file
               (plist-get opts :input)
               (plist-get opts :output)
               (plist-get opts :manifest)
               (plist-get opts :target)
               (plist-get opts :load-paths)
               (plist-get opts :preloads)
               (plist-get opts :requested-feature)
               kind
               (plist-get opts :native-policy)
               (plist-get opts :module-policy))))))
        0)
    (error
     (nelisp-artifact--print-error
      (format "compile-elisp-artifact: %s" (error-message-string err)))
     1)))

(defun nelisp-artifact--el-file-p (path)
  "Return non-nil when PATH names an `.el' source file."
  (and (stringp path)
       (string-suffix-p ".el" path)
       (not (string-suffix-p ".manifest.el" path))))

(defun nelisp-artifact--nonempty-lines (text)
  "Return non-empty newline-delimited lines from TEXT."
  (let ((pos 0)
        (len (length text))
        (out nil))
    (while (< pos len)
      (let ((start pos))
        (while (and (< pos len)
                    (not (= (aref text pos) ?\n)))
          (setq pos (1+ pos)))
        (let ((line (substring text start pos)))
          (when (> (length line) 0)
            (setq out (append out (list line)))))
        (when (and (< pos len) (= (aref text pos) ?\n))
          (setq pos (1+ pos)))))
    out))

(defun nelisp-artifact--collect-el-files-with-find (input)
  "Return `.el' files under INPUT using POSIX `find', or nil on failure."
  (let ((find (and (fboundp 'executable-find)
                   (executable-find "find"))))
    (when find
      (condition-case nil
          (with-temp-buffer
            (when (eq 0 (call-process find nil t nil
                                       input "-type" "f"
                                       "-name" "*.el"
                                       "!" "-name" "*.manifest.el"))
              (nelisp-artifact--nonempty-lines (buffer-string))))
        (error nil)))))

(defun nelisp-artifact--collect-el-files (input)
  "Return `.el' source files under INPUT.
INPUT may be a file or directory.  Directory traversal is recursive and
returns a stable sorted list."
  (cond
   ((and (file-exists-p input)
         (not (file-directory-p input))
         (nelisp-artifact--el-file-p input))
    (list (expand-file-name input)))
   ((file-directory-p input)
    (let ((out (nelisp-artifact--collect-el-files-with-find input)))
      (unless out
        (let ((pending (list input)))
          (while pending
            (let ((dir (car pending)))
              (setq pending (cdr pending))
              (dolist (entry (directory-files dir t "\\`[^.]"))
                (cond
                 ((file-directory-p entry)
                  (setq pending (cons entry pending)))
                 ((nelisp-artifact--el-file-p entry)
                  (setq out (cons (expand-file-name entry) out)))))))))
      (sort out #'string<)))
   (t nil)))

(defun nelisp-artifact--neln-artifact-p (path)
  "Return non-nil when PATH names a NeLisp native artifact."
  (and (stringp path)
       (string-suffix-p ".neln" path)))

(defun nelisp-artifact--collect-neln-artifacts-with-find (input)
  "Return `.neln' artifacts under INPUT using POSIX `find', or nil on failure."
  (let ((find (and (fboundp 'executable-find)
                   (executable-find "find"))))
    (when find
      (condition-case nil
          (with-temp-buffer
            (when (eq 0 (call-process find nil t nil
                                       input "-type" "f"
                                       "-name" "*.neln"))
              (nelisp-artifact--nonempty-lines (buffer-string))))
        (error nil)))))

(defun nelisp-artifact--collect-neln-artifacts (input)
  "Return `.neln' artifact files under INPUT.
INPUT may be an artifact file or a directory.  Directory traversal is recursive
and returns a stable sorted list."
  (cond
   ((and (file-exists-p input)
         (not (file-directory-p input))
         (nelisp-artifact--neln-artifact-p input))
    (list (expand-file-name input)))
   ((file-directory-p input)
    (let ((out (nelisp-artifact--collect-neln-artifacts-with-find input)))
      (unless out
        (let ((pending (list input)))
          (while pending
            (let ((dir (car pending)))
              (setq pending (cdr pending))
              (dolist (entry (directory-files dir t "\\`[^.]"))
                (cond
                 ((file-directory-p entry)
                  (setq pending (cons entry pending)))
                 ((nelisp-artifact--neln-artifact-p entry)
                  (setq out (cons (expand-file-name entry) out)))))))))
      (sort out #'string<)))
   (t nil)))

(defun nelisp-artifact--audit-input-source-paths (inputs)
  "Return unique source paths named by INPUTS."
  (let ((sources nil))
    (dolist (input inputs)
      (cond
       ((and (file-exists-p input)
             (not (file-directory-p input))
             (nelisp-artifact--el-file-p input))
        (setq sources (append sources (list (expand-file-name input)))))
       ((file-directory-p input)
        (setq sources
              (append sources (nelisp-artifact--collect-el-files input))))))
    (nelisp-artifact--unique-strings sources)))

(defun nelisp-artifact--audit-input-artifact-paths (inputs)
  "Return unique `.neln' artifact paths named by INPUTS."
  (let ((artifacts nil))
    (dolist (input inputs)
      (cond
       ((and (file-exists-p input)
             (not (file-directory-p input))
             (nelisp-artifact--neln-artifact-p input))
        (setq artifacts (append artifacts (list (expand-file-name input)))))
       ((file-directory-p input)
        (setq artifacts
              (append artifacts
                      (nelisp-artifact--collect-neln-artifacts input))))))
    (nelisp-artifact--unique-strings artifacts)))

(defun nelisp-artifact--native-report-native-count (report)
  "Return how many REPORT entries are native-covered."
  (let ((count 0))
    (when (consp report)
      (dolist (entry report)
        (when (plist-get entry :native)
          (setq count (1+ count)))))
    count))

(defun nelisp-artifact--native-report-gap-names (report)
  "Return native coverage gap names from REPORT."
  (let ((names nil))
    (when (consp report)
      (dolist (entry report)
        (unless (plist-get entry :native)
          (setq names (append names (list (or (plist-get entry :name)
                                              "<unknown>")))))))
    names))

(defun nelisp-artifact--audit-existing-neln (artifact)
  "Return a native coverage audit plist for existing ARTIFACT."
  (condition-case err
      (let* ((manifest (nelisp-artifact--read-manifest-for-audit artifact))
             (kind (plist-get manifest :kind))
             (source (plist-get (plist-get manifest :source) :path))
             (raw-report (plist-get manifest :native-report))
             (report (if (consp raw-report) raw-report nil))
             (defuns (length report))
             (native (nelisp-artifact--native-report-native-count report))
             (gaps (nelisp-artifact--native-report-gap-names report)))
        (if (eq kind 'neln)
            (list :status (if gaps 'gaps 'ok)
                  :source source
                  :artifact (expand-file-name artifact)
                  :defuns defuns
                  :native native
                  :gaps (length gaps)
                  :gap-names gaps)
          (list :status 'invalid
                :artifact (expand-file-name artifact)
                :reason (format "expected neln manifest, got %S" kind))))
    (error
     (list :status 'invalid
           :artifact (expand-file-name artifact)
           :reason (error-message-string err)))))

(defun nelisp-artifact--audit-source-neln (source)
  "Return a native coverage audit plist for SOURCE's adjacent `.neln'."
  (let ((artifact (nelisp-artifact-source-artifact-path source 'neln)))
    (if (file-exists-p artifact)
        (let ((entry (nelisp-artifact--audit-existing-neln artifact)))
          (if (plist-get entry :source)
              entry
            (plist-put entry :source (expand-file-name source))))
      (list :status 'missing
            :source (expand-file-name source)
            :artifact artifact
            :defuns 0
            :native 0
            :gaps 0
            :gap-names nil))))

(defun nelisp-artifact--audit-status-rank (status)
  "Return numeric severity rank for audit STATUS."
  (cond
   ((eq status 'invalid) 3)
   ((eq status 'missing) 2)
   ((eq status 'gaps) 1)
   (t 0)))

(defun nelisp-artifact--audit-entry-line (entry)
  "Return a stable one-line representation of audit ENTRY."
  (let ((status (plist-get entry :status))
        (source (or (plist-get entry :source) "-"))
        (artifact (or (plist-get entry :artifact) "-"))
        (defuns (or (plist-get entry :defuns) 0))
        (native (or (plist-get entry :native) 0))
        (gaps (or (plist-get entry :gaps) 0))
        (gap-names (plist-get entry :gap-names))
        (reason (plist-get entry :reason)))
    (concat
     "artifact_audit"
     " status=" (symbol-name status)
     " source=" (prin1-to-string source)
     " artifact=" (prin1-to-string artifact)
     " defuns=" (number-to-string defuns)
     " native=" (number-to-string native)
     " gaps=" (number-to-string gaps)
     (if gap-names
         (concat " gap_names=" (prin1-to-string gap-names))
       "")
     (if reason
         (concat " reason=" (prin1-to-string reason))
       ""))))

(defun nelisp-artifact--audit-summary (entries)
  "Return summary plist for audit ENTRIES."
  (let ((missing 0)
        (invalid 0)
        (gap-artifacts 0)
        (defuns 0)
        (native 0)
        (gaps 0)
        (worst 'ok))
    (dolist (entry entries)
      (let ((status (plist-get entry :status)))
        (when (> (nelisp-artifact--audit-status-rank status)
                 (nelisp-artifact--audit-status-rank worst))
          (setq worst status))
        (cond
         ((eq status 'missing) (setq missing (1+ missing)))
         ((eq status 'invalid) (setq invalid (1+ invalid)))
         ((eq status 'gaps) (setq gap-artifacts (1+ gap-artifacts))))
        (setq defuns (+ defuns (or (plist-get entry :defuns) 0)))
        (setq native (+ native (or (plist-get entry :native) 0)))
        (setq gaps (+ gaps (or (plist-get entry :gaps) 0)))))
    (list :status worst
          :audited (length entries)
          :missing missing
          :invalid invalid
          :gap-artifacts gap-artifacts
          :defuns defuns
          :native native
          :gaps gaps)))

(defun nelisp-artifact--parse-audit-args (args)
  "Parse `audit-elisp-artifacts' ARGS into a plist."
  (let ((rest (cdr args))
        (required nil)
        (inputs nil))
    (while rest
      (let ((arg (car rest)))
        (cond
         ((equal arg "--required")
          (setq required t)
          (setq rest (cdr rest)))
         ((string-prefix-p "--" arg)
          (error "unknown flag %s" arg))
         (t
          (setq inputs (append inputs (list arg)))
          (setq rest (cdr rest))))))
    (unless inputs
      (error "audit-elisp-artifacts requires FILE.el, FILE.neln, or DIR"))
    (list :required required :inputs inputs)))

(defun audit-elisp-artifacts (args)
  "CLI entry point for `nelisp audit-elisp-artifacts'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-audit-args args))
             (inputs (plist-get opts :inputs))
             (sources (nelisp-artifact--audit-input-source-paths inputs))
             (artifacts (nelisp-artifact--audit-input-artifact-paths inputs))
             (entries nil)
             (summary nil))
        (dolist (source sources)
          (setq entries
                (append entries
                        (list (nelisp-artifact--audit-source-neln source)))))
        (dolist (artifact artifacts)
          (unless (member artifact
                          (mapcar (lambda (source)
                                    (nelisp-artifact-source-artifact-path
                                     source 'neln))
                                  sources))
            (setq entries
                  (append entries
                          (list (nelisp-artifact--audit-existing-neln
                                 artifact))))))
        (unless entries
          (error "no .el sources or .neln artifacts found"))
        (dolist (entry entries)
          (nelisp-artifact--write-stdout
           (concat (nelisp-artifact--audit-entry-line entry) "\n")))
        (setq summary (nelisp-artifact--audit-summary entries))
        (nelisp-artifact--write-stdout
         (format
          "artifact_audit_summary status=%s audited=%d missing=%d invalid=%d gap_artifacts=%d defuns=%d native=%d gaps=%d\n"
          (symbol-name (plist-get summary :status))
          (plist-get summary :audited)
          (plist-get summary :missing)
          (plist-get summary :invalid)
          (plist-get summary :gap-artifacts)
          (plist-get summary :defuns)
          (plist-get summary :native)
          (plist-get summary :gaps)))
        (if (and (plist-get opts :required)
                 (not (eq (plist-get summary :status) 'ok)))
            1
          0))
    (error
     (nelisp-artifact--print-error
      (format "audit-elisp-artifacts: %s" (error-message-string err)))
     1)))

(defun nelisp-artifact--parse-compile-many-args (args)
  "Parse `compile-elisp-artifacts' ARGS into a plist."
  (let ((rest (cdr args))
        (kind nil)
        (target nil)
        (load-paths nil)
        (preloads nil)
        (native-policy nil)
        (module-policy nil)
        (profile-stages nil)
        (profile-forms nil)
        (inputs nil))
    (while rest
      (let ((flag (car rest)))
        (cond
         ((equal flag "--profile-stages")
          (setq profile-stages t)
          (setq rest (cdr rest)))
         ((equal flag "--profile-forms")
          (setq profile-forms t)
          (setq rest (cdr rest)))
         ((member flag '("--kind" "--target" "--load-path" "--preload"
                         "--native-policy" "--module-policy"))
          (let ((value (cadr rest)))
            (unless value
              (error "missing value for %s" flag))
            (cond
	             ((equal flag "--kind") (setq kind value))
	             ((equal flag "--target") (setq target value))
	             ((equal flag "--load-path")
	              (setq load-paths (append load-paths (list value))))
	             ((equal flag "--preload")
	              (setq preloads (append preloads (list value))))
	             ((equal flag "--native-policy")
	              (setq native-policy (nelisp-artifact--normalize-native-policy
	                                   value)))
	             ((equal flag "--module-policy")
	              (setq module-policy (nelisp-artifact--normalize-module-policy
	                                   value))))
            (setq rest (cddr rest))))
         ((string-prefix-p "--" flag)
          (error "unknown flag %s" flag))
         (t
          (setq inputs (append inputs (list flag)))
          (setq rest (cdr rest))))))
    (unless kind
      (error "compile-elisp-artifacts requires --kind"))
    (unless inputs
      (error "compile-elisp-artifacts requires at least one FILE.el or DIR"))
    (unless (member kind '("nelc" "neln" "auto"))
      (error "unsupported --kind %s" kind))
    (list :kind (if (equal kind "auto") "neln" kind)
	          :target target
	          :load-paths load-paths
	          :preloads preloads
	          :native-policy native-policy
	          :module-policy module-policy
	          :profile-stages profile-stages
	          :profile-forms profile-forms
	          :inputs inputs)))

(defun nelisp-artifact--unique-strings (strings)
  "Return STRINGS with duplicate entries removed, preserving first order."
  (let ((seen nil)
        (out nil))
    (dolist (string strings)
      (unless (member string seen)
        (setq seen (cons string seen))
        (setq out (append out (list string)))))
    out))

(defun compile-elisp-artifacts (args)
  "CLI entry point for `nelisp compile-elisp-artifacts'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-compile-many-args args))
             (kind (intern (plist-get opts :kind)))
             (sources nil)
             (compiled 0)
             (failed 0))
        (dolist (input (plist-get opts :inputs))
          (setq sources
                (append sources (nelisp-artifact--collect-el-files input))))
        (setq sources (nelisp-artifact--unique-strings sources))
        (let ((nelisp-artifact-profile-stages
               (plist-get opts :profile-stages))
              (nelisp-artifact-profile-forms
               (plist-get opts :profile-forms)))
          (dolist (source sources)
            (let* ((output (nelisp-artifact-source-artifact-path source kind))
                   (file-opts
                    (list :kind (symbol-name kind)
                          :input source
                          :output output
                          :manifest (nelisp-artifact--sibling-manifest-path
                                     output)
                          :target (plist-get opts :target)
                          :load-paths (plist-get opts :load-paths)
                          :preloads (plist-get opts :preloads)
                          :requested-feature nil
                          :native-policy (plist-get opts :native-policy)
                          :module-policy (plist-get opts :module-policy)
                          :profile-stages (plist-get opts :profile-stages)
                          :profile-forms (plist-get opts :profile-forms))))
              (condition-case file-err
                  (progn
                    (or (nelisp-artifact--standalone-host-helper-compile
                         file-opts kind)
	                (nelisp-artifact-compile-file
	                 source output nil
	                 (plist-get opts :target)
	                 (plist-get opts :load-paths)
	                 (plist-get opts :preloads)
	                 nil kind
	                 (plist-get opts :native-policy)
	                 (plist-get opts :module-policy)))
	                    (setq compiled (1+ compiled)))
                (error
                 (setq failed (1+ failed))
                 (nelisp-artifact--write-stderr
                  (format "compile-elisp-artifacts: %s: %s"
                          source (error-message-string file-err))))))))
        (nelisp-artifact--write-stdout
         (format "compiled=%d failed=%d kind=%s\n"
                 compiled failed (symbol-name kind)))
        (if (= failed 0) 0 1))
    (error
     (nelisp-artifact--print-error
      (format "compile-elisp-artifacts: %s" (error-message-string err)))
     1)))

(defun nelisp-artifact--flat-image-cache-sidecar-path (image-path)
  "Return freshness sidecar path for flat arena IMAGE-PATH."
  (concat image-path ".manifest.el"))

(defun nelisp-artifact--sha256-file (path)
  "Return SHA-256 hex digest of PATH without materializing it in the heap."
  (let* ((program (or (and (file-exists-p "/usr/bin/sha256sum")
                           "/usr/bin/sha256sum")
                      (and (file-exists-p "/bin/sha256sum")
                           "/bin/sha256sum")
                      (executable-find "sha256sum")
                      (executable-find "shasum")))
         (out (nelisp-artifact--make-temp-path
               (expand-file-name
                "nelisp-sha256"
                (if (and (boundp 'temporary-file-directory)
                         (stringp temporary-file-directory))
                    temporary-file-directory
                  "/tmp/"))
               "out"))
         (destination (if (fboundp 'nelisp-process-call-process)
                          out
                        (list :file out)))
         (rc nil)
         (text nil))
    (unless program
      (error "no streaming SHA-256 program available"))
    (unwind-protect
        (progn
          (setq rc
                (if (string-suffix-p "shasum" program)
                    (call-process program nil destination nil
                                  "-a" "256" path)
                  (call-process program nil destination nil path)))
          (unless (= rc 0)
            (error "SHA-256 program failed for %s: %s" path rc))
          (setq text (nelisp-artifact--read-file-as-string out))
          (unless (and (stringp text) (>= (length text) 64))
            (error "invalid SHA-256 output for %s" path))
          (substring text 0 64))
      (nelisp-artifact--delete-if-exists out))))

(defun nelisp-artifact--read-flat-image-header-bytes (image-path)
  "Return exactly the first 64 raw bytes of IMAGE-PATH."
  (let ((bytes
         (if (fboundp 'nl-syscall-read-file)
             (nl-syscall-read-file image-path 0 64)
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally image-path nil 0 64)
             (buffer-substring-no-properties (point-min) (point-max))))))
    (unless (and (stringp bytes)
                 (= (if (fboundp 'nelisp--string-byte-at)
                        (string-bytes bytes)
                      (length bytes))
                    64))
      (error "short flat-image header: %s" image-path))
    bytes))

(defun nelisp-artifact--u64le-at (bytes offset)
  "Decode one unsigned little-endian u64 from BYTES at OFFSET."
  (let ((i 7)
        (value 0))
    (while (>= i 0)
      (setq value
            (+ (* value 256)
               (if (fboundp 'nelisp--string-byte-at)
                   (nelisp--string-byte-at bytes (+ offset i))
                 (aref bytes (+ offset i)))))
      (setq i (1- i)))
    value))

(defun nelisp-artifact--flat-image-header (image-path &optional known-size)
  "Read and validate flat arena IMAGE-PATH's 64-byte header.
Return a plist with the header fields and exact expected file length."
  (setq nelisp-artifact--flat-header-stage 'size)
  (let* ((actual-size
          (condition-case err
              (or known-size (nelisp-artifact--file-size image-path))
            (error (error "flat header size: %S" err))))
         ;; Keep the raw byte string and all byte access in this SAME frame.
         ;; Standalone string argument cloning is UTF-8-oriented; passing a
         ;; binary header through helper calls can transcode bytes >= 0x80.
         (bytes
          (condition-case err
              (progn
                (setq nelisp-artifact--flat-header-stage 'read)
                (if (fboundp 'nl-syscall-read-file)
                    nil
                  (nelisp-artifact--read-flat-image-header-bytes image-path)))
            (error (error "flat header read: %S" err))))
         (_byte-check
          (unless (or (fboundp 'nl-syscall-read-file)
                      (and (stringp bytes) (= (length bytes) 64)))
            (error "short flat-image header: %s" image-path)))
         (_pin (setq nelisp-artifact--flat-header-bytes bytes))
         (_stage (setq nelisp-artifact--flat-header-stage 'decode))
         (offset 0)
         (byte-index nil)
         (word nil)
         (magic nil)
         (slen nil)
         (isz nil)
         (tlen nil)
         (goff nil)
         (foff nil)
         (uoff nil)
         (span nil)
         (expected nil))
    (while (< offset 56)
      (setq byte-index 7)
      (setq word 0)
      (while (>= byte-index 0)
        (setq word
              (+ (* word 256)
                 (if (fboundp 'nl-syscall-read-file)
                     (progn
                       ;; The current standalone collector can run while this
                       ;; interpreted loop is active, but does not yet retain
                       ;; every temporary string reliably after a very large
                       ;; arena dump.  Read and consume one byte at a time so
                       ;; only immediate integers cross allocation points.
                       (setq nelisp-artifact--flat-header-bytes
                             (nl-syscall-read-file
                              image-path
                              (+ offset byte-index)
                              (+ offset byte-index 1)))
                       (unless (= (string-bytes
                                   nelisp-artifact--flat-header-bytes)
                                  1)
                         (error "short flat-image header: %s" image-path))
                       (nelisp--string-byte-at
                        nelisp-artifact--flat-header-bytes 0))
                   (aref nelisp-artifact--flat-header-bytes
                         (+ offset byte-index)))))
        (setq byte-index (1- byte-index)))
      ;; Keep decoded fields as immediate integers.  Building a temporary
      ;; list here makes that list another mid-form heap root, which the
      ;; current standalone evaluator does not yet preserve reliably.
      (cond
       ((= offset 0) (setq magic word))
       ((= offset 8) (setq slen word))
       ((= offset 16) (setq isz word))
       ((= offset 24) (setq tlen word))
       ((= offset 32) (setq goff word))
       ((= offset 40) (setq foff word))
       (t (setq uoff word)))
      (setq offset (+ offset 8)))
    (setq span (+ slen isz))
    (setq expected (+ 64 (* tlen 8) span))
    (setq nelisp-artifact--flat-header-stage 'magic)
    (unless (= magic 1179407692)
      (error "invalid flat-image magic: %s" image-path))
    (setq nelisp-artifact--flat-header-stage 'bounds)
    (unless (and (> slen 0) (>= isz 0) (>= tlen 0)
                 (< goff span) (< foff span) (< uoff span)
                 (= expected actual-size))
      (signal 'error
              (list 'flat-header-bounds
                    slen isz tlen goff foff uoff span expected actual-size)))
    (setq nelisp-artifact--flat-header-stage 'done)
    (list :magic magic :slen slen :isz isz :tlen tlen
          :globals-offset goff :frames-offset foff
          :unbound-offset uoff :expected-size expected)))

(defun nelisp-artifact--flat-image-u64-via-od (image-path offset)
  "Read one little-endian u64 at OFFSET in IMAGE-PATH via system `od'."
  (let ((out (nelisp-artifact--make-temp-path image-path "header-u64"))
        (rc nil)
        (text nil)
        (start 0))
    (unwind-protect
        (progn
          (setq rc
                (call-process "od" nil out nil
                              "-An" "-tu8"
                              "-j" (number-to-string offset)
                              "-N8" image-path))
          (unless (= rc 0)
            (error "od failed for flat-image header at %s: %s" offset rc))
          (setq text (nelisp-artifact--read-file-as-string out))
          (unless (and (stringp text) (> (length text) 0))
            (error "empty flat-image header field at %s" offset))
          ;; Standalone `string-to-number' deliberately implements only the
          ;; numeric token itself and does not skip `od''s leading padding.
          (while (and (< start (length text))
                      (<= (aref text start) 32))
            (setq start (1+ start)))
          (string-to-number (substring text start)))
      (nelisp-artifact--delete-if-exists out))))

(defun nelisp-artifact--validate-flat-image-header-via-od (image-path)
  "Validate IMAGE-PATH's flat header without retaining decoded heap strings."
  (let ((stage 'size))
    (condition-case err
        (let* ((actual-size (nelisp-artifact--file-size image-path))
               (_magic-stage (setq stage 'magic))
               (magic (nelisp-artifact--flat-image-u64-via-od image-path 0))
               (_slen-stage (setq stage 'slen))
               (slen (nelisp-artifact--flat-image-u64-via-od image-path 8))
               (_isz-stage (setq stage 'isz))
               (isz (nelisp-artifact--flat-image-u64-via-od image-path 16))
               (_tlen-stage (setq stage 'tlen))
               (tlen (nelisp-artifact--flat-image-u64-via-od image-path 24))
               (_goff-stage (setq stage 'goff))
               (goff (nelisp-artifact--flat-image-u64-via-od image-path 32))
               (_foff-stage (setq stage 'foff))
               (foff (nelisp-artifact--flat-image-u64-via-od image-path 40))
               (_uoff-stage (setq stage 'uoff))
               (uoff (nelisp-artifact--flat-image-u64-via-od image-path 48))
               (span (+ slen isz))
               (expected (+ 64 (* tlen 8) span)))
          (setq stage 'validate)
          (unless (= magic 1179407692)
            (error "invalid flat-image magic: %s" image-path))
          (unless (and (> slen 0) (>= isz 0) (>= tlen 0)
                       (< goff span) (< foff span) (< uoff span)
                       (= expected actual-size))
            (signal 'error
                    (list 'flat-header-bounds
                          slen isz tlen goff foff uoff
                          span expected actual-size)))
          (list :magic magic :slen slen :isz isz :tlen tlen
                :globals-offset goff :frames-offset foff
                :unbound-offset uoff :expected-size expected))
      (error
       (error "od-header stage=%S error=%S" stage err)))))

(defun nelisp-artifact--flat-image-cache-generation
    (artifact-manifest-sha runtime-sha image-sha)
  "Return deterministic generation id for one flat-image cache pair."
  (secure-hash
   'sha256
   (prin1-to-string
    (list nelisp-artifact--flat-image-cache-format
          nelisp-artifact--flat-image-abi
          artifact-manifest-sha runtime-sha image-sha))))

(defun nelisp-artifact--flat-artifact-generation-token
    (artifact-manifest-sha artifact-sha runtime-sha)
  "Return the token embedded before dumping one artifact/runtime heap."
  (secure-hash
   'sha256
   (prin1-to-string
    (list nelisp-artifact--flat-image-cache-format
          nelisp-artifact--flat-image-abi
          artifact-manifest-sha artifact-sha runtime-sha))))

(defun nelisp-artifact--flat-image-cache-record
    (artifact-path image-path runtime-path manifest
                   &optional verified-header artifact-generation-token)
  "Return verified cache record for ARTIFACT-PATH, IMAGE-PATH, and RUNTIME-PATH."
  (let* ((manifest-path
          (nelisp-artifact--sibling-manifest-path artifact-path))
         (manifest-sha (nelisp-artifact--sha256-file manifest-path))
         (runtime-sha (nelisp-artifact--sha256-file runtime-path))
         (image-sha (nelisp-artifact--sha256-file image-path))
         (header (or verified-header
                     (nelisp-artifact--flat-image-header image-path))))
    (list :format nelisp-artifact--flat-image-cache-format
          :flat-image-abi nelisp-artifact--flat-image-abi
          :generation
          (nelisp-artifact--flat-image-cache-generation
           manifest-sha runtime-sha image-sha)
          :artifact-generation-token
          (or artifact-generation-token
              (nelisp-artifact--flat-artifact-generation-token
               manifest-sha
               (plist-get manifest :artifact-sha256)
               runtime-sha))
          :artifact (expand-file-name artifact-path)
          :artifact-manifest-sha256 manifest-sha
          :kind (plist-get manifest :kind)
          :artifact-format (plist-get manifest :artifact-format)
          :artifact-sha256 (plist-get manifest :artifact-sha256)
          :artifact-size (plist-get manifest :artifact-size)
          :artifact-class (plist-get manifest :artifact-class)
          :runtime-abi (plist-get manifest :runtime-abi)
          :compiler (plist-get manifest :compiler)
          :nelisp-version (plist-get manifest :nelisp-version)
          :target (plist-get manifest :target)
          :source (plist-get manifest :source)
          :runtime-image (plist-get manifest :runtime-image)
          :preloads (plist-get manifest :preloads)
          :runtime-executable (expand-file-name runtime-path)
          :runtime-sha256 runtime-sha
          :runtime-build-id (concat "sha256:" runtime-sha)
          :image-sha256 image-sha
          :image-size (plist-get header :expected-size)
          :image-header header)))

(defun nelisp-artifact--read-flat-image-cache-sidecar (image-path)
  "Read IMAGE-PATH freshness sidecar, returning nil when absent or invalid."
  (let ((path (nelisp-artifact--flat-image-cache-sidecar-path image-path)))
    (and (file-exists-p path)
         (condition-case nil
             (nelisp-artifact--read-one-private-form
              (nelisp-artifact--read-file-as-string path)
              path)
           (error nil)))))

(defun nelisp-artifact--flat-image-cache-hit-p
    (artifact-path image-path runtime-path manifest)
  "Return non-nil when IMAGE-PATH exactly matches validated MANIFEST."
  (and (file-exists-p image-path)
       (let ((sidecar
              (nelisp-artifact--read-flat-image-cache-sidecar image-path)))
         (and sidecar
              (condition-case nil
                  (equal sidecar
                         (nelisp-artifact--flat-image-cache-record
                          artifact-path image-path runtime-path manifest))
                (error nil))))))

(defun nelisp-artifact--flat-image-cache-sidecar-hit
    (artifact-path image-path runtime-path)
  "Return trusted sidecar when the warm cache pair is current, else nil.
The exact manifest digest binds the cached semantic projection, so this path
does not parse the potentially multi-megabyte artifact manifest."
  (let* ((artifact (expand-file-name artifact-path))
         (image (expand-file-name image-path))
         (runtime (expand-file-name runtime-path))
         (sidecar
          (nelisp-artifact--read-flat-image-cache-sidecar image)))
    (condition-case nil
        (when (and sidecar
                   (file-exists-p artifact)
                   (file-exists-p image)
                   (file-exists-p runtime)
                   (eq (plist-get sidecar :format)
                       nelisp-artifact--flat-image-cache-format)
                   (equal (plist-get sidecar :flat-image-abi)
                          nelisp-artifact--flat-image-abi)
                   (equal (plist-get sidecar :artifact) artifact)
                   (equal (plist-get sidecar :runtime-executable) runtime)
                   (eq (plist-get sidecar :kind) 'neln)
                   (eq (plist-get sidecar :artifact-format)
                       nelisp-artifact--format)
                   (eq (plist-get sidecar :artifact-class)
                       nelisp-artifact--native-class)
                   (equal (plist-get sidecar :runtime-abi)
                          nelisp-artifact--native-runtime-abi)
                   (equal (plist-get sidecar :compiler)
                          (nelisp-artifact--compiler-plist))
                   (equal (plist-get sidecar :artifact-manifest-sha256)
                          (nelisp-artifact--sha256-file
                           (nelisp-artifact--sibling-manifest-path artifact)))
                   (equal (plist-get sidecar :artifact-size)
                          (nelisp-artifact--file-size artifact))
                   (equal (plist-get sidecar :artifact-sha256)
                          (nelisp-artifact--sha256-file artifact))
                   (equal (plist-get sidecar :runtime-sha256)
                          (nelisp-artifact--sha256-file runtime))
                   (equal
                    (plist-get sidecar :artifact-generation-token)
                    (nelisp-artifact--flat-artifact-generation-token
                     (plist-get sidecar :artifact-manifest-sha256)
                     (plist-get sidecar :artifact-sha256)
                     (plist-get sidecar :runtime-sha256)))
                   (equal (plist-get sidecar :image-size)
                          (nelisp-artifact--file-size image))
                   (equal (plist-get
                           (plist-get sidecar :image-header)
                           :expected-size)
                          (plist-get sidecar :image-size))
                   (= (plist-get (plist-get sidecar :image-header) :magic)
                      1179407692)
                   (equal (plist-get sidecar :image-sha256)
                          (nelisp-artifact--sha256-file image))
                   (equal
                    (plist-get sidecar :generation)
                    (nelisp-artifact--flat-image-cache-generation
                     (plist-get sidecar :artifact-manifest-sha256)
                     (plist-get sidecar :runtime-sha256)
                     (plist-get sidecar :image-sha256))))
          (let ((mv (plist-get sidecar :nelisp-version))
                (cv (and (boundp 'nelisp--cli-version) nelisp--cli-version)))
            (unless (and mv cv
                         (not (equal mv "unknown"))
                         (not (equal cv "unknown"))
                         (not (equal mv cv)))
              (nelisp-artifact--validate-input-record
               (plist-get sidecar :runtime-image) "runtime image" artifact)
              (dolist (rec (plist-get sidecar :preloads))
                (nelisp-artifact--validate-input-record
                 rec "preload" artifact))
              (nelisp-artifact--validate-input-record
               (plist-get sidecar :source) "source" artifact)
              sidecar)))
      (error nil))))

(defun nelisp-artifact--validate-flat-image-artifact (artifact-path)
  "Validate ARTIFACT-PATH for flat-image reuse without native metadata.
This is the warm-cache trust check: it verifies the exact artifact bytes,
manifest/compiler/ABI semantics, and every recorded freshness input, while
deliberately omitting `:native' / `:native-sections' because no module replay
or wrapper installation occurs on this path."
  (let* ((artifact (expand-file-name artifact-path))
         (manifest-path (nelisp-artifact--sibling-manifest-path artifact))
         (manifest
          (nelisp-artifact--read-manifest-fast
           artifact
           '(:format :kind :artifact-format :artifact-class :runtime-abi
             :artifact-sha256 :artifact-size :nelisp-version :target :source
             :runtime-image :preloads :load-path :compiler))))
    (unless (eq (plist-get manifest :format)
                nelisp-artifact--manifest-format)
      (signal 'nelisp-artifact-invalid
              (list "unsupported manifest format" manifest-path)))
    (unless (eq (plist-get manifest :kind) 'neln)
      (signal 'nelisp-artifact-invalid
              (list "flat image cache requires .neln" artifact)))
    (unless (and (eq (plist-get manifest :artifact-format)
                     nelisp-artifact--format)
                 (eq (plist-get manifest :artifact-class)
                     nelisp-artifact--native-class)
                 (equal (plist-get manifest :runtime-abi)
                        nelisp-artifact--native-runtime-abi)
                 (equal (plist-get manifest :compiler)
                        (nelisp-artifact--compiler-plist)))
      (signal 'nelisp-artifact-invalid
              (list "flat image artifact ABI/compiler mismatch" manifest-path)))
    (unless (and (equal (plist-get manifest :artifact-size)
                        (nelisp-artifact--file-size artifact))
                 (equal (plist-get manifest :artifact-sha256)
                        (nelisp-artifact--sha256-file artifact)))
      (signal 'nelisp-artifact-invalid
              (list "flat image artifact hash/size mismatch" artifact)))
    (let ((mv (plist-get manifest :nelisp-version))
          (cv (and (boundp 'nelisp--cli-version) nelisp--cli-version)))
      (when (and mv cv
                 (not (equal mv "unknown"))
                 (not (equal cv "unknown"))
                 (not (equal mv cv)))
        (signal 'nelisp-artifact-invalid
                (list "nelisp-version mismatch" mv cv artifact))))
    (nelisp-artifact--validate-input-record
     (plist-get manifest :runtime-image) "runtime image" artifact)
    (dolist (rec (plist-get manifest :preloads))
      (nelisp-artifact--validate-input-record rec "preload" artifact))
    (nelisp-artifact--validate-input-record
     (plist-get manifest :source) "source" artifact)
    manifest))

(defun nelisp-artifact-validate-flat-image-cache
    (artifact-path image-path runtime-path)
  "Validate a warm flat-image cache without replaying ARTIFACT-PATH.
Return a hit result plist, or nil when the image/sidecar pair is stale."
  (let* ((artifact (expand-file-name artifact-path))
         (image (expand-file-name image-path))
         (runtime (expand-file-name runtime-path)))
    (and (nelisp-artifact--flat-image-cache-sidecar-hit
          artifact image runtime)
         (list :status 'hit :artifact artifact :image image
               :runtime runtime
               :sidecar
               (nelisp-artifact--flat-image-cache-sidecar-path image)))))

(defun nelisp-artifact--finalize-flat-image-cache
    (artifact-path temp-image-path image-path runtime-path generation-token)
  "Validate and publish TEMP-IMAGE-PATH as IMAGE-PATH.
This runs in a fresh standalone process after a large arena dump.  Keeping
finalization outside the dumping evaluator avoids exposing newly allocated
metadata strings to its currently incomplete mid-form GC root set."
  (let* ((stage 'artifact)
         (artifact (expand-file-name artifact-path))
         (temp-image (expand-file-name temp-image-path))
         (image (expand-file-name image-path))
         (runtime (expand-file-name runtime-path))
         (manifest nil)
         (header nil)
         (sidecar (nelisp-artifact--flat-image-cache-sidecar-path image))
         (verify-log
          (nelisp-artifact--make-temp-path image "verify-generation")))
    (condition-case err
        (progn
          (unless (and (stringp generation-token)
                       (> (length generation-token) 0))
            (error "missing flat-image artifact generation token"))
          (setq manifest
                (nelisp-artifact--validate-flat-image-artifact artifact))
          (setq stage 'header)
          (setq header
                (if (fboundp 'nl-syscall-read-file)
                    (nelisp-artifact--validate-flat-image-header-via-od
                     temp-image)
                  (nelisp-artifact--flat-image-header temp-image)))
          (setq stage 'image-binding)
          (when (fboundp 'nl-syscall-read-file)
            (unless (= (call-process
                        runtime nil verify-log nil
                        "--cold-load-from" temp-image
                        "--eval"
                        (concat
                         "(unless (equal nelisp-artifact--flat-generation-token "
                         (prin1-to-string generation-token)
                         ") (error \"flat image artifact token mismatch\"))"))
                       0)
              (error "flat image artifact binding failed: %s"
                     (nelisp-artifact--read-file-as-string verify-log))))
          (setq stage 'rename)
          (rename-file temp-image image t)
          (setq stage 'record)
          (let ((record
                 (nelisp-artifact--flat-image-cache-record
                  artifact image runtime manifest header generation-token)))
            (setq stage 'sidecar)
            (nelisp-artifact--replace-file-atomically
             sidecar (concat (prin1-to-string record) "\n")))
          (nelisp-artifact--delete-if-exists verify-log)
          (list :status 'rebuilt :artifact artifact :image image
                :runtime runtime :sidecar sidecar))
      (error
       (let ((diagnostic
              (format "flat-finalizer stage=%S error=%S\n" stage err)))
         ;; The parent directs stdout to a bounded diagnostic file.  Stderr
         ;; remains inherited for interactive command diagnostics.
         (nelisp-artifact--write-stdout diagnostic)
         (nelisp-artifact--delete-if-exists verify-log)
         (error "flat-finalizer stage=%S error=%S" stage err))))))

(defun nelisp-artifact-prepare-flat-image-cache
    (artifact-path image-path runtime-path)
  "Ensure IMAGE-PATH is a fresh flat-arena snapshot of ARTIFACT-PATH.
ARTIFACT-PATH is fully validated, including its source freshness, exact
artifact hash, compiler format, and runtime ABI, before an existing image is
accepted.  Cache hits never replay the artifact module.  A miss loads the
artifact once, dumps the already-loaded heap through
`nelisp--arena-dump-image-stream', and writes a freshness sidecar.  Return a
plist containing `:status' (`hit' or `rebuilt') and absolute paths."
  (let* ((artifact (expand-file-name artifact-path))
         (image (expand-file-name image-path))
         (runtime (expand-file-name runtime-path))
         (sidecar (nelisp-artifact--flat-image-cache-sidecar-path image)))
    (if (nelisp-artifact--flat-image-cache-sidecar-hit
         artifact image runtime)
        (list :status 'hit :artifact artifact :image image
              :runtime runtime :sidecar sidecar)
      (let ((manifest
             (nelisp-artifact--validate-flat-image-artifact artifact))
            (external-finalize (fboundp 'nl-syscall-read-file))
            (generation-token nil))
        (unless (fboundp 'nelisp--arena-dump-image-stream)
          (error "flat arena image dumping is unavailable in this runtime"))
        (setq generation-token
              (nelisp-artifact--flat-artifact-generation-token
               (nelisp-artifact--sha256-file
                (nelisp-artifact--sibling-manifest-path artifact))
               (plist-get manifest :artifact-sha256)
               (nelisp-artifact--sha256-file runtime)))
        (setq nelisp-artifact--flat-generation-token generation-token)
        (nelisp-artifact-load-file artifact)
        (let ((temp (nelisp-artifact--make-temp-path image "tmp"))
              (finalize-log
               (nelisp-artifact--make-temp-path image "finalize-log"))
              (stage 'dump))
          (setq nelisp-artifact--flat-dump-temp temp)
          (unwind-protect
              (condition-case err
                  (progn
                    ;; Executable mmap addresses are process-local.  Registry
                    ;; metadata remains dumpable, but no live mapping pointer
                    ;; may cross the flat image boundary.
                    (nelisp-artifact-clear-native-runtime-mappings)
                    (let ((written
                           (nelisp--arena-dump-image-stream
                            nelisp-artifact--flat-dump-temp)))
                      (setq stage 'dump-length)
                      ;; Report BOTH numbers.  The message used to name only
                      ;; the path, so a mismatch on the 13.24 MB bootstrap said
                      ;; nothing about which side was wrong -- return value,
                      ;; short write, or missing file -- and the temp is removed
                      ;; on the way out, so nothing could be measured after the
                      ;; fact either.
                      (let ((actual
                             (if (fboundp 'nelisp--syscall-stat-field)
                                 ;; Substrate-level stat: the check runs AFTER
                                 ;; the replayed bundle may have redefined
                                 ;; file-attributes and friends (measured: the
                                 ;; temp held 491,925,008 bytes on disk while
                                 ;; the file-attributes path reported 0), so a
                                 ;; dump integrity check must not depend on the
                                 ;; user-replaceable file-* namespace.
                                 (let ((sz (nelisp--syscall-stat-field
                                            nelisp-artifact--flat-dump-temp 48)))
                                   (and (integerp sz) (>= sz 0) sz))
                               (and (file-exists-p
                                     nelisp-artifact--flat-dump-temp)
                                    (nelisp-artifact--file-size
                                     nelisp-artifact--flat-dump-temp)))))
                        (unless (and (integerp written) (> written 0)
                                     (integerp actual)
                                     (= written actual))
                          ;; Post-mortem forensics: the temp is deleted on the
                          ;; way out, so on mismatch preserve it under .keep
                          ;; and report a second, syscall-level size reading --
                          ;; the two paths disagreeing is itself the clue.
                          (nelisp-artifact--write-stderr
                           (format "dump-length forensics: syscall-size=%S exists=%S"
                                   (and (fboundp 'nelisp--syscall-stat-field)
                                        (nelisp--syscall-stat-field
                                         nelisp-artifact--flat-dump-temp 48))
                                   (file-exists-p
                                    nelisp-artifact--flat-dump-temp)))
                          (ignore-errors
                            (rename-file nelisp-artifact--flat-dump-temp
                                         (concat nelisp-artifact--flat-dump-temp
                                                 ".keep")
                                         t))
                          (error "flat arena image dump length mismatch: %s (returned %S, file %S, delta %S)"
                                 nelisp-artifact--flat-dump-temp written actual
                                 (and (integerp written) (integerp actual)
                                      (- written actual)))))
                      (if external-finalize
                          (progn
                            (setq stage 'finalize)
                            (unless (= (call-process
                                        runtime nil finalize-log nil
                                        "compile-runtime-image"
                                        "--flat-artifact-finalize"
                                        "--runtime" runtime
                                        "--input" artifact
                                        "--temp-image"
                                        nelisp-artifact--flat-dump-temp
                                        "--generation" generation-token
                                        "--output" image)
                                       0)
                              (error "fresh flat-image finalizer failed: %s"
                                     (nelisp-artifact--read-file-as-string
                                      finalize-log))))
                        (setq stage 'header)
                        (nelisp-artifact--flat-image-header
                         nelisp-artifact--flat-dump-temp written)
                        (setq stage 'publish)
                        (rename-file nelisp-artifact--flat-dump-temp image t))))
                (error
                 (error "flat image cache stage=%S/%S: %S"
                        stage nelisp-artifact--flat-header-stage err)))
            (nelisp-artifact--delete-if-exists
             nelisp-artifact--flat-dump-temp)
            (nelisp-artifact--delete-if-exists finalize-log)
            (setq nelisp-artifact--flat-dump-temp nil)))
        (unless external-finalize
          (nelisp-artifact--replace-file-atomically
           sidecar
           (concat
            (prin1-to-string
             (nelisp-artifact--flat-image-cache-record
              artifact image runtime manifest))
            "\n")))
        (list :status 'rebuilt :artifact artifact :image image
              :runtime runtime :sidecar sidecar)))))

(defun nelisp-artifact--parse-flat-image-cache-args (args)
  "Parse flat artifact image cache form of `compile-runtime-image' ARGS."
  (let ((rest (cdr args))
        (input nil)
        (output nil)
        (runtime nil)
        (temp-image nil)
        (generation nil)
        (finalize nil)
        (profile-load nil)
        (profile-load-detail nil))
    (while rest
      (let ((flag (car rest)))
        (cond
         ((equal flag "--flat-artifact-cache")
          (setq rest (cdr rest)))
         ((equal flag "--flat-artifact-finalize")
          (setq finalize t)
          (setq rest (cdr rest)))
         ((equal flag "--profile-load")
          (setq profile-load t)
          (setq rest (cdr rest)))
         ((equal flag "--profile-load-detail")
          (setq profile-load t)
          (setq profile-load-detail t)
          (setq rest (cdr rest)))
         ((or (equal flag "--input") (equal flag "--output")
              (equal flag "--runtime") (equal flag "--temp-image")
              (equal flag "--generation"))
          (unless (cadr rest)
            (error "missing value for %s" flag))
          (if (equal flag "--input")
              (setq input (cadr rest))
            (if (equal flag "--output")
                (setq output (cadr rest))
              (if (equal flag "--runtime")
                  (setq runtime (cadr rest))
                (if (equal flag "--temp-image")
                    (setq temp-image (cadr rest))
                  (setq generation (cadr rest))))))
          (setq rest (cddr rest)))
         (t (error "unsupported flat image cache flag %s" flag)))))
    (unless input
      (error "flat image cache requires --input FILE.neln"))
    (unless output
      (error "flat image cache requires --output FILE.flat.nlri"))
    (unless runtime
      (error "flat image cache requires --runtime NELISP-EXECUTABLE"))
    (unless (string-suffix-p ".neln" input)
      (error "flat image cache input must be .neln: %s" input))
    (unless (string-suffix-p ".flat.nlri" output)
      (error "flat image cache output must end in .flat.nlri: %s" output))
    (when (and finalize (null temp-image))
      (error "flat image finalization requires --temp-image FILE"))
    (when (and finalize (null generation))
      (error "flat image finalization requires --generation TOKEN"))
    (list :input input :output output :runtime runtime
          :temp-image temp-image :generation generation :finalize finalize
          :profile-load profile-load
          :profile-load-detail profile-load-detail)))

(defun nelisp-artifact--parse-compile-runtime-image-args (args)
  "Parse `compile-runtime-image' ARGS into a plist."
  (let ((opts (nelisp-artifact--parse-compile-args
               (cons "compile-elisp-artifact" (cdr args))
               t)))
    (when (equal (plist-get opts :kind) "elc")
      (error "compile-runtime-image does not support --kind elc"))
    opts))

(defun compile-runtime-image (args)
  "CLI entry point for `nelisp compile-runtime-image'."
  (condition-case err
      (if (or (member "--flat-artifact-cache" args)
              (member "--flat-artifact-finalize" args))
          (let* ((opts (nelisp-artifact--parse-flat-image-cache-args args))
                 (result
                  (if (plist-get opts :finalize)
                      (nelisp-artifact--finalize-flat-image-cache
                       (plist-get opts :input)
                       (plist-get opts :temp-image)
                       (plist-get opts :output)
                       (plist-get opts :runtime)
                       (plist-get opts :generation))
                    (let ((nelisp-artifact-profile-load
                           (plist-get opts :profile-load))
                          (nelisp-artifact-profile-load-detail
                           (plist-get opts :profile-load-detail)))
                      ;; Profiling a load without knowing whether the command
                      ;; runtime's own native sections installed makes the
                      ;; numbers unreadable: an all-bytecode decode looks like
                      ;; a decoder defect when it is really a link/preflight
                      ;; failure.  Summarise the dispatch events collected by
                      ;; the runtime-cache self-bootstrap before decoding.
                      (when nelisp-artifact-profile-load
                        (let ((rest (nelisp-artifact-native-dispatch-report)))
                          (nelisp-artifact--write-stderr
                           (format "artifact_load_profile runtime-native report-count=%d"
                                   (length rest)))
                          (while rest
                            (let ((e (car rest)))
                              (nelisp-artifact--write-stderr
                               (format
                                "artifact_load_profile runtime-native event=%s status=%s installed=%s skipped=%s error=%s"
                                (plist-get e :event) (plist-get e :status)
                                (plist-get e :installed)
                                (plist-get e :skipped)
                                (plist-get e :error))))
                            (setq rest (cdr rest)))))
                      (nelisp-artifact-prepare-flat-image-cache
                       (plist-get opts :input)
                       (plist-get opts :output)
                       (plist-get opts :runtime))))))
            (nelisp-artifact--write-stdout
             (format "flat-image-cache=%s image=%s\n"
                     (symbol-name (plist-get result :status))
                     (plist-get result :image)))
            0)
        (let* ((opts (nelisp-artifact--parse-compile-runtime-image-args args))
               (kind (intern (plist-get opts :kind)))
               (target (plist-get opts :target)))
          (let ((nelisp-artifact-profile-stages
                 (plist-get opts :profile-stages))
                (nelisp-artifact-profile-forms
                 (plist-get opts :profile-forms))
                (nelisp-artifact--rewrite-defalias-late
                 (plist-get opts :rewrite-defalias-late)))
            (if (nelisp-artifact--runtime-image-wasm-target-p target)
                (nelisp-artifact--compile-runtime-image-wasm
                 (plist-get opts :input)
                 (plist-get opts :output)
                 target
                 (plist-get opts :load-paths)
                 (plist-get opts :preloads)
                 (plist-get opts :requested-feature))
              (nelisp-artifact-compile-runtime-image-file
               (plist-get opts :input)
               (plist-get opts :output)
               (plist-get opts :manifest)
               target
               (plist-get opts :load-paths)
               (plist-get opts :preloads)
               (plist-get opts :requested-feature)
               kind
               (plist-get opts :native-policy)
               (plist-get opts :module-policy))))
          0))
    (error
     (nelisp-artifact--print-error
      (format "compile-runtime-image: %s" (error-message-string err)))
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
          (let ((kind (or (nelisp-artifact--artifact-kind-from-suffix path)
                          (nelisp-artifact--artifact-kind path))))
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
                (kind (or (nelisp-artifact--artifact-kind-from-suffix path)
                          (nelisp-artifact--artifact-kind path))))
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

(defun nelisp-artifact--parse-source-command-args (args &optional require-forms)
  "Parse source-load command ARGS.
When REQUIRE-FORMS is non-nil, at least one form after FILE.el is required."
  (let ((rest (cdr args))
        (auto-compile nil)
        (kind 'neln)
        (target nil)
        (load-paths nil)
        (preloads nil)
        (native-policy nil)
        (source nil)
        (forms nil))
    (while (and rest (null source))
      (let ((flag (car rest)))
        (cond
         ((equal flag "--auto-compile")
          (setq auto-compile t)
          (setq rest (cdr rest)))
         ((member flag '("--kind" "--target" "--load-path" "--preload"
                         "--native-policy"))
          (let ((value (cadr rest)))
            (unless value
              (error "missing value for %s" flag))
            (cond
             ((equal flag "--kind")
              (unless (member value '("nelc" "neln" "auto"))
                (error "unsupported --kind %s" value))
              (setq kind (if (equal value "auto") 'neln (intern value))))
             ((equal flag "--target") (setq target value))
             ((equal flag "--load-path")
              (setq load-paths (append load-paths (list value))))
             ((equal flag "--preload")
              (setq preloads (append preloads (list value))))
             ((equal flag "--native-policy")
              (setq native-policy
                    (nelisp-artifact--normalize-native-policy value))))
            (setq rest (cddr rest))))
         ((string-prefix-p "--" flag)
          (error "unknown flag %s" flag))
         (t
          (setq source flag)
          (setq forms (cdr rest))
          (setq rest nil)))))
    (unless source
      (error "source command requires FILE.el"))
    (when (and require-forms (null forms))
      (error "eval-elisp-source requires at least one FORM"))
    (list :source source
          :forms forms
          :auto-compile auto-compile
          :kind kind
          :target target
          :load-paths load-paths
          :preloads preloads
          :native-policy native-policy)))

(defun load-elisp-source (args)
  "CLI entry point for `nelisp load-elisp-source'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-source-command-args args))
             (hit (nelisp-artifact-load-source-or-source-file
                   (plist-get opts :source)
                   (plist-get opts :auto-compile)
                   (plist-get opts :kind)
                   (plist-get opts :target)
                   (plist-get opts :load-paths)
                   (plist-get opts :preloads)
                   (plist-get opts :native-policy))))
        (unless hit
          (error "cannot load source or adjacent artifact: %s"
                 (plist-get opts :source)))
        (nelisp-artifact--write-stdout
         (prin1-to-string (plist-get hit :value)))
        (nelisp-artifact--write-stdout "\n")
        0)
    (error
     (nelisp-artifact--print-error
      (format "load-elisp-source: %s" (error-message-string err)))
     1)))

(defun eval-elisp-source (args)
  "CLI entry point for `nelisp eval-elisp-source'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-source-command-args args t))
             (hit (nelisp-artifact-load-source-or-source-file
                   (plist-get opts :source)
                   (plist-get opts :auto-compile)
                   (plist-get opts :kind)
                   (plist-get opts :target)
                   (plist-get opts :load-paths)
                   (plist-get opts :preloads)
                   (plist-get opts :native-policy))))
        (unless hit
          (error "cannot load source or adjacent artifact: %s"
                 (plist-get opts :source)))
        (nelisp-artifact--write-stdout
         (prin1-to-string
          (nelisp-artifact--eval-forms
           (plist-get opts :forms)
           (if (plist-get hit :artifact)
               (or (nelisp-artifact--artifact-kind-from-suffix
                    (plist-get hit :artifact))
                   (nelisp-artifact--artifact-kind (plist-get hit :artifact)))
             nil))))
        (nelisp-artifact--write-stdout "\n")
        0)
    (error
     (nelisp-artifact--print-error
      (format "eval-elisp-source: %s" (error-message-string err)))
     1)))

(defun native-exec-elisp-artifact (args)
  "CLI entry point for `nelisp native-exec-elisp-artifact'."
  (let ((path (nth 1 args))
        (symbol (nth 2 args))
        (raw-args (cdddr args)))
    (if (or (null path) (null symbol))
        (progn
          (nelisp-artifact--print-error nelisp-artifact--usage)
          2)
      (condition-case err
	  (let ((native-args nil)
	        (all-integer-args nil)
	        (result nil)
	        (printed nil))
	    (setq native-args
	          (mapcar (lambda (arg)
	                    (if (nelisp-artifact--canonical-integer-token-p
	                         arg)
	                        (string-to-number arg)
	                      arg))
	                  raw-args))
	    (setq all-integer-args
	          (let ((rest native-args)
	                (ok t))
                    (while rest
                      (unless (integerp (car rest))
                        (setq ok nil))
                      (setq rest (cdr rest)))
            ok))
            (setq result
                  (if all-integer-args
                      (let* ((manifest (nelisp-artifact-read-manifest path))
                             (kind (plist-get manifest :kind))
                             (native
                              (nelisp-artifact--serialized-native-section-for-symbol
                               path symbol))
                             (meta (and native
                                        (nelisp-artifact--native-defun-metadata
                                         native symbol)))
                             (externs (and native
                                           (plist-get native :extern-symbols))))
                        (unless (eq kind 'neln)
                          (error "native-exec-elisp-artifact requires a .neln artifact, got %S"
                                 kind))
                        (unless native
                          (error "native symbol %s not in artifact %s"
                                 symbol path))
                        (if (and (nelisp-artifact--native-simple-integer-abi-p meta)
                                 (null externs))
                            (condition-case fast-err
                                (progn
                                  (nelisp-artifact-native-exec-fast-simple-write-stdout
                                   path symbol native-args)
                                  (setq printed t)
                                  nil)
                              (error
                               (condition-case simple-err
                                   (nelisp-artifact-native-exec
                                    path symbol native-args)
                                 (error
                                  (error "fast native exec failed: %s; simple native exec failed: %s"
                                         (error-message-string fast-err)
                                         (error-message-string simple-err))))))
                          (condition-case general-err
                              (nelisp-artifact-native-exec-general
                               path symbol native-args)
                            (error
                             (condition-case simple-err
                                 (nelisp-artifact-native-exec
                                  path symbol native-args)
                               (error
                                (error "general native exec failed: %s; simple native exec failed: %s"
                                       (error-message-string general-err)
                                       (error-message-string simple-err))))))))
	            (progn
                      (unless (eq (nelisp-artifact--artifact-kind-from-suffix
                                   path)
                                  'neln)
	                (let* ((manifest (nelisp-artifact-read-manifest path))
	                       (kind (plist-get manifest :kind)))
	                  (unless (eq kind 'neln)
	                    (error "native-exec-elisp-artifact requires a .neln artifact, got %S"
	                           kind))))
                      (nelisp-artifact-native-exec-general
                       path symbol native-args))))
            (unless printed
              (nelisp-artifact--write-stdout (prin1-to-string result))
              (nelisp-artifact--write-stdout "\n"))
            0)
        (error
         (nelisp-artifact--print-error
          (format "native-exec-elisp-artifact: artifact=%s format=%s phase=native-exec: %s"
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
          (let ((manifest (nelisp-artifact--read-manifest-for-inspect path)))
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
