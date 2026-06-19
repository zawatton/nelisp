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
(declare-function nelisp-elf-write-binary "nelisp-elf-write"
                  (file-path sections))
(declare-function nelisp--rd-one "nelisp-stdlib-prelude"
                  (source position length))
(declare-function nelisp--read-all-from-string-native "nelisp-standalone"
                  (source))

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
(defconst nelisp-artifact--native-section-version 2)
(defconst nelisp-artifact--usage
  "usage: nelisp compile-elisp-artifact --kind nelc|neln|elc|auto --input FILE.el --output FILE.nelc|FILE.neln|FILE.elc [--manifest FILE.manifest.el] [--load-path DIR]... [--preload FILE.el]... [--feature FEATURE] [--target TARGET] [--native-policy opportunistic|required] [--module-policy bytecode|eval-only] [--profile-stages] [--profile-forms] [--cache-key KEY]
       nelisp compile-elisp-artifacts --kind nelc|neln|auto [--load-path DIR]... [--preload FILE.el]... [--target TARGET] [--native-policy opportunistic|required] [--module-policy bytecode|eval-only] [--profile-stages] [--profile-forms] FILE.el|DIR...
       nelisp compile-runtime-image --kind nelc|neln|auto --input FILE.nlri --output FILE.nelc|FILE.neln [--native-policy opportunistic|required] [--module-policy bytecode|eval-only] [--profile-stages] [--profile-forms]
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
only the needed values, falling back to full plist parsing on unexpected input.")

(defvar nelisp-artifact--last-native-compile-report nil
  "Most recent `.neln' native compile coverage report.")

(defvar nelisp-artifact-default-native-policy 'opportunistic
  "Default native policy for `.neln' artifact compilation.
`opportunistic' means every `.el' file can produce a `.neln' artifact:
native-eligible top-level defuns are compiled, while unsupported defuns and
non-defun top-level forms keep bytecode/eval fallback.  `required' means every
top-level defun must enter the native section; otherwise compilation fails
before writing the artifact pair.")

(defvar nelisp-artifact-default-module-policy 'bytecode
  "Default module compile policy for private `.nelc' / `.neln' artifacts.
`bytecode' preserves the normal behavior: eligible top-level defuns are lowered
to NeLisp bytecode closures and other forms replay through `nelisp-eval'.
`eval-only' skips bytecode lowering and records every top-level form as replay.
It is intended for very large bootstrap substrates where proving the cache
boundary matters before the bytecode compiler is fast enough for CI.")

(defvar nelisp-artifact-profile-stages nil
  "Non-nil means artifact compile commands emit stage timings to stderr.")

(defvar nelisp-artifact-profile-forms nil
  "Non-nil means artifact compile commands emit per-form reader timings.")

(defvar nelisp-artifact-profile-load nil
  "Non-nil means private artifact loads emit stage timings to stderr.")

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
DETAIL, when non-nil, is appended as a compact Lisp datum."
  (when nelisp-artifact-profile-load
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
    found))

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

(defun nelisp-artifact--write-native-object-file (artifact-path path)
  "Extract ARTIFACT-PATH's embedded native object into PATH.
Use a small external pipeline first.  This avoids pulling the whole `.neln'
payload, including the large base64 object string, through standalone
NeLisp's reader/string compatibility layer."
  (let ((sh (and (fboundp 'executable-find)
                 (executable-find "sh")))
        (script "sed -n 's/.*:object-base64 \"\\([^\"]*\\)\" :text-size.*/\\1/p' \"$1\" | base64 -d > \"$2\""))
    (nelisp-artifact--delete-if-exists path)
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
       path))))

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

(defun nelisp-artifact--private-keyword-value-pos
    (source keyword label &optional missing-ok)
  "Return generated plist value start position for KEYWORD in SOURCE."
  (let* ((needle (concat (symbol-name keyword) " "))
         (pos (nelisp-artifact--string-search-literal needle source)))
    (if (null pos)
        (if missing-ok
            nil
          (error "missing private artifact key %S in %s" keyword label))
      (nelisp-read--skip-ws source (+ pos (length needle))))))

(defun nelisp-artifact--read-private-symbol-token
    (source keyword label &optional missing-ok)
  "Read KEYWORD's generated symbol value without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok)))
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
    (source keyword label &optional missing-ok)
  "Read KEYWORD's generated integer value without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok)))
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
    (source keyword label &optional missing-ok)
  "Read KEYWORD's generated string value without invoking the sexp reader."
  (let ((pos (nelisp-artifact--private-keyword-value-pos
              source keyword label missing-ok)))
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

(defun nelisp-artifact--plist-put-present (plist key value)
  "Return PLIST with KEY VALUE appended unless VALUE is the missing sentinel."
  (if (eq value nelisp-artifact--missing-key)
      plist
    (append plist (list key value))))

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

(defun nelisp-artifact--compile-top-level-form (form &optional module-policy)
  "Lower FORM into a `.nelc' module instruction (Doc 142 §6.1).
An eligible top-level `defun' becomes a precompiled (:fn NAME BCL)
install; every other form (and any defun the bytecode VM cannot lower)
becomes (:eval FORM) replayed through `nelisp-eval' at load."
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
    :artifact-schema-version 3
    :native-section-version 2))

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

(defun nelisp-artifact--native-defun-metadata (native symbol)
  "Return NATIVE defun metadata for SYMBOL."
  (let ((name (if (symbolp symbol) (symbol-name symbol) symbol))
        (found nil)
        (defs (plist-get native :defuns)))
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

(defun nelisp-artifact--install-native-functions (artifact-path native)
  "Install native wrappers from ARTIFACT-PATH and NATIVE metadata.
Each wrapper keeps the existing bytecode/interpreter fallback, so normal
NeLisp calls prefer native code when possible without losing semantic
coverage when a native executor rejects the call."
  (let ((symbols (plist-get native :symbols))
        (installed 0)
        (skipped 0))
    (dolist (name symbols)
      (let* ((sym (if (symbolp name) name (intern name)))
             (fallback (gethash sym nelisp--functions nelisp--unbound))
             (meta (nelisp-artifact--native-defun-metadata native sym)))
        (if (or (eq fallback nelisp--unbound) (null meta))
            (setq skipped (1+ skipped))
          (nelisp-artifact--install-function
           sym
           (nelisp-artifact--native-function-wrapper
            artifact-path sym fallback meta))
          (setq installed (1+ installed)))))
    (nelisp-artifact--note-native-dispatch
     (list :event 'install
           :artifact (expand-file-name artifact-path)
           :installed installed
           :skipped skipped))
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
                 (if (and (nelisp-artifact--all-integers-p args)
                          (nelisp-artifact--native-simple-integer-abi-p meta))
                     (condition-case _fast-err
                         (nelisp-artifact-native-exec-fast-simple
                          artifact symbol args)
                       (error
                        (nelisp-artifact-native-exec-general
                         artifact symbol args)))
                   (nelisp-artifact-native-exec-general
                    artifact symbol args))))
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
  "Return top-level defun forms in FORMS that have a symbol name."
  (seq-filter (lambda (f) (and (consp f) (eq (car f) 'defun)
                               (symbolp (nth 1 f))))
              forms))

(defun nelisp-artifact--native-unsupported-report (forms reason)
  "Return a native compile report for FORMS with shared failure REASON."
  (mapcar (lambda (form)
            (list :name (symbol-name (nth 1 form))
                  :native nil
                  :reason reason))
          (nelisp-artifact--native-defun-forms forms)))

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
                 "nelisp-asm-x86_64.el"
                 "nelisp-cc-runtime.el"
                 "nelisp-elf-write.el"
                 "nelisp-sexp-layout.el"
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

(defun nelisp-artifact--native-section-plist (obj unit arch symbols compile-report)
  "Return the serialized native section plist for OBJ/UNIT."
  (let* ((text-bytes (plist-get unit :text))
         (bytes (nelisp-artifact--read-binary obj)))
    (list :native-section-version
          nelisp-artifact--native-section-version
          :object-format nelisp-artifact--native-object-format
          :arch (symbol-name arch)
          :symbols symbols
          :object-size (length bytes)
          :object-sha256 (secure-hash 'sha256 bytes)
          :object-base64 (base64-encode-string bytes t)
          :text-size (length text-bytes)
          :text-base64 (base64-encode-string text-bytes t)
          :relocs (plist-get unit :relocs)
          :extern-symbols (plist-get unit :extern-symbols)
          :compile-report compile-report
          :defuns (mapcar #'nelisp-artifact--native-defun-entry
                          (plist-get unit :defuns)))))

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
                (setq unit
                      (nelisp-aot-compile-to-link-unit
                       (cons 'seq eligible)
                       :arch arch :format 'elf))
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

(defun nelisp-artifact--native-compile-fast-batch-section (defuns arch)
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
                      compile-report)
                (setq native
                      (nelisp-artifact--native-section-plist
                       obj unit arch symbols compile-report)))
            (error
             (setq native nil)))
        (nelisp-artifact--delete-if-exists obj))
      native)))

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
      (setq defuns (nelisp-artifact--native-defun-forms forms))
      (nelisp-artifact--profile-log
       "native-defun-forms" stage-start
       (list :forms (length forms) :defuns (length defuns)))
      (let ((eligible nil)
            (symbols nil)
            (compile-report nil))
        (if (eq policy 'required)
            (nelisp-artifact--native-compile-required-section defuns arch)
          (or (nelisp-artifact--native-compile-fast-batch-section defuns arch)
              (progn
                (dolist (d defuns)
                  (let ((probe (nelisp-artifact--make-temp-path "neln-probe" "o")))
                    (condition-case err
                        (progn
                          (nelisp-aot-compile-to-object d probe :arch arch :format 'elf)
                          (push d eligible)
                          (push (symbol-name (nth 1 d)) symbols)
                          (push (list :name (symbol-name (nth 1 d))
                                      :native t)
                                compile-report))
                      (error
                       (push (list :name (symbol-name (nth 1 d))
                                   :native nil
                                   :reason (error-message-string err))
                             compile-report)))
                    (nelisp-artifact--delete-if-exists probe)))
                (setq nelisp-artifact--last-native-compile-report
                      (nreverse compile-report))
                (when eligible
                  (let ((obj (nelisp-artifact--make-temp-path "neln-obj" "o")))
                    (unwind-protect
                        (let* ((unit
                                (nelisp-aot-compile-to-link-unit
                                 (cons 'seq (nreverse eligible))
                                 :arch arch :format 'elf)))
                          (nelisp-artifact--write-elf-rel-object obj unit)
                          (nelisp-artifact--native-section-plist
                           obj unit arch (nreverse symbols)
                           nelisp-artifact--last-native-compile-report))
                      (nelisp-artifact--delete-if-exists obj))))))))))))

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
         (native-report (plist-get payload :native-report))
         (module (plist-get payload :module-init))
         (module-start (nelisp-artifact--profile-time))
         (module-string
          (if eval-source
              (nelisp-artifact--eval-source-module-string eval-source)
            (nelisp-artifact--printed-list-string module)))
         (wrap-start nil))
    (nelisp-artifact--profile-log
     "artifact-module-string" module-start
     (list :eval-source (and eval-source t)
           :items (length module)
           :bytes (length module-string)))
    (setq wrap-start (nelisp-artifact--profile-time))
    (prog1
        (concat
         nelisp-artifact--magic
         "(:format " (prin1-to-string (plist-get payload :format))
         " :kind " (prin1-to-string kind)
         " :source " (prin1-to-string (plist-get payload :source))
         " :module-init " module-string
         " :features " (prin1-to-string (plist-get payload :features))
         " :top-level-count " (prin1-to-string
                                (plist-get payload :top-level-count))
         " :module-policy " (prin1-to-string
                              (plist-get payload :module-policy))
         " :compiler " (prin1-to-string (plist-get payload :compiler))
         (if native
             (concat " :native " (prin1-to-string native))
           "")
         (if (eq kind 'neln)
             (concat " :native-report " (prin1-to-string native-report))
           "")
         " :entry " (prin1-to-string (plist-get payload :entry))
         ")\n")
      (nelisp-artifact--profile-log "artifact-wrap-string" wrap-start))))

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
                         :compile-report (plist-get native :compile-report)
                         :defuns (plist-get native :defuns))))
   (when (eq kind 'neln)
     (list :native-report native-report))
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
    (setq forms (nelisp-artifact--read-top-level-forms source source-path))
    (nelisp-artifact--profile-log
     "read-forms" stage-start
     (list :forms (length forms) :source source-path))
    (setq stage-start (nelisp-artifact--profile-time))
    (let ((load-path (append load-paths load-path))
          (nelisp-load-path (append load-paths nelisp-load-path)))
      (dolist (preload preloads)
        (load preload nil t))
      (setq module
            (mapcar (lambda (form)
                      (nelisp-artifact--compile-top-level-form
                       form module-policy))
                    forms)))
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
               (> (length source) nelisp-artifact-raw-eval-source-threshold))
      (setq eval-source source))
    (setq artifact-content
          (nelisp-artifact--artifact-string artifact-payload eval-source))
    (nelisp-artifact--profile-log
     "artifact-string" stage-start
     (list :bytes (length artifact-content)))
    (setq stage-start (nelisp-artifact--profile-time))
    (setq manifest
          (nelisp-artifact--manifest-plist
           source-path features (length forms) target
	           (secure-hash 'sha256 artifact-content)
           (length artifact-content)
	           (nelisp-artifact--preload-records preloads)
	           load-paths kind native native-report native-policy
                   module-policy))
    (nelisp-artifact--profile-log
     "manifest" stage-start
     (list :kind kind :module-policy module-policy))
    (setq stage-start (nelisp-artifact--profile-time))
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
    (unless (string-prefix-p ";;; nelisp-runtime-image source-v1\n" source)
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
           (native (and manifest (plist-get manifest :native))))
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
  (if (fboundp 'nelisp--rd-one)
      (condition-case nil
          (car (nelisp--rd-one source start end))
        (error
         (car (read-from-string (substring source start end)))))
    (car (read-from-string (substring source start end)))))

(defun nelisp-artifact--replay-module-item (item)
  "Replay one module ITEM and return its resulting value."
  (cond
   ((and (consp item) (eq (car item) :fn))
    (nelisp-artifact--install-function (nth 1 item) (nth 2 item))
    (nth 1 item))
   ((and (consp item) (eq (car item) :eval))
    (nelisp-eval (nth 1 item)))
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

(defun nelisp-artifact--read-decimal-at (source pos)
  "Return (NUMBER . END) for decimal digits in SOURCE at POS."
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

(defun nelisp-artifact--replay-module-streaming (content artifact-path)
  "Replay CONTENT's `:module-init' list without materializing the whole list."
  (let* ((pos (nelisp-artifact--private-keyword-value-pos
               content :module-init artifact-path))
         (len (length content))
         (last nil)
         (count 0)
         (done nil)
         end item)
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
        (let ((raw (nelisp-artifact--replay-raw-eval-source-item content pos)))
          (if raw
              (progn
                (setq last (cadr raw))
                (setq pos len)
                (setq done t))
            (setq end (nelisp-artifact--private-item-end
                       content pos len artifact-path))
            (let ((fast (nelisp-artifact--replay-generated-eval-source-item
                         content pos end)))
              (if fast
                  (setq last (cdr fast))
                (setq item (nelisp-artifact--read-private-item content pos end))
                (setq last (nelisp-artifact--replay-module-item item))))
            (setq pos end)))
        (setq count (1+ count))
        (when (and nelisp-artifact-profile-load
                   (= 0 (% count 100)))
          (nelisp-artifact--write-stderr
           (concat "artifact_load_profile progress=module-item"
                   " count=" (number-to-string count)
                   " pos=" (number-to-string pos))))
        )))
    last))

(defun nelisp-artifact--load-private-fast (full-path content manifest)
  "Load private artifact CONTENT using generated-key and streaming readers."
  (let* ((total-start (nelisp-artifact--profile-time))
         (key-start total-start)
         (prefix-len (length nelisp-artifact--magic))
         (format (nelisp-artifact--read-private-keyword-value
                  content :format full-path nil prefix-len))
         (features (nelisp-artifact--read-private-keyword-value
                    content :features full-path t prefix-len))
         (kind (or (plist-get manifest :kind)
                   (nelisp-artifact--read-private-keyword-value
                    content :kind full-path nil prefix-len)))
         (native (and manifest (plist-get manifest :native)))
         (last nil))
    (nelisp-artifact--load-profile-log "fast-key-read" key-start
                                       (list :kind kind))
    (unless (string-prefix-p nelisp-artifact--magic content)
      (signal 'nelisp-artifact-invalid
              (list "invalid .nelc magic header" full-path)))
    (unless (eq format nelisp-artifact--format)
      (signal 'nelisp-artifact-invalid
              (list "unsupported .nelc format" format full-path)))
    (let ((replay-start (nelisp-artifact--profile-time)))
      (setq last (nelisp-artifact--replay-module-streaming content full-path))
      (nelisp-artifact--load-profile-log "fast-replay" replay-start))
    (when (and (eq kind 'neln) native nelisp-artifact-native-dispatch-enabled)
      (let ((native-start (nelisp-artifact--profile-time)))
        (nelisp-artifact--install-native-functions full-path native)
        (nelisp-artifact--load-profile-log "native-install" native-start)))
    (unless (eq features nelisp-artifact--missing-key)
      (let ((feature-start (nelisp-artifact--profile-time)))
        (dolist (feature features)
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

(defun nelisp-artifact--read-manifest-fast (artifact-path &optional keys)
  "Read ARTIFACT-PATH's sibling manifest via generated-key scanner.
When KEYS is non-nil, read only those top-level keys."
  (let* ((manifest-path (nelisp-artifact--sibling-manifest-path artifact-path))
         (source (nelisp-artifact--read-file-as-string manifest-path))
         (manifest nil)
         (compiler-text (prin1-to-string (nelisp-artifact--compiler-plist))))
    (dolist (key (or keys
                     '(:format :kind :artifact-format :artifact-class
                       :runtime-abi :artifact-sha256 :artifact-size
                       :nelisp-version :target :source :runtime-image
                       :preloads :load-path :features :top-level-count
                       :compiler :native-policy :native :native-report
                       :emacs-compat :entry)))
      (setq manifest
            (nelisp-artifact--plist-put-present
             manifest key
             (cond
              ((memq key '(:format :kind :artifact-format :artifact-class
                           :native-policy))
               (nelisp-artifact--read-private-symbol-token
                source key manifest-path t))
              ((memq key '(:runtime-abi :artifact-sha256 :nelisp-version
                           :target))
               (nelisp-artifact--read-private-string-token
                source key manifest-path t))
              ((memq key '(:artifact-size :top-level-count))
               (nelisp-artifact--read-private-integer-token
                source key manifest-path t))
              ((eq key :compiler)
               (if (nelisp-artifact--string-search-literal
                    (concat ":compiler " compiler-text)
                    source)
                   (nelisp-artifact--compiler-plist)
                 (nelisp-artifact--read-private-keyword-value
                  source key manifest-path t)))
              (t
               (nelisp-artifact--read-private-keyword-value
                source key manifest-path t))))))
    manifest))

(defun nelisp-artifact--read-manifest-for-load (artifact-path)
  "Read only manifest fields needed by private artifact load/validation."
  (if nelisp-artifact-fast-private-read
      (condition-case nil
          (nelisp-artifact--read-manifest-fast
           artifact-path
           '(:format :kind :artifact-format :artifact-class :runtime-abi
             :artifact-sha256 :artifact-size :nelisp-version :source
             :runtime-image :preloads :compiler :native))
        (error
         (nelisp-artifact--read-manifest-full artifact-path)))
    (nelisp-artifact--read-manifest-full artifact-path)))

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
                         (secure-hash
                          'sha256
                          (nelisp-artifact--read-file-as-string path)))
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
	      ;; Source freshness: only enforced when the recorded source still
	      ;; exists.  Prefer :truename (symlink-resolved) over :path.  FAST
	      ;; PATH: if size, mtime, and ctime all match the manifest, treat the
	      ;; source as unchanged WITHOUT re-hashing it — re-reading the whole
	      ;; source on every load would defeat the cache (it is exactly the
	      ;; cost the artifact is meant to avoid).  Only when metadata differ
	      ;; (or were not recorded) do we read + sha256 to decide stale-ness
	      ;; precisely.
      (let* ((src (plist-get manifest :source))
             (spath (and src (or (plist-get src :truename)
                                 (plist-get src :path))))
             (swant (and src (plist-get src :sha256))))
        (when (and spath swant (file-exists-p spath))
	          (let ((want-size (plist-get src :size))
	                (want-mtime (plist-get src :mtime))
	                (want-ctime (plist-get src :ctime)))
	            (unless (and want-size want-mtime want-ctime
	                         (equal want-size (nelisp-artifact--file-size spath))
	                         (equal want-mtime (nelisp-artifact--file-mtime spath))
	                         (equal want-ctime (nelisp-artifact--file-ctime spath)))
	              ;; size/mtime changed or unrecorded → hash to be sure.
	              (unless (equal swant
	                             (secure-hash 'sha256
                                          (nelisp-artifact--read-file-as-string spath)))
                (signal 'nelisp-artifact-stale
                        (list "source changed since compile"
                              spath artifact-path)))))))
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
      (nelisp-artifact--validate-elc full-path)
      (load full-path nil t)
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
          (nelisp-artifact--load-profile-log "validate" validate-start)
          (if nelisp-artifact-fast-private-read
              (condition-case nil
                  (let ((last (nelisp-artifact--load-private-fast
                               full-path content manifest)))
                    (setq nelisp-artifact--loaded
                          (cons full-path nelisp-artifact--loaded))
                    (nelisp-artifact--load-profile-log "load-total"
                                                       total-start
                                                       '(:path fast))
                    last)
                (error
                 (nelisp-artifact--load-profile-log "fast-fallback"
                                                    total-start)
                 (let* ((payload (nelisp-artifact--parse-payload
                                  content full-path))
                        (module (plist-get payload :module-init))
                        (features (plist-get payload :features))
                        (native (plist-get payload :native))
                        (last nil))
                   ;; Replay the module onto the NeLisp runtime: install
                   ;; precompiled bytecode closures into the function table,
                   ;; `nelisp-eval' the remaining top-level effects.
                   (dolist (item module)
                     (setq last (nelisp-artifact--replay-module-item item)))
                   (when (and native nelisp-artifact-native-dispatch-enabled)
                     (nelisp-artifact--install-native-functions full-path native))
                   (dolist (feature features)
                     (when (fboundp 'nelisp-provide)
                       (nelisp-provide feature))
                     (unless (featurep feature)
                       (provide feature)))
                   (setq nelisp-artifact--loaded
                         (cons full-path nelisp-artifact--loaded))
                   (nelisp-artifact--load-profile-log "load-total"
                                                      total-start
                                                      '(:path fallback))
                   last)))
            (let* ((payload (nelisp-artifact--parse-payload content full-path))
                   (module (plist-get payload :module-init))
                   (features (plist-get payload :features))
                   (native (plist-get payload :native))
                   (last nil))
              ;; Replay the module onto the NeLisp runtime: install
              ;; precompiled bytecode closures into the function table,
              ;; `nelisp-eval' the remaining top-level effects.
              (dolist (item module)
                (setq last (nelisp-artifact--replay-module-item item)))
              (when (and native nelisp-artifact-native-dispatch-enabled)
                (nelisp-artifact--install-native-functions full-path native))
              (dolist (feature features)
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
  "Return the adjacent artifact path for SOURCE-PATH and KIND."
  (concat (expand-file-name source-path) "." (symbol-name kind)))

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
           (native (plist-get manifest :native))
           (symbols (and native (plist-get native :symbols))))
      (unless native
        (error "%s has no embedded native object" artifact-path))
      (unless (member symbol symbols)
        (error "native symbol %s not in artifact (have %S)" symbol symbols))
      (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec"))
             (obj (expand-file-name "mod.o" dir))
             (obj2 (expand-file-name "mod-c.o" dir))
             (csrc (expand-file-name "drv.c" dir))
             (exe (expand-file-name "run" dir))
             (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol))
             (argc (length args)))
        (unwind-protect
            (progn
              (nelisp-artifact--write-native-object-file artifact-path obj)
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
    (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec-fast"))
           (obj (expand-file-name "mod.o" dir))
           (obj2 (expand-file-name "mod-c.o" dir))
           (csrc (expand-file-name "drv.c" dir))
           (built-exe (expand-file-name "run" dir))
           (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol)))
      (unwind-protect
          (progn
            (nelisp-artifact--write-native-object-file artifact-path obj)
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
    (let* ((dir (nelisp-artifact--make-temp-directory "neln-exec-fast"))
           (obj (expand-file-name "mod.o" dir))
           (obj2 (expand-file-name "mod-c.o" dir))
           (csrc (expand-file-name "drv.c" dir))
           (exe (expand-file-name "run" dir))
           (csym (replace-regexp-in-string "[^A-Za-z0-9_]" "_" symbol))
           (argc (length args)))
      (unwind-protect
          (progn
            (nelisp-artifact--write-native-object-file artifact-path obj)
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
  (let* ((manifest (nelisp-artifact-read-manifest artifact-path))
         (native (plist-get manifest :native))
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
            (nelisp-artifact--write-native-object-file artifact-path obj)
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
        (requested-feature nil)
        (native-policy nil)
        (module-policy nil)
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
	              :requested-feature requested-feature
	              :native-policy native-policy
	              :module-policy module-policy
	              :profile-stages profile-stages
	              :profile-forms profile-forms)))))

(defun nelisp-artifact--standalone-runtime-p ()
  "Return non-nil when running inside the generated standalone CLI."
  (and (fboundp 'nelisp--write-stdout-bytes)
       (boundp 'nelisp-artifact-standalone-repo-root)
       (stringp nelisp-artifact-standalone-repo-root)
       (> (length nelisp-artifact-standalone-repo-root) 0)))

(defun nelisp-artifact--host-helper-emacs ()
  "Return the host Emacs executable for standalone helper builds, or nil."
  (let ((candidate (or (and (fboundp 'getenv)
                            (getenv "NELISP_HOST_EMACS"))
                       "emacs")))
    (and (fboundp 'executable-find)
         (executable-find candidate))))

(defun nelisp-artifact--standalone-host-helper-eligible-p (opts kind)
  "Return non-nil when OPTS/KIND should use the host native build helper."
  (and (nelisp-artifact--standalone-runtime-p)
       (eq kind 'neln)
       (eq (plist-get opts :native-policy) 'required)
       (not (and (fboundp 'getenv)
                 (equal (getenv "NELISP_DISABLE_HOST_HELPER") "1")))
       (nelisp-artifact--host-helper-emacs)
       (file-exists-p
        (expand-file-name "lisp/nelisp-artifact.el"
                          nelisp-artifact-standalone-repo-root))))

(defun nelisp-artifact--standalone-host-helper-compile (opts kind)
  "Compile one required-native artifact through host Emacs.
Return non-nil when the helper was used successfully.  On failure, emit a
compact diagnostic and return nil so the caller can fall back to the native
standalone implementation."
  (when (nelisp-artifact--standalone-host-helper-eligible-p opts kind)
    (let* ((start (nelisp-artifact--profile-time))
           (root nelisp-artifact-standalone-repo-root)
           (emacs (nelisp-artifact--host-helper-emacs))
           (log (nelisp-artifact--make-temp-path "nelisp-host-helper" "log"))
           (eval-form
            (concat
             "(progn (setq load-prefer-newer t)"
             " (require 'nelisp-artifact)"
             " (let ((nelisp-artifact-profile-stages "
             (prin1-to-string (plist-get opts :profile-stages))
             ") (nelisp-artifact-profile-forms "
             (prin1-to-string (plist-get opts :profile-forms))
             ")) (nelisp-artifact-compile-file "
             (prin1-to-string (plist-get opts :input)) " "
             (prin1-to-string (plist-get opts :output)) " "
             (prin1-to-string (plist-get opts :manifest)) " "
             (prin1-to-string (plist-get opts :target)) " "
             (prin1-to-string (plist-get opts :load-paths)) " "
             (prin1-to-string (plist-get opts :preloads)) " "
             (prin1-to-string (plist-get opts :requested-feature)) " "
             (prin1-to-string (list 'quote kind)) " "
             (prin1-to-string (list 'quote (plist-get opts :native-policy))) " "
             (let ((mp (plist-get opts :module-policy)))
               (if mp
                   (prin1-to-string (list 'quote mp))
                 "nil"))
             ")))"))
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
              (nelisp-artifact--write-stderr
               (format "host-helper failed status=%S: %s"
                       status
                       (nelisp-artifact--read-log-if-exists log)))
              nil))
        (nelisp-artifact--delete-if-exists log)))))

(defun compile-elisp-artifact (args)
  "CLI entry point for `nelisp compile-elisp-artifact'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-compile-args args))
             (kind (intern (plist-get opts :kind))))
        (let ((nelisp-artifact-profile-stages
               (plist-get opts :profile-stages))
              (nelisp-artifact-profile-forms
               (plist-get opts :profile-forms)))
          (if (eq kind 'elc)
              (nelisp-artifact-compile-elc-file
               (plist-get opts :input)
               (plist-get opts :output)
               (plist-get opts :manifest)
               (plist-get opts :target)
               (plist-get opts :load-paths)
               (plist-get opts :preloads)
               (plist-get opts :requested-feature))
            (or (nelisp-artifact--standalone-host-helper-compile opts kind)
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
	             (plist-get opts :module-policy)))))
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

(defun nelisp-artifact--parse-compile-runtime-image-args (args)
  "Parse `compile-runtime-image' ARGS into a plist."
  (let ((opts (nelisp-artifact--parse-compile-args
               (cons "compile-elisp-artifact" (cdr args)))))
    (when (equal (plist-get opts :kind) "elc")
      (error "compile-runtime-image does not support --kind elc"))
    opts))

(defun compile-runtime-image (args)
  "CLI entry point for `nelisp compile-runtime-image'."
  (condition-case err
      (let* ((opts (nelisp-artifact--parse-compile-runtime-image-args args))
             (kind (intern (plist-get opts :kind))))
        (let ((nelisp-artifact-profile-stages
               (plist-get opts :profile-stages))
              (nelisp-artifact-profile-forms
               (plist-get opts :profile-forms)))
          (nelisp-artifact-compile-runtime-image-file
           (plist-get opts :input)
           (plist-get opts :output)
	           (plist-get opts :manifest)
	           (plist-get opts :target)
	           (plist-get opts :load-paths)
	           (plist-get opts :preloads)
	           (plist-get opts :requested-feature)
	           kind
	           (plist-get opts :native-policy)
	           (plist-get opts :module-policy)))
        0)
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
                             (native (plist-get manifest :native))
                             (meta (and native
                                        (nelisp-artifact--native-defun-metadata
                                         native symbol)))
                             (externs (and native
                                           (plist-get native :extern-symbols))))
                        (unless (eq kind 'neln)
                          (error "native-exec-elisp-artifact requires a .neln artifact, got %S"
                                 kind))
                        (if (and (nelisp-artifact--native-simple-integer-abi-p meta)
                                 (null externs))
                            (condition-case fast-err
                                (progn
                                  (nelisp-artifact--write-stdout
                                   (nelisp-artifact-native-exec-fast-simple-stdout
                                    path symbol native-args))
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
