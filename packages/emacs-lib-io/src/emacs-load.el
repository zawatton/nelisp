;;; emacs-load.el --- Standalone load/load-file override  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Provenance: copied from
;; /home/madblack-21/Cowork/Notes/dev/nelisp/lisp/nelisp-stdlib-misc.el
;; lines 495-580.  Keep this file in sync with that donor block.
;;
;; Local delta: keep the override standalone-only in this repository, and
;; preserve absolute-path `load' when `locate-library' does not resolve it.

;;; Code:

(declare-function nelisp--syscall-stat-field "nelisp-runtime" (path offset))

(when (or (fboundp 'rdf)
          (fboundp 'nelisp--eval-source-string)
          (not (boundp 'emacs-version))
          (not (stringp emacs-version)))
  (defvar load-garbage-collect-interval 64
    "Number of forms between opportunistic `garbage-collect' calls in `load'.
Nil or 0 disables the periodic collection.  The standalone reader uses a
flat arena, so large source files must not keep every already-read
top-level form reachable until the end of the load.")

  (defvar emacs-load-large-source-threshold 32768
    "Byte threshold above which source loads use the incremental evaluator.
Nil or a non-positive value disables the size-based routing and preserves
the historical hybrid path for all source loads that can use it.")

  (defvar nelisp--cc-replay-file nil
    "Base name (sans extension) of the CC Mode file currently being loaded.
The outer `(setq load-file-name RESOLVED)' in `nelisp--load-resolved-file'
is not visible to the interpreted elisp running inside the native source
evaluator, so `c-get-current-file' cannot recover the per-file identity of
each cc-*.el file from `load-file-name'.  A marker `setq' spliced at the
head of each cc-*.el source establishes this variable inside the same
eval-source-string, which the redefined `c-get-current-file' consults.")

  (defvar nelisp--cc-replay-file-stack nil
    "Saved `nelisp--cc-replay-file' values across nested cc-*.el loads.
A cc-*.el file may `require' another (e.g. cc-fonts -> cc-langs); the head
splice pushes the enclosing marker here and the tail splice pops it back so
the outer file's identity is restored after the nested load returns.")

  (defun nelisp--load-skip-space-and-comments (source pos)
    "Return first non-whitespace/comment position in SOURCE at or after POS."
    (let ((len (length source))
          (done nil))
      (while (and (< pos len) (not done))
        (let ((c (aref source pos)))
          (cond
           ((or (= c ?\s) (= c ?\t) (= c ?\n) (= c ?\r) (= c ?\f))
            (setq pos (+ pos 1)))
           ((= c ?\;)
            (while (and (< pos len) (not (= (aref source pos) ?\n)))
              (setq pos (+ pos 1))))
          (t
            (setq done t)))))
      pos))

  (defun nelisp--load-map-source-forms (source callback)
    "Read SOURCE one top-level form at a time and call CALLBACK on each.
The reader uses the same whitespace/comment skipping and closing-paren
compatibility as the standalone normalizer so large forms still advance
correctly when `read-from-string' stops on a trailing close paren.
Return the last CALLBACK result."
    (let ((pos 0)
          (len (length source))
          (last nil)
          (count 0)
          (read nil))
      (while (progn
               (setq pos (nelisp--load-skip-space-and-comments source pos))
               (< pos len))
        (setq read (read-from-string source pos))
        (let ((next (cdr read)))
          (when (and (> next pos)
                     (< next len)
                     (= (aref source next) ?\)))
            (setq next (+ next 1)))
          (when (or (not (consp read)) (<= next pos))
            (signal 'end-of-file
                    (list "rewrite load reader made no progress" pos)))
          (setq last
                (funcall callback
                         (nelisp--load-rewrite-defalias-form (car read))))
          (setq pos next)
          (setq count (+ count 1))
          (when (and load-garbage-collect-interval
                     (> load-garbage-collect-interval 0)
                     (= (% count load-garbage-collect-interval) 0)
                     (fboundp 'garbage-collect))
            (garbage-collect))))
      last))

  (defun nelisp--load-eval-source-incremental (source)
    "Read and eval SOURCE top-level forms one at a time.
Return the value of the last form.  This deliberately avoids
`nelisp--read-all-from-string', which materializes the whole AST and can
overflow the standalone arena on upstream-sized package files."
    (nelisp--load-map-source-forms
     source
     (lambda (form)
       ;; NeLisp's source evaluator bare-aborts on the CC Mode
       ;; compile-time marker even when `cc-provide' is callable.
       ;; At interpreted load time its semantics are exactly `provide'.
       (when (and (consp form) (eq (car form) 'cc-provide))
         (setcar form 'provide))
       (eval form))))

  (defun nelisp--load-normalize-source-rewriting (source)
    "Return SOURCE normalized for native one-shot evaluation.
Top-level forms are read with `nelisp--load-skip-space-and-comments',
rewritten structurally with `nelisp--load-rewrite-defalias-form', and
serialized with `prin1-to-string'.  The result preserves the fast native
throughput path while fixing forward and macro alias semantics without
evaluating any forms here."
    (let ((forms nil))
      (nelisp--load-map-source-forms
       source
       (lambda (form)
         (let ((text (prin1-to-string form)))
           (push text forms)
           text)))
      (mapconcat #'identity (nreverse forms) "\n")))

  (defun emacs-load--artifact-write-normalized-source (source path)
    "Write SOURCE normalized for artifact compilation to PATH incrementally."
    (with-temp-file path
      (insert ";;; nelisp-private-nelc-v2\n")
      (nelisp--load-map-source-forms
       source
       (lambda (form)
         (prin1 form (current-buffer))
         (terpri (current-buffer))
         form))))

  (defun emacs-load--artifact-write-raw-source (source path &optional header)
    "Write SOURCE directly to PATH for low-memory compiler staging.
When HEADER is non-nil, insert it before SOURCE; otherwise prepend the
standard harmless artifact staging comment."
    (with-temp-file path
      (insert (or header ";;; nelisp-private-nelc-v2\n"))
      (insert source)))

  (defun nelisp--load-eval-source-hybrid (source)
    "Evaluate SOURCE once after normalizing top-level defalias rewrites.
This is the correctness-first standalone load path; cache and throughput
work belong elsewhere."
    (nelisp--eval-source-string
     (nelisp--load-normalize-source-rewriting source)))

  (defun nelisp--load-source-large-p (source)
    "Return non-nil when SOURCE should use incremental loading."
    (let ((threshold emacs-load-large-source-threshold))
      (and (integerp threshold)
           (> threshold 0)
           (> (emacs-load--artifact-byte-length source) threshold))))

  (defun nelisp--load-source-loader (source)
    "Return the preferred source evaluator for SOURCE."
    (if (or (not (fboundp 'nelisp--eval-source-string))
            (nelisp--load-source-large-p source))
        #'nelisp--load-eval-source-incremental
      #'nelisp--load-eval-source-hybrid))

  (defun nelisp--load-artifact-source-transform (source _path)
    "Return SOURCE normalized for artifact-first loading."
    (nelisp--load-normalize-source-rewriting source))

  (defun emacs-load--artifact-cache-directory ()
    "Return the standalone artifact cache root."
    (cond
     ((and (boundp 'user-emacs-directory)
           (stringp user-emacs-directory)
           (> (length user-emacs-directory) 0))
      (expand-file-name "nelisp-cache/" user-emacs-directory))
     ((and (boundp 'temporary-file-directory)
           (stringp temporary-file-directory)
           (> (length temporary-file-directory) 0))
      (expand-file-name "nelisp-cache/" temporary-file-directory))
     (t "/tmp/nelisp-cache/")))

  (defvar emacs-load-auto-native-compile nil
    "When non-nil, allow standalone loads to compile and replay artifacts.
Bootstrap completes before `bin/nemacs' enables this.")

  (defvar emacs-load-artifact-replay-streaming-threshold 65536
    "Byte threshold above which artifact replay uses the streaming reader.
Nil or a non-positive value disables the size-based streaming path and
forces the historical whole-payload reader for all artifacts.")

  (defvar emacs-load-artifact-replay-preflight-gc t
    "When non-nil, run one preflight `garbage-collect' before top-level
artifact replay in `emacs-load--artifact-replay-file'.")

  (defvar emacs-load-artifact-replay-pressure-threshold 536870912
    "When positive, reclaim memory on nested artifact replay once growth
meets this used-bytes delta.  Nil or non-positive disables pressure-based
nested `garbage-collect'.")

  (defvar emacs-load-artifact-replay-garbage-collect-interval-cap 20
    "Artifact-only cap for positive `load-garbage-collect-interval' values.
When this variable and `load-garbage-collect-interval' are both positive
integers, top-level artifact replay uses the smaller value so large replay
tails collect sooner without changing ordinary source loading.  Nil or a
non-positive value disables the artifact-only cap.  Nil or disabled
`load-garbage-collect-interval' semantics stay unchanged, and nested
artifact replay inherits the outer replay binding.")

  (defvar emacs-load-artifact-native-sections-native-reader-threshold 65536
    "Byte threshold for whole-string native reads during native metadata replay.
When `nelisp--read-all-from-string-native' is available, native section
spans and heavy native metadata list fields at or below this size use the
whole-string native reader.  Larger ranges use the existing incremental
range-based parsers so 300KB-1MB `:relocs'/`:defuns' lists do not
materialize as a single retained object with no safe GC boundary.  Nil or
a non-positive value disables the size-based routing and preserves the
historical whole-string native read path whenever the native reader is
available.")

  (defvar emacs-load-artifact-module-init-native-batch-items 32
    "Maximum number of `:module-init' entries to read per native batch.")

  (defvar emacs-load--artifact-native-batch-diagnostic nil
    "Compact diagnostic for the last failed native artifact batch read.")

  (defvar emacs-load-artifact-native-batch-capture-failed-slice nil
    "When non-nil, retain failed native batch source in its diagnostic.")

  (defconst emacs-load--artifact-cache-source-digest-salt
    "source-defun-fallback-v6-standalone-replay-gc-safe-native-shard-budget-4-defuns-per-section-64KiB-serialized-native-section-replay-byte-budget"
    "Schema salt mixed into cache sidecar source digests.
Salt v6 explicitly invalidates artifacts built before the 64KiB serialized
native-section replay byte budget was part of the cache contract, so old
budget=4 but oversized section artifacts do not replay as cache hits.")

  (defvar emacs-load--last-artifact-payload nil
    "Last artifact payload replayed by `emacs-load--artifact-replay-file'.
Large-artifact streaming stores a compact summary plist here instead of the
full payload, because production callers only depend on the truthy replay
result and the installed functions.")

  (defvar emacs-load--artifact-native-hash-cache (make-hash-table :test 'equal)
    "Cache of native artifact blob hashes to mapped base addresses.")

  (defvar emacs-load--artifact-native-diagnostic-report nil
    "Last native artifact replay diagnostic report.")

  (defvar emacs-load--artifact-compile-diagnostic-report nil
    "Last standalone artifact compile diagnostic report.
When non-nil, this is a plist describing the most recent compile failure
that caused the loader to fall back to source evaluation.")

  (defvar emacs-load--artifact-replay-depth 0
    "Dynamic recursion depth for artifact replay preflight GC guard.")

  (defvar emacs-load--artifact-replay-pressure-baseline nil
    "Dynamic baseline of arena used bytes for artifact replay pressure GC.")

  (defvar emacs-load--artifact-replay-pressure-enabled nil
    "Whether pressure-based nested replay GC is enabled for this outermost call.")

  (defun emacs-load--artifact-replay-garbage-collect-interval ()
    "Return the effective `load-garbage-collect-interval' for artifact replay."
    (let ((current load-garbage-collect-interval)
          (cap emacs-load-artifact-replay-garbage-collect-interval-cap))
      (if (and (integerp current)
               (> current 0)
               (integerp cap)
               (> cap 0))
          (if (< current cap) current cap)
        current)))

  (defun emacs-load--artifact-replay-pressure-bytes ()
    "Return a nonnegative replay pressure metric, or nil when unavailable.
Prefer `nelisp--allocation-debt' so pressure tracks monotonic allocation
requests including free-list reuse.  Fall back to used bytes from
`nelisp--arena-stats' on older runtimes."
    (condition-case nil
        (cond
         ((fboundp 'nelisp--allocation-debt)
          (let ((debt (nelisp--allocation-debt)))
            (and (integerp debt)
                 (>= debt 0)
                 debt)))
         ((fboundp 'nelisp--arena-stats)
          (let ((stats (nelisp--arena-stats))
                (used nil)
                (count 0)
                (tail nil))
            (when (consp stats)
              (setq tail stats)
              (while (consp tail)
                (when (= count 11)
                  (setq used (car tail)))
                (setq tail (cdr tail))
                (setq count (1+ count)))
              (when (and (null tail)
                         (>= count 12)
                         (integerp used)
                         (>= used 0))
                used)))))
      (error nil)))

  (defun emacs-load--artifact-replay-arena-used-bytes ()
    "Compatibility wrapper for `emacs-load--artifact-replay-pressure-bytes'."
    (emacs-load--artifact-replay-pressure-bytes))

  (defun emacs-load--artifact-replay-pressure-maybe-gc ()
    "Run pressure-driven GC when artifact replay usage growth exceeds threshold."
    (let ((threshold emacs-load-artifact-replay-pressure-threshold))
      (when (and emacs-load--artifact-replay-pressure-enabled
                 (fboundp 'garbage-collect)
                 (integerp threshold)
                 (> threshold 0)
                 (integerp emacs-load--artifact-replay-pressure-baseline)
                 (>= emacs-load--artifact-replay-pressure-baseline 0))
        (let ((used (emacs-load--artifact-replay-pressure-bytes)))
          (if (not (integerp used))
              (setq emacs-load--artifact-replay-pressure-enabled nil)
            (when (>= (- used emacs-load--artifact-replay-pressure-baseline)
                      threshold)
              (garbage-collect)
              (setq emacs-load--artifact-replay-pressure-baseline
                    (emacs-load--artifact-replay-pressure-bytes))
              (unless (integerp emacs-load--artifact-replay-pressure-baseline)
                (setq emacs-load--artifact-replay-pressure-enabled nil))))))))

  (defun emacs-load--artifact-native-reader-within-threshold-p
      (source start end)
    "Return non-nil when the native whole-string reader is allowed for SOURCE.
START and END delimit the candidate range.  The range must fit within
`emacs-load-artifact-native-sections-native-reader-threshold' bytes unless
that threshold is disabled."
    (let ((threshold emacs-load-artifact-native-sections-native-reader-threshold))
      (and (fboundp 'nelisp--read-all-from-string-native)
           (or (not (integerp threshold))
               (<= threshold 0)
               (let ((char-span (- end start)))
                 (and (<= char-span threshold)
                      (<= (emacs-load--artifact-byte-length
                           (substring source start end))
                          threshold)))))))

  (defun emacs-load--artifact-compiler ()
    "Return the first usable standalone artifact compiler executable."
    (let ((candidates
           (list (and (boundp 'nemacs-nelisp-executable)
                      nemacs-nelisp-executable)
                 (and (fboundp 'getenv) (getenv "NEMACS_NELISP"))
                 (let ((home (and (fboundp 'getenv) (getenv "NELISP_HOME"))))
                   (and home (expand-file-name "target/nelisp" home)))
                 (and (fboundp 'executable-find)
                      (executable-find "nelisp"))))
          (found nil))
      (while (and candidates (not found))
        (let ((candidate (pop candidates)))
          (when (and (stringp candidate)
                     (> (length candidate) 0)
                     (file-executable-p candidate))
            (setq found candidate))))
      found))

  (defun emacs-load--artifact-compiler-identity (compiler)
    "Return a cheap stable identity for COMPILER, or nil if unavailable."
    (let* ((path (expand-file-name compiler))
           (attributes
            (and (not (fboundp 'nelisp--syscall-stat-field))
                 (fboundp 'file-attributes)
                 (file-attributes path 'integer)))
           (size
            (if (fboundp 'nelisp--syscall-stat-field)
                (nelisp--syscall-stat-field path 48)
              (and attributes (nth 7 attributes))))
           (mtime-seconds
            (if (fboundp 'nelisp--syscall-stat-field)
                (nelisp--syscall-stat-field path 88)
              (and attributes (nth 5 attributes))))
           (mtime-nanoseconds
            (and (fboundp 'nelisp--syscall-stat-field)
                 (nelisp--syscall-stat-field path 96)))
           (mtime (if mtime-nanoseconds
                      (list mtime-seconds mtime-nanoseconds)
                    mtime-seconds)))
      (and (integerp size)
           (> size 0)
           (or (and (integerp mtime-seconds)
                    (>= mtime-seconds 0)
                    (integerp mtime-nanoseconds)
                    (>= mtime-nanoseconds 0))
               (and attributes mtime))
           (list :path path :size size :mtime mtime))))

  (defun emacs-load--read-file-string (path)
    "Return PATH contents as a string, or nil when unreadable."
    (cond
     ((fboundp 'nelisp--syscall-read-file)
      (nelisp--syscall-read-file path))
     ((file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))
     (t nil)))

  (defun emacs-load--artifact-load-path-present-p (path seen)
    "Return non-nil when PATH already appears in SEEN."
    (let ((found nil))
      (while (and seen (not found))
        (when (string= path (car seen))
          (setq found t))
        (setq seen (cdr seen)))
      found))

  (defun emacs-load--artifact-load-path-cli-args ()
    "Return stable repeated `--load-path' args from the active `load-path'."
    (let ((paths load-path)
          (seen nil)
          (args nil))
      (while paths
        (let ((entry (car paths)))
          (when (and (stringp entry)
                     (> (length entry) 0))
            (let ((expanded (expand-file-name entry)))
              (when (and (file-directory-p expanded)
                         (not (emacs-load--artifact-load-path-present-p
                               expanded seen)))
                (setq seen (cons expanded seen))
                (setq args (cons expanded
                                 (cons "--load-path" args)))))))
        (setq paths (cdr paths)))
      (nreverse args)))

  (defun emacs-load--artifact-string-search (needle haystack start)
    "Search HAYSTACK for NEEDLE from START.
Prefer the standalone native `nelisp--string-search' when available and
fall back to host `string-search' otherwise."
    (if (fboundp 'nelisp--string-search)
        (nelisp--string-search needle haystack start)
      (string-search needle haystack start)))

  (defun emacs-load--artifact-byte-at (string index)
    "Return byte at INDEX from STRING."
    (cond
     ((fboundp 'nelisp--string-byte-at)
      (nelisp--string-byte-at string index))
     ((fboundp 'string-byte)
      (string-byte string index))
     (t
      (aref string index))))

  (defun emacs-load--artifact-source-char-at (string index)
    "Return character at INDEX from SOURCE string STRING."
    (aref string index))

  (defun emacs-load--artifact-source-skip-ws-comments (source pos)
    "Return first non-whitespace/comment position in SOURCE at or after POS."
    (let ((len (length source))
          (done nil))
      (while (and (< pos len) (not done))
        (let ((ch (emacs-load--artifact-source-char-at source pos)))
          (cond
           ((or (= ch ?\s) (= ch ?\t) (= ch ?\n) (= ch ?\r) (= ch ?\f))
            (setq pos (1+ pos)))
           ((= ch ?\;)
            (while (and (< pos len)
                        (not (= (emacs-load--artifact-source-char-at source pos)
                                ?\n)))
              (setq pos (1+ pos))))
           (t
            (setq done t)))))
      pos))

  (defun emacs-load--artifact-source-rskip-ws (source pos)
    "Return last non-whitespace position in SOURCE at or before POS."
    (let ((done nil))
      (while (and (>= pos 0) (not done))
        (let ((ch (emacs-load--artifact-source-char-at source pos)))
          (if (or (= ch ?\s) (= ch ?\t) (= ch ?\n) (= ch ?\r) (= ch ?\f))
              (setq pos (1- pos))
            (setq done t))))
      pos))

  (defun emacs-load--artifact-source-string-end (source pos)
    "Return one past the string starting at POS in SOURCE."
    (let ((len (length source)))
      (if (fboundp 'nelisp--rd-string-end-native)
          (let* ((read (nelisp--rd-string-end-native source (1+ pos) len))
                 (quote-pos (car-safe read)))
            (if (and (integerp quote-pos)
                     (>= quote-pos (1+ pos))
                     (< quote-pos len))
                (1+ quote-pos)
              (error "unterminated source string")))
        (let ((i (1+ pos))
              (escaped nil)
              (done nil))
          (while (and (< i len) (not done))
            (let ((ch (emacs-load--artifact-source-char-at source i)))
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
          i))))

  (defun emacs-load--artifact-source-container-end (source pos)
    "Return one past the list/vector container starting at POS in SOURCE."
    (if (fboundp 'nelisp--source-container-end)
        (let ((end (nelisp--source-container-end source pos))
              (len (length source)))
          (if (and (integerp end)
                   (> end pos)
                   (<= end len))
              end
            (error "unterminated source container")))
      (let ((len (length source))
            (i pos)
            (depth 0)
            (in-string nil)
            (atom-escaped nil)
            (escaped nil)
            (done nil))
        (while (and (< i len) (not done))
          (let ((ch (emacs-load--artifact-source-char-at source i)))
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
              (while (and (< i len)
                          (not (= (emacs-load--artifact-source-char-at source i)
                                  ?\n)))
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
        i)))

  (defun emacs-load--artifact-source-atom-end (source pos)
    "Return one past the atom starting at POS in SOURCE."
    (let ((len (length source))
          (i pos)
          (done nil))
      (while (and (< i len) (not done))
        (let ((ch (emacs-load--artifact-source-char-at source i)))
          (if (or (= ch ?\s) (= ch ?\t) (= ch ?\n) (= ch ?\r)
                  (= ch ?\f) (= ch ?\;) (= ch ?\() (= ch ?\))
                  (= ch ?\[) (= ch ?\]))
              (setq done t)
            (setq i (1+ i)))))
      i))

  (defun emacs-load--artifact-source-find-marker
      (source marker start path keyword &optional required)
    "Return MARKER position in SOURCE at or after START.
Signal a payload error when KEYWORD is missing from PATH unless REQUIRED
is nil.  MARKER is the exact top-level field token without assuming a
leading space, because the first plist key after the opening `(' has no
prefix space."
      (let ((pos (emacs-load--artifact-string-search marker source start)))
      (cond
       ((null pos)
        (when required
          (error "missing %S in %s" keyword path))
        nil)
       ((and (> pos 0)
             (let ((prev (emacs-load--artifact-source-char-at source (1- pos))))
               (not (or (= prev ?\()
                        (= prev ?\s)
                        (= prev ?\t)
                        (= prev ?\n)
                        (= prev ?\r)
                        (= prev ?\f)))))
        (error "invalid %S in %s" keyword path))
       (t pos))))

  (defun emacs-load--artifact-source-form-end (source pos)
    "Return one past the top-level form starting at POS in SOURCE."
    (let* ((len (length source))
           (pos (emacs-load--artifact-source-skip-ws-comments source pos)))
      (when (>= pos len)
        (error "no source form at end of input"))
      (let ((ch (emacs-load--artifact-source-char-at source pos)))
        (cond
         ((or (= ch ?\') (= ch ?`))
          (emacs-load--artifact-source-form-end source (1+ pos)))
         ((= ch ?,)
          (emacs-load--artifact-source-form-end
           source
           (if (and (< (1+ pos) len)
                    (= (emacs-load--artifact-source-char-at source (1+ pos))
                       ?@))
               (+ pos 2)
             (1+ pos))))
         ((and (= ch ?#)
               (< (1+ pos) len)
               (= (emacs-load--artifact-source-char-at source (1+ pos))
                  ?\'))
          (emacs-load--artifact-source-form-end source (+ pos 2)))
         ((and (= ch ?#)
               (< (1+ pos) len)
               (= (emacs-load--artifact-source-char-at source (1+ pos))
                  ?\())
          (emacs-load--artifact-source-container-end source (1+ pos)))
         ((or (= ch ?\() (= ch ?\[))
          (emacs-load--artifact-source-container-end source pos))
         ((= ch ?\")
          (emacs-load--artifact-source-string-end source pos))
         (t
          (emacs-load--artifact-source-atom-end source pos))))))

  (defun emacs-load--artifact-source-read-form (source pos &optional end)
    "Read one form from SOURCE at POS and return it with the new position."
    (let ((read (if end
                    (read-from-string source pos end)
                  (read-from-string source pos))))
      (unless (consp read)
        (error "invalid artifact form at %S" pos))
      (when (<= (cdr read) pos)
        (error "invalid artifact form at %S" pos))
      (cons (car read) (cdr read))))

  (defun emacs-load--artifact-source-read-form-value (source range path keyword)
    "Read VALUE from SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let ((read (emacs-load--artifact-source-read-form
                 source (car range) (cdr range))))
      (unless (and (consp read) (>= (cdr read) (cdr range)))
        (error "invalid %S in %s" keyword path))
      (car read)))

  (defun emacs-load--artifact-signal-invalid-read (keyword path caught)
    "Signal an invalid read for KEYWORD in PATH, preserving CAUGHT."
    (error "invalid %S in %s (%s)"
           keyword path (error-message-string caught)))

  (defun emacs-load--artifact-source-read-single-form-value
      (source range path keyword)
    "Read exactly one form from SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (slice (substring source start end)))
      (if (fboundp 'nelisp--read-all-from-string-native)
          (let ((forms (condition-case caught
                           (nelisp--read-all-from-string-native slice)
                         (error
                          (emacs-load--artifact-signal-invalid-read
                           keyword path caught)))))
            (unless (and (consp forms) (null (cdr forms)))
              (error "invalid %S in %s" keyword path))
            (car forms))
        (let ((read (read-from-string slice 0 (length slice))))
          (unless (and (consp read)
                       (= (cdr read) (length slice)))
            (error "invalid %S in %s" keyword path))
          (car read)))))

  (defun emacs-load--artifact-source-read-incremental-form-value
      (source range path keyword)
    "Read exactly one incremental form slice from SOURCE at RANGE.
Use ordinary `read-from-string' on the slice and reject trailing input."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (slice (substring source start end))
           (read (read-from-string slice 0 (length slice))))
      (unless (and (consp read)
                   (= (cdr read) (length slice)))
        (error "invalid %S in %s" keyword path))
      (car read)))

  (defun emacs-load--artifact-source-read-form-nil-or-list-value
      (source range path keyword)
    "Read a nil or proper list VALUE from SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (pos (emacs-load--artifact-source-skip-ws-comments source start)))
      (if (emacs-load--artifact-native-reader-within-threshold-p
           source start end)
          (condition-case caught
              (let* ((forms (nelisp--read-all-from-string-native
                             (substring source start end))))
                (unless (and (consp forms) (null (cdr forms)))
                  (error "invalid %S in %s" keyword path))
                (let ((value (car forms)))
                  (unless (proper-list-p value)
                    (error "invalid %S in %s" keyword path))
                  value))
            (error
             (emacs-load--artifact-signal-invalid-read
              keyword path caught)))
        (cond
         ((>= pos end)
          (error "invalid %S in %s" keyword path))
         ((and (< (+ pos 2) end)
               (= (emacs-load--artifact-source-char-at source pos) ?n)
               (= (emacs-load--artifact-source-char-at source (1+ pos)) ?i)
               (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
          (unless (= (+ pos 3) end)
            (error "invalid %S in %s" keyword path))
          nil)
         ((/= (emacs-load--artifact-source-char-at source pos) ?\()
          (error "invalid %S in %s" keyword path))
         (t
          (let* ((list-open pos)
                 (list-end (emacs-load--artifact-source-form-end source pos))
                 (close (1- list-end)))
            (when (> list-end end)
              (error "invalid %S in %s" keyword path))
            (setq pos (1+ pos))
            (while (progn
                     (setq pos (emacs-load--artifact-source-skip-ws-comments
                                source pos))
                     (< pos close))
              (let ((item-end (emacs-load--artifact-source-form-end source pos)))
                (when (>= item-end list-end)
                  (error "invalid %S in %s" keyword path))
                (when (and (= (emacs-load--artifact-source-char-at source pos) ?.)
                           (= item-end (1+ pos)))
                  (error "invalid %S in %s" keyword path))
                (setq pos item-end)))
            (setq pos (emacs-load--artifact-source-skip-ws-comments source pos))
            (unless (and (< pos end)
                         (= (emacs-load--artifact-source-char-at source pos)
                            ?\)))
              (error "invalid %S in %s" keyword path))
            (setq pos (1+ pos))
            (unless (= pos end)
              (error "invalid %S in %s" keyword path))
            (let ((list-source (substring source (1+ list-open) close)))
              (if (or (fboundp 'nelisp--read-batch-vector-from-string-native)
                      (fboundp 'nelisp--read-batch-from-string-native))
                  (let ((items nil))
                    (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                     list-source path keyword
                     (lambda (item)
                       (push item items)))
                    (nreverse items))
                (let ((items nil)
                      (count 0)
                      (item-pos (1+ list-open)))
                  (while (progn
                           (setq item-pos (emacs-load--artifact-source-skip-ws-comments
                                           source item-pos))
                           (< item-pos close))
                    (let ((item-end (emacs-load--artifact-source-form-end
                                     source item-pos)))
                      (push (emacs-load--artifact-source-read-incremental-form-value
                             source (cons item-pos item-end) path keyword)
                            items)
                      (setq item-pos item-end)
                      (setq count (1+ count))
                      (emacs-load--artifact-replay-pressure-maybe-gc)
                      (when (and load-garbage-collect-interval
                                 (> load-garbage-collect-interval 0)
                                 (= (% count load-garbage-collect-interval) 0)
                                 (fboundp 'garbage-collect))
                        (garbage-collect))))
                  (nreverse items))))))))))

  (defun emacs-load--artifact-source-iterate-form-nil-or-list-value
      (source range path keyword handler)
    "Iterate over a nil or proper list VALUE from SOURCE at RANGE.
Call HANDLER for each item in source order and return the number of items
processed."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (pos (emacs-load--artifact-source-skip-ws-comments source start))
           (count 0))
      (cond
       ((>= pos end)
        (error "invalid %S in %s" keyword path))
       ((and (= (- end pos) 3)
             (= (emacs-load--artifact-source-char-at source pos) ?n)
             (= (emacs-load--artifact-source-char-at source (1+ pos)) ?i)
             (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
        (unless (= (+ pos 3) end)
          (error "invalid %S in %s" keyword path))
        0)
       ((/= (emacs-load--artifact-source-char-at source pos) ?\()
        (error "invalid %S in %s" keyword path))
       (t
        (let* ((list-end (emacs-load--artifact-source-form-end source pos))
               (close (1- list-end)))
          (when (> list-end end)
            (error "invalid %S in %s" keyword path))
          (setq pos (1+ pos))
          (while (progn
                   (setq pos (emacs-load--artifact-source-skip-ws-comments
                              source pos))
                   (< pos close))
            (let* ((item-start pos)
                   (item-end (emacs-load--artifact-source-form-end
                              source item-start)))
              (when (>= item-end list-end)
                (error "invalid %S in %s" keyword path))
              (when (and (= (emacs-load--artifact-source-char-at source pos) ?.)
                         (= item-end (1+ pos)))
                (error "invalid %S in %s" keyword path))
              (funcall handler
                       (emacs-load--artifact-source-read-single-form-value
                        source (cons item-start item-end) path keyword))
              (setq pos item-end)
              (setq count (1+ count))
              (emacs-load--artifact-replay-pressure-maybe-gc)
              (when (and load-garbage-collect-interval
                         (> load-garbage-collect-interval 0)
                         (= (% count load-garbage-collect-interval) 0)
                         (fboundp 'garbage-collect))
                (garbage-collect))))
          (setq pos (emacs-load--artifact-source-skip-ws-comments source pos))
          (unless (and (< pos end)
                       (= (emacs-load--artifact-source-char-at source pos)
                          ?\)))
            (error "invalid %S in %s" keyword path))
          (setq pos (1+ pos))
          (setq pos (emacs-load--artifact-source-skip-ws-comments source pos))
          (when (> pos end)
            (error "invalid %S in %s" keyword path))
          (unless (= pos end)
            (error "invalid %S in %s" keyword path))
          count)))))

  (defun emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
      (source path keyword handler)
    "Iterate over a nil or proper list VALUE from SOURCE via native batch reader.
   SOURCE is the list interior (without surrounding parentheses).
   Call HANDLER for each item in source order and return the number of items
processed."
   (setq emacs-load--artifact-native-batch-diagnostic nil)
   (let* ((source-length (length source))
           (char-cursor 0)
           (count 0)
           (batch-size emacs-load-artifact-module-init-native-batch-items)
           (native-batch-size nil))
     (unless (integerp batch-size)
       (error "invalid %S in %s" keyword path))
     (unless (> batch-size 0)
       (error "invalid %S in %s" keyword path))
     (setq native-batch-size (min batch-size 32))
     (while (progn
              (setq char-cursor
                    (emacs-load--artifact-source-skip-ws-comments
                     source char-cursor))
              (< char-cursor source-length))
       (let ((slice-start char-cursor)
             (slice-end char-cursor)
             (slice-form-count 0))
         (condition-case caught
             (while (and (< slice-form-count native-batch-size)
                         (< char-cursor source-length))
               (setq slice-end
                     (emacs-load--artifact-source-form-end
                      source char-cursor))
               (unless (> slice-end char-cursor)
                 (error "invalid source form"))
               (setq slice-form-count (1+ slice-form-count))
               (setq char-cursor
                     (emacs-load--artifact-source-skip-ws-comments
                      source slice-end)))
           (error
            (setq emacs-load--artifact-native-batch-diagnostic
                  (list :stage :scan-error
                        :path path
                        :keyword keyword
                        :slice-start slice-start
                        :slice-end slice-end
                        :slice-chars (- slice-end slice-start)
                        :expected-items slice-form-count
                        :error (error-message-string caught)))
            (error "invalid %S in %s" keyword path)))
         (let* ((slice (substring source slice-start slice-end))
                (slice-byte-length
                 (emacs-load--artifact-byte-length slice))
                (vector-response-p
                 (fboundp 'nelisp--read-batch-vector-from-string-native))
                (response (condition-case caught
                              (if vector-response-p
                                  (nelisp--read-batch-vector-from-string-native
                                   slice 0 native-batch-size)
                                (nelisp--read-batch-from-string-native
                                 slice 0 native-batch-size))
                            (error
                             (setq emacs-load--artifact-native-batch-diagnostic
                                   (append
                                    (list :stage :native-error
                                          :path path
                                          :keyword keyword
                                          :slice-start slice-start
                                          :slice-end slice-end
                                          :slice-chars (length slice)
                                          :slice-bytes slice-byte-length
                                          :expected-items slice-form-count
                                          :error (error-message-string caught))
                                    (and emacs-load-artifact-native-batch-capture-failed-slice
                                         (list :slice-text slice))))
                             (error "invalid %S in %s" keyword path))))
                (items nil)
                (next nil)
                (actual-items nil))
          (unless (if vector-response-p
                      (vectorp response)
                    (consp response))
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage (if vector-response-p
                                    :response-not-vector
                                  :response-not-cons)
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (when vector-response-p
            (unless (= (length response) (+ native-batch-size 2))
              (setq emacs-load--artifact-native-batch-diagnostic
                    (append
                     (list :stage :response-vector-length
                           :path path
                           :keyword keyword
                           :slice-start slice-start
                           :slice-end slice-end
                           :slice-chars (length slice)
                           :slice-bytes slice-byte-length
                           :expected-items slice-form-count
                           :response-length (length response)
                           :expected-length (+ native-batch-size 2))
                     (and emacs-load-artifact-native-batch-capture-failed-slice
                          (list :slice-text slice))))
              (error "invalid %S in %s" keyword path)))
          (if vector-response-p
              (setq next (aref response 0)
                    actual-items (aref response 1))
            (setq items (car response)
                  next (cdr response)
                  actual-items (and (proper-list-p items) (length items))))
          (unless (or vector-response-p (proper-list-p items))
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :items-improper
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items :improper
                         :next (if (integerp next) next :non-integer))
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (when vector-response-p
            (unless (integerp actual-items)
              (setq emacs-load--artifact-native-batch-diagnostic
                    (append
                     (list :stage :count-not-int
                           :path path
                           :keyword keyword
                           :slice-start slice-start
                           :slice-end slice-end
                           :slice-chars (length slice)
                           :slice-bytes slice-byte-length
                           :expected-items slice-form-count
                           :actual-items :non-integer
                           :next (if (integerp next) next :non-integer))
                     (and emacs-load-artifact-native-batch-capture-failed-slice
                          (list :slice-text slice))))
              (error "invalid %S in %s" keyword path))
            (unless (and (>= actual-items 0)
                         (<= actual-items native-batch-size))
              (setq emacs-load--artifact-native-batch-diagnostic
                    (append
                     (list :stage :count-out-of-bounds
                           :path path
                           :keyword keyword
                           :slice-start slice-start
                           :slice-end slice-end
                           :slice-chars (length slice)
                           :slice-bytes slice-byte-length
                           :expected-items slice-form-count
                           :actual-items actual-items
                           :next (if (integerp next) next :non-integer))
                     (and emacs-load-artifact-native-batch-capture-failed-slice
                          (list :slice-text slice))))
              (error "invalid %S in %s" keyword path)))
          (unless (integerp next)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :next-not-int
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next :non-integer)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (unless (>= next 0)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :next-negative
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next next)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (unless (= next slice-byte-length)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :next-mismatch
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next next)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (unless (= actual-items slice-form-count)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :item-count-mismatch
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next next)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (if vector-response-p
              (let ((item-index 0))
                (while (< item-index actual-items)
                  (let ((item (aref response (+ item-index 2))))
                    (funcall handler item)
                    (aset response (+ item-index 2) nil)
                    (setq item-index (1+ item-index))
                    (setq count (+ count 1))
                    (when (and load-garbage-collect-interval
                               (> load-garbage-collect-interval 0)
                               (= (% count load-garbage-collect-interval) 0)
                               (fboundp 'garbage-collect))
                      (garbage-collect)))))
            (while items
              (let ((item (car items)))
                (funcall handler item)
                (setq items (cdr items))
                (setq count (+ count 1))
                (setf (car response) items)
                (when (and load-garbage-collect-interval
                           (> load-garbage-collect-interval 0)
                           (= (% count load-garbage-collect-interval) 0)
                           (fboundp 'garbage-collect))
                  (garbage-collect)))))
          (if vector-response-p
              (progn
                (aset response 0 nil)
                (aset response 1 nil))
            (setf (car response) nil))
          (emacs-load--artifact-replay-pressure-maybe-gc))))
      count))

  (defun emacs-load--artifact-source-read-form-ranges (source start end path)
    "Return an alist of canonical top-level plist ranges from SOURCE."
      (let ((pos (emacs-load--artifact-source-skip-ws-comments source start))
          (pairs nil))
      (unless (and (< pos end)
                   (= (emacs-load--artifact-source-char-at source pos) ?\())
        (error "invalid artifact payload in %s" path))
      (setq pos (1+ pos))
      (while (progn
               (setq pos (emacs-load--artifact-source-skip-ws-comments
                          source pos))
               (< pos end))
        (when (= (emacs-load--artifact-source-char-at source pos) ?\))
          (setq pos end))
        (when (< pos end)
          (let* ((key-end (emacs-load--artifact-source-atom-end source pos))
                 (keyword-text (substring source pos key-end)))
            (unless (and (> (length keyword-text) 0)
                         (= (emacs-load--artifact-source-char-at keyword-text 0)
                            ?:))
              (error "invalid artifact payload key in %s" path))
            (setq pos (emacs-load--artifact-source-skip-ws-comments
                       source key-end))
            (when (>= pos end)
              (error "invalid artifact payload in %s" path))
            (let* ((value-start pos)
                   (value-end (emacs-load--artifact-source-form-end
                               source value-start))
                   (keyword (intern keyword-text)))
              (push (cons keyword (cons value-start value-end)) pairs)
              (setq pos value-end)))))
      (nreverse pairs)))

  (defun emacs-load--artifact-streaming-summary
      (path byte-length native-mode module-init-count field-names)
    "Return a compact truthy summary for large streaming artifact replays."
    (list :artifact path
          :streaming t
          :byte-length byte-length
          :native-mode native-mode
          :module-init-count module-init-count
          :fields field-names))

  (defun emacs-load--standalone-runtime-p ()
    "Return non-nil when native standalone SHA helpers should be preferred."
    (and (fboundp 'nelisp--sha256)
         (or (fboundp 'nelisp--write-stdout-bytes)
             (fboundp 'nelisp--eval-source-string))
         (or (and (fboundp 'emacs-standalone-mode-p)
                  (emacs-standalone-mode-p))
             (and (fboundp 'emacs-standalone-active-p)
                  (emacs-standalone-active-p))
             (not (boundp 'emacs-version))
             (not (stringp (and (boundp 'emacs-version) emacs-version))))))


  (defun emacs-load--sha256 (object)
    "Return the SHA-256 digest for OBJECT as a string."
    (cond
     ((emacs-load--standalone-runtime-p)
      (or (let ((digest (and (fboundp 'nelisp--sha256)
                             (nelisp--sha256 object))))
            (and digest digest))
          (and (fboundp 'secure-hash)
               (or (secure-hash 'sha256 object)
                   (error "secure-hash returned nil for SHA-256")))))
     ((fboundp 'secure-hash)
      (or (secure-hash 'sha256 object)
          (and (fboundp 'nelisp--sha256)
               (nelisp--sha256 object))
          (error "secure-hash returned nil for SHA-256")))
     ((fboundp 'nelisp--sha256)
      (nelisp--sha256 object))
     (t
      (error "no SHA-256 helper available"))))

  (defun emacs-load--artifact-cache-source-digest (source)
    "Return the salted source digest used by cache sidecars."
    (emacs-load--sha256
     (concat emacs-load--artifact-cache-source-digest-salt "\0" source)))

  (defun emacs-load--artifact-cache-paths (resolved)
    "Return cache artifact and sidecar paths for RESOLVED."
    (let* ((cache-directory (emacs-load--artifact-cache-directory))
           (hash (emacs-load--sha256 (expand-file-name resolved)))
           (prefix (substring hash 0 2))
           (suffix (substring hash 2))
           (base (file-name-nondirectory resolved))
           (artifact-dir (expand-file-name prefix cache-directory))
           (artifact
            (expand-file-name
             (concat suffix "-" base ".neln")
             artifact-dir))
           (sidecar (concat artifact ".source-sha256")))
      (make-directory artifact-dir t)
      (list artifact sidecar)))

  (defun emacs-load--artifact-cache-read-plist (path)
    "Read one plist from PATH, returning nil for an invalid cache record."
    (condition-case nil
        (let ((text (emacs-load--read-file-string path)))
          (and (stringp text)
               (let ((read (read-from-string text)))
                 (and (consp read)
                      (listp (car read))
                      (car read)))))
      (error nil)))

  (defun emacs-load--artifact-cache-record
      (source-hash compiler-identity)
    "Return a cache record for SOURCE-HASH and COMPILER-IDENTITY."
    (list :cache-record-version 1
          :source-sha256 source-hash
          :compiler compiler-identity))

  (defun emacs-load--artifact-cache-hit-p
      (artifact sidecar source-hash compiler-identity)
    "Return non-nil when the cached ARTIFACT triple is safe to replay."
    (and compiler-identity
         (file-readable-p artifact)
         (file-readable-p sidecar)
         (file-readable-p (concat artifact ".manifest.el"))
         (equal (emacs-load--artifact-cache-read-plist sidecar)
                (emacs-load--artifact-cache-record
                 source-hash compiler-identity))))

  (defun emacs-load--artifact-cache-invalidate (artifact sidecar)
    "Delete the cache triple belonging to ARTIFACT and SIDECAR."
    (dolist (path (list artifact (concat artifact ".manifest.el") sidecar))
      (when (file-exists-p path)
        (delete-file path))))

  (defun emacs-load--artifact-compile-and-replay
      (resolved source source-hash compiler compiler-identity artifact sidecar
                &optional cached-error)
    "Compile and replay RESOLVED once, optionally recovering from CACHED-ERROR."
    (let ((temp (make-temp-file "emacs-load-artifact-" nil ".el")))
      (unwind-protect
          (progn
            (emacs-load--artifact-write-raw-source source temp)
            ;; No `--rewrite-defalias-late': the flag is not in any released
            ;; NeLisp CLI, and passing it makes compilation exit unsuccessfully.
            (let* ((command (list "compile-elisp-artifact"
                                  "--kind" "neln"
                                  "--input" temp
                                  "--output" artifact
                                  "--native-policy" "opportunistic"))
                   (command (append command
                                    (emacs-load--artifact-load-path-cli-args)))
                   (status (apply #'call-process compiler nil nil nil command)))
              (cond
               ((not (and (integerp status) (= status 0)))
                (setq emacs-load--artifact-compile-diagnostic-report
                      (list :resolved resolved
                            :compiler compiler
                            :compiler-identity compiler-identity
                            :status status
                            :artifact artifact
                            :sidecar sidecar
                            :source-hash source-hash
                            :temp temp
                            :command command))
                (emacs-load--artifact-cache-invalidate artifact sidecar)
                (if cached-error
                    (error "cached artifact failed (%s); recompilation failed with status %S"
                           (error-message-string cached-error) status)
                  nil))
               (t
                (condition-case caught
                    (progn
                      (emacs-load--artifact-replay-file artifact)
                      (write-region
                       (prin1-to-string
                        (emacs-load--artifact-cache-record
                         source-hash compiler-identity))
                       nil sidecar nil 'silent)
                      (setq emacs-load--artifact-compile-diagnostic-report nil)
                      t)
                  (error
                   (setq emacs-load--artifact-compile-diagnostic-report
                         (list :resolved resolved
                               :compiler compiler
                               :compiler-identity compiler-identity
                               :status 'artifact-replay-failed
                               :artifact artifact
                               :sidecar sidecar
                               :source-hash source-hash
                               :temp temp
                               :command command
                               :error (error-message-string caught)))
                   (emacs-load--artifact-cache-invalidate artifact sidecar)
                   (signal (car caught) (cdr caught))))))))
        (when (file-exists-p temp)
          (delete-file temp)))))

  (defun emacs-load--artifact-load-or-compile (resolved source)
    "Replay or compile a standalone artifact for RESOLVED and SOURCE."
    (when emacs-load-auto-native-compile
      (let ((compiler (emacs-load--artifact-compiler)))
        (when compiler
          (let* ((source-hash (emacs-load--artifact-cache-source-digest source))
                 (paths (emacs-load--artifact-cache-paths resolved))
                 (artifact (car paths))
                 (sidecar (cadr paths))
                 (compiler-identity
                  (emacs-load--artifact-compiler-identity compiler)))
            (if (emacs-load--artifact-cache-hit-p
                 artifact sidecar source-hash compiler-identity)
                (condition-case cached-error
                    (progn
                      (setq emacs-load--artifact-compile-diagnostic-report nil)
                      (emacs-load--artifact-replay-file artifact)
                      t)
                  (error
                   (emacs-load--artifact-cache-invalidate artifact sidecar)
                   (emacs-load--artifact-compile-and-replay
                    resolved source source-hash compiler compiler-identity
                    artifact sidecar cached-error)))
              (emacs-load--artifact-cache-invalidate artifact sidecar)
              (emacs-load--artifact-compile-and-replay
               resolved source source-hash compiler compiler-identity
               artifact sidecar)))))))

  (defconst emacs-load--artifact-native-section-version 5
    "Current compact native section wire version supported by the loader.")

  (defconst emacs-load--artifact-native-runtime-prefix-layout-version 2
    "Compact `:runtime-prefix' layout version supported by the loader.")

  (defconst emacs-load--artifact-native-compact-reloc-format 'indexed-plt32-v1
    "Compact relocation format used by the v5 native artifact wire.")

  (defun emacs-load--artifact-native-runtime-prefix-p (value)
    "Return non-nil when VALUE is a supported compact runtime prefix."
    (or (and (vectorp value)
             (= (length value) 10)
             (= (aref value 0)
                emacs-load--artifact-native-runtime-prefix-layout-version))
        (and (listp value)
             (plist-member value :runtime-prefix-char-size)
             (plist-member value :arch)
             (plist-member value :text-base64)
             (plist-member value :extern-symbols)
             (plist-member value :defuns))))

  (defun emacs-load--artifact-native-v5-section-p (value)
    "Return non-nil when VALUE is a raw v5 compact native section."
    (and (listp value)
         (= (or (plist-get value :native-section-version) 0)
            emacs-load--artifact-native-section-version)
         (emacs-load--artifact-native-runtime-prefix-p
          (plist-get value :runtime-prefix))))

  (defun emacs-load--artifact-native-section-get (section key)
    "Return KEY from SECTION across legacy and v5 compact wire shapes."
    (if (emacs-load--artifact-native-v5-section-p section)
        (let ((runtime (plist-get section :runtime-prefix)))
          (if (vectorp runtime)
              (pcase key
                (:native-section-version
                 emacs-load--artifact-native-section-version)
                (:runtime-prefix runtime)
                (:runtime-prefix-char-size (aref runtime 1))
                (:arch (aref runtime 2))
                (:symbols (aref runtime 3))
                (:text-base64 (aref runtime 4))
                (:reloc-format (aref runtime 5))
                (:reloc-count (aref runtime 6))
                (:reloc-data (aref runtime 7))
                (:extern-symbols (aref runtime 8))
                (:defuns (aref runtime 9))
                (_ (plist-get section key)))
            (or (plist-get runtime key)
                (plist-get section key))))
      (plist-get section key)))

  (defun emacs-load--artifact-native-compact-relocs-p (section)
    "Return non-nil when SECTION carries supported compact reloc metadata."
    (let ((format (emacs-load--artifact-native-section-get section :reloc-format))
          (count (emacs-load--artifact-native-section-get section :reloc-count))
          (data (emacs-load--artifact-native-section-get section :reloc-data)))
      (and (eq format emacs-load--artifact-native-compact-reloc-format)
           (integerp count)
           (>= count 0)
           (or (null data) (vectorp data) (listp data))
           (= (length data) (* count 3)))))

  (defun emacs-load--artifact-native-section-relocs (section path)
    "Return SECTION relocations in canonical legacy plist form."
    (if (and (listp section)
             (plist-member section :relocs))
        (plist-get section :relocs)
      (let ((externs (emacs-load--artifact-native-section-get
                      section :extern-symbols))
            (data (emacs-load--artifact-native-section-get
                   section :reloc-data))
            (count (emacs-load--artifact-native-section-get
                    section :reloc-count))
            (out nil))
        (unless (and (emacs-load--artifact-native-compact-relocs-p section)
                     (listp externs))
          (error "invalid native section in %s" path))
        (dotimes (i count)
          (let* ((base (* i 3))
                 (symbol-index (elt data (1+ base)))
                 (symbol (and (integerp symbol-index)
                              (>= symbol-index 0)
                              (< symbol-index (length externs))
                              (nth symbol-index externs))))
            (unless (stringp symbol)
              (error "invalid native section in %s" path))
            (push (list :offset (elt data base)
                        :type 'plt32
                        :symbol symbol
                        :addend (elt data (+ base 2)))
                  out)))
        (nreverse out))))

  (defun emacs-load--artifact-native-plist-p (value)
    "Return non-nil when VALUE looks like native function metadata."
    (or (and (listp value)
             (plist-member value :arch)
             (plist-member value :text-base64)
             (plist-member value :relocs)
             (plist-member value :extern-symbols)
             (plist-member value :defuns))
        (emacs-load--artifact-native-v5-section-p value)))

  (defconst emacs-load--artifact-native-extern-whitelist
    '("nl_alloc_symbol"
      "nl_alloc_str"
      "nl_alloc_mut_str"
      "nl_mut_str_push_byte"
      "nl_mut_str_finalize"
      "nelisp_aot_builtin_call1"
      "nelisp_aot_builtin_calln")
    "Runtime extern names that may be replayed in-process.")

  (defun emacs-load--artifact-native-content-key (native)
    "Return a cache key string for NATIVE contents."
    (emacs-load--sha256
     (prin1-to-string
      (list :text-base64
            (emacs-load--artifact-native-section-get native :text-base64)
            :relocs
            (emacs-load--artifact-native-section-relocs native "<cache-key>")
            :extern-symbols
            (emacs-load--artifact-native-section-get native :extern-symbols)))))

  (defun emacs-load--artifact-native-extern-allowed-p (symbol)
    "Return non-nil when SYMBOL is whitelisted for runtime resolution."
    (and (stringp symbol)
         (member symbol emacs-load--artifact-native-extern-whitelist)))

  (defun emacs-load--artifact-native-prefix-eligible-p (native)
    "Return non-nil when NATIVE satisfies the cheap eligibility prefix."
    (and (fboundp 'nelisp--runtime-symbol-address)
         (fboundp 'nelisp--native-call-boundary)
         (equal (emacs-load--artifact-native-section-get native :arch) "x86_64")
         (let ((extern-symbols
                (emacs-load--artifact-native-section-get
                 native :extern-symbols)))
           (and (proper-list-p extern-symbols)
                (catch 'unsupported
                  (dolist (symbol extern-symbols t)
                    (unless (emacs-load--artifact-native-extern-allowed-p symbol)
                      (throw 'unsupported nil))))))))

  (defun emacs-load--artifact-native-form-kind-from-range
      (source range path keyword)
    "Return the syntactic kind of SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "invalid native section in %s" path))
    (let* ((start (car range))
           (end (cdr range))
           (pos (emacs-load--artifact-source-skip-ws-comments source start)))
      (when (>= pos end)
        (error "invalid %S in %s" keyword path))
      (let ((ch (emacs-load--artifact-source-char-at source pos)))
        (cond
         ((= ch ?\") :string)
         ((= ch ?\() :list)
         ((and (= ch ?n)
               (= (+ pos 3) end)
               (= (emacs-load--artifact-source-char-at source (1+ pos)) ?i)
               (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
          :nil)
         (t
          (error "invalid %S in %s" keyword path))))))

  (defun emacs-load--artifact-native-section-preflight-from-ranges
      (source ranges path)
    "Return non-nil when RANGES describe a cheap-eligible native section."
    (emacs-load--artifact-native-prefix-eligible-p
     (emacs-load--artifact-native-section-from-ranges
      source ranges path)))

  (defun emacs-load--artifact-native-decode-text (text &optional path quiet)
    "Return decoded bytes for base64 TEXT.
When QUIET is non-nil, return nil instead of signaling on decoder failures."
    (let ((decoded
           (cond
            ((fboundp 'nelisp--base64-decode-bytes)
             (nelisp--base64-decode-bytes text))
            ((fboundp 'base64-decode-string)
             (base64-decode-string text))
            (quiet nil)
            (t
             (error "no base64 decoder available for %S" (or path text))))))
      (cond
       ((stringp decoded)
        (let ((decoded-bytes (emacs-load--artifact-byte-length decoded)))
          (if (and (> (emacs-load--artifact-byte-length text) 0)
                   (<= decoded-bytes 0))
              (if quiet
                  nil
                (error "decoded native payload is empty for %S" (or path text)))
            decoded)))
       (quiet nil)
       (t
        (error "invalid native payload decode for %S" (or path text))))))

  (defun emacs-load--artifact-native-rel32-value (stub-address addend patch-address)
    "Return the unsigned 32-bit rel32 value for STUB-ADDRESS, ADDEND, and PATCH-ADDRESS."
    (let ((delta (+ stub-address addend (- patch-address))))
      (unless (and (integerp delta)
                   (<= -2147483648 delta)
                   (<= delta 2147483647))
        (error "native rel32 relocation out of range: %S" delta))
      (logand delta #xffffffff)))

  (defun emacs-load--artifact-native-write-u64 (base offset value u8-fn u32-fn)
    "Write VALUE as an unsigned 64-bit little-endian integer at BASE+OFFSET."
    (let ((low (logand value #xffffffff))
          (high (logand (lsh value -32) #xffffffff)))
      (if u32-fn
          (progn
            (funcall u32-fn base offset low)
            (funcall u32-fn base (+ offset 4) high))
        (funcall u8-fn base offset (logand low #xff))
        (funcall u8-fn base (+ offset 1) (logand (lsh low -8) #xff))
        (funcall u8-fn base (+ offset 2) (logand (lsh low -16) #xff))
        (funcall u8-fn base (+ offset 3) (logand (lsh low -24) #xff))
        (funcall u8-fn base (+ offset 4) (logand high #xff))
        (funcall u8-fn base (+ offset 5) (logand (lsh high -8) #xff))
        (funcall u8-fn base (+ offset 6) (logand (lsh high -16) #xff))
        (funcall u8-fn base (+ offset 7) (logand (lsh high -24) #xff)))))

  (defun emacs-load--artifact-native-write-stub (base offset runtime-address u8-fn u32-fn)
    "Write a 16-byte near stub at BASE+OFFSET for RUNTIME-ADDRESS."
    (let ((stub-address (+ base offset)))
      (funcall u8-fn base offset 72)
      (funcall u8-fn base (+ offset 1) 184)
      (emacs-load--artifact-native-write-u64
       base (+ offset 2) runtime-address u8-fn u32-fn)
      (funcall u8-fn base (+ offset 10) 255)
      (funcall u8-fn base (+ offset 11) 224)
      (funcall u8-fn base (+ offset 12) 0)
      (funcall u8-fn base (+ offset 13) 0)
      (funcall u8-fn base (+ offset 14) 0)
      (funcall u8-fn base (+ offset 15) 0)
      stub-address))

  (defun emacs-load--artifact-native-validate-externs (extern-symbols path)
    "Validate extern metadata and return a deduplicated symbol list."
    (let ((seen nil)
          (result nil))
      (dolist (symbol extern-symbols)
        (unless (emacs-load--artifact-native-extern-allowed-p symbol)
          (error "unsupported native extern %S in %s"
                 symbol (or path "<native>")))
        (unless (member symbol seen)
          (push symbol seen)
          (push symbol result)))
      (nreverse result)))

  (defun emacs-load--artifact-native-validate-relocs (relocs extern-symbols text-bytes path)
    "Validate RELOCS against EXTERN-SYMBOLS for TEXT-BYTES in PATH."
    (dolist (reloc relocs)
      (unless (listp reloc)
        (error "invalid native relocation entry %S in %s"
               reloc (or path "<native>")))
      (let ((offset (plist-get reloc :offset))
            (type (plist-get reloc :type))
            (symbol (plist-get reloc :symbol))
            (addend (plist-get reloc :addend)))
        (unless (eq type 'plt32)
          (error "unsupported native relocation type %S for %S in %s"
                 type symbol (or path "<native>")))
        (unless (and (stringp symbol)
                     (member symbol extern-symbols))
          (error "native relocation references unknown extern %S in %s"
                 symbol (or path "<native>")))
        (unless (and (integerp offset)
                     (<= 0 offset)
                     (<= (+ offset 4) text-bytes))
          (error "native relocation offset %S out of range for %s"
                 offset (or path "<native>")))
        (unless (integerp addend)
          (error "native relocation addend %S is not an integer for %s"
                 addend (or path "<native>"))))))

  (defun emacs-load--artifact-native-validate-defuns (defuns text-bytes path)
    "Validate native DEFUNS metadata against TEXT-BYTES in PATH."
    (dolist (entry defuns)
      (unless (listp entry)
        (error "invalid native defun entry %S in %s"
               entry (or path "<native>")))
      (let ((name (plist-get entry :name))
            (offset (plist-get entry :offset))
            (body-offset (plist-get entry :body-offset))
            (arity (plist-get entry :arity))
            (rt-slot-count (plist-get entry :rt-slot-count)))
        (unless (and (integerp offset)
                     (integerp body-offset)
                     (integerp rt-slot-count)
                     (integerp arity)
                     (<= 0 arity 6)
                     (<= 0 offset)
                     (<= 0 body-offset)
                     (<= 0 rt-slot-count)
                     (< (+ offset body-offset) text-bytes))
          (error "invalid native metadata for %S in %s"
                 name (or path "<native>"))))))

  (defun emacs-load--artifact-byte-length (string)
    "Return the byte length of STRING."
    (if (fboundp 'string-bytes)
        (string-bytes string)
      (length string)))

  (defun emacs-load--artifact-find-defuns-metadata (name native-context)
    "Find native :defuns metadata for NAME inside NATIVE-CONTEXT."
    (let* ((target (if (symbolp name) (symbol-name name) name))
           (defuns (and (listp native-context)
                        (emacs-load--artifact-native-section-get
                         native-context :defuns)))
           (found nil))
      (while (and defuns (not found))
        (let ((entry (pop defuns)))
          (when (and (listp entry)
                     (equal (plist-get entry :name) target))
            (setq found entry))))
      found))

  (defun emacs-load--artifact-defuns-metadata-name-key (name)
    "Return the canonical metadata lookup key for NAME."
    (if (symbolp name) (symbol-name name) name))

  (defun emacs-load--artifact-native-section-well-formed-p (native)
    "Return non-nil when NATIVE has structurally valid native metadata."
    (and (emacs-load--artifact-native-plist-p native)
         (stringp (emacs-load--artifact-native-section-get native :arch))
         (stringp (emacs-load--artifact-native-section-get native :text-base64))
         (or (and (listp native)
                  (plist-member native :relocs)
                  (listp (plist-get native :relocs)))
             (emacs-load--artifact-native-compact-relocs-p native))
         (listp (emacs-load--artifact-native-section-get native :extern-symbols))
         (listp (emacs-load--artifact-native-section-get native :defuns))))

  (defun emacs-load--artifact-native-section-canonicalize (native path)
    "Return a canonical native section plist for NATIVE."
    (unless (emacs-load--artifact-native-section-well-formed-p native)
      (error "invalid native section in %s" path))
    (list :arch (emacs-load--artifact-native-section-get native :arch)
          :text-base64
          (emacs-load--artifact-native-section-get native :text-base64)
          :relocs (emacs-load--artifact-native-section-relocs native path)
          :extern-symbols
          (emacs-load--artifact-native-section-get native :extern-symbols)
          :defuns
          (emacs-load--artifact-native-section-get native :defuns)))

  (defun emacs-load--artifact-native-section-compact (native path)
    "Return compact replay metadata for NATIVE."
    (unless (emacs-load--artifact-native-section-well-formed-p native)
      (error "invalid native section in %s" path))
    (list :arch (emacs-load--artifact-native-section-get native :arch)
          :defuns (emacs-load--artifact-native-section-get native :defuns)
          :path path
          :text-hash
          (emacs-load--sha256
           (emacs-load--artifact-native-section-get native :text-base64))))

  (defun emacs-load--artifact-native-section-replay-hash (native)
    "Return the diagnostic text hash for NATIVE."
    (or (plist-get native :text-hash)
        (let ((text (emacs-load--artifact-native-section-get
                     native :text-base64)))
          (and (stringp text)
               (emacs-load--sha256 text)))))

  (defun emacs-load--artifact-native-section-from-ranges
      (source ranges path)
    "Materialize a canonical native section plist from RANGES in SOURCE."
    (let* ((version-range (assq :native-section-version ranges))
           (runtime-prefix-range (assq :runtime-prefix ranges))
           (version
            (and version-range
                 (emacs-load--artifact-source-read-single-form-value
                  source (cdr version-range) path :native-section-version))))
      (cond
       ((or runtime-prefix-range
            (= (or version 0) emacs-load--artifact-native-section-version))
        (unless (and (= (or version 0) emacs-load--artifact-native-section-version)
                     runtime-prefix-range)
          (error "invalid native section in %s" path))
        (emacs-load--artifact-native-section-canonicalize
         (list :native-section-version version
               :runtime-prefix
               (emacs-load--artifact-source-read-single-form-value
                source (cdr runtime-prefix-range) path :runtime-prefix))
         path))
       (t
        (emacs-load--artifact-native-section-canonicalize
         (list :arch (emacs-load--artifact-source-read-form-value
                      source (cdr (assq :arch ranges)) path :arch)
               :text-base64 (emacs-load--artifact-source-read-form-value
                             source (cdr (assq :text-base64 ranges))
                             path :text-base64)
               :relocs (emacs-load--artifact-source-read-form-nil-or-list-value
                        source (cdr (assq :relocs ranges)) path :relocs)
               :extern-symbols
               (emacs-load--artifact-source-read-form-nil-or-list-value
                source (cdr (assq :extern-symbols ranges))
                path :extern-symbols)
               :defuns
               (emacs-load--artifact-source-read-form-nil-or-list-value
                source (cdr (assq :defuns ranges)) path :defuns))
         path)))))

  (defun emacs-load--artifact-native-sections-use-native-reader-p
      (source start end)
    "Return non-nil when SOURCE between START and END should use native read-all."
    (emacs-load--artifact-native-reader-within-threshold-p source start end))

  (defun emacs-load--artifact-native-sections-from-source
      (source start end path &optional handler)
    "Return canonical native section plists from SOURCE between START and END.
When HANDLER is non-nil, call it for each canonical section and collect the
non-nil return values instead of retaining the original section objects."
    (let ((pos (emacs-load--artifact-source-skip-ws-comments source start))
          (sections nil)
          (count 0))
      (cond
      ((>= pos end)
        (error "invalid native sections in %s" path))
       ((emacs-load--artifact-native-sections-use-native-reader-p source start end)
        (let* ((forms (condition-case nil
                          (nelisp--read-all-from-string-native
                           (substring source start end))
                        (error
                         (error "invalid native sections in %s" path))))
               (value nil))
          (unless (and (consp forms)
                       (null (cdr forms)))
            (error "invalid native sections in %s" path))
          (setq value (car forms))
          (unless (proper-list-p value)
            (error "invalid native sections in %s" path))
          (dolist (section value (nreverse sections))
            (let ((canonical
                   (emacs-load--artifact-native-section-canonicalize
                    section path)))
              (if handler
                  (let ((handled (funcall handler canonical)))
                    (when handled
                      (push handled sections)))
                (push canonical sections))))))
       ((and (= (- end pos) 3)
             (= (emacs-load--artifact-source-char-at source pos) ?n)
             (= (emacs-load--artifact-source-char-at source (+ pos 1)) ?i)
             (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
        nil)
       ((not (= (emacs-load--artifact-source-char-at source pos) ?\())
        (error "invalid native sections in %s" path))
       (t
        (setq pos (1+ pos))
        (while (progn
                 (setq pos (emacs-load--artifact-source-skip-ws-comments
                            source pos))
                 (< pos end))
          (when (= (emacs-load--artifact-source-char-at source pos) ?\))
            (setq pos end))
          (when (< pos end)
            (let* ((section-start pos)
                   (section-end (emacs-load--artifact-source-form-end
                                 source section-start))
                   (section-source (substring source section-start section-end))
                   (section-length (length section-source)))
              (let ((section
                     (emacs-load--artifact-native-section-from-ranges
                      section-source
                      (emacs-load--artifact-source-read-form-ranges
                       section-source 0 section-length path)
                      path)))
                (if handler
                    (let ((handled (funcall handler section)))
                      (when handled
                        (push handled sections)))
                  (push section sections)))
              (setq pos section-end))))
          (setq count (1+ count))
          (when (and load-garbage-collect-interval
                     (> load-garbage-collect-interval 0)
                     (= (% count load-garbage-collect-interval) 0)
                     (fboundp 'garbage-collect))
            (garbage-collect))))
        (nreverse sections)))

  (defun emacs-load--artifact-native-sections-from-payload (payload path)
    "Return sharded native sections from PAYLOAD, validating structure."
    (let ((sections (plist-get payload :native-sections)))
      (unless (listp sections)
        (error "invalid native sections in %s" path))
      (let ((result nil))
        (dolist (section sections (nreverse result))
          (push (emacs-load--artifact-native-section-canonicalize
                 section path)
                result)))))

  (defun emacs-load--artifact-native-section-bases (sections path)
    "Return eligible native SECTIONS paired with their mapped base addresses."
    (let ((result nil))
      (dolist (section sections (nreverse result))
        (when (emacs-load--artifact-native-eligible-p section)
          (push (cons section
                      (emacs-load--artifact-native-map-section section path))
                result)))))

  (defun emacs-load--artifact-find-defuns-metadata-in-sections (name section-bases path)
    "Find native :defuns metadata for NAME across SECTION-BASES.
Return a list of the matching native section, base address, and metadata."
    (let ((matches nil))
      (dolist (entry section-bases)
        (let ((section (car entry))
              (base (cdr entry)))
          (let ((meta (emacs-load--artifact-find-defuns-metadata name section)))
            (when meta
              (push (list section base meta) matches)))))
      (setq matches (nreverse matches))
      (cond
       ((null matches) nil)
       ((null (cdr matches)) (car matches))
       (t
        (error "native function %S appears in multiple sections in %s"
               name (or path "<artifact>"))))))

  (defun emacs-load--artifact-build-defuns-metadata-index (section-bases path)
    "Return a hash index of native defun metadata for SECTION-BASES."
    (let ((index (make-hash-table :test 'equal)))
      (dolist (entry section-bases index)
        (let ((section (car entry))
              (base (cdr entry))
              (defuns nil))
          (setq defuns (and (listp section)
                            (plist-get section :defuns)))
          (dolist (meta defuns)
            (let ((key (emacs-load--artifact-defuns-metadata-name-key
                        (plist-get meta :name))))
              (when key
                (when (gethash key index)
                  (error "native function %S appears in multiple sections in %s"
                         key (or path "<artifact>")))
                (puthash key (list section base meta) index))))))))

  (defun emacs-load--artifact-find-defuns-metadata-in-index (name index)
    "Return the indexed native metadata match for NAME from INDEX."
    (and index
         (gethash (emacs-load--artifact-defuns-metadata-name-key name) index)))

  (defun emacs-load--artifact-native-eligible-p (native)
    "Return non-nil when NATIVE can be replayed in-process."
    (and (emacs-load--artifact-native-plist-p native)
         (stringp (emacs-load--artifact-native-section-get native :text-base64))
         (listp (emacs-load--artifact-native-section-get native :defuns))
         (emacs-load--artifact-native-prefix-eligible-p native)
         (let ((relocs (emacs-load--artifact-native-section-relocs
                        native "<native>"))
               (extern-symbols
                (emacs-load--artifact-native-section-get
                 native :extern-symbols)))
           (let ((decoded
                  (emacs-load--artifact-native-decode-text
                   (emacs-load--artifact-native-section-get
                    native :text-base64)
                   nil t))
                 (text-bytes nil))
             (cond
              ((null decoded)
               nil)
              ((and (null relocs)
                    (null extern-symbols))
               t)
              ((or (null relocs)
                   (null extern-symbols))
               nil)
              (t
               (setq text-bytes (emacs-load--artifact-byte-length decoded))
               (let ((deduped-externs
                      (emacs-load--artifact-native-validate-externs
                       extern-symbols nil)))
                 (emacs-load--artifact-native-validate-relocs
                  relocs deduped-externs
                  text-bytes
                  nil)
                 (emacs-load--artifact-native-validate-defuns
                  (emacs-load--artifact-native-section-get native :defuns)
                  text-bytes
                  nil)
                 t)))))))

  (defun emacs-load--artifact-bcl-replay-available-p ()
    "Return non-nil when the legacy BCL replay substrate is available."
    (and (boundp 'nelisp--functions)
         (fboundp 'nelisp--apply)))

  (defun emacs-load--artifact-source-defun-fallback-valid-p (name source-defun)
    "Return non-nil when SOURCE-DEFUN is an exact defun for NAME."
    (and (consp source-defun)
         (eq (car source-defun) 'defun)
         (let ((defun-name (nth 1 source-defun)))
           (and (symbolp defun-name)
                (equal (if (symbolp name)
                           (symbol-name name)
                         name)
                       (symbol-name defun-name))))))

  (defun emacs-load--artifact-fn-source-defun (item path)
    "Return ITEM's source-defun fallback or signal if malformed."
    (let ((tail (nthcdr 3 item)))
      (when tail
        (let ((source-defun (car tail)))
          (unless (and (null (nthcdr 4 item))
                       (emacs-load--artifact-source-defun-fallback-valid-p
                        (nth 1 item) source-defun))
            (error "invalid source defun fallback for %S in artifact%s"
                   (nth 1 item)
                   (if path (format " (%s)" path) "")))
          source-defun))))

  (defun emacs-load--artifact-native-lambda-form (body-address arity rt-slot-count)
    "Build a fixed-arity native call lambda form."
    (pcase arity
      (0 `(lambda nil
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count)))
      (1 `(lambda (a0)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0)))
      (2 `(lambda (a0 a1)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1)))
      (3 `(lambda (a0 a1 a2)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2)))
      (4 `(lambda (a0 a1 a2 a3)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2 a3)))
      (5 `(lambda (a0 a1 a2 a3 a4)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2 a3 a4)))
      (6 `(lambda (a0 a1 a2 a3 a4 a5)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2 a3 a4 a5)))
      (_ (error "unsupported native arity %S" arity))))

  (defun emacs-load--artifact-native-map-section (native &optional path)
    "Map the native text for NATIVE and return its base address."
    (let* ((text (emacs-load--artifact-native-section-get native :text-base64))
           (relocs (or (emacs-load--artifact-native-section-relocs
                        native (or path "<native>"))
                       nil))
           (extern-symbols
            (or (emacs-load--artifact-native-section-get
                 native :extern-symbols)
                nil))
           (cache-key (emacs-load--artifact-native-content-key native))
           (base (gethash cache-key emacs-load--artifact-native-hash-cache))
           (decoded nil)
           (text-bytes nil)
           (total-bytes nil)
           (page-size nil)
           (rounded-page-size nil)
           (syscall-direct-fn nil)
           (ptr-write-u8-fn nil)
           (ptr-copy-string-bytes-fn nil)
           (ptr-write-u32-fn nil)
           (stub-symbols nil)
           (stub-count 0)
           (stub-addresses nil))
      (unless (and (integerp base) (> base 0))
        (setq decoded
              (emacs-load--artifact-native-decode-text text path))
        (setq text-bytes (emacs-load--artifact-byte-length decoded))
        (setq page-size
              (or (and (fboundp 'page-size) (page-size))
                  4096))
        (setq stub-symbols
              (if (and (null relocs) (null extern-symbols))
                  nil
                (progn
                  (when (or (null relocs) (null extern-symbols))
                    (error "native section must provide both relocs and extern-symbols for %S"
                           (or path text)))
                  (emacs-load--artifact-native-validate-externs
                   extern-symbols path))))
        (setq stub-count (length stub-symbols))
        (emacs-load--artifact-native-validate-relocs
         relocs stub-symbols text-bytes
         path)
        (emacs-load--artifact-native-validate-defuns
         (emacs-load--artifact-native-section-get native :defuns)
         text-bytes
         path)
        (setq total-bytes (+ text-bytes (* stub-count 16)))
        (setq rounded-page-size
              (* page-size
                 (/ (+ total-bytes page-size -1)
                    page-size)))
        (setq syscall-direct-fn
              (cond
               ((fboundp 'syscall-direct) #'syscall-direct)
               ((fboundp 'nelisp--syscall-direct) #'nelisp--syscall-direct)
               (t nil)))
        (unless syscall-direct-fn
          (error "no syscall-direct helper available for %S"
                 (or path text)))
        (setq base (funcall syscall-direct-fn 9 0 rounded-page-size 7 34 -1 0))
        (unless (and (integerp base) (> base 0))
          (error "mmap failed for %S" (or path text)))
        (setq ptr-write-u8-fn
              (cond
               ((fboundp 'ptr-write-u8) #'ptr-write-u8)
               ((fboundp 'nelisp--ptr-write-u8) #'nelisp--ptr-write-u8)
               (t nil)))
        (unless ptr-write-u8-fn
          (error "no ptr-write-u8 helper available for %S" (or path text)))
        (setq ptr-copy-string-bytes-fn
              (and (fboundp 'nelisp--ptr-copy-string-bytes)
                   #'nelisp--ptr-copy-string-bytes))
        (setq ptr-write-u32-fn
              (cond
               ((fboundp 'ptr-write-u32) #'ptr-write-u32)
               ((fboundp 'nelisp--ptr-write-u32) #'nelisp--ptr-write-u32)
               (t nil)))
        (if ptr-copy-string-bytes-fn
            (let ((copied (funcall ptr-copy-string-bytes-fn base decoded)))
              (unless (and (integerp copied)
                           (= copied text-bytes))
                (error "native copy count mismatch for %s: expected %d, got %S"
                       (or path text) text-bytes copied)))
          (dotimes (i text-bytes)
            (funcall ptr-write-u8-fn base i
                     (emacs-load--artifact-byte-at decoded i))))
        (setq stub-addresses nil)
        (dolist (symbol stub-symbols)
          (let ((runtime-address (nelisp--runtime-symbol-address symbol))
                (stub-offset (+ text-bytes (* (length stub-addresses) 16))))
            (unless (and (integerp runtime-address) (> runtime-address 0))
              (error "runtime symbol %S resolved to invalid address %S"
                     symbol runtime-address))
            (emacs-load--artifact-native-write-stub
             base stub-offset runtime-address ptr-write-u8-fn ptr-write-u32-fn)
            (push (cons symbol (+ base stub-offset)) stub-addresses)))
        (dolist (reloc relocs)
          (let* ((offset (plist-get reloc :offset))
                 (symbol (plist-get reloc :symbol))
                 (addend (plist-get reloc :addend))
                 (stub-address (cdr (assoc symbol stub-addresses)))
                 (value (emacs-load--artifact-native-rel32-value
                         stub-address addend (+ base offset))))
            (if ptr-write-u32-fn
                (funcall ptr-write-u32-fn base offset value)
              (funcall ptr-write-u8-fn base offset (logand value #xff))
              (funcall ptr-write-u8-fn base (+ offset 1) (logand (lsh value -8) #xff))
              (funcall ptr-write-u8-fn base (+ offset 2) (logand (lsh value -16) #xff))
              (funcall ptr-write-u8-fn base (+ offset 3) (logand (lsh value -24) #xff)))))
        (puthash cache-key base emacs-load--artifact-native-hash-cache))
      base))

  (defun emacs-load--artifact-native-install-fn (name native base meta)
    "Install NAME from native metadata NATIVE into `nelisp--functions'."
    (let* ((offset (plist-get meta :offset))
           (arity (plist-get meta :arity))
           (body-offset (plist-get meta :body-offset))
           (rt-slot-count (plist-get meta :rt-slot-count)))
      (unless (and (integerp offset)
                   (integerp body-offset)
                   (integerp rt-slot-count)
                   (integerp arity)
                   (<= 0 arity 6)
                   (<= 0 body-offset)
                   (<= 0 rt-slot-count))
        (error "invalid native metadata for %S" name))
      (let* ((address (+ base offset))
             (body-address (+ address body-offset)))
        (setq emacs-load--artifact-native-diagnostic-report
              (list :name name
                    :arch (plist-get native :arch)
                    :hash (emacs-load--artifact-native-section-replay-hash native)
                    :offset offset
                    :body-offset body-offset
                    :address address
                    :body-address body-address
                    :arity arity
                    :rt-slot-count rt-slot-count
                    :base base
                    :path (plist-get native :path)))
        (fset name
              (emacs-load--artifact-native-lambda-form
               body-address arity rt-slot-count))
        name)))

  (defun emacs-load--artifact-install-fn (name fn)
    "Install NAME from artifact function FN into `nelisp--functions'."
    (unless (and (boundp 'nelisp--functions) (fboundp 'nelisp--apply))
      (error "no BCL replay path for %S" name))
    (puthash name fn nelisp--functions)
    (fset name
          `(lambda (&rest args)
             (nelisp--apply (gethash ',name nelisp--functions) args)))
    name)

  (defun emacs-load--artifact-literal-value (form)
    "Return a literal value for artifact FORM."
    (cond
     ((or (null form)
          (eq form t)
          (numberp form)
          (stringp form)
          (keywordp form))
      form)
     ((and (consp form) (eq (car form) 'quote))
      (nth 1 form))
     (t
      (eval form))))

  (defun emacs-load--artifact-eval-form (form)
    "Evaluate a replay artifact FORM and return its replay value."
    (cond
     ((and (consp form) (eq (car form) 'defun))
      (if (fboundp 'nelisp-eval)
          (nelisp-eval form)
        (eval form)))
     ((and (consp form) (eq (car form) 'defvar))
      (puthash (nth 1 form) t nelisp--specials)
      (nth 1 form))
     ((and (consp form) (eq (car form) 'defconst))
      (puthash (nth 1 form) t nelisp--specials)
      (puthash (nth 1 form)
               (emacs-load--artifact-literal-value (nth 2 form))
               nelisp--globals)
      (nth 1 form))
     (t
      (eval form))))

  (defun emacs-load--artifact-replay-item (item &optional path native-section base)
    "Replay one artifact ITEM and return its replay value."
    (cond
     ((and (consp item) (eq (car item) :fn))
      (let* ((name (nth 1 item))
             (fn (nth 2 item))
             (source-defun (emacs-load--artifact-fn-source-defun item path))
             (meta (and native-section
                        (emacs-load--artifact-find-defuns-metadata
                         name native-section))))
        (cond
         ((and meta (integerp base))
          (emacs-load--artifact-native-install-fn
           name native-section base meta))
         ((emacs-load--artifact-bcl-replay-available-p)
          (emacs-load--artifact-install-fn name fn))
         (source-defun
          (emacs-load--artifact-eval-form source-defun))
         (t
          (error "no native or BCL replay path for %S in artifact%s"
                 name (if path (format " (%s)" path) ""))))))
     ((and (consp item) (eq (car item) :eval))
      (eval (nth 1 item)))
     (t
      (emacs-load--artifact-eval-form item))))

  (defun emacs-load--artifact-replay-item-with-native-sections
      (item &optional path section-bases defuns-index)
    "Replay one artifact ITEM against sharded native SECTION-BASES."
    (cond
     ((and (consp item) (eq (car item) :fn))
      (let* ((name (nth 1 item))
             (fn (nth 2 item))
             (source-defun (emacs-load--artifact-fn-source-defun item path))
             (match (if defuns-index
                        (emacs-load--artifact-find-defuns-metadata-in-index
                         name defuns-index)
                      (and section-bases
                           (emacs-load--artifact-find-defuns-metadata-in-sections
                            name section-bases path)))))
        (cond
         (match
          (emacs-load--artifact-native-install-fn
           name (nth 0 match) (nth 1 match) (nth 2 match)))
         ((emacs-load--artifact-bcl-replay-available-p)
          (emacs-load--artifact-install-fn name fn))
         (source-defun
          (emacs-load--artifact-eval-form source-defun))
         (t
          (error "no native or BCL replay path for %S in artifact%s"
                 name (if path (format " (%s)" path) ""))))))
     ((and (consp item) (eq (car item) :eval))
      (eval (nth 1 item)))
     (t
      (emacs-load--artifact-eval-form item))))

  (defun emacs-load--artifact-replay-payload-small (payload path)
    "Replay PAYLOAD using the historical whole-payload reader."
    (unless (plist-member payload :module-init)
      (error "missing :module-init in %s" path))
    (setq emacs-load--last-artifact-payload payload)
    (let* ((native-sections-present (plist-member payload :native-sections))
           (native-sections (and native-sections-present
                                 (emacs-load--artifact-native-sections-from-payload
                                  payload path)))
           (section-bases (and native-sections-present
                               (emacs-load--artifact-native-section-bases
                                native-sections path)))
           (defuns-index (and section-bases
                              (emacs-load--artifact-build-defuns-metadata-index
                               section-bases path)))
           (section-raw (and (plist-member payload :native)
                             (plist-get payload :native)))
           (section (and section-raw
                         (emacs-load--artifact-native-section-canonicalize
                          section-raw path)))
           (base (and (emacs-load--artifact-native-eligible-p section)
                      (emacs-load--artifact-native-map-section
                       section path))))
      (dolist (item (plist-get payload :module-init))
        (if native-sections-present
            (emacs-load--artifact-replay-item-with-native-sections
             item path section-bases defuns-index)
          (emacs-load--artifact-replay-item
           item path section base))))
    payload)

  (defun emacs-load--artifact-replay-payload-streaming (content path prefix-end)
    "Replay a large artifact CONTENT without materializing the full payload."
    (let* ((byte-length (emacs-load--artifact-byte-length content))
           (content-length (length content))
           (payload-start (emacs-load--artifact-source-skip-ws-comments
                           content prefix-end))
           (payload-close (emacs-load--artifact-source-rskip-ws
                           content (1- content-length)))
           (module-marker
            (emacs-load--artifact-source-find-marker
             content ":module-init " prefix-end path :module-init t))
           (native-sections-marker
            (emacs-load--artifact-source-find-marker
             content ":native-sections " prefix-end path :native-sections nil))
       (native-marker
        (and (not native-sections-marker)
             (emacs-load--artifact-source-find-marker
              content ":native " prefix-end path :native nil)))
           (field-names nil)
           (native-mode nil)
           (section-bases nil)
           (defuns-index nil)
           (module-count 0)
           (module-pos nil)
           (module-end nil))
      (unless (and (< payload-start content-length)
                   (= (aref content payload-start) ?\())
        (error "invalid artifact payload in %s" path))
      (unless (and (<= 0 payload-close)
                   (< payload-close content-length)
                   (= (aref content payload-close) ?\)))
        (error "unterminated artifact payload in %s" path))
      (setq field-names
            (let ((entries nil))
              (push (cons module-marker :module-init) entries)
              (when native-sections-marker
                (push (cons native-sections-marker :native-sections) entries))
              (when native-marker
                (push (cons native-marker :native) entries))
              (mapcar #'cdr
                      (sort entries (lambda (a b) (< (car a) (car b)))))))
      (cond
       (native-sections-marker
        (setq native-mode :native-sections)
        (let* ((value-start (emacs-load--artifact-source-skip-ws-comments
                             content
                             (+ native-sections-marker
                                (length ":native-sections "))))
               (value-end (emacs-load--artifact-source-form-end
                           content value-start)))
          (setq section-bases
                (emacs-load--artifact-native-sections-from-source
                 content value-start value-end path
                 (lambda (section)
                   (when (emacs-load--artifact-native-eligible-p section)
                     (cons (emacs-load--artifact-native-section-compact
                            section path)
                           (emacs-load--artifact-native-map-section
                            section path)))))))
        (setq defuns-index
              (emacs-load--artifact-build-defuns-metadata-index
               section-bases path)))
       (native-marker
        (setq native-mode :native)
        (let* ((value-start (emacs-load--artifact-source-skip-ws-comments
                            content (+ native-marker (length ":native "))))
               (value-end (emacs-load--artifact-source-form-end
                           content value-start))
               (ranges (emacs-load--artifact-source-read-form-ranges
                       content value-start value-end path)))
          (when (emacs-load--artifact-native-section-preflight-from-ranges
                 content ranges path)
            (let ((section
                   (emacs-load--artifact-native-section-from-ranges
                    content ranges path)))
              (when (emacs-load--artifact-native-eligible-p section)
                (setq section-bases
                      (list (cons (emacs-load--artifact-native-section-compact
                                   section path)
                                  (emacs-load--artifact-native-map-section
                                   section path))))
                (setq defuns-index
                      (emacs-load--artifact-build-defuns-metadata-index
                       section-bases path)))))))
      )
      (setq module-pos
            (emacs-load--artifact-source-skip-ws-comments
             content (+ module-marker (length ":module-init "))))
      (unless (< module-pos content-length)
        (error "invalid :module-init in %s" path))
      (let ((module-form-end (emacs-load--artifact-source-form-end
                              content module-pos)))
        (setq module-end module-form-end)
        (setq module-count
              (if (and (= (emacs-load--artifact-source-char-at
                           content module-pos) ?\()
                       (or (fboundp 'nelisp--read-batch-vector-from-string-native)
                           (fboundp 'nelisp--read-batch-from-string-native)))
                  (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                   (substring content (1+ module-pos) (1- module-form-end))
                   path :module-init
                   (lambda (item)
                     (if section-bases
                         (emacs-load--artifact-replay-item-with-native-sections
                          item path section-bases defuns-index)
                       (emacs-load--artifact-replay-item
                        item path nil nil))))
                 (emacs-load--artifact-source-iterate-form-nil-or-list-value
                 content (cons module-pos module-form-end) path :module-init
                 (lambda (item)
                   (if section-bases
                       (emacs-load--artifact-replay-item-with-native-sections
                        item path section-bases defuns-index)
                     (emacs-load--artifact-replay-item
                      item path nil nil))))))
    )
      (setq module-pos
            (emacs-load--artifact-source-skip-ws-comments content module-end))
      (when (> module-pos payload-close)
        (error "invalid trailing data in %s" path))
      (setq emacs-load--last-artifact-payload
            (emacs-load--artifact-streaming-summary
             path byte-length native-mode module-count field-names))
      emacs-load--last-artifact-payload))

(defun emacs-load--artifact-replay-file (path &optional native-context)
  "Replay a private artifact file at PATH and return its payload."
  (let* ((top-level-p (zerop emacs-load--artifact-replay-depth))
         (threshold emacs-load-artifact-replay-pressure-threshold)
         (load-garbage-collect-interval
          (if top-level-p
              (emacs-load--artifact-replay-garbage-collect-interval)
            load-garbage-collect-interval)))
    (when (and emacs-load-artifact-replay-preflight-gc
               top-level-p
               (fboundp 'garbage-collect))
      (garbage-collect))
    (if top-level-p
        (let* ((initial-baseline (and (integerp threshold)
                                      (> threshold 0)
                                      (emacs-load--artifact-replay-arena-used-bytes)))
               (pressure-enabled (and (integerp threshold)
                                     (> threshold 0)
                                     (integerp initial-baseline)
                                     (>= initial-baseline 0))))
          (let ((emacs-load--artifact-replay-pressure-baseline initial-baseline)
                (emacs-load--artifact-replay-pressure-enabled pressure-enabled)
                (emacs-load--artifact-replay-depth (1+ emacs-load--artifact-replay-depth))
                (result nil))
            (setq result
                  (let ((content (emacs-load--read-file-string path))
                        (prefix ";;; nelisp-private-nelc-v2\n"))
                    (unless (stringp content)
                      (error "invalid artifact contents in %s" path))
                    (unless (string-prefix-p prefix content)
                      (error "invalid artifact prefix in %s" path))
                    (if (and (integerp emacs-load-artifact-replay-streaming-threshold)
                             (> emacs-load-artifact-replay-streaming-threshold 0)
                             (> (emacs-load--artifact-byte-length content)
                                emacs-load-artifact-replay-streaming-threshold))
                        (emacs-load--artifact-replay-payload-streaming
                         content path (length prefix))
                      (let ((read (read-from-string content (length prefix))))
                        (unless (and (consp read) (>= (cdr read) (length prefix)))
                          (error "invalid artifact payload in %s" path))
                        (let ((payload (car read)))
                          (unless (listp payload)
                            (error "invalid artifact payload in %s" path))
                          (emacs-load--artifact-replay-payload-small payload path))))))
            (emacs-load--artifact-replay-pressure-maybe-gc)
            result))
      (let ((emacs-load--artifact-replay-depth (1+ emacs-load--artifact-replay-depth)))
        (emacs-load--artifact-replay-pressure-maybe-gc)
        (let ((content (emacs-load--read-file-string path))
              (prefix ";;; nelisp-private-nelc-v2\n"))
          (unless (stringp content)
            (error "invalid artifact contents in %s" path))
          (unless (string-prefix-p prefix content)
            (error "invalid artifact prefix in %s" path))
          (if (and (integerp emacs-load-artifact-replay-streaming-threshold)
                   (> emacs-load-artifact-replay-streaming-threshold 0)
                   (> (emacs-load--artifact-byte-length content)
                      emacs-load-artifact-replay-streaming-threshold))
              (emacs-load--artifact-replay-payload-streaming
               content path (length prefix))
            (let ((read (read-from-string content (length prefix)))
                  (result nil))
              (unless (and (consp read) (>= (cdr read) (length prefix)))
                (error "invalid artifact payload in %s" path))
              (let ((payload (car read)))
                (unless (listp payload)
                  (error "invalid artifact payload in %s" path))
                (emacs-load--artifact-replay-payload-small payload path)))))))))

  ;; NOTE (2026-07-12): a retry-with-rewrite fallback (rewriting top-level
  ;; `defalias' to `nelisp--defalias-late' on load failure) was attempted
  ;; here and REVERTED: wrapping the fast loader in `condition-case'
  ;; mis-captures its internal non-local exits on this substrate (known
  ;; condition-case/throw defect), silently turning successful by-name
  ;; loads into zero-form no-ops.  Any future retry design must not wrap
  ;; `nelisp--eval-source-string' in a handler.
  (defvar nelisp--load-rewrite-wrapper-heads
    '(progn prog1 prog2 when unless if while eval-when-compile
      eval-and-compile with-no-warnings with-suppressed-warnings
      condition-case let let*)
    "Wrapper heads whose nested forms are eligible for load-time rewrites.")

  (defun nelisp--load-rewrite-defalias-form (nelisp--load-rw-form)
    "Rewrite top-level DEFALIAS forms in FORM for standalone load retry.
Only descend through `nelisp--load-rewrite-wrapper-heads'.  Quoted data
subtrees are left untouched."
    (if (consp nelisp--load-rw-form)
        (let ((nelisp--load-rw-head (car nelisp--load-rw-form)))
          (cond
           ((memq nelisp--load-rw-head '(quote function))
            nelisp--load-rw-form)
           ((eq nelisp--load-rw-head 'defalias)
            (setcar nelisp--load-rw-form 'nelisp--defalias-late)
            nelisp--load-rw-form)
           ((memq nelisp--load-rw-head nelisp--load-rewrite-wrapper-heads)
            (let ((nelisp--load-rw-tail (cdr nelisp--load-rw-form))
                  (nelisp--load-rw-node nil))
              (while (consp nelisp--load-rw-tail)
                (setq nelisp--load-rw-node (car nelisp--load-rw-tail))
                (when (consp nelisp--load-rw-node)
                  (setcar nelisp--load-rw-tail
                          (nelisp--load-rewrite-defalias-form
                           nelisp--load-rw-node)))
                (setq nelisp--load-rw-tail (cdr nelisp--load-rw-tail))))
            nelisp--load-rw-form)
           (t nelisp--load-rw-form)))
      nelisp--load-rw-form))

  (defun nelisp--load-eval-source-rewriting (source)
    "Retry loader for SOURCE with top-level `defalias' rewritten late.
This interpreted fallback is intentionally slow and is a future native
optimization candidate; keep the normal load path on the fast reader."
    (let ((nelisp--load-rw-pos 0)
          (nelisp--load-rw-len (length source))
          (nelisp--load-rw-last nil)
          (nelisp--load-rw-count 0)
          (nelisp--load-rw-read nil))
      (while (progn
               (setq nelisp--load-rw-pos
                     (nelisp--load-skip-space-and-comments
                      source nelisp--load-rw-pos))
               (< nelisp--load-rw-pos nelisp--load-rw-len))
        (setq nelisp--load-rw-read
              (read-from-string source nelisp--load-rw-pos))
        (when (or (not (consp nelisp--load-rw-read))
                  (<= (cdr nelisp--load-rw-read) nelisp--load-rw-pos))
            (signal 'end-of-file
                    (list "rewrite load reader made no progress"
                          nelisp--load-rw-pos)))
        (setq nelisp--load-rw-last
              (eval
               (nelisp--load-rewrite-defalias-form
                (car nelisp--load-rw-read))))
        (setq nelisp--load-rw-pos (cdr nelisp--load-rw-read))
        (setq nelisp--load-rw-count (+ nelisp--load-rw-count 1))
        (when (and load-garbage-collect-interval
                   (> load-garbage-collect-interval 0)
                   (= (% nelisp--load-rw-count
                         load-garbage-collect-interval)
                      0)
                   (fboundp 'garbage-collect))
          (garbage-collect)))
      nelisp--load-rw-last))

  (defun emacs-load--regular-file-p (resolved)
    "Return non-nil when RESOLVED names an existing non-directory file."
    (if (fboundp 'nelisp--syscall-stat)
        (eq (nelisp--syscall-stat resolved) 'file)
      (and (file-exists-p resolved)
           (not (file-directory-p resolved)))))

  (defun nelisp--load-resolved-file (resolved noerror)
    "Load exact absolute RESOLVED path, honoring NOERROR for open failures."
    (cond
     ((not (emacs-load--regular-file-p resolved))
      (if noerror nil
        (signal 'file-error (list "Cannot open load file" resolved))))
     (t
      (let* ((base (file-name-nondirectory resolved))
             (source (emacs-load--read-file-string resolved))
             (artifact (and emacs-load-auto-native-compile
                            (not (string-prefix-p "cc-" base))
                            (stringp source)
                            (emacs-load--artifact-load-or-compile
                             resolved source))))
        (if artifact
            t
          (cond
           ((null source)
            (if noerror nil
              (signal 'file-error (list "read error" resolved))))
           (t
            ;; `cc-provide' is a compile-time wrapper around `provide', but
            ;; NeLisp's fast source evaluator bare-aborts on that form head.
            ;; Normalize the sole runtime form before handing off the source.
            (when (and (>= (length resolved) 10)
                       (string= (substring resolved (- (length resolved) 10))
                                "cc-defs.el"))
              (let ((tail-start (max 0 (- (length source) 256))))
                (setq source
                      (concat
                       (substring source 0 tail-start)
                       (string-replace "(cc-provide 'cc-defs)"
                                       "(provide 'cc-defs)"
                                       (substring source tail-start))))))
            ;; Per-file replay marker for the CC Mode language-constant
            ;; machinery.  `(setq load-file-name resolved)' below is invisible
            ;; to the interpreted elisp inside the native source evaluator, so
            ;; `c-get-current-file' (cc-defs.el) would otherwise collapse every
            ;; cc-*.el onto one synthetic key.  The per-file `source' anchor
            ;; alist that `c-define-lang-constant' relies on then overwrites
            ;; prior files' bindings, producing spurious cyclic-reference
            ;; errors.  Splice a marker `setq' at the head of the source (same
            ;; eval-source-string, hence visible to the following top-level
            ;; forms -- decisively unlike the outer `setq') plus a restoring
            ;; `pop' at the tail.  For cc-defs.el itself, also append a
            ;; `c-get-current-file' redefinition (defun, so the new function
            ;; cell reaches every caller) that consults the marker.
            (when (string-prefix-p "cc-" (file-name-sans-extension base))
              (let* ((base (file-name-sans-extension base))
                     (cc-redef
                      (if (string= base "cc-defs")
                          (concat
                           "\n(defun c-get-current-file ()"
                           " (let* ((c-or-l (cc-bytecomp-compiling-or-loading))"
                           "        (file (cond ((eq c-or-l 'loading) load-file-name)"
                           "                    ((eq c-or-l 'compiling) byte-compile-dest-file)"
                           "                    ((null c-or-l) (buffer-file-name)))))"
                           "   (setq file (or file load-file-name nelisp--cc-replay-file \"cc-lang-replay\"))"
                           "   (and file (file-name-sans-extension (file-name-nondirectory file)))))")
                        "")))
                (setq source
                      (concat
                       "(push nelisp--cc-replay-file nelisp--cc-replay-file-stack)"
                       "(setq nelisp--cc-replay-file "
                       (prin1-to-string base) ")\n"
                       source
                       cc-redef
                       "\n(setq nelisp--cc-replay-file"
                       " (pop nelisp--cc-replay-file-stack))"))))
            (let* ((parent (or (file-name-directory resolved) "./"))
                   (prior-lfn (and (boundp 'load-file-name) load-file-name))
                   (prior-dd (and (boundp 'default-directory) default-directory))
                   (loader (nelisp--load-source-loader source)))
              (setq load-file-name resolved)
              (setq default-directory parent)
              (unwind-protect
                  (progn
                    (funcall loader source)
                    t)
                (setq load-file-name prior-lfn)
                (setq default-directory prior-dd))))))))))

  (defun load (file &optional noerror _nomessage _nosuffix _must-suffix)
    "Resolve FILE through `load-path' and execute it."
    (let* ((absolute-p
            (and (stringp file)
                 (> (length file) 0)
                 (or (and (fboundp 'file-name-absolute-p)
                          (file-name-absolute-p file))
                     (= (aref file 0) ?/))))
           (resolved (locate-library file)))
      (cond
       (resolved
        (nelisp--load-resolved-file resolved noerror))
       (absolute-p
        (nelisp--load-resolved-file (expand-file-name file) noerror))
       (noerror nil)
       (t
        (signal 'file-error (list "Cannot open load file" file))))))

(defun load-file (file)
  "Execute exactly FILE after absolute-name expansion.
Unlike `load', this never searches `load-path' or adds a suffix."
  (let ((resolved (expand-file-name file)))
    (nelisp--load-resolved-file resolved nil)))

)

(provide 'emacs-load)

;;; emacs-load.el ends here
