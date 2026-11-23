;;; nelisp-standalone-cachekey.el --- standalone unit cache key and entries -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Pure helpers for the per-unit cache of `nelisp-standalone-build.el'.
;; They need no compiler, so unit tests can run them without a build.
;;
;; Key completeness (step 3): the toolchain digest covers the resolved .el
;; SOURCE of every library the compiler can load (a static `require' closure
;; from the root list below), never a stale .elc, plus the codegen-affecting
;; environment and live settings (see `nelisp-standalone-cachekey-env-alist').
;; Entry integrity (step 4): an entry is a header line carrying the key and
;; a sha256 of the payload; a reader verifies both and treats any failure as
;; a miss.  Writes go to a unique temp name and are renamed into place.
;; Digest narrowing (step 2): see `nelisp-standalone-cachekey-masked-digest'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-standalone-cachekey-root-features
  '("nelisp-aot-compiler" "nelisp-standalone-arena-rewrite" "nelisp-static-linker"
    "nelisp-elf-write" "nelisp-cc-runtime" "nelisp-standalone-argframe"
    "nelisp-heap-accessors" "nelisp-cc-fxir-native"
    ;; Added by the 2026-11 key audit: required by nelisp-aot-compiler.el:111-114
    ;; and nelisp-cc-fxir-native.el:57-58, absent from the old dependency list.
    "nelisp-asm-x86_64" "nelisp-asm-arm64" "nelisp-sexp-layout"
    "nelisp-cc-fxir-rows" "nelisp-cc-fxir-effects"
    "nelisp-standalone-cachekey")
  "Library features whose source is hashed into the toolchain digest.
The static `require' closure of these (inside the repository) is added.")

(defconst nelisp-standalone-cachekey-env-names
  '("NELISP_STACKMAP" "NELISP_ALIGNHOT_PERTURB" "NC2_LAZY" "NELISP_LINUX_ARENA_SIZE"
    "NELISP_XINLINE" "NELISP_INLINE" "NELISP_INLINE_MAX_NODES" "NELISP_TCO"
    "NELISP_READER_DYNAMIC" "NELISP_REPR_CENSUS"
    ;; Read by `runtime-reload-enabled-p' (xinline-enabled-p): found by the closure scan.
    "NELISP_RUNTIME_RELOAD")
  "Environment variables read on the compile path.
All are part of the key, so a value that changes generated code is a miss.")

(defun nelisp-standalone-cachekey--locate (feature load-path-list)
  "Resolve FEATURE (a string, \".el\" tolerated) to its .el source, or nil.
Never returns a .elc: the loader runs with `load-prefer-newer' and the key
must hash the file whose text defines the code."
  (let ((name (if (string-suffix-p ".el" feature) (substring feature 0 -3) feature)))
    ;; NOSUFFIX non-nil with an explicit \".el\": never the .elc beside it.
    (let ((load-path load-path-list)) (locate-library (concat name ".el") t))))

(defun nelisp-standalone-cachekey-require-closure (roots _repo-root load-path-list)
  "Return the sorted list of .el files in ROOTS and what they `require'.
Files of the host Emacs distribution are not followed (the Emacs version is
part of the key instead); a feature that does not resolve is
returned as its bare name so the digest records the gap instead of hiding it.
The scan is textual, so it over-approximates (nested and conditional requires
are included), which can only cost a miss, never a stale hit."
  (let ((seen (make-hash-table :test 'equal)) (files nil) (missing nil)
        (queue (copy-sequence roots))
        (system-lisp (file-name-as-directory (expand-file-name "../lisp" data-directory))))
    (while queue
      (let* ((feat (pop queue))
             (file (nelisp-standalone-cachekey--locate feat load-path-list)))
        (cond
         ((null file) (unless (gethash feat seen) (puthash feat t seen) (push feat missing)))
         ((gethash (expand-file-name file) seen))
         (t
          (setq file (expand-file-name file))
          (puthash file t seen)
          (unless (string-prefix-p system-lisp file)
            (push file files)
            (with-temp-buffer
              (insert-file-contents-literally file)
              (goto-char (point-min))
              (while (re-search-forward "(require[ \t\n]+'\\([^ \t\n()]+\\)" nil t)
                (push (match-string 1) queue))))))))
    (append (sort files (lambda (a b) (string< (file-name-nondirectory a) (file-name-nondirectory b)))) (mapcar (lambda (m) (concat "?" m)) (sort missing #'string<)))))

(defun nelisp-standalone-cachekey-file-requires (file)
  "Features FILE requires at top level (a line starting with \"(require '\")."
  (let (r)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (re-search-forward "^(require[ \t]+'\\([^ \t\n()]+\\)" nil t)
        (push (match-string 1) r)))
    (delete-dups (nreverse r))))

(defconst nelisp-standalone-cachekey-opaque-dep-regexp
  (concat "^[ \t]*(\\(load[ \t\n]+\"\\|require[ \t\n]+(\\|autoload[ \t\n]+'\\)"
          "\\|^[ \t]*(require[ \t\n]+[a-z]")
  "Dependency forms the textual closure cannot follow.")

(defun nelisp-standalone-cachekey-opaque-deps (files)
  "Return \"file:line\" for each form in FILES that loads code the closure cannot see.
`(load \"x\")', `(require (quote x))', `(require VAR)' and `(autoload ...)'
add dependencies that the scan for `(require 'x)' misses, so a source tree
that uses them would let an edit go unkeyed."
  (let (hits)
    (dolist (f files)
      (unless (string-prefix-p "?" f)
        (with-temp-buffer
          (insert-file-contents-literally f)
          (goto-char (point-min))
          (while (re-search-forward nelisp-standalone-cachekey-opaque-dep-regexp nil t)
            (push (format "%s:%d" (file-name-nondirectory f) (line-number-at-pos)) hits)))))
    (nreverse hits)))

(defun nelisp-standalone-cachekey-files-digest (files)
  "Digest FILES by repository-relative name and content (sha1 hex)."
  (secure-hash
   'sha1
   (mapconcat
    (lambda (f)
      (if (and (not (string-prefix-p "?" f)) (file-readable-p f))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally f)
            (concat (file-name-nondirectory f) ":" (secure-hash 'sha1 (current-buffer))))
        (concat (if (string-prefix-p "?" f) f (file-name-nondirectory f)) ":-")))
    files "\n")))

(defun nelisp-standalone-cachekey-allow-incomplete-p ()
  "Non-nil when NELISP_STANDALONE_UNIT_CACHE_ALLOW_INCOMPLETE=1 (explicit opt-out)."
  (equal (getenv "NELISP_STANDALONE_UNIT_CACHE_ALLOW_INCOMPLETE") "1"))

(defun nelisp-standalone-cachekey-check-files (files)
  "Signal an error when FILES (a closure result) hides an unresolved nelisp feature.
An unresolved repository feature is hashed as a constant, so editing it would
never re-key and the cache could return stale code.  Refuse instead of caching.
Emacs stdlib features that do not resolve are tolerated."
  (let ((bad (seq-filter (lambda (f) (string-prefix-p "?nelisp" f)) files)))
    (when (and bad (not (nelisp-standalone-cachekey-allow-incomplete-p)))
      (error "Unit cache key incomplete: unresolved toolchain features %S" bad))
    files))

(defun nelisp-standalone-cachekey-check-settings (settings)
  "Signal an error when the codegen SETTINGS text records an unavailable native-load."
  (when (and (string-match-p ":native-load-unavailable" settings)
             (not (nelisp-standalone-cachekey-allow-incomplete-p)))
    (error "Unit cache key incomplete: nelisp-native-load is unavailable"))
  settings)

(defun nelisp-standalone-cachekey-env-alist (&optional extra)
  "Alist of the codegen-affecting environment plus EXTRA (live settings)."
  (append (mapcar (lambda (n) (cons n (getenv n))) nelisp-standalone-cachekey-env-names)
          (list (cons "emacs-version" emacs-version))
          extra))

(defun nelisp-standalone-cachekey-settings-string (alist)
  "Stable text for ALIST (env and live settings), part of every unit key."
  (let ((print-length nil) (print-level nil) (print-circle t))
    (format "settings=%S" alist)))

;;;; Step 4: self-verifying entries.

(defconst nelisp-standalone-cachekey-entry-magic ";nelisp-unit-cache v1")

(defun nelisp-standalone-cachekey-verify-p ()
  "Non-nil unless NELISP_STANDALONE_UNIT_CACHE_VERIFY=0 (test-only escape)."
  (not (equal (getenv "NELISP_STANDALONE_UNIT_CACHE_VERIFY") "0")))

(defun nelisp-standalone-cachekey-atomic-write (file text)
  "Write TEXT (a string) to FILE through a unique temp file and a rename.
The temp name carries the pid and random bits, so concurrent writers on one
or several hosts never share it; the destination is replaced atomically."
  (let ((tmp (format "%s.%d.%06x.tmp" file (emacs-pid) (random #xffffff)))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (unwind-protect
        (progn (with-temp-file tmp (insert text))
               (rename-file tmp file t)
               (setq tmp nil))
      (when (and tmp (file-exists-p tmp)) (ignore-errors (delete-file tmp))))))

(defun nelisp-standalone-cachekey-entry-text (key payload)
  "Entry file text for KEY and PAYLOAD (a string): header line then payload."
  (format "%s key=%s sha256=%s bytes=%d\n%s"
          nelisp-standalone-cachekey-entry-magic key
          (secure-hash 'sha256 payload) (string-bytes payload) payload))

(defun nelisp-standalone-cachekey-entry-write (file key payload)
  "Atomically write the entry for KEY with PAYLOAD to FILE."
  (nelisp-standalone-cachekey-atomic-write
   file (nelisp-standalone-cachekey-entry-text key payload)))

(defun nelisp-standalone-cachekey-entry-read (file key)
  "Return (OK . PAYLOAD-or-REASON) for the entry FILE expected to hold KEY.
OK is t with the payload string when the header, key and sha256 all verify,
else nil with a reason symbol (`missing', `header', `key', `size', `sha').
A file removed between listing and reading is `missing', never an error.
With verification off (NELISP_STANDALONE_UNIT_CACHE_VERIFY=0) only the header
is split off."
  (condition-case nil
      (let ((text (with-temp-buffer
                    (set-buffer-multibyte t)
                    (let ((coding-system-for-read 'utf-8-unix))
                      (insert-file-contents file))
                    (buffer-string))))
        (if (not (string-match
                  (concat "\\`" (regexp-quote nelisp-standalone-cachekey-entry-magic)
                          " key=\\([0-9a-f]+\\) sha256=\\([0-9a-f]+\\) bytes=\\([0-9]+\\)\n")
                  text))
            (cons nil 'header)
          (let ((hkey (match-string 1 text)) (hsha (match-string 2 text))
                (hbytes (string-to-number (match-string 3 text)))
                (payload (substring text (match-end 0))))
            (cond
             ((not (nelisp-standalone-cachekey-verify-p)) (cons t payload))
             ((not (equal hkey key)) (cons nil 'key))
             ((/= hbytes (string-bytes payload)) (cons nil 'size))
             ((not (equal hsha (secure-hash 'sha256 payload))) (cons nil 'sha))
             (t (cons t payload))))))
    (file-missing (cons nil 'missing))
    (file-error (cons nil 'missing))))

(defun nelisp-standalone-cachekey-touch (file)
  "Set FILE's mtime to now so age-based collection sees the entry as in use.
Failure (read-only seed, race with a collector) is ignored."
  (ignore-errors (set-file-times file (current-time)))
  nil)

(defun nelisp-standalone-cachekey-prune-p ()
  "Non-nil when a miss may delete older entries of the same unit name.
Pruning is safe only in a lane-private directory: in a directory shared
by lanes it deletes another lane's live entry between its existence check
and its read.  A directory named by NELISP_STANDALONE_UNIT_CACHE_DIR is
treated as shared unless NELISP_STANDALONE_UNIT_CACHE_PRUNE=1; NOPRUNE=1
always disables.  Shared directories are collected by tools/dev/nl-cache-gc.sh."
  (and (not (equal (getenv "NELISP_STANDALONE_UNIT_CACHE_NOPRUNE") "1"))
       (or (equal (getenv "NELISP_STANDALONE_UNIT_CACHE_PRUNE") "1")
           (let ((d (getenv "NELISP_STANDALONE_UNIT_CACHE_DIR"))) (or (null d) (string= d ""))))))

;;;; Step 2 (design2 A): traced closure digest of the build script.
;;
;; A unit key hashes the post-producer source sexp, so build.el text is only
;; needed in the key for code that runs AFTER the key is taken: the roots
;; below and everything they reach, cut at the pre-key producers (whose
;; effect is already in the keyed source).  The closure is a static symbol
;; BFS over the top-level definitions of the script.  Anything the BFS cannot
;; follow (computed names, `eval', unlisted `getenv', ...) is an opaque form:
;; the caller then falls back to the whole-file digest ("global key").

(defconst nelisp-standalone-cachekey-closure-prefix "nelisp-standalone--")

(defconst nelisp-standalone-cachekey-closure-roots
  '("compile-to-unit" "cached-unit" "unit-cache-key" "toolchain-digest"
    "codegen-settings-string" "unit-cache-encode" "unit-cache-decode")
  "Definitions (names without the script prefix) that run at or after the key.")

;; census-instrument and abi-box-split-source are NOT cut: the trace oracle
;; (tools/dev/nl-key-trace.el) showed compile-to-unit applies both a second
;; time (3374-3376), so an edit of either changes the compiled code.
(defconst nelisp-standalone-cachekey-closure-cut
  '("unit-inline-imports"
    "xinline-prepare" "xinline-register" "unit-for" "reader-driver-unit"
    "glue-source" "applyfn-assemble" "applyfn-baked-source" "applyfn-source"
    "applyfn-split-units" "applyfn-frame-units" "reader-units" "prewarm-wait"
    "box-census-record")
  "Pre-key producers: the BFS neither enters nor hashes these.")

;; Diagnostics called inside compile-to-unit that only write census/dump files.
(defconst nelisp-standalone-cachekey-trace-diagnostics
  '("box-census-record" "box-dump-record")
  "Functions the trace oracle may see outside the closure: they record, never transform.")

(defconst nelisp-standalone-cachekey-bookkeeping-vars
  '("recompiled" "build-trace-misses" "build-trace-hits" "build-trace-unit" "prewarm-unit" "cache-dir")
  "Variables writers may set without reaching codegen (not a writer trigger).")

(defconst nelisp-standalone-cachekey-writer-heads
  '(setq setf push pop puthash cl-incf cl-decf cl-pushnew add-to-list nconc
    nreverse sort delete aset setcar setcdr remhash clrhash put let let*)
  "Forms that mutate or rebind a variable: a def outside the closure using
one of them on a closure variable joins the closure.")

(defconst nelisp-standalone-cachekey-dynamic-vars
  '(nelisp-aot-compiler--x86-private-continuations
    nelisp-aot-compiler--allow-external-user-calls
    nelisp-aot-compiler--x86-direct-only-defuns
    nelisp-aot-compiler--x86-jump-tables-enabled)
  "Variables read in the closure that producers `let'-bind around `cached-unit'.
Their VALUES (not the binding text) enter every key.  A new hit found by the
scanner without being listed here fails the ERT (and the key refuses).")

(defconst nelisp-standalone-cachekey-opaque-allow
  '(;; FUNCALL of an arithmetic operator symbol taken from a literal table.
    (nelisp-standalone--inline-constant-base-case . funcall)
    (nelisp-standalone--inline-parallel-fold-safe-p . funcall)
    ;; INTERN builds a fresh local name from the parameter and a counter: a
    ;; pure function of text already in the closure (names `x__N').
    (nelisp-standalone--inline-fresh-name . intern)
    ;; FBOUNDP of `nl-jit-call-out-2' is printed in the key (:native-jit-callout).
    (nelisp-standalone--inline-parallel-mode-p . fboundp)
    (nelisp-standalone--inline-wrapper-initializers . fboundp)
    (nelisp-standalone--unit-cache-key . fboundp)
    ;; FUNCALL of a local `cl-labels' walker.
    (nelisp-standalone--inline-wrapper-initializers . funcall)
    ;; INTERN derives `NAME_w' style names from the defun name text.
    (nelisp-standalone--abi-box-split-defun . intern)
    (nelisp-standalone--abi-box-split-source . intern)
    (nelisp-standalone--abi-definer-word . intern)
    ;; NELISP_RUNTIME_RELOAD is in `-env-names'.
    (nelisp-standalone--runtime-reload-enabled-p . getenv))
  "Alist (DEF . HEAD) of opaque sites reviewed as safe (each has a reason above).")

(defconst nelisp-standalone-cachekey-lib-callback-callers
  '(nelisp-argframe-lower nelisp-argframe-walk-lower)
  "Library entry points that funcall the build-script callbacks assigned by
top-level `setq's (argframe-inline-table, helper-defs).  Reaching one from
the closure makes the callback targets roots: that is reported as opaque, so
the build takes the global key instead of silently missing them.")

(defun nelisp-standalone-cachekey--read-script (file)
  "Return (DEFS . TOPS) of FILE: symbol -> definition form, and other top forms."
  (let ((defs (make-hash-table :test 'eq)) (tops nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((f (read (current-buffer))))
              (if (and (consp f) (memq (car f) '(defun defmacro defconst defvar defsubst))
                       (symbolp (cadr f)))
                  (puthash (cadr f) f defs)
                (push f tops))))
        (end-of-file nil)))
    (cons defs (nreverse tops))))

(defun nelisp-standalone-cachekey--form-defs (form defs acc)
  "Add to hash ACC every symbol of FORM that names a definition in DEFS."
  (cond ((symbolp form) (when (and form (gethash form defs)) (puthash form t acc)))
        ((consp form)
         (nelisp-standalone-cachekey--form-defs (car form) defs acc)
         (let ((d (cdr form)))
           (while (consp d)
             (nelisp-standalone-cachekey--form-defs (car d) defs acc)
             (setq d (cdr d)))
           (when d (nelisp-standalone-cachekey--form-defs d defs acc)))))
  acc)

(defun nelisp-standalone-cachekey--bfs (roots cut defs)
  (let ((seen (make-hash-table :test 'eq)) (q (copy-sequence roots)))
    (while q
      (let ((r (pop q)))
        (unless (or (gethash r seen) (memq r cut) (not (gethash r defs)))
          (puthash r t seen)
          (maphash (lambda (k _) (push k q))
                   (nelisp-standalone-cachekey--form-defs (gethash r defs) defs
                                                          (make-hash-table :test 'eq))))))
    seen))

(defun nelisp-standalone-cachekey--walk (form fn)
  "Call FN on every cons of FORM (depth first, proper and dotted)."
  (when (consp form)
    (funcall fn form)
    (let ((d form))
      (while (consp d)
        (nelisp-standalone-cachekey--walk (car d) fn)
        (setq d (cdr d))))))

(defun nelisp-standalone-cachekey--opaque-sites (name form)
  "List of (NAME . HEAD) for forms in FORM the static closure cannot follow."
  (let (hits)
    (nelisp-standalone-cachekey--walk
     form
     (lambda (x)
       (let ((h (car x)) (a (car-safe (cdr x))))
         (cond
          ((memq h '(intern intern-soft symbol-function symbol-value fset eval
                     indirect-function))
           (push (cons name h) hits))
          ((memq h '(load require autoload))
           (unless (and (eq h 'require) (eq (car-safe a) 'quote))
             (push (cons name h) hits)))
          ((eq h 'fboundp) (push (cons name h) hits))
          ((memq h nelisp-standalone-cachekey-lib-callback-callers) (push (cons name h) hits))
          ((eq h 'getenv)
           (unless (and (stringp a)
                        (or (member a nelisp-standalone-cachekey-env-names)
                            (string-prefix-p "NELISP_STANDALONE_" a)))
             (push (cons name 'getenv) hits)))
          ((memq h '(funcall apply))
           ;; A call through a symbol variable can reach a def the BFS never saw.
           (when (and (symbolp a) a (not (keywordp a)))
             (push (cons name h) hits)))))))
    (nreverse hits)))

(defun nelisp-standalone-cachekey--set-var-targets (x)
  "Variables a writer form X mutates or rebinds (symbols)."
  (let ((h (car x)))
    (cond ((memq h '(let let*))
           (and (listp (car-safe (cdr x)))
                (mapcar (lambda (b) (if (consp b) (car b) b)) (car-safe (cdr x)))))
          ((memq h '(setq))
           (let (r (l (cdr x))) (while (consp l) (push (car l) r) (setq l (cdr-safe (cdr l)))) r))
          ;; push/puthash/remhash/aset/add-to-list/... put the place in
          ;; different argument positions: take every symbol of the form.
          (t (let (r) (nelisp-standalone-cachekey--walk
                       x (lambda (y) (let ((l y)) (while (consp l) (when (and (symbolp (car l)) (car l)) (push (car l) r)) (setq l (cdr l))))))
                r)))))

(defun nelisp-standalone-cachekey-closure-compute (file)
  "Analyse script FILE.  Return a plist:
:defs sorted closure names, :bytes prin1 size, :digest sha1 or nil,
:opaque list of (DEF . HEAD), :writers (added defs), :tops hashed top forms."
  (let* ((rd (nelisp-standalone-cachekey--read-script file))
         (defs (car rd)) (tops (cdr rd))
         (pre nelisp-standalone-cachekey-closure-prefix)
         (sym (lambda (n) (intern (concat pre n))))
         (roots (mapcar sym nelisp-standalone-cachekey-closure-roots))
         (cut (mapcar sym nelisp-standalone-cachekey-closure-cut))
         (book (mapcar sym nelisp-standalone-cachekey-bookkeeping-vars))
         (set (nelisp-standalone-cachekey--bfs roots cut defs))
         (hashed-tops nil) (writers nil) (changed t))
    (when (zerop (hash-table-count set))
      (setq changed nil))
    ;; Fixed point: writers of closure variables and top-level forms touching
    ;; the closure join it (their own symbols are roots too).
    (while changed
      (setq changed nil)
      (maphash
       (lambda (n f)
         (unless (or (gethash n set) (memq n cut))
           (let (hit)
             (nelisp-standalone-cachekey--walk
              f (lambda (x)
                  (when (and (not hit) (memq (car x) nelisp-standalone-cachekey-writer-heads)
                             (consp (cdr x)) (listp (cdr (last x))))
                    (dolist (v (nelisp-standalone-cachekey--set-var-targets x))
                      (when (and (symbolp v) (gethash v set) (not (memq v book))
                                 (not (string-prefix-p "nelisp-standalone--build-trace-" (symbol-name v)))
                                 (memq (car (gethash v defs)) '(defvar defconst)))
                        (setq hit v))))))
             (when hit
               (push (cons n hit) writers)
               ;; The writer's own text joins; its callees do not (a callee
               ;; that mutates a closure variable is found by this same loop).
               (puthash n t set) (setq changed t)))))
       defs)
      (dolist (tp tops)
        (when (and (consp tp) (memq (car tp) '(setq when add-to-list put unless if progn))
                   (not (memq tp hashed-tops)))
          (let ((m (nelisp-standalone-cachekey--form-defs tp defs (make-hash-table :test 'eq))) touch)
            (maphash (lambda (k _) (when (gethash k set) (setq touch t))) m)
            (when touch
              (push tp hashed-tops) (setq changed t))))))
    (let* ((names (sort (let (l) (maphash (lambda (k _) (push k l)) set) l)
                        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
           (print-length nil) (print-level nil) (print-circle t) (print-escape-nonascii t)
           (bytes 0) opaque
           (parts (mapcar (lambda (n)
                            (let ((s (prin1-to-string (gethash n defs))))
                              (setq bytes (+ bytes (length s)))
                              (dolist (o (nelisp-standalone-cachekey--opaque-sites n (gethash n defs)))
                                (unless (member o nelisp-standalone-cachekey-opaque-allow)
                                  (push o opaque)))
                              (concat (symbol-name n) "=" s)))
                          names))
           (toptxt (mapcar (lambda (tp)
                             (dolist (o (nelisp-standalone-cachekey--opaque-sites 'top tp))
                               (unless (member o nelisp-standalone-cachekey-opaque-allow)
                                 (push o opaque)))
                             (prin1-to-string tp))
                           (reverse hashed-tops))))
      ;; Top-level assignment of a build-script function to a variable that
      ;; this script does not define (a library callback) is classified: a
      ;; target outside the closure is producer-only; inside it is hashed.
      (list :defs names :bytes bytes :writers (nreverse writers)
            :tops (nreverse (copy-sequence hashed-tops))
            :opaque (nreverse opaque)
            :roots-missing (seq-filter (lambda (r) (not (gethash r defs))) roots)
            :digest (and names (null opaque)
                         (secure-hash 'sha1 (mapconcat #'identity (append parts toptxt) "\n")))))))

(defun nelisp-standalone-cachekey-closure-fallback-reason (info)
  "Reason string when INFO (from `closure-compute') forces the global key, else nil."
  (cond ((equal (getenv "NELISP_STANDALONE_FULL_KEY") "1") "NELISP_STANDALONE_FULL_KEY=1")
        ((plist-get info :roots-missing)
         (format "missing root %S" (plist-get info :roots-missing)))
        ((null (plist-get info :defs)) "empty closure")
        ((plist-get info :opaque)
         (format "opaque form in %s" (car (car (plist-get info :opaque)))))
        ((null (plist-get info :digest)) "no digest")))

(defconst nelisp-standalone-cachekey-printed-vars
  '(nelisp-aot-compiler--x86-direct-only-defuns nelisp-aot-compiler--x86-jump-tables-enabled)
  "Dynamic variables the unit key already prints (see `unit-cache-key').")

(defun nelisp-standalone-cachekey-let-bound-vars (file)
  "Variables read inside the closure of FILE but `let'-bound by defs outside it.
Only names containing \"--\" (library or script variables) are reported.  Each
must be in `-dynamic-vars' or `-printed-vars', else a producer could change a
codegen input the key never sees."
  (let* ((info (nelisp-standalone-cachekey-closure-compute file))
         (rd (nelisp-standalone-cachekey--read-script file))
         (defs (car rd)) (inside (make-hash-table :test 'eq))
         (read (make-hash-table :test 'eq)) (res nil))
    (dolist (n (plist-get info :defs)) (puthash n t inside))
    (maphash (lambda (n f)
               (when (gethash n inside)
                 (let (w) (setq w (lambda (x)
                                    (cond ((symbolp x) (when x (puthash x t read)))
                                          ((consp x) (funcall w (car x))
                                           (let ((d (cdr x))) (while (consp d) (funcall w (car d)) (setq d (cdr d)))
                                                (when d (funcall w d)))))))
                      (funcall w f))))
             defs)
    (maphash (lambda (n f)
               (unless (gethash n inside)
                 (nelisp-standalone-cachekey--walk
                  f (lambda (x)
                      (when (and (memq (car x) '(let let*)) (listp (car-safe (cdr x))))
                        (dolist (b (cadr x))
                          (let ((v (if (consp b) (car b) b)))
                            (when (and (symbolp v) (gethash v read) (not (gethash v inside))
                                       (string-match-p "--" (symbol-name v)))
                              (cl-pushnew v res)))))))))
             defs)
    (sort res (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun nelisp-standalone-cachekey-dynamic-values (vars)
  "Stable text of the current values of VARS (symbols) for the key."
  (let ((print-length nil) (print-level nil) (print-circle t))
    (format "dynamic=%S"
            (mapcar (lambda (v) (cons v (if (boundp v) (symbol-value v) :unbound))) vars))))

(provide 'nelisp-standalone-cachekey)
;;; nelisp-standalone-cachekey.el ends here
