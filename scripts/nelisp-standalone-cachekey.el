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
    "NELISP_READER_DYNAMIC" "NELISP_REPR_CENSUS")
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

(provide 'nelisp-standalone-cachekey)
;;; nelisp-standalone-cachekey.el ends here
