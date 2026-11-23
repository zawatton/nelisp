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

(defun nelisp-standalone-cachekey-env-alist (&optional extra)
  "Alist of the codegen-affecting environment plus EXTRA (live settings)."
  (append (mapcar (lambda (n) (cons n (getenv n))) nelisp-standalone-cachekey-env-names)
          (list (cons "emacs-version" emacs-version))
          extra))

(defun nelisp-standalone-cachekey-settings-string (alist)
  "Stable text for ALIST (env and live settings), part of every unit key."
  (let ((print-length nil) (print-level nil) (print-circle t))
    (format "settings=%S" alist)))

(provide 'nelisp-standalone-cachekey)
;;; nelisp-standalone-cachekey.el ends here
