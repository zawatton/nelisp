;;; compile-elisp-objects.el --- Doc 99 §99.B build orchestrator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 99 §99.B build-time orchestrator.  Invoked by
;; `build-tool/build.rs' as
;;
;;   emacs --batch -Q -L lisp -l scripts/compile-elisp-objects.el \
;;         -f compile-elisp-objects-emit-all
;;
;; The function walks a hard-coded manifest of (elisp-source-feature .
;; output-object) pairs, runs `nelisp-phase47-compile-to-object' on the
;; canonical source from each feature, and writes the resulting ET_REL
;; .o files under `target/elisp-objects/'.
;;
;; The manifest is intentionally a defconst (not TOML) — §99.B is a
;; one-entry spike and parsing TOML inside `emacs --batch' would just
;; add a build dep on `toml.el'.  When the manifest grows past ~5
;; entries we can migrate.

;;; Code:

(require 'cl-lib)

;; Hard-coded `lisp/' resolution: this script is run from the repo
;; root by `cargo build', so relative paths resolve from there.
(let* ((this (or load-file-name buffer-file-name))
       (script-dir (and this (file-name-directory this)))
       (repo-root (and script-dir
                       (file-name-as-directory
                        (expand-file-name ".." script-dir))))
       (lisp-dir (and repo-root (expand-file-name "lisp" repo-root))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-phase47-compiler)

(defconst compile-elisp-objects-manifest
  '((nelisp-cc-spike-noop
     :source-var nelisp-cc-spike-noop--source
     :output "nelisp_spike_noop.o")
    (nelisp-cc-fact-i64
     :source-var nelisp-cc-fact-i64--source
     :output "nelisp_fact_i64.o")
    ;; Doc 100 §100.C — first real bi_* swap: `(truncate INT)' Int arm.
    (nelisp-cc-truncate-int
     :source-var nelisp-cc-truncate-int--source
     :output "nelisp_truncate_int.o")
    ;; Doc 100 Tier A wear-test #1: `(nl-int-neg N)' = `(- N)'.
    (nelisp-cc-int-neg
     :source-var nelisp-cc-int-neg--source
     :output "nelisp_int_neg.o"))
  "Build-time manifest of elisp features → ET_REL output files.
Each entry is `(FEATURE :source-var SYM :output BASENAME)' where
FEATURE is the feature to `require', SYM is the defconst holding
the canonical source form, and BASENAME is the .o filename written
to OUT-DIR.  When the spike grows past one entry, switch to a TOML
manifest under `build-tool/elisp-objects.toml'.")

(defun compile-elisp-objects--out-dir ()
  "Return the absolute path of `target/elisp-objects/' under the repo root.
Honors the `NELISP_ELISP_OBJECTS_DIR' environment variable when set
(= `build.rs' passes `$OUT_DIR' / elisp-objects so cargo doesn't see
stale artifacts across feature combinations)."
  (or (getenv "NELISP_ELISP_OBJECTS_DIR")
      (let* ((this (or load-file-name buffer-file-name))
             (script-dir (and this (file-name-directory this)))
             (repo-root (and script-dir
                             (file-name-as-directory
                              (expand-file-name ".." script-dir)))))
        (expand-file-name "target/elisp-objects" repo-root))))

(defun compile-elisp-objects-emit-all ()
  "Compile every manifest entry to its output `.o' file.
Returns the list of absolute paths written.  Used by
`build-tool/build.rs' as the `-f' callback (= no args, exits the
batch process via the outer `emacs --batch' driver)."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (written nil))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (dolist (entry compile-elisp-objects-manifest)
      (let* ((feature (car entry))
             (props   (cdr entry))
             (src-var (plist-get props :source-var))
             (output  (plist-get props :output))
             (out-path (expand-file-name output out-dir)))
        (require feature)
        (unless (boundp src-var)
          (error "compile-elisp-objects: %S has no :source-var %S"
                 feature src-var))
        (let ((sexp (symbol-value src-var)))
          (message "[compile-elisp-objects] %s -> %s"
                   feature out-path)
          (nelisp-phase47-compile-to-object sexp out-path)
          (push out-path written))))
    (nreverse written)))

(provide 'compile-elisp-objects)

;;; compile-elisp-objects.el ends here
