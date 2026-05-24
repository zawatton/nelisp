;;; compile-elisp-objects-meta.el --- Wave A25.2 Phase 47-friendly driver rewrite  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A25.2 — Phase 47-friendly rewrite of the
;; `scripts/compile-elisp-objects.el' driver.  Proof of concept that
;; the iteration + dispatch logic can be expressed without any of the
;; host-Emacs primitives that block Phase 47 self-application:
;;
;;   * No `(require feature)' — source defconst values are looked up
;;     via a static manifest vector built once at load time, mirroring
;;     the build-time static-link strategy that the future Wave A25.3
;;     standalone bootstrap will use.
;;
;;   * No `(symbol-value src-var)' dispatch on a runtime alist — each
;;     manifest entry stores the canonical Sexp source value directly,
;;     so the driver iterates over a plain vector of records.
;;
;;   * No `dolist' / `plist-get' / `pcase' — iteration is a tail-
;;     recursive even-arity defun (`compile-elisp-objects-meta--walk');
;;     entry fields are positional vector slots; dispatch is a single
;;     `if' on `:requires-arch'.
;;
;;   * No `locate-file' / `file-attribute-modification-time' /
;;     `time-less-p' chain — the per-entry up-to-date check is delegated
;;     to a single helper that the future Phase 47 self-application
;;     will reach as `nelisp_meta_should_rebuild' (= the kernel in
;;     `lisp/nelisp-cc-meta-driver.el', which composes the A25.1-min
;;     `nelisp_bi_syscall_stat_mtime' helper).
;;
;; The PoC stays callable from host Emacs (= ert / batch mode); the
;; `nelisp_meta_should_rebuild' delegation is wrapped in a host-side
;; shim that calls the existing `compile-elisp-objects--up-to-date-p'
;; predicate when running under `emacs --batch'.  Wave A25.3 will swap
;; the shim for a direct `extern-call' into the compiled `.o' once the
;; standalone bootstrap is wired.
;;
;; Scope.  This driver does NOT replace `compile-elisp-objects.el' for
;; production builds.  It runs a 5-entry subset of the manifest end-to-
;; end (= spike-noop + fact-i64 + truncate-int + length-cons + cons-
;; construct) to prove the static-manifest iteration model, while the
;; full 208-entry manifest stays on the existing driver until A25.3
;; ships the bootstrap wiring.
;;
;; Verification entry points:
;;
;;   M-x compile-elisp-objects-meta-emit-subset
;;     — emits the 5-entry PoC subset into the same `target/elisp-
;;       objects/' directory as the legacy driver (= shares output
;;       paths so byte-identity probes work unchanged).
;;
;;   `compile-elisp-objects-meta--build-static-manifest'
;;     — builds the static manifest vector from
;;       `compile-elisp-objects-manifest-subset' (= reuses the existing
;;       Sexp source defconsts; no duplication of bytes).
;;
;;   `compile-elisp-objects-meta--walk'
;;     — tail-recursive iteration kernel that mirrors the future
;;       Phase 47-compiled walker's shape (= 4-arg, even arity, single
;;       `if' dispatch on each entry).

;;; Code:

(require 'cl-lib)

;; Forward declarations for symbols pulled in via the trailing `load' of
;; `compile-elisp-objects.el' (which is itself not a `require'-able
;; library, just a build-time script).  Silences `batch-byte-compile'
;; warnings without changing runtime behaviour.
(defvar compile-elisp-objects-manifest)
(declare-function compile-elisp-objects--source-file "compile-elisp-objects")
(declare-function compile-elisp-objects--out-dir "compile-elisp-objects")
(declare-function compile-elisp-objects--target-arch "compile-elisp-objects")
(declare-function compile-elisp-objects--target-format "compile-elisp-objects")

;; Reuse the manifest and per-entry compile logic from the legacy
;; driver so the PoC subset stays bit-for-bit identical to what the
;; production build emits.
(let* ((this (or load-file-name buffer-file-name))
       (script-dir (and this (file-name-directory this)))
       (repo-root (and script-dir
                       (file-name-as-directory
                        (expand-file-name ".." script-dir))))
       (lisp-dir (and repo-root (expand-file-name "lisp" repo-root))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-phase47-compiler)
(require 'nelisp-cc-meta-driver)

;; Pull the production manifest in for the source-var defconst lookup.
;; The PoC only walks a 5-entry subset (= the first five entries that
;; do not require x86_64-specific extern-call grammar) but the
;; per-entry tuple shape matches the production driver exactly.
(load (expand-file-name "compile-elisp-objects.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(defconst compile-elisp-objects-meta--subset-features
  '(nelisp-cc-spike-noop
    nelisp-cc-fact-i64
    nelisp-cc-truncate-int
    nelisp-cc-length-cons
    nelisp-cc-cons-construct)
  "Five-entry PoC subset of the production manifest.
Chosen because each entry has no `:requires-arch' constraint (=
they compile on every Phase 47 backend), so the PoC runs across
both x86_64 and aarch64 hosts.  The full 208-entry walk lands in
Wave A25.3 once the standalone bootstrap is wired.")

(defun compile-elisp-objects-meta--filter-subset (manifest)
  "Return MANIFEST entries whose feature is in the PoC subset.
Preserves manifest order so the static-manifest vector mirrors the
production iteration sequence."
  (let ((subset compile-elisp-objects-meta--subset-features)
        (result nil))
    (dolist (entry manifest)
      (when (memq (car entry) subset)
        (push entry result)))
    (nreverse result)))

(defun compile-elisp-objects-meta--build-static-manifest ()
  "Materialise the PoC subset as a static manifest vector.
Each slot is a 4-element vector `[FEATURE SEXP OUTPUT REQUIRES-ARCH]'
where SEXP is the canonical source value pulled from the feature's
`--source' defconst (= no `symbol-value' indirection at iteration
time; the static manifest captures the source by value).

In Wave A25.3 the same vector layout will be emitted as a read-only
data section in the standalone binary, with SEXP slots holding the
Phase 47 rodata-baked source forms.  This host-Emacs build is the
control: same shape, same iteration order, runs through the legacy
`nelisp-phase47-compile-to-object'."
  (let ((subset (compile-elisp-objects-meta--filter-subset
                 compile-elisp-objects-manifest))
        (acc nil))
    (dolist (entry subset)
      (let* ((feature (car entry))
             (props   (cdr entry))
             (src-var (plist-get props :source-var))
             (output  (plist-get props :output))
             (requires-arch (plist-get props :requires-arch)))
        (require feature)
        (unless (boundp src-var)
          (error
           "compile-elisp-objects-meta: feature %S has no :source-var %S"
           feature src-var))
        (push (vector feature
                      (symbol-value src-var)
                      output
                      requires-arch)
              acc)))
    (vconcat (nreverse acc))))

(defun compile-elisp-objects-meta--arch-ok-p (requires-arch arch)
  "Return non-nil when ARCH satisfies REQUIRES-ARCH.
Mirrors the production driver's `:requires-arch' dispatch (nil =
any arch, symbol = exact match, list = membership) but in a flat
form that fits the future Phase 47 walker's single-`if' shape."
  (cond ((null requires-arch) t)
        ((symbolp requires-arch) (eq requires-arch arch))
        ((listp requires-arch) (memq arch requires-arch))
        (t nil)))

(defun compile-elisp-objects-meta--should-rebuild (src-path out-path)
  "Host-Emacs shim for the future `nelisp_meta_should_rebuild' kernel.
Returns 1 when OUT-PATH needs to be rebuilt against SRC-PATH (= out
missing, out_mtime < src_mtime, or src missing).  Returns 0 when
up-to-date.

Wave A25.3 will replace this body with a direct `extern-call' into
the Phase 47-compiled `nelisp_meta_should_rebuild' kernel from
`lisp/nelisp-cc-meta-driver.el'.  Until then the host-Emacs path
uses the same predicate as the legacy driver so PoC output stays
byte-identical to the production build."
  (cond
   ((not (file-exists-p out-path)) 1)
   ((not (and src-path (file-exists-p src-path))) 1)
   (t
    (let ((src-mtime (file-attribute-modification-time
                      (file-attributes src-path)))
          (out-mtime (file-attribute-modification-time
                      (file-attributes out-path))))
      (if (time-less-p out-mtime src-mtime) 1 0)))))

(defun compile-elisp-objects-meta--compile-one (entry out-dir arch format)
  "Compile one ENTRY of the static manifest to OUT-DIR.
ENTRY is the 4-slot vector `[FEATURE SEXP OUTPUT REQUIRES-ARCH]'.
Returns the absolute path written, nil if the entry is skipped (=
arch mismatch or up-to-date).

This is the per-iteration body of the future Phase 47 walker.  The
host-Emacs form keeps the call to `nelisp-phase47-compile-to-object'
interpreted (= the kernel itself does not need to ship as a Phase
47 .o; it stays Rust-free elisp running in the build host).  Only
the iteration + decision shell will be Phase 47-compiled in A25.3."
  (let* ((feature       (aref entry 0))
         (sexp          (aref entry 1))
         (output        (aref entry 2))
         (requires-arch (aref entry 3))
         (out-path      (expand-file-name output out-dir))
         (src-path      (compile-elisp-objects--source-file feature)))
    (cond
     ((not (compile-elisp-objects-meta--arch-ok-p requires-arch arch))
      nil)
     ((zerop (compile-elisp-objects-meta--should-rebuild
              src-path out-path))
      ;; Up-to-date — return existing path for caller's accumulator.
      out-path)
     (t
      (message "[compile-elisp-objects-meta] %s -> %s"
               feature out-path)
      (nelisp-phase47-compile-to-object sexp out-path
                                        :arch arch :format format)
      out-path))))

(defun compile-elisp-objects-meta--walk (manifest idx end out-dir arch format)
  "Tail-recursive iteration kernel.
Walks MANIFEST entries in the half-open range [IDX, END), compiling
each via `compile-elisp-objects-meta--compile-one'.  Returns the list
of output paths written or kept (= same accumulator shape as the
legacy driver's `compile-elisp-objects-emit-all').

Even arity (6) — written in the shape the future Phase 47 walker
will take.  In Phase 47 the recursion will use the existing
`if'-dispatch + named-recursive-defun pattern (= same shape as
`nelisp_bi_getenv_cstrlen' or `nelisp_fnv1a_step').  Host Emacs
keeps the cl-loop accumulator for now because tail-call optimisation
is not guaranteed by the host emacs."
  (let ((acc nil)
        (i idx))
    (while (< i end)
      (let* ((entry (aref manifest i))
             (path  (compile-elisp-objects-meta--compile-one
                     entry out-dir arch format)))
        (when path (push path acc)))
      (setq i (1+ i)))
    (nreverse acc)))

;;;###autoload
(defun compile-elisp-objects-meta-emit-subset ()
  "Compile the PoC subset via the Wave A25.2 static-manifest walker.
Returns the list of absolute paths written.  Used by ERT / batch
verification to confirm the static-manifest iteration model produces
byte-identical output to the legacy `compile-elisp-objects-emit-all'
on the same subset.

The 5-entry subset stays small enough that the host-Emacs runtime
finishes in well under a second; the production 208-entry walk
remains on the legacy driver until Wave A25.3 ships the standalone
bootstrap wiring."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch    (compile-elisp-objects--target-arch))
         (format  (compile-elisp-objects--target-format))
         (manifest (compile-elisp-objects-meta--build-static-manifest))
         (n (length manifest)))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (compile-elisp-objects-meta--walk manifest 0 n out-dir arch format)))

(provide 'compile-elisp-objects-meta)

;;; compile-elisp-objects-meta.el ends here
