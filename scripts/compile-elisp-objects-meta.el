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
;; predicate when running under `emacs --batch'.  Wave A25.6 swaps
;; the shim's standalone-NeLisp branch for a direct `nl-jit-call-out-
;; 2' dispatch into the compiled `.o' kernel (host Emacs path
;; unchanged — falls back to `file-attribute-modification-time' for
;; ERT / byte-identity reference runs).
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
;;
;; Wave A25.3 — wrap in `featurep' guard so standalone NeLisp callers
;; (= `nelisp--cli-meta-driver-main') can pre-load `compile-elisp-
;; objects' before requiring this file.  Without the guard the nested
;; `(load (expand-file-name ...))' path is sensitive to standalone
;; NeLisp's `default-directory' / `expand-file-name' interaction
;; (when the outer load lives in `scripts/' the expansion yields the
;; doubled path `scripts/scripts/compile-elisp-objects.el', which
;; `locate-library' can't resolve).  Host Emacs still hits the load
;; branch since its `compile-elisp-objects' is not pre-required there.
(unless (featurep 'compile-elisp-objects)
  (load (expand-file-name "compile-elisp-objects.el"
                          (file-name-directory
                           (or load-file-name buffer-file-name)))
        nil t))

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
  "Decide whether OUT-PATH must be rebuilt against SRC-PATH.
Returns 1 when out missing, out_mtime < src_mtime, or src missing.
Returns 0 when up-to-date.

Wave A25.6 — extern-call swap.  When the standalone NeLisp
interpreter's `nl-jit-call-out-2' bridge is bound (= standalone
runtime with the A25.2 `nelisp_meta_should_rebuild' kernel linked
into the binary's `+whole-archive' Phase 47 helper set), this
function dispatches directly into the compiled `.o' kernel:

  (nl-jit-call-out-2 \"nelisp_meta_should_rebuild\" SRC OUT)

The bridge marshals SRC / OUT as `*const Sexp', allocates a Rust-
local `Sexp::Nil' result slot, casts `dlsym(\"nelisp_meta_should_
rebuild\")' to `extern \"C\" fn(*const Sexp, *const Sexp, *mut Sexp)
-> i64', invokes it (= the Phase 47 kernel writes `Sexp::Int(0|1)'
into the result slot, returns TRAMPOLINE_OK), and surfaces the slot
contents as the call's `Sexp::Int' return.  `eq'-comparing the
return against the integer 0 yields the 0/1 decision the caller
needs.

On host Emacs (= no `nl-jit-call-out-2'), the function falls back
to the pure-elisp predicate (`file-attribute-modification-time' +
`time-less-p') so the meta-driver stays callable from `emacs
--batch' for byte-identity reference runs and ERT smoke.

Both paths share the same decision table — out missing → 1; src
missing → 1; out_mtime < src_mtime → 1; else 0 — so the standalone
extern-call swap is byte-identity-safe by construction."
  (cond
   ((not (file-exists-p out-path)) 1)
   ((not (and src-path (file-exists-p src-path))) 1)
   ((fboundp 'nl-jit-call-out-2)
    ;; Standalone NeLisp path — dispatch into the Phase 47 .o.  The
    ;; bridge returns `Sexp::Int(0|1)' via the result slot; comparing
    ;; against the integer 0 yields the 0/1 decision shape this
    ;; predicate's callers expect.  `condition-case' is intentional:
    ;; on dlsym miss or arity mismatch the bridge signals and we want
    ;; to surface a clear rebuild=1 (= safe over-build) rather than
    ;; silently skip up-to-date entries.
    (condition-case _err
        (let ((decision (nl-jit-call-out-2 "nelisp_meta_should_rebuild"
                                           src-path out-path)))
          (if (eq decision 0) 0 1))
      (error 1)))
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

(defun compile-elisp-objects-meta--collect-path-vectors (manifest out-dir arch)
  "Build parallel `srcs' / `outs' vectors + an `entries' vector for MANIFEST.
Used by the Wave A26 walker dispatch (= `nelisp_meta_walk') which
takes two parallel `Sexp::Vec' of `Sexp::Str' path arguments instead
of a single record vector.

Returns a plist `(:srcs SRCS-VEC :outs OUTS-VEC :entries ENTRIES-VEC
:dirty-arch-mask MASK)' where:

  SRCS-VEC       - vector of source `.el' absolute paths (strings).
  OUTS-VEC       - vector of output `.o' absolute paths (strings).
  ENTRIES-VEC    - vector of the original manifest entries (= the
                   4-slot records the walker dispatches against
                   after the bitmask comes back).
  DIRTY-ARCH-MASK - i64 bitmask; bit i set iff entry i should be
                   skipped due to `:requires-arch' mismatch.  The
                   walker still processes those entries (their
                   bitmask bit is unreliable) but the elisp driver
                   ignores both the walker-bit AND the entry on
                   the emit pass when the arch-skip bit is set.

ENTRIES-VEC is parallel to SRCS-VEC / OUTS-VEC (= same length, same
order), so bit `i' of the walker's returned bitmask maps to
ENTRIES-VEC[i] / SRCS-VEC[i] / OUTS-VEC[i] one-to-one."
  (let ((n (length manifest))
        (srcs (make-vector (length manifest) ""))
        (outs (make-vector (length manifest) ""))
        (entries (make-vector (length manifest) nil))
        (arch-skip 0)
        (i 0))
    (while (< i n)
      (let* ((entry (aref manifest i))
             (feature       (aref entry 0))
             (output        (aref entry 2))
             (requires-arch (aref entry 3))
             (out-path      (expand-file-name output out-dir))
             (src-path      (compile-elisp-objects--source-file feature)))
        (aset srcs i (or src-path ""))
        (aset outs i out-path)
        (aset entries i entry)
        (unless (compile-elisp-objects-meta--arch-ok-p requires-arch arch)
          (setq arch-skip (logior arch-skip (ash 1 i))))
        (setq i (1+ i))))
    (list :srcs srcs :outs outs :entries entries
          :dirty-arch-mask arch-skip)))

(defun compile-elisp-objects-meta--missing-file-mask (srcs outs)
  "Return a bitmask of entries with a missing src OR missing out file.
Bit i is set iff `srcs[i]' is empty / does not exist, OR `outs[i]'
does not exist.  Used to pre-flight the walker dispatch so the
Phase 47 `nelisp_meta_should_rebuild' kernel only sees entries
where both files are stat-able (= same pre-flight invariant the
pre-A26 `compile-elisp-objects-meta--should-rebuild' shim relied
on)."
  (let ((n (length srcs))
        (mask 0)
        (i 0))
    (while (< i n)
      (let ((src (aref srcs i))
            (out (aref outs i)))
        (when (or (null src)
                  (not (stringp src))
                  (string-empty-p src)
                  (not (file-exists-p src))
                  (not (file-exists-p out)))
          (setq mask (logior mask (ash 1 i)))))
      (setq i (1+ i)))
    mask))

(defun compile-elisp-objects-meta--compute-bitmask (srcs outs)
  "Return an i64 bitmask of per-entry rebuild decisions.
Bit i is 1 iff `nelisp_meta_should_rebuild' decided entry i needs
rebuilding (= source newer than output, or output missing, or src
missing).

Wave A26 — when the standalone NeLisp interpreter's
`nl-jit-call-out-2' bridge is bound (= standalone runtime with the
A26 `nelisp_meta_walk' kernel linked into `+whole-archive'), this
function makes a SINGLE bridge call into the Phase 47 walker:

  (nl-jit-call-out-2 \"nelisp_meta_walk\" SRCS OUTS)

The walker iterates internally via Phase 47-native tail recursion,
calling `nelisp_meta_should_rebuild' for each (src, out) pair, and
packs the 0/1 decisions into a single `Sexp::Int(bitmask)' return.
This collapses N elisp -> native bridge calls into 1 (= the A26
walker is the iteration kernel itself, not a per-entry shim).

Missing-file pre-flight: the A25.2 `nelisp_meta_should_rebuild'
kernel's `extern-call stat' currently masks the stat-failure rc
indistinguishably from the rc-success branch (= the kernel's
`(if (< rc 0) -1 ...)' check is x86_64-correct but the elisp-side
extern-call return-shape returns 0 for the rc<0 branch as well, so
the kernel's missing-file decision-bit is unreliable).  We work
around this by pre-flighting missing files on the elisp side via
`compile-elisp-objects-meta--missing-file-mask' and OR-ing the
result into the walker's bitmask — same byte-identity contract,
same safety, just without depending on the kernel's missing-file
branch.  When the kernel-side stat-failure path is fixed in a
later wave the pre-flight can be dropped.

On host Emacs (= no `nl-jit-call-out-2'), the function falls back
to the per-entry `compile-elisp-objects-meta--should-rebuild' shim
called in an elisp loop.  The bitmask is built one bit at a time
so the byte-identity reference path stays untouched."
  (let ((missing-mask
         (compile-elisp-objects-meta--missing-file-mask srcs outs)))
    (cond
     ((fboundp 'nl-jit-call-out-2)
      ;; Standalone NeLisp path — single bridge call into the Phase 47
      ;; walker.  The bridge returns `Sexp::Int(bitmask)' via the result
      ;; slot; on dlsym miss or arity mismatch we fall back to the
      ;; per-entry safe-over-build (= mark every entry dirty).  OR with
      ;; the missing-file pre-flight so entries with missing src or out
      ;; are flagged dirty regardless of what the walker decided (= the
      ;; kernel's `extern-call stat' rc<0 branch is unreliable, see
      ;; docstring).
      (logior missing-mask
              (condition-case _err
                  (let ((result (nl-jit-call-out-2 "nelisp_meta_walk"
                                                   srcs outs)))
                    (if (integerp result) result
                      ;; Walker miss — mark every entry dirty so the
                      ;; emit pass still produces correct output (=
                      ;; safe over-build).
                      (1- (ash 1 (length srcs)))))
                (error
                 ;; Walker dispatch failed — mark every entry dirty.
                 (1- (ash 1 (length srcs)))))))
     (t
      ;; Host Emacs reference path — per-entry shim in an elisp loop.
      ;; Byte-identity-safe: the per-entry shim is the same code the
      ;; pre-A26 walker called, just lifted out of the iteration body.
      (let ((n (length srcs))
            (mask 0)
            (i 0))
        (while (< i n)
          (let* ((src (aref srcs i))
                 (out (aref outs i))
                 (decision (compile-elisp-objects-meta--should-rebuild
                            (and (not (string-empty-p src)) src)
                            out)))
            (when (not (zerop decision))
              (setq mask (logior mask (ash 1 i)))))
          (setq i (1+ i)))
        mask)))))

(defun compile-elisp-objects-meta--walk (manifest _idx _end out-dir arch format)
  "Wave A26 manifest walker.
Drives the per-entry up-to-date decision through the Phase 47-
compiled `nelisp_meta_walk' kernel (= a single bridge call returns
an i64 bitmask of dirty-bit decisions) when running under
standalone NeLisp, or falls back to the host-Emacs per-entry shim
for ERT / batch reference runs.

The elisp driver then iterates the manifest once more to dispatch
the actual `nelisp-phase47-compile-to-object' emit step for every
entry whose bit is set in the bitmask, skipping arch-mismatched
entries by consulting the `:dirty-arch-mask' value the path-vector
builder produced alongside the parallel `srcs' / `outs' vectors.

The IDX / END args are kept for ABI compatibility with the legacy
caller in `compile-elisp-objects-meta-emit-subset' (= which always
passed [0, N)); the walker now consumes the full MANIFEST.

Returns the same accumulator shape as the pre-A26 walker — a list
of absolute output paths written or kept up-to-date — so
byte-identity probes / ERT references continue to pass through
unchanged."
  (let* ((paths (compile-elisp-objects-meta--collect-path-vectors
                 manifest out-dir arch))
         (srcs (plist-get paths :srcs))
         (outs (plist-get paths :outs))
         (entries (plist-get paths :entries))
         (arch-skip (plist-get paths :dirty-arch-mask))
         (n (length entries))
         (bitmask (compile-elisp-objects-meta--compute-bitmask srcs outs))
         (acc nil)
         (i 0))
    (while (< i n)
      (let* ((entry (aref entries i))
             (feature (aref entry 0))
             (sexp    (aref entry 1))
             (out-path (aref outs i))
             (arch-skip-p (not (zerop (logand arch-skip (ash 1 i))))))
        (cond
         (arch-skip-p
          nil)
         ((zerop (logand bitmask (ash 1 i)))
          ;; Up-to-date — return existing path for caller's accumulator.
          (push out-path acc))
         (t
          (message "[compile-elisp-objects-meta] %s -> %s"
                   feature out-path)
          (nelisp-phase47-compile-to-object sexp out-path
                                            :arch arch :format format)
          (push out-path acc))))
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
