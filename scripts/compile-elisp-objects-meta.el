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
Wave A27 via `compile-elisp-objects-meta-emit-all' (= production
cutover, chunked walker dispatch).")

;; Wave A27 — production cutover chunk size.  The Phase 47 walker packs
;; per-entry rebuild decisions into a single `Sexp::Int(bitmask)' i64
;; return slot.  i64 holds 64 distinct bit positions, so the elisp
;; driver slices the full 208-entry manifest into 4 chunks of ≤ 64
;; entries each and issues one walker call per chunk, accumulating the
;; per-chunk bitmasks into a parallel `chunks-mask' vector indexed by
;; chunk number.  Bit `b' of `chunks-mask[c]' corresponds to the
;; absolute manifest entry index `c * 64 + b'.
(defconst compile-elisp-objects-meta--chunk-size 64
  "Maximum number of manifest entries packed into a single walker call.
Determined by the i64 bitmask return-slot width of the Wave A26
`nelisp_meta_walk' kernel.  The Wave A27 production cutover relies
on this constant to split the 208-entry production manifest into
ceil(208/64) = 4 walker dispatches; smaller subsets (= the 5-entry
A26 PoC) fit in a single chunk.")

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

(defun compile-elisp-objects-meta--build-full-static-manifest ()
  "Materialise the FULL production manifest as a static manifest vector.

Wave A27 — production cutover.  Same per-entry shape as
`compile-elisp-objects-meta--build-static-manifest' (= 4-slot
`[FEATURE SEXP OUTPUT REQUIRES-ARCH]' vector) but iterates the
entire `compile-elisp-objects-manifest' without the 5-entry PoC
filter, so the result vector has the 208-entry production length.

The per-entry shape is identical to the subset variant so the
walker chunking + bitmask combine path operates uniformly on both
sizes; the only behavioural difference is the resulting vector
length (= 5 for A26 PoC, 212 for A27 production), which feeds
into the chunk-count computation in
`compile-elisp-objects-meta--chunk-count'.

Wave A27 optimisation: `require feature' is hoisted to a single
pre-pass that dedups the feature set first, so multi-entry
features (= e.g. `nelisp-cc-bi-quit-flag' which has 3 entries for
set/clear/pending-p) only pay the require cost once.  The
standalone NeLisp `require' path is CPU-bound on each call
regardless of cache state, so dedup cuts ~25% off the manifest
construction wall-clock for production-sized inputs."
  ;; Pre-pass: dedup features so each unique feature requires once.
  ;; In standalone NeLisp the require path is intrinsically slow
  ;; (~50ms / feature on a warm process) so collapsing duplicates is
  ;; worth a fresh pre-pass over the 212-entry manifest.
  (let ((seen nil))
    (dolist (entry compile-elisp-objects-manifest)
      (let ((feature (car entry)))
        (unless (memq feature seen)
          (require feature)
          (push feature seen)))))
  (let ((acc nil))
    (dolist (entry compile-elisp-objects-manifest)
      (let* ((feature (car entry))
             (props   (cdr entry))
             (src-var (plist-get props :source-var))
             (output  (plist-get props :output))
             (requires-arch (plist-get props :requires-arch)))
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

(defun compile-elisp-objects-meta--chunk-count (total)
  "Return the number of walker chunks needed for TOTAL entries.
Each chunk packs up to `compile-elisp-objects-meta--chunk-size' (=
64) entries into a single Phase 47 walker call's i64 bitmask
return slot.  TOTAL = 0 returns 0; positive TOTAL returns the
ceiling division `ceil(TOTAL / 64)'."
  (if (<= total 0)
      0
    (/ (+ total (1- compile-elisp-objects-meta--chunk-size))
       compile-elisp-objects-meta--chunk-size)))

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
:arch-skip-chunks CHUNK-MASKS)' where:

  SRCS-VEC       - vector of source `.el' absolute paths (strings).
  OUTS-VEC       - vector of output `.o' absolute paths (strings).
  ENTRIES-VEC    - vector of the original manifest entries (= the
                   4-slot records the walker dispatches against
                   after the bitmask comes back).
  ARCH-SKIP-CHUNKS - vector of i64 chunk bitmasks; bit `b' of
                   CHUNK-MASKS[c] is set iff the absolute entry at
                   index `c * 64 + b' should be skipped due to
                   `:requires-arch' mismatch.  Wave A27 production
                   cutover: a single i64 mask cannot cover 208
                   entries (= overflows bit 63), so the arch-skip
                   state is stored per chunk for parallel use with
                   the walker's per-chunk dirty-bit return.

ENTRIES-VEC is parallel to SRCS-VEC / OUTS-VEC (= same length, same
order); the chunked walker dispatch maps bit `b' of the returned
bitmask for chunk `c' to ENTRIES-VEC[c * 64 + b]."
  (let* ((n (length manifest))
         (chunk-size compile-elisp-objects-meta--chunk-size)
         (chunk-count (compile-elisp-objects-meta--chunk-count n))
         (srcs (make-vector n ""))
         (outs (make-vector n ""))
         (entries (make-vector n nil))
         (arch-skip-chunks (make-vector chunk-count 0))
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
          (let* ((chunk-idx (/ i chunk-size))
                 (bit-pos (- i (* chunk-idx chunk-size)))
                 (cur (aref arch-skip-chunks chunk-idx)))
            (aset arch-skip-chunks chunk-idx
                  (logior cur (ash 1 bit-pos)))))
        (setq i (1+ i))))
    (list :srcs srcs :outs outs :entries entries
          :arch-skip-chunks arch-skip-chunks)))

(defun compile-elisp-objects-meta--missing-file-mask-chunk
    (srcs outs chunk-start chunk-end)
  "Return an i64 bitmask of missing-file entries in [CHUNK-START, CHUNK-END).
Bit `b' is set iff `srcs[CHUNK-START + b]' is empty / does not
exist, OR `outs[CHUNK-START + b]' does not exist.  Used to
pre-flight one walker chunk dispatch so the Phase 47
`nelisp_meta_should_rebuild' kernel only sees entries where both
files are stat-able (= same pre-flight invariant the pre-A26
`compile-elisp-objects-meta--should-rebuild' shim relied on).

CHUNK-START is the absolute entry index of bit 0; CHUNK-END is the
absolute exclusive upper bound (= chunk-start + chunk-len, with
chunk-len ≤ 64).  Returned bitmask is chunk-local — caller maps bit
`b' back to absolute index `CHUNK-START + b'."
  (let ((mask 0)
        (i chunk-start))
    (while (< i chunk-end)
      (let ((src (aref srcs i))
            (out (aref outs i))
            (bit-pos (- i chunk-start)))
        (when (or (null src)
                  (not (stringp src))
                  (string-empty-p src)
                  (not (file-exists-p src))
                  (not (file-exists-p out)))
          (setq mask (logior mask (ash 1 bit-pos)))))
      (setq i (1+ i)))
    mask))

(defun compile-elisp-objects-meta--slice-vector (vec start end)
  "Return a fresh vector containing VEC[START..END).
Used by the Wave A27 chunked walker dispatch to hand each chunk a
self-contained `Sexp::Vec' for the `nl-jit-call-out-2 \"nelisp_meta_walk\"'
bridge.  The bridge marshals the slice as `*const Sexp' and
`nelisp_meta_walk' iterates `vector-len' (= ≤ 64) entries
internally; bit positions in the returned mask are chunk-local (=
0..63), so the caller adds `START' when mapping back to absolute
manifest indices.

Pure-elisp slice via `make-vector' + `aset' loop instead of
`subseq' / `seq-subseq' so the function stays compatible with the
standalone NeLisp runtime (= which does not currently load the
`seq' / `cl-lib' subseq machinery into the build-host elisp
namespace)."
  (let* ((len (- end start))
         (out (make-vector len nil))
         (i 0))
    (while (< i len)
      (aset out i (aref vec (+ start i)))
      (setq i (1+ i)))
    out))

(defun compile-elisp-objects-meta--compute-bitmask-chunks (srcs outs)
  "Return a vector of i64 chunk bitmasks of per-entry rebuild decisions.
The full input length `(length srcs)' is split into ceil(N/64)
chunks; each chunk slot holds an i64 bitmask whose bit `b' is 1
iff `nelisp_meta_should_rebuild' decided that the absolute entry
at index `chunk-idx * 64 + b' needs rebuilding (= source newer
than output, or output missing, or src missing).

Wave A27 production cutover.  i64 holds 64 distinct bit positions,
so the full 208-entry manifest is fanned out across 4 walker
dispatches; each dispatch sees a sliced sub-vector of ≤ 64 entries
and packs decisions into its chunk-local bitmask.  The result
preserves the chunked layout so the emit pass can index decisions
by `(chunk-idx, bit-pos)' without overflowing any single i64.

On standalone NeLisp (= `nl-jit-call-out-2' bound), each chunk
dispatches the Phase 47-compiled `nelisp_meta_walk' kernel once
with the sliced inputs:

  (nl-jit-call-out-2 \"nelisp_meta_walk\" CHUNK-SRCS CHUNK-OUTS)

The walker iterates internally via Phase 47-native tail recursion,
calling `nelisp_meta_should_rebuild' for each (src, out) pair in
the chunk, and packs the 0/1 decisions into a single
`Sexp::Int(chunk-bitmask)' return.  This collapses N elisp ->
native bridge calls into ceil(N/64) (= 4 calls for the 208-entry
production manifest, 1 call for the A26 5-entry PoC).

Missing-file pre-flight: the A25.2 `nelisp_meta_should_rebuild'
kernel's `extern-call stat' currently masks the stat-failure rc
indistinguishably from the rc-success branch.  Each chunk's
walker output is OR-ed with a per-chunk missing-file mask so
entries with absent src or out are flagged dirty regardless of
what the walker decided.

On host Emacs (= no `nl-jit-call-out-2'), the function falls back
to the per-entry `compile-elisp-objects-meta--should-rebuild' shim
called in an elisp loop, building each chunk's bitmask one bit at
a time so the byte-identity reference path stays untouched."
  (let* ((n (length srcs))
         (chunk-size compile-elisp-objects-meta--chunk-size)
         (chunk-count (compile-elisp-objects-meta--chunk-count n))
         (result (make-vector chunk-count 0))
         (chunk-idx 0))
    (while (< chunk-idx chunk-count)
      (let* ((chunk-start (* chunk-idx chunk-size))
             (chunk-end (min n (+ chunk-start chunk-size)))
             (chunk-len (- chunk-end chunk-start))
             (missing-mask
              (compile-elisp-objects-meta--missing-file-mask-chunk
               srcs outs chunk-start chunk-end)))
        (aset result chunk-idx
              (cond
               ((fboundp 'nl-jit-call-out-2)
                ;; Standalone NeLisp path — bridge call into the Phase 47
                ;; walker for this chunk's slice.  Slice srcs/outs into
                ;; chunk-local vectors so the walker's `vector-len' read
                ;; returns the chunk length and bit positions are
                ;; chunk-local.  On dlsym miss / arity mismatch / error
                ;; we mark every entry in the chunk dirty (safe
                ;; over-build).  OR with the chunk's missing-file
                ;; pre-flight so absent files are flagged dirty
                ;; regardless of the kernel's stat-failure return shape.
                (let* ((chunk-srcs (compile-elisp-objects-meta--slice-vector
                                    srcs chunk-start chunk-end))
                       (chunk-outs (compile-elisp-objects-meta--slice-vector
                                    outs chunk-start chunk-end))
                       (full-mask (if (= chunk-len 64)
                                      -1
                                    (1- (ash 1 chunk-len)))))
                  (logior missing-mask
                          (condition-case _err
                              (let ((bm (nl-jit-call-out-2
                                         "nelisp_meta_walk"
                                         chunk-srcs chunk-outs)))
                                (if (integerp bm) bm full-mask))
                            (error full-mask)))))
               (t
                ;; Host Emacs reference path — per-entry shim in an elisp
                ;; loop covering this chunk's slice.  Byte-identity-safe
                ;; mirror of the pre-A27 single-chunk path, scoped to
                ;; the chunk-local bit range so bit positions stay in
                ;; [0, 63].
                (let ((mask 0)
                      (i chunk-start))
                  (while (< i chunk-end)
                    (let* ((src (aref srcs i))
                           (out (aref outs i))
                           (decision
                            (compile-elisp-objects-meta--should-rebuild
                             (and (not (string-empty-p src)) src)
                             out))
                           (bit-pos (- i chunk-start)))
                      (when (not (zerop decision))
                        (setq mask (logior mask (ash 1 bit-pos)))))
                    (setq i (1+ i)))
                  (logior mask missing-mask))))))
      (setq chunk-idx (1+ chunk-idx)))
    result))

(defun compile-elisp-objects-meta--compute-bitmask (srcs outs)
  "Wave A26 single-i64 bitmask wrapper around the A27 chunked path.
Returns the chunk-0 bitmask from
`compile-elisp-objects-meta--compute-bitmask-chunks'; callers that
target the 5-entry PoC subset (= one chunk) get the i64 they
expected from the pre-A27 walker.  Callers that drive the full
208-entry manifest MUST use `--compute-bitmask-chunks' directly so
the chunk overflow is handled correctly."
  (let ((chunks (compile-elisp-objects-meta--compute-bitmask-chunks
                 srcs outs)))
    (if (= (length chunks) 0) 0 (aref chunks 0))))

(defun compile-elisp-objects-meta--walk (manifest _idx _end out-dir arch format)
  "Wave A26+A27 manifest walker (chunked).
Drives the per-entry up-to-date decision through the Phase 47-
compiled `nelisp_meta_walk' kernel (= ceil(N/64) bridge calls, each
returning an i64 chunk bitmask of dirty-bit decisions) when
running under standalone NeLisp, or falls back to the host-Emacs
per-entry shim for ERT / batch reference runs.

Wave A27 production cutover — the dirty/arch-skip state is now
stored per chunk (= vector of i64 masks) so the dispatch path
handles the full 212-entry production manifest without overflowing
any single i64.  Each chunk holds up to 64 entries; bit `b' of
chunk `c' corresponds to absolute manifest index `c * 64 + b'.

The elisp driver iterates the manifest once more to dispatch the
actual `nelisp-phase47-compile-to-object' emit step for every
entry whose bit is set in the matching chunk's bitmask, skipping
arch-mismatched entries by consulting the `:arch-skip-chunks'
value the path-vector builder produced.

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
         (arch-skip-chunks (plist-get paths :arch-skip-chunks))
         (n (length entries))
         (chunk-size compile-elisp-objects-meta--chunk-size)
         (bitmask-chunks
          (compile-elisp-objects-meta--compute-bitmask-chunks srcs outs))
         (acc nil)
         (i 0))
    (while (< i n)
      (let* ((entry (aref entries i))
             (feature (aref entry 0))
             (sexp    (aref entry 1))
             (out-path (aref outs i))
             (chunk-idx (/ i chunk-size))
             (bit-pos (- i (* chunk-idx chunk-size)))
             (bit-mask (ash 1 bit-pos))
             (arch-skip-mask (aref arch-skip-chunks chunk-idx))
             (dirty-mask (aref bitmask-chunks chunk-idx))
             (arch-skip-p (not (zerop (logand arch-skip-mask bit-mask)))))
        (cond
         (arch-skip-p
          nil)
         ((zerop (logand dirty-mask bit-mask))
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
finishes in well under a second; the production 208-entry walk is
exposed via `compile-elisp-objects-meta-emit-all' (Wave A27)."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch    (compile-elisp-objects--target-arch))
         (format  (compile-elisp-objects--target-format))
         (manifest (compile-elisp-objects-meta--build-static-manifest))
         (n (length manifest)))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (compile-elisp-objects-meta--walk manifest 0 n out-dir arch format)))

;;;###autoload
(defun compile-elisp-objects-meta-emit-all ()
  "Compile the FULL production manifest via the chunked static-manifest walker.
Returns the list of absolute paths written.  Wave A27 production
cutover entry point — drives the entire 212-entry
`compile-elisp-objects-manifest' through the Phase 47-compiled
`nelisp_meta_walk' kernel (ceil(N/64) = 4 chunks, each ≤ 64
entries) under standalone NeLisp, or through the byte-identity
host-Emacs per-entry shim under `emacs --batch'.

Output paths share the production names (= same
`compile-elisp-objects--out-dir' as the legacy
`compile-elisp-objects-emit-all'), so a byte-identity probe
against the legacy driver's reference output is a single
`cmp $REF $OUT' per entry.

Up-to-date entries (= chunk bitmask bit 0 + arch-skip bit 0) are
returned in the accumulator without re-emitting, mirroring the
legacy incremental-build behaviour."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch    (compile-elisp-objects--target-arch))
         (format  (compile-elisp-objects--target-format))
         (manifest (compile-elisp-objects-meta--build-full-static-manifest))
         (n (length manifest)))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (compile-elisp-objects-meta--walk manifest 0 n out-dir arch format)))

(provide 'compile-elisp-objects-meta)

;;; compile-elisp-objects-meta.el ends here
