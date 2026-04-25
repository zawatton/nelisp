;;; nelisp-cc-bootstrap.el --- Phase 7.1.5 self-host bootstrap (4-stage protocol) scaffold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 7.1.5 *self-host bootstrap scaffold* — see
;; docs/design/28-phase7.1-native-compiler.org §3.5.  Implements the
;; structural skeleton for the 4-stage A1 → A2 → A3 self-host protocol
;; over the Phase 7.1.4 simulator (`nelisp-cc-runtime'); real
;; mmap-PROT_EXEC execution and the actual 3-axis bench gate
;; (fib 30x / fact-iter 20x / alloc-heavy 5x) land in Phase 7.5.
;;
;; The four stages, mapped 1:1 onto the doc:
;;
;;   stage0  host Emacs (Stage D Phase 6.1 path) byte-compiles the
;;           source — the resulting .elc *is* A2's input bytecode and
;;           is fed (conceptually) to the bytecode VM that runs A2.
;;           Failure mode: host Emacs is unavailable → entire bootstrap
;;           halts with `nelisp-cc-bootstrap-error'.
;;
;;   stage1  A2 (= byte-compiled `nelisp-cc.el' running on the bytecode
;;           VM) native-compiles SOURCE-FILE through
;;           `nelisp-cc-runtime-compile-and-allocate' (T11 simulator
;;           pipeline).  Result = A3 *candidate*.  Failure mode: A3
;;           generation errors → bytecode path (A2) maintained as
;;           fallback, bootstrap returns :status fail with
;;           :fallback-active t.
;;
;;   stage2  A2 and the A3 candidate are fed the same set of TEST-INPUT
;;           expressions; the resulting `:final-bytes' vectors are
;;           compared by SHA-256.  Equal hash for every input ⇒ pass;
;;           otherwise stage2 fails, A3 candidate is rejected, and the
;;           A2 bytecode path remains authoritative.  This is the
;;           "golden hash" gate from §3.5 stage2.
;;
;;   stage3  The A3 candidate re-compiles SOURCE-FILE itself; the
;;           resulting bytes are hashed and compared against the A3
;;           candidate's own hash.  Equal ⇒ A3 promoted to authoritative
;;           (Rhodes target-2 cold-init analogue, §3.5 stage3); diff ⇒
;;           A3 rejected, A2 fallback maintained.
;;
;; Scope (this file, ~600 LOC):
;;
;;   - Stage 0/1/2/3 helpers + the umbrella entry point
;;     `nelisp-cc-bootstrap-run' which threads the four stages and
;;     returns a status plist with the per-stage hashes.
;;   - SHA-256 helpers (`secure-hash' wrapper that copes with
;;     vector / list / string byte stores) + a byte-level
;;     `--compare-bytes' that reports the first mismatch position.
;;
;; Out of scope (deferred to Phase 7.1.5 full implementation /
;; Phase 7.5):
;;
;;   - Real native code execution: stage1/stage3 use the Phase 7.1.4
;;     simulator pipeline, so the "execution" is structural only — the
;;     simulator records syscall traces but does not actually jump into
;;     the produced bytes.  Phase 7.5 wires the FFI bridge to the
;;     `nelisp-runtime/' Rust crate.
;;   - 3-axis bench gate measurement: `test/nelisp-cc-bench.el' ships
;;     three skip-unless-gated ERTs that document the API shape but
;;     skip cleanly until Phase 7.5 supplies real measurement.
;;   - Full NeLisp form coverage: the SSA frontend (T6) handles
;;     constants / `let' / `if' / direct calls today; full coverage
;;     (defun / while / setq) ships with the linear-scan spill+phi
;;     work that the runtime layer already deferred to Phase 7.1.5.
;;
;; Module convention (matches Phase 7.1.4):
;;   - `nelisp-cc-bootstrap-' = public API
;;   - `nelisp-cc-bootstrap--' = private helper
;;   - errors derive from `nelisp-cc-bootstrap-error', a sibling of
;;     `nelisp-cc-error'.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'subr-x)
(require 'nelisp-cc)
(require 'nelisp-cc-runtime)

;;; Constants -------------------------------------------------------

(defconst nelisp-cc-bootstrap-protocol-version 1
  "Current 4-stage bootstrap protocol version (Doc 28 §3.5).
Bumped whenever the per-stage status plist contract changes shape;
Phase 7.5 reads this first and refuses to consume a status it does
not understand.")

(defconst nelisp-cc-bootstrap-default-test-inputs
  '((lambda (x) x)
    (lambda () 42)
    (lambda (a b) a))
  "Fallback set of S-expression inputs fed through stage1 / stage2.
Picked so each form lowers cleanly through the T6 frontend without
tripping the spill / phi-resolution band the linear-scan layer
defers to Phase 7.1.5 full implementation.  Callers passing
:test-inputs may extend this set; the scaffold guarantees these
three exercise the entry / param-passthrough / multi-arg paths.")

;;; Errors ----------------------------------------------------------

(define-error 'nelisp-cc-bootstrap-error
  "NeLisp Phase 7.1.5 bootstrap protocol error" 'nelisp-cc-error)

(define-error 'nelisp-cc-bootstrap-stage-failure
  "NeLisp Phase 7.1.5 bootstrap stage diff / hash mismatch"
  'nelisp-cc-bootstrap-error)

;;; Byte / hash helpers --------------------------------------------

(defun nelisp-cc-bootstrap--bytes-as-unibyte-string (bytes)
  "Render BYTES (vector / list / string) as a unibyte string.
The bytecode VM hands the runtime layer vectors of small integers in
[0, 255]; `secure-hash' on Emacs 27+ accepts a string only.  This
helper centralises the conversion so every stage agrees on the
canonical byte layout before hashing."
  (cond
   ((stringp bytes)
    ;; Already a string — but ensure unibyte encoding.  A multibyte
    ;; string would let two different byte vectors hash to the same
    ;; value (BUG in the bytecode boundary), so we coerce explicitly
    ;; via `encode-coding-string' on the no-conversion coding system.
    (if (multibyte-string-p bytes)
        (encode-coding-string bytes 'no-conversion)
      bytes))
   ((vectorp bytes)
    (let ((s (make-string (length bytes) 0)))
      (dotimes (i (length bytes))
        (let ((b (aref bytes i)))
          (unless (and (integerp b) (>= b 0) (< b 256))
            (signal 'nelisp-cc-bootstrap-error
                    (list :byte-out-of-range b :index i)))
          (aset s i b)))
      ;; `make-string' with an integer fill < 128 produces a unibyte
      ;; string already; force the coding-system through to be safe.
      (encode-coding-string s 'no-conversion)))
   ((listp bytes)
    (let ((s (make-string (length bytes) 0))
          (i 0))
      (dolist (b bytes)
        (unless (and (integerp b) (>= b 0) (< b 256))
          (signal 'nelisp-cc-bootstrap-error
                  (list :byte-out-of-range b :index i)))
        (aset s i b)
        (cl-incf i))
      (encode-coding-string s 'no-conversion)))
   (t (signal 'nelisp-cc-bootstrap-error
              (list :bytes-not-recognised (type-of bytes))))))

(defun nelisp-cc-bootstrap--sha256-of-bytes (bytes)
  "Compute SHA-256 of BYTES (vector / list / string).
Returns the canonical 64-character lowercase hex digest.  Uses the
built-in `secure-hash' (always available since Emacs 24); no extra
dependency.  Empty input hashes to the well-known SHA-256 of the
empty byte sequence, so callers can use the helper as a sentinel."
  (secure-hash 'sha256
               (nelisp-cc-bootstrap--bytes-as-unibyte-string bytes)))

(defun nelisp-cc-bootstrap--compare-bytes (a b)
  "Compare byte stores A and B; return diff plist or nil when equal.
Each of A / B may be vector / list / string.  When equal, returns
nil.  When unequal, returns a plist:
  (:length-a INT
   :length-b INT
   :first-mismatch INDEX-or-nil)
Where :first-mismatch is nil when one is a strict prefix of the
other (the lengths plist tell you which) and the integer position
of the first byte that differs otherwise."
  (let* ((sa (nelisp-cc-bootstrap--bytes-as-unibyte-string a))
         (sb (nelisp-cc-bootstrap--bytes-as-unibyte-string b))
         (la (length sa))
         (lb (length sb))
         (min-len (min la lb))
         (mismatch nil))
    (cl-block scan
      (dotimes (i min-len)
        (unless (= (aref sa i) (aref sb i))
          (setq mismatch i)
          (cl-return-from scan))))
    (cond
     ((and (= la lb) (null mismatch)) nil)
     (t (list :length-a la
              :length-b lb
              :first-mismatch mismatch)))))

;;; Stage 0 — host Emacs byte-compile -------------------------------
;;
;; The scaffold cannot byte-compile a *source-file* during ERT
;; (`byte-compile-file' would litter the test tree with .elc files
;; and is sensitive to the compile-time `load-path'), so the helper
;; accepts either a source-file path *or* an already-loaded form.
;; In production Phase 7.5 will dispatch on the source path and
;; produce an actual .elc; the scaffold returns a deterministic hash
;; over the form's `prin1-to-string' representation.
;;
;; This is sufficient for the protocol skeleton: stage0 produces a
;; stable hash, stage1+ consume it, stage2 / stage3 verify against
;; their own hashes.  When the real .elc replaces the placeholder,
;; the consumer code at every stage needs no change because the
;; "hash + path-or-nil" cons stays shape-compatible.

(defun nelisp-cc-bootstrap--stage0-compile-bytecode (source)
  "Stage 0: produce A2 input bytecode for SOURCE.
SOURCE may be:
  - a string path to a .el file (production Phase 7.5 path)
  - a quoted lambda / form (scaffold-friendly path used by tests)

The scaffold path serialises SOURCE via `prin1-to-string' and hashes
the bytes; the .el path delegates to `byte-compile-file' and hashes
the resulting .elc.  Returns a cons (BYTECODE-PATH-OR-NIL .
SHA-256-HEX-DIGEST).  The path slot is nil for the scaffold path
because no .elc lands on disk.

Doc 28 §3.5 stage0 contract: this hash is the seed A2 consumes; if
host Emacs is unavailable the bootstrap halts here (we currently
trust that any caller of this function is on a working Emacs
already, since we *are* running Elisp)."
  (cond
   ;; .el source-file path (Phase 7.5 production).
   ((and (stringp source)
         (file-readable-p source)
         (string-suffix-p ".el" source))
    (let* ((elc (concat source "c"))
           ;; We deliberately do *not* call `byte-compile-file' here in
           ;; the scaffold — it would write to the source tree and
           ;; contaminate the test environment.  Phase 7.5 swaps the
           ;; body for `(byte-compile-file source)' and reads the
           ;; resulting .elc.  Until then, prefer an existing .elc
           ;; sibling and fall back to hashing the .el source itself.
           (elc-readable (file-readable-p elc))
           (target (if elc-readable elc source))
           (bytes (with-temp-buffer
                    (insert-file-contents-literally target)
                    (buffer-substring-no-properties (point-min)
                                                    (point-max)))))
      (cons (and elc-readable elc)
            (nelisp-cc-bootstrap--sha256-of-bytes bytes))))
   ;; Form path (scaffold-friendly).
   (t
    (let ((repr (prin1-to-string source)))
      (cons nil (nelisp-cc-bootstrap--sha256-of-bytes repr))))))

;;; Stage 1 — A2 native-compiles → A3 candidate --------------------

(defun nelisp-cc-bootstrap--stage1-native-compile (form &optional backend)
  "Stage 1: compile FORM through the Phase 7.1.4 simulator pipeline.
FORM is a quoted lambda (e.g. `(lambda (x) x)') — the scaffold's
analogue of a source-file's body.  BACKEND is `x86_64' (Linux) or
`arm64' (macOS Apple Silicon); defaults to host inference via
`nelisp-cc-runtime--default-backend'.

Returns a plist:
  (:result COMPILE-AND-ALLOCATE-RESULT  ; the runtime layer's plist
   :hash SHA-256-HEX-OF-FINAL-BYTES
   :final-bytes VECTOR
   :gc-metadata META
   :backend BACKEND)

Doc 28 §3.5 stage1: the final-bytes hash is the A3-candidate
identity; stage2 / stage3 compare it byte-for-byte (and bit-for-bit
via the SHA digest) against future runs.  When `compile-and-allocate'
itself signals — e.g. the SSA frontend trips on an unsupported form
— the error propagates to the caller.  Doc 28 stage1 fallback path
(A3 generation failure → A2 maintained) is realised at the
`nelisp-cc-bootstrap-run' level, not here."
  (let* ((be (or backend (nelisp-cc-runtime--default-backend)))
         (result (nelisp-cc-runtime-compile-and-allocate form be))
         (final  (plist-get result :final-bytes))
         (meta   (plist-get result :gc-metadata)))
    (list :result result
          :hash (nelisp-cc-bootstrap--sha256-of-bytes final)
          :final-bytes final
          :gc-metadata meta
          :backend be)))

;;; Stage 2 — semantic diff (A2 vs A3 candidate) -------------------
;;
;; Doc 28 §3.5 stage2: "A2 vs A3-candidate semantic diff via golden
;; hash".  The scaffold's interpretation, faithful to the doc:
;; given the same INPUT-FORM, A2 (= bytecode-VM-driven compiler) and
;; the A3 candidate must produce *byte-identical* mmap pages.  In the
;; scaffold "byte-identical" reduces to "their `:final-bytes' SHA-256
;; matches".  A2 here is *modelled* by simply re-running stage1 — in
;; production A2 would actually be the byte-compiled `nelisp-cc.el'
;; running on the host bytecode VM; for the structural protocol the
;; two paths produce the same bytes by construction (the same
;; `nelisp-cc' function ran in the same Emacs), so the scaffold
;; gates on the determinism of `compile-and-allocate', which is the
;; gate Phase 7.5 needs to keep.

(defun nelisp-cc-bootstrap--stage2-semantic-diff (a2-stage1-result a3-candidate-stage1-result)
  "Stage 2: compare two stage1 results' `:final-bytes' hashes.
Returns a plist:
  (:status pass | fail
   :a2-hash A2-DIGEST
   :a3-candidate-hash A3-DIGEST
   :diff DIFF-PLIST-OR-NIL)
Where :diff is nil for pass and the `--compare-bytes' return value
otherwise (giving Phase 7.5 the first mismatch byte for triage).

Doc 28 §3.5 stage2: hash mismatch ⇒ A3 candidate rejected, A2
fallback maintained.  `nelisp-cc-bootstrap-run' converts a fail
status here into the corresponding plist."
  (let* ((h-a2 (plist-get a2-stage1-result :hash))
         (h-a3 (plist-get a3-candidate-stage1-result :hash))
         (b-a2 (plist-get a2-stage1-result :final-bytes))
         (b-a3 (plist-get a3-candidate-stage1-result :final-bytes))
         (equal-hash (and h-a2 h-a3 (string= h-a2 h-a3)))
         (diff (unless equal-hash
                 (nelisp-cc-bootstrap--compare-bytes b-a2 b-a3))))
    (list :status (if equal-hash 'pass 'fail)
          :a2-hash h-a2
          :a3-candidate-hash h-a3
          :diff diff)))

(defun nelisp-cc-bootstrap--stage2-semantic-diff-multi (test-inputs &optional backend)
  "Run stage2 on every form in TEST-INPUTS.
For each form: run stage1 twice (modelling A2 + A3-candidate, which
in the scaffold are structurally identical), compare hashes via
`--stage2-semantic-diff'.  Returns:
  (:status pass | fail
   :input-results ((FORM . STAGE2-RESULT-PLIST) ...))

The whole-set status is `pass' when *every* input passes, else
`fail' (the doc's golden-hash semantics is conjunctive)."
  (let* ((be (or backend (nelisp-cc-runtime--default-backend)))
         (acc nil)
         (overall 'pass))
    (dolist (form test-inputs)
      (let* ((a2 (nelisp-cc-bootstrap--stage1-native-compile form be))
             (a3 (nelisp-cc-bootstrap--stage1-native-compile form be))
             (st (nelisp-cc-bootstrap--stage2-semantic-diff a2 a3)))
        (unless (eq (plist-get st :status) 'pass)
          (setq overall 'fail))
        (push (cons form st) acc)))
    (list :status overall :input-results (nreverse acc))))

;;; Stage 3 — A3 self-recompile ------------------------------------

(defun nelisp-cc-bootstrap--stage3-self-recompile (a3-candidate-stage1-result form &optional backend)
  "Stage 3: A3 candidate re-compiles FORM, hash must match own.
Doc 28 §3.5 stage3 / Rhodes target-2 cold-init analogue.

A3-CANDIDATE-STAGE1-RESULT is the plist returned by
`--stage1-native-compile' on the same FORM.  In a real run the
re-compile would happen *through* the A3 candidate (i.e. the
candidate's exec page is jumped into and produces the bytes); in
the scaffold the simulator pipeline is deterministic, so calling
`compile-and-allocate' a second time models the self-recompile.

Returns a plist:
  (:status pass | fail
   :authoritative-hash A3-HASH-when-pass-else-nil
   :recompile-hash RE-RUN-HASH
   :diff DIFF-PLIST-OR-NIL)

`pass' ⇒ A3 promoted to authoritative.  `fail' ⇒ A3 rejected, A2
fallback maintained (the run-level umbrella encodes the latter)."
  (let* ((be (or backend (nelisp-cc-runtime--default-backend)))
         (rerun (nelisp-cc-bootstrap--stage1-native-compile form be))
         (h-orig (plist-get a3-candidate-stage1-result :hash))
         (h-new  (plist-get rerun :hash))
         (equal-hash (and h-orig h-new (string= h-orig h-new)))
         (diff (unless equal-hash
                 (nelisp-cc-bootstrap--compare-bytes
                  (plist-get a3-candidate-stage1-result :final-bytes)
                  (plist-get rerun :final-bytes)))))
    (list :status (if equal-hash 'pass 'fail)
          :authoritative-hash (when equal-hash h-orig)
          :recompile-hash h-new
          :diff diff)))

;;; Umbrella: 4-stage protocol -------------------------------------

(defun nelisp-cc-bootstrap-run (source-or-form &optional backend test-inputs)
  "Run the full Phase 7.1.5 4-stage bootstrap protocol.
SOURCE-OR-FORM is either:
  - a path to a .el file (Phase 7.5 production path), in which case
    the file is also used as the FORM passed to stages 1 and 3
    *only when* the caller supplies TEST-INPUTS — the scaffold
    cannot lower a whole .el file through T6 yet, so for path-only
    invocations it derives a single `lambda' form from the first
    test input or falls back to `nelisp-cc-bootstrap-default-test-inputs'
  - a quoted lambda (scaffold path) used directly as the stage1/3 form

BACKEND is `x86_64' / `arm64' (default: host inference).
TEST-INPUTS is an optional list of forms run through stage2; defaults
to `nelisp-cc-bootstrap-default-test-inputs'.

Returns a status plist matching the briefing template:
  (:status pass | fail
   :stage stage0 | stage1 | stage2 | stage3 | done
   :a2-hash STAGE0-DIGEST
   :a3-candidate-hash STAGE1-DIGEST
   :diff-result pass | fail
   :authoritative-promoted t | nil
   :fallback-active t | nil
   :elapsed-seconds FLOAT
   :backend BACKEND
   :protocol-version INT)

Stage failure semantics (Doc 28 §3.5):
  - stage0 fails  → :status fail :stage stage0 :fallback-active t
                    (impossible in the scaffold; included for shape)
  - stage1 fails  → :status fail :stage stage1 :fallback-active t
                    (raised by `compile-and-allocate', surfaced here
                     as a :status fail with `:diff-result fail')
  - stage2 fails  → :status fail :stage stage2 :diff-result fail
                    :fallback-active t (A3 rejected, A2 maintained)
  - stage3 fails  → :status fail :stage stage3 :authoritative-promoted nil
                    :fallback-active t (A3 rejected post-candidacy)
  - all stages    → :status pass :stage done :authoritative-promoted t
    pass             :fallback-active nil

The umbrella catches stage1 errors with `condition-case' so a single
malformed form does not abort the whole bootstrap; ERTs assert this
behaviour explicitly."
  (let* ((be (or backend (nelisp-cc-runtime--default-backend)))
         (inputs (or test-inputs nelisp-cc-bootstrap-default-test-inputs))
         (start (float-time))
         ;; Pick the form fed to stage1/3.  When SOURCE-OR-FORM is a
         ;; path we cannot lower the whole .el yet, so we use the
         ;; first test input as the surrogate.  This is not a
         ;; correctness compromise: stage0's hash is over the source
         ;; bytes anyway, and stage1/3 are about the *compiler's*
         ;; determinism on a *probe* form — picking the first probe
         ;; from TEST-INPUTS is exactly what the doc envisions for
         ;; the scaffold.
         (probe-form (cond
                      ((and (consp source-or-form)
                            (eq (car source-or-form) 'lambda))
                       source-or-form)
                      ((and (stringp source-or-form) (car inputs))
                       (car inputs))
                      ((car inputs))
                      (t (signal 'nelisp-cc-bootstrap-error
                                 (list :no-probe-form source-or-form)))))
         ;; Stage 0.
         (stage0 (nelisp-cc-bootstrap--stage0-compile-bytecode source-or-form))
         (a2-hash (cdr stage0))
         ;; Stage 1.
         (stage1-pair
          (condition-case err
              (cons :ok (nelisp-cc-bootstrap--stage1-native-compile probe-form be))
            (error (cons :error err))))
         (stage1-ok (eq (car stage1-pair) :ok))
         (stage1-result (and stage1-ok (cdr stage1-pair)))
         (a3-hash (and stage1-ok (plist-get stage1-result :hash))))
    (cond
     ;; Stage 1 failure → A2 fallback path.
     ((not stage1-ok)
      (list :status 'fail
            :stage 'stage1
            :a2-hash a2-hash
            :a3-candidate-hash nil
            :diff-result 'fail
            :authoritative-promoted nil
            :fallback-active t
            :elapsed-seconds (- (float-time) start)
            :backend be
            :protocol-version nelisp-cc-bootstrap-protocol-version
            :error (cdr stage1-pair)))
     (t
      ;; Stage 2.
      (let* ((stage2 (nelisp-cc-bootstrap--stage2-semantic-diff-multi inputs be))
             (stage2-status (plist-get stage2 :status)))
        (cond
         ((not (eq stage2-status 'pass))
          (list :status 'fail
                :stage 'stage2
                :a2-hash a2-hash
                :a3-candidate-hash a3-hash
                :diff-result 'fail
                :authoritative-promoted nil
                :fallback-active t
                :elapsed-seconds (- (float-time) start)
                :backend be
                :protocol-version nelisp-cc-bootstrap-protocol-version
                :stage2-detail stage2))
         (t
          ;; Stage 3.
          (let* ((stage3 (nelisp-cc-bootstrap--stage3-self-recompile
                          stage1-result probe-form be))
                 (stage3-status (plist-get stage3 :status)))
            (cond
             ((not (eq stage3-status 'pass))
              (list :status 'fail
                    :stage 'stage3
                    :a2-hash a2-hash
                    :a3-candidate-hash a3-hash
                    :diff-result 'pass
                    :authoritative-promoted nil
                    :fallback-active t
                    :elapsed-seconds (- (float-time) start)
                    :backend be
                    :protocol-version nelisp-cc-bootstrap-protocol-version
                    :stage3-detail stage3))
             (t
              ;; All four stages green.
              (list :status 'pass
                    :stage 'done
                    :a2-hash a2-hash
                    :a3-candidate-hash a3-hash
                    :diff-result 'pass
                    :authoritative-promoted t
                    :fallback-active nil
                    :elapsed-seconds (- (float-time) start)
                    :backend be
                    :protocol-version nelisp-cc-bootstrap-protocol-version)))))))))))

(provide 'nelisp-cc-bootstrap)
;;; nelisp-cc-bootstrap.el ends here
