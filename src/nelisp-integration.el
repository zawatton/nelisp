;;; nelisp-integration.el --- Phase 7.5 cross-module wire-up helpers  -*- lexical-binding: t; -*-

;; Phase 7.5.1 (Doc 32 v2 LOCKED 2026-04-25 §3.1) — partial.  Acts as a
;; central hub for the Phase 7.5 integration pieces that bridge the
;; allocator (T16 — bare-symbol family namespace) and the gc-inner
;; layer (T17 — keyword-prefixed family namespace) plus stubs for the
;; Phase 7.5.2 cold-init coordinator and Phase 7.5.3 release-artifact
;; verifier.
;;
;; Phase 7.5.2 (Doc 32 v2 LOCKED 2026-04-25 §3.2, this expansion) —
;; lifts the cold-init coordinator stub to a *real* implementation
;; that runs the Doc 28 §3.5 four-stage self-host bootstrap protocol
;; embedded inside a single process, plus a pre-flight verifier that
;; bin/anvil --strict-no-emacs uses to decide whether to drop the host
;; Emacs fallback.  The release-artifact verifier remains a stub for
;; Phase 7.5.3.
;;
;; Scope (Phase 7.5.1 partial, retained):
;;   - keyword family map (allocator bare ↔ gc-inner keyword) with
;;     bidirectional translation helpers + fallback for unknown
;;     family names.
;;   - cold-init coordinator stub (Phase 7.5.2 4-stage bootstrap).
;;   - release-artifact verifier stub (Phase 7.5.3 stage-d-v2.0
;;     binary checksum + symbol-presence check).
;;
;; Scope (Phase 7.5.2, this expansion):
;;   - `nelisp-integration-required-contract-versions' constant that
;;     records the heap-region registry (Doc 29 §1.4) and GC metadata
;;     wire (Doc 28 §6.10) contract versions the cold-init coordinator
;;     refuses to skew against.
;;   - `nelisp-integration-cold-init-verify' pre-flight check that
;;     bin/anvil --strict-no-emacs invokes before committing to the
;;     no-emacs path.  Returns (:ready t) or (:ready nil :missing
;;     (REASON ...)).
;;   - `nelisp-integration-cold-init-run' real coordinator that wires
;;     `nelisp-cc-bootstrap-run' (T12) and translates its plist into
;;     the Phase 7.5.2 status shape (briefing template).
;;   - `nelisp-integration-cold-init-smoke' multi-trial helper used by
;;     the Doc 32 v2 §7 success-rate gate (≥ 95% in 100 trials on the
;;     blocker class; this helper supports any trial count).
;;
;; Deferred to Phase 7.5.3:
;;   - stage-d-v2.0 release artifact verifier (cdylib symbol probe +
;;     SHA-256 checksum + GPG signature verification per §2.10).
;;   - actual MCP serve via the static-linked binary; today the
;;     coordinator simulates the cold-init protocol and bin/anvil
;;     falls back to host Emacs after a green coordinator run.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-cc-runtime)
(require 'nelisp-cc-bootstrap)

;;;; Family symbol bridge (T21 mismatch resolution)

(defconst nelisp-integration-keyword-family-map
  '((cons-pool     . :cons-pool)
    (closure-pool  . :closure-pool)
    (string-span   . :string-span)
    (vector-span   . :vector-span)
    (large-object  . :large-object))
  "Bridge: allocator (T16) bare symbol → gc-inner (T17) keyword family.
Phase 7.5 wire-up needs both namespaces to round-trip cleanly because
the allocator emits bare symbols (`cons-pool', etc.) while gc-inner
indexes families via keyword tags (`:cons-pool', etc.).  T21 surfaced
the mismatch; this map is the single authoritative bridge that
Phase 7.5 callers use.")

(defun nelisp-integration-family-to-keyword (sym)
  "Translate bare family SYM (e.g. `cons-pool') to keyword (e.g. `:cons-pool').
Falls back to `intern' on a `:'-prefixed symbol name when SYM is not
listed in `nelisp-integration-keyword-family-map' so future families
keep working without a code edit (the canonical map should still be
extended in lockstep)."
  (or (cdr (assq sym nelisp-integration-keyword-family-map))
      (intern (concat ":" (symbol-name sym)))))

(defun nelisp-integration-family-from-keyword (kw)
  "Translate keyword family KW (e.g. `:cons-pool') back to bare symbol.
Falls back to `intern' on the keyword name with the leading `:' stripped
when KW is not listed in `nelisp-integration-keyword-family-map'.  This
is the inverse of `nelisp-integration-family-to-keyword'."
  (or (car (rassq kw nelisp-integration-keyword-family-map))
      (let ((name (symbol-name kw)))
        (intern (if (and (> (length name) 0) (eq (aref name 0) ?:))
                    (substring name 1)
                  name)))))

;;;; Phase 7.5.2 / 7.5.3 stubs

(define-error 'nelisp-integration-todo
  "Phase 7.5 integration TODO not yet implemented")

(defun nelisp-integration--cold-init-coordinator-stub ()
  "Phase 7.5.2 cold-init coordinator stub.
Reserves the public entry point for the 4-stage bootstrap
orchestration described in Doc 28 §3.5 + Doc 32 v2 §3.2.  Until the
real body lands in Phase 7.5.2 this signals `nelisp-integration-todo'
with a tag identifying the deferred work.

Phase 7.5.2 ships the *real* coordinator under
`nelisp-integration-cold-init-run'; this stub stays in place as a
back-stop / regression sentinel — ERT continues to assert it signals
todo so accidental callers cannot silently bypass the public API."
  (signal 'nelisp-integration-todo
          (list 'cold-init-coordinator "Phase 7.5.2")))

(defun nelisp-integration--release-artifact-verifier-stub ()
  "Phase 7.5.3 release artifact verifier stub.
Reserves the public entry point for the stage-d-v2.0 binary verifier
described in Doc 32 v2 §3.3 (cdylib symbol presence + SHA-256
checksum + optional GPG signature).  Until the real body lands in
Phase 7.5.3 this signals `nelisp-integration-todo' with a tag
identifying the deferred work."
  (signal 'nelisp-integration-todo
          (list 'release-artifact-verifier "Phase 7.5.3")))

;;;; Phase 7.5.2 — contract version registry

(defconst nelisp-integration-required-contract-versions
  '((heap-region-registry . 1)  ;; Doc 29 §1.4
    (gc-metadata-wire     . 1)) ;; Doc 28 §6.10
  "Required contract versions for Phase 7.5.2 cold-init invocation.

Mismatch is silent corruption risk per Doc 30 v2 §6.4 — when the
allocator's heap-region registry (Doc 29 §1.4) or gc-inner's GC
metadata wire format (Doc 28 §6.10) skews against the bootstrapped
runtime, root scanning addresses the wrong slots and live objects
get swept.  The cold-init coordinator therefore refuses to run when
either contract version is missing or different from the values
recorded here.

This alist is single-sourced: any contract bump anywhere must edit
*this* table in lockstep with the bumping module's own version
constant.  ERT
`nelisp-integration-contract-version-mismatch-detects' guards the
coupling.")

(defun nelisp-integration--lookup-contract-version (key)
  "Return the loaded module's exposed version for contract KEY.

KEY is one of the symbols in `nelisp-integration-required-contract-versions'.
Returns the integer version when the corresponding module exposes
its constant in the running Emacs, or nil when the module is absent.

The lookup uses `boundp' against the module's documented version
constant after best-effort loading the providing module.  This is
forward-compatible because new contracts bump the constant in place
rather than renaming it."
  (pcase key
    ('heap-region-registry
     ;; Doc 29 §1.4 names the constant `nelisp-heap-region-version'
     ;; in `src/nelisp-allocator.el'.  We `require' it lazily so the
     ;; verifier still works when integration.el is loaded standalone
     ;; (e.g. from a stripped tarball that only ships nelisp-cc-*);
     ;; absence collapses to nil and the verifier reports a clean
     ;; :contract-version-mismatch entry.
     (ignore-errors (require 'nelisp-allocator nil t))
     (let ((sym 'nelisp-heap-region-version))
       (when (boundp sym) (symbol-value sym))))
    ('gc-metadata-wire
     ;; Doc 28 §6.10 / §2.9 → `nelisp-cc-runtime-gc-metadata-version'
     ;; in `src/nelisp-cc-runtime.el' (already required at top level).
     (let ((sym 'nelisp-cc-runtime-gc-metadata-version))
       (when (boundp sym) (symbol-value sym))))
    (_ nil)))

(defun nelisp-integration--verify-contract-versions ()
  "Compare loaded modules' contract versions against the registry.

Returns nil when every key in `nelisp-integration-required-contract-versions'
is loaded with the expected version.  Otherwise returns a list of
`(:contract KEY :expected EXP :actual ACT)' plists describing each
mismatch (or absence — :actual nil)."
  (let (mismatches)
    (dolist (entry nelisp-integration-required-contract-versions)
      (let* ((key (car entry))
             (expected (cdr entry))
             (actual (nelisp-integration--lookup-contract-version key)))
        (unless (and (integerp actual) (= actual expected))
          (push (list :contract key :expected expected :actual actual)
                mismatches))))
    (nreverse mismatches)))

;;;; Phase 7.5.2 — cold-init pre-flight verifier

(defun nelisp-integration--locate-runtime-binary ()
  "Locate the `nelisp-runtime' Rust binary (delegates to T13 helper).

Returns a string path when the binary is on disk and executable,
otherwise nil.  Wraps `nelisp-cc-runtime--locate-runtime-bin' with a
guard: T13 signals `nelisp-cc-runtime-binary-missing' on failure;
the cold-init verifier prefers a returned-nil shape so it can report
the failure via the :missing list rather than propagating an error."
  (condition-case _err
      (let ((bin (nelisp-cc-runtime--locate-runtime-bin)))
        (and (stringp bin) (file-executable-p bin) bin))
    (error nil)))

(defun nelisp-integration--locate-runtime-staticlib ()
  "Locate the `libnelisp_runtime.a' staticlib produced by Phase 7.5.1.

Returns a string path when the .a artifact is on disk, otherwise nil.
Path resolution mirrors `--locate-runtime-binary': use the worktree's
`nelisp-runtime/target/release/libnelisp_runtime.a' under whichever
root T13's locator already discovered.  The staticlib is the linkage
that Phase 7.5.3's stage-d-v2.0 binary will consume; Phase 7.5.2
verifies its presence as part of the cold-init readiness probe but
does not yet link against it (the coordinator still runs everything
through the simulator-backed T12 protocol)."
  (condition-case _err
      (let* ((bin (nelisp-cc-runtime--locate-runtime-bin))
             (dir (and bin (file-name-directory bin)))
             (lib (and dir
                       (expand-file-name "libnelisp_runtime.a" dir))))
        (and lib (file-readable-p lib) lib))
    (error nil)))

(defun nelisp-integration--locate-bootstrap-source ()
  "Return the `nelisp-cc.el' bootstrap source path, or nil when absent.

T12 `nelisp-cc-bootstrap-run' lowers a probe form rather than the
whole .el file in the scaffold path, but Phase 7.5.2 still wants to
flag a missing canonical source as a cold-init readiness blocker —
otherwise the bootstrap protocol's stage0 hash would be over an
empty / wrong file."
  (when (boundp 'nelisp-cc-runtime--this-file)
    (let* ((src-dir (and (bound-and-true-p nelisp-cc-runtime--this-file)
                         (file-name-directory
                          nelisp-cc-runtime--this-file)))
           (cc-src  (and src-dir (expand-file-name "nelisp-cc.el" src-dir))))
      (and cc-src (file-readable-p cc-src) cc-src))))

(defun nelisp-integration-cold-init-verify ()
  "Verify Phase 7.5.2 cold-init readiness; pre-flight before strict-no-emacs.

Performs five readiness checks in order — short-circuit failure is
preserved so the first missing dependency is the one reported:

  1. T13 `nelisp-runtime' binary exists and is executable.
  2. Phase 7.5.1 `libnelisp_runtime.a' staticlib exists (advisory:
     stage-d-v2.0 link target; absence reported but does not block
     the coordinator path which uses the simulator).
  3. T12 bootstrap source file (`src/nelisp-cc.el') is on disk.
  4. heap-region registry contract version (Doc 29 §1.4 = 1) matches.
  5. GC metadata wire contract version (Doc 28 §6.10 = 1) matches.

Returns one of:

  (:ready t)
  (:ready nil :missing ((REASON . DETAIL) ...))

REASON keywords:
  :runtime-binary-missing      — step 1 failed
  :runtime-staticlib-missing   — step 2 failed (advisory; still :ready t-able)
  :bootstrap-source-missing    — step 3 failed
  :contract-version-mismatch   — step 4 / 5 failed; DETAIL is the
                                 list returned by
                                 `nelisp-integration--verify-contract-versions'.

Phase 7.5.2 treats #2 (staticlib) as advisory because the simulator
path does not link against it.  bin/anvil --strict-no-emacs uses the
return value to decide whether to fall back to the host Emacs path:
:ready t → proceed with cold-init; :ready nil → log + fallback."
  (let ((missing nil))
    ;; (1) runtime binary.
    (unless (nelisp-integration--locate-runtime-binary)
      (push (cons :runtime-binary-missing
                  "run `make runtime' to build nelisp-runtime/target/release/nelisp-runtime")
            missing))
    ;; (2) staticlib (advisory).
    (let ((lib (nelisp-integration--locate-runtime-staticlib)))
      (unless lib
        (push (cons :runtime-staticlib-missing
                    "run `make runtime-staticlib' to build libnelisp_runtime.a (advisory; simulator path still works)")
              missing)))
    ;; (3) bootstrap source.
    (unless (nelisp-integration--locate-bootstrap-source)
      (push (cons :bootstrap-source-missing
                  "src/nelisp-cc.el not found on this Emacs's load path / file tree")
            missing))
    ;; (4) + (5) contract versions.
    (let ((cv (nelisp-integration--verify-contract-versions)))
      (when cv
        (push (cons :contract-version-mismatch cv) missing)))
    ;; Coordinator gating: any *non-advisory* failure flips :ready nil.
    ;; Advisory = staticlib only.  Phase 7.5.2 wants the verifier to
    ;; report staticlib absence without blocking the run, so we filter
    ;; it out before deciding readiness.
    (let* ((non-advisory (cl-remove-if (lambda (entry)
                                         (eq (car entry)
                                             :runtime-staticlib-missing))
                                       missing))
           (ready (null non-advisory)))
      (if (and ready (null missing))
          (list :ready t)
        (list :ready ready :missing (nreverse missing))))))

;;;; Phase 7.5.2 — cold-init coordinator (real implementation)

(defun nelisp-integration--exec-mode-default ()
  "Default `:exec-mode' for the cold-init status plist.

Returns `real' when the T13 binary is reachable and the host is
x86_64 (matching `nelisp-cc-real-exec-test--host-x86_64-p'); else
returns `simulator'.  The plist value is informational only — the
underlying T12 `nelisp-cc-bootstrap-run' always uses the simulator
pipeline because Phase 7.1.X frontend coverage does not yet support
direct execution of the bootstrap probe forms (Doc 28 §3.5 deferred
to Phase 7.5.3 actual binary)."
  (let* ((bin (nelisp-integration--locate-runtime-binary))
         (cfg (downcase (or (and (boundp 'system-configuration)
                                 system-configuration)
                            "")))
         (x86_64 (or (string-match-p "x86_64" cfg)
                     (string-match-p "amd64"  cfg))))
    (if (and bin x86_64) 'real 'simulator)))

(defun nelisp-integration-cold-init-run (&optional source-or-form)
  "Run the Phase 7.5.2 cold-init coordinator (Doc 28 §3.5 four stages).

Real implementation that lifts the Phase 7.5.1 stub and plugs the
T12 `nelisp-cc-bootstrap-run' protocol into the Phase 7.5.2 status
shape.  bin/anvil --strict-no-emacs invokes this through emacs --batch
to decide whether to commit to the host-Emacs-free path.

SOURCE-OR-FORM is either:
  - a path to a .el file (production path: src/nelisp-cc.el for the
    self-host probe),
  - a quoted lambda (test-friendly path),
  - nil (default = the canonical bootstrap source located via
    `nelisp-integration--locate-bootstrap-source').

Returns a plist matching the Doc 32 v2 §3.2 briefing template:
  (:status pass | fail
   :stage stage0 | stage1 | stage2 | stage3 | done
   :a2-hash STAGE0-DIGEST
   :a3-candidate-hash STAGE1-DIGEST
   :authoritative-promoted t | nil
   :fallback-active t | nil
   :elapsed-seconds FLOAT
   :exec-mode \\='simulator | \\='real
   :readiness READINESS-PLIST
   :protocol-version INT)

The function never raises — verify failures collapse to :status fail
with :stage \\='stage0 (the cold-init never reached the bootstrap), and
T12 stage failures pass through unchanged.  This makes the helper
safe for batch-mode callers (bin/anvil) which need a deterministic
exit path."
  (let* ((start (float-time))
         (readiness (nelisp-integration-cold-init-verify))
         (ready (plist-get readiness :ready))
         (exec-mode (nelisp-integration--exec-mode-default))
         (probe (or source-or-form
                    (nelisp-integration--locate-bootstrap-source)
                    ;; Last-resort probe: a no-op identity lambda.  This
                    ;; lets the coordinator still complete a structural
                    ;; bootstrap when `src/nelisp-cc.el' is absent (e.g.
                    ;; on a stripped-down release tarball) — the stage0
                    ;; hash will be over the lambda repr instead.
                    '(lambda (x) x))))
    (cond
     ;; Pre-flight failure → bail before the bootstrap.
     ((not ready)
      (list :status 'fail
            :stage 'stage0
            :a2-hash nil
            :a3-candidate-hash nil
            :authoritative-promoted nil
            :fallback-active t
            :elapsed-seconds (- (float-time) start)
            :exec-mode exec-mode
            :readiness readiness
            :protocol-version (if (boundp 'nelisp-cc-bootstrap-protocol-version)
                                  nelisp-cc-bootstrap-protocol-version
                                1)))
     ;; Pre-flight green → run T12 4-stage protocol.
     (t
      (let* ((boot (condition-case err
                       (cons :ok (nelisp-cc-bootstrap-run probe))
                     (error (cons :error err))))
             (ok (eq (car boot) :ok))
             (status-plist (and ok (cdr boot))))
        (cond
         ;; T12 itself raised — translate into a fail plist.
         ((not ok)
          (list :status 'fail
                :stage 'stage0
                :a2-hash nil
                :a3-candidate-hash nil
                :authoritative-promoted nil
                :fallback-active t
                :elapsed-seconds (- (float-time) start)
                :exec-mode exec-mode
                :readiness readiness
                :error (cdr boot)
                :protocol-version (if (boundp 'nelisp-cc-bootstrap-protocol-version)
                                      nelisp-cc-bootstrap-protocol-version
                                    1)))
         ;; T12 returned a structured plist — flatten the fields the
         ;; coordinator shape demands.  T12's plist already carries
         ;; :status / :stage / :a2-hash / :a3-candidate-hash /
         ;; :authoritative-promoted / :fallback-active /
         ;; :elapsed-seconds / :protocol-version, so we mostly pass
         ;; them through and append the coordinator-specific fields.
         (t
          (append
           (list :status (plist-get status-plist :status)
                 :stage (plist-get status-plist :stage)
                 :a2-hash (plist-get status-plist :a2-hash)
                 :a3-candidate-hash (plist-get status-plist :a3-candidate-hash)
                 :authoritative-promoted
                 (plist-get status-plist :authoritative-promoted)
                 :fallback-active (plist-get status-plist :fallback-active)
                 :elapsed-seconds (- (float-time) start)
                 :exec-mode exec-mode
                 :readiness readiness
                 :protocol-version
                 (plist-get status-plist :protocol-version))
           ;; Forward through diagnostic detail for stage2 / stage3
           ;; failures so callers can triage without re-running.
           (when (plist-get status-plist :stage2-detail)
             (list :stage2-detail
                   (plist-get status-plist :stage2-detail)))
           (when (plist-get status-plist :stage3-detail)
             (list :stage3-detail
                   (plist-get status-plist :stage3-detail)))))))))))

(defun nelisp-integration-cold-init-smoke (&optional iterations source-or-form)
  "Run cold-init ITERATIONS times and return the success-rate plist.

ITERATIONS defaults to 10 (Phase 7.5.2 in-tree smoke); Doc 32 v2 §7
fixes the blocker gate at 100 trials × 3 environments per release.

SOURCE-OR-FORM is forwarded to `nelisp-integration-cold-init-run'.

Returns:
  (:trials N
   :passed P
   :failed F
   :rate FLOAT-IN-[0.0 1.0]
   :first-failure FAIL-PLIST-OR-NIL)

The first-failure plist (when present) is the result of the first
failing run, suitable for triage logging.  The function never raises;
even a panic from `cold-init-run' (which itself catches all errors)
is captured."
  (let* ((n (or iterations 10))
         (passed 0)
         (failed 0)
         first-fail)
    (dotimes (_ n)
      (let ((r (nelisp-integration-cold-init-run source-or-form)))
        (if (eq (plist-get r :status) 'pass)
            (cl-incf passed)
          (cl-incf failed)
          (unless first-fail (setq first-fail r)))))
    (list :trials n
          :passed passed
          :failed failed
          :rate (if (zerop n) 0.0 (/ (float passed) n))
          :first-failure first-fail)))

(provide 'nelisp-integration)
;;; nelisp-integration.el ends here
