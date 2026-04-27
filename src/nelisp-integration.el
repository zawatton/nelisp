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

;;;; Phase 7.5.2 — scaffold-layer dispatcher + per-stage stubs
;;
;; The real coordinator (`nelisp-integration-cold-init-run' above)
;; delegates the four bootstrap stages to T12 `nelisp-cc-bootstrap-run',
;; which in turn requires the T13 `nelisp-runtime' binary to be on
;; disk.  Hosts without that binary (= release tarballs that ship only
;; `src/' + `bin/anvil', CI runners that skip `make runtime') cannot
;; exercise the real path.
;;
;; The scaffold-layer dispatcher below offers a strictly structural
;; alternative: a 4-stage harness that runs each stage through either
;; the *real* handler (when supplied via `:handlers' override) or a
;; clearly-marked stub handler that returns
;;   (:status :stub-not-yet-impl-pending-phase-X.Y :stage STAGE).
;; Production callers therefore cannot mistake a stub return for a
;; passing run — every stub status keyword starts with `:stub-' and
;; the dispatcher refuses to count a stub as completed (the
;; `:stages-completed' counter only advances on `pass').
;;
;; This separation lets Phase 7.5.2 ship the *contract* (= dispatcher
;; shape, error propagation, timing, soak harness) before Phase
;; 7.0-7.4 closure unblocks the real 4-stage embed.  The ERT layer
;; pins the contract so a future Phase 7.0-7.4 close cannot drift the
;; plist shape silently.

(defconst nelisp-integration-cold-init-stage-order
  '(stage1 stage2 stage3 stage4)
  "Ordered list of cold-init stages run by the scaffold-layer dispatcher.
Doc 32 v2 §3.2 + §4 names four stages:
  - stage1 = Phase 7.0 syscall-stub init (mmap arena ready)
  - stage2 = Phase 7.2 allocator + GC scheduler init
  - stage3 = Phase 7.4 coding (UTF-8 IO codec attach)
  - stage4 = Phase 7.1 native compiler bootstrap (= Doc 28 §3.5
             4-stage embed; the scaffold treats the embed as a
             single stage4 unit because the inner four sub-stages
             are owned by `nelisp-cc-bootstrap-run' / T12).
The stage4 inner sub-stages are deliberately not exposed at this
layer — Phase 7.5.2 contract is at the *outer* 4-stage granularity.")

(defconst nelisp-integration-cold-init-stage-blocked-by
  '((stage1 . "Phase 7.0 syscall-stub init (= Phase 7.0.x close)")
    (stage2 . "Phase 7.2 allocator + GC scheduler init (= Phase 7.2.x close)")
    (stage3 . "Phase 7.4 coding init (= Phase 7.4.x close)")
    (stage4 . "Phase 7.1 native compiler bootstrap (= Phase 7.1.x close)"))
  "Per-stage blocker tag returned by the stub handlers.
The scaffold dispatcher embeds this tag into the stub status keyword
so callers can grep for the blocking phase without re-reading the
design doc.  Stub keyword shape:
  :stub-not-yet-impl-pending-phase-7.0
  :stub-not-yet-impl-pending-phase-7.1
  :stub-not-yet-impl-pending-phase-7.2
  :stub-not-yet-impl-pending-phase-7.4")

(defconst nelisp-integration-cold-init-stage-pending-keyword
  '((stage1 . :stub-not-yet-impl-pending-phase-7.0)
    (stage2 . :stub-not-yet-impl-pending-phase-7.2)
    (stage3 . :stub-not-yet-impl-pending-phase-7.4)
    (stage4 . :stub-not-yet-impl-pending-phase-7.1))
  "Per-stage stub-status keyword returned by the default stub handlers.
The keyword names the phase whose close unblocks the real
implementation; production callers can `memq' the keyword against
the value list of this alist to detect a stub return without
introspecting the symbol name.")

(defun nelisp-integration--cold-init-stub-handler (stage)
  "Return the default stub handler closure for STAGE.
The closure ignores its argument and returns a plist with
`:status :stub-not-yet-impl-pending-phase-X.Y :stage STAGE'."
  (let ((kw (cdr (assq stage
                       nelisp-integration-cold-init-stage-pending-keyword))))
    (lambda (&rest _ignored)
      (list :status kw
            :stage stage
            :blocked-by (cdr (assq stage
                                   nelisp-integration-cold-init-stage-blocked-by))))))

(defun nelisp-integration--cold-init-default-handlers ()
  "Build the default `:handlers' alist (every stage = stub).
Returns ((stage1 . STUB) ... (stage4 . STUB))."
  (mapcar (lambda (stage)
            (cons stage
                  (nelisp-integration--cold-init-stub-handler stage)))
          nelisp-integration-cold-init-stage-order))

(defun nelisp-integration--cold-init-run-stage (stage handler ctx)
  "Run HANDLER for STAGE with CTX (an alist), capturing timing + errors.
Returns a plist:
  (:stage STAGE
   :status STATUS
   :elapsed-seconds FLOAT
   :error nil | (CONDITION . DATA)
   :result HANDLER-RETURN-OR-NIL)
STATUS is `pass' on a real handler that returned without error,
`fail' when the handler raised, or whatever stub keyword the handler
returned (`:stub-not-yet-impl-pending-phase-X.Y' for the default)."
  (let ((start (float-time)))
    (condition-case err
        (let* ((ret (funcall handler ctx))
               (status (or (and (listp ret) (plist-get ret :status))
                           'pass)))
          (list :stage stage
                :status status
                :elapsed-seconds (- (float-time) start)
                :error nil
                :result ret))
      (error
       (list :stage stage
             :status 'fail
             :elapsed-seconds (- (float-time) start)
             :error err
             :result nil)))))

(defun nelisp-integration-cold-init-dispatch (&rest args)
  "Phase 7.5.2 scaffold-layer 4-stage cold-init dispatcher.

This is the *structural* dispatcher; the *real* coordinator that
exercises the Doc 28 §3.5 self-host bootstrap lives at
`nelisp-integration-cold-init-run' and requires the T13 binary.

ARGS is a plist:
  :handlers ALIST          — per-stage handler override.  Default = every
                             stage gets the stub handler from
                             `nelisp-integration--cold-init-stub-handler',
                             which returns
                             `:status :stub-not-yet-impl-pending-phase-X.Y'.
  :ctx ALIST               — opaque handler context, threaded through.
  :stop-on STATUS-LIST     — stop dispatch as soon as a stage returns a
                             status in this list (default = (fail), so
                             stub returns do *not* stop dispatch and the
                             counter reflects the stages structurally
                             reached).

Returns a plist matching the Doc 32 v2 §3.2 dispatcher contract:
  (:status pass | fail
   :stages-completed N        ; count of stages that returned pass
   :stages [(:stage S :status S :elapsed-seconds F :error E) ...]
   :elapsed-seconds FLOAT     ; total wall-clock for the dispatch
   :error MSG-OR-NIL          ; first non-pass status (for triage)
   :scaffold t)               ; sentinel: this is the scaffold path,
                              ; not the real coordinator

`:status pass' is reserved for runs where every stage returned `pass'.
A run with all four stages returning a stub keyword reports
`:status fail :error :stub-not-yet-impl-pending-phase-X.Y' so callers
cannot mistake the scaffold for a real green path.

The function never raises; every error path is captured into the
returned plist."
  (let* ((handlers (or (plist-get args :handlers)
                       (nelisp-integration--cold-init-default-handlers)))
         (ctx (plist-get args :ctx))
         (stop-on (or (plist-get args :stop-on) '(fail)))
         (start (float-time))
         (stages-completed 0)
         (stage-results nil)
         (first-nonpass nil))
    (catch 'cold-init-dispatch-stop
      (dolist (stage nelisp-integration-cold-init-stage-order)
        (let* ((handler (or (cdr (assq stage handlers))
                            (nelisp-integration--cold-init-stub-handler stage)))
               (r (nelisp-integration--cold-init-run-stage stage handler ctx))
               (status (plist-get r :status)))
          (push r stage-results)
          (cond
           ((eq status 'pass)
            (cl-incf stages-completed))
           (t
            (unless first-nonpass (setq first-nonpass status))
            (when (memq status stop-on)
              (throw 'cold-init-dispatch-stop nil)))))))
    (list :status (if (and (= stages-completed
                              (length nelisp-integration-cold-init-stage-order))
                           (null first-nonpass))
                      'pass
                    'fail)
          :stages-completed stages-completed
          :stages (nreverse stage-results)
          :elapsed-seconds (- (float-time) start)
          :error first-nonpass
          :scaffold t)))

;;;; Phase 7.5.3 dependency — 24h soak harness scaffold
;;
;; The real 24h soak (= Doc 32 v2 §2.7 / §7) measures GC pause p99,
;; RSS growth, and crash count over a 24h window — those numbers
;; require the static-linked stage-d-v2.0 binary which lands in
;; Phase 7.5.3.  Phase 7.5.2 ships only the *test framework* so the
;; soak script can be wired into CI ahead of the real measurement;
;; the harness invokes a per-iteration handler N times, captures
;; per-iteration timing, and returns a structural summary.
;;
;; No actual GC / RSS measurement happens here — the soak metric
;; collection lands in Phase 7.5.3 alongside the static binary.

(defconst nelisp-integration-soak-harness-default-iterations 4
  "Default iteration count for the Phase 7.5.3 soak harness scaffold.
Tiny on purpose — Phase 7.5.2 ERT runs the harness inline so the
default cannot be a 24h figure.  Real soak runs override this with
`:iterations 86400' (1Hz × 24h) or a duration-driven loop.")

(defun nelisp-integration-soak-harness (&rest args)
  "Phase 7.5.3 dependency — 24h soak harness *scaffold* (no metric capture).

ARGS is a plist:
  :iterations N            — number of harness iterations (default
                             `nelisp-integration-soak-harness-default-iterations').
  :handler FN              — called once per iteration with the
                             iteration index (0-based).  Default =
                             `nelisp-integration-cold-init-dispatch'
                             with the scaffold defaults.
  :stop-on-fail BOOL       — stop the soak as soon as any iteration
                             returns a non-pass status (default nil so
                             the harness completes the full iteration
                             count even when every iteration is a stub
                             return).

Returns a plist:
  (:iterations N
   :passed P                ; iterations where handler returned pass
   :failed F                ; iterations where handler returned non-pass
   :elapsed-seconds FLOAT   ; total wall-clock
   :first-failure FAIL-PLIST-OR-NIL
   :scaffold t              ; sentinel: actual GC/RSS metric collection
                            ; lands in Phase 7.5.3 alongside the
                            ; static-linked stage-d-v2.0 binary
   :status pass | fail)     ; pass iff all iterations passed

The function never raises; handler errors are captured into the
per-iteration result via `nelisp-integration--cold-init-run-stage'-style
condition-case wrapping inside the handler itself (the dispatcher
already handles this).

Callers wiring this to a real 24h soak should pass a `:handler' that
also pushes GC pause / RSS samples into a Phase 7.5.3 metric collector;
the scaffold itself is metric-agnostic so the contract does not change
when real measurement lands."
  (let* ((iterations (or (plist-get args :iterations)
                         nelisp-integration-soak-harness-default-iterations))
         (handler (or (plist-get args :handler)
                      (lambda (_i) (nelisp-integration-cold-init-dispatch))))
         (stop-on-fail (plist-get args :stop-on-fail))
         (start (float-time))
         (passed 0)
         (failed 0)
         (first-failure nil))
    (catch 'soak-harness-stop
      (dotimes (i iterations)
        (let* ((r (condition-case err
                      (funcall handler i)
                    (error (list :status 'fail :error err))))
               (status (and (listp r) (plist-get r :status))))
          (if (eq status 'pass)
              (cl-incf passed)
            (cl-incf failed)
            (unless first-failure (setq first-failure r))
            (when stop-on-fail
              (throw 'soak-harness-stop nil))))))
    (list :iterations iterations
          :passed passed
          :failed failed
          :elapsed-seconds (- (float-time) start)
          :first-failure first-failure
          :scaffold t
          :status (if (and (= passed iterations) (zerop failed))
                      'pass
                    'fail))))

;;;; Phase 7.5.3 — release artifact verifier (Doc 32 v2 §3.3 prep)

(defconst nelisp-integration-release-artifact-platforms
  '("linux-x86_64" "macos-arm64" "linux-arm64")
  "Platforms recognised by the Phase 7.5.3 release artifact verifier.
Keep this list in sync with `tools/build-release-artifact.sh' and
`.github/workflows/release-qualification.yml' — the §11 LOCKED tier
matrix records linux-x86_64 as the v1.0 blocker; the two arm64 entries
are non-blocker (v1.0 時限) and continue-on-error in CI.")

(defconst nelisp-integration-release-artifact-default-version
  "stage-d-v2.0"
  "Default release artifact version recognised by the verifier.
Mirrors the `RELEASE_VERSION' default in `Makefile' and the
`VERSION' default in `tools/build-release-artifact.sh'.")

(defun nelisp-integration--release-artifact-locate-dist ()
  "Locate the `dist/' directory under the active worktree root.
Returns an absolute path string when the directory exists, otherwise
nil.  The lookup walks up from `nelisp-cc-runtime--this-file' (the same
locator T13 uses for the runtime binary) so the verifier works
regardless of the caller's `default-directory'."
  (let* ((src (and (boundp 'nelisp-cc-runtime--this-file)
                   nelisp-cc-runtime--this-file))
         (root (and src (locate-dominating-file src "Makefile")))
         (dist (and root (expand-file-name "dist" root))))
    (and dist (file-directory-p dist) dist)))

(defun nelisp-integration--release-artifact-paths (version platform)
  "Return the absolute (TARBALL CHECKSUM SIG) paths for VERSION × PLATFORM.
Resolves under the discovered `dist/' directory; returns nil when
`dist/' itself is absent.  The paths are returned even when the files
do not yet exist — callers use `file-exists-p' to discriminate."
  (let* ((dist (nelisp-integration--release-artifact-locate-dist))
         (base (and dist (format "%s-%s.tar.gz" version platform))))
    (and dist
         (list (expand-file-name base dist)
               (expand-file-name (concat base ".sha256") dist)
               (expand-file-name (concat base ".sig") dist)))))

(defun nelisp-integration--release-artifact-recompute-sha256 (tarball)
  "Return the lowercase hex SHA-256 of TARBALL, or nil on failure.
Uses Emacs' built-in `secure-hash' so no external tool is required —
the verifier still runs cleanly under emacs --batch on a host without
sha256sum / shasum on PATH."
  (condition-case _err
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally tarball)
        (downcase (secure-hash 'sha256 (current-buffer))))
    (error nil)))

(defun nelisp-integration--release-artifact-parse-sha256-line (line)
  "Extract the hex SHA-256 from LINE in the `<hash>  <file>' format.
Returns the lowercase hash string when LINE matches the standard
sha256sum / shasum line shape; nil otherwise."
  (when (and (stringp line)
             (string-match "\\`\\([0-9a-fA-F]\\{64\\}\\)[ \t]" line))
    (downcase (match-string 1 line))))

(defun nelisp-integration--release-artifact-read-checksum (checksum-file)
  "Read CHECKSUM-FILE and return the embedded hex SHA-256 or nil."
  (when (and checksum-file (file-readable-p checksum-file))
    (with-temp-buffer
      (insert-file-contents checksum-file)
      (let ((line (buffer-substring-no-properties
                   (point-min)
                   (line-end-position))))
        (nelisp-integration--release-artifact-parse-sha256-line line)))))

(defun nelisp-integration--release-artifact-validate-signature (sig-file
                                                                version
                                                                platform)
  "Validate the ad-hoc signature payload at SIG-FILE for VERSION × PLATFORM.
Returns one of:
  - t                         when the file exists and the payload
                              starts with the documented prefix
                              (Doc 32 v2 §2.5 ad-hoc placeholder).
  - `:missing'                when the file does not exist.
  - `:malformed'              when the file exists but the payload
                              does not match the prefix shape.
  - `:platform-mismatch'      when the payload references a different
                              platform / version pair than requested."
  (cond
   ((not (and sig-file (file-readable-p sig-file)))
    :missing)
   (t
    (let* ((body (with-temp-buffer
                   (insert-file-contents sig-file)
                   (buffer-substring-no-properties (point-min) (point-max))))
           (line (car (split-string body "\n" t))))
      (cond
       ((null line) :malformed)
       ((not (string-prefix-p "ad-hoc-signature " line)) :malformed)
       ;; Payload shape: `ad-hoc-signature <version> <platform> <iso8601>'
       ((not (and (string-match-p (regexp-quote (concat " " version " "))
                                  line)
                  (string-match-p (regexp-quote (concat " " platform " "))
                                  line)))
        :platform-mismatch)
       (t t))))))

(defun nelisp-integration-release-artifact-verify (&optional version platform)
  "Verify the Phase 7.5.3 release artifact for VERSION × PLATFORM.

VERSION defaults to `nelisp-integration-release-artifact-default-version'
(currently `stage-d-v2.0'); PLATFORM defaults to `linux-x86_64' which
matches the §11 blocker tier.  The verifier checks:

  1. `dist/' is present (= `make release-artifact' was at least
     attempted in this worktree).
  2. The tarball, .sha256, and .sig files all exist for the
     (version, platform) tuple.
  3. The recomputed SHA-256 of the tarball matches the value in
     the .sha256 file.
  4. The .sig file carries the documented ad-hoc payload for the
     correct (version, platform) tuple (Doc 32 v2 §2.5 placeholder
     until v2.1 GPG signing).

Returns a plist:
  (:ready t | nil
   :version VERSION
   :platform PLATFORM
   :tarball PATH-OR-NIL
   :checksum PATH-OR-NIL
   :signature PATH-OR-NIL
   :checksum-recomputed HEX-OR-NIL
   :checksum-recorded   HEX-OR-NIL
   :signature-status    t | :missing | :malformed | :platform-mismatch
   :missing  ((REASON-KEY . DETAIL) ...))

Reasons in `:missing' include `:dist-dir-missing',
`:tarball-missing', `:checksum-missing', `:checksum-mismatch',
`:signature-missing', `:signature-malformed', and
`:signature-platform-mismatch'.  The function never raises; on every
failure path it returns `:ready nil' with a populated `:missing' list
so bin/anvil callers can triage without a `condition-case' wrapper.

This is the Phase 7.5.3 prep entry point — the *real* GPG signature
verification + cdylib symbol probe land when v2.1 ships per Doc 32
v2 §8 / §2.5.  Until then the ad-hoc signature placeholder is the
only thing the verifier asserts on, and `--no-emacs' will not gate on
this verifier (the cold-init coordinator at §3.2 is the operative
gate).  Phase 7.5.3 wires the verifier into the release-artifact CI
matrix so a malformed tarball never reaches GitHub Releases."
  (let* ((version (or version
                      nelisp-integration-release-artifact-default-version))
         (platform (or platform "linux-x86_64"))
         (paths (nelisp-integration--release-artifact-paths version platform))
         missing tarball checksum signature
         recomputed recorded sig-status)
    (cond
     ((null paths)
      ;; dist/ itself missing — short-circuit with a single reason.
      (push (cons :dist-dir-missing
                  "no `dist/' directory found under worktree root — \
run `make release-artifact'")
            missing)
      (list :ready nil
            :version version
            :platform platform
            :tarball nil
            :checksum nil
            :signature nil
            :checksum-recomputed nil
            :checksum-recorded nil
            :signature-status :missing
            :missing (nreverse missing)))
     (t
      (setq tarball   (nth 0 paths)
            checksum  (nth 1 paths)
            signature (nth 2 paths))
      ;; (a) tarball.
      (unless (file-readable-p tarball)
        (push (cons :tarball-missing
                    (format "expected tarball at %s — \
run `make release-artifact PLATFORM=%s'" tarball platform))
              missing))
      ;; (b) checksum file + recompute + compare.
      (cond
       ((not (file-readable-p checksum))
        (push (cons :checksum-missing
                    (format "expected checksum at %s" checksum))
              missing))
       ((not (file-readable-p tarball))
        ;; Cannot recompute without the tarball; already noted above.
        nil)
       (t
        (setq recorded (nelisp-integration--release-artifact-read-checksum
                        checksum))
        (setq recomputed (nelisp-integration--release-artifact-recompute-sha256
                          tarball))
        (cond
         ((null recorded)
          (push (cons :checksum-malformed
                      (format "could not parse hex SHA-256 from %s"
                              checksum))
                missing))
         ((null recomputed)
          (push (cons :checksum-recompute-failed
                      (format "could not recompute SHA-256 of %s"
                              tarball))
                missing))
         ((not (string= recorded recomputed))
          (push (cons :checksum-mismatch
                      (format "recorded=%s recomputed=%s"
                              recorded recomputed))
                missing)))))
      ;; (c) signature.
      (setq sig-status
            (nelisp-integration--release-artifact-validate-signature
             signature version platform))
      (pcase sig-status
        (:missing
         (push (cons :signature-missing
                     (format "expected signature at %s" signature))
               missing))
        (:malformed
         (push (cons :signature-malformed
                     (format "signature payload at %s does not match \
`ad-hoc-signature <version> <platform> <iso8601>' shape"
                             signature))
               missing))
        (:platform-mismatch
         (push (cons :signature-platform-mismatch
                     (format "signature payload at %s does not reference \
version=%s platform=%s" signature version platform))
               missing))
        (_ nil))
      (list :ready (null missing)
            :version version
            :platform platform
            :tarball tarball
            :checksum checksum
            :signature signature
            :checksum-recomputed recomputed
            :checksum-recorded recorded
            :signature-status sig-status
            :missing (nreverse missing))))))

(provide 'nelisp-integration)
;;; nelisp-integration.el ends here
