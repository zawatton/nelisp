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

;;;; Phase 7.5.2 — stage 1 real handler (Doc 32 v2 §3.2 / §4)
;;
;; Stage 1 = "Phase 7.0 syscall-stub init (mmap arena ready)" per Doc 32
;; v2 §3.2 + §4 architecture diagram.  The closed Phase 7.0 (= 7df50f8
;; main + 20ecef4 MAP_JIT update, ~819 LOC Rust) ships:
;;
;;   - `libnelisp_runtime.{a,so}'   — cdylib + staticlib with the
;;                                     `nelisp_syscall_*' surface.
;;   - `nelisp-runtime' CLI         — `--syscall-smoke' subcommand that
;;                                     exercises write / mmap / munmap /
;;                                     getenv / stat through the FFI
;;                                     symbols (exit 0 = green).
;;   - `nelisp-runtime-module.so'   — Phase 7.5.4 Emacs module that
;;                                     dlsyms the cdylib in-process and
;;                                     exposes `nelisp-runtime-module-
;;                                     syscall-smoke' (no subprocess hop).
;;
;; The stage-1 handler probes both the in-process module path (preferred
;; ~10 µs) and the subprocess path (fallback ~1 ms), invokes the
;; available syscall-smoke entry point, and reports the discovered
;; artifacts on `:status pass'.  The mmap arena itself (= the ~64MB
;; nursery + ~256MB tenured slot in the §4 diagram) is *advertised* as
;; ready by Phase 7.0 (the syscall surface is wired) but its actual
;; reservation lands in stage 2 (= Phase 7.2 allocator init), so the
;; handler does not pre-allocate — that would conflate stage 1 and
;; stage 2 ownership boundaries.

(defun nelisp-integration--cold-init-stage1-probe-artifacts ()
  "Probe Phase 7.0 cdylib artifacts for the stage-1 handler.
Returns a plist:
  (:binary PATH-OR-NIL          ; nelisp-runtime CLI binary
   :module PATH-OR-NIL          ; nelisp-runtime-module.so
   :staticlib PATH-OR-NIL       ; libnelisp_runtime.a
   :cdylib PATH-OR-NIL)         ; libnelisp_runtime.so (sibling of module)
Each value is an absolute path string when the artifact is on disk and
readable, otherwise nil.  No artifact reachability raises — every
locator failure short-circuits to nil so the handler can compose a
single missing-artifacts diagnostic."
  (let* ((bin (nelisp-integration--locate-runtime-binary))
         (lib (nelisp-integration--locate-runtime-staticlib))
         (mod (condition-case _err
                  (let ((p (nelisp-cc-runtime--locate-runtime-module)))
                    (and (stringp p) (file-readable-p p) p))
                (error nil)))
         (cdy (and mod
                   (let ((so (expand-file-name "libnelisp_runtime.so"
                                               (file-name-directory mod))))
                     (and (file-readable-p so) so)))))
    (list :binary bin
          :module mod
          :staticlib lib
          :cdylib cdy)))

(defun nelisp-integration--cold-init-stage1-via-module ()
  "Try to invoke `nelisp-runtime-module-syscall-smoke' in-process.
Returns one of:
  (:ok    :exit-code N)        ; module loaded + smoke returned N (0 = green)
  (:no-module REASON)          ; module unavailable (host lacks dynamic
                               ; modules, .so missing, or load failed)
  (:error MSG)                 ; module loaded but smoke raised; MSG is
                               ; the `error-message-string' payload
The function never raises — every error path is captured into the
returned tuple so the stage-1 handler can decide between fallback +
fail without a `condition-case' around its own dispatcher."
  (cond
   ((not (nelisp-cc-runtime--module-supported-p))
    (list :no-module "host Emacs lacks dynamic-module support"))
   (t
    (condition-case err
        (progn
          (nelisp-cc-runtime--ensure-module-loaded)
          (if (fboundp 'nelisp-runtime-module-syscall-smoke)
              (let ((rc (funcall (intern "nelisp-runtime-module-syscall-smoke"))))
                (list :ok :exit-code rc))
            (list :no-module
                  "module loaded but `nelisp-runtime-module-syscall-smoke' unbound (stale .so?)")))
      (error
       ;; Differentiate "module never loaded" (= no-module) from
       ;; "module loaded but smoke raised" (= error).  The
       ;; `--ensure-module-loaded' helper signals `nelisp-cc-runtime-
       ;; module-{missing,unsupported}' for the former, plain `error'
       ;; for genuine smoke failures.
       (let* ((sym (car err))
              (kind (if (memq sym '(nelisp-cc-runtime-module-missing
                                    nelisp-cc-runtime-module-unsupported))
                        :no-module
                      :error))
              (msg (error-message-string err)))
         (list kind msg)))))))

(defun nelisp-integration--cold-init-stage1-via-subprocess (binary)
  "Invoke `BINARY --syscall-smoke' as a subprocess and parse exit code.
Returns one of:
  (:ok    :exit-code N)        ; subprocess exited with N (0 = green)
  (:error MSG)                 ; subprocess could not be started
BINARY is an absolute path string returned by
`nelisp-integration--locate-runtime-binary'; callers that pass nil get
back a `:error' tuple rather than a crash."
  (cond
   ((not (and binary (file-executable-p binary)))
    (list :error
          (format "nelisp-runtime binary not executable: %S" binary)))
   (t
    (condition-case err
        (with-temp-buffer
          (let ((exit (call-process binary nil t nil "--syscall-smoke")))
            (list :ok :exit-code exit)))
      (error
       (list :error (error-message-string err)))))))

(defun nelisp-integration-cold-init-stage1-handler (&optional _ctx)
  "Phase 7.5.2 stage-1 real handler — wires Phase 7.0 syscall-stub init.

Doc 32 v2 §3.2 + §4 specifies stage 1 as `Phase 7.0 syscall-stub init
\(mmap arena ready)\\='; the closed Phase 7.0 ships the `nelisp-runtime'
cdylib + CLI + Emacs module that exposes `nelisp_syscall_*' through
both subprocess (`--syscall-smoke') and in-process module
\(`nelisp-runtime-module-syscall-smoke') paths.  This handler:

  1. probes for the runtime artifacts (binary / module / staticlib / cdylib),
  2. runs the syscall-smoke probe through the in-process module path
     when available (preferred — ~10 µs round-trip),
  3. falls back to the subprocess path when the module is unreachable
     (stale build, host without dynamic modules, etc.) — ~1 ms,
  4. returns the discovered artifacts so callers can introspect which
     path was used + which dependencies were located.

Returns a plist with the Doc 32 v2 §3.2 stage shape:
  (:status pass
   :stage 1
   :module-loaded BOOL          ; t when in-process module path was used
   :exec-mode :module | :subprocess
   :artifacts (:binary PATH :module PATH :staticlib PATH :cdylib PATH)
   :smoke-exit-code N)
on success, or
  (:status fail
   :stage 1
   :error MSG
   :artifacts (:binary PATH ...)
   :module-result MODULE-TUPLE
   :subprocess-result SUBPROCESS-TUPLE-OR-NIL)
on failure.

The handler never raises; every error path is captured into the `:error'
field so the dispatcher's `condition-case' wrapper sees a normal
return.  CTX is currently unused (reserved for future stage-state
threading; the dispatcher already supplies it for forward-compat)."
  (let* ((artifacts (nelisp-integration--cold-init-stage1-probe-artifacts))
         (binary (plist-get artifacts :binary))
         (module-result (nelisp-integration--cold-init-stage1-via-module)))
    (cond
     ;; (a) in-process module path returned green (= preferred fast path).
     ((and (eq (car module-result) :ok)
           (eq (cadr module-result) :exit-code)
           (eq (nth 2 module-result) 0))
      (list :status 'pass
            :stage 1
            :module-loaded t
            :exec-mode :module
            :artifacts artifacts
            :smoke-exit-code 0))
     ;; (b) module reachable but smoke returned non-zero — hard fail
     ;; (the module path is the most direct probe; non-zero means the
     ;; FFI surface is broken on this host).
     ((and (eq (car module-result) :ok)
           (not (eq (nth 2 module-result) 0)))
      (list :status 'fail
            :stage 1
            :error (format "syscall-smoke (module path) returned non-zero: %s"
                           (nth 2 module-result))
            :artifacts artifacts
            :module-result module-result
            :subprocess-result nil))
     ;; (c) module raised — same hard-fail semantics as (b) so a smoke
     ;; failure cannot silently downgrade to a subprocess-path success.
     ((eq (car module-result) :error)
      (list :status 'fail
            :stage 1
            :error (format "syscall-smoke (module path) raised: %s"
                           (cadr module-result))
            :artifacts artifacts
            :module-result module-result
            :subprocess-result nil))
     ;; (d) module unavailable → try subprocess fallback.
     (t
      (let ((sub (nelisp-integration--cold-init-stage1-via-subprocess binary)))
        (cond
         ((and (eq (car sub) :ok)
               (eq (cadr sub) :exit-code)
               (eq (nth 2 sub) 0))
          (list :status 'pass
                :stage 1
                :module-loaded nil
                :exec-mode :subprocess
                :artifacts artifacts
                :smoke-exit-code 0))
         ((eq (car sub) :ok)
          (list :status 'fail
                :stage 1
                :error (format "syscall-smoke (subprocess path) exited with %s"
                               (nth 2 sub))
                :artifacts artifacts
                :module-result module-result
                :subprocess-result sub))
         (t
          (list :status 'fail
                :stage 1
                :error (format "no usable runtime path: module=%s subprocess=%s"
                               (cadr module-result) (cadr sub))
                :artifacts artifacts
                :module-result module-result
                :subprocess-result sub))))))))

(defun nelisp-integration-cold-init-handlers-with-stage1-real ()
  "Return a `:handlers' alist whose stage1 entry is the real handler.
Convenience wrapper for callers that want stage-1 to actually exercise
the Phase 7.0 syscall surface while leaving stages 2/3/4 as stubs (=
`nelisp-integration--cold-init-stub-handler' returning the documented
`:stub-not-yet-impl-pending-phase-X.Y' keyword).  Stages 2/3/4 lift to
real handlers when Phase 7.2 / 7.4 / 7.1 close, respectively (see
`nelisp-integration-cold-init-stage-blocked-by')."
  (let ((handlers (nelisp-integration--cold-init-default-handlers)))
    (setf (alist-get 'stage1 handlers)
          #'nelisp-integration-cold-init-stage1-handler)
    handlers))

;;;; Phase 7.5.2 — stage 2 real handler (Phase 7.2 allocator init)
;;
;; Stage 2 = "Phase 7.2 allocator + GC scheduler init" per Doc 32 v2
;; §3.2 + §4.  The real handler initialises a fresh nursery via
;; `nelisp-allocator-init-nursery' (the canonical "exactly once at
;; startup" entry point per the function docstring), then verifies the
;; heap-region registry version (Doc 29 §1.4) across every registered
;; region.  Any version skew between the running allocator module and
;; the registered regions short-circuits to :status fail with a
;; structured diagnostic so the cold-init coordinator does not hand a
;; corrupt heap to stage 3 / stage 4.
;;
;; The handler restores the *previous* nursery binding on exit so
;; running it inside the dispatcher does not leak state across the
;; surrounding ERT / production session — production callers that own
;; the global lifetime invoke `nelisp-allocator-init-nursery' directly
;; and skip this dispatcher path entirely.

(defun nelisp-integration--cold-init-stage2-allocator-loaded-p ()
  "Return non-nil when the Phase 7.2 allocator module is loadable."
  (or (featurep 'nelisp-allocator)
      (condition-case _err
          (require 'nelisp-allocator nil t)
        (error nil))))

(defun nelisp-integration-cold-init-stage2-handler (&optional _ctx)
  "Phase 7.5.2 stage-2 real handler — Phase 7.2 allocator + region init.

Doc 32 v2 §3.2 + §4 specifies stage 2 as `Phase 7.2 allocator + GC
scheduler init'.  The handler:

  1. requires the `nelisp-allocator' module (= Phase 7.2 entry point;
     pure Elisp, so it loads on every host that ships `src/'),
  2. snapshots the current `nelisp-allocator--current-nursery' so the
     dispatcher run does not leak state into the surrounding session,
  3. invokes `nelisp-allocator-init-nursery' to reserve a fresh nursery
     region (Doc 29 §1.4 heap-region registry entry, generation =
     `nursery'),
  4. verifies the heap-region registry version across every registered
     region via `nelisp-allocator-region-table-check-versions' (Doc 29
     §1.4 — silent corruption risk per Doc 30 v2 §6.4 if skew exists),
  5. records the nursery descriptor + region snapshot into the result
     plist so stage 3 / stage 4 / production callers can introspect
     what the handler reserved,
  6. restores the prior nursery binding on exit (production code owns
     the global lifetime and will not invoke this through the scaffold
     dispatcher).

Returns a plist on success:
  (:status pass
   :stage 2
   :nursery-size BYTES
   :region-version INT
   :region-count INT
   :regions PLIST-LIST)
or on failure:
  (:status fail
   :stage 2
   :error MSG
   :error-data DATA-OR-NIL)

Never raises — every error path is captured into the `:error' field
so the dispatcher's `condition-case' wrapper sees a normal return.

Skips with `:status :stub-not-yet-impl-pending-phase-7.2' (= the
documented stub keyword) when `nelisp-allocator' is unavailable so
the dispatcher contract degrades gracefully on a stripped tarball
that omits the allocator module."
  (cond
   ((not (nelisp-integration--cold-init-stage2-allocator-loaded-p))
    (list :status (cdr (assq 'stage2
                             nelisp-integration-cold-init-stage-pending-keyword))
          :stage 2
          :blocked-by (cdr (assq 'stage2
                                 nelisp-integration-cold-init-stage-blocked-by))))
   (t
    (let ((prior-nursery (and (boundp 'nelisp-allocator--current-nursery)
                              nelisp-allocator--current-nursery))
          result)
      (setq result
            (condition-case err
                (let* ((nursery (nelisp-allocator-init-nursery))
                       (region (nelisp-allocator--nursery-region nursery))
                       (size (- (nelisp-heap-region-end region)
                                (nelisp-heap-region-start region)))
                       (snapshot (nelisp-allocator-region-table-check-versions))
                       (regions-plist (nelisp-allocator-snapshot-regions)))
                  (list :status 'pass
                        :stage 2
                        :nursery-size size
                        :region-version nelisp-heap-region-version
                        :region-count (length snapshot)
                        :regions regions-plist))
              (error
               (list :status 'fail
                     :stage 2
                     :error (error-message-string err)
                     :error-data (cdr err)))))
      ;; Restore prior nursery to keep dispatcher invocations
      ;; idempotent across the surrounding ERT / session.
      (when (boundp 'nelisp-allocator--current-nursery)
        (setq nelisp-allocator--current-nursery prior-nursery))
      result))))

;;;; Phase 7.5.2 — stage 3 real handler (Phase 7.4 coding init)
;;
;; Stage 3 = "Phase 7.4 coding (UTF-8 IO codec attach)" per Doc 32 v2
;; §3.2 + §4.  Phase 7.4 closure shipped four IO codecs (UTF-8,
;; Latin-1, Shift-JIS / CP932, EUC-JP) plus the JIS table golden
;; SHA-256 (Doc 31 v2 §6.10).  The stage-3 handler verifies:
;;
;;   - the JIS tables match the golden hash (= no silent table
;;     mutation since the last `tools/coding-table-gen.el' run),
;;   - a UTF-8 round-trip works on a Japanese seed string (= the
;;     codec surface is wired and runtime-callable, not just
;;     loadable as data).
;;
;; Both checks are pure Elisp + the loaded coding tables; no cdylib
;; / module dependency.  When `nelisp-coding-jis-tables' is absent
;; the handler degrades to the documented stub keyword so stripped
;; tarballs do not trip the dispatcher.

(defconst nelisp-integration--cold-init-stage3-utf8-roundtrip-seed
  "日本語テスト string — \U0001F600 emoji"
  "Seed string for the stage-3 UTF-8 round-trip probe.
Mixes BMP CJK + ASCII + a non-BMP emoji so the encode / decode path
exercises 1-, 3-, and 4-byte UTF-8 sequences.")

(defun nelisp-integration--cold-init-stage3-coding-loaded-p ()
  "Return non-nil when the Phase 7.4 coding modules are loadable."
  (and (or (featurep 'nelisp-coding)
           (condition-case _err
               (require 'nelisp-coding nil t)
             (error nil)))
       (or (featurep 'nelisp-coding-jis-tables)
           (condition-case _err
               (require 'nelisp-coding-jis-tables nil t)
             (error nil)))))

(defun nelisp-integration-cold-init-stage3-handler (&optional _ctx)
  "Phase 7.5.2 stage-3 real handler — Phase 7.4 coding init + golden hash.

Doc 32 v2 §3.2 + §4 specifies stage 3 as `Phase 7.4 coding (UTF-8 IO
codec attach)'.  The handler:

  1. requires `nelisp-coding' + `nelisp-coding-jis-tables' (Phase 7.4
     closure modules; pure Elisp),
  2. verifies the JIS tables match the golden SHA-256
     (`nelisp-coding-jis-tables-verify-hash' per Doc 31 v2 §6.10) —
     guards against silent table mutation since the last
     `tools/coding-table-gen.el' run,
  3. round-trips a CJK + emoji seed string through
     `nelisp-coding-utf8-encode-string' →
     `nelisp-coding-utf8-decode' to prove the codec surface is
     callable, not merely loadable as data.

Returns a plist on success:
  (:status pass
   :stage 3
   :jis-tables-hash GOLDEN-SHA256-STRING
   :utf8-roundtrip-bytes-len INT
   :utf8-roundtrip-restored STRING)
or on failure:
  (:status fail
   :stage 3
   :error MSG
   :error-data DATA-OR-NIL)

Never raises.  Degrades to `:status :stub-not-yet-impl-pending-phase-7.4'
when the coding modules are unavailable (= stripped tarball)."
  (cond
   ((not (nelisp-integration--cold-init-stage3-coding-loaded-p))
    (list :status (cdr (assq 'stage3
                             nelisp-integration-cold-init-stage-pending-keyword))
          :stage 3
          :blocked-by (cdr (assq 'stage3
                                 nelisp-integration-cold-init-stage-blocked-by))))
   (t
    (condition-case err
        (let* ((golden-ok (nelisp-coding-jis-tables-verify-hash))
               (seed nelisp-integration--cold-init-stage3-utf8-roundtrip-seed)
               (encoded (nelisp-coding-utf8-encode-string seed))
               ;; `nelisp-coding-utf8-decode' returns a plist with
               ;; the decoded string under :string per Doc 31 v2 §2.3
               ;; (the plist surface preserves invalid-byte positions
               ;; for streaming callers).
               (decode-plist (nelisp-coding-utf8-decode encoded))
               (decoded (and (listp decode-plist)
                             (plist-get decode-plist :string)))
               (round-ok (and (stringp decoded)
                              (string= decoded seed))))
          (cond
           ((not (eq golden-ok t))
            (list :status 'fail
                  :stage 3
                  :error "JIS tables golden hash mismatch"
                  :error-data golden-ok))
           ((not round-ok)
            (list :status 'fail
                  :stage 3
                  :error (format "UTF-8 round-trip mismatch: %S vs %S"
                                 seed decoded)
                  :error-data nil))
           (t
            (list :status 'pass
                  :stage 3
                  :jis-tables-hash nelisp-coding-jis-tables-sha256
                  :utf8-roundtrip-bytes-len (length encoded)
                  :utf8-roundtrip-restored decoded))))
      (error
       (list :status 'fail
             :stage 3
             :error (error-message-string err)
             :error-data (cdr err)))))))

;;;; Phase 7.5.2 — stage 4 real handler (Phase 7.1 native compile bootstrap)
;;
;; Stage 4 = "Phase 7.1 native compiler bootstrap (= Doc 28 §3.5
;; 4-stage embed)" per Doc 32 v2 §3.2 + §4.  The real coordinator at
;; `nelisp-integration-cold-init-run' already runs the full T12
;; protocol; the stage-4 dispatcher handler is a *narrower* probe
;; that calls `nelisp-cc-bootstrap-run' on a tiny seed lambda and
;; verifies the bootstrap produced bytes (= stage-1 native compile
;; emitted a non-empty `:final-bytes' vector).  This separation lets
;; the scaffold dispatcher exercise the *contract* (a stage-4 handler
;; that returns pass with native bytes) without the full 4-stage
;; pipeline overhead — the cold-init coordinator's tests cover the
;; protocol-level pipeline.
;;
;; The handler skip-degrades when the runtime binary is missing (=
;; verifier reports :ready nil with anything other than the advisory
;; staticlib entry) so a stripped tarball does not flake the
;; dispatcher.

(defconst nelisp-integration--cold-init-stage4-seed-form
  '(lambda (x) x)
  "Seed form for the stage-4 native compile bootstrap probe.
Identity lambda — smallest non-trivial form the T6 frontend lowers
cleanly through `nelisp-cc-bootstrap--stage1-native-compile'.  Real
production callers (`nelisp-integration-cold-init-run') pass the
canonical bootstrap source path; the scaffold-layer stage-4 handler
runs against this seed because it must complete in <1s for the soak
harness inline.")

(defun nelisp-integration--cold-init-stage4-bootstrap-loaded-p ()
  "Return non-nil when the Phase 7.1 native compile bootstrap is loadable."
  (or (featurep 'nelisp-cc-bootstrap)
      (condition-case _err
          (require 'nelisp-cc-bootstrap nil t)
        (error nil))))

(defun nelisp-integration-cold-init-stage4-handler (&optional _ctx)
  "Phase 7.5.2 stage-4 real handler — Phase 7.1 native compile bootstrap.

Doc 32 v2 §3.2 + §4 specifies stage 4 as the Phase 7.1 native
compiler bootstrap (= Doc 28 §3.5 4-stage embed).  The handler:

  1. requires `nelisp-cc-bootstrap' (Phase 7.1 closure entry point),
  2. runs `nelisp-cc-bootstrap-run' on a tiny identity-lambda seed,
  3. asserts the returned plist carries a non-empty
     `:a3-candidate-hash' (= stage-1 native compile actually
     produced bytes; an empty hash means the bootstrap fell back to
     the A2 path and stage-4 should report fail).

Returns a plist on success:
  (:status pass
   :stage 4
   :bootstrap-status pass | fail   ; T12's umbrella status
   :a2-hash STRING                 ; stage-0 hash
   :a3-candidate-hash STRING       ; stage-1 hash (non-nil = bytes emitted)
   :authoritative-promoted BOOL
   :elapsed-seconds FLOAT)
or on failure:
  (:status fail
   :stage 4
   :error MSG
   :bootstrap-result PLIST-OR-NIL)

Never raises.  Degrades to `:status :stub-not-yet-impl-pending-phase-7.1'
when the bootstrap module is unavailable.

Skip-degrades to the same stub keyword when the runtime binary is
unbuilt — the bootstrap requires the T13 binary to exercise its
real pipeline, and absent the binary `nelisp-cc-bootstrap-run' would
fall through to a fallback path that does not emit native bytes.
Production callers route through `nelisp-integration-cold-init-run'
(which surfaces the verifier's :missing list directly) when they
need the binary-aware path."
  (cond
   ((not (nelisp-integration--cold-init-stage4-bootstrap-loaded-p))
    (list :status (cdr (assq 'stage4
                             nelisp-integration-cold-init-stage-pending-keyword))
          :stage 4
          :blocked-by (cdr (assq 'stage4
                                 nelisp-integration-cold-init-stage-blocked-by))))
   ((not (nelisp-integration--locate-runtime-binary))
    (list :status (cdr (assq 'stage4
                             nelisp-integration-cold-init-stage-pending-keyword))
          :stage 4
          :blocked-by "nelisp-runtime binary missing — run `make runtime'"))
   (t
    (condition-case err
        (let* ((start (float-time))
               (boot (nelisp-cc-bootstrap-run
                      nelisp-integration--cold-init-stage4-seed-form))
               (a3 (plist-get boot :a3-candidate-hash))
               (a3-ok (and (stringp a3) (= (length a3) 64))))
          (cond
           ((not a3-ok)
            (list :status 'fail
                  :stage 4
                  :error (format "stage-1 native compile produced no bytes (a3-hash=%S)"
                                 a3)
                  :bootstrap-result boot))
           (t
            (list :status 'pass
                  :stage 4
                  :bootstrap-status (plist-get boot :status)
                  :a2-hash (plist-get boot :a2-hash)
                  :a3-candidate-hash a3
                  :authoritative-promoted
                  (plist-get boot :authoritative-promoted)
                  :elapsed-seconds (- (float-time) start)))))
      (error
       (list :status 'fail
             :stage 4
             :error (error-message-string err)
             :bootstrap-result nil))))))

(defun nelisp-integration-cold-init-handlers-with-real-stages ()
  "Return a `:handlers' alist with stages 1/2/3/4 wired to real handlers.
Convenience wrapper for callers that want every stage to exercise its
real Phase 7.X surface.  Each handler degrades gracefully when its
dependency is unavailable (= returns the documented
`:stub-not-yet-impl-pending-phase-X.Y' keyword for that stage) so the
dispatcher contract still holds on stripped tarballs / hosts without
the runtime binary."
  (list (cons 'stage1 #'nelisp-integration-cold-init-stage1-handler)
        (cons 'stage2 #'nelisp-integration-cold-init-stage2-handler)
        (cons 'stage3 #'nelisp-integration-cold-init-stage3-handler)
        (cons 'stage4 #'nelisp-integration-cold-init-stage4-handler)))

;;;; Phase 7.5.3 dependency — 24h soak harness (real GC/RSS measurement)
;;
;; The real 24h soak (= Doc 32 v2 §2.7 / §7) measures GC pause p99,
;; RSS growth, and crash count over a 24h window — the static-linked
;; stage-d-v2.0 binary lands in Phase 7.5.3 (the canonical soak target)
;; but the *measurement scaffolding* lands here in Phase 7.5.3 prep so
;; the metric-collection contract is exercised before the binary swaps
;; in.  This harness:
;;
;;   - runs the configured per-iteration handler N times,
;;   - captures Emacs `gcs-done' delta + `gc-elapsed' delta around
;;     each iteration to derive the per-iteration GC pause,
;;   - captures `memory-info' (Linux) or `process-attributes' RSS
;;     when available so RSS growth across the soak is observable,
;;   - aggregates min/max/mean over the iteration count.
;;
;; Real binary-driven soak in Phase 7.5.3 will swap the per-iteration
;; handler for one that drives the static binary in a subprocess + a
;; sampler that scrapes child-process metrics; the harness contract
;; (= the returned plist shape) does not change because the collector
;; is already structured as `gc-stats' / `rss-stats' nested plists.

(defconst nelisp-integration-soak-harness-default-iterations 4
  "Default iteration count for the Phase 7.5.3 soak harness.
Tiny on purpose — Phase 7.5.2 ERT runs the harness inline so the
default cannot be a 24h figure.  Real soak runs override this with
`:iterations 86400' (1Hz × 24h) or a duration-driven loop.")

(defun nelisp-integration--soak-rss-bytes ()
  "Return the current Emacs process RSS in bytes, or nil when unavailable.
Tries `memory-info' first (Linux + Emacs 27+); falls back to
`process-attributes' (cross-platform when /proc is mounted) and
multiplies by the page size.  Returns nil on platforms that expose
neither so the soak harness reports `:rss-stats nil' rather than
fabricating zeros."
  (or
   ;; (a) `memory-info' — preferred when the host exposes it.  The
   ;; return shape is `(TOTAL-KB FREE-KB SWAP-TOTAL-KB SWAP-FREE-KB)'
   ;; when emacs is built against system memory probes; nil otherwise.
   ;; This gives us system-wide totals, not per-process RSS, so we
   ;; only use it when `process-attributes' is unavailable.
   (let* ((attrs (and (fboundp 'process-attributes)
                      (ignore-errors
                        (process-attributes (emacs-pid)))))
          (rss-pages (and attrs (alist-get 'vsize attrs)))
          ;; `vsize' is in bytes on most systems; `rss' in pages.
          (rss-pages-2 (and attrs (alist-get 'rss attrs))))
     (cond
      ((and rss-pages-2 (integerp rss-pages-2))
       ;; `process-attributes' returns RSS in pages on Linux; multiply
       ;; by the page size to get bytes.
       (* rss-pages-2 4096))
      ((and rss-pages (integerp rss-pages))
       rss-pages)
      (t nil)))
   ;; (b) Fallback: read /proc/self/status VmRSS line directly.  This
   ;; works on Linux even when `process-attributes' is unavailable.
   (and (file-readable-p "/proc/self/status")
        (with-temp-buffer
          (insert-file-contents "/proc/self/status")
          (goto-char (point-min))
          (when (re-search-forward "^VmRSS:[ \t]+\\([0-9]+\\)[ \t]+kB" nil t)
            (* (string-to-number (match-string 1)) 1024))))))

(defun nelisp-integration--soak-aggregate-stats (samples)
  "Return min/max/mean over SAMPLES (a list of numbers, possibly empty/nil).
Returns nil when SAMPLES is empty.  Otherwise returns a plist:
  (:min N :max N :mean FLOAT :samples LENGTH :first VAL :last VAL)
with `nil' entries filtered out before aggregation so a missing RSS
sample (= nil) does not poison the aggregate."
  (let ((numeric (cl-remove-if-not #'numberp samples)))
    (cond
     ((null numeric) nil)
     (t
      (list :min (apply #'min numeric)
            :max (apply #'max numeric)
            :mean (/ (apply #'+ numeric) (float (length numeric)))
            :samples (length numeric)
            :first (car samples)
            :last (car (last samples)))))))

(defun nelisp-integration-soak-harness (&rest args)
  "Phase 7.5.3 24h soak harness with real GC + RSS measurement.

ARGS is a plist:
  :iterations N            — number of harness iterations (default
                             `--soak-harness-default-iterations').
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
   :gc-stats (:min N :max N :mean FLOAT :samples N
              :first N :last N)        ; gcs-done delta per iteration
   :gc-elapsed-stats (... same shape ...)  ; gc-elapsed delta per iter
   :rss-stats (... same shape ...) | nil   ; process RSS in bytes
   :scaffold t              ; sentinel: kept for backwards-compat with
                            ; the Phase 7.5.2 caller contract; soak
                            ; metric collection lands here in Phase
                            ; 7.5.3 prep, real binary-driven soak in
                            ; Phase 7.5.3 ship still toggles this flag
                            ; off when wired
   :status pass | fail)     ; pass iff all iterations passed

The function never raises; handler errors are captured into the
per-iteration result via `nelisp-integration--cold-init-run-stage'-style
condition-case wrapping inside the handler itself (the dispatcher
already handles this).

Real measurement notes:
  - GC delta = (`gcs-done' AFTER) - (`gcs-done' BEFORE) per iteration.
  - GC pause = (`gc-elapsed' AFTER) - (`gc-elapsed' BEFORE) per
    iteration (seconds, float).
  - RSS = `process-attributes' rss field × page size when available,
    or /proc/self/status VmRSS line as a Linux fallback; nil on
    platforms that expose neither (= `:rss-stats nil')."
  (let* ((iterations (or (plist-get args :iterations)
                         nelisp-integration-soak-harness-default-iterations))
         (handler (or (plist-get args :handler)
                      (lambda (_i) (nelisp-integration-cold-init-dispatch))))
         (stop-on-fail (plist-get args :stop-on-fail))
         (start (float-time))
         (passed 0)
         (failed 0)
         (first-failure nil)
         (gc-deltas nil)
         (gc-elapsed-deltas nil)
         (rss-samples nil))
    (catch 'soak-harness-stop
      (dotimes (i iterations)
        (let* ((gcs-before gcs-done)
               (gc-elapsed-before gc-elapsed)
               (r (condition-case err
                      (funcall handler i)
                    (error (list :status 'fail :error err))))
               (status (and (listp r) (plist-get r :status)))
               (gcs-after gcs-done)
               (gc-elapsed-after gc-elapsed)
               (rss (nelisp-integration--soak-rss-bytes)))
          (push (- gcs-after gcs-before) gc-deltas)
          (push (- gc-elapsed-after gc-elapsed-before) gc-elapsed-deltas)
          (when rss (push rss rss-samples))
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
          :gc-stats (nelisp-integration--soak-aggregate-stats
                     (nreverse gc-deltas))
          :gc-elapsed-stats (nelisp-integration--soak-aggregate-stats
                             (nreverse gc-elapsed-deltas))
          :rss-stats (nelisp-integration--soak-aggregate-stats
                      (nreverse rss-samples))
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
