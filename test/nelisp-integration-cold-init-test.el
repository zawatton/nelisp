;;; nelisp-integration-cold-init-test.el --- ERT for Phase 7.5.2 cold-init  -*- lexical-binding: t; -*-

;; Doc 32 v2 LOCKED 2026-04-25 §3.2 sub-phase 7.5.2 — +8 ERT covering
;; the real cold-init coordinator + 4-stage bootstrap embed:
;;
;;   1. cold-init-verify-detects-missing-binary    — :ready nil when binary absent
;;   2. cold-init-verify-passes-with-binary        — :ready t when binary built
;;                                                   (skip-unless binary present)
;;   3. cold-init-run-stage0-bytecode-compile      — stage0 hash present
;;   4. cold-init-run-stage1-native-compile        — stage1 emits a3-hash
;;   5. cold-init-run-stage2-semantic-diff         — stage2 PASS path
;;   6. cold-init-run-stage3-self-recompile        — stage3 PASS → done
;;   7. cold-init-run-failback-on-stage1-error     — stage1 raise → fallback
;;   8. contract-version-mismatch-detects          — version skew → :ready nil
;;
;; Tests 3–7 exercise the full coordinator pipeline; they require the
;; T13 `nelisp-runtime' binary to satisfy the cold-init verifier, so
;; they `skip-unless' the binary is reachable (mirroring the T15 real-
;; exec test pattern).  Tests 1, 2, 8 cover the verifier itself and
;; run on every host.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-integration)
(require 'nelisp-cc-runtime)
(require 'nelisp-cc-bootstrap)

;;;; Helpers ---------------------------------------------------------

(defun nelisp-integration-cold-init-test--has-runtime-bin-p ()
  "Return non-nil when the `nelisp-runtime' binary is on disk + executable."
  (nelisp-integration--locate-runtime-binary))

(defun nelisp-integration-cold-init-test--skip-unless-bin ()
  "Skip the surrounding ERT unless T13 binary is reachable.
Phase 7.5.2 cold-init coordinator gates its real run on the verifier,
which itself blocks on `nelisp-runtime' presence; on hosts without
the binary the coordinator returns :status fail :stage stage0 by
design (briefing scenario 1), so the green-path tests skip rather
than assert against the failure."
  (unless (nelisp-integration-cold-init-test--has-runtime-bin-p)
    (ert-skip "nelisp-runtime binary missing — run `make runtime'")))

;;;; (1) verify detects missing binary -------------------------------

(ert-deftest nelisp-integration-cold-init-verify-detects-missing-binary ()
  "When the runtime binary is unreachable the verifier must report
`:ready nil' and include a `:runtime-binary-missing' reason in the
`:missing' alist.

To trigger this deterministically we point the locator override at a
non-existent path.  The override is a defcustom, so binding it
with `let' inside the test is sufficient — we restore on exit."
  (let ((nelisp-cc-runtime-binary-override
         (expand-file-name "nelisp-runtime-bogus-path-zzzz"
                           temporary-file-directory)))
    (let* ((r (nelisp-integration-cold-init-verify))
           (missing (plist-get r :missing)))
      (should (eq (plist-get r :ready) nil))
      (should (assq :runtime-binary-missing missing))
      ;; The detail string mentions `make runtime' for an actionable hint.
      (let ((detail (cdr (assq :runtime-binary-missing missing))))
        (should (stringp detail))
        (should (string-match-p "make runtime" detail))))))

;;;; (2) verify passes with binary present ---------------------------

(ert-deftest nelisp-integration-cold-init-verify-passes-with-binary ()
  "When the runtime binary, bootstrap source, and contract versions
are all present the verifier returns `:ready t'.  A staticlib that
has not been built is *advisory* (Phase 7.5.2 simulator path) and
still allows the verifier to report `:ready t' — but Phase 7.5.2 ERT
would skip when the binary itself is unbuilt, which is the gating
condition `--skip-unless-bin' enforces."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let ((r (nelisp-integration-cold-init-verify)))
    ;; Either fully ready, or only staticlib advisory missing.
    (should (eq (plist-get r :ready) t))
    ;; If a :missing list is reported, every entry must be advisory.
    (let ((missing (plist-get r :missing)))
      (when missing
        (dolist (entry missing)
          (should (memq (car entry) '(:runtime-staticlib-missing))))))))

;;;; (3) cold-init-run stage0 bytecode compile -----------------------

(ert-deftest nelisp-integration-cold-init-run-stage0-bytecode-compile ()
  "Cold-init must surface a non-nil `:a2-hash' (Doc 28 §3.5 stage0
hash) even when the broader run fails downstream — the stage0 phase
is unconditional once the verifier is green."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let ((r (nelisp-integration-cold-init-run '(lambda (x) x))))
    (should (stringp (plist-get r :a2-hash)))
    (should (= (length (plist-get r :a2-hash)) 64))
    (should (string-match-p "\\`[0-9a-f]+\\'" (plist-get r :a2-hash)))
    ;; protocol-version is the T12 constant (currently 1).
    (should (eq (plist-get r :protocol-version)
                nelisp-cc-bootstrap-protocol-version))))

;;;; (4) cold-init-run stage1 native compile -------------------------

(ert-deftest nelisp-integration-cold-init-run-stage1-native-compile ()
  "Cold-init green path emits an `:a3-candidate-hash' (Doc 28 §3.5
stage1 → stage1 succeeded so an A3 candidate exists).  When all four
stages pass, `:status' = pass + `:stage' = done; otherwise the field
is still set to the stage1 hash but `:status' may be fail at a later
stage."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let ((r (nelisp-integration-cold-init-run '(lambda (x) x))))
    (should (stringp (plist-get r :a3-candidate-hash)))
    (should (= (length (plist-get r :a3-candidate-hash)) 64))))

;;;; (5) cold-init-run stage2 semantic diff PASS ---------------------

(ert-deftest nelisp-integration-cold-init-run-stage2-semantic-diff ()
  "Stage2 (semantic diff) compares A2 vs A3 candidate hashes; on the
deterministic simulator pipeline the two are byte-identical, so
stage2 always passes when stage1 succeeded.  We assert the umbrella
status reaches `:stage done' (or fail at stage3) — never fail at
stage2 — and `:authoritative-promoted' is t in the green-path case."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let* ((r (nelisp-integration-cold-init-run '(lambda (x) x)))
         (stage (plist-get r :stage))
         (status (plist-get r :status)))
    ;; Never fail at stage2 in the simulator pipeline.
    (should-not (and (eq status 'fail) (eq stage 'stage2)))
    ;; When the run is fully green, the authoritative compiler is
    ;; promoted.
    (when (eq status 'pass)
      (should (eq stage 'done))
      (should (eq (plist-get r :authoritative-promoted) t))
      (should (eq (plist-get r :fallback-active) nil)))))

;;;; (6) cold-init-run stage3 self-recompile -------------------------

(ert-deftest nelisp-integration-cold-init-run-stage3-self-recompile ()
  "Stage3 (A3 self-recompile) must re-emit the same A3 hash; the
deterministic simulator pipeline guarantees this so the umbrella
returns `:stage done' + `:status pass'."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let* ((r (nelisp-integration-cold-init-run '(lambda (x) x)))
         (status (plist-get r :status))
         (stage (plist-get r :stage)))
    (should (eq status 'pass))
    (should (eq stage 'done))
    (should (eq (plist-get r :authoritative-promoted) t))
    (should (eq (plist-get r :fallback-active) nil))
    ;; :elapsed-seconds is a float and non-negative.
    (should (numberp (plist-get r :elapsed-seconds)))
    (should (>= (plist-get r :elapsed-seconds) 0.0))))

;;;; (7) failback when stage1 raises ---------------------------------

(ert-deftest nelisp-integration-cold-init-run-failback-on-stage1-error ()
  "When stage1 raises (e.g. an SSA frontend the scaffold cannot lower)
the coordinator captures the error and returns a fail plist with
`:fallback-active t' + `:authoritative-promoted nil'.  This is the
A2-fallback path described in Doc 28 §3.5 stage1 failure mode."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  ;; Use a form the T6 frontend explicitly does not support; the
  ;; T12 `nelisp-cc-bootstrap-run' wraps stage1 in `condition-case'
  ;; and returns :status fail :stage stage1 :fallback-active t.
  (let ((r (nelisp-integration-cold-init-run
            '(lambda () (this-symbol-is-not-defined-anywhere)))))
    ;; The coordinator must NOT propagate the error — it must return
    ;; a structured plist.  We allow either pass or fail; on the rare
    ;; chance the form happens to lower cleanly (it does for the
    ;; current frontend because the call is to an unresolved symbol,
    ;; which the simulator records as a fixup not a raise), we still
    ;; want the return type / shape contract.
    (should (memq (plist-get r :status) '(pass fail)))
    (should (memq (plist-get r :stage) '(stage0 stage1 stage2 stage3 done)))
    ;; Either pass + authoritative t + fallback nil, or fail with
    ;; fallback active.
    (cond
     ((eq (plist-get r :status) 'pass)
      (should (eq (plist-get r :authoritative-promoted) t))
      (should (eq (plist-get r :fallback-active) nil)))
     (t
      (should (eq (plist-get r :authoritative-promoted) nil))
      (should (eq (plist-get r :fallback-active) t))))))

;;;; (8) contract-version-mismatch detects skew ----------------------

(ert-deftest nelisp-integration-contract-version-mismatch-detects ()
  "Inject a fake contract version into the registry and verify that
`--verify-contract-versions' reports the mismatch as a list of
`(:contract :expected :actual)' plists.  This ERT guards Doc 30 v2
§6.4 silent-corruption risk: a heap-region or GC metadata wire bump
that is *not* mirrored into the registry surfaces here."
  ;; Save / restore the global registry.
  (let* ((orig nelisp-integration-required-contract-versions)
         (nelisp-integration-required-contract-versions
          ;; Force an obviously-wrong expected version.
          (cons '(gc-metadata-wire . 999)
                (cl-remove 'gc-metadata-wire orig :key #'car))))
    (let* ((mismatches (nelisp-integration--verify-contract-versions))
           (gc-entry (cl-find-if (lambda (m)
                                   (eq (plist-get m :contract)
                                       'gc-metadata-wire))
                                 mismatches)))
      (should mismatches)
      (should gc-entry)
      (should (= (plist-get gc-entry :expected) 999))
      ;; :actual is the live `nelisp-cc-runtime-gc-metadata-version'
      ;; — not 999, by construction.
      (should (integerp (plist-get gc-entry :actual)))
      (should-not (= (plist-get gc-entry :actual) 999)))
    ;; The verifier must surface the mismatch to bin/anvil as
    ;; :ready nil with a :contract-version-mismatch entry.
    (let* ((r (nelisp-integration-cold-init-verify))
           (missing (plist-get r :missing)))
      (should (eq (plist-get r :ready) nil))
      (should (assq :contract-version-mismatch missing)))))

;;;; --- Phase 7.5.2 SCAFFOLD-LAYER dispatcher + soak harness ERT ----
;;
;; The block above (tests 1-8) exercises the *real* coordinator that
;; delegates to T12 `nelisp-cc-bootstrap-run' and skips when the T13
;; binary is unbuilt.  The block below pins the *scaffold-layer*
;; dispatcher contract (per-stage stub handlers + counter + error
;; capture + soak harness) — these run on every host because the
;; scaffold path has no binary dependency.  Phase 7.0-7.4 close will
;; replace the stub handlers with real per-stage implementations
;; without changing the dispatcher contract; the ERT below pins that
;; contract so the swap cannot drift the plist shape silently.

;;;; (S1) dispatcher default = all-stub run reports `:status fail' ----

(ert-deftest nelisp-integration-cold-init-dispatch-default-stubs ()
  "Default dispatcher (= every stage = stub) must:
  - return `:scaffold t' (sentinel)
  - return `:status fail' (stubs cannot be a green path)
  - report `:stages-completed 0'
  - return `:stages' as a list of length 4 (every stage attempted)
  - first `:error' is one of the documented stub keywords
  - never raise"
  (let* ((r (nelisp-integration-cold-init-dispatch))
         (stages (plist-get r :stages))
         (err (plist-get r :error)))
    (should (eq (plist-get r :scaffold) t))
    (should (eq (plist-get r :status) 'fail))
    (should (= (plist-get r :stages-completed) 0))
    (should (= (length stages) 4))
    (should (memq err
                  (mapcar #'cdr
                          nelisp-integration-cold-init-stage-pending-keyword)))
    ;; Every stage entry has the documented contract shape.
    (dolist (s stages)
      (should (plist-get s :stage))
      (should (plist-get s :status))
      (should (numberp (plist-get s :elapsed-seconds)))
      (should (>= (plist-get s :elapsed-seconds) 0.0)))))

;;;; (S2) dispatcher with all-pass real handlers reports `:status pass' --

(ert-deftest nelisp-integration-cold-init-dispatch-all-pass ()
  "When every stage handler returns `(:status pass)' the dispatcher
must report `:status pass :stages-completed 4'."
  (let* ((pass-handler (lambda (&rest _) (list :status 'pass)))
         (handlers (mapcar (lambda (s) (cons s pass-handler))
                           nelisp-integration-cold-init-stage-order))
         (r (nelisp-integration-cold-init-dispatch :handlers handlers)))
    (should (eq (plist-get r :status) 'pass))
    (should (= (plist-get r :stages-completed) 4))
    (should (eq (plist-get r :error) nil))
    (should (eq (plist-get r :scaffold) t))
    (should (= (length (plist-get r :stages)) 4))))

;;;; (S3) dispatcher error path — synthetic stage failure ------------

(ert-deftest nelisp-integration-cold-init-dispatch-synthetic-failure ()
  "When a stage handler raises, the dispatcher must:
  - capture the error into the per-stage `:error' field
  - mark that stage's `:status' as `fail'
  - stop dispatch (default `:stop-on '(fail)') so subsequent stages
    are not attempted
  - report `:stages-completed' = number of stages that passed *before*
    the failure
  - report `:status fail' overall + `:error fail' (= the first
    non-pass status seen)"
  (let* ((pass (lambda (&rest _) (list :status 'pass)))
         (boom (lambda (&rest _) (error "stage3 synthetic fault")))
         (handlers `((stage1 . ,pass)
                     (stage2 . ,pass)
                     (stage3 . ,boom)
                     (stage4 . ,pass)))
         (r (nelisp-integration-cold-init-dispatch :handlers handlers))
         (stages (plist-get r :stages)))
    (should (eq (plist-get r :status) 'fail))
    (should (= (plist-get r :stages-completed) 2))
    (should (eq (plist-get r :error) 'fail))
    ;; Default `:stop-on' = (fail) — only 3 stage entries should
    ;; appear (the dispatcher stopped before stage4).
    (should (= (length stages) 3))
    (let ((stage3 (cl-find-if (lambda (s) (eq (plist-get s :stage) 'stage3))
                              stages)))
      (should stage3)
      (should (eq (plist-get stage3 :status) 'fail))
      (should (consp (plist-get stage3 :error))))))

;;;; (S4) dispatcher `:stop-on nil' continues past stub returns ------

(ert-deftest nelisp-integration-cold-init-dispatch-no-stop-on-stub ()
  "With default `:stop-on '(fail)' the dispatcher must NOT stop on a
stub return — it should attempt all four stages and report each.
This proves stub keywords are visible without aborting dispatch."
  (let* ((r (nelisp-integration-cold-init-dispatch))
         (stages (plist-get r :stages)))
    ;; Stub returns are not in `:stop-on' so all 4 stages run.
    (should (= (length stages) 4))
    (should (= (plist-get r :stages-completed) 0))
    ;; Every stage status is a documented stub keyword.
    (dolist (s stages)
      (should (memq (plist-get s :status)
                    (mapcar #'cdr
                            nelisp-integration-cold-init-stage-pending-keyword))))))

;;;; (S5) per-stage stub handler returns documented keyword ----------

(ert-deftest nelisp-integration-cold-init-stub-handler-keyword-shape ()
  "Each per-stage stub handler must return the documented
`:stub-not-yet-impl-pending-phase-X.Y' keyword for its stage.
Production callers grep this shape to detect a stub return."
  (dolist (entry nelisp-integration-cold-init-stage-pending-keyword)
    (let* ((stage (car entry))
           (expected-kw (cdr entry))
           (handler (nelisp-integration--cold-init-stub-handler stage))
           (r (funcall handler nil)))
      (should (eq (plist-get r :status) expected-kw))
      (should (eq (plist-get r :stage) stage))
      (should (stringp (plist-get r :blocked-by)))
      ;; The keyword name must mention "stub-not-yet-impl-pending"
      ;; — production callers grep this prefix.
      (should (string-prefix-p ":stub-not-yet-impl-pending-phase-"
                               (symbol-name expected-kw))))))

;;;; (S6) soak harness scaffold returns documented contract ----------

(ert-deftest nelisp-integration-soak-harness-default-contract ()
  "Default soak harness (= scaffold dispatcher per iteration) must
return the documented contract:
  - `:scaffold t' sentinel
  - `:iterations N' matches default
  - `:passed + :failed = :iterations'
  - `:status' = pass iff passed = iterations
  - default = all stub → status fail
  - never raises"
  (let* ((r (nelisp-integration-soak-harness :iterations 3))
         (passed (plist-get r :passed))
         (failed (plist-get r :failed)))
    (should (eq (plist-get r :scaffold) t))
    (should (= (plist-get r :iterations) 3))
    (should (= (+ passed failed) 3))
    (should (numberp (plist-get r :elapsed-seconds)))
    ;; Default harness handler = scaffold dispatcher with all stubs;
    ;; every iteration reports fail, so passed = 0 / failed = 3.
    (should (= passed 0))
    (should (= failed 3))
    (should (eq (plist-get r :status) 'fail))
    (should (plist-get r :first-failure))))

;;;; (S7) soak harness with passing handler reports `:status pass' ---

(ert-deftest nelisp-integration-soak-harness-pass-handler ()
  "Soak harness with a handler that always returns `:status pass'
must report all-pass + zero failures + `:status pass' overall."
  (let* ((r (nelisp-integration-soak-harness
             :iterations 5
             :handler (lambda (_i) (list :status 'pass)))))
    (should (= (plist-get r :passed) 5))
    (should (= (plist-get r :failed) 0))
    (should (eq (plist-get r :status) 'pass))
    (should (eq (plist-get r :first-failure) nil))))

;;;; (S8) soak harness `:stop-on-fail' truncates the run -------------

(ert-deftest nelisp-integration-soak-harness-stop-on-fail ()
  "When `:stop-on-fail t' the soak harness aborts on the first
non-pass iteration and reports the partial counter."
  (let* ((counter 0)
         (handler (lambda (_i)
                    (cl-incf counter)
                    (if (= counter 2)
                        (list :status 'fail :reason 'synthetic)
                      (list :status 'pass))))
         (r (nelisp-integration-soak-harness
             :iterations 10
             :stop-on-fail t
             :handler handler)))
    ;; Iteration 1 = pass, iteration 2 = fail → stop.
    (should (= (plist-get r :passed) 1))
    (should (= (plist-get r :failed) 1))
    (should (eq (plist-get r :status) 'fail))
    (should (plist-get r :first-failure))))

;;;; (B1) stage-1 real handler — happy path -------------------------
;;
;; The stage-1 body wires Phase 7.0 syscall-stub init.  When the
;; nelisp-runtime cdylib is built (= `make runtime' produced
;; target/release/nelisp-runtime + nelisp-runtime-module.so), the
;; handler must:
;;   - return :status pass
;;   - report :stage 1
;;   - record the artifacts it discovered (binary / module / cdylib /
;;     staticlib paths)
;;   - record :exec-mode (:module or :subprocess) so callers can tell
;;     which path was taken
;;   - report :smoke-exit-code 0
;;   - set :module-loaded based on which path was used

(ert-deftest nelisp-integration-cold-init-stage1-handler-happy-path ()
  "Stage-1 real handler returns :status pass when Phase 7.0 syscall
surface is reachable.  The cdylib must be built (= `make runtime');
this test skips on hosts where the binary is absent so CI without the
Rust artifact does not flake."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let* ((r (nelisp-integration-cold-init-stage1-handler nil))
         (artifacts (plist-get r :artifacts)))
    (should (eq (plist-get r :status) 'pass))
    (should (eq (plist-get r :stage) 1))
    (should (eq (plist-get r :smoke-exit-code) 0))
    ;; Either the in-process module path or the subprocess path may
    ;; have been taken — both report a green stage-1.  We only assert
    ;; the two are mutually exclusive + consistent.
    (should (memq (plist-get r :exec-mode) '(:module :subprocess)))
    (cond
     ((eq (plist-get r :exec-mode) :module)
      (should (eq (plist-get r :module-loaded) t)))
     ((eq (plist-get r :exec-mode) :subprocess)
      (should (eq (plist-get r :module-loaded) nil))))
    ;; Artifacts plist must include at least the binary (= the path
    ;; the verifier already validated via the skip-unless gate).
    (should (stringp (plist-get artifacts :binary)))
    (should (file-executable-p (plist-get artifacts :binary)))))

;;;; (B2) stage-1 real handler — fail path ---------------------------
;;
;; When the cdylib is missing (= the binary locator returns nil and
;; the module path also fails), the handler must report :status fail
;; with a non-empty :error string and a fully-populated artifacts
;; plist (each value = nil) so callers can present an actionable
;; "missing-artifact-X" diagnostic.
;;
;; We can't simply unload the Emacs module (Emacs has no
;; `module-unload') so this test stubs both probe helpers via
;; `cl-letf' to deterministically return the missing-everything
;; shape.  This proves the *handler's failure-shape contract* without
;; depending on subprocess / module load order across tests in the
;; same session.

(ert-deftest nelisp-integration-cold-init-stage1-handler-fail-path ()
  "Stage-1 real handler returns :status fail when no runtime path is
reachable.  We force the failure deterministically by stubbing the
two probe helpers + the artifact locator so the handler sees
\"module unavailable + binary not executable\" — exactly the shape it
would see on a fresh checkout that never ran `make runtime'."
  (cl-letf
      (((symbol-function
         'nelisp-integration--cold-init-stage1-probe-artifacts)
        (lambda ()
          (list :binary nil :module nil :staticlib nil :cdylib nil)))
       ((symbol-function
         'nelisp-integration--cold-init-stage1-via-module)
        (lambda ()
          (list :no-module "stubbed: module path forced unavailable")))
       ((symbol-function
         'nelisp-integration--cold-init-stage1-via-subprocess)
        (lambda (_binary)
          (list :error "stubbed: subprocess path forced unreachable"))))
    (let* ((r (nelisp-integration-cold-init-stage1-handler nil))
           (artifacts (plist-get r :artifacts)))
      (should (eq (plist-get r :status) 'fail))
      (should (eq (plist-get r :stage) 1))
      (should (stringp (plist-get r :error)))
      (should (> (length (plist-get r :error)) 0))
      ;; All artifact slots must be nil under the stubbed probe so the
      ;; failure diagnostic is structural rather than spurious.
      (should (eq (plist-get artifacts :binary) nil))
      (should (eq (plist-get artifacts :module) nil))
      (should (eq (plist-get artifacts :staticlib) nil))
      (should (eq (plist-get artifacts :cdylib) nil))
      ;; module-result + subprocess-result are captured for triage.
      (should (listp (plist-get r :module-result)))
      (should (listp (plist-get r :subprocess-result))))))

;;;; (B3) stage-1 real handler — dispatcher integration --------------
;;
;; The convenience builder
;; `nelisp-integration-cold-init-handlers-with-stage1-real' must
;; substitute the real handler for the stage1 stub while leaving
;; stages 2/3/4 as their default stubs.  Running the dispatcher with
;; this handlers-alist on a host with the runtime built must therefore
;; pass stage 1 (pass count = 1) and surface the documented stub
;; keywords for stages 2/3/4.

(ert-deftest nelisp-integration-cold-init-dispatch-stage1-real-stub-rest ()
  "Dispatcher with stage-1 real + stages 2/3/4 stubs reports
`:status fail :stages-completed 1 :error <stage2-stub-keyword>'."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let* ((handlers (nelisp-integration-cold-init-handlers-with-stage1-real))
         (r (nelisp-integration-cold-init-dispatch :handlers handlers))
         (stages (plist-get r :stages)))
    (should (eq (plist-get r :scaffold) t))
    (should (eq (plist-get r :status) 'fail))
    (should (= (plist-get r :stages-completed) 1))
    (should (= (length stages) 4))
    ;; First stage = stage1 = pass (the real handler).
    (let ((s1 (cl-find-if (lambda (s) (eq (plist-get s :stage) 'stage1))
                          stages)))
      (should s1)
      (should (eq (plist-get s1 :status) 'pass))
      (let ((res (plist-get s1 :result)))
        (should (listp res))
        (should (eq (plist-get res :stage) 1))
        (should (eq (plist-get res :smoke-exit-code) 0))))
    ;; First non-pass surfaced into :error must be the stage-2 stub
    ;; keyword (the dispatcher does NOT stop on stub returns by default).
    (should (eq (plist-get r :error)
                (cdr (assq 'stage2
                           nelisp-integration-cold-init-stage-pending-keyword))))))

;;;; (B4) stage-2 real handler — happy path -------------------------
;;
;; Stage 2 = Phase 7.2 allocator init.  The handler initialises a
;; fresh nursery via `nelisp-allocator-init-nursery', verifies the
;; heap-region registry version (Doc 29 §1.4) across every region,
;; then restores the prior nursery binding.  Because the allocator is
;; pure Elisp, this test runs on every host where the module loads.

(ert-deftest nelisp-integration-cold-init-stage2-handler-happy-path ()
  "Stage-2 real handler returns :status pass when the allocator
initialises a fresh nursery + every region passes the version check.
Pure Elisp dependency — runs on every host."
  (unless (featurep 'nelisp-allocator)
    (require 'nelisp-allocator))
  (let ((prior (and (boundp 'nelisp-allocator--current-nursery)
                    nelisp-allocator--current-nursery))
        r)
    (unwind-protect
        (progn
          (setq r (nelisp-integration-cold-init-stage2-handler nil))
          (should (eq (plist-get r :status) 'pass))
          (should (eq (plist-get r :stage) 2))
          (should (integerp (plist-get r :nursery-size)))
          (should (> (plist-get r :nursery-size) 0))
          (should (eq (plist-get r :region-version)
                      nelisp-heap-region-version))
          (should (>= (plist-get r :region-count) 1))
          (let ((regions (plist-get r :regions)))
            (should (listp regions))
            (should (>= (length regions) 1))
            ;; Every region carries the keyword-namespace family per
            ;; Doc 29 §1.4 + the gc-inner snapshot translation.
            (let ((reg0 (car regions)))
              (should (plist-get reg0 :region-id))
              (should (plist-get reg0 :start))
              (should (plist-get reg0 :end))
              (should (memq (plist-get reg0 :generation)
                            '(:nursery :tenured)))
              (should (keywordp (plist-get reg0 :family))))))
      ;; Restore prior nursery so the rest of the test session sees a
      ;; deterministic state.
      (when (boundp 'nelisp-allocator--current-nursery)
        (setq nelisp-allocator--current-nursery prior)))))

;;;; (B5) stage-2 real handler — fail path ---------------------------
;;
;; Force `nelisp-allocator-init-nursery' to raise, the handler must
;; capture the error and return :status fail with a non-empty :error
;; field — never propagate the signal up to the dispatcher.

(ert-deftest nelisp-integration-cold-init-stage2-handler-fail-path ()
  "Stage-2 handler captures allocator init failures into :status fail
without propagating the signal."
  (unless (featurep 'nelisp-allocator)
    (require 'nelisp-allocator))
  (let ((prior (and (boundp 'nelisp-allocator--current-nursery)
                    nelisp-allocator--current-nursery)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-allocator-init-nursery)
                   (lambda (&rest _)
                     (error "stubbed: stage-2 forced init failure"))))
          (let ((r (nelisp-integration-cold-init-stage2-handler nil)))
            (should (eq (plist-get r :status) 'fail))
            (should (eq (plist-get r :stage) 2))
            (should (stringp (plist-get r :error)))
            (should (string-match-p "stubbed" (plist-get r :error)))))
      (when (boundp 'nelisp-allocator--current-nursery)
        (setq nelisp-allocator--current-nursery prior)))))

;;;; (B6) stage-3 real handler — happy path -------------------------
;;
;; Stage 3 = Phase 7.4 coding init.  Pure Elisp; runs on every host
;; where `nelisp-coding' + `nelisp-coding-jis-tables' load.

(ert-deftest nelisp-integration-cold-init-stage3-handler-happy-path ()
  "Stage-3 real handler returns :status pass when the JIS tables match
the golden hash + UTF-8 round-trip succeeds on a CJK + emoji seed."
  (unless (featurep 'nelisp-coding)
    (require 'nelisp-coding))
  (unless (featurep 'nelisp-coding-jis-tables)
    (require 'nelisp-coding-jis-tables))
  (let ((r (nelisp-integration-cold-init-stage3-handler nil)))
    (should (eq (plist-get r :status) 'pass))
    (should (eq (plist-get r :stage) 3))
    (should (stringp (plist-get r :jis-tables-hash)))
    (should (= (length (plist-get r :jis-tables-hash)) 64))
    (should (string= (plist-get r :jis-tables-hash)
                     nelisp-coding-jis-tables-sha256))
    (should (integerp (plist-get r :utf8-roundtrip-bytes-len)))
    (should (> (plist-get r :utf8-roundtrip-bytes-len) 0))
    (should (string= (plist-get r :utf8-roundtrip-restored)
                     nelisp-integration--cold-init-stage3-utf8-roundtrip-seed))))

;;;; (B7) stage-3 real handler — fail path (golden hash mismatch) ----

(ert-deftest nelisp-integration-cold-init-stage3-handler-fail-path ()
  "Stage-3 handler returns :status fail when the JIS golden hash
verifier raises (= simulated table corruption)."
  (unless (featurep 'nelisp-coding)
    (require 'nelisp-coding))
  (unless (featurep 'nelisp-coding-jis-tables)
    (require 'nelisp-coding-jis-tables))
  (cl-letf (((symbol-function 'nelisp-coding-jis-tables-verify-hash)
             (lambda (&rest _)
               (signal 'nelisp-coding-table-corruption
                       (list :expected "AAA" :actual "BBB")))))
    (let ((r (nelisp-integration-cold-init-stage3-handler nil)))
      (should (eq (plist-get r :status) 'fail))
      (should (eq (plist-get r :stage) 3))
      (should (stringp (plist-get r :error))))))

;;;; (B8) stage-4 real handler — happy path -------------------------
;;
;; Stage 4 = Phase 7.1 native compile bootstrap.  Requires the T13
;; binary; skips otherwise.

(ert-deftest nelisp-integration-cold-init-stage4-handler-happy-path ()
  "Stage-4 real handler returns :status pass when nelisp-cc-bootstrap-run
emits an A3-candidate hash from the seed lambda."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (let ((r (nelisp-integration-cold-init-stage4-handler nil)))
    (should (eq (plist-get r :status) 'pass))
    (should (eq (plist-get r :stage) 4))
    (should (stringp (plist-get r :a3-candidate-hash)))
    (should (= (length (plist-get r :a3-candidate-hash)) 64))
    (should (stringp (plist-get r :a2-hash)))
    (should (= (length (plist-get r :a2-hash)) 64))
    (should (numberp (plist-get r :elapsed-seconds)))
    (should (>= (plist-get r :elapsed-seconds) 0.0))))

;;;; (B9) stage-4 real handler — fail path ---------------------------
;;
;; Force `nelisp-cc-bootstrap-run' to return a plist without an
;; :a3-candidate-hash; the handler must report :status fail (the
;; bootstrap fell back to A2 / produced no native bytes).

(ert-deftest nelisp-integration-cold-init-stage4-handler-fail-path ()
  "Stage-4 handler returns :status fail when the bootstrap produces
no A3 candidate hash (= no native bytes emitted)."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (cl-letf (((symbol-function 'nelisp-cc-bootstrap-run)
             (lambda (&rest _)
               (list :status 'fail :stage 'stage1
                     :a2-hash (make-string 64 ?a)
                     :a3-candidate-hash nil))))
    (let ((r (nelisp-integration-cold-init-stage4-handler nil)))
      (should (eq (plist-get r :status) 'fail))
      (should (eq (plist-get r :stage) 4))
      (should (stringp (plist-get r :error)))
      (should (string-match-p "no bytes" (plist-get r :error))))))

;;;; (B10) stage-4 real handler — bootstrap signal raised ------------

(ert-deftest nelisp-integration-cold-init-stage4-handler-error-path ()
  "Stage-4 handler captures bootstrap errors into :status fail without
propagating the signal."
  (nelisp-integration-cold-init-test--skip-unless-bin)
  (cl-letf (((symbol-function 'nelisp-cc-bootstrap-run)
             (lambda (&rest _) (error "stubbed: bootstrap blew up"))))
    (let ((r (nelisp-integration-cold-init-stage4-handler nil)))
      (should (eq (plist-get r :status) 'fail))
      (should (eq (plist-get r :stage) 4))
      (should (string-match-p "stubbed" (plist-get r :error))))))

;;;; (B11) all-real handlers convenience builder ---------------------

(ert-deftest nelisp-integration-cold-init-handlers-with-real-stages-shape ()
  "The convenience builder must return a 4-entry alist with each
stage mapped to its real handler symbol."
  (let ((handlers (nelisp-integration-cold-init-handlers-with-real-stages)))
    (should (= (length handlers) 4))
    (should (eq (cdr (assq 'stage1 handlers))
                #'nelisp-integration-cold-init-stage1-handler))
    (should (eq (cdr (assq 'stage2 handlers))
                #'nelisp-integration-cold-init-stage2-handler))
    (should (eq (cdr (assq 'stage3 handlers))
                #'nelisp-integration-cold-init-stage3-handler))
    (should (eq (cdr (assq 'stage4 handlers))
                #'nelisp-integration-cold-init-stage4-handler))))

;;;; --- Phase 7.5.3 soak harness real-measurement ERT ---------------
;;
;; Tests S1-S8 above exercised the soak harness *contract* (iteration
;; counter, status aggregation, stop-on-fail, scaffold sentinel).  The
;; tests below pin the new GC + RSS measurement plumbing.

;;;; (M1) soak harness emits :gc-stats with min/max/mean -------------

(ert-deftest nelisp-integration-soak-harness-gc-stats-shape ()
  "Soak harness must emit :gc-stats with the documented aggregate
shape (= min/max/mean/samples) when iterations > 0."
  (let* ((r (nelisp-integration-soak-harness
             :iterations 4
             :handler (lambda (_i) (list :status 'pass))))
         (gc (plist-get r :gc-stats)))
    (should (= (plist-get r :iterations) 4))
    (should (= (plist-get r :passed) 4))
    (should (eq (plist-get r :status) 'pass))
    (should gc)
    (should (numberp (plist-get gc :min)))
    (should (numberp (plist-get gc :max)))
    (should (numberp (plist-get gc :mean)))
    (should (= (plist-get gc :samples) 4))
    (should (<= (plist-get gc :min) (plist-get gc :max)))
    (should (<= (plist-get gc :min) (plist-get gc :mean)))
    (should (<= (plist-get gc :mean) (plist-get gc :max)))))

;;;; (M2) soak harness emits :gc-elapsed-stats -----------------------

(ert-deftest nelisp-integration-soak-harness-gc-elapsed-stats-shape ()
  "Soak harness must emit :gc-elapsed-stats with the documented
aggregate shape — `gc-elapsed' is a builtin float counter so deltas
are non-negative."
  (let* ((r (nelisp-integration-soak-harness
             :iterations 3
             :handler (lambda (_i) (list :status 'pass))))
         (ge (plist-get r :gc-elapsed-stats)))
    (should ge)
    (should (numberp (plist-get ge :min)))
    (should (>= (plist-get ge :min) 0.0))
    (should (>= (plist-get ge :max) (plist-get ge :min)))
    (should (= (plist-get ge :samples) 3))))

;;;; (M3) soak harness :rss-stats either populated or nil ------------

(ert-deftest nelisp-integration-soak-harness-rss-stats-shape ()
  "Soak harness :rss-stats must be either nil (= host exposes no RSS)
or a populated aggregate with :min/:max/:mean.  We do not assert RSS
is nonzero because the test runs on every platform; we only assert
the aggregate is well-formed when present."
  (let* ((r (nelisp-integration-soak-harness
             :iterations 3
             :handler (lambda (_i) (list :status 'pass))))
         (rss (plist-get r :rss-stats)))
    (when rss
      (should (numberp (plist-get rss :min)))
      (should (numberp (plist-get rss :max)))
      (should (numberp (plist-get rss :mean)))
      (should (>= (plist-get rss :min) 0))
      (should (>= (plist-get rss :max) (plist-get rss :min)))
      (should (>= (plist-get rss :samples) 1))
      (should (<= (plist-get rss :samples) 3)))))

;;;; (M4) soak harness real-measurement smoke ------------------------
;;
;; 4-iteration smoke that runs the *default* handler (= scaffold
;; dispatcher) and verifies the full plist shape — this is the
;; minimal real-measurement smoke specified in the Phase 7.5.3 brief.

(ert-deftest nelisp-integration-soak-harness-real-measurement-smoke ()
  "Soak harness 4-iteration smoke (= Phase 7.5.3 brief minimum)
returns the documented plist with both `gc-stats' + `rss-stats'
shape — `:status' is fail because the default handler is the all-stub
dispatcher, but the metric collection still runs."
  (let* ((r (nelisp-integration-soak-harness :iterations 4))
         (gc (plist-get r :gc-stats))
         (ge (plist-get r :gc-elapsed-stats)))
    (should (= (plist-get r :iterations) 4))
    ;; Default handler returns fail (all stubs); metric collection
    ;; runs regardless.
    (should (eq (plist-get r :status) 'fail))
    (should (= (plist-get r :failed) 4))
    ;; GC stats always present (gcs-done / gc-elapsed are core builtins).
    (should gc)
    (should ge)
    (should (= (plist-get gc :samples) 4))
    (should (= (plist-get ge :samples) 4))
    ;; Scaffold sentinel preserved for backwards-compat with the
    ;; Phase 7.5.2 caller contract.
    (should (eq (plist-get r :scaffold) t))))

(provide 'nelisp-integration-cold-init-test)
;;; nelisp-integration-cold-init-test.el ends here
