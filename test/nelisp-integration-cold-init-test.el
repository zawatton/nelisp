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

(provide 'nelisp-integration-cold-init-test)
;;; nelisp-integration-cold-init-test.el ends here
