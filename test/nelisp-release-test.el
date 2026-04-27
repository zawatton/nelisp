;;; nelisp-release-test.el --- Phase 7.5.3 release artifact infra ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.5.3 (Doc 32 v2 LOCKED §3.3) release-infra smoke.  The shell
;; scripts and CI workflow templates that ship the `stage-d-v2.0'
;; tarball are intentionally *not* exercised end-to-end by ERT — that
;; is CI matrix scope and would require a full Rust toolchain plus
;; cross-arch runners.  Instead, these tests assert on the *structural*
;; invariants the rest of Phase 7.5.3 depends on:
;;
;;   - `tools/build-release-artifact.sh' is present and executable
;;   - `tools/soak-test.sh' is present and executable, both tier
;;     thresholds (1h blocker / 24h post-ship、Doc 32 v2 §7) recorded
;;   - Makefile exposes the four new targets (release-artifact,
;;     release-checksum, soak-blocker, soak-post-ship)
;;   - `RELEASE_NOTES.md' template carries the Doc 32 v2 §11 LOCKED
;;     arm64-as-non-blocker (v1.0 時限) language so a future audit
;;     catches accidental promotion before v1.1+
;;   - `.github/workflows/release-qualification.yml' template covers
;;     all three platforms (linux-x86_64 + macos-arm64 + linux-arm64)
;;
;; Cheap structural checks only — no shell invocation, no network, no
;; Cargo.  The actual smoke runs in CI.

;;; Code:

(require 'ert)

(defconst nelisp-release-test--repo-root
  (or (getenv "NELISP_REPO_ROOT")
      (let ((dir (locate-dominating-file
                  (or load-file-name
                      buffer-file-name
                      default-directory)
                  "Makefile")))
        (and dir (file-name-as-directory (expand-file-name dir)))))
  "Absolute path to the NeLisp worktree root, or nil if not found.")

(defun nelisp-release-test--path (relative)
  "Expand RELATIVE against the repo root."
  (expand-file-name relative nelisp-release-test--repo-root))

(defun nelisp-release-test--read-file (relative)
  "Return the contents of RELATIVE under the repo root as a string."
  (let ((file (nelisp-release-test--path relative)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest nelisp-release-build-script-exists ()
  "tools/build-release-artifact.sh must be present and executable."
  ;; CI-smoke gate: `file-executable-p' on Windows native Emacs returns
  ;; nil for `.sh' scripts because Windows uses extension-based exec
  ;; rather than POSIX permission bits.  The script is a Linux/macOS
  ;; release artefact; no Windows runner ever invokes it.  Skip the
  ;; executable assertion on Windows (existence still asserted on POSIX).
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (let ((script (nelisp-release-test--path "tools/build-release-artifact.sh")))
    (should (file-exists-p script))
    (should (file-executable-p script))
    ;; Sanity-check the platform default is the §11 blocker tier so a
    ;; bare invocation matches CI behaviour.
    (let ((body (nelisp-release-test--read-file "tools/build-release-artifact.sh")))
      (should (string-match-p "PLATFORM=\"\\${1:-linux-x86_64}\"" body))
      (should (string-match-p "VERSION=\"\\${2:-stage-d-v2.0}\"" body)))))

(ert-deftest nelisp-release-soak-script-exists ()
  "tools/soak-test.sh must be executable and encode both soak tiers."
  ;; CI-smoke gate: same Windows `file-executable-p' caveat as the
  ;; sibling `nelisp-release-build-script-exists'.
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (let* ((script (nelisp-release-test--path "tools/soak-test.sh"))
         (body (nelisp-release-test--read-file "tools/soak-test.sh")))
    (should (file-exists-p script))
    (should (file-executable-p script))
    ;; Doc 32 v2 §7 thresholds — cheap regex spot-check that both tiers
    ;; (1h blocker, 24h post-ship audit) are wired with the canonical
    ;; RSS-growth ceilings (5 MB / 10 MB).
    (should (string-match-p "SOAK_DURATION_HOURS" body))
    (should (string-match-p "THRESHOLD_MB=5" body))
    (should (string-match-p "THRESHOLD_MB=10" body))
    (should (string-match-p "blocker" body))
    (should (string-match-p "post-ship audit" body))))

(ert-deftest nelisp-release-makefile-targets-defined ()
  "Makefile must declare release-artifact / release-checksum / soak-blocker / soak-post-ship."
  (let ((makefile (nelisp-release-test--read-file "Makefile")))
    ;; Targets in the .PHONY list (so `make' treats them as commands).
    (should (string-match-p "release-artifact" makefile))
    (should (string-match-p "release-checksum" makefile))
    (should (string-match-p "soak-blocker" makefile))
    (should (string-match-p "soak-post-ship" makefile))
    ;; Each target has its own rule body — anchor on the colon to avoid
    ;; matching the .PHONY line alone.
    (should (string-match-p "^release-artifact:" makefile))
    (should (string-match-p "^release-checksum:" makefile))
    (should (string-match-p "^soak-blocker:" makefile))
    (should (string-match-p "^soak-post-ship:" makefile))))

(ert-deftest nelisp-release-notes-template-exists ()
  "RELEASE_NOTES.md template must exist and reference the Doc 32 v2 tier matrix."
  (let* ((file (nelisp-release-test--path "RELEASE_NOTES.md"))
         (body (nelisp-release-test--read-file "RELEASE_NOTES.md")))
    (should (file-exists-p file))
    ;; Tier matrix headings — header + every required tier label.
    (should (string-match-p "Tier matrix" body))
    (should (string-match-p "blocker" body))
    (should (string-match-p "non-blocker" body))
    (should (string-match-p "post-ship audit" body))
    (should (string-match-p "release artifact" body))
    ;; All three platform suffixes referenced.
    (should (string-match-p "linux-x86_64" body))
    (should (string-match-p "macos-arm64" body))
    (should (string-match-p "linux-arm64" body))))

(ert-deftest nelisp-release-ci-workflow-template-exists ()
  ".github/workflows/release-qualification.yml must define all three platform jobs.

Skipped when the workflow file is missing — this happens when the
repository was pushed to GitHub via a Personal Access Token that lacks
the `workflow' scope (= the file is held back from the push), but the
template body itself is still required to exist whenever the file is
present.  Re-add the file via the GitHub web UI or grant the token
`workflow' scope and push to restore green coverage."
  (let* ((file (nelisp-release-test--path
                ".github/workflows/release-qualification.yml")))
    (skip-unless (file-exists-p file))
    (let ((body (nelisp-release-test--read-file
                 ".github/workflows/release-qualification.yml")))
      ;; One job per platform, matching the §7 4-tier gate.
      (should (string-match-p "build-linux-x86_64:" body))
      (should (string-match-p "build-macos-arm64:" body))
      (should (string-match-p "build-linux-arm64:" body))
      ;; blocker tier must NOT be `continue-on-error: true' so a CI red
      ;; on linux-x86_64 actually breaks the workflow.  arm64 jobs do
      ;; carry continue-on-error per Doc 32 v2 §11 (v1.0 時限).
      (should (string-match-p "build-linux-x86_64:[^z]*?runs-on: ubuntu-latest" body))
      ;; arm64 legs marked non-blocker via continue-on-error.
      (should (string-match-p "continue-on-error: true" body)))))

(ert-deftest nelisp-release-tier-matrix-includes-arm64-non-blocker ()
  "RELEASE_NOTES.md must keep arm64 = non-blocker (v1.0 時限) language so
Doc 32 v2 §11 LOCKED is not silently violated.  Promotion to blocker is
expected for v1.1+; an audit script grepping this file should still
spot v1.0 時限 wording until that promotion ships."
  (let ((body (nelisp-release-test--read-file "RELEASE_NOTES.md")))
    (should (string-match-p "v1.0 時限" body))
    (should (string-match-p "v1.1\\+" body))
    ;; Both arm64 platforms appear under the non-blocker label.
    (should (string-match-p
             "non-blocker.*macos-arm64\\|macos-arm64.*non-blocker" body))
    (should (string-match-p
             "non-blocker.*linux-arm64\\|linux-arm64.*non-blocker" body))))

(provide 'nelisp-release-test)

;;; nelisp-release-test.el ends here
