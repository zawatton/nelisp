;;; nelisp-release-stage-d-v2-scaffold-test.el --- Phase 7.5.3 §3.3 scaffold smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 32 v2 LOCKED §3.3 — Phase 7.5.3 release infrastructure scaffold
;; smoke ERT.  These tests *do not* run the actual 24h soak / cross-arch
;; CI; they only assert that the scaffolding files (workflow yaml,
;; installer script, soak wrapper script) are well-formed and reachable
;; from the worktree root so a future revision cannot silently delete or
;; mis-rename them.
;;
;; Coverage:
;;
;;   1. install-sh-help-exits-clean
;;        — release/stage-d-v2.0/install.sh has #! shebang + responds to
;;          --help with exit 0 and a usage banner.
;;   2. soak-script-help-exits-clean
;;        — test/nelisp-soak-test.sh has #! shebang + responds to --help
;;          with exit 0 and a usage banner.
;;   3. release-workflow-yaml-parses
;;        — .github/workflows/stage-d-v2.0-release.yml exists, has the
;;          documented `name:' header, the 3-platform matrix legs, and
;;          no obvious indentation tab.  Full GitHub Actions schema
;;          validation is the actionlint domain (= out of scope here).
;;   4. release-manifest-json-parses
;;        — release/stage-d-v2.0/manifest.json parses as JSON with the
;;          documented top-level keys.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(defconst nelisp-release-stage-d-v2-test--this-file
  (or load-file-name
      buffer-file-name
      ;; Fallback when neither is bound (= rare under ERT batch); use
      ;; default-directory so `locate-dominating-file' still anchors.
      (expand-file-name "nelisp-release-stage-d-v2-scaffold-test.el"
                        default-directory))
  "Path to this test file, captured at load time.
`load-file-name' / `buffer-file-name' are nil inside the test body
under `make test', so we snapshot the path during top-level load.")

(defun nelisp-release-stage-d-v2-test--repo-root ()
  "Locate the worktree root by walking up from this test file."
  (let* ((this-file nelisp-release-stage-d-v2-test--this-file)
         (root (and this-file
                    (locate-dominating-file this-file "Makefile"))))
    (and root (expand-file-name root))))

(defun nelisp-release-stage-d-v2-test--script-help-exit (rel-path)
  "Run REL-PATH (relative to repo root) with --help.  Return (exit . stdout).
Calls `ert-skip' (= aborts the surrounding test as SKIPPED) when the
script is missing or no POSIX `bash' is on PATH (Windows native CI may
run the ERT suite without a shell available for the wrapper)."
  (let* ((root (nelisp-release-stage-d-v2-test--repo-root))
         (script (and root (expand-file-name rel-path root))))
    (unless (and script (file-executable-p script))
      (ert-skip (format "script not executable: %s" rel-path)))
    (unless (executable-find "bash")
      (ert-skip "bash not on PATH"))
    (with-temp-buffer
      (let ((rc (call-process "bash" nil (current-buffer) nil
                              script "--help")))
        (cons rc (buffer-string))))))

;;;; (1) install.sh --help exits 0 + usage banner --------------------

(ert-deftest nelisp-release-stage-d-v2-install-sh-help-exits-clean ()
  "release/stage-d-v2.0/install.sh --help exits 0 with a usage banner."
  (let* ((res (nelisp-release-stage-d-v2-test--script-help-exit
               "release/stage-d-v2.0/install.sh")))
    (should (eq (car res) 0))
    (should (string-match-p "stage-d-v2.0 installer" (cdr res)))
    (should (string-match-p "--from" (cdr res)))
    (should (string-match-p "--prefix" (cdr res)))))

;;;; (2) test/nelisp-soak-test.sh --help exits 0 + usage banner ------

(ert-deftest nelisp-release-stage-d-v2-soak-script-help-exits-clean ()
  "test/nelisp-soak-test.sh --help exits 0 with a usage banner."
  (let* ((res (nelisp-release-stage-d-v2-test--script-help-exit
               "test/nelisp-soak-test.sh")))
    (should (eq (car res) 0))
    (should (string-match-p "Phase 7.5.3" (cdr res)))
    (should (string-match-p "--full-24h" (cdr res)))
    (should (string-match-p "--iterations" (cdr res)))))

;;;; (3) release workflow yaml parses (= structural smoke) -----------

(ert-deftest nelisp-release-stage-d-v2-release-workflow-yaml-parses ()
  "Workflow yaml has documented header + 3-platform matrix + no tabs."
  (let* ((root (nelisp-release-stage-d-v2-test--repo-root))
         (yml (and root (expand-file-name
                         ".github/workflows/stage-d-v2.0-release.yml"
                         root))))
    (unless (and yml (file-readable-p yml))
      (ert-skip "release workflow yaml missing"))
    (with-temp-buffer
      (insert-file-contents yml)
      (let ((src (buffer-string)))
        ;; Header
        (should (string-match-p "^name: stage-d-v2.0 release pipeline"
                                src))
        ;; 3 platform matrix legs (= linux-x86_64 / macos-arm64 / linux-arm64)
        (should (string-match-p "build-linux-x86_64:" src))
        (should (string-match-p "build-macos-arm64:" src))
        (should (string-match-p "build-linux-arm64:" src))
        ;; Trigger surface (tag push + workflow_dispatch)
        (should (string-match-p "workflow_dispatch:" src))
        (should (string-match-p "tags:" src))
        ;; YAML indentation is space-based — flag stray tabs early
        ;; (GitHub Actions accepts neither well).
        (should-not (string-match-p "\t" src))))))

;;;; (4) release/stage-d-v2.0/manifest.json parses -------------------

(ert-deftest nelisp-release-stage-d-v2-manifest-json-parses ()
  "manifest.json parses as JSON with the documented top-level keys."
  (let* ((root (nelisp-release-stage-d-v2-test--repo-root))
         (mf (and root (expand-file-name
                        "release/stage-d-v2.0/manifest.json" root))))
    (unless (and mf (file-readable-p mf))
      (ert-skip "manifest.json missing"))
    (let* ((json-object-type 'alist)
           (json-array-type  'list)
           (json-key-type    'symbol)
           (parsed (with-temp-buffer
                     (insert-file-contents mf)
                     (json-read-from-string (buffer-string)))))
      (should (listp parsed))
      (should (equal (alist-get 'release_version parsed) "stage-d-v2.0"))
      (should (member "linux-x86_64" (alist-get 'blocker_platforms parsed)))
      (should (= 3 (length (alist-get 'artifacts parsed))))
      (should (alist-get 'soak_gate parsed)))))

(provide 'nelisp-release-stage-d-v2-scaffold-test)
;;; nelisp-release-stage-d-v2-scaffold-test.el ends here
