;;; nelisp-integration-release-artifact-test.el --- Phase 7.5.3 verifier ERT  -*- lexical-binding: t; -*-

;; Doc 32 v2 LOCKED 2026-04-25 §3.3 prep — +6 ERT covering the
;; Phase 7.5.3 release artifact verifier
;; (`nelisp-integration-release-artifact-verify') introduced in this
;; sub-step.  The verifier is a pure-elisp structural check (file
;; existence + recomputed SHA-256 + ad-hoc signature payload) so the
;; tests do not need a Rust toolchain or a built tarball — they
;; synthesize a minimal `dist/' under `make-temp-file' and assert the
;; verifier walks every documented `:missing' branch.
;;
;; Coverage:
;;
;;   1. paths-helper-returns-correct-shape
;;        — the (tarball checksum sig) triple is well-formed when
;;          dist/ exists, and nil when dist/ is absent.
;;   2. verify-reports-dist-dir-missing
;;        — :dist-dir-missing dominates everything else when there is
;;          no dist/ at all.
;;   3. verify-reports-tarball-missing
;;        — :tarball-missing fires when dist/ exists but the tarball
;;          does not.
;;   4. verify-reports-checksum-mismatch
;;        — recorded SHA-256 differs from recomputed → :checksum-mismatch.
;;   5. verify-green-path-on-fully-built-fixture
;;        — happy path: tarball + matching checksum + valid signature
;;          → :ready t with zero `:missing' entries.
;;   6. verify-reports-signature-platform-mismatch
;;        — signature payload references the wrong platform name.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-integration)

;;;; Fixture helpers --------------------------------------------------

(defun nelisp-release-artifact-test--scratch-root ()
  "Create a fresh scratch worktree root with src/ + Makefile + dist/.
The verifier's locator walks up from `nelisp-cc-runtime--this-file'
looking for a Makefile — but we cannot relocate the loaded file path
at runtime.  So instead we redirect the locator to the scratch root
via a `cl-letf' on the helper.  Returns the absolute root path; the
caller is responsible for cleanup via `delete-directory ... t'."
  (let* ((root (file-name-as-directory
                (make-temp-file "nelisp-release-artifact-test-" t))))
    (make-directory (expand-file-name "src" root) t)
    (make-directory (expand-file-name "dist" root) t)
    ;; Sentinel Makefile so `locate-dominating-file' anchors here.
    (with-temp-file (expand-file-name "Makefile" root)
      (insert "# scratch fixture for nelisp-integration release verifier ERT\n"))
    root))

(defmacro nelisp-release-artifact-test--with-scratch (root-sym &rest body)
  "Bind ROOT-SYM to a scratch worktree root and rebind the dist locator.
BODY runs with `nelisp-integration--release-artifact-locate-dist'
intercepted so it returns ROOT-SYM/dist (or nil when that directory
does not exist).  The scratch root is deleted on unwind."
  (declare (indent 1))
  `(let ((,root-sym (nelisp-release-artifact-test--scratch-root)))
     (unwind-protect
         (cl-letf (((symbol-function
                     'nelisp-integration--release-artifact-locate-dist)
                    (lambda ()
                      (let ((d (expand-file-name "dist" ,root-sym)))
                        (and (file-directory-p d) d)))))
           ,@body)
       (when (file-directory-p ,root-sym)
         (delete-directory ,root-sym t)))))

(defun nelisp-release-artifact-test--write-tarball (path content)
  "Write CONTENT (string) at PATH binary-safely so SHA-256 is stable."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert content))))

(defun nelisp-release-artifact-test--write-checksum (path tarball-path
                                                          tarball-name)
  "Write a `sha256sum'-shaped line to PATH for the contents of TARBALL-PATH.
TARBALL-NAME is the bare basename embedded in the line."
  (let ((hash (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally tarball-path)
                (downcase (secure-hash 'sha256 (current-buffer))))))
    (with-temp-file path
      (insert (format "%s  %s\n" hash tarball-name)))))

(defun nelisp-release-artifact-test--write-signature (path version platform)
  "Write a documented ad-hoc signature payload to PATH."
  (with-temp-file path
    (insert (format "ad-hoc-signature %s %s 2026-04-26T00:00:00Z\n"
                    version platform))))

;;;; (1) paths helper shape -----------------------------------------

(ert-deftest nelisp-integration-release-artifact-paths-helper-returns-correct-shape ()
  "The internal paths helper returns a 3-tuple under dist/ and nil
when dist/ is absent.  The triple shape is the contract every
downstream branch of the verifier depends on."
  (nelisp-release-artifact-test--with-scratch root
    (let* ((paths (nelisp-integration--release-artifact-paths
                   "stage-d-v2.0" "linux-x86_64")))
      (should (= (length paths) 3))
      (let ((tarball (nth 0 paths))
            (checksum (nth 1 paths))
            (sig      (nth 2 paths)))
        (should (string-suffix-p
                 "stage-d-v2.0-linux-x86_64.tar.gz" tarball))
        (should (string-suffix-p
                 "stage-d-v2.0-linux-x86_64.tar.gz.sha256" checksum))
        (should (string-suffix-p
                 "stage-d-v2.0-linux-x86_64.tar.gz.sig" sig))
        ;; All three live under the scratch dist/.
        (let ((dist (expand-file-name "dist" root)))
          (should (string-prefix-p (file-name-as-directory dist) tarball))
          (should (string-prefix-p (file-name-as-directory dist) checksum))
          (should (string-prefix-p (file-name-as-directory dist) sig))))))
  ;; And nil when dist/ is absent.
  (cl-letf (((symbol-function
              'nelisp-integration--release-artifact-locate-dist)
             (lambda () nil)))
    (should (null (nelisp-integration--release-artifact-paths
                   "stage-d-v2.0" "linux-x86_64")))))

;;;; (2) :dist-dir-missing dominates --------------------------------

(ert-deftest nelisp-integration-release-artifact-verify-reports-dist-dir-missing ()
  "When dist/ itself is absent the verifier returns :ready nil with
exactly one `:missing' entry: `:dist-dir-missing'."
  (cl-letf (((symbol-function
              'nelisp-integration--release-artifact-locate-dist)
             (lambda () nil)))
    (let* ((r (nelisp-integration-release-artifact-verify
               "stage-d-v2.0" "linux-x86_64"))
           (missing (plist-get r :missing)))
      (should (eq (plist-get r :ready) nil))
      (should (= (length missing) 1))
      (should (assq :dist-dir-missing missing))
      (should (eq (plist-get r :signature-status) :missing))
      ;; tarball / checksum / signature paths are nil in this branch.
      (should (null (plist-get r :tarball)))
      (should (null (plist-get r :checksum)))
      (should (null (plist-get r :signature))))))

;;;; (3) :tarball-missing -------------------------------------------

(ert-deftest nelisp-integration-release-artifact-verify-reports-tarball-missing ()
  "When dist/ exists but the tarball is absent the verifier reports
`:tarball-missing'.  The companion checksum + signature absence are
also reported in the same call so a single verify run surfaces every
gap simultaneously."
  (nelisp-release-artifact-test--with-scratch _root
    (let* ((r (nelisp-integration-release-artifact-verify
               "stage-d-v2.0" "linux-x86_64"))
           (missing (plist-get r :missing)))
      (should (eq (plist-get r :ready) nil))
      (should (assq :tarball-missing missing))
      (should (assq :checksum-missing missing))
      (should (assq :signature-missing missing))
      (should (eq (plist-get r :signature-status) :missing)))))

;;;; (4) :checksum-mismatch -----------------------------------------

(ert-deftest nelisp-integration-release-artifact-verify-reports-checksum-mismatch ()
  "When the recorded SHA-256 differs from the recomputed value the
verifier reports `:checksum-mismatch' with both hashes captured in
the result plist for triage."
  (nelisp-release-artifact-test--with-scratch root
    (let* ((dist (expand-file-name "dist" root))
           (tarball (expand-file-name
                     "stage-d-v2.0-linux-x86_64.tar.gz" dist))
           (checksum (concat tarball ".sha256"))
           (sig (concat tarball ".sig")))
      (nelisp-release-artifact-test--write-tarball
       tarball "real-tarball-bytes")
      ;; Hand-write a checksum line with a wrong hash.
      (with-temp-file checksum
        (insert (format "%s  stage-d-v2.0-linux-x86_64.tar.gz\n"
                        (make-string 64 ?0))))
      (nelisp-release-artifact-test--write-signature
       sig "stage-d-v2.0" "linux-x86_64")
      (let* ((r (nelisp-integration-release-artifact-verify
                 "stage-d-v2.0" "linux-x86_64"))
             (missing (plist-get r :missing)))
        (should (eq (plist-get r :ready) nil))
        (should (assq :checksum-mismatch missing))
        (should (stringp (plist-get r :checksum-recomputed)))
        (should (= (length (plist-get r :checksum-recomputed)) 64))
        (should (string= (plist-get r :checksum-recorded)
                         (make-string 64 ?0)))))))

;;;; (5) green path -------------------------------------------------

(ert-deftest nelisp-integration-release-artifact-verify-green-path-on-fully-built-fixture ()
  "When the tarball, checksum, and signature are all present and
internally consistent the verifier returns `:ready t' with no
`:missing' entries.  This exercises the full happy path so a
regression in any of the three checks bubbles up here."
  (nelisp-release-artifact-test--with-scratch root
    (let* ((dist (expand-file-name "dist" root))
           (tarball-name "stage-d-v2.0-linux-x86_64.tar.gz")
           (tarball (expand-file-name tarball-name dist))
           (checksum (concat tarball ".sha256"))
           (sig (concat tarball ".sig")))
      (nelisp-release-artifact-test--write-tarball
       tarball "fixture-tarball-payload")
      (nelisp-release-artifact-test--write-checksum
       checksum tarball tarball-name)
      (nelisp-release-artifact-test--write-signature
       sig "stage-d-v2.0" "linux-x86_64")
      (let ((r (nelisp-integration-release-artifact-verify
                "stage-d-v2.0" "linux-x86_64")))
        (should (eq (plist-get r :ready) t))
        (should (null (plist-get r :missing)))
        (should (eq (plist-get r :signature-status) t))
        (should (string= (plist-get r :checksum-recorded)
                         (plist-get r :checksum-recomputed)))
        (should (string= (plist-get r :version) "stage-d-v2.0"))
        (should (string= (plist-get r :platform) "linux-x86_64"))))))

;;;; (6) :signature-platform-mismatch -------------------------------

(ert-deftest nelisp-integration-release-artifact-verify-reports-signature-platform-mismatch ()
  "A signature payload that references the wrong platform name must
produce `:signature-platform-mismatch'.  This guards against a CI
matrix bug where one platform leg accidentally signs another's
artifact."
  (nelisp-release-artifact-test--with-scratch root
    (let* ((dist (expand-file-name "dist" root))
           (tarball-name "stage-d-v2.0-linux-x86_64.tar.gz")
           (tarball (expand-file-name tarball-name dist))
           (checksum (concat tarball ".sha256"))
           (sig (concat tarball ".sig")))
      (nelisp-release-artifact-test--write-tarball
       tarball "platform-mismatch-payload")
      (nelisp-release-artifact-test--write-checksum
       checksum tarball tarball-name)
      ;; Sign with the WRONG platform on purpose.
      (nelisp-release-artifact-test--write-signature
       sig "stage-d-v2.0" "macos-arm64")
      (let* ((r (nelisp-integration-release-artifact-verify
                 "stage-d-v2.0" "linux-x86_64"))
             (missing (plist-get r :missing)))
        (should (eq (plist-get r :ready) nil))
        (should (assq :signature-platform-mismatch missing))
        (should (eq (plist-get r :signature-status)
                    :platform-mismatch))))))

(provide 'nelisp-integration-release-artifact-test)

;;; nelisp-integration-release-artifact-test.el ends here
