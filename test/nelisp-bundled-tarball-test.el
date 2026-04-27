;;; nelisp-bundled-tarball-test.el --- Phase 7.5.3 stage-d-v2.0 bundled-Emacs ERT  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 32 v2 LOCKED §3.3 — Phase 7.5.3 stage-d-v2.0 bundled-Emacs
;; tarball ERT smoke.  The actual `make stage-d-v2-tarball' target
;; bundles a stripped Emacs binary (~22 MB) which is too heavy for the
;; default ERT pass to build on every run.  Instead we cover the
;; *invariants* that the tarball pipeline depends on:
;;
;;   1. bundled-emacs-detect-prefers-anvil-home
;;        — `bin/anvil' detect_emacs() resolution puts
;;          $ANVIL_HOME/emacs/bin/emacs ahead of system PATH.  Run the
;;          launcher's `version' command with a synthesized
;;          $ANVIL_HOME containing a dummy bundled emacs and confirm
;;          the version report flags it as `(bundled)'.
;;   2. bundled-emacs-detect-falls-back-when-bundled-missing
;;        — without a bundled emacs the launcher reports `(system)'
;;          (= backward-compat path for dev checkouts).
;;   3. tarball-script-exists-and-is-executable
;;        — `tools/build-bundled-tarball.sh' is present + executable +
;;          documents the expected file list.
;;   4. tarball-verify-script-exists-and-is-executable
;;        — `tools/verify-bundled-tarball.sh' ditto.
;;   5. makefile-stage-d-v2-tarball-target-defined
;;        — Makefile declares `stage-d-v2-tarball' + `stage-d-v2-tarball-verify'
;;          on .PHONY and defines the rule body.
;;   6. tarball-built-fixture-has-expected-shape (skipped unless
;;        $NELISP_BUNDLED_TARBALL_BUILT=1)
;;        — when the tarball has been pre-built (= `make stage-d-v2-tarball'
;;          ran), assert it ships every documented top-level entry.
;;          Skipped by default to keep CI fast; the verify-script smoke
;;          covers the runtime-side assertion path.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-bundled-tarball-test--repo-root
  (let ((dir (locate-dominating-file
              (or load-file-name buffer-file-name default-directory)
              "Makefile")))
    (and dir (file-name-as-directory (expand-file-name dir))))
  "Absolute path to the NeLisp worktree root, or nil if not found.")

(defun nelisp-bundled-tarball-test--path (rel)
  (expand-file-name rel nelisp-bundled-tarball-test--repo-root))

(defun nelisp-bundled-tarball-test--read (rel)
  (with-temp-buffer
    (insert-file-contents (nelisp-bundled-tarball-test--path rel))
    (buffer-string)))

(defun nelisp-bundled-tarball-test--bash-available-p ()
  (and (executable-find "bash")
       (memq system-type '(gnu/linux darwin berkeley-unix))))

;;;; (1) bundled-emacs detect prefers $ANVIL_HOME/emacs/bin/emacs ----

(ert-deftest nelisp-bundled-tarball-detect-prefers-anvil-home ()
  "When $ANVIL_HOME/emacs/bin/emacs is executable the launcher uses
it (= reported as `(bundled)' in `anvil version' output) ahead of any
system Emacs on PATH.  Synthesizes a minimal bundle directory with the
host emacs binary symlinked into place so the dummy passes the `[[ -x
... ]]' check inside `detect_emacs'."
  (skip-unless (nelisp-bundled-tarball-test--bash-available-p))
  (let* ((host-emacs (or (executable-find "emacs")
                         (ert-skip "no host emacs to symlink")))
         (anvil-home (file-name-as-directory
                      (make-temp-file "anvil-bundled-test-" t)))
         (script (nelisp-bundled-tarball-test--path "bin/anvil"))
         (src-dst (expand-file-name "src" anvil-home))
         (bin-dst (expand-file-name "bin" anvil-home))
         (emacs-bin-dst (expand-file-name "emacs/bin" anvil-home)))
    (unwind-protect
        (progn
          ;; Synthesize the bundle layout the launcher expects.
          (make-directory src-dst t)
          (make-directory bin-dst t)
          (make-directory emacs-bin-dst t)
          ;; Symlink the launcher + host emacs into the bundle.
          (make-symbolic-link script (expand-file-name "anvil" bin-dst) t)
          (make-symbolic-link host-emacs
                              (expand-file-name "emacs" emacs-bin-dst) t)
          ;; Copy at least one nelisp src file so cmd_tools_list does
          ;; not error out before printing the version line.
          (copy-file (nelisp-bundled-tarball-test--path "src/nelisp-tools.el")
                     (expand-file-name "nelisp-tools.el" src-dst)
                     t)
          ;; Run `bin/anvil version' with $ANVIL_HOME pointing at the
          ;; synthesized bundle and confirm the bundled-emacs flag.
          ;; `bin/anvil' echoes the resolved bundled emacs as
          ;; `<ANVIL_HOME>/emacs/bin/emacs'; when ANVIL_HOME ends with
          ;; `/' (= `make-temp-file' default) the concat produces a
          ;; cosmetic double-slash — accept either via `/+' regex.
          (let* ((process-environment
                  (append (list (concat "ANVIL_HOME=" anvil-home)
                                ;; Strip $EMACS so detect_emacs walks
                                ;; the ANVIL_HOME bundle path.
                                "EMACS=")
                          process-environment))
                 (out (with-temp-buffer
                        (call-process "bash" nil (current-buffer) nil
                                      script "version")
                        (buffer-string))))
            (should (string-match-p "(bundled)" out))
            (should (string-match-p "emacs path " out))
            (should (string-match-p
                     (concat "emacs path +"
                             (regexp-quote (directory-file-name anvil-home))
                             "/+emacs/bin/emacs")
                     out))))
      (when (file-directory-p anvil-home)
        (delete-directory anvil-home t)))))

;;;; (2) backward-compat: no bundled emacs => `(system)' --------------

(ert-deftest nelisp-bundled-tarball-detect-falls-back-to-system ()
  "Without a bundled emacs the launcher reports `(system)' so dev
checkouts (= no `emacs/bin/emacs' under $ANVIL_HOME) keep working."
  (skip-unless (nelisp-bundled-tarball-test--bash-available-p))
  (skip-unless (executable-find "emacs"))
  (let* ((script (nelisp-bundled-tarball-test--path "bin/anvil"))
         ;; Stand up an empty $ANVIL_HOME so the bundled probe misses.
         (anvil-home (file-name-as-directory
                      (make-temp-file "anvil-system-fallback-test-" t)))
         (src-dst (expand-file-name "src" anvil-home))
         (bin-dst (expand-file-name "bin" anvil-home)))
    (unwind-protect
        (progn
          (make-directory src-dst t)
          (make-directory bin-dst t)
          (make-symbolic-link script (expand-file-name "anvil" bin-dst) t)
          (copy-file (nelisp-bundled-tarball-test--path "src/nelisp-tools.el")
                     (expand-file-name "nelisp-tools.el" src-dst)
                     t)
          (let* ((process-environment
                  (append (list (concat "ANVIL_HOME=" anvil-home)
                                "EMACS=")
                          process-environment))
                 (out (with-temp-buffer
                        (call-process "bash" nil (current-buffer) nil
                                      script "version")
                        (buffer-string))))
            (should (string-match-p "(system)" out))
            (should-not (string-match-p "(bundled)" out))))
      (when (file-directory-p anvil-home)
        (delete-directory anvil-home t)))))

;;;; (3) build-bundled-tarball.sh exists -----------------------------

(ert-deftest nelisp-bundled-tarball-build-script-exists ()
  "`tools/build-bundled-tarball.sh' must be present + executable +
encode the documented entry layout (bin/anvil + emacs/bin/emacs +
nelisp-runtime + sha256 emit)."
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (let* ((script (nelisp-bundled-tarball-test--path
                  "tools/build-bundled-tarball.sh"))
         (body (nelisp-bundled-tarball-test--read
                "tools/build-bundled-tarball.sh")))
    (should (file-exists-p script))
    (should (file-executable-p script))
    ;; Documented layout markers — anchor regex on the explicit dir
    ;; the build script asserts.
    (should (string-match-p "emacs/bin/emacs" body))
    (should (string-match-p "nelisp-runtime/target/release" body))
    (should (string-match-p "VERSION=\"\\${1:-stage-d-v2.0}\"" body))
    (should (string-match-p "PLATFORM=\"\\${2:-linux-x86_64}\"" body))
    ;; Tarball + checksum emit.
    (should (string-match-p "tar -czf" body))
    (should (string-match-p "sha256sum\\|shasum" body))))

;;;; (4) verify-bundled-tarball.sh exists ----------------------------

(ert-deftest nelisp-bundled-tarball-verify-script-exists ()
  "`tools/verify-bundled-tarball.sh' must be present + executable +
assert on `(bundled)' marker in `bin/anvil version' output."
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (let* ((script (nelisp-bundled-tarball-test--path
                  "tools/verify-bundled-tarball.sh"))
         (body (nelisp-bundled-tarball-test--read
                "tools/verify-bundled-tarball.sh")))
    (should (file-exists-p script))
    (should (file-executable-p script))
    (should (string-match-p "(bundled)" body))
    (should (string-match-p "tools registered" body))
    (should (string-match-p "ANVIL_HOME=" body))))

;;;; (5) Makefile targets defined ------------------------------------

(ert-deftest nelisp-bundled-tarball-makefile-targets-defined ()
  "Makefile must declare `stage-d-v2-tarball' + `stage-d-v2-tarball-verify'
on .PHONY and define a rule body for each.  Rules must reference the
two helper scripts under tools/."
  (let ((makefile (nelisp-bundled-tarball-test--read "Makefile")))
    (should (string-match-p "stage-d-v2-tarball" makefile))
    (should (string-match-p "stage-d-v2-tarball-verify" makefile))
    (should (string-match-p "tools/build-bundled-tarball.sh" makefile))
    (should (string-match-p "tools/verify-bundled-tarball.sh" makefile))
    ;; .PHONY entry (= avoid the case where someone deletes the rule
    ;; but leaves the alias which would make a stale file the de facto
    ;; target).  The .PHONY block uses backslash-newline continuation,
    ;; so the regex must walk lines until the first non-indented line
    ;; (= a fresh top-level directive).  Use `\\(?:.\\|\n\\)*?' to
    ;; span newlines in lazy mode.
    (let* ((case-fold-search nil)
           (phony-block
            (when (string-match
                   "\\.PHONY:\\(?:.\\|\n\\)*?\n[a-zA-Z]" makefile)
              (match-string 0 makefile))))
      (should phony-block)
      (should (string-match-p "stage-d-v2-tarball" phony-block)))))

;;;; (6) built fixture (opt-in via $NELISP_BUNDLED_TARBALL_BUILT=1) --

(ert-deftest nelisp-bundled-tarball-built-fixture-has-expected-shape ()
  "When opt-in env $NELISP_BUNDLED_TARBALL_BUILT=1 is set, assert the
prebuilt tarball ships every documented top-level entry.  Skipped by
default so plain `make test' stays fast (the tarball is ~22 MB and
takes ~30 s to build on a warm host)."
  (skip-unless (equal "1" (getenv "NELISP_BUNDLED_TARBALL_BUILT")))
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (skip-unless (executable-find "tar"))
  (let* ((root nelisp-bundled-tarball-test--repo-root)
         (tar  (expand-file-name
                "dist/anvil-stage-d-v2.0-linux-x86_64.tar.gz" root))
         (sha  (concat tar ".sha256")))
    (should (file-readable-p tar))
    (should (file-readable-p sha))
    ;; Walk the tarball entries; assert the documented top-level dirs
    ;; + key files appear at least once.
    (let* ((entries (split-string
                     (with-temp-buffer
                       (call-process "tar" nil (current-buffer) nil
                                     "-tzf" tar)
                       (buffer-string))
                     "\n" t))
           (found-bin nil)
           (found-emacs nil)
           (found-runtime nil)
           (found-src nil))
      (dolist (e entries)
        (cond
         ((string-match-p "/bin/anvil$" e)              (setq found-bin t))
         ((string-match-p "/emacs/bin/emacs$" e)        (setq found-emacs t))
         ((string-match-p "/nelisp-runtime/target/release/libnelisp_runtime\\." e)
          (setq found-runtime t))
         ((string-match-p "/src/nelisp-server\\.el$" e) (setq found-src t))))
      (should found-bin)
      (should found-emacs)
      (should found-runtime)
      (should found-src))))

(provide 'nelisp-bundled-tarball-test)
;;; nelisp-bundled-tarball-test.el ends here
