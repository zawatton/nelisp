;;; nelisp-runtime-test.el --- Phase 7.0 Rust syscall stub ERT smoke  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Subprocess-level smoke for the Phase 7.0 Rust syscall stub.  These
;; tests deliberately do *not* dlsym the cdylib from Emacs — that path
;; is Phase 7.5 territory.  Instead we kick the `nelisp-runtime' CLI
;; binary built by `cargo build --release' and assert on its exit
;; status / stdout, which is what Doc 27 §3 7.0 gate calls for.
;;
;; The binary lives next to the `Makefile' under
;; `nelisp-runtime/target/release/'.  When the binary is missing we
;; *skip* (not fail) so a fresh checkout that has not yet run
;; `make runtime' still produces a green ERT total — the same
;; convention the bench targets use.  CI / `make test-runtime'
;; depends on `runtime' so the full pipeline still proves the smoke.

;;; Code:

(require 'ert)

(defconst nelisp-runtime-test--repo-root
  (or (getenv "NELISP_REPO_ROOT")
      (let ((dir (locate-dominating-file
                  (or load-file-name
                      buffer-file-name
                      default-directory)
                  "Makefile")))
        (and dir (file-name-as-directory (expand-file-name dir)))))
  "Absolute path to the NeLisp worktree root, or nil if not found.")

(defconst nelisp-runtime-test--bin
  (and nelisp-runtime-test--repo-root
       (expand-file-name "nelisp-runtime/target/release/nelisp-runtime"
                         nelisp-runtime-test--repo-root))
  "Path to the `nelisp-runtime' binary built by `cargo build --release'.")

(defconst nelisp-runtime-test--cdylib-candidates
  (and nelisp-runtime-test--repo-root
       (mapcar (lambda (name)
                 (expand-file-name
                  (concat "nelisp-runtime/target/release/" name)
                  nelisp-runtime-test--repo-root))
               '("libnelisp_runtime.so"
                 "libnelisp_runtime.dylib"
                 "nelisp_runtime.dll")))
  "Candidate paths for the cdylib built alongside the binary.")

;; Phase 7.5.1 (Doc 32 v2 LOCKED §3.1) — staticlib path next to the
;; cdylib.  Same target dir, single file (Linux/macOS use `.a',
;; Windows MSVC uses `nelisp_runtime.lib').  We probe both names and
;; let the test pick the first that exists.
(defconst nelisp-runtime-test--staticlib-candidates
  (and nelisp-runtime-test--repo-root
       (mapcar (lambda (name)
                 (expand-file-name
                  (concat "nelisp-runtime/target/release/" name)
                  nelisp-runtime-test--repo-root))
               '("libnelisp_runtime.a"
                 "nelisp_runtime.lib")))
  "Candidate paths for the staticlib built alongside the cdylib.")

(defun nelisp-runtime-test--skip-unless-built ()
  "Skip the current ERT unless the Rust binary has been built."
  (unless (and nelisp-runtime-test--bin
               (file-executable-p nelisp-runtime-test--bin))
    (ert-skip
     (format "nelisp-runtime binary not built — run `make runtime' (looked at %s)"
             nelisp-runtime-test--bin))))

(defun nelisp-runtime-test--run (&rest args)
  "Run the runtime binary with ARGS and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (apply #'call-process nelisp-runtime-test--bin
                       nil t nil args)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest nelisp-runtime-binary-exists ()
  "Phase 7.0 cargo build --release must produce an executable."
  (nelisp-runtime-test--skip-unless-built)
  (should (file-executable-p nelisp-runtime-test--bin)))

(ert-deftest nelisp-runtime-syscall-smoke-exit-0 ()
  "`--syscall-smoke' must exit 0 on a healthy host."
  (nelisp-runtime-test--skip-unless-built)
  (let ((r (nelisp-runtime-test--run "--syscall-smoke")))
    (should (eq (car r) 0))))

(ert-deftest nelisp-runtime-syscall-smoke-stdout ()
  "`--syscall-smoke' must print the canonical OK marker."
  (nelisp-runtime-test--skip-unless-built)
  (let ((r (nelisp-runtime-test--run "--syscall-smoke")))
    (should (string-match-p "syscall smoke OK" (cdr r)))))

(ert-deftest nelisp-runtime-help-exits-2 ()
  "Calling the binary with no subcommand must exit 2 (usage)."
  (nelisp-runtime-test--skip-unless-built)
  (let ((code (call-process nelisp-runtime-test--bin nil nil nil)))
    (should (eq code 2))))

(ert-deftest nelisp-runtime-cdylib-exists ()
  "cargo must produce a cdylib next to the binary so Phase 7.5 can dlsym."
  (nelisp-runtime-test--skip-unless-built)
  (should (cl-some #'file-exists-p nelisp-runtime-test--cdylib-candidates)))

;; ---------------------------------------------------------------------------
;; MAP_JIT update (Doc 28 v2 §6.9) — three additional FFI symbols feed
;; the Phase 7.1.3 arm64 native backend.  The ERT layer only proves
;; the cdylib exports them; cargo test exercises the live no-op paths.
;; ---------------------------------------------------------------------------

(defun nelisp-runtime-test--cdylib ()
  "Return the first cdylib candidate that exists, or nil."
  (cl-find-if #'file-exists-p nelisp-runtime-test--cdylib-candidates))

(defun nelisp-runtime-test--cdylib-exports-symbol-p (sym)
  "Return non-nil if SYM appears in the cdylib's dynamic symbol table.
Uses `nm -D' on Linux / macOS.  Skips cleanly when `nm' is missing
so the test stays portable on hosts without binutils."
  (let ((cdylib (nelisp-runtime-test--cdylib))
        (nm (executable-find "nm")))
    (cond
     ((null cdylib) (ert-skip "cdylib not built"))
     ((null nm)     (ert-skip "nm not on PATH"))
     (t
      (with-temp-buffer
        (let ((rc (call-process nm nil t nil "-D" cdylib)))
          (and (eq rc 0)
               (goto-char (point-min))
               (re-search-forward
                (concat "\\b" (regexp-quote sym) "\\b")
                nil t))))))))

(ert-deftest nelisp-runtime-mmap-jit-symbol-exists ()
  "cdylib must export `nelisp_syscall_mmap_jit' for Phase 7.1.3 arm64 backend."
  (nelisp-runtime-test--skip-unless-built)
  (should (nelisp-runtime-test--cdylib-exports-symbol-p
           "nelisp_syscall_mmap_jit")))

(ert-deftest nelisp-runtime-clear-icache-symbol-exists ()
  "cdylib must export `nelisp_syscall_clear_icache' for arm64 I-cache flush."
  (nelisp-runtime-test--skip-unless-built)
  (should (nelisp-runtime-test--cdylib-exports-symbol-p
           "nelisp_syscall_clear_icache")))

(ert-deftest nelisp-runtime-jit-write-protect-symbol-exists ()
  "cdylib must export `nelisp_syscall_jit_write_protect' for Apple Silicon W^X."
  (nelisp-runtime-test--skip-unless-built)
  (should (nelisp-runtime-test--cdylib-exports-symbol-p
           "nelisp_syscall_jit_write_protect")))

;; ---------------------------------------------------------------------------
;; Phase 7.5.1 (Doc 32 v2 LOCKED §3.1) — staticlib build.
;; cargo build --release with crate-type = ["cdylib", "rlib", "staticlib"]
;; must produce libnelisp_runtime.a alongside the .so / .dylib.  These
;; smokes prove the dual-output is intact so the upcoming
;; `--strict-no-emacs' (Phase 7.5.2) static-link path has its
;; prerequisite artifact ready.  Each test skips cleanly when the
;; staticlib is missing (= `make runtime' / `make runtime-static' has
;; not been run on this checkout) so the green baseline is preserved
;; on hosts without a Rust toolchain.
;; ---------------------------------------------------------------------------

(defun nelisp-runtime-test--staticlib ()
  "Return the first staticlib candidate that exists, or nil."
  (cl-find-if #'file-exists-p nelisp-runtime-test--staticlib-candidates))

(defun nelisp-runtime-test--skip-unless-staticlib-built ()
  "Skip the current ERT unless the Rust staticlib has been built."
  (unless (nelisp-runtime-test--staticlib)
    (ert-skip
     (format
      "staticlib not built — run `make runtime-static' (looked at %s)"
      (mapconcat #'identity
                 nelisp-runtime-test--staticlib-candidates ", ")))))

(ert-deftest nelisp-runtime-staticlib-exists ()
  "cargo must produce a staticlib (.a) alongside the cdylib so the
Phase 7.5.2 static-link path has its prerequisite artifact.  This
is the load-bearing acceptance check for the `runtime-static'
Makefile target."
  (nelisp-runtime-test--skip-unless-built)
  (nelisp-runtime-test--skip-unless-staticlib-built)
  (should (cl-some #'file-exists-p
                   nelisp-runtime-test--staticlib-candidates)))

(ert-deftest nelisp-runtime-staticlib-and-cdylib-coexist ()
  "Phase 7.5.1 dual-output build: when *either* artifact is present,
both should be present.  Mismatched output (e.g. only .so produced
because Cargo.toml lost `staticlib' from crate-type) is a red flag
that the `make runtime' / `make runtime-static' pair has drifted
out of sync with the Cargo.toml `[lib].crate-type' list."
  (nelisp-runtime-test--skip-unless-built)
  (let ((cdylib  (nelisp-runtime-test--cdylib))
        (staticl (nelisp-runtime-test--staticlib)))
    ;; If neither is built, skip — fresh checkout, no toolchain.
    (when (and (null cdylib) (null staticl))
      (ert-skip "neither cdylib nor staticlib built — run `make runtime-static'"))
    ;; Otherwise both must be present (dual-output build invariant).
    (should cdylib)
    (should staticl)))

(ert-deftest nelisp-runtime-staticlib-is-ar-archive ()
  "The staticlib (.a) must be a real `ar' archive, not a stub.
Mirrors the `make runtime-static' Makefile smoke check (`file' must
report `current ar archive') so the assertion is reachable from
both the ERT layer and the build pipeline."
  (nelisp-runtime-test--skip-unless-built)
  (nelisp-runtime-test--skip-unless-staticlib-built)
  (let ((path (nelisp-runtime-test--staticlib))
        (file-bin (executable-find "file")))
    (cond
     ((null file-bin)
      (ert-skip "`file' not on PATH — cannot verify ar archive header"))
     ;; .lib (Windows MSVC) is not a Unix ar archive — skip the magic
     ;; check on that path, the `file-exists-p' assertion above is the
     ;; load-bearing one for MSVC builds.
     ((string-match-p "\\.lib\\'" path)
      (ert-skip "Windows MSVC .lib is not a Unix ar archive — header check N/A"))
     (t
      (with-temp-buffer
        (let ((rc (call-process file-bin nil t nil path)))
          (should (eq rc 0))
          (goto-char (point-min))
          (should (re-search-forward "ar archive" nil t))))))))

(provide 'nelisp-runtime-test)
;;; nelisp-runtime-test.el ends here
