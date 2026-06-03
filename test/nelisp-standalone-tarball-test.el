;;; nelisp-standalone-tarball-test.el --- stage-d-v3.0 standalone tarball ERT -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-standalone-tarball-test--repo-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-standalone-tarball-test--path (rel)
  (expand-file-name rel nelisp-standalone-tarball-test--repo-root))

(defun nelisp-standalone-tarball-test--read (rel)
  (with-temp-buffer
    (insert-file-contents (nelisp-standalone-tarball-test--path rel))
    (buffer-string)))

(defun nelisp-standalone-tarball-test--call-script (&rest args)
  "Run the tarball build script with ARGS and return (STATUS . OUTPUT)."
  (let ((buf (generate-new-buffer " *nelisp-standalone-tarball-script*")))
    (unwind-protect
        (let ((status (apply #'call-process
                             (nelisp-standalone-tarball-test--path
                              "tools/build-standalone-tarball.sh")
                             nil (list buf t) nil args)))
          (cons status
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer buf))))

(ert-deftest nelisp-standalone-tarball-scripts-and-targets-exist ()
  (let ((makefile (nelisp-standalone-tarball-test--read "Makefile"))
        (build (nelisp-standalone-tarball-test--read "tools/build-standalone-tarball.sh"))
        (verify (nelisp-standalone-tarball-test--read "tools/verify-standalone-tarball.sh"))
        (installer (nelisp-standalone-tarball-test--read "release/stage-d-v3.0/install-v3.sh")))
    (should (string-match-p "standalone-tarball:" makefile))
    (should (string-match-p "standalone-tarball-verify:" makefile))
    (should (string-match-p "build-standalone-tarball.sh" makefile))
    (should (string-match-p "verify-standalone-tarball.sh" makefile))
    (should (string-match-p "--emacs \"\\$(EMACS)\"" makefile))
    (should (string-match-p "--emacs EMACS" build))
    (should (string-match-p "EMACS=\"\\$EMACS_BIN\" NELISP_STANDALONE_TARGET=\"\\$PLATFORM\" make standalone-reader" build))
    (should (string-match-p "macos-aarch64" build))
    (should (string-match-p "windows-x86_64" build))
    (should (string-match-p "bin/nelisp" build))
    (should (string-match-p "bin/nelisp.exe" build))
    (should (string-match-p "README-stage-d-v3.0.org" build))
    (should (string-match-p "15 MB cap" build))
    (should (string-match-p "bin/\\$NELISP_BIN_NAME eval" verify))
    (should (string-match-p "repl --no-prompt" verify))
    (should (string-match-p "zero-Rust standalone tarball smoke PASS" verify))
    (should (string-match-p "Darwin-arm64) echo \"macos-aarch64\"" installer))
    (should (string-match-p "windows-x86_64" installer))))

(ert-deftest nelisp-standalone-tarball-build-script-usage-is-checked-before-build ()
  "The POSIX tarball builder rejects usage errors before invoking make."
  (pcase-let ((`(,help-status . ,help-out)
               (nelisp-standalone-tarball-test--call-script "--help"))
              (`(,missing-status . ,missing-out)
               (nelisp-standalone-tarball-test--call-script "--emacs"))
              (`(,unknown-status . ,unknown-out)
               (nelisp-standalone-tarball-test--call-script "--bogus")))
    (should (= help-status 0))
    (should (string-match-p "usage: .*\\[--emacs EMACS\\]" help-out))
    (should-not (string-match-p "building standalone reader" help-out))
    (should (= missing-status 2))
    (should (string-match-p "usage: .*\\[--emacs EMACS\\]" missing-out))
    (should-not (string-match-p "building standalone reader" missing-out))
    (should (= unknown-status 2))
    (should (string-match-p "usage: .*\\[--emacs EMACS\\]" unknown-out))
    (should-not (string-match-p "building standalone reader" unknown-out))))

(ert-deftest nelisp-standalone-tarball-scripts-stay-zero-rust ()
  (let ((build (nelisp-standalone-tarball-test--read "tools/build-standalone-tarball.sh"))
        (verify (nelisp-standalone-tarball-test--read "tools/verify-standalone-tarball.sh")))
    (should-not (string-match-p "\\_<cargo\\_>" build))
    (should-not (string-match-p "\\_<rustc\\_>" build))
    (should-not (string-match-p "anvil-runtime" build))
    (should-not (string-match-p "libnelisp_runtime" build))
    (should-not (string-match-p "cp bin/anvil" build))
    (should-not (string-match-p "\\_<cargo\\_>" verify))
    (should-not (string-match-p "\\_<rustc\\_>" verify))
    (should-not (string-match-p "anvil-runtime" verify))
    (should-not (string-match-p "libnelisp_runtime" verify))))

(ert-deftest nelisp-standalone-tarball-built-fixture-has-expected-shape ()
  (skip-unless (equal "1" (getenv "NELISP_STANDALONE_TARBALL_BUILT")))
  (let* ((root nelisp-standalone-tarball-test--repo-root)
         (tarball (expand-file-name "dist/anvil-stage-d-v3.0-linux-x86_64.tar.gz" root))
         (sha (concat tarball ".sha256"))
         (listing (and (file-exists-p tarball)
                       (with-temp-buffer
                         (process-file "tar" nil t nil "-tzf" tarball)
                         (buffer-string)))))
    (should (file-exists-p tarball))
    (should (file-exists-p sha))
    (should (< (file-attribute-size (file-attributes tarball)) (* 15 1024 1024)))
    (should-not (string-match-p "anvil-stage-d-v3.0-linux-x86_64/bin/anvil\n" listing))
    (should-not (string-match-p "anvil-stage-d-v3.0-linux-x86_64/bin/anvil.cmd\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/bin/nelisp\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/src/nelisp\\.el\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/scripts/nelisp-standalone-build\\.el\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/lisp/nelisp-static-linker\\.el\n" listing))))

(provide 'nelisp-standalone-tarball-test)
;;; nelisp-standalone-tarball-test.el ends here
