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

(ert-deftest nelisp-standalone-tarball-scripts-and-targets-exist ()
  (let ((makefile (nelisp-standalone-tarball-test--read "Makefile"))
        (build (nelisp-standalone-tarball-test--read "tools/build-standalone-tarball.sh"))
        (verify (nelisp-standalone-tarball-test--read "tools/verify-standalone-tarball.sh")))
    (should (string-match-p "stage-d-v3-tarball" makefile))
    (should (string-match-p "stage-d-v3-tarball-verify" makefile))
    (should (string-match-p "build-standalone-tarball.sh" makefile))
    (should (string-match-p "verify-standalone-tarball.sh" makefile))
    (should (string-match-p "anvil-runtime" build))
    (should (string-match-p "README-stage-d-v3.0.org" build))
    (should (string-match-p "install-v3.sh" build))
    (should (string-match-p "15 MB cap" build))
    (should (string-match-p "tools/list" verify))
    (should (string-match-p "anvil-host-info" verify))))

(ert-deftest nelisp-standalone-tarball-launcher-docs-mention-standalone ()
  (let ((launcher (nelisp-standalone-tarball-test--read "bin/anvil"))
        (readme (nelisp-standalone-tarball-test--read "README-stage-d-v3.0.org"))
        (installer (nelisp-standalone-tarball-test--read "release/stage-d-v3.0/install-v3.sh")))
    (should (string-match-p "v3.0 standalone (= no emacs)" launcher))
    (should (string-match-p "bin/anvil-runtime" launcher))
    (should (string-match-p "ANVIL_EL_DIR" launcher))
    (should (string-match-p "NELISP_SRC_DIR" launcher))
    (should (string-match-p "true standalone tarball" readme))
    (should (string-match-p "install-v3.sh" readme))
    (should (string-match-p "stage-d-v3.0 installer" installer))))

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
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/bin/anvil\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/bin/anvil-runtime\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/anvil-lib/anvil-host\\.el\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/anvil-lib/anvil-shell-filter\\.el\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/anvil-lib/anvil-data\\.el\n" listing))
    (should (string-match-p "anvil-stage-d-v3.0-linux-x86_64/lib/libnelisp_runtime\\.so\n" listing))))

(provide 'nelisp-standalone-tarball-test)
;;; nelisp-standalone-tarball-test.el ends here
