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
        (installer (nelisp-standalone-tarball-test--read "release/stage-d-v3.0/install-v3.sh"))
        (windows-installer (nelisp-standalone-tarball-test--read "release/stage-d-v3.0/install-v3.ps1")))
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
    (should (string-match-p "ad-hoc signing bin/nelisp for macOS arm64" build))
    (should (string-match-p "codesign -f -s - \"\\$STAGE_DIR/bin/nelisp\"" build))
    (should (string-match-p "codesign --verify \"\\$NELISP_EXE\"" verify))
    (should (string-match-p "code signature OK" verify))
    (should (string-match-p "is not signed; rebuild the tarball on macOS arm64" verify))
    (should-not (string-match-p "codesign -f -s - \"\\$NELISP_EXE\"" verify))
    (should (string-match-p "bin/\\$NELISP_BIN_NAME eval" verify))
    (should (string-match-p "repl --no-prompt" verify))
    (should (string-match-p "zero-Rust standalone tarball smoke PASS" verify))
    (should (string-match-p "Darwin-arm64) echo \"macos-aarch64\"" installer))
    (should (string-match-p "windows-x86_64" installer))
    (should (string-match-p "missing tool: codesign" installer))
    (should (string-match-p "codesign --verify \"\\${ANVIL_PREFIX}/bin/nelisp\"" installer))
    (should (string-match-p "installed bin/nelisp is not signed; rebuild the macOS tarball" installer))
    (should-not (string-match-p "codesign -f -s - \"\\${ANVIL_PREFIX}/bin/nelisp\"" installer))
    (should (string-match-p "\\[string\\]\\$Target = \"windows-x86_64\"" windows-installer))
    (should (string-match-p "Get-FileHash -Algorithm SHA256" windows-installer))
    (should (string-match-p "tar -xzf \\$TarPath -C \\$Prefix --strip-components=1" windows-installer))
    (should (string-match-p "installed bin\\\\nelisp.exe missing" windows-installer))
    (should-not (string-match-p "\\_<cargo\\_>" windows-installer))
    (should-not (string-match-p "\\_<rustc\\_>" windows-installer))))

(ert-deftest nelisp-standalone-tarball-build-script-usage-is-checked-before-build ()
  "The POSIX tarball builder rejects usage errors before invoking make."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
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

(ert-deftest nelisp-standalone-tarball-readmes-document-native-parity ()
  "README files document the short CLI, macOS signing, and full verify gates."
  (let ((root-readme (nelisp-standalone-tarball-test--read "README.org"))
        (stage-readme (nelisp-standalone-tarball-test--read
                       "README-stage-d-v3.0.org")))
    (should (string-match-p "scripts/verify-cross-platform.sh --parallel-jobs 2 --include-tarball"
                            root-readme))
    (should (string-match-p "verification also installs from the freshly built"
                            root-readme))
    (should (string-match-p "executes the installed =bin/nelisp= or =bin/nelisp.exe="
                            root-readme))
    (should (string-match-p "The tarball gate includes a real install smoke"
                            root-readme))
    (should (string-match-p "\\.github/workflows/stage-d-v3.0-standalone.yml"
                            root-readme))
    (should (string-match-p "\\.\\\\scripts\\\\verify-cross-platform\\.ps1 -IncludeTarball"
                            root-readme))
    (should (string-match-p "\\.\\\\install-v3\\.ps1" root-readme))
    (should (string-match-p "\\$env:LOCALAPPDATA\\\\anvil-stage-d-v3.0" root-readme))
    (should (string-match-p "Standalone native CLI matrix" root-readme))
    (should (string-match-p "macos-aarch64.*signed CLI tarball"
                            root-readme))
    (should (string-match-p "windows-x86_64.*PE32[+] self-host"
                            root-readme))
    (should-not (string-match-p "macOS aarch64 build re-validation is[ \n]+pending"
                                root-readme))
    (should (string-match-p "codesign -s -" stage-readme))
    (should (string-match-p "does not[ \n]+repair or re-sign" stage-readme))
    (should (string-match-p "install-v3.sh= also verifies" stage-readme))
    (should (string-match-p "verification also installs from the freshly built"
                            stage-readme))
    (should (string-match-p "runs the installed =bin/nelisp= or =bin/nelisp.exe="
                            stage-readme))
    (should (string-match-p "\\.github/workflows/stage-d-v3.0-standalone.yml"
                            stage-readme))
    (should (string-match-p "\\.\\\\release\\\\stage-d-v3.0\\\\install-v3\\.ps1" stage-readme))
    (should (string-match-p "\\.\\\\scripts\\\\verify-cross-platform\\.ps1 -IncludeTarball"
                            stage-readme))
    (should (string-match-p "macOS arm64 native verification also checks the existing code signature"
                            stage-readme))))

(ert-deftest nelisp-standalone-tarball-v3-workflow-runs-native-parity ()
  "The stage-d-v3.0 workflow gates Linux, macOS, and Windows standalone parity."
  (let ((workflow (nelisp-standalone-tarball-test--read
                   ".github/workflows/stage-d-v3.0-standalone.yml")))
    (should (string-match-p "name: stage-d-v3.0 standalone parity" workflow))
    (should (string-match-p "workflow_dispatch:" workflow))
    (should (string-match-p "pull_request:" workflow))
    (should (string-match-p "linux-x86_64:" workflow))
    (should (string-match-p "runs-on: ubuntu-latest" workflow))
    (should (string-match-p "macos-aarch64:" workflow))
    (should (string-match-p "runs-on: macos-14" workflow))
    (should (string-match-p "windows-x86_64:" workflow))
    (should (string-match-p "runs-on: windows-latest" workflow))
    (should (string-match-p "version: '30.2'" workflow))
    (should (string-match-p "jcs090218/setup-emacs-windows" workflow))
    (should (string-match-p "purcell/setup-emacs" workflow))
    (should (string-match-p
             "./scripts/verify-cross-platform.sh --parallel-jobs 2 --include-tarball"
             workflow))
    (should-not (string-match-p "--skip-native-smokes" workflow))
    (should-not (string-match-p "--build-only-standalone-smokes" workflow))
    (should-not (string-match-p "tools/macos-selfhost-test.sh --emit-only"
                                workflow))
    (should (string-match-p
             "\\.\\\\scripts\\\\verify-cross-platform\\.ps1 -ParallelJobs 2 -IncludeTarball"
             workflow))
    (should (string-match-p "stage-d-v3.0-linux-x86_64" workflow))
    (should (string-match-p "stage-d-v3.0-macos-aarch64" workflow))
    (should (string-match-p "stage-d-v3.0-windows-x86_64" workflow))
    (should-not (string-match-p "\\_<cargo\\_>" workflow))
    (should-not (string-match-p "\\_<rustc\\_>" workflow))
    (should-not (string-match-p "dtolnay/rust-toolchain" workflow))))

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
