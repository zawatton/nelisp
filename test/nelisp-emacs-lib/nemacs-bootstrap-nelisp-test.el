;;; nemacs-bootstrap-nelisp-test.el --- Phase 5 close-gate ERT smoke  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 5 close-gate (= bootstrap binary self-host).
;; Runs the host-driver ERT framework and shells out to
;; `bin/nemacs --driver=nelisp --batch ...' as a subprocess to
;; assert the nelisp driver bootstraps cleanly without any host
;; Emacs runtime in the loop.
;;
;; All tests skip gracefully when the nelisp binary is not present
;; (= contributors who haven't run `make nelisp' yet).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nemacs-bootstrap-nelisp-test--repo-root
  (expand-file-name
   "../"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the nelisp-emacs repo root.")

(defconst nemacs-bootstrap-nelisp-test--bin
  (expand-file-name "bin/nemacs" nemacs-bootstrap-nelisp-test--repo-root)
  "Path to bin/nemacs from the test file.")

(defun nemacs-bootstrap-nelisp-test--nelisp-home ()
  "Resolve the directory holding the nelisp Rust runtime.

Honours `NELISP_HOME' first (= contributor explicitly opted in to
running the subprocess gate), then the vendored copy populated by
`make nelisp'.  The legacy =~/Notes/dev/nelisp/= fallback used to
be probed too, but it was matching cross-repo binaries whose
bootstrap was incompatible with this branch's loadup, hanging
=make test= for several minutes per CI run; ship-gate-grade soak
should be opt-in via NELISP_HOME, not driven by an
implicit-path heuristic.  Returns nil when no candidate has a
built `target/release/nelisp' binary."
  (let* ((vendor (expand-file-name "vendor/nelisp"
                                   nemacs-bootstrap-nelisp-test--repo-root))
         (env (getenv "NELISP_HOME")))
    (cl-find-if
     (lambda (d)
       (and d (file-executable-p (expand-file-name "target/release/nelisp" d))))
     (list env vendor))))

(defmacro nemacs-bootstrap-nelisp-test--skip-unless-binary (&rest body)
  "Evaluate BODY only when the nelisp binary + bin/nemacs are present."
  (declare (indent 0) (debug t))
  `(let ((home (nemacs-bootstrap-nelisp-test--nelisp-home)))
     (cond
      ((not (file-executable-p nemacs-bootstrap-nelisp-test--bin))
       (ert-skip "bin/nemacs not executable"))
      ((not home)
       (ert-skip "no nelisp binary found (run `make nelisp')"))
      (t
       (let ((process-environment
              (cons (format "NELISP_HOME=%s" home) process-environment)))
         ,@body)))))

(defun nemacs-bootstrap-nelisp-test--run (&rest extra-args)
  "Invoke `bin/nemacs --driver=nelisp' with EXTRA-ARGS, return stdout string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process
             nemacs-bootstrap-nelisp-test--bin nil t nil
             "--driver=nelisp" extra-args))))

;;;; A. surface

(ert-deftest nemacs-bootstrap-nelisp-test/version-reports-driver ()
  "`--version' should announce the nelisp driver."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run "--version")))
     (should (string-match-p "nemacs 0\\.1\\.0" out))
     (should (string-match-p "driver=nelisp" out)))))

;;;; B. boot

(ert-deftest nemacs-bootstrap-nelisp-test/batch-completes-cleanly ()
  "`--batch --eval' under nelisp driver should print user output and `ok'."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval" "(princ (format \"BOOT=%S\\n\" t))")))
     (should (string-match-p "BOOT=t" out))
     (should (string-match-p "ok" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/loadup-feature-count ()
  "Loadup under nelisp driver should pull in at least 60 features
(= the substrate baseline; below that means the dependency chain
broke and a require failed silently)."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let* ((out (nemacs-bootstrap-nelisp-test--run
                "--batch" "--no-banner"
                "--eval"
                "(princ (format \"FEATURES=%d\\n\" (length features)))"))
          (m (string-match "FEATURES=\\([0-9]+\\)" out)))
     (should m)
     (should (>= (string-to-number (match-string 1 out)) 60)))))

(ert-deftest nemacs-bootstrap-nelisp-test/core-features-present ()
  "Every nemacs-defined module that `nemacs-loadup' transitively requires
must be in `features' after loadup under the nelisp driver.  This is a
regression gate: if any module's `(provide ...)' fires under host but
breaks under nelisp (= conditional require on a host-only symbol), the
list below catches it."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let* ((out (nemacs-bootstrap-nelisp-test--run
                "--batch" "--no-banner"
                "--eval"
                (concat
                 "(dolist (f features) (princ (format \"FEATURE=%s\\n\" f)))")))
          (loaded (let (acc)
                    (dolist (line (split-string out "\n" t))
                      (when (string-match "^FEATURE=\\(.+\\)$" line)
                        (push (match-string 1 line) acc)))
                    acc)))
     (dolist (sym '(;; bootstrap entry points
                    "nemacs-loadup" "nemacs-main"
                    "emacs-init" "emacs-dump"
                    ;; Layer-1 substrate
                    "nelisp-emacs-compat" "nelisp-emacs-compat-fileio"
                    "nelisp-text-buffer" "nelisp-regex"
                    "nelisp-coding" "nelisp-coding-jis-tables"
                    ;; Layer-2 elisp builtin shims
                    "emacs-fns" "emacs-eval" "emacs-list"
                    "emacs-hash" "emacs-symbol" "emacs-vars"
                    "emacs-string" "emacs-error" "emacs-backquote"
                    "emacs-numeric" "emacs-time" "emacs-callproc"
                    "emacs-pcase" "emacs-cl-macros" "emacs-stub"
                    "emacs-sqlite"
                    ;; user-facing APIs (Layer-2 / Layer-3 dispatch)
                    "emacs-buffer" "emacs-buffer-builtins"
                    "emacs-window" "emacs-window-builtins"
                    "emacs-frame" "emacs-frame-builtins"
                    "emacs-keymap" "emacs-keymap-builtins"
                    "emacs-minibuffer" "emacs-minibuffer-builtins"
                    "emacs-undo" "emacs-undo-builtins"
                    "emacs-mode" "emacs-mode-builtins"
                    "emacs-faces" "emacs-faces-builtins"
                    "emacs-font-lock" "emacs-font-lock-builtins"
                    "emacs-syntax-table"
                    "emacs-edit-builtins" "emacs-line-builtins"
                    "emacs-search-builtins" "emacs-fileio-builtins"
                    "emacs-process" "emacs-process-builtins"
                    "emacs-command-loop" "emacs-command-loop-builtins"
                    "emacs-elisp-mode"
                    ;; Layer-3 TUI backend (= bootstrap close-gate path)
                    "emacs-redisplay" "emacs-redisplay-builtins"
                    "emacs-tui-backend" "emacs-tui-event"
                    "emacs-standalone"))
       (should (member sym loaded))))))

;;;; C. eval

(ert-deftest nemacs-bootstrap-nelisp-test/edit-cycle-buffer-string ()
  "A buffer + insert + buffer-string round-trip should work end-to-end
under the nelisp driver — proves the core Layer 2 substrate works
without a host Emacs."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(let ((b (nelisp-ec-generate-new-buffer \"smoke\")))"
                "  (nelisp-ec-with-current-buffer b"
                "    (nelisp-ec-insert \"hello, phase5\"))"
                "  (princ (format \"BUF=%S\\n\""
                "                  (nelisp-ec-with-current-buffer b"
                "                    (nelisp-ec-buffer-string)))))"))))
     (should (string-match-p "BUF=\"hello, phase5\"" out)))))

;;;; D. file I/O

(ert-deftest nemacs-bootstrap-nelisp-test/fileio-bridges-bound ()
  "The unprefixed fileio commands must resolve to substrate primitives
under the nelisp driver.  This is the *static* half of the file-I/O
gate — the round-trip half (= read+write actual bytes) requires the
NeLisp Doc 33 §3.1 `nl-syscall-read-file' / `nl-syscall-write-file'
externs, which are tracked by a separate skip below."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(princ (format \"BOUND=%S\\n\""
                "                (mapcar (function fboundp)"
                "                        (list (quote find-file-noselect)"
                "                              (quote save-buffer)"
                "                              (quote write-region)"
                "                              (quote insert-file-contents)"
                "                              (quote buffer-file-name)"
                "                              (quote set-visited-file-name)))))"))))
     (should (string-match-p "BOUND=(t t t t t t)" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/file-write-read-round-trip ()
  "Phase 5 close-gate: full write+read round-trip via the substrate.
Blocks on NeLisp Doc 33 §3.1 (= `nl-syscall-write-file' /
`nl-syscall-read-file' wired into the Rust runtime).  When the
syscalls are missing this test ert-skip's so the rest of the
suite stays clean — this is a real follow-up, not a regression."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((have-syscalls
          (nemacs-bootstrap-nelisp-test--run
           "--batch" "--no-banner"
           "--eval"
           (concat
            "(princ (format \"S=%S\\n\""
            "                (and (fboundp (quote nl-syscall-read-file))"
            "                     (fboundp (quote nl-syscall-write-file)))))"))))
     (unless (string-match-p "S=t" have-syscalls)
       (ert-skip "NeLisp Doc 33 §3.1 nl-syscall-read-file / nl-syscall-write-file not wired"))
     (let* ((tmp (make-temp-file "nemacs-bootstrap-nelisp-"))
            (form
             (format
              (concat
               "(let* ((f %S) (b (find-file-noselect f)))"
               "  (nelisp-ec-with-current-buffer b"
               "    (nelisp-ec-insert \"phase5 round trip\"))"
               "  (nelisp-ec-set-buffer b)"
               "  (save-buffer)"
               "  (princ (format \"WROTE=%%S\\n\" (file-exists-p f))))")
              tmp)))
       (unwind-protect
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval" form)))
             (should (string-match-p "WROTE=t" out))
             (should (file-exists-p tmp))
             (with-temp-buffer
               (insert-file-contents tmp)
               (should (string= "phase5 round trip" (buffer-string)))))
         (when (file-exists-p tmp) (delete-file tmp)))))))

;;;; E. interactive TUI smoke (Phase 5 close-gate, sans save)

(ert-deftest nemacs-bootstrap-nelisp-test/tui-realise-edit-shutdown ()
  "Phase 5 close-gate: under the nelisp driver, the runner can
realise the TUI backend, expose scratch through Layer 2, accept an
insertion, surface the resulting buffer-string back to the caller,
and shut the backend down cleanly.  This is the interactive smoke
half of Phase 5 modulo file save (= which lives in
`file-write-read-round-trip' and is gated on Doc 33 §3.1)."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (let ((h (nemacs-main--realise-tui)))"
                "    (princ (format \"REALISED=%S\\n\""
                "                    (and h nemacs-main--backend"
                "                         nemacs-main--frame t))))"
                "  (let ((b (cdr (assoc \"*scratch*\" nelisp-ec--buffers))))"
                "    (nelisp-ec-with-current-buffer b"
                "      (nelisp-ec-insert \"phase5 tui smoke\"))"
                "    (princ (format \"BUF=%S\\n\""
                "                    (nelisp-ec-with-current-buffer b"
                "                      (nelisp-ec-buffer-string)))))"
                "  (when (fboundp (function nemacs-main--shutdown-tui))"
                "    (nemacs-main--shutdown-tui))"
                "  (princ (format \"SHUTDOWN=%S\\n\""
                "                  (and (null nemacs-main--backend)"
                "                       (null nemacs-main--frame)))))"))))
     (should (string-match-p "REALISED=t" out))
     (should (string-match-p "BUF=\"phase5 tui smoke\"" out))
     (should (string-match-p "SHUTDOWN=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/quit-flag-stops-event-loop ()
  "Phase 5 close-gate: pre-setting the quit flag should let the event
loop exit immediately under the nelisp driver — the close-gate
shape needs interactive boot + interactive teardown."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (nemacs-main--realise-tui)"
                "  (setq nemacs-main--quit-flag t)"
                "  (when (fboundp (function nemacs-main--event-loop))"
                "    (nemacs-main--event-loop))"
                "  (princ \"EVENT-LOOP-RETURNED\\n\")"
                "  (when (fboundp (function nemacs-main--shutdown-tui))"
                "    (nemacs-main--shutdown-tui)))"))))
     (should (string-match-p "EVENT-LOOP-RETURNED" out)))))

(provide 'nemacs-bootstrap-nelisp-test)

;;; nemacs-bootstrap-nelisp-test.el ends here
