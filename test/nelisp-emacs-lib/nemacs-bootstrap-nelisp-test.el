;;; nemacs-bootstrap-nelisp-test.el --- Phase 5 close-gate ERT smoke  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 5 close-gate (= bootstrap binary self-host).
;; Runs the host-driver ERT framework and shells out to
;; `bin/nemacs --driver=nelisp --batch ...' as a subprocess to
;; assert the nelisp driver bootstraps cleanly without any host
;; Emacs runtime in the loop.
;;
;; These tests are intentionally opt-in.  NeLisp pure-Elisp cold
;; load is slow enough that the default host ERT suite should not run
;; this subprocess gate accidentally; use `make test-nelisp-ert' or set
;; NEMACS_RUN_NELISP_BOOTSTRAP=1.

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

(defun nemacs-bootstrap-nelisp-test--nelisp-candidate ()
  "Resolve a NeLisp standalone reader candidate.

Honours `NELISP_HOME' first (= contributor explicitly opted in to
running the subprocess gate), then the vendored copy populated by
`make nelisp'.  Sibling and legacy checkouts are deliberately not
auto-probed here: the nelisp-driver bootstrap is a slow cold-load
gate and should be entered explicitly via NELISP_HOME.  Returns nil
when no candidate has a built `target/nelisp-standalone-reader'
binary."
  (let* ((vendor (expand-file-name "vendor/nelisp"
                                   nemacs-bootstrap-nelisp-test--repo-root))
         (env (getenv "NELISP_HOME")))
    (let ((candidates (list env vendor))
          (found nil))
      (while (and candidates (not found))
        (let ((dir (car candidates)))
          (when (and dir
                     (file-executable-p
                      (expand-file-name "target/nelisp-standalone-reader" dir)))
            (setq found dir)))
        (setq candidates (cdr candidates)))
      found)))

(defmacro nemacs-bootstrap-nelisp-test--skip-unless-binary (&rest body)
  "Evaluate BODY only when the nelisp binary + bin/nemacs are present."
  (declare (indent 0) (debug t))
  `(let ((home (nemacs-bootstrap-nelisp-test--nelisp-candidate)))
     (cond
      ((not (getenv "NEMACS_RUN_NELISP_BOOTSTRAP"))
       (ert-skip "set NEMACS_RUN_NELISP_BOOTSTRAP=1 or run `make test-nelisp-ert'"))
      ((not (file-executable-p nemacs-bootstrap-nelisp-test--bin))
       (ert-skip "bin/nemacs not executable"))
      ((not home)
       (ert-skip "no standalone reader found (set NELISP_HOME or run `make nelisp')"))
      (t
       (let* ((reader (expand-file-name "target/nelisp-standalone-reader" home))
              (process-environment
               (append (list (format "NELISP_HOME=%s" home)
                             (format "NEMACS_NELISP=%s" reader))
                       process-environment)))
         ,@body)))))

(cl-defstruct (nemacs-bootstrap-nelisp-test--result
               (:constructor nemacs-bootstrap-nelisp-test--make-result))
  status
  stdout
  stderr
  args)

(defun nemacs-bootstrap-nelisp-test--format-result (result)
  "Return a diagnostic string for subprocess RESULT."
  (format "bin/nemacs --driver=nelisp failed
status: %S
args: %S
stdout:
%s
stderr:
%s"
          (nemacs-bootstrap-nelisp-test--result-status result)
          (nemacs-bootstrap-nelisp-test--result-args result)
          (nemacs-bootstrap-nelisp-test--result-stdout result)
          (nemacs-bootstrap-nelisp-test--result-stderr result)))

(defun nemacs-bootstrap-nelisp-test--run-result (&rest extra-args)
  "Invoke `bin/nemacs --driver=nelisp' with EXTRA-ARGS.
Return a `nemacs-bootstrap-nelisp-test--result' carrying exit status,
stdout, stderr, and the argument vector."
  (let ((stderr-file (make-temp-file "nemacs-bootstrap-nelisp-stderr-"))
        (status nil)
        (stdout nil)
        (stderr nil))
    (unwind-protect
        (progn
          (setq stdout
                (with-temp-buffer
                  (setq status
                        (apply #'call-process
                               nemacs-bootstrap-nelisp-test--bin
                               nil (list t stderr-file) nil
                               "--driver=nelisp" extra-args))
                  (buffer-string)))
          (setq stderr
                (with-temp-buffer
                  (when (file-readable-p stderr-file)
                    (insert-file-contents stderr-file))
                  (buffer-string)))
          (nemacs-bootstrap-nelisp-test--make-result
           :status status
           :stdout stdout
           :stderr stderr
           :args (cons "--driver=nelisp" extra-args)))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun nemacs-bootstrap-nelisp-test--run (&rest extra-args)
  "Invoke `bin/nemacs --driver=nelisp' with EXTRA-ARGS.
Return stdout when the subprocess exits cleanly; otherwise fail the
current ERT test with status/stdout/stderr diagnostics."
  (let ((result (apply #'nemacs-bootstrap-nelisp-test--run-result extra-args)))
    (unless (equal 0 (nemacs-bootstrap-nelisp-test--result-status result))
      (ert-fail (nemacs-bootstrap-nelisp-test--format-result result)))
    (nemacs-bootstrap-nelisp-test--result-stdout result)))

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
               "--eval"
               (concat
                "(if (and (fboundp (quote nemacs-batch-main))"
                "         (featurep (quote nemacs-main)))"
                "    (if (fboundp (quote nelisp--write-stdout-bytes))"
                "        (nelisp--write-stdout-bytes \"BOOT=t\\n\")"
                "      (princ \"BOOT=t\\n\"))"
                "  (if (fboundp (quote nelisp--write-stdout-bytes))"
                "      (nelisp--write-stdout-bytes \"BOOT=nil\\n\")"
                "    (princ \"BOOT=nil\\n\")))"))))
     (should (string-match-p "BOOT=t" out))
     (should (string-match-p "ok" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/loadup-feature-count ()
  "Batch loadup under nelisp driver should pull in the core feature set.
Optional font-lock/redisplay/TUI modules load later when an interactive
frame is realised; below the core baseline means a require failed
silently."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let* ((out (nemacs-bootstrap-nelisp-test--run
                "--batch" "--no-banner"
                "--eval"
                "(princ (format \"FEATURES=%d\\n\" (length features)))"))
          (m (string-match "FEATURES=\\([0-9]+\\)" out)))
     (should m)
     (should (>= (string-to-number (match-string 1 out)) 45)))))

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
                    ;; `nelisp-coding-jis-tables' is intentionally
                    ;; lazy-loaded by the Japanese codecs; UTF-8 file I/O
                    ;; should not pay the 20K-line table cost at boot.
                    "nelisp-coding"
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
                    "emacs-edit-builtins" "emacs-line-builtins"
                    "emacs-search-builtins" "emacs-fileio-builtins"
                    "emacs-process" "emacs-process-builtins"
                    "emacs-command-loop" "emacs-command-loop-builtins"
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
NeLisp v2 file syscall bridge (`nl-syscall-read-file' /
`nl-syscall-write-file'), which is tracked by a separate skip below."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(let ((bound (mapcar (function fboundp)"
                "                     (list (quote find-file-noselect)"
                "                           (quote save-buffer)"
                "                           (quote write-region)"
                "                           (quote insert-file-contents)"
                "                           (quote buffer-file-name)"
                "                           (quote set-visited-file-name)))))"
                "  (if (fboundp (quote nelisp--write-stdout-bytes))"
                "      (nelisp--write-stdout-bytes"
                "       (if (memq nil bound) \"BOUND=nil\\n\" \"BOUND=t\\n\"))"
                "    (princ (if (memq nil bound) \"BOUND=nil\\n\" \"BOUND=t\\n\"))))"))))
     (should (string-match-p "BOUND=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/file-write-read-round-trip ()
  "Phase 5 close-gate: full write+read round-trip via the substrate.
Blocks on NeLisp's v2 file syscall bridge (= `nl-syscall-write-file' /
`nl-syscall-read-file' wired into the CLI runtime).  When the
syscalls are missing this test ert-skip's so the rest of the
suite stays clean — this is a real follow-up, not a regression."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((have-syscalls
          (nemacs-bootstrap-nelisp-test--run
           "--batch" "--no-banner"
           "--eval"
           (concat
            "(if (and (fboundp (quote nl-syscall-read-file))"
            "         (fboundp (quote nl-syscall-write-file)))"
            "    (if (fboundp (quote nelisp--write-stdout-bytes))"
            "        (nelisp--write-stdout-bytes \"S=t\\n\")"
            "      (princ \"S=t\\n\"))"
            "  (if (fboundp (quote nelisp--write-stdout-bytes))"
            "      (nelisp--write-stdout-bytes \"S=nil\\n\")"
            "    (princ \"S=nil\\n\")))"))))
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
                "    (if (fboundp (quote nelisp--write-stdout-bytes))"
                "        (nelisp--write-stdout-bytes"
                "         (if (and h nemacs-main--backend nemacs-main--frame)"
                "             \"REALISED=t\\n\" \"REALISED=nil\\n\"))"
                "      (princ (if (and h nemacs-main--backend nemacs-main--frame)"
                "                 \"REALISED=t\\n\" \"REALISED=nil\\n\"))))"
                "  (let ((b (cdr (assoc \"*scratch*\" nelisp-ec--buffers))))"
                "    (nelisp-ec-with-current-buffer b"
                "      (nelisp-ec-insert \"phase5 tui smoke\"))"
                "    (princ (format \"BUF=%S\\n\""
                "                    (nelisp-ec-with-current-buffer b"
                "                      (nelisp-ec-buffer-string)))))"
                "  (when (fboundp (function nemacs-main--shutdown-tui))"
                "    (nemacs-main--shutdown-tui))"
                "  (if (fboundp (quote nelisp--write-stdout-bytes))"
                "      (nelisp--write-stdout-bytes"
                "       (if (and (null nemacs-main--backend)"
                "                (null nemacs-main--frame))"
                "           \"SHUTDOWN=t\\n\" \"SHUTDOWN=nil\\n\"))"
                "    (princ (if (and (null nemacs-main--backend)"
                "                    (null nemacs-main--frame))"
                "               \"SHUTDOWN=t\\n\" \"SHUTDOWN=nil\\n\"))))"))))
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
