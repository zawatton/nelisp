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
when no candidate has a built `target/nelisp' or compatibility
`target/nelisp-standalone-reader' binary."
  (let* ((vendor (expand-file-name "vendor/nelisp"
                                   nemacs-bootstrap-nelisp-test--repo-root))
         (env (getenv "NELISP_HOME")))
    (let ((candidates (list env vendor))
          (found nil))
      (while (and candidates (not found))
        (let ((dir (car candidates)))
          (when (and dir
                     (or (file-executable-p
                          (expand-file-name "target/nelisp" dir))
                         (file-executable-p
                          (expand-file-name "target/nelisp-standalone-reader" dir))))
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
       (let* ((reader (or (and (file-executable-p
                                (expand-file-name "target/nelisp" home))
                               (expand-file-name "target/nelisp" home))
                          (expand-file-name "target/nelisp-standalone-reader" home)))
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
  "`--batch --eval' under nelisp driver should print user output."
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
     (should (string-match-p "BOOT=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/real-init-residual-parity ()
  "The prefix-help, headless cursor, and finite-generator residuals stay fixed."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out
          (nemacs-bootstrap-nelisp-test--run
           "--batch" "--no-banner"
           "--eval"
           (concat
            "(condition-case err"
            "    (progn"
            "      (blink-cursor-mode 0)"
            "      (setq residual-blink-0 blink-cursor-mode)"
            "      (blink-cursor-mode 1)"
            "      (setq residual-blink-1 blink-cursor-mode)"
            "      (blink-cursor-mode -1)"
            "      (setq residual-blink-minus-1 blink-cursor-mode)"
            "      (blink-cursor-mode nil)"
            "      (setq residual-blink-nil blink-cursor-mode)"
            "      (blink-cursor-mode 'toggle)"
            "      (setq residual-blink-toggle blink-cursor-mode)"
            "      (iter-defun residual-iterator (items)"
            "        (let ((rest items))"
            "          (while rest"
            "            (iter-yield (car rest))"
            "            (setq rest (cdr rest)))))"
            "      (let ((iterator (residual-iterator '(alpha beta))))"
            "        (nelisp--write-stdout-bytes"
            "         (format \"RESIDUAL prefix=%S helper=%S blink=%S iter=%S\\n\""
            "                 prefix-help-command"
            "                 (fboundp 'describe-prefix-bindings)"
            "                 (list residual-blink-0 residual-blink-1"
            "                       residual-blink-minus-1 residual-blink-nil"
            "                       residual-blink-toggle)"
            "                 (list (iter-next iterator)"
            "                       (iter-next iterator))))))"
            "  (error"
            "   (nelisp--write-stdout-bytes (format \"RESIDUAL ERROR %S\\n\" err))))"))))
     (should (string-match-p
              (regexp-quote
               "RESIDUAL prefix=describe-prefix-bindings helper=t blink=(nil t nil t nil) iter=(alpha beta)")
              out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/generated-bootstrap-preserves-features ()
  "Direct bootstrap bundle loads must preserve the provided feature set."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let* ((bootstrap
           (expand-file-name "build/nemacs-bootstrap.el"
                             nemacs-bootstrap-nelisp-test--repo-root))
          (out (nemacs-bootstrap-nelisp-test--run
                "--batch" "--no-banner"
                "--load" bootstrap
                "--eval"
                (concat
                 "(nelisp--write-stdout-bytes"
                 " (concat \"FEATURE-NEMACS-MAIN=\""
                 "         (if (featurep 'nemacs-main) \"t\" \"nil\")"
                 "         \"\\n\"))"
                 "(nelisp--write-stdout-bytes"
                 " (concat \"FEATURE-EMACS-SHELL=\""
                 "         (if (featurep 'emacs-shell) \"t\" \"nil\")"
                 "         \"\\n\"))"
                 "(nelisp--write-stdout-bytes"
                 " (concat \"FEATURE-COUNT=\""
                 "         (number-to-string (length features))"
                 "         \"\\n\"))"))))
     (should (string-match-p "FEATURE-NEMACS-MAIN=t" out))
     (should (string-match-p "FEATURE-EMACS-SHELL=t" out))
     (let ((m (string-match "FEATURE-COUNT=\\([0-9]+\\)" out)))
       (should m)
       (should (>= (string-to-number (match-string 1 out)) 45))))))

(ert-deftest nemacs-bootstrap-nelisp-test/generated-bootstrap-includes-org-version-before-tail ()
  "The generated bundle should carry `org-version.el' once and before the tail."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((bootstrap
          (expand-file-name "build/nemacs-bootstrap.el"
                            nemacs-bootstrap-nelisp-test--repo-root)))
     (with-temp-buffer
       (insert-file-contents bootstrap)
       (let ((org-marker "\n;;; >>> vendor/emacs-lisp/org/org-version.el\n")
             (tail-marker "\n;;; >>> src/emacs-load.el\n"))
         (should (= 1 (count-matches (regexp-quote org-marker)
                                     (point-min) (point-max))))
         (should (= 1 (count-matches (regexp-quote tail-marker)
                                     (point-min) (point-max))))
         (goto-char (point-min))
         (let ((org-pos (search-forward org-marker nil t))
               (tail-pos (search-forward tail-marker nil t)))
           (should org-pos)
           (should tail-pos)
           (should (< org-pos tail-pos))))))))

(ert-deftest nemacs-bootstrap-nelisp-test/bootstrap-loads-org-version-api ()
  "Loading the generated bootstrap should expose real Org version helpers."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let* ((bootstrap
           (expand-file-name "build/nemacs-bootstrap.el"
                             nemacs-bootstrap-nelisp-test--repo-root))
          (out (nemacs-bootstrap-nelisp-test--run
                "--batch" "--no-banner"
                "--load" bootstrap
                "--eval"
                (concat
                 "(progn"
                 "  (require 'org-macs)"
                 "  (org-assert-version)"
                 "  (nelisp--write-stdout-bytes"
                 "   (concat \"FEATURE-ORG-VERSION=\""
                 "           (if (featurep 'org-version) \"t\" \"nil\")"
                 "           \"\\nFB-ORG-RELEASE=\""
                 "           (if (fboundp 'org-release) \"t\" \"nil\")"
                 "           \"\\nFB-ORG-GIT-VERSION=\""
                 "           (if (fboundp 'org-git-version) \"t\" \"nil\")"
                 "           \"\\nORG-RELEASE=\" (org-release)"
                 "           \"\\nORG-GIT-VERSION=\" (org-git-version)"
                 "           \"\\n\"))"
                 ")"))))
     (should (string-match-p "FEATURE-ORG-VERSION=t" out))
     (should (string-match-p "FB-ORG-RELEASE=t" out))
     (should (string-match-p "FB-ORG-GIT-VERSION=t" out))
     (should (string-match-p "ORG-RELEASE=9\\.7\\.11" out))
     (should (string-match-p "ORG-GIT-VERSION=release_9\\.7\\.11" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/loadup-feature-count ()
  "Batch loadup under nelisp driver should pull in the core feature set.
Optional font-lock/redisplay/TUI modules load later when an interactive
frame is realised; below the core baseline means a require failed
silently."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let* ((out (nemacs-bootstrap-nelisp-test--run
                "--batch" "--no-banner"
                "--eval"
                (concat
                 "(nelisp--write-stdout-bytes \"FEATURES=\")"
                 "(nelisp--write-stdout-bytes (number-to-string (length features)))"
                 "(nelisp--write-stdout-bytes \"\\n\")")))
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
                 "(dolist (f features)"
                 "  (nelisp--write-stdout-bytes \"FEATURE=\")"
                 "  (nelisp--write-stdout-bytes (symbol-name f))"
                 "  (nelisp--write-stdout-bytes \"\\n\"))")))
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
                    "emacs-syntax-table"
                    "emacs-font-lock" "emacs-font-lock-builtins"
                    "emacs-edit-builtins" "emacs-line-builtins"
                    "emacs-search-builtins" "emacs-fileio-builtins"
                    "emacs-special-buffers"
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
                "  (nelisp--write-stdout-bytes \"BUF=\")"
                "  (nelisp--write-stdout-bytes"
                "   (nelisp-ec-with-current-buffer b"
                "     (nelisp-ec-buffer-string)))"
                "  (nelisp--write-stdout-bytes \"\\n\"))"))))
     (should (string-match-p "BUF=hello, phase5" out)))))

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
                "                           (quote find-file)"
                "                           (quote save-buffer)"
                "                           (quote write-file)"
                "                           (quote write-region)"
                "                           (quote insert-file-contents)"
                "                           (quote buffer-file-name)"
                "                           (quote set-visited-file-name))))"
                "      (commands (mapcar (function commandp)"
                "                        (list (quote find-file)"
                "                              (quote save-buffer)"
                "                              (quote write-file)))))"
                "  (if (fboundp (quote nelisp--write-stdout-bytes))"
                "      (nelisp--write-stdout-bytes"
                "       (if (or (memq nil bound) (memq nil commands))"
                "           \"BOUND=nil\\n\" \"BOUND=t\\n\"))"
                "    (princ (if (or (memq nil bound) (memq nil commands))"
                "               \"BOUND=nil\\n\" \"BOUND=t\\n\"))))"))))
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
               "    (nelisp--write-stdout-bytes \"BUF=\")"
               "    (nelisp--write-stdout-bytes"
               "     (nelisp-ec-with-current-buffer b"
               "       (nelisp-ec-buffer-string)))"
               "    (nelisp--write-stdout-bytes \"\\n\"))"
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
     (should (string-match-p "BUF=phase5 tui smoke" out))
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
               "  (nelisp--write-stdout-bytes \"EVENT-LOOP-RETURNED\\n\")"
                "  (when (fboundp (function nemacs-main--shutdown-tui))"
                "    (nemacs-main--shutdown-tui)))"))))
     (should (string-match-p "EVENT-LOOP-RETURNED" out)))))

;;;; F. dev surfaces (imenu / xref)

(ert-deftest nemacs-bootstrap-nelisp-test/imenu-xref-callable ()
  "imenu symbol index and xref jump-to-definition work on the reader.
Proves the `imenu' / `xref' facades install on the standalone runtime,
the Elisp definition scan finds defs (excluding `define-key'), and the
jump + jump-back stack run end-to-end."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(with-temp-buffer"
                "  (insert \"(defun aaa () 1)\\n(defvar bbb 2)\\n"
                "(defun ccc () 3)\\n(define-key m k c)\\n\")"
                "  (let ((idx (emacs-imenu-create-index)))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"IMENU-COUNT=\" (number-to-string (length idx)) \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"IMENU-NAMES=\" (mapconcat (function car) idx \",\") \"\\n\")))"
                "  (goto-char (point-max))"
                "  (let ((hit (emacs-xref-find-definitions \"ccc\")))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"XREF-FOUND=\" (if hit \"t\" \"nil\") \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"XREF-PAREN=\""
                "             (if (and hit (= (char-after (point)) 40)) \"t\" \"nil\")"
                "             \"\\n\")))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"XREF-POP=\" (if (emacs-xref-pop-marker-stack) \"t\" \"nil\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-IMENU=\" (if (fboundp (quote imenu)) \"t\" \"nil\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-XREF=\""
                "           (if (fboundp (quote xref-find-definitions)) \"t\" \"nil\")"
                "           \"\\n\")))"))))
     ;; facades installed on the standalone runtime
     (should (string-match-p "FB-IMENU=t" out))
     (should (string-match-p "FB-XREF=t" out))
     ;; symbol index finds the three defs and skips `define-key'
     (should (string-match-p "IMENU-COUNT=3" out))
     (should (string-match-p "IMENU-NAMES=aaa,bbb,ccc" out))
     ;; jump-to-definition lands on the opening paren, jump-back returns
     (should (string-match-p "XREF-FOUND=t" out))
     (should (string-match-p "XREF-PAREN=t" out))
     (should (string-match-p "XREF-POP=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/compile-callable ()
  "compile/grep diagnostic capture and next-error work on the reader.
Proves the `compile' facade installs, `call-process' (via /bin/sh)
captures output, the `FILE:LINE[:COL]:' parser runs, and next-error
advances over the parsed diagnostics."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (emacs-compile-run"
                "   \"echo 'a.c:12: oops'; echo 'b.c:5: warn'\")"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CC-COUNT=\""
                "           (number-to-string (length (emacs-compile-errors)))"
                "           \"\\n\"))"
                "  (let ((e (emacs-compile-next-error)))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"CC-FILE=\" (plist-get e :file) \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"CC-LINE=\" (number-to-string (plist-get e :line)) \"\\n\")))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-COMPILE=\" (if (fboundp (quote compile)) \"t\" \"nil\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-NEXT-ERROR=\""
                "           (if (fboundp (quote next-error)) \"t\" \"nil\")"
                "           \"\\n\")))"))))
     ;; facade installed on the standalone runtime
     (should (string-match-p "FB-COMPILE=t" out))
     (should (string-match-p "FB-NEXT-ERROR=t" out))
     ;; two diagnostics captured + parsed; next-error visits the first
     (should (string-match-p "CC-COUNT=2" out))
     (should (string-match-p "CC-FILE=a.c" out))
     (should (string-match-p "CC-LINE=12" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/vc-callable ()
  "Git-only read-only VC status works on the reader.
Proves the `vc' facade installs the read-only family, the git program
resolves to an absolute path (the reader has no PATH lookup), and
`emacs-vc-status' parses porcelain output of a real work-tree."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (executable-find "git"))
   (let ((repo (make-temp-file "nemacs-vc-smoke-" t)))
     (unwind-protect
         (progn
           ;; build a work-tree with one modified tracked file + one untracked
           (let ((default-directory (file-name-as-directory repo)))
             (call-process "git" nil nil nil "init" "-q")
             (call-process "git" nil nil nil "config" "user.email" "t@example.com")
             (call-process "git" nil nil nil "config" "user.name" "t")
             (with-temp-file (expand-file-name "tracked.txt" repo) (insert "v1\n"))
             (call-process "git" nil nil nil "add" "tracked.txt")
             (call-process "git" nil nil nil "commit" "-q" "-m" "init")
             (with-temp-file (expand-file-name "tracked.txt" repo) (insert "v2\n"))
             (with-temp-file (expand-file-name "fresh.txt" repo) (insert "new\n")))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(let ((entries (emacs-vc-status \""
                        (file-name-as-directory repo)
                        "\")))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"VC-COUNT=\" (number-to-string (length entries)) \"\\n\"))"
                        "  (dolist (e entries)"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"VC-ENTRY=\" (car e) \"|\" (cdr e) \"\\n\")))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"FB-VC-DIFF=\""
                        "           (if (fboundp (quote vc-diff)) \"t\" \"nil\")"
                        "           \"\\n\")))"))))
             ;; facade installed the read-only family on the standalone runtime
             (should (string-match-p "FB-VC-DIFF=t" out))
             ;; status saw the modified tracked file and the untracked file.
             ;; `regexp-quote' the markers: the git state codes (" M", "??")
             ;; and the "|" separator contain regexp metacharacters.
             (should (string-match-p "VC-COUNT=2" out))
             (should (string-match-p (regexp-quote "VC-ENTRY= M|tracked.txt") out))
             (should (string-match-p (regexp-quote "VC-ENTRY=??|fresh.txt") out))))
       (delete-directory repo t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/process-file-git-add-callable ()
  "Standalone `process-file' should run `git add' in `default-directory'.
This is the narrow substrate gate underneath Magit's stage workflow:
prove that the reader's sync process wrapper both chdirs into the repo
and mutates the index, without going through any Magit code."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (executable-find "git"))
   (let ((repo (make-temp-file "nemacs-process-file-git-add-" t)))
     (unwind-protect
         (progn
           (let ((default-directory (file-name-as-directory repo)))
             (call-process "git" nil nil nil "init" "-q")
             (call-process "git" nil nil nil "config" "user.email" "t@example.com")
             (call-process "git" nil nil nil "config" "user.name" "t")
             (with-temp-file (expand-file-name "tracked.txt" repo) (insert "v1\n"))
             (call-process "git" nil nil nil "add" "tracked.txt")
             (call-process "git" nil nil nil "commit" "-q" "-m" "init")
             (with-temp-file (expand-file-name "tracked.txt" repo) (insert "v2\n")))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(let ((default-directory \""
                        (file-name-as-directory repo)
                        "\"))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"ADD-RC=\""
                        "           (number-to-string"
                        "            (process-file \"git\" nil nil nil \"add\" \"tracked.txt\"))"
                        "           \"\\n\"))"
                        "  (with-temp-buffer"
                        "    (process-file \"git\" nil t nil \"diff\" \"--name-only\")"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"UNSTAGED=\" (buffer-string) \"\\n\")))"
                        "  (with-temp-buffer"
                        "    (process-file \"git\" nil t nil \"diff\" \"--cached\" \"--name-only\")"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"STAGED=\" (buffer-string) \"\\n\")))"
                        "  (with-temp-buffer"
                        "    (process-file \"git\" nil t nil \"status\" \"--short\")"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"STATUS=\" (buffer-string) \"\\n\"))))"))))
             (should (string-match-p "ADD-RC=0" out))
             (should (string-match-p "UNSTAGED=\n" out))
             (should (string-match-p (regexp-quote "STAGED=tracked.txt\n") out))
             (should (string-match-p (regexp-quote "STATUS=M  tracked.txt") out))))
       (delete-directory repo t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/cl-dolist-return-through-process-path-p ()
  "`cl-return' from `cl-dolist' must retain its anonymous block catch."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(nelisp--write-stdout-bytes"
                " (format \"CL-DOLIST-PROCESS=%S\\n\""
                "  (cl-dolist (program (list \"C:\"))"
                "    (cl-return"
                "     (emacs-process--program-name-absolute-p program)))))"))))
     (should (string-match-p "CL-DOLIST-PROCESS=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/vc-workflow-callable ()
  "Reader-side VC workflow runs through real git state transitions.
Starts from a repo with one modified tracked file, proves the shared VC
facade can render status/diff/log/annotate buffers, then stages the file
through `process-file' and confirms the VC status view reflects the new
index/worktree state."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (executable-find "git"))
   (let ((repo (make-temp-file "nemacs-vc-workflow-" t)))
     (unwind-protect
         (progn
           (let ((default-directory (file-name-as-directory repo)))
             (call-process "git" nil nil nil "init" "-q")
             (call-process "git" nil nil nil "config" "user.email" "t@example.com")
             (call-process "git" nil nil nil "config" "user.name" "t")
             (with-temp-file (expand-file-name "tracked.txt" repo) (insert "v1\n"))
             (call-process "git" nil nil nil "add" "tracked.txt")
             (call-process "git" nil nil nil "commit" "-q" "-m" "init")
             (with-temp-file (expand-file-name "tracked.txt" repo) (insert "v1\nv2\n")))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(let ((default-directory \""
                        (file-name-as-directory repo)
                        "\"))"
                        "  (let ((entries (emacs-vc-status)))"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"VCW-COUNT1=\" (number-to-string (length entries)) \"\\n\"))"
                        "    (dolist (e entries)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"VCW-ENTRY1=\" (car e) \"|\" (cdr e) \"\\n\"))))"
                        "  (let ((buf (vc-dir)))"
                        "    (with-current-buffer buf"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"VCW-DIR=\""
                        "               (if (string-match-p \"tracked.txt\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))))"
                        "  (let ((buf (vc-diff)))"
                        "    (with-current-buffer buf"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"VCW-DIFF=\""
                        "               (if (string-match-p (regexp-quote \"+v2\") (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))))"
                        "  (let ((buf (vc-print-log)))"
                        "    (with-current-buffer buf"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"VCW-LOG=\""
                        "               (if (string-match-p \"init\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))))"
                        "  (let ((buf (emacs-vc-annotate \"tracked.txt\")))"
                        "    (with-current-buffer buf"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"VCW-ANN=\""
                        "               (if (string-match-p \"Not Committed Yet\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"VCW-ADD=\""
                        "           (number-to-string"
                        "            (process-file \"git\" nil nil nil \"add\" \"tracked.txt\"))"
                        "           \"\\n\"))"
                        "  (let ((entries (emacs-vc-status)))"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"VCW-COUNT2=\" (number-to-string (length entries)) \"\\n\"))"
                        "    (dolist (e entries)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"VCW-ENTRY2=\" (car e) \"|\" (cdr e) \"\\n\"))))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"FB-VC-WORKFLOW=\""
                        "           (if (and (fboundp 'vc-dir)"
                        "                    (fboundp 'vc-diff)"
                        "                    (fboundp 'vc-print-log)"
                        "                    (fboundp 'vc-annotate))"
                        "               \"t\" \"nil\")"
                        "           \"\\n\")))"))))
             (should (string-match-p "FB-VC-WORKFLOW=t" out))
             (should (string-match-p "VCW-COUNT1=1" out))
             (should (string-match-p (regexp-quote "VCW-ENTRY1= M|tracked.txt") out))
             (should (string-match-p "VCW-DIR=t" out))
             (should (string-match-p "VCW-DIFF=t" out))
             (should (string-match-p "VCW-LOG=t" out))
             (should (string-match-p "VCW-ANN=t" out))
             (should (string-match-p "VCW-ADD=0" out))
             (should (string-match-p "VCW-COUNT2=1" out))
             (should (string-match-p (regexp-quote "VCW-ENTRY2=M |tracked.txt") out))))
       (delete-directory repo t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/comint-callable ()
  "comint machinery (output mark, input ring, send-input) works on the reader.
Proves the `comint' facade installs and the buffer/ring machinery runs.
A live subprocess round-trip is NOT exercised here: the reader's
`make-process' cannot yet hold an interactive subprocess open (an L1
substrate gap), so this gate covers the process-independent machinery
that the daily-driver REPL/shell buffers build on."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(with-temp-buffer"
                "  (emacs-comint-mode)"
                "  (emacs-comint--set-mark (point-max))"
                "  (emacs-comint-output-filter nil \"out1\\n\")"
                "  (emacs-comint-output-filter nil \"out2\\n\")"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CO-OUTPUT-OK=\""
                "           (if (and (string-match-p \"out1\" (buffer-string))"
                "                    (string-suffix-p \"out2\\n\" (buffer-string)))"
                "               \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (emacs-comint-add-to-input-history \"cmd-a\")"
                "  (emacs-comint-add-to-input-history \"cmd-b\")"
                "  (emacs-comint-add-to-input-history \"   \")"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CO-RING=\""
                "           (mapconcat (function identity) (emacs-comint-input-ring) \",\")"
                "           \"\\n\"))"
                "  (goto-char (point-max))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CO-NAV=\" (emacs-comint-previous-input 1) \"\\n\"))"
                "  (emacs-comint--set-mark (point-max))"
                "  (goto-char (point-max))"
                "  (insert \"typed-input\")"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CO-SEND=\" (emacs-comint-send-input) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-COMINT-SEND=\""
                "           (if (fboundp (quote comint-send-input)) \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-MAKE-COMINT=\""
                "           (if (fboundp (quote make-comint-in-buffer)) \"t\" \"nil\")"
                "           \"\\n\")))"))))
     ;; facade installed on the standalone runtime
     (should (string-match-p "FB-COMINT-SEND=t" out))
     (should (string-match-p "FB-MAKE-COMINT=t" out))
     ;; output accumulates at the mark; blank input is skipped in the ring
     (should (string-match-p "CO-OUTPUT-OK=t" out))
     (should (string-match-p "CO-RING=cmd-b,cmd-a" out))
     ;; previous-input recalls the newest entry; send-input lifts the pending input
     (should (string-match-p "CO-NAV=cmd-b" out))
     (should (string-match-p "CO-SEND=typed-input" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/replace-occur-callable ()
  "occur / replace / line-filter machinery works on the reader.
Proves the `replace' facade installs and the `string-match'-based scan
collects occur matches, navigates to a source position, counts matches,
and rewrites the buffer via flush-lines / replace-regexp."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(with-temp-buffer"
                "  (insert \"alpha 1\\nbeta 2\\nalpha 3\\n\")"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-OCCUR=\" (number-to-string (emacs-occur \"alpha\")) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-GOTO=\" (number-to-string (or (emacs-occur-goto 2) -1)) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-HOWMANY=\" (number-to-string (emacs-replace-how-many \"alpha\")) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-FLUSH=\" (number-to-string (emacs-replace-flush-lines \"beta\")) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-FLUSH-OK=\""
                "           (if (string= (buffer-string) \"alpha 1\\nalpha 3\\n\") \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-REPLACE=\" (number-to-string (emacs-replace-regexp \"alpha\" \"AAA\")) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"RP-REPLACE-OK=\""
                "           (if (string= (buffer-string) \"AAA 1\\nAAA 3\\n\") \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-OCCUR=\" (if (fboundp (quote occur)) \"t\" \"nil\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-REPLACE-REGEXP=\""
                "           (if (fboundp (quote replace-regexp)) \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-HOW-MANY=\""
                "           (if (fboundp (quote how-many)) \"t\" \"nil\")"
                "           \"\\n\")))"))))
     ;; facade installed on the standalone runtime
     (should (string-match-p "FB-OCCUR=t" out))
     (should (string-match-p "FB-REPLACE-REGEXP=t" out))
     (should (string-match-p "FB-HOW-MANY=t" out))
     ;; occur found two lines and goto reached line 3's start (pos 16)
     (should (string-match-p "RP-OCCUR=2" out))
     (should (string-match-p "RP-GOTO=16" out))
     (should (string-match-p "RP-HOWMANY=2" out))
     ;; flush-lines dropped "beta", replace-regexp rewrote both "alpha"
     (should (string-match-p "RP-FLUSH=1" out))
     (should (string-match-p "RP-FLUSH-OK=t" out))
     (should (string-match-p "RP-REPLACE=2" out))
     (should (string-match-p "RP-REPLACE-OK=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/query-replace-callable ()
  "Interactive-engine query-replace works on the reader.
Drives `emacs-query-replace' with an injected decision sequence (no live
keystrokes) to exercise the act / skip / act-all paths, and confirms the
`query-replace' command name installs."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (defvar qr-d nil)"
                "  (defun qr-pop ()"
                "    (let ((d (car qr-d))) (setq qr-d (cdr qr-d)) (if d d (quote skip))))"
                ;; scenario 1: act / skip / act
                "  (setq qr-d (list (quote act) (quote skip) (quote act)))"
                "  (with-temp-buffer"
                "    (insert \"x A x B x C\")"
                "    (goto-char (point-min))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"QR1-COUNT=\""
                "             (number-to-string"
                "              (emacs-query-replace \"x\" \"Z\""
                "                                   (function (lambda (m b e) (qr-pop)))))"
                "             \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"QR1-OK=\""
                "             (if (string= (buffer-string) \"Z A x B Z C\") \"t\" \"nil\")"
                "             \"\\n\")))"
                ;; scenario 2: skip then act-all
                "  (setq qr-d (list (quote skip) (quote act-all)))"
                "  (with-temp-buffer"
                "    (insert \"a a a a\")"
                "    (goto-char (point-min))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"QR2-COUNT=\""
                "             (number-to-string"
                "              (emacs-query-replace \"a\" \"Z\""
                "                                   (function (lambda (m b e) (qr-pop)))))"
                "             \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"QR2-OK=\""
                "             (if (string= (buffer-string) \"a Z Z Z\") \"t\" \"nil\")"
                "             \"\\n\")))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-QUERY-REPLACE=\""
                "           (if (fboundp (quote query-replace)) \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-QR-REGEXP=\""
                "           (if (fboundp (quote query-replace-regexp)) \"t\" \"nil\")"
                "           \"\\n\")))"))))
     ;; command names installed on the standalone runtime
     (should (string-match-p "FB-QUERY-REPLACE=t" out))
     (should (string-match-p "FB-QR-REGEXP=t" out))
     ;; act / skip / act replaced the 1st and 3rd match only
     (should (string-match-p "QR1-COUNT=2" out))
     (should (string-match-p "QR1-OK=t" out))
     ;; skip then act-all replaced the remaining three
     (should (string-match-p "QR2-COUNT=3" out))
     (should (string-match-p "QR2-OK=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/isearch-callable ()
  "Incremental search works on the reader, including the full driver.
The search engine runs over the `nelisp-ec' buffer search, and the
interactive `isearch-forward' is driven end-to-end by injecting the key
events (the query string, C-s to repeat, RET to commit) into the
minibuffer input queue -- the same path the host ERT exercises."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                ;; raw engine over a nelisp-ec buffer
                "  (with-temp-buffer"
                "    (insert \"hello target here\")"
                "    (goto-char (point-min))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"IS-ENGINE=\""
                "             (number-to-string (or (emacs-isearch--search-forward \"target\") -1))"
                "             \"\\n\")))"
                ;; full isearch-forward: query \"foo\" then RET -> first match end (4)
                "  (emacs-isearch-reset)"
                "  (let ((buf (nelisp-ec-generate-new-buffer \" *is1*\")))"
                "    (nelisp-ec-with-current-buffer buf"
                "      (nelisp-ec-insert \"foo bar foo baz\")"
                "      (nelisp-ec-goto-char (nelisp-ec-point-min)))"
                "    (nelisp-ec-set-buffer buf)"
                "    (setq emacs-minibuffer--input-queue (list \"foo\" (quote return)))"
                "    (isearch-forward)"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"IS-FWD=\""
                "             (number-to-string (nelisp-ec-with-current-buffer buf (nelisp-ec-point)))"
                "             \"\\n\")))"
                ;; query \"foo\" then C-s (19) to repeat, then RET -> 2nd match end (12)
                "  (emacs-isearch-reset)"
                "  (let ((buf (nelisp-ec-generate-new-buffer \" *is2*\")))"
                "    (nelisp-ec-with-current-buffer buf"
                "      (nelisp-ec-insert \"foo bar foo baz foo\")"
                "      (nelisp-ec-goto-char (nelisp-ec-point-min)))"
                "    (nelisp-ec-set-buffer buf)"
                "    (setq emacs-minibuffer--input-queue (list \"foo\" 19 (quote return)))"
                "    (isearch-forward)"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"IS-CYCLE=\""
                "             (number-to-string (nelisp-ec-with-current-buffer buf (nelisp-ec-point)))"
                "             \"\\n\")))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-ISEARCH-FORWARD=\""
                "           (if (fboundp (quote isearch-forward)) \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-ISEARCH-BACKWARD=\""
                "           (if (fboundp (quote isearch-backward)) \"t\" \"nil\")"
                "           \"\\n\")))"))))
     ;; command names installed on the standalone runtime
     (should (string-match-p "FB-ISEARCH-FORWARD=t" out))
     (should (string-match-p "FB-ISEARCH-BACKWARD=t" out))
     ;; engine + full driver land on the expected match positions
     (should (string-match-p "IS-ENGINE=13" out))
     (should (string-match-p "IS-FWD=4" out))
     (should (string-match-p "IS-CYCLE=12" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/ielm-callable ()
  "The in-process ielm REPL evaluates and prints results on the reader.
Creates the `*ielm*' buffer, evaluates two forms through
`ielm-input-handler', and confirms the printed results and input ring."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (when (get-buffer ielm-buffer-name)"
                "    (kill-buffer (get-buffer ielm-buffer-name)))"
                "  (let ((buf (ielm)))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"IELM-LIVE=\" (if (buffer-live-p buf) \"t\" \"nil\") \"\\n\"))"
                "    (with-current-buffer buf"
                "      (goto-char (point-max)) (insert \"(+ 1 2)\\n\") (ielm-input-handler)"
                "      (nelisp--write-stdout-bytes"
                "       (concat \"IELM-EVAL=\""
                "               (if (string-suffix-p (concat \"(+ 1 2)\\n3\\n\" ielm-prompt)"
                "                                    (buffer-string)) \"t\" \"nil\")"
                "               \"\\n\"))"
                "      (goto-char (point-max)) (insert \"(* 6 7)\\n\") (ielm-input-handler)"
                "      (nelisp--write-stdout-bytes"
                "       (concat \"IELM-EVAL2=\""
                "               (if (string-suffix-p (concat \"(* 6 7)\\n42\\n\" ielm-prompt)"
                "                                    (buffer-string)) \"t\" \"nil\")"
                "               \"\\n\"))"
                "      (nelisp--write-stdout-bytes"
                "       (concat \"IELM-HIST-N=\""
                "               (number-to-string (length (emacs-ielm--history)))"
                "               \"\\n\"))))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-IELM=\" (if (fboundp (quote ielm)) \"t\" \"nil\") \"\\n\")))"))))
     (should (string-match-p "FB-IELM=t" out))
     (should (string-match-p "IELM-LIVE=t" out))
     (should (string-match-p "IELM-EVAL=t" out))
     (should (string-match-p "IELM-EVAL2=t" out))
     (should (string-match-p "IELM-HIST-N=2" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/project-callable ()
  "Git project root detection and file listing work on the reader.
A temp git work-tree is built host-side; the reader detects its root from
a nested directory and lists the tracked-area files (VC admin files and
the absent `file-relative-name' are handled by the reader fallbacks)."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (executable-find "git"))
   (let ((repo (make-temp-file "nemacs-project-smoke-" t)))
     (unwind-protect
         (progn
           (let ((default-directory (file-name-as-directory repo)))
             (call-process "git" nil nil nil "init" "-q")
             (with-temp-file (expand-file-name "a.el" repo) (insert "(defvar a 1)\n"))
             (make-directory (expand-file-name "lib" repo) t)
             (with-temp-file (expand-file-name "lib/b.el" repo) (insert "(defvar b 2)\n")))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(progn"
                        "  (let ((p (project-current nil \""
                        (file-name-as-directory repo) "lib\")))"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"PROJ-FOUND=\" (if p \"t\" \"nil\") \"\\n\"))"
                        "    (when p"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"PROJ-ROOT=\" (project-root p) \"\\n\"))))"
                        "  (let ((files (project--relative-candidates \""
                        (file-name-as-directory repo) "\" nil)))"
                        "    (nelisp--write-stdout-bytes"
                        "     (concat \"PROJ-FILES=\" (mapconcat (function identity) files \",\") \"\\n\")))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"FB-PROJECT-FIND-FILE=\""
                        "           (if (fboundp (quote project-find-file)) \"t\" \"nil\")"
                        "           \"\\n\")))"))))
             (should (string-match-p "FB-PROJECT-FIND-FILE=t" out))
             ;; root detected from the nested lib/ directory
             (should (string-match-p "PROJ-FOUND=t" out))
             (should (string-match-p (concat "PROJ-ROOT=" (regexp-quote
                                                           (file-name-as-directory repo)))
                                     out))
             ;; tracked-area files listed, VC admin (.git) excluded
             (should (string-match-p (regexp-quote "a.el") out))
             (should (string-match-p (regexp-quote "lib/b.el") out))
             (should-not (string-match-p (regexp-quote "/.git/") out))))
       (delete-directory repo t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/help-callable ()
  "The shared Help buffer workflow runs on the reader.
Exercises `describe-function' and `describe-variable' against the
standalone runtime, then uses `help-go-back' / `help-go-forward' to
prove the navigation history stays live inside one reader process."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (describe-function 'find-file)"
                "  (let ((buf (get-buffer \"*Help*\")))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"HELP-BUF1=\" (if (buffer-live-p buf) \"t\" \"nil\") \"\\n\"))"
                "    (with-current-buffer buf"
                "      (nelisp--write-stdout-bytes"
                "       (concat \"HELP-MODE1=\""
                "               (if (eq major-mode 'help-mode) \"t\" \"nil\")"
                "               \"\\n\"))"
                "      (nelisp--write-stdout-bytes"
                "       (concat \"HELP-FUNC=\""
                "               (if (string-match-p \"find-file is a function\" (buffer-string)) \"t\" \"nil\")"
                "               \"\\n\"))))"
                "  (describe-variable 'load-path)"
                "  (let ((buf (get-buffer \"*Help*\")))"
                "    (with-current-buffer buf"
                "      (nelisp--write-stdout-bytes"
                "       (concat \"HELP-VAR=\""
                "               (if (string-match-p \"load-path is a variable\" (buffer-string)) \"t\" \"nil\")"
                "               \"\\n\"))))"
                "  (help-go-back)"
                "  (with-current-buffer (get-buffer \"*Help*\")"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"HELP-BACK=\""
                "             (if (string-match-p \"find-file is a function\" (buffer-string)) \"t\" \"nil\")"
                "             \"\\n\")))"
                "  (help-go-forward)"
                "  (with-current-buffer (get-buffer \"*Help*\")"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"HELP-FWD=\""
                "             (if (string-match-p \"load-path is a variable\" (buffer-string)) \"t\" \"nil\")"
                "             \"\\n\")))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-HELP=\""
                "           (if (and (fboundp 'describe-function)"
                "                    (fboundp 'describe-variable)"
                "                    (fboundp 'help-go-back)"
                "                    (fboundp 'help-go-forward))"
                "               \"t\" \"nil\")"
                "           \"\\n\")))"))))
     (should (string-match-p "FB-HELP=t" out))
     (should (string-match-p "HELP-BUF1=t" out))
     (should (string-match-p "HELP-MODE1=t" out))
     (should (string-match-p "HELP-FUNC=t" out))
     (should (string-match-p "HELP-VAR=t" out))
     (should (string-match-p "HELP-BACK=t" out))
     (should (string-match-p "HELP-FWD=t" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/dired-workflow-callable ()
  "Standalone Dired workflow runs through the shared directory facade.
Proves the reader can render a listing, enter a subdirectory, return to
the parent, refresh after on-disk mutation, and run mark/copy/delete
operations through the real Dired commands."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((root (make-temp-file "nemacs-dired-workflow-" t)))
     (unwind-protect
         (let* ((subdir (expand-file-name "subdir" root))
                (alpha (expand-file-name "alpha.txt" root))
                (nested (expand-file-name "nested.txt" subdir))
                (gamma (expand-file-name "gamma.txt" root))
                (alpha-copy (expand-file-name "alpha-copy.txt" root)))
           (make-directory subdir)
           (with-temp-file alpha
             (insert "alpha"))
           (with-temp-file nested
             (insert "nested"))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(let ((root " (prin1-to-string (file-name-as-directory root)) ")"
                        "      (gamma " (prin1-to-string gamma) ")"
                        "      (alpha-copy " (prin1-to-string alpha-copy) "))"
                        "  (let ((buf (dired root)))"
                        "    (with-current-buffer buf"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-LIST=\""
                        "               (if (and (string-match-p \"alpha.txt\" (buffer-string))"
                        "                        (string-match-p \"subdir\" (buffer-string)))"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-min))"
                        "      (search-forward \"subdir\\t\")"
                        "      (beginning-of-line)"
                        "      (setq buf (dired-find-file))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-ENTER=\""
                        "               (if (and (string-match-p \"nested.txt\" (buffer-string))"
                        "                        (not (string-match-p \"alpha.txt\" (buffer-string))))"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (setq buf (dired-up-directory))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-UP=\""
                        "               (if (string-match-p \"alpha.txt\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (with-temp-file gamma (insert \"gamma!\"))"
                        "      (emacs-dired-min-revert-buffer)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-REFRESH=\""
                        "               (if (string-match-p \"gamma.txt\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-min))"
                        "      (search-forward \"alpha.txt\\t\")"
                        "      (beginning-of-line)"
                        "      (dired-mark)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-MARK=\""
                        "               (if (eq (cdr (assoc \"alpha.txt\""
                        "                                  (plist-get (emacs-dired-min--current-state) :marks)))"
                        "                       ?*)"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-min))"
                        "      (search-forward \"alpha.txt\\t\")"
                        "      (beginning-of-line)"
                        "      (dired-unmark)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-UNMARK=\""
                        "               (if (not (assoc \"alpha.txt\""
                        "                              (plist-get (emacs-dired-min--current-state) :marks)))"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (cl-letf (((symbol-function (quote read-file-name))"
                        "                 (lambda (&rest _) alpha-copy)))"
                        "        (dired-do-copy))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-COPY=\""
                        "               (if (and (file-exists-p alpha-copy)"
                        "                        (string-match-p \"alpha-copy.txt\" (buffer-string)))"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-min))"
                        "      (search-forward \"gamma.txt\\t\")"
                        "      (beginning-of-line)"
                        "      (dired-flag-file-deletion)"
                        "      (let ((deleted (dired-do-flagged-delete)))"
                        "        (nelisp--write-stdout-bytes"
                        "         (concat \"DW-DELETE-COUNT=\" (number-to-string deleted) \"\\n\")))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"DW-DELETE=\""
                        "               (if (and (not (file-exists-p gamma))"
                        "                        (not (string-match-p \"gamma.txt\" (buffer-string))))"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"FB-DIRED-WORKFLOW=\""
                        "               (if (and (fboundp (quote dired))"
                        "                        (fboundp (quote dired-find-file))"
                        "                        (fboundp (quote dired-up-directory))"
                        "                        (fboundp (quote dired-mark))"
                        "                        (fboundp (quote dired-unmark))"
                        "                        (fboundp (quote dired-do-copy))"
                        "                        (fboundp (quote dired-flag-file-deletion))"
                        "                        (fboundp (quote dired-do-flagged-delete)))"
                        "                   \"t\" \"nil\")"
                        "               \"\\n\")))))"))))
             (should (string-match-p "FB-DIRED-WORKFLOW=t" out))
             (should (string-match-p "DW-LIST=t" out))
             (should (string-match-p "DW-ENTER=t" out))
             (should (string-match-p "DW-UP=t" out))
             (should (string-match-p "DW-REFRESH=t" out))
             (should (string-match-p "DW-MARK=t" out))
             (should (string-match-p "DW-UNMARK=t" out))
             (should (string-match-p "DW-COPY=t" out))
             (should (string-match-p "DW-DELETE-COUNT=1" out))
             (should (string-match-p "DW-DELETE=t" out))))
       (delete-directory root t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/minibuffer-callable ()
  "Minibuffer completion/history/cancel workflow runs on the reader.
Combines prompt/initial-input visibility, history push semantics,
require-match completion, abort unwind, and the `*Completions*' UI
selection path inside one standalone reader process."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (require 'emacs-minibuffer-builtins)"
                "  (require 'emacs-completion-ui)"
                "  (setq nemacs-bootstrap-minibuffer-history nil)"
                "  (setq emacs-bootstrap-minibuffer-seen-prompt nil)"
                "  (setq emacs-bootstrap-minibuffer-seen-contents nil)"
                "  (setq emacs-minibuffer--read-fn"
                "        (lambda (_p _i _d _h _k _r)"
                "          (setq emacs-bootstrap-minibuffer-seen-prompt"
                "                (emacs-minibuffer-minibuffer-prompt))"
                "          (setq emacs-bootstrap-minibuffer-seen-contents"
                "                (emacs-minibuffer-minibuffer-contents))"
                "          \"typed\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"MB-READ=\" (read-from-minibuffer \"Prompt: \" \"seed\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"MB-PROMPT=\""
                "           (if (equal emacs-bootstrap-minibuffer-seen-prompt \"Prompt: \") \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"MB-CONTENTS=\""
                "           (if (equal emacs-bootstrap-minibuffer-seen-contents \"seed\") \"t\" \"nil\")"
                "           \"\\n\"))"
                "  (setq emacs-minibuffer--read-fn nil)"
                "  (emacs-minibuffer-feed-input \"alpha\" \"beta\" \"\")"
                "  (read-from-minibuffer \"P: \" nil nil nil 'nemacs-bootstrap-minibuffer-history)"
                "  (read-from-minibuffer \"P: \" nil nil nil 'nemacs-bootstrap-minibuffer-history)"
                "  (read-from-minibuffer \"P: \" nil nil nil 'nemacs-bootstrap-minibuffer-history)"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"MB-HIST=\""
                "           (mapconcat 'identity nemacs-bootstrap-minibuffer-history \",\")"
                "           \"\\n\"))"
                "  (emacs-minibuffer-feed-input :abort)"
                "  (let ((caught nil))"
                "    (condition-case _err"
                "        (read-from-minibuffer \"P: \")"
                "      (quit (setq caught t)))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MB-ABORT=\" (if caught \"t\" \"nil\") \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MB-DEPTH=\" (number-to-string emacs-minibuffer--depth) \"\\n\")))"
                "  (emacs-minibuffer-feed-input \"apple\")"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"MB-CR=\""
                "           (completing-read \"Pick: \" '(\"apple\" \"banana\") nil t)"
                "           \"\\n\"))"
                "  (emacs-minibuffer-feed-input \"durian\")"
                "  (let ((caught nil))"
                "    (condition-case _err"
                "        (completing-read \"Pick: \" '(\"apple\" \"banana\") nil t)"
                "      (emacs-minibuffer-error (setq caught t)))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MB-REQ=\" (if caught \"t\" \"nil\") \"\\n\")))"
                "  (let ((before (and (fboundp 'emacs-minibuffer-exit-minibuffer)"
                "                     (symbol-function 'emacs-minibuffer-exit-minibuffer)))"
                "        (done nil)"
                "        (shown nil)"
                "        (selected nil))"
                "    (fset 'emacs-minibuffer-exit-minibuffer"
                "          (lambda () (setq done t) nil))"
                "    (emacs-minibuffer--with-frame"
                "     \"Pick: \" \"fo\""
                "     (lambda ()"
                "       (setq minibuffer-completion-table '(\"foobar\" \"food\"))"
                "       (minibuffer-complete)"
                "       (setq shown (not (null (plist-get emacs-completion-ui--completion-state :buffer))))"
                "       (switch-to-completions)"
                "       (next-completion)"
                "       (setq selected (choose-completion))))"
                "    (if before"
                "        (fset 'emacs-minibuffer-exit-minibuffer before)"
                "      nil)"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MB-COMP-BUF=\" (if shown \"t\" \"nil\") \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MB-COMP-EXIT=\" (if done \"t\" \"nil\") \"\\n\"))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MB-COMP-CHOICE=\" selected \"\\n\")))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-MB=\""
                "           (if (and (fboundp 'read-from-minibuffer)"
                "                    (fboundp 'completing-read)"
                "                    (fboundp 'yes-or-no-p)"
                "                    (fboundp 'minibuffer-complete)"
                "                    (fboundp 'minibuffer-complete-and-exit))"
                "               \"t\" \"nil\")"
                "           \"\\n\")))"))))
     (should (string-match-p "FB-MB=t" out))
     (should (string-match-p "MB-READ=typed" out))
     (should (string-match-p "MB-PROMPT=t" out))
     (should (string-match-p "MB-CONTENTS=t" out))
     (should (string-match-p "MB-HIST=beta,alpha" out))
     (should (string-match-p "MB-ABORT=t" out))
     (should (string-match-p "MB-DEPTH=0" out))
     (should (string-match-p "MB-CR=apple" out))
     (should (string-match-p "MB-REQ=t" out))
     (should (string-match-p "MB-COMP-BUF=t" out))
     (should (string-match-p "MB-COMP-EXIT=t" out))
     (should (string-match-p "MB-COMP-CHOICE=food" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/shell-callable ()
  "The comint-based shell runs commands on the reader.
Each input line runs via `call-process' (a persistent subprocess is an L1
gap), so this drives `echo', a `cd' into a host-made temp dir, and `ls'
to confirm the working-directory tracking + output capture."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (file-executable-p "/bin/sh"))
   (let ((dir (make-temp-file "nemacs-shell-smoke-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "marker-xyz.txt" dir) (insert "m\n"))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(progn"
                        "  (when (get-buffer emacs-shell-buffer-name)"
                        "    (kill-buffer emacs-shell-buffer-name))"
                        "  (let ((buf (emacs-shell)))"
                        "    (with-current-buffer buf"
                        "      (goto-char (point-max)) (insert \"echo shell-on-comint\")"
                        "      (emacs-shell-send-input)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"SH-ECHO=\""
                        "               (if (string-match-p \"shell-on-comint\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-max)) (insert \"cd " dir "\")"
                        "      (emacs-shell-send-input)"
                        "      (goto-char (point-max)) (insert \"ls\")"
                        "      (emacs-shell-send-input)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"SH-CD-LS=\""
                        "               (if (string-match-p \"marker-xyz\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"SH-RING-N=\""
                        "               (number-to-string (length (emacs-comint-input-ring)))"
                        "               \"\\n\"))))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"FB-SHELL=\" (if (fboundp (quote shell)) \"t\" \"nil\") \"\\n\")))"))))
             (should (string-match-p "FB-SHELL=t" out))
             ;; echo ran and its output landed in the buffer
             (should (string-match-p "SH-ECHO=t" out))
             ;; cd into the temp dir then ls shows the host-made marker file
             (should (string-match-p "SH-CD-LS=t" out))
             ;; three commands recorded in the comint input ring
             (should (string-match-p "SH-RING-N=3" out))))
       (delete-directory dir t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/eshell-callable ()
  "eshell's hybrid dispatch works on the reader.
Drives a Lisp form (evaluated in-process), the `echo' built-in, and an
external `ls' (via call-process) after `cd' into a host-made temp dir."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (file-executable-p "/bin/sh"))
   (let ((dir (make-temp-file "nemacs-eshell-smoke-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "marker-esh.txt" dir) (insert "m\n"))
           (let ((out (nemacs-bootstrap-nelisp-test--run
                       "--batch" "--no-banner"
                       "--eval"
                       (concat
                        "(progn"
                        "  (when (get-buffer eshell-buffer-name)"
                        "    (kill-buffer eshell-buffer-name))"
                        "  (let ((buf (eshell)))"
                        "    (with-current-buffer buf"
                        "      (goto-char (point-max)) (insert \"(+ 2 3)\") (eshell-send-input)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"ESH-LISP=\""
                        "               (if (string-match-p \"\\n5\\n\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-max)) (insert \"echo eshell-builtin\")"
                        "      (eshell-send-input)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"ESH-ECHO=\""
                        "               (if (string-match-p \"eshell-builtin\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (goto-char (point-max)) (insert \"cd " dir "\") (eshell-send-input)"
                        "      (goto-char (point-max)) (insert \"ls\") (eshell-send-input)"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"ESH-EXT-LS=\""
                        "               (if (string-match-p \"marker-esh\" (buffer-string)) \"t\" \"nil\")"
                        "               \"\\n\"))"
                        "      (nelisp--write-stdout-bytes"
                        "       (concat \"ESH-RING-N=\""
                        "               (number-to-string (length (emacs-comint-input-ring)))"
                        "               \"\\n\"))))"
                        "  (nelisp--write-stdout-bytes"
                        "   (concat \"FB-ESHELL=\" (if (fboundp (quote eshell)) \"t\" \"nil\") \"\\n\")))"))))
             (should (string-match-p "FB-ESHELL=t" out))
             ;; Lisp form evaluated in-process
             (should (string-match-p "ESH-LISP=t" out))
             ;; echo built-in (pure Elisp)
             (should (string-match-p "ESH-ECHO=t" out))
             ;; external ls (call-process) after cd shows the host marker
             (should (string-match-p "ESH-EXT-LS=t" out))
             ;; four commands recorded in the comint input ring
             (should (string-match-p "ESH-RING-N=4" out))))
       (delete-directory dir t)))))

(ert-deftest nemacs-bootstrap-nelisp-test/man-callable ()
  "The man viewer fetches and displays a page on the reader.
Runs `man true' through call-process + col -b and confirms the page text,
the not-found error path, and the `man' / `woman' command names."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (skip-unless (executable-find "man"))
   (skip-unless (executable-find "col"))
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (let ((buf (ignore-errors (man \"true\"))))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"MAN-OK=\""
                "             (if (and buf (buffer-live-p buf)) \"t\" \"nil\") \"\\n\"))"
                "    (when (and buf (buffer-live-p buf))"
                "      (with-current-buffer buf"
                "        (nelisp--write-stdout-bytes"
                "         (concat \"MAN-NAME=\""
                "                 (if (string-match-p \"NAME\" (buffer-string)) \"t\" \"nil\")"
                "                 \"\\n\"))"
                "        (nelisp--write-stdout-bytes"
                "         (concat \"MAN-TRUE=\""
                "                 (if (string-match-p \"true\" (buffer-string)) \"t\" \"nil\")"
                "                 \"\\n\")))))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"MAN-NOTFOUND=\""
                "           (condition-case nil"
                "               (progn (man \"no-such-manpage-xyzzy-99\") \"no-error\")"
                "             (error \"errored\"))"
                "           \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-MAN=\" (if (fboundp (quote man)) \"t\" \"nil\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-WOMAN=\" (if (fboundp (quote woman)) \"t\" \"nil\") \"\\n\")))"))))
     ;; command names installed
     (should (string-match-p "FB-MAN=t" out))
     (should (string-match-p "FB-WOMAN=t" out))
     ;; page fetched and rendered
     (should (string-match-p "MAN-OK=t" out))
     (should (string-match-p "MAN-NAME=t" out))
     (should (string-match-p "MAN-TRUE=t" out))
     ;; missing page signals an error
     (should (string-match-p "MAN-NOTFOUND=errored" out)))))

(ert-deftest nemacs-bootstrap-nelisp-test/calc-callable ()
  "The RPN calculator evaluates and renders on the reader.
Exercises `emacs-calc-eval' (pure RPN), the buffer stack operators, and
the `calc' / `calc-eval' command names."
  (nemacs-bootstrap-nelisp-test--skip-unless-binary
   (let ((out (nemacs-bootstrap-nelisp-test--run
               "--batch" "--no-banner"
               "--eval"
               (concat
                "(progn"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CALC-EVAL1=\" (number-to-string (emacs-calc-eval \"2 3 +\")) \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CALC-EVAL2=\" (number-to-string (emacs-calc-eval \"2 3 + 4 *\")) \"\\n\"))"
                "  (emacs-calc-reset)"
                "  (emacs-calc-push 6) (emacs-calc-push 7) (emacs-calc-times)"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"CALC-TOP=\" (number-to-string (emacs-calc-top)) \"\\n\"))"
                "  (let ((buf (emacs-calc)))"
                "    (nelisp--write-stdout-bytes"
                "     (concat \"CALC-BUF=\" (if (and buf (buffer-live-p buf)) \"t\" \"nil\") \"\\n\"))"
                "    (when (and buf (buffer-live-p buf))"
                "      (with-current-buffer buf"
                "        (nelisp--write-stdout-bytes"
                "         (concat \"CALC-RENDER=\""
                "                 (if (string-match-p \"1:  42\" (buffer-string)) \"t\" \"nil\")"
                "                 \"\\n\")))))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-CALC=\" (if (fboundp (quote calc)) \"t\" \"nil\") \"\\n\"))"
                "  (nelisp--write-stdout-bytes"
                "   (concat \"FB-CALC-EVAL=\" (if (fboundp (quote calc-eval)) \"t\" \"nil\") \"\\n\")))"))))
     (should (string-match-p "FB-CALC=t" out))
     (should (string-match-p "FB-CALC-EVAL=t" out))
     ;; pure RPN evaluation
     (should (string-match-p "CALC-EVAL1=5" out))
     (should (string-match-p "CALC-EVAL2=20" out))
     ;; buffer stack: 6 7 * -> 42, rendered as the top (`1:')
     (should (string-match-p "CALC-TOP=42" out))
     (should (string-match-p "CALC-BUF=t" out))
     (should (string-match-p "CALC-RENDER=t" out)))))

(provide 'nemacs-bootstrap-nelisp-test)

;;; nemacs-bootstrap-nelisp-test.el ends here
