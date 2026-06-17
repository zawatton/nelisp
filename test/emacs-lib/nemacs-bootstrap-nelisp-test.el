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

(provide 'nemacs-bootstrap-nelisp-test)

;;; nemacs-bootstrap-nelisp-test.el ends here
