;;; -*- lexical-binding: t; -*-
;;; nelisp-dev-replay-test.el --- explicit host replay process tests
(require 'ert)
(require 'cl-lib)
(require 'json)
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../lisp" here)))
(require 'nelisp-dev-session)
(require 'nelisp-dev-replay)

(defun nelisp-dev-replay-test--request (manifest &optional policy timeout)
  (list (cons "operation" "session.replay") (cons "request_id" "replay-test")
        (cons "target" "host-emacs")
        (cons "arguments"
              (append (list (cons "manifest" manifest))
                      (and policy (list (cons "effects-policy" policy)))
                      (and timeout (list (cons "timeout" timeout)))))))
(defun nelisp-dev-replay-test--status (result) (cdr (assoc "status" result)))

(defmacro nelisp-dev-replay-test--session (source-text &rest body)
  `(let* ((directory (make-temp-file "nelisp-dev-replay-test-" t))
          (source (expand-file-name "source-utf8.el" directory))
          (recipe (expand-file-name "recipe.el" directory))
          (manifest (expand-file-name "recipe.manifest.json" directory))
          (marker (expand-file-name "child-marker" directory)))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'utf-8-unix))
             (with-temp-file source
               (insert (replace-regexp-in-string "%MARKER%" marker ,source-text t t))))
           (let ((nelisp-repl-session--records nil)
                 (nelisp-repl-session--next-id 1))
             (nelisp-repl-session-record-load source)
             (nelisp-dev-session-dispatch "session.export"
              (list (cons "session-id" "replay-test") (cons "recipe" recipe)
                    (cons "manifest" manifest))))
           ,@body)
       (delete-directory directory t))))

(ert-deftest nelisp-dev-replay/explicit-record-load-runs-in-fresh-process ()
  (nelisp-dev-replay-test--session
   "(with-temp-file \"%MARKER%\" (insert \"child\")) (princ \"日本語\")\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "ok" (nelisp-dev-replay-test--status result)))
     (should (file-exists-p marker))
     (should (string-match-p "日本語"
                            (or (cdr (assoc "stdout" (cdr (assoc "data" result)))) "")))
     (should (= 1 (cdr (assoc "executed_forms" (cdr (assoc "data" result))))))
     (should (= 0 (cdr (assoc "worker_exit_code" (cdr (assoc "data" result))))))
     (should (null (bound-and-true-p nelisp-dev-replay-test-child))))))

(ert-deftest nelisp-dev-replay/missing-policy-does-not-run ()
  (nelisp-dev-replay-test--session
   "(with-temp-file \"%MARKER%\" (insert \"unexpected\"))\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest nil)
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should-not (file-exists-p marker)))))

(ert-deftest nelisp-dev-replay/tampered-source-is-refused-before-execution ()
  (nelisp-dev-replay-test--session
   "(with-temp-file \"%MARKER%\" (insert \"executed\"))\n"
   (with-temp-file source
     (insert (replace-regexp-in-string "%MARKER%" marker
                                       "(with-temp-file \"%MARKER%\" (insert \"tampered\"))\n"
                                       t t)))
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should-not (file-exists-p marker)))))

(ert-deftest nelisp-dev-replay/error-before-success-marker-is-failure ()
  (nelisp-dev-replay-test--session
   "(error \"intentional replay failure\")\n(setq nelisp-dev-replay-test-success-marker t)\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (null (bound-and-true-p nelisp-dev-replay-test-success-marker))))))

(ert-deftest nelisp-dev-replay/expired-deadline-without-record-is-a-timeout ()
  "An exhausted deadline with no terminal record is a timeout, not a worker fault.

The classification used to be derived from a `process-live-p\=' sample taken
after the wait loop.  On windows-latest/30.1 the child was already reaped at
that moment, so `nelisp-dev-replay/timeout-kills-worker\=' observed phase
\"worker\" after burning its full 1s deadline (CI run 34608788600).  These
cases pin the decision to the clock and the terminal record instead."
  (should (eq 'timeout (nelisp-dev-replay--outcome t nil nil)))
  (should (eq 'worker (nelisp-dev-replay--outcome nil nil nil)))
  (should (eq 'terminal (nelisp-dev-replay--outcome nil nil t)))
  ;; A worker that finished just under the wire keeps its record even though
  ;; the poll noticed only after the deadline had passed.
  (should (eq 'terminal (nelisp-dev-replay--outcome t nil t)))
  (should (eq 'output-limit (nelisp-dev-replay--outcome t t nil)))
  (should (eq 'output-limit (nelisp-dev-replay--outcome nil t t))))

(defun nelisp-dev-replay-test--worker-scratch-p (directory)
  "Say whether DIRECTORY is a replay worker's scratch directory.
The test harness's own temporary directory shares its prefix, and deleting
that one has to keep working while a stub refuses the worker's."
  (let ((base (file-name-nondirectory (directory-file-name directory))))
    (and (string-prefix-p "nelisp-dev-replay-" base)
         (not (string-prefix-p "nelisp-dev-replay-test-" base)))))

(ert-deftest nelisp-dev-replay/verdict-survives-a-cleanup-the-platform-refuses ()
  "A scratch directory the platform will not remove does not erase the verdict.

MS-Windows refuses to remove a directory that is still a live process's
working directory, and the killed worker exits asynchronously, so the
removal in `nelisp-dev-replay--worker\='s cleanup raced the child.  The
`file-error\=' it raised escaped the `unwind-protect\=', discarding an
already-correct result: a run that had burned its whole 1s deadline
reported phase \"worker\" instead of \"timeout\" (CI run 34682155790,
windows-latest/30.1).  Signalling from the removal reproduces that here on
any platform."
  (nelisp-dev-replay-test--session
   "(while t)\n"
   (let* ((real (symbol-function 'delete-directory))
          (refused 0) (held nil) result)
     (unwind-protect
         (setq result
               (cl-letf (((symbol-function 'delete-directory)
                          (lambda (directory &rest arguments)
                            (if (nelisp-dev-replay-test--worker-scratch-p directory)
                                (progn (setq refused (1+ refused) held directory)
                                       (signal 'file-error
                                               (list "Removing directory"
                                                     "Permission denied" directory)))
                              (apply real directory arguments)))))
                 (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only" 1)
                  (list :target "host-emacs" :root directory))))
       ;; The stub blocked the real removal; do not leave the scratch behind.
       (when (and held (file-directory-p held)) (funcall real held t)))
     (should (> refused 0))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (equal "timeout"
                    (cdr (assoc "phase" (cdr (assoc "summary" result))))))
     (should (eq t (cdr (assoc "deadline_expired" (cdr (assoc "data" result)))))))))

(ert-deftest nelisp-dev-replay/discarding-a-held-directory-retries-then-reports ()
  "The removal retries a directory the platform holds, and never raises."
  (let* ((scratch (make-temp-file "nelisp-dev-replay-discard-" t))
         (real (symbol-function 'delete-directory))
         (calls 0)
         (refuse (lambda (directory &rest _)
                   (setq calls (1+ calls))
                   (signal 'file-error (list "Removing directory"
                                             "Permission denied" directory)))))
    (unwind-protect
        (progn
          ;; A hold that lets go is waited out rather than reported.
          (should (cl-letf (((symbol-function 'delete-directory)
                             (lambda (directory &rest arguments)
                               (setq calls (1+ calls))
                               (if (< calls 3)
                                   (signal 'file-error
                                           (list "Removing directory"
                                                 "Permission denied" directory))
                                 (apply real directory arguments)))))
                    (nelisp-dev-replay--discard-directory scratch)))
          (should (= 3 calls))
          (should-not (file-directory-p scratch))
          ;; A hold that never lets go returns nil instead of raising.
          (setq calls 0)
          (should-not (cl-letf (((symbol-function 'delete-directory) refuse))
                        (nelisp-dev-replay--discard-directory scratch)))
          (should (= nelisp-dev-replay--discard-attempts calls)))
      (when (file-directory-p scratch) (delete-directory scratch t)))))

(ert-deftest nelisp-dev-replay/timeout-reports-its-deadline-evidence ()
  "A timed-out replay reports the clock evidence its verdict rests on."
  (nelisp-dev-replay-test--session
   "(while t)\n"
   (let* ((result (nelisp-dev-replay-dispatch
                   (nelisp-dev-replay-test--request manifest "explicit-only" 1)
                   (list :target "host-emacs" :root directory)))
          (data (cdr (assoc "data" result))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     ;; The allowed budget is reported on every failure path, including the
     ;; one a raise during the worker's own teardown takes.  Asserted first
     ;; and separately, because it is the only one of these fields that does
     ;; not depend on reaching the deadline logic.
     (should (equal 1 (cdr (assoc "timeout_seconds" data))))
     ;; The remaining fields only exist when the run actually got as far as
     ;; the wait loop.  On a platform where the teardown raised instead,
     ;; `phase' is "worker" and there is no clock evidence to check -- that
     ;; is a different (already covered) outcome, not a silent pass here.
     (let ((phase (cdr (assoc "phase" (cdr (assoc "summary" result))))))
       (when (equal phase "timeout")
         (should (eq t (cdr (assoc "deadline_expired" data))))
         (should (>= (cdr (assoc "elapsed_seconds" data)) 1))
         (should (stringp (cdr (assoc "process_status" data)))))))))

(ert-deftest nelisp-dev-replay/timeout-kills-worker ()
  (nelisp-dev-replay-test--session
   "(while t)\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only" 1)
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (equal "timeout"
                    (cdr (assoc "phase" (cdr (assoc "summary" result)))))))))

(ert-deftest nelisp-dev-replay/output-is-bounded ()
  (nelisp-dev-replay-test--session
   "(princ (make-string 100000 ?x))\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (let ((data (cdr (assoc "data" result))))
       (should (cdr (assoc "output_truncated" data)))
       (should (< (string-bytes (or (cdr (assoc "stdout" data)) "")) 70000))))))

(ert-deftest nelisp-dev-replay/early-zero-exit-has-no-terminal-success ()
  (nelisp-dev-replay-test--session
   "(princ \"SUCCESS\") (kill-emacs 0)\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (equal "NELISP-DEV-REPLAY-INCOMPLETE"
                    (cdr (assoc "code" (aref (cdr (assoc "diagnostics" result)) 0))))))))

(ert-deftest nelisp-dev-replay/buffer-edits-cannot-skip-later-forms ()
  (nelisp-dev-replay-test--session
   "nil\n"
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record '(erase-buffer))
     (nelisp-repl-session-record '(error "must still execute"))
     (nelisp-dev-session-dispatch "session.export"
       (list :session-id "fixture" :recipe recipe :manifest manifest)))
   (let* ((result (nelisp-dev-replay-dispatch
                   (nelisp-dev-replay-test--request manifest "explicit-only")
                   (list :target "host-emacs" :root directory)))
          (data (cdr (assoc "data" result))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (= 1 (cdr (assoc "executed_forms" data)))))))

(ert-deftest nelisp-dev-replay/empty-recipe-is-not-a-successful-run ()
  (nelisp-dev-replay-test--session
   "nil\n"
   (let ((nelisp-repl-session--records nil))
     (nelisp-dev-session-dispatch "session.export"
       (list :session-id "fixture" :recipe recipe :manifest manifest)))
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "inconclusive" (nelisp-dev-replay-test--status result)))
     (should (= 0 (cdr (assoc "executed_forms" (cdr (assoc "data" result)))))))))

(ert-deftest nelisp-dev-replay/non-utf8-locale-preserves-worker-output ()
  (nelisp-dev-replay-test--session
   "(princ \"日本語\")\n"
   (let ((original (symbol-function 'make-process)))
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (let ((command (plist-get arguments :command)))
                    (setq arguments
                          (plist-put arguments :command
                            (append (cl-subseq command 0 3)
                                    '("--eval" "(progn (setq locale-coding-system 'us-ascii coding-system-for-write 'us-ascii) (set-terminal-coding-system 'us-ascii))")
                                    (nthcdr 3 command))))
                    (apply original arguments)))))
       (let ((result (nelisp-dev-replay-dispatch
                      (nelisp-dev-replay-test--request manifest "explicit-only")
                      (list :target "host-emacs" :root directory))))
         (should (equal "ok" (nelisp-dev-replay-test--status result)))
         (should (equal "日本語" (cdr (assoc "stdout" (cdr (assoc "data" result)))))))))))

(provide 'nelisp-dev-replay-test)
