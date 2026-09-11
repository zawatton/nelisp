;;; nelisp-native-unit-development-test.el --- native REPL rebuild contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-native-unit-development)

(defun nelisp-native-unit-development-test--write (path content)
  "Write CONTENT to PATH with LF line endings on every platform.
Without pinning the coding system, `with-temp-file' writes CRLF on
MS-Windows, so a raw-byte hash of the file taken later would not match a
hash of the literal string used to build the fixture."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path (insert content))))

(defun nelisp-native-unit-development-test--eval-form (form)
  "Return (COMMAND . ARGS) that runs the current Emacs executable on FORM.
Used to stand in for the real compiler subprocess: no shell, no separate
binary, so this works identically on Linux, macOS and Windows."
  (cons (expand-file-name invocation-name invocation-directory)
        (list "-Q" "--batch" "--eval" (prin1-to-string form))))

(defun nelisp-native-unit-development-test--write-and-exit (artifact source content)
  "Return a form that writes CONTENT into SOURCE and ARTIFACT, then exits 0.
Simulates a compiler that both produces a real artifact and (for the
race-condition tests) edits the original source file, all from inside the
bounded subprocess rather than from the test process."
  `(progn
     (let ((coding-system-for-write 'utf-8-unix))
       ,@(when artifact `((with-temp-file ,artifact (insert "stub-artifact"))))
       ,@(when source `((with-temp-file ,source (insert ,content)))))
     (kill-emacs 0)))

(ert-deftest nelisp-native-unit-development/compile-phase-classifies-outcomes ()
  (should (eq :success (nelisp-native-unit-development--compile-phase nil nil 0 t)))
  (should (eq :compiler-error (nelisp-native-unit-development--compile-phase nil nil 1 nil)))
  (should (eq :compiler-error (nelisp-native-unit-development--compile-phase nil nil 0 nil)))
  (should (eq :output-limit (nelisp-native-unit-development--compile-phase nil t 0 t)))
  (should (eq :output-limit (nelisp-native-unit-development--compile-phase t t 1 nil)))
  ;; The exact race the deadline check exists for: the wait loop noticed the
  ;; deadline had already passed, and by then the child had already been
  ;; reaped with no valid completion record (no artifact). A racy
  ;; `process-live-p' sample taken after the fact could read either way;
  ;; the wall clock plus the absence of a successful record decide this one
  ;; deterministically: timeout, regardless of what nominal exit code came
  ;; back with the reaped child.
  (should (eq :timeout (nelisp-native-unit-development--compile-phase t nil nil nil)))
  (should (eq :timeout (nelisp-native-unit-development--compile-phase t nil 0 nil)))
  (should (eq :timeout (nelisp-native-unit-development--compile-phase t nil 1 nil)))
  ;; A deadline that expired just as the child finished cleanly is not a
  ;; timeout: a valid successful completion record forgives the lateness.
  (should (eq :success (nelisp-native-unit-development--compile-phase t nil 0 t))))

(ert-deftest nelisp-native-unit-development/run-bounded-timeout-kills-and-reports ()
  (let ((before (process-list))
        (result (nelisp-native-unit-development-run-bounded
                 (car (nelisp-native-unit-development-test--eval-form '(sleep-for 999)))
                 (cdr (nelisp-native-unit-development-test--eval-form '(sleep-for 999)))
                 1)))
    (should (eq (plist-get result :phase) :timeout))
    (should (plist-get result :deadline-expired))
    (should-not (plist-get result :cancelled))
    (should-not (plist-get result :output-limited))
    (should (>= (plist-get result :elapsed-seconds) 1))
    (should (= 1 (plist-get result :timeout-seconds)))
    (should (equal before (process-list)))))

(ert-deftest nelisp-native-unit-development/run-bounded-cancel-predicate-stops-early ()
  (let* ((checks 0) (before (process-list)) (start (float-time))
         (command-and-args (nelisp-native-unit-development-test--eval-form '(sleep-for 999)))
         (result (nelisp-native-unit-development-run-bounded
                  (car command-and-args) (cdr command-and-args) 30
                  (lambda () (> (setq checks (1+ checks)) 1)))))
    (should (eq (plist-get result :phase) :cancelled))
    (should (plist-get result :cancelled))
    (should (> checks 1))
    (should (< (- (float-time) start) 5))
    (should (equal before (process-list)))))

(ert-deftest nelisp-native-unit-development/run-bounded-output-limit-truncates-and-kills ()
  (let* ((before (process-list))
         (command-and-args
          (nelisp-native-unit-development-test--eval-form
           '(progn (dotimes (_ 400) (princ (make-string 2048 ?a))) (sleep-for 999))))
         (result (nelisp-native-unit-development-run-bounded
                  (car command-and-args) (cdr command-and-args) 30)))
    (should (eq (plist-get result :phase) :output-limit))
    (should (plist-get result :output-truncated))
    (should (plist-get result :output-limited))
    (should (<= (plist-get result :stdout-bytes)
                nelisp-native-unit-development-output-limit))
    (should (> (plist-get result :stdout-bytes) 0))
    (should (equal before (process-list)))))

(ert-deftest nelisp-native-unit-development/failed-or-changed-source-never-stages ()
  (dolist (mode '(compiler-error source-edit))
    (let ((source (make-temp-file "native-unit-source-" nil ".el"))
          (stages 0) result)
      (unwind-protect
          (progn
            (nelisp-native-unit-development-test--write
             source "(defun score (x) (+ x 1))\n")
            (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                       (lambda () (make-string 64 ?a)))
                      (nelisp-native-unit-development--command-function
                       (lambda (_root _script _snapshot artifact _binary)
                         (nelisp-native-unit-development-test--eval-form
                          (if (eq mode 'compiler-error)
                              '(kill-emacs 1)
                            (nelisp-native-unit-development-test--write-and-exit
                             artifact source "(defun score (x) (+ x 2))\n")))))
                      ((symbol-function 'nelisp-native-unit-stage)
                       (lambda (&rest _) (setq stages (1+ stages)))))
              (setq result (nelisp-native-unit-rebuild-and-reload source))
              (should (eq 'rejected (plist-get result :status)))
              (should (= 0 stages))
              (should-not (file-exists-p (plist-get result :artifact)))
              (should-not (file-exists-p (plist-get result :snapshot)))))
        (delete-file source)))))

(ert-deftest nelisp-native-unit-development/publishes-only-explicit-stage-candidate ()
  (let ((source (make-temp-file "native-unit-source-" nil ".el"))
        (published nil) (staged-unit nil))
    (unwind-protect
        (progn
          (nelisp-native-unit-development-test--write
           source "(defun score (x) (+ x 1))\n")
          (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    (nelisp-native-unit-development--command-function
                     (lambda (_root _script _snapshot artifact _binary)
                       (nelisp-native-unit-development-test--eval-form
                        (nelisp-native-unit-development-test--write-and-exit
                         artifact nil nil))))
                    ((symbol-function 'nelisp-native-unit-stage)
                     (lambda (_path unit _exports)
                       (setq staged-unit unit)
                       '(:status staged :candidate-id "candidate")))
                    ((symbol-function 'nelisp-native-unit-publish)
                     (lambda (candidate)
                       (setq published candidate)
                       ;; A fresh list every call: production `plist-put's
                       ;; this destructively, and a shared quoted literal
                       ;; would then mutate across calls (and across tests,
                       ;; since the literal lives inside the compiled
                       ;; lambda, not in a per-call binding).
                       (list :status 'published :unit-id "unit" :generation 2))))
            (let* ((result (nelisp-native-unit-rebuild-and-reload source "unit"))
                   ;; Compare against the bytes actually on disk rather than
                   ;; a hardcoded literal: `with-temp-file' + LF above makes
                   ;; the two identical on every platform, but re-reading
                   ;; the file is what makes this assertion prove that,
                   ;; instead of assuming it.
                   (on-disk (nelisp-native-unit-development--bytes source)))
              (should (eq 'published (plist-get result :status)))
              (should (equal staged-unit "unit"))
              (should (equal published "candidate"))
              ;; Pinned literal: the fixture itself has not silently drifted.
              (should (equal on-disk "(defun score (x) (+ x 1))\n"))
              (should (equal (plist-get result :source-sha256)
                             (secure-hash 'sha256 on-disk))))))
      (delete-file source))))

(ert-deftest nelisp-native-unit-development/source-edited-during-stage-revokes-candidate ()
  (let ((source (make-temp-file "native-unit-source-" nil ".el"))
        discarded published result)
    (unwind-protect
        (progn
          (nelisp-native-unit-development-test--write
           source "(defun score (x) (+ x 1))\n")
          (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    (nelisp-native-unit-development--command-function
                     (lambda (_root _script _snapshot artifact _binary)
                       (nelisp-native-unit-development-test--eval-form
                        (nelisp-native-unit-development-test--write-and-exit
                         artifact nil nil))))
                    ((symbol-function 'nelisp-native-unit-stage)
                     (lambda (&rest _)
                       (nelisp-native-unit-development-test--write
                        source "(defun score (x) (+ x 2))\n")
                       '(:status staged :candidate-id "candidate")))
                    ((symbol-function 'nelisp-native-unit-discard)
                     (lambda (id) (setq discarded id)))
                    ((symbol-function 'nelisp-native-unit-publish)
                     (lambda (&rest _) (setq published t))))
            (setq result (nelisp-native-unit-rebuild-and-reload source "unit"))
            (should (eq (plist-get result :status) 'rejected))
            (should (eq (plist-get result :phase) :publish))
            (should (equal discarded "candidate"))
            (should-not published)
            (should (equal (plist-get result :deleted)
                           (list (plist-get result :artifact)
                                 (plist-get result :snapshot))))
            (should-not (file-exists-p (plist-get result :artifact)))
            (should-not (file-exists-p (plist-get result :snapshot)))))
      (delete-file source))))

(ert-deftest nelisp-native-unit-development/subprocess-timeout-kills-and-reports ()
  (let ((source (make-temp-file "native-unit-source-" nil ".el")))
    (unwind-protect
        (progn
          (nelisp-native-unit-development-test--write source "(defun score (x) (+ x 1))\n")
          (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    (nelisp-native-unit-development--command-function
                     (lambda (&rest _)
                       (nelisp-native-unit-development-test--eval-form '(sleep-for 999)))))
            (let* ((before (process-list))
                   (result (nelisp-native-unit-rebuild-and-reload source nil nil nil 1)))
              (should (eq (plist-get result :status) 'rejected))
              (should (eq (plist-get result :phase) :timeout))
              (should (numberp (plist-get result :elapsed-seconds)))
              (should (>= (plist-get result :elapsed-seconds) 1))
              (should (= 1 (plist-get result :timeout-seconds)))
              (should-not (file-exists-p (plist-get result :artifact)))
              (should-not (file-exists-p (plist-get result :snapshot)))
              (should (equal before (process-list))))))
      (delete-file source))))

(ert-deftest nelisp-native-unit-development/cancel-predicate-stops-before-timeout ()
  (let ((source (make-temp-file "native-unit-source-" nil ".el")))
    (unwind-protect
        (progn
          (nelisp-native-unit-development-test--write source "(defun score (x) (+ x 1))\n")
          (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    (nelisp-native-unit-development--command-function
                     (lambda (&rest _)
                       (nelisp-native-unit-development-test--eval-form '(sleep-for 999)))))
            (let* ((checks 0) (before (process-list)) (start (float-time))
                   (result (nelisp-native-unit-rebuild-and-reload
                            source nil nil nil 30
                            (lambda () (> (setq checks (1+ checks)) 1)))))
              (should (eq (plist-get result :status) 'rejected))
              (should (eq (plist-get result :phase) :cancelled))
              (should (< (- (float-time) start) 5))
              (should-not (file-exists-p (plist-get result :artifact)))
              (should-not (file-exists-p (plist-get result :snapshot)))
              (should (equal before (process-list))))))
      (delete-file source))))

(ert-deftest nelisp-native-unit-development/quit-signal-is-treated-as-cancellation ()
  "A C-g arriving inside the wait loop cancels, and cleans up as such.

Driven through the cancel predicate rather than a timer setting `quit-flag\='.
The timer form passed locally on Emacs 31.1 and failed on every CI platform
with phase `:timeout\=' instead of `:cancelled\=' (run 34623283652): whether
`accept-process-output\=' turns a timer-set `quit-flag\=' into a `quit\=' signal
in batch mode is not something this test gets to assume.  Signalling `quit\='
from the predicate reaches the same handler deterministically, and it also
covers the window the predicate itself occupies -- a C-g can land while
caller-supplied code is on the stack just as easily as during the wait."
  (let ((source (make-temp-file "native-unit-source-" nil ".el"))
        (polls 0))
    (unwind-protect
        (progn
          (nelisp-native-unit-development-test--write source "(defun score (x) (+ x 1))\n")
          (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    (nelisp-native-unit-development--command-function
                     (lambda (&rest _)
                       (nelisp-native-unit-development-test--eval-form '(sleep-for 999)))))
            (let* ((before (process-list)) (start (float-time))
                   (result (nelisp-native-unit-rebuild-and-reload
                            source nil nil nil 30
                            (lambda ()
                              (setq polls (1+ polls))
                              (when (> polls 1) (signal 'quit nil))
                              nil))))
              (should (eq (plist-get result :status) 'rejected))
              (should (eq (plist-get result :phase) :cancelled))
              (should (> polls 1))
              ;; Well inside the 30s deadline, so `:cancelled' cannot be a
              ;; timeout wearing another name.
              (should (< (- (float-time) start) 10))
              ;; The quit must not survive the call and fire at the caller's
              ;; next checkpoint.
              (should-not quit-flag)
              (should-not (file-exists-p (plist-get result :artifact)))
              (should-not (file-exists-p (plist-get result :snapshot)))
              (should (equal before (process-list))))))
      (setq quit-flag nil)
      (delete-file source))))

(ert-deftest nelisp-native-unit-development/subprocess-success-reaches-stage ()
  (let ((source (make-temp-file "native-unit-source-" nil ".el"))
        staged-artifact result)
    (unwind-protect
        (progn
          (nelisp-native-unit-development-test--write source "(defun score (x) (+ x 1))\n")
          (cl-letf (((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    (nelisp-native-unit-development--command-function
                     (lambda (_root _script _snapshot artifact _binary)
                       (nelisp-native-unit-development-test--eval-form
                        (nelisp-native-unit-development-test--write-and-exit
                         artifact nil nil))))
                    ((symbol-function 'nelisp-native-unit-stage)
                     (lambda (path &rest _)
                       (setq staged-artifact path)
                       '(:status staged :candidate-id "candidate")))
                    ((symbol-function 'nelisp-native-unit-publish)
                     (lambda (&rest _) (list :status 'published :unit-id "unit" :generation 1))))
            (setq result (nelisp-native-unit-rebuild-and-reload source "unit"))
            (should (eq (plist-get result :status) 'published))
            (should (equal staged-artifact (plist-get result :artifact)))
            (should (file-exists-p (plist-get result :artifact)))
            (should (file-exists-p (plist-get result :snapshot)))))
      (delete-file source)
      ;; The success path deliberately leaves the artifact/snapshot for the
      ;; publication-owning workstream (see the docstring); clean up this
      ;; test's own leftovers so repeated runs do not litter temp space.
      (when result
        (ignore-errors (delete-file (plist-get result :artifact)))
        (ignore-errors (delete-file (plist-get result :snapshot)))))))

(provide 'nelisp-native-unit-development-test)
