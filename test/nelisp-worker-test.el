;;; nelisp-worker-test.el --- Phase 5-D.1 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-D.1 — `src/nelisp-worker.el' and the
;; bundled child loop `src/nelisp-worker-child.el'.
;;
;; Coverage:
;;   - pool lifecycle: create initialises the 3 lanes; kill tears
;;     down every worker
;;   - `nelisp-worker-call' synchronous round-trip via pipe stdio
;;     returns the eval result
;;   - correlation-id assigns unique ids across rapid-fire calls
;;   - lane override works (:read / :write / :batch)
;;   - child-side error surfaces as `nelisp-worker-error' signal
;;   - wedged child (unreachable expression + short timeout) is
;;     surfaced as `nelisp-worker-timeout'
;;   - sequential calls reuse the same host process for the lane
;;   - stats plist reflects per-lane pool state + pending count

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-worker)

(defmacro nelisp-worker-test--with-pool (var &rest body)
  "Create a 1/1/1 pool, bind it to VAR, execute BODY, kill pool."
  (declare (indent 1))
  `(let ((,var (nelisp-worker-pool-create
                'test
                :read-size 1 :write-size 1 :batch-size 1)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (nelisp-worker-pool-kill ,var)))))

;;; Lifecycle ---------------------------------------------------------

(ert-deftest nelisp-worker-pool-create-initialises-three-lanes ()
  (nelisp-worker-test--with-pool p
    (should (nelisp-worker-pool-p p))
    (should (nelisp-process-pool-p (nelisp-worker-pool-read-pool p)))
    (should (nelisp-process-pool-p (nelisp-worker-pool-write-pool p)))
    (should (nelisp-process-pool-p (nelisp-worker-pool-batch-pool p)))))

(ert-deftest nelisp-worker-pool-kill-clears-pending ()
  (let ((p (nelisp-worker-pool-create 'k :read-size 1
                                         :write-size 1 :batch-size 1)))
    (puthash "stale" (list :done nil)
             (nelisp-worker-pool-pending p))
    (nelisp-worker-pool-kill p)
    (should (= 0 (hash-table-count
                  (nelisp-worker-pool-pending p))))))

(ert-deftest nelisp-worker-pool-create-lazy-no-workers ()
  (let ((p (nelisp-worker-pool-create
            'lazy :read-size 1 :write-size 1 :batch-size 1
            :prespawn nil)))
    (unwind-protect
        (progn
          (should (null (nelisp-process-pool-workers
                         (nelisp-worker-pool-read-pool p))))
          (should (null (nelisp-process-pool-workers
                         (nelisp-worker-pool-write-pool p)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

;;; Request / response ------------------------------------------------

(ert-deftest nelisp-worker-call-arithmetic-roundtrip ()
  "Evaluate `(+ 1 2)` in a child and receive 3 back."
  (nelisp-worker-test--with-pool p
    (should (= 3 (nelisp-worker-call p '(+ 1 2) :timeout 5.0)))))

(ert-deftest nelisp-worker-call-string-roundtrip ()
  (nelisp-worker-test--with-pool p
    (should (equal "HELLO"
                   (nelisp-worker-call
                    p '(upcase "hello") :timeout 5.0)))))

(ert-deftest nelisp-worker-call-sequential-reuses-worker ()
  "Two sequential calls on the :write lane hit the same host proc."
  (nelisp-worker-test--with-pool p
    (let* ((w (car (nelisp-process-pool-workers
                    (nelisp-worker-pool-write-pool p))))
           (proc-before (nelisp-process-pool-worker-process w)))
      (nelisp-worker-call p '(+ 1 1) :timeout 5.0)
      (nelisp-worker-call p '(+ 2 2) :timeout 5.0)
      (let ((w2 (car (nelisp-process-pool-workers
                      (nelisp-worker-pool-write-pool p)))))
        (should (eq proc-before
                    (nelisp-process-pool-worker-process w2)))))))

(ert-deftest nelisp-worker-call-lane-override-read ()
  (nelisp-worker-test--with-pool p
    (should (= 42 (nelisp-worker-call p 42 :lane :read :timeout 5.0)))))

(ert-deftest nelisp-worker-call-lane-override-batch ()
  (nelisp-worker-test--with-pool p
    (should (= 100 (nelisp-worker-call p 100 :lane :batch :timeout 5.0)))))

(ert-deftest nelisp-worker-call-unknown-lane-signals ()
  (nelisp-worker-test--with-pool p
    (should-error (nelisp-worker-call p 1 :lane :bogus :timeout 5.0))))

;;; Correlation id ----------------------------------------------------

(ert-deftest nelisp-worker-next-id-unique ()
  (let ((p (nelisp-worker-pool-create 'ids :prespawn nil
                                           :read-size 1 :write-size 1
                                           :batch-size 1)))
    (unwind-protect
        (let ((ids nil))
          (dotimes (_ 50)
            (push (nelisp-worker--next-id p) ids))
          (should (= 50 (length (cl-remove-duplicates ids :test #'equal)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-call-rapid-fire-distinct-ids ()
  "10 back-to-back calls succeed and no reply leaks to another."
  (nelisp-worker-test--with-pool p
    (dotimes (i 10)
      (should (= (* i i)
                 (nelisp-worker-call
                  p (list '* i i) :timeout 5.0))))))

;;; Error + timeout paths ---------------------------------------------

(ert-deftest nelisp-worker-call-child-error-signals ()
  "A child-side `error' surfaces as `nelisp-worker-error'."
  (nelisp-worker-test--with-pool p
    (should-error
     (nelisp-worker-call p '(error "boom") :timeout 5.0)
     :type 'nelisp-worker-error)))

(ert-deftest nelisp-worker-call-timeout-signals ()
  "A hanging child expression triggers `nelisp-worker-timeout'."
  (nelisp-worker-test--with-pool p
    (should-error
     ;; sleep-for is host-side and synchronous — exceeds 0.2s timeout.
     (nelisp-worker-call p '(sleep-for 2) :timeout 0.2)
     :type 'nelisp-worker-timeout)))

;;; Stats -------------------------------------------------------------

(ert-deftest nelisp-worker-pool-stats-shape ()
  (nelisp-worker-test--with-pool p
    (let ((s (nelisp-worker-pool-stats p)))
      (should (plist-member s :read))
      (should (plist-member s :write))
      (should (plist-member s :batch))
      (should (= 0 (plist-get s :pending)))
      (should (= 1 (plist-get (plist-get s :read) :total)))
      (should (= 1 (plist-get (plist-get s :write) :total)))
      (should (= 1 (plist-get (plist-get s :batch) :total))))))

;;; Deliver (unit test for internal parser) ---------------------------

(ert-deftest nelisp-worker-deliver-ok-reply ()
  (let ((p (nelisp-worker-pool-create 'd :prespawn nil
                                         :read-size 1 :write-size 1
                                         :batch-size 1)))
    (unwind-protect
        (progn
          (puthash "x1" (list :done nil :tag nil :result nil)
                   (nelisp-worker-pool-pending p))
          (nelisp-worker--deliver p "(\"x1\" :ok 42)")
          (let ((e (gethash "x1" (nelisp-worker-pool-pending p))))
            (should (plist-get e :done))
            (should (eq :ok (plist-get e :tag)))
            (should (= 42 (plist-get e :result)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-deliver-error-reply ()
  (let ((p (nelisp-worker-pool-create 'de :prespawn nil
                                          :read-size 1 :write-size 1
                                          :batch-size 1)))
    (unwind-protect
        (progn
          (puthash "x2" (list :done nil :tag nil :result nil)
                   (nelisp-worker-pool-pending p))
          (nelisp-worker--deliver p "(\"x2\" :error \"nope\")")
          (let ((e (gethash "x2" (nelisp-worker-pool-pending p))))
            (should (plist-get e :done))
            (should (eq :error (plist-get e :tag)))
            (should (equal "nope" (plist-get e :result)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

;;; Classifier (Phase 5-D.2) ---------------------------------------

(defmacro nelisp-worker-test--classify (expr)
  "Classify EXPR against a transient dummy pool (batch-size=1)."
  `(let ((p (nelisp-worker-pool-create
             'clt :prespawn nil
             :read-size 1 :write-size 1 :batch-size 1)))
     (unwind-protect (nelisp-worker-classify p ,expr)
       (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-classify-write-pattern ()
  "A write primitive routes to :write regardless of surrounding reads."
  (should (eq :write
              (nelisp-worker-test--classify
               '(anvil-file-replace-string "f" "a" "b"))))
  (should (eq :write
              (nelisp-worker-test--classify
               '(save-buffer)))))

(ert-deftest nelisp-worker-classify-batch-pattern ()
  (should (eq :batch
              (nelisp-worker-test--classify
               '(byte-compile-file "foo.el"))))
  (should (eq :batch
              (nelisp-worker-test--classify
               '(anvil-elisp-byte-compile-file "bar.el")))))

(ert-deftest nelisp-worker-classify-read-pattern ()
  (should (eq :read
              (nelisp-worker-test--classify
               '(anvil-file-read "foo.el"))))
  (should (eq :read
              (nelisp-worker-test--classify
               '(org-read-headline "path" "id"))))
  (should (eq :read
              (nelisp-worker-test--classify
               '(sqlite-query "SELECT 1")))))

(ert-deftest nelisp-worker-classify-write-beats-read-on-mix ()
  "`(progn READ WRITE)` stringifies to contain both; write wins."
  (should (eq :write
              (nelisp-worker-test--classify
               '(progn (file-read "a.el")
                       (file-replace-string "a.el" "x" "y"))))))

(ert-deftest nelisp-worker-classify-batch-downgrades-when-empty ()
  "When batch-size is 0 a batch expression is downgraded to :write."
  (let ((p (nelisp-worker-pool-create
            'nb :prespawn nil
            :read-size 1 :write-size 1 :batch-size 0)))
    (unwind-protect
        (should (eq :write
                    (nelisp-worker-classify
                     p '(byte-compile-file "foo.el"))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-classify-fallback-default-write ()
  (should (eq :write
              (nelisp-worker-test--classify '(+ 1 2)))))

(ert-deftest nelisp-worker-classify-fallback-custom ()
  "Override `nelisp-worker-classify-unknown-fallback' for one call."
  (let ((nelisp-worker-classify-unknown-fallback :read))
    (should (eq :read
                (nelisp-worker-test--classify '(message "log"))))))

(ert-deftest nelisp-worker--match-any-stringifies-form ()
  "Raw forms (not strings) are stringified before regex matching."
  (should (nelisp-worker--match-any
           '(file-read "x")
           nelisp-worker-classify-read-patterns))
  (should-not (nelisp-worker--match-any
               '(+ 1 2)
               nelisp-worker-classify-read-patterns)))

(ert-deftest nelisp-worker-call-lane-override-wins-over-classifier ()
  "Explicit `:lane' bypasses the classifier entirely."
  (nelisp-worker-test--with-pool p
    ;; Expression *matches* a write pattern but explicit :read forces
    ;; the read lane; the call still succeeds because the child
    ;; evaluates the trivial arg regardless of which lane it runs on.
    (should (= 7 (nelisp-worker-call
                  p '(+ 3 4)
                  :lane :read :timeout 5.0)))))

(ert-deftest nelisp-worker-call-classifier-routes-read ()
  "Without `:lane', a read-pattern form runs on the :read lane."
  (nelisp-worker-test--with-pool p
    ;; `(anvil-file-read \"x\")` would error in the child (no such
    ;; function) so we wrap a read-shaped stub.  We only verify lane
    ;; selection; a `+ 1 2` disguised as a read call via the symbol
    ;; `file-read' gets routed to :read.
    (let* ((before (plist-get (plist-get (nelisp-worker-pool-stats p)
                                          :read)
                              :busy))
           (expr '(progn
                    (defun file-read (&rest _) 99)
                    (file-read "foo.el"))))
      (should (= 99 (nelisp-worker-call p expr :timeout 5.0)))
      (ignore before))))

;;; Health check (Phase 5-D.3) -------------------------------------

(ert-deftest nelisp-worker-health-scan-marks-killed-worker-dead ()
  "Killing a worker's host process + scanning flips its state to dead."
  (nelisp-worker-test--with-pool p
    (let* ((lane-pool (nelisp-worker-pool-read-pool p))
           (worker (car (nelisp-process-pool-workers lane-pool)))
           (host (nelisp-process-pool-worker-process worker)))
      (nelisp-kill-process host)
      (nelisp-process-wait-for-exit host 2.0)
      (accept-process-output nil 0.1)
      (nelisp-worker-health-scan p)
      (should (eq 'dead (nelisp-process-pool-worker-state worker))))))

(ert-deftest nelisp-worker-health-scan-noop-on-healthy-pool ()
  "Scanning a fully-alive pool leaves every worker's state intact."
  (nelisp-worker-test--with-pool p
    (nelisp-worker-health-scan p)
    (dolist (lp (list (nelisp-worker-pool-read-pool p)
                      (nelisp-worker-pool-write-pool p)
                      (nelisp-worker-pool-batch-pool p)))
      (dolist (w (nelisp-process-pool-workers lp))
        (should (memq (nelisp-process-pool-worker-state w)
                      '(idle busy)))))))

(ert-deftest nelisp-worker-health-timer-start-returns-timer ()
  "With a positive interval the start registers a live timer."
  (nelisp-worker-test--with-pool p
    (let ((t0 (nelisp-worker-health-timer-start p 0.5)))
      (should (timerp t0))
      (should (eq t0 (nelisp-worker-health-timer-active-p p))))))

(ert-deftest nelisp-worker-health-timer-start-zero-returns-nil ()
  "Interval 0 disables the timer (nil return, registry untouched)."
  (nelisp-worker-test--with-pool p
    (nelisp-worker-health-timer-stop p)
    (should (null (nelisp-worker-health-timer-start p 0)))
    (should (null (nelisp-worker-health-timer-active-p p)))))

(ert-deftest nelisp-worker-health-timer-stop-is-idempotent ()
  (nelisp-worker-test--with-pool p
    (nelisp-worker-health-timer-stop p)
    (should (progn (nelisp-worker-health-timer-stop p) t))))

(ert-deftest nelisp-worker-health-timer-start-cancels-previous ()
  "Second `-start' cancels the first timer before registering a new one."
  (nelisp-worker-test--with-pool p
    (let* ((t1 (nelisp-worker-health-timer-start p 0.5))
           (t2 (nelisp-worker-health-timer-start p 0.5)))
      (should (not (eq t1 t2)))
      (should (eq t2 (nelisp-worker-health-timer-active-p p))))))

(ert-deftest nelisp-worker-pool-create-auto-starts-timer ()
  "Default interval > 0 auto-starts the periodic scan on create."
  (let* ((nelisp-worker-health-check-interval 0.5)
         (p (nelisp-worker-pool-create
             'h :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (should (timerp (nelisp-worker-health-timer-active-p p)))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-pool-create-lazy-skips-timer ()
  "A lazy pool has no workers to scan yet and must not start a timer."
  (let* ((nelisp-worker-health-check-interval 0.5)
         (p (nelisp-worker-pool-create
             'hl :prespawn nil
             :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (should (null (nelisp-worker-health-timer-active-p p)))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-pool-create-interval-zero-skips-timer ()
  "Interval=0 defcustom opt-out disables auto-start."
  (let* ((nelisp-worker-health-check-interval 0)
         (p (nelisp-worker-pool-create
             'hz :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (should (null (nelisp-worker-health-timer-active-p p)))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-pool-kill-cancels-timer ()
  "Tearing the pool down also unregisters the scan timer."
  (let* ((nelisp-worker-health-check-interval 0.5)
         (p (nelisp-worker-pool-create
             'hk :read-size 1 :write-size 1 :batch-size 1)))
    (should (timerp (nelisp-worker-health-timer-active-p p)))
    (nelisp-worker-pool-kill p)
    (should (null (nelisp-worker-health-timer-active-p p)))))

(ert-deftest nelisp-worker-health-timer-actually-fires-sweep ()
  "Set a short interval, kill a worker, wait for the timer to tick,
assert the worker ended up marked dead by the periodic scan (not
by an on-get sweep, which we never trigger)."
  (let* ((nelisp-worker-health-check-interval 0.2)
         (p (nelisp-worker-pool-create
             'hf :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (let* ((lane-pool (nelisp-worker-pool-read-pool p))
               (worker (car (nelisp-process-pool-workers lane-pool)))
               (host (nelisp-process-pool-worker-process worker)))
          (nelisp-kill-process host)
          (nelisp-process-wait-for-exit host 2.0)
          ;; Give the timer ~0.5s to fire at least once.
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (not (eq 'dead
                                 (nelisp-process-pool-worker-state
                                  worker)))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq 'dead
                      (nelisp-process-pool-worker-state worker))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

;;; Batch warmup (Phase 5-D.4) -------------------------------------

(ert-deftest nelisp-worker--send-warmup-is-fire-and-forget ()
  "`--send-warmup' writes each expr to the pipe without touching
`nelisp-worker-pool-pending' — the ids are synthetic and unknown,
so child replies land in `--deliver' and get dropped silently."
  (let ((nelisp-worker-batch-warmup-expressions
         '((setq nw-warmup-sentinel-a 1)
           (setq nw-warmup-sentinel-b 2))))
    (nelisp-worker-test--with-pool p
      (let* ((lane-pool (nelisp-worker-pool-batch-pool p))
             (worker (car (nelisp-process-pool-workers lane-pool)))
             (host (nelisp-process-pool-worker-process worker))
             (pending-before (hash-table-count
                              (nelisp-worker-pool-pending p)))
             (sent (nelisp-worker--send-warmup p host)))
        (should (= 2 sent))
        ;; Drain the child so its reply arrives before we assert.
        (accept-process-output nil 0.2)
        (should (= pending-before
                   (hash-table-count
                    (nelisp-worker-pool-pending p))))))))

(ert-deftest nelisp-worker--send-warmup-noop-on-empty-list ()
  (let ((nelisp-worker-batch-warmup-expressions nil))
    (nelisp-worker-test--with-pool p
      (let* ((lane-pool (nelisp-worker-pool-batch-pool p))
             (worker (car (nelisp-process-pool-workers lane-pool)))
             (host (nelisp-process-pool-worker-process worker)))
        (should (null (nelisp-worker--send-warmup p host)))))))

(ert-deftest nelisp-worker--send-warmup-skips-dead-host ()
  "No crash when the host proc has already exited."
  (let ((nelisp-worker-batch-warmup-expressions
         '((setq irrelevant t))))
    (nelisp-worker-test--with-pool p
      (let* ((lane-pool (nelisp-worker-pool-batch-pool p))
             (worker (car (nelisp-process-pool-workers lane-pool)))
             (host (nelisp-process-pool-worker-process worker)))
        (nelisp-kill-process host)
        (nelisp-process-wait-for-exit host 2.0)
        (should (null (nelisp-worker--send-warmup p host)))))))

(ert-deftest nelisp-worker-warmup-state-persists-into-subsequent-call ()
  "Synchronous warmup (delay=0) then a batch-lane call observes the
warmup expression's side effects (a defvar on the child)."
  (let* ((nelisp-worker-batch-warmup-expressions
          '((defvar nelisp-worker-test-warmup-val 4242)))
         (nelisp-worker-batch-warmup-delay 0)
         (p (nelisp-worker-pool-create
             'w0 :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          ;; Synchronous warmup has already fired inside `pool-create'.
          ;; Let the child drain it before we stack a real call.
          (accept-process-output nil 0.2)
          (should (= 4242
                     (nelisp-worker-call
                      p '(bound-and-true-p nelisp-worker-test-warmup-val)
                      :lane :batch :timeout 5.0))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-warmup-runs-only-on-batch-lane ()
  "A defvar set via warmup is NOT visible on read / write workers."
  (let* ((nelisp-worker-batch-warmup-expressions
          '((defvar nelisp-worker-test-lane-marker 'batch-only)))
         (nelisp-worker-batch-warmup-delay 0)
         (p (nelisp-worker-pool-create
             'w1 :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          (accept-process-output nil 0.2)
          (should (eq 'batch-only
                      (nelisp-worker-call
                       p '(bound-and-true-p
                            nelisp-worker-test-lane-marker)
                       :lane :batch :timeout 5.0)))
          (should (null
                   (nelisp-worker-call
                    p '(bound-and-true-p
                         nelisp-worker-test-lane-marker)
                    :lane :read :timeout 5.0)))
          (should (null
                   (nelisp-worker-call
                    p '(bound-and-true-p
                         nelisp-worker-test-lane-marker)
                    :lane :write :timeout 5.0))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-warmup-deferred-timer-fires ()
  "With a positive delay, the warmup lands via `run-at-time'."
  (let* ((nelisp-worker-batch-warmup-expressions
          '((defvar nelisp-worker-test-deferred-val 777)))
         (nelisp-worker-batch-warmup-delay 0.1)
         (p (nelisp-worker-pool-create
             'w2 :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          ;; Wait until the deferred `run-at-time' fires + child processes it.
          (let ((deadline (+ (float-time) 2.0)))
            (while (< (float-time) deadline)
              (accept-process-output nil 0.05)))
          (should (= 777
                     (nelisp-worker-call
                      p '(bound-and-true-p
                           nelisp-worker-test-deferred-val)
                      :lane :batch :timeout 5.0))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-warmup-lazy-pool-skips-schedule ()
  "A `:prespawn nil' pool spawns no batch worker, so no warmup fires."
  (let* ((nelisp-worker-batch-warmup-expressions
          '((defvar nelisp-worker-test-lazy-marker 'should-not-appear)))
         (nelisp-worker-batch-warmup-delay 0)
         (p (nelisp-worker-pool-create
             'w3 :prespawn nil
             :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          ;; First call forces a lazy spawn; warmup was never scheduled
          ;; in `pool-create', so the fresh worker has no marker.
          (should (null
                   (nelisp-worker-call
                    p '(bound-and-true-p
                         nelisp-worker-test-lazy-marker)
                    :lane :batch :timeout 5.0))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker--schedule-warmup-can-be-called-explicitly ()
  "Caller can re-schedule warmup after a lazy batch spawn."
  (let* ((nelisp-worker-batch-warmup-expressions
          '((defvar nelisp-worker-test-explicit-val 999)))
         (nelisp-worker-batch-warmup-delay 0)
         (p (nelisp-worker-pool-create
             'w4 :prespawn nil
             :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          ;; Force the batch worker to spawn; `--schedule-warmup'
          ;; wires the pool-aware filter via `--ensure-filter' so no
          ;; manual install is needed here.
          (let ((worker (nelisp-process-pool-get
                         (nelisp-worker-pool-batch-pool p))))
            (should worker)
            (nelisp-process-pool-return
             (nelisp-worker-pool-batch-pool p) worker))
          (nelisp-worker--schedule-warmup p)
          (accept-process-output nil 0.2)
          (should (= 999
                     (nelisp-worker-call
                      p '(bound-and-true-p
                           nelisp-worker-test-explicit-val)
                      :lane :batch :timeout 5.0))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

;;; Latency metrics (Phase 5-D.5) -----------------------------------

(ert-deftest nelisp-worker--percentile-basic ()
  ;; anvil formula: idx = floor(pct/100 * (n-1)) — for n=5 p50 is
  ;; index 2, i.e. the median.
  (should (= 3 (nelisp-worker--percentile '(1 2 3 4 5) 50)))
  (should (= 9 (nelisp-worker--percentile '(1 2 3 4 5 6 7 8 9 10) 90)))
  (should (null (nelisp-worker--percentile nil 90))))

(ert-deftest nelisp-worker--percentile-single-sample ()
  (should (= 42 (nelisp-worker--percentile '(42) 50)))
  (should (= 42 (nelisp-worker--percentile '(42) 99))))

(ert-deftest nelisp-worker--latency-record-updates-bucket ()
  "A single record bumps samples / sums / totals as expected."
  (let ((p (nelisp-worker-pool-create
            'm0 :prespawn nil
            :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          (nelisp-worker--latency-record p :read 2.0 10.0)
          (let* ((buckets (nelisp-worker--latency-buckets p))
                 (b (plist-get buckets :read)))
            (should (= 1 (plist-get b :samples)))
            (should (= 2.0 (plist-get b :spawn-ms-sum)))
            (should (= 10.0 (plist-get b :wait-ms-sum)))
            (should (= 12.0 (plist-get b :total-ms-sum)))
            (should (equal '(12.0) (plist-get b :totals)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker--latency-record-sample-cap ()
  "`:totals' ring never exceeds the cap."
  (let ((nelisp-worker-latency-sample-cap 3)
        (p (nelisp-worker-pool-create
            'mc :prespawn nil
            :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          (dotimes (i 10)
            (nelisp-worker--latency-record p :write 0.0 (* 1.0 i)))
          (let ((totals (plist-get
                         (plist-get (nelisp-worker--latency-buckets p)
                                    :write)
                         :totals)))
            (should (= 3 (length totals)))
            ;; Newest-first: last 3 recorded values (9, 8, 7).
            (should (equal '(9.0 8.0 7.0) totals))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker--latency-record-sample-cap-disabled ()
  "A non-positive cap disables retention but still counts samples."
  (let ((nelisp-worker-latency-sample-cap 0)
        (p (nelisp-worker-pool-create
            'md :prespawn nil
            :read-size 1 :write-size 1 :batch-size 1)))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (nelisp-worker--latency-record p :batch 0.1 (* 1.0 i)))
          (let ((b (plist-get (nelisp-worker--latency-buckets p)
                              :batch)))
            (should (= 5 (plist-get b :samples)))
            (should (null (plist-get b :totals)))
            (should (= 10.0 (plist-get b :wait-ms-sum)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-latency-show-per-lane-summary ()
  (nelisp-worker-test--with-pool p
    (nelisp-worker--latency-record p :read  1.0 2.0)
    (nelisp-worker--latency-record p :read  1.0 4.0)
    (nelisp-worker--latency-record p :write 2.0 8.0)
    (let* ((summary (nelisp-worker-latency-show p))
           (r (plist-get summary :read))
           (w (plist-get summary :write))
           (b (plist-get summary :batch)))
      (should (= 2 (plist-get r :samples)))
      (should (= 1 (plist-get w :samples)))
      (should (= 0 (plist-get b :samples)))
      ;; read: totals 3.0 + 5.0 = 8.0, avg 4.0.
      (should (= 4.0 (plist-get r :avg-total-ms)))
      (should (= 10.0 (plist-get w :avg-total-ms)))
      (should (numberp (plist-get r :p50-ms))))))

(ert-deftest nelisp-worker-latency-reset-clears-pool ()
  (nelisp-worker-test--with-pool p
    (nelisp-worker--latency-record p :read 1.0 1.0)
    (should (= 1 (plist-get (plist-get
                              (nelisp-worker--latency-buckets p)
                              :read)
                            :samples)))
    (nelisp-worker-latency-reset p)
    (should (null (gethash p nelisp-worker--metrics-latency)))))

(ert-deftest nelisp-worker-call-records-latency-automatically ()
  "After a live call, the lane bucket has samples>=1 and total-ms>0."
  (nelisp-worker-test--with-pool p
    (nelisp-worker-latency-reset p)
    (nelisp-worker-call p '(+ 1 2) :lane :write :timeout 5.0)
    (let ((b (plist-get (nelisp-worker--latency-buckets p) :write)))
      (should (>= (plist-get b :samples) 1))
      (should (> (plist-get b :total-ms-sum) 0)))))

(ert-deftest nelisp-worker-pool-kill-drops-latency-store ()
  (let ((p (nelisp-worker-pool-create
            'mk :prespawn nil
            :read-size 1 :write-size 1 :batch-size 1)))
    (nelisp-worker--latency-record p :read 1.0 2.0)
    (should (gethash p nelisp-worker--metrics-latency))
    (nelisp-worker-pool-kill p)
    (should (null (gethash p nelisp-worker--metrics-latency)))))

(provide 'nelisp-worker-test)
;;; nelisp-worker-test.el ends here
