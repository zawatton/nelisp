;;; nelisp-worker-soak-test.el --- Phase 5-D.6 soak -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-D.6 per Doc 15.  Advisory-only soak ERT exercising the
;; 3-lane worker pool under sustained mixed load.  Not gated —
;; invoked via `make soak' and explicitly excluded from the default
;; `make test' glob so CI never picks it up by accident.
;;
;; Scenarios:
;;
;;   sequential-mix
;;     20 :read + 5 :write + 1 :batch calls back-to-back through the
;;     blocking `nelisp-worker-call' surface.  Verifies per-lane
;;     latency metrics populate and total wall-clock stays sane.
;;
;;   no-starvation-under-batch
;;     Fires `(sleep-for 2.0)' on :batch asynchronously via a
;;     test-local async-send helper (same primitives
;;     `nelisp-worker-call' uses internally), then runs 20 :read +
;;     5 :write serial calls.  The flagship assertion is that the
;;     20+5 serial calls complete in well under 2.0 s while the
;;     batch worker is mid-sleep — proving per-lane isolation (a
;;     saturated batch lane does not starve read / write).
;;
;; Notes:
;;
;;   - Read pool size is 2, but the host-side `nelisp-worker-call'
;;     surface is blocking, so reads are serialised from this host
;;     regardless.  The soak proves no cross-lane starvation; a
;;     concurrency-within-lane soak is deferred until an actor-
;;     friendly call variant lands (§3.1 commentary in
;;     `src/nelisp-worker.el').
;;   - Wall-clock budgets are deliberately loose so CI under load
;;     does not flap.  Tighter numbers live in bench/ (future).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-process)
(require 'nelisp-process-pool)
(require 'nelisp-worker)

;;; Test-local async primitives -------------------------------------
;;
;; The public `nelisp-worker-call' is blocking; proving per-lane
;; isolation requires firing :batch without waiting for its reply so
;; :read / :write traffic overlaps it.  We do that here by reusing
;; the same internal plumbing `nelisp-worker-call' uses — no new
;; public surface, no parallel code path to drift.

(defun nelisp-worker-soak--async-send (pool expression lane)
  "Fire EXPRESSION on LANE of POOL without waiting.  Return a ticket.
The returned ticket plist carries `:id' / `:entry' / `:worker' /
`:wrap' / `:lane' / `:lane-pool' / `:t-sent'; `nelisp-worker-soak
--poll-all' consumes and finalises it."
  (let* ((lane-pool (nelisp-worker--lane-pool pool lane))
         (worker (nelisp-process-pool-get lane-pool))
         (_ (unless worker
              (signal 'nelisp-worker-busy (list lane))))
         (wrap (nelisp-process-pool-worker-process worker))
         (id (nelisp-worker--next-id pool))
         (entry (list :done nil :tag nil :result nil))
         (t-sent (float-time))
         (request (cons id expression)))
    (nelisp-worker--ensure-filter pool wrap)
    (puthash id entry (nelisp-worker-pool-pending pool))
    (nelisp-process-send-string
     wrap (concat (prin1-to-string request) "\n"))
    (list :id id :lane lane :worker worker :wrap wrap
          :lane-pool lane-pool :entry entry :t-sent t-sent)))

(defun nelisp-worker-soak--poll-all (pool tickets timeout)
  "Poll TICKETS until each is :done or TIMEOUT seconds elapse.
Returns TICKETS augmented with `:done' / `:tag' / `:result' /
`:t-done' / `:latency-ms'.  Also releases every ticket's worker
back to its lane pool and drops the pending entry."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (< (float-time) deadline)
                (cl-find-if
                 (lambda (tk)
                   (not (plist-get (plist-get tk :entry) :done)))
                 tickets))
      ;; `nil' process drains output from every live process in one
      ;; shot, so replies on any lane land in the pending table.
      (accept-process-output nil 0.05)))
  (mapcar
   (lambda (tk)
     (let* ((entry (plist-get tk :entry))
            (t-done (float-time))
            (latency-ms
             (* 1000.0 (- t-done (plist-get tk :t-sent)))))
       (remhash (plist-get tk :id)
                (nelisp-worker-pool-pending pool))
       (nelisp-process-pool-return
        (plist-get tk :lane-pool) (plist-get tk :worker))
       (append tk
               (list :done       (plist-get entry :done)
                     :tag        (plist-get entry :tag)
                     :result     (plist-get entry :result)
                     :t-done     t-done
                     :latency-ms latency-ms))))
   tickets))

;;; Sequential mix --------------------------------------------------

(ert-deftest nelisp-worker-soak/sequential-mix ()
  "20 :read + 5 :write + 1 :batch back-to-back — metrics populate on every lane."
  (skip-unless (executable-find "emacs"))
  (let* ((pool (nelisp-worker-pool-create
                'soak-sequential
                :read-size 2 :write-size 1 :batch-size 1
                :prespawn t))
         (t0 (float-time)))
    (unwind-protect
        (progn
          (dotimes (i 20)
            (should (= (1+ i)
                       (nelisp-worker-call
                        pool `(+ ,i 1) :lane :read :timeout 10.0))))
          (dotimes (i 5)
            (should (equal (list i)
                           (nelisp-worker-call
                            pool `(list ,i) :lane :write :timeout 10.0))))
          (should (= 6 (nelisp-worker-call
                        pool '(* 2 3) :lane :batch :timeout 10.0)))
          (let* ((metrics (nelisp-worker-latency-show pool))
                 (r (plist-get metrics :read))
                 (w (plist-get metrics :write))
                 (b (plist-get metrics :batch)))
            (should (= 20 (plist-get r :samples)))
            (should (= 5  (plist-get w :samples)))
            (should (= 1  (plist-get b :samples)))
            (should (numberp (plist-get r :p50-ms)))
            (should (numberp (plist-get r :p90-ms)))
            (should (numberp (plist-get r :p99-ms)))
            ;; Conservative stability bound — a read round-trip on a
            ;; warm :batch-Q child on commodity hardware is well under
            ;; 100 ms.  10 s leaves ample room for loaded CI.
            (should (< (plist-get r :p99-ms) 10000.0)))
          ;; Whole-suite budget — catches accidental O(n^2) regressions.
          (should (< (- (float-time) t0) 60.0)))
      (nelisp-worker-pool-kill pool))))

;;; No-starvation under batch ---------------------------------------

(ert-deftest nelisp-worker-soak/no-starvation-under-batch ()
  "A saturated :batch lane does not starve :read / :write.

Sends `(sleep-for 2.0)' asynchronously on :batch; then runs 20
serial :read + 5 serial :write calls.  The serial 25 calls must
finish in well under 2.0 s while the batch worker is blocked
inside `sleep-for', which is only true if the batch lane does not
block the other two lanes' workers.  Finally, polls for and
validates the batch reply."
  (skip-unless (executable-find "emacs"))
  (let* ((pool (nelisp-worker-pool-create
                'soak-starvation
                :read-size 2 :write-size 1 :batch-size 1
                :prespawn t)))
    (unwind-protect
        (let* ((t0 (float-time))
               (batch-ticket
                (nelisp-worker-soak--async-send
                 pool '(progn (sleep-for 2.0) 'batch-done) :batch))
               (t-batch-sent (float-time))
               read-latencies
               write-latencies)
          ;; Let the batch child actually enter `sleep-for' before we
          ;; start hammering the other lanes — without this the batch
          ;; may not be "mid-flight" when the reads fire.
          (sleep-for 0.1)
          (dotimes (i 20)
            (let ((t-s (float-time)))
              (should (= (1+ i)
                         (nelisp-worker-call
                          pool `(+ ,i 1) :lane :read :timeout 5.0)))
              (push (* 1000.0 (- (float-time) t-s)) read-latencies)))
          (dotimes (i 5)
            (let ((t-s (float-time)))
              (should (equal (list i)
                             (nelisp-worker-call
                              pool `(list ,i) :lane :write :timeout 5.0)))
              (push (* 1000.0 (- (float-time) t-s)) write-latencies)))
          (let ((elapsed-mid (- (float-time) t-batch-sent)))
            ;; The decisive proof: 25 serial calls finished while the
            ;; batch worker is still sleeping (~2 s total).  1.8 s
            ;; leaves ~200 ms headroom; the calls themselves take
            ;; tens of ms each so normal wall-clock is ~0.5-1.0 s.
            (should (< elapsed-mid 1.8)))
          ;; Now collect the batch reply.
          (nelisp-worker-soak--poll-all pool (list batch-ticket) 5.0)
          (let ((entry (plist-get batch-ticket :entry)))
            (should (plist-get entry :done))
            (should (eq :ok (plist-get entry :tag)))
            (should (eq 'batch-done (plist-get entry :result))))
          ;; Read-lane stability across the soak.
          (let* ((sorted (sort (copy-sequence read-latencies) #'<))
                 (n (length sorted))
                 (p99 (nth (max 0 (min (1- n)
                                       (floor (* 0.99 (1- n)))))
                           sorted)))
            ;; Individual read round-trip must not degrade while batch
            ;; is mid-sleep.  500 ms is a generous per-call ceiling.
            (should (< p99 500.0)))
          (should (< (- (float-time) t0) 10.0)))
      (nelisp-worker-pool-kill pool))))

(provide 'nelisp-worker-soak-test)
;;; nelisp-worker-soak-test.el ends here
