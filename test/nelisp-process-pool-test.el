;;; nelisp-process-pool-test.el --- Phase 5-C.1b ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-C.1b — `src/nelisp-process-pool.el'.
;;
;; Coverage:
;;   - create with and without prespawn
;;   - get reuses existing idle worker
;;   - get spawns up to SIZE
;;   - get returns nil when full + all busy
;;   - return flips busy -> idle
;;   - kill terminates every worker, empties roster
;;   - stats plist reflects state transitions
;;   - crash sweep: a dead worker is replaced on next get
;;   - quick-alive-p distinguishes live / dead

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-process)
(require 'nelisp-process-pool)

(defconst nelisp-process-pool-test--cmd
  '("sleep" "30")
  "Long-lived stub command used as a generic pool worker.")

(defmacro nelisp-process-pool-test--with-pool (pool-var &rest body)
  "Create a 2-worker pool, bind it to POOL-VAR, run BODY, kill pool."
  (declare (indent 1))
  `(let ((,pool-var (nelisp-process-pool-create
                     'test
                     :command nelisp-process-pool-test--cmd
                     :size 2)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (nelisp-process-pool-kill ,pool-var)))))

;;; Create ------------------------------------------------------------

(ert-deftest nelisp-process-pool-create-returns-struct ()
  (nelisp-process-pool-test--with-pool p
    (should (nelisp-process-pool-p p))
    (should (equal "test" (nelisp-process-pool-name p)))
    (should (= 2 (nelisp-process-pool-size p)))
    (should (= 2 (length (nelisp-process-pool-workers p))))))

(ert-deftest nelisp-process-pool-create-lazy-no-workers ()
  (let ((p (nelisp-process-pool-create
            'lazy :command nelisp-process-pool-test--cmd
            :size 3 :prespawn nil)))
    (unwind-protect
        (should (null (nelisp-process-pool-workers p)))
      (ignore-errors (nelisp-process-pool-kill p)))))

(ert-deftest nelisp-process-pool-create-missing-command-signals ()
  (should-error (nelisp-process-pool-create 'bad :size 2)))

;;; Get / Return ------------------------------------------------------

(ert-deftest nelisp-process-pool-get-marks-busy ()
  (nelisp-process-pool-test--with-pool p
    (let ((w (nelisp-process-pool-get p)))
      (should (nelisp-process-pool-worker-p w))
      (should (eq 'busy (nelisp-process-pool-worker-state w)))
      (should (nelisp-process-live-p
               (nelisp-process-pool-worker-process w))))))

(ert-deftest nelisp-process-pool-return-flips-to-idle ()
  (nelisp-process-pool-test--with-pool p
    (let ((w (nelisp-process-pool-get p)))
      (nelisp-process-pool-return p w)
      (should (eq 'idle (nelisp-process-pool-worker-state w))))))

(ert-deftest nelisp-process-pool-get-reuses-returned-idle ()
  (nelisp-process-pool-test--with-pool p
    (let* ((w1 (nelisp-process-pool-get p))
           (_ (nelisp-process-pool-return p w1))
           (w2 (nelisp-process-pool-get p)))
      (should (eq w1 w2)))))

(ert-deftest nelisp-process-pool-get-spawns-when-lazy ()
  (let ((p (nelisp-process-pool-create
            'lazy2 :command nelisp-process-pool-test--cmd
            :size 2 :prespawn nil)))
    (unwind-protect
        (let ((w (nelisp-process-pool-get p)))
          (should w)
          (should (= 1 (length (nelisp-process-pool-workers p)))))
      (ignore-errors (nelisp-process-pool-kill p)))))

(ert-deftest nelisp-process-pool-get-respects-size-cap ()
  (let ((p (nelisp-process-pool-create
            'cap :command nelisp-process-pool-test--cmd
            :size 1)))
    (unwind-protect
        (let ((w1 (nelisp-process-pool-get p))
              (w2 (nelisp-process-pool-get p)))
          (should w1)
          (should (null w2)))
      (ignore-errors (nelisp-process-pool-kill p)))))

(ert-deftest nelisp-process-pool-get-full-all-busy-returns-nil ()
  (nelisp-process-pool-test--with-pool p
    (let ((w1 (nelisp-process-pool-get p))
          (w2 (nelisp-process-pool-get p))
          (w3 (nelisp-process-pool-get p)))
      (should w1)
      (should w2)
      (should (null w3)))))

;;; Kill --------------------------------------------------------------

(ert-deftest nelisp-process-pool-kill-empties-roster ()
  (let ((p (nelisp-process-pool-create
            'k :command nelisp-process-pool-test--cmd :size 3)))
    (should (= 3 (length (nelisp-process-pool-workers p))))
    (nelisp-process-pool-kill p)
    (should (null (nelisp-process-pool-workers p)))))

(ert-deftest nelisp-process-pool-kill-terminates-each-worker ()
  (let* ((p (nelisp-process-pool-create
             'kk :command nelisp-process-pool-test--cmd :size 2))
         (procs (mapcar #'nelisp-process-pool-worker-process
                        (nelisp-process-pool-workers p))))
    (nelisp-process-pool-kill p)
    (dolist (proc procs)
      (nelisp-process-wait-for-exit proc 2.0)
      (should (null (nelisp-process-live-p proc))))))

;;; Stats -------------------------------------------------------------

(ert-deftest nelisp-process-pool-stats-reflects-state ()
  (nelisp-process-pool-test--with-pool p
    (let ((s0 (nelisp-process-pool-stats p)))
      (should (= 2 (plist-get s0 :total)))
      (should (= 2 (plist-get s0 :idle)))
      (should (= 0 (plist-get s0 :busy)))
      (should (= 2 (plist-get s0 :size))))
    (let ((w (nelisp-process-pool-get p)))
      (let ((s1 (nelisp-process-pool-stats p)))
        (should (= 1 (plist-get s1 :busy)))
        (should (= 1 (plist-get s1 :idle))))
      (nelisp-process-pool-return p w)
      (let ((s2 (nelisp-process-pool-stats p)))
        (should (= 0 (plist-get s2 :busy)))
        (should (= 2 (plist-get s2 :idle)))))))

;;; Crash refill ------------------------------------------------------

(ert-deftest nelisp-process-pool-dead-worker-replaced-on-next-get ()
  "When a worker dies (killed externally), the next `get' after its
sentinel fires replaces it with a fresh spawn."
  (nelisp-process-pool-test--with-pool p
    ;; Mark first worker busy + kill its process externally.
    (let* ((target (car (nelisp-process-pool-workers p)))
           (proc (nelisp-process-pool-worker-process target)))
      (nelisp-kill-process proc)
      (nelisp-process-wait-for-exit proc 2.0)
      ;; Sentinel should have flipped it to dead.  Allow one extra
      ;; accept-process-output for the sentinel to run.
      (accept-process-output nil 0.1)
      (should (eq 'dead
                  (nelisp-process-pool-worker-state target))))
    ;; Next get sweeps the dead one + replaces (roster stays at size).
    (let ((w (nelisp-process-pool-get p)))
      (should w)
      (should (not (eq 'dead (nelisp-process-pool-worker-state w))))
      (should (<= (length (nelisp-process-pool-workers p))
                  (nelisp-process-pool-size p))))))

(ert-deftest nelisp-process-pool-quick-alive-p-distinguishes ()
  (nelisp-process-pool-test--with-pool p
    (let ((w (car (nelisp-process-pool-workers p))))
      (should (nelisp-process-pool--quick-alive-p w))
      (nelisp-kill-process (nelisp-process-pool-worker-process w))
      (nelisp-process-wait-for-exit
       (nelisp-process-pool-worker-process w) 2.0)
      (accept-process-output nil 0.1)
      (should (not (nelisp-process-pool--quick-alive-p w))))))

(provide 'nelisp-process-pool-test)
;;; nelisp-process-pool-test.el ends here
