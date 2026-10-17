;;; nelisp-bt-sync-smoke.el --- Doc 214 S4 locks, condition variables, semaphores -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S4 acceptance, run from the repository root:
;;   nelisp --load tools/nelisp-bt-sync-smoke.el
;; - BT-LOCK-PASS 80000: 8 threads each add 1 to a shared counter 10000
;;   times under one nl-bt lock.  The same job without the lock is also
;;   run and its (usually smaller) total printed, to show the lock is what
;;   makes the count exact.
;; - BT-CONDVAR-PASS: a producer pushes 1..1000 onto a locked queue and
;;   notifies; a consumer waits on the condition variable while the queue
;;   is empty and sums what it pops (500500).
;; - BT-SEM-PASS: 4 threads each take a semaphore 250 times while the main
;;   thread signals it 1000 times; a wait on an empty semaphore with a
;;   0.05 s timeout returns nil.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(princ (format "BT-SYNC backend %S\n" nl-bt-backend))

;; Locks
(let* ((lock (nl-bt-make-lock "counter"))
       (locked (list 0))
       (unlocked (list 0))
       (workers nil))
  (dotimes (_ 8)
    (push (nl-bt-make-thread
           (lambda ()
             (let ((i 0))
               (while (< i 10000)
                 (nl-bt-with-lock-held lock
                   (setcar locked (1+ (car locked))))
                 (setq i (1+ i))))))
          workers))
  (dolist (w workers) (nl-bt-join-thread w))
  (setq workers nil)
  (dotimes (_ 8)
    (push (nl-bt-make-thread
           (lambda ()
             (let ((i 0))
               (while (< i 10000)
                 (setcar unlocked (1+ (car unlocked)))
                 (setq i (1+ i))))))
          workers))
  (dolist (w workers) (nl-bt-join-thread w))
  (princ (format "BT-LOCK locked=%d unlocked=%d\n" (car locked) (car unlocked)))
  (princ (if (= (car locked) 80000) "BT-LOCK-PASS 80000\n" "BT-LOCK-FAIL\n")))

;; Condition variable: producer/consumer
(let* ((lock (nl-bt-make-lock "queue"))
       (cv (nl-bt-make-condition-variable "queue-nonempty"))
       (queue (list nil))
       (consumer
        (nl-bt-make-thread
         (lambda ()
           (let ((got 0) (sum 0))
             (while (< got 1000)
               (nl-bt-with-lock-held lock
                 (while (null (car queue))
                   (nl-bt-condition-wait cv lock))
                 (setq sum (+ sum (car (car queue))))
                 (setcar queue (cdr (car queue)))
                 (setq got (1+ got))))
             sum))))
       (producer
        (nl-bt-make-thread
         (lambda ()
           (let ((i 1))
             (while (<= i 1000)
               (nl-bt-with-lock-held lock
                 (setcar queue (cons i (car queue)))
                 (nl-bt-condition-notify cv))
               (setq i (1+ i))))
           t))))
  (nl-bt-join-thread producer)
  (let ((sum (nl-bt-join-thread consumer)))
    (princ (format "BT-CONDVAR sum=%S\n" sum))
    (princ (if (equal sum 500500) "BT-CONDVAR-PASS\n" "BT-CONDVAR-FAIL\n"))))

;; Semaphore
(let* ((sem (nl-bt-make-semaphore "jobs"))
       (takers nil))
  (dotimes (_ 4)
    (push (nl-bt-make-thread
           (lambda ()
             (let ((n 0))
               (while (< n 250)
                 (when (nl-bt-wait-on-semaphore sem)
                   (setq n (1+ n))))
               n)))
          takers))
  (dotimes (_ 1000) (nl-bt-signal-semaphore sem))
  (let ((total 0))
    (dolist (th takers) (setq total (+ total (nl-bt-join-thread th))))
    (let ((timed-out (null (nl-bt-wait-on-semaphore sem 0.05))))
      (princ (format "BT-SEM total=%d left=%d timeout-nil=%S\n"
                     total (nl-bt-semaphore-count sem) timed-out))
      (princ (if (and (= total 1000) (= 0 (nl-bt-semaphore-count sem)) timed-out)
                 "BT-SEM-PASS\n" "BT-SEM-FAIL\n")))))

;;; nelisp-bt-sync-smoke.el ends here
