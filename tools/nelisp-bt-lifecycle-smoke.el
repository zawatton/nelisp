;;; nelisp-bt-lifecycle-smoke.el --- Doc 214 S7 thread lifecycle, interrupt, destroy -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S7 acceptance, run from the repository root:
;;   nelisp --load tools/nelisp-bt-lifecycle-smoke.el
;; - BT-LIFE-PASS: names, `nl-bt-threadp', `nl-bt-current-thread' inside a
;;   thread is that thread's object, `nl-bt-all-threads' lists the main
;;   thread and every live thread, and `nl-bt-thread-alive-p' turns nil
;;   once a thread has been joined.
;; - BT-INTERRUPT-PASS:
;;     * `nl-bt-interrupt-thread' runs a function inside a busy-looping
;;       worker; the function sees itself on that worker, and the loop
;;       carries on afterwards;
;;     * a worker interrupts the main thread while it waits in a join;
;;     * `nl-bt-destroy-thread' ends a looping worker, running its
;;       `unwind-protect' cleanup; joining it returns nil, and
;;       `nl-bt-thread-destroyed-p' is t.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(defun nl-bt-life--wait-for (pred secs)
  "Spin until PRED returns non-nil or SECS pass; return PRED's value."
  (let ((deadline (+ (float-time) secs)) (v nil))
    (while (and (not (setq v (funcall pred))) (< (float-time) deadline))
      (nl-bt-thread-yield))
    v))

;; Lifecycle
(let* ((gate (nl-bt-make-semaphore "gate"))
       (a (nl-bt-make-thread (lambda () (nl-bt-wait-on-semaphore gate)
                                (nl-bt-current-thread))
                             "worker-a"))
       (b (nl-bt-make-thread (lambda () (nl-bt-wait-on-semaphore gate) 'b)
                             "worker-b"))
       (live (nl-bt-all-threads))
       (alive-before (and (nl-bt-thread-alive-p a) (nl-bt-thread-alive-p b))))
  (nl-bt-signal-semaphore gate 2)
  (let* ((self-a (nl-bt-join-thread a))
         (vb (nl-bt-join-thread b))
         (ok (and (nl-bt-threadp a) (not (nl-bt-threadp 'x))
                  (equal (nl-bt-thread-name a) "worker-a")
                  (eq self-a a) (eq vb 'b)
                  (memq (nl-bt-current-thread) live)
                  (memq a live) (memq b live)
                  alive-before
                  (not (nl-bt-thread-alive-p a))
                  (not (memq a (nl-bt-all-threads)))
                  (nl-bt-thread-alive-p (nl-bt-current-thread)))))
    (princ (format "BT-LIFE ok=%S\n" (and ok t)))
    (princ (if ok "BT-LIFE-PASS\n" "BT-LIFE-FAIL\n"))))

;; Interrupt and destroy
(let* ((n (list 0))
       (stop (list nil))
       (seen (list nil))
       (cleaned (list nil))
       (looper (nl-bt-make-thread
                (lambda ()
                  (while (not (car stop)) (setcar n (1+ (car n))))
                  (car n))
                "looper"))
       (main (nl-bt-current-thread))
       (main-seen (list nil)))
  ;; 1. interrupt a busy worker
  (nl-bt-life--wait-for (lambda () (> (car n) 10)) 10)
  (nl-bt-interrupt-thread looper (lambda (tag) (setcar seen (list tag (nl-bt-current-thread)
                                                                  (car n))))
                          'hello)
  (nl-bt-life--wait-for (lambda () (car seen)) 10)
  (let ((before (car n)))
    (nl-bt-life--wait-for (lambda () (> (car n) (+ before 10))) 10))
  (setcar stop t)
  (let* ((final (nl-bt-join-thread looper))
         (ok1 (and (eq (nth 0 (car seen)) 'hello)
                   (eq (nth 1 (car seen)) looper)
                   (> final (nth 2 (car seen))))))
    ;; 2. a worker interrupts the main thread while it joins
    (let ((pinger (nl-bt-make-thread
                   (lambda ()
                     (nl-bt-interrupt-thread
                      main (lambda () (setcar main-seen (nl-bt-current-thread))))
                     (nl-bt-life--wait-for (lambda () (car main-seen)) 10)
                     t))))
      (nl-bt-join-thread pinger))
    (let ((ok2 (eq (car main-seen) main)))
      ;; 3. destroy a looping worker
      (let* ((started (list nil))
             (victim (nl-bt-make-thread
                      (lambda ()
                        (unwind-protect
                            (let ((k 0))
                              (setcar started t)
                              (while t (setq k (1+ k))))
                          (setcar cleaned t)))
                      "victim")))
        ;; Destroy only once the cleanup is armed.
        (nl-bt-life--wait-for (lambda () (car started)) 10)
        (nl-bt-destroy-thread victim)
        (let* ((t0 (float-time))
               (v (nl-bt-join-thread victim))
               (ok3 (and (null v) (car cleaned)
                         (nl-bt-thread-destroyed-p victim)
                         (not (nl-bt-thread-alive-p victim))
                         (< (- (float-time) t0) 10))))
          (princ (format "BT-INTERRUPT busy=%S main=%S destroy=%S\n" ok1 ok2 ok3))
          (princ (if (and ok1 ok2 ok3) "BT-INTERRUPT-PASS\n" "BT-INTERRUPT-FAIL\n")))))))

;;; nelisp-bt-lifecycle-smoke.el ends here
