;;; nelisp-bt-standalone-smoke.el --- end-to-end nl-bt API smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S8.4: one pass over the whole nl-bt API on the NeLisp
;; standalone runtime (and, unchanged, on host Emacs), run from the
;; repository root:
;;   nelisp --load packages/nelisp-bt/test/nelisp-bt-standalone-smoke.el
;; Prints one line per check, then NL-BT-PASS when all of them hold.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(define-error 'nl-bt-e2e-error "nl-bt e2e error")
(defvar nl-bt-e2e-global 0)

(defvar nl-bt-e2e--failures 0)

(defun nl-bt-e2e--check (name ok)
  (princ (format "  %-28s %s\n" name (if ok "ok" "FAIL")))
  (unless ok (setq nl-bt-e2e--failures (1+ nl-bt-e2e--failures))))

(defun nl-bt-e2e--wait (pred secs)
  (let ((deadline (+ (float-time) secs)) (v nil))
    (while (and (not (setq v (funcall pred))) (< (float-time) deadline))
      (nl-bt-thread-yield))
    v))

(princ (format "nl-bt e2e on %s backend\n" nl-bt-backend))

;; Threads: values, errors, identity, bindings
(let ((x 20))
  (nl-bt-e2e--check "value"
                    (equal (nl-bt-join-thread
                            (nl-bt-make-thread (lambda () (list (* x 2) "s" [v]))))
                           '(40 "s" [v]))))
(nl-bt-e2e--check "error re-signalled"
                  (equal (condition-case e
                             (nl-bt-join-thread
                              (nl-bt-make-thread
                               (lambda () (signal 'nl-bt-e2e-error '(7)))))
                           (nl-bt-e2e-error e))
                         '(nl-bt-e2e-error 7)))
(let ((th (nl-bt-make-thread (lambda () (nl-bt-current-thread)) "named")))
  (nl-bt-e2e--check "current-thread / name"
                    (and (eq (nl-bt-join-thread th) th)
                         (equal (nl-bt-thread-name th) "named")
                         (not (nl-bt-thread-alive-p th)))))
(nl-bt-e2e--check "initial bindings"
                  (eq (nl-bt-join-thread
                       (nl-bt-make-thread (lambda () nl-bt-e2e-global) nil
                                          '((nl-bt-e2e-global . 'bound))))
                      'bound))

;; Locks
(let* ((lock (nl-bt-make-lock)) (c (list 0))
       (ths (mapcar (lambda (_)
                      (nl-bt-make-thread
                       (lambda () (dotimes (_ 500)
                                    (nl-bt-with-lock-held lock
                                      (setcar c (1+ (car c))))))))
                    '(1 2 3 4))))
  (mapc #'nl-bt-join-thread ths)
  (nl-bt-e2e--check "lock 4x500" (= (car c) 2000)))
(let ((r (nl-bt-make-recursive-lock)) (plain (nl-bt-make-lock)))
  (nl-bt-e2e--check "recursive lock"
                    (eq (nl-bt-with-recursive-lock-held r
                          (nl-bt-with-recursive-lock-held r 'in))
                        'in))
  (nl-bt-e2e--check "non-recursive refuses"
                    (nl-bt-with-lock-held plain
                      (condition-case nil (progn (nl-bt-acquire-lock plain) nil)
                        (nl-bt-error t)))))

;; Condition variable, semaphore, atomics
(let* ((lock (nl-bt-make-lock)) (cv (nl-bt-make-condition-variable))
       (box (list nil))
       (waiter (nl-bt-make-thread
                (lambda ()
                  (nl-bt-with-lock-held lock
                    (while (null (car box)) (nl-bt-condition-wait cv lock))
                    (car box))))))
  (nl-bt-e2e--wait (lambda () nil) 0.1)
  (nl-bt-with-lock-held lock (setcar box 'woken) (nl-bt-condition-broadcast cv))
  (nl-bt-e2e--check "condition variable" (eq (nl-bt-join-thread waiter) 'woken)))
(let ((sem (nl-bt-make-semaphore "s" 1)))
  (nl-bt-e2e--check "semaphore"
                    (and (nl-bt-wait-on-semaphore sem)
                         (not (nl-bt-wait-on-semaphore sem 0.05))
                         (progn (nl-bt-signal-semaphore sem) (= 1 (nl-bt-semaphore-count sem))))))
(let* ((a (nl-bt-make-atomic-integer 0))
       (ths (mapcar (lambda (_) (nl-bt-make-thread
                                 (lambda () (dotimes (_ 300) (nl-bt-atomic-integer-incf a)))))
                    '(1 2 3 4))))
  (mapc #'nl-bt-join-thread ths)
  (nl-bt-e2e--check "atomic integer" (= (nl-bt-atomic-integer-value a) 1200)))

;; Globals and interning from threads
(nl-bt-join-thread (nl-bt-make-thread (lambda ()
                                        (setq nl-bt-e2e-global 42)
                                        (defalias 'nl-bt-e2e-fn (lambda () 'from-thread))
                                        (intern "nl-bt-e2e-fresh-symbol"))))
(nl-bt-e2e--check "global writes / intern"
                  (and (= nl-bt-e2e-global 42)
                       (eq (funcall 'nl-bt-e2e-fn) 'from-thread)
                       (intern-soft "nl-bt-e2e-fresh-symbol")))

;; GC requested from a thread
(nl-bt-e2e--check "garbage-collect in thread"
                  (eq (nl-bt-join-thread
                       (nl-bt-make-thread (lambda () (garbage-collect) 'ok)))
                      'ok))

;; Interrupt and destroy
(let* ((stop (list nil)) (seen (list nil))
       (th (nl-bt-make-thread (lambda () (while (not (car stop)) (nl-bt-thread-yield)) 'done))))
  (nl-bt-interrupt-thread th (lambda () (setcar seen (nl-bt-current-thread))))
  (nl-bt-e2e--wait (lambda () (car seen)) 10)
  (setcar stop t)
  (nl-bt-e2e--check "interrupt"
                    (and (eq (car seen) th) (eq (nl-bt-join-thread th) 'done))))
(let* ((started (list nil)) (cleaned (list nil))
       (th (nl-bt-make-thread
            (lambda ()
              (unwind-protect
                  (progn (setcar started t) (while t (nl-bt-thread-yield)))
                (setcar cleaned t))))))
  (nl-bt-e2e--wait (lambda () (car started)) 10)
  (nl-bt-destroy-thread th)
  (nl-bt-e2e--check "destroy"
                    (and (null (nl-bt-join-thread th)) (car cleaned)
                         (nl-bt-thread-destroyed-p th))))

(princ (if (= nl-bt-e2e--failures 0) "NL-BT-PASS\n"
         (format "NL-BT-FAIL %d\n" nl-bt-e2e--failures)))

;;; nelisp-bt-standalone-smoke.el ends here
