;;; nelisp-bt-test.el --- ERT for nelisp-bt on host Emacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S8.1.  Runs the nl-bt API on its emacs backend (Emacs threads),
;; the same contract the native backend's standalone smokes check.

;;; Code:

(require 'ert)
(require 'nelisp-bt)

(define-error 'nelisp-bt-test-error "nelisp-bt test error")

(ert-deftest nelisp-bt-test-backend-is-emacs-on-host ()
  (should (eq nl-bt-backend 'emacs)))

(ert-deftest nelisp-bt-test-join-returns-value ()
  (let ((x 40))
    (should (equal (nl-bt-join-thread
                    (nl-bt-make-thread (lambda () (list (+ x 2) "two"))))
                   '(42 "two")))))

(ert-deftest nelisp-bt-test-join-resignals-error ()
  (let ((th (nl-bt-make-thread
             (lambda () (signal 'nelisp-bt-test-error (list 1 2))))))
    (should (equal (condition-case e (nl-bt-join-thread th)
                     (nelisp-bt-test-error e))
                   '(nelisp-bt-test-error 1 2)))))

(defvar nelisp-bt-test--special 'global)

(ert-deftest nelisp-bt-test-new-thread-sees-global-not-creator-binding ()
  (let ((nelisp-bt-test--special 'creator))
    (should (eq (nl-bt-join-thread
                 (nl-bt-make-thread (lambda () nelisp-bt-test--special)))
                'global))))

(ert-deftest nelisp-bt-test-initial-bindings ()
  (should (eq (nl-bt-join-thread
               (nl-bt-make-thread (lambda () nelisp-bt-test--special)
                                  "bound" '((nelisp-bt-test--special . 'given))))
              'given)))

(ert-deftest nelisp-bt-test-thread-identity ()
  (let* ((th (nl-bt-make-thread (lambda () (nl-bt-current-thread)) "named")))
    (should (nl-bt-threadp th))
    (should (equal (nl-bt-thread-name th) "named"))
    (should (eq (nl-bt-join-thread th) th))
    (should-not (nl-bt-thread-alive-p th))
    (should (nl-bt-thread-alive-p (nl-bt-current-thread)))
    (should (memq (nl-bt-current-thread) (nl-bt-all-threads)))))

(ert-deftest nelisp-bt-test-join-self-is-an-error ()
  (should-error (nl-bt-join-thread (nl-bt-current-thread)) :type 'nl-bt-error))

(ert-deftest nelisp-bt-test-lock-counter ()
  (let* ((lock (nl-bt-make-lock "counter"))
         (n (list 0))
         (threads
          (mapcar (lambda (_)
                    (nl-bt-make-thread
                     (lambda ()
                       (dotimes (_ 200)
                         (nl-bt-with-lock-held lock
                           (let ((v (car n)))
                             (nl-bt-thread-yield)
                             (setcar n (1+ v))))))))
                  '(1 2 3 4))))
    (mapc #'nl-bt-join-thread threads)
    (should (= (car n) 800))))

(ert-deftest nelisp-bt-test-lock-is-not-recursive ()
  (let ((lock (nl-bt-make-lock)))
    (nl-bt-with-lock-held lock
      (should-error (nl-bt-acquire-lock lock) :type 'nl-bt-error))))

(ert-deftest nelisp-bt-test-recursive-lock ()
  (let ((lock (nl-bt-make-recursive-lock)))
    (should (equal (nl-bt-with-recursive-lock-held lock
                     (nl-bt-with-recursive-lock-held lock 'inner))
                   'inner))
    ;; Fully released: another thread can take it without waiting.
    (should (nl-bt-join-thread
             (nl-bt-make-thread (lambda () (nl-bt-acquire-lock lock :no-wait)))))))

(ert-deftest nelisp-bt-test-release-unheld-lock-is-an-error ()
  (should-error (nl-bt-release-lock (nl-bt-make-lock)) :type 'nl-bt-error))

(ert-deftest nelisp-bt-test-lock-no-wait-and-timeout ()
  (let* ((lock (nl-bt-make-lock))
         (holder (nl-bt-make-thread
                  (lambda ()
                    (nl-bt-with-lock-held lock (sleep-for 0.3)) t))))
    (sleep-for 0.05)
    (should-not (nl-bt-acquire-lock lock :no-wait))
    (should-not (nl-bt-acquire-lock lock nil 0.05))
    (nl-bt-join-thread holder)
    (should (nl-bt-acquire-lock lock :no-wait))
    (nl-bt-release-lock lock)))

(ert-deftest nelisp-bt-test-condition-variable-producer-consumer ()
  (let* ((lock (nl-bt-make-lock))
         (cv (nl-bt-make-condition-variable))
         (queue (list nil))
         (consumer
          (nl-bt-make-thread
           (lambda ()
             (let ((sum 0))
               (dotimes (_ 50)
                 (nl-bt-with-lock-held lock
                   (while (null (car queue))
                     (nl-bt-condition-wait cv lock))
                   (setq sum (+ sum (pop (car queue))))))
               sum)))))
    (dotimes (i 50)
      (nl-bt-with-lock-held lock
        (push (1+ i) (car queue))
        (nl-bt-condition-notify cv))
      (nl-bt-thread-yield))
    (should (= (nl-bt-join-thread consumer) 1275))))

(ert-deftest nelisp-bt-test-condition-wait-timeout ()
  (let ((lock (nl-bt-make-lock))
        (cv (nl-bt-make-condition-variable)))
    (nl-bt-with-lock-held lock
      (should-not (nl-bt-condition-wait cv lock 0.05))
      ;; The lock is held again after the wait.
      (should-error (nl-bt-acquire-lock lock) :type 'nl-bt-error))))

(ert-deftest nelisp-bt-test-semaphore ()
  (let* ((sem (nl-bt-make-semaphore "jobs" 0))
         (takers (mapcar (lambda (_)
                           (nl-bt-make-thread
                            (lambda ()
                              (let ((n 0))
                                (dotimes (_ 10)
                                  (when (nl-bt-wait-on-semaphore sem) (setq n (1+ n))))
                                n))))
                         '(1 2 3))))
    (dotimes (_ 30) (nl-bt-signal-semaphore sem))
    (should (= (apply #'+ (mapcar #'nl-bt-join-thread takers)) 30))
    (should (= (nl-bt-semaphore-count sem) 0))
    (should-not (nl-bt-wait-on-semaphore sem 0.05))
    (nl-bt-signal-semaphore sem 2)
    (should (= (nl-bt-semaphore-count sem) 2))))

(ert-deftest nelisp-bt-test-atomic-integer ()
  (let ((a (nl-bt-make-atomic-integer 5)))
    (should (= (nl-bt-atomic-integer-incf a) 6))
    (should (= (nl-bt-atomic-integer-incf a 4) 10))
    (should (nl-bt-atomic-integer-cas a 10 1))
    (should-not (nl-bt-atomic-integer-cas a 10 2))
    (should (= (nl-bt-atomic-integer-value a) 1))))

;;; nelisp-bt-test.el ends here
