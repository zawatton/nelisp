;;; emacs-time-test.el --- Tests for emacs-time  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the time / truncate polyfills split out of `emacs-stub.el'.
;; This file deliberately reloads the module in a controlled way so we
;; can exercise both the host-preserved path and the actual polyfill
;; path without changing the repo sources.

;;; Code:

(require 'ert)
(require 'emacs-time)

(defconst emacs-time-test--module-file
  (expand-file-name "../src/emacs-time.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-time-test--with-reloaded-module (symbols thunk)
  "Reload `emacs-time' with SYMBOLS temporarily unbound, then call THUNK."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym symbols)
            (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)
            (fmakunbound sym))
          (load emacs-time-test--module-file t t)
          (funcall thunk))
      (dolist (cell saved)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell)))))))

;;;; Load / feature contract

(ert-deftest emacs-time-test/require-loads-cleanly ()
  (should (featurep 'emacs-time))
  (should (fboundp 'float-time))
  (should (fboundp 'current-time))
  (should (fboundp 'truncate)))

(ert-deftest emacs-time-test/guard-keeps-an-already-correct-truncate ()
  (let ((original (symbol-function 'truncate)))
    (emacs-time-test--with-reloaded-module
     nil
     (lambda ()
       (should (eq original (symbol-function 'truncate)))
       (should (= 3 (truncate 3.7)))))))

(ert-deftest emacs-time-test/guard-replaces-marked-bulk-truncate-without-calling-it ()
  (let ((original (and (fboundp 'truncate) (symbol-function 'truncate)))
        (original-marker (get 'truncate 'emacs-stub-bulk))
        (called nil))
    (unwind-protect
        (progn
          (fset 'truncate
                (lambda (&rest _)
                  (setq called t)
                  (error "bulk truncate stub should not be called")))
          (put 'truncate 'emacs-stub-bulk t)
          (load emacs-time-test--module-file t t)
          (should-not called)
          (should (= 3 (truncate 3.7)))
          (should-not (get 'truncate 'emacs-stub-bulk)))
      (if original
          (fset 'truncate original)
        (fmakunbound 'truncate))
      (put 'truncate 'emacs-stub-bulk original-marker))))

;;;; Polyfill path

(ert-deftest emacs-time-test/float-time-defaults-to-zero ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time nelisp--syscall)
   (lambda ()
     (should (numberp (float-time)))
     (should (= 0 (float-time))))))

(ert-deftest emacs-time-test/float-time-uses-nl-current-unix-time-when-available ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (fset 'nl-current-unix-time (lambda () 1234))
     (should (= 1234 (float-time))))))

(ert-deftest emacs-time-test/float-time-can-use-nelisp-syscall-time ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time nelisp--syscall)
   (lambda ()
     (fset 'nelisp--syscall
           (lambda (nr ptr)
             (and (= nr 201) (= ptr 0) 5678)))
     (should (= 5678 (float-time))))))

(ert-deftest emacs-time-test/current-time-is-a-four-element-list ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time nelisp--syscall)
   (lambda ()
     (let ((now (current-time)))
       (should (listp now))
       (should (= 4 (length now)))
       (should (= (float-time) (car now)))
       (should (equal now (list (float-time) 0 0 0)))))))

(ert-deftest emacs-time-test/truncate-integer-and-positive-float ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (should (= 4 (truncate 4)))
     (should (= 3 (truncate 3.7))))))

(ert-deftest emacs-time-test/truncate-negative-float ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (should (= -3 (truncate -3.7))))))

(ert-deftest emacs-time-test/truncate-divisor-and-nil ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (should (= 3 (truncate 7 2)))
     (should (= 0 (truncate nil))))))

(provide 'emacs-time-test)

;;; emacs-time-test.el ends here

(ert-deftest emacs-time-test/to-number-converts-time-forms ()
  "emacs-time--to-number converts all Emacs time-value shapes to seconds."
  (should (= 100 (emacs-time--to-number 100)))
  (should (= 1.5 (emacs-time--to-number 1.5)))
  (should (= 65536.0 (emacs-time--to-number '(1 0))))
  (should (= 5.0 (emacs-time--to-number '(5000 . 1000))))
  (should (= 65536.5 (emacs-time--to-number '(1 0 500000)))))

(ert-deftest emacs-time-test/time-less-p-orders-time-values ()
  "time-less-p orders integers, floats, (HIGH LOW) and (TICKS . HZ) values."
  (should (time-less-p 100 200))
  (should-not (time-less-p 200 100))
  (should-not (time-less-p 1.5 1.4))
  (should (time-less-p '(1 0) '(2 0)))
  (should (time-less-p '(5000 . 1000) '(6000 . 1000)))
  (should (time-less-p 100 '(1 0))))

(ert-deftest emacs-time-test/timers ()
  "run-with-timer / run-with-idle-timer / cancel-timer fire and cancel (B2)."
  (let ((timer-list nil) (timer-idle-list nil) (fired nil))
    ;; a due (past) timer fires once
    (emacs-timer-run-with-timer -1 nil (lambda () (setq fired 'a)))
    (should (= 1 (emacs-timer-run-pending)))
    (should (eq fired 'a))
    ;; a future timer does not fire
    (setq fired nil)
    (emacs-timer-run-with-timer 1000 nil (lambda () (setq fired 'b)))
    (should (= 0 (emacs-timer-run-pending)))
    (should-not fired)
    ;; idle timer fires once idle time reaches its delay
    (emacs-timer-run-with-idle-timer 5 nil (lambda () (setq fired 'idle)))
    (should (= 0 (emacs-timer-run-idle 2)))
    (should-not fired)
    (should (= 1 (emacs-timer-run-idle 6)))
    (should (eq fired 'idle))
    ;; cancel removes a timer from the list
    (let ((tm (emacs-timer-run-with-timer 1000 nil #'ignore)))
      (should (memq tm timer-list))
      (emacs-timer-cancel tm)
      (should-not (memq tm timer-list)))
    (should (emacs-timer-p (emacs-timer-run-with-timer 1 nil #'ignore)))))
