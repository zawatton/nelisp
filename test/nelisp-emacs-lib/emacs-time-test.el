;;; emacs-time-test.el --- Tests for emacs-time  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the time / truncate polyfills split out of `emacs-stub.el'.
;; This file deliberately reloads the module in a controlled way so we
;; can exercise both the host-preserved path and the actual polyfill
;; path without changing the repo sources.

;;; Code:

(require 'ert)
(require 'cl-lib)
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

(defun emacs-time-test--with-standalone-time-primitives (thunk)
  "Install the time shim over standalone-shaped primitives, then call THUNK.
The stand-in `float-time' deliberately ignores its argument, matching the
NeLisp v1.2.0 builtin that the compatibility shim must replace."
  (let ((saved-float-time (symbol-function 'float-time))
        (saved-standalone-float-time
         (and (fboundp 'emacs-time--standalone-float-time)
              (symbol-function 'emacs-time--standalone-float-time))))
    (unwind-protect
        (progn
          (fmakunbound 'emacs-time--standalone-float-time)
          (fset 'float-time (lambda (&optional _time-value) 1000000.0))
          (cl-letf (((symbol-function 'emacs-time--standalone-runtime-p)
                     (lambda () t)))
            (emacs-time--install-float-time))
          (funcall thunk))
      (fset 'float-time saved-float-time)
      (if saved-standalone-float-time
          (fset 'emacs-time--standalone-float-time
                saved-standalone-float-time)
        (fmakunbound 'emacs-time--standalone-float-time)))))

;;;; Load / feature contract

(ert-deftest emacs-time-test/require-loads-cleanly ()
  (should (featurep 'emacs-time))
  (should (fboundp 'float-time))
  (should (fboundp 'current-time))
  (should (fboundp 'truncate))
  (should (fboundp 'set-time-zone-rule))
  (should (fboundp 'display-time-mode)))

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

(ert-deftest emacs-time-test/parse-zone-offset ()
  (should (= 32400 (emacs-time--parse-zone-offset "+0900")))
  (should (= -19800 (emacs-time--parse-zone-offset "-0530")))
  (should-not (emacs-time--parse-zone-offset "JST"))
  (should-not (emacs-time--parse-zone-offset "+09")))

(ert-deftest emacs-time-test/current-time-zone-uses-date-and-caches ()
  (emacs-time-test--with-reloaded-module
   '(current-time-zone emacs-time--current-time-zone-cache)
   (lambda ()
     (let ((calls nil)
           (emacs-time--current-time-zone-cache nil))
       (cl-letf (((symbol-function 'call-process)
                  (lambda (program _infile destination _display &rest args)
                    (should (equal "date" program))
                    (should destination)
                    (let ((format (car args)))
                      (setq calls (cons format calls))
                      (insert (if (equal format "+%z")
                                  "+0900\n"
                                "JST\n"))
                      0))))
         (should (equal '(32400 "JST") (current-time-zone)))
         (should (equal '(32400 "JST") (current-time-zone)))
         (should (equal '("+%z" "+%Z") (nreverse calls))))))))

(ert-deftest emacs-time-test/current-time-zone-falls-back-to-utc ()
  (emacs-time-test--with-reloaded-module
   '(current-time-zone emacs-time--current-time-zone-cache)
   (lambda ()
     (let ((emacs-time--current-time-zone-cache nil))
       (cl-letf (((symbol-function 'call-process)
                  (lambda (&rest _args) 1)))
         (should (equal '(0 "UTC") (current-time-zone))))))))

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

(ert-deftest emacs-time-test/display-time-mode-and-time-zone-rule-stubs ()
  (emacs-time-test--with-reloaded-module
   '(display-time-mode set-time-zone-rule)
   (lambda ()
     (let ((display-time-mode nil)
           (emacs-time--time-zone-rule nil)
           (emacs-time--current-time-zone-cache '(1 "X")))
       (should-not (display-time-mode 1))
       (should (equal "UTC0" (set-time-zone-rule "UTC0")))
       (should (equal "UTC0" emacs-time--time-zone-rule))
       (should-not emacs-time--current-time-zone-cache)))))

(ert-deftest emacs-time-test/calendar-absolute-days-match-emacs ()
  "Calendar helpers use Emacs absolute days, not Unix-epoch day offsets."
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time
              time-to-days days-to-time encode-time decode-time
              calendar-absolute-from-gregorian
              calendar-gregorian-from-absolute)
   (lambda ()
     (should (= 719163 (calendar-absolute-from-gregorian '(1 1 1970))))
     (should (= 739807 (calendar-absolute-from-gregorian '(7 10 2026))))
     (should (equal '(7 10 2026)
                    (calendar-gregorian-from-absolute 739807)))
     (should (= 739807 (time-to-days (encode-time 0 0 0 10 7 2026))))
     (should (equal '(0 0 0 10 7 2026 5 nil 0)
                    (decode-time (days-to-time 739807)))))))

(ert-deftest emacs-time-test/to-number-converts-time-forms ()
  "emacs-time--to-number converts all Emacs time-value shapes to seconds."
  (should (= 100 (emacs-time--to-number 100)))
  (should (= 1.5 (emacs-time--to-number 1.5)))
  (should (= 65536.0 (emacs-time--to-number '(1 0))))
  (should (= 5.0 (emacs-time--to-number '(5000 . 1000))))
  (should (= 65536.5 (emacs-time--to-number '(1 0 500000))))
  (should (= 1783641600 (emacs-time--to-number '(1783641600 0 0 0)))))

(ert-deftest emacs-time-test/time-convert-polyfill ()
  "time-convert covers integer, float, list, and tick-rate forms."
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time time-convert)
   (lambda ()
     (put 'time-convert 'emacs-stub-bulk t)
     (load emacs-time-test--module-file t t)
     (should (= 12 (time-convert 12.9 'integer)))
     (should (= 12.5 (time-convert 12.5 'float)))
     (should (equal '(12 0 0 0) (time-convert 12.5 'list)))
     (should (equal '(5 . 2) (time-convert 2.5 2)))
     (should (equal '(12000 . 1000) (time-convert 12 1000))))))

(ert-deftest emacs-time-test/time-less-p-orders-time-values ()
  "time-less-p orders integers, floats, (HIGH LOW) and (TICKS . HZ) values."
  (should (time-less-p 100 200))
  (should-not (time-less-p 200 100))
  (should-not (time-less-p 1.5 1.4))
  (should (time-less-p '(1 0) '(2 0)))
  (should (time-less-p '(5000 . 1000) '(6000 . 1000)))
  (should (time-less-p 100 '(1 0))))

(ert-deftest emacs-time-test/standalone-subtract-current-times-is-small ()
  "Converting a difference must not discard it and reread wall-clock time."
  (emacs-time-test--with-standalone-time-primitives
   (lambda ()
     (let ((elapsed
            (float-time (time-subtract (current-time) (current-time)))))
       (should (< (abs elapsed) 1.0))))))

(ert-deftest emacs-time-test/standalone-add-then-subtract-is-five-seconds ()
  "Adding five seconds and subtracting now yields approximately five."
  (emacs-time-test--with-standalone-time-primitives
   (lambda ()
     (let ((elapsed
            (time-subtract (time-add (current-time) 5) (current-time))))
       (should (< (abs (- (emacs-time--to-number elapsed) 5.0)) 0.01))))))

(ert-deftest emacs-time-test/standalone-time-less-p-orders-current-time ()
  "Current time is earlier than current time plus one second."
  (emacs-time-test--with-standalone-time-primitives
   (lambda ()
     (should (time-less-p (current-time) (time-add (current-time) 1))))))

(ert-deftest emacs-time-test/standalone-time-since-current-time-is-small ()
  "Converting `time-since' must preserve its near-zero difference."
  (emacs-time-test--with-standalone-time-primitives
   (lambda ()
     (let ((elapsed (float-time (time-since (current-time)))))
       (should (>= elapsed 0.0))
       (should (< elapsed 1.0))))))

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

(provide 'emacs-time-test)

;;; emacs-time-test.el ends here
