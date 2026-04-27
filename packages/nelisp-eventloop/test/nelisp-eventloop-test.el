;;; nelisp-eventloop-test.el --- Phase 5-B.4 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-B.4 — `src/nelisp-eventloop.el'.
;;
;; Coverage:
;;   - nelisp-event struct (kind / data / timestamp)
;;   - key-sequence normalisation (int / string / other)
;;   - bind / unbind / lookup / reset
;;   - dispatch of key / timer / quit / unknown
;;   - synchronous queue: enqueue / step / drain / run-scripted
;;     (FIFO order, quit mid-stream, flag clear after quit)
;;   - actor-integrated main loop: receive + dispatch + quit
;;   - timer wrapper routes a timer event to a target actor

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-actor)
(require 'nelisp-eventloop)

(defun nelisp-eventloop-test--reset ()
  (nelisp-eventloop-reset-bindings)
  (nelisp-eventloop-drain)
  (setq nelisp-eventloop--running nil)
  (nelisp-actor--reset))

(defmacro nelisp-eventloop-test--fresh (&rest body)
  "Run BODY under a freshly reset event loop + actor registry."
  (declare (indent 0))
  `(progn
     (nelisp-eventloop-test--reset)
     (unwind-protect
         (progn ,@body)
       (nelisp-eventloop-test--reset))))

(ert-deftest nelisp-eventloop-event-struct-basic ()
  (let ((ev (nelisp-make-event 'key ?a 3.14)))
    (should (nelisp-event-p ev))
    (should (eq 'key (nelisp-event-kind ev)))
    (should (= ?a (nelisp-event-data ev)))
    (should (= 3.14 (nelisp-event-timestamp ev)))))

(ert-deftest nelisp-eventloop-event-timestamp-defaulted ()
  (let ((ev (nelisp-make-event 'key ?x)))
    (should (> (nelisp-event-timestamp ev) 0))))

(ert-deftest nelisp-eventloop-key-seq-int ()
  (should (equal "a" (nelisp-eventloop--key-seq ?a)))
  (should (equal "\C-f" (nelisp-eventloop--key-seq 6))))

(ert-deftest nelisp-eventloop-key-seq-string-verbatim ()
  (should (equal "xyz" (nelisp-eventloop--key-seq "xyz"))))

(ert-deftest nelisp-eventloop-bind-and-lookup ()
  (nelisp-eventloop-test--fresh
   (let ((cmd (lambda (_ev) 'hit)))
     (nelisp-eventloop-bind "a" cmd)
     (should (eq cmd (nelisp-eventloop-lookup "a")))
     (should (null (nelisp-eventloop-lookup "z"))))))

(ert-deftest nelisp-eventloop-unbind ()
  (nelisp-eventloop-test--fresh
   (nelisp-eventloop-bind "a" (lambda (_ev) 'hit))
   (nelisp-eventloop-unbind "a")
   (should (null (nelisp-eventloop-lookup "a")))))

(ert-deftest nelisp-eventloop-reset-clears-all ()
  (nelisp-eventloop-test--fresh
   (nelisp-eventloop-bind "a" (lambda (_ev) 'x))
   (nelisp-eventloop-bind "b" (lambda (_ev) 'y))
   (nelisp-eventloop-reset-bindings)
   (should (null (nelisp-eventloop-lookup "a")))
   (should (null (nelisp-eventloop-lookup "b")))))

(ert-deftest nelisp-eventloop-dispatch-key-invokes-cmd ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "a" (lambda (ev) (push (nelisp-event-data ev) seen)))
     (nelisp-eventloop--dispatch (nelisp-make-event 'key ?a))
     (should (equal '(?a) seen)))))

(ert-deftest nelisp-eventloop-dispatch-unbound-key-no-op ()
  (nelisp-eventloop-test--fresh
   (should (null (nelisp-eventloop--dispatch
                  (nelisp-make-event 'key ?z))))))

(ert-deftest nelisp-eventloop-dispatch-timer ()
  (nelisp-eventloop-test--fresh
   (let* ((seen nil)
          (cmd (lambda (_ev) (push 'fired seen))))
     (nelisp-eventloop--dispatch (nelisp-make-event 'timer cmd))
     (should (equal '(fired) seen)))))

(ert-deftest nelisp-eventloop-dispatch-quit-clears-running ()
  (nelisp-eventloop-test--fresh
   (setq nelisp-eventloop--running t)
   (nelisp-eventloop--dispatch (nelisp-make-event 'quit nil))
   (should (null nelisp-eventloop--running))))

(ert-deftest nelisp-eventloop-dispatch-unknown-kind ()
  (nelisp-eventloop-test--fresh
   (should (null (nelisp-eventloop--dispatch
                  (nelisp-make-event 'resize '(80 . 24)))))))

(ert-deftest nelisp-eventloop-enqueue-and-step ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "a" (lambda (ev) (push (nelisp-event-data ev) seen)))
     (nelisp-eventloop-enqueue (nelisp-make-event 'key ?a))
     (nelisp-eventloop-step)
     (should (equal '(?a) seen))
     (should (null nelisp-eventloop--queue)))))

(ert-deftest nelisp-eventloop-drain ()
  (nelisp-eventloop-test--fresh
   (nelisp-eventloop-enqueue (nelisp-make-event 'key ?a))
   (nelisp-eventloop-enqueue (nelisp-make-event 'key ?b))
   (nelisp-eventloop-drain)
   (should (null nelisp-eventloop--queue))))

(ert-deftest nelisp-eventloop-run-scripted-fifo ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "a" (lambda (_ev) (push 'a seen)))
     (nelisp-eventloop-bind "b" (lambda (_ev) (push 'b seen)))
     (nelisp-eventloop-bind "c" (lambda (_ev) (push 'c seen)))
     (nelisp-eventloop-run-scripted
      (list (nelisp-make-event 'key ?a)
            (nelisp-make-event 'key ?b)
            (nelisp-make-event 'key ?c)))
     (should (equal '(a b c) (nreverse seen))))))

(ert-deftest nelisp-eventloop-run-scripted-stops-on-quit ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "a" (lambda (_ev) (push 'a seen)))
     (nelisp-eventloop-bind "b" (lambda (_ev) (push 'b seen)))
     (nelisp-eventloop-run-scripted
      (list (nelisp-make-event 'key ?a)
            (nelisp-make-event 'quit nil)
            (nelisp-make-event 'key ?b)))
     (should (equal '(a) (nreverse seen)))
     (should (null nelisp-eventloop--running)))))

(ert-deftest nelisp-eventloop-main-actor-dispatches-key ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "a" (lambda (_ev) (push 'hit seen)))
     (let ((main (nelisp-eventloop-spawn-main-actor)))
       (nelisp-send main (nelisp-make-event 'key ?a))
       (nelisp-send main (nelisp-make-event 'quit nil))
       (nelisp-actor-run-until-idle)
       (should (equal '(hit) seen))))))

(ert-deftest nelisp-eventloop-main-actor-interleaves-many-keys ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "a" (lambda (_ev) (push 'a seen)))
     (nelisp-eventloop-bind "b" (lambda (_ev) (push 'b seen)))
     (nelisp-eventloop-bind "c" (lambda (_ev) (push 'c seen)))
     (let ((main (nelisp-eventloop-spawn-main-actor)))
       (dolist (ch '(?a ?b ?c))
         (nelisp-send main (nelisp-make-event 'key ch)))
       (nelisp-send main (nelisp-make-event 'quit nil))
       (nelisp-actor-run-until-idle)
       (should (equal '(a b c) (nreverse seen)))))))

(ert-deftest nelisp-eventloop-main-actor-quit-terminates ()
  (nelisp-eventloop-test--fresh
   (let ((main (nelisp-eventloop-spawn-main-actor)))
     (nelisp-send main (nelisp-make-event 'quit nil))
     (nelisp-actor-run-until-idle)
     (should (memq (nelisp-actor-status main) '(:dead :done))))))

(ert-deftest nelisp-eventloop-schedule-timer-routes-to-target ()
  (nelisp-eventloop-test--fresh
   (let ((seen nil))
     (nelisp-eventloop-bind "timer" (lambda (_ev) 'noop))  ; unused
     (let* ((cmd (lambda (_ev) (push 'tick seen)))
            (main (nelisp-eventloop-spawn-main-actor)))
       (nelisp-eventloop-schedule-timer 0 cmd main)
       ;; Wait for the host timer to fire.
       (let ((deadline (+ (float-time) 2.0)))
         (while (and (null seen) (< (float-time) deadline))
           (sit-for 0.02)
           (nelisp-actor-run-until-idle)))
       (nelisp-send main (nelisp-make-event 'quit nil))
       (nelisp-actor-run-until-idle)
       (should (equal '(tick) seen))))))

(provide 'nelisp-eventloop-test)
;;; nelisp-eventloop-test.el ends here
