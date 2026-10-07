;;; selection-send.el --- Outgoing INCR readiness and deadlines -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-gui-frontend)

(defmacro selection-send-with-queue (events now &rest body)
  (declare (indent 2))
  `(let* ((send (vector 40 50 60 65536 'storage 32768 10.0 200.0))
          (nelisp-gui-selection--state [1 2])
          (nelisp-gui-selection--sends (list send))
          (nelisp-gui-selection--receive nil)
          (nelisp-gui-frontend--timing nil)
          (nelisp-gui-frontend--transport-pending nil)
          (nelisp-gui-frontend-maximum-pump-time 1.0)
          (queue ,events) (writes nil) (drops nil))
     (cl-letf (((symbol-function 'float-time) (lambda (&rest _) ,now))
               ((symbol-function 'nl-ffi-libffi-u32)
                (lambda (_ offset) (pcase offset (4 40) (8 50) (12 42))))
               ((symbol-function 'ptr-read-u8) (lambda (&rest _) 1))
               ((symbol-function 'nl-ffi-memory-address) (lambda (_) 1000))
               ((symbol-function 'nelisp-gui-selection--property)
                (lambda (_window _property _type _format count _pointer)
                  (push count writes)))
               ((symbol-function 'nelisp-gui-selection--flush) #'ignore)
               ((symbol-function 'nelisp-gui-selection--send-drop)
                (lambda (value reason)
                  (push reason drops)
                  (setq nelisp-gui-selection--sends (delq value nelisp-gui-selection--sends))))
               ((symbol-function 'nelisp-gui-selection-poll)
                (lambda ()
                  (let ((event (pop queue)))
                    (when event
                      (when (eq event 'ack) (nelisp-gui-selection-event 28 'ack))
                      '(:ignored t))))))
       ,@body)))

(ert-deftest selection-send/queued-ack-beats-idle-expiry ()
  ;; The requestor deleted the property while the owner was descheduled.
  ;; Earlier unrelated events must not make that ready ack expire either.
  (selection-send-with-queue '(unrelated ack) 100.0
    (nelisp-gui-frontend--pump)
    (should (equal writes '(32768)))
    (should (= (aref send 5) 65536))
    (should (= (aref send 6) 103.0))
    (should-not drops)))

(ert-deftest selection-send/bounded-batch-preserves-ready-ack ()
  (selection-send-with-queue (append (make-list 64 'unrelated) '(ack)) 100.0
    (nelisp-gui-frontend--pump)
    (should nelisp-gui-frontend--transport-pending)
    (should-not writes)
    (should-not drops)
    (nelisp-gui-frontend--pump)
    (should (equal writes '(32768)))
    (should-not drops)))

(ert-deftest selection-send/quiet-stalled-requestor-expires ()
  (selection-send-with-queue nil 100.0
    (nelisp-gui-frontend--pump)
    (should (equal drops '(timeout)))
    (should-not writes)))

(ert-deftest selection-send/queued-traffic-cannot-extend-total-cap ()
  (selection-send-with-queue '(ack) 201.0
    (nelisp-gui-frontend--pump)
    (should (equal drops '(timeout)))
    (should-not writes)))

(ert-run-tests-batch-and-exit)
