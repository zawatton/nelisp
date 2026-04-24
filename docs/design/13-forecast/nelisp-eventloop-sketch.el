;;; nelisp-eventloop-sketch.el --- Phase 5-B.4 forecast sketch -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct nelisp-event
  kind         ; 'key / 'timer / 'resize
  data         ; key byte, timer symbol, (W . H)
  (timestamp 0))

(defvar nelisp-eventloop--queue nil
  "Pending events (FIFO).")

(defvar nelisp-eventloop--running nil)

(defvar nelisp-eventloop--bindings (make-hash-table :test 'equal)
  "Key sequence -> command symbol.")

(defun nelisp-eventloop-bind (seq cmd)
  (puthash seq cmd nelisp-eventloop--bindings))

(defun nelisp-eventloop-enqueue (ev)
  (setq nelisp-eventloop--queue (nconc nelisp-eventloop--queue (list ev))))

(defun nelisp-eventloop--read-key ()
  "Blocking read of one byte from stdin — stubbed here."
  (let ((b (read-char-exclusive)))
    (make-nelisp-event :kind 'key :data b)))

(defun nelisp-eventloop--dispatch (ev)
  (pcase (nelisp-event-kind ev)
    ('key
     (let* ((seq (char-to-string (nelisp-event-data ev)))
            (cmd (gethash seq nelisp-eventloop--bindings)))
       (when cmd (funcall cmd))))
    ('timer
     (let ((cmd (nelisp-event-data ev)))
       (when cmd (funcall cmd))))
    ('resize
     nil)))

(defun nelisp-eventloop-step ()
  "Drain one event from the queue."
  (when nelisp-eventloop--queue
    (let ((ev (car nelisp-eventloop--queue)))
      (setq nelisp-eventloop--queue (cdr nelisp-eventloop--queue))
      (nelisp-eventloop--dispatch ev))))

(defun nelisp-eventloop-run ()
  "Run until `nelisp-eventloop--running' goes nil."
  (setq nelisp-eventloop--running t)
  (while nelisp-eventloop--running
    (unless nelisp-eventloop--queue
      (nelisp-eventloop-enqueue (nelisp-eventloop--read-key)))
    (nelisp-eventloop-step)))

(defun nelisp-eventloop-stop ()
  (setq nelisp-eventloop--running nil))

(defun nelisp-eventloop-schedule-timer (seconds cmd)
  "Placeholder — Phase 4 actor tick integration."
  (run-at-time seconds nil
               (lambda ()
                 (nelisp-eventloop-enqueue
                  (make-nelisp-event :kind 'timer :data cmd)))))

(provide 'nelisp-eventloop-sketch)
;;; nelisp-eventloop-sketch.el ends here
