;;; integ-wh-maxmin-speed.el --- ns per call of max and min (minimal native surface: before/after figure)  -*- lexical-binding: t; -*-

;; Prints one MAXMIN-SPEED line per form: nanoseconds per iteration of an interpreted loop over the form, and the same above the
;; control loop (an `eq' call), best of five runs of 100,000 iterations.  Run it pinned to one CPU on a quiet machine, on the
;; binary before and after the change (tools/ai/integ-wh-progress.org S2.34 holds both).

(defvar iwh-ms-sink nil)
(defun iwh-ms-time (thunk n)
  (let ((best nil) (r 0))
    (while (< r 5)
      (let ((t0 (float-time)))
        (funcall thunk n)
        (let ((dt (- (float-time) t0))) (when (or (null best) (< dt best)) (setq best dt))))
      (setq r (1+ r)))
    (/ (* best 1e9) n)))
(defmacro iwh-ms-loop (expr)
  `(lambda (n) (let ((i 0) (a 3) (b 9) (c 4) (d 7) (l '(3 9 4 7))) (while (< i n) (setq iwh-ms-sink ,expr) (setq i (1+ i))))))
(let* ((control (iwh-ms-time (iwh-ms-loop (eq i 7)) 100000)))
  (dolist (row (list (cons "(max i 7)" (iwh-ms-loop (max i 7)))
                     (cons "(min i 7)" (iwh-ms-loop (min i 7)))
                     (cons "(max a b c d)" (iwh-ms-loop (max a b c d)))
                     (cons "(min a b c d)" (iwh-ms-loop (min a b c d)))
                     (cons "(max 3.5 i)" (iwh-ms-loop (max 3.5 i)))
                     (cons "(apply #'max l)" (iwh-ms-loop (apply #'max l)))))
    (let ((ns (iwh-ms-time (cdr row) 100000)))
      (princ (format "MAXMIN-SPEED %-16s %8.0f ns/iter, %8.0f above the control (eq %.0f)\n" (car row) ns (- ns control) control)))))
