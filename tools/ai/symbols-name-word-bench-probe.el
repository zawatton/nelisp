;;; symbols-name-word-bench-probe.el --- timings of the symbol-name paths -*- lexical-binding: t; -*-

;; BIN --load tools/ai/symbols-name-word-bench-probe.el with SNW_BENCH=reader | intern.
;; Prints one line "SNWB MODE key=value ..." (milliseconds).  Run by tools/ai/symbols-name-word-bench.py, which
;; alternates the baseline and the candidate binary round by round on one CPU.

(defun snwb-now () (float-time))

(defun snwb-names (prefix n)
  (let ((names nil) (i 0))
    (while (< i n)
      (setq names (cons (format "%s-%d" prefix i) names))
      (setq i (1+ i)))
    (nreverse names)))

;; The reader's cost of N names it has never seen (the first read interns each, gives it its object and its name)
;; and of the same names again (the second and third reads hit the intern table and the cells vector).
(defun snwb-reader (n)
  (let* ((text (concat "(" (mapconcat #'identity (snwb-names "snw-bench-sym" n) " ") ")"))
         (t0 (snwb-now))
         (l1 (car (read-from-string text)))
         (t1 (snwb-now))
         (l2 (car (read-from-string text)))
         (t2 (snwb-now))
         (l3 (car (read-from-string text)))
         (t3 (snwb-now)))
    (ignore l2 l3)
    (princ (format "SNWB reader first_ms=%.3f second_ms=%.3f third_ms=%.3f len=%d\n"
                   (* 1000 (- t1 t0)) (* 1000 (- t2 t1)) (* 1000 (- t3 t2)) (length l1)))))

(defun snwb-loop-intern (names)
  (dolist (s names) (intern s)))

(defun snwb-loop-symbol-name (syms)
  (let ((p 0))
    (while (< p 10)
      (dolist (s syms) (symbol-name s))
      (setq p (1+ p)))))

(defun snwb-loop-control (syms)
  (let ((p 0))
    (while (< p 10)
      (dolist (s syms) (symbolp s))
      (setq p (1+ p)))))

;; intern of N new names, of the same names again, and symbol-name over them (ten passes) against a control
;; loop of the same shape that calls symbolp.
(defun snwb-intern (n)
  (let* ((names (snwb-names "snw-bench-int" n))
         (t0 (snwb-now)))
    (snwb-loop-intern names)
    (let ((t1 (snwb-now)))
      (snwb-loop-intern names)
      (let* ((t2 (snwb-now))
             (syms (mapcar #'intern names))
             (t3 (snwb-now)))
        (snwb-loop-symbol-name syms)
        (let ((t4 (snwb-now)))
          (snwb-loop-control syms)
          (let ((t5 (snwb-now)))
            (princ (format "SNWB intern new_ms=%.3f existing_ms=%.3f symname_ms=%.3f control_ms=%.3f\n"
                           (* 1000 (- t1 t0)) (* 1000 (- t2 t1)) (* 1000 (- t4 t3)) (* 1000 (- t5 t4))))))))))

(let ((mode (getenv "SNW_BENCH")))
  (cond ((equal mode "reader") (snwb-reader 30000))
        ((equal mode "intern") (snwb-intern 30000))
        (t (princ "SNWB error unknown SNW_BENCH\n"))))
(kill-emacs 0)
