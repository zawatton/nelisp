;;; fx-spike-check.el --- word VM spike gives the evaluator's answers -*- lexical-binding: t -*-

;; Usage: nelisp --load test/perf/fx-spike-check.el
;; tools/ai/eval-core-progress.org S1.1.  Each form must give the same value
;; on the word VM (`nelisp--fx-eval') as under `eval', and the covered forms
;; must actually run on the VM (compile non-nil).  Prints one FX-SPIKE line.

(defvar fx-spike-failures nil)
(dolist (c '(((let ((i 0) (n 0)) (while (< i n) (setq i (1+ i))) i) . t)
             ((let ((i 0) (n 1)) (while (< i n) (setq i (1+ i))) i) . t)
             ((let ((i 0) (n 1000)) (while (< i n) (setq i (1+ i))) i) . t)
             ((let ((i 1099511627776)) (1+ i)) . t)
             ((let ((a 5) (b 7)) (if (> a b) (- a b) (+ a b))) . t)
             ((let* ((s 0) (i 10)) (while (> i 0) (setq s (+ s i)) (setq i (1- i))) s) . t)
             ((let ((x 2305843009213693951)) (1+ x)) . t)
             ((let ((x 3)) (if (= x 3) t nil)) . t)
             ((let ((x "a")) x) . nil)
             ((list 1 2) . nil)))
  (let* ((form (car c))
         (covered (and (nelisp--fx-compile form) t))
         (want (eval form t))
         (got (condition-case e (nelisp--fx-eval form) (error (list 'error e)))))
    (unless (and (equal got want) (eq covered (cdr c)))
      (push (list form got want covered) fx-spike-failures))))
(dolist (f (reverse fx-spike-failures)) (princ (format "  MISMATCH %S\n" f)))
(princ (format "FX-SPIKE failures=%d %s\n" (length fx-spike-failures)
               (if fx-spike-failures "FAIL" "PASS")))
(kill-emacs (if fx-spike-failures 1 0))
