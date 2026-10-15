;;; standalone-split-string-cost-check.el --- short split-string call cost -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-split-string-cost-check.el
;;
;; Times 2000 calls of `split-string' on a short ASCII string with a
;; one-character separator against 2000 calls of `substring', both inside
;; a function and net of the empty loop (best of three each).  A split
;; must cost at most 4x a substring: the per-call overhead, not the
;; scan, is what short strings pay.  Prints one SPLIT-COST line and exits
;; non-zero on failure.

(defvar split-cost--s "alpha/beta/gamma")

(defmacro split-cost--time (&rest body)
  `(progn
     (defalias 'split-cost--fn
       (lambda ()
         (let ((a split-cost--s) (i 0))
           (ignore a)
           (while (< i 2000) ,@body (setq i (1+ i))))))
     (garbage-collect)
     (let ((t0 (float-time)))
       (split-cost--fn)
       (- (float-time) t0))))

(defun split-cost--best (thunk)
  (min (funcall thunk) (funcall thunk) (funcall thunk)))

(let* ((empty (split-cost--best (lambda () (split-cost--time nil))))
       (split (- (split-cost--best (lambda () (split-cost--time (split-string a "/"))))
                 empty))
       (sub (- (split-cost--best (lambda () (split-cost--time (substring a 3 9))))
               empty))
       (ratio (/ split (max 1e-6 sub)))
       (ok (and (equal (split-string split-cost--s "/") '("alpha" "beta" "gamma"))
                (<= ratio 4.0))))
  (princ (format "SPLIT-COST split=%.1fus substring=%.1fus ratio=%.2f (limit 4) %s\n"
                 (/ (* 1e6 split) 2000) (/ (* 1e6 sub) 2000) ratio
                 (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-split-string-cost-check.el ends here
