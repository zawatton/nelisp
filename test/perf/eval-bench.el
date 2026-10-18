;;; eval-bench.el --- evaluator per-form costs -*- lexical-binding: t -*-

;; Usage: BENCH=NAME nelisp --load test/perf/eval-bench.el
;;
;; One case per run, named by $BENCH.  Each prints "EVAL-BENCH NAME value
;; us (limit L) PASS|FAIL" and exits non-zero over the limit, for
;; tools/ai/evaluator-speed-progress.org.  `empty-loop' is the cost of one
;; (while (< i n) ... (setq i (1+ i))) iteration; every other case is the
;; extra cost of its form inside that loop.  The best of three runs is
;; reported, so a stray collection or a busy machine does not decide it.

(defmacro eval-bench-loop (n &rest body)
  `(let ((t0 (float-time)) (i 0) (n ,n))
     (while (< i n) ,@body (setq i (1+ i)))
     (- (float-time) t0)))

(defun eval-bench-best (thunk)
  (let ((best nil) (k 0))
    (while (< k 3)
      (let ((v (funcall thunk))) (when (or (null best) (< v best)) (setq best v)))
      (setq k (1+ k)))
    best))

;; Cases are evaluated with `eval', as top-level code is, not from inside a
;; lambda: a closure body can be run differently from a top-level form.
(defun eval-bench-net (n form)
  "Microseconds per evaluation of FORM beyond the empty loop.
The best base and the best run are taken separately, then subtracted: a
collection landing in one base trial made that trial's difference
negative, clamped to 0, and the best of three then reported 0.00 for
any form."
  (let ((base (eval-bench-best (lambda () (eval `(eval-bench-loop ,n nil) t))))
        (run (eval-bench-best (lambda () (eval `(eval-bench-loop ,n ,form) t)))))
    (max 0.0 (/ (* 1e6 (- run base)) n))))

(defun eval-bench-f3 (a _b _c) a)
(defvar eval-bench-special 0)
(defvar eval-bench-string (make-string 20000 ?a))

(defconst eval-bench-cases
  '((empty-loop . nil)
    (if-eq . (if (eq i 1) 1 2))
    (let-1 . (let ((x i)) x))
    (setq-dynamic . (setq eval-bench-special i))
    (defun-call-3 . (eval-bench-f3 i 2 3))
    (funcall-lambda . (funcall (lambda (x) x) i))
    (aref-loop . (aref eval-bench-string i))))

(defun eval-bench-run (name)
  (if (eq name 'empty-loop)
      (eval-bench-best (lambda () (/ (* 1e6 (eval '(eval-bench-loop 20000 nil) t)) 20000)))
    (eval-bench-net 20000 (cdr (assq name eval-bench-cases)))))

(let* ((name (intern (or (getenv "BENCH") "")))
       (limit (and (getenv "LIMIT") (string-to-number (getenv "LIMIT"))))
       (case (assq name eval-bench-cases))
       (ok nil))
  (if (not case)
      (princ (format "EVAL-BENCH unknown BENCH=%s; one of %S\n" name (mapcar #'car eval-bench-cases)))
    (let ((v (eval-bench-run name)))
      (setq ok (or (null limit) (<= v limit)))
      (princ (format "EVAL-BENCH %s %.2f us (limit %s) %s\n" name v limit (if ok "PASS" "FAIL")))))
  (kill-emacs (if ok 0 1)))

;;; eval-bench.el ends here
