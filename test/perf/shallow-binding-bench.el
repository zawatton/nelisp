;;; shallow-binding-bench.el --- dynamic variable access cost -*- lexical-binding: t -*-

;; Usage: nelisp --load test/perf/shallow-binding-bench.el
;;
;; Prints SB-BENCH lines (microseconds per operation):
;;   regex    -- (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" NAME), the
;;               locate-library pattern from the S1.3 init profile
;;   dynread  -- reading a let-bound special from a callee
;;   dynread-deep -- the same under 40 extra nested special bindings
;;   globread -- reading a never-bound global from a callee

(defvar sbb-x 0)
(defvar sbb-g 1)
(defun sbb-read-x () sbb-x)
(defun sbb-read-g () sbb-g)
(defun sbb-loop-x (n) (let ((i 0)) (while (< i n) (sbb-read-x) (setq i (1+ i)))))
(defun sbb-loop-g (n) (let ((i 0)) (while (< i n) (sbb-read-g) (setq i (1+ i)))))
(defun sbb-nest (k thunk)
  (if (< k 1) (funcall thunk)
    (let ((sbb-g (1+ sbb-g))) (sbb-nest (1- k) thunk))))

(defun sbb-time (thunk n)
  (let ((t0 (float-time)))
    (funcall thunk)
    (/ (* 1e6 (- (float-time) t0)) n)))

(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))))
  (princ (format "SB-BENCH regex %.1f\n"
                 (sbb-time (lambda () (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s)))
                           300))))
(princ (format "SB-BENCH dynread %.2f\n"
               (sbb-time (lambda () (let ((sbb-x 5)) (sbb-loop-x 20000))) 20000)))
(princ (format "SB-BENCH dynread-deep %.2f\n"
               (sbb-time (lambda () (sbb-nest 40 (lambda () (let ((sbb-x 5)) (sbb-loop-x 20000))))) 20000)))
(princ (format "SB-BENCH globread %.2f\n"
               (sbb-time (lambda () (sbb-loop-g 20000)) 20000)))
