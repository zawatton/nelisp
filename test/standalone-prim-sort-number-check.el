;;; standalone-prim-sort-number-check.el --- Exact numeric ordering -*- lexical-binding: t -*-

;; Powers of two avoid depending on decimal-reader rounding at IEEE edges.
(defun ps-number-power (exponent)
  (let ((result 1.0))
    (if (< exponent 0)
        (dotimes (_ (- exponent)) (setq result (/ result 2.0)))
      (dotimes (_ exponent) (setq result (* result 2.0))))
    result))
(defvar ps-number-values
  (list 0 1 -1 2 -2 1.5 -1.5 0.0 -0.0
        (ps-number-power -1074) (- (ps-number-power -1074))
        (ps-number-power -1022) (- (ps-number-power -1022))
        (1+ (ash 1 52)) (ash 1 60) (1+ (ash 1 60)) (- (1+ (ash 1 60)))
        (ps-number-power 60) (- (ps-number-power 60))
        (ash 1 80) (1+ (ash 1 80)) (1- (ash 1 80)) (- (ash 1 80)) (- (1+ (ash 1 80)))
        (ps-number-power 80) (- (ps-number-power 80))
        (ash 1 1023) (1+ (ash 1 1023)) (- (ash 1 1023))
        (ps-number-power 1023) (- (ps-number-power 1023))
        (ash 1 1024) (1- (ash 1 1024))
        (ps-number-power 1024) (- (ps-number-power 1024)) 0.0e+NaN
        (* 2.0 (- (ps-number-power 1023) (ps-number-power 970)))
        (- (* 2.0 (- (ps-number-power 1023) (ps-number-power 970))))))
(let ((index 0))
  (unless (= (length ps-number-values) 38) (error "numeric population"))
  (dolist (a ps-number-values)
    (dolist (b ps-number-values)
      (princ (format "PS-NUMBER %d " index))
      (prin1
       (list
        (mapcar (lambda (pred)
                  (condition-case problem
                      (let ((result (sort (list (cons 0 a) (cons 1 b)) :key #'cdr :lessp pred)))
                        (list (car (car result)) (car (cadr result))))
                    (error (car problem))))
                '(< > value<))
        (mapcar (lambda (pred)
                  (condition-case problem (funcall pred a b) (error (car problem))))
                '(< > <= >= = value<))))
      (terpri)
      (when (= (% index 128) 0) (garbage-collect))
      (setq index (1+ index))))
  (unless (= index 1444) (error "numeric pair count")))
(princ "PS-NUMBER COMPLETE\n")
