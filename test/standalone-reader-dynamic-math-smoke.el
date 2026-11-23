;;; standalone-reader-dynamic-math-smoke.el --- float math vs host Emacs -*- lexical-binding: t; -*-

;; Run by `make standalone-reader-dynamic-math-smoke' on host Emacs and on the
;; dynamically linked reader; the two outputs must be identical.  One line per
;; argument: the argument, then sin cos tan exp log, log to bases 10 / 2 / 3.5,
;; atan, atan2 against 2.0, and asin / acos of a scaled-down argument.  A
;; signalled error prints its condition symbol.

(dolist (x (list 0 1 -1 2 0.0 -0.0 0.5 -0.5 1.3 3.0 10.0 100.0 -2.25
                 1e-300 1e300 -745.2 709.8 1.0e+INF -1.0e+INF 0.0e+NaN
                 (expt 10 30)))
  (princ
   (format "%S %S\n" x
           (mapcar (lambda (f) (condition-case e (funcall f) (error (car e))))
                   (list (lambda () (sin x)) (lambda () (cos x))
                         (lambda () (tan x)) (lambda () (exp x))
                         (lambda () (log x)) (lambda () (log x 10))
                         (lambda () (log x 2)) (lambda () (log x 3.5))
                         (lambda () (atan x)) (lambda () (atan x 2.0))
                         (lambda () (asin (/ (float x) 1e300)))
                         (lambda () (acos (/ (float x) 1e300))))))))

;;; standalone-reader-dynamic-math-smoke.el ends here
