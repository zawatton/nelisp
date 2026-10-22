;;; word-heap-numeric-parity.el --- Float, bignum and marker parity (Doc 216 S5) -*- lexical-binding: t; -*-

;; Runs unchanged on NeLisp and GNU Emacs 31.1 and prints one
;; "WH-NUM LABEL RESULT" line per case: the prin1 form of the value, or
;; "ERROR" and the signal data.  word-heap-stage-check.py s5.1 requires
;; identical lines.  The cases exercise the objects word-heap S5 moves out
;; of header boxes: their values, arithmetic, printing, identity and the
;; errors their primitives signal.  Bignums are built with ash and *.  The
;; cases leave out float-pi, copysign, frexp and bignum results of expt,
;; library functions NeLisp lacks today.  They keep the conversions
;; between floats, bignums and text (format %d, float, truncate), which
;; read the representation; on the base, four of them differ from GNU
;; (big-format, big-float, big-truncate, err-truncate-inf).
(defmacro whn--case (label form)
  `(princ (format "WH-NUM %s %s\n" ,label
                  (condition-case err (prin1-to-string ,form)
                    (error (format "ERROR %S" err))))))

;; Floats.
(whn--case "float-add" (+ 1.5 2.25))
(whn--case "float-div" (/ 7 2.0))
(whn--case "float-inf" (* 1e300 1e300))
(whn--case "float-ninf" (- (* 1e300 1e300)))
(whn--case "float-nan" (isnan (/ 0.0 0.0)))
(whn--case "float-of-int" (float 3))
(whn--case "float-truncate" (truncate 2.7))
(whn--case "float-round" (list (round 2.5) (round -2.5) (round 3.5)))
(whn--case "float-floor" (floor -1.5))
(whn--case "float-ffloor" (ffloor 1.5))
(whn--case "float-sqrt" (sqrt 2.0))
(whn--case "float-format" (format "%.3f" 3.14159))
(whn--case "float-string" (number-to-string 0.1))
(whn--case "float-read" (string-to-number "1e3"))
(whn--case "float-signed-zero" (list (eql 0.0 -0.0) (= 0.0 -0.0) (equal 1.0 1.0)))
(whn--case "float-eq-self" (let ((x 1.5)) (eq x x)))
(whn--case "float-expt" (expt 2.0 10))
(whn--case "float-minmax" (list (min 1 2.0) (max 1 2.0)))
(whn--case "float-list" (mapcar (lambda (x) (* x 0.5)) '(1 2 3)))
;; Bignums.
(whn--case "big-mul" (* (ash 1 50) (ash 1 50)))
(whn--case "big-fixnum-edge" (list (* most-positive-fixnum 2) (+ most-positive-fixnum 1)
                                   (- most-negative-fixnum 1)))
(whn--case "big-predicates" (list (bignump (ash 1 100)) (fixnump (ash 1 10))))
(whn--case "big-logand" (logand (ash 1 100) (1- (ash 1 100))))
(whn--case "big-ash" (ash 1 100))
(whn--case "big-div" (let ((x (* 1000000000000000 1000000000000000))) (list (/ x 7) (% x 7))))
(whn--case "big-compare" (list (= (ash 1 100) (ash 1 100)) (eql (ash 1 100) (ash 1 100))
                               (< (ash 1 99) (ash 1 100))))
(whn--case "big-format" (format "%d" (* 847288609443 847288609443)))
(whn--case "big-negative" (number-to-string (- (ash 1 70))))
(whn--case "big-read" (string-to-number "123456789012345678901234567890"))
(whn--case "big-float" (float (ash 1 100)))
(whn--case "big-truncate" (truncate (* 1.0 (ash 1 70))))
(whn--case "big-abs" (abs (- (ash 1 80))))
;; Errors.
(whn--case "err-div-zero" (/ 1 0))
(whn--case "err-float-div-zero" (/ 1.0 0))
(whn--case "err-type" (+ 1 'a))
(whn--case "err-truncate-inf" (truncate 1.0e+INF))
(whn--case "err-expt" (expt 0 -1))
;; Markers.
(whn--case "marker-detached" (let ((m (make-marker))) (list (marker-position m) (marker-buffer m))))
(whn--case "marker-print" (format "%S" (make-marker)))
(whn--case "marker-predicate" (markerp (make-marker)))
(whn--case "marker-moves" (with-temp-buffer
                            (insert "hello")
                            (let ((m (copy-marker 3)))
                              (list (marker-position m)
                                    (progn (goto-char 1) (insert "xx") (marker-position m))))))
(whn--case "marker-arith" (with-temp-buffer
                            (insert "abc")
                            (let ((m (point-marker))) (list (+ m 1) (= m 4) (< m 5)))))
(whn--case "marker-insertion-type" (with-temp-buffer
                                     (insert "abc")
                                     (let ((m (copy-marker 2 t)))
                                       (goto-char 2) (insert "Z") (marker-position m))))
(whn--case "marker-unset" (with-temp-buffer
                            (insert "abc")
                            (let ((m (copy-marker 2))) (set-marker m nil) (marker-position m))))

;; The four gaps and the cases around them (every line is read back from GNU
;; Emacs 31.1): format of a bignum, float of a bignum, floats beyond the
;; fixnum range, and the overflow of an infinity.
(whn--case "big-format-neg" (format "%d" (- (ash 1 100))))
(whn--case "big-format-radix" (list (format "%x" (ash 1 70)) (format "%X" (- (ash 1 70)))
                                    (format "%o" (ash 1 70)) (format "%#x" (ash 1 70))
                                    (format "%#o" (ash 1 70))))
(whn--case "big-format-flags" (list (format "%+d" (ash 1 70)) (format "% d" (ash 1 70))
                                    (format "%40d|" (ash 1 100)) (format "%-40d|" (ash 1 100))
                                    (format "%040d|" (- (ash 1 100)))))
(whn--case "big-format-edge" (list (format "%d" (1+ most-positive-fixnum))
                                   (format "%d" (1- most-negative-fixnum))
                                   (format "%x" (1+ most-positive-fixnum))))
(whn--case "big-format-float" (list (format "%e" (ash 1 70)) (format "%f" (ash 1 70))
                                    (format "%g" (ash 1 70)) (format "%.2f" (- (ash 1 80)))))
(whn--case "big-format-float-arg" (list (format "%d" 1e20) (format "%x" 1e20)))
(whn--case "big-float-rounding" (list (float (+ (ash 1 53) 1)) (float (+ (ash 1 53) 3))
                                      (float (+ (ash 1 54) 2)) (float (+ (ash 1 54) 3))
                                      (float (1- (ash 1 64))) (float (- (ash 1 100)))
                                      (float (1+ (ash 1 100)))
                                      (float (+ (ash 1 100) (ash 1 47)))
                                      (float (+ (ash 1 100) (ash 1 47) 1))))
(whn--case "big-float-range" (list (float (ash 1 1023)) (float (1- (ash 1 1024)))
                                   (float (- (ash 1 1024) (ash 1 971)))
                                   (float (- (ash 1 1024) (ash 1 972)))
                                   (float (- (ash 1 2000)))))
(whn--case "big-float-arith" (list (+ 1.5 (ash 1 100)) (* 2.0 (- (ash 1 70)))
                                   (/ (ash 1 100) 2.0) (- 1.0 (ash 1 62))
                                   (/ 1.0 (ash 1 2000))))
(whn--case "big-float-compare" (list (< 1.5 (ash 1 100)) (> 1e30 (ash 1 100))
                                     (= 1.0 (ash 1 0))))
(whn--case "big-truncate-family" (list (truncate 1e20) (floor -1e20) (ceiling 1e20)
                                       (round 1e20) (round -1e30) (truncate 1e30)))
(whn--case "big-truncate-edge" (list (truncate 4.611686018427388e18)
                                     (truncate -4.611686018427388e18)
                                     (floor 9.223372036854776e18)
                                     (ceiling -9.223372036854776e18)
                                     (truncate 2.305843009213693e18)
                                     (truncate 2.305843009213694e18)))
(whn--case "big-truncate-kind" (list (bignump (truncate 4.611686018427388e18))
                                     (fixnump (truncate 2.305843009213693e18))
                                     (bignump (truncate 2.305843009213694e18))
                                     (bignump (truncate (ash 1 100)))))
(whn--case "big-integer-passthrough" (list (truncate (ash 1 100)) (floor (- (ash 1 100)))
                                           (ceiling (ash 1 70))))
(whn--case "big-ffloor" (list (ffloor 1e30) (fceiling -1e30) (fround 2.5) (ftruncate -1e30)))
(whn--case "err-overflow-family" (list (condition-case e (truncate 1.0e+INF) (error e))
                                       (condition-case e (floor -1.0e+INF) (error e))
                                       (condition-case e (ceiling 0.0e+NaN) (error e))
                                       (condition-case e (round 1.0e+INF) (error e))
                                       (condition-case e (truncate 1.0e+INF 2) (error e))
                                       (condition-case e (floor 5 1.0e+INF) (error e))))
(princ "WH-NUM-DONE\n")
nil
