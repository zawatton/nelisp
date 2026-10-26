;; -*- lexical-binding: t; -*-
;; Differential corpus: decimal text -> double -> shortest text, NeLisp vs GNU.
;; One line per string: "I: INPUT => OUTPUT".  Inputs come from a fixed LCG, so both engines
;; see the same strings; they cover 1 to 18 significant digits, decimal exponents -40..60,
;; exact halfway cases (dyadic values with 54 significant bits) and near-halfway cases.
(defvar algo1-seed 424242)
(defun algo1-rnd (m) (setq algo1-seed (% (+ (* algo1-seed 48271) 11) 2147483647)) (% algo1-seed m))
(defun algo1-digits (n) (let ((s (number-to-string (1+ (algo1-rnd 9))))) (dotimes (_ (1- n)) (setq s (concat s (number-to-string (algo1-rnd 10))))) s))
(defun algo1-show (s)
  (let ((v (condition-case e (string-to-number s) (error (list 'ERR e)))))
    (format "%s => %S %S" s v (if (floatp v) (= v (string-to-number (format "%S" v))) 'int))))
(let ((i 0))
  (dotimes (_ 1500)
    (let* ((nd (1+ (algo1-rnd 18))) (d (algo1-digits nd)) (pt (algo1-rnd (1+ nd))) (ex (- (algo1-rnd 100) 40))
           (ip (if (= pt 0) "0" (substring d 0 pt))) (fp (if (= pt nd) "0" (substring d pt)))
           (txt (concat ip "." fp (if (< (algo1-rnd 4) 1) (format "e%d" ex) ""))))
      (setq i (1+ i))
      (princ (format "%d: %s\n" i (algo1-show txt))))))
;; 2^53 + d scaled by powers of two, written in decimal (exact halfway cases)
(let ((i 2000))
  (dolist (k '(0 1 2 3 5 10 20 30 40))
    (dolist (d '(1 3 5 -1 -3))
      (let* ((n (+ 9007199254740992 d)) (v (* n (expt 2 k))))
        (setq i (1+ i))
        (princ (format "%d: %s\n" i (algo1-show (format "%d.0" v)))))))
  ;; dyadic fractions m / 2^j written exactly as m * 5^j / 10^j
  (dotimes (_ 200)
    (let* ((j (1+ (algo1-rnd 27))) (m (+ 1 (algo1-rnd 1000000000))) (txt nil)
           (digits (number-to-string (* m (expt 5 j)))) (len (length digits)))
      (setq i (1+ i))
      (setq txt (if (> len j)
                    (concat (substring digits 0 (- len j)) "." (substring digits (- len j)))
                  (concat "0." (make-string (- j len) ?0) digits)))
      (princ (format "%d: %s\n" i (algo1-show txt))))))
;; halfway cases between adjacent doubles above 1: (2^53 + 2k+1) / 2^53 as its exact decimal expansion
(let ((i 3000))
  (dotimes (k 40)
    (let* ((big (* (+ (expt 2 53) (+ (* 2 k) 1)) (expt 5 53))) (s (number-to-string big)) (len (length s)))
      (setq i (1+ i))
      (princ (format "%d: %s\n" i (algo1-show (concat (substring s 0 (- len 53)) "." (substring s (- len 53)))))))))
(let ((i 4000))
  (dolist (txt '("0.0" "-0.0" "1e308" "1.7976931348623157e308" "2.2250738585072014e-308" "4.9e-324" "5e-324" "2e-324" "1e-400"
                 "123456789012345678" "0.123456789012345678" "123456789012345678e-20" "12345678901234567890.5" "0.30000000000000004" "0.1" "0.2"
                 "9007199254740993" "9007199254740993.0" "9007199254740993e0" "18014398509481985" "18014398509481984.5" "1.5e300" ".5" "5." "1e" "+1.5" "-1.5e-5"
                 "3.141592653589793" "2.718281828459045" "1.0e22" "1e23" "9.999999999999999e22" "8.41e21" "5.708990770823839e-300"))
    (setq i (1+ i))
    (princ (format "%d: %s\n" i (algo1-show txt)))))
;; long digit strings (the parser clears only 16 words for texts up to 120 characters)
(let ((i 5000))
  (dolist (txt (list (concat "0." (make-string 130 ?0) "1") (concat "1" (make-string 150 ?0) ".5") (concat "123456789." (make-string 140 ?7))
                     (concat (make-string 400 ?9) "e-400") (concat "0." (make-string 400 ?1)) "1e-320" "4.9406564584124654e-324"
                     "2.4703282292062327e-324" "2.4703282292062328e-324" (concat "1." (make-string 119 ?1)) (concat "1." (make-string 120 ?1))
                     (concat "1." (make-string 121 ?1)) "1.7976931348623158e308" "9007199254740993.0000000000000000001"
                     "0.000000000000000000000000001234567890123456789" "1234567890123456789e-27" "123456789012345678e-18"))
    (setq i (1+ i))
    (princ (format "%d: %s\n" i (algo1-show txt)))))
