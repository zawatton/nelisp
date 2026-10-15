;;; standalone-compare-strings-parity.el --- compare-strings fast path parity -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-compare-strings-parity.el
;;
;; Compares `compare-strings' with its general implementation
;; (`nelisp--compare-strings-general') over ASCII, Japanese, emoji, raw
;; bytes, unibyte/multibyte pairs and END values that are nil, in range,
;; past the end or negative, including the errors they signal.  Then
;; times `string-prefix-p' (GNU's, which calls `compare-strings') against
;; `string=' on short strings.  Prints one COMPARE-PARITY line and exits
;; non-zero on any difference or when `string-prefix-p' costs more than
;; 3x `string='.

(defun cs-parity--call (fn &rest args)
  (condition-case err (apply fn args) (error (list 'error (car err)))))

(defun cs-parity--time (fn)
  (garbage-collect)
  (let ((t0 (float-time)) (i 0))
    (while (< i 2000) (funcall fn) (setq i (1+ i)))
    (- (float-time) t0)))

(let ((strings (list "" "a" "ab" "abc" "abd" "abcd" "ABC" "あい" "あいう" "あいえ"
                     "😀x" "😀y" (string-to-unibyte "abc") (string 200 201)
                     (string 200) (string-to-multibyte "abc")))
      (ends '(nil 0 1 2 3 10 -1))
      (n 0) (bad 0))
  (dolist (s1 strings)
    (dolist (s2 strings)
      (dolist (e1 ends)
        (dolist (e2 ends)
          (setq n (1+ n))
          (let ((want (cs-parity--call #'nelisp--compare-strings-general
                                       s1 0 e1 s2 nil e2 nil))
                (got (cs-parity--call #'compare-strings s1 0 e1 s2 nil e2 nil)))
            (unless (equal want got)
              (setq bad (1+ bad))
              (when (< bad 20)
                (princ (format "DIFF %S %S %S %S: %S vs %S\n"
                               s1 e1 s2 e2 want got)))))))))
  ;; Wrong types still signal from the native path.
  (dolist (args (list (list 'x 0 nil "a" 0 nil) (list "a" 0 nil 7 0 nil)
                      (list "a" 0 'z "a" 0 nil) (list "a" 0 nil "a" 0 1.5)))
    (setq n (1+ n))
    (let ((want (apply #'cs-parity--call #'nelisp--compare-strings-general
                       (append args '(nil))))
          (got (apply #'cs-parity--call #'compare-strings args)))
      (unless (equal want got)
        (setq bad (1+ bad))
        (princ (format "DIFF %S: %S vs %S\n" args want got)))))
  (progn
    (let* ((empty (min (cs-parity--time (lambda () nil)) (cs-parity--time (lambda () nil))))
           (prefix (- (min (cs-parity--time (lambda () (string-prefix-p "じ" "じかん")))
                           (cs-parity--time (lambda () (string-prefix-p "じ" "じかん"))))
                      empty))
           (equal (- (min (cs-parity--time (lambda () (string= "じ" "じ")))
                          (cs-parity--time (lambda () (string= "じ" "じ"))))
                     empty))
           (ratio (/ prefix (max 1e-6 equal)))
           (ok (and (= bad 0) (eq (string-prefix-p "じ" "じかん") t)
                    (null (string-prefix-p "か" "じかん")) (<= ratio 3.0))))
      (princ (format "COMPARE-PARITY cases=%d diffs=%d prefix=%.1fus string==%.1fus ratio=%.2f %s\n"
                     n bad (/ (* 1e6 prefix) 2000) (/ (* 1e6 equal) 2000) ratio
                     (if ok "PASS" "FAIL")))
      (kill-emacs (if ok 0 1)))))

;;; standalone-compare-strings-parity.el ends here
