;;; nelisp-interp-primitives-corpus.el --- Interpreted argv parity -*- lexical-binding: t; -*-

;; Every case enters EVAL with an unevaluated form, including primitive
;; calls inside lambdas.  The same file runs unchanged on GNU and NeLisp.
(defconst wip-cases
  '((aref-vector (aref (make-vector 4 'ok) 1))
    (aref-legacy (aref (vector 'a 'b 'c) 1))
    (aref-seven (aref (make-vector 7 'ok) 6))
    (aset-vector (let ((v (make-vector 4 nil))) (list (aset v 2 'ok) v)))
    (aset-legacy (let ((v (vector 1 2 3))) (list (aset v 1 'ok) v)))
    (aset-seven (let ((v (make-vector 7 nil))) (list (aset v 6 'ok) v)))
    (length-vector (length (make-vector 4 nil)))
    (length-list (length '(a b c)))
    (length-nil (length nil))
    (length-string (length "a日本λ"))
    (aref-string (aref "a日本λ" 2))
    (aset-string (let ((s (copy-sequence "abcd"))) (list (aset s 1 ?z) s)))
    (aset-multibyte (let ((s (copy-sequence "a日本λ"))) (list (aset s 1 ?月) s)))
    (aref-bool (aref (make-bool-vector 65 t) 64))
    (aset-bool (let ((v (make-bool-vector 65 nil)))
                 (list (aset v 64 'truth) (aref v 64) (length v))))
    (aref-chartable (let ((v (make-char-table nil 'default))) (aref v ?a)))
    (aset-chartable (let ((v (make-char-table nil nil)))
                      (list (aset v ?a 'ok) (aref v ?a))))
    (aref-type (aref 9 0))
    (aref-index-type (aref [a b] 'bad))
    (aref-low (aref [a b] -1))
    (aref-high (aref [a b] 2))
    (aref-huge (aref [a b] 9223372036854775808))
    (aref-string-high (aref "a日" 2))
    (aref-bool-high (aref (make-bool-vector 1 nil) 1))
    (aset-type (aset 9 0 'x))
    (aset-index-type (aset [a b] 'bad 'x))
    (aset-low (aset [a b] -1 'x))
    (aset-high (aset [a b] 2 'x))
    (aset-character-type (aset (copy-sequence "abc") 1 'bad))
    (length-type (length 9))
    (length-dotted (length '(a . b)))
    (rem-positive (% 13 4))
    (rem-negative (% -13 4))
    (rem-negative-divisor (% 13 -4))
    (rem-negative-both (% -13 -4))
    (rem-zero (% 13 0))
    (rem-type (% 'bad 4))
    (rem-divisor-type (% 13 'bad))
    (rem-float (% 13.0 4))
    (rem-divisor-float (% 13 4.0))
    (rem-bignum (% 9223372036854775808 3))
    (rem-bignum-divisor (% 13 9223372036854775808))
    (rem-fixnum-min (% -2305843009213693952 -1))
    (rem-marker (with-temp-buffer (insert "abc") (% (copy-marker 3) 2)))
    (logand-zero (logand))
    (logand-one (logand -1))
    (logand-two (logand 15 7))
    (logand-many (logand 31 15 7 3))
    (logand-type (logand 15 'bad))
    (logand-float (logand 15 3.0))
    (logand-bignum (logand 9223372036854775808 -1))
    (logand-marker (with-temp-buffer (insert "abc") (logand 7 (copy-marker 3))))
    (eval-order (let ((i 0) (v (make-vector 4 nil)))
                  (list (aset (progn (setq i (+ i 1)) v)
                              (progn (setq i (+ i 1)) 2)
                              (progn (setq i (+ i 1)) i)) i v)))
    (decline-eval-once (let ((i 0))
                         (list (condition-case e
                                   (% (progn (setq i (+ i 1)) 13.0)
                                      (progn (setq i (+ i 1)) 4))
                                 (error e)) i)))
    (arity-before-eval (let ((i 0))
                         (list (condition-case e (aref (setq i 1)) (error e)) i)))
    (interpreted-lambda ((lambda (a i) (aref a (% i 4))) (make-vector 4 'ok) 9))
    (alias (progn (fset 'wip-aref-alias 'aref) (wip-aref-alias [a b] 1)))))

(defun wip-print-case (name form)
  "Print NAME and FORM's value or complete condition data."
  (let ((value (condition-case e (eval form t) (error e))))
    (princ (format "%S\n" (list name value)))))

(dolist (case wip-cases) (wip-print-case (car case) (cadr case)))

;; Check exact arity errors for all five, including the pre-evaluation
;; check on each fixed-arity builtin.  logand has no upper arity bound.
(dolist (case '((aref) (aref [a] 0 1) (aset [a] 0) (aset [a] 0 1 2)
                (length) (length nil nil) (%) (% 1) (% 1 2 3)))
  (wip-print-case (list 'arity (car case) (length (cdr case))) case))

(dolist (name '(aref aset length % logand))
  (let ((saved (symbol-function name)) result)
    (unwind-protect
        (progn
          (fset name (lambda (&rest xs) (list 'redefined xs)))
          ;; Use fixed arguments, since LENGTH itself is one of the targets.
          (setq result (eval (list name 10 20) t)))
      (fset name saved))
    (princ (format "%S\n" (list (list 'redefined name) result)))))

(defun wip-advice (original &rest args)
  (if (eq (car args) 'wip-token) 'advised (apply original args)))
(dolist (name '(aref aset length % logand))
  (let ((saved (symbol-function name)) result)
    (unwind-protect
        (progn (advice-add name :around #'wip-advice)
               (setq result (eval (list name '(quote wip-token)) t)))
      (fset name saved))
    (princ (format "%S\n" (list (list 'advised name) result)))))

(princ "WIP-CORPUS COMPLETE\n")
nil
;;; nelisp-interp-primitives-corpus.el ends here
