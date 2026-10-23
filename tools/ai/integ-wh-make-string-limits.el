;;; integ-wh-make-string-limits.el --- make-string at and beyond the size limits  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1 under ulimit -v 8 GiB; both must print the same bytes.
;; GNU alloc.c: Fmake_string multiplies the encoded width by the count with ckd_mul and allocate_string_data refuses a
;; string of more than STRING_BYTES_MAX (= most-positive-fixnum) bytes with (error "Maximum string size exceeded") before
;; allocating anything; a size that passes the limit but cannot be allocated signals GNU's memory-full error, which is
;; (error "Memory exhausted--use C-x s then exit and restart Emacs") with a propertized text and is printed here as
;; OUT-OF-MEMORY (the runtime signals its own `memory-full', an `error' too).  A binary that tries to allocate the
;; product instead of refusing it crashes.

(defun iwh-ml (label thunk)
  (princ (format "ML %-40s %s\n" label
                 (condition-case e (let ((r (funcall thunk))) (format "STRING %S" r))
                   (error (cond ((equal (cdr e) '("Maximum string size exceeded")) (format "%S" e))
                                ((memq (car e) '(error memory-full)) "OUT-OF-MEMORY")
                                (t (format "%S" e))))))))

(iwh-ml "3-byte char, most-positive-fixnum" (lambda () (make-string most-positive-fixnum #x65e5)))
(iwh-ml "2-byte raw-byte char, 2^60" (lambda () (make-string (1+ (/ most-positive-fixnum 2)) #x3fffff)))
(iwh-ml "3-byte char, limit / 3" (lambda () (make-string (/ most-positive-fixnum 3) #x65e5)))
(iwh-ml "3-byte char, limit / 3 + 1" (lambda () (make-string (1+ (/ most-positive-fixnum 3)) #x65e5)))
(iwh-ml "5-byte char, limit / 5 + 1" (lambda () (make-string (1+ (/ most-positive-fixnum 5)) #x200000)))
(iwh-ml "5-byte char, limit / 5" (lambda () (make-string (/ most-positive-fixnum 5) #x200000)))
(iwh-ml "4-byte char, 2^61" (lambda () (make-string (ash 1 59) #x10000)))
(iwh-ml "ascii, most-positive-fixnum" (lambda () (make-string most-positive-fixnum ?a)))
(iwh-ml "ascii forced multibyte, limit" (lambda () (make-string most-positive-fixnum ?a t)))
(iwh-ml "ascii, 2^40" (lambda () (make-string (ash 1 40) ?a)))
(iwh-ml "ascii, 2^34" (lambda () (make-string (ash 1 34) ?a)))
(iwh-ml "2-byte char, 2^34" (lambda () (make-string (ash 1 34) #xe9)))
(iwh-ml "negative" (lambda () (make-string -1 ?a)))
(iwh-ml "float length" (lambda () (make-string 2.0 ?a)))
(iwh-ml "small ascii" (lambda () (make-string 3 ?x)))
(iwh-ml "small wide" (lambda () (make-string 2 #x65e5)))
(iwh-ml "zero" (lambda () (make-string 0 #x65e5)))
(princ "INTEG-WH-MAKE-STRING-LIMITS DONE\n")
