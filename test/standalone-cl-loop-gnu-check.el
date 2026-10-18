;;; standalone-cl-loop-gnu-check.el --- cl-loop clause shapes GNU code uses -*- lexical-binding: t -*-

;; Each form below is evaluated and printed; NeLisp standalone must print
;; exactly what GNU Emacs 31.1 prints:
;;   target/nelisp --load test/standalone-cl-loop-gnu-check.el > a
;;   emacs --batch -l test/standalone-cl-loop-gnu-check.el > b; diff a b
;; The first two are what GNU ert.el needs (ert--parse-keys-and-body and
;; ert--make-stats).

(require 'cl-lib)

(dolist (form
         '((cl-loop for (key . value) in '((:a . 1) (:b . 2))
                    collect key collect value)
           (let ((h (make-hash-table)))
             (cl-loop for i from 0 for x across [a b c] do (puthash x i h))
             (list (gethash 'a h) (gethash 'c h)))
           (cl-loop for x in '(1 2) collect x collect (* 10 x))
           (let ((flag t))
             (cl-loop for x in '(1 2) do (ignore x) finally (setq flag nil))
             flag)
           (cl-loop for x across [1 2 3] for i from 0
                    when (cl-oddp x) collect (cons i x) into r
                    finally return r)
           (cl-loop repeat 3 collect 'z)
           (cl-loop for x on '(1 2 3) by #'cddr collect x)
           (cl-loop for i from 10 downto 1 by 3 collect i)
           (cl-loop for x in '(3 1 4 1 5) maximize x)
           (cl-loop for x in '(3 1 4 1 5) minimize x)
           (cl-loop for x in '(1 2 3) thereis (and (> x 1) x))
           (cl-loop for x in '(1 2 3) never (> x 5))
           (cl-loop for x in '(1 2 3) always (> x 0))
           (cl-loop for x in '(1 2 3 4) if (cl-evenp x) collect x else collect (- x) end)
           (cl-loop for x in '((1 2) (3 4)) append x)
           (cl-loop for x in '(a b) vconcat (vector x x))
           (cl-loop for x in '("a" "b") concat x)
           (cl-loop with s = 0 for x in '(1 2 3) do (setq s (+ s x)) finally return s)
           (cl-loop for x = 1 then (* 2 x) while (< x 50) collect x)
           (cl-loop for x in '(1 2 3) until (= x 2) collect x)
           (cl-loop named outer for x in '(1 2 3) do (when (= x 2) (cl-return-from outer x)))
           (let ((h (make-hash-table :test 'equal)))
             (puthash "k" 1 h)
             (cl-loop for k being the hash-keys of h using (hash-values v) collect (cons k v)))
           (cl-loop for (a b) in '((1 2) (3 4)) sum (* a b))
           (cl-loop for x in '(1 2 3) count (cl-oddp x))
           (cl-loop for c across "ab" collect c)
           (cl-loop for x in '(1 2) for y = (* x 10) collect (list x y))
           (cl-loop for x in '(1 2 3) nconc (list x))
           (let (out) (cl-loop initially (push 'init out) for x in '(1) do (push x out)) out)
           ;; Shapes anvil-standalone uses at startup.
           (cl-loop for (k v) on '(:a 1 :b 2 :c 3) by #'cddr collect (cons k v))
           (cl-loop for (k v) on '(:a 1 :b 2) by #'cddr collect k collect v)
           (cl-loop for (a . b) in '((1 . 2) (3 . 4)) collect (+ a b))
           (cl-loop for (a (b c)) in '((1 (2 3)) (4 (5 6))) collect (list c b a))
           (cl-loop for [a b] across [[1 2] [3 4]] collect (* a b))
           (cl-loop for x in '(1 2 3)
                    collect x into odds-and-all
                    when (cl-oddp x) collect x into odds
                    append (list x x) into doubled
                    nconc (list (- x)) into negs
                    finally return (list odds-and-all odds doubled negs))
           (cl-loop for (k . v) in '((a . 1) (b . 2)) collect k into ks collect v into vs
                    finally return (list ks vs))))
  (princ (format "%S\n" (condition-case err (eval form t) (error (list 'error err))))))

;;; standalone-cl-loop-gnu-check.el ends here
