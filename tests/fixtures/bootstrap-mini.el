;;; bootstrap-mini.el --- Doc 47 Stage 8 fixture  -*- lexical-binding: t -*-

;; Exercise the build-tool evaluator against a multi-defun source so
;; the mint-eval-file → image → boot pipeline rolls forward like the
;; real `src/nelisp-bootstrap.el' would, minus the file-I/O builtins
;; (require / load / file-name-directory) that the minimal Doc 44
;; interpreter does not yet ship.

(defun bm-add (a b) (+ a b))

(defun bm-fact (n)
  (if (= n 0)
      1
    (* n (bm-fact (- n 1)))))

(defun bm-fib (n)
  (if (< n 2)
      n
    (+ (bm-fib (- n 1)) (bm-fib (- n 2)))))

(defun bm-apply-twice (f x)
  (funcall f (funcall f x)))

;; Last form is the file's value — Doc 47 mint-eval-file treats it
;; as the result that goes into the image.  bm-fib(5) = 5, then
;; bm-fact(5) = 120.  bm-add and bm-apply-twice are exercised
;; further down so the file is not a single trivial expression.

(bm-add 1 (bm-add 2 (bm-fact (bm-fib 5))))
