;;; standalone-dotimes-check.el --- `dotimes' is GNU 31's macro -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-dotimes-check.el
;;
;; The evaluator used to match `dotimes' by name as a native special form
;; and run it as (mapc (lambda (VAR) BODY...) (number-sequence 0 (1- N))):
;; a closure call and a consed list per iteration, about 35 us each, and
;; it shadowed every `dotimes' macro.  `dotimes' is now the prelude macro
;; with GNU 31's expansion: a hidden counter, VAR freshly bound per
;; iteration.  Expected values are GNU 31.1's.  Prints one DOTIMES line
;; and exits non-zero on failure.

(let ((fails 0))
  (dolist (c (list
              (cons (let (r) (dotimes (i 3) (push i r)) r) '(2 1 0))
              (cons (dotimes (i 3 i)) 3)
              ;; Assigning VAR does not change the iteration count.
              (cons (let ((n 0)) (dotimes (i 5) (setq i 10) (setq n (1+ n))) n) 5)
              ;; Each closure captures its own iteration's binding.
              (cons (mapcar #'funcall (let (r) (dotimes (i 3) (push (lambda () i) r)) r))
                    '(2 1 0))
              (cons (dotimes (_ 0) 'x) nil)
              (cons (let ((k 0)) (dotimes (i 4 (* k 10)) (setq k (+ k i)))) 60)
              ;; The expansion is a plain loop, not a call to a special form.
              (cons (eq (car (macroexpand '(dotimes (i 2) i))) 'let) t)))
    (unless (equal (car c) (cdr c))
      (setq fails (1+ fails))
      (princ (format "DOTIMES FAIL got %S want %S\n" (car c) (cdr c)))))
  (princ (format "DOTIMES failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
