;;; nelisp-aot-switch-sym-smoke.el --- macroexp--expand-all switch arm -*- lexical-binding: t; -*-
;; Run on the standalone binary: target/nelisp --load THIS (prints PASS/FAIL lines, exit 1 on failure).
;; `macroexp--expand-all' dispatches the form head through one `switch' on an eq table
;; (cond condition-case defconst defvar function quote let* let while unwind-protect
;; setq funcall); its AOT row takes `wf_aot_switch_sym' first.  Each case covers a hit
;; arm, a miss (default arm), a non-symbol head and an uninterned head.
(defvar swt--fail 0)
(defun swt--check (name form expected)
  (let ((got (condition-case e (macroexpand-all form) (error (list 'ERR e)))))
    (if (equal got expected) (princ (format "PASS %s\n" name))
      (setq swt--fail (1+ swt--fail))
      (princ (format "FAIL %s: got %S want %S\n" name got expected)))))
(swt--check 'quote '(quote (when a b)) '(quote (when a b)))
(swt--check 'function '(function (lambda (x) (when x 1))) '#'(lambda (x) (if x (progn 1))))
(swt--check 'cond '(cond ((when a b))) '(cond ((if a (progn b)))))
(swt--check 'let '(let ((a (when x 1))) (unless a 2)) '(let ((a (if x (progn 1)))) (if a nil 2)))
(swt--check 'let* '(let* ((a (when x 1))) a) '(let* ((a (if x (progn 1)))) a))
(swt--check 'setq '(setq a (when x 1)) '(setq a (if x (progn 1))))
(swt--check 'while '(while (when x 1) (unless y 2)) '(while (if x (progn 1)) (if y nil 2)))
(swt--check 'unwind '(unwind-protect (when a b) (unless c d)) '(unwind-protect (if a (progn b)) (if c nil d)))
(swt--check 'funcall '(funcall (when a b) (when c d)) '(funcall (if a (progn b)) (if c (progn d))))
(swt--check 'cc '(condition-case e (when a b) (error (when c d))) '(condition-case e (if a (progn b)) (error (if c (progn d)))))
(swt--check 'defvar '(defvar v (when a b)) '(defvar v (if a (progn b))))
(swt--check 'defconst '(defconst v (when a b) "d") '(defconst v (if a (progn b)) "d"))
(swt--check 'miss-call '(foo (when a b) 1) '(foo (if a (progn b)) 1))
(swt--check 'miss-many '(list (car x) (cdr y) (+ 1 2) (when a b)) '(list (car x) (cdr y) (+ 1 2) (if a (progn b))))
(swt--check 'lambda-head '((lambda (x) (when x 1)) 2) '((lambda (x) (if x (progn 1))) 2))
(let ((u (make-symbol "quote")))
  (swt--check 'uninterned (list u '(when a b)) (list u '(if a (progn b)))))
(swt--check 'nil-head '(nil) '(nil))
(swt--check 'int-atom 5 5)
(princ (format "swt-failures=%d\n" swt--fail))
(kill-emacs (if (= swt--fail 0) 0 1))
