;; Phase 2 Week 1-2 fixture: a slightly larger NeLisp program that
;; defines a higher-order helper (`my-foldl') in pure NeLisp and uses
;; it through `funcall' to recover sum / max / length on a list.  The
;; goal is to demonstrate that non-trivial NeLisp source — closures,
;; recursion, function-valued arguments — runs cleanly from disk.

(defun my-foldl (fn init seq)
  (if (null seq)
      init
    (my-foldl fn (funcall fn init (car seq)) (cdr seq))))

(defun my-sum (lst)
  (my-foldl (function +) 0 lst))

(defun my-max (lst)
  (my-foldl (function max) (car lst) (cdr lst)))

(defun my-length (lst)
  (my-foldl (lambda (acc _) (+ acc 1)) 0 lst))

(defvar *sample* (list 3 1 4 1 5 9 2 6))

(defvar *sample-sum*    (my-sum    *sample*))   ; 31
(defvar *sample-max*    (my-max    *sample*))   ;  9
(defvar *sample-length* (my-length *sample*))   ;  8
