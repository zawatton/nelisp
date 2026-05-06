;;; nelisp-stdlib.el --- Sweep 9 S0 Elisp stdlib (Rust→Elisp migration)  -*- lexical-binding: t; -*-

(defun identity (x) x)
(defun null (x) (eq x nil))
(defun not (x) (eq x nil))
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

;; Rust-min batch 6l (2026-05-06): `mod' migrated from Rust to
;; elisp.  Reproduces the previous `bi_mod' contract exactly:
;;   r = euclidean_mod(a, |b|)   (always >= 0)
;;   result = sign(b) * r
;; Built from `/' (NeLisp int-div = trunc toward zero) plus a
;; sign-adjust step.  This matches NeLisp's prior Rust semantics,
;; not host Emacs's pure floor-mod — the two differ only when
;; sign(a) != sign(b), and a codebase grep confirmed no extant
;; caller passes a negative divisor.
(defun mod (a b)
  (when (= b 0) (error "Arithmetic error"))
  (let* ((n (if (< b 0) (- b) b))
         (r (- a (* n (/ a n)))))
    (when (< r 0) (setq r (+ r n)))
    (if (< b 0) (- r) r)))

;; Rust-min batch 6j (2026-05-06): variadic bitwise fold via 2-arg
;; primitives `nelisp--logior2' / -logand2 / -logxor2.  Elisp
;; folds over INTS with the identity element of each operation
;; (= 0 for OR/XOR, -1 for AND).  54 callers in the substrate are
;; all exactly 2-arg, so the fold path is fast in practice.
(defun logior (&rest ints)
  (let ((acc 0) (cur ints))
    (while cur
      (setq acc (nelisp--logior2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

(defun logand (&rest ints)
  (let ((acc -1) (cur ints))
    (while cur
      (setq acc (nelisp--logand2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

(defun logxor (&rest ints)
  (let ((acc 0) (cur ints))
    (while cur
      (setq acc (nelisp--logxor2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

;; nelisp-stdlib.el ends here
