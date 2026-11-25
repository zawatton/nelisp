;;; par-late-special.el --- Special declarations at binding time -*- lexical-binding: t; -*-

(defun late-special-value (symbol)
  (if (boundp symbol) (symbol-value symbol) 'unbound))

;; Warm the same interpreted closures before and after the declaration, so
;; an evaluator that caches lexical binding decisions must invalidate them.
(defun late-special-parallel ()
  (let ((late-special-x 1) (y 9))
    (list late-special-x y (late-special-value 'late-special-x))))

(defun late-special-sequential ()
  (let* ((late-special-x 2) (y late-special-x))
    (list late-special-x y (late-special-value 'late-special-x))))

(dotimes (_ 300)
  (late-special-parallel)
  (late-special-sequential))
(prin1 (list (late-special-parallel) (late-special-sequential)))
(terpri)
(eval '(defvar late-special-x 'global) t)
(prin1 (list (late-special-parallel) (late-special-sequential)
             (symbol-value 'late-special-x)))
(terpri)

;; A declaration can also run during a value form. The value form must
;; run just once, and the declaration must affect the binding that follows.
(defvar late-special-count 0)
(defun late-special-declare (form)
  (setq late-special-count (1+ late-special-count))
  (eval form t)
  late-special-count)

(defun late-special-init-parallel ()
  (let ((late-special-p 10)
        (n (late-special-declare '(defvar late-special-p 'global))))
    (list late-special-p n (symbol-value 'late-special-p))))

(defun late-special-init-sequential ()
  (let* ((late-special-s (late-special-declare '(defvar late-special-s 'global)))
         (n (symbol-value 'late-special-s)))
    (list late-special-s n)))

(prin1 (list (late-special-init-parallel) (symbol-value 'late-special-p)
             (late-special-init-sequential) (symbol-value 'late-special-s)
             late-special-count))
(terpri)

;; The call at the end of the first iteration affects the second binding.
(defun late-special-loop ()
  (let ((pass 0) first second)
    (while (not (eq pass 2))
      (let ((late-special-loop-var 3))
        (if (eq pass 0)
            (setq first (late-special-value 'late-special-loop-var))
          (setq second (late-special-value 'late-special-loop-var))))
      (when (eq pass 0)
        (late-special-declare '(defvar late-special-loop-var 'global)))
      (setq pass (1+ pass)))
    (list first second (symbol-value 'late-special-loop-var))))

(prin1 (late-special-loop))
(terpri)
