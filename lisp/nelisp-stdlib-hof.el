;;; nelisp-stdlib-hof.el --- Sweep 9 G2 higher-order functions  -*- lexical-binding: t; -*-

(defun mapcar (fn seq)
  "Apply FN to each element of SEQ (list, vector, or string); collect results.
Doc 22 A6: arrays are iterated by index via `aref'/`length' (cons-cell
walking only works for lists)."
  (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
  (if (or (vectorp seq) (stringp seq))
      (let ((n (length seq)) (i 0) (acc nil))
        (while (< i n)
          (setq acc (cons (funcall fn (aref seq i)) acc))
          (setq i (1+ i)))
        (nreverse acc))
    (let ((acc nil))
      (while seq
        (setq acc (cons (funcall fn (car seq)) acc))
        (setq seq (cdr seq)))
      (nreverse acc))))

(defun mapc (fn seq)
  "Apply FN to each element of SEQ for side effect; return SEQ.
Doc 22 A6: arrays are iterated by index."
  (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
  (if (or (vectorp seq) (stringp seq))
      (let ((n (length seq)) (i 0))
        (while (< i n) (funcall fn (aref seq i)) (setq i (1+ i)))
        seq)
    (let ((orig seq))
      (while seq (funcall fn (car seq)) (setq seq (cdr seq))) orig)))

;; nelisp-stdlib-hof.el ends here
