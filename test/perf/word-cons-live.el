;;; word-cons-live.el --- Retained runtime conses -*- lexical-binding: t; -*-
;; Includes the keeper vector's eight-byte slot per object. Drop the keeper
;; and churn to distinguish reachable child boxes from a leak.
(nelisp--debug-switch 28)
(defun live () (garbage-collect) (garbage-collect) (nth 4 (nelisp--arena-stats)))
(defvar keep nil)
(let* ((a (live)))
  (setq keep (make-vector 40000 nil))
  (dotimes (i 40000) (aset keep i (cons nil nil)))
  (let ((b (live)))
    (setq keep nil)
    (let ((c (live)))
      (dotimes (r 5) (let ((v (make-vector 40000 nil))) (dotimes (i 40000) (aset v i (cons i nil)))))
      (let ((d (live)))
        (princ (format "LEAK per-cons-live=%d after-drop-delta=%d after-5x-churn-delta=%d\n"
                       (/ (- b a) 40000) (- c a) (- d a)))))))
