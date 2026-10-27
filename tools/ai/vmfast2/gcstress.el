;;; gcstress.el --- vmfast2 GC stress: byte-code callbacks of walks with forced collections -*- lexical-binding: t; -*-
;; Callbacks run in the walkers' reserved frames (the lean frame); every workload allocates, and
;; `v2s-tick' runs `garbage-collect' after each 7th callback while the live objects are held only
;; by callback frames, the walkers' rooted slots and the callers' VM stacks.
(load (expand-file-name "bc.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(defvar v2s-n 0)
(defun v2s-tick () (setq v2s-n (1+ v2s-n)) (when (= (% v2s-n 7) 0) (garbage-collect)))
(defun v2s-keep (k)
  (let* ((held (list (make-string 5 ?k) (cons k k) (vector k (1+ k))))
         (a (mapcar (lambda (x) (v2s-tick) (v2-cons (+ x k))) (number-sequence 1 30)))
         (b (let ((h (make-hash-table)) (acc nil))
              (dotimes (i 20) (puthash i (list i (* i k)) h))
              (maphash (lambda (key v) (v2s-tick) (push (cons key (copy-sequence v)) acc)) h)
              (sort acc (lambda (x y) (< (car x) (car y))))))
         (c (mapcar #'v2-gc (number-sequence 1 120)))
         (d (let ((acc 0)) (mapc (lambda (x) (v2s-tick) (setq acc (+ acc (length (v2-calls-list x))))) '(1 2 3 4)) acc)))
    (list held (length a) (nth 7 a) (nth 19 b) (length c) (nth 99 c) d)))
(defun v2-calls-list (x) (make-list (v2-calls x) x))
(let ((r nil))
  (dotimes (k 25) (push (v2s-keep k) r))
  (princ (format "gcstress %S\n" (list (length r) (car r) (nth 24 r) (v2-recur 7) (v2-deep 80)))))
(kill-emacs 0)
