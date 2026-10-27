;;; gcstress.el --- regalloc GC stress: corpus calls with a forced collection every N allocations -*- lexical-binding: t; -*-
;; No allocation-count GC trigger was found in the runtime, so the driver forces one: every
;; workload below allocates, and `ra-tick' runs `garbage-collect' after each N-th allocation
;; batch, while live objects are held only by locals and VM stack slots of the callers.
(load (expand-file-name "bc.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(defvar ra-n 0)
(defun ra-tick () (setq ra-n (1+ ra-n)) (when (= (% ra-n 7) 0) (garbage-collect)))
(defun ra-keep (k)
  (let ((held (list (make-string 5 ?k) (cons k k) (vector k (1+ k)))) (out nil))
    (dotimes (i 40)
      (push (ra-mix 5) out) (ra-tick)
      (push (ra-stack i) out) (ra-tick)
      (push (ra-deep i 1 2 3 4 5 6 7) out) (ra-tick)
      (push (ra-switch (% i 10)) out))
    (list held (length out) (nth 3 out) (nth 50 out) (car (last out)))))
(let ((r nil))
  (dotimes (k 30) (push (ra-keep k) r))
  (princ (format "gcstress %S\n" (list (length r) (car r) (nth 29 r) (ra-gc 4000) (ra-sum 5000 [1 2 3] 3)))))
(kill-emacs 0)
