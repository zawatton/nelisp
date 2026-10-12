;;; standalone-call-overhead-check.el --- interpreted call cost ratios -*- lexical-binding: t -*-

;; Usage: RT_CHECK=let|steady|wrappers nelisp --load test/standalone-call-overhead-check.el
;;
;; Times 20000 calls per shape inside a function (top-level loop forms
;; re-expand their macros each iteration), subtracts the empty loop and
;; compares ratios, which hold across machines and load:
;;   let       a two-binding let <= 2.2 x a one-binding let (net of the
;;             empty loop); measured 39 vs 11 us at baseline
;;   steady    a two-binding let loop of 100000 iterations costs <= 1.3 x
;;             per iteration what 1000 iterations do (heap growth must
;;             not slow allocation down)
;;   wrappers  `substring' (an interpreted &optional closure) and `string='
;;             (a byte-code function) on strings <= 2 x the native arm
;; Prints one CALL-OVERHEAD line and exits non-zero on failure.

(defun call-oh--steady (n)
  (let ((i 0))
    (while (< i n) (let ((x 1) (y 2)) x y) (setq i (1+ i)))))
(defun call-oh--per-iter (n)
  (garbage-collect)
  (let ((t0 (float-time)))
    (call-oh--steady n)
    (/ (- (float-time) t0) n)))
(defvar call-oh--s "abcdefghij/klmnop")

(defmacro call-oh--time (&rest body)
  `(progn
     (defalias 'call-oh--fn
       (lambda ()
         (let ((a call-oh--s) (i 0))
           (ignore a)
           (while (< i 20000) ,@body (setq i (1+ i))))))
     (garbage-collect)
     (let ((t0 (float-time)))
       (call-oh--fn)
       (- (float-time) t0))))

(defun call-oh--best (thunk)
  ;; Best of three keeps a stray GC or scheduler pause out of the ratio.
  (min (funcall thunk) (funcall thunk) (funcall thunk)))

(let* ((empty (call-oh--best (lambda () (call-oh--time nil))))
       (net (lambda (thunk) (max 1e-6 (- (call-oh--best thunk) empty))))
       (check (or (getenv "RT_CHECK") "let"))
       (pairs
        (cond
         ((equal check "let")
          (list (list "let2/let1"
                      (funcall net (lambda () (call-oh--time (let ((x 1) (y 2)) x y))))
                      (funcall net (lambda () (call-oh--time (let ((x 1)) x))))
                      2.2)))
         ((equal check "steady")
          (list (list "100000/1000"
                      ;; Scaled to 20000 iterations like the other rows.
                      (* 20000 (call-oh--per-iter 100000))
                      (* 20000 (min (call-oh--per-iter 1000) (call-oh--per-iter 1000)))
                      1.3)))
         ((equal check "wrappers")
          (list (list "substring/native"
                      (funcall net (lambda () (call-oh--time (substring a 3 9))))
                      (funcall net (lambda () (call-oh--time (nelisp--native-substring a 3 9))))
                      2.0)
                (list "string=/native"
                      (funcall net (lambda () (call-oh--time (string= a a))))
                      (funcall net (lambda () (call-oh--time (nelisp--native-equal a a))))
                      2.0)))))
       (ok (and pairs t)))
  (dolist (p pairs)
    (let ((ratio (/ (nth 1 p) (nth 2 p))))
      (unless (<= ratio (nth 3 p)) (setq ok nil))
      (princ (format "CALL-OVERHEAD %s %.2f (limit %.1f; %.2f vs %.2f us/call)\n"
                     (nth 0 p) ratio (nth 3 p)
                     (/ (* 1e6 (nth 1 p)) 20000) (/ (* 1e6 (nth 2 p)) 20000)))))
  (princ (format "CALL-OVERHEAD %s %s\n" check (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-call-overhead-check.el ends here
