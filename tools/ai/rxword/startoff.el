;;; startoff.el --- string-match cost versus start offset on a 1.26M-char multibyte string  -*- lexical-binding: t; -*-
;; Prints "SO label us@1000 us@900000 ratio"; ratio = cost at the far start / cost at the near start.
(defvar so-big (apply #'concat (make-list 140000 "日本語abc\n")))
;; Word-boundary rows use a text whose boundary neighbours are ASCII (a non-ASCII neighbour makes the
;; native matcher decline to the Lisp matcher, which is a different and much slower path).
(defvar so-bigw (apply #'concat (make-list 140000 "日本語 abc\n")))
(defun so-us (re st reps big)
  (string-match re big st) (string-match re big st)
  (let ((s (float-time)))
    (dotimes (i reps) (string-match re big (+ st (% i 7))))
    (/ (* 1e6 (- (float-time) s)) reps)))
(dolist (p '(("nonascii" "c\n日") ("ascii" "bc\n") ("word" "\\<abc\\>" so-bigw) ("wordsym" "\\_<abc\\_>" so-bigw)))
  (let* ((big (if (nth 2 p) (symbol-value (nth 2 p)) so-big))
         (near (so-us (nth 1 p) 1000 300 big)) (far (so-us (nth 1 p) 900000 300 big)))
    (princ (format "SO %s %.1f %.1f %.2f\n" (car p) near far (/ far near)))))
(kill-emacs 0)
