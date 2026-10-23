;;; integ-wh-gc-builders.el --- string builders that call back into Lisp while a collection may run  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp, normally and after (nelisp--debug-switch 70), which makes every safepoint poll collect.
;; GNU forbids a raw string-data pointer that is the only reference to its string across an allocation or a call back
;; into Lisp; the builders of this runtime keep their partial result in a box that holds one.  Each case builds a string
;; while a Lisp function allocates and loops (so polls happen), and compares the result with the same string built
;; without the callback.  A string freed too early shows as a wrong result or a crash.
;; Prints GC-BUILDERS PASS N or GC-BUILDERS FAIL N and the failing cases.

(defvar iwh-gb-fail 0)
(defvar iwh-gb-n 0)
(defun iwh-gb-check (name got want)
  (setq iwh-gb-n (1+ iwh-gb-n))
  (unless (equal got want)
    (setq iwh-gb-fail (1+ iwh-gb-fail))
    (princ (format "FAIL %s: got %S want %S\n" name (if (stringp got) (substring got 0 (min 60 (length got))) got)
                   (if (stringp want) (substring want 0 (min 60 (length want))) want)))))
(defun iwh-gb-churn (k)
  "Allocate strings, conses and vectors and loop, so that polls happen; answer a string."
  (let ((i 0) (acc nil))
    (while (< i 20)
      (push (make-string (+ 5 (% (+ i k) 17)) (+ 97 (% (+ i k) 26))) acc)
      (setq i (1+ i)))
    (vector (length acc) (apply #'concat (nreverse acc)))
    (number-to-string k)))

(let ((n 60) (k 0) want got)
  ;; mapconcat with a function that churns
  (setq want (mapconcat #'number-to-string (number-sequence 0 (1- n)) ","))
  (setq got (mapconcat (lambda (i) (iwh-gb-churn i)) (number-sequence 0 (1- n)) ","))
  (iwh-gb-check "mapconcat churn" got want)
  ;; format with arguments produced by churning functions
  (setq k 0)
  (while (< k 40)
    (iwh-gb-check (format "format %d" k)
                  (format "<%s|%s|%S|%d>" (iwh-gb-churn k) (iwh-gb-churn (1+ k)) (iwh-gb-churn (+ k 2)) k)
                  (format "<%s|%s|%S|%d>" (number-to-string k) (number-to-string (1+ k)) (number-to-string (+ k 2)) k))
    (setq k (1+ k)))
  ;; concat of churned parts
  (setq want (apply #'concat (mapcar #'number-to-string (number-sequence 0 (1- n)))))
  (setq got (apply #'concat (mapcar (lambda (i) (iwh-gb-churn i)) (number-sequence 0 (1- n)))))
  (iwh-gb-check "apply concat mapcar churn" got want)
  ;; with-output-to-string with princ of churned strings
  (setq want (apply #'concat (mapcar #'number-to-string (number-sequence 0 (1- n)))))
  (setq got (with-output-to-string (let ((i 0)) (while (< i n) (princ (iwh-gb-churn i)) (setq i (1+ i))))))
  (iwh-gb-check "with-output-to-string" got want)
  ;; prin1-to-string of a structure holding strings, then the same after churn
  (let* ((data (list "alpha" (vector "beta" (list "gamma" (make-string 40 ?z))) (cons "delta" "epsilon")))
         (want (prin1-to-string data)))
    (setq k 0)
    (while (< k 30)
      (iwh-gb-churn k)
      (iwh-gb-check (format "prin1-to-string %d" k) (prin1-to-string data) want)
      (setq k (1+ k))))
  ;; replace-regexp-in-string with a churning function
  (setq want (replace-regexp-in-string "[0-9]+" "N" "a1b22c333d4444e55555"))
  (setq got (replace-regexp-in-string "[0-9]+" (lambda (m) (iwh-gb-churn 1) "N") "a1b22c333d4444e55555"))
  (iwh-gb-check "replace-regexp-in-string fn" got want)
  ;; string-join and mapconcat of generated strings with a multibyte separator
  (setq want (mapconcat #'number-to-string (number-sequence 0 29) "日"))
  (setq got (mapconcat (lambda (i) (iwh-gb-churn i)) (number-sequence 0 29) "日"))
  (iwh-gb-check "mapconcat multibyte separator" got want)
  ;; read-from-string of a large form while churning
  (let* ((form (cons 'list (mapcar #'number-to-string (number-sequence 0 99)))) (text (prin1-to-string form)))
    (setq k 0)
    (while (< k 10)
      (iwh-gb-churn k)
      (iwh-gb-check (format "read-from-string %d" k) (car (read-from-string text)) form)
      (setq k (1+ k))))
  ;; builders interleaved: a string built from pieces of another built string
  (setq k 0)
  (while (< k 30)
    (let* ((a (iwh-gb-churn k)) (b (concat a "-" (iwh-gb-churn (1+ k)))) (c (substring b 0 (min 4 (length b)))))
      (iwh-gb-check (format "pieces %d" k) (concat c "|" (format "%s" b)) (concat (substring (concat (number-to-string k) "-" (number-to-string (1+ k))) 0 (min 4 (length (concat (number-to-string k) "-" (number-to-string (1+ k)))))) "|" (concat (number-to-string k) "-" (number-to-string (1+ k))))))
    (setq k (1+ k))))

(princ (if (= iwh-gb-fail 0) (format "GC-BUILDERS PASS %d\n" iwh-gb-n) (format "GC-BUILDERS FAIL %d of %d\n" iwh-gb-fail iwh-gb-n)))
