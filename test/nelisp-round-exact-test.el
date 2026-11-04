;;; nelisp-round-exact-test.el --- exact 2-arg round  -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)

(defconst nelisp-round-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory)))))

(defun nelisp-round-test--extract (file name)
  "Return the (defun NAME ...) form read from FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file nelisp-round-test--root))
    (goto-char (point-min))
    (re-search-forward (format "^(defun %s " (regexp-quote name)))
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(defun nelisp-round-test--load ()
  (let ((form (copy-sequence (nelisp-round-test--extract
                              "scripts/nelisp-stdlib-prelude.el" "round"))))
    (setcar (cdr form) 'nelisp-test--round)
    ;; Helpers: floor-div from the prelude, plus guard stubs.
    (eval (nelisp-round-test--extract "scripts/nelisp-stdlib-prelude.el"
                                      "nelisp--int-floor-div") t)
    (unless (fboundp 'nelisp--check-number)
      (defalias 'nelisp--check-number
        (lambda (x) (unless (numberp x) (signal 'wrong-type-argument (list 'numberp x))) x)))
    (eval form t)))

(nelisp-round-test--load)

(defun nelisp-round-test--chk (x d)
  (should (equal (nelisp-test--round x d) (round x d))))

(ert-deftest nelisp-round-exact-signs ()
  (dolist (x '(0 1 -1 7 -7 5 -5 3 -3 100 -100))
    (dolist (d '(1 -1 2 -2 3 -3 7 -7 4 -4))
      (nelisp-round-test--chk x d))))

(ert-deftest nelisp-round-exact-ties ()
  (dolist (c `((7 2) (-7 2) (5 2) (3 -2) (-5 -2) (1 2) (-1 2) (3 2) (-3 -2)
               (,(* 3 (expt 2 100)) ,(* 2 (expt 2 100)))
               (,(* 5 (expt 2 100)) ,(* 2 (expt 2 100)))
               (,(- (* 5 (expt 2 100))) ,(* 2 (expt 2 100)))
               (,(* 5 (expt 2 100)) ,(- (* 2 (expt 2 100))))))
    (nelisp-round-test--chk (car c) (cadr c))))

(ert-deftest nelisp-round-exact-bignum-row ()
  (let ((a (+ (expt 2 200) 12345)) (b (+ (expt 2 70) 99)))
    (dolist (s '(1 -1))
      (dolist (t2 '(1 -1))
        (nelisp-round-test--chk (* s a) (* t2 b))))))

(ert-deftest nelisp-round-exact-random ()
  (random "nelisp-round-exact-seed")
  (cl-flet ((rnd ()
              (let* ((bits (random 201))
                     (v (if (= bits 0) 0 (random (ash 1 bits)))))
                (if (= 0 (random 2)) v (- v)))))
    (dotimes (_ 2000)
      (let ((x (rnd)) (d (rnd)))
        (unless (= d 0) (nelisp-round-test--chk x d))))
    ;; exact-tie stress
    (dotimes (_ 200)
      (let* ((d (* 2 (1+ (random (ash 1 100))))) (k (1+ (* 2 (random (ash 1 60))))))
        (nelisp-round-test--chk (* k (/ d 2)) d)
        (nelisp-round-test--chk (- (* k (/ d 2))) d)))))
(provide 'nelisp-round-exact-test)
