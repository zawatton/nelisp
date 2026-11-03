;;; nelisp-prelude-equal-test.el --- structural equality regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)

(defconst nelisp-prelude-equal-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

;; Evaluate only the real comparator; leave the host's public equal intact.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "scripts/nelisp-stdlib-prelude.el" nelisp-prelude-equal-test--root))
  (goto-char (point-min))
  (re-search-forward "^(defun nelisp--equal-recursive ")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t))

;; Model the native primitive's conservative answers on the host.  Opaque
;; arrays need identity; their structural comparison must exercise the Lisp.
(defun nelisp-prelude-equal-test--native (a b)
  (cond ((and (consp a) (consp b))
         (and (nelisp-prelude-equal-test--native (car a) (car b))
              (nelisp-prelude-equal-test--native (cdr a) (cdr b))))
        ((or (stringp a) (symbolp a) (numberp a)) (equal a b))
        (t (eq a b))))

(defmacro nelisp-prelude-equal-test--with-native (&rest body)
  `(cl-letf (((symbol-function 'nelisp--native-equal)
              #'nelisp-prelude-equal-test--native)
             ((symbol-function 'nelisp-marker-p) #'markerp)
             ((symbol-function 'nelisp-marker-buffer) #'marker-buffer)
             ((symbol-function 'nelisp-marker-position) #'marker-position))
     ,@body))

(ert-deftest nelisp-prelude-equal/byte-code-identical ()
  (nelisp-prelude-equal-test--with-native
   (should (nelisp--equal-recursive
            (make-byte-code 514 (unibyte-string 1 135) [] 3)
            (make-byte-code 514 (unibyte-string 1 135) [] 3)))))

(ert-deftest nelisp-prelude-equal/byte-code-every-slot-and-length ()
  (nelisp-prelude-equal-test--with-native
   (let ((slots (list 514 (unibyte-string 1 135) [7] 3 "doc" "p"))
         (different (list 513 (unibyte-string 0 135) [8] 4 "other" "r")))
     (dotimes (i 6)
       (let ((changed (copy-sequence slots)))
         (setcar (nthcdr i changed) (nth i different))
         (should-not (nelisp--equal-recursive
                      (apply #'make-byte-code slots)
                      (apply #'make-byte-code changed)))))
     (should-not (nelisp--equal-recursive
                  (apply #'make-byte-code slots)
                  (apply #'make-byte-code (butlast slots)))))))

(ert-deftest nelisp-prelude-equal/byte-code-nested ()
  (nelisp-prelude-equal-test--with-native
   (should (nelisp--equal-recursive
            (list (vector (make-byte-code 514 (unibyte-string 1 135) [[7]] 3)))
            (list (vector (make-byte-code 514 (unibyte-string 1 135) [[7]] 3)))))))

(ert-deftest nelisp-prelude-equal/array-kinds-stay-distinct ()
  (nelisp-prelude-equal-test--with-native
   (should-not (nelisp--equal-recursive
                (make-byte-code 514 (unibyte-string 1 135) [] 3)
                (vector 514 (unibyte-string 1 135) [] 3)))
   (should-not (nelisp--equal-recursive (bool-vector t nil) [t nil]))))

(ert-deftest nelisp-prelude-equal/native-true-skips-list-walk ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp--native-equal)
               (lambda (a b) (setq calls (1+ calls)) (equal a b))))
      (should (nelisp--equal-recursive (list '(a b) '(c d))
                                      (list '(a b) '(c d))))
      (should (= calls 1)))))

(ert-deftest nelisp-prelude-equal/list-tail-is-iterative ()
  (nelisp-prelude-equal-test--with-native
   (let ((a nil) (b nil) (attempts nil) (max-lisp-eval-depth 200))
     (dotimes (_ 512) (push (vector 7) a) (push (vector 7) b))
     (cl-letf (((symbol-function 'nelisp--native-equal)
                (lambda (left right)
                  (if (consp left)
                      (progn (push left attempts) nil)
                    (nelisp-prelude-equal-test--native left right)))))
       (should (nelisp--equal-recursive a b))
       (should (= (length attempts) 1))
       (should (eq (car attempts) a))
       (setq attempts nil)
       (setcar (nthcdr 511 b) [8])
       (should-not (nelisp--equal-recursive a b))
       (should (= (length attempts) 1))))))

(ert-deftest nelisp-prelude-equal/native-nil-keeps-marker-rules ()
  (nelisp-prelude-equal-test--with-native
   (with-temp-buffer
     (insert "abc")
     (let ((a (copy-marker 2)) (b (copy-marker 2 t)))
       (should (nelisp--equal-recursive (list a) (list b)))
       (set-marker a nil) (set-marker b nil)
       (should (nelisp--equal-recursive a b))))))

(ert-deftest nelisp-prelude-equal/interpreted-closure-slots ()
  (skip-unless (fboundp 'make-interpreted-closure))
  (nelisp-prelude-equal-test--with-native
   (let ((a (make-interpreted-closure '(x) '((+ x y)) '((y . 7))))
         (b (make-interpreted-closure '(x) '((+ x y)) '((y . 7))))
         (c (make-interpreted-closure '(x) '((- x y)) '((y . 7)))))
     (should (nelisp--equal-recursive a b))
     (should-not (nelisp--equal-recursive a c)))))

(provide 'nelisp-prelude-equal-test)
;;; nelisp-prelude-equal-test.el ends here
