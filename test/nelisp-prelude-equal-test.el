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
  ;; c552d6bbb split the former `nelisp--equal-reference' into the
  ;; `nelisp--equal-container-p' .. `nelisp--equal-reference' family.
  (re-search-forward "^(defun nelisp--equal-container-p ")
  (goto-char (match-beginning 0))
  (let ((end (save-excursion
               (re-search-forward "^(defun nelisp--equal-reference ")
               (forward-sexp 1)
               (point))))
    (while (< (point) end)
      (eval (read (current-buffer)) t)
      (skip-chars-forward " \t\n"))))

;; Model the native primitive's conservative answers on the host.  Opaque
;; arrays need identity; their structural comparison must exercise the Lisp.
(defun nelisp-prelude-equal-test--native (a b)
  (cond ((and (consp a) (consp b))
         (and (nelisp-prelude-equal-test--native (car a) (car b))
              (nelisp-prelude-equal-test--native (cdr a) (cdr b))))
        ((or (stringp a) (symbolp a) (numberp a)) (equal a b))
        ;; The native comparator owns compiled and interpreted closures
        ;; (the Lisp reference no longer walks their slots; c552d6bbb).
        ((or (byte-code-function-p a)
             (and (fboundp 'interpreted-function-p) (interpreted-function-p a)))
         (equal a b))
        (t (eq a b))))

(defmacro nelisp-prelude-equal-test--with-native (&rest body)
  `(cl-letf (((symbol-function 'nelisp--native-equal)
              #'nelisp-prelude-equal-test--native)
             ((symbol-function 'nelisp--native-eq) #'eq)
             ((symbol-function 'nelisp-overlay-p) #'overlayp)
             ((symbol-function 'nelisp-ov--buffer) #'overlay-buffer)
             ((symbol-function 'nelisp-ov--start) #'overlay-start)
             ((symbol-function 'nelisp-ov--stop) #'overlay-end)
             ((symbol-function 'nelisp-ov--plist) #'overlay-properties)
             ((symbol-function 'nelisp-marker-p) #'markerp)
             ((symbol-function 'nelisp-marker-buffer) #'marker-buffer)
             ((symbol-function 'nelisp-marker-position) #'marker-position))
     ,@body))

(ert-deftest nelisp-prelude-equal/byte-code-identical ()
  (nelisp-prelude-equal-test--with-native
   (should (nelisp--equal-reference
            (make-byte-code 514 (unibyte-string 1 135) [] 3)
            (make-byte-code 514 (unibyte-string 1 135) [] 3)))))

(ert-deftest nelisp-prelude-equal/byte-code-every-slot-and-length ()
  (nelisp-prelude-equal-test--with-native
   (let ((slots (list 514 (unibyte-string 1 135) [7] 3 "doc" "p"))
         (different (list 513 (unibyte-string 0 135) [8] 4 "other" "r")))
     (dotimes (i 6)
       (let ((changed (copy-sequence slots)))
         (setcar (nthcdr i changed) (nth i different))
         (should-not (nelisp--equal-reference
                      (apply #'make-byte-code slots)
                      (apply #'make-byte-code changed)))))
     (should-not (nelisp--equal-reference
                  (apply #'make-byte-code slots)
                  (apply #'make-byte-code (butlast slots)))))))

(ert-deftest nelisp-prelude-equal/byte-code-nested ()
  (nelisp-prelude-equal-test--with-native
   (should (nelisp--equal-reference
            (list (vector (make-byte-code 514 (unibyte-string 1 135) [[7]] 3)))
            (list (vector (make-byte-code 514 (unibyte-string 1 135) [[7]] 3)))))))

(ert-deftest nelisp-prelude-equal/array-kinds-stay-distinct ()
  (nelisp-prelude-equal-test--with-native
   (should-not (nelisp--equal-reference
                (make-byte-code 514 (unibyte-string 1 135) [] 3)
                (vector 514 (unibyte-string 1 135) [] 3)))
   (should-not (nelisp--equal-reference (bool-vector t nil) [t nil]))))

(ert-deftest nelisp-prelude-equal/native-comparator-only-sees-atoms ()
  "The Lisp reference walks conses itself and delegates only leaves."
  (let ((seen nil))
    (cl-letf (((symbol-function 'nelisp--native-equal)
               (lambda (a b) (push a seen) (equal a b)))
              ((symbol-function 'nelisp--native-eq) #'eq))
      (should (nelisp--equal-reference
               (list (list (copy-sequence "a") 2) (list (copy-sequence "c") 4))
               (list (list (copy-sequence "a") 2) (list (copy-sequence "c") 4))))
      (should seen)
      (should-not (cl-some #'consp seen)))))

(ert-deftest nelisp-prelude-equal/list-tail-is-iterative ()
  (nelisp-prelude-equal-test--with-native
   (let ((a nil) (b nil) (attempts nil) (max-lisp-eval-depth 200))
     (dotimes (_ 512) (push (vector 7) a) (push (vector 7) b))
     (cl-letf (((symbol-function 'nelisp--native-equal)
                (lambda (left right)
                  (if (consp left)
                      (progn (push left attempts) nil)
                    (nelisp-prelude-equal-test--native left right)))))
       ;; The walk is iterative (depth 200 < 512 elements) and never hands a
       ;; cons to the native comparator.
       (should (nelisp--equal-reference a b))
       (should-not attempts)
       (setcar (nthcdr 511 b) [8])
       (should-not (nelisp--equal-reference a b))
       (should-not attempts)))))

(ert-deftest nelisp-prelude-equal/native-nil-keeps-marker-rules ()
  (nelisp-prelude-equal-test--with-native
   (with-temp-buffer
     (insert "abc")
     (let ((a (copy-marker 2)) (b (copy-marker 2 t)))
       (should (nelisp--equal-reference (list a) (list b)))
       (set-marker a nil) (set-marker b nil)
       (should (nelisp--equal-reference a b))))))

(ert-deftest nelisp-prelude-equal/interpreted-closure-slots ()
  (skip-unless (fboundp 'make-interpreted-closure))
  (nelisp-prelude-equal-test--with-native
   (let ((a (make-interpreted-closure '(x) '((+ x y)) '((y . 7))))
         (b (make-interpreted-closure '(x) '((+ x y)) '((y . 7))))
         (c (make-interpreted-closure '(x) '((- x y)) '((y . 7)))))
     (should (nelisp--equal-reference a b))
     (should-not (nelisp--equal-reference a c)))))

(provide 'nelisp-prelude-equal-test)
;;; nelisp-prelude-equal-test.el ends here
