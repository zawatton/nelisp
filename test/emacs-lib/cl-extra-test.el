;;; cl-extra-test.el --- ERT for the cl-lib facade's cl-extra additions  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/cl-lib.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; S2 coverage batch (2026-09-28): a handful of pure `cl-extra' names --
;; predicate, mapping, property-list, integer-parsing, and random-number
;; helpers -- ported verbatim from GNU Emacs 31.1's
;; lisp/emacs-lisp/cl-extra.el.  See the commentary at their definitions
;; in `src/cl-lib.el' for why `cl--random-state''s default is created
;; lazily here rather than eagerly like upstream.  Values pinned against
;; real GNU Emacs 31.1.

(ert-deftest cl-extra-test/s2-batch-fboundp ()
  (dolist (sym '(cl-equalp cl--mapcar-many cl-map cl-mapl cl-concatenate
                 cl-nreconc cl-list-length cl--do-remf cl-remprop cl-get
                 cl-fresh-line cl-parse-integer cl--random-time
                 cl--make-random-state cl-make-random-state cl-random
                 cl-random-state-p))
    (should (fboundp sym)))
  (should (boundp 'cl--random-state)))

(ert-deftest cl-extra-test/cl-equalp-values ()
  (should (cl-equalp "AB" "ab"))
  (should (cl-equalp 1 1.0))
  (should (cl-equalp '("A" 1) '("a" 1.0)))
  (should (cl-equalp [1 2] [1.0 2.0]))
  (should-not (cl-equalp [1 2] [1.0 3.0])))

(ert-deftest cl-extra-test/mapping-helpers ()
  (should (equal (cl--mapcar-many #'+ '((1 2 3) (10 20 30)) t) '(11 22 33)))
  (should (equal (cl-map 'list #'1+ '(1 2 3)) '(2 3 4)))
  (should (equal (cl-map 'string #'1+ "abc") "bcd"))
  (should (equal (let (acc)
                   (cl-mapl (lambda (l) (push (car l) acc)) '(1 2 3))
                   (nreverse acc))
                 '(1 2 3)))
  (should (equal (cl-concatenate 'list '(1 2) '(3 4)) '(1 2 3 4)))
  (should (equal (cl-nreconc (list 1 2 3) (list 4 5)) '(3 2 1 4 5))))

(ert-deftest cl-extra-test/cl-list-length-values ()
  (should (= (cl-list-length '(1 2 3)) 3))
  ;; A circular list returns nil rather than signaling or hanging.
  (should (null (cl-list-length
                  (let ((c (list 1 2 3)))
                    (setcdr (cddr c) c)
                    c)))))

(ert-deftest cl-extra-test/property-list-helpers ()
  (put 'cl-extra-test--sym 'cl-extra-test--prop 42)
  (should (= (cl-get 'cl-extra-test--sym 'cl-extra-test--prop) 42))
  (should (eq (cl-get 'cl-extra-test--sym 'missing 'dflt) 'dflt))
  (cl-remprop 'cl-extra-test--sym 'cl-extra-test--prop)
  (should (eq (cl-get 'cl-extra-test--sym 'cl-extra-test--prop 'gone) 'gone))
  (should (equal (let ((pl (list 'x 1 'y 2 'z 3)))
                   (cl--do-remf pl 'y)
                   pl)
                 '(x 1 z 3))))

(ert-deftest cl-extra-test/cl-fresh-line-only-newlines-when-needed ()
  (should (equal (with-temp-buffer
                   (let ((standard-output (current-buffer)))
                     (princ "abc")
                     (cl-fresh-line)
                     (princ "|")
                     (buffer-string)))
                 "abc\n|"))
  ;; Already at the beginning of a line: no extra newline is inserted.
  (should (equal (with-temp-buffer
                   (let ((standard-output (current-buffer)))
                     (princ "abc\n")
                     (cl-fresh-line)
                     (princ "|")
                     (buffer-string)))
                 "abc\n|")))

(ert-deftest cl-extra-test/cl-parse-integer-values ()
  (should (= (cl-parse-integer "  42abc" :junk-allowed t) 42))
  (should (= (cl-parse-integer "-7") -7))
  (should (= (cl-parse-integer "ff" :radix 16) 255))
  (should (eq (car (should-error (cl-parse-integer "xyz"))) 'error)))

(ert-deftest cl-extra-test/random-number-helpers ()
  (let ((n (cl-random 100)))
    (should (and (>= n 0) (< n 100))))
  (let ((n (cl-random 1.0)))
    (should (and (>= n 0) (< n 1.0))))
  (should (cl-random-state-p (cl-make-random-state t)))
  ;; Two states copied from the same seed produce the same draws.
  (let ((s1 (cl-make-random-state 7))
        (s2 (cl-make-random-state 7)))
    (should (equal (list (cl-random 1000 s1) (cl-random 1000 s1))
                   (list (cl-random 1000 s2) (cl-random 1000 s2))))))

(provide 'cl-extra-test)

;;; cl-extra-test.el ends here
