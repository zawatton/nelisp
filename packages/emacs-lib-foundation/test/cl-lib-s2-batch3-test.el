;;; cl-lib-s2-batch3-test.el --- ERT for the cl-lib S2 coverage batch 3  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/cl-lib.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; S2 coverage batch 3 (2026-09-28): the pure `cl-lib' accessors/constants
;; ported verbatim in `src/cl-lib.el' (3/4-deep car/cdr accessors,
;; cl-fourth..cl-tenth, cl-acons, cl-pairlis, cl-constantly, cl--do-subst,
;; cl-copy-seq, cl-svref, cl-digit-char-table, the float-limit constants,
;; and the multiple-value/block/declaration stand-ins).  On host Emacs
;; these guard-skip to the genuine `cl-lib' (already preloaded), so the
;; expected values below double as the real-Emacs oracle for the same
;; assertions run against the standalone build
;; (`build/nemacs-bootstrap.el') by hand -- see
;; ~/.cache/tmp/nel-lib-s2c/logs/probe2.log for that run.

(ert-deftest cl-lib-s2-batch3-test/fboundp ()
  (dolist (sym '(cl-acons cl--block-throw cl--block-wrapper
                 cl-caaaar cl-caaadr cl-caaar cl-caadar cl-caaddr cl-caadr
                 cl-cadaar cl-cadadr cl-cadar cl-caddar cl-cadddr
                 cl-cdaaar cl-cdaadr cl-cdaar cl-cdadar cl-cdaddr cl-cdadr
                 cl-cddaar cl-cddadr cl-cddar cl-cdddar cl-cddddr cl-cdddr
                 cl--compiling-file cl-constantly cl-copy-seq
                 cl--defalias cl--do-subst cl-eighth cl-fifth cl-floatp-safe
                 cl-fourth cl-multiple-value-apply cl-multiple-value-call
                 cl-multiple-value-list cl-ninth cl-nth-value cl-pairlis
                 cl--set-buffer-substring cl--set-substring cl-seventh
                 cl-sixth cl-svref cl-tenth))
    (should (fboundp sym)))
  (dolist (sym '(cl--optimize-safety cl--optimize-speed
                 cl-custom-print-functions cl-digit-char-table
                 cl-float-epsilon cl-float-negative-epsilon
                 cl-least-negative-float cl-least-negative-normalized-float
                 cl-least-positive-float cl-least-positive-normalized-float
                 cl-most-negative-float cl-most-positive-float
                 cl--proclaims-deferred))
    (should (boundp sym))))

(ert-deftest cl-lib-s2-batch3-test/car-cdr-accessors ()
  ;; x = (((10 . 20) . (30 . 40)) . 999); car(x) = ((10.20).(30.40)),
  ;; so all 3-deep combinations of car/cdr on x land on a real value.
  (let ((x '(((10 . 20) . (30 . 40)) . 999)))
    (should (= (cl-caaar x) 10))
    (should (= (cl-cdaar x) 20))
    (should (= (cl-cadar x) 30))
    (should (= (cl-cddar x) 40)))
  (should (= (cl-caaaar '((((1))))) 1))
  (should (= (cl-cadddr '(a b c 4)) 4))
  (should (= (cl-cddddr '(1 2 3 4 . 5)) 5))
  ;; error case: a non-cons argument signals wrong-type-argument, same as
  ;; the built-in `car'/`cdr' it is built from.
  (should (eq (car (should-error (cl-caaar 5) :type 'wrong-type-argument))
              'wrong-type-argument)))

(ert-deftest cl-lib-s2-batch3-test/nth-accessors ()
  (let ((lst '(0 1 2 3 4 5 6 7 8 9)))
    (should (= (cl-fourth lst) 3))
    (should (= (cl-fifth lst) 4))
    (should (= (cl-sixth lst) 5))
    (should (= (cl-seventh lst) 6))
    (should (= (cl-eighth lst) 7))
    (should (= (cl-ninth lst) 8))
    (should (= (cl-tenth lst) 9)))
  ;; error case: a non-list argument signals wrong-type-argument via `nth'.
  (should (eq (car (should-error (cl-fourth 5) :type 'wrong-type-argument))
              'wrong-type-argument)))

(ert-deftest cl-lib-s2-batch3-test/acons-pairlis ()
  (should (equal (cl-acons 'k 'v '((x . y))) '((k . v) (x . y))))
  (should (equal (cl-pairlis '(a b) '(1 2)) '((a . 1) (b . 2))))
  (should (equal (cl-pairlis '(a b) '(1 2) '((z . 9))) '((a . 1) (b . 2) (z . 9))))
  ;; error case: too many values for keys signals wrong-type-argument from
  ;; the underlying `pop' on a non-cons nil tail is avoided by design (the
  ;; loop simply stops), so exercise the real error path instead: a
  ;; non-list KEYS argument.
  (should (eq (car (should-error (cl-pairlis 5 '(1)) :type 'wrong-type-argument))
              'wrong-type-argument)))

(ert-deftest cl-lib-s2-batch3-test/constantly-and-do-subst ()
  (should (= (funcall (cl-constantly 42)) 42))
  (should (= (funcall (cl-constantly 42) 1 2 3) 42))
  (should (equal (cl--do-subst 'new 'old '(a old (b old) old)) '(a new (b new) new))))

(ert-deftest cl-lib-s2-batch3-test/aliases-and-constants ()
  (should (equal (cl-copy-seq [1 2 3]) [1 2 3]))
  (should (= (cl-svref [10 20 30] 1) 20))
  (should (= (aref cl-digit-char-table ?7) 7))
  (should (= (aref cl-digit-char-table ?f) 15))
  (should (null (aref cl-digit-char-table ?!))))

(provide 'cl-lib-s2-batch3-test)

;;; cl-lib-s2-batch3-test.el ends here
