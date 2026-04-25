;;; nelisp-special-forms-test.el --- Phase 7+B special-forms ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; ERT pin for Doc 40 §3.B Phase 7+B basic special-forms.
;; Covers the T143 core 11 (quote/function/setq/if/cond/and/or/progn/
;; let/let*/lambda) and the §3.B additional 11 (save-* family,
;; setq-default/local, eval-*-compile, let-when-compile, inline-let,
;; declare).  Aim: 40+ tests, byte-compile clean, regression-free.

;;; Code:

(require 'ert)
(require 'nelisp-special-forms)

(defmacro nelisp-sf-test--with-fresh-state (&rest body)
  "Run BODY after clearing all Phase 7+B globals."
  (declare (indent 0))
  `(progn
     (nelisp-special-forms-reset)
     (unwind-protect
         (progn ,@body)
       (nelisp-special-forms-reset))))

(defalias 'nelisp-sf-eval 'nelisp-special-forms-eval)
(defalias 'nelisp-sf-funcall 'nelisp-special-forms-funcall)
(defalias 'nelisp-sf-apply 'nelisp-special-forms-apply)


;;; -----------------------------------------------------------------
;;; Self-evaluating + lookup
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-self-eval-integer ()
  (should (= (nelisp-sf-eval 42) 42)))

(ert-deftest nelisp-sf-self-eval-string ()
  (should (equal (nelisp-sf-eval "hi") "hi")))

(ert-deftest nelisp-sf-self-eval-nil-and-t ()
  (should (eq (nelisp-sf-eval nil) nil))
  (should (eq (nelisp-sf-eval t) t)))

(ert-deftest nelisp-sf-self-eval-keyword ()
  (should (eq (nelisp-sf-eval :foo) :foo)))

(ert-deftest nelisp-sf-lookup-from-env ()
  (should (= (nelisp-sf-eval 'x '((x . 7))) 7)))

(ert-deftest nelisp-sf-lookup-unbound ()
  (nelisp-sf-test--with-fresh-state
    (should-error (nelisp-sf-eval 'undefined-x)
                  :type 'nelisp-special-forms-unbound-variable)))


;;; -----------------------------------------------------------------
;;; quote / function
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-quote-symbol ()
  (should (eq (nelisp-sf-eval '(quote a)) 'a)))

(ert-deftest nelisp-sf-quote-list ()
  (should (equal (nelisp-sf-eval '(quote (1 2 3))) '(1 2 3))))

(ert-deftest nelisp-sf-quote-self-evaluating ()
  (should (= (nelisp-sf-eval '(quote 5)) 5)))

(ert-deftest nelisp-sf-function-lambda-yields-closure ()
  (let ((c (nelisp-sf-eval '(function (lambda (x) x)))))
    (should (nelisp-special-forms--closure-p c))))

(ert-deftest nelisp-sf-function-symbol-resolves ()
  (nelisp-sf-test--with-fresh-state
    ;; host fboundp fallback
    (should (functionp (nelisp-sf-eval '(function +))))))


;;; -----------------------------------------------------------------
;;; if / cond
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-if-then ()
  (should (= (nelisp-sf-eval '(if t 1 2)) 1)))

(ert-deftest nelisp-sf-if-else ()
  (should (= (nelisp-sf-eval '(if nil 1 2)) 2)))

(ert-deftest nelisp-sf-if-else-progn ()
  (should (= (nelisp-sf-eval '(if nil 1 (quote 7) (quote 9))) 9)))

(ert-deftest nelisp-sf-if-no-else ()
  (should (eq (nelisp-sf-eval '(if nil 1)) nil)))

(ert-deftest nelisp-sf-cond-multi-clause ()
  (should (eq (nelisp-sf-eval '(cond (nil 1) (t 'two))) 'two)))

(ert-deftest nelisp-sf-cond-test-only ()
  ;; Clause without body returns the test value.
  (should (= (nelisp-sf-eval '(cond (5) (t 1))) 5)))

(ert-deftest nelisp-sf-cond-no-match ()
  (should (eq (nelisp-sf-eval '(cond (nil 1) (nil 2))) nil)))

(ert-deftest nelisp-sf-cond-bad-clause ()
  (should-error (nelisp-sf-eval '(cond not-a-list))
                :type 'nelisp-special-forms-error))


;;; -----------------------------------------------------------------
;;; and / or
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-and-empty ()
  (should (eq (nelisp-sf-eval '(and)) t)))

(ert-deftest nelisp-sf-and-all-true ()
  (should (= (nelisp-sf-eval '(and 1 2 3)) 3)))

(ert-deftest nelisp-sf-and-short-circuit ()
  (should (eq (nelisp-sf-eval '(and 1 nil (error "should not eval"))) nil)))

(ert-deftest nelisp-sf-or-empty ()
  (should (eq (nelisp-sf-eval '(or)) nil)))

(ert-deftest nelisp-sf-or-first-truthy ()
  (should (= (nelisp-sf-eval '(or nil nil 7 (error "should not eval"))) 7)))

(ert-deftest nelisp-sf-or-all-nil ()
  (should (eq (nelisp-sf-eval '(or nil nil)) nil)))


;;; -----------------------------------------------------------------
;;; progn
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-progn-empty ()
  (should (eq (nelisp-sf-eval '(progn)) nil)))

(ert-deftest nelisp-sf-progn-sequential ()
  (should (= (nelisp-sf-eval '(progn 1 2 3)) 3)))

(ert-deftest nelisp-sf-progn-let-side-effect ()
  (nelisp-sf-test--with-fresh-state
    (should
     (= (nelisp-sf-eval
         '(progn (setq counter 1)
                 (setq counter (+ counter 10))
                 counter))
        11))))


;;; -----------------------------------------------------------------
;;; let / let*
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-let-binding ()
  (should (= (nelisp-sf-eval '(let ((x 3) (y 4)) (+ x y))) 7)))

(ert-deftest nelisp-sf-let-bare-symbol-binding ()
  ;; Bare symbol = bind to nil.
  (should (eq (nelisp-sf-eval '(let (x) x)) nil)))

(ert-deftest nelisp-sf-let-parallel ()
  ;; Parallel binding: y sees outer x, not the freshly bound x.
  (should
   (= (nelisp-sf-eval
       '(let ((x 1))
          (let ((x 10) (y x))
            y)))
      1)))

(ert-deftest nelisp-sf-let*-sequential ()
  ;; Sequential binding: y sees the freshly bound x.
  (should
   (= (nelisp-sf-eval
       '(let ((x 1))
          (let* ((x 10) (y x))
            y)))
      10)))

(ert-deftest nelisp-sf-let-malformed-binding ()
  (should-error (nelisp-sf-eval '(let ((1 2)) 1))
                :type 'nelisp-special-forms-error))


;;; -----------------------------------------------------------------
;;; lambda + funcall + apply
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-lambda-no-arg ()
  (should (= (nelisp-sf-funcall (nelisp-sf-eval '(lambda () 42))) 42)))

(ert-deftest nelisp-sf-lambda-with-args ()
  (should
   (= (nelisp-sf-funcall (nelisp-sf-eval '(lambda (x y) (+ x y))) 3 4) 7)))

(ert-deftest nelisp-sf-lambda-captures-env ()
  (let ((c (nelisp-sf-eval '(let ((cap 100))
                              (lambda (x) (+ x cap))))))
    (should (= (nelisp-sf-funcall c 5) 105))))

(ert-deftest nelisp-sf-lambda-rest-arg ()
  (let ((c (nelisp-sf-eval '(lambda (a &rest r) (cons a r)))))
    (should (equal (nelisp-sf-funcall c 1 2 3 4) '(1 2 3 4)))))

(ert-deftest nelisp-sf-lambda-optional-arg ()
  (let ((c (nelisp-sf-eval '(lambda (a &optional b) (cons a b)))))
    (should (equal (nelisp-sf-funcall c 1 2) '(1 . 2)))
    (should (equal (nelisp-sf-funcall c 9) '(9)))))

(ert-deftest nelisp-sf-funcall-host-builtin ()
  (should (= (nelisp-sf-funcall '+ 2 3 4) 9)))

(ert-deftest nelisp-sf-apply-spread-list ()
  (should (= (nelisp-sf-apply '+ '(1 2 3 4)) 10)))

(ert-deftest nelisp-sf-apply-mixed-tail-list ()
  ;; Emacs apply: last arg list is spliced in.
  (should (= (nelisp-sf-apply '+ '(1 2 (3 4))) 10)))

(ert-deftest nelisp-sf-eval-call-symbol-host ()
  ;; Call site dispatching to host fn via fboundp fallback.
  (should (= (nelisp-sf-eval '(+ 1 2 3)) 6)))


;;; -----------------------------------------------------------------
;;; setq
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-setq-global ()
  (nelisp-sf-test--with-fresh-state
    (should (= (nelisp-sf-eval '(setq g1 99)) 99))
    (should (= (nelisp-sf-eval 'g1) 99))))

(ert-deftest nelisp-sf-setq-multi-pair ()
  (nelisp-sf-test--with-fresh-state
    (should (= (nelisp-sf-eval '(setq a 1 b 2 c 3)) 3))
    (should (= (nelisp-sf-eval 'a) 1))
    (should (= (nelisp-sf-eval 'b) 2))
    (should (= (nelisp-sf-eval 'c) 3))))

(ert-deftest nelisp-sf-setq-mutates-lexical ()
  (should
   (= (nelisp-sf-eval
       '(let ((x 5))
          (setq x 99)
          x))
      99)))

(ert-deftest nelisp-sf-setq-missing-value ()
  (should-error (nelisp-sf-eval '(setq oops))
                :type 'nelisp-special-forms-error))

(ert-deftest nelisp-sf-setq-non-symbol ()
  (should-error (nelisp-sf-eval '(setq 5 5))
                :type 'nelisp-special-forms-error))


;;; -----------------------------------------------------------------
;;; save-* family (host delegate smoke)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-save-excursion-restores-point ()
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 3)
    (nelisp-sf-eval '(save-excursion (goto-char 1)))
    (should (= (point) 3))))

(ert-deftest nelisp-sf-save-restriction-restores ()
  (with-temp-buffer
    (insert "1234567890")
    (narrow-to-region 1 5)
    (nelisp-sf-eval '(save-restriction (widen)))
    (should (= (point-min) 1))
    (should (= (point-max) 5))))

(ert-deftest nelisp-sf-save-current-buffer-restores ()
  (let ((other (generate-new-buffer " *sf-test-other*"))
        (orig (current-buffer)))
    (unwind-protect
        (progn
          (nelisp-sf-eval `(save-current-buffer (set-buffer ,other)))
          (should (eq (current-buffer) orig)))
      (kill-buffer other))))

(ert-deftest nelisp-sf-save-match-data-restores ()
  (string-match "abc" "xxabcxx")
  (let ((before (match-beginning 0)))
    (nelisp-sf-eval '(save-match-data (string-match "yyy" "yyy")))
    (should (= (match-beginning 0) before))))


;;; -----------------------------------------------------------------
;;; setq-default / setq-local
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-setq-default-writes-global ()
  (nelisp-sf-test--with-fresh-state
    (should (= (nelisp-sf-eval '(setq-default sd1 11)) 11))
    (should (= (nelisp-sf-eval 'sd1) 11))))

(ert-deftest nelisp-sf-setq-local-shadows-default ()
  (nelisp-sf-test--with-fresh-state
    (nelisp-sf-eval '(setq-default sl1 1))
    (nelisp-sf-eval '(setq-local sl1 99))
    (should (= (nelisp-sf-eval 'sl1) 99))))

(ert-deftest nelisp-sf-setq-default-multi-pair ()
  (nelisp-sf-test--with-fresh-state
    (should (= (nelisp-sf-eval '(setq-default a 10 b 20)) 20))
    (should (= (nelisp-sf-eval 'a) 10))
    (should (= (nelisp-sf-eval 'b) 20))))


;;; -----------------------------------------------------------------
;;; eval-when-compile / eval-and-compile
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-eval-when-compile-immediate ()
  (should (= (nelisp-sf-eval '(eval-when-compile (+ 1 2))) 3)))

(ert-deftest nelisp-sf-eval-and-compile-immediate ()
  (should (= (nelisp-sf-eval '(eval-and-compile (+ 4 5))) 9)))


;;; -----------------------------------------------------------------
;;; let-when-compile / inline-let
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-let-when-compile-acts-like-let ()
  (should (= (nelisp-sf-eval '(let-when-compile ((x 7)) x)) 7)))

(ert-deftest nelisp-sf-inline-let-acts-like-let* ()
  (should
   (= (nelisp-sf-eval '(inline-let ((x 1) (y (+ x 10))) y)) 11)))


;;; -----------------------------------------------------------------
;;; declare
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-declare-returns-nil ()
  (should (eq (nelisp-sf-eval '(declare (indent 1))) nil)))

(ert-deftest nelisp-sf-declare-multiple-clauses ()
  (should (eq (nelisp-sf-eval '(declare (indent 1) (debug t))) nil)))


;;; -----------------------------------------------------------------
;;; Integration smoke (composition across forms)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-sf-fact-via-lambda-recursion ()
  ;; Anonymous Y-style recursion via setq + funcall.
  (nelisp-sf-test--with-fresh-state
    (nelisp-sf-eval
     '(setq fact (lambda (n)
                   (if (= n 0) 1
                     (* n (funcall fact (- n 1)))))))
    (should (= (nelisp-sf-funcall (nelisp-sf-eval 'fact) 5) 120))))

(ert-deftest nelisp-sf-cond-and-or-mixture ()
  (should
   (eq (nelisp-sf-eval
        '(cond ((and 1 2 nil) 'first)
               ((or nil nil 'hit) 'second)
               (t 'third)))
       'second)))

(ert-deftest nelisp-sf-let-with-lambda-capture-and-setq ()
  (nelisp-sf-test--with-fresh-state
    (nelisp-sf-eval
     '(setq make-counter
            (lambda ()
              (let ((n 0))
                (lambda () (setq n (+ n 1)))))))
    (let ((c (nelisp-sf-funcall (nelisp-sf-eval 'make-counter))))
      (should (= (nelisp-sf-funcall c) 1))
      (should (= (nelisp-sf-funcall c) 2))
      (should (= (nelisp-sf-funcall c) 3)))))


(provide 'nelisp-special-forms-test)

;;; nelisp-special-forms-test.el ends here
