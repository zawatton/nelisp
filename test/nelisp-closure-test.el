;;; nelisp-closure-test.el --- Phase 7+C closure ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; ERT pin for Doc 40 §3.C / §3.3 Phase 7+C closure / lexical
;; binding.  Covers the four-form smoke gate (self-rec /
;; mutual-rec / captured-mutate / over-captured), &optional /
;; &rest argument handling, capture-set analysis, lexical-binding
;; default toggling, and the bridge to / from the Phase 7+B
;; `nelisp-sf-closure' tag.
;;
;; Aim: 30+ tests, byte-compile clean, regression-free.

;;; Code:

(require 'ert)
(require 'nelisp-closure)
(require 'nelisp-special-forms)

(defmacro nelisp-cl-test--with-fresh-state (&rest body)
  "Run BODY after clearing all Phase 7+B / 7+C globals."
  (declare (indent 0))
  `(progn
     (nelisp-special-forms-reset)
     (let ((nelisp-closure--lexical-binding-default t))
       (unwind-protect
           (progn ,@body)
         (nelisp-special-forms-reset)))))


;;; -----------------------------------------------------------------
;;; Construction / predicate / accessors (8 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-make-empty-env ()
  (let ((c (nelisp-closure-make nil '(x) '((+ x 1)))))
    (should (nelisp-closure-p c))
    (should (equal (nelisp-closure-env c) nil))
    (should (equal (nelisp-closure-arglist c) '(x)))
    (should (equal (nelisp-closure-body c) '((+ x 1))))))

(ert-deftest nelisp-cl-make-with-env ()
  (let* ((env '((y . 10)))
         (c (nelisp-closure-make env '(x) '((+ x y)))))
    (should (nelisp-closure-p c))
    (should (equal (nelisp-closure-env c) env))))

(ert-deftest nelisp-cl-make-with-source-marker ()
  (let ((c (nelisp-closure-make nil '() '(42) '("file.el" . 100))))
    (should (equal (nelisp-closure-source-marker c) '("file.el" . 100)))))

(ert-deftest nelisp-cl-predicate-rejects-non-struct ()
  (should-not (nelisp-closure-p nil))
  (should-not (nelisp-closure-p '(nelisp-sf-closure nil nil nil)))
  (should-not (nelisp-closure-p 42))
  (should-not (nelisp-closure-p "string"))
  (should-not (nelisp-closure-p (lambda () nil))))

(ert-deftest nelisp-cl-malformed-rest-without-name ()
  (should-error (nelisp-closure-make nil '(x &rest) '(x))
                :type 'nelisp-closure-malformed-arglist))

(ert-deftest nelisp-cl-malformed-duplicate-rest ()
  (should-error (nelisp-closure-make nil '(&rest a &rest b) '(a))
                :type 'nelisp-closure-malformed-arglist))

(ert-deftest nelisp-cl-malformed-keyword-param ()
  (should-error (nelisp-closure-make nil '(:foo) '(:foo))
                :type 'nelisp-closure-malformed-arglist))

(ert-deftest nelisp-cl-malformed-optional-after-rest ()
  (should-error (nelisp-closure-make nil '(&rest r &optional x) '(r))
                :type 'nelisp-closure-malformed-arglist))


;;; -----------------------------------------------------------------
;;; apply: simple positional (3)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-apply-no-args ()
  (let ((c (nelisp-closure-make nil '() '(42))))
    (should (= (nelisp-closure-apply c nil) 42))))

(ert-deftest nelisp-cl-apply-positional-1 ()
  (let ((c (nelisp-closure-make nil '(x) '((+ x 1)))))
    (should (= (nelisp-closure-apply c '(10)) 11))))

(ert-deftest nelisp-cl-apply-positional-3 ()
  (let ((c (nelisp-closure-make nil '(a b c) '((+ a b c)))))
    (should (= (nelisp-closure-apply c '(1 2 3)) 6))))


;;; -----------------------------------------------------------------
;;; apply: arity errors (3)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-arity-too-few ()
  (let ((c (nelisp-closure-make nil '(a b) '((+ a b)))))
    (should-error (nelisp-closure-apply c '(1))
                  :type 'nelisp-closure-arity-error)))

(ert-deftest nelisp-cl-arity-too-many ()
  (let ((c (nelisp-closure-make nil '(a) '(a))))
    (should-error (nelisp-closure-apply c '(1 2 3))
                  :type 'nelisp-closure-arity-error)))

(ert-deftest nelisp-cl-not-a-closure ()
  (should-error (nelisp-closure-apply 'not-a-closure '(1))
                :type 'nelisp-closure-error))


;;; -----------------------------------------------------------------
;;; apply: &optional (3)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-optional-supplied ()
  (let ((c (nelisp-closure-make nil '(a &optional b) '((or b a)))))
    (should (= (nelisp-closure-apply c '(1 2)) 2))))

(ert-deftest nelisp-cl-optional-default-nil ()
  (let ((c (nelisp-closure-make nil '(a &optional b) '((or b a)))))
    (should (= (nelisp-closure-apply c '(7)) 7))))

(ert-deftest nelisp-cl-optional-default-form ()
  ;; (NAME DEFAULT) syntax — DEFAULT is evaluated per call.
  (let ((c (nelisp-closure-make nil '(a &optional (b (+ a 100)))
                                '((+ a b)))))
    (should (= (nelisp-closure-apply c '(5)) (+ 5 105)))
    (should (= (nelisp-closure-apply c '(5 1)) (+ 5 1)))))


;;; -----------------------------------------------------------------
;;; apply: &rest (3)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-rest-empty ()
  (let ((c (nelisp-closure-make nil '(&rest xs) '(xs))))
    (should (equal (nelisp-closure-apply c nil) nil))))

(ert-deftest nelisp-cl-rest-one ()
  (let ((c (nelisp-closure-make nil '(&rest xs) '(xs))))
    (should (equal (nelisp-closure-apply c '(7)) '(7)))))

(ert-deftest nelisp-cl-rest-many ()
  (let ((c (nelisp-closure-make nil '(a &rest xs) '(xs))))
    (should (equal (nelisp-closure-apply c '(1 2 3 4)) '(2 3 4)))))


;;; -----------------------------------------------------------------
;;; Capture: simple env access (2)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-no-capture ()
  (let ((c (nelisp-closure-make nil '(x) '((+ x 1)))))
    (should (= (nelisp-closure-apply c '(5)) 6))))

(ert-deftest nelisp-cl-captures-outer-var ()
  (let* ((env '((y . 100)))
         (c (nelisp-closure-make env '(x) '((+ x y)))))
    (should (= (nelisp-closure-apply c '(7)) 107))))


;;; -----------------------------------------------------------------
;;; Capture: shared mutable env (Doc 40 §3.3 captured-mutate) (2)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-shared-mutable-env-read ()
  (let* ((shared (list (cons 'counter 0)))
         (reader (nelisp-closure-make shared '() '(counter))))
    (should (= (nelisp-closure-apply reader nil) 0))
    (setcdr (assq 'counter shared) 7)
    (should (= (nelisp-closure-apply reader nil) 7))))

(ert-deftest nelisp-cl-shared-mutable-env-write-via-setq ()
  (nelisp-cl-test--with-fresh-state
    (let* ((shared (list (cons 'counter 0)))
           (writer (nelisp-closure-make shared '(n)
                                        '((setq counter (+ counter n))
                                          counter)))
           (reader (nelisp-closure-make shared '() '(counter))))
      (should (= (nelisp-closure-apply writer '(1)) 1))
      (should (= (nelisp-closure-apply reader nil) 1))
      (should (= (nelisp-closure-apply writer '(4)) 5))
      (should (= (nelisp-closure-apply reader nil) 5)))))


;;; -----------------------------------------------------------------
;;; Recursion patterns (2)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-recursive-via-fset-style ()
  (nelisp-cl-test--with-fresh-state
    ;; Install a closure under a global name so the body can call
    ;; itself by name through the special-forms function table.
    (let ((fact (nelisp-closure-make
                 nil '(n)
                 '((if (= n 0)
                       1
                     (* n (fact (- n 1))))))))
      (puthash 'fact fact nelisp-special-forms--functions)
      (should (= (nelisp-closure-apply fact '(0)) 1))
      (should (= (nelisp-closure-apply fact '(5)) 120)))))

(ert-deftest nelisp-cl-mutual-recursive ()
  (nelisp-cl-test--with-fresh-state
    (let ((my-evenp (nelisp-closure-make
                     nil '(n)
                     '((if (= n 0) t (my-oddp (- n 1))))))
          (my-oddp (nelisp-closure-make
                    nil '(n)
                    '((if (= n 0) nil (my-evenp (- n 1)))))))
      (puthash 'my-evenp my-evenp nelisp-special-forms--functions)
      (puthash 'my-oddp  my-oddp  nelisp-special-forms--functions)
      (should (eq (nelisp-closure-apply my-evenp '(0)) t))
      (should (eq (nelisp-closure-apply my-evenp '(4)) t))
      (should (eq (nelisp-closure-apply my-oddp '(3)) t))
      (should (eq (nelisp-closure-apply my-evenp '(3)) nil)))))


;;; -----------------------------------------------------------------
;;; Tail-call style pattern (1)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-tail-call-pattern ()
  (nelisp-cl-test--with-fresh-state
    ;; Tree-walk evaluator does not optimize tail calls; this just
    ;; pins that a sum-to-N pattern terminates correctly under the
    ;; default `max-lisp-eval-depth' for small N.
    (let ((sum-to (nelisp-closure-make
                   nil '(n acc)
                   '((if (= n 0) acc (sum-to (- n 1) (+ acc n)))))))
      (puthash 'sum-to sum-to nelisp-special-forms--functions)
      (should (= (nelisp-closure-apply sum-to '(10 0)) 55)))))


;;; -----------------------------------------------------------------
;;; Capture-vars analysis (5)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-capture-vars-empty ()
  (should (equal (nelisp-closure-capture-vars '(+ 1 2) '((y . 7))) nil)))

(ert-deftest nelisp-cl-capture-vars-simple ()
  (should (equal (nelisp-closure-capture-vars 'y '((y . 7) (z . 8)))
                 '(y))))

(ert-deftest nelisp-cl-capture-vars-mixed ()
  (let ((env '((a . 1) (b . 2) (c . 3))))
    (should (equal (sort (nelisp-closure-capture-vars '(+ a c) env) #'string-lessp)
                   '(a c)))))

(ert-deftest nelisp-cl-capture-vars-respects-let ()
  ;; `a' is shadowed by the inner let, so it should NOT count as
  ;; captured, but `b' (free in body) should.
  (let ((env '((a . 1) (b . 2))))
    (should (equal (nelisp-closure-capture-vars
                    '(let ((a 99)) (+ a b))
                    env)
                   '(b)))))

(ert-deftest nelisp-cl-capture-vars-respects-lambda ()
  ;; `a' is bound by the inner lambda, so it should NOT count.  `b'
  ;; remains free.
  (let ((env '((a . 1) (b . 2))))
    (should (equal (nelisp-closure-capture-vars
                    '(function (lambda (a) (+ a b)))
                    env)
                   '(b)))))


;;; -----------------------------------------------------------------
;;; Lexical-binding default (3)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-lex-binding-default-read ()
  (let ((nelisp-closure--lexical-binding-default t))
    (should (eq (nelisp-closure-lexical-binding-default) t))))

(ert-deftest nelisp-cl-lex-binding-default-set-nil ()
  (let ((nelisp-closure--lexical-binding-default t))
    (nelisp-closure-lexical-binding-default 'nil-explicit)
    (should (eq nelisp-closure--lexical-binding-default nil))))

(ert-deftest nelisp-cl-lex-binding-default-set-t ()
  (let ((nelisp-closure--lexical-binding-default nil))
    (nelisp-closure-lexical-binding-default t)
    (should (eq nelisp-closure--lexical-binding-default t))))


;;; -----------------------------------------------------------------
;;; Bridge to / from sf-tag (3)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-from-sf-tag-roundtrip ()
  (let* ((sf '(nelisp-sf-closure ((y . 7)) (x) ((+ x y))))
         (c  (nelisp-closure-from-sf-tag sf)))
    (should (nelisp-closure-p c))
    (should (equal (nelisp-closure-env c) '((y . 7))))
    (should (equal (nelisp-closure-arglist c) '(x)))
    (should (equal (nelisp-closure-body c) '((+ x y))))
    (should (= (nelisp-closure-apply c '(3)) 10))))

(ert-deftest nelisp-cl-to-sf-tag-roundtrip ()
  (let* ((c  (nelisp-closure-make '((y . 7)) '(x) '((+ x y))))
         (sf (nelisp-closure-to-sf-tag c)))
    (should (consp sf))
    (should (eq (car sf) 'nelisp-sf-closure))
    (should (equal (nth 1 sf) '((y . 7))))
    (should (equal (nth 2 sf) '(x)))
    (should (equal (nth 3 sf) '((+ x y))))))

(ert-deftest nelisp-cl-from-sf-tag-rejects-bad-input ()
  (should-error (nelisp-closure-from-sf-tag '(not-sf-closure nil nil nil))
                :type 'nelisp-closure-error))


;;; -----------------------------------------------------------------
;;; Body that returns a new closure (1)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cl-body-builds-inner-closure ()
  ;; Outer captures `n', returns a (Phase 7+B `nelisp-sf-closure' tag)
  ;; lambda — funcall through special-forms apply path.
  (nelisp-cl-test--with-fresh-state
    (let* ((adder-builder
            (nelisp-closure-make
             nil '(n)
             '((function (lambda (x) (+ x n))))))
           (add-3 (nelisp-closure-apply adder-builder '(3))))
      (should (consp add-3))
      (should (eq (car add-3) 'nelisp-sf-closure))
      (should (= (nelisp-special-forms-funcall add-3 10) 13)))))

(provide 'nelisp-closure-test)

;;; nelisp-closure-test.el ends here
