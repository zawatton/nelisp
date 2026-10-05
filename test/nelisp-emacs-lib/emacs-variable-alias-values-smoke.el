;;; emacs-variable-alias-values-smoke.el --- Heap-image live-cell reader checks -*- lexical-binding: t; -*-
;; Load after the normal bootstrap heap image, without a diagnostic overlay.
(defvar b88-alias-values-base 10)
(defvar b88-alias-values-other 30)
(defvar b88-alias-values-nil nil)
(defun b88-alias-values--check (label expected actual)
  (unless (equal expected actual)
    (error "Alias values %s: expected %S, got %S" label expected actual)))
(let* ((void (make-symbol "void"))
       (unbound (list 'unbound))
       (symbols (list 'b88-alias-values-base 'b88-alias-values-one void)))
  (defvaralias 'b88-alias-values-one 'b88-alias-values-base)
  (defvaralias 'b88-alias-values-two 'b88-alias-values-one)
  (b88-alias-values--check 'bound [10 10]
                          (emacs-variable-alias-values (list 'b88-alias-values-base
                                                             'b88-alias-values-two) unbound))
  (let ((b88-alias-values-base 20))
    (b88-alias-values--check 'dynamic [20 20]
                            (emacs-variable-alias-values (list 'b88-alias-values-base
                                                               'b88-alias-values-two) unbound)))
  (b88-alias-values--check 'nil-and-void t
                          (let ((values (emacs-variable-alias-values
                                         (list 'b88-alias-values-nil void) unbound)))
                            (and (null (aref values 0)) (eq (aref values 1) unbound))))
  (b88-alias-values--check 'cycle 'cyclic-variable-indirection
                          (condition-case err
                              (defvaralias 'b88-alias-values-base 'b88-alias-values-two)
                            (error (car err))))
  (defvaralias 'b88-alias-values-one 'b88-alias-values-other)
  (b88-alias-values--check 'retarget [10 30]
                          (emacs-variable-alias-values (list 'b88-alias-values-base
                                                             'b88-alias-values-two) unbound))
  (let ((input (list 'b88-alias-values-base)))
    (emacs-variable-alias-values input unbound)
    (setcar input 'b88-alias-values-other)
    (b88-alias-values--check 'mutated-input [30]
                            (emacs-variable-alias-values input unbound)))
  (makunbound 'b88-alias-values-one)
  (b88-alias-values--check 'removed-alias t
                          (eq (aref (emacs-variable-alias-values
                                     '(b88-alias-values-one) unbound) 0) unbound))
  (set 'b88-alias-values-base 'emacs-buffer--swap-unset)
  (b88-alias-values--check 'bound-sentinel 'emacs-buffer--swap-unset
                          (aref (emacs-variable-alias-values symbols unbound) 0))
  (b88-alias-values--check 'wrong-type 'wrong-type-argument
                          (condition-case err
                              (emacs-variable-alias-values '(42) unbound)
                            (error (car err)))))
(let* ((a (make-symbol "void-a")) (b (make-symbol "void-b")) (v (list nil)))
  (b88-alias-values--check 'empty [] (emacs-variable-alias-values nil v))
  (b88-alias-values--check 'all-void (vector v v v v v v)
                          (emacs-variable-alias-values (list a b a b a b) v))
  (b88-alias-values--check 'stretches (vector v 30 v v nil v)
                          (emacs-variable-alias-values
                           (list a 'b88-alias-values-other b a 'b88-alias-values-nil b) v))
  (set a 7)
  (b88-alias-values--check 'newly-bound [7 30 7]
                          (emacs-variable-alias-values (list a 'b88-alias-values-other a) v))
  (makunbound a)
  (b88-alias-values--check 'newly-void (vector v 30 v)
                          (emacs-variable-alias-values (list a 'b88-alias-values-other a) v)))
(princ "B88-ALIAS-VALUES-PASS|14\n")
t
