;;; emacs-pcase-test.el --- Tests for emacs-pcase  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the minimal `pcase' port split out of `emacs-stub.el'.
;; Under batch host Emacs the vendor `pcase' family wins by default, so
;; a few tests temporarily unbind the relevant symbols and reload the
;; module to exercise the local helpers and stub macroexpansions.

;;; Code:

(require 'ert)
(require 'emacs-pcase)

(defconst emacs-pcase-test--module-file
  (expand-file-name "../src/emacs-pcase.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-pcase-test--with-reloaded-module (symbols thunk)
  "Reload `emacs-pcase' with SYMBOLS temporarily unbound, then call THUNK."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym symbols)
            (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)
            (fmakunbound sym))
          (load emacs-pcase-test--module-file t t)
          (funcall thunk))
      (dolist (cell saved)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell)))))))

;;;; Load / feature contract

(ert-deftest emacs-pcase-test/require-loads-cleanly ()
  (should (featurep 'emacs-pcase))
  (should (featurep 'pcase))
  (should (fboundp 'pcase))
  (should (fboundp 'pcase-defmacro))
  (should (fboundp 'pcase-let))
  (should (fboundp 'pcase-let*))
  (should (fboundp 'pcase-dolist)))

;;;; Helper coverage

(ert-deftest emacs-pcase-test/test-helper-covers-basic-patterns ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '_ 'v) '(t)))
     (should (equal (emacs-pcase--test 'sym 'v) '(t (sym v))))
     (should (equal (emacs-pcase--test 7 'v) '((equal v 7))))
     (should (equal (emacs-pcase--test "x" 'v) '((equal v "x")))))))

(ert-deftest emacs-pcase-test/test-helper-covers-quote-pred-and-bare-cons ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '(quote q) 'v) '((eq v 'q))))
     (should (equal (emacs-pcase--test '(pred symbolp) 'v) '((funcall #'symbolp v))))
     (should (equal (emacs-pcase--test '(foo . bar) 'v) '(t))))))

(ert-deftest emacs-pcase-test/test-helper-covers-and-and-or ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--and '(sym (quote :a)) 'v)
                    '((and t (eq v ':a)) (sym v))))
     (should (equal (emacs-pcase--or '((quote :a) (quote :b)) 'v)
                    '((or (eq v ':a) (eq v ':b))))))))

(ert-deftest emacs-pcase-test/test-helper-covers-cons-and-not-pred ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '(cons a b) 'v)
                    '((and (consp v) t t) (a (car v)) (b (cdr v)))))
     (should (equal (emacs-pcase--test '(pred (not consp)) 'v)
                    '((not (funcall #'consp v))))))))

(ert-deftest emacs-pcase-test/test-helper-covers-backquote-comma-and-comma-at ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--backquote '(comma x) 'v) '(t (x v))))
     (should (equal (emacs-pcase--backquote '(comma-at rest) 'v) '(t (rest v)))))))

(ert-deftest emacs-pcase-test/test-helper-covers-backquote-nested-cons-and-tail ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--backquote 'foo 'v) '((equal v 'foo))))
     (should (equal (emacs-pcase--backquote '(a (comma x) (comma-at rest) nil) 'v)
                    '((and (consp v)
                           (equal (car v) 'a)
                           (and (consp (cdr v))
                                t
                                (and (consp (cdr (cdr v)))
                                     t
                                     (and (consp (cdr (cdr (cdr v))))
                                          (null (car (cdr (cdr (cdr v)))))
                                          (null (cdr (cdr (cdr (cdr v)))))))))
                      (x (car (cdr v)))
                      (rest (car (cdr (cdr v))))))))))

(ert-deftest emacs-pcase-test/pattern-aware-let-and-dolist-evaluate ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase pcase-let pcase-let* pcase-dolist)
   (lambda ()
     (should (equal (eval '(pcase-let ((`(,key . ,def) '("p" . previous-line)))
                             (cons key def))
                          t)
                    '("p" . previous-line)))
     (should (equal (eval '(pcase-let* ((`(,first . ,rest) '(1 2 3))
                                        (`(,second . ,tail) rest))
                             (list first second tail))
                          t)
                    '(1 2 (3))))
     (should (equal (eval '(let (seen)
                             (pcase-dolist (`(,key . ,def)
                                            '(("p" . previous-line)
                                              ("n" . next-line)))
                               (push (cons key def) seen))
                             (nreverse seen))
                          t)
                    '(("p" . previous-line) ("n" . next-line)))))))

(ert-deftest emacs-pcase-test/bulk-stub-macros-are-overwritten ()
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym '(pcase-let pcase-let* pcase-dolist))
            (push (list sym
                        (and (fboundp sym) (symbol-function sym))
                        (get sym 'emacs-stub-bulk))
                  saved)
            (fset sym (cons 'macro (lambda (&rest _) nil)))
            (put sym 'emacs-stub-bulk t))
          (load emacs-pcase-test--module-file t t)
          (should (equal (eval '(pcase-let ((`(,key . ,def) '("p" . previous-line)))
                                  (cons key def))
                               t)
                         '("p" . previous-line)))
          (should (equal (eval '(let (seen)
                                  (pcase-dolist (`(,key . ,def)
                                                 '(("p" . previous-line)
                                                   ("n" . next-line)))
                                    (push (cons key def) seen))
                                  (nreverse seen))
                               t)
                         '(("p" . previous-line) ("n" . next-line)))))
      (dolist (cell saved)
        (if (cadr cell)
            (fset (car cell) (cadr cell))
          (fmakunbound (car cell)))
        (put (car cell) 'emacs-stub-bulk (caddr cell))))))

(ert-deftest emacs-pcase-test/pcase-expands-and-evaluates ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (let ((expanded (macroexpand '(pcase x ((quote :a) 1) ('b 2) (_ 3)))))
       (should (eq 'let (car expanded)))
       (should (memq 'cond (flatten-tree expanded)))
       (should (equal (let ((x :a))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      1))
       (should (equal (let ((x 'b))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      2))
     (should (equal (let ((x 99))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      3))))))

(ert-deftest emacs-pcase-test/pcase-defmacro-expands-top-level-or-branches ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase pcase-defmacro)
   (lambda ()
     (eval '(pcase-defmacro nelisp-test-leaf (vpat)
              `(or `(t . ,,vpat) (and (pred (not consp)) ,vpat)))
           t)
     (should (eq 'nelisp-test-leaf--pcase-macroexpander
                 (get 'nelisp-test-leaf 'pcase-macroexpander)))
     (let* ((form (list 'pcase (list 'quote '(t . 42))
                        '((nelisp-test-leaf v) v)
                        '(_ :no)))
            (expanded (macroexpand form)))
       (should (memq 'let (flatten-tree expanded)))
       (should (equal (eval expanded t) 42)))
     (let* ((form (list 'pcase (list 'quote 'leaf)
                        '((nelisp-test-leaf v) v)
                        '(_ :no)))
            (expanded (macroexpand form)))
       (should (equal (eval expanded t) 'leaf)))
     (let* ((form (list 'pcase (list 'quote '(branch . nil))
                        '((nelisp-test-leaf v) v)
                        '(_ :no)))
            (expanded (macroexpand form)))
       (should (equal (eval expanded t) :no))))))

(ert-deftest emacs-pcase-test/doc16-round26-pcase-lambda ()
  "Doc 16 round 26: pcase-lambda destructures pattern parameters on call.
The batch host has the real pcase-lambda, pinning the contract."
  (let ((f (pcase-lambda (`(,a ,b) c) (list a b c))))
    (should (equal '(1 2 3) (funcall f '(1 2) 3))))
  ;; plain-symbol parameters pass through unchanged
  (let ((g (pcase-lambda (x y) (+ x y))))
    (should (= 5 (funcall g 2 3))))
  ;; mix of plain and pattern parameters
  (let ((h (pcase-lambda (a `(,b . ,c)) (list a b c))))
    (should (equal '(1 2 3) (funcall h 1 '(2 . 3))))))

(provide 'emacs-pcase-test)

;;; emacs-pcase-test.el ends here
