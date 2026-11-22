;;; emacs-forward-word-native-test.el --- native forward-word (standalone) -*- lexical-binding: t; -*-

;;; Commentary:

;; Standalone NeLisp answers `forward-word' natively: explicit user instruction, and GNU
;; implements it in C (syntax.c Fforward_word / scan_words).  The native ASCII scan must agree
;; with the GNU-exact Lisp scan `emacs-edit--forward-word-lisp' and decline to
;; `nelisp--forward-word-reference' for everything it does not cover.  Skipped on host Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar emacs-buffer--char-props-seen)
(defvar nfwn--calls 0)

(defun nfwn--standalone-p ()
  (and (fboundp 'nelisp--forward-word-reference)
       (fboundp 'emacs-edit--forward-word-lisp)))

(defun nfwn--counted (thunk)
  "Run THUNK with the decline reference counted: (RESULT . CALLS)."
  (let ((orig (symbol-function 'nelisp--forward-word-reference)) (nfwn--calls 0))
    (unwind-protect
        (progn (fset 'nelisp--forward-word-reference
                     (lambda (&rest args) (cl-incf nfwn--calls) (apply orig args)))
               (let ((r (funcall thunk))) (cons r nfwn--calls)))
      (fset 'nelisp--forward-word-reference orig))))

(defun nfwn--observe (fn text pt count)
  (with-temp-buffer
    (insert text) (goto-char pt)
    (let ((v (condition-case e (funcall fn count) (error (list 'err (car e))))))
      (list v (point)))))

(ert-deftest nfwn-native-subr ()
  (skip-unless (nfwn--standalone-p))
  (should (subrp (symbol-function 'forward-word))))

(ert-deftest nfwn-ascii-native-agrees-and-never-declines ()
  (skip-unless (nfwn--standalone-p))
  (let ((emacs-buffer--char-props-seen nil) (n 0))
    (dolist (text '("" "foo bar" "  foo  bar_baz-qux 12 ab\nxy\tz  " "..." "a b c d e f"))
      (dolist (pt (number-sequence 1 (1+ (length text))))
        (dolist (cnt '(nil 0 1 -1 2 -2 5 -5 1000))
          (let* ((want (nfwn--observe #'emacs-edit--forward-word-lisp text pt cnt))
                 (got (nfwn--counted (lambda () (nfwn--observe #'forward-word text pt cnt)))))
            (cl-incf n)
            (should (equal want (car got)))
            (should (= 0 (cdr got)))))))
    (should (> n 100))))

(ert-deftest nfwn-declines-and-agrees ()
  (skip-unless (nfwn--standalone-p))
  (let ((cases
         `(("non-ascii" "ab あい cd" nil)
           ("props-seen" "ab cd ef" ((emacs-buffer--char-props-seen t)))
           ("escapes" "ab\\cd ef" ((words-include-escapes t)))
           ("lookup-props" "ab cd ef" ((parse-sexp-lookup-properties t)))
           ("separating" "ab cd ef" ((word-separating-categories '((?H . ?K) (?C . ?H)))))
           ("alias" "ab cd ef" ((char-property-alias-alist '((foo field)))))
           ("default-props" "ab cd ef" ((default-text-properties '(field x))))
           ("non-fixnum" "ab cd" nil))))
    (dolist (c cases)
      (let* ((text (nth 1 c)) (binds (nth 2 c))
             (cnt (if (equal (car c) "non-fixnum") 1.5 2))
             (run (lambda (fn)
                    (eval `(let ((emacs-buffer--char-props-seen nil) ,@binds)
                             (nfwn--observe #',fn ,text 1 ,cnt))
                          t)))
             (want (funcall run 'emacs-edit--forward-word-lisp))
             (got (nfwn--counted (lambda () (funcall run 'forward-word)))))
        (ert-info ((car c))
          (should (equal want (car got)))
          (should (> (cdr got) 0)))))))

(ert-deftest nfwn-narrowing-and-limit-values ()
  (skip-unless (nfwn--standalone-p))
  (with-temp-buffer
    (insert "aa bb cc")
    (narrow-to-region 4 6)
    (goto-char 4)
    (should (null (forward-word 3)))
    (should (= (point) 6))
    (should (eq t (forward-word -1)))
    (should (= (point) 4))
    (should (null (forward-word -1)))
    (should (= (point) 4))))

(provide 'emacs-forward-word-native-test)
;;; emacs-forward-word-native-test.el ends here
