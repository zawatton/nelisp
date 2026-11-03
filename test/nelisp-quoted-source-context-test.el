;;; nelisp-quoted-source-context-test.el --- Syntax-only controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-quoted-source-context)
(defvar quoted-context-test--sentinel 0)

(defmacro quoted-context-test--file (text &rest body)
  (declare (indent 1))
  `(let ((file (make-temp-file "quoted-source-" nil ".el")))
     (unwind-protect (progn (with-temp-file file (insert ,text)) ,@body)
       (delete-file file))))

(ert-deftest quoted-context/direct-and-quoted-sequences ()
  (dolist (text '("(defun wanted (x) x)"
                  "(defconst source '(seq (defun wanted (x) x)))"
                  "(defconst source (quote (seq (defun wanted (x) x)))) ; end"))
    (quoted-context-test--file text
      (let ((result (nelisp-quoted-source-context file 'wanted 100)))
        (should (equal (plist-get result :form) '(defun wanted (x) x)))
        (should (= (plist-get result :printed-characters) 20))
        (should (= (length (plist-get result :source-sha256)) 64))))))

(ert-deftest quoted-context/missing-and-duplicate ()
  (quoted-context-test--file "(defun other () 1)"
    (should-error (nelisp-quoted-source-context file 'wanted 100)))
  (quoted-context-test--file "(defun wanted () 1) (defconst s '(seq (defun wanted () 2)))"
    (should-error (nelisp-quoted-source-context file 'wanted 100))))

(ert-deftest quoted-context/no-body-or-quoted-data-search ()
  (dolist (text '("'(defun wanted () 1)"
                  "(defconst data '(defun wanted () 1))"
                  "(defun outer () (defun wanted () 1))"
                  "(defconst s '(seq (quote (defun wanted () 1))))"
                  "(defconst s '(seq (defun outer () '(defun wanted () 1))))"))
    (quoted-context-test--file text
      (should-error (nelisp-quoted-source-context file 'wanted 200)))))

(ert-deftest quoted-context/overflow-syntax-and-no-evaluation ()
  (quoted-context-test--file "(defun wanted () 123)"
    (should-error (nelisp-quoted-source-context file 'wanted 5)))
  (quoted-context-test--file "(defun wanted () 1) ("
    (should-error (nelisp-quoted-source-context file 'wanted 100)))
  (let ((quoted-context-test--sentinel 0))
    (quoted-context-test--file
        "(setq quoted-context-test--sentinel 99) (defun wanted () (error \"never\"))"
      (should (plist-get (nelisp-quoted-source-context file 'wanted 100) :form))
      (should (= quoted-context-test--sentinel 0))))
  (quoted-context-test--file (make-string 8388609 ?x)
    (should-error (nelisp-quoted-source-context file 'wanted 100))))

(ert-deftest quoted-context/selector-verifier-negative-controls ()
  (let ((expected '(defun wanted () 1)))
    (dolist (broken (list (lambda (&rest _) nil)
                          (lambda (&rest _) '(:form (defun wrong () 1)))))
      (should-not (equal expected (plist-get (funcall broken "unused" 'wanted 100) :form))))))

(ert-deftest quoted-context/reject-invalid-utf8-comment ()
  (let ((file (make-temp-file "quoted-context-invalid-")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert ";" (unibyte-string 255) "\n(defun wanted () 1)\n")
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) file nil 'silent)))
          (should-error (nelisp-quoted-source-context file 'wanted 100)))
      (delete-file file))))

(ert-deftest quoted-context/template-explicit-literal ()
  (quoted-context-test--file "(defconst s `(seq (defun wanted (x) (+ x 1))))"
    (should-error (nelisp-quoted-source-context file 'wanted 100))
    (let ((r (nelisp-quoted-source-context file 'wanted 100 t)))
      (should (equal (plist-get r :form) '(defun wanted (x) (+ x 1))))
      (should (equal (plist-get r :source-kind) "SOURCE TEMPLATE")))
    (should-error (nelisp-quoted-source-context file 'wanted 100 'automatic))))

(ert-deftest quoted-context/template-dynamic-no-evaluation ()
  (let ((quoted-context-test--sentinel 0))
    (dolist (text '("(defconst s `(seq (defun wanted () ,(setq quoted-context-test--sentinel 9))))"
                    "(defconst s `(seq (defun wanted () [,@things])))"
                    "(defconst s `(seq (defun wanted () '(,thing))))"))
      (quoted-context-test--file text
        (should-error (nelisp-quoted-source-context file 'wanted 300 t))))
    (quoted-context-test--file
        "(setq quoted-context-test--sentinel 99) (defconst s `(seq ,@(error \"never\") (defun wanted () 1)))"
      (should (equal (plist-get (nelisp-quoted-source-context file 'wanted 300 t) :form)
                     '(defun wanted () 1))))
    (should (= quoted-context-test--sentinel 0))))

(ert-deftest quoted-context/template-duplicate-and-budget ()
  (dolist (text '("(defconst s `(seq (defun wanted () 1) (defun wanted () 2)))"
                  "(defun wanted () 1) (defconst s `(seq (defun wanted () 2)))"))
    (quoted-context-test--file text
      (should-error (nelisp-quoted-source-context file 'wanted 300 t))))
  (quoted-context-test--file "(defconst s `(seq (defun wanted () 1)))"
    (should-error (nelisp-quoted-source-context file 'wanted 5 t))))

(ert-deftest quoted-context/template-unsupported-wrapper-and-fakes ()
  (dolist (text '("(defconst s (append `(seq (defun wanted () 1)) nil))"
                  "(defconst s `(seq '(defun wanted () 1)))"
                  "(defconst s `(seq (defun outer () (defun wanted () 1))))"
                  "(defconst s `(seq ,(defun wanted () 1)))"))
    (quoted-context-test--file text
      (should-error (nelisp-quoted-source-context file 'wanted 300 t)))))

(provide 'nelisp-quoted-source-context-test)
