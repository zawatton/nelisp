;;; nelisp-cl-defstruct-docstring-test.el --- struct docstring metadata -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name
            "../lisp/nelisp-cl-macros.el"
            (file-name-directory load-file-name)))

(ert-deftest nelisp-cl-defstruct-retains-leading-docstring ()
  (should (equal
           (nelisp-cl-macros--struct-slots
            '("GNU cl-defstruct leading documentation" (slot nil :type symbol)))
           '((slot nil :type symbol))))
  (should-not
   (nelisp-cl-macros--struct-slots
    '("GNU cl-defstruct documentation with no declared slots"))))

(provide 'nelisp-cl-defstruct-docstring-test)

;;; nelisp-cl-defstruct-docstring-test.el ends here
