;;; nelisp-cl-defstruct-docstring-test.el --- struct docstring metadata -*- lexical-binding: t; -*-

(require 'ert)
;; Only this source helper is under test. Loading all of cl-macros here
;; replaces GNU's setf/cl-defstruct before the generic tests can save them,
;; and makes unrelated host structs fail eager macro expansion.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../lisp/nelisp-cl-macros.el"
                     (file-name-directory load-file-name)))
  (goto-char (point-min))
  (unless (re-search-forward "^(defun nelisp-cl-macros--struct-slots " nil t)
    (error "Struct slot source helper is missing"))
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t))

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
