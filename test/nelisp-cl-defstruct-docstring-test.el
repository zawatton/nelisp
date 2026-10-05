;;; nelisp-cl-defstruct-docstring-test.el --- struct docstring metadata -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-cl-defstruct-docstring-test--file
  (or load-file-name buffer-file-name))

(defconst nelisp-cl-defstruct-docstring-test--root
  (expand-file-name "../" (file-name-directory
                           nelisp-cl-defstruct-docstring-test--file)))

;; This library installs NeLisp's own `setf', `cl-defstruct', and other
;; host names unconditionally.  Keep it in a child Emacs: loading it in
;; the suite process breaks subsequent expansion of host struct places
;; such as `nelisp-actor-status' in the daemon's collector generator.
(defun nelisp-cl-defstruct-docstring-test--probe (form)
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "--batch" "-Q" "--eval"
                   (prin1-to-string form))))
      (ert-info ((buffer-string))
        (should (equal status 0)))
      (read (buffer-string)))))

(ert-deftest nelisp-cl-defstruct-retains-leading-docstring ()
  (let ((actual
         (nelisp-cl-defstruct-docstring-test--probe
          `(progn
             (load ,(expand-file-name "lisp/nelisp-cl-macros.el"
                                      nelisp-cl-defstruct-docstring-test--root)
                   nil t)
             (prin1
              (list
               (nelisp-cl-macros--struct-slots
                '("GNU cl-defstruct leading documentation"
                  (slot nil :type symbol)))
               (nelisp-cl-macros--struct-slots
                '("GNU cl-defstruct documentation with no declared slots"))))))))
    (should (equal (car actual) '((slot nil :type symbol))))
    (should-not (cadr actual))))

(ert-deftest nelisp-cl-defstruct-docstring-test-preserves-host-actor-setf ()
  "Loading this test must preserve host macros and actor accessor places."
  (should
   (equal
    (nelisp-cl-defstruct-docstring-test--probe
     `(progn
        (add-to-list 'load-path
                     ,(expand-file-name "src" nelisp-cl-defstruct-docstring-test--root))
        (add-to-list 'load-path
                     ,(expand-file-name "lisp" nelisp-cl-defstruct-docstring-test--root))
        (add-to-list 'load-path
                     ,(expand-file-name "packages/nelisp-actor/src"
                                        nelisp-cl-defstruct-docstring-test--root))
        (require 'nelisp-actor)
        (let ((host-setf (symbol-function 'setf))
              (host-defstruct (symbol-function 'cl-defstruct)))
          (load ,nelisp-cl-defstruct-docstring-test--file nil t)
          (let ((actor (nelisp-actor--make)))
            (funcall (eval '(lambda (actor)
                             (setf (nelisp-actor-status actor) :dead)) t)
                     actor)
            (prin1 (list (eq host-setf (symbol-function 'setf))
                         (eq host-defstruct (symbol-function 'cl-defstruct))
                         (nelisp-actor-status actor)))))))
    '(t t :dead))))

(provide 'nelisp-cl-defstruct-docstring-test)

;;; nelisp-cl-defstruct-docstring-test.el ends here
