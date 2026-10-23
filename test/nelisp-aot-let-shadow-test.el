;;; nelisp-aot-let-shadow-test.el --- Constant binding scope -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-shadow-test--gnu-form (form)
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ((eq (car form) 'exit)
         `(throw 'let-shadow-exit ,(nelisp-aot-shadow-test--gnu-form (cadr form))))
        (t (cons (if (eq (car form) 'seq) 'progn (car form))
                 (mapcar #'nelisp-aot-shadow-test--gnu-form (cdr form))))))

(ert-deftest nelisp-aot-let/constant-shadows-runtime-local ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((count 0))
    (dolist (body
             '((let ((copy x)) (let ((x 9)) (+ copy x)))
               (let ((copy x) (x 9)) (+ copy x))
               (let ((copy x)) (let ((copy 9)) (+ copy x)))
               (let ((x 9) (y (+ x 1))) (+ x y))
               (let ((result 0)) (let ((x 9)) (setq result (+ x 3))) result)
               (let ((result 0))
                 (let ((x 9) (y (+ x 1))) (setq result (+ x y))) result)
               (let ((x 9)) (let ((x (+ x 1))) (setq x (+ x 1)) x))))
      (dolist (n '(0 3 4 19))
        (let* ((program `(seq (defun f (x) ,body) (exit (f ,n))))
               (golden (catch 'let-shadow-exit
                         (eval (nelisp-aot-shadow-test--gnu-form program) t)))
               (path (make-temp-file "let-shadow-")))
          (unwind-protect
              (progn (nelisp-aot-compile-sexp program path)
                     (should (= (call-process path nil nil nil) golden)))
            (delete-file path))
          (setq count (1+ count)))))
    (message "Constant-let shadow GNU/native cases: %d" count)))

(provide 'nelisp-aot-let-shadow-test)
;;; nelisp-aot-let-shadow-test.el ends here
