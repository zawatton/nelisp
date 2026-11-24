;;; nelisp-boot-image-byte-run-bake-test.el --- compiled defun expander parity  -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-boot-image--byte-run-bake' installs host-compiled versions of byte-run.el's
;; `defun' expander and `byte-run--parse-body'.  The expansion of a corpus of real
;; `defun' argument lists must be `equal' to the source expander's.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-boot-image)
(require 'nelisp-prelude-bytecode)

(defvar nelisp-bake-test--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-bake-test--source-lambda (name)
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "vendor/emacs-lisp/emacs-lisp/byte-run.el"
                                                        nelisp-bake-test--root))
                (buffer-string))))
    (cl-some (lambda (span)
               (let ((spec (nelisp-boot-image--byte-run-lambda (nth 2 span))))
                 (and spec (eq (car spec) name) spec)))
             (nelisp-prelude-bytecode--top-level-spans text))))

(defun nelisp-bake-test--baked-function (name)
  "The object the baked text installs for NAME (macro functions without the `macro' cons)."
  (let* ((spec (nelisp-bake-test--source-lambda name))
         (text (apply #'nelisp-boot-image--byte-run-compile spec))
         (form (car (read-from-string text)))   ; (fset 'NAME FN)
         (fn (nth 2 form)))
    (should text)
    (eval (if (nth 1 spec) (nth 2 fn) fn) t)))

(defun nelisp-bake-test--source-function (name)
  (eval (nth 2 (nelisp-bake-test--source-lambda name)) t))

(defconst nelisp-bake-test--corpus
  '((foo (a b) "Doc." (+ a b))
    (foo nil)
    (foo (x) (declare (indent 1) (side-effect-free t)) x)
    (foo (x) "Doc." (declare (pure t)) (interactive "p") x)
    (foo (&optional x &rest y) (interactive) (list x y))
    (foo (x) "Doc.\n\n(fn X)" x)
    (foo (x) "only a string")
    (foo (x) (interactive "P") "string last")
    (foo (x) "a" "b" x)
    (foo (x) (declare (obsolete bar "1.0")) x)
    (foo (x) (declare (indent 1)) (declare (pure t)) x)
    (foo (x) (interactive "p") (declare (pure t)) x)
    (foo (x) :documentation "d" x)
    (foo (x) (let ((y 1)) (setq y x)) (cond ((> y 1) y) (t nil))))
  "Argument lists of the `defun' expander.")

(ert-deftest nelisp-byte-run-bake-defun-expansion-equal ()
  (let ((src-parse (nelisp-bake-test--source-function 'byte-run--parse-body))
        (src-defun (nelisp-bake-test--source-function 'defun))
        (bk-parse (let ((f (nelisp-bake-test--baked-function 'byte-run--parse-body)))
                    ;; NELISP_BAKE_TEST_SABOTAGE=1 proves the comparison can go red.
                    (if (getenv "NELISP_BAKE_TEST_SABOTAGE")
                        (lambda (body _allow) (funcall f body nil))
                      f)))
        (bk-defun (nelisp-bake-test--baked-function 'defun)))
        (should (byte-code-function-p bk-defun))
    (dolist (args nelisp-bake-test--corpus)
      (let ((a (cl-letf (((symbol-function 'byte-run--parse-body) src-parse)) (apply src-defun args)))
            (b (cl-letf (((symbol-function 'byte-run--parse-body) bk-parse)) (apply bk-defun args))))
        (should (equal (format "%S" a) (format "%S" b)))))))

(ert-deftest nelisp-byte-run-bake-parse-body-equal ()
  (let ((src (nelisp-bake-test--source-function 'byte-run--parse-body))
        (bk (nelisp-bake-test--baked-function 'byte-run--parse-body)))
    (dolist (body '(nil ("d" x) ("d") (x) ((declare (indent 1)) x) ((interactive) x)
                    ("a" "b" x) ((declare a) (declare b) x) ("d" (interactive "p") (declare a) x)))
      (dolist (allow '(nil t))
        (should (equal (format "%S" (funcall src body allow))
                       (format "%S" (funcall bk body allow))))))))

(ert-deftest nelisp-byte-run-bake-errors-equal ()
  (let ((src-defun (nelisp-bake-test--source-function 'defun))
        (bk-defun (nelisp-bake-test--baked-function 'defun)))
    (dolist (args '((nil (x) x) (foo (x . y) x) (foo "not-list" x)))
      (should (equal (condition-case e (apply src-defun args) (error e))
                     (condition-case e (apply bk-defun args) (error e)))))))

(provide 'nelisp-boot-image-byte-run-bake-test)
;;; nelisp-boot-image-byte-run-bake-test.el ends here
