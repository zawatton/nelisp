;;; nelisp-standalone-inline-hygiene-test.el --- Hygienic argument binders -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(defun nelisp-inline-init-test--expand (source external)
  (let ((process-environment (copy-sequence process-environment))
        (nelisp-standalone--inline-counter 0)
        (nelisp-standalone--xinline-used (make-hash-table :test 'eq))
        (nelisp-standalone--xinline-table (make-hash-table :test 'eq)))
    (setenv "NELISP_INLINE" "2")
    (if external
        (progn
          (dolist (f external)
            (puthash (cadr f) (cons (nth 2 f) (nth 3 f))
                     nelisp-standalone--xinline-table))
          (nelisp-standalone--xinline-expand source))
      (nelisp-standalone--inline-small-defuns source))))

(defun nelisp-inline-init-test--calls (source name)
  (let ((count 0))
    (cl-labels ((walk (x)
                  (when (consp x)
                    (when (eq (car x) name) (cl-incf count))
                    (cond
                     ((memq (car x) '(quote data-addr)))
                     ((memq (car x) '(let let*))
                      (dolist (binding (cadr x))
                        (when (consp binding) (walk (cadr binding))))
                      (dolist (child (cddr x)) (walk child)))
                     (t (dolist (child x) (walk child)))))))
      (walk source))
    count))

(defun nelisp-inline-init-test--run (program)
  (let ((path (make-temp-file "nelisp-inline-init-")))
    (unwind-protect
        (progn (nelisp-aot-compile-sexp program path)
               (should (= 0 (call-process path nil nil nil))))
      (delete-file path))))

(ert-deftest nelisp-inline-hygiene/gnu-native-fullword-and-effect-order ()
  (let ((helper '(defun nii_mix (x y) (let* ((temp (+ x y))) (- (* temp 3) y))))
        (values '(0 1 -1 4294967295 4294967296 9223372036854775807
                    -9223372036854775808 18446744073709551615))
        (count 0))
    (dolist (kind '(let let*))
      (dolist (cross '(nil t))
        (let* ((body `(let ((state 0))
                       (logxor (+ (nii_mix (seq (setq state (+ state 1)) q) (+ q state))
                                  (nii_mix (seq (setq state (+ state 3)) q) state))
                               (* state 7))))
               (caller `(defun nii_owner (q) ,body))
               (source `(seq ,@(unless cross (list helper)) ,caller))
               (out (nelisp-inline-init-test--expand source (and cross (list helper)))))
          (dolist (v values)
            (let ((expected (logand #xffffffffffffffff
                                   (eval `(progn ,helper
                                                 (let ((q ,v)) ,(cl-subst 'progn 'seq body))) t))))
              (nelisp-inline-init-test--run
               `(seq ,@(and cross (list helper)) ,@(cdr out)
                     (exit (if (= (nii_owner ,v) ,expected) 0 91))))
              (cl-incf count))))))
    (should (= count 32))
    (message "Inline initializer GNU/native full-word cases: %d" count)))

(ert-deftest nelisp-inline-hygiene/fresh-arguments-avoid-caller-and-callee-capture ()
  (let ((count 0))
    (dolist (cross '(nil t))
      (dolist (helper '((defun nii_sum (x y) (+ x y))
                        (defun nii_sum (x y) (let* ((x__i1 7) (x__x1 9)) (+ x y x__i1 x__x1)))))
        (let* ((body '(let* ((x__i1 99) (x__x1 101)) (nii_sum 1 (+ x__i1 x__x1))))
               (expected (eval `(progn ,helper ,body) t))
               (source `(seq ,@(unless cross (list helper)) (defun nii_owner () ,body)))
               (out (nelisp-inline-init-test--expand source (and cross (list helper)))))
          (nelisp-inline-init-test--run
           `(seq ,@(and cross (list helper)) ,@(cdr out)
                 (exit (if (= (nii_owner) ,expected) 0 92))))
          (cl-incf count))))
    (should (= count 4))))

(ert-deftest nelisp-inline-hygiene/protected-forms-and-arity ()
  (let* ((helper '(defun nii_leaf (x) (+ x 1)))
         (spawn '(defun nii_spawn () (seq (syscall-direct 56 0 0 0 0 0 0)
                                          (let ((v (nii_leaf 3))) v))))
         (source `(seq ,helper ,spawn
                       (defun nii_owner ()
                         (let ((v (nii_leaf 1 2))) (quote ((nii_leaf 9)))))))
         (out (nelisp-inline-init-test--expand source nil)))
    (should (equal spawn (nth 2 out)))
    (should (equal (nth 3 source) (nth 3 out)))))

(provide 'nelisp-standalone-inline-hygiene-test)
