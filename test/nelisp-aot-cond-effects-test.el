;;; nelisp-aot-cond-effects-test.el --- Cond result elision -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(ert-deftest nelisp-aot-cond-effects/discarded-body-uses-effect-context ()
  (let* ((nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--promoted-slots '((0 . r8)))
         (node (nelisp-aot-compiler--make-ir
                'cond :id 'cond-effect-gauge :clauses
                (list (cons 'always (nelisp-aot-compiler--make-ir
                                     'setq-local :slot 0 :value-ir
                                     (nelisp-aot-compiler--make-ir
                                      'arith :op '+ :a
                                      (nelisp-aot-compiler--make-ir 'ref :slot 0 :class 'gp)
                                      :b (nelisp-aot-compiler--make-ir 'imm :value 1)))))))
         (selected 0)
         (original (symbol-function 'nelisp-aot-compiler--emit-setq-direct-x86)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-setq-direct-x86)
               (lambda (&rest args) (cl-incf selected) (apply original args))))
      (nelisp-aot-compiler--emit-effect node (nelisp-asm-x86_64-make-buffer))
      (should (= selected 1))
      (setq selected 0)
      (nelisp-aot-compiler--emit-value node (nelisp-asm-x86_64-make-buffer))
      (should (= selected 0)))))

(ert-deftest nelisp-aot-cond-effects/gnu-native-first-match-and-nesting ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((cases 0))
    (dolist (dispatch
             '((cond ((= i 0) (setq seen (+ seen 1)) (setq sum (+ sum 3)))
                     ((< i 2) (setq seen (+ seen 1)) (setq sum (+ sum 7)))
                     ((< i 4) (setq seen (+ seen 1)) (setq sum (+ sum 11)))
                     (t (setq seen (+ seen 1)) (setq sum (+ sum 13))))
               (cond ((= i 0) (setq seen (+ seen 1)) (setq sum (+ sum 3)))
                     ((= i 3) (setq seen (+ seen 1)) (setq sum (+ sum 7))))
               (cond ((progn (setq seen (+ seen 1)) (< i 3))
                      (cond ((= i 1) (setq sum (+ sum 7)))
                            (t (setq sum (+ sum 3)))))
                     ((progn (setq seen (+ seen 1)) (< i 6))
                      (setq sum (+ sum 11))))
               (cond ((= i 0) (setq sum -9223372036854775808))
                     ((= i 1) (setq sum 9223372036854775807))
                     (t (setq sum (if (< sum 0) 29 31))))))
      (dolist (n '(0 1 2 3 4 5 6 7))
        (let* ((body `(let ((i 0) (sum 0) (seen 0))
                        (while (< i n) ,dispatch (setq i (+ i 1)))
                        (+ sum (* seen 16))))
               (golden (eval `(let ((n ,n)) ,body) t))
               (path (make-temp-file "cond-effects-")))
          (unwind-protect
              (progn
                (nelisp-aot-compile-sexp
                 `(seq (defun f (n) ,body) (exit (if (= (f ,n) ,golden) 0 1))) path)
                (should (= (call-process path nil nil nil) 0)))
            (delete-file path))
          (cl-incf cases))))
    (message "Cond effects GNU/native cases: %d" cases)))
(provide 'nelisp-aot-cond-effects-test)
;;; nelisp-aot-cond-effects-test.el ends here
