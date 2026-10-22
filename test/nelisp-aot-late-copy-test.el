;;; nelisp-aot-late-copy-test.el --- Late readonly copy coalescing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-late-copy-test--node (body)
  (nelisp-aot-compiler--make-ir
   'let-rt :slot 1 :copy-candidate t
   :value-ir (nelisp-aot-compiler--make-ir 'ref :slot 0 :class 'gp)
   :body body))

(ert-deftest nelisp-aot-late-copy/selection-retains-cache-and-ownership ()
  (let* ((nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--promoted-slots '((0 . r8) (1 . r9)))
         (ref (nelisp-aot-compiler--make-ir 'ref :slot 1 :class 'gp))
         (node (nelisp-aot-late-copy-test--node ref)))
    (should (eq 'r8 (nelisp-aot-compiler--x86-copy-let-source-register node)))
    (dolist (slot '(0 1))
      (should-not
       (nelisp-aot-compiler--x86-copy-let-source-register
        (nelisp-aot-late-copy-test--node
         (nelisp-aot-compiler--make-ir
          'setq-local :slot slot :value-ir
          (nelisp-aot-compiler--make-ir 'imm :value 9))))))
    (let ((nelisp-aot-compiler--promoted-slots '((1 . r9))))
      ;; Keep the original copy when it caches an unallocated source.
      (should-not (nelisp-aot-compiler--x86-copy-let-source-register node)))
    (let ((nelisp-aot-compiler--runtime-entry-params '(x)))
      (should-not (nelisp-aot-compiler--x86-copy-let-source-register node)))
    (let ((nelisp-aot-compiler--checked-arith t))
      (should-not (nelisp-aot-compiler--x86-copy-let-source-register node)))
    (should-not (nelisp-aot-compiler--x86-copy-let-source-register
                 (nelisp-aot-late-copy-test--node
                  (nelisp-aot-compiler--make-ir 'value-seq :forms nil))))
    (let ((node (nelisp-aot-compiler--ir-with node :copy-candidate nil)))
      (should-not (nelisp-aot-compiler--x86-copy-let-source-register node)))
    (let* ((root (nelisp-aot-compiler--make-ir 'ref :slot 0 :class 'gp :root-p t))
           (node (nelisp-aot-compiler--ir-with node :value-ir root)))
      (should-not (nelisp-aot-compiler--x86-copy-let-source-register node)))))

(defun nelisp-aot-late-copy-test--gnu-form (form)
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ((eq (car form) 'exit)
         `(throw 'late-copy-exit ,(nelisp-aot-late-copy-test--gnu-form (cadr form))))
        (t (cons (if (eq (car form) 'seq) 'progn (car form))
                 (mapcar #'nelisp-aot-late-copy-test--gnu-form (cdr form))))))

(defvar nelisp-aot-late-copy-test--selected 0)
(defun nelisp-aot-late-copy-test--record (reg)
  (when reg (setq nelisp-aot-late-copy-test--selected
                  (1+ nelisp-aot-late-copy-test--selected)))
  reg)

(ert-deftest nelisp-aot-late-copy/gnu-native-loop-lifetimes ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (advice-add 'nelisp-aot-compiler--x86-copy-let-source-register
              :filter-return #'nelisp-aot-late-copy-test--record)
  (unwind-protect
      (let ((cases 0))
        (dolist (entry '((t (let ((copy x)) (setq sum (+ sum copy))))
                         (nil (let ((copy x)) (setq x (+ x 1)) (setq sum (+ sum copy))))
                         (nil (let ((copy x)) (setq copy (+ copy 1)) (setq sum (+ sum copy))))))
          (dolist (n '(0 1 5))
            (let* ((program `(seq
                             (defun f (n x)
                               (let ((i 0) (sum 0))
                                 (while (< i n) ,(cadr entry) (setq i (+ i 1))) sum))
                             (exit (f ,n 3))))
                   (golden (catch 'late-copy-exit
                             (eval (nelisp-aot-late-copy-test--gnu-form program) t)))
                   (nelisp-aot-late-copy-test--selected 0)
                   (path (make-temp-file "late-copy-")))
              (unwind-protect
                  (progn (nelisp-aot-compile-sexp program path)
                         (should (= (call-process path nil nil nil) golden))
                         (should (eq (car entry)
                                     (> nelisp-aot-late-copy-test--selected 0))))
                (delete-file path))
              (setq cases (1+ cases)))))
        (message "Late copy GNU/native cases: %d" cases))
    (advice-remove 'nelisp-aot-compiler--x86-copy-let-source-register
                   #'nelisp-aot-late-copy-test--record)))

(provide 'nelisp-aot-late-copy-test)
;;; nelisp-aot-late-copy-test.el ends here
