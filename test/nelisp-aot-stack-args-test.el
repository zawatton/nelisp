;;; nelisp-aot-stack-args-test.el --- Direct SysV GP stack tail -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-stack-test--node (&optional count)
  (nelisp-aot-compiler--make-ir
   'call :name 'callee
   :args (cl-loop for i below (or count 8)
                  collect (nelisp-aot-compiler--make-ir 'ref :slot (+ i 14) :class 'gp))))

(ert-deftest nelisp-aot-stack-args/preflight-borders ()
  (let* ((nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (node (nelisp-aot-stack-test--node)))
    (should (nelisp-aot-compiler--x86-direct-gp-stack-call-p node))
    (should-not (nelisp-aot-compiler--x86-direct-gp-stack-call-p
                 (nelisp-aot-stack-test--node 6)))
    (dolist (pair '((:varargs-p . t) (:ret-class . f64)
                    (:internal-boundary-disp . 0) (:fn-value . indirect)))
      (should-not (nelisp-aot-compiler--x86-direct-gp-stack-call-p
                   (nelisp-aot-compiler--ir-with node (car pair) (cdr pair)))))
    (dolist (arg (list (nelisp-aot-compiler--make-ir 'call :name 'effect)
                       (nelisp-aot-compiler--make-ir 'ref :slot 0 :class 'f64)))
      (should-not (nelisp-aot-compiler--x86-direct-gp-stack-call-p
                   (nelisp-aot-compiler--ir-with
                    node :args (cons arg (cdr (nelisp-aot-compiler--ir-get node :args)))))))
    (dolist (reg '(rax rdi rsi rdx rcx r8 r9))
      (let ((nelisp-aot-compiler--promoted-slots (list (cons 14 reg))))
        (should-not (nelisp-aot-compiler--x86-direct-gp-stack-call-p node))))
    (let ((nelisp-aot-compiler--abi 'win64))
      (should-not (nelisp-aot-compiler--x86-direct-gp-stack-call-p node)))
    (let ((nelisp-aot-compiler--runtime-entry-params '(x)))
      (should-not (nelisp-aot-compiler--x86-direct-gp-stack-call-p node)))))

(defun nelisp-aot-stack-test--run (program)
  (let ((path (make-temp-file "gp-stack-")))
    (unwind-protect
        (progn (nelisp-aot-compile-sexp program path)
               (call-process path nil nil nil))
      (delete-file path))))
(defun nelisp-aot-stack-test--signed (n)
  (let ((u (logand n (1- (ash 1 64)))))
    (if (>= u (ash 1 63)) (- u (ash 1 64)) u)))
(defun nelisp-aot-stack-test--sum (params)
  (cl-reduce (lambda (a b) (list '+ a b))
             (cl-loop for p in params for i from 0 collect (list '* p (+ 3 (* i 2))))))

(ert-deftest nelisp-aot-stack-args/named-full-width-and-side-effects ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((cases 0))
    (dolist (n '(7 8 10 12))
      (let* ((params (cl-loop for i below n collect (intern (format "p%d" i))))
             (sum (nelisp-aot-stack-test--sum params)))
        (dolist (wide '(nil t))
          (let* ((args (cl-loop for i below n
                                collect (if wide (+ (ash 1 48) (* i 4294967301)) (+ i 1))))
                 (golden (nelisp-aot-stack-test--signed
                          (eval `(let ,(cl-mapcar #'list params args) ,sum) t))))
            (should (= 0 (nelisp-aot-stack-test--run
                          `(seq (defun callee ,params ,sum)
                                (defun sender ,params (callee ,@params))
                                (exit (if (= (sender ,@args) ,golden) 0 1))))))
            (setq cases (1+ cases))))))
    ;; Effectful mixed args retain left-to-right evaluation and the old path.
    (should (= 28 (nelisp-aot-stack-test--run
                   '(seq (defun callee (a b c d e f g)
                           (+ (+ (+ a b) (+ c d)) (+ (+ e f) g)))
                         (defun sender (state)
                           (callee (seq (setq state 1) state)
                                   (seq (setq state (+ state 1)) state)
                                   (seq (setq state (+ state 1)) state)
                                   4 5 6 7))
                         (exit (sender 0))))))
    (message "GP stack named GNU/native full-width cases: %d; effectful fallback:1" cases)))

(defvar nelisp-aot-stack-test--push-count nil)
(defun nelisp-aot-stack-test--count-push (&rest _)
  (when nelisp-aot-stack-test--push-count
    (setcar nelisp-aot-stack-test--push-count (1+ (car nelisp-aot-stack-test--push-count)))))
(defun nelisp-aot-stack-test--check-call (fn node buf)
  (let ((nelisp-aot-stack-test--push-count
         (and (eq (nelisp-aot-compiler--ir-get node :name) 'sum7) (list 0))))
    (prog1 (funcall fn node buf)
      (when nelisp-aot-stack-test--push-count
        (should (= 1 (car nelisp-aot-stack-test--push-count)))))))
(ert-deftest nelisp-aot-stack-args/only-outgoing-word-is-pushed ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (advice-add 'nelisp-aot-compiler--emit-call :around #'nelisp-aot-stack-test--check-call)
  (advice-add 'nelisp-aot-compiler--emit-temp-push :before #'nelisp-aot-stack-test--count-push)
  (unwind-protect
      (should (= 28 (nelisp-aot-stack-test--run
                     '(seq (defun sum7 (a b c d e f g)
                             (+ (+ (+ a b) (+ c d)) (+ (+ e f) g)))
                           (exit (sum7 1 2 3 4 5 6 7))))))
    (advice-remove 'nelisp-aot-compiler--emit-call #'nelisp-aot-stack-test--check-call)
    (advice-remove 'nelisp-aot-compiler--emit-temp-push #'nelisp-aot-stack-test--count-push)))

(defvar nelisp-aot-stack-test--depths nil)
(defun nelisp-aot-stack-test--record-depth (node _buf extern-p)
  (when (and extern-p (string-prefix-p "ext" (symbol-name (nelisp-aot-compiler--ir-get node :name))))
    (push nelisp-aot-compiler--rsp-temp-depth nelisp-aot-stack-test--depths)))

(ert-deftest nelisp-aot-stack-args/c-callee-alignment-and-full-width ()
  (skip-unless (and (executable-find "gcc-14") (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let* ((dir (make-temp-file "gp-stack-abi-" t))
         (c (expand-file-name "callee.c" dir))
         (obj (expand-file-name "callee.o" dir))
         (probe (expand-file-name "probe.o" dir))
         (bin (expand-file-name "probe" dir))
         (nelisp-aot-stack-test--depths nil) (cases 0))
    (unwind-protect
        (progn
          (with-temp-file c
            (insert "#include <stdint.h>\n")
            (insert "__asm__(\".text\\n.globl _start\\n_start:\\ncall probe\\nmov %rax,%rdi\\nmov $60,%eax\\nsyscall\\n\");\n")
            (dolist (n '(7 8 10 12))
              (insert (format "uint64_t ext%d(%s) {\n" n
                              (mapconcat (lambda (i) (format "int64_t a%d" i))
                                         (number-sequence 0 (1- n)) ",")))
              ;; With a normal SysV entry, push rbp gives a 16-byte frame base.
              (insert "if ((uintptr_t)__builtin_frame_address(0) & 15) __builtin_trap();\nreturn ")
              (insert (mapconcat (lambda (i) (format "(uint64_t)a%d * UINT64_C(%d)" i (+ 3 (* i 2))))
                                 (number-sequence 0 (1- n)) " + "))
              (insert ";\n}\n")))
          (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-fno-omit-frame-pointer"
                                    "-fno-stack-protector" "-fno-pie" "-c" c "-o" obj)))
          (advice-add 'nelisp-aot-compiler--emit-direct-gp-stack-call-x86
                      :before #'nelisp-aot-stack-test--record-depth)
          (dolist (n '(7 8 10 12))
            (dolist (nested '(nil t))
              (let* ((vars (cl-loop for i below 20 collect (intern (format "v%d" i))))
                     (values (cl-loop for i below 20
                                      collect (* (if (= (logand i 1) 0) 1 -1)
                                                 (+ (ash 1 48) (* i 4294967301)))))
                     (args (append (cl-subseq vars (- 20 n) 19) '(marker)))
                     (arg-values (append (cl-subseq values (- 20 n) 19)
                                         (list (if nested 1 0))))
                     (golden (nelisp-aot-stack-test--signed
                              (+ (cl-loop for v in arg-values for i from 0
                                          sum (* v (+ 3 (* i 2))))
                                 (if nested (* (car values) (cadr values)) 0))))
                     (call `(extern-call ,(intern (format "ext%d" n)) ,@args))
                     (expr (if nested
                               `(+ (seq (setq marker 1) (* ,(car vars) ,(cadr vars))) ,call)
                             call)))
                (nelisp-aot-compile-to-object
                 `(seq (defun seed (x) x)
                       (defun probe ()
                         (let* ,(append (cl-mapcar (lambda (v n) (list v `(seed ,n))) vars values)
                                        '((marker (seed 0))))
                           (if (= ,expr ,golden) 0 1)))) probe)
                (should (= 0 (call-process "gcc-14" nil nil nil "-nostdlib" "-no-pie"
                                          obj probe "-o" bin)))
                (should (= 0 (call-process bin nil nil nil)))
                (setq cases (1+ cases)))))
          (should (equal '(0 1) (sort (delete-dups nelisp-aot-stack-test--depths) #'<)))
          (message "GP stack C ABI cases: %d; incoming temp depths:0/1" cases))
      (advice-remove 'nelisp-aot-compiler--emit-direct-gp-stack-call-x86
                     #'nelisp-aot-stack-test--record-depth)
      (delete-directory dir t))))

(provide 'nelisp-aot-stack-args-test)
;;; nelisp-aot-stack-args-test.el ends here
