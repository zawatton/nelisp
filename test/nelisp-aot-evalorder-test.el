;;; nelisp-aot-evalorder-test.el --- Operand sequencing -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(ert-deftest nelisp-aot-evalorder/emitter-order ()
  "Arithmetic, shifts and comparisons sequence effectful operands on both ISAs."
  (dolist (arch '(x86_64 aarch64))
    (dolist (entry '((arith +) (arith -) (arith *) (arith /) (arith mod)
                     (arith logior) (arith logand) (arith logxor)
                     (shift shl) (shift sar) (shift shr)
                     (cmp =) (cmp /=) (cmp <) (cmp >) (cmp <=) (cmp >=)))
      (let* ((nelisp-aot-compiler--arch arch)
             (a (nelisp-aot-compiler--make-ir 'call :name 'left))
             (b (nelisp-aot-compiler--make-ir 'call :name 'right))
             (node (nelisp-aot-compiler--make-ir (car entry) :op (cadr entry) :a a :b b))
             (buf (if (eq arch 'aarch64)
                      (nelisp-asm-arm64-make-buffer)
                    (nelisp-asm-x86_64-make-buffer)))
             order)
        (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-value)
                   (lambda (operand _buf) (push operand order))))
          (funcall (intern (format "nelisp-aot-compiler--emit-%s" (car entry))) node buf))
        (should (equal (nreverse order) (list a b)))))))

(ert-deftest nelisp-aot-evalorder/local-mutation-e2e ()
  "Execute machine code: an earlier local read must precede a later write."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (dolist (entry '((+ 12) (- 2) (* 35) (/ 1) (mod 2)
                   (logior 7) (logand 5) (logxor 2) (< 0) (> 1) (= 0)))
    (let ((path (make-temp-file "aot-evalorder-")))
      (unwind-protect
          (progn
            (nelisp-aot-compile-sexp
             `(seq (defun probe () (let ((x 7)) (,(car entry) x (seq (setq x 5) x)))) (exit (probe))) path)
            (should (= (call-process path nil nil nil) (cadr entry))))
        (delete-file path)))))

(ert-deftest nelisp-aot-evalorder/f64-emitter-order ()
  "Float conversions may contain calls and must retain source order."
  (dolist (arch '(x86_64 aarch64))
    (dolist (entry '((f64-binop f64-add) (f64-binop f64-sub)
                     (f64-binop f64-mul) (f64-binop f64-div)
                     (f64-cmp f64-lt) (f64-cmp f64-le)
                     (f64-cmp f64-gt) (f64-cmp f64-ge)))
      (let* ((nelisp-aot-compiler--arch arch)
             (a (nelisp-aot-compiler--make-ir 'i64-to-f64 :int-expr
                  (nelisp-aot-compiler--make-ir 'call :name 'left)))
             (b (nelisp-aot-compiler--make-ir 'bits-to-f64 :int-expr
                  (nelisp-aot-compiler--make-ir 'call :name 'right)))
             (node (nelisp-aot-compiler--make-ir (car entry) :op (cadr entry) :a a :b b))
             (buf (if (eq arch 'aarch64)
                      (nelisp-asm-arm64-make-buffer)
                    (nelisp-asm-x86_64-make-buffer)))
             order)
        (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-f64-leaf-into)
                   (lambda (operand _buf _reg) (push operand order))))
          (funcall (intern (format "nelisp-aot-compiler--emit-%s" (car entry))) node buf))
        (should (equal (nreverse order) (list a b)))))))

(ert-deftest nelisp-aot-evalorder/f64-local-mutation-e2e ()
  "Run a float comparison whose right conversion mutates the left local."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((path (make-temp-file "aot-f64-evalorder-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq (defun probe ()
                   (let ((x 7))
                     (f64-gt (i64-to-f64 x)
                             (i64-to-f64 (seq (setq x 5) x)))))
                 (exit (probe))) path)
          (should (= (call-process path nil nil nil) 1)))
      (delete-file path))))

(ert-deftest nelisp-aot-evalorder/pure-bytes-unchanged ()
  "Independent operands retain the exact historical spill sequence."
  (dolist (arch '(x86_64 aarch64))
    (let ((nelisp-aot-compiler--arch arch))
      (dolist (pair
               (list
                (list (nelisp-aot-compiler--make-ir 'ref :slot 0)
                      (nelisp-aot-compiler--make-ir 'ref :slot 1))
                (list (nelisp-aot-compiler--make-ir 'imm :value 7)
                      (nelisp-aot-compiler--make-ir 'ref :slot 1))
                (list (nelisp-aot-compiler--make-ir 'ref :slot 0)
                      (nelisp-aot-compiler--make-ir 'imm :value 5))))
        (let* ((a (car pair)) (b (cadr pair))
               (make-buffer (if (eq arch 'aarch64)
                                #'nelisp-asm-arm64-make-buffer
                              #'nelisp-asm-x86_64-make-buffer))
               (bytes (if (eq arch 'aarch64)
                          #'nelisp-asm-arm64-buffer-bytes
                        #'nelisp-asm-x86_64-buffer-bytes))
               (old (funcall make-buffer)) (new (funcall make-buffer)))
          (nelisp-aot-compiler--emit-value b old)
          (if (eq arch 'aarch64)
              (progn
                (nelisp-asm-arm64-str-pre-sp-16 old 'x0)
                (nelisp-aot-compiler--emit-value a old)
                (nelisp-asm-arm64-ldr-post-sp-16 old 'x9))
            (nelisp-aot-compiler--emit-temp-push old 'rax)
            (nelisp-aot-compiler--emit-value a old)
            (nelisp-aot-compiler--emit-temp-pop old 'r10))
          (nelisp-aot-compiler--emit-binary-operands a b new)
          (should (equal (funcall bytes old) (funcall bytes new)))
          (should (= nelisp-aot-compiler--rsp-temp-depth 0)))))))

(ert-deftest nelisp-aot-evalorder/general-call-order ()
  "Direct and external calls retain source order with register/stack arguments."
  (dolist (arch '(x86_64 aarch64))
    (let ((nelisp-aot-compiler--arch arch)
          (nelisp-aot-compiler--abi 'sysv)
          (nelisp-aot-compiler--object-mode t))
      (dolist (shape '(call extern-call))
        (dolist (n '(2 9))
          (let* ((nelisp-aot-compiler--rsp-temp-depth 0)
                 (args (cl-loop for i below n collect
                               (nelisp-aot-compiler--make-ir 'call :cls 'gp :name (intern (format "arg%d" i)))))
                 (node (nelisp-aot-compiler--make-ir shape :name 'callee :args args))
                 (buf (if (eq arch 'aarch64)
                          (nelisp-asm-arm64-make-buffer)
                        (nelisp-asm-x86_64-make-buffer)))
                 order)
            (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-value)
                       (lambda (operand _buf) (push operand order))))
              (if (and (eq arch 'aarch64) (eq shape 'extern-call))
                  (nelisp-aot-compiler--emit-extern-call-arm64 node buf)
                (if (and (eq arch 'aarch64) (eq shape 'call))
                    (nelisp-aot-compiler--emit-call-arm64 node buf)
                  (funcall (intern (format "nelisp-aot-compiler--emit-%s" shape)) node buf))))
            (should (equal (nreverse order) args))
            (should (= nelisp-aot-compiler--rsp-temp-depth 0))))))))
