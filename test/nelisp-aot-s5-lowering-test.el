;;; nelisp-aot-s5-lowering-test.el --- Generic S5 lowering contracts -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Existing codegen and evalorder suites cover executable control flow and
;; call order.  These tests cover the conservative selection and ABI borders.
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(defun nelisp-aot-s5-test--ir (kind &rest fields)
  (apply #'nelisp-aot-compiler--make-ir kind fields))
(defun nelisp-aot-s5-test--ref (slot &optional class)
  (nelisp-aot-s5-test--ir 'ref :slot slot :class (or class 'gp) :cls (or class 'gp)))
(defun nelisp-aot-s5-test--imm (value)
  (nelisp-aot-s5-test--ir 'imm :value value))
(defun nelisp-aot-s5-test--read (offset)
  (nelisp-aot-s5-test--ir 'ptr-read-u8 :ptr (nelisp-aot-s5-test--ref 0)
                          :offset (nelisp-aot-s5-test--ir
                                   'arith :op '+ :a (nelisp-aot-s5-test--ref 1)
                                   :b (nelisp-aot-s5-test--imm offset))))
(ert-deftest nelisp-aot-s5/predicate-result-profitability ()
  (let* ((raw (nelisp-aot-s5-test--ref 0))
         (cmp (nelisp-aot-s5-test--ir 'cmp :op '> :a raw :b (nelisp-aot-s5-test--imm 4))))
    ;; A Boolean test does not make two raw result arms profitable.
    (should-not (nelisp-aot-compiler--branch-structured-result-p
                 (nelisp-aot-s5-test--ir 'if :test cmp :then raw :else raw)))
    ;; Earlier effects stay outside the final result's classification.
    (should (nelisp-aot-compiler--branch-structured-result-p
             (nelisp-aot-s5-test--ir
              'value-seq :forms (list (nelisp-aot-s5-test--ir 'call :name 'effect) cmp))))
    (should (nelisp-aot-compiler--branch-structured-result-p (nelisp-aot-s5-test--imm -7)))))
(ert-deftest nelisp-aot-s5/direct-args-eligibility-and-abi ()
  (let* ((nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--abi 'sysv)
         (args (list (nelisp-aot-s5-test--ref 15) (nelisp-aot-s5-test--imm 4294967295)))
         (call (nelisp-aot-s5-test--ir 'call :name 'callee :args args)))
    (should (nelisp-aot-compiler--x86-direct-call-args-p call))
    (dolist (bad (list
                 (nelisp-aot-s5-test--ir 'call :name 'callee :args
                                         (list (nelisp-aot-s5-test--ir 'call :name 'effect)))
                 (nelisp-aot-s5-test--ir 'call :name 'callee :args args :fn-value (car args))
                 (nelisp-aot-s5-test--ir 'extern-call :name 'callee :args args :varargs-p t)
                 (nelisp-aot-s5-test--ir 'extern-call :name 'callee :args
                                         (list (nelisp-aot-s5-test--ref 0 'f64)))))
      (should-not (nelisp-aot-compiler--x86-direct-call-args-p bad)))
    (dolist (abi '((x86_64 win64) (aarch64 sysv)))
      (let ((nelisp-aot-compiler--arch (car abi)) (nelisp-aot-compiler--abi (cadr abi)))
        (should-not (nelisp-aot-compiler--x86-direct-call-args-p call))))))
(ert-deftest nelisp-aot-s5/direct-args-reject-source-register-cycle ()
  (let ((nelisp-aot-compiler--arch 'x86_64)
        (nelisp-aot-compiler--abi 'sysv)
        (nelisp-aot-compiler--promoted-slots '((15 . rsi) (16 . rdi))))
    (should-not
     (nelisp-aot-compiler--x86-direct-call-args-p
      (nelisp-aot-s5-test--ir 'call :name 'callee
                              :args (list (nelisp-aot-s5-test--ref 15)
                                          (nelisp-aot-s5-test--ref 16)))))))
(ert-deftest nelisp-aot-s5/compact-vector-convention-is-explicit ()
  (should (eq (default-value 'nelisp-aot-compiler--vector-value-convention) 'legacy-view))
  (let ((nelisp-aot-compiler--vector-value-convention 'compact-word))
    (should
     (equal (should-error (nelisp-aot-compiler--emit-vector-len-arm64 nil nil)
                          :type 'nelisp-aot-compiler-error)
            '(nelisp-aot-compiler-error :unsupported-arm64-vector-value-convention compact-word)))))
(ert-deftest nelisp-aot-s5/memory-tree-register-temporaries ()
  (let* ((nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (nelisp-aot-compiler--rsp-temp-depth 1)
         (a (nelisp-aot-s5-test--read 1))
         (b (nelisp-aot-s5-test--ir 'shift :op 'shl :a (nelisp-aot-s5-test--read 2)
                                    :b (nelisp-aot-s5-test--imm 8)))
         (buf (nelisp-asm-x86_64-make-buffer)))
    (should (equal (nelisp-aot-compiler--x86-memory-tree-info b) '(1 . t)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-temp-push)
               (lambda (&rest _) (ert-fail "pure tree spilled")))
              ((symbol-function 'nelisp-aot-compiler--emit-temp-pop)
               (lambda (&rest _) (ert-fail "pure tree spilled"))))
      (should (nelisp-aot-compiler--emit-memory-tree-binop-x86 '+ a b buf)))
    (should (= nelisp-aot-compiler--rsp-temp-depth 1))
    (should (> (length (nelisp-asm-x86_64-buffer-bytes buf)) 0))))
(ert-deftest nelisp-aot-s5/memory-tree-preflight-rejects-effects-and-budget ()
  (let* ((nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (read (nelisp-aot-s5-test--read 1))
         (pair (nelisp-aot-s5-test--ir 'arith :op '+ :a read :b read))
         (triple (nelisp-aot-s5-test--ir 'arith :op '+ :a read :b pair)))
    (dolist (b (list (nelisp-aot-s5-test--ir 'call :name 'effect)
                     (nelisp-aot-s5-test--ir 'setq-local :slot 2 :value-ir read)
                     (nelisp-aot-s5-test--ir 'shift :op 'shl :a read :b (nelisp-aot-s5-test--ref 2))
                     triple))
      (let ((buf (nelisp-asm-x86_64-make-buffer)))
        (should-not (nelisp-aot-compiler--emit-memory-tree-binop-x86 '+ read b buf))
        (should (equal (nelisp-asm-x86_64-buffer-bytes buf) ""))))))
(ert-deftest nelisp-aot-s5/memory-tree-preserves-checked-and-live-registers ()
  (let ((read (nelisp-aot-s5-test--read 1)))
    (dolist (config '((t nil) (nil ((0 . r10))) (nil ((3 . rdi)))))
      (let ((nelisp-aot-compiler--checked-arith (car config))
            (nelisp-aot-compiler--promoted-slots (cadr config))
            (buf (nelisp-asm-x86_64-make-buffer)))
        (should-not (nelisp-aot-compiler--emit-memory-tree-binop-x86 '+ read read buf))
        (should (equal (nelisp-asm-x86_64-buffer-bytes buf) ""))))
    (let ((nelisp-aot-compiler--checked-arith nil)
          (nelisp-aot-compiler--promoted-slots nil)
          (buf (nelisp-asm-x86_64-make-buffer)))
      (should-not (nelisp-aot-compiler--emit-memory-tree-binop-x86
                   '+ (nelisp-aot-s5-test--ref 0) (nelisp-aot-s5-test--ref 1) buf))
      (should (equal (nelisp-asm-x86_64-buffer-bytes buf) "")))))
(provide 'nelisp-aot-s5-lowering-test)
;;; nelisp-aot-s5-lowering-test.el ends here
