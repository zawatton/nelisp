;;; nelisp-bytecode-equal-gateway-test.el --- Opcode154 route -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun bce-test-find (tree predicate)
  "Return the first subtree satisfying PREDICATE."
  (cond ((funcall predicate tree) tree)
        ((consp tree) (or (bce-test-find (car tree) predicate)
                         (bce-test-find (cdr tree) predicate)))))

(defun bce-test-function (name)
  (cl-find-if (lambda (form) (and (eq (car-safe form) 'defun)
                                (eq (cadr form) name)))
              nelisp-standalone--applyfn-bytecode-helpers))

(defconst bce-test-expected-arm
  '((= base 154)
    (if (= (wf_bytecode_call_named env mem sp 2
                                  (data-addr wf_bytecode_name_equal_bytes) 5) 0)
        (setq sp (- sp 1))
      (setq done 4))))

(defun bce-test-arm-valid (arm)
  "Fail closed on routing, arity, provider mode or success/error mutations."
  (unless (equal arm bce-test-expected-arm) (error "Invalid opcode154 route"))
  t)

(ert-deftest bce-production-opcode-and-static-name ()
  (should (equal (cl-remove-if-not (lambda (f) (eq (car-safe f) 'data-blob))
                                  nelisp-standalone--applyfn-bytecode-helpers)
                 '((data-blob wf_bytecode_name_equal_bytes "equal" rodata))))
  (should (bce-test-arm-valid
           (bce-test-find (bce-test-function 'wf_bytecode_execute_frame)
                          (lambda (x) (equal (car-safe x) '(= base 154)))))))

(ert-deftest bce-old-leaf-route-is-rejected ()
  (should-error
   (bce-test-arm-valid
    '((= base 154)
      (seq (ptr-write-u64 next 0
             (if (= (bf_equal2
                     (wf_bytecode_w_view (heap-read-u64 next 0) (wf_bytecode_scratch curfp))
                     (wf_bytecode_w_view (heap-read-u64 top 0) (+ (wf_bytecode_scratch curfp) 32))) 1) 7 3))
           (setq sp (- sp 1)))))))

(ert-deftest bce-error-pop-and-result-order-mutations-are-rejected ()
  (dolist (arm
           '(((= base 154) (seq (setq sp (- sp 1))
                                (wf_bytecode_call_named env mem sp 2
                                  (data-addr wf_bytecode_name_equal_bytes) 5)))
             ((= base 154) (if (= (wf_bytecode_call_named env mem sp 2
                                   (data-addr wf_bytecode_name_equal_bytes) 5) 0)
                               (setq sp (- sp 1)) (setq sp (- sp 1))))
             ((= base 154) (if (= (wf_bytecode_call_named_value env mem sp 2
                                   (data-addr wf_bytecode_name_equal_bytes) 5 1) 0)
                               (setq sp (- sp 1)) (setq done 4)))))
    (should-error (bce-test-arm-valid arm))))

(ert-deftest bce-existing-gateway-mode-result-store-and-roots ()
  (should (equal (cdddr (bce-test-function 'wf_bytecode_call_named))
                 '((wf_bytecode_call_named_value env mem sp argc name name_len 0))))
  (let ((gateway (bce-test-function 'wf_bytecode_call_named_value)))
    (should (bce-test-find gateway
              (lambda (x) (equal x '(heap-read-u64 (+ mem (* (+ first (- k 1)) 8)) 0)))))
    (should (bce-test-find gateway
              (lambda (x) (equal x '(setq rw (nl_apply_function_w func arglist env 0))))))
    (should (bce-test-find gateway
              (lambda (x) (equal x '(if (/= rw 0)
                                        (seq (ptr-write-u64 (+ mem (* first 8)) 0 rw) 0)
                                      1))))))
  (should (bce-test-find (bce-test-function 'wf_bytecode_execute_frame)
            (lambda (x) (and (consp x) (eq (car x) 'nl_root_frame))))))

(ert-deftest bce-boxed-provider-and-bootstrap-contracts-unchanged ()
  (let ((boxed (bce-test-function 'wf_bytecode_w_boxed))
        (bridge (cl-find-if (lambda (f) (eq (cadr f) 'bf_n2_call_lisp_equal_w))
                            (nelisp-standalone--n2-lisp-delegates))))
    (should boxed)
    (should-not (bce-test-find boxed
                  (lambda (x) (memq (car-safe x)
                                    '(wf_bytecode_call_named wf_bytecode_call_named_value
                                      nl_apply_function_w bf_n2_call_lisp_equal_w)))))
    (should (bce-test-find bridge (lambda (x) (equal x '(bf_equal_w args 0)))))
    (should (bce-test-find bridge
              (lambda (x) (equal x '(nl_apply_function_w function-word args env 0)))))))

(provide 'nelisp-bytecode-equal-gateway-test)
