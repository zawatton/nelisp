;;; nelisp-bytecode-textop-fast-test.el --- concat/substring opcodes on WORD bodies -*- lexical-binding: t; -*-

;; w1f: the fast loop (raw 79-82) and the outer loop (79, 80-82, 176) call the
;; directcall text body (ids 115/116) on the WORD stack.  Structural checks on
;; the generated source; semantics are covered by the native corpus.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun w1f--find (tree predicate)
  (cond ((funcall predicate tree) tree)
        ((consp tree) (or (w1f--find (car tree) predicate)
                         (w1f--find (cdr tree) predicate)))))

(defun w1f--function (name)
  (cl-find-if (lambda (form) (and (eq (car-safe form) 'defun)
                                (eq (cadr form) name)))
              nelisp-standalone--applyfn-bytecode-helpers))

(defun w1f--calls (name callee)
  (w1f--find (w1f--function name)
             (lambda (f) (eq (car-safe f) callee))))

(ert-deftest w1f-helper-calls-text-body ()
  (should (w1f--function 'wf_bytecode_w_text_op))
  (should (w1f--calls 'wf_bytecode_w_text_op 'nl_directcall_text_run)))

(ert-deftest w1f-fast-loop-arm ()
  (should (w1f--calls 'wf_bytecode_w_fast_run 'wf_bytecode_w_text_op))
  (should (w1f--find (w1f--function 'wf_bytecode_w_fast_run)
                     (lambda (f) (equal f '(< raw 79))))))

(ert-deftest w1f-outer-loop-arms ()
  (should (w1f--calls 'wf_bytecode_execute_frame 'wf_bytecode_w_text_op))
  ;; The unchanged routes stay as the decline path.
  (should (w1f--calls 'wf_bytecode_execute_frame 'wf_bytecode_w_boxed))
  (should (w1f--calls 'wf_bytecode_execute_frame 'wf_bytecode_call_primitive)))

(provide 'nelisp-bytecode-textop-fast-test)
