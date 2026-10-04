;;; nelisp-bytecode-native-guarded-lowering-test.el --- Mode boundary -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-native-guarded-lowering)
(ert-deftest nelisp-guarded-lowering/composed-private-copy-owner-refusal ()
  (let ((owner (symbol-function 'nelisp-native-optimization-guard-v1--copy)) (calls 0))
    (unwind-protect
        (progn
          (fset 'nelisp-native-optimization-guard-v1--copy
                (lambda (&rest _) (setq calls (1+ calls)) nil))
          (should-error (nelisp-bytecode-native-guarded-lowering-owner-valid-p))
          (should-error (nelisp-bytecode-native-guarded-lowering-dependency-context))
          (should (= calls 0)))
      (fset 'nelisp-native-optimization-guard-v1--copy owner))
    (should (nelisp-bytecode-native-guarded-lowering-owner-valid-p))))
(ert-deftest nelisp-guarded-lowering/opaque-and-nested-size-refusal ()
  (let ((table (make-hash-table)))
    (puthash 'cycle table table)
    (dolist (value (list table (list table) (record 'opaque table)
                        (list (make-vector 1025 'x))
                        (list (make-string 4097 ?x))))
      (should-not (nelisp-bytecode-native-guarded-lowering--bounded-p
                   value nil (list 16384) 0))
      (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-select value)
                            :status) 'refused))))
  (dolist (value (list 'x 42 1.25 (symbol-function 'car) '(a . b) [1 2]))
    (should (nelisp-bytecode-native-guarded-lowering--bounded-p
             value nil (list 16384) 0))))
(ert-deftest nelisp-guarded-lowering/string-bounds ()
  (dolist (value '("" "plain"))
    (should (nelisp-bytecode-native-guarded-lowering--bounded-p value nil (list 16384) 0))
    ;; Some providers return nil when no property transition exists.
    (cl-letf (((symbol-function 'next-property-change) (lambda (&rest _) nil)))
      (should (nelisp-bytecode-native-guarded-lowering--bounded-p value nil (list 16384) 0))))
  (let ((value (copy-sequence "plain")))
    (put-text-property 0 1 'face 'bold value)
    (should-not (nelisp-bytecode-native-guarded-lowering--bounded-p value nil (list 16384) 0))
    (set-text-properties 0 (length value) nil value)
    (put-text-property 2 3 'face 'bold value)
    (should-not (nelisp-bytecode-native-guarded-lowering--bounded-p value nil (list 16384) 0))))
(ert-deftest nelisp-guarded-lowering/mode-source-and-roots ()
  (let ((operation '(:opcode add :bytecode-opcode 92 :input-roots (1 2)
                    :output-root 3 :exit-root-base 4)))
    (dolist (mode '(on off))
      (let ((result (nelisp-bytecode-native-guarded-lowering-build operation 7 'env 'ticket mode)))
        (should (eq (plist-get result :status) 'complete))
        (should (eq (plist-get result :arithmetic-guard-mode) mode))
        (should (equal (cdr (plist-get result :call)) '(env ticket 1 2 3 4)))
        (should (eq (car (plist-get result :call))
                    (if (eq mode 'on) 'nl_native_add_guard_v1 'nl_native_add_v2)))
        (should (equal (plist-get result :additional-source)
                       (nelisp-native-optimization-guard-v1-source mode)))
        (should (equal (plist-get result :runtime-imports)
                       (nelisp-native-arithmetic-v2-runtime-imports)))))
    (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-build
                            operation 7 'env 'ticket 'unknown) :status) 'refused))
    (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-build
                            operation 6 'env 'ticket 'on) :status) 'refused))))

(ert-deftest nelisp-guarded-lowering/sealed-mode-interface ()
  ;; A modeled canonical owner isolates the interface; this is not native or
  ;; genuine planner qualification. The real adapter is still required.
  (require 'nelisp-bytecode-native-rooted-cfg-plan)
  (let ((canonical '(:status complete :input fixture :lowering-mode shared
                    :arithmetic-guard-mode on)))
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-plan)
               (lambda (input lowering mode)
                 (should (eq input 'fixture))
                 (should (eq lowering 'shared))
                 (should (memq mode '(on off))) canonical)))
      (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-select canonical)
                            :status) 'complete))
      (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-select
                              '(:status complete :input fixture :lowering-mode shared))
                            :status) 'refused))
      (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-select
                              '(:status complete :input fixture :lowering-mode shared
                                :arithmetic-guard-mode off)) :status) 'refused))))
  (let ((cyclic (list :status 'complete)))
    (setcdr (cdr cyclic) cyclic)
    (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-select cyclic)
                          :status) 'refused))))
