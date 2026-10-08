;;; nelisp-native-owner-gate-test.el --- Pre-copy owner admission -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-bytecode-native-guarded-lowering)
(ert-deftest nelisp-owner-gate/private-helpers-refuse-before-call ()
  (dolist (name '(nelisp-native-arithmetic-v2--copy-node
                  nelisp-native-arithmetic-v2--snapshot
                  nelisp-native-optimization-guard-v1--copy
                  nelisp-bytecode-native-arithmetic-lowering--operation-p
                  nelisp-bytecode-native-guarded-lowering--bounded-p))
    (let ((owner (symbol-function name)) (calls 0))
      (unwind-protect
          (progn
            (fset name (lambda (&rest _) (setq calls (1+ calls)) nil))
            (should-error (nelisp-bytecode-native-guarded-lowering-owner-valid-p))
            (should-error (nelisp-bytecode-native-guarded-lowering-dependency-context))
            (should-error (nelisp-bytecode-native-guarded-lowering-build
                           '(:opcode add :bytecode-opcode 92 :input-roots (1 2)
                             :output-root 3 :exit-root-base 4) 7 'env 'ticket 'on))
            (should (= calls 0)))
        (fset name owner))
      (should (nelisp-bytecode-native-guarded-lowering-owner-valid-p))))
  (should (eq (plist-get (nelisp-bytecode-native-guarded-lowering-build
                         '(:opcode add :bytecode-opcode 92 :input-roots (1 2)
                           :output-root 3 :exit-root-base 4) 7 'env 'ticket 'on)
                        :status) 'complete)))

(ert-deftest nelisp-owner-gate/no-copy-and-forged-public-gate ()
  (let ((owner (symbol-function 'nelisp-native-optimization-guard-v1-owner-valid-p))
        (calls 0))
    (unwind-protect
        (progn
          (fset 'nelisp-native-optimization-guard-v1-owner-valid-p
                (lambda () (setq calls (1+ calls)) t))
          (should-error (nelisp-bytecode-native-guarded-lowering-owner-valid-p))
          (should (= calls 0)))
      (fset 'nelisp-native-optimization-guard-v1-owner-valid-p owner)))
  (let ((owner (symbol-function 'nelisp-native-arithmetic-v2--copy-node)))
    (unwind-protect
        (progn
          (fset 'nelisp-native-arithmetic-v2--copy-node (lambda (&rest _) (error "unexpected copy")))
          (should-error (nelisp-native-arithmetic-v2-owner-valid-p)))
      (fset 'nelisp-native-arithmetic-v2--copy-node owner))))
