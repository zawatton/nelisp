;;; nelisp-aot-affine-result-test.el --- Retained GP first consumer -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-affine-loop-test)
(defvar nelisp-aot-affine-result-test--enabled t)
(defmacro nelisp-aot-affine-result-test--context (&rest body)
  (declare (indent 0))
  `(let* ((defs (nelisp-aot-affine-loop-test--defs))
          (trace (nelisp-aot-affine-loop-test--plan defs))
          (nelisp-aot-compiler--x86-affine-loop-state (plist-get trace :affine))
          (a (plist-get nelisp-aot-compiler--x86-affine-loop-state :lhs))
          (b (plist-get nelisp-aot-compiler--x86-affine-loop-state :expression))
          (nelisp-aot-compiler--promoted-slots (car (plist-get trace :plan)))
          (nelisp-aot-compiler--x86-interval-leaf-p t)
          (nelisp-aot-compiler--rsp-temp-depth 0))
     (should nelisp-aot-compiler--x86-affine-loop-state)
     ,@body))
(ert-deftest nelisp-aot-affine-result/exact-store-consumer-context ()
  (nelisp-aot-affine-result-test--context
    (should (nelisp-aot-compiler--x86-affine-result-register-p a b))
    (should-not (nelisp-aot-compiler--x86-affine-result-register-p (copy-sequence a) b))
    (should-not (nelisp-aot-compiler--x86-affine-result-register-p a (copy-sequence b)))
    (dolist (binding '((nelisp-aot-compiler--x86-affine-result-compare . nil)
                       (nelisp-aot-compiler--x86-affine-loop-state . nil)
                       (nelisp-aot-compiler--x86-interval-leaf-p . nil)
                       (nelisp-aot-compiler--promoted-slots . ((0 . rbx) (1 . r8) (2 . r9)))
                       (nelisp-aot-compiler--x86-frame-read-aliases . ((1 . 2)))
                       (nelisp-aot-compiler--x86-trace-write-through . (0))
                       (nelisp-aot-compiler--x86-trace-loop-exits . ((0 . x)))
                       (nelisp-aot-compiler--x86-shrink-wrap-state . (:prefix t))
                       (nelisp-aot-compiler--x86-leaf-entry-state . (:active t))
                       (nelisp-aot-compiler--x86-tco-entry-state . (:active t))
                       (nelisp-aot-compiler--rsp-temp-depth . 8)
                       (nelisp-aot-compiler--checked-arith . t)
                       (nelisp-aot-compiler--arch . aarch64)
                       (nelisp-aot-compiler--abi . win64)))
      (cl-progv (list (car binding)) (list (cdr binding))
        (should-not (nelisp-aot-compiler--x86-affine-result-register-p a b))))
    (let ((nelisp-aot-compiler--promoted-slots
           (list (cons (nelisp-aot-compiler--ir-get a :slot) 'rbx) '(1 . r8))))
      (should-not (nelisp-aot-compiler--x86-affine-result-register-p a b)))
    (dolist (property '((:class . f64) (:repr . f64) (:repr . mystery) (:root-p . t)))
      (let ((old (nelisp-aot-compiler--ir-get a (car property))))
        (unwind-protect
            (progn (nelisp-aot-affine-loop-test--set a (car property) (cdr property))
                   (should-not (nelisp-aot-compiler--x86-affine-result-register-p a b)))
          (nelisp-aot-affine-loop-test--set a (car property) old))))))
(defun nelisp-aot-affine-result-test--gauge (result)
  ;; This is the production selector's actual return, not a fabricated plan.
  (when result (push t nelisp-aot-affine-loop-test--plans)) result)
(ert-deftest nelisp-aot-affine-result/fullword-hot-cold-stores-flags-native ()
  ;; Keep the original AE test and its complete C/WORD/frame/FLAGS model
  ;; unchanged.  In this wrapper both objects retain accepted AE; the old
  ;; test's two-mode switch controls only AH.  Count actual register-CMP
  ;; selections instead of the unchanged AE descriptor for the positive gauge.
  (let ((compile (symbol-function 'nelisp-aot-compile-to-object))
        (record (symbol-function 'nelisp-aot-affine-loop-test--record)))
    (cl-letf (((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (&rest args)
                 (let ((nelisp-aot-compiler--x86-affine-result-compare
                        (and nelisp-aot-affine-result-test--enabled
                             nelisp-aot-compiler--x86-affine-loops))
                       (nelisp-aot-compiler--x86-affine-loops t))
                   (apply compile args))))
              ((symbol-function 'nelisp-aot-affine-loop-test--record)
               (lambda (&rest args)
                 (let ((nelisp-aot-affine-loop-test--plans nil)) (apply record args)))))
      (advice-add 'nelisp-aot-compiler--x86-affine-result-register-p
                  :filter-return #'nelisp-aot-affine-result-test--gauge)
      (unwind-protect
          (funcall (ert-test-body
                    (ert-get-test 'nelisp-aot-affine-loop/fullword-cold-frame-RAX-FLAGS-native)))
        (advice-remove 'nelisp-aot-compiler--x86-affine-result-register-p
                       #'nelisp-aot-affine-result-test--gauge)))))
(provide 'nelisp-aot-affine-result-test)
