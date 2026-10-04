;;; nelisp-gnu-bytecode-vm-opcode-test.el --- opcode lowering regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-gnu-bytecode-vm)

(ert-deftest nelisp-gnu-bytecode-vm-opcode/dup-preserves-stack-shape ()
  "GNU DUP copies the top entry and contributes to declared stack depth."
  (let ((signatures (make-hash-table :test #'eq)))
    (should (equal
             (append (nelisp-gnu-bytecode-vm--lower-code
                      (string 192 137 24 41 135) [value] 0 2 signatures
                      (make-hash-table :test #'eq)) nil)
             '(1 0 4 18 0 19 1 0)))
    (should-error
     (nelisp-gnu-bytecode-vm--lower-code
      (string 137 135) [] 0 1 signatures (make-hash-table :test #'eq)))
    (should-error
     (nelisp-gnu-bytecode-vm--lower-code
      (string 192 137 24 41 135) [value] 0 1 signatures
     (make-hash-table :test #'eq)))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/nconc-two-and-three-argument-parity ()
  "GNU NCONC opcodes mutate the first cons spine and preserve the tail."
  (require 'nelisp-eval)
  (nelisp--reset)
  (let* ((signatures (make-hash-table :test #'eq))
         (forward (make-hash-table :test #'eq))
         (two-host (byte-compile '(lambda (left right) (nconc left right))))
         (three-host (byte-compile '(lambda (first second third)
                                      (nconc first second third))))
         (two (nelisp-gnu-bytecode-vm--lower-function two-host signatures forward))
         (three (nelisp-gnu-bytecode-vm--lower-function three-host signatures forward)))
    (should (equal (append (aref two-host 1) nil) '(1 1 164 135)))
    (should (equal (append (aref three-host 1) nil) '(2 2 164 1 164 135)))
    (should (= (nelisp-bc-stack-depth two) 7))
    (let* ((left (list 'a 'b)) (tail (list 'c))
           (result (nelisp-bc-run two (list left tail))))
      (should (eq result left))
      (should (eq (cdr (cdr left)) tail))
      (should (equal result '(a b c))))
    (should (eq (nelisp-bc-run two (list nil 'right)) 'right))
    (let ((left (list 'left)))
      (should (eq (nelisp-bc-run two (list left nil)) left))
      (should (equal left '(left))))
    (let ((left (cons 'a 'old)))
      (should (eq (nelisp-bc-run two (list left 'tail)) left))
      (should (eq (cdr left) 'tail)))
    (let* ((first (list 'a)) (second (list 'b)) (third (list 'c))
           (result (nelisp-bc-run three (list first second third))))
      (should (eq result first))
      (should (eq (cdr first) second))
      (should (eq (cdr second) third))
      (should (equal result '(a b c))))
    (should (equal (condition-case err (nelisp-bc-run two (list 7 nil))
                     (wrong-type-argument err))
                   '(wrong-type-argument consp 7)))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/nconc-uses-captured-primitives ()
  "GNU NCONC bypasses later NCONC/CDR/SETCDR function-cell changes."
  (require 'nelisp-eval)
  (nelisp--reset)
  (let* ((function (byte-compile '(lambda (left right) (nconc left right))))
         (bcl (nelisp-gnu-bytecode-vm--lower-function
               function (make-hash-table :test #'eq)
               (make-hash-table :test #'eq)))
         (poison (nelisp-bc-make nil '(left right) (vector 'poison) [1 0 0] 1 0))
         (left (list 'left))
         (right (list 'right)))
    (puthash 'nconc poison nelisp--functions)
    (puthash 'cdr poison nelisp--functions)
    (puthash 'setcdr poison nelisp--functions)
    (should (eq (nelisp-bc-run bcl (list left right)) left))
    (should (eq (funcall nelisp-gnu-bytecode-vm--cdr-callable left) right))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/nconc-bypasses-host-cell-overrides ()
  "The intrinsic implementation uses captured CDR/SETCDR providers."
  (let ((left (list 'left))
        (right (list 'right))
        returned-tail
        result)
    (cl-letf (((symbol-function 'nconc)
               (lambda (&rest _) (error "public nconc override called")))
              ((symbol-function 'cdr)
               (lambda (&rest _) (error "public cdr override called")))
              ((symbol-function 'setcdr)
               (lambda (&rest _) (error "public setcdr override called"))))
      (setq result
            (funcall nelisp-gnu-bytecode-vm--nconc-intrinsic-callable
                     left right)
            returned-tail
            (funcall nelisp-gnu-bytecode-vm--cdr-callable left)))
    (should (eq result left))
    (should (eq returned-tail right))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/nconc-refuses-missing-intrinsic-before-lowering ()
  "A missing trusted NCONC provider is rejected during validation."
  (let ((nelisp-gnu-bytecode-vm--nconc-intrinsic-callable nil))
    (should-error
     (nelisp-gnu-bytecode-vm--lower-code
      (string 1 1 164 135)
      [] 2 2 (make-hash-table :test #'eq) (make-hash-table :test #'eq))
     :type 'nelisp-gnu-bytecode-vm-error)))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/extended-dynamic-alias-shape ()
  "The GNU dynamic-alias body keeps its duplicated initializer through bind."
  (should (equal
           (append (nelisp-gnu-bytecode-vm--lower-code
                    (string 194 137 24 9 41 66 135)
                    [alias base dynamic-value] 0 2
                    (make-hash-table :test #'eq)
                    (make-hash-table :test #'eq)) nil)
           '(1 2 4 18 0 16 1 19 1 24 0))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/symbol-value-is-runtime-call ()
  "GNU SYMBOL_VALUE lowers through its captured intrinsic and preserves symbol."
  (let ((signatures (make-hash-table :test #'eq)))
    (should (equal
             (append (nelisp-gnu-bytecode-vm--lower-code
                      (string 192 74 135)
                      (vector 'variable
                              nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable)
                      0 1
                      signatures (make-hash-table :test #'eq)) nil)
             '(1 0 4 1 1 20 2 30 1 0)))
    (should-error
     (nelisp-gnu-bytecode-vm--lower-code
      (string 74 135) [] 0 1 signatures (make-hash-table :test #'eq)))
    (should-error
     (nelisp-gnu-bytecode-vm--lower-code
      (string 192 32 135)
      (vector nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable)
      0 1 signatures (make-hash-table :test #'eq)))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/callable-bundle-is-atomic ()
  "The public BCL registry installs exact identities and rejects bad bundles."
  (require 'nelisp-eval)
  (nelisp--reset)
  (let* ((name 'nelisp-gnu-bytecode-vm-opcode-callable)
         (bcl (nelisp-bc-make nil '(arg &optional other) [] [0] 1 0)))
    (should-error
     (nelisp-gnu-bytecode-vm-install-callable-bundle
      (list (list name bcl '(1 3)))))
    (should-not (gethash name nelisp--functions))
    (should (nelisp-gnu-bytecode-vm-install-callable-bundle
             (list (list name bcl '(1 2)))))
    (should (eq (gethash name nelisp--functions) bcl))
    (should (equal (gethash name
                            (nelisp-gnu-bytecode-vm--live-call-signatures))
                   '(1 2)))
    (let ((before (gethash name nelisp--functions)))
      (should-error
       (nelisp-gnu-bytecode-vm-install-callable-bundle
        (list (list name bcl '(1)) '(bad-entry))))
      (should (eq (gethash name nelisp--functions) before)))))

(ert-deftest nelisp-gnu-bytecode-vm-opcode/symbol-value-reads-logical-store ()
  "The lowered runtime call reads NeLisp's global store, not host globals."
  (require 'nelisp-eval)
  (nelisp--reset)
  (let* ((sym 'nelisp-gnu-bytecode-vm-opcode-value)
         (function (byte-compile `(lambda () (symbol-value ',sym))))
         (lowered (nelisp-gnu-bytecode-vm--lower-function
                   function (nelisp-gnu-bytecode-vm--live-call-signatures)
                   (make-hash-table :test #'eq))))
    (nelisp-variable-put sym 731)
    (should (= (nelisp-variable-get sym) 731))
    ;; The intrinsic opcode must ignore a later public function-cell override.
    (puthash 'symbol-value (nelisp-bc-make nil nil [:poison] [1 0 0] 1 0)
             nelisp--functions)
    (should (= (nelisp-bc-run lowered) 731))
    (should-error (nelisp-bc-run
                   (nelisp-gnu-bytecode-vm--lower-function
                    (byte-compile '(lambda () (symbol-value
                                                'nelisp-gnu-bytecode-vm-opcode-unbound)))
                    (nelisp-gnu-bytecode-vm--live-call-signatures)
                    (make-hash-table :test #'eq)))
                  :type 'void-variable)
    (let* ((code (nelisp-gnu-bytecode-vm--lower-code
                  (string 74 135)
                  (vector nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable)
                  1 1
                  (nelisp-gnu-bytecode-vm--live-call-signatures)
                  (make-hash-table :test #'eq)))
           (bcl (nelisp-bc-make
                 nil '(value)
                 (vector nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable)
                 code 3 0)))
      (should (equal (condition-case err (nelisp-bc-run bcl '(23))
                       (wrong-type-argument err))
                     '(wrong-type-argument symbolp 23))))))

(provide 'nelisp-gnu-bytecode-vm-opcode-test)
;;; nelisp-gnu-bytecode-vm-opcode-test.el ends here
