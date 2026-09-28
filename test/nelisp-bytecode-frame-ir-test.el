;;; nelisp-bytecode-frame-ir-test.el --- Frame IR tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-bytecode-frame-ir)

(ert-deftest nelisp-bytecode-frame-ir/builds-branch-join-with-stable-entry-slots ()
  (let* ((code (unibyte-string 192 131 8 0 193 130 9 0 194 135))
         (result (nelisp-bytecode-frame-ir-build code [t 1 2]))
         (blocks (plist-get result :blocks))
         (join (cl-find 9 blocks :key (lambda (block) (plist-get block :start))))
         (edges (append (append (plist-get (aref blocks 1) :successors) nil)
                        (append (plist-get (aref blocks 2) :successors) nil))))
    (should (eq (plist-get result :status) 'complete))
    (should (= (length blocks) 4))
    (should (= (plist-get join :entry-stack-depth) 1))
    (should (= (length edges) 2))
    (dolist (edge edges)
      (should (equal (aref (plist-get edge :target-slots) 0) '(:entry 9 0))))))

(ert-deftest nelisp-bytecode-frame-ir/accepts-balanced-backedge-and-preserved-branch-value ()
  (let* ((code (unibyte-string 192 133 6 0 193 135 130 1 0))
         (result (nelisp-bytecode-frame-ir-build code [t nil]))
         (blocks (plist-get result :blocks))
         (branch (cl-find 1 blocks :key (lambda (block) (plist-get block :start))))
         (edges (append (plist-get branch :successors) nil)))
    (should (eq (plist-get result :status) 'complete))
    (should (= (plist-get (cl-find 4 blocks :key (lambda (b) (plist-get b :start)))
                          :entry-stack-depth) 0))
    (should (equal (aref (plist-get (cadr edges) :slots) 0) '(:entry 1 0)))
    (should (= (length (plist-get (car edges) :slots)) 0))
    (let* ((backedge (cl-find 6 blocks :key (lambda (b) (plist-get b :start))))
           (edge (aref (plist-get backedge :successors) 0)))
      (should (= (plist-get edge :target) 1))
      (should (equal (aref (plist-get edge :target-slots) 0) '(:entry 1 0))))))

(ert-deftest nelisp-bytecode-frame-ir/models-stack-ref-dup-and-discard-slots ()
  (let* ((code (unibyte-string 192 193 1 137 136 135))
         (result (nelisp-bytecode-frame-ir-build code [1 2]))
         (block (aref (plist-get result :blocks) 0))
         (instructions (append (plist-get block :instructions) nil)))
    (should (eq (plist-get result :status) 'complete))
    (should (equal (plist-get (nth 2 instructions) :inputs)
                   '((:value 0 0))))
    (should (equal (plist-get (nth 3 instructions) :inputs)
                   '((:value 2 0))))
    (should (eq (plist-get (nth 4 instructions) :kind) 'discard))))

(ert-deftest nelisp-bytecode-frame-ir/calls-remain-effects-without-preflight-execution ()
  (let ((effects 0)
        (zero-arg (lambda () (setq effects (1+ effects))))
        (one-arg (lambda (_arg) (setq effects (1+ effects))))
        result-zero result-one)
    (setq result-zero
          (nelisp-bytecode-frame-ir-build
           (unibyte-string 129 0 0 32 135) (vector zero-arg)))
    (setq result-one
          (nelisp-bytecode-frame-ir-build
           (unibyte-string 129 0 0 193 33 135) (vector one-arg 7)))
    (should (eq (plist-get result-zero :status) 'complete))
    (should (eq (plist-get result-one :status) 'complete))
    (should (= effects 0))
    (dolist (result (list result-zero result-one))
      (let* ((instructions (plist-get (aref (plist-get result :blocks) 0)
                                      :instructions))
             (call (cl-find 'call instructions
                            :key (lambda (instruction)
                                   (plist-get instruction :kind)))))
        (should call)
        (should (= (length (plist-get call :inputs))
                   (if (eq result result-zero) 1 2)))
        (should (equal (plist-get (aref instructions 0) :constant-index) 0)))))
  )

(ert-deftest nelisp-bytecode-frame-ir/accepts-host-byte-compiled-branch-and-call ()
  (let* ((branch (byte-compile '(lambda (x) (if x 1 2))))
         (call (byte-compile '(lambda (f x) (funcall f x))))
         (branch-ir (nelisp-bytecode-frame-ir-build
                     (aref branch 1) (aref branch 2) 1))
         (call-ir (nelisp-bytecode-frame-ir-build
                   (aref call 1) (aref call 2) 2))
         (instructions (plist-get (aref (plist-get call-ir :blocks) 0)
                                  :instructions)))
    (should (eq (plist-get branch-ir :status) 'complete))
    (should (eq (plist-get call-ir :status) 'complete))
    (should (equal (string-to-list (aref call 1)) '(1 1 33 135)))
    (should (cl-find 'call instructions
                     :key (lambda (instruction) (plist-get instruction :kind)))))
  (let* ((function (byte-compile '(lambda (x) (null x))))
         (result (nelisp-bytecode-frame-ir-build
                  (aref function 1) (aref function 2) 1)))
    (should (eq (plist-get result :status) 'complete))
    (should (= (plist-get (aref (plist-get (aref (plist-get result :blocks) 0)
                                            :instructions) 0) :opcode)
               63))))

(ert-deftest nelisp-bytecode-frame-ir/retains-decoded-extended-variable-pool-index ()
  (let ((constants (make-vector 43 nil)))
    (aset constants 42 'frame-ir-test-variable)
    (let* ((ref (nelisp-bytecode-frame-ir-build
                 (unibyte-string 14 42 135) constants))
           (set (nelisp-bytecode-frame-ir-build
                 (unibyte-string 22 42 192 135) constants 1))
           (ref-instruction (aref (plist-get (aref (plist-get ref :blocks) 0)
                                             :instructions) 0))
           (set-instruction (aref (plist-get (aref (plist-get set :blocks) 0)
                                             :instructions) 0)))
      (should (eq (plist-get ref :status) 'complete))
      (should (eq (plist-get set :status) 'complete))
      (should (= (plist-get ref-instruction :constant-index) 42))
      (should (= (plist-get set-instruction :constant-index) 42)))))

(ert-deftest nelisp-bytecode-frame-ir/rejects-known-but-unimplemented-primitive ()
  (let ((result (nelisp-bytecode-frame-ir-build
                 (unibyte-string 192 193 62 135) [a b])))
    (should (eq (plist-get result :status) 'unsupported))
    (should-not (plist-get result :blocks))))

(ert-deftest nelisp-bytecode-frame-ir/rejects-invalid-branch-target-before-emission ()
  (let ((result (nelisp-bytecode-frame-ir-build
                 (unibyte-string 130 3 0) [])))
    (should (eq (plist-get result :status) 'malformed))
    (should (string-match-p "target" (plist-get result :reason)))
    (should-not (plist-get result :blocks))))

(ert-deftest nelisp-bytecode-frame-ir/rejects-underflow-hidden-by-net-stack-delta ()
  (let ((binary (nelisp-bytecode-frame-ir-build (unibyte-string 85 135) []))
        (call (nelisp-bytecode-frame-ir-build (unibyte-string 32 135) []))
        (emission
         (nelisp-bytecode-frame-ir--emit-block
          10 (list (vector 10 33 12 1 '(:kind call :stack-delta -1)))
          1 nil)))
    (should (eq (plist-get binary :status) 'malformed))
    (should (eq (plist-get call :status) 'malformed))
    (should (string-match-p "operand underflow" (cdr emission)))))

(ert-deftest nelisp-bytecode-frame-ir/rejects-handler-transfer-without-changing-stack-contract ()
  (let ((result (nelisp-bytecode-frame-ir-build (unibyte-string 49 0 0 135) [])))
    (should (eq (plist-get result :status) 'unsupported))
    (should-not (plist-get result :blocks))))

(provide 'nelisp-bytecode-frame-ir-test)
;;; nelisp-bytecode-frame-ir-test.el ends here
