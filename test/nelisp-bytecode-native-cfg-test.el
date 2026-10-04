;;; nelisp-bytecode-native-cfg-test.el --- Native CFG lowering tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-bytecode-native-cfg)
(require 'nelisp-native-load)

(ert-deftest nelisp-bytecode-native-cfg/uses-loader-running-image-identity ()
  (let ((invocation-name (or (executable-find "sh") "/bin/sh"))
        (invocation-directory "/"))
    (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
               (lambda ()
                 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")))
      (should (equal (nelisp-bytecode-native-cfg--current-binary-sha256)
                     "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")))
    (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
               (lambda () nil)))
      (should-error (nelisp-bytecode-native-cfg--current-binary-sha256)
                    :type 'error))))

(defun nelisp-bytecode-native-cfg-test--rel32-target (bytes instruction-offset)
  "Return the resolved target offset for a rel32 branch in BYTES."
  (let* ((slot (+ instruction-offset
                  (if (= (aref bytes instruction-offset) #xe9) 1 2)))
         (value (+ (aref bytes slot)
                   (ash (aref bytes (1+ slot)) 8)
                   (ash (aref bytes (+ slot 2)) 16)
                   (ash (aref bytes (+ slot 3)) 24)))
         (signed (if (>= value #x80000000) (- value #x100000000) value)))
    (+ slot 4 signed)))

(ert-deftest nelisp-bytecode-native-cfg/emits-a-real-cfg-branch-artifact ()
  (let* ((code (unibyte-string 192 131 8 0 193 130 9 0 194 135))
         (lowered (nelisp-bytecode-native-cfg-lower code [t 1 2]))
         (bytes (plist-get lowered :machine-bytes)))
    (should (eq (plist-get lowered :status) 'complete))
    (should (eq (plist-get (plist-get lowered :backend) :kind) 'x86_64-cfg))
    (should (stringp bytes))
    (should (string-match (regexp-quote (unibyte-string #xe9)) bytes))
    (should (assq 9 (plist-get lowered :pc-labels)))
    (let* ((fixups (plist-get lowered :branch-fixups))
           (targets (sort (mapcar (lambda (fixup)
                                    (plist-get fixup :target-pc))
                                  fixups)
                          #'<)))
      (should (equal targets '(4 8 9 9)))
      (dolist (fixup fixups)
        (should (= (nelisp-bytecode-native-cfg-test--rel32-target
                    bytes (plist-get fixup :instruction-offset))
                   (plist-get fixup :target-offset)))))
    (should (eq (plist-get lowered :execution) 'not-loaded))
    (should-not (plist-get lowered :vm-trampoline))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-gnu-bswitch-integer-table ()
  ;; GNU Emacs 31.1 byte-compiles (pcase x (1 10) (2 20) (_ 30))
  ;; to this Bswitch stream and an eq table with the shown bytecode targets.
  (let* ((function (byte-compile '(lambda (x) (pcase x (1 10) (2 20) (_ 30)))))
         (code (aref function 1))
         (constants (aref function 2))
         (frame (nelisp-bytecode-frame-ir-build code constants 1))
           (lowered (nelisp-bytecode-native-cfg-lower code constants 1 '(raw-i64))))
    (should (equal (string-to-list code)
                   '(137 192 183 130 10 0 193 135 194 135 195 135)))
    (should (eq (plist-get frame :status) 'complete))
    (should (= (funcall function 1) 10))
    (should (= (funcall function 2) 20))
    (should (= (funcall function 7) 30))
    (should (eq (plist-get lowered :status) 'complete))
    (should (> (length (plist-get lowered :machine-bytes)) 30))))

(ert-deftest nelisp-bytecode-native-cfg/gnu-bswitch-fixture-make-byte-code-abi ()
  (let* ((compiled (byte-compile '(lambda (x) (pcase x (1 10) (2 20) (_ 30)))))
         (code (aref compiled 1))
         (constants (aref compiled 2))
         (frame (nelisp-bytecode-frame-ir-build code constants 1))
         (manual (make-byte-code 257 code constants
                                 (max 2 (plist-get frame :max-stack-depth)) nil)))
    (should (= (funcall manual 1) 10))
    (should (= (funcall manual 2) 20))
    (should (= (funcall manual 7) 30))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-unsupported-bswitch-tables ()
  (let* ((function (byte-compile '(lambda (x) (pcase x (1 10) (2 20) (_ 30)))))
         (code (aref function 1))
         (constants (copy-sequence (aref function 2)))
         (equal-table (make-hash-table :test 'equal))
         (boxed-table (make-hash-table :test 'eq)))
    (puthash 1 6 equal-table)
    (puthash 2 8 equal-table)
    (puthash "one" 6 boxed-table)
    (aset constants 0 equal-table)
    (let ((lowered (nelisp-bytecode-native-cfg-lower code constants 1 '(raw-i64))))
      (should (eq (plist-get lowered :status) 'unsupported))
      (should (string-match-p "eq/eql table" (plist-get lowered :reason)))
      (should-not (plist-get lowered :machine-bytes)))
    (aset constants 0 boxed-table)
    (let ((lowered (nelisp-bytecode-native-cfg-lower code constants 1 '(raw-i64))))
      (should (eq (plist-get lowered :status) 'unsupported))
      (should (string-match-p "signed-i32" (plist-get lowered :reason)))
      (should-not (plist-get lowered :machine-bytes)))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-dynamic-bswitch-table ()
  (let ((lowered (nelisp-bytecode-native-cfg-lower
                  (unibyte-string 183 192 135) [7] 2 '(raw-i64 raw-i64))))
    (should (eq (plist-get lowered :status) 'unsupported))
    (should (string-match-p "table provenance" (plist-get lowered :reason)))
    (should-not (plist-get lowered :machine-bytes))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-explicit-raw-arguments ()
  (let* ((one (nelisp-bytecode-native-cfg-lower
               (unibyte-string 135) [] 1 '(raw-i64)))
         (two (nelisp-bytecode-native-cfg-lower
               (unibyte-string 1 137 135) [] 2 '(raw-i64 raw-i64)))
         (six (nelisp-bytecode-native-cfg-lower
               (unibyte-string 135) [] 6
               '(raw-i64 raw-i64 raw-i64 raw-i64 raw-i64 raw-i64))))
    (should (eq (plist-get one :status) 'complete))
    (should (= (plist-get one :arity) 1))
    (should (eq (plist-get two :status) 'complete))
    (should (= (plist-get two :arity) 2))
    (should (eq (plist-get six :status) 'complete))
    (should (= (plist-get six :arity) 6))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-invalid-argument-contracts ()
  (dolist (arguments '((7 (raw-i64 raw-i64 raw-i64 raw-i64 raw-i64 raw-i64 raw-i64))
                       (1 nil)
                       (1 (sexp-ptr))
                       (1 (raw-i64 . t))
                       (one (raw-i64))))
    (let ((lowered (nelisp-bytecode-native-cfg-lower
                    (unibyte-string 135) [] (car arguments) (cadr arguments))))
      (should (eq (plist-get lowered :status) 'unsupported))
      (should-not (plist-get lowered :machine-bytes)))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-branch-on-raw-argument ()
  (let ((lowered (nelisp-bytecode-native-cfg-lower
                  (unibyte-string 131 5 0 192 135 193 135)
                  [nil 7] 1 '(raw-i64))))
    (should (eq (plist-get lowered :status) 'unsupported))
    (should (string-match-p "not proven nil/t" (plist-get lowered :reason)))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-unproven-byte-not-and-boxed-constant ()
  (let ((raw-boolean
         (nelisp-bytecode-native-cfg-lower (unibyte-string 63 135) []
                                           1 '(raw-i64)))
        (boxed-constant
         (nelisp-bytecode-native-cfg-lower (unibyte-string 192 135) ["boxed"])))
    (should (eq (plist-get raw-boolean :status) 'unsupported))
    (should (string-match-p "byte-not input is not proven nil/t"
                            (plist-get raw-boolean :reason)))
    (should-not (plist-get raw-boolean :machine-bytes))
    (should (eq (plist-get boxed-constant :status) 'unsupported))
    (should (string-match-p "boxed or out-of-range constant"
                            (plist-get boxed-constant :reason)))
    (should-not (plist-get boxed-constant :machine-bytes))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-integer-zero-as-nil-branch-test ()
  (let ((lowered
         (nelisp-bytecode-native-cfg-lower
          (unibyte-string 192 131 8 0 193 130 9 0 194 135) [0 10 20])))
    (should (eq (plist-get lowered :status) 'unsupported))
    (should (string-match-p "not proven nil/t" (plist-get lowered :reason)))
    (dolist (opcode '(133 134))
      (let ((else-pop
             (nelisp-bytecode-native-cfg-lower
              (unibyte-string 192 opcode 5 0 193 135) [0 7])))
        (should (eq (plist-get else-pop :status) 'unsupported))))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-live-slots-across-branch-join ()
  (let* ((code (unibyte-string 192 193 194 131 10 0 195 130 11 0 196 135))
         (lowered (nelisp-bytecode-native-cfg-lower code [5 10 t 20 30])))
    (should (eq (plist-get lowered :status) 'complete))
    (should (equal (plist-get (plist-get lowered :backend) :value-repr)
                   'raw-i64-frame-slots))
    (should (> (length (plist-get lowered :machine-bytes)) 30))
    (let ((targets (sort (mapcar (lambda (fixup)
                                   (plist-get fixup :target-pc))
                                 (plist-get lowered :branch-fixups))
                         #'<)))
      (should (equal targets '(6 10 11 11)))
      (dolist (fixup (plist-get lowered :branch-fixups))
        (should (= (plist-get fixup :target-offset)
                   (cdr (assq (plist-get fixup :target-pc)
                                  (plist-get lowered :pc-offsets)))))))))

(ert-deftest nelisp-bytecode-native-cfg/branch-join-proves-distinct-values-and-rejects-bypass ()
  ;; Both branch arms leave their own raw integer in the same live return slot.
  ;; Replacing one producer with discard makes that incoming join slot absent.
  (let* ((code (unibyte-string 192 193 194 131 10 0 195 130 11 0 196 135))
         (constants [5 10 t 20 30])
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (frame (plist-get lowered :frame-ir))
         (blocks (plist-get frame :blocks))
         (branch (aref blocks 0))
         (left (aref blocks 1))
         (right (aref blocks 2))
         (join (aref blocks 3))
         (join-pc (plist-get join :start))
         (bad-code (copy-sequence code)))
    (should (eq (plist-get lowered :status) 'complete))
    (should (= join-pc 11))
    (should (equal (mapcar (lambda (edge) (plist-get edge :target))
                           (append (plist-get branch :successors) nil))
                   '(6 10)))
    (should (equal (mapcar (lambda (edge) (plist-get edge :target))
                           (append (plist-get left :successors) nil))
                   (list join-pc)))
    (should (equal (mapcar (lambda (edge) (plist-get edge :target))
                           (append (plist-get right :successors) nil))
                   (list join-pc)))
    (should (equal (plist-get (aref (plist-get left :successors) 0)
                              :target-slots)
                   (plist-get (aref (plist-get right :successors) 0)
                              :target-slots)))
    (should (= (plist-get (aref (plist-get left :instructions) 0)
                          :constant-index)
               3))
    (should (= (plist-get (aref (plist-get right :instructions) 0)
                          :constant-index)
               4))
    (should-not
     (equal (aref (plist-get (aref (plist-get left :successors) 0) :slots) 2)
            (aref (plist-get (aref (plist-get right :successors) 0) :slots) 2)))
    (should (= (length (plist-get (aref (plist-get left :successors) 0)
                                  :target-slots))
               (plist-get join :entry-stack-depth)))
    (should (= (funcall (make-byte-code
                         nil code (vector 5 10 nil 20 30)
                         (plist-get frame :max-stack-depth) nil))
               30))
    (should (= (funcall (make-byte-code
                         nil code (vector 5 10 t 20 30)
                         (plist-get frame :max-stack-depth) nil))
               20))
    (aset bad-code 6 136)
    (let ((bad (nelisp-bytecode-native-cfg-lower bad-code constants)))
      (should (eq (plist-get bad :status) 'unsupported))
      (should-not (plist-get bad :machine-bytes))
      (should (string-match-p "inconsistent stack depth"
                              (plist-get bad :reason))))))

(ert-deftest nelisp-bytecode-native-cfg/branch-emission-mutation-is-detected ()
  (let* ((code (unibyte-string 192 193 194 131 10 0 195 130 11 0 196 135))
         (expected (nelisp-bytecode-native-cfg-lower code [5 10 t 20 30]))
         (mutated
          (cl-letf (((symbol-function 'nelisp-asm-x86_64-jz-rel32)
                     (lambda (&rest _args) nil)))
            (nelisp-bytecode-native-cfg-lower code [5 10 t 20 30]))))
    (should (eq (plist-get expected :status) 'complete))
    (should-not (equal (mapcar (lambda (fixup) (plist-get fixup :target-pc))
                               (plist-get expected :branch-fixups))
                       (mapcar (lambda (fixup) (plist-get fixup :target-pc))
                               (plist-get mutated :branch-fixups))))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-a-backedge-without-calling-it ()
  (let* ((lowered (nelisp-bytecode-native-cfg-lower
                   (unibyte-string 130 0 0) []))
         (fixup (car (plist-get lowered :branch-fixups))))
    (should (eq (plist-get lowered :status) 'complete))
    (should (= (plist-get fixup :target-pc) 0))
    (should (= (plist-get fixup :target-offset)
               (cdr (assq 0 (plist-get lowered :pc-offsets)))))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-nested-loops-with-two-live-backedges ()
  ;; Two nested loop headers each preserve both live stack values on the edge.
  ;; Constant-true guards select finite exits while the backedges remain in the CFG.
  (let* ((code (unibyte-string 192 193 194 131 15 0
                               194 131 12 0 136 135
                               130 6 0 130 2 0))
         (lowered (nelisp-bytecode-native-cfg-lower code [42 7 t]))
         (frame (plist-get lowered :frame-ir))
         (blocks (plist-get frame :blocks))
         (outer (cl-find 2 blocks :key (lambda (block) (plist-get block :start))))
         (inner (cl-find 6 blocks :key (lambda (block) (plist-get block :start))))
         (outer-backedge (cl-find 15 blocks :key (lambda (block) (plist-get block :start))))
         (inner-backedge (cl-find 12 blocks :key (lambda (block) (plist-get block :start))))
         (outer-edge (aref (plist-get outer-backedge :successors) 0))
         (inner-edge (aref (plist-get inner-backedge :successors) 0)))
    (should (eq (plist-get lowered :status) 'complete))
    (should (= (plist-get outer :entry-stack-depth) 2))
    (should (= (plist-get inner :entry-stack-depth) 2))
    (should (= (plist-get outer-edge :target) 2))
    (should (= (length (plist-get outer-edge :target-slots)) 2))
    (should (= (plist-get inner-edge :target) 6))
    (should (= (length (plist-get inner-edge :target-slots)) 2))
    (should (equal (sort (mapcar (lambda (fixup) (plist-get fixup :target-pc))
                                 (plist-get lowered :branch-fixups)) #'<)
                   '(2 2 6 6 10 12 15)))))

(ert-deftest nelisp-bytecode-native-cfg/nested-loop-fixture-matches-gnu-vm-exit-path ()
  (let* ((code (unibyte-string 192 193 194 131 15 0
                               194 131 12 0 136 135
                               130 6 0 130 2 0))
         (constants [42 7 t])
         (frame (nelisp-bytecode-frame-ir-build code constants))
         (function (make-byte-code nil code constants
                                   (plist-get frame :max-stack-depth) nil))
         (lowered (nelisp-bytecode-native-cfg-lower code constants)))
    (should (eq (plist-get frame :status) 'complete))
    (should (= (funcall function) 42))
    (should (eq (plist-get lowered :status) 'complete))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-and-executes-two-terminating-backedges ()
  ;; `not' toggles the inner and outer boolean guards once, so both backedges
  ;; execute before the source-free loop fixture reaches its return.
  (let* ((code (unibyte-string 192 193 137 131 20 0
                               194 137 131 15 0 63 130 7 0
                               136 63 130 2 0 136 135))
         (constants [42 t t])
         (frame (nelisp-bytecode-frame-ir-build code constants))
         (function (make-byte-code nil code constants
                                   (plist-get frame :max-stack-depth) nil))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (instructions
          (apply #'append
                 (mapcar (lambda (block)
                           (append (plist-get block :instructions) nil))
                         (append (plist-get frame :blocks) nil))))
         (targets (mapcar (lambda (fixup) (plist-get fixup :target-pc))
                          (plist-get lowered :branch-fixups)))
         (backedge-targets (mapcar (lambda (instruction)
                                     (plist-get instruction :operand))
                                   (cl-remove-if-not
                                    (lambda (instruction)
                                      (and (= (plist-get instruction :opcode) 130)
                                           (< (plist-get instruction :operand)
                                              (plist-get instruction :pc))))
                                    instructions))))
    (should (eq (plist-get frame :status) 'complete))
    (should (= (cl-count 63 instructions :key (lambda (instruction)
                                               (plist-get instruction :opcode)))
               2))
    (should (equal (sort backedge-targets #'<) '(2 7)))
    (should (= (funcall function) 42))
    (should (eq (plist-get lowered :status) 'complete))
    (should (= (length (plist-get lowered :branch-fixups)) 8))
    (should (memq 2 targets))
    (should (memq 7 targets))))

(ert-deftest nelisp-bytecode-native-cfg/rejects-nested-loop-backedge-into-operand ()
  (let ((code (copy-sequence
               (unibyte-string 192 193 194 131 15 0
                               194 131 12 0 136 135
                               130 6 0 130 2 0))))
    (aset code 4 5)
    (let ((lowered (nelisp-bytecode-native-cfg-lower code [42 7 t])))
      (should-not (eq (plist-get lowered :status) 'complete))
      (should-not (plist-get lowered :machine-bytes))
      (should (string-match-p "not an instruction boundary"
                              (plist-get lowered :reason))))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-stack-ref-dup-and-discard ()
  (dolist (code (list (unibyte-string 192 193 1 135)
                      (unibyte-string 192 137 136 135)))
    (let ((lowered (nelisp-bytecode-native-cfg-lower code [5 10])))
      (should (eq (plist-get lowered :status) 'complete))
      (should (stringp (plist-get lowered :machine-bytes)))))
  )

(ert-deftest nelisp-bytecode-native-cfg/stack-ref-vm-uses-declared-depth-three ()
  (let* ((code (unibyte-string 192 193 1 135))
         (constants [5 10])
         (frame-ir (nelisp-bytecode-frame-ir-build code constants))
         (depth (plist-get frame-ir :max-stack-depth))
         (function (make-byte-code nil code constants depth nil)))
    (should (= depth 3))
    (should (= (funcall function) 5))))

(ert-deftest nelisp-bytecode-native-cfg/lowers-edge-dependent-conditional-pop ()
  (dolist (opcode '(133 134))
    (let ((lowered
           (nelisp-bytecode-native-cfg-lower
            (unibyte-string 192 opcode 5 0 193 135) [nil 7])))
      (should (eq (plist-get lowered :status) 'complete))
      (should (equal (sort (mapcar (lambda (fixup)
                                     (plist-get fixup :target-pc))
                                   (plist-get lowered :branch-fixups))
                           #'<)
                     '(4 5 5)))
      (let* ((frame-ir (plist-get lowered :frame-ir))
             (entry (aref (plist-get frame-ir :blocks) 0))
             (fallthrough (aref (plist-get frame-ir :blocks) 1))
             (join (aref (plist-get frame-ir :blocks) 2))
             (taken-edge (aref (plist-get entry :successors) 1))
             (fallthrough-edge (aref (plist-get entry :successors) 0)))
        (should (= (length (plist-get taken-edge :slots)) 1))
        (should (= (length (plist-get fallthrough-edge :slots)) 0))
        (should (= (plist-get fallthrough :entry-stack-depth) 0))
        (should (= (plist-get join :entry-stack-depth) 1)))
      (let* ((bytes (plist-get lowered :machine-bytes))
             (conditional
              (cl-find-if (lambda (fixup)
                            (= (aref bytes (plist-get fixup :instruction-offset)) #x0f))
                          (plist-get lowered :branch-fixups))))
        (should conditional)
        (should (= (aref bytes (1+ (plist-get conditional :instruction-offset)))
                   (if (= opcode 133) #x84 #x85)))))))

(ert-deftest nelisp-bytecode-native-cfg/conditional-pop-polarity-mutation-is-detected ()
  (let* ((code (unibyte-string 192 133 5 0 193 135))
         (normal (nelisp-bytecode-native-cfg-lower code [nil 7]))
         (mutated
          (cl-letf (((symbol-function 'nelisp-asm-x86_64-jz-rel32)
                     (symbol-function 'nelisp-asm-x86_64-jnz-rel32)))
            (nelisp-bytecode-native-cfg-lower code [nil 7])))
         (normal-bytes (plist-get normal :machine-bytes))
         (mutated-bytes (plist-get mutated :machine-bytes)))
    (should (eq (plist-get normal :status) 'complete))
    (should-not (equal normal-bytes mutated-bytes))))

(provide 'nelisp-bytecode-native-cfg-test)
;;; nelisp-bytecode-native-cfg-test.el ends here
