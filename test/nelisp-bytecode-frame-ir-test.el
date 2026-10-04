;;; nelisp-bytecode-frame-ir-test.el --- Frame IR tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-frame-ir)

(defconst nelisp-bytecode-frame-ir-test--host-byte-compile-file
  (symbol-function 'byte-compile-file))

(defconst nelisp-bytecode-frame-ir-test--catch-throw-fixture
  (expand-file-name
   "fixtures/native-bytecode/frame-ir-catch-throw.el"
   (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-bytecode-frame-ir/describes-source-free-catch-throw-as-unresolved ()
  "A real GNU ELC retains catch target and unresolved nonlocal transfer data."
  (skip-unless (equal emacs-version "31.1"))
  (let* ((dir (make-temp-file "frame-ir-catch-throw" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c"))
         (function-symbol 'nelisp-bytecode-frame-ir-catch-throw)
         (special-symbol 'nelisp-bytecode-frame-ir-handler-special))
    (unwind-protect
        (progn
          (copy-file nelisp-bytecode-frame-ir-test--catch-throw-fixture source)
          (unless (funcall nelisp-bytecode-frame-ir-test--host-byte-compile-file
                           source)
            (ert-fail "GNU byte compiler did not produce catch/throw ELC"))
          (delete-file source)
          (let (forms)
            (with-temp-buffer
              (insert-file-contents elc)
              (goto-char (point-min))
              (while (< (point) (point-max))
                (skip-chars-forward " \t\r\n")
                (unless (eobp) (push (read (current-buffer)) forms))))
            (setq forms (nreverse forms))
            (should-not (fboundp function-symbol))
            (should-not (boundp special-symbol))
            (should-not (featurep 'nelisp-bytecode-frame-ir-catch-throw))
            (let* ((definition
                    (cl-find-if
                     (lambda (form)
                       (and (eq (car-safe form) 'defalias)
                            (equal (cadr form) (list 'quote function-symbol))))
                     forms))
                   (function (nth 2 definition))
                 (code (aref function 1))
                 (constants (aref function 2))
                 (frame (nelisp-bytecode-frame-ir-build code constants 2))
                 (control (plist-get frame :handler-control-flow))
                 (pushes (plist-get control :pushes))
                 (transfers (plist-get control :possible-nonlocal-transfers))
                 (bad-code (copy-sequence code))
                 (bad-frame nil))
            (should (equal (string-to-list code)
                           '(1 50 12 0 137 24 193 2 8 34 41 48 135)))
            (should (eq (plist-get frame :status) 'unsupported))
            (should-not (plist-get frame :blocks))
            (should (eq (plist-get control :status) 'unsupported))
            (should (equal (plist-get (aref pushes 0) :target) 12))
            (should (= (plist-get (aref pushes 0) :handler-depth) 1))
            (should (= (plist-get (aref transfers 0) :from-pc) 9))
            (should (= (plist-get (aref transfers 0) :target) 12))
            (should (= (plist-get (aref transfers 0) :handler-depth) 1))
            (should (= (plist-get (aref transfers 0) :binding-depth) 1))
            (should (= (plist-get (aref transfers 0) :saved-binding-depth) 0))
            (should (eq (plist-get (aref transfers 0) :stack-transfer)
                        'unresolved))
            (should (eq (plist-get (aref transfers 0) :binding-transfer)
                        'unresolved))
            (aset bad-code 2 2)
            (setq bad-frame (nelisp-bytecode-frame-ir-build bad-code constants 2))
            (should (eq (plist-get bad-frame :status) 'malformed))
            (should (string-match-p "instruction boundary"
                                    (plist-get bad-frame :reason)))
              (should-not (plist-get bad-frame :blocks)))))
      (when (fboundp function-symbol) (fmakunbound function-symbol))
      (when (boundp special-symbol) (makunbound special-symbol))
      (when (file-exists-p source) (delete-file source))
      (when (file-exists-p elc) (delete-file elc))
      (delete-directory dir t))))

(ert-deftest nelisp-bytecode-frame-ir/tracks-dynamic-bind-effects-and-depth-joins ()
  (let* ((code (unibyte-string 137 24 193 41 135))
         (frame (nelisp-bytecode-frame-ir-build code [frame-ir-special nil] 1))
         (decoded (nelisp-bytecode-ir-decode-result code [frame-ir-special nil]))
         (block (aref (plist-get frame :blocks) 0))
         (instructions (append (plist-get block :instructions) nil))
         (bind (nth 1 instructions))
         (unbind (nth 3 instructions))
         (join-mismatch
          (nelisp-bytecode-frame-ir-build
           (unibyte-string 137 24 193 131 10 0 41 130 10 0 135)
           [frame-ir-special nil] 1))
         (underflow
          (nelisp-bytecode-frame-ir-build (unibyte-string 41 135) [] 0)))
    (should (eq (plist-get frame :status) 'complete))
    (should (eq (plist-get bind :kind) 'dynamic-bind))
    (should-not (plist-get (aref (aref (plist-get decoded :instructions) 1) 4)
                           :lowerable))
    (should (= (plist-get bind :constant-index) 0))
    (should (equal (plist-get (plist-get bind :operation-effect) :pc) 1))
    (should (= (plist-get (plist-get bind :operation-effect) :stack-delta) -1))
    (should (= (plist-get (plist-get bind :operation-effect) :binding-delta) 1))
    (should (plist-get (plist-get bind :operation-effect) :may-nonlocal-exit))
    (should (equal (plist-get unbind :pc) 3))
    (should-not (plist-get (aref (aref (plist-get decoded :instructions) 3) 4)
                           :lowerable))
    (should (= (plist-get (plist-get unbind :operation-effect) :binding-count) 1))
    (should (= (plist-get block :entry-binding-depth) 0))
    (should (= (plist-get block :exit-binding-depth) 0))
    (should (= (plist-get frame :max-binding-depth) 1))
    (should (eq (plist-get join-mismatch :status) 'malformed))
    (should (string-match-p "binding depth mismatch"
                            (plist-get join-mismatch :reason)))
    (should (eq (plist-get underflow :status) 'malformed))))

(ert-deftest nelisp-bytecode-frame-ir/covers-compact-dynamic-bind-and-unbind-families ()
  (let ((constants (vector 'frame-ir-var0 'frame-ir-value 'frame-ir-var2
                           'frame-ir-var3 'frame-ir-var4 'frame-ir-var5
                           'frame-ir-var6 'frame-ir-var7)))
    (dotimes (index 8)
      (let* ((opcode (+ 24 index))
             (code (apply #'unibyte-string
                          (append (list 137 opcode)
                                  (cond ((= index 6) '(6))
                                        ((= index 7) '(7 0)))
                                  '(193 41 135))))
             (frame (nelisp-bytecode-frame-ir-build code constants 1))
             (instructions (append
                            (plist-get (aref (plist-get frame :blocks) 0)
                                       :instructions) nil))
             (bind (cl-find 'dynamic-bind instructions
                            :key (lambda (insn) (plist-get insn :kind)))))
        (should (eq (plist-get frame :status) 'complete))
        (should (= (plist-get bind :opcode) opcode))
        (should (= (plist-get bind :constant-index) index))))
    (dotimes (count 8)
      (let* ((opcode (+ 40 count))
             (binds (apply #'append (make-list count '(137 24))))
             (code (apply #'unibyte-string
                          (append binds (list 193 opcode)
                                  (cond ((= count 6) '(6))
                                        ((= count 7) '(7 0))
                                        (t nil))
                                  '(135))))
             (frame (nelisp-bytecode-frame-ir-build code constants count))
             (instructions (append
                            (plist-get (aref (plist-get frame :blocks) 0)
                                       :instructions) nil))
             (unbind (cl-find 'dynamic-unbind instructions
                              :key (lambda (insn) (plist-get insn :kind)))))
        (should (eq (plist-get frame :status) 'complete))
        (should (= (plist-get unbind :opcode) opcode))
        (should (= (plist-get (plist-get unbind :operation-effect)
                              :binding-count) count))
        (should (= (plist-get frame :max-binding-depth) count))))))

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

(ert-deftest nelisp-bytecode-frame-ir/accepts-source-free-cfg-with-two-backedges ()
  ;; Two independent branches return to the same loop header.  This verifies
  ;; general frame-CFG acceptance without relying on source byte compilation.
  (let* ((code (unibyte-string 192 131 14 0 193 131 11 0
                               130 0 0 130 0 0 194 135))
         (result (nelisp-bytecode-frame-ir-build code [t nil 0]))
         (blocks (plist-get result :blocks))
         (backedges (list (cl-find 8 blocks :key (lambda (b) (plist-get b :start)))
                          (cl-find 11 blocks :key (lambda (b) (plist-get b :start))))))
    (should (eq (plist-get result :status) 'complete))
    (should (= (length blocks) 5))
    (dolist (block backedges)
      (let ((edge (aref (plist-get block :successors) 0)))
        (should (= (plist-get edge :target) 0))
        (should (equal (plist-get edge :target-slots) []))))))

(ert-deftest nelisp-bytecode-frame-ir/accepts-source-free-emacs31-bswitch-cfg ()
  ;; GNU Emacs 31.1 pcase shape: dup the selector, push an eq hash table,
  ;; dispatch to two case bodies, and fall through to the default body.
  (let* ((table (make-hash-table :test 'eq))
         (code (unibyte-string 137 192 183 130 10 0 193 135
                               194 135 195 135)))
    (puthash 'a 6 table)
    (puthash 'b 8 table)
    (let* ((result (nelisp-bytecode-frame-ir-build code (vector table 1 2 3) 1))
           (blocks (plist-get result :blocks))
           (dispatch (aref (plist-get (aref blocks 0) :instructions) 2))
           (edges (append (plist-get (aref blocks 0) :successors) nil)))
      (should (eq (plist-get result :status) 'complete))
      (should (eq (plist-get dispatch :kind) 'switch))
      (should (= (plist-get dispatch :table-constant-index) 0))
      (should (equal (mapcar (lambda (edge) (plist-get edge :target)) edges)
                     '(3 6 8)))
      (should (equal (mapcar (lambda (edge) (length (plist-get edge :slots))) edges)
                     '(1 1 1)))
      (should (equal (plist-get (aref (plist-get (aref blocks 0) :successors) 1)
                                :keys)
                     '(a)))
      (should (equal (plist-get (aref (plist-get (aref blocks 0) :successors) 2)
                                :keys)
                     '(b))))))

(ert-deftest nelisp-bytecode-frame-ir/marks-switch-edges-as-mutable-table-snapshot ()
  (let* ((table (make-hash-table :test 'eq))
         (code (unibyte-string 137 192 183 130 10 0 193 135
                               194 135 195 135)))
    (puthash 'a 6 table)
    (puthash 'b 8 table)
    (let ((result (nelisp-bytecode-frame-ir-build code (vector table 1 2 3) 1)))
      (puthash 'a 9 table)
      (should (eq (plist-get result :status) 'complete))
      (should (eq (plist-get result :switch-edges) 'constant-table-snapshot))
      (should (eq (plist-get result :runtime-switch-lowering)
                  'unsupported-until-mutation-guard))
      (should (equal (mapcar (lambda (edge) (plist-get edge :target))
                             (append (plist-get (aref (plist-get result :blocks) 0)
                                                :successors) nil))
                     '(3 6 8))))))

(ert-deftest nelisp-bytecode-frame-ir/rejects-bswitch-table-target-inside-operand ()
  (let* ((table (make-hash-table :test 'eq))
         (code (unibyte-string 137 192 183 130 10 0 193 135
                               194 135 195 135)))
    (puthash 'a 4 table)
    (puthash 'b 8 table)
    (let ((result (nelisp-bytecode-frame-ir-build code (vector table 1 2 3) 1)))
      (should (eq (plist-get result :status) 'malformed))
      (should (string-match-p "switch target" (plist-get result :reason)))
      (should-not (plist-get result :blocks)))))

(ert-deftest nelisp-bytecode-frame-ir/preserves-structural-error-with-unreachable-bswitch ()
  (let ((result (nelisp-bytecode-frame-ir-build
                 (unibyte-string 130 9 0 137 192 183 130 1 0)
                 [nil] 1)))
    (should (eq (plist-get result :status) 'malformed))
    (should (string-match-p "target" (plist-get result :reason)))))

(ert-deftest nelisp-bytecode-frame-ir/reports-bswitch-with-unknown-table-as-unsupported ()
  (let ((result (nelisp-bytecode-frame-ir-build (unibyte-string 183 135) [] 2)))
    (should (eq (plist-get result :status) 'unsupported))
    (should (string-match-p "provenance" (plist-get result :reason)))
    (should-not (plist-get result :blocks))))

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

(ert-deftest nelisp-bytecode-frame-ir/accepts-nested-source-free-loop-cfg-with-live-joins ()
  ;; Captured from GNU Emacs 31.1 byte compilation of two nested while loops.
  ;; The bytecode remains source-free and exercises two distinct backedges.
  (skip-unless (string-match-p "\\`31\\.1\\(?:\\'\\|[.-]\\)" emacs-version))
  (let* ((code (unibyte-string 192 137 3 87 131 23 0
                               192 137 3 87 131 18 0
                               84 130 8 0 136
                               84 130 1 0
                               193 135))
         (constants [0 77])
         (function (make-byte-code 514 code constants 6))
         (result (nelisp-bytecode-frame-ir-build code constants 2))
         (blocks (plist-get result :blocks))
         (outer (cl-find 1 blocks :key (lambda (block) (plist-get block :start))))
         (inner (cl-find 8 blocks :key (lambda (block) (plist-get block :start))))
         (outer-backedge (aref (plist-get (cl-find 18 blocks
                                                   :key (lambda (block)
                                                          (plist-get block :start)))
                                          :successors) 0))
         (inner-backedge (aref (plist-get (cl-find 14 blocks
                                                   :key (lambda (block)
                                                          (plist-get block :start)))
                                          :successors) 0)))
    (should (= (funcall function 2 3) 77))
    (should (eq (plist-get result :status) 'complete))
    (should (= (plist-get result :max-stack-depth) 6))
    (should (= (plist-get outer :entry-stack-depth) 3))
    (should (= (plist-get inner :entry-stack-depth) 4))
    (should (= (plist-get outer-backedge :target) 1))
    (should (= (length (plist-get outer-backedge :target-slots)) 3))
    (should (= (plist-get inner-backedge :target) 8))
    (should (= (length (plist-get inner-backedge :target-slots)) 4))))

(ert-deftest nelisp-bytecode-frame-ir/rejects-nested-loop-backedge-into-operand ()
  (let ((code (copy-sequence
               (unibyte-string 192 137 3 87 131 23 0
                               192 137 3 87 131 18 0
                               84 130 8 0 136
                               84 130 1 0
                               193 135))))
    ;; Change the outer goto's relative target to byte 6, inside an operand.
    (aset code 21 6)
    (let ((result (nelisp-bytecode-frame-ir-build code [0 77] 2)))
      (should (eq (plist-get result :status) 'malformed))
      (should (string-match-p "not an instruction boundary"
                              (plist-get result :reason)))
      (should-not (plist-get result :blocks)))))

(ert-deftest nelisp-bytecode-frame-ir/reports-handler-stack-underflow-as-malformed ()
  (let ((result (nelisp-bytecode-frame-ir-build
                 (unibyte-string 49 3 0 85 135) [] 1)))
    (should (eq (plist-get result :status) 'malformed))
    (should (string-match-p "underflow" (plist-get result :reason)))
    (should-not (plist-get result :blocks))))

(ert-deftest nelisp-bytecode-frame-ir/keeps-gnu-reserved-opcode-zero-malformed ()
  (let ((result (nelisp-bytecode-frame-ir-build (unibyte-string 0) [])))
    (should (eq (plist-get result :status) 'malformed))
    (should (equal (plist-get result :reason) "reserved opcode 0"))
    (should-not (plist-get result :blocks))))

(ert-deftest nelisp-bytecode-frame-ir/verifies-exact-cons-stack-transfer ()
  (let* ((result (nelisp-bytecode-frame-ir-build
                  (unibyte-string 1 1 66 135) [] 2))
         (instructions
          (append (plist-get (aref (plist-get result :blocks) 0)
                             :instructions)
                  nil))
         (cons-op (nth 2 instructions)))
    (should (eq (plist-get result :status) 'complete))
    (should (eq (plist-get cons-op :kind) 'primitive))
    (should (equal (plist-get cons-op :inputs)
                   '((:value 0 0) (:value 1 0))))
    (should (equal (plist-get cons-op :outputs) '((:value 2 0))))
    (should (= (plist-get result :max-stack-depth) 4))))

(provide 'nelisp-bytecode-frame-ir-test)
;;; nelisp-bytecode-frame-ir-test.el ends here
