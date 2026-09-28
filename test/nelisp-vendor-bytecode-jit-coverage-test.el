;;; nelisp-vendor-bytecode-jit-coverage-test.el --- Coverage probe tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-ir)
(require 'nelisp-bytecode-jit)
(require 'nelisp-vendor-bytecode-jit-coverage)

(ert-deftest nelisp-vendor-bytecode-jit-coverage/optional-arguments-seed-max-entry-depth ()
  (let* ((function (byte-compile '(lambda (x &optional y) (list x y))))
         (descriptor (aref function 0))
         (minimum-arity (logand descriptor 255))
         (maximum-arity (ash descriptor -8))
         (validation (nelisp-bytecode-ir-validate
                      (aref function 1) (aref function 2) maximum-arity))
         (blocker (nelisp-vendor-bytecode-jit-coverage--blocker function)))
    (should (= minimum-arity 1))
    (should (= maximum-arity 2))
    (should (equal (funcall function 42) '(42 nil)))
    (should (eq (plist-get validation :status) 'unsupported))
    (should (eq (plist-get (plist-get validation :stack-analysis) :status)
                'complete))
    (should (eq (plist-get blocker :stage) 'semantics))
    (let ((classification
           (nelisp-vendor-bytecode-jit-coverage--classify function)))
      (should (eq (plist-get classification :generic-ir-status) 'unsupported))
      (should (= (plist-get classification :first-offset)
                 (caar (plist-get validation :unsupported))))
      (should (integerp (plist-get classification :first-opcode)))
      (should (equal (plist-get classification :first-ir-rejection-reason)
                     (symbol-name (cdar (plist-get validation :unsupported))))))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/advances-after-fixed-width-object-ops ()
  ;; The fixed-width predicate, byte-car, stack-set and switch precede a reserved opcode.
  (let* ((function (make-byte-code 257 (unibyte-string 58 64 178 0 183 184 135) [] 2))
         (blocker (nelisp-vendor-bytecode-jit-coverage--blocker function)))
    (should (eq (plist-get blocker :stage) 'structural))
    (should (eq (plist-get blocker :reason) 'malformed-bytecode))
    (should (equal (plist-get blocker :detail) "reserved opcode 184 at 5"))
    (should (= (plist-get blocker :offset) 5))
    (should (= (plist-get blocker :opcode) 184))
    (should-not (plist-get
                 (nelisp-vendor-bytecode-jit-coverage--classify function)
                 :generic-ir-decoded))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/sample-remains-thirteen-vendor-functions ()
  (should (= 13
             (apply #'+ (mapcar (lambda (group) (length (cdr group)))
                                nelisp-vendor-bytecode-jit-coverage--functions)))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/small-tier-is-source-pinned-and-separate ()
  (let ((expected
         '((zerop . "6683de2c492f3752bd51b517569fc85085c6faabd98e8ec9efbfc331cc7aeafb")
           (caar . "54657f49c4d902c5a7c19d4a30e977cb6c49f11c8add17ae6ab0166f845f2643")
           (cadr . "122913e0b5f7d30c803c78773dc279f3c053af5f5202cf8562c14d2148ad0c78")
           (fixnump . "102e639e742351efbc457d8517db951ead880750dbc9eb1b96409f5fe063762d")
           (bignump . "28ab0a68fdfdb2ad17665d5e25c15fca14b03ac24a1a3beee8a5f0f6228b71cc")
           (frame-configuration-p . "3d94ac76fce88b09b2f4ea29b815040031990e1e9ec6103072c78373952473f0"))))
    (should (= 13 (apply #'+ (mapcar (lambda (group) (length (cdr group)))
                                    nelisp-vendor-bytecode-jit-coverage--functions))))
    (should (equal (cdr nelisp-vendor-bytecode-jit-coverage--small-functions)
                   '(zerop caar cadr fixnump bignump frame-configuration-p)))
    (dolist (entry expected)
      (let ((object
             (nelisp-vendor-bytecode-jit-coverage--source-function-object
              (car entry))))
        (should (equal
                 (nelisp-vendor-bytecode-jit-coverage--verify-fingerprint
                  (car entry) object)
                 (cdr entry))))))
  (let* ((report (nelisp-vendor-bytecode-jit-coverage-report))
         (small (cl-remove-if-not (lambda (row) (eq (plist-get row :tier) 'small))
                                  (plist-get report :functions))))
    (should (= (plist-get report :selected) 19))
    (should (= (length small) 6))
    (should (assq 'subr (plist-get report :source-fingerprints)))
    (should (equal
             (mapcar (lambda (row) (plist-get row :function))
                     (cl-remove-if-not
                      (lambda (row)
                        (plist-get row :static-jit-eligible-for-fixnum-inputs))
                      small))
             '(zerop fixnump frame-configuration-p)))
    (let ((zerop-row (assq 'zerop (mapcar (lambda (row)
                                           (cons (plist-get row :function) row))
                                         small))))
      (should (eq (plist-get (cdr zerop-row) :generic-ir-status) 'valid))
      (should (plist-get (cdr zerop-row) :generic-ir-decoded))
      (should (plist-get (cdr zerop-row) :legacy-instruction-decoder-decoded))
      (should (plist-get (cdr zerop-row) :static-jit-eligible-for-fixnum-inputs))
      (should (eq (plist-get (cdr zerop-row) :rejection-stage)
                  'static-jit-eligible)))
    (let ((fixnump-row (assq 'fixnump (mapcar (lambda (row)
                                               (cons (plist-get row :function) row))
                                             small))))
      (should (eq (plist-get (cdr fixnump-row) :generic-ir-status) 'valid))
      (should (plist-get (cdr fixnump-row) :generic-ir-decoded))
      (should (plist-get (cdr fixnump-row) :legacy-instruction-decoder-decoded))
      (should (plist-get (cdr fixnump-row) :static-jit-eligible-for-fixnum-inputs)))
    (let ((json (nelisp-vendor-bytecode-jit-coverage--json-object report)))
      (should (eq (alist-get 'native_execution_measured json) :json-false)))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/retains-malformed-label-for-invalid-target ()
  (let* ((function (make-byte-code 257 (unibyte-string 130 9 0 135) [] 2))
         (blocker (nelisp-vendor-bytecode-jit-coverage--blocker function)))
    (should (eq (plist-get blocker :reason) 'malformed-bytecode))
    (should (eq (plist-get blocker :stage) 'structural))
    (should (= (plist-get blocker :offset) 0))
    (should (= (plist-get blocker :opcode) 130))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/structural-error-pins-first-byte ()
  (let* ((function (make-byte-code 257 (unibyte-string 0) [] 1))
         (classification
          (nelisp-vendor-bytecode-jit-coverage--classify function)))
    (should (eq (plist-get classification :rejection-stage) 'structural))
    (should (= (plist-get classification :first-opcode) 0))
    (should (= (plist-get classification :first-offset) 0))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/marginal-counts-follow-enabled-opcodes ()
  (let* ((rows '((:function first :rejection-stage semantics
                              :first-opcode 58 :semantic-opcodes (58))
                 (:function second :rejection-stage semantics
                              :first-opcode 58 :semantic-opcodes (58 61))
                 (:function third :rejection-stage semantics
                              :first-opcode 61 :semantic-opcodes (61))))
         (marginals
          (nelisp-vendor-bytecode-jit-coverage--opcode-marginals rows))
         (op58 (assq 58 (mapcar (lambda (row)
                                  (cons (plist-get row :opcode) row))
                                marginals)))
         (op61 (assq 61 (mapcar (lambda (row)
                                  (cons (plist-get row :opcode) row))
                                marginals))))
    (should (= (plist-get (cdr op58) :first-blocker-count) 2))
    (should (= (plist-get (cdr op58) :marginal-unlocked-count) 1))
    (should (= (plist-get (cdr op61) :first-blocker-count) 1))
    (should (= (plist-get (cdr op61) :marginal-unlocked-count) 2))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/single-opcode-unlocks-use-baseline ()
  (let* ((rows '((:function first :rejection-stage semantics
                              :first-opcode 58 :semantic-opcodes (58))
                 (:function second :rejection-stage semantics
                              :first-opcode 58 :semantic-opcodes (58 61))
                 (:function third :rejection-stage semantics
                              :first-opcode 61 :semantic-opcodes (61))))
         (marginals
          (nelisp-vendor-bytecode-jit-coverage--opcode-marginals rows))
         (op58 (assq 58 (mapcar (lambda (row)
                                  (cons (plist-get row :opcode) row))
                                marginals)))
         (op61 (assq 61 (mapcar (lambda (row)
                                  (cons (plist-get row :opcode) row))
                                marginals))))
    (should (= (plist-get (cdr op58) :single-opcode-unlocked-count) 1))
    (should (equal (plist-get (cdr op58) :single-opcode-unlocked-functions)
                   '(first)))
    (should (= (plist-get (cdr op61) :single-opcode-unlocked-count) 1))
    (should (equal (plist-get (cdr op61) :single-opcode-unlocked-functions)
                   '(third)))
    (should-not (memq 'second
                      (plist-get (cdr op58) :single-opcode-unlocked-functions)))
    (should-not (memq 'second
                      (plist-get (cdr op61) :single-opcode-unlocked-functions)))))

(ert-deftest nelisp-vendor-bytecode-jit-coverage/changed-bytecode-fingerprint-is-red ()
  (load-file (expand-file-name
              "macroexp.el"
              (nelisp-vendor-bytecode-jit-coverage--source-root)))
  (let* ((original (byte-compile (symbol-function 'macroexp--all-forms)))
         (code (copy-sequence (aref original 1)))
         (mutated nil))
    (aset code 0 (if (= (aref code 0) 58) 59 58))
    (setq mutated (make-byte-code (aref original 0) code
                                  (aref original 2) (aref original 3)))
    (should (equal
             (nelisp-vendor-bytecode-jit-coverage--verify-fingerprint
              'macroexp--all-forms original)
             "d5fb6cb4da360407ecc92efe907c953ce8424569af083474d9e64a59af806ad4"))
    (should-error
     (nelisp-vendor-bytecode-jit-coverage--verify-fingerprint
      'macroexp--all-forms mutated))))

(provide 'nelisp-vendor-bytecode-jit-coverage-test)
;;; nelisp-vendor-bytecode-jit-coverage-test.el ends here
