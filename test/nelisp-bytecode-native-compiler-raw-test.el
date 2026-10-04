;;; nelisp-bytecode-native-compiler-raw-test.el --- Raw compiler API tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-compiler-raw)

(defun nelisp-bytecode-native-compiler-raw-test--build
    (function artifact export contract)
  "Exercise the public API with a locally pinned dialect test fixture."
  (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input--dialect)
             (lambda () (list :status 'pinned :dialect "GNU Emacs 31.1")))
            ((symbol-function 'nelisp-native-load-running-binary-sha256)
             (lambda () (make-string 64 ?0)))
            ((symbol-function 'nelisp-bytecode-native-cfg--current-binary-sha256)
             (lambda () (make-string 64 ?0))))
    (nelisp-bytecode-native-compiler-raw-build
     function artifact export contract)))

(ert-deftest nelisp-bytecode-native-compiler-raw/compiles-explicit-i64-argument ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-public-"
                                                      temporary-file-directory)))
         (function (make-byte-code 257 (unibyte-string 135) [] 1))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_identity" '(raw-i64))))
    (unwind-protect
        (progn
          (should (eq (plist-get result :status) 'complete))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/compiles-source-free-diamond-join ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-diamond-"
                                                      temporary-file-directory)))
         (code (unibyte-string 192 193 194 131 10 0 195 130 11 0 196 135))
         (constants [5 10 nil 20 30])
         (frame (nelisp-bytecode-frame-ir-build code constants))
         (function (make-byte-code nil code constants
                                   (plist-get frame :max-stack-depth)))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_diamond" nil)))
    (unwind-protect
        (progn
          (should (= (funcall function) 30))
          (should (eq (plist-get result :status) 'complete))
          (should (eq (plist-get result :return-repr) 'raw-i64))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/compiles-grounded-backedge-join ()
  ;; The nested-loop CFG retains two backedges; 42 grounds the returned slot.
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-loop-"
                                                      temporary-file-directory)))
         (code (unibyte-string 192 193 194 131 15 0
                               194 131 12 0 136 135
                               130 6 0 130 2 0))
         (constants [42 7 t])
         (frame (nelisp-bytecode-frame-ir-build code constants))
         (function (make-byte-code nil code constants
                                   (plist-get frame :max-stack-depth)))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_loop" nil)))
    (unwind-protect
        (progn
          (should (= (funcall function) 42))
          (should (eq (plist-get result :status) 'complete))
          (should (eq (plist-get (plist-get (plist-get result :input)
                                            :ir-result)
                                 :status)
                      'unsupported))
          (should (eq (plist-get result :return-repr) 'raw-i64))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/rejects-boxed-loop-return-before-write ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-boxed-loop-"
                                                      temporary-file-directory)))
         (code (unibyte-string 192 193 194 131 15 0
                               194 131 12 0 136 135
                               130 6 0 130 2 0))
         (boxed (cons 'unsafe 'boxed))
         (constants (vector boxed 7 t))
         (frame (nelisp-bytecode-frame-ir-build code constants))
         (function (make-byte-code nil code constants
                                   (plist-get frame :max-stack-depth)))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_boxed_loop" nil)))
    (unwind-protect
        (progn
          (should (eq (funcall function) boxed))
          (should (eq (plist-get result :status) 'unsupported))
          (should (eq (plist-get (plist-get result :input) :status)
                      'unsupported))
          (should-not (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/rejects-ungrounded-cyclic-phi ()
  (let* ((value '(:entry 10 0))
         (block '(:start 10
                  :instructions ((:kind return :inputs ((:entry 10 0))))
                  :successors ((:target 10 :slots ((:entry 10 0))
                                :target-slots ((:entry 10 0))))))
         (frame (list :blocks (vector block))))
    (should-not
     (nelisp-bytecode-native-compiler-raw--returns-proven-p frame [] 0))
    (should (equal
             (nelisp-bytecode-native-compiler-raw--integer-value-p
              value block (vector block) [] 0 nil)
             '(t)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/refuses-arity-and-boxed-return-before-write ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-public-refuse-"
                                                      temporary-file-directory)))
         (function (make-byte-code 257 (unibyte-string 135) [] 1)))
    (unwind-protect
        (progn
          (should (eq (plist-get
                       (nelisp-bytecode-native-compiler-raw-test--build
                        function artifact "nl_raw_bad_arity" nil)
                       :status)
                      'unsupported))
          (should-not (file-exists-p artifact))
          (should (eq (plist-get
                       (nelisp-bytecode-native-compiler-raw-test--build
                        (make-byte-code nil (unibyte-string 192 135)
                                        [boxed-value] 1)
                        artifact "nl_raw_bad_return" nil)
                       :status)
                      'unsupported))
          (should-not (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/rejects-unknown-dialect-before-write ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-unknown-dialect-"
                                                      temporary-file-directory)))
         (function (make-byte-code nil (unibyte-string 192 135) [42] 1))
         (result
          (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input--dialect)
                     (lambda () (list :status 'unsupported
                                      :reason "unknown test dialect"))))
            (nelisp-bytecode-native-compiler-raw-build
             function artifact "nl_raw_unknown_dialect" nil))))
    (should (eq (plist-get result :status) 'unsupported))
    (should (equal (plist-get result :reason) "unknown test dialect"))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler-raw/rejects-malformed-frame-before-write ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-malformed-"
                                                      temporary-file-directory)))
         (function (make-byte-code nil (unibyte-string 131) [] 0))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_malformed" nil)))
    (should (eq (plist-get result :status) 'malformed))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler-raw/rejects-nil-as-unproven-integer-return ()
  (let* ((artifact (make-temp-name (expand-file-name "nelisp-raw-nil-return-"
                                                      temporary-file-directory)))
         (function (make-byte-code nil (unibyte-string 192 135) [nil] 1))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_nil_return" nil)))
    (should (eq (plist-get result :status) 'unsupported))
    (should (string-match-p "return value is not proven"
                            (plist-get result :reason)))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler-raw/compiles-materialized-bswitch-table ()
  ;; This is the GNU 31.1 materialized byte-code ABI fixture.  The public
  ;; compiler sees only the byte-code function; it never receives source.
  (let* ((code (unibyte-string 137 192 183 130 10 0
                               193 135 194 135 195 135))
         (table (make-hash-table :test 'eq))
         (constants (vector table 10 20 30))
         (_ (progn (puthash 1 6 table) (puthash 2 8 table)))
         (frame (nelisp-bytecode-frame-ir-build code constants 1))
         (function (make-byte-code 257 code constants
                                   (plist-get frame :max-stack-depth)))
         (artifact (make-temp-name (expand-file-name "nelisp-raw-bswitch-"
                                                      temporary-file-directory)))
         (result (nelisp-bytecode-native-compiler-raw-test--build
                  function artifact "nl_raw_bswitch" '(raw-i64))))
    (unwind-protect
        (progn
          (should (= (funcall function 1) 10))
          (should (= (funcall function 2) 20))
          (should (= (funcall function 7) 30))
          (should (eq (plist-get result :status) 'complete))
          (should (eq (plist-get result :return-repr) 'raw-i64))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-raw/rejects-invalid-bswitch-table-and-selector ()
  (let* ((code (unibyte-string 137 192 183 130 10 0
                               193 135 194 135 195 135))
         (table (make-hash-table :test 'equal))
         (constants (vector table 10 20 30))
         (artifact (make-temp-name (expand-file-name "nelisp-raw-bswitch-reject-"
                                                      temporary-file-directory))))
    (puthash 1 6 table)
    (puthash 2 8 table)
    (unwind-protect
        (progn
          (let* ((function (make-byte-code 257 code constants 3))
                 (result (nelisp-bytecode-native-compiler-raw-test--build
                          function artifact "nl_raw_bswitch_bad_table" '(raw-i64))))
            (should (eq (plist-get result :status) 'unsupported))
            (should (string-match-p "eq/eql table" (plist-get result :reason)))
            (should-not (file-exists-p artifact)))
          ;; A nil selector is valid for the GNU VM's default arm but is not
          ;; an admissible raw-i64 dispatch operand.
          (let* ((bad-code (unibyte-string 192 193 183 130 10 0
                                          194 135 195 135 196 135))
                 (bad-table (make-hash-table :test 'eq))
                 (bad-constants (vector nil bad-table 10 20 30)))
            (puthash 1 6 bad-table)
            (puthash 2 8 bad-table)
            (let* ((frame (nelisp-bytecode-frame-ir-build bad-code bad-constants))
                   (function (make-byte-code nil bad-code bad-constants
                                             (plist-get frame :max-stack-depth)))
                   (result (nelisp-bytecode-native-compiler-raw-test--build
                            function artifact "nl_raw_bswitch_bad_selector" nil)))
              (should (= (funcall function) 30))
              (should (eq (plist-get result :status) 'unsupported))
              (should (string-match-p "proven raw integer"
                                      (plist-get result :reason)))
              (should-not (file-exists-p artifact)))))
      (when (file-exists-p artifact) (delete-file artifact)))))

(provide 'nelisp-bytecode-native-compiler-raw-test)
;;; nelisp-bytecode-native-compiler-raw-test.el ends here
