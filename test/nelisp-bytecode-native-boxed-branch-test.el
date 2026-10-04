;;; nelisp-bytecode-native-boxed-branch-test.el --- boxed branch lowering -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-constant)
(require 'nelisp-bytecode-native-boxed-branch)
(require 'nelisp-bytecode-native-compiler)

(defconst nelisp-bytecode-native-boxed-branch-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest nelisp-bytecode-native-boxed-branch-emits-real-nil-tag-jcc ()
  ;; Descriptor 257 is the packed one-required-argument form; list descriptors
  ;; bind arguments separately and are deliberately outside this slice.
  (let* ((code (unibyte-string 137 134 6 0 192 135 135))
         (constants [chosen-value])
         (artifact (make-temp-file "nelisp-boxed-branch-test-" nil ".neln"))
         (old-rejected nil) result)
    (delete-file artifact)
    (unwind-protect
        (progn
          (condition-case err
            (nelisp-bytecode-native-constant-return-build
               code constants artifact "nl_boxed_branch" 1 '(branch-argument))
            (error (setq old-rejected t)))
          (should old-rejected)
          (should-not (file-exists-p artifact))
          (setq result
                (nelisp-bytecode-native-boxed-branch-build
                 code constants artifact "nl_boxed_branch" 257))
          (let* ((vm (make-byte-code 257 code constants 2))
                 (cons-argument (cons 'branch-truthy nil)))
            (should (eq (funcall vm nil) 'chosen-value))
            (should (eq (funcall vm t) t))
            (should (eq (funcall vm cons-argument) cons-argument)))
          (let ((bytes (string-to-list (plist-get result :machine-code))))
            (should (eq (plist-get result :status) 'complete))
            (should (= (length (plist-get (plist-get result :verified-cfg)
                                          :blocks)) 3))
            (should (cl-some (lambda (i) (and (= (nth i bytes) 15)
                                               (= (nth (1+ i) bytes) 133)))
                             (number-sequence 0 (max 0 (- (length bytes) 2)))))
            (should (equal (cl-subseq bytes 0 4) '(72 139 70 0)))
            (let ((jcc (cl-position 133 bytes)))
              (should jcc)
              (should-not (equal (cl-subseq bytes (+ jcc 1) (+ jcc 5))
                                 '(0 0 0 0))))
            (should (cl-some (lambda (i) (= (nth i bytes) 233))
                             (number-sequence 0 (max 0 (1- (length bytes)))))))
          (should (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-branch-refuses-unsupported-and-malformed ()
  (let ((artifact (make-temp-file "nelisp-boxed-branch-negative-" nil ".neln"))
        (call-rejected nil))
    (delete-file artifact)
    (unwind-protect
        (progn
          (condition-case err
              (nelisp-bytecode-native-boxed-branch-build
               (unibyte-string 137 131 7 0 32 135 192 135)
               [chosen-value] artifact "nl_boxed_branch" 257)
            (error (setq call-rejected
                         (string-match-p "unsupported call"
                                         (error-message-string err)))))
          (should call-rejected)
          (should-not (file-exists-p artifact))
          (should-error
           (nelisp-bytecode-native-boxed-branch-build
            (unibyte-string 131) [chosen-value]
            artifact "nl_boxed_branch" 257)
           :type 'error)
          (should-error
           (nelisp-bytecode-native-boxed-branch-build
            (unibyte-string 137 134 6 0 192 135 135) [chosen-value]
            artifact "nl_boxed_branch" '(branch-argument))
           :type 'error)
          (should-not (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-branch-lowers-one-live-boxed-join ()
  (let* ((code (unibyte-string 137 131 8 0 192 130 12 0 193 130 12 0 135))
         (left (cons 'left-value nil))
         (right (cons 'right-value nil))
         (constants (vector left right))
         (artifact (make-temp-file "nelisp-boxed-join-test-" nil ".neln"))
         (vm (make-byte-code 257 code constants 2))
         result)
    (delete-file artifact)
    (unwind-protect
        (progn
          (setq result
                (nelisp-bytecode-native-boxed-branch-build
                 code constants artifact "nl_boxed_join" 257))
          (should (eq (funcall vm nil) right))
          (should (eq (funcall vm (cons 'truthy-condition nil)) left))
          (garbage-collect)
          (should (eq (funcall vm nil) right))
          (should (eq (funcall vm t) left))
          (should (eq (plist-get result :status) 'complete))
          (should (= (length (plist-get (plist-get result :verified-cfg) :blocks)) 4))
          (should (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-branch-supports-packed-two-argument-abi ()
  (let* ((code (unibyte-string 137 134 6 0 192 135 135))
         (hidden (cons 'hidden-value nil))
         (constants (vector hidden))
         (artifact (make-temp-file "nelisp-boxed-branch-two-arg-" nil ".neln"))
         (vm (make-byte-code 514 code constants 3))
         (first (cons 'first-argument nil))
         (second (cons 'second-argument nil))
         result)
    (delete-file artifact)
    (unwind-protect
        (progn
          (setq result
                (nelisp-bytecode-native-boxed-branch-build
                 code constants artifact "nl_boxed_branch_two" 514))
          (should (eq (funcall vm first nil) hidden))
          (should (eq (funcall vm first second) second))
          (should (eq (plist-get result :status) 'complete))
          (should (= (plist-get (aref (plist-get (plist-get result :verified-cfg)
                                                  :blocks)
                                      0)
                                :entry-stack-depth)
                     2))
          (should (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-routes-packed-two-argument-branch ()
  (let* ((function (make-byte-code 514 (unibyte-string 137 134 6 0 192 135 135)
                                   [hidden] 3))
         (artifact (make-temp-file "nelisp-boxed-compiler-two-arg-" nil ".neln"))
         result)
    (delete-file artifact)
    (unwind-protect
        (progn
          (setq result
                (nelisp-bytecode-native-compiler-build
                 function artifact "nl_boxed_compiler_two"))
          (should (eq (plist-get result :status) 'complete))
          (should (= (plist-get (aref (plist-get (plist-get (plist-get result :input)
                                                            :frame-result)
                                                 :blocks)
                                     0)
                                :entry-stack-depth)
                     2)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler-refuses-wrong-arity-and-call-code ()
  (let* ((branch-code (unibyte-string 137 134 6 0 192 135 135))
         (call-code (unibyte-string 137 131 7 0 32 135 192 135))
         (wrong-artifact (make-temp-file "nelisp-boxed-compiler-wrong-" nil ".neln"))
         (call-artifact (make-temp-file "nelisp-boxed-compiler-call-" nil ".neln"))
         wrong call)
    (delete-file wrong-artifact)
    (delete-file call-artifact)
    (unwind-protect
        (progn
          (setq wrong
                (nelisp-bytecode-native-compiler-build
                 (make-byte-code 771 branch-code [hidden] 4)
                 wrong-artifact "nl_boxed_compiler_wrong")
                call
                (nelisp-bytecode-native-compiler-build
                 (make-byte-code 514 call-code [hidden] 4)
                 call-artifact "nl_boxed_compiler_call"))
          (should (eq (plist-get wrong :status) 'unsupported))
          (should (eq (plist-get call :status) 'unsupported))
          (should-not (file-exists-p wrong-artifact))
          (should-not (file-exists-p call-artifact)))
      (when (file-exists-p wrong-artifact) (delete-file wrong-artifact))
      (when (file-exists-p call-artifact) (delete-file call-artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-branch-rejects-unbounded-contracts ()
  (let* ((artifact (make-temp-file "nelisp-boxed-branch-two-negative-" nil ".neln"))
         (code (unibyte-string 137 134 6 0 192 135 135)))
    (delete-file artifact)
    (unwind-protect
        (progn
          (should-error
           (nelisp-bytecode-native-boxed-branch-build
            code [hidden] artifact "nl_boxed_branch_two" 771)
           :type 'error)
          (should-error
           (nelisp-bytecode-native-boxed-branch-build
            (unibyte-string 137 131 7 0 32 135 192 135)
            [hidden] artifact "nl_boxed_branch_two" 514)
           :type 'error)
          (should-not (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-branch-rejects-join-bypass-edge ()
  ;; The taken edge reaches the return join without passing through either
  ;; slot-writing arm. Frame depth is valid, but its join slot is uninitialized.
  (let* ((code (unibyte-string 137 134 13 0 192 136 130 9 0
                              193 130 13 0 135))
         (frame (nelisp-bytecode-frame-ir-build code [left right] 1))
         (artifact (make-temp-file "nelisp-boxed-join-bypass-" nil ".neln")))
    (delete-file artifact)
    (unwind-protect
        (progn
          (should (eq (plist-get frame :status) 'complete))
          (should-error
           (nelisp-bytecode-native-boxed-branch-build
            code [left right] artifact "nl_boxed_join_bypass" 257)
           :type 'error)
          (should-not (file-exists-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-branch-admits-named-optional-dataflow ()
  (let* ((fixture (expand-file-name
                   "fixtures/native-bytecode/optional-truthiness.el"
                   nelisp-bytecode-native-boxed-branch-test--directory))
         (directory (make-temp-file "nelisp-optional-dataflow-" t))
         (source (expand-file-name "optional-truthiness.el" directory))
         (or-artifact (expand-file-name "optional-or.neln" directory))
         (and-artifact (expand-file-name "optional-and.neln" directory))
         (bad-artifact (expand-file-name "optional-bad.neln" directory))
         or-result and-result bad-result)
    (unwind-protect
        (progn
          (copy-file fixture source)
          (should (byte-compile-file source))
          (load (concat source "c") nil nil t)
          (let* ((or-function (symbol-function 'nelisp-native-optional-or))
                 (and-function (symbol-function 'nelisp-native-optional-and))
                 (or-input (nelisp-bytecode-compiler-input-build or-function))
                 (and-input (nelisp-bytecode-compiler-input-build and-function))
                 (or-code (aref or-function 1))
                 (mutated (copy-sequence or-code)))
            (should (equal (string-to-list or-code) '(8 134 5 0 9 135)))
            (should (equal (string-to-list (aref and-function 1))
                           '(8 133 5 0 9 135)))
            (should (equal (plist-get or-input :argument-descriptor)
                           '(value &optional supplied)))
            (setq or-result
                  (nelisp-bytecode-native-compiler-build
                   or-function or-artifact "nl_optional_or_ert")
                  and-result
                  (nelisp-bytecode-native-compiler-build
                   and-function and-artifact "nl_optional_and_ert"))
            ;; Redirect the verified taken edge onto the fallthrough block.
            ;; Stack analysis still completes, but the required distinct-arm
            ;; dataflow proof must reject it before writing an artifact.
            (aset mutated 2 4)
            (setq bad-result
                  (nelisp-bytecode-native-compiler-build
                   (make-byte-code (aref or-function 0) mutated
                                   (aref or-function 2) (aref or-function 3))
                   bad-artifact "nl_optional_bad_ert")))
          (should (eq (plist-get or-result :status) 'complete))
          (should (eq (plist-get and-result :status) 'complete))
          (should (memq (plist-get bad-result :status) '(unsupported malformed)))
          (should (file-readable-p or-artifact))
          (should (file-readable-p and-artifact))
          (should-not (file-exists-p bad-artifact)))
      (dolist (path (list or-artifact and-artifact bad-artifact
                          (expand-file-name "optional-truthiness.elc" directory)))
        (when (file-exists-p path) (delete-file path)))
      (delete-directory directory t))))

(provide 'nelisp-bytecode-native-boxed-branch-test)
;;; nelisp-bytecode-native-boxed-branch-test.el ends here
