;;; nelisp-vendor-bytecode-triparity-test.el --- Runner regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-vendor-bytecode-triparity)
(require 'nelisp-vendor-source)

(defconst nelisp-vendor-bytecode-triparity-test--zerop-bytecode-sha256
  "55b766b78af0a19a0dfea8ba55cf5536a6fe8b9d5ef135e9935c3e74fbfba7c8")

(ert-deftest nelisp-vendor-bytecode-triparity-minimal-source-form ()
  (should
   (equal (cdr (assq 'cconv-closure-convert
                    nelisp-vendor-bytecode-triparity--fixtures))
          '((lambda (x) (+ x 1)))))
  (let* ((object (make-byte-code 257 (unibyte-string 135) [] 1))
         (arguments '((lambda (x) (+ x 1)))))
    (dolist (lane '(host vm jit))
      (let ((form (nelisp-vendor-bytecode-triparity--child-form
                   object arguments lane '("macroexp.el"))))
        (should (string-match-p
                 (regexp-quote "macroexp.el") form))
        (should-not (string-match-p (regexp-quote "cconv.el") form))
        (should (string-match-p
                 (regexp-quote
                  (nelisp-vendor-bytecode-triparity--argument-expressions
                   arguments))
                 form))))))

(ert-deftest nelisp-vendor-bytecode-triparity-byte-compile-lambda-minimal-input ()
  (should (equal (assq 'byte-compile-lambda
                       nelisp-vendor-bytecode-triparity--fixtures)
                 '(byte-compile-lambda (lambda nil 17))))
  (should (equal (nelisp-vendor-bytecode-triparity--sources
                  'byte-compile-lambda)
                 '("macroexp.el" "cconv.el" "bytecomp.el")))
  ;; Load the pinned GNU 31.1 source before capturing its implementation.
  (nelisp-vendor-bytecode-jit-coverage--load-sources)
  (let* ((object (nelisp-vendor-bytecode-triparity--object
                  'byte-compile-lambda))
         (arguments (cdr (assq 'byte-compile-lambda
                               nelisp-vendor-bytecode-triparity--fixtures)))
         (hash "5594c96ebd0a50f44cd672c801c25db0270aafe5156b88d62a73bc33a303084c")
         (binary (or (getenv "NELISP_BIN")
                     (expand-file-name "target/nelisp"
                                       (nelisp-vendor-bytecode-triparity--root))))
         (sources (nelisp-vendor-bytecode-triparity--sources
                   'byte-compile-lambda))
         (host (nelisp-vendor-bytecode-triparity--lane
                (or (getenv "EMACS") "emacs") object arguments 'host sources))
         (vm (nelisp-vendor-bytecode-triparity--lane
              binary object arguments 'vm sources))
         (jit (nelisp-vendor-bytecode-triparity--lane
               binary object arguments 'jit sources))
         (row (list :bytecode_sha256 hash :host host :vm vm :jit jit)))
    (should (= (aref object 0) 513))
    (should (equal (nelisp-vendor-bytecode-triparity--bytecode-hash object)
                   hash))
    (should (equal (plist-get host :status) "ok"))
    (should (byte-code-function-p (plist-get host :value)))
    (should (= (funcall (plist-get host :value)) 17))
    ;; A missing primitive may block source loading before the VM/JIT lane runs.
    ;; Keep the fixture useful when that primitive is implemented.
    (dolist (lane (list vm jit))
      (if (equal (plist-get lane :status) "process-error")
          (progn
            (should (equal (plist-get lane :reason) "missing-function"))
            (should (string-match-p "void-function: (try-completion)"
                                    (plist-get lane :detail))))
        (should (equal (plist-get lane :status) "ok"))
        (should (byte-code-function-p (plist-get lane :value)))
        (should (= (funcall (plist-get lane :value)) 17))))
    (should (equal (nelisp-vendor-bytecode-triparity--parity row)
                   (if (and (equal (plist-get vm :status) "ok")
                            (equal (plist-get jit :status) "ok"))
                       "pass"
                     "not-comparable")))))

(ert-deftest nelisp-vendor-bytecode-triparity-zerop-fixture-is-pinned ()
  (should (equal (assq 'zerop nelisp-vendor-bytecode-triparity--fixtures)
                 '(zerop 0)))
  (should (eq (nelisp-vendor-bytecode-triparity--sources 'zerop) :none))
  (should (equal
           (nelisp-vendor-bytecode-triparity--bytecode-hash
           (nelisp-vendor-bytecode-triparity--object 'zerop))
           nelisp-vendor-bytecode-triparity--zerop-bytecode-sha256)))

(ert-deftest nelisp-vendor-bytecode-triparity-cadr-fixture-is-pinned ()
  (let ((object (nelisp-vendor-bytecode-triparity--object 'cadr)))
    (should (equal (assq 'cadr nelisp-vendor-bytecode-triparity--fixtures)
                   '(cadr (head middle tail))))
    (should (= (aref object 0) 257))
    (should (equal (append (aref object 1) nil) '(137 65 64 135)))
    (should (equal (aref object 2) []))
    (should (= (aref object 3) 2))
    (should (equal (nelisp-vendor-bytecode-triparity--coverage-bytecode-hash object)
                   "122913e0b5f7d30c803c78773dc279f3c053af5f5202cf8562c14d2148ad0c78"))
    (should (equal (nelisp-vendor-bytecode-triparity--bytecode-hash object)
                   nelisp-vendor-bytecode-triparity--cadr-bytecode-sha256))))

(ert-deftest nelisp-vendor-bytecode-triparity-small-tier-fixtures ()
  (dolist (name '(zerop caar cadr fixnump bignump frame-configuration-p))
    (should (assq name nelisp-vendor-bytecode-triparity--fixtures))
    (let* ((object (nelisp-vendor-bytecode-triparity--object name))
           (expected (cdr (assq name
                                nelisp-vendor-bytecode-triparity--coverage-bytecode-pins))))
      (should (equal (nelisp-vendor-bytecode-triparity--coverage-bytecode-hash object)
                     expected))))
  ;; These hashes encode different payload formats and must stay separate.
  (let ((zerop-object (nelisp-vendor-bytecode-triparity--object 'zerop)))
    (should-not (equal (nelisp-vendor-bytecode-triparity--bytecode-hash zerop-object)
                       (nelisp-vendor-bytecode-triparity--coverage-bytecode-hash
                        zerop-object))))
  (should (eq (nelisp-vendor-bytecode-triparity--sources 'caar) :none))
  ;; caar returns the original cons object, including its identity.
  (let* ((shared (cons 'payload 'tail))
         (argument (list (cons shared 'inner)))
         (function (nelisp-vendor-bytecode-triparity--object 'caar)))
    (should (eq shared (funcall function argument))))
  ;; The boxed-return JIT patch should compile the cons-return caar fixture.
  (should (= (cdr (assq 'caar
                        nelisp-vendor-bytecode-triparity--native-expected))
             1))
  (should (= (cdr (assq 'cadr
                        nelisp-vendor-bytecode-triparity--native-expected))
             1))
  (should (= (cdr (assq 'zerop
                        nelisp-vendor-bytecode-triparity--native-expected))
             1))
  (should (= (cdr (assq 'fixnump
                        nelisp-vendor-bytecode-triparity--native-expected))
             1)))

(ert-deftest nelisp-vendor-bytecode-triparity-child-fingerprint-negative-control ()
  (let ((row '(:native_expected 0 :bytecode_sha256 "triparity-hash"
               :host (:status "ok" :value 42
                     :bytecode_sha256 "triparity-hash")
               :vm (:status "ok" :value 42
                   :bytecode_sha256 "triparity-hash")
               :jit (:status "ok" :value 42 :native 0
                    :bytecode_sha256 "triparity-hash"))))
    (should (nelisp-vendor-bytecode-triparity--equal-p row))
    ;; A fabricated fingerprint from one child must invalidate parity.
    (setq row (plist-put row :vm '(:status "ok" :value 42
                                   :bytecode_sha256 "forged-child-hash")))
    (should-not (nelisp-vendor-bytecode-triparity--equal-p row))
    (should (equal (nelisp-vendor-bytecode-triparity--parity row) "fail"))))

(ert-deftest nelisp-vendor-bytecode-triparity-timeout-budgets ()
  (should (= (nelisp-vendor-bytecode-triparity--timeout-budget 'caar 'jit) 90))
  (should (= (nelisp-vendor-bytecode-triparity--timeout-budget 'cadr 'jit) 90))
  (should (= (nelisp-vendor-bytecode-triparity--timeout-budget 'fixnump 'jit) 90))
  (should (= (nelisp-vendor-bytecode-triparity--timeout-budget 'macroexpand-1 'jit) 45))
  (should (= (nelisp-vendor-bytecode-triparity--timeout-budget 'caar 'vm) 15))
  (should (= (nelisp-vendor-bytecode-triparity--timeout-budget 'cadr 'vm) 15))
  (let ((row '(:host (:status "ok")
               :vm (:status "ok")
               :jit (:status "not-executable" :reason "timeout"))))
    (should (equal (nelisp-vendor-bytecode-triparity--parity row)
                   "not-comparable"))))

(ert-deftest nelisp-vendor-bytecode-triparity-selected-vendor-slice ()
  (let ((old (getenv "NELISP_TRIPARITY_NAMES")))
    (unwind-protect
        (progn
          (setenv "NELISP_TRIPARITY_NAMES" "zerop,cadr,fixnump")
          (should (equal (mapcar #'car
                                 (nelisp-vendor-bytecode-triparity--selected-specs))
                         '(triparity-add1 cadr fixnump zerop))))
      (setenv "NELISP_TRIPARITY_NAMES" old))))

(ert-deftest nelisp-vendor-bytecode-triparity-json-has-decoder-and-phase-evidence ()
  (let* ((row '(:name fixnump :bytecode_sha256 "abc"
                :native_expected 1
                :jit_decoder (:generic-ir-status valid
                              :first-ir-rejection-reason "unsupported semantics at 3"
                              :generic-ir-decoded t
                              :static-jit-eligible-for-fixnum-inputs t
                              :elapsed_ms 0.5)
                :host (:status "ok" :value t)
                :vm (:status "ok" :value t)
                :jit (:status "ok" :value t :native 1 :native_delta 1
                      :fallback_delta 0 :compile_phase "native-executed"
                      :runtime_call_elapsed_ms 3.0 :process_elapsed_ms 40.0)))
         (json (nelisp-vendor-bytecode-triparity--json-row row))
         (encoded (json-encode json)))
    (should (string-match-p "\\\"generic-ir-decoded\\\":true" encoded))
    (should (string-match-p "\\\"native_delta\\\":1" encoded))
    (should (string-match-p
             "\\\"first-ir-rejection-reason\\\":\\\"unsupported semantics at 3\\\""
             encoded))
    (should (string-match-p "\\\"compile_phase\\\":\\\"native-executed\\\"" encoded))))

(ert-deftest nelisp-vendor-bytecode-triparity-counterfactual-is-single-opcode-only ()
  (let* ((encoded
          (json-encode
           (nelisp-vendor-bytecode-triparity--json-counterfactual
            '(:opcode 58 :single-opcode-unlocked-functions (fixture)
              :single-opcode-unlocked-count 1)))))
    (should (string-match-p "functions_unlocked_by_single_opcode" encoded))
    (should (string-match-p "fixture" encoded))
    (should (string-match-p "function_count.*1" encoded))))

(ert-deftest nelisp-vendor-bytecode-triparity-missing-counters-are-not-zero ()
  (let ((summary
         (nelisp-vendor-bytecode-triparity--counter-summary
          '((:name fixture :jit (:status "process-error"))) :native_delta)))
    (should (equal (plist-get summary :status) "unavailable"))
    (should-not (plist-get summary :value))
    (should (= (plist-get summary :measured-count) 0))
    (should (equal (plist-get summary :missing-functions) '(fixture)))))

(ert-deftest nelisp-vendor-bytecode-triparity-rejects-zero-native-delta ()
  (let ((row '(:native_expected 1 :bytecode_sha256 "same"
               :host (:status "ok" :value t :bytecode_sha256 "same")
               :vm (:status "ok" :value t :bytecode_sha256 "same")
               :jit (:status "ok" :value t :native 1 :native_delta 0
                     :bytecode_sha256 "same"))))
    (should-not (nelisp-vendor-bytecode-triparity--equal-p row))))

(ert-deftest nelisp-vendor-bytecode-triparity-macroexpand-host-vm-jit-parity ()
  (let* ((binary (or (getenv "NELISP_BIN")
                     (expand-file-name "target/nelisp"
                                       (nelisp-vendor-bytecode-triparity--root))))
         (arguments '(triparity-not-a-macro))
         host vm jit)
    (skip-unless (file-executable-p binary))
    (nelisp-vendor-bytecode-jit-coverage--load-sources)
    (let* ((function (symbol-function 'macroexpand-1))
           (object (if (byte-code-function-p function)
                       function
                     (byte-compile function))))
      (setq host (nelisp-vendor-bytecode-triparity--lane
                  "emacs" object arguments 'host '("macroexp.el"))
            vm (nelisp-vendor-bytecode-triparity--lane
                binary object arguments 'vm '("macroexp.el"))
            jit (nelisp-vendor-bytecode-triparity--lane
                 binary object arguments 'jit '("macroexp.el")
                 (nelisp-vendor-bytecode-triparity--timeout-budget
                  'macroexpand-1 'jit))))
    (should (equal (mapcar (lambda (lane) (plist-get lane :status))
                   (list host vm jit))
                   '("ok" "ok" "ok")))
    (should (equal (mapcar (lambda (lane)
                             (nelisp-bytecode-corpus--print
                              (plist-get lane :value)))
                           (list host vm jit))
                   (make-list 3 (nelisp-bytecode-corpus--print
                                 (plist-get host :value)))))
    (let ((hashes (mapcar (lambda (lane)
                            (plist-get lane :bytecode_sha256))
                          (list host vm jit))))
      (should (cl-every #'stringp hashes))
      (should (equal (delete-dups hashes) (list (car hashes)))))))

(ert-deftest nelisp-vendor-bytecode-triparity-gnu31-zerop-native-regression ()
  (let* ((root (nelisp-vendor-bytecode-triparity--root))
         (source (expand-file-name "vendor/staged-emacs-lisp/subr.el" root))
         (form (nelisp-vendor-source-form
                "vendor/staged-emacs-lisp/subr.el" 'zerop))
         (object (byte-compile (cons 'lambda (cddr (read form)))))
         (expected-hash
          nelisp-vendor-bytecode-triparity-test--zerop-bytecode-sha256)
         (input-values '(0 2 -1 1))
         (host-program (or (getenv "EMACS") "emacs"))
         (vm-program (or (getenv "NELISP_BIN")
                         (expand-file-name "target/nelisp" root)))
         host-results vm-results jit-result)
    ;; Pin both the source provenance and this exact GNU Emacs 31.1 function.
    (should (file-exists-p source))
    (should (equal (with-temp-buffer
                     (insert-file-contents-literally source)
                     (secure-hash 'sha256 (current-buffer)))
                   "410c34e030bdd667ff21842a8513e41100394662dacf5e54b40938106f0d6327"))
    (should (string-prefix-p "(defun zerop (number)" form))
    (should (string-match-p (regexp-quote "(= 0 number))") form))
    (should (= (aref object 0) 257))
    (should (equal (append (aref object 1) nil) '(137 192 85 135)))
    (should (equal (aref object 2) [0]))
    (should (= (aref object 3) 3))
    (should (equal (nelisp-vendor-bytecode-triparity--bytecode-hash object)
                   expected-hash))
    (skip-unless (file-executable-p vm-program))
    (nelisp-vendor-bytecode-jit-coverage--load-sources)
    (dolist (value input-values)
      (push (nelisp-vendor-bytecode-triparity--lane
             host-program object (list value) 'host '()) host-results)
      (push (nelisp-vendor-bytecode-triparity--lane
             vm-program object (list value) 'vm '()) vm-results))
    (setq host-results (nreverse host-results)
          vm-results (nreverse vm-results))
    ;; Keep all four calls in one process so the measured 3 native calls and
    ;; 1 interpreter fallback are real.
    (let ((jit-form
           (format
            "(progn (load %S nil nil t) (fset 'triparity-zerop (make-byte-code %s %s %s %s)) (setq nelisp-bytecode-jit-threshold 2) (let ((fn (symbol-function 'triparity-zerop)) (values (list (triparity-zerop 0) (triparity-zerop 2) (triparity-zerop -1) (triparity-zerop 1)))) (prin1 (list :status \"ok\" :values values :native nelisp-bytecode-jit--native-call-count :fallback nelisp-bytecode-jit--interpreter-fallback-count :bytecode (list (aref fn 0) (append (aref fn 1) nil) (aref fn 2) (aref fn 3))))) )"
            (expand-file-name "lisp/nelisp-bytecode-jit.el" root)
            (nelisp-bytecode-corpus--print (aref object 0))
            (nelisp-vendor-bytecode-triparity--code-expression
             (aref object 1))
            (nelisp-bytecode-corpus--print (aref object 2))
            (nelisp-bytecode-corpus--print (aref object 3)))))
      (setq jit-result
            (nelisp-vendor-bytecode-triparity--run-child
             vm-program (list "-Q" "--batch" "--eval" jit-form))))
    (let* ((expected-values '(t nil nil nil))
           (jit-bytecode (plist-get jit-result :bytecode))
           (jit-object (make-byte-code
                        (nth 0 jit-bytecode)
                        (apply #'unibyte-string (nth 1 jit-bytecode))
                        (nth 2 jit-bytecode) (nth 3 jit-bytecode)))
           (jit-hash (nelisp-vendor-bytecode-triparity--bytecode-hash
                      jit-object))
           (hashes (append (mapcar (lambda (row)
                                     (plist-get row :bytecode_sha256))
                                   host-results)
                           (mapcar (lambda (row)
                                     (plist-get row :bytecode_sha256))
                                   vm-results)
                           (list jit-hash)))
           (row (list :native_expected 3
                      :bytecode_sha256 expected-hash
                      :host (list :status "ok"
                                  :value (mapcar (lambda (lane)
                                                   (plist-get lane :value))
                                                 host-results)
                                  :bytecode_sha256 expected-hash)
                      :vm (list :status "ok"
                                :value (mapcar (lambda (lane)
                                                 (plist-get lane :value))
                                               vm-results)
                                :bytecode_sha256 expected-hash)
                      :jit (list :status (plist-get jit-result :status)
                                 :value (plist-get jit-result :values)
                                 :native (plist-get jit-result :native)
                                 :bytecode_sha256 jit-hash))))
      (should (cl-every (lambda (lane)
                          (equal (plist-get lane :status) "ok"))
                        (append host-results vm-results (list jit-result))))
      (should (equal (mapcar (lambda (lane) (plist-get lane :value))
                             host-results)
                     expected-values))
      (should (equal (mapcar (lambda (lane) (plist-get lane :value))
                             vm-results)
                     expected-values))
      (should (equal (plist-get jit-result :values) expected-values))
      (should (equal hashes (make-list (length hashes) expected-hash)))
      (should (= (plist-get jit-result :native) 3))
      (should (= (plist-get jit-result :fallback) 1))
      (should (nelisp-vendor-bytecode-triparity--equal-p row))
      ;; Negative control: the same row predicate must reject a missing native call.
      (setq row (plist-put row :jit
                           (plist-put (plist-get row :jit) :native 2)))
      (should-not (nelisp-vendor-bytecode-triparity--equal-p row)))))

(ert-deftest nelisp-vendor-bytecode-triparity-negative-control ()
  (let ((row '(:native_expected 0
               :bytecode_sha256 "same"
               :host (:status "ok" :value 42 :bytecode_sha256 "same")
               :vm (:status "ok" :value 42 :bytecode_sha256 "same")
               :jit (:status "ok" :value 42 :native 0
                     :bytecode_sha256 "same"))))
    (should (nelisp-vendor-bytecode-triparity--equal-p row))
    (should (nelisp-vendor-bytecode-triparity--self-test row))
    (setq row (plist-put row :native_expected 1))
    (should-not (nelisp-vendor-bytecode-triparity--equal-p row))
    (should (equal (nelisp-vendor-bytecode-triparity--parity row) "fail"))
    (setq row (plist-put row :native_expected 0))
    (setq row (plist-put row :jit '(:status "ok" :value 42 :native 0
                                    :bytecode_sha256 "different")))
    (should-not (nelisp-vendor-bytecode-triparity--equal-p row))
    (should (equal (nelisp-vendor-bytecode-triparity--parity row) "fail"))
    (setq row (plist-put row :jit '(:status "runtime-error" :native 0)))
    (should (equal (nelisp-vendor-bytecode-triparity--parity row)
                   "not-comparable"))))

(ert-deftest nelisp-vendor-bytecode-triparity-process-reasons ()
  (let ((file (make-temp-file "triparity-test-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "invalid-read-syntax"))
          (should (equal (plist-get
                          (nelisp-vendor-bytecode-triparity--process-failure 1 file)
                          :reason)
                         "unencodable-constant"))
          (with-temp-file file (insert "void-function: (try-completion)"))
          (should (equal (plist-get
                          (nelisp-vendor-bytecode-triparity--process-failure 1 file)
                          :reason)
                         "missing-function"))
          (should (equal (plist-get
                          (nelisp-vendor-bytecode-triparity--process-failure 124 file)
                          :reason)
                         "timeout"))
          (should (equal (plist-get
                          (nelisp-vendor-bytecode-triparity--process-failure 124 file)
                          :status)
                         "not-executable"))
          (with-temp-file file (insert "other failure"))
          (should (equal (plist-get
                          (nelisp-vendor-bytecode-triparity--process-failure 1 file)
                          :reason)
                         "process-error"))
          )
      (delete-file file))))

(provide 'nelisp-vendor-bytecode-triparity-test)
;;; nelisp-vendor-bytecode-triparity-test.el ends here
