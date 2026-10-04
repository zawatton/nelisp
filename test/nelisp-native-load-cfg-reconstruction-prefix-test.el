;;; nelisp-native-load-cfg-reconstruction-prefix-test.el --- pre-AOT admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-rooted-cfg-contract)

(defun nelisp-cfg-prefix-test--fixture ()
  "Construct genuine GNU byte-code and its independently verified source."
  (let* ((function (byte-compile '(lambda (a b) (cons a b))))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input nil 'off))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                   plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
         (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
                    input plan emitted))
         (forms (mapcar (lambda (entry)
                          (list 'defun (intern (car entry))
                                (cl-loop for i below (cdr entry)
                                         collect (intern (format "arg%d" i))) 0))
                        (nelisp-native-load--raw-v2-contract))))
    (should contract)
    (should (equal (funcall function 'left 'right) '(left . right)))
    (list (list :input input :plan plan :emitted emitted :contract contract)
          (append forms (list (plist-get emitted :form))
                  (cdr (plist-get emitted :additional-source))))))

(defun nelisp-cfg-prefix-test--run (fixture &optional validator backend)
  "Execute the original compiler, stopping only at its AOT boundary."
  (let ((dir (make-temp-file "cfg-prefix-" t)) (calls 0) (emissions 0) result
        (emitter (symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build))
        (owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)))
    (unwind-protect
        (let ((source (expand-file-name "source.el" dir))
              (artifact (expand-file-name "output.nelr" dir)))
          (with-temp-file source
            (dolist (form (cadr fixture)) (prin1 form (current-buffer)) (insert "\n")))
          (cl-letf (((symbol-function 'nelisp-aot-compile-to-link-unit)
                     (or backend (lambda (ast &rest _) (throw 'cfg-prefix-cutoff ast))))
                    ;; The actual rewrite uses its source-owned facade, not this legacy fallback.
                    ((symbol-function 'nelisp-standalone--chunk-arena-rewrite) #'identity)
                    ((symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build)
                     (lambda (&rest args)
                       (setq emissions (1+ emissions)) (apply emitter args)))
                    ((symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)
                     (if validator
                         (lambda (contract &optional mode)
                           (setq calls (1+ calls))
                           (funcall validator contract mode owner))
                       owner)))
            (setq result
                  (catch 'cfg-prefix-cutoff
                    (nelisp-native-load-raw-v2-compile-file
                     source artifact "ert" (make-string 64 ?0)
                     nil nil nil nil nil (car fixture)))))
          (should-not (file-exists-p artifact))
          (list result calls emissions))
      (delete-directory dir t))))

(ert-deftest nelisp-cfg-prefix/one-validator-and-exact-output ()
  (let* ((fixture (nelisp-cfg-prefix-test--fixture))
         (result (nelisp-cfg-prefix-test--run fixture))
         (prepared (nelisp-native-load--raw-v2-chunk-rewrite (cadr fixture)))
         (expected
          (cons 'seq
                (cl-remove-if
                 (lambda (form)
                   (assoc (symbol-name (cadr form))
                          (nelisp-native-load--raw-v2-contract)))
                 (nelisp-native-load--raw-v2-rewrite-data-addr prepared)))))
    (should (= (nth 2 result) 1))
    (should (equal (car result) expected))))

(ert-deftest nelisp-cfg-prefix/caller-mismatches-refused ()
  (dolist (field '(:declared-stack-depth :dialect-evidence))
    (let ((fixture (nelisp-cfg-prefix-test--fixture)))
      (plist-put (plist-get (car fixture) :input) field 'forged)
      (should-error (nelisp-cfg-prefix-test--run fixture))))
  (let ((fixture (nelisp-cfg-prefix-test--fixture)))
    (plist-put (plist-get (car fixture) :input) :function
               (byte-compile '(lambda (a b) (cons b a))))
    (should-error (nelisp-cfg-prefix-test--run fixture))))

(ert-deftest nelisp-cfg-prefix/forged-and-mutated-validator-refused ()
  (let ((fixture (nelisp-cfg-prefix-test--fixture)))
    (should-error (nelisp-cfg-prefix-test--run fixture (lambda (&rest _) t)))
    (should-error
     (nelisp-cfg-prefix-test--run
      fixture (lambda (contract mode owner)
                (prog1 (funcall owner contract mode)
                  (fset 'nelisp-bytecode-native-rooted-cfg-contract-valid-p
                        (lambda (&rest _) t))))))))

(ert-deftest nelisp-cfg-prefix/replayed-and-fabricated-record-refused ()
  (let* ((fixture (nelisp-cfg-prefix-test--fixture))
         (record (nelisp-bytecode-native-rooted-cfg-contract-valid-p
                  (plist-get (car fixture) :contract) :reconstruction)))
    (should-error (nelisp-cfg-prefix-test--run fixture (lambda (&rest _) record)))
    (plist-put record :expected-contract (plist-get (car fixture) :contract))
    (plist-put record :plan (plist-get (car fixture) :plan))
    (plist-put record :emitted (plist-get (car fixture) :emitted))
    (should-error (nelisp-cfg-prefix-test--run fixture (lambda (&rest _) record)))))

(ert-deftest nelisp-cfg-prefix/validator-mutated-during-reconstruction-refused ()
  (let* ((fixture (nelisp-cfg-prefix-test--fixture))
         (emitter (symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build))
         (owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build)
                   (lambda (&rest args)
                     (prog1 (apply emitter args)
                       (fset 'nelisp-bytecode-native-rooted-cfg-contract-valid-p
                             (lambda (&rest _) t))))))
          (should-error (nelisp-cfg-prefix-test--run fixture)))
      (fset 'nelisp-bytecode-native-rooted-cfg-contract-valid-p owner))))

(ert-deftest nelisp-cfg-prefix/stage-order-and-failure-cutoffs ()
  (let ((process-environment (copy-sequence process-environment))
        (log (make-temp-file "cfg-stage-")))
    (unwind-protect
        (cl-labels ((events ()
                      (with-temp-buffer
                        (insert-file-contents log)
                        (mapcar (lambda (line) (car (split-string line " ")))
                                (split-string (buffer-string) "\n" t))))
                    (clear () (with-temp-file log)))
          (setenv "NELISP_ROOTED_CFG_STAGE_LOG" log)
          (nelisp-cfg-prefix-test--run (nelisp-cfg-prefix-test--fixture))
          (should (equal (events) '("producer-raw-contract-validation-start"
                                   "producer-raw-contract-validation-end"
                                   "producer-raw-aot-start")))
          (clear)
          (should-error (nelisp-cfg-prefix-test--run
                         (nelisp-cfg-prefix-test--fixture) (lambda (&rest _) t)))
          (should (equal (events) '("producer-raw-contract-validation-start")))
          (clear)
          (should-error
           (nelisp-cfg-prefix-test--run (nelisp-cfg-prefix-test--fixture) nil
                                      (lambda (&rest _) (list :rodata "forbidden"))))
          (should (equal (events) '("producer-raw-contract-validation-start"
                                   "producer-raw-contract-validation-end"
                                   "producer-raw-aot-start" "producer-raw-aot-end"
                                   "producer-raw-manifest-materialization-start"))))
      (delete-file log))))

(provide 'nelisp-native-load-cfg-reconstruction-prefix-test)

(defun nelisp-cfg-prefix-test--log-failure-cleanup (condition)
  (require 'nelisp-bytecode-native-rooted-cfg-call)
  (require 'nelisp-bytecode-native-rooted-cfg-native)
  (let ((process-environment (copy-sequence process-environment)) released unloaded)
    (setenv "NELISP_ROOTED_CFG_STAGE_LOG" "unused-diagnostic")
    (cl-letf (((symbol-function 'write-region) (lambda (&rest _) (signal condition '("log failed"))))
              ((symbol-function 'nelisp-bytecode-native-rooted-cfg-native-authenticated-result-p) (lambda (_) t))
              ((symbol-function 'nelisp-native-load-raw-v2-check) (lambda (&rest _) nil))
              ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () "binary"))
              ((symbol-function 'nelisp-native-load-root-v2-addresses)
               (lambda (_) '(:environment 1 :begin 10 :reserve 11 :slot 11 :end 12)))
              ((symbol-function 'nelisp-native-load-raw-v2-artifact) (lambda (&rest _) 'mapping))
              ((symbol-function 'nelisp-native-load-unload) (lambda (_) (setq unloaded t)))
              ((symbol-function 'ptr-call)
               (lambda (address &rest _)
                 (cond ((= address 10) 7)
                       ((= address 12) (setq released t) 1)
                       (t (error "primary slot failure"))))))
      (let ((failure
             (should-error
              (nelisp-bytecode-native-rooted-cfg-call
               '(:status complete :plan (:status complete :required-root-count 1)
                 :manifest (:native (:imports nil)) :argument-count 0
                 :required-root-count 1 :runtime-binary-sha256 "binary"
                 :contract (:imports nil))))))
        (should (equal (cadr failure) "primary slot failure")))
      (should released)
      (should unloaded))))

(ert-deftest nelisp-cfg-prefix/log-failure-does-not-interrupt-consumer-cleanup ()
  (nelisp-cfg-prefix-test--log-failure-cleanup 'error))

(ert-deftest nelisp-cfg-prefix/log-quit-does-not-interrupt-consumer-cleanup ()
  (condition-case nil
      (nelisp-cfg-prefix-test--log-failure-cleanup 'quit)
    (quit (ert-fail "Diagnostic quit escaped cleanup"))))
