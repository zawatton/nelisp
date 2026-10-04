;;; nelisp-bytecode-native-rooted-cfg-stage-log-test.el --- Producer phases -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-native)

(defun nelisp-rooted-stage-test--run (failure runtime replacement logging)
  "Run the genuine producer with bounded dependency fixtures."
  (let* ((directory (make-temp-file "rooted-stage-test-" t))
         (log (expand-file-name "stages.log" directory))
         (artifact (expand-file-name "result.nelr" directory))
         (process-environment (copy-sequence process-environment))
         (nelisp-bytecode-native-rooted-cfg-native--registry nil)
         (original (symbol-function 'nelisp-bytecode-native-rooted-cfg-native--build))
         (source nil) (compiled nil) (checked nil) (condition nil) (result nil))
    (setenv "NELISP_ROOTED_CFG_STAGE_LOG" (and logging log))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-plan)
                   (lambda (&rest _) (when (eq failure 'plan) (signal 'arith-error '(plan)))
                     '(:status complete :arity 0)))
                  ((symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build)
                   (lambda (&rest _) '(:status complete :form (quote nil))))
                  ((symbol-function 'nelisp-native-load-running-binary-sha256)
                   (lambda () "binary"))
                  ((symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2)
                   (lambda (&rest _) '(:fixture t)))
                  ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
                   (lambda () runtime))
                  ((symbol-function 'nelisp-native-load-compiler-constructor-contract-p)
                   (lambda (_) (setq checked t)
                     (when (eq failure 'constructor) (signal 'arith-error '(constructor)))
                     (not (eq failure 'refuse))))
                  ((symbol-function 'nelisp-native-load-raw-v2-contract) (lambda () nil))
                  ((symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)
                   (lambda (_) t))
                  ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
                   (lambda (path &rest _) (setq compiled t source path)
                     (with-temp-file artifact (insert "fixture")) '(:fixture manifest)))
                  ((symbol-function 'nelisp-native-load-raw-v2-check) (lambda (&rest _) nil))
                  ((symbol-function 'nelisp-bytecode-native-rooted-cfg-native--fingerprint)
                   (lambda (_) "fixture")))
          ;; Capture the fixture dependency owner through the original lexical seal.
          (load "nelisp-bytecode-native-rooted-cfg-native" nil t)
          (when replacement
            (fset 'nelisp-native-load-compiler-constructor-contract-p
                  (lambda (_) (error "Replacement checker must not run"))))
          (condition-case caught
              (setq result (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2
                            'fixture artifact))
            (error (setq condition caught)))
          (let ((text (and (file-exists-p log)
                           (with-temp-buffer (insert-file-contents log) (buffer-string)))))
            ;; Recover the temporary source from its public log on admission errors.
            (when (and (not source) text
                       (string-match "producer-contract-start source=\\([^ \n]+\\) artifact=" text))
              (unless (equal (match-string 1 text) "nil")
                (setq source (match-string 1 text))))
            (list :condition condition :result result :compiled compiled :checked checked
                  :log text :source-exists (and source (file-exists-p source)))))
      (fset 'nelisp-bytecode-native-rooted-cfg-native--build original)
      (when (and source (file-exists-p source)) (delete-file source))
      (delete-directory directory t))))

(defun nelisp-rooted-stage-test--labels (result)
  (mapcar (lambda (line) (car (split-string line " ")))
          (split-string (or (plist-get result :log) "") "\n" t)))

(ert-deftest nelisp-rooted-stage-plan-failure-entry ()
  (let ((result (nelisp-rooted-stage-test--run 'plan nil nil t)))
    (should (equal (plist-get result :condition) '(arith-error plan)))
    (should-not (plist-get result :compiled))
    (should (equal (nelisp-rooted-stage-test--labels result) '("producer-plan-start")))))

(ert-deftest nelisp-rooted-stage-constructor-failure-entry ()
  (let ((result (nelisp-rooted-stage-test--run 'constructor nil nil t)))
    (should (equal (plist-get result :condition) '(arith-error constructor)))
    (should-not (plist-get result :compiled))
    (should (equal (car (last (nelisp-rooted-stage-test--labels result)))
                   "producer-constructor-check-start"))
    ;; Admission exceptions historically precede the producer cleanup owner.
    (should (plist-get result :source-exists))))

(ert-deftest nelisp-rooted-stage-phase-order-and-cleanup ()
  (let ((result (nelisp-rooted-stage-test--run nil nil nil t)))
    (should-not (plist-get result :condition))
    (should (eq (plist-get (plist-get result :result) :status) 'complete))
    (should-not (plist-get result :source-exists))
    (should (equal (nelisp-rooted-stage-test--labels result)
                   '("producer-plan-start" "producer-plan-end"
                     "producer-emit-start" "producer-emit-end"
                     "producer-binary-start" "producer-binary-end"
                     "producer-source-start" "producer-source-end"
                     "producer-contract-start" "producer-contract-end"
                     "producer-runtime-match-start" "producer-runtime-match-end"
                     "producer-constructor-check-start" "producer-constructor-check-end"
                     "producer-compile-start" "producer-compile-return"
                     "producer-manifest-accepted"
                     "producer-result-seal-start" "producer-result-seal-end"
                     "producer-cleanup-source-start" "producer-cleanup-source-end")))
    (should (string-match "producer-compile-start source=.+ artifact="
                          (plist-get result :log)))))

(ert-deftest nelisp-rooted-stage-runtime-short-circuit ()
  (let ((result (nelisp-rooted-stage-test--run nil t nil t)))
    (should (plist-get result :compiled))
    (should-not (plist-get result :checked))
    (should-not (member "producer-constructor-check-start"
                        (nelisp-rooted-stage-test--labels result)))))

(ert-deftest nelisp-rooted-stage-constructor-owner-refusal ()
  (let ((result (nelisp-rooted-stage-test--run nil nil t t)))
    (should (eq (car (plist-get result :condition)) 'error))
    (should-not (plist-get result :checked))
    (should-not (plist-get result :compiled))
    (should-not (plist-get result :source-exists))
    (should-not (member "producer-constructor-check-start"
                        (nelisp-rooted-stage-test--labels result)))))

(ert-deftest nelisp-rooted-stage-unset-does-not-write ()
  (let ((result (nelisp-rooted-stage-test--run nil t nil nil)))
    (should (plist-get result :compiled))
    (should-not (plist-get result :condition))
    (should-not (plist-get result :log))
    (should-not (plist-get result :source-exists))))

;;; nelisp-bytecode-native-rooted-cfg-stage-log-test.el ends here
