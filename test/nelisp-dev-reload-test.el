;;; nelisp-dev-reload-test.el --- reload plan authority tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-dev-reload)

(defun nelisp-dev-reload-test--request (op args)
  (list (cons "schema_version" "1") (cons "operation" op)
        (cons "request_id" "reload-test") (cons "arguments" args)
        (cons "target" "native-linux-x86_64")
        (cons "session_id" :null) (cons "limits" nil)))

(defun nelisp-dev-reload-test--context ()
  (list :root default-directory :target "native-linux-x86_64"
        :session-id (copy-sequence nelisp-dev-reload--session-id) :live-session t))

(ert-deftest nelisp-dev-reload/file-hash-is-content-bound-and-changes-on-edit ()
  (let ((path (make-temp-file "nelisp-reload-hash-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "one"))
          (let ((first (nelisp-dev-reload--sha256-file path)))
            (with-temp-file path (insert "two"))
            (should (stringp first))
            (should-not (equal first (nelisp-dev-reload--sha256-file path)))))
      (delete-file path))))

(ert-deftest nelisp-dev-reload/inputs-track-file-addition-and-removal ()
  (let ((root (make-temp-file "nelisp-reload-inputs-" t)))
    (unwind-protect
        (progn
          (dolist (dir '("lisp" "src" "scripts"))
            (make-directory (expand-file-name dir root)))
          (with-temp-file (expand-file-name "lisp/one.el" root) (insert "one"))
          (let ((one (nelisp-dev-reload--inputs root)))
            (with-temp-file (expand-file-name "src/two.el" root) (insert "two"))
            (let ((two (nelisp-dev-reload--inputs root)))
              (should-not (equal one two))
              (delete-file (expand-file-name "src/two.el" root))
              (should (equal one (nelisp-dev-reload--inputs root))))))
      (delete-directory root t))))

(defmacro nelisp-dev-reload-test--with-stubs (&rest body)
  `(let ((nelisp-dev-reload--plans (make-hash-table :test #'equal))
         (nelisp-dev-reload--session-id "reload-test-session")
         (generation 4) (install-calls 0) (build-calls 0))
     (cl-letf (((symbol-function 'nelisp-runtime-reload-status)
                (lambda () (list :status 'ready :generation generation)))
               ((symbol-function 'nelisp-native-load--running-binary-sha256)
                (lambda () "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
               ((symbol-function 'nelisp-runtime-reload-contract-hash)
                (lambda () "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"))
               ((symbol-function 'nelisp-dev-reload--inputs)
                (lambda (_root)
                  '(("scripts/compiler.el" .
                     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
               ((symbol-function 'nelisp-runtime-build-and-stage)
                (lambda (_root)
                  (setq build-calls (1+ build-calls))
                  (list :status 'staged :source "reload-source"
                        :artifact "reload-artifact"
                        :generation generation :binary-sha256
                        "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                        :alloc-handle '(:entry-name "alloc"
                                        :artifact-sha256 "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :source-sha256 "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
                        :gc-handle '(:entry-name "gc"
                                     :artifact-sha256 "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                     :source-sha256 "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"))))
               ((symbol-function 'nelisp-dev-reload--sha256-file)
                (lambda (_path) "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"))
               ((symbol-function 'nelisp-native-load-raw-install)
                (lambda (_alloc _gc) (setq install-calls (1+ install-calls))
                  (list :status 'published :generation (1+ generation)))))
       ,@body)))

(ert-deftest nelisp-dev-reload/plan-requires-explicit-effects ()
  (nelisp-dev-reload-test--with-stubs
   (let ((result (nelisp-dev-reload-plan-dispatch
                  (nelisp-dev-reload-test--request
                   "reload.plan"
                   '(("unit" . "allocator-gc") ("atomicity" . "runtime-unit")))
                  (nelisp-dev-reload-test--context))))
     (should (equal "failed" (cdr (assoc "status" result))))
     (should (equal "NELISP-DEV-EFFECTS-POLICY-REQUIRED"
                    (cdr (assoc "code" (aref (cdr (assoc "diagnostics" result)) 0))))))))

(ert-deftest nelisp-dev-reload/apply-uses-authority-after-return-mutation ()
  (nelisp-dev-reload-test--with-stubs
   (let* ((ctx (nelisp-dev-reload-test--context))
          (planned (nelisp-dev-reload-plan-dispatch
                    (nelisp-dev-reload-test--request "reload.plan"
                                                     '(("unit" . "allocator-gc")
                                                       ("atomicity" . "runtime-unit")
                                                       ("effects_policy" . "explicit-only")))
                    ctx))
          (data (cdr (assoc "data" planned)))
          (id (cdr (assoc "plan_id" data))))
     (should (= build-calls 1))
     (setcdr (assoc "plan_id" data) "tampered")
     (let ((applied (nelisp-dev-reload-apply-dispatch
                     (nelisp-dev-reload-test--request
                      "reload.apply" `(("plan_id" . ,id) ("effects_policy" . "explicit-only")))
                     ctx)))
       (should (equal "ok" (cdr (assoc "status" applied))))
       (should (= install-calls 1))
       (should (= build-calls 1))))))

(ert-deftest nelisp-dev-reload/apply-stale-generation-does-not-install ()
  (nelisp-dev-reload-test--with-stubs
   (let* ((ctx (nelisp-dev-reload-test--context))
          (planned (nelisp-dev-reload-plan-dispatch
                    (nelisp-dev-reload-test--request "reload.plan"
                                                     '(("unit" . "allocator-gc")
                                                       ("atomicity" . "runtime-unit")
                                                       ("effects_policy" . "explicit-only")))
                    ctx))
          (id (cdr (assoc "plan_id" (cdr (assoc "data" planned))))))
     (setq generation 5)
     (let ((result (nelisp-dev-reload-apply-dispatch
                    (nelisp-dev-reload-test--request
                     "reload.apply" `(("plan_id" . ,id) ("effects_policy" . "explicit-only")))
                    ctx)))
       (should (equal "failed" (cdr (assoc "status" result))))
       (should (= install-calls 0))
       (should (= build-calls 1))))))

(ert-deftest nelisp-dev-reload/returned-plan-string-mutation-does-not-change-authority ()
  (nelisp-dev-reload-test--with-stubs
   (let* ((ctx (nelisp-dev-reload-test--context))
          (planned (nelisp-dev-reload-plan-dispatch
                    (nelisp-dev-reload-test--request
                     "reload.plan"
                     '(("unit" . "allocator-gc") ("atomicity" . "runtime-unit")
                       ("effects_policy" . "explicit-only"))) ctx))
          (data (cdr (assoc "data" planned)))
          (id (cdr (assoc "plan_id" data)))
          (expected (copy-sequence id)))
     (aset id 0 (if (= (aref id 0) ?a) ?b ?a))
     (dolist (field '("session_id" "binary_hash" "abi_hash" "unit" "atomicity"))
       (let ((text (cdr (assoc field data))))
         (when (stringp text) (aset text 0 ?!))))
     (let ((result (nelisp-dev-reload-apply-dispatch
                    (nelisp-dev-reload-test--request
                     "reload.apply" `(("plan_id" . ,expected)
                                       ("effects_policy" . "explicit-only"))) ctx)))
       (should (equal "ok" (cdr (assoc "status" result))))
       (should (= install-calls 1))))))

(ert-deftest nelisp-dev-reload/install-error-consumes-plan ()
  (nelisp-dev-reload-test--with-stubs
   (cl-letf (((symbol-function 'nelisp-native-load-raw-install)
              (lambda (&rest _) (setq install-calls (1+ install-calls))
                (error "simulated install interruption"))))
     (let* ((ctx (nelisp-dev-reload-test--context))
            (planned (nelisp-dev-reload-plan-dispatch
                      (nelisp-dev-reload-test--request
                       "reload.plan"
                       '(("unit" . "allocator-gc") ("atomicity" . "runtime-unit")
                         ("effects_policy" . "explicit-only"))) ctx))
            (id (cdr (assoc "plan_id" (cdr (assoc "data" planned))))))
       (should (equal "failed"
                      (cdr (assoc "status"
                                  (nelisp-dev-reload-apply-dispatch
                                   (nelisp-dev-reload-test--request
                                    "reload.apply" `(("plan_id" . ,id)
                                                      ("effects_policy" . "explicit-only"))) ctx)))))
       (should (= 0 (hash-table-count nelisp-dev-reload--plans)))
       (should (= install-calls 1))))))

(ert-deftest nelisp-dev-reload/apply-rejects-each-changed-identity ()
  (dolist (kind '(inputs options binary abi source artifact))
    (nelisp-dev-reload-test--with-stubs
     (let* ((ctx (nelisp-dev-reload-test--context))
            (planned (nelisp-dev-reload-plan-dispatch
                      (nelisp-dev-reload-test--request
                       "reload.plan"
                       '(("unit" . "allocator-gc") ("atomicity" . "runtime-unit")
                         ("effects_policy" . "explicit-only"))) ctx))
            (id (cdr (assoc "plan_id" (cdr (assoc "data" planned))))))
       (cl-letf (((symbol-function 'nelisp-dev-reload--inputs)
                  (if (eq kind 'inputs)
                      (lambda (_root) '(("scripts/changed.el" . "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")))
                    (symbol-function 'nelisp-dev-reload--inputs)))
                 ((symbol-function 'nelisp-dev-reload--options)
                  (if (eq kind 'options) (lambda () "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
                    (symbol-function 'nelisp-dev-reload--options)))
                 ((symbol-function 'nelisp-native-load--running-binary-sha256)
                  (if (eq kind 'binary) (lambda () "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
                    (symbol-function 'nelisp-native-load--running-binary-sha256)))
                 ((symbol-function 'nelisp-runtime-reload-contract-hash)
                  (if (eq kind 'abi) (lambda () "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
                    (symbol-function 'nelisp-runtime-reload-contract-hash)))
                 ((symbol-function 'nelisp-dev-reload--sha256-file)
                  (if (memq kind '(source artifact))
                      (lambda (path)
                        (if (equal path (if (eq kind 'source) "reload-source" "reload-artifact"))
                            (make-string 64 ?e) (make-string 64 ?c)))
                    (symbol-function 'nelisp-dev-reload--sha256-file))))
         (let ((result (nelisp-dev-reload-apply-dispatch
                        (nelisp-dev-reload-test--request
                         "reload.apply" `(("plan_id" . ,id) ("effects_policy" . "explicit-only"))) ctx)))
           (should (equal "failed" (cdr (assoc "status" result))))
           (should (= install-calls 0))))))))

(defun nelisp-dev-reload-test--plan (ctx)
  (let ((result (nelisp-dev-dispatch
                 (nelisp-dev-reload-test--request
                  "reload.plan" '(("unit" . "allocator-gc")
                                  ("atomicity" . "runtime-unit")
                                  ("effects_policy" . "explicit-only"))) ctx)))
    (should (equal "ok" (cdr (assoc "status" result))))
    (cdr (assoc "plan_id" (cdr (assoc "data" result))))))

(ert-deftest nelisp-dev-reload/lifetime-limits-and-last-instant-generation ()
  (dolist (mode '(expired capacity cleared repeated late-generation))
    (nelisp-dev-reload-test--with-stubs
     (let* ((system-type 'gnu/linux)
            (ctx (nelisp-dev-reload-context))
            (id (nelisp-dev-reload-test--plan ctx))
            (request (nelisp-dev-reload-test--request
                      "reload.apply" (list (cons "plan_id" id)
                                           '("effects_policy" . "explicit-only")))))
       (pcase mode
         ('expired (setf (plist-get (gethash id nelisp-dev-reload--plans) :expires) 0))
         ('capacity
          (let ((nelisp-dev-reload--max-plans 1))
            (let ((result (nelisp-dev-dispatch
                           (nelisp-dev-reload-test--request
                            "reload.plan" '(("unit" . "allocator-gc")
                                            ("atomicity" . "runtime-unit")
                                            ("effects_policy" . "explicit-only"))) ctx)))
              (should (equal "failed" (cdr (assoc "status" result))))
              (should (= build-calls 1))))
          (nelisp-dev-reload-clear))
         ('cleared (should (= 1 (nelisp-dev-reload-clear))))
         ('repeated
          (should (equal "ok" (cdr (assoc "status" (nelisp-dev-dispatch request ctx)))))))
       (let ((hash-fn (symbol-function 'nelisp-dev-reload--sha256-file)))
         (cl-letf (((symbol-function 'nelisp-dev-reload--sha256-file)
                    (lambda (path)
                      (when (eq mode 'late-generation) (setq generation 9))
                      (funcall hash-fn path))))
           (should (equal "failed" (cdr (assoc "status" (nelisp-dev-dispatch request ctx)))))))
     (should (= install-calls (if (eq mode 'repeated) 1 0)))))))

(provide 'nelisp-dev-reload-test)
;;; nelisp-dev-reload-test.el ends here
