;;; nelisp-native-load-call1-contract-test.el --- CALL1 gate controls -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-native-load)
(require 'nelisp-runtime-reload-abi)

(ert-deftest nelisp-native-load/raw-v2-call1-needs-exact-typed-contract ()
  "The CALL1 exit import resolves only with its exact typed manifest proof."
  (let* ((entry (list :name nelisp-native-load-raw-v2-call1-import
                      :kind 'func :abi nelisp-native-load-raw-runtime-abi-v2
                      :arity 6 :params '(u64 u64 u64 u64 u64 u64) :return 'u64))
         (manifest (list :call1-contract-version
                         nelisp-native-load-raw-v2-call1-contract-version
                         :call1-contract-hash
                         (nelisp-native-load--raw-v2-call1-contract-hash)
                         :call1-producer-validation-version
                         "nelisp-call1-exact-ast-v1"
                         :call1-producer-ast-sha256 (make-string 64 ?b)
                         :call1-caller
                         '(:name "nl_native_bytecode_call1_exit"
                           :arity 2 :params (u64 u64) :return u64
                           :slots (4 5 2 0))))
         (routes nil))
    (should-not (nelisp-native-load--raw-v2-import-mode
                 nelisp-native-load-raw-v2-call1-import))
    (should (nelisp-native-load--raw-v2-call1-import-valid-p manifest entry))
    (dolist (broken
             (list (plist-put (copy-sequence entry) :arity 5)
                   (plist-put (copy-sequence entry) :params
                              '(sexp-ptr u64 u64 u64 u64 u64))
                   (plist-put (copy-sequence entry) :return 'sexp-ptr)
                   (plist-put (copy-sequence entry) :kind 'data)))
      (should-not (nelisp-native-load--raw-v2-call1-import-valid-p manifest broken)))
    (should-not
     (nelisp-native-load--raw-v2-call1-import-valid-p
      manifest (let ((copy (copy-sequence entry)))
                 (plist-put copy :params '(u64 u64 u64 u64 u64)))))
    (should-not
     (nelisp-native-load--raw-v2-call1-import-valid-p
      (plist-put (copy-sequence manifest) :call1-contract-hash "bad") entry))
    (let* ((raw (list :raw-abi nelisp-native-load-raw-runtime-abi-v2
                      :object-format 'nelisp-aot-raw-unit-v2 :text-size 1 :object-size 1
                      :text-base64 (base64-encode-string "x" t)
                      :object-sha256 (nelisp-native-load--raw-digest "x") :imports nil
                      :exports (list (list :name nelisp-native-load-raw-v2-call1-entry
                                           :value 0 :size 1 :type 'func
                                           :abi nelisp-native-load-raw-runtime-abi-v2
                                           :arity 2 :params '(u64 u64) :return 'u64))
                      :relocs nil :data-size 0 :bss-size 0))
           (candidate (append manifest
                              (list :kind 'raw-runtime :format nelisp-native-load-raw-artifact-format-v2
                                    :runtime-kind 'gc-arena :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                                    :runtime-opt-in t :layout-id nelisp-native-load-raw-layout-id-v2
                                    :arch nelisp-native-load-raw-supported-arch
                                    :binary-sha256 (make-string 64 ?a) :native raw))))
      (let* ((valid-entry (append (copy-sequence entry)
                                  (list :index (cl-position nelisp-native-load-raw-v2-call1-import
                                                            nelisp-native-load-bridgeable-symbols :test #'equal)
                                        :address-mode 'call1-typed-v1)))
             (valid-native (plist-put (copy-sequence raw) :imports (list valid-entry)))
             (valid-manifest (plist-put (copy-sequence candidate) :native valid-native))
             (problems (nelisp-native-load-raw-v2-check
                        valid-manifest nelisp-native-load-raw-v2-call1-entry)))
        (should-not (member :raw-call1-contract (mapcar #'car problems)))
        (should-not (member :raw-import-index (mapcar #'car problems)))
        (should-not (member :raw-call1-entry (mapcar #'car problems)))
        (should-not (member :raw-call1-selected-entry (mapcar #'car problems))))
      (dolist (bad-import
               (list (plist-put (copy-sequence entry) :arity 5)
                     (plist-put (copy-sequence entry) :params '(sexp-ptr u64 u64 u64 u64 u64))
                     nelisp-native-load-raw-v2-call1-import "wf_bytecode_call_gateway_exit_typo"))
        (let* ((bad-native (plist-put (copy-sequence raw) :imports (list bad-import)))
               (bad-manifest (plist-put (copy-sequence candidate) :native bad-native))
               (maps 0) (problems (nelisp-native-load-raw-v2-check bad-manifest)))
          (should problems)
          (when (and (listp bad-import)
                     (equal (plist-get bad-import :name) nelisp-native-load-raw-v2-call1-import))
            (should (member :raw-call1-contract (mapcar #'car problems))))
          (cl-letf (((symbol-function 'nelisp-native-load-manifest) (lambda (_path) bad-manifest))
                    ((symbol-function 'nelisp-native-load--mmap)
                     (lambda (&rest _args) (setq maps (1+ maps)))))
            (should-error (nelisp-native-load-raw-v2-artifact "synthetic.nelr"))
            (should (= maps 0))))))
    (let* ((entry0 (list :name nelisp-native-load-raw-v2-call1-import
                         :kind 'func :abi nelisp-native-load-raw-runtime-abi-v2
                         :arity 6 :params '(u64 u64 u64 u64 u64 u64) :return 'u64
                         :index (cl-position nelisp-native-load-raw-v2-call1-import
                                             nelisp-native-load-bridgeable-symbols :test #'equal)
                         :address-mode 'call1-typed-v1))
           (raw (list :raw-abi nelisp-native-load-raw-runtime-abi-v2
                      :object-format 'nelisp-aot-raw-unit-v2 :text-size 1 :object-size 1
                      :text-base64 (base64-encode-string "x" t)
                      :object-sha256 (nelisp-native-load--raw-digest "x")
                      :imports (list entry0)
                      :exports (list (list :name nelisp-native-load-raw-v2-call1-entry
                                           :value 0 :size 1 :type 'func
                                           :abi nelisp-native-load-raw-runtime-abi-v2
                                           :arity 5 :params '(u64 u64 u64 u64 u64) :return 'u64))
                      :relocs nil :data-size 0 :bss-size 0))
           (candidate (append manifest (list :kind 'raw-runtime :format nelisp-native-load-raw-artifact-format-v2
                                             :runtime-kind 'gc-arena :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                                             :runtime-opt-in t :layout-id nelisp-native-load-raw-layout-id-v2
                                             :arch nelisp-native-load-raw-supported-arch
                                             :binary-sha256 (make-string 64 ?a) :native raw)))
           (maps 0))
      (should (member :raw-call1-entry
                      (mapcar #'car (nelisp-native-load-raw-v2-check
                                     candidate nelisp-native-load-raw-v2-call1-entry))))
      (cl-letf (((symbol-function 'nelisp-native-load-manifest) (lambda (_path) candidate))
                ((symbol-function 'nelisp-native-load--mmap)
                 (lambda (&rest _args) (setq maps (1+ maps)))))
        (should-error (nelisp-native-load-raw-v2-artifact "synthetic.nelr" nelisp-native-load-raw-v2-call1-entry))
        (should (= maps 0)))
      (should (member :raw-call1-selected-entry
                      (mapcar #'car (nelisp-native-load-raw-v2-check candidate "alternate_entry")))))
    (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (name) (push name routes) 101))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_name) (error "untyped resolver must not run"))))
      (should-error (nelisp-native-load--raw-v2-symbol-addr
                     nelisp-native-load-raw-v2-call1-import entry nil))
      (should-not routes)
      (should (= (nelisp-native-load--raw-v2-symbol-addr
                  nelisp-native-load-raw-v2-call1-import entry manifest) 101))
      (should (equal routes (list nelisp-native-load-raw-v2-call1-import))))))

(ert-deftest nelisp-native-load/raw-v2-call1-source-gate-precedes-backend ()
  "Reject CALL1 AST mutations before backend work and prove the red control."
  (let* ((contract '(("nl_gc_probe" . 1)))
         (gc '(defun nl_gc_probe (arg0) 0))
         (import '(defun wf_bytecode_call_gateway_exit
                    (env ticket slot-function slot-argument slot-status slot-exit) 0))
         (good '(defun nl_native_bytecode_call1_exit (env ticket)
                  (extern-call wf_bytecode_call_gateway_exit env ticket 4 5 2 0)))
         (bad '(defun nl_native_bytecode_call1_exit (env ticket)
                 (extern-call wf_bytecode_call_gateway_exit env ticket 4 5 2)))
         (valid (list gc import good)))
    (should (nelisp-native-load--raw-v2-call1-source-valid-p valid contract))
    (dolist (mutant (list (list gc import bad)
                          (list gc import '(defun nl_native_bytecode_call1_exit (env ticket)
                                             (extern-call wf_bytecode_call_gateway_call1 env ticket 4 5 2 0)))
                          (list gc import '(defun nl_native_bytecode_call1_exit (env ticket)
                                             (extern-call wf_bytecode_call_gateway_exit (logior env 1) ticket 4 5 2 0)))
                          (append valid (list '(defun unexpected () 0)))))
      (should-not (nelisp-native-load--raw-v2-call1-source-valid-p mutant contract)))
    (let* ((source-path (make-temp-file "nelisp-call1-source-" nil ".el"))
           (artifact-path (concat source-path ".nelr"))
           (forms (list gc import bad)) (backend-calls 0))
      (unwind-protect
          (progn
            (with-temp-file source-path
              (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))
            (cl-letf (((symbol-function 'nelisp-native-load--raw-v2-contract) (lambda () contract))
                      ((symbol-function 'nelisp-native-load--raw-v2-symbols) (lambda () '("resolver")))
                      ((symbol-function 'nelisp-aot-compile-to-link-unit)
                       (lambda (&rest _args) (setq backend-calls (1+ backend-calls)) (error "backend-reached")))
                      ((symbol-function 'nelisp-standalone--chunk-arena-rewrite) (lambda (form) form))
                      ((symbol-function 'nelisp-native-load--raw-v2-chunk-rewrite) (lambda (forms) forms))
                      ((symbol-function 'nelisp-native-load--raw-v2-rewrite-data-addr) (lambda (forms) forms))
                      ((symbol-function 'nelisp-native-load--raw-v2-fixed-address-p) (lambda (_forms) nil))
                      ((symbol-function 'nelisp-native-load--raw-compile-defun-p) (lambda (_form) t)))
              (should-error (nelisp-native-load-raw-v2-compile-call1-file
                             source-path artifact-path "test" (make-string 64 ?a)))
              (should (= backend-calls 0)))
            ;; Negative control: disable only the validator and prove compilation reaches backend.
            (cl-letf (((symbol-function 'nelisp-native-load--raw-v2-contract) (lambda () contract))
                      ((symbol-function 'nelisp-native-load--raw-v2-symbols) (lambda () '("resolver")))
                      ((symbol-function 'nelisp-native-load--raw-v2-call1-source-valid-p) (lambda (&rest _args) t))
                      ((symbol-function 'nelisp-aot-compile-to-link-unit)
                       (lambda (&rest _args) (setq backend-calls (1+ backend-calls)) (error "backend-reached")))
                      ((symbol-function 'nelisp-standalone--chunk-arena-rewrite) (lambda (form) form))
                      ((symbol-function 'nelisp-native-load--raw-v2-chunk-rewrite) (lambda (forms) forms))
                      ((symbol-function 'nelisp-native-load--raw-v2-rewrite-data-addr) (lambda (forms) forms))
                      ((symbol-function 'nelisp-native-load--raw-v2-fixed-address-p) (lambda (_forms) nil))
                      ((symbol-function 'nelisp-native-load--raw-compile-defun-p) (lambda (_form) t)))
              (should-error (nelisp-native-load-raw-v2-compile-call1-file
                             source-path artifact-path "test" (make-string 64 ?a)))
              (should (= backend-calls 1)))
        (ignore-errors (delete-file source-path))
        (ignore-errors (delete-file artifact-path)))))))

(provide 'nelisp-native-load-call1-contract-test)
;;; nelisp-native-load-call1-contract-test.el ends here
