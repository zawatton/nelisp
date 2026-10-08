;;; nelisp-bytecode-native-rooted-cfg-safe-producer-test.el --- safe producer -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)
(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)
(require 'nelisp-aot-compiler)

(defun nelisp-bytecode-native-rooted-cfg-safe-producer-test--spec ()
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile '(lambda (value) (car-safe value)))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan
                input 'safe-primitives-v3))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan nelisp-bytecode-native-rooted-cfg-safe-contract-entry))
         (contract (nelisp-bytecode-native-rooted-cfg-safe-contract-create
                    input plan emitted)))
    (list :input input :plan plan :emitted emitted :contract contract)))

(defun nelisp-bytecode-native-rooted-cfg-safe-producer-test--contract ()
  (plist-get (nelisp-bytecode-native-rooted-cfg-safe-producer-test--spec)
             :contract))

(defun nelisp-bytecode-native-rooted-cfg-safe-producer-test--manifest (contract)
  (let* ((imports (plist-get contract :imports))
         (descriptors
          (mapcar
           (lambda (name)
             (let* ((slot (equal name "nl_root_pin_slot_v2"))
                    (index (if slot
                               (nelisp-native-load--raw-v2-conditional-import-index name)
                             (cl-position name nelisp-native-load-bridgeable-symbols
                                          :test #'equal))))
               (list :name name :kind 'func
                     :abi nelisp-native-load-raw-runtime-abi-v2
                     :index index
                     :address-mode (if slot 'conditional-root-slot-v1
                                     'native-bridgeable-v1)
                     :arity 6 :params '(u64 u64 u64 u64 u64 u64) :return 'u64)))
           imports))
         (entry (list :name nelisp-bytecode-native-rooted-cfg-safe-contract-entry
                      :type 'func :abi nelisp-native-load-raw-runtime-abi-v2
                      :arity 4 :params '(u64 u64 u64 u64) :return 'u64)))
    (list :native-rooted-cfg-safe-v3-contract-version
          nelisp-bytecode-native-rooted-cfg-safe-contract-version
          :native-rooted-cfg-safe-v3-contract contract
          :native-rooted-cfg-safe-v3-entry
          nelisp-bytecode-native-rooted-cfg-safe-contract-entry
          :native-rooted-cfg-safe-v3-imports imports
          :native-rooted-cfg-safe-v3-import-descriptors descriptors
          :native-rooted-cfg-safe-v3-contract-hash (plist-get contract :digest)
          :native (list :imports descriptors :exports (list entry)))))

(defun nelisp-bytecode-native-rooted-cfg-safe-producer-test--rehash (contract)
  (let ((rest contract) (canonical nil))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (unless (eq key :digest)
          (setq canonical (append canonical (list key value))))))
    (let ((print-length nil) (print-level nil) (print-circle t))
      (plist-put contract :digest
                 (secure-hash 'sha256 (prin1-to-string canonical))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-loader/refuses-cyclic-and-mixed-spec-before-backend ()
  (let ((source "lisp/nelisp-bytecode-native-rooted-cfg-plan.el")
        (backend-calls 0) (temp-calls 0)
        (cyclic (list :input nil :plan nil :emitted nil :contract nil)))
    (setcdr (last cyclic) cyclic)
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (&rest _args) (setq temp-calls (1+ temp-calls))
                 (error "unexpected staging file")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (&rest _args) (setq backend-calls (1+ backend-calls))
                 (error "unexpected backend"))))
      (should-error
       (nelisp-native-load-raw-v2-compile-file
        source "unused.nelr" "safe-test" "0" nil nil nil nil nil nil cyclic))
      (should-error
       (nelisp-native-load-raw-v2-compile-file
        source "unused.nelr" "safe-test" "0" nil nil nil nil nil t
        '(:input nil :plan nil :emitted nil :contract nil))))
    (should (= temp-calls 0))
    (should (= backend-calls 0))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-loader/refuses-cyclic-manifest-before-validator ()
  (let* ((contract (list :version "nelisp-native-rooted-cfg-safe-v3"))
         (manifest (list :native-rooted-cfg-safe-v3-contract-version
                         "nelisp-native-rooted-cfg-safe-v3"
                         :native-rooted-cfg-safe-v3-contract contract))
         (slow-calls 0))
    (setcdr (last contract) contract)
    (cl-letf (((symbol-function
                'nelisp-native-load--raw-v2-rooted-cfg-safe-v3-contract-valid-p)
               (lambda (&rest _args) (setq slow-calls (1+ slow-calls)) t)))
      (should-error (nelisp-native-load-raw-v2-check manifest)))
    (should (= slow-calls 0))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-loader/admit-valid-v3-manifest-and-reject-rehashed-mutations ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((contract (nelisp-bytecode-native-rooted-cfg-safe-producer-test--contract))
         (manifest (nelisp-bytecode-native-rooted-cfg-safe-producer-test--manifest
                    contract)))
    (should contract)
    (unload-feature 'nelisp-bytecode-native-rooted-cfg-safe-contract t)
    (should-not (featurep 'nelisp-bytecode-native-rooted-cfg-safe-contract))
    (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
    (should (featurep 'nelisp-bytecode-native-rooted-cfg-safe-contract))
    (let ((problems
           (nelisp-native-load-raw-v2-check
            manifest nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
      (should-not (member '(:raw-native-rooted-cfg-contract-invalid) problems))
      (should (listp problems)))
    (dolist (mutate
             (list (lambda (candidate)
                     (setf (plist-get (plist-get candidate
                                                  :native-rooted-cfg-safe-v3-contract)
                                      :abi) 1))
                   (lambda (candidate)
                     (setf (plist-get (plist-get candidate
                                                  :native-rooted-cfg-safe-v3-contract)
                                      :argument-count) 99))
                   (lambda (candidate)
                     (setf (plist-get (plist-get candidate
                                                  :native-rooted-cfg-safe-v3-contract)
                                      :entry-params) '(u32 u64 u64 u64)))
                   (lambda (candidate)
                     (setf (plist-get (car (plist-get candidate
                                                       :native-rooted-cfg-safe-v3-import-descriptors))
                                      :index) -1))
                   (lambda (candidate)
                     (setf (plist-get (car (plist-get candidate
                                                       :native-rooted-cfg-safe-v3-import-descriptors))
                                      :address-mode) 'untyped-address))
                   (lambda (candidate)
                     (let ((without-root-pin
                            (cl-remove-if
                             (lambda (descriptor)
                               (equal (plist-get descriptor :name)
                                      "nl_root_pin_slot_v2"))
                             (plist-get candidate
                                        :native-rooted-cfg-safe-v3-import-descriptors))))
                       (setf (plist-get candidate
                                        :native-rooted-cfg-safe-v3-import-descriptors)
                             without-root-pin)
                       (setf (plist-get (plist-get candidate :native) :imports)
                             without-root-pin)))
                   (lambda (candidate)
                     (setf (plist-get candidate :native-rooted-cfg-contract-version)
                           "mixed-legacy-contract"))))
      (let* ((candidate (copy-tree manifest))
             (candidate-contract
              (plist-get candidate :native-rooted-cfg-safe-v3-contract)))
        (funcall mutate candidate)
        (when (memq :digest candidate-contract)
          (nelisp-bytecode-native-rooted-cfg-safe-producer-test--rehash
           candidate-contract)
          (setf (plist-get candidate :native-rooted-cfg-safe-v3-contract-hash)
                (plist-get candidate-contract :digest)))
        (should-not
         (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p candidate))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-producer/fingerprint-ignores-printer-truncation-but-covers-mutations ()
  (let* ((result (list :artifact-file-sha256 "artifact" :manifest
                       (list :abi 2 :nested (list 1 2 3))))
         (full (nelisp-bytecode-native-rooted-cfg-native--fingerprint result)))
    (let ((print-length 0) (print-level 0))
      (should (equal full
                     (nelisp-bytecode-native-rooted-cfg-native--fingerprint result)))
      (setf (plist-get (plist-get result :manifest) :abi) 3)
      (should-not (equal full
                         (nelisp-bytecode-native-rooted-cfg-native--fingerprint
                          result))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract/public-bounded-data-facade ()
  (let ((cyclic (list :field 'value)))
    (setcdr (last cyclic) cyclic)
    (should (nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-data-p
             '(:field (1 2 3))))
    (should-not (nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-data-p
                 cyclic))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-loader/revalidates-before-root-address-or-copy ()
  (let* ((bad-contract (list :broken t)) (manifest nil)
         (address-calls 0) (copy-calls 0))
    (setcdr (last bad-contract) bad-contract)
    (setq manifest (list :native-rooted-cfg-safe-v3-contract-version
                         "nelisp-native-rooted-cfg-safe-v3"
                         :native-rooted-cfg-safe-v3-contract bad-contract))
    (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (&rest _args) (setq address-calls (1+ address-calls)) 77))
              ((symbol-function 'nelisp--native-pin-copy-v2)
               (lambda (&rest _args) (setq copy-calls (1+ copy-calls)) 88)))
      (should-error (nelisp-native-load-root-v2-addresses manifest))
      (should-error (nelisp-native-load-root-v2-copy 1 2 0 nil manifest)))
    (should (= address-calls 0))
    (should (= copy-calls 0))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-producer/refuses-default-unsupported-input-before-effects ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile '(lambda (value) (1+ value)))))
         (temp-calls 0)
         (backend-calls 0))
    (cl-letf (((symbol-function 'make-temp-file)
               (lambda (&rest _args) (setq temp-calls (1+ temp-calls))
                 (error "unexpected temporary source")))
              ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
               (lambda (&rest _args) (setq backend-calls (1+ backend-calls))
                 (error "unexpected backend"))))
      (should-error
       (nelisp-bytecode-native-rooted-cfg-native-build-safe-v3
        input "unused.nelr")))
    (should (= temp-calls 0))
    (should (= backend-calls 0))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-producer/bounds-bytecode-function-metadata-before-backend ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((spec (nelisp-bytecode-native-rooted-cfg-safe-producer-test--spec))
         (input (plist-get spec :input))
         (function (plist-get input :function))
         (mutated (copy-tree spec))
         (extra-function (copy-tree spec))
         (nested-function (byte-compile '(lambda () nil)))
         (nested-base (byte-compile '(lambda (value) (list value "constant"))))
         (nested-input (nelisp-bytecode-compiler-input-build
                        (let ((constants (copy-sequence (aref nested-base 2))))
                          (aset constants 0 nested-function)
                          (apply #'make-byte-code
                                 (append (list (aref nested-base 0)
                                               (aref nested-base 1)
                                               constants (aref nested-base 3))
                                         (when (> (length nested-base) 4)
                                           (list (aref nested-base 4)))
                                         (when (> (length nested-base) 5)
                                           (list (aref nested-base 5))))))))
         (nested-spec (copy-tree spec))
         (cycle-spec (copy-sequence spec))
         (constructor-calls 0))
    (should (byte-code-function-p function))
    (should (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p spec))
    (setf (plist-get (plist-get mutated :input) :constants)
          (make-vector 5000 nil))
    (should-not (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p mutated))
    (setq extra-function (append extra-function (list :extra-function function)))
    (should-not (nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-compiler-spec-p
                 extra-function))
    (setf (plist-get nested-spec :input) nested-input)
    (should-not (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p nested-spec))
    (setcdr (last cycle-spec) cycle-spec)
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
               (lambda (&rest _args)
                 (setq constructor-calls (1+ constructor-calls))
                 (error "compiler input constructor ran before bounds check"))))
      (should-not (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p cycle-spec)))
    (should (= constructor-calls 0))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-producer/preflights-bytecode-metadata-before-canonical-rebuild ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((spec (nelisp-bytecode-native-rooted-cfg-safe-producer-test--spec))
         (input (plist-get spec :input))
         (plan (plist-get spec :plan))
         (function (plist-get input :function))
         (oversized-function
          (make-byte-code (aref function 0) (aref function 1)
                          (make-vector 5000 nil) (aref function 3)))
         (cycle (list :cycle))
         (cyclic-function nil)
         (constructor-calls 0))
    (setcdr cycle cycle)
    (setq cyclic-function
          (make-byte-code (aref function 0) (aref function 1)
                          (vector cycle) (aref function 3)))
    (cl-labels
        ((spec-with-function (function)
           (let* ((copy (copy-sequence spec))
                  (input-copy (copy-sequence input))
                  (plan-copy (copy-sequence plan))
                  (plan-input-copy (copy-sequence input)))
             (setf (plist-get input-copy :function) function)
             (setf (plist-get plan-input-copy :function) function)
             (setf (plist-get copy :input) input-copy)
             (setf (plist-get plan-copy :input) plan-input-copy)
             (setf (plist-get copy :plan) plan-copy)
             copy)))
      (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
                 (lambda (&rest _args)
                   (setq constructor-calls (1+ constructor-calls))
                   (error "canonical compiler-input rebuild ran too early"))))
        (should-not (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p
                     (spec-with-function oversized-function)))
        (should-not (nelisp-native-load--rooted-cfg-safe-v3-spec-shape-p
                     (spec-with-function cyclic-function))))
      (should (= constructor-calls 0)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract/portable-dialect-requires-pinned-host-or-runtime-witness ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((inventory nelisp-bytecode-compiler-input--inventory-sha256)
         (host (nelisp-bytecode-compiler-input-dialect))
         (runtime (list :status 'pinned :dialect "GNU Emacs 31.1"
                        :inventory-sha256 inventory
                        :runtime-evidence 'standalone-build-verified))
         (expected (list :status 'pinned :dialect "GNU Emacs 31.1"
                         :inventory-sha256 inventory)))
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
               (lambda () host)))
      (should (equal expected
                     (nelisp-bytecode-native-rooted-cfg-safe-contract--portable-dialect))))
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
               (lambda () runtime)))
      (should (equal expected
                     (nelisp-bytecode-native-rooted-cfg-safe-contract--portable-dialect))))
    (dolist (bad
             (list (plist-put (copy-sequence runtime) :inventory-sha256 "wrong")
                   (plist-put (copy-sequence runtime) :dialect "GNU Emacs 30.1")
                   (plist-put (copy-sequence runtime) :runtime-evidence 'unverified)
                   (list :status 'pinned :dialect "GNU Emacs 31.1"
                         :inventory-sha256 inventory)))
      (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                 (lambda () bad)))
        (should-not
         (nelisp-bytecode-native-rooted-cfg-safe-contract--portable-dialect))))
    (let* ((input (nelisp-bytecode-compiler-input-build
                   (byte-compile '(lambda (value) (car-safe value)))))
           (forged (copy-tree input))
           (evidence (copy-tree (plist-get input :dialect-evidence))))
      (setf (plist-get evidence :comp-sha256) "wrong-source-sha256")
      (setf (plist-get forged :dialect-evidence) evidence)
      (should-not
       (nelisp-bytecode-native-rooted-cfg-safe-contract--canonical-input-p
        forged)))
    (let* ((host-spec
            (nelisp-bytecode-native-rooted-cfg-safe-producer-test--spec))
           (host-contract (plist-get host-spec :contract))
           (function (plist-get (plist-get host-spec :input) :function))
           (runtime-contract
            (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                       (lambda () runtime))
                      ((symbol-function 'nelisp-bytecode-compiler-input--dialect)
                       (lambda () runtime)))
              (let* ((input (nelisp-bytecode-compiler-input-build function))
                     (plan (nelisp-bytecode-native-rooted-cfg-plan
                            input 'safe-primitives-v3))
                     (emitted
                      (nelisp-bytecode-native-rooted-cfg-emit
                       plan nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
                (nelisp-bytecode-native-rooted-cfg-safe-contract-create
                 input plan emitted)))))
      (should (equal host-contract runtime-contract)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-producer/compiles-and-admits-a-real-safe-v3-manifest ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile '(lambda (value) (car-safe value)))))
         (artifact (make-temp-file "nelisp-safe-v3-producer-" nil ".nelr"))
         ;; Host GNU Emacs has no NeLisp reload-contract endpoint.  Give the
         ;; producer/checker one stable test identity; the 88cb reader test is
         ;; the separate proof of runtime identity and mapping.
         (binary (secure-hash 'sha256 "safe-v3-host-test-runtime"))
         (result nil))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
                   (lambda () binary))
                  ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
                   (lambda () t)))
          (setq result
                (nelisp-bytecode-native-rooted-cfg-native-build-safe-v3
                 input artifact))
          (should (eq (plist-get result :status) 'complete))
          (should (file-readable-p artifact))
          (should (equal (plist-get (plist-get result :contract) :imports)
                         '("nl_native_car_v2" "nl_root_pin_slot_v2")))
          (should-not
           (nelisp-native-load-raw-v2-check
            (plist-get result :manifest)
            nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
      (when (file-exists-p artifact)
        (delete-file artifact)))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-native-rooted-cfg-safe-producer-test.el ends here
