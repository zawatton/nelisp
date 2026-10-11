;;; nelisp-bytecode-native-rooted-cfg-contract-test.el --- generic CFG contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-rooted-cfg-contract)

(defun nelisp-bytecode-native-rooted-cfg-contract-test--fixture ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile
    (lambda (condition-a left-a right-a condition-b left-b right-b)
      (cons (car (if condition-a left-a right-a))
            (cdr (if condition-b left-b right-b)))))))

(defun nelisp-bytecode-native-rooted-cfg-contract-test--manifest (contract)
  (let* ((imports
          (sort
           (mapcar
            (lambda (name)
              (let* ((slot (equal name "nl_root_pin_slot_v2"))
                     (index (if slot
                                (cl-position name nelisp-native-load-bridgeable-symbols
                                             :test #'equal)
                              (cl-position name nelisp-native-load-bridgeable-symbols
                                           :test #'equal))))
                (append (list :name name :kind 'func
                              :abi nelisp-native-load-raw-runtime-abi-v2
                              :index index
                              :address-mode (if slot 'conditional-root-slot-v1
                                              'native-bridgeable-v1))
                        '(:arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64))))
            (plist-get contract :imports))
           (lambda (a b) (< (plist-get a :index) (plist-get b :index)))))
         (native (list :exports
                       (list (list :name (plist-get contract :entry)
                                   :value 16 :size 80 :type 'func
                                   :abi nelisp-native-load-raw-runtime-abi-v2
                                   :arity 4 :params '(u64 u64 u64 u64) :return 'u64))
                       :imports imports)))
    (list :native native
          :native-rooted-cfg-contract-version
          (plist-get contract :version)
          :native-rooted-cfg-contract contract
          :native-rooted-cfg-import-descriptors imports)))

(defun nelisp-bytecode-native-rooted-cfg-contract-test--redigest (contract)
  (let ((rest contract) (canonical nil))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (unless (eq key :digest)
          (setq canonical (append canonical (list key value))))))
    (secure-hash 'sha256 (prin1-to-string canonical))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract/recomputes-genuine-two-diamond-contract ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires pinned GNU byte-code 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-contract-test--fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_probe_v1"))
         (contract (nelisp-bytecode-native-rooted-cfg-contract-create
                    input plan emitted))
         (manifest (nelisp-bytecode-native-rooted-cfg-contract-test--manifest contract)))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get emitted :status) 'complete))
    (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract))
    (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
    (dolist (key '(:entry-ast :initializers :imports :root-count :input-recipe))
      (let ((mutated (copy-tree contract)))
        (plist-put mutated key
                   (cond ((eq key :imports) '("nl_native_cons_v2"))
                         ((eq key :input-recipe) '(:code "bad"))
                         (t 999)))
        (should-not (nelisp-bytecode-native-rooted-cfg-contract-valid-p mutated))))
    (let* ((native (copy-tree manifest))
           (section (plist-get native :native))
           (descriptors (plist-get section :imports))
           (root (cl-find "nl_root_pin_slot_v2" descriptors
                          :key (lambda (d) (plist-get d :name)) :test #'equal)))
      (plist-put root :index (1+ (plist-get root :index)))
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p native)))
    (dolist (field '(:abi :arity :params :address-mode :index))
      (let* ((mutated (copy-tree manifest))
             (section (plist-get mutated :native))
             (descriptors (plist-get section :imports))
             (gateway (cl-find "nl_native_car_v2" descriptors
                               :key (lambda (d) (plist-get d :name)) :test #'equal)))
        (plist-put gateway field
                   (pcase field
                     (:abi "forged")
                     (:arity 5)
                     (:params '(u64))
                     (:address-mode 'conditional-root-slot-v1)
                     (:index (1+ (plist-get gateway :index)))))
        (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p mutated))))
    (let ((mutated (copy-tree manifest)))
      (plist-put mutated :native-rooted-cfg-contract-version "wrong-version")
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p mutated)))
    (let* ((section (plist-get manifest :native))
           (root (cl-find "nl_root_pin_slot_v2" (plist-get section :imports)
                          :key (lambda (d) (plist-get d :name)) :test #'equal)))
      ;; Mutating the same EQ object must invalidate the validated snapshot.
      (plist-put root :index (1+ (plist-get root :index)))
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract/imports-follow-emitted-calls ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires pinned GNU byte-code 31.1"))
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile (lambda (value) (car value)))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_probe_v1"))
         (contract (nelisp-bytecode-native-rooted-cfg-contract-create
                    input plan emitted))
         (shared (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                  plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
         (shared-contract
          (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
           input plan shared)))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get emitted :status) 'complete))
    (should (equal (plist-get contract :imports) '("nl_native_car_v2")))
    (should (equal (plist-get shared-contract :imports) '("nl_native_car_v2")))
    (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract))
    (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p shared-contract))
    (let ((overclaimed (copy-tree contract)))
      (plist-put overclaimed :imports
                 '("nl_native_car_v2" "nl_root_pin_slot_v2"))
      (plist-put overclaimed :digest
                 (nelisp-bytecode-native-rooted-cfg-contract-test--redigest overclaimed))
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-valid-p overclaimed)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract/recomputes-shared-v2-and-rejects-resigned-mutations ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires pinned GNU byte-code 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-contract-test--fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                   plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
         (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
                    input plan emitted))
         (manifest (nelisp-bytecode-native-rooted-cfg-contract-test--manifest contract)))
    (should (eq (plist-get emitted :status) 'complete))
    (should (equal (plist-get contract :version)
                   nelisp-bytecode-native-rooted-cfg-contract-shared-version))
    (should (equal (plist-get contract :emitter-mode) "postdom-shared-v2"))
    (should (equal (plist-get contract :plan-schema-version) "shared-flat-v1"))
    (dolist (kind '(missing duplicate nil-blocks))
      (let ((mutated (copy-tree contract)))
        (pcase kind
          ('missing
           (let ((rest mutated) data)
             (while rest
               (let ((key (pop rest)) (value (pop rest)))
                 (unless (eq key :plan-schema-version) (push key data) (push value data))))
             (setq mutated (nreverse data))))
          ('duplicate (setq mutated (append mutated '(:plan-schema-version "shared-flat-v1"))))
          ('nil-blocks (plist-put (plist-get mutated :plan) :blocks nil)))
        (plist-put mutated :digest (nelisp-bytecode-native-rooted-cfg-contract-test--redigest mutated))
        (should-not (nelisp-bytecode-native-rooted-cfg-contract-valid-p mutated))))
    (should-not (plist-member (plist-get contract :plan) :entry-ast))
    (should-not (plist-member (plist-get contract :plan) :blocks))
    (let ((mutated (copy-tree contract)))
      (plist-put (plist-get mutated :plan) :blocks (plist-get plan :blocks))
      (plist-put mutated :digest (nelisp-bytecode-native-rooted-cfg-contract-test--redigest mutated))
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-valid-p mutated)))
    (should (equal (plist-get contract :entry)
                   nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
    (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract))
    (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
    (dolist (key '(:version :plan-schema-version :emitter-mode :entry :entry-ast :initializers
                            :imports :root-count :plan))
      (let ((mutated (copy-tree contract t)))
        (plist-put mutated key
                   (pcase key
                     (:version nelisp-bytecode-native-rooted-cfg-contract-version)
                     (:plan-schema-version "forged")
                     (:emitter-mode "reference-v1")
                     (:entry "nl_native_rooted_cfg_probe_v1")
                     (:entry-ast '(defun forged () 512))
                     (:initializers '((:root 9 :value forged)))
                     (:imports '("nl_native_cons_v2"))
                     (:root-count 255)
                     (:plan '(:status complete))))
        ;; A valid digest cannot rescue a structurally false serialized contract.
        (plist-put mutated :digest
                   (nelisp-bytecode-native-rooted-cfg-contract-test--redigest mutated))
        (should-not (nelisp-bytecode-native-rooted-cfg-contract-valid-p mutated))))
    (let* ((wrong-entry (copy-tree manifest t))
           (export (car (plist-get (plist-get wrong-entry :native) :exports))))
      (plist-put export :name "nl_native_rooted_cfg_probe_v1")
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p wrong-entry)))
    (let ((wrong-version (copy-tree manifest t)))
      (plist-put wrong-version :native-rooted-cfg-contract-version
                 nelisp-bytecode-native-rooted-cfg-contract-version)
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p wrong-version)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract/cache-keys-copy-runtime-and-mutations ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires pinned GNU byte-code 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-contract-test--fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_probe_v1"))
         (contract (nelisp-bytecode-native-rooted-cfg-contract-create input plan emitted))
         (manifest (nelisp-bytecode-native-rooted-cfg-contract-test--manifest contract))
         (start-stats (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics))
         (start-hits (plist-get start-stats :hits))
         (start-misses (plist-get start-stats :misses)))
    (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (1+ start-misses)))
    (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p
             (copy-tree manifest)))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :hits)
               (1+ start-hits)))
    (let ((nelisp-native-load-bridgeable-symbols
           (append nelisp-native-load-bridgeable-symbols '("cache-key-sentinel"))))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p
               (copy-tree manifest))))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (+ start-misses 2)))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-state)
               (lambda () '(:generation 987654))))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p
               (copy-tree manifest))))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (+ start-misses 3)))
    (let* ((vector-manifest (append (copy-tree manifest)
                                    (list :cache-test-vector [1 2 3]))))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p vector-manifest))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p
               (copy-tree vector-manifest)))
      (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :hits)
                 (+ start-hits 2)))
      (aset (plist-get vector-manifest :cache-test-vector) 1 99)
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p vector-manifest)))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (+ start-misses 5)))
    (let* ((string-manifest (copy-tree manifest))
           (descriptor (car (plist-get (plist-get string-manifest :native) :imports)))
           (_ (plist-put descriptor :name (copy-sequence (plist-get descriptor :name))))
           (name (plist-get (car (plist-get (plist-get string-manifest :native) :imports))
                            :name)))
      (aset name 0 ?x)
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p string-manifest)))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (+ start-misses 6)))
    (let ((opaque (append (copy-tree manifest)
                          (list :cache-test-opaque (make-hash-table :test #'equal))))
          (deep (copy-tree manifest)))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p opaque))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p opaque))
      (setq deep (append deep (list :cache-test-deep
                                   (let ((value nil))
                                     (dotimes (_ 270) (setq value (list value))) value))))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p deep))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p deep)))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (+ start-misses 10)))
    (let* ((root (cl-find "nl_root_pin_slot_v2"
                          (plist-get (plist-get manifest :native) :imports)
                          :key (lambda (d) (plist-get d :name)) :test #'equal)))
      (plist-put root :index (1+ (plist-get root :index)))
      (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)))
    (should (= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
               (+ start-misses 11)))
    (should-not (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p '(:kind raw-runtime)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract/runtime-key-snapshot-reuses-and-invalidates ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires pinned GNU byte-code 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-contract-test--fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_probe_v1"))
         (contract (nelisp-bytecode-native-rooted-cfg-contract-create input plan emitted))
         (manifest (nelisp-bytecode-native-rooted-cfg-contract-test--manifest contract))
         (start (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics))
         (start-computations (plist-get start :runtime-key-computations))
         (generation (1+ (random 1000000000))))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-state)
               (lambda () (list :generation generation))))
      (let ((nelisp-native-load-bridgeable-symbols
             (mapcar #'copy-sequence nelisp-native-load-bridgeable-symbols))
            (nelisp-native-load-raw-v2-bridgeable-imports
             (mapcar #'copy-sequence nelisp-native-load-raw-v2-bridgeable-imports))
            (nelisp-runtime-reload-symbols
             (and (boundp 'nelisp-runtime-reload-symbols)
                  (mapcar #'copy-sequence nelisp-runtime-reload-symbols))))
        ;; Real ABI/import input lists are cacheable: the second validation reuses
        ;; their digest while still checking the complete manifest snapshot.
        (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
        (let ((after-first (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)))
          (should (= (plist-get after-first :runtime-key-computations)
                     (1+ start-computations)))
          (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
          (let ((after-repeat (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)))
            (should (= (plist-get after-repeat :runtime-key-computations)
                       (plist-get after-first :runtime-key-computations)))
            (should (= (plist-get after-repeat :hits) (1+ (plist-get after-first :hits)))))
          ;; In-place changes to typed import names and bridge order invalidate.
          (let ((before-import (plist-get
                                (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                :runtime-key-computations)))
            (aset (car nelisp-native-load-raw-v2-bridgeable-imports) 0 ?x)
            (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)
            (should (> (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                  :runtime-key-computations)
                       before-import)))
          (let ((before-index (plist-get
                               (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                               :runtime-key-computations)))
            (setq nelisp-native-load-bridgeable-symbols
                  (append (cdr nelisp-native-load-bridgeable-symbols)
                          (list (car nelisp-native-load-bridgeable-symbols))))
            (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)
            (should (> (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                  :runtime-key-computations)
                       before-index)))
          (when (and (boundp 'nelisp-runtime-reload-symbols)
                     nelisp-runtime-reload-symbols)
            (aset (car nelisp-runtime-reload-symbols) 0
                  (if (= (aref (car nelisp-runtime-reload-symbols) 0) ?x) ?y ?x))
            (let ((before-symbol (plist-get
                                  (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                  :runtime-key-computations)))
              (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)
              (should (> (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                    :runtime-key-computations)
                         before-symbol))))
          (setq generation (1+ generation))
          (let ((before-generation (plist-get
                                    (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                    :runtime-key-computations)))
            (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)
            (should (> (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                  :runtime-key-computations)
                       before-generation)))
          (let ((nelisp-native-load-raw-runtime-abi-v2 "changed-abi")
                (before-abi (plist-get
                             (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                             :runtime-key-computations)))
            (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest)
            (should (> (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)
                                  :runtime-key-computations)
                       before-abi)))))
      (should (>= (plist-get (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics) :misses)
                  (+ (plist-get start :misses) 1))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract/snapshot-preserves-eq-sharing-under-collisions ()
  (let* ((shared (list 1 2)) (distinct (list 1 2))
         (text (propertize "abc" 'face 'bold))
         (opaque (lambda () 7))
         (tree (vector shared shared distinct text text opaque)))
    (cl-letf (((symbol-function 'sxhash-eq) (lambda (_) 0)))
      (let ((copy (nelisp-bytecode-native-rooted-cfg-contract--snapshot-data tree 0)))
        (should (equal tree copy))
        (should-not (eq tree copy))
        (should (eq (aref copy 0) (aref copy 1)))
        (should-not (eq (aref copy 0) (aref copy 2)))
        (should-not (eq shared (aref copy 0)))
        (should (eq (aref copy 3) (aref copy 4)))
        (should-not (eq text (aref copy 3)))
        (should (eq (get-text-property 0 'face (aref copy 3)) 'bold))
        (should (eq opaque (aref copy 5))))
      (let ((cycle (cons 1 nil)))
        (setcdr cycle cycle)
        (should-error (nelisp-bytecode-native-rooted-cfg-contract--snapshot-data cycle 0))))))

(provide 'nelisp-bytecode-native-rooted-cfg-contract-test)
;;; nelisp-bytecode-native-rooted-cfg-contract-test.el ends here
