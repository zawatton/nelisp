;;; provider-import-test.el --- Scoped provider import controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-native-load)
(require 'nelisp-native-arithmetic-v2)

(defun nelisp-provider-import-test--contract ()
  (let ((source (nelisp-native-arithmetic-v2-source))
        (imports (nelisp-native-arithmetic-v2-runtime-imports)))
    (list :additional-source source :runtime-imports imports
          :local-functions (mapcar (lambda (form) (symbol-name (cadr form))) (cdr source))
          :imports (sort (mapcar (lambda (record) (plist-get record :name)) imports) #'string<))))

(defun nelisp-provider-import-test--descriptor (contract name)
  (append (copy-tree (nelisp-native-load--rooted-cfg-provider-import contract name))
          (list :abi nelisp-native-load-raw-runtime-abi-v2
                :address-mode 'arithmetic-provider-v1
                :index (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal))))

(ert-deftest nelisp-provider-import/scoped-and-legacy-names ()
  (let* ((contract (nelisp-provider-import-test--contract))
         (names (plist-get contract :imports)))
    (should (nelisp-native-load--rooted-cfg-import-names-valid-p names contract))
    (should-not (nelisp-native-load--rooted-cfg-import-names-valid-p names))
    (should (nelisp-native-load--rooted-cfg-import-names-valid-p
             '("nl_native_cons_v2" "nl_root_pin_slot_v2")))
    (should-not (nelisp-native-load--rooted-cfg-import-names-valid-p
                 '("nl_native_add_v2") contract))
    (should-not (nelisp-native-load--rooted-cfg-import-names-valid-p
                 '("nl_native_cons_v2" "nl_native_cons_v2")))
    (let ((mutated (copy-tree contract)))
      (setf (plist-get (car (plist-get mutated :runtime-imports)) :size) 16)
      (should-not (nelisp-native-load--rooted-cfg-import-names-valid-p names mutated)))))

(ert-deftest nelisp-provider-import/exact-eight-descriptors ()
  (let ((contract (nelisp-provider-import-test--contract)))
    (dolist (record (plist-get contract :runtime-imports))
      (let* ((name (plist-get record :name))
             (descriptor (nelisp-provider-import-test--descriptor contract name)))
        (should (nelisp-native-load--rooted-cfg-provider-import-valid-p descriptor contract))
        (dolist (pair '((:index . -1) (:kind . unknown) (:address-mode . resolver)))
          (let ((bad (copy-tree descriptor)))
            (setf (plist-get bad (car pair)) (cdr pair))
            (should-not (nelisp-native-load--rooted-cfg-provider-import-valid-p bad contract))))))))

(ert-deftest nelisp-provider-import/resolver-authenticates-before-address-access ()
  ;; The family validator is stubbed only to isolate resolver ordering; this
  ;; test does not establish genuine contract or native runtime acceptance.
  (let* ((contract (nelisp-provider-import-test--contract))
         (descriptor (nelisp-provider-import-test--descriptor contract "nl_alloc_symbol"))
         (manifest (list :native-rooted-cfg-contract-version "fixture" :native-rooted-cfg-contract contract))
         (bridge 0) (raw 0) (validation 0)
         (owner (symbol-function 'nelisp-native-arithmetic-v2-source)))
    (cl-letf (((symbol-function 'nelisp-native-load--raw-v2-rooted-import-contract-valid-p)
               (lambda (_manifest) (setq validation (1+ validation)) t))
              ((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (_name) (setq bridge (1+ bridge)) 1234))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_name) (setq raw (1+ raw)) 5678)))
      (should (= 1234 (nelisp-native-load--raw-v2-symbol-addr "nl_alloc_symbol" descriptor manifest)))
      (should (= validation 1)) (should (= bridge 1)) (should (= raw 0))
      (unwind-protect
          (progn
            (fset 'nelisp-native-arithmetic-v2-source (lambda () '(seq)))
            (should-error (nelisp-native-load--raw-v2-symbol-addr "nl_alloc_symbol" descriptor manifest))
            (should (= bridge 1)) (should (= raw 0)))
        (fset 'nelisp-native-arithmetic-v2-source owner))
      (let ((bad (copy-tree descriptor)))
        (setf (plist-get bad :index) -1)
        (should-error (nelisp-native-load--raw-v2-symbol-addr "nl_alloc_symbol" bad manifest))
        (should (= bridge 1)) (should (= raw 0))))))

(ert-deftest nelisp-provider-import/memo-owner-identity-invalidates ()
  ;; Isolate memo eligibility with a counted slow validator; no native proof.
  (let* ((contract (nelisp-provider-import-test--contract))
         (manifest (list :native-rooted-cfg-contract-version "fixture"
                         :native-rooted-cfg-contract contract))
         (nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
         (nelisp-native-load--raw-v2-rooted-cfg-cache-hits 0)
         (nelisp-native-load--raw-v2-rooted-cfg-cache-misses 0)
         (slow 0) (source (nelisp-native-arithmetic-v2-source))
         (owner (symbol-function 'nelisp-native-arithmetic-v2-source)))
    (cl-letf (((symbol-function 'nelisp-native-load--raw-v2-rooted-cfg-runtime-key)
               (lambda () "unchanged-runtime-fixture"))
              ((symbol-function 'nelisp-native-load--raw-v2-rooted-cfg-contract-valid-slow-p)
               (lambda (_manifest) (setq slow (1+ slow)) t)))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
      (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
      (should (= slow 1))
      (should (= nelisp-native-load--raw-v2-rooted-cfg-cache-hits 1))
      (unwind-protect
          (progn
            ;; Same returned data, distinct function owner: EQUAL is insufficient.
            (fset 'nelisp-native-arithmetic-v2-source (lambda () (copy-tree source)))
            (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p manifest))
            (should (= slow 2))
            (should (= nelisp-native-load--raw-v2-rooted-cfg-cache-hits 1)))
        (fset 'nelisp-native-arithmetic-v2-source owner)))))
