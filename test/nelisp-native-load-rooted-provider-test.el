;;; rooted-provider-forms-test.el --- Canonical provider and retained GC controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-load)
(require 'nelisp-native-arithmetic-v2)
(ert-deftest nelisp-provider-forms-exact-partition ()
  (let* ((entry '(defun fixture-entry (env ticket count roots) 0))
         (gc '(defun gc-a (arg0) 0)) (contract '(("gc-a" . 1)))
         (additional (nelisp-native-arithmetic-v2-source))
         (provider (cdr additional))
         (forms (append (list gc) provider (list entry))))
    (should (nelisp-native-load--rooted-cfg-provider-forms-valid-p forms entry contract additional))
    (should (nelisp-native-load--rooted-cfg-provider-forms-valid-p (list gc entry) entry contract nil))
    (should-not (nelisp-native-load--rooted-stack-gc-forms-valid-p
                 (append (list gc) provider) contract))
    (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p forms entry contract nil))
    (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                 (append forms (list (car provider))) entry contract additional))
    (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                 (append (list gc) (cdr provider) (list entry)) entry contract additional))
    (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                 (append (list '(defun gc-a (arg0) 1)) provider (list entry)) entry contract additional))
    (let ((mutated (copy-tree additional)))
      (setcar (nthcdr 3 (cadr mutated)) '(seq 99))
      (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                   (append (list gc) (cdr mutated) (list entry)) entry contract mutated)))))
(ert-deftest nelisp-provider-imports-local-runtime-distinction ()
  (let ((imports (nelisp-native-arithmetic-v2-runtime-imports)))
    (should (= (length imports) 8))
    (should-not (cl-find "wf_write_int" imports :key (lambda (x) (plist-get x :name)) :test #'equal))
    (should-not (cl-find "nl_native_add_v2" imports :key (lambda (x) (plist-get x :name)) :test #'equal))
    (should (equal (plist-get (nelisp-native-arithmetic-v2-descriptor) :exit-kind-layout)
                   '(:bytes 32 :tag 2 :tag-offset 0 :payload-offset 8 :zero-offsets (16 24))))
    (let ((context (nelisp-native-arithmetic-v2-dependency-context))
          (owner (symbol-function 'nelisp-native-arithmetic-v2-runtime-imports)))
      (unwind-protect
          (progn (fset 'nelisp-native-arithmetic-v2-runtime-imports (lambda () nil))
                 (should-not (equal context (nelisp-native-arithmetic-v2-dependency-context))))
        (fset 'nelisp-native-arithmetic-v2-runtime-imports owner)))))
(provide 'rooted-provider-forms-test)
