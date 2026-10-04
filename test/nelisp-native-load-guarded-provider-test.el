;;; Guarded provider admission controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-guarded-lowering)
(ert-deftest nelisp-loader-guarded-authenticated-plan-contract ()
  (require 'nelisp-bytecode-native-rooted-cfg-contract)
  (require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
  (let* ((fixture (byte-compile '(lambda (left right) (+ left right))))
         (input (nelisp-bytecode-compiler-input-build fixture)))
    (dolist (mode '(off on))
      (let* ((plan (nelisp-bytecode-native-rooted-cfg-plan input nil mode))
             (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                       plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
             (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan emitted))
             (source (plist-get emitted :additional-source))
             (entry (plist-get emitted :form)))
        (should (eq (plist-get plan :status) 'complete))
        (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract))
        (should (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                 (append (cdr source) (list entry)) entry nil source contract))
        (let ((mutant (copy-tree contract)))
          (setf (plist-get mutant :arithmetic-guard-mode) (if (eq mode 'on) 'off 'on))
          (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                       (append (cdr source) (list entry)) entry nil source mutant)))))))
(ert-deftest nelisp-loader-guarded-exact-forms ()
  (let* ((entry '(defun fixture-entry (env ticket count roots) 0))
         (gc '(defun gc-a (arg0) 0))
         (gc-contract '(("gc-a" . 1))))
    (dolist (mode '(off on))
      (let* ((source (nelisp-native-load--rooted-cfg-provider-source mode))
             (contract (list :arithmetic-guard-mode mode))
             (forms (append (list gc) (cdr source) (list entry))))
        (should (= (length (cdr source)) (if (eq mode 'on) 5 4)))
        (should (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                 forms entry gc-contract source contract))
        (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                     (append forms (list (cadr source))) entry gc-contract source contract))
        (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                     (append (list '(defun gc-a (arg0) 1)) (cdr source) (list entry))
                     entry gc-contract source contract))
        (when (eq mode 'on)
          (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                       forms entry gc-contract source))
          (should-not (nelisp-native-load--rooted-cfg-provider-forms-valid-p
                       (append (list gc) (butlast (cdr source)) (list entry))
                       entry gc-contract source contract)))))))
(ert-deftest nelisp-loader-guarded-opaque-context ()
  (let* ((owner (lambda () 1)) (a (vector (list owner) "data"))
         (clone (car (read-from-string (prin1-to-string owner)))))
    (should (nelisp-native-load--rooted-cfg-provider-context-data-equal-p a a))
    (should (equal owner clone))
    (should-not (nelisp-native-load--rooted-cfg-provider-context-data-equal-p
                 a (vector (list clone) "data")))))
(ert-deftest nelisp-loader-guarded-runtime-partition-and-owner ()
  (let* ((source (nelisp-native-load--rooted-cfg-provider-source 'on))
         (imports (nelisp-native-arithmetic-v2-runtime-imports))
         (contract (list :arithmetic-guard-mode 'on :additional-source source
                         :local-functions (mapcar (lambda (form) (symbol-name (cadr form))) (cdr source))
                         :runtime-imports imports))
         (name (plist-get (car imports) :name)))
    (should (equal (nelisp-native-load--rooted-cfg-provider-import contract name) (car imports)))
    (should-not (nelisp-native-load--rooted-cfg-provider-import contract "nl_native_add_guard_v1"))
    (let ((mutant (copy-tree contract)))
      (setf (plist-get (car (plist-get mutant :runtime-imports)) :size) 999)
      (should-not (nelisp-native-load--rooted-cfg-provider-import mutant name)))
    (let ((original (symbol-function 'nelisp-native-optimization-guard-v1--copy)) (calls 0))
      (unwind-protect
          (progn
            (fset 'nelisp-native-optimization-guard-v1--copy (lambda (&rest _) (setq calls (1+ calls)) nil))
            (should-not (nelisp-native-load--rooted-cfg-provider-import contract name))
            (should-error (nelisp-native-load--rooted-cfg-provider-cache-context
                           (list :native-rooted-cfg-contract contract)))
            (should (= calls 0)))
        (fset 'nelisp-native-optimization-guard-v1--copy original)))
    (let ((context (nelisp-native-load--rooted-cfg-provider-cache-context
                    (list :native-rooted-cfg-contract contract))))
      (should (nelisp-native-load--rooted-cfg-provider-cache-context-equal-p
               context (nelisp-native-load--rooted-cfg-provider-cache-context
                        (list :native-rooted-cfg-contract contract)))))))
