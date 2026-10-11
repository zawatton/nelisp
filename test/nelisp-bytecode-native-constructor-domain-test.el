;;; nelisp-bytecode-native-constructor-domain-test.el --- Narrow runtime domain -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-bytecode-native-consumer)

(defun nelisp-constructor-domain-test--contract (form)
  (let* ((input (nelisp-bytecode-compiler-input-build (byte-compile form)))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                   plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry)))
    (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan emitted)))

(ert-deftest nelisp-constructor-domain/admits-constant-argument-and-cons ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (form '((lambda () 7) (lambda (value) value)
                  (lambda (left right) (cons left right))))
    (should (nelisp-bytecode-native-rooted-cfg-contract-constructor-p
             (nelisp-constructor-domain-test--contract form)))))

(ert-deftest nelisp-constructor-domain/admit-source-free-genuine-gnu-cons-elc ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((directory (make-temp-file "nelisp-constructor-elc-" t))
         (source (expand-file-name "constructor.el" directory))
         (elc (concat source "c")))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defalias 'nelisp-constructor-source-free-fixture "
                    "(lambda (left right) (cons left right)))\n"))
          (should (byte-compile-file source))
          (delete-file source)
          (should-not (file-exists-p source))
          (let* ((function (cdr (assq 'nelisp-constructor-source-free-fixture
                                     (nelisp-bytecode-native-consumer-read-elc-functions elc))))
                 (input (nelisp-bytecode-compiler-input-build function))
                 (plan (nelisp-bytecode-native-rooted-cfg-plan input))
                 (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                           plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
                 (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
                            input plan emitted)))
            (should (memq 66 (append (aref function 1) nil)))
            (should (= (plist-get input :argument-descriptor) 514))
            (should (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract))))
      (delete-directory directory t))))

(ert-deftest nelisp-constructor-domain/refuses-arithmetic-accessors-and-branches ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (form '((lambda (left right) (+ left right))
                  (lambda (value) (car value))
                  (lambda (value) (cdr value))
                  (lambda (condition left right) (cons (if condition left right) 0))))
    (let ((contract (nelisp-constructor-domain-test--contract form)))
      (ert-info ((format "Constructor-domain rejection fixture: %S" form))
        (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract)))
      (when (memq 'condition (cadr form))
        (should (equal (plist-get contract :imports)
                       '("nl_native_cons_v2" "nl_root_pin_slot_v2"))))
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract)))))

(ert-deftest nelisp-constructor-domain/refuses-tampered-imports-or-operations ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((contract (nelisp-constructor-domain-test--contract
                   '(lambda (left right) (cons left right)))))
    (should (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract))
    (let ((copy (copy-tree contract t)))
      (setq copy (plist-put copy :imports nil))
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-constructor-p copy)))
    (let* ((copy (copy-tree contract t))
           (_ (plist-put (plist-get copy :plan) :blocks
                         (plist-get (plist-get (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract :reconstruction) :plan) :blocks)))
           (operation (car (plist-get (car (plist-get (plist-get copy :plan) :blocks))
                                      :operations))))
      (plist-put operation :opcode 'call)
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-constructor-p copy)))))

(ert-deftest nelisp-constructor-domain/refuses-branch-mutant-with-constructor-imports ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((contract (nelisp-constructor-domain-test--contract
                    '(lambda (left right) (cons left right))))
         (copy (copy-tree contract t))
         (plan (plist-get copy :plan))
         (_ (plist-put plan :blocks
                       (plist-get (plist-get (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract :reconstruction) :plan) :blocks)))
         (block (car (plist-get plan :blocks))))
    (should (equal (plist-get copy :imports) '("nl_native_cons_v2")))
    (plist-put block :successors '((:kind taken :target 1)))
    (plist-put plan :blocks (list block (copy-tree block)))
    (should (equal (plist-get copy :imports) (plist-get contract :imports)))
    (should-not (nelisp-bytecode-native-rooted-cfg-contract-constructor-p copy))
    ;; A counterfeit validator cannot authenticate added branch topology.
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)
               (lambda (_) t)))
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-constructor-p copy)))))

(ert-deftest nelisp-constructor-domain/refuses-replaced-validator-before-invocation ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((contract (nelisp-constructor-domain-test--contract
                   '(lambda (left right) (cons left right))))
        (calls 0))
    (should (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract))
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)
               (lambda (_) (setq calls (1+ calls)) t)))
      (should-not (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract))
      (should (= calls 0)))
    (should (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract))))

(ert-deftest nelisp-constructor-domain/producer-refuses-counterfeit-public-capability ()
  (let ((input (nelisp-bytecode-compiler-input-build
                (byte-compile '(lambda (left right) (cons left right)))))
        (requests nil) (backend-calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-reload-contract-matches-p) (lambda () nil))
              ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () "host-control"))
              ((symbol-function 'nelisp-native-compiler-runtime-capability-p)
               (lambda (operations) (push operations requests) t))
              ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
               (lambda (&rest _) (setq backend-calls (1+ backend-calls))
                 (error "constructor-test-backend-reached"))))
      (should-error (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2
                     input "constructor-control.nelr"))
      (should (null requests))
      (should (= backend-calls 0)))))

(ert-deftest nelisp-constructor-domain/producer-refuses-replaced-loader-getter ()
  (let ((input (nelisp-bytecode-compiler-input-build
                (byte-compile '(lambda (left right) (cons left right)))))
        (getter-calls 0) (backend-calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-reload-contract-matches-p) (lambda () nil))
              ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () "host-control"))
              ((symbol-function 'nelisp-native-load-compiler-constructor-contract-p)
               (lambda (_) (setq getter-calls (1+ getter-calls)) t))
              ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
               (lambda (&rest _) (setq backend-calls (1+ backend-calls)) nil)))
      (should-error (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2
                     input "counterfeit-getter.nelr"))
      (should (= getter-calls 0))
      (should (= backend-calls 0)))))

(ert-deftest nelisp-constructor-domain/producer-keeps-arithmetic-fail-closed ()
  (let ((input (nelisp-bytecode-compiler-input-build
                (byte-compile '(lambda (left right) (+ left right)))))
        (capability-calls 0) (backend-calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-reload-contract-matches-p) (lambda () nil))
              ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () "host-control"))
              ((symbol-function 'nelisp-native-compiler-runtime-capability-p)
               (lambda (_) (setq capability-calls (1+ capability-calls)) t))
              ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
               (lambda (&rest _) (setq backend-calls (1+ backend-calls)) nil)))
      (should-error (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2
                     input "arithmetic-control.nelr"))
      (should (= capability-calls 0))
      (should (= backend-calls 0)))))
