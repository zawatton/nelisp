;;; nelisp-bytecode-native-rooted-branch-join-test.el --- joined CFG admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-branch-join)
(require 'nelisp-native-load)

(defconst nelisp-rooted-branch-join-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-rooted-branch-join/admit-genuine-gnu31-four-block-car-cdr-frames ()
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  (let* ((dir (make-temp-file "gnu-rooted-branch-join-" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c")) definitions)
    (unwind-protect
        (progn
          (copy-file (expand-file-name
                      "test/fixtures/native-bytecode/gnu-31.1-rooted-branch-join.el"
                      nelisp-rooted-branch-join-test--root) source t)
          (should (byte-compile-file source))
          (delete-file source)
          (setq definitions (nelisp-bytecode-native-package-read-elc-functions elc))
          (dolist (case '((gnu-rooted-branch-join-car car 64)
                          (gnu-rooted-branch-join-cdr cdr 65)))
            (let* ((name (nth 0 case)) (operation (nth 1 case)) (opcode (nth 2 case))
                   (v (cdr (assq name definitions)))
                   (input (nelisp-bytecode-compiler-input-build
                           (make-byte-code (aref v 0) (aref v 1) (aref v 2) (aref v 3))))
                   (plan (nelisp-bytecode-native-rooted-branch-join-plan input operation)))
              (should (eq (plist-get input :status) 'complete))
              (should (equal (plist-get input :code)
                             (if (= opcode 64)
                                 (unibyte-string 2 131 8 0 1 130 9 0 137 64 135)
                               (unibyte-string 2 131 8 0 1 130 9 0 137 65 135))))
              (should (eq (plist-get plan :status) 'complete))
              (should (= (plist-get plan :required-root-count) 5))
              (should (equal (plist-get plan :gateway-imports)
                             (list (format "nl_native_%s_v2" operation)
                                   "nl_root_pin_slot_v2"))))))
      (delete-directory dir t))))

(ert-deftest nelisp-rooted-branch-join/reject-wrong-gateway-and-nearby-control-flow ()
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  (let* ((dir (make-temp-file "gnu-rooted-branch-join-negative-" t))
         (source (expand-file-name "fixture.el" dir)) (elc (concat source "c"))
         definitions)
    (unwind-protect
        (progn
          (copy-file (expand-file-name
                      "test/fixtures/native-bytecode/gnu-31.1-rooted-branch-join.el"
                      nelisp-rooted-branch-join-test--root) source t)
          (should (byte-compile-file source))
          (delete-file source)
          (setq definitions (nelisp-bytecode-native-package-read-elc-functions elc))
          (let* ((v (cdr (assq 'gnu-rooted-branch-join-car definitions)))
                 (input (nelisp-bytecode-compiler-input-build
                         (make-byte-code (aref v 0) (aref v 1) (aref v 2) (aref v 3))))
                 (mutated (copy-sequence input)))
            (should (eq (plist-get
                         (nelisp-bytecode-native-rooted-branch-join-plan input 'cdr)
                         :status) 'unsupported))
            (setq mutated (plist-put mutated :code
                                     (unibyte-string 2 131 8 0 1 130 8 0 137 64 135)))
            (should (eq (plist-get
                         (nelisp-bytecode-native-rooted-branch-join-plan mutated 'car)
                         :status) 'unsupported))))
      (delete-directory dir t))))

(ert-deftest nelisp-rooted-branch-join/public-route-and-boxed-package-pre-effect-refusal ()
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  (let* ((dir (make-temp-file "gnu-rooted-branch-join-public-" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c"))
         (artifact (expand-file-name "wrong-entry.nelr" dir))
         (wrong-artifact (expand-file-name "wrong-input.nelr" dir))
         (package-dir (expand-file-name "boxed-package" dir))
         input function)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name
              "test/fixtures/native-bytecode/gnu-31.1-rooted-branch-join.el"
              nelisp-rooted-branch-join-test--root))
            (goto-char (point-max))
            (insert "\n(provide 'gnu-rooted-branch-join-test)\n")
            (write-region (point-min) (point-max) source nil 'silent))
          (should (byte-compile-file source))
          (delete-file source)
          (let* ((definitions (nelisp-bytecode-native-package-read-elc-functions elc))
                 (v (cdr (assq 'gnu-rooted-branch-join-car definitions))))
            (setq function (make-byte-code (aref v 0) (aref v 1)
                                           (aref v 2) (aref v 3))
                  input (nelisp-bytecode-compiler-input-build function)))
          (let ((result (nelisp-bytecode-native-compiler-build
                         function artifact "wrong_join_entry")))
            (should (eq (plist-get result :status) 'unsupported))
            (should (not (file-exists-p artifact))))
          (let* ((identity (byte-compile '(lambda (value) value)))
                 (result (nelisp-bytecode-native-compiler-build
                          identity wrong-artifact
                          nelisp-bytecode-native-rooted-branch-join-entry)))
            (should (eq (plist-get result :status) 'unsupported))
            (should (not (file-exists-p wrong-artifact))))
          (should-error
           (nelisp-bytecode-native-package-compile-elc
            elc 'gnu-rooted-branch-join-test
            '(gnu-rooted-branch-join-car) package-dir))
          (should-not (file-exists-p package-dir))
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "lisp/nelisp-bytecode-native-package.el"
                               nelisp-rooted-branch-join-test--root))
            (should-not (search-forward
                         "nelisp-bytecode-native-compiler--rooted-branch-join-operation"
                         nil t))))
      (delete-directory dir t))))

(ert-deftest nelisp-rooted-branch-join/typed-import-contract-mutations-refuse ()
  (let* ((operation 'car)
         (gateway "nl_native_car_v2")
         (entry "nl_native_rooted_branch_join_probe_v1")
         (pin "nl_root_pin_slot_v2")
         (params '(u64 u64 u64 u64 u64 u64))
         (gateway-import (list :name gateway :kind 'func
                               :abi nelisp-native-load-raw-runtime-abi-v2
                               :arity 6 :params params :return 'u64
                               :address-mode 'native-bridgeable-v1
                               :index (cl-position gateway
                                                   nelisp-native-load-bridgeable-symbols
                                                   :test #'equal)))
         (pin-import (list :name pin :kind 'func
                           :abi nelisp-native-load-raw-runtime-abi-v2
                           :arity 6 :params params :return 'u64
                           :address-mode 'conditional-root-slot-v1
                           :index (nelisp-native-load--raw-v2-conditional-import-index pin)))
         (export (list :name entry :value 1 :size 64 :type 'func
                       :abi nelisp-native-load-raw-runtime-abi-v2 :arity 4
                       :return 'u64 :params '(u64 u64 u64 u64)))
         (manifest
          (list :native (list :imports (list gateway-import pin-import)
                              :exports (list export))
                :native-rooted-branch-join-contract-version
                nelisp-native-load-raw-v2-rooted-branch-join-contract-version
                :native-rooted-branch-join-operation operation
                :native-rooted-branch-join-entry entry
                :native-rooted-branch-join-imports (sort (list gateway pin) #'string<)
                :native-rooted-branch-join-status-base 256
                :native-rooted-branch-join-contract-hash
                (nelisp-native-load--rooted-branch-join-contract-hash operation))))
    (should (nelisp-native-load--raw-v2-rooted-branch-join-contract-valid-p manifest))
    (dolist (mutation
             '((gateway-abi . "wrong-abi")
               (gateway-arity . 5)
               (gateway-params . (u64 u64 u64 u64 u64))
               (gateway-mode . conditional-root-slot-v1)
               (gateway-index . 0)
               (pin-index . 32)
               (operation . cdr)
               (conflict . stale-stack-contract)))
      (let* ((bad (copy-tree manifest))
             (native (plist-get bad :native))
             (imports (plist-get native :imports))
             (g (copy-sequence (car imports)))
             (p (copy-sequence (cadr imports))))
        (pcase (car mutation)
          ('gateway-abi (plist-put g :abi (cdr mutation)))
          ('gateway-arity (plist-put g :arity (cdr mutation)))
          ('gateway-params (plist-put g :params (cdr mutation)))
          ('gateway-mode (plist-put g :address-mode (cdr mutation)))
          ('gateway-index (plist-put g :index (cdr mutation)))
          ('pin-index (plist-put p :index (cdr mutation)))
          ('operation (plist-put bad :native-rooted-branch-join-operation
                                 (cdr mutation)))
          ('conflict (plist-put bad :native-rooted-stack-contract-version
                                (cdr mutation))))
        (plist-put native :imports (list g p))
        (plist-put bad :native native)
        (should-not (nelisp-native-load--raw-v2-rooted-branch-join-contract-valid-p bad))))))

(ert-deftest nelisp-rooted-branch-join/body-refuses-bad-counts-before-root-lookup ()
  (let* ((body (nelisp-bytecode-native-rooted-branch-join--body 'car))
         (first (nth 3 body)))
    (should (equal (cadr first) '(/= argument-count 3)))
    (should (equal (cadr (nth 3 first)) '(/= root-count 5)))
    (should (equal (car (nth 3 (nth 3 first))) 'let))))

(provide 'nelisp-bytecode-native-rooted-branch-join-test)
;;; nelisp-bytecode-native-rooted-branch-join-test.el ends here
