;;; nelisp-native-compiler-runtime-capability.el --- Immutable compiler operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The generated proof and this module are loaded during trusted startup.
;; A missing startup provider stays unavailable; later function definitions
;; cannot install a certificate. Constructor eligibility grants no CALL or
;; numeric eligibility and never changes the ticket/GC proof domain.

;;; Code:

(require 'cl-lib)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-native-compiler-runtime-proof nil t)
(require 'nelisp-native-compiler-f1-runtime-proof nil t)
(declare-function nelisp-native-load-running-binary-sha256 "nelisp-native-load")
(declare-function nelisp-native-compiler-runtime-capability--metadata-p
                  "nelisp-native-compiler-runtime-capability")

(defconst nelisp-native-compiler-runtime-capability--constructor-exports
  '(("nl_arena_base" data 8)
    ("nl_alloc_symbol" func 3)
    ("nelisp_cons_construct" func 3)
    ("nl_native_cons_v2" func 5)
    ("nl_root_pin_begin_v2" func 1)
    ("nl_root_pin_end_v2" func 2)
    ("nl_root_pin_reserve_v2" func 2)
    ("nl_root_pin_slot_v2" func 3)))

(defun nelisp-native-compiler-runtime-capability--digest-p (value)
  "Check one exact lowercase SHA256 VALUE."
  (and (stringp value) (= (length value) 64)
       (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value)))

(let ((expected-exports
       (copy-tree nelisp-native-compiler-runtime-capability--constructor-exports)))
(defun nelisp-native-compiler-runtime-capability--metadata-p (metadata)
  "Validate the bounded constructor proof metadata schema.
This shape check cannot issue or install a runtime capability."
  (let* ((exports (plist-get metadata :exports))
         (f1 (eq (plist-get metadata :domain) 'compiler-f1-runtime-v1))
         (expected-exports (if f1 (append expected-exports '(("nl_native_funcall_v2" func 6))) expected-exports)))
    (and (eq (plist-get metadata :version) (if f1 2 1))
         (or (not f1)
             (and (equal (plist-get metadata :funcall-descriptor) (nelisp-native-funcall-v2-descriptor))
                  (equal (plist-get metadata :funcall-hash) (nelisp-native-funcall-v2-hash))))
         (memq (plist-get metadata :domain) '(compiler-runtime-v1 compiler-f1-runtime-v1))
         (equal (plist-get metadata :operation-eligibility) (if f1 '(f1) '(constructor)))
         (cl-every #'nelisp-native-compiler-runtime-capability--digest-p
                   (mapcar (lambda (key) (plist-get metadata key))
                           '(:abi-sha256 :binary-sha256 :active-manifest-sha256)))
         (listp exports) (= (length exports) (if f1 9 8))
         (cl-every
          (lambda (expected)
            (let ((matches (cl-remove-if-not
                            (lambda (entry)
                              (equal (plist-get entry :name) (car expected))) exports)))
              (and (= (length matches) 1)
                   (let ((entry (car matches)))
                     (and (eq (plist-get entry :kind) (nth 1 expected))
                          (if (eq (nth 1 expected) 'data)
                              (= (or (plist-get entry :size) -1) (nth 2 expected))
                            (= (or (plist-get entry :arity) -1) (nth 2 expected)))
                          (integerp (plist-get entry :address))
                          (> (plist-get entry :address) 0)
                          (integerp (plist-get entry :size))
                          (<= 1 (plist-get entry :size) 65536))))))
          expected-exports)))))

(let* ((names '(nelisp-native-compiler-runtime-proof-create
                nelisp-native-compiler-runtime-proof-valid-p
                nelisp-native-compiler-runtime-proof-metadata
                nelisp-native-compiler-runtime-proof-owners-valid-p))
       (f1-names '(nelisp-native-compiler-f1-runtime-proof-create
                   nelisp-native-compiler-f1-runtime-proof-valid-p
                   nelisp-native-compiler-f1-runtime-proof-metadata
                   nelisp-native-compiler-f1-runtime-proof-owners-valid-p))
       (f1-available (cl-every #'fboundp f1-names))
       (f1-create (and f1-available (symbol-function (nth 0 f1-names))))
       (f1-valid (and f1-available (symbol-function (nth 1 f1-names))))
       (f1-owners-valid (and f1-available (symbol-function (nth 3 f1-names))))
       (f1-proof nil)
       (available (cl-every #'fboundp names))
       (create (and available (symbol-function (nth 0 names))))
       (valid (and available (symbol-function (nth 1 names))))
       (provider-owners-valid (and available (symbol-function (nth 3 names))))
       (lookup (symbol-function 'symbol-function))
       (same (symbol-function 'eq))
       (owners nil) (proof nil) (capability-owner nil) (owner-predicate nil))

(defun nelisp-native-compiler-runtime-capability-owner-p (candidate)
  "Recognize the original boot-loaded capability CANDIDATE, refusing copies."
  (and available (funcall same candidate capability-owner)
       (funcall same capability-owner
                (funcall lookup 'nelisp-native-compiler-runtime-capability-p))
       (funcall same owner-predicate
                (funcall lookup 'nelisp-native-compiler-runtime-capability-owner-p))
       (cl-every (lambda (entry)
                   (funcall same (cdr entry) (funcall lookup (car entry)))) owners)
       (funcall provider-owners-valid)
       (or (not f1-available) (funcall f1-owners-valid))))

;;;###autoload
(defun nelisp-native-compiler-runtime-capability-p (&optional operations)
  "Check source-owned immutable eligibility for OPERATIONS.
Omitted OPERATIONS requests all compiler operations and therefore refuses a
constructor-only proof. No caller-supplied certificate is accepted."
  (condition-case nil
      (and available
           (cl-every (lambda (entry)
                       (funcall same (cdr entry) (funcall lookup (car entry)))) owners)
           ;; The generated proof authenticates actual units, mapped code,
           ;; bridge owners and the running binary before registry issuance.
           (let* ((requested (or operations '(numeric call constructor)))
                  (f1 (equal requested '(f1)))
                  (record (if f1
                              (and f1-available (or f1-proof (setq f1-proof (funcall f1-create)))
                                   (funcall f1-valid f1-proof nil :metadata))
                            (and (or proof (setq proof (funcall create)))
                                 (funcall valid proof nil :metadata)))))
             (and (nelisp-native-compiler-runtime-capability--metadata-p record)
                  (equal (plist-get record :abi-sha256)
                         (nelisp-runtime-reload-contract-hash))
                  (fboundp 'nelisp-native-load-running-binary-sha256)
                  (equal (plist-get record :binary-sha256)
                         (nelisp-native-load-running-binary-sha256))
                  (member requested '((constructor) (f1))))))
    (error nil)))

  (setq capability-owner (funcall lookup 'nelisp-native-compiler-runtime-capability-p)
        owner-predicate (funcall lookup 'nelisp-native-compiler-runtime-capability-owner-p)
        owners
        (mapcar (lambda (name) (cons name (funcall lookup name)))
                (append (and available names) (and f1-available f1-names)
                        '(nelisp-native-compiler-runtime-capability-p
                          nelisp-native-compiler-runtime-capability-owner-p
                          nelisp-native-compiler-runtime-capability--metadata-p
                          nelisp-native-compiler-runtime-capability--digest-p
                          nelisp-runtime-reload-contract-hash
                          symbol-function eq cl-every cl-remove-if-not
                          fboundp funcall car cdr plist-get mapcar equal
                          length listp stringp integerp string-match-p nth
                          = > <= not or and condition-case setq)))))

(provide 'nelisp-native-compiler-runtime-capability)
;;; nelisp-native-compiler-runtime-capability.el ends here
