;;; nelisp-bytecode-native-compiler-test.el --- Native compiler tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-rooted-stack)

(ert-deftest nelisp-bytecode-native-compiler/compiles-materialized-user-argument ()
  (let* ((artifact (make-temp-name
                    (expand-file-name "nelisp-bytecode-entry-"
                                      temporary-file-directory)))
         (function (make-byte-code '(x) (unibyte-string 8 135)
                                   (vector 'x) 1 "native entry doc" "p"))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_bc_materialized_arg")))
    (unwind-protect
        (progn
          (should (eq (plist-get result :status) 'complete))
          (should (= (plist-get result :user-arity) 1))
          (should (eq (plist-get (plist-get result :input) :function)
                      function))
          (should (equal (plist-get (plist-get result :input) :doc)
                         "native entry doc"))
          (should (equal (plist-get (plist-get result :input) :interactive)
                         "p"))
          (should (eq (plist-get (plist-get result :verified-cfg) :status)
                      'complete))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler/routes-real-gnu-rooted-plan-to-raw-entry ()
  (let* ((functions (mapcar (lambda (form) (byte-compile form))
                            '((lambda (x) (car x)) (lambda (x) (cdr x))
                              (lambda (x) (cons nil x))
                              (lambda (x) (cons nil (car (cdr x)))))))
         (artifact (expand-file-name "unused-rooted-stack.nelr"
                                     temporary-file-directory))
         (called nil) result)
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-stack-build)
               (lambda (actual path)
                 (setq called (list actual path))
                 '(:status complete :artifact-kind raw-runtime-v2))))
      (dolist (function functions)
        (let* ((input (nelisp-bytecode-compiler-input-build function))
               (plan (nelisp-bytecode-native-rooted-stack-plan input)))
          (should (eq (plist-get plan :status) 'complete))
          (setq result (nelisp-bytecode-native-compiler-build
                        function artifact "nl_native_stack_probe_v1"))
          (should (eq (plist-get result :status) 'complete))
          (should (eq (plist-get (car called) :function) function))
          (should (equal (nelisp-bytecode-native-rooted-stack-plan
                          (car called)) plan))
          (should (equal (cadr called) artifact)))))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/rejects-optional-descriptor ()
  (let* ((artifact (make-temp-name "nelisp-bytecode-unsupported-"))
         (function (make-byte-code '(x &optional y) (unibyte-string 8 135)
                                   (vector 'x) 1))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_bc_optional")))
    (should (eq (plist-get result :status) 'unsupported))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/rejects-packed-descriptor-without-names ()
  (let* ((artifact (make-temp-name
                    (expand-file-name "nelisp-bytecode-packed-"
                                      temporary-file-directory)))
         (function (make-byte-code 257 (unibyte-string 192 135) [42] 2))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_bc_packed")))
    (should (eq (plist-get result :status) 'unsupported))
    (should (string-match-p "does not preserve argument names"
                            (plist-get result :reason)))
    (should (= (plist-get (plist-get result :input) :argument-min) 1))
    (should (= (plist-get (plist-get result :input) :argument-max) 1))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/rejects-unproven-variable-reference ()
  (let* ((artifact (make-temp-name
                    (expand-file-name "nelisp-bytecode-unknown-"
                                      temporary-file-directory)))
         (function (make-byte-code '(y) (unibyte-string 8 135)
                                   (vector 'x) 1))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_bc_unknown_var")))
    (should (eq (plist-get result :status) 'unsupported))
    (should (string-match-p "variable-ref" (plist-get result :reason)))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/rejects-non-byte-code-as-malformed ()
  (let ((result (nelisp-bytecode-native-compiler-build
                 '(lambda (x) x)
                 (expand-file-name "unused-bytecode-entry.neln"
                                   temporary-file-directory)
                 "entry")))
    (should (eq (plist-get result :status) 'malformed))))

(ert-deftest nelisp-bytecode-native-compiler/routes-cons-to-authenticated-raw-v2-abi ()
  (let* ((artifact (concat (make-temp-name
                            (expand-file-name "nelisp-bytecode-cons-"
                                              temporary-file-directory))
                           ".nelr"))
         (function (make-byte-code 514 (unibyte-string 1 1 66 135) [] 4))
         (emitted nil)
         (result nil))
    (require 'nelisp-runtime-reload-abi)
    (require 'nelisp-native-load)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
                     (lambda () (make-string 64 ?a)))
                    ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
                     (lambda () t))
                    ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
                     (lambda (source output build-id binary-sha)
                       (setq emitted (with-temp-buffer
                                       (insert-file-contents source)
                                       (buffer-string)))
                       (should (equal output artifact))
                       (should (equal build-id "gnu31-bytecode-cons-v1"))
                       (should (equal binary-sha (make-string 64 ?a)))
                       (write-region "raw-v2" nil output nil 'silent)
                       '(:kind raw-runtime :runtime-abi nl-raw-v2))))
            (setq result
                  (nelisp-bytecode-native-compiler-build
                   function artifact "nl_native_cons_probe")))
          (should (eq (plist-get result :status) 'complete))
          (should (eq (plist-get result :artifact-kind) 'raw-runtime-v2))
          (should (eq (plist-get result :return-repr) 'u64))
          (should (eq (plist-get result :evaluator-return-repr) 'sexp))
          (should (equal (plist-get result :gateway-import) "nl_native_cons_v2"))
          (should (string-match-p "extern-call nl_native_cons_v2" emitted))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler/dispatches-rooted-operations-with-admitted-plan-to-raw-target ()
  (let* ((directory (make-temp-file "compiler-rooted-stack-" t))
         (source (expand-file-name "fixture.el" directory))
         (elc (concat source "c")) captured result (build-count 0)
         (real-plan (symbol-function 'nelisp-bytecode-native-rooted-stack-plan)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(defun rooted-car (x) (car x))\n")
            (insert "(defun rooted-cdr (x) (cdr x))\n")
            (insert "(defun rooted-cons (x) (cons nil x))\n")
            (insert "(defun rooted-mixed (x) (cons nil (car (cdr x))))\n")
            (insert "(defun rooted-branch (x) (if x (car x) (cdr x)))\n"))
          (unless (byte-compile-file source) (error "byte compilation failed"))
          (load elc nil t t)
          (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-stack-plan)
                     (lambda (input)
                       (let ((actual (funcall real-plan input)))
                         (if (and (eq (plist-get actual :status) 'complete)
                                  (consp (plist-get actual :operations)))
                             actual
                           (if (> (length (plist-get (plist-get input :frame-result) :blocks)) 1)
                               '(:status unsupported)
                             '(:status complete :operations ((:operation car))))))))
                    ((symbol-function 'nelisp-bytecode-native-rooted-stack-build)
                     (lambda (input path)
                       (setq build-count (1+ build-count))
                       (setq captured (list input path))
                       '(:status complete :artifact-kind raw-runtime-v2))))
            (dolist (name '(rooted-car rooted-cdr rooted-cons rooted-mixed))
              (setq result
                    (nelisp-bytecode-native-compiler-build
                     (symbol-function name) (expand-file-name "out.nelr" directory)
                     "nl_native_stack_probe_v1"))
              (should (eq (plist-get result :status) 'complete))
              (should (eq (plist-get (car captured) :status) 'complete))
              (should (equal (cadr captured) (expand-file-name "out.nelr" directory))))
            (should (= build-count 4))
            (setq result
                  (nelisp-bytecode-native-compiler-build
                   (symbol-function 'rooted-branch)
                   (expand-file-name "branch.nelr" directory)
                   "nl_native_stack_probe_v1"))
            (should (eq (plist-get result :status) 'unsupported))
            (setq result
                  (nelisp-bytecode-native-compiler-build
                   (symbol-function 'rooted-mixed)
                   (expand-file-name "out.neln" directory)
                   "nl_native_stack_probe_v1"))
            (should (eq (plist-get result :status) 'unsupported))
            (should (= build-count 4))
            (should-not (file-exists-p (expand-file-name "out.neln" directory)))))
      (delete-directory directory t))))

(ert-deftest nelisp-bytecode-native-compiler-cons-refuses-nearby-shapes ()
  (let* ((artifact (concat (make-temp-name
                            (expand-file-name "nelisp-bytecode-cons-nearby-"
                                              temporary-file-directory))
                           ".nelr"))
         (function (make-byte-code 514 (unibyte-string 1 1 66 135) [nil] 4))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact
                  "nl_native_cons_probe")))
    (should (eq (plist-get result :status) 'unsupported))
    (should (string-match-p "pinned GNU 31.1 two-argument CONS template"
                            (plist-get result :reason)))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler-preserves-malformed-cons-diagnostic ()
  (let* ((artifact (concat (make-temp-name
                            (expand-file-name "nelisp-bytecode-cons-malformed-"
                                              temporary-file-directory))
                           ".nelr"))
         (function (make-byte-code 514 (unibyte-string 1 1 66 135) [] 3))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact
                  "nl_native_cons_probe")))
    (should (eq (plist-get result :status) 'malformed))
    (should (string-match-p "declared depth"
                            (plist-get result :reason)))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/compiles-packed-terminal-branch ()
  (let* ((code (unibyte-string 137 134 6 0 192 135 135))
         (constants [chosen-value])
         (function (make-byte-code 257 code constants 2))
         (artifact (make-temp-name
                    (expand-file-name "nelisp-public-branch-"
                                      temporary-file-directory)))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_public_branch")))
    (unwind-protect
        (progn
          (should (eq (plist-get result :status) 'complete))
          (should (eq (funcall function nil) 'chosen-value))
          (should (eq (funcall function t) t))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler/compiles-packed-two-arm-join ()
  (let* ((code (unibyte-string 137 131 8 0 192 130 12 0 193 130 12 0 135))
         (constants [left-value right-value])
         (function (make-byte-code 257 code constants 2))
         (artifact (make-temp-name
                    (expand-file-name "nelisp-public-join-"
                                      temporary-file-directory)))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_public_join")))
    (unwind-protect
        (progn
          (should (eq (plist-get result :status) 'complete))
          (should (eq (funcall function nil) 'right-value))
          (should (eq (funcall function t) 'left-value))
          (should (file-readable-p artifact)))
      (when (file-exists-p artifact) (delete-file artifact)))))

(ert-deftest nelisp-bytecode-native-compiler/rejects-packed-effects-and-keeps-malformed-distinct ()
  (let* ((artifact (make-temp-name
                    (expand-file-name "nelisp-public-branch-negative-"
                                      temporary-file-directory)))
         (call (make-byte-code 257 (unibyte-string 137 131 7 0 32 135 192 135)
                               [nil] 2))
         (malformed (make-byte-code 257 (unibyte-string 131) [] 1))
         (call-result (nelisp-bytecode-native-compiler-build
                       call artifact "nl_public_branch_call"))
         (malformed-result (nelisp-bytecode-native-compiler-build
                            malformed artifact "nl_public_branch_bad")))
    (should (eq (plist-get call-result :status) 'unsupported))
    (should (eq (plist-get malformed-result :status) 'malformed))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/rejects-packed-join-bypass ()
  (let* ((function
          (make-byte-code 257
                          (unibyte-string 137 134 13 0 192 136 130 9 0
                                          193 130 13 0 135)
                          [left-value right-value] 2))
         (artifact (make-temp-name
                    (expand-file-name "nelisp-public-join-bypass-"
                                      temporary-file-directory)))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_public_join_bypass")))
    (should (eq (plist-get result :status) 'unsupported))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-compiler/admits-exact-rest-slot-return-only ()
  "Compile only the authenticated GNU required-plus-REST return template."
  (let* ((function (make-byte-code '(required &rest values)
                                   (unibyte-string 8 135) [values] 1))
         (artifact (make-temp-name "nelisp-rest-template-"))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_rest_template"))
         (other (byte-compile '(lambda (required &rest values) (car values))))
         (other-artifact (concat artifact ".other"))
         (other-result (nelisp-bytecode-native-compiler-build
                        other other-artifact "nl_rest_other")))
    (unwind-protect
        (progn
          (should (eq (plist-get result :status) 'complete))
          (should (= (plist-get result :rest-required-count) 1))
          (should (equal (plist-get result :hidden-constant-indices) []))
          (should (= (plist-get result :hidden-constant-count) 0))
          (should (file-readable-p artifact))
          (should (eq (plist-get other-result :status) 'unsupported))
          (should-not (file-exists-p other-artifact)))
      (when (file-exists-p artifact) (delete-file artifact))
      (when (file-exists-p other-artifact) (delete-file other-artifact)))))

(provide 'nelisp-bytecode-native-compiler-test)
;;; nelisp-bytecode-native-compiler-test.el ends here
