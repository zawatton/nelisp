;;; nelisp-bytecode-compiler-input-test.el --- Compiler input adapter tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-stack)

(defvar nelisp-bytecode-runtime-dialect-id nil)

(defconst nelisp-bytecode-compiler-input-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defvar nelisp-bytecode-compiler-input-test--calls 0)

(defun nelisp-bytecode-compiler-input-test--elc-forms (path)
  "Read compiled forms from PATH as data; never evaluate them."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (unless (looking-at ";ELC")
      (error "Not an ELC file: %s" path))
    (forward-line 1)
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun nelisp-bytecode-compiler-input-test--objects (object)
  "Collect byte-code functions nested in unread OBJECT data."
  (cond
   ((byte-code-function-p object) (list object))
   ((consp object)
    (append (nelisp-bytecode-compiler-input-test--objects (car object))
            (nelisp-bytecode-compiler-input-test--objects (cdr object))))
   ((and (vectorp object) (not (stringp object)))
    (cl-loop for item across object
             append (nelisp-bytecode-compiler-input-test--objects item)))
   (t nil)))

(defun nelisp-bytecode-compiler-input-test--rest-fixture-functions ()
  "Compile the source fixture with GNU 31.1 and read the `.elc' as data."
  (unless (equal emacs-version "31.1")
    (error "Rest fixture requires GNU Emacs 31.1, got %s" emacs-version))
  (let* ((source (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-rest.el"
                                   nelisp-bytecode-compiler-input-test--root))
         (directory (make-temp-file "nelisp-rest-elc-" t))
         (temp-source (expand-file-name "rest.el" directory))
         (temp-elc (concat temp-source "c")))
    (unwind-protect
        (progn
          (copy-file source temp-source t)
          (unless (byte-compile-file temp-source)
            (error "Could not compile GNU 31.1 rest fixture"))
          (delete-file temp-source)
          (let ((forms (nelisp-bytecode-compiler-input-test--elc-forms temp-elc)))
            (apply #'append (mapcar #'nelisp-bytecode-compiler-input-test--objects
                                    forms))))
      (delete-directory directory t))))

(defun nelisp-bytecode-compiler-input-test--closure-template-functions ()
  "Compile the closure fixture, delete its source, then read its ELC as data."
  (unless (equal emacs-version "31.1")
    (error "Closure template fixture requires GNU Emacs 31.1, got %s"
           emacs-version))
  (let* ((source (expand-file-name
                  "test/fixtures/native-bytecode/gnu-31.1-closure-template.el"
                  nelisp-bytecode-compiler-input-test--root))
         (directory (make-temp-file "nelisp-closure-elc-" t))
         (temp-source (expand-file-name "closure-template.el" directory))
         (temp-elc (concat temp-source "c")))
    (unwind-protect
        (progn
          (copy-file source temp-source t)
          (unless (byte-compile-file temp-source)
            (error "Could not compile GNU 31.1 closure fixture"))
          (delete-file temp-source)
          (let ((forms (nelisp-bytecode-compiler-input-test--elc-forms temp-elc)))
            (apply #'append
                   (mapcar #'nelisp-bytecode-compiler-input-test--objects forms))))
      (delete-directory directory t))))

(ert-deftest nelisp-bytecode-compiler-input/records-static-closure-template-metadata ()
  "Record only GNU 31.1's template descriptor, never runtime capture values."
  (skip-unless (equal emacs-version "31.1"))
  (let* ((functions
          (nelisp-bytecode-compiler-input-test--closure-template-functions))
         (function
          (cl-find-if
           (lambda (candidate)
             (nelisp-bytecode-compiler-input--closure-template-descriptor-p
              (and (> (length candidate) 4) (aref candidate 4))))
           functions))
         (result (nelisp-bytecode-compiler-input-build function)))
    (should function)
    (should-not (fboundp 'closure-template-fixture-function))
    (should-not (boundp 'closure-template-fixture-top-level-effect))
    (should (equal (plist-get result :closure-template-descriptor) '(nil . 83)))
    (should (eq (plist-get result :documentation-reference)
                (plist-get result :closure-template-descriptor)))
    (should (eq (plist-get result :metadata-role)
                'lazy-documentation-reference))
    (should (plist-get result :potential-capture-placeholder-p))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-stack-plan result)
                           :status)
                'unsupported))
    (should (eq (plist-get result :capture-values-available) nil))
    (should-not (plist-get result :captured-values))
    ;; The standalone reader may annotate the static offset with source name.
    (should (nelisp-bytecode-compiler-input--closure-template-descriptor-p
             '("compiled-source.el" . 175)))
    (require 'nelisp-bytecode-native-compiler)
    (let* ((artifact-path
            (expand-file-name (make-temp-name "closure-template-")
                              temporary-file-directory))
           (native-result
            (nelisp-bytecode-native-compiler-build
             function artifact-path 'closure-template-entry)))
      (should (eq (plist-get native-result :status) 'unsupported))
      (should-not (file-exists-p artifact-path)))))

(ert-deftest nelisp-bytecode-compiler-input/classifies-elc-lazy-docrefs-without-stripping-them ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((directory (make-temp-file "gnu-docref-plan-" t))
         (source (expand-file-name "docref.el" directory))
         (elc (concat source "c"))
         (names '(gnu-docref-car gnu-docref-cdr gnu-docref-cons gnu-docref-mixed))
         (forms '((defun gnu-docref-car (x) "car doc" (car x))
                  (defun gnu-docref-cdr (x) "cdr doc" (cdr x))
                  (defun gnu-docref-cons (x) "cons doc" (cons nil x))
                  (defun gnu-docref-mixed (x) "mixed doc"
                    (cons nil (car (cdr x))))))
         (values '((left . right) (left . right) (left . right)
                   ((first . second) . (third . fourth))))
         (expected '(left right (nil left . right) (nil . third))))
    (unwind-protect
        (progn
          (unless (equal emacs-version "31.1")
            (ert-skip "Requires the pinned GNU Emacs 31.1 byte compiler"))
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))
          (unless (byte-compile-file source) (error "GNU fixture compilation failed"))
          (load elc nil t t)
          (cl-loop for name in names for value in values for answer in expected
                   for input = (nelisp-bytecode-compiler-input-build
                                (symbol-function name))
                   for plan = (nelisp-bytecode-native-rooted-stack-plan input)
                   do (should (eq (plist-get input :status) 'complete))
                   do (should (eq (plist-get input :metadata-role)
                                  'lazy-documentation-reference))
                   do (should (eq (plist-get input :documentation-reference)
                                  (plist-get input :closure-template-descriptor)))
                   do (should (eq (plist-get plan :status) 'complete))
                   do (should (equal (funcall (symbol-function name) value) answer)))
          (dolist (name names) (fmakunbound name)))
      (dolist (name names) (when (fboundp name) (fmakunbound name)))
      (delete-directory directory t))))

(ert-deftest nelisp-bytecode-compiler-input/conservatively-refuses-literal-v0-prefix-collision ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (byte-compile '(lambda (x) (cons 'V0 x))))
         (input (nelisp-bytecode-compiler-input-build function)))
    (should (eq (plist-get input :status) 'complete))
    (should (plist-get input :potential-capture-placeholder-p))
    (should (equal (funcall function '(a . b)) '(V0 a . b)))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-stack-plan input)
                           :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-compiler-input/rejects-malformed-closure-template-descriptor ()
  "A malformed template marker is rejected rather than treated as doc data."
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function
          (cl-find-if
           (lambda (candidate)
             (nelisp-bytecode-compiler-input--closure-template-descriptor-p
              (and (> (length candidate) 4) (aref candidate 4))))
           (nelisp-bytecode-compiler-input-test--closure-template-functions)))
         (mutated
          (make-byte-code (aref function 0) (aref function 1)
                          (aref function 2) (aref function 3) '(t . 83))))
    (should function)
    ;; Negative verifier mutation: corrupt one pinned closure marker component.
    (should-not
     (nelisp-bytecode-compiler-input--closure-template-descriptor-p
      (aref mutated 4)))
    (should (eq (plist-get (nelisp-bytecode-compiler-input-build mutated) :status)
                'malformed))))

(ert-deftest nelisp-bytecode-compiler-input/inspects-function-fields-without-calling-it ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((nelisp-bytecode-compiler-input-test--calls 0)
         (function
          (byte-compile
           '(lambda (required &optional optional &rest rest)
              "adapter doc"
              (interactive "p")
              nil)))
         (result (nelisp-bytecode-compiler-input-build function)))
    (should (byte-code-function-p function))
    (should (eq (plist-get result :function) function))
    (should (eq (plist-get result :status) 'complete))
    (should-not (plist-get result :argument-list))
    (should-not (plist-get result :argument-count))
    (should (= (plist-get result :argument-min) 1))
    (should-not (plist-get result :argument-max))
    (should (= (plist-get result :initial-stack-depth) 3))
    (should (= (plist-get result :rest-binding-stack-index) 2))
    (should-not (plist-get result :reason))
    (should (integerp (plist-get result :argument-descriptor)))
    (should (stringp (plist-get result :code)))
    (should (vectorp (plist-get result :constants)))
    (should (= (plist-get result :declared-stack-depth) (aref function 3)))
    (should (string-prefix-p "adapter doc" (plist-get result :doc)))
    (should (equal (plist-get result :interactive) "p"))
    (should (eq (plist-get result :code) (aref function 1)))
    (should (eq (plist-get result :constants) (aref function 2)))
    (should (= nelisp-bytecode-compiler-input-test--calls 0))))

(ert-deftest nelisp-bytecode-compiler-input/records-materialized-rest-frame-slots ()
  "Record required and rest slots without retaining source lambda forms."
  (skip-unless (equal emacs-version "31.1"))
  (should-not (fboundp 'dialect-fixture-rest-only))
  (should-not (fboundp 'dialect-fixture-rest-required))
  (dolist (case '((128 0) (385 1)))
    (let* ((descriptor (car case))
           (required (cadr case))
           (function
            (cl-find-if (lambda (candidate)
                          (= (aref candidate 0) descriptor))
                        (nelisp-bytecode-compiler-input-test--rest-fixture-functions)))
           (result (nelisp-bytecode-compiler-input-build function))
           (layout (plist-get result :argument-layout))
           (frame (plist-get result :frame-result))
           (entry (aref (plist-get frame :blocks) 0))
           (return (car (last (append (plist-get entry :instructions) nil)))))
      (should (eq (plist-get result :status) 'complete))
      (should (equal (plist-get (plist-get result :dialect-evidence) :dialect)
                     "GNU Emacs 31.1"))
      (should (integerp (plist-get result :argument-descriptor)))
      (should (= (plist-get result :argument-descriptor) descriptor))
      (should (eq (plist-get result :rest-argument-p) t))
      (should (= (plist-get result :required-argument-count) required))
      (should (= (plist-get result :rest-binding-stack-index) required))
      (should-not (plist-get result :argument-count))
      (should-not (plist-get result :argument-max))
      (should (= (plist-get result :initial-stack-depth) (1+ required)))
      (should (equal (plist-get layout :rest-binding-stack-index) required))
      (should (equal (plist-get frame :argument-layout) layout))
      (should (equal (plist-get return :inputs)
                     (list (list :entry 0 required))))))
  (should-not (fboundp 'dialect-fixture-rest-only))
  (should-not (fboundp 'dialect-fixture-rest-required)))

(ert-deftest nelisp-bytecode-compiler-input/rejects-malformed-rest-descriptors ()
  "Reject out-of-range descriptors and fixed descriptors with min over max."
  (skip-unless (equal emacs-version "31.1"))
  (dolist (descriptor '(258 65536))
    (let ((function (make-byte-code descriptor (unibyte-string 135) [] 2)))
      (should-not
       (nelisp-bytecode-compiler-input--argument-descriptor-p descriptor))
      (should (eq (plist-get (nelisp-bytecode-compiler-input-build function)
                             :status)
                  'malformed)))))

(ert-deftest nelisp-bytecode-compiler-input/does-not-misclassify-optional-rest ()
  "A materialized optional/rest descriptor has a bounded entry stack."
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (byte-compile
                    '(lambda (required &optional optional &rest rest) nil)))
         (result (nelisp-bytecode-compiler-input-build function)))
    (should (nelisp-bytecode-compiler-input--argument-descriptor-p
             (plist-get result :argument-descriptor)))
    (should (eq (plist-get result :status) 'complete))
    (should (plist-get result :rest-argument-p))
    (should (= (plist-get result :argument-min) 1))
    (should-not (plist-get result :argument-max))
    (should (= (plist-get result :initial-stack-depth) 3))
    (should (= (plist-get (plist-get result :argument-layout) :rest-binding-stack-index) 2))))

(ert-deftest nelisp-bytecode-compiler-input/admit-only-exact-rest-slot-return-template ()
  "Admit only the exact GNU variable-ref REST return byte-code shape."
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (make-byte-code '(required &rest values)
                                   (unibyte-string 8 135) [values] 1))
         (result (nelisp-bytecode-compiler-input-build function))
         (mutated (nelisp-bytecode-compiler-input-build
                   (make-byte-code '(required &rest values)
                                   (unibyte-string 9 135) [values] 1))))
    (should (eq (plist-get result :status) 'complete))
    (should (eq (plist-get result :rest-slot-return-template-p) t))
    (should (= (plist-get result :required-argument-count) 1))
    (should (= (plist-get result :rest-binding-stack-index) 1))
    (should (= (plist-get result :initial-stack-depth) 2))
    (should (equal (plist-get result :rest-native-code) (unibyte-string 135)))
    (should (eq (plist-get mutated :status) 'unsupported))
    (should-not (plist-get mutated :rest-slot-return-template-p))
    (should-not (plist-get mutated :rest-native-code))))

(ert-deftest nelisp-bytecode-compiler-input/verifier-rejects-rest-slot-mutation ()
  "A mutated rest binding slot must fail the layout verifier."
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function
          (cl-find-if (lambda (candidate) (= (aref candidate 0) 128))
                      (nelisp-bytecode-compiler-input-test--rest-fixture-functions)))
         (result (nelisp-bytecode-compiler-input-build function))
         (layout (copy-sequence (plist-get result :argument-layout))))
    (should (nelisp-bytecode-compiler-input--rest-layout-valid-p
             (plist-get result :argument-descriptor)
             (plist-get result :initial-stack-depth) layout))
    (setq layout (plist-put layout :rest-binding-stack-index 1))
    (should-not
     (nelisp-bytecode-compiler-input--rest-layout-valid-p
      (plist-get result :argument-descriptor)
      (plist-get result :initial-stack-depth) layout))))

(ert-deftest nelisp-bytecode-compiler-input/decodes-packed-min-max-arity-514 ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((code (unibyte-string 1 135))
         (packed (make-byte-code 514 code [] 3))
         (list-descriptor (make-byte-code '(x y) code [] 3))
         (too-shallow (make-byte-code 514 code [] 2))
         (packed-result (nelisp-bytecode-compiler-input-build packed))
         (list-result
          (nelisp-bytecode-compiler-input-build list-descriptor))
         (shallow-result
          (nelisp-bytecode-compiler-input-build too-shallow))
         (first (cons 'first nil)) (second (cons 'second nil)))
    (should (eq (plist-get packed-result :status) 'complete))
    (should (= (plist-get packed-result :argument-min) 2))
    (should (= (plist-get packed-result :argument-max) 2))
    (should (= (plist-get packed-result :argument-count) 2))
    (should (= (plist-get packed-result :initial-stack-depth) 2))
    (should-not (plist-get packed-result :argument-list))
    (should (eq (plist-get (plist-get list-result :frame-result) :status)
                'malformed))
    (should (eq (plist-get list-result :status) 'malformed))
    (should (= (plist-get list-result :initial-stack-depth) 0))
    (should (eq (funcall packed first second) first))
    (should (eq (plist-get shallow-result :status) 'malformed))
    (should (equal (plist-get shallow-result :reason)
                   "computed stack depth exceeds declared depth"))))

(ert-deftest nelisp-bytecode-compiler-input/packed-optional-uses-maximum-depth ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (make-byte-code 513 (unibyte-string 1 135) [] 3))
         (result (nelisp-bytecode-compiler-input-build function)))
    (should (eq (plist-get result :status) 'complete))
    (should (= (plist-get result :argument-min) 1))
    (should (= (plist-get result :argument-max) 2))
    (should-not (plist-get result :argument-count))
    (should (= (plist-get result :initial-stack-depth) 2))))

(ert-deftest nelisp-bytecode-compiler-input/reads-real-elc-function-as-data ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((defined-before (fboundp 'dialect-fixture-branch))
         (path (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-mini.elc"
                                 nelisp-bytecode-compiler-input-test--root))
         (forms (nelisp-bytecode-compiler-input-test--elc-forms path))
         (functions (apply #'append
                           (mapcar #'nelisp-bytecode-compiler-input-test--objects forms)))
         (function (car functions))
         (result (nelisp-bytecode-compiler-input-build function)))
    (should (= (length forms) 2))
    (should (= (length functions) 2))
    (should (eq (fboundp 'dialect-fixture-branch) defined-before))
    (should (eq (plist-get result :status) 'complete))
    (should (eq (plist-get result :function) function))
    (should (equal (plist-get (plist-get result :dialect-evidence) :dialect)
                   "GNU Emacs 31.1"))
    (should (equal (plist-get result :argument-descriptor) (aref function 0)))
    (should (equal (plist-get result :doc)
                   (and (> (length function) 4) (aref function 4))))
    (should (equal (plist-get result :interactive)
                   (and (> (length function) 5) (aref function 5))))
    (should (eq (plist-get (plist-get result :frame-result) :status) 'complete))))

(ert-deftest nelisp-bytecode-compiler-input/reports-malformed-object-and-constant ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((bad-constant (make-byte-code 0 (unibyte-string 192 135) [] 1))
         (constant-result (nelisp-bytecode-compiler-input-build bad-constant))
         (object-result (nelisp-bytecode-compiler-input-build '(lambda () nil))))
    (should (eq (plist-get constant-result :status) 'malformed))
    (should (plist-get constant-result :reason))
    (should (eq (plist-get object-result :status) 'malformed))
    (should (equal (plist-get object-result :reason)
                   "input is not a byte-code function"))))

(ert-deftest nelisp-bytecode-compiler-input/refuses-unpinned-runtime-identity ()
  (let ((emacs-version "31.2")
        (function (make-byte-code 0 (unibyte-string 135) [] 0)))
    (let ((result (nelisp-bytecode-compiler-input-build function)))
      (should (eq (plist-get result :status) 'unsupported))
      (should (string-match-p "not pinned" (plist-get result :reason))))))

(ert-deftest nelisp-bytecode-compiler-input/refuses-runtime-without-dialect-identity ()
  (let* ((original-boundp (symbol-function 'boundp))
         (function (make-byte-code 0 (unibyte-string 135) [] 0))
         result)
    (cl-letf (((symbol-function 'boundp)
               (lambda (symbol)
                 (if (eq symbol 'emacs-version)
                     nil
                   (funcall original-boundp symbol)))))
      (setq result (nelisp-bytecode-compiler-input-build function))
      (should (eq (plist-get result :status) 'unsupported))
      (should (string-match-p "dialect identity is unavailable"
                              (plist-get result :reason))))))

(ert-deftest nelisp-bytecode-compiler-input/accepts-build-verified-standalone-identity ()
  (let* ((original-boundp (symbol-function 'boundp))
         (original-subrp (symbol-function 'subrp))
         (native-comp-enable-subr-trampolines nil)
         ;; Model the native reader as well as its build marker. Genuine
         ;; standalone execution is checked separately against the reader image.
         (reader (lambda (source)
                   (unless (equal source "(quote :status)")
                     (error "unexpected native attestation probe"))
                   :status))
         (reader-subrp (lambda (object)
                         (or (eq object reader)
                             (funcall original-subrp object))))
         (nelisp-bytecode-compiler-input--runtime-source-evaluator reader)
         (nelisp-bytecode-compiler-input--runtime-probe-primitives
          (mapcar (lambda (name)
                    (cons name (if (eq name 'subrp) reader-subrp
                                 (symbol-function name))))
                  '(subrp funcall eq equal secure-hash)))
         (nelisp-bytecode-runtime-dialect-id
          (concat "GNU Emacs 31.1; inventory-sha256="
                  nelisp-bytecode-compiler-input--inventory-sha256))
         (function (make-byte-code 0 (unibyte-string 192 135) [42] 1))
         result)
    (cl-letf (((symbol-function 'nelisp--eval-source-string) reader)
              ((symbol-function 'subrp) reader-subrp)
              ((symbol-function 'boundp)
               (lambda (symbol)
                 (if (eq symbol 'emacs-version)
                     nil
                   (funcall original-boundp symbol)))))
      (setq result (nelisp-bytecode-compiler-input-build function)))
    (should (eq (plist-get result :status) 'complete))
    (should (eq (plist-get (plist-get result :dialect-evidence)
                           :runtime-evidence)
                'standalone-build-verified))))

(ert-deftest nelisp-bytecode-compiler-input/refuses-mutated-standalone-identity ()
  (let* ((original-boundp (symbol-function 'boundp))
         (nelisp-bytecode-runtime-dialect-id
          "GNU Emacs 31.1; inventory-sha256=wrong")
         (function (make-byte-code 0 (unibyte-string 192 135) [42] 1))
         result)
    (cl-letf (((symbol-function 'boundp)
               (lambda (symbol)
                 (if (eq symbol 'emacs-version)
                     nil
                   (funcall original-boundp symbol)))))
      (setq result (nelisp-bytecode-compiler-input-build function)))
    (should (eq (plist-get result :status) 'unsupported))
    (should (string-match-p "dialect identity is unavailable"
                            (plist-get result :reason)))))

(ert-deftest nelisp-bytecode-compiler-input/argument-values-do-not-seed-operand-stack ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((first (cons 'first nil))
         (second (cons 'second nil))
         (function (make-byte-code '(x y) (unibyte-string 1 135) [] 2))
         (vm-result (funcall function first second))
         (result (nelisp-bytecode-compiler-input-build function)))
    (should-not (memq vm-result (list first second)))
    (should (eq (plist-get result :status) 'malformed))
    (should (= (plist-get result :argument-count) 2))
    (should (= (plist-get result :initial-stack-depth) 0))))

(ert-deftest nelisp-bytecode-compiler-input/pins-only-verified-cons-template ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (make-byte-code 514 (unibyte-string 1 1 66 135) [] 4))
         (input (nelisp-bytecode-compiler-input-build function))
         (nearby (nelisp-bytecode-compiler-input-build
                  (make-byte-code 514 (unibyte-string 1 1 66 136 135) [] 4)))
         (malformed (nelisp-bytecode-compiler-input-build
                     (make-byte-code 514 (unibyte-string 1 1 66 135) [] 3))))
    (should (eq (plist-get input :status) 'complete))
    (should (= (plist-get input :argument-descriptor) 514))
    (should (= (plist-get input :initial-stack-depth) 2))
    (should (= (plist-get input :computed-temporary-stack-depth) 4))
    (should (equal (plist-get input :code) (unibyte-string 1 1 66 135)))
    (should (equal (plist-get input :constants) []))
    (should (eq (plist-get nearby :status) 'complete))
    (should (= (plist-get nearby :initial-stack-depth) 2))
    (should (= (plist-get nearby :computed-temporary-stack-depth) 4))
    (should (eq (plist-get malformed :status) 'malformed))))

(provide 'nelisp-bytecode-compiler-input-test)
;;; nelisp-bytecode-compiler-input-test.el ends here
