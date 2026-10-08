;;; nelisp-bytecode-native-raw-v2-check-memo-test.el --- raw-v2 memo tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-bytecode-native-raw-v2-check-memo)

(defmacro nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn
         (nelisp-bytecode-native-raw-v2-check-memo-clear)
         ,@body)
     (nelisp-bytecode-native-raw-v2-check-memo-clear)))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/caches-only-unchanged-success ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (manifest (list :native (list :object-sha256 "object-digest")))
          (runtime-key (list :binary-sha256 "binary-digest" :generation 1))
          (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest runtime-key validator 'entry))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest runtime-key validator 'entry))
      (should (= calls 1)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/in-place-manifest-mutation-misses ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (manifest (list :status 'valid))
          (validator
           (lambda (candidate _name)
             (setq calls (1+ calls))
             (unless (eq (plist-get candidate :status) 'valid)
               '(:manifest-mutated)))))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest '(runtime 1) validator))
      (setcar (cdr manifest) 'mutated)
      (should (equal (nelisp-bytecode-native-raw-v2-check-memo-run
                      manifest '(runtime 1) validator)
                     '(:manifest-mutated)))
      (should (= calls 2)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/in-place-runtime-mutation-misses ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (manifest (list :status 'valid))
          (runtime-key (list :artifact-file-sha "file-a" :generation 1))
          (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest runtime-key validator))
      (setcar (cdr runtime-key) "file-b")
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest runtime-key validator))
      (should (= calls 2)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/nested-string-and-vector-mutations-miss ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
           (digest (copy-sequence "artifact-digest"))
           (addresses (vector 101 202))
           (manifest (list :artifact-sha digest :root-addresses addresses))
           (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest 'runtime validator))
      (aset digest 0 ?x)
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest 'runtime validator))
      (aset addresses 1 303)
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest 'runtime validator))
      (should (= calls 3)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/name-is-part-of-key ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   'manifest 'runtime validator 'entry-a))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   'manifest 'runtime validator 'entry-b))
      (should (= calls 2)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/refusals-are-never-cached ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (validator
           (lambda (_manifest _name)
             (setq calls (1+ calls))
             '(:raw-object-hash-mismatch))))
      (should (equal (nelisp-bytecode-native-raw-v2-check-memo-run
                      'manifest 'runtime validator)
                     '(:raw-object-hash-mismatch)))
      (should (equal (nelisp-bytecode-native-raw-v2-check-memo-run
                      'manifest 'runtime validator)
                     '(:raw-object-hash-mismatch)))
      (should (= calls 2)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/validator-mutation-is-not-cached ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (manifest (list :status 'before))
          (validator
           (lambda (candidate _name)
             (setq calls (1+ calls))
             (setcar (cdr candidate) 'after)
             nil)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest 'runtime validator))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   manifest 'runtime validator))
      (should (= calls 2)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/validator-redefinition-misses ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   'manifest 'runtime validator))
      (setq validator (lambda (_manifest _name) (setq calls (1+ calls)) nil))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   'manifest 'runtime validator))
      (should (= calls 2)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/oversized-and-unsupported-inputs-bypass ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil))
          (deep nil)
          (wide nil)
          (oversized-string
           (make-string
            (1+ nelisp-bytecode-native-raw-v2-check-memo-max-string-bytes) ?x))
          (oversized-vector
           (make-vector
            (1+ nelisp-bytecode-native-raw-v2-check-memo-max-vector-length) nil))
          (many-nodes nil)
          (cyclic (list nil))
          (unsupported (make-hash-table :test 'eq)))
      (dotimes (_ (1+ nelisp-bytecode-native-raw-v2-check-memo-max-depth))
        (setq deep (cons nil deep)))
      (dotimes (_ 205) (push (make-vector 100 :node) many-nodes))
      (setq many-nodes (nreverse many-nodes))
      (setq wide (list :payload oversized-vector))
      (setcar cyclic cyclic)
      (dolist (manifest
               (list (list :payload oversized-string)
                     wide deep many-nodes cyclic
                     (list :payload unsupported)))
        (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                     manifest 'runtime validator))
        (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                     manifest 'runtime validator)))
      (should (= calls 12)))))

(ert-deftest nelisp-bytecode-native-raw-v2-check-memo/evicts-past-record-bound ()
  (nelisp-bytecode-native-raw-v2-check-memo-test--with-empty-cache
    (let* ((calls 0)
          (validator (lambda (_manifest _name) (setq calls (1+ calls)) nil)))
      (dotimes (version
                (1+ nelisp-bytecode-native-raw-v2-check-memo-max-records))
        (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                     (list :version version) 'runtime validator)))
      (should (= calls (1+ nelisp-bytecode-native-raw-v2-check-memo-max-records)))
      (should-not (nelisp-bytecode-native-raw-v2-check-memo-run
                   '(:version 0) 'runtime validator))
      (should (= calls (+ 2 nelisp-bytecode-native-raw-v2-check-memo-max-records))))))

(provide 'nelisp-bytecode-native-raw-v2-check-memo-test)
;;; nelisp-bytecode-native-raw-v2-check-memo-test.el ends here
