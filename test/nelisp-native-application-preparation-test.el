;;; nelisp-native-application-preparation-test.el --- private closure prep -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-native-application-producer)
(require 'nelisp-cc-fxir-admission)

(defun nnap-prep-test--native-shaped-constructor (form)
  "Return a host-only fixture matching the authenticated NeLisp closure shape.
This mock does not exercise the real native constructor or application body."
  (cons 'closure (cons '(t) (cons (nth 2 form) (nthcdr 3 form)))))

(defun nnap-prep-test--plan (forms)
  (let ((names (mapcar #'cadr forms)))
    (list :status :validated-inputs :names names
          :forms (mapcar (lambda (form) (cons (cadr form) form)) forms)
          :form-hashes
          (mapcar (lambda (form)
                    (cons (cadr form) (nelisp-cc-fxir-admission--digest form)))
                  forms))))

(defun nnap-prep-test--assert-unbound (names)
  (dolist (name names) (should-not (fboundp name))))

(ert-deftest nnap-prep-prepares-renamed-definitions-in-plan-order-without-binding ()
  (let* ((first (make-symbol "nnap-prep-first"))
         (second (make-symbol "nnap-prep-second"))
         (forms (list `(defun ,second (left right) (+ left right))
                      `(defun ,first (input) (let ((copy input)) copy))))
         (plan (nnap-prep-test--plan forms))
         (candidates (nelisp-native-application-producer--prepare-candidates
                      plan #'nnap-prep-test--native-shaped-constructor)))
    (should (equal (mapcar (lambda (candidate) (plist-get candidate :name)) candidates)
                   (list second first)))
    (dolist (candidate candidates)
      (let ((form (plist-get candidate :form)))
        (should (eq (plist-get candidate :name) (cadr form)))
        (should (equal (plist-get candidate :args) (nth 2 form)))
        (should (equal (plist-get candidate :body) (nthcdr 3 form)))
        (should (equal (plist-get candidate :form-sha256)
                       (nelisp-cc-fxir-admission--digest form)))
        (should (nelisp-native-application-producer--closure-authentic-p
                 (plist-get candidate :closure) form
                 #'nnap-prep-test--native-shaped-constructor)))
    (nnap-prep-test--assert-unbound (list first second)))))

(ert-deftest nnap-prep-rejects-constructor-environment-argument-and-body-drift ()
  (let* ((name (make-symbol "nnap-prep-drift"))
         (form `(defun ,name (value) (+ value 1)))
         (plan (nnap-prep-test--plan (list form)))
         (names (plist-get plan :names)))
    (dolist (kind '(environment arguments body))
      (should-error
       (nelisp-native-application-producer--prepare-candidates
        plan
        (lambda (source)
          (let ((closure (copy-tree
                          (nnap-prep-test--native-shaped-constructor source))))
            (pcase kind
              ('environment (setcar (cdr closure) nil))
              ('arguments (setcar (nthcdr 2 closure) '(other)))
              ('body (setcar (nthcdr 3 closure) 'different)))
            closure))))
      (nnap-prep-test--assert-unbound names))))

(ert-deftest nnap-prep-rejects-duplicate-or-missing-plan-entries-without-binding ()
  (let* ((first (make-symbol "nnap-prep-plan-first"))
         (second (make-symbol "nnap-prep-plan-second"))
         (forms (list `(defun ,first (x) x) `(defun ,second (y) y)))
         (plan (nnap-prep-test--plan forms))
         (one-form (car (plist-get plan :forms)))
         (one-hash (car (plist-get plan :form-hashes)))
         (bad-plans
          (list (plist-put (copy-sequence plan) :names (list first first))
                (plist-put (copy-sequence plan) :forms (list one-form))
                (plist-put (copy-sequence plan) :forms
                           (list one-form one-form))
                (plist-put (copy-sequence plan) :form-hashes (list one-hash))
                (plist-put (copy-sequence plan) :form-hashes
                           (list one-hash one-hash)))))
    (dolist (bad bad-plans)
      (should-error (nelisp-native-application-producer--prepare-candidates bad))
      (nnap-prep-test--assert-unbound (list first second)))))

(provide 'nelisp-native-application-preparation-test)
;;; nelisp-native-application-preparation-test.el ends here
