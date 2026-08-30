;;; nl-hygiene-test.el --- ERT tests for nl-hygiene -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 198 Phase 3.  The classic introduced-binding capture is pinned in
;; both directions, and a separate test proves that a literal free helper
;; read in the definition namespace cannot resolve to the same short name.

;;; Code:

(require 'ert)
(require 'nl-hygiene)
(require 'nl-ns-in)

(defmacro nlh-test-plain-capture (user-form)
  "Deliberately capture USER-FORM when it names `scratch'."
  `(let ((scratch 41))
     (list scratch ,user-form)))

(nl-defmacro-hygienic nlh-test-hygienic-capture (user-form)
  "Keep the introduced scratch binding distinct from USER-FORM."
  `(let ((scratch\# 41))
     (list scratch\# ,user-form)))

(defun nlh-reader-helper (x)
  "The conflicting short-name helper used by the negative control."
  (+ x 1000))

(defmacro nlh-test-plain-free-reference (x)
  "Call the unqualified, call-site-visible helper."
  `(nlh-reader-helper ,x))

(nl-defmacro-hygienic nlh-test-unresolved-free-reference (x)
  "Show that the macro form alone does not activate reader resolution."
  `(nlh-reader-helper ,x))

(defun nl-hygiene-test--eval-all-in (namespace source)
  "Read and evaluate every form in SOURCE under NAMESPACE resolution."
  (let ((forms (nl-ns-read-all-in namespace source))
        (value nil))
    (while forms
      (setq value (eval (car forms) t))
      (setq forms (cdr forms)))
    value))

(defun nl-hygiene-test--install-resolved-macro ()
  "Install the definition-resolved helper and macro used by tests."
  (eval '(nl-ns-define nlh-reader-space :members (nlh-reader-helper)) t)
  (nl-hygiene-test--eval-all-in
   'nlh-reader-space
   (concat
    "(defun nlh-reader-helper (x) (* x 2))\n"
    "(nl-defmacro-hygienic nlh-test-resolved-free-reference (x)\n"
    "  `(nlh-reader-helper ,x))\n")))

(ert-deftest nl-hygiene-classic-introduced-binding-captures-without-pass ()
  "RED control: ordinary defmacro captures the caller's `scratch'."
  (let ((scratch 7))
    (should (equal (nlh-test-plain-capture scratch) '(41 41)))))

(ert-deftest nl-hygiene-classic-introduced-binding-does-not-capture ()
  "GREEN: the trailing-# binding is fresh and caller `scratch' survives."
  (let ((scratch 7))
    (should (equal (nlh-test-hygienic-capture scratch) '(41 7)))))

(ert-deftest nl-hygiene-introduced-binding-is-fresh-per-expansion ()
  (let* ((first (macroexpand '(nlh-test-hygienic-capture scratch)))
         (second (macroexpand '(nlh-test-hygienic-capture scratch)))
         (first-binding (car (car (nth 1 first))))
         (second-binding (car (car (nth 1 second)))))
    (should-not (eq first-binding second-binding))))

(ert-deftest nl-hygiene-definition-reader-resolves-literal-free-identifier ()
  (nl-hygiene-test--install-resolved-macro)
  (let ((expansion (macroexpand '(nlh-test-resolved-free-reference 5))))
    (should (eq (car expansion) 'nlh-reader-space-nlh-reader-helper))
    (should (equal (eval expansion t) 10))
    ;; The same literal short name without definition-time resolution
    ;; reaches the conflicting global helper instead.
    (should (equal (nlh-test-plain-free-reference 5) 1005))))

(ert-deftest nl-hygiene-plain-read-does-not-imply-definition-resolution ()
  "The new defining form alone preserves ordinary Elisp name resolution."
  (let ((expansion
         (macroexpand '(nlh-test-unresolved-free-reference 5))))
    (should (eq (car expansion) 'nlh-reader-helper))
    (should (equal (eval expansion t) 1005))))

(provide 'nl-hygiene-test)

;;; nl-hygiene-test.el ends here
