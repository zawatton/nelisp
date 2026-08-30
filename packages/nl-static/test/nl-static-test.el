;;; nl-static-test.el --- ERT tests for nl-static -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Against-the-bug tests for Doc 197: violations fail while the
;; annotation macro expands, correct syntax expands cleanly, disabled
;; annotations are expansion-identical to their plain forms, and an
;; opaque cross-function borrow remains nl-safe's runtime error.
;; Keep this file free of ert-x helpers: the standalone smoke runs these
;; exact test bodies through its minimal ERT shim.

;;; Code:

(require 'ert)
(require 'nl-static)

(nl-defdata nl-static-test-option
  (nl-static-test-none)
  (nl-static-test-some value))

(defun nl-static-test--opaque-mutable-borrow (cell)
  "Acquire a mutable borrow in a callee invisible to the scope macro."
  (nl-with-borrow-mut (_value cell) nil))

(defun nl-static-test--register-text-callee ()
  "Reset the typed registry and declare a one-string-argument callee."
  (clrhash nl-static--typed-signatures)
  (let ((nl--strict nil))
    (macroexpand
     '(nl-typed defun nl-static-test-takes-text ((text string)) text))))

;;; Error hierarchy and non-invasiveness --------------------------------

(ert-deftest nl-static-error-hierarchy-is-named ()
  (dolist (condition '(nl-total-error nl-borrow-scope-error nl-typed-error))
    (let ((conditions (get condition 'error-conditions)))
      (should (memq condition conditions))
      (should (memq 'nl-static-error conditions))
      (should (memq 'nl-error conditions))
      (should (memq 'error conditions)))))

(ert-deftest nl-static-require-does-not-rewrite-unannotated-defun ()
  (let ((plain '(defun nl-static-test-plain (x) (+ x 1))))
    (should
     (equal (let ((nl-static--enabled t)) (macroexpand plain))
            (let ((nl-static--enabled nil)) (macroexpand plain))))))

;;; nl-total -------------------------------------------------------------

(ert-deftest nl-static-total-unregistered-match-errors-during-expansion ()
  (let ((err
         (should-error
          (macroexpand
           '(nl-total defun nl-static-test-bad-total (value)
              (nl-match value
                ((nl-static-test-unregistered x) x))))
          :type 'nl-total-error)))
    (should (eq (plist-get (cdr err) :rule)
                'registered-exhaustive-nl-match))
    (should (eq (car (plist-get (cdr err) :form)) 'nl-match))))

(ert-deftest nl-static-total-non-exhaustive-match-errors-during-expansion ()
  (should-error
   (macroexpand
    '(nl-total defun nl-static-test-missing-total (value)
       (nl-match value
         ((nl-static-test-none) nil))))
   :type 'nl-total-error))

(ert-deftest nl-static-total-correct-program-expands-to-plain-defun ()
  (let ((expected
         '(defun nl-static-test-good-total (value)
            (nl-match value
              ((nl-static-test-none) nil)
              ((nl-static-test-some x) x)))))
    (should
     (equal
      (macroexpand
       '(nl-total defun nl-static-test-good-total (value)
          (nl-match value
            ((nl-static-test-none) nil)
            ((nl-static-test-some x) x))))
      (macroexpand expected)))))

(ert-deftest nl-static-total-disabled-expansion-is-plain-defun ()
  (should
   (equal
    (let ((nl-static--enabled nil))
      (macroexpand
       '(nl-total defun nl-static-test-disabled-total (value)
          (nl-match value ((unknown value) value)))))
    (macroexpand
     '(defun nl-static-test-disabled-total (value)
        (nl-match value ((unknown value) value)))))))

;;; nl-borrow-scope ------------------------------------------------------

(ert-deftest nl-static-borrow-mut-mut-errors-during-expansion ()
  (let ((err
         (should-error
          (macroexpand
           '(nl-borrow-scope
              (nl-with-borrow-mut (outer cell)
                (nl-with-borrow-mut (inner cell) inner))))
          :type 'nl-borrow-scope-error)))
    (should (eq (plist-get (cdr err) :rule)
                'conflicting-lexical-borrow))
    (should (eq (plist-get (plist-get (cdr err) :detail) :cell) 'cell))))

(ert-deftest nl-static-borrow-shared-then-mut-errors-during-expansion ()
  (should-error
   (macroexpand
    '(nl-borrow-scope
       (nl-with-borrow (outer cell)
         (nl-with-borrow-mut (inner cell) inner))))
   :type 'nl-borrow-scope-error))

(ert-deftest nl-static-borrow-mut-then-shared-errors-during-expansion ()
  (should-error
   (macroexpand
    '(nl-borrow-scope
       (nl-with-borrow-mut (outer cell)
         (nl-with-borrow (inner cell) inner))))
   :type 'nl-borrow-scope-error))

(ert-deftest nl-static-borrow-shared-shared-expands-cleanly ()
  (let ((body
         '(nl-with-borrow (outer cell)
            (nl-with-borrow (inner cell) (+ outer inner)))))
    (should (equal (macroexpand (list 'nl-borrow-scope body))
                   (list 'progn body)))))

(ert-deftest nl-static-borrow-sequential-mutable-scopes-expand-cleanly ()
  (let ((first '(nl-with-borrow-mut (a cell) a))
        (second '(nl-with-borrow-mut (b cell) b)))
    (should (equal (macroexpand (list 'nl-borrow-scope first second))
                   (list 'progn first second)))))

(ert-deftest nl-static-borrow-respects-let-shadowing ()
  (should
   (macroexpand
    '(nl-borrow-scope
       (nl-with-borrow-mut (outer cell)
         (let ((cell other-cell))
           (nl-with-borrow-mut (inner cell) inner)))))))

(ert-deftest nl-static-borrow-skips-quoted-data ()
  (should
   (equal
    (macroexpand
     '(nl-borrow-scope
        '(nl-with-borrow-mut (a cell)
           (nl-with-borrow-mut (b cell) b))))
    '(progn
       '(nl-with-borrow-mut (a cell)
          (nl-with-borrow-mut (b cell) b))))))

(ert-deftest nl-static-borrow-disabled-expansion-is-plain-progn ()
  (let ((body
         '(nl-with-borrow-mut (a cell)
            (nl-with-borrow-mut (b cell) b))))
    (should
     (equal (let ((nl-static--enabled nil))
              (macroexpand (list 'nl-borrow-scope body)))
            (list 'progn body)))))

(ert-deftest nl-static-borrow-opaque-callee-falls-through-to-nl-safe ()
  "Tier 3 is accepted by expansion and rejected by nl-safe at runtime."
  (let ((form
         '(nl-borrow-scope
            (let ((cell (nl-cell 1)))
              (nl-with-borrow-mut (_outer cell)
                (nl-static-test--opaque-mutable-borrow cell))))))
    (should (equal (macroexpand form)
                   (cons 'progn (cdr form))))
    (should-error (eval form) :type 'nl-borrow-error)))

;;; nl-typed -------------------------------------------------------------

(ert-deftest nl-static-typed-literal-mismatch-errors-during-expansion ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict t))
    (let ((err
           (should-error
            (macroexpand
             '(nl-typed defun nl-static-test-bad-literal ()
                (nl-static-test-takes-text 42)))
            :type 'nl-typed-error)))
      (should (eq (plist-get (cdr err) :rule) 'literal-shape-mismatch)))))

(ert-deftest nl-static-typed-declared-mismatch-errors-during-expansion ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict t))
    (let ((err
           (should-error
            (macroexpand
             '(nl-typed defun nl-static-test-bad-declared ((count integer))
                (nl-static-test-takes-text count)))
            :type 'nl-typed-error)))
      (should (eq (plist-get (cdr err) :rule)
                  'declared-binding-shape-mismatch)))))

(ert-deftest nl-static-typed-correct-literal-expands-cleanly ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict t))
    (should
     (macroexpand
      '(nl-typed defun nl-static-test-good-literal ()
         (nl-static-test-takes-text "ok"))))))

(ert-deftest nl-static-typed-nonstrict-keeps-static-check-advisory ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict nil))
    (should
     (macroexpand
      '(nl-typed defun nl-static-test-nonstrict ()
         (nl-static-test-takes-text 42))))))

(ert-deftest nl-static-typed-does-not-infer-local-let-bindings ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict t))
    (should
     (macroexpand
      '(nl-typed defun nl-static-test-no-inference ()
         (let ((local 42))
           (nl-static-test-takes-text local)))))))

(ert-deftest nl-static-typed-direct-assignment-invalidates-declaration ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict t))
    (should
     (macroexpand
      '(nl-typed defun nl-static-test-assigned ((value integer))
         (setq value "now text")
         (nl-static-test-takes-text value))))))

(ert-deftest nl-static-typed-runtime-assertion-catches-opaque-input ()
  (eval
   '(nl-typed defun nl-static-test-runtime-integer ((value integer))
      (+ value 1)))
  (should (= (nl-static-test-runtime-integer 4) 5))
  (should-error (nl-static-test-runtime-integer "four")))

(ert-deftest nl-static-typed-disabled-expansion-is-plain-defun ()
  (should
   (equal
    (let ((nl-static--enabled nil))
      (macroexpand
       '(nl-typed defun nl-static-test-disabled-typed ((value integer))
          (+ value 1))))
    (macroexpand
     '(defun nl-static-test-disabled-typed (value) (+ value 1))))))

(ert-deftest nl-static-typed-unannotated-call-site-is-not-checked ()
  (nl-static-test--register-text-callee)
  (let ((nl--strict t)
        (plain '(defun nl-static-test-plain-caller ()
                  (nl-static-test-takes-text 42))))
    ;; Stock `defun' expands on Emacs 30, but the literal mismatch in
    ;; its body must not be inspected or rejected by nl-static.
    (should (macroexpand plain))))

(provide 'nl-static-test)

;;; nl-static-test.el ends here
