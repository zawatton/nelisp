;;; nl-ns-in-test.el --- ERT tests for nl-ns-in -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nl-ns-in)

(defun nl-ns-in-test--reset ()
  "Start from a clean registry with namespace `tns' declared."
  (nl-ns-clear-namespaces)
  (eval '(nl-ns-define tns :members (limit wrap chunk helper err items)) t))

(defun nl-ns-in-test--expand (body)
  "Expand BODY in namespace `tns' after resetting the registry."
  (nl-ns-in-test--reset)
  (nl-ns-expand 'tns body))

(ert-deftest nl-ns-in-define-sets-default-prefix ()
  (nl-ns-in-test--reset)
  (should (equal (nl-ns-prefix 'tns) "tns-"))
  (should (equal (nl-ns-member-list 'tns) '(limit wrap chunk helper err items))))

(ert-deftest nl-ns-in-define-accepts-prefix-and-members ()
  (nl-ns-clear-namespaces)
  (eval '(nl-ns-define tns :prefix "t/" :members (foo)) t)
  (should (eq (nl-ns-qualify 'tns 'foo) 't/foo)))

(ert-deftest nl-ns-in-define-rejects-bad-input ()
  (should-error (macroexpand '(nl-ns-define "tns")))
  (should-error (macroexpand '(nl-ns-define tns :prefix sym)))
  (should-error (macroexpand '(nl-ns-define tns :members foo)))
  (should-error (macroexpand '(nl-ns-define tns :members (foo 3))))
  (should-error (macroexpand '(nl-ns-define tns :suffix "x"))))

(ert-deftest nl-ns-in-qualify ()
  (nl-ns-in-test--reset)
  (should (eq (nl-ns-qualify 'tns 'wrap) 'tns-wrap)))

(ert-deftest nl-ns-in-renames-declared-definitions-and-calls ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap (s) (chunk s)) (defun chunk (s) s)))
                 '((defun tns-wrap (s) (tns-chunk s))
                   (defun tns-chunk (s) s)))))

(ert-deftest nl-ns-in-renames-declared-variables ()
  (should (equal (nl-ns-in-test--expand
                  '((defvar limit 80) (defun wrap (s) (substring s 0 limit))))
                 '((defvar tns-limit 80)
                   (defun tns-wrap (s) (substring s 0 tns-limit))))))

(ert-deftest nl-ns-in-member-may-be-defined-by-another-block ()
  (should (equal (nl-ns-in-test--expand '((defun wrap (s) (helper s))))
                 '((defun tns-wrap (s) (tns-helper s))))))

(ert-deftest nl-ns-in-does-not-discover-members-from-the-body ()
  (nl-ns-clear-namespaces)
  (eval '(nl-ns-define tns :members (wrap)) t)
  (should-error (nl-ns-expand 'tns '((defun wrap () (chunk))
                                      (defun chunk () 1)))))

(ert-deftest nl-ns-in-rejects-definition-of-non-member ()
  (nl-ns-in-test--reset)
  (let ((err (should-error (nl-ns-expand 'tns '((defun stray () 1)))
                           :type 'nl-ns-in-non-member-error)))
    (should (memq 'stray (cdr err)))))

(ert-deftest nl-ns-in-leaves-foreign-names-alone ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap (s) (mapcar #'car (substring s 0 1)))))
                 '((defun tns-wrap (s) (mapcar #'car (substring s 0 1)))))))

(ert-deftest nl-ns-in-leaves-quote-alone ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap (s) (list 'chunk s))))
                 '((defun tns-wrap (s) (list 'chunk s))))))

(ert-deftest nl-ns-in-rewrites-sharp-quoted-function-refs ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap (l) (mapcar #'chunk l))))
                 '((defun tns-wrap (l) (mapcar #'tns-chunk l))))))

(ert-deftest nl-ns-in-backquote-template-is-literal ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap () `(chunk . 1))))
                 '((defun tns-wrap () `(chunk . 1))))))

(ert-deftest nl-ns-in-backquote-unquote-is-rewritten ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap () `(result ,chunk ,@items))))
                 '((defun tns-wrap () `(result ,tns-chunk ,@tns-items))))))

(ert-deftest nl-ns-in-nested-backquote-is-literal ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap () `(outer `(chunk ,chunk)))))
                 '((defun tns-wrap () `(outer `(chunk ,chunk)))))))

(defun nl-ns-in-test--shadow-error (form symbol)
  "Assert that FORM rejects a local binding of SYMBOL."
  (let ((err (should-error (nl-ns-in-test--expand form)
                           :type 'nl-ns-in-member-binding-error)))
    (should (memq symbol (cdr err)))))

(ert-deftest nl-ns-in-let-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap () (let ((limit 1)) limit))) 'limit))
(ert-deftest nl-ns-in-let-star-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap () (let* ((limit 1)) limit))) 'limit))
(ert-deftest nl-ns-in-lambda-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap () (lambda (limit) limit))) 'limit))
(ert-deftest nl-ns-in-defun-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap (limit) limit)) 'limit))
(ert-deftest nl-ns-in-defmacro-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defmacro wrap (limit) limit)) 'limit))
(ert-deftest nl-ns-in-defsubst-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defsubst wrap (limit) limit)) 'limit))
(ert-deftest nl-ns-in-cl-defun-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((cl-defun wrap (limit) limit)) 'limit))
(ert-deftest nl-ns-in-cl-defmacro-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((cl-defmacro wrap (limit) limit)) 'limit))
(ert-deftest nl-ns-in-optional-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap (&optional limit) limit)) 'limit))
(ert-deftest nl-ns-in-keyword-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((cl-defun wrap (&key limit) limit)) 'limit))
(ert-deftest nl-ns-in-supplied-p-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error
   '((defun wrap (&optional (x 1 limit)) x)) 'limit))
(ert-deftest nl-ns-in-dolist-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap (xs) (dolist (limit xs) limit))) 'limit))
(ert-deftest nl-ns-in-dotimes-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error '((defun wrap () (dotimes (limit 2) limit))) 'limit))
(ert-deftest nl-ns-in-condition-case-member-binding-is-an-error ()
  (nl-ns-in-test--shadow-error
   '((defun wrap () (condition-case err (error "x") (error err)))) 'err))

(ert-deftest nl-ns-in-default-forms-are-rewritten ()
  (should (equal (nl-ns-in-test--expand
                  '((defun wrap (&optional (x limit)) x)))
                 '((defun tns-wrap (&optional (x tns-limit)) x)))))

(ert-deftest nl-ns-in-defines-qualified-functions ()
  (nl-ns-in-test--reset)
  (eval '(nl-ns-in tns
           (defvar limit 3)
           (defun chunk (s) (substring s 0 limit))
           (defun wrap (s) (chunk s))) t)
  (should (fboundp 'tns-wrap))
  (should (equal (funcall 'tns-wrap "abcdef") "abc")))

(ert-deftest nl-ns-in-requires-a-defined-namespace ()
  (nl-ns-clear-namespaces)
  (should-error (macroexpand '(nl-ns-in nowhere (defun f () 1)))))

(ert-deftest nl-ns-in-empty-body-is-progn ()
  (nl-ns-in-test--reset)
  (should (equal (macroexpand '(nl-ns-in tns)) '(progn))))

;;; Doc 189 §4 Phase 1: declaration-time collision refusal -------------
;;
;; `pcoll-' fixture namespaces (never `tns') so these tests never
;; contend with `nl-ns-in-test--reset''s shared `tns' registration, and
;; each test calls `nl-ns-clear-namespaces' itself rather than relying
;; on that helper, since it also needs a clean `nl-ns--collision-owners'
;; ledger before asserting about it.

(ert-deftest nl-ns-in-collision-silently-wins-when-enforcement-is-off ()
  "Against-the-bug: two namespaces sharing a qualified name, neither
opted in, still let the later `nl-ns-in' definition silently win --
Doc 189 Phase 1's default-off contract leaves this exactly as it was."
  (nl-ns-clear-namespaces)
  (should-not nl-ns-enforce-collisions)
  (eval '(nl-ns-define pcoll-a :prefix "pcoll-" :members (wrap)) t)
  (eval '(nl-ns-define pcoll-b :prefix "pcoll-" :members (wrap)) t)
  (eval '(nl-ns-in pcoll-a (defun wrap () 'a)) t)
  (eval '(nl-ns-in pcoll-b (defun wrap () 'b)) t)
  (should (eq (funcall 'pcoll-wrap) 'b)))

(ert-deftest nl-ns-in-define-refuses-cross-namespace-collision-when-enforced ()
  (nl-ns-clear-namespaces)
  (let ((nl-ns-enforce-collisions t))
    (eval '(nl-ns-define pcoll-c :prefix "pcoll-" :members (wrap)) t)
    (let ((err (should-error
                (eval '(nl-ns-define pcoll-d :prefix "pcoll-" :members (wrap)) t)
                :type 'nl-ns-collision-error)))
      (should (memq 'pcoll-d (cdr err)))
      (should (memq 'pcoll-c (cdr err)))
      (should (memq 'pcoll-wrap (cdr err)))
      (should (memq 'wrap (cdr err))))
    ;; A refused declaration leaves no trace: `pcoll-d' never registered.
    (should-error (nl-ns-member-list 'pcoll-d))
    (should (equal (nl-ns-member-list 'pcoll-c) '(wrap)))))

(ert-deftest nl-ns-in-define-own-namespace-redefinition-is-not-a-collision ()
  (nl-ns-clear-namespaces)
  (let ((nl-ns-enforce-collisions t))
    (eval '(nl-ns-define pcoll-e :prefix "pcoll-" :members (wrap chunk)) t)
    (eval '(nl-ns-define pcoll-e :prefix "pcoll-" :members (wrap helper)) t)
    (should (equal (nl-ns-member-list 'pcoll-e) '(wrap helper)))
    ;; `chunk' was released when `pcoll-e' stopped claiming it; a
    ;; different namespace may now claim it without a collision.
    (eval '(nl-ns-define pcoll-f :prefix "pcoll-" :members (chunk)) t)
    (should (equal (nl-ns-member-list 'pcoll-f) '(chunk)))))

(ert-deftest nl-ns-in-define-identical-redeclaration-is-not-a-collision ()
  (nl-ns-clear-namespaces)
  (let ((nl-ns-enforce-collisions t))
    (eval '(nl-ns-define pcoll-g :prefix "pcoll-" :members (wrap)) t)
    (eval '(nl-ns-define pcoll-g :prefix "pcoll-" :members (wrap)) t)
    (should (equal (nl-ns-member-list 'pcoll-g) '(wrap)))))

(ert-deftest nl-ns-in-define-per-namespace-enforce-t-overrides-global-off ()
  (nl-ns-clear-namespaces)
  (should-not nl-ns-enforce-collisions)
  (eval '(nl-ns-define pcoll-h :prefix "pcoll-" :members (wrap) :enforce t) t)
  (should-error
   (eval '(nl-ns-define pcoll-i :prefix "pcoll-" :members (wrap) :enforce t) t)
   :type 'nl-ns-collision-error))

(ert-deftest nl-ns-in-define-per-namespace-enforce-nil-overrides-global-on ()
  (nl-ns-clear-namespaces)
  (let ((nl-ns-enforce-collisions t))
    (eval '(nl-ns-define pcoll-j :prefix "pcoll-" :members (wrap) :enforce nil) t)
    (eval '(nl-ns-define pcoll-k :prefix "pcoll-" :members (wrap) :enforce nil) t)
    (should (equal (nl-ns-member-list 'pcoll-k) '(wrap)))))

(ert-deftest nl-ns-in-define-enforce-property-accepts-t-and-nil-only ()
  (should-error (macroexpand '(nl-ns-define pcoll-l :members (wrap) :enforce maybe))))

(ert-deftest nl-ns-in-define-enforce-is-not-an-unknown-property ()
  (nl-ns-clear-namespaces)
  (should (eq (eval '(nl-ns-define pcoll-m :members (wrap) :enforce nil) t)
              'pcoll-m)))

(provide 'nl-ns-in-test)

;;; nl-ns-in-test.el ends here
