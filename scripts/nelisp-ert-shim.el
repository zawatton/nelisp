;;; nelisp-ert-shim.el --- Minimal ERT for the standalone reader -*- lexical-binding: t; -*-

;; Provides just enough of ERT -- `ert-deftest', `should', `should-not',
;; `should-error' and a batch runner -- to execute the existing NeLisp test
;; suites on the Emacs-less standalone image, where the real `ert.el' is
;; unavailable.  The surface syntax mirrors ERT so the suites load unmodified.
;;
;; Usage:
;;   (load "scripts/nelisp-stdlib-prelude.el")
;;   (load "scripts/nelisp-ert-shim.el")
;;   (load "<module>.el")            ; the unit under test
;;   (load "<module>-test.el")       ; registers tests via `ert-deftest'
;;   (nelisp-ert-run-all "<label>")  ; runs them, prints a pass/fail summary

(defvar nelisp-ert--tests nil
  "Reverse-registration-order alist of (NAME . THUNK) from `ert-deftest'.")

(defmacro ert-deftest (name _arglist &rest body)
  "Register NAME with BODY as a standalone test.
A leading docstring and any leading keyword/value pairs (e.g. `:tags',
`:expected-result') are skipped, matching ERT's surface syntax.  ARGLIST
is always `()' in the suites and is ignored."
  (while (and body (or (stringp (car body)) (keywordp (car body))))
    (setq body (if (keywordp (car body)) (cddr body) (cdr body))))
  `(setq nelisp-ert--tests
         (cons (cons ',name (lambda () ,@body))
               (assq-delete-all ',name nelisp-ert--tests))))

(defmacro should (form)
  "Signal an error unless FORM evaluates to non-nil."
  `(or ,form (error "should failed: %S" ',form)))

(defmacro should-not (form)
  "Signal an error unless FORM evaluates to nil."
  `(and ,form (error "should-not failed: %S" ',form)))

(defmacro should-error (form &rest keys)
  "Assert that FORM signals an error; with `:type' SYM check the error symbol."
  (let ((type (plist-get keys :type)))
    `(let ((nelisp-ert--ok nil))
       (condition-case nelisp-ert--e
           ,form
         (error
          (setq nelisp-ert--ok t)
          ,(when type
             `(unless (eq (car nelisp-ert--e) ,type)
                (error "should-error: %S signalled %S, wanted %S"
                       ',form (car nelisp-ert--e) ,type)))))
       (unless nelisp-ert--ok
         (error "should-error: %S did not signal" ',form)))))

(defun nelisp-ert-run-all (&optional label filter exact reporter starter)
  "Run registered tests in registration order; print a summary.
Non-nil FILTER selects names containing that literal, case-sensitive string.
Non-nil EXACT instead requires the whole name to equal FILTER.
FILTER may also be a list of complete names.  Tests retain registration order.
REPORTER, when non-nil, is called with each selected name and its success flag.
STARTER, when non-nil, is called with its name immediately before each test.
Return the list (PASS FAIL)."
  (let ((tests (reverse nelisp-ert--tests))
        (pass 0) (fail 0))
    (dolist (tc tests)
      (when (or (null filter)
                (cond ((listp filter) (member (symbol-name (car tc)) filter))
                      (exact (equal filter (symbol-name (car tc))))
                      (t (string-search filter (symbol-name (car tc))))))
        (let ((success t))
          (when starter (funcall starter (symbol-name (car tc))))
          (condition-case e
              (progn (funcall (cdr tc)) (setq pass (1+ pass)))
            (error
             (setq success nil fail (1+ fail))
             (princ (format "FAIL %s: %S\n" (car tc) e))))
          (when reporter (funcall reporter (symbol-name (car tc)) success)))))
    (princ (format "== %s: %d passed, %d failed (of %d) ==\n"
                   (or label "ert") pass fail (+ pass fail)))
    (list pass fail)))

(provide 'ert)
(provide 'nelisp-ert-shim)
;;; nelisp-ert-shim.el ends here
