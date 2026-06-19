;;; ert.el --- Minimal ERT shim for NeLisp standalone  -*- lexical-binding: t; -*-

;;; Commentary:

;; Host Emacs should use its standard ERT.  Standalone NeLisp needs enough of
;; the command-line ERT surface for `nemacs -Q --batch -l ert -l test.el
;; -f ert-run-tests-batch-and-exit' smoke tests.

;;; Code:

(defvar ert--standalone-p
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(defun ert--host-load-standard ()
  "Load host Emacs's standard ERT library."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "ert" nil t))))

(if (not ert--standalone-p)
    (ert--host-load-standard)

  (defvar ert--tests nil
    "Registered standalone ERT test names.")

  (defvar ert--standalone-failed 0
    "Number of failures from the current standalone ERT run.")

  (defvar ert--standalone-skipped 0
    "Number of skipped tests from the current standalone ERT run.")

  (put 'ert-skip 'error-conditions '(ert-skip error))
  (put 'ert-skip 'error-message "Test skipped")

  (defun ert--register-test (name function)
    "Register test NAME with zero-argument FUNCTION."
    (put name 'ert--test-body function)
    (put name 'ert--test-passed nil)
    (setq ert--tests (cons name ert--tests))
    name)

  (defmacro ert-deftest (name args &rest body)
    "Define standalone ERT test NAME.
ARGS is accepted for source compatibility and must be nil."
    (ignore args)
    (let ((function-name
           (intern (concat "ert--test-body--" (symbol-name name)))))
      `(progn
         (defun ,function-name () ,@body)
         (ert--register-test ',name ',function-name)
         ',name)))

  (defun ert-fail (data)
    "Signal a standalone ERT failure with DATA."
    (error "ert failure: %S" data))

  (defun ert-skip (reason)
    "Skip the current standalone ERT test with REASON."
    (signal 'ert-skip (list reason)))

  (defmacro should (form)
    "Signal a test failure when FORM evaluates to nil."
    `(let ((value ,form))
       (unless value
         (ert-fail (list 'should ',form)))
       value))

  (defmacro should-not (form)
    "Signal a test failure when FORM evaluates non-nil."
    `(let ((value ,form))
       (when value
         (ert-fail (list 'should-not ',form value)))
       (not value)))

  (defun ert--condition-symbol (condition)
    "Return CONDITION's primary condition symbol."
    (if (consp condition)
        (car condition)
      condition))

  (defun ert--condition-type-matches-p (condition type)
    "Return non-nil when CONDITION satisfies expected TYPE."
    (let* ((symbol (ert--condition-symbol condition))
           (conditions (and (symbolp symbol)
                            (get symbol 'error-conditions))))
      (or (null type)
          (eq type 'error)
          (eq symbol type)
          (and conditions (memq type conditions)))))

  (defmacro should-error (form &rest keys)
    "Signal a test failure unless FORM signals an expected error.
The standalone shim supports the common `:type' keyword."
    `(let ((raised nil)
           (expected-type nil)
           (keys (list ,@keys)))
       (while keys
         (when (eq (car keys) :type)
           (setq expected-type (car (cdr keys))))
         (setq keys (cdr (cdr keys))))
       (condition-case err
           ,form
         (error
          (setq raised err)))
       (unless raised
         (ert-fail (list 'should-error ',form)))
       (unless (ert--condition-type-matches-p raised expected-type)
         (ert-fail (list 'should-error-type ',form expected-type raised)))
       raised))

  (defun ert--selector-match-p (selector test-symbol)
    "Return non-nil when SELECTOR matches TEST-SYMBOL."
    (cond
     ((or (null selector) (eq selector t)) t)
     ((symbolp selector) (eq selector test-symbol))
     ((stringp selector) (string-match-p selector (symbol-name test-symbol)))
     (t t)))

  (defun ert-run-tests-batch-and-exit (&optional selector)
    "Run standalone ERT tests matching SELECTOR and exit nonzero on failure."
    (let ((tests nil)
          (total 0)
          (body nil))
      (setq ert--standalone-failed 0)
      (setq ert--standalone-skipped 0)
      (dolist (test-symbol ert--tests)
        (when (ert--selector-match-p selector test-symbol)
          (setq tests (cons test-symbol tests))))
      (setq total (length tests))
      (princ (format "Running %d tests\n" total))
      (dolist (test-symbol tests)
        ;; Standalone NeLisp currently mishandles larger test bodies when they
        ;; run under the dynamic scope of loop variables.  Generate a no-arg
        ;; runner with concrete test symbols embedded in the body.
        (setq body
              (append body
                      (list
                       (list 'condition-case 'err
                             (list 'progn
                                   (list 'funcall
                                         (list 'get
                                               (list 'quote test-symbol)
                                               (list 'quote 'ert--test-body)))
                                   (list 'put
                                         (list 'quote test-symbol)
                                         (list 'quote 'ert--test-passed)
                                         t)
                                   (list 'princ
                                         (list 'format
                                               "   passed  %S\n"
                                               (list 'quote test-symbol))))
                             (list 'ert-skip
                                   (list 'setq
                                         'ert--standalone-skipped
                                         (list '+
                                               'ert--standalone-skipped
                                               1))
                                   (list 'princ
                                         (list 'format
                                               "  skipped  %S: %S\n"
                                               (list 'quote test-symbol)
                                               'err)))
                             (list 'error
                                   (list 'setq
                                         'ert--standalone-failed
                                         (list '+
                                               'ert--standalone-failed
                                               1))
                                   (list 'princ
                                         (list 'format
                                               "   FAILED  %S: %S\n"
                                               (list 'quote test-symbol)
                                               'err))))))))
      (eval (append (list 'defun 'ert--run-selected-tests nil) body) t)
      (ert--run-selected-tests)
      (princ (format "\nRan %d tests, %d failed\n"
                     total ert--standalone-failed))
      (when (and (> ert--standalone-failed 0) (fboundp 'kill-emacs))
        (kill-emacs 1))
      (if (> ert--standalone-failed 0) 'failed 'ok))))

(provide 'ert)

;;; ert.el ends here
