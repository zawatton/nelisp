;;; nelisp-eln-native-subr-bridge-test.el --- bridge cleanup tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-native-subr)

(define-error 'nelisp-eln-native-subr-bridge-test-error "Bridge test error")
(defvar nelisp-eln-native-subr-bridge-test--released nil)

(defun nelisp-eln-native-subr-bridge-test--run (outcome fail-cleanup)
  (setq nelisp-eln-native-subr-bridge-test--released nil)
    (cl-letf (((symbol-function 'nelisp-eln-objects-create)
               (lambda () 'objects))
              ((symbol-function 'nelisp-eln-raw-call-context-create)
               (lambda () 'context))
              ((symbol-function 'nelisp-eln-objects-encode)
               (lambda (_owner value) (if (eq value 'input) 29
                                        (error "unexpected value"))))
              ((symbol-function 'nelisp-eln-objects-activation-acquire)
               (lambda (_owner) 'activation))
              ((symbol-function 'nelisp-eln-raw-call-word)
               (lambda (&rest _args)
                 (pcase outcome
                   ('error (signal 'nelisp-eln-native-subr-bridge-test-error
                                   '(primary)))
                   ('throw (throw 'bridge-test-exit 'thrown))
                   ('quit (signal 'quit nil))
                   (_ 29))))
              ((symbol-function 'nelisp-eln-objects-activation-decode)
               (lambda (_activation _word) 'input))
              ((symbol-function 'nelisp-eln-objects-activation-release)
               (lambda (_owner)
                 (push 'activation nelisp-eln-native-subr-bridge-test--released)
                 (when fail-cleanup (error "cleanup activation"))))
              ((symbol-function 'nelisp-eln-objects-release)
               (lambda (_owner)
                 (push 'objects nelisp-eln-native-subr-bridge-test--released)
                 (when fail-cleanup (error "cleanup objects"))))
              ((symbol-function 'nelisp-eln-raw-call-context-release)
               (lambda (_owner)
                 (push 'context nelisp-eln-native-subr-bridge-test--released)
                 (when fail-cleanup (error "cleanup context")))))
      (nelisp-eln-native-subr--unary-bridge 123 'input)))

(defun nelisp-eln-native-subr-bridge-test--assert-released ()
  (should (equal (nreverse nelisp-eln-native-subr-bridge-test--released)
                 '(activation objects context))))

(ert-deftest nelisp-eln-native-subr-bridge-releases-after-success ()
  (should (eq (nelisp-eln-native-subr-bridge-test--run 'success nil) 'input))
  (nelisp-eln-native-subr-bridge-test--assert-released))

(ert-deftest nelisp-eln-native-subr-bridge-call-error-wins-over-cleanup-errors ()
  (let ((err (condition-case e
                 (nelisp-eln-native-subr-bridge-test--run 'error t)
               (error e))))
    (should (eq (car err) 'nelisp-eln-native-subr-bridge-test-error))
    (should (equal (cdr err) '(primary)))
    (nelisp-eln-native-subr-bridge-test--assert-released)))

(ert-deftest nelisp-eln-native-subr-bridge-preserves-throw-and-cleans-all ()
  (let ((result (catch 'bridge-test-exit
                  (nelisp-eln-native-subr-bridge-test--run 'throw t))))
    (should (eq result 'thrown))
    (nelisp-eln-native-subr-bridge-test--assert-released)))

(ert-deftest nelisp-eln-native-subr-bridge-preserves-quit-and-cleans-all ()
  (let ((err (condition-case e
                 (nelisp-eln-native-subr-bridge-test--run 'quit t)
               (quit e))))
    (should (eq (car err) 'quit)))
  (nelisp-eln-native-subr-bridge-test--assert-released))

(provide 'nelisp-eln-native-subr-bridge-test)

;;; nelisp-eln-native-subr-bridge-test.el ends here
