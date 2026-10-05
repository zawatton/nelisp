;;; emacs-process-network-error-test.el --- Network failure parity -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-process-events)

(ert-deftest emacs-process-network-error/denied-bind ()
  (dolist (entry '((1 . "Operation not permitted") (13 . "Permission denied")))
    (should
     (equal
      (condition-case err
          (emacs-process-events--signal-network-error
           (list :error (format "bind(/tmp/server) failed: errno=%d" (car entry))))
        (file-error err))
      (list 'file-error "Cannot bind server socket" (cdr entry))))))

(ert-deftest emacs-process-network-error/other-diagnostics-survive ()
  (dolist (diagnostic '("connect(/tmp/server) failed: errno=2"
                        "bind(/tmp/server) failed: errno=98"))
    (should
     (equal
      (condition-case err
          (emacs-process-events--signal-network-error (list :error diagnostic))
        (file-error err))
      (list 'file-error diagnostic)))))

;;; emacs-process-network-error-test.el ends here
