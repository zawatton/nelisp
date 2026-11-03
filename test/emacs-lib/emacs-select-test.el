;;; emacs-select-test.el --- Shared selection capability tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-select)

(ert-deftest emacs-select-capability-follows-transport ()
  (let ((emacs-select-backend nil))
    (should-not (emacs-select-display-selections-p))
    (setq emacs-select-backend (list :set #'ignore))
    (should-not (emacs-select-display-selections-p))
    (setq emacs-select-backend (list :set #'ignore :get #'ignore
                                    :owner #'ignore :exists #'ignore))
    (should (emacs-select-display-selections-p))))

(ert-deftest emacs-select-load-preserves-host-capability ()
  (let ((host (symbol-function 'display-selections-p))
        (emacs-select-backend nil))
    (load "emacs-select" nil t)
    (should (eq host (symbol-function 'display-selections-p)))))

(ert-deftest emacs-select-timestamp-only-requests-advertised-target ()
  (dolist (targets '(nil [TARGETS UTF8_STRING] [TARGETS TIMESTAMP] (TARGETS TIMESTAMP)))
    (let* ((requests nil)
           (emacs-select-backend
            (list :get (lambda (_selection target)
                         (push target requests)
                         (if (eq target 'TARGETS) targets 1234)))))
      (let ((supported (memq 'TIMESTAMP (append targets nil))))
        (should (equal (emacs-select--timestamp 'CLIPBOARD) (and supported 1234)))
        (should (equal (reverse requests) (if supported '(TARGETS TIMESTAMP) '(TARGETS))))))))

(provide 'emacs-select-test)
