;;; nelisp-bytecode-build-host-test.el --- Standalone compiler host checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(ert-deftest nelisp-standalone-build-host-refuses-unverified-dialect-before-compilation ()
  "An unverified host fails before any expensive reader preparation."
  (let ((nelisp-standalone--verified-bytecode-dialect-id nil)
        prepared)
    (cl-letf (((symbol-function 'nelisp-standalone--reader-units)
               (lambda ()
                 (setq prepared t)
                 (error "Reader preparation entered"))))
      (let ((failure (should-error (nelisp-standalone-build-reader))))
        (should (string-match-p "requires the pinned GNU Emacs 31.1 toolchain"
                                (error-message-string failure)))
        (should-not prepared)))))

(provide 'nelisp-bytecode-build-host-test)
