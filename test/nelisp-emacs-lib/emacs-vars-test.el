;;; emacs-vars-test.el --- ERT tests for emacs-vars  -*- lexical-binding: t; -*-

;;; Commentary:

;; Checks for C-core global variable compatibility.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emacs-vars)

(ert-deftest emacs-vars-test/require-loads-cleanly ()
  (should (featurep 'emacs-vars))
  (dolist (sym '(user-emacs-directory temporary-file-directory
                 locale-coding-system system-type path-separator
                 exec-path exec-suffixes file-name-handler-alist
                 inhibit-file-name-handlers inhibit-file-name-operation
                 pre-redisplay-function))
    (should (boundp sym))))

(ert-deftest emacs-vars-test/exec-globals-have-unix-shape ()
  (should (stringp path-separator))
  (should (listp exec-path))
  (should (listp exec-suffixes)))

(ert-deftest emacs-vars-test/pre-redisplay-function-bootstrap-sentinel ()
  (should (boundp 'pre-redisplay-function))
  (should (functionp pre-redisplay-function)))

(provide 'emacs-vars-test)

;;; emacs-vars-test.el ends here

(ert-deftest emacs-vars-test/gc-cons-vars-present-and-settable ()
  "gc-cons-threshold / gc-cons-percentage exist with numeric defaults and are
settable (Doc 06 A2)."
  (should (boundp 'gc-cons-threshold))
  (should (integerp gc-cons-threshold))
  (should (boundp 'gc-cons-percentage))
  (should (numberp gc-cons-percentage))
  (should (let ((gc-cons-threshold 123456)) (= 123456 gc-cons-threshold))))
