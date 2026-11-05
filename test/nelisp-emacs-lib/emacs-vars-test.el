;;; emacs-vars-test.el --- ERT tests for emacs-vars  -*- lexical-binding: t; -*-

;;; Commentary:

;; Checks for C-core global variable compatibility.

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'cl-lib)
(require 'ert)
(require 'emacs-vars)

(defconst emacs-vars-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest emacs-vars-test/require-loads-cleanly ()
  (should (featurep 'emacs-vars))
  (dolist (sym '(user-emacs-directory temporary-file-directory
                 locale-coding-system system-type path-separator
                 exec-path exec-suffixes file-name-handler-alist
                 inhibit-file-name-handlers inhibit-file-name-operation
                 pre-redisplay-function menu-bar-final-items))
    (should (boundp sym))))

(ert-deftest emacs-vars-test/exec-globals-have-unix-shape ()
  (should (stringp path-separator))
  (should (listp exec-path))
  (should (listp exec-suffixes)))

(ert-deftest emacs-vars-test/pre-redisplay-function-bootstrap-sentinel ()
  (should (boundp 'pre-redisplay-function))
  (should (functionp pre-redisplay-function)))

(ert-deftest emacs-vars-test/comint-can-append-menu-bar-final-items ()
  "Match the unguarded update performed while comint.el is loaded."
  (let ((menu-bar-final-items nil))
    (setq menu-bar-final-items
          (append '(completion inout signals) menu-bar-final-items))
    (should (equal menu-bar-final-items '(completion inout signals)))))

(ert-deftest emacs-vars-test/package-owns-source ()
  "The nelisp-emacs-foundation package is the only owner of emacs-vars.el.
A second copy under another packages/*/src would be resolved arbitrarily."
  (should (equal (mapcar #'file-truename
                         (nemacs-test-paths-source-owners "emacs-vars.el"))
                 (list (file-truename
                        (expand-file-name
                         "packages/nelisp-emacs-foundation/src/emacs-vars.el"
                         nemacs-test-paths-repo-root))))))

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
