;;; emacs-cc-byteorder-1-test.el --- byteorder FFI contract -*- lexical-binding: t; -*-

(require 'ert)
(defvar emacs-cc-byteorder-1-test--host-byteorder (symbol-function 'byteorder))
(defconst emacs-cc-byteorder-1-test--source
  (expand-file-name
   "../../packages/nelisp-emacs-foundation/src/emacs-cc-byteorder-1.el"
   (file-name-directory (or load-file-name buffer-file-name))))
(load emacs-cc-byteorder-1-test--source nil t)

(ert-deftest emacs-cc-byteorder-1/host-byteorder-is-preserved ()
  (should (eq (symbol-function 'byteorder)
              emacs-cc-byteorder-1-test--host-byteorder)))

(ert-deftest emacs-cc-byteorder-1/decodes-both-native-observations ()
  (should (= (emacs-cc-byteorder-1--decode-htonl 1) ?B))
  (should (= (emacs-cc-byteorder-1--decode-htonl #x01000000) ?l))
  (should-error (emacs-cc-byteorder-1--decode-htonl 0)))

(ert-deftest emacs-cc-byteorder-1/fallback-arity-names-primitive ()
  (let ((native (symbol-function 'byteorder)))
    (unwind-protect
        (progn
          (fmakunbound 'byteorder)
          (load emacs-cc-byteorder-1-test--source nil t)
          (should (equal '(wrong-number-of-arguments (byteorder 1))
                         (condition-case err (byteorder 'extra)
                           (error (list (car err) (cdr err)))))))
      (fset 'byteorder native))))

(provide 'emacs-cc-byteorder-1-test)
;;; emacs-cc-byteorder-1-test.el ends here
