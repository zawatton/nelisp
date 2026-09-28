;;; emacs-sqlite-ffi-test.el --- Tests for emacs-sqlite-ffi -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defvar emacs-sqlite-ffi-test--host-open
  (and (fboundp 'sqlite-open) (symbol-function 'sqlite-open)))

(require 'emacs-sqlite-ffi)

(ert-deftest emacs-sqlite-ffi-test/host-builtins-remain-installed ()
  "Loading the compatibility layer must not replace host SQLite."
  (when emacs-sqlite-ffi-test--host-open
    (should (eq emacs-sqlite-ffi-test--host-open
                (symbol-function 'sqlite-open)))))

(ert-deftest emacs-sqlite-ffi-test/opaque-object-recognition ()
  "Only tagged compatibility objects satisfy the private predicate."
  (let ((database (emacs-sqlite-ffi--object 42)))
    (should (emacs-sqlite-ffi--object-p database))
    (aset database 1 0)
    (should (emacs-sqlite-ffi--object-p database))
    (should-not (emacs-sqlite-ffi--object-p 42))
    (should-not (emacs-sqlite-ffi--object-p [emacs-sqlite-ffi]))))

(ert-deftest emacs-sqlite-ffi-test/parameter-sequences ()
  "Parameter helpers accept lists and vectors without changing order."
  (should (= 0 (emacs-sqlite-ffi--value-count nil)))
  (should (= 4 (emacs-sqlite-ffi--value-count '(nil 2 3.0 "four"))))
  (should (= 4 (emacs-sqlite-ffi--value-count [nil 2 3.0 "four"])))
  (should (equal "four"
                 (emacs-sqlite-ffi--value-at
                  '(nil 2 3.0 "four") 3)))
  (should (= 2 (emacs-sqlite-ffi--value-at [nil 2 3.0 "four"] 1)))
  (should-error (emacs-sqlite-ffi--value-count "not-a-sequence")
                :type 'wrong-type-argument))

(provide 'emacs-sqlite-ffi-test)

;;; emacs-sqlite-ffi-test.el ends here
