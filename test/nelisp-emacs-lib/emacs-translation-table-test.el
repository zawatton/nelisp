;;; emacs-translation-table-test.el --- tests for translation-table substrate  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-translation-table)

(ert-deftest emacs-translation-table-test/registers-symbol-properties ()
  (define-translation-table 'emacs-translation-table-test-table
    '((#x61 . #x62)
      (#x63 . #x64)))
  (should (get 'emacs-translation-table-test-table 'translation-table))
  (should (integerp (get 'emacs-translation-table-test-table
                         'translation-table-id)))
  (should (= (emacs-translation-table-get
              'emacs-translation-table-test-table #x61)
             #x62)))

(provide 'emacs-translation-table-test)

;;; emacs-translation-table-test.el ends here
