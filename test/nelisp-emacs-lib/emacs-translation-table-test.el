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

(ert-deftest emacs-translation-table-test/dense-table-uses-vector ()
  (let ((table (emacs-translation-table--alist-table
                '((0 . 10)
                  (1 . 11)
                  (2 . 12)))))
    (should (vectorp table))
    (should (= (aref table 1) 11))))

(ert-deftest emacs-translation-table-test/sparse-table-survives-source-mutation ()
  (let ((alist (list (cons #x61 #x62)
                     (cons #x63 #x64))))
    (put 'emacs-translation-table-test-vector 'translation-table
         (emacs-translation-table--alist-table alist))
    (should (listp (get 'emacs-translation-table-test-vector
                        'translation-table)))
    (setcar (car alist) #x62)
    (setcdr (car alist) #x61)
    (should (= (emacs-translation-table-get
                'emacs-translation-table-test-vector #x61)
               #x62))))

(ert-deftest emacs-translation-table-test/large-key-table-survives-source-mutation ()
  (let ((alist (list (cons #x5000 #x62))))
    (put 'emacs-translation-table-test-large-key 'translation-table
         (emacs-translation-table--alist-table alist))
    (setcar (car alist) #x5001)
    (setcdr (car alist) #x63)
    (should (= (emacs-translation-table-get
                'emacs-translation-table-test-large-key #x5000)
               #x62))))

(provide 'emacs-translation-table-test)

;;; emacs-translation-table-test.el ends here
