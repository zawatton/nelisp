;;; eucjp-ms-test.el --- tests for lightweight eucJP-ms tables  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-translation-table)
(load "eucjp-ms" nil t)

(ert-deftest eucjp-ms-test/feature-loads ()
  (should (featurep 'eucjp-ms))
  (should (get 'eucjp-ms-decode 'translation-table))
  (should (get 'eucjp-ms-encode 'translation-table)))

(ert-deftest eucjp-ms-test/representative-jis-decode-mappings ()
  (let ((circled-one (decode-char 'japanese-jisx0208 #x2d21))
        (roman-one (decode-char 'japanese-jisx0208 #x2d35)))
    (should (= (emacs-translation-table-get 'eucjp-ms-decode circled-one)
               #x2460))
    (should (= (emacs-translation-table-get 'eucjp-ms-decode roman-one)
               #x2160))))

(ert-deftest eucjp-ms-test/private-use-and-encode-mappings ()
  (let ((circled-one (decode-char 'japanese-jisx0208 #x2d21)))
    (should (= (emacs-translation-table-get 'eucjp-ms-decode #xf5a1)
               #xe000))
    (should (= (emacs-translation-table-get 'eucjp-ms-encode #xe000)
               #xf5a1))
    (should (= (emacs-translation-table-get 'eucjp-ms-encode #x2460)
               circled-one))))

(provide 'eucjp-ms-test)

;;; eucjp-ms-test.el ends here
