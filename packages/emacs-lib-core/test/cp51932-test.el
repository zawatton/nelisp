;;; cp51932-test.el --- tests for lightweight CP51932 tables  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-translation-table)
(load "cp51932" nil t)

(ert-deftest cp51932-test/feature-loads ()
  (should (featurep 'cp51932))
  (should (get 'cp51932-decode 'translation-table))
  (should (get 'cp51932-encode 'translation-table)))

(ert-deftest cp51932-test/representative-decode-mappings ()
  (let ((circled-one (decode-char 'japanese-jisx0208 #x2d21))
        (roman-one (decode-char 'japanese-jisx0208 #x2d35))
        (numero (decode-char 'japanese-jisx0208 #x2d62)))
    (should (= (emacs-translation-table-get 'cp51932-decode circled-one)
               #x2460))
    (should (= (emacs-translation-table-get 'cp51932-decode roman-one)
               #x2160))
    (should (= (emacs-translation-table-get 'cp51932-decode numero)
               #x2116))))

(ert-deftest cp51932-test/representative-encode-mappings ()
  (let ((circled-one (decode-char 'japanese-jisx0208 #x2d21))
        (roman-one (decode-char 'japanese-jisx0208 #x2d35)))
    (should (= (emacs-translation-table-get 'cp51932-encode #x2460)
               circled-one))
    (should (= (emacs-translation-table-get 'cp51932-encode #x2160)
               roman-one))))

(provide 'cp51932-test)

;;; cp51932-test.el ends here
