;;; fontset-test.el --- tests for lightweight fontset facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(load "fontset" nil t)

(ert-deftest fontset-test/feature-and-data-load ()
  (should (featurep 'fontset))
  (should (assoc "jisx0208" font-encoding-alist))
  (should (assq 'latin script-representative-chars))
  (should (assq 'han script-representative-chars)))

(ert-deftest fontset-test/xlfd-roundtrip ()
  (let* ((name "-misc-fixed-medium-r-normal--13-120-75-75-c-70-fontset-standard")
         (fields (x-decompose-font-name name)))
    (should (vectorp fields))
    (should (= (length fields) 12))
    (should (equal (aref fields xlfd-regexp-family-subnum) "misc-fixed"))
    (should (equal (aref fields xlfd-regexp-registry-subnum)
                   "fontset-standard"))
    (should (equal (x-compose-font-name fields) name))))

(ert-deftest fontset-test/fontset-name-and-menu ()
  (let ((fontset-alias-alist nil))
    (setup-default-fontset)
    (should (fontset-name-p standard-fontset-spec))
    (should (equal (fontset-plain-name standard-fontset-spec)
                   "fontset-standard"))
    (should (equal (car (generate-fontset-menu)) "Fontset"))))

(ert-deftest fontset-test/set-font-encoding-upserts ()
  (let ((font-encoding-alist (copy-tree font-encoding-alist)))
    (set-font-encoding "test-encoding" 'test-charset)
    (should (eq (cdr (assoc "test-encoding" font-encoding-alist))
                'test-charset))
    (set-font-encoding "test-encoding" 'other-charset)
    (should (eq (cdr (assoc "test-encoding" font-encoding-alist))
                'other-charset))))

(provide 'fontset-test)

;;; fontset-test.el ends here
