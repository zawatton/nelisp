;;; idna-mapping-test.el --- tests for lightweight IDNA mapping  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'idna-mapping)

(ert-deftest idna-mapping-test/feature-loads ()
  (should (featurep 'idna-mapping))
  (should (vectorp idna-mapping-table))
  (should (> (length idna-mapping-table) #x10ffff)))

(ert-deftest idna-mapping-test/ascii-and-controls ()
  (should (eq (elt idna-mapping-table 0) t))
  (should (eq (elt idna-mapping-table #x7f) t))
  (should (equal (elt idna-mapping-table ?A) "a"))
  (should (equal (elt idna-mapping-table ?Z) "z"))
  (should (null (elt idna-mapping-table ?a))))

(ert-deftest idna-mapping-test/representative-compatibility-maps ()
  (should (eq (elt idna-mapping-table #x00ad) 'ignored))
  (should (equal (elt idna-mapping-table #x00b9) "1"))
  (should (equal (elt idna-mapping-table #x00bd) "1⁄2"))
  (should (equal (elt idna-mapping-table #x212a) "k")))

(provide 'idna-mapping-test)

;;; idna-mapping-test.el ends here
