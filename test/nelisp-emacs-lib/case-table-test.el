;;; case-table-test.el --- ERT for lightweight case-table facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/case-table.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest case-table-test/require-loads-standard-feature ()
  (should (featurep 'case-table))
  (dolist (sym '(describe-buffer-case-table case-table-get-table
                 get-upcase-table copy-case-table
                 set-case-syntax-delims set-case-syntax-pair
                 set-upcase-syntax set-downcase-syntax set-case-syntax))
    (should (fboundp sym))))

(ert-deftest case-table-test/lightweight-char-table-core ()
  (let ((table (case-table--make-char-table 'case-table nil)))
    (should (case-table--char-table-p table))
    (should (= (case-table--char-table-range table ?A) ?A))
    (case-table--set-char-table-range table '(?A . ?C) ?a)
    (should (= (case-table--char-table-range table ?A) ?a))
    (should (= (case-table--char-table-range table ?B) ?a))
    (case-table--set-char-table-extra-slot table 0 'up)
    (should (eq (case-table--char-table-extra-slot table 0) 'up))))

(ert-deftest case-table-test/set-case-syntax-pair-updates-down-and-up ()
  (let ((table (case-table--make-char-table 'case-table nil)))
    (set-case-syntax-pair ?A ?a table)
    (should (= (aref table ?A) ?a))
    (should (= (aref table ?a) ?a))
    (let ((up (case-table-get-table table 'up)))
      (should (= (aref up ?A) ?A))
      (should (= (aref up ?a) ?A)))))

(ert-deftest case-table-test/copy-case-table-invalidates-derived-slots ()
  (let ((table (case-table--make-char-table 'case-table nil)))
    (set-case-syntax-pair ?A ?a table)
    (case-table--put-extra-slot table 1 'canon)
    (let ((copy (copy-case-table table)))
      (should (= (aref copy ?A) ?a))
      (should (case-table--get-extra-slot copy 0))
      (should-not (case-table--get-extra-slot copy 1))
      (should-not (case-table--get-extra-slot copy 2)))))

(ert-deftest case-table-test/set-case-table-tracks-current ()
  (let ((table (make-char-table 'case-table)))
    (set-case-table table)
    (should (eq (current-case-table) table))))

(provide 'case-table-test)

;;; case-table-test.el ends here
